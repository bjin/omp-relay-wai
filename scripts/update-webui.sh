#!/usr/bin/env bash
set -euo pipefail

msg() {
  echo "[*] $*" >&2
}

warn() {
  echo "WARNING: $*" >&2
}

fail() {
  echo "[*] $*" >&2
  exit 1
}

cmd() {
  echo "[$] $*" >&2
  "$@"
}

usage() {
  cat <<'USAGE'
Usage: scripts/update-webui.sh [--master|--release]

Build dist/ from the pinned omp/ submodule commit by default.

Options:
  --master   update omp/ to the latest upstream default branch commit
  --release  update omp/ to the latest v*.*.* release tag
USAGE
}

update_mode=pinned
while (($#)); do
  case "$1" in
    --master)
      [[ $update_mode == pinned ]] || {
        usage >&2
        exit 2
      }
      update_mode=master
      ;;
    --release)
      [[ $update_mode == pinned ]] || {
        usage >&2
        exit 2
      }
      update_mode=release
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
  shift
done

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
omp_dir="$repo_root/omp"
patch_dir="$repo_root/omp-patches"
collab_dist="$omp_dir/packages/collab-web/dist"
relay_source="packages/collab-web/scripts/local-relay.ts"

ensure_clean_submodule() {
  local status
  status=$(git -C "$omp_dir" status --porcelain --untracked-files=normal)
  [[ -z $status ]] || fail "omp/ has local changes; reset or commit them before updating webui"
}

checkout_master() {
  local branch=master

  if git -C "$omp_dir" ls-remote --exit-code --heads origin master >/dev/null 2>&1; then
    branch=master
  elif git -C "$omp_dir" ls-remote --exit-code --heads origin main >/dev/null 2>&1; then
    branch=main
  else
    fail "origin has neither master nor main"
  fi

  cmd git -C "$omp_dir" fetch origin "refs/heads/$branch:refs/remotes/origin/$branch"
  cmd git -C "$omp_dir" checkout --detach "origin/$branch"
}

checkout_latest_release() {
  local latest_tag=
  local tag

  cmd git -C "$omp_dir" fetch --tags --prune origin
  while IFS= read -r tag; do
    if [[ $tag =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
      latest_tag=$tag
      break
    fi
  done < <(git -C "$omp_dir" tag --list 'v*.*.*' --sort=-v:refname)

  [[ -n $latest_tag ]] || fail "no v*.*.* release tag found"
  cmd git -C "$omp_dir" checkout --detach "$latest_tag"
}

warn_on_relay_source_changes() {
  local before_commit=$1
  local after_commit=$2
  local diff_status=0

  [[ $before_commit != "$after_commit" ]] || return 0

  git -C "$omp_dir" diff --quiet "$before_commit" "$after_commit" -- "$relay_source" || diff_status=$?
  case "$diff_status" in
    0)
      return 0
      ;;
    1)
      ;;
    *)
      fail "failed to compare $relay_source between $before_commit and $after_commit"
      ;;
  esac

  warn "$relay_source changed between ${before_commit:0:12} and ${after_commit:0:12}."
  warn "Review the WAI WebSocket relay implementation before deploying these assets."
  cmd git -C "$omp_dir" diff --no-ext-diff "$before_commit" "$after_commit" -- "$relay_source"
}

cleanup_omp() {
  git -C "$omp_dir" reset --hard -q HEAD >/dev/null 2>&1 || true
  rm -rf "$collab_dist"
}

apply_patches() {
  shopt -s nullglob
  local patches=("$patch_dir"/*.patch)
  shopt -u nullglob

  ((${#patches[@]} > 0)) || fail "no patches found in omp-patches/"

  local patch
  for patch in "${patches[@]}"; do
    msg "Applying ${patch#"$repo_root"/}"
    cmd git -C "$omp_dir" apply --whitespace=nowarn "$patch"
  done
}

validate_dist() {
  python3 - "$repo_root/dist" <<'PY'
from pathlib import Path
import sys

dist = Path(sys.argv[1])
index_path = dist / "index.html"
robots_path = dist / "robots.txt"
sitemap_path = dist / "sitemap.xml"

failures = []

if not index_path.is_file():
    failures.append("dist/index.html is missing")
else:
    index = index_path.read_text(encoding="utf-8")
    if "um.can.ac" in index:
        failures.append("dist/index.html still contains um.can.ac")
    if "https://my.omp.sh/" in index:
        failures.append("dist/index.html still contains https://my.omp.sh/")

if robots_path.exists() and "my.omp.sh" in robots_path.read_text(encoding="utf-8"):
    failures.append("dist/robots.txt still contains my.omp.sh")
if sitemap_path.exists():
    failures.append("dist/sitemap.xml still exists")

if failures:
    for failure in failures:
        print(failure, file=sys.stderr)
    sys.exit(1)
PY
}

cmd git -C "$repo_root" submodule update --init -- omp
[[ -e "$omp_dir/.git" ]] || fail "omp/ is not an initialized git submodule"

ensure_clean_submodule
base_omp_commit=$(git -C "$omp_dir" rev-parse HEAD)
case "$update_mode" in
  master)
    checkout_master
    ;;
  release)
    checkout_latest_release
    ;;
esac
selected_omp_commit=$(git -C "$omp_dir" rev-parse HEAD)
ensure_clean_submodule
warn_on_relay_source_changes "$base_omp_commit" "$selected_omp_commit"

trap cleanup_omp EXIT
apply_patches

cmd bun install --cwd "$omp_dir" --frozen-lockfile --ignore-scripts
cmd bun --cwd="$omp_dir/packages/collab-web" run build

cmd rm -rf "$repo_root/dist"
cmd mkdir -p "$repo_root/dist"
cmd cp -a "$collab_dist/." "$repo_root/dist/"

validate_dist
