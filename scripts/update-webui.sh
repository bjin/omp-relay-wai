#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/update-webui.sh [--master|--release]

Build dist/ from the pinned omp/ submodule commit by default.

Options:
  --master   update omp/ to the latest upstream default branch commit
  --release  update omp/ to the latest v*.*.* release tag
USAGE
}

die() {
  printf 'update-webui.sh: %s\n' "$*" >&2
  exit 1
}

update_mode=pinned
while (($#)); do
  case "$1" in
    --master)
      [[ "$update_mode" == pinned ]] || {
        usage >&2
        exit 2
      }
      update_mode=master
      ;;
    --release)
      [[ "$update_mode" == pinned ]] || {
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

ensure_clean_submodule() {
  local status
  status=$(git -C "$omp_dir" status --porcelain --untracked-files=normal)
  [[ -z "$status" ]] || die "omp/ has local changes; reset or commit them before updating webui"
}

checkout_master() {
  local branch=master

  if git -C "$omp_dir" ls-remote --exit-code --heads origin master >/dev/null 2>&1; then
    branch=master
  elif git -C "$omp_dir" ls-remote --exit-code --heads origin main >/dev/null 2>&1; then
    branch=main
  else
    die "origin has neither master nor main"
  fi

  git -C "$omp_dir" fetch origin "refs/heads/$branch:refs/remotes/origin/$branch"
  git -C "$omp_dir" checkout --detach "origin/$branch"
}

checkout_latest_release() {
  local latest_tag=
  local tag

  git -C "$omp_dir" fetch --tags --prune origin
  while IFS= read -r tag; do
    if [[ "$tag" =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
      latest_tag=$tag
      break
    fi
  done < <(git -C "$omp_dir" tag --list 'v*.*.*' --sort=-v:refname)

  [[ -n "$latest_tag" ]] || die "no v*.*.* release tag found"
  git -C "$omp_dir" checkout --detach "$latest_tag"
}

cleanup_omp() {
  git -C "$omp_dir" reset --hard -q HEAD >/dev/null 2>&1 || true
  rm -rf "$collab_dist"
}

apply_patches() {
  shopt -s nullglob
  local patches=("$patch_dir"/*.patch)
  shopt -u nullglob

  ((${#patches[@]} > 0)) || die "no patches found in omp-patches/"

  local patch
  for patch in "${patches[@]}"; do
    printf 'Applying %s\n' "${patch#$repo_root/}"
    git -C "$omp_dir" apply --whitespace=nowarn "$patch"
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

git -C "$repo_root" submodule update --init -- omp
[[ -e "$omp_dir/.git" ]] || die "omp/ is not an initialized git submodule"

ensure_clean_submodule
case "$update_mode" in
  master)
    checkout_master
    ;;
  release)
    checkout_latest_release
    ;;
esac
ensure_clean_submodule

trap cleanup_omp EXIT
apply_patches

bun install --cwd "$omp_dir" --frozen-lockfile --ignore-scripts
bun --cwd="$omp_dir/packages/collab-web" run build

rm -rf "$repo_root/dist"
mkdir -p "$repo_root/dist"
cp -a "$collab_dist/." "$repo_root/dist/"

validate_dist
