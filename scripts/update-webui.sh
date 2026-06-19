#!/usr/bin/env bash
set -euo pipefail

OH_MY_PI_REPO="${OH_MY_PI_REPO:-https://github.com/can1357/oh-my-pi.git}"
OH_MY_PI_REF="${OH_MY_PI_REF:-main}"

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
source_repo=$OH_MY_PI_REPO
if [[ "$source_repo" != *"://"* && "$source_repo" != git@* && -e "$repo_root/$source_repo" ]]; then
  source_repo=$(cd -- "$repo_root/$source_repo" && pwd -P)
fi

checkout_dir=$(mktemp -d)
trap 'rm -rf "$checkout_dir"' EXIT

git -C "$checkout_dir" init
git -C "$checkout_dir" remote add origin "$source_repo"
git -C "$checkout_dir" sparse-checkout set --no-cone \
  '/package.json' \
  '/bun.lock' \
  '/bunfig.toml' \
  '/tsconfig.base.json' \
  '/tsconfig.json' \
  '/packages/tsconfig.workspace.json' \
  '/packages/*/package.json' \
  '/packages/collab-web/**' \
  '/packages/wire/**' \
  '/python/robomp/web/package.json' \
  '/patches/**'
git -C "$checkout_dir" fetch --depth=1 origin "$OH_MY_PI_REF"
git -C "$checkout_dir" checkout --detach FETCH_HEAD

bun install --cwd "$checkout_dir" --frozen-lockfile --ignore-scripts
bun --cwd="$checkout_dir/packages/collab-web" run build

rm -rf "$repo_root/dist"
mkdir -p "$repo_root/dist"
cp -a "$checkout_dir/packages/collab-web/dist/." "$repo_root/dist/"

python3 - "$repo_root/dist" <<'PY'
from pathlib import Path
import re
import sys

dist = Path(sys.argv[1])
index_path = dist / "index.html"
robots_path = dist / "robots.txt"
sitemap_path = dist / "sitemap.xml"

index = index_path.read_text(encoding="utf-8")
index = re.sub(
    r"\s*<script\b(?=[^>]*\bsrc=[\"']https://um\.can\.ac/script\.js[\"'])[^>]*>\s*</script>\s*",
    "\n",
    index,
    flags=re.IGNORECASE,
)
index = re.sub(
    r"\s*<link\b(?=[^>]*\brel=[\"']canonical[\"'])(?=[^>]*\bhref=[\"']https://my\.omp\.sh/[\"'])[^>]*>\s*",
    "\n",
    index,
    flags=re.IGNORECASE,
)
index = re.sub(
    r"\s*<meta\b(?=[^>]*(?:property|name)=[\"'](?:og:url|twitter:url)[\"'])(?=[^>]*\bcontent=[\"']https://my\.omp\.sh/[\"'])[^>]*>\s*",
    "\n",
    index,
    flags=re.IGNORECASE,
)
index = re.sub(
    r"\s*<script\b(?=[^>]*\btype=[\"']application/ld\+json[\"'])[^>]*>.*?</script>\s*",
    "\n",
    index,
    flags=re.IGNORECASE | re.DOTALL,
)
index = index.replace("https://my.omp.sh/og-image.png", "/og-image.png")
index_path.write_text(index, encoding="utf-8")

if robots_path.exists():
    robots = robots_path.read_text(encoding="utf-8")
    robots = robots.replace(
        "Sitemap: https://my.omp.sh/sitemap.xml",
        "# Sitemap intentionally omitted: deployment host is selected at runtime.",
    )
    robots_path.write_text(robots, encoding="utf-8")

sitemap_path.unlink(missing_ok=True)

failures = []
index_after = index_path.read_text(encoding="utf-8")
if "um.can.ac" in index_after:
    failures.append("dist/index.html still contains um.can.ac")
if "https://my.omp.sh/" in index_after:
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
