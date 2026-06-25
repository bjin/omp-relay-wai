#!/usr/bin/env python3
"""Regenerate local omp/ source patches for the embedded collab web UI."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
OMP_DIR = REPO_ROOT / "omp"
PATCH_DIR = REPO_ROOT / "omp-patches"
PATCH_PATH = PATCH_DIR / "0001-collab-web-runtime-neutral-metadata.patch"
COLLAB_WEB = OMP_DIR / "packages" / "collab-web"


def run(*args: str, capture: bool = False) -> str:
    result = subprocess.run(
        args,
        cwd=REPO_ROOT,
        check=True,
        text=True,
        stdout=subprocess.PIPE if capture else None,
    )
    return result.stdout if capture else ""


def reset_omp() -> None:
    run("git", "-C", str(OMP_DIR), "reset", "--hard", "HEAD")
    run("git", "-C", str(OMP_DIR), "clean", "-fd")
    shutil.rmtree(COLLAB_WEB / "dist", ignore_errors=True)


def replace_once(text: str, old: str, new: str, path: Path) -> str:
    if old not in text:
        raise RuntimeError(f"expected text not found in {path}: {old!r}")
    return text.replace(old, new, 1)


def patch_index_html() -> None:
    path = COLLAB_WEB / "index.html"
    text = path.read_text(encoding="utf-8")

    removals = [
        '\t\t<link rel="canonical" href="https://my.omp.sh/" />\n',
        '\t\t<meta property="og:url" content="https://my.omp.sh/" />\n',
        '\t\t<meta name="twitter:url" content="https://my.omp.sh/" />\n',
        """\t\t<!-- Structured data: WebApplication -->\n\t\t<script type=\"application/ld+json\">\n\t\t\t{\n\t\t\t\t\"@context\": \"https://schema.org\",\n\t\t\t\t\"@type\": \"WebApplication\",\n\t\t\t\t\"name\": \"omp collab\",\n\t\t\t\t\"url\": \"https://my.omp.sh/\",\n\t\t\t\t\"applicationCategory\": \"DeveloperApplication\",\n\t\t\t\t\"operatingSystem\": \"Any\",\n\t\t\t\t\"description\": \"Browser guest client for omp collab live sessions: streaming transcript, tool calls, subagent panel, and a composer that prompts the host agent. End-to-end encrypted.\",\n\t\t\t\t\"isPartOf\": { \"@type\": \"WebSite\", \"name\": \"omp\", \"url\": \"https://omp.sh/\" }\n\t\t\t}\n\t\t</script>\n\n""",
        """\t\t<!-- Analytics. data-exclude-hash keeps the E2E room key (URL fragment) out of analytics. -->\n\t\t<script defer src=\"https://um.can.ac/script.js\" data-website-id=\"28ab5a7d-3ca4-4c85-9da5-666fa731cb92\" data-exclude-hash=\"true\"></script>\n""",
    ]

    for old in removals:
        text = replace_once(text, old, "", path)

    old_image = "https://my.omp.sh/og-image.png"
    if text.count(old_image) != 2:
        raise RuntimeError(f"expected two metadata image URLs in {path}")
    text = text.replace(old_image, "/og-image.png")

    path.write_text(text, encoding="utf-8")


def patch_robots() -> None:
    path = COLLAB_WEB / "public" / "robots.txt"
    text = path.read_text(encoding="utf-8")
    text = replace_once(
        text,
        "Sitemap: https://my.omp.sh/sitemap.xml\n",
        "# Sitemap intentionally omitted: deployment host is selected at runtime.\n",
        path,
    )
    path.write_text(text, encoding="utf-8")


def remove_sitemap() -> None:
    path = COLLAB_WEB / "public" / "sitemap.xml"
    if not path.exists():
        raise RuntimeError(f"expected {path} to exist")
    path.unlink()


def write_patch() -> None:
    PATCH_DIR.mkdir(exist_ok=True)
    for old_patch in PATCH_DIR.glob("*.patch"):
        old_patch.unlink()

    diff = run(
        "git",
        "-C",
        str(OMP_DIR),
        "diff",
        "--binary",
        "--",
        "packages/collab-web/index.html",
        "packages/collab-web/public/robots.txt",
        "packages/collab-web/public/sitemap.xml",
        capture=True,
    )
    if not diff:
        raise RuntimeError("patch generation produced an empty diff")
    PATCH_PATH.write_text(diff, encoding="utf-8")


def main() -> None:
    run("git", "-C", str(REPO_ROOT), "submodule", "update", "--init", "--", "omp")
    try:
        reset_omp()
        patch_index_html()
        patch_robots()
        remove_sitemap()
        write_patch()
    finally:
        reset_omp()

    print(f"wrote {PATCH_PATH.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    main()
