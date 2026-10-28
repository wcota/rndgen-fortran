#!/usr/bin/env bash
# Bump the version (semver), then, after showing the diff and asking for
# confirmation, commit and create an annotated git tag.
# Nothing is pushed: the push and undo commands are printed at the end.
#
# Usage:
#   scripts/release.sh major     # 2.3.4 -> 3.0.0
#   scripts/release.sh minor     # 2.3.4 -> 2.4.0
#   scripts/release.sh patch     # 2.3.4 -> 2.3.5
#   scripts/release.sh current   # tag the version already in fpm.toml (no bump)
#
# Files updated: fpm.toml (version), docs/formal.md (version), the header of
# src/rndgen_mod.f90 (Version/Date) and the fpm install tag in README.md and
# docs/guide/index.md.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

FPM=fpm.toml
HEADER=src/rndgen_mod.f90
FORMAL=docs/formal.md
INSTALL_DOCS=(README.md docs/guide/index.md)
FILES=("$FPM" "$HEADER" "$FORMAL" "${INSTALL_DOCS[@]}")

die() { echo "error: $*" >&2; exit 1; }

[[ $# -eq 1 ]] || die "usage: $0 major|minor|patch|current"
kind=$1

if ! git diff --quiet || ! git diff --cached --quiet; then
    die "working tree has uncommitted changes; commit or stash them first"
fi

current=$(sed -nE 's/^version[[:space:]]*=[[:space:]]*"([0-9]+\.[0-9]+\.[0-9]+)".*/\1/p' "$FPM")
[[ -n $current ]] || die "could not read version from $FPM"
IFS=. read -r major minor patch <<< "$current"

case $kind in
    major)   new="$((major + 1)).0.0" ;;
    minor)   new="$major.$((minor + 1)).0" ;;
    patch)   new="$major.$minor.$((patch + 1))" ;;
    current) new=$current ;;
    *)       die "unknown bump '$kind' (use major, minor, patch or current)" ;;
esac

tag="v$new"
git rev-parse -q --verify "refs/tags/$tag" >/dev/null && die "tag $tag already exists"

branch=$(git rev-parse --abbrev-ref HEAD)
echo "Branch:  $branch"
echo "Version: $current -> $new"
echo "Tag:     $tag"
read -r -p "Update version in files? [y/N] " answer
[[ $answer == [yY] ]] || { echo "aborted"; exit 1; }

if [[ $new != "$current" ]]; then
    today=$(LC_ALL=C date '+%d %b %Y')
    sed -E "s/^(version[[:space:]]*=[[:space:]]*)\"[^\"]*\"/\1\"$new\"/" "$FPM" > "$FPM.tmp" && mv "$FPM.tmp" "$FPM"
    sed -E -e "s/^(! Version[[:space:]]*:[[:space:]]*).*/\1$new/" \
           -e "s/^(! Date[[:space:]]*:[[:space:]]*).*/\1$today/" "$HEADER" > "$HEADER.tmp" && mv "$HEADER.tmp" "$HEADER"
    sed -E "s/^(version:[[:space:]]*).*/\1$new/" "$FORMAL" > "$FORMAL.tmp" && mv "$FORMAL.tmp" "$FORMAL"
    for f in "${INSTALL_DOCS[@]}"; do
        sed -E "s/(rndgen-fortran\", tag = \")v[0-9]+\.[0-9]+\.[0-9]+\"/\1$tag\"/" "$f" > "$f.tmp" && mv "$f.tmp" "$f"
    done
    echo
    git --no-pager diff -- "${FILES[@]}"
    echo
    read -r -p "Commit these changes as 'chore: release $tag' and create tag $tag? [y/N] " answer
    if [[ $answer != [yY] ]]; then
        echo "not committed; files left modified. To discard:"
        echo "    git checkout -- ${FILES[*]}"
        exit 1
    fi
    git add "${FILES[@]}"
    git commit -m "chore: release $tag"
else
    read -r -p "Create tag $tag on $(git rev-parse --short HEAD)? [y/N] " answer
    [[ $answer == [yY] ]] || { echo "aborted"; exit 1; }
fi

git tag -a "$tag" -m "Release $tag"

echo
echo "Created tag $tag locally. To publish:"
echo "    git push origin $branch && git push origin $tag"
echo "To undo (before pushing):"
echo "    git tag -d $tag$([[ $new != "$current" ]] && echo " && git reset --hard HEAD~1")"
