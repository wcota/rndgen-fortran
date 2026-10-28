#!/usr/bin/env bash
# ==============================================================================
# build_docs.sh - Generate API docs using FORMAL and build VitePress website
# ==============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

echo "==> Resolving formal executable..."
if [ -x "${ROOT_DIR}/.venv/bin/formal" ]; then
    FORMAL_BIN="${ROOT_DIR}/.venv/bin/formal"
elif command -v formal &> /dev/null; then
    FORMAL_BIN="formal"
elif command -v uvx &> /dev/null; then
    FORMAL_BIN="uvx --from formal-ford2vitepress formal"
else
    echo "ERROR: formal not found. Install it with: pipx install formal-ford2vitepress or uv pip install formal-ford2vitepress"
    exit 1
fi

echo "==> Generating Markdown API documentation via FORMAL..."
${FORMAL_BIN} generate --project docs/formal.md --output docs/api --diagrams

echo "==> Building VitePress documentation site..."
if [ -f "${ROOT_DIR}/docs/node_modules/vitepress/bin/vitepress.js" ]; then
    node "${ROOT_DIR}/docs/node_modules/vitepress/bin/vitepress.js" build docs
elif command -v npx &> /dev/null; then
    npx vitepress build docs
else
    echo "WARNING: node/vitepress not found to build HTML. Markdown API docs are ready in docs/api/."
    exit 0
fi

echo "==> Documentation successfully generated in docs/.vitepress/dist!"
