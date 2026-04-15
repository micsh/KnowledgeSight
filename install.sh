#!/usr/bin/env bash
# Install knowledge-sight — downloads the latest release binary and adds it to PATH.
# Usage: curl -fsSL https://raw.githubusercontent.com/micsh/KnowledgeSight/main/install.sh | bash

set -euo pipefail

REPO="micsh/KnowledgeSight"
TOOL="knowledge-sight"
INSTALL_DIR="$HOME/.knowledge-sight"

# Detect platform
OS="$(uname -s)"
ARCH="$(uname -m)"
case "$OS-$ARCH" in
  Linux-x86_64)  RID="linux-x64" ; EXT="tar.gz" ;;
  Darwin-arm64)  RID="osx-arm64" ; EXT="tar.gz" ;;
  Darwin-x86_64) RID="osx-arm64" ; EXT="tar.gz" ;; # Rosetta fallback
  *) echo "Unsupported platform: $OS-$ARCH"; exit 1 ;;
esac

# Get latest release tag
TAG=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | grep '"tag_name"' | head -1 | cut -d'"' -f4)
if [ -z "$TAG" ]; then echo "Could not find latest release"; exit 1; fi
echo "Installing $TOOL $TAG for $RID..."

# Download and extract
ARCHIVE="$TOOL-$RID.$EXT"
URL="https://github.com/$REPO/releases/download/$TAG/$ARCHIVE"
mkdir -p "$INSTALL_DIR"
curl -fsSL "$URL" -o "/tmp/$ARCHIVE"
tar -xzf "/tmp/$ARCHIVE" -C "$INSTALL_DIR"
rm -f "/tmp/$ARCHIVE"
chmod +x "$INSTALL_DIR/$TOOL"

# Add to PATH if not already there
SHELL_RC=""
if [ -f "$HOME/.zshrc" ]; then SHELL_RC="$HOME/.zshrc"
elif [ -f "$HOME/.bashrc" ]; then SHELL_RC="$HOME/.bashrc"
fi

if [ -n "$SHELL_RC" ] && ! grep -q "$INSTALL_DIR" "$SHELL_RC" 2>/dev/null; then
  echo "" >> "$SHELL_RC"
  echo "# knowledge-sight" >> "$SHELL_RC"
  echo "export PATH=\"$INSTALL_DIR:\$PATH\"" >> "$SHELL_RC"
  echo "Added $INSTALL_DIR to PATH in $SHELL_RC — restart your shell or run: source $SHELL_RC"
fi

echo "✓ Installed $TOOL $TAG to $INSTALL_DIR"
echo "  Run: $TOOL --help"
