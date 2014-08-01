
IGNORE="_install.sh"

echo "Installing dot files..."

for src in *; do
  dst="~/.$src"


  if echo "$IGNORE" | grep -q "$src"; then
    echo "Ignoring $src"
    continue
  fi

  # Backup old files
  if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]; then
    echo "Backup: $dst -> ${dst}.back"
    mv "$dst" "${dst}.back"
  fi

  # Link new guys into place.
  echo "Linking $src -> $dst"
  ln -s "$src" "$dst"
done
