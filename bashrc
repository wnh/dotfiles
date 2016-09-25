
for f in ~/.bashrc.d/*; do
  source "$f"
done



export NVM_DIR="/Users/wharding/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
