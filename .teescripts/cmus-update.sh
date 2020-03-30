#!/usr/bin/env bash

cmus-remote -C clear
cmus-remote -C "add ~/Music/Music"
cmus-remote -C "add /Volumes/tees/Music"
cmus-remote -C "update-cache -f"

# Bind the above to  cmus using the following:
# navigate to CMUS, enter ex mode and run:
# bind -f common u shell ~/dotfiles/.teescripts/cmus-update.sh
