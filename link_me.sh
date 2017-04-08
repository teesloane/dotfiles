#!/bin/bash

# Variables
new_home=~


# Makes sym links from dot files repo to a home directory. 
echo "Creating a .config folder if it doesn't exist already"
mkdir -p $new_home/.config

echo "symlinking nvim folder to home config"
ln -s $(pwd)/.config/nvim  $new_home/.config/
echo "symlinked $(pwd)/.config/nvim to  $new_home/.config/"

echo "symlinking .tmux.conf"
ln -s $(pwd)/.tmux.conf  $new_home/
