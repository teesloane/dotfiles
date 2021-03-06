#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Install homebrew if it is not installed
which brew 1>&/dev/null
if [ ! "$?" -eq 0 ] ; then
	echo "No Homebrew found! Attempting to install Homebrew. Sip sip sip sip."
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	if [ ! "$?" -eq 0 ] ; then
		echo "Something went wrong. Exiting..." && exit 1
	fi
fi

# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# Core Utils
brew install coreutils


# -----------------------------------------------------------------------------
# The Good Stuff
# Most of what I need is covered by ThoughtBots's Laptop install script:
# >>> https://github.com/thoughtbot/laptop
# This section comprises everything else.
# -----------------------------------------------------------------------------

# Lil utils.
brew install bat                      # A nicer Cat. 
brew install ripgrep                  # Find water fast.      
brew install exa                      # LS replacement.
brew install fzf                      # Nice CTRL-R. And like, other stuff.
brew install mpw                      # Speak Friend and Enter
brew install trash                    # Protect RM.
brew install watch                    # Who's on lookout?
brew install fish                     # Goodbye zsh
brew install ranger                   # Everyone's Favourite Park Ranger

brew install koekeishiya/formulae/skhd
brew install koekeishiya/formulae/yabai #nees additional installation
# UI FOR AN UI
brew cask install visual-studio-code  # Big'un. 
# brew cask install spectacle           # Snappy windows
brew cask install anki                # Ultra Lapsus memoriae
# brew cask install emacs               # DOOM...
# brew cask install anybar              # Menubar dots
# brew cask install google-chrome       # Everything is shiney.
# brew cask install anybar              # Little dots.
# brew cask install firefox             # Fast Little Fox
# brew cask install iterm2              # I...Term...too?
# brew cask install telegram            # Greetings STOP
 brew cask install michaelvillar-timer # Time TIME TIME

# Other
brew tap homebrew/cask-fonts
brew install --cask font-fira-code
brew install --cask font-inconsolata
# Quick look plugins
brew install --cask qlcolorcode qlstephen qlmarkdown quicklook-json qlimagesize qlvideo



brew cleanup
