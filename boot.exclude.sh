#!/bin/sh

# Update submodules etc etc
update () {
	echo "updating submodules..."
  	git submodule update --init --recursive
  	echo "done!"
}


link () {
	echo "This utility will symlink the files in this repo to the home directory"
	echo "Proceed? (y/n)"
	read resp
	# TODO - regex here?
	if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		for file in $( ls -A | grep -vE '\.exclude*|\.git$|\.gitignore|.*.md' ) ; do
			ln -sv "$PWD/$file" "$HOME"
		done
		# TODO: source files here?
		echo "Symlinking complete"
	else
		echo "Symlinking cancelled by user"
		return 1
	fi
}

install_tools () {
	if [ $( echo "$OSTYPE" | grep 'darwin' ) ] ; then
		echo "This utility will install useful utilities using Homebrew"
		echo "Proceed? (y/n)"
		read resp
		# TODO - regex here?
		if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
			echo "Installing useful stuff using brew. This may take a while..."
			sh brew.exclude.sh
		else
			echo "Brew installation cancelled by user"
		fi
	else
		echo "Skipping installations using Homebrew because MacOS was not detected..."
	fi
}


 
if [ "$1" == "update" ] 
then
	update
elif [ "$1" == "link" ] 
then
	link
elif [ "$1" == "install" ]
then
	install_tools
elif [ "$1" == "all" ]
then
	update
	link
	install_tools
else
	echo "Gimme a command: 'update', 'link', 'install', or 'all' "
fi