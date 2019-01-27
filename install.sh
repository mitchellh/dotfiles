#!/usr/bin/env bash
#
# This installation is destructive, as it removes exisitng files/directories.
# Use at your own risk.

# Get path to the current script
SCRIPT_NAME="$(basename ${BASH_SOURCE[0]})"
pushd $(dirname ${BASH_SOURCE[0]}) > /dev/null
SCRIPT_DIR=$(pwd)
popd > /dev/null

UNAME=$(uname)

for path in $SCRIPT_DIR/*; do
  name=$(basename $path)
  if [ ! $name == "README.md" -a ! $name == "install.sh" ]; then
    target="$name"
    if [ ! $name == "nvim" ]; then
        target=".$name"
    fi
    target="$HOME/$target"

    if [ -h $target ]; then
      rm $target
    elif [ -d $target ]; then
      rm -rf $target
    fi

    case $UNAME in
        CYGWIN* | MINGW32*)
            cp -R "$PWD/$name" "$target"
            echo "Copied $PWD/$name to $target."
            ;;
        *)
            ln -s $path "$target"
            echo "Linked $path to $target."
            ;;
    esac
  fi
done
