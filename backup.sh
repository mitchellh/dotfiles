#!/usr/bin/env bash
#
# This backs up our secrets so that we can put them on another computer.
# These secrets are not (and should not) be put online. We copy them to
# a physical drive and copy them to the target machine.

# Get path to the current script
SCRIPT_NAME="$(basename ${BASH_SOURCE[0]})"
pushd $(dirname ${BASH_SOURCE[0]}) > /dev/null
SCRIPT_DIR=$(pwd)
popd > /dev/null

tar -czvf ${SCRIPT_DIR}/backup.tar.gz \
  -C $HOME \
  --exclude='.gnupg/.#*' \
  --exclude='.gnupg/S.*' \
  --exclude='.gnupg/*.conf' \
  --exclude='.ssh/environment' \
  .ssh/ \
  .gnupg
