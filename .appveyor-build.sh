#!/usr/bin/env sh

# .appveyor-build.sh runs faial-bin's make on the Appveyor Win64 image.  This
# should only be called by .appveyor.yml.

export CYGWIN='winsymlinks:native'

# The Windows PATH is huge and slows down Cygwin; shorten our PATH:
export PATH=/usr/local/bin:/usr/bin:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0

# Set up environment:
eval $(ocaml-env cygwin)
eval $(opam config env)

# Run build:
echo ===== starting build of faial-bin!
cd $APPVEYOR_BUILD_FOLDER
echo ===== make clean:
make clean
echo ===== make:
make
echo ===== finished make!
#./faial-bin --help
