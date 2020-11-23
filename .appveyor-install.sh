#!/usr/bin/env sh

# .appveyor-install.sh installs opam and OCaml on an Appveyor Win64 image, and
# then runs ./configure.  This should only be called by .appveyor.yml.  The
# following resources were helpful:
# - https://fdopen.github.io/opam-repository-mingw/
# - https://github.com/ocaml/ocaml-ci-scripts

export CYGWIN='winsymlinks:native'

# The Windows PATH is huge and slows down Cygwin; shorten our PATH:
export PATH=/usr/local/bin:/usr/bin:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0

if command -V opam; then
    # If opam is installed, skip install.  Should only be true case when cached.
    echo ===== opam found
else
    # Install opam:
    echo ===== installing opam
    cd /tmp
    wget --quiet https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz
    tar -xf opam64.tar.xz
    opam64/install.sh --quiet
    cd $APPVEYOR_BUILD_FOLDER
    echo ===== opam installed
fi

# Attempt to load environment:
echo ===== Can we load the environment?
eval $(ocaml-env cygwin)
eval $(opam config env)

if command -V ocamlc; then
    # If ocamlc is installed, skip install.  Should only be true case when cached.
    echo ===== OCaml found
else
    # Install OCaml:
    echo ===== no - installing OCaml
    opam init default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.$OCAML" -a --disable-sandboxing --enable-shell-hook
    opam install depext
    eval $(ocaml-env cygwin)
    eval $(opam config env)
    echo ===== OCaml installed
fi


# Install faial-bin dependencies:
echo ===== running configure.sh
bash $APPVEYOR_BUILD_FOLDER/configure.sh

# Debugging information:
echo ===== printing opam and OCaml versions
opam --version
ocamlc --version
echo ===== .appveyor.sh finished

# Cache fix: opam-switch/sources breaks crc32 check, causing cache failure
rm -rf /home/appveyor/.opam/ocaml-variants.*/.opam-switch/sources
