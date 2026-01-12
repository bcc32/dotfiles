#!/bin/sh
set -o errexit -o nounset

cd "$(dirname "$0")"

prefix=$(pwd)

version=2.3.1
filename=stow-$version

curl -fsSL https://ftp.gnu.org/gnu/stow/$filename.tar.gz -o $filename.tar.gz
tar -xzf $filename.tar.gz

(
  cd $filename

  ./configure --prefix="$prefix" --with-pmdir="$prefix/lib"
  make
  make install prefix="$prefix"
)

rm -rf share "$filename" "$filename".tar.gz

# stow's build script inserts this line to add the library directory to @INC at
# runtime, but the absolute path depends on the machine so this is wrong.
# Instead, @INC is modified by stow.sh.
sed -i '/^use lib /d' bin/stow
