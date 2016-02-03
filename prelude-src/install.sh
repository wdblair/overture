#!/bin/bash
# Execute in a fresh Prelude directory to avoid garbage to come with.
if test "$1" = "-d";
then
  PREFIX_DIR=$2
else
  PREFIX_DIR=`pwd`
fi

INSTALL_DIR=$PREFIX_DIR/prelude

# Configure
./configure --prefix $INSTALL_DIR

# Build
make all

# Install binaries
make install

# libraries
mkdir -p $INSTALL_DIR/lib
mkdir -p $INSTALL_DIR/lib/prelude
cp lib/prelude/* $INSTALL_DIR/lib/prelude/

# examples
mkdir -p $INSTALL_DIR/Examples
cp Exemples/*.plu $INSTALL_DIR/Examples
mkdir -p $INSTALL_DIR/Examples/sampling_loop
cp Exemples/sampling_loop/* $INSTALL_DIR/Examples/sampling_loop
