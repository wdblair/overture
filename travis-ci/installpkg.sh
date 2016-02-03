#!/usr/bin/env sh

######

cd ${HOME}

###### for OSX ######

is_osx=`expr "${TRAVIS_OS_NAME}" : "osx"`;

# echo "is_osx = ${is_osx}"

if
  expr ${is_osx} > 0
then
#
  echo "is_osx = ${is_osx}"
#
# export CC=gcc-4.8
# export GCC=gcc-4.8
#
  brew install gmp
  brew install bdw-gc
  brew install pcre
  brew install glib
  brew install cairo
  brew install gtk+3
  brew install libev
  brew install json-c
  brew install jansson
  brew install caskroom/cask/brew-cask
  brew cask install xquartz
#
  export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig:$PKG_CONFIG_PATH
#
fi

###### for LINUX ######

is_linux=`expr "${TRAVIS_OS_NAME}" : "linux"`;

# echo "is_linux = ${is_linux}"

if
  expr ${is_linux} > 0
then
  curl \
      https://s3.amazonaws.com/json-c_releases/releases/json-c-0.12.tar.gz \
      | tar -xz
  cd ${HOME}
#
fi

######

exit 0

###### end of [installpkg.sh] ######
