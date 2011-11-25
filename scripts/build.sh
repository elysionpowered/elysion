#!/bin/sh
# Compile script to compile against Elysion library
# Works for Mac OS X and Linux

FPC_BIN=`which fpc`
INSTANT_BIN=`which instantfpc`

BIN_FOLDER="../bin"
LIB_FOLDER="../lib"
RES_FOLDER="../resources"
SRC_FOLDER="../source"

SRC_MAIN="main.lpr"
BIN_MAIN=`basename {SRC_NAME} .lpr`
EXEC_NAME="wolkenwelt"

CONFIG_FILE="@config.cfg"

# Mac OS X
if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
  ${INSTANT_BIN} "./build-mac.pas"
else
  # Linux
  cd ${SRC_FOLDER}
  
  ${FPC_BIN} ${CONFIG_FILE} -XX -CX -O3 -Sc -Si ${SRC_MAIN}
  
  if [ -f "${BIN_FOLDER}/${BIN_MAIN}" ]
  then
    mv "${BIN_FOLDER}/${BIN_MAIN}" "${BIN_FOLDER}/${EXEC_NAME}"
  fi

fi
