#!/bin/sh
# Compile script to compile against Elysion library
# Works for Mac OS X and Linux



FPC_BIN=`which fpc`

BIN_FOLDER="../bin"
LIB_FOLDER="../lib"
RES_FOLDER="../resources"
SRC_FOLDER="../source"

# Info.plist constants
BUNDLE_REGION = "English"
BUNDLE_ICON = "logo.icns"
BUNDLE_IDENT = "com.mycompanyname"
BUNDLE_SIGNATURE = "????"
BUNDLE_VERSION = "1.0"


SRC_MAIN="main.lpr"
BIN_MAIN=


CONFIG_FILE="@config.cfg"

EXEC_NAME="myapp"
APP_NAME="My App Title"


cd ${SRC_FOLDER}

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
  SDL_PATH = ""
  SDL_MIXER_PATH = ""
  SDL_TTF_PATH = ""
  SDL_NET_PATH = ""
  
  if [ -f /Library/Frameworks/SDL.framework ] SDL_PATH = "/Library/Frameworks/SDL.framework"
  if [ -f /]
  
  

  FPC_BIN=`which ppc386`

  # Compiling Intel x86 binary
  ${FPC_BIN} ${CONFIG_FILE} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv ${BIN_FOLDER}/main ${BIN_FOLDER}/main-intel_x86
  rm ${BIN_FOLDER}/link.res
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  rm ${SRC_HEADER}/*.o ${SRC_HEADER}/*.ppu ${SRC_HEADER}/*.dcu ${SRC_HEADER}/*.a
  
  FPC_BIN=`which ppcx64`
  
  # Compiling Intel x64 binary
  ${FPC_BIN} ${CONFIG_FILE} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv ${BIN_FOLDER}/main ${BIN_FOLDER}/main-intel_x64
  rm ${BIN_FOLDER}/link.res
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  rm ${SRC_HEADER}/*.o ${SRC_HEADER}/*.ppu ${SRC_HEADER}/*.dcu ${SRC_HEADER}/*.a
  
  FPC_BIN=`which ppcppc`
  
  # Compiling PowerPC binary
  ${FPC_BIN} ${CONFIG_FILE} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv ${BIN_FOLDER}/main ${BIN_FOLDER}/main-ppc
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  
  
  # Creating universal binary
  if [ -f ${BIN_FOLDER}/main-intel_x86 ] &&  [ -f ${BIN_FOLDER}/main-ppc ]
  then
    strip ${BIN_FOLDER}/main-intel_x86
    strip ${BIN_FOLDER}/main-intel_x64
    strip ${BIN_FOLDER}/main-ppc

    lipo -create ${BIN_FOLDER}/main-intel_x86 ${BIN_FOLDER}/main-intel_x64 ${BIN_FOLDER}/main-ppc -output ${BIN_FOLDER}/${EXEC_NAME}
    rm -rf ${BIN_FOLDER}/main-intel_x86
    rm -rf ${BIN_FOLDER}/main-intel_x64
    rm -rf ${BIN_FOLDER}/main-ppc
  fi
  
  if [ -d "${BIN_FOLDER}/${APP_NAME}.app" ] 
  then
    echo "  ... Removing old application"
	rm -rf "${BIN_FOLDER}/${APP_NAME}.app"
  fi

  echo "  ... Creating Application Bundle"

  mkdir "${BIN_FOLDER}/${APP_NAME}.app"
  mkdir "${BIN_FOLDER}/${APP_NAME}.app/Contents"
  mkdir "${BIN_FOLDER}/${APP_NAME}.app/Contents/MacOS"
  mkdir "${BIN_FOLDER}/${APP_NAME}.app/Contents/Resources"
  mkdir "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks"
  
  cp -R  ${RES_FOLDER} "${BIN_FOLDER}/${APP_NAME}.app/Contents/Resources/"
  
  # Copy frameworks from System
  cp -R /Library/Frameworks/SDL.framework "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  cp -R /Library/Frameworks/SDL_mixer.framework "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  cp -R /Library/Frameworks/SDL_ttf.framework "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  
	mv "${BIN_FOLDER}/${EXEC_NAME}" "${BIN_FOLDER}/${APP_NAME}.app/Contents/MacOS/" 
	
	echo "<?xml version='1.0' encoding='UTF-8'?>\
	<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
	<plist version=\"1.0\">\
	<dict>\
	        <key>CFBundleDevelopmentRegion</key>\
	        <string>${BUNDLE_REGION}</string>\
	        <key>CFBundleExecutable</key>\
	        <string>${EXEC_NAME}</string>\
	        <key>CFBundleIconFile</key>\
	        <string>${BUNDLE_ICON}</string>\
	        <key>CFBundleIdentifier</key>\
	        <string>${BUNDLE_IDENT}</string>\
	        <key>CFBundleInfoDictionaryVersion</key>\
	        <string>6.0</string>\
	        <key>CFBundleName</key>\
	        <string>${APP_NAME}</string>\
	        <key>CFBundlePackageType</key>\
	        <string>APPL</string>\
	        <key>CFBundleSignature</key>\
	        <string>${BUNDLE_SIGNATURE}</string>\
	        <key>CFBundleVersion</key>\
	        <string>${BUNDLE_VERSION}</string>\
	        <key>CSResourcesFileMapped</key>\
	        <true/>\
	</dict>\
	</plist>" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/Info.plist"

	echo "APPL${BUNDLE_SIGNATURE}" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/PkgInfo"
	  
else

  ${FPC_BIN} ${CONFIG_FILE} ${SRC_MAIN}
  
  if [ -f "${BIN_FOLDER}/main" ]
  then
    mv "${BIN_FOLDER}/main" "${BIN_FOLDER}/${EXEC_NAME}"
  fi
fi
