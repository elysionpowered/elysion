#!/bin/sh
# Compile script to compile against Elysion library
# Works for Mac OS X and Linux



FPC_BIN=`which fpc`
BIN_FOLDER="../bin"
SRC_MAIN="main.lpr"
LIB_FOLDER="../lib"
EXEC_NAME="mrfire"
APP_NAME="Mr Fire Takes A Walk"


cd ../source
if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then

  FPC_BIN=`which ppc386`

  # Compiling Intel binary
  ${FPC_BIN} @config.cfg -k-L/usr/X11R6/lib ${SRC_MAIN}
  mv ${BIN_FOLDER}/main ${BIN_FOLDER}/main-intel
  rm ${BIN_FOLDER}/link.res
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  rm ${SRC_HEADER}/*.o ${SRC_HEADER}/*.ppu ${SRC_HEADER}/*.dcu ${SRC_HEADER}/*.a
  
  
  FPC_BIN=`which ppcppc`
  
  # Compiling PowerPC binary
  ${FPC_BIN} @config.cfg -k"-L${LIB_FOLDER}/frameworks -L/usr/X11R6/lib" ${SRC_MAIN} -XR/Developer/SDKs/MacOSX10.5.sdk/ -k-macosx_version_min -k10.5
  mv ${BIN_FOLDER}/main ${BIN_FOLDER}/main-ppc
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  
  
  # Creating universal binary
  if [ -f ${BIN_FOLDER}/main-intel ] &&  [ -f ${BIN_FOLDER}/main-ppc ]
  then
    strip ${BIN_FOLDER}/main-intel
    strip ${BIN_FOLDER}/main-ppc

    lipo -create ${BIN_FOLDER}/main-intel ${BIN_FOLDER}/main-ppc -output ${BIN_FOLDER}/${EXEC_NAME}
    rm -rf ${BIN_FOLDER}/main-intel
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
  
  cp -R  ../resources/ "${BIN_FOLDER}/${APP_NAME}.app/Contents/Resources/"
  
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
	        <string>English</string>\
	        <key>CFBundleExecutable</key>\
	        <string>${EXEC_NAME}</string>\
	        <key>CFBundleIconFile</key>\
	        <string>logo.icns</string>\
	        <key>CFBundleIdentifier</key>\
	        <string>com.ponyprincessstudios</string>\
	        <key>CFBundleInfoDictionaryVersion</key>\
	        <string>6.0</string>\
	        <key>CFBundleName</key>\
	        <string>${APP_NAME}</string>\
	        <key>CFBundlePackageType</key>\
	        <string>APPL</string>\
	        <key>CFBundleSignature</key>\
	        <string>MRTW</string>\
	        <key>CFBundleVersion</key>\
	        <string>1.0</string>\
	        <key>CSResourcesFileMapped</key>\
	        <true/>\
	</dict>\
	</plist>" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/Info.plist"

	echo "APPLFDRS" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/PkgInfo"
	  
else
  if [ -f /usr/lib64 ]
  then
    ${FPC_BIN} @config.cfg ${SRC_MAIN}
    rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  else
    ${FPC_BIN} @config.cfg ${SRC_MAIN}
    rm ${BIN_FOLDER}/*.o  ${BIN_FOLDER}/*.ppu
  fi
  
  if [ -f "${BIN_FOLDER}/main" ]
  then
    mv "${BIN_FOLDER}/main" "${BIN_FOLDER}/${EXEC_NAME}"
  fi
fi
