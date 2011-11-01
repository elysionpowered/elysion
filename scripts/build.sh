#!/bin/sh
# Compile script to compile against Elysion library
# Works for Mac OS X and Linux



FPC_BIN=`which fpc`

BIN_FOLDER="../bin"
LIB_FOLDER="../lib"
RES_FOLDER="../resources"
SRC_FOLDER="../source"

# Info.plist constants
BUNDLE_REGION="English"
BUNDLE_ICON="logo.icns"
BUNDLE_IDENT="com.mycompanyname"
BUNDLE_SIGNATURE="????"
BUNDLE_VERSION="1.0"


SRC_MAIN="main.lpr"
BIN_MAIN=`basename ${SRC_MAIN} .lpr`


CONFIG_FILE="@config.cfg"

EXEC_NAME="myapp"
APP_NAME="My App Title"


cd ${SRC_FOLDER}

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
  SDL_PATH=
  SDL_MIXER_PATH=
  SDL_TTF_PATH=
  SDL_NET_PATH=

  DEV_LINK_PPC=
  DEV_LINK_INTEL32=
  DEV_LINK_INTEL64=

  MIN_PPC=
  MIN_INTEL32=
  MIN_INTEL64=


  
  if [ -d /Library/Frameworks/SDL.framework ]
  then
    SDL_PATH="/Library/Frameworks/SDL.framework"
  elif [ -d ~/Library/Frameworks/SDL.framework ]
  then
    SDL_PATH="~/Library/Frameworks/SDL.framework"
  else
    echo "SDL not detected. Please check: https://github.com/freezedev/elysion/wiki/Setting-up-our-development-environment"
    exit 1
  fi

  if [ -d /Library/Frameworks/SDL_mixer.framework ]
  then
    SDL_MIXER_PATH="/Library/Frameworks/SDL_mixer.framework"
  elif [ -d ~/Library/Frameworks/SDL_mixer.framework ]
  then
    SDL_MIXER_PATH="~/Library/Frameworks/SDL_mixer.framework"
  fi

  if [ -d /Library/Frameworks/SDL_ttf.framework ]
  then
    SDL_TTF_PATH="/Library/Frameworks/SDL_ttf.framework"
  elif [ -d ~/Library/Frameworks/SDL_ttf.framework ]
  then
    SDL_TTF_PATH="~/Library/Frameworks/SDL_ttf.framework"
  fi

  if [ -d /Library/Frameworks/SDL_net.framework ]
  then
    SDL_NET_PATH="/Library/Frameworks/SDL_net.framework"
  elif [ -d ~/Library/Frameworks/SDL_net.framework ]
  then
    SDL_NET_PATH="~/Library/Frameworks/SDL_net.framework"
  fi

  if [ [ -d /Developer/SDKs/MacOSX10.7.sdk ] || [ -d /Developer/SDKs/MacOSX10.6.sdk ] || [ -d /Developer/SDKs/MacOSX10.5.sdk ] || [ -d /Developer/SDKs/MacOSX10.4u.sdk ] ]
  then
    echo "At least one Mac OS X SDK found"
  else
    echo "XCode does not seem be installed. Please install XCode."
    exit 1
  fi

  if [ -d "/Developer/SDKs/MacOSX10.7.sdk" ]
  then
    DEV_LINK_PPC=
    DEV_LINK_INTEL32="/Developer/SDKs/MacOSX10.7.sdk"
    DEV_LINK_INTEL64="/Developer/SDKs/MacOSX10.7.sdk"

    MIN_INTEL32="10.7.0"
    MIN_INTEL64="10.7.0"
  fi

  if [ -d "/Developer/SDKs/MacOSX10.6.sdk" ]
  then
    DEV_LINK_PPC=
    DEV_LINK_INTEL32="/Developer/SDKs/MacOSX10.6.sdk"
    DEV_LINK_INTEL64="/Developer/SDKs/MacOSX10.6.sdk"

    MIN_INTEL32="10.6.0"
    MIN_INTEL64="10.6.0"
  fi

  if [ -d "/Developer/SDKs/MacOSX10.5.sdk" ]
  then
    DEV_LINK_PPC="/Developer/SDKs/MacOSX10.5.sdk"
    DEV_LINK_INTEL32="/Developer/SDKs/MacOSX10.5.sdk"

    MIN_INTEL32="10.5.0"
  fi

  if [ -d "/Developer/SDKs/MacOSX10.4u.sdk" ]
  then
    DEV_LINK_PPC="/Developer/SDKs/MacOSX10.4u.sdk"
	DEV_LINK_INTEL32="/Developer/SDKs/MacOSX10.4u.sdk"

    MIN_PPC="10.4.0"
    MIN_INTEL32="10.4.0"
  fi


  FPC_BIN=`which ppc386`

  # Compiling Intel x86 binary
  ${FPC_BIN} ${CONFIG_FILE} -XR${DEV_LINK_INTEL32} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv "${BIN_FOLDER}/${BIN_MAIN}" "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  
  FPC_BIN=`which ppcx64`
  
  # Compiling Intel x64 binary
  ${FPC_BIN} ${CONFIG_FILE} -XR${DEV_LINK_INTEL64} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv "${BIN_FOLDER}/${BIN_MAIN}" "${BIN_FOLDER}/${BIN_MAIN}-intel_x64"
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  
  FPC_BIN=`which ppcppc`
  
  # Compiling PowerPC binary
  ${FPC_BIN} ${CONFIG_FILE} -XR${DEV_LINK_PPC} -k"-L${LIB_FOLDER}/MacOSX -L/usr/X11R6/lib" ${SRC_MAIN}
  mv "${BIN_FOLDER}/${BIN_MAIN}" "${BIN_FOLDER}/${BIN_MAIN}-ppc"
  rm ${BIN_FOLDER}/*.o ${BIN_FOLDER}/*.ppu
  
  
  # Creating universal binary

  # Strip executables
  if [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" ]
  then
    strip "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"
  fi

  if [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x64" ]
  then
    strip "${BIN_FOLDER}/${BIN_MAIN}-intel_x64"
  fi

  if [ -f "${BIN_FOLDER}/${BIN_MAIN}-ppc" ]
  then
    strip "${BIN_FOLDER}/${BIN_MAIN}-ppc"
  fi

  # All three compilers are here... yeah, universal binary de luxe (Intel 32, Intel 64 + PowerPC 32)
  if [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" ] && [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x64" ] && [ -f "${BIN_FOLDER}/${BIN_MAIN}-ppc" ]
  then
    lipo -create "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" "${BIN_FOLDER}/${BIN_MAIN}-intel_x64" "${BIN_FOLDER}/${BIN_MAIN}-ppc" -output "${BIN_FOLDER}/${EXEC_NAME}"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-intel_x64"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-ppc"

  # PowerPC 32 + Intel 32
  elif [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" ] && [ -f "${BIN_FOLDER}/${BIN_MAIN}-ppc" ]
  then
    lipo -create "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" "${BIN_FOLDER}/${BIN_MAIN}-ppc" -output "${BIN_FOLDER}/${EXEC_NAME}"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-ppc"

  # Intel 32 + Intel 64
  elif [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" ] && [ -f "${BIN_FOLDER}/${BIN_MAIN}-intel_x64" ]
  then
    lipo -create "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" "${BIN_FOLDER}/${BIN_MAIN}-intel_x64" -output "${BIN_FOLDER}/${EXEC_NAME}"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"
    rm -rf "${BIN_FOLDER}/${BIN_MAIN}-intel_x64"

  else
    strip "${BIN_FOLDER}/${BIN_MAIN}-intel_x86"

    mv "${BIN_FOLDER}/${BIN_MAIN}-intel_x86" "${BIN_FOLDER}/${EXEC_NAME}"

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
  
  cp -R  "${RES_FOLDER}/" "${BIN_FOLDER}/${APP_NAME}.app/Contents/Resources/"
  
  # Copy frameworks from System
  cp -R "${SDL_PATH}" "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  cp -R "${SDL_MIXER_PATH}" "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  cp -R "${SDL_TTF_PATH}" "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  cp -R "${SDL_NET_PATH}" "${BIN_FOLDER}/${APP_NAME}.app/Contents/Frameworks/"
  
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
            <key>LSMinimumSystemVersionByArchitecture</key>\
 	        <dict>\
 		        <key>x86_64</key>\
 		        <string>${MIN_INTEL64}</string>\
 		        <key>i386</key>\
 		        <string>${MIN_INTEL32}</string>\
 		        <key>ppc</key>\
 		        <string>${MIN_PPC}</string>\
 	        </dict>\
	</dict>\
	</plist>" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/Info.plist"

	echo "APPL${BUNDLE_SIGNATURE}" >> "${BIN_FOLDER}/${APP_NAME}.app/Contents/PkgInfo"
	  
else

  ${FPC_BIN} ${CONFIG_FILE} ${SRC_MAIN}
  
  if [ -f "${BIN_FOLDER}/${BIN_MAIN}" ]
  then
    mv "${BIN_FOLDER}/${BIN_MAIN}" "${BIN_FOLDER}/${EXEC_NAME}"
  fi

fi
