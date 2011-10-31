#!/bin/sh

# Builds libSDLmain which is needed to compile against SDL 1.2 on Mac OS X
gcc -fno-stack-protector -arch i386 -c -o ../lib/MacOSX/SDLMain-x86.o ../lib/MacOSX/SDLMain.m -I /Library/Frameworks/SDL.framework/Headers
gcc -fno-stack-protector -arch x86_64 -c -o ../lib/MacOSX/SDLMain-x64.o ../lib/MacOSX/SDLMain.m -I /Library/Frameworks/SDL.framework/Headers
gcc -fno-stack-protector -arch ppc -c -o ../lib/MacOSX/SDLMain-ppc.o ../lib/MacOSX/SDLMain.m -I /Library/Frameworks/SDL.framework/Headers

lipo -create ../lib/MacOSX/SDLMain-x86.o ../lib/MacOSX/SDLMain-x64.o ../lib/MacOSX/SDLMain-ppc.o -output ../lib/MacOSX/SDLMain.o
ar r ../lib/MacOSX/libSDLMain.a ../lib/MacOSX/SDLMain.o
ranlib ../lib/MacOSX/libSDLMain.a

