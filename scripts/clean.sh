#!/bin/sh

find ../ -name "*.o" -exec rm {} \;
find ../ -name "*.ppu" -exec rm {} \;
find ../ -name "*.dcu" -exec rm {} \;
find ../bin/ -name "*.a" -exec rm {} \;
find ../ -name "*.or" -exec rm {} \;
find ../ -name "*.compiled" -exec rm {} \;
