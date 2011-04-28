cd ..\source
fpc @config.cfg -O3 -Sc -XX main.dpr

del /q ..\bin\*.o ..\bin\*.ppu ..\bin\*.a ..\bin\*.res ..\bin\*.or

pause