cd ..\source
fpc @config.cfg -O3 -Sc -XX main.lpr

del /q ..\bin\*.o ..\bin\*.ppu ..\bin\*.a ..\bin\*.res ..\bin\*.or

pause
