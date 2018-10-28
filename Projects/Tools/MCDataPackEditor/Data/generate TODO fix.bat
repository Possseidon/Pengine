@echo off

echo Generating...
java -cp %1 net.minecraft.data.Main --all

echo Moving files into place...
move generated/data data
move generated/reports reports

echo Removing the rest...
rd /S/Q generated
rd /S/Q logs

echo Done!
pause
