#!/bin/bash
cd DotNumerics
#dotnet clean
dotnet build -c Release -o ./bin/
cd ..
cp DotNumerics/bin/DotNumerics.dll VGL/lib/
cp DotNumerics/bin/DotNumerics.dll Viewer/lib/
cd VGL
#dotnet clean
dotnet build -c Release -o ./bin/
cp ./bin/VGL ./
cp ./bin/VGL.dll ../Viewer/lib/
cd ..
cd Viewer
dotnet build -c Release -o ./bin/
cp src/Shaders/* bin/
