#!/bin/bash
export DOTNET_ROOT=$HOME/Toolchains/dotnet
export PATH=$PATH:$HOME/Toolchains/dotnet
cd DotNumerics
#dotnet clean
dotnet build -c Release -o ./bin/
cd ..
cp DotNumerics/bin/DotNumerics.dll VGL/lib/
cd VGL
#dotnet clean
dotnet build -c Release -o ./bin/
cp ./bin/VGL ./