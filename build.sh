#!/bin/sh
curl -sSL https://dot.net/v1/dotnet-install.sh > dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh -c 8.0 -InstallDir ./dotnet

export PATH="$PWD/dotnet:$PATH"

dotnet --version
dotnet workload install wasm-tools
dotnet publish -c Release -o output src/Web/Web.fsproj