# Graphoscope
A pragmatic approach to network science.

## Prerequisites

### Users 

- .NET 6.0 or higher (https://dotnet.microsoft.com/download/dotnet)

### Developers

- .NET 6.0 or higher (https://dotnet.microsoft.com/download/dotnet)
- An IDE suited to work in the .NET and F# ecosystem, here are some suggestions:
	- Visual Studio 2022 (https://visualstudio.microsoft.com/vs/)
	- Visual Studio Code (https://code.visualstudio.com/) with the Ionide plugin (https://ionide.io/)
	- JetBrains Rider (https://www.jetbrains.com/rider/)

## repo structure

- The `src` folder contains 1 subfolder per project. 

- The `tests` folder contains 1 subfolder per test project.

- The `docs` folder contains documentation snippets (`.fsx` and `.md` files)

- The `build` folder contains the buildproject which contains all build tasks.

## Develop

in general, `dotnet tool restore` must be run in the repo root once after cloning to install the dotnet tools used in this repo

The `build.cmd` and `build.sh` scripts are shorthand scripts that execute the `/build/Build.fsproj` build project. This build project contains various tasks.

### Build

In the repo root, run the following command based on your OS:

Linux/MacOS:

```bash
./build.sh
```

Windows:

```powershell
./build.cmd
```

### Test

#### via IDE

If your IDE supports TestAdapters, open the solution and run the tests from Test Explorer.

#### via CLI

In the repo root, run the following command based on your OS:

Linux/MacOS:

```bash
./build.sh runTests
```

Windows:

```powershell
./build.cmd runTests
```