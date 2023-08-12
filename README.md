# paracletus

the fourth version of paracletus

## Prerequisites

pre-requisites for building include [vulkan](https://vulkan.lunarg.org/sdk/home), [glfw](https://www.glfw.org/download.html),
and [freetype2](https://download.savannah.gnu.org/releases/freetype/).  all of these should be in
most package managers.  there are other libraries
such as libgmp3, libtinfo, and libx* stuff
that may be required.  works on windows, linux,
and mac; probably anything with GHC and vulkan.
if vulkan acts up and displays weird artifacts,
its because vulkan ships configured to override
your validation layers, run vkconfig and click
the box "fully controlled by vulkan application".
on windows the required LUNARG vulkan validation
layer may need to be downloaded from the sdk.

## Building

to build use `cabal new-build paracletus` to
download dependencies and compile.
`glslangValidator` must be in your path or
passed as a cabal argument as `cabal
--extra-lib-dirs=...\glslangValidator.exe
new-build opentowns`.

## Usage

use `+RTS -s -M[x]m -N[n]` to run with x megabytes
and n cores.

performance is increased drastically by disabling
the development flag, but compilation takes
forever.

to create profiling files, use `cabal new-build
--enable-library-profiling --enable-profiling
paracletus` to build, and `-prof -fprof-auto` in the
ghc-options of the cabal file.  run with flags
`+RTS -s -p -hy -M[x]m -N[n]`


## Development

the code is written with unicode, some extra
unicode is defined in `src/UPrelude`. which is
imported in every file.

feel free to open any issues for any reasons, pull
requests, forks, and stars are all appreciated.
most of the code is original, the files in
`/src/Vulk/` and `/src/Prog.hs`
are mostly from the [vulkan-api example](https://github.com/achirkin/vulkan/tree/master/vulkan-triangles).

the code structure is devided up into many parts
arbitrarily named:

* vulk - the graphics engine itself.
* prog - the continuation monad and main thread functionality
* luau - the lua interpreter and code
to generate game objects from the lua scripts.
* sign - a collection of threading functions to
pass data around between the various components.
* load - a thread that calculates verticies and dynamic data
* elem - code for abstract window elements
