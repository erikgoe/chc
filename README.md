# Compiler for lecture "Compiler Design"
My implementation of a compiler for a C-like language, called L4 (rather similar to C0). This compiler was created as submission for a lab exercise of the teaching course "Compiler Design" by Prof. André Platzer at KIT.

## Features
* Well... it compiles the language and outputs x86-64 assembly.
* Dependency free (except for c++17 standard library and gcc assembler). Yes, everything's written from scratch.
* The language supports so far: arithmetic & variables, control flow, functions, structs, pointers, arrays, and simple I/O.
* Register allocator
* "Unorthodox" implementation of some features like parsing.
* Many yet unused opportunities for optimizations ;)

## Current state
The lab ended, so I thing it's done. There are still some TODOs remaining and things can break at unexpected places, but it has been tested pretty well (with an external test case corpus). Some language features (like strings...) are not implemented, simply because the specification does not define them. Also the code might be seen as being messy in some places, but I tried to keep it as simple as possible.

## Building
### Linux
    git clone https://github.com/erikgoe/chc
    mkdir -p chc/build && cd $_
    cmake -DCMAKE_BUILD_TYPE=Debug ..
    cmake --build . --config Debug -j 8

Alternatively there is a simple script for unity builds

    # cd chc
    ./build.sh

## Usage
First write a simple program like the following into `main.c`:

    int main() {
        int some_value = 0x34;

        print( some_value );
        print( magic( 7 ) );
        print( 0xA ); // Just a newline

        return 0;
    }

    int magic( int s ) {
        return ( 18 + s ) * 2;
    }

Then compile it like:

    chc main.c app

And finally run it:

    ./app

