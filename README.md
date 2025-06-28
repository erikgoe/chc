# Compiler for lecture "Compiler Design"
My implementation of a compiler for a C0-like language, called L3. This compiler was created as submission for the lab exercise of the teaching course "Compiler Design" by André Platzer.

## Features
* Well... it compiles the language and outputs x86-64 assembly.
* Dependency free (except for c++17 standard library and gcc assembler). Yes, everything's written from scratch.
* What the language supports so far: arithmetic & variables, control flow, functions, and simple I/O.
* Register allocator
* "Unorthodox" implementation of some features like parsing.
* Many yet unused opportunities for optimizations ;)

## Current state
In early development. Everything can potentially break. Code is messy at various places.

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
