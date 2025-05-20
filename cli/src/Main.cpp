#define CHC_IMPL
#include "chc.hpp"

using namespace chc;

int main( int argc, char** argv ) {
    if ( argc < 3 ) {
        log( "No input or output file specified!" );
        return -1;
    }

    CompilerConf conf;
    for ( int i = 1; i < argc - 1; i++ )
        conf.in_files.push_back( String( argv[i] ) );
    conf.out_file = String( argv[argc - 1] );

    // Stopwatch
#ifndef NDEBUG
    auto begin_clock = std::chrono::steady_clock::now();
#endif

    // The compiler core
    Core core( conf );
    core.compile();

// Calculate time
#ifndef NDEBUG
    auto end_clock = std::chrono::steady_clock::now();
    f32 seconds =
        static_cast<f32>( std::chrono::duration_cast<std::chrono::milliseconds>(
                              end_clock - begin_clock )
                              .count() ) /
        1000.f;
    log( "Elapsed time: " + to_string( seconds ) + " sec" );
#endif

    return core.get_ret_code();
}

void olog( const String& str ) {
    std::cout << str << std::endl;
}

void log( const String& str ) {
#ifndef NDEBUG
    std::cout << str << std::endl;
#endif
}
