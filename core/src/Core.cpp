#include "../include/chc/Core.hpp"
#include "../include/chc/Lexer.hpp"

namespace chc {

void Core::compile() {
    for ( auto &file_path : conf.in_files ) {
        // Read file
        std::ifstream stream( file_path );
        auto file_content =
            String( ( std::istreambuf_iterator<char>( stream ) ),
                    ( std::istreambuf_iterator<char>() ) );

        // Compile TODO
        auto lexer = make_lexer( state, file_content );

        // Print messages
        for ( auto &m : state.messages ) {
            olog( generate_message_string( m, file_content ) );
        }
    }
}

} // namespace chc
