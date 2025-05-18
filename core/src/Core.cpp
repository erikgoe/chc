#include "../include/chc/Core.hpp"
#include "../include/chc/Lexer.hpp"
#include "../include/chc/Parser.hpp"
#include "../include/chc/SemanticAnalyzer.hpp"
#include "../include/chc/Mir.hpp"

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
        if ( state.success ) {
            auto root = make_parser( state, lexer );
            if ( state.success ) {
                operator_transformation( state, root );
                basic_semantic_checks( state, root );
            }
            if ( state.success ) {
                auto mir = construct_mir( state, root );
            }
        }

        // Print messages
        for ( auto &m : state.messages ) {
            olog( generate_message_string( m, file_content ) );
        }
    }
}

} // namespace chc
