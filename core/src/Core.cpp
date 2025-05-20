#include "../include/chc/Core.hpp"
#include "../include/chc/Lexer.hpp"
#include "../include/chc/Parser.hpp"
#include "../include/chc/SemanticAnalyzer.hpp"
#include "../include/chc/Mir.hpp"
#include "../include/chc/Codegen.hpp"

namespace chc {

void Core::compile() {
    String assembly;
    for ( auto &file_path : conf.in_files ) {
        // Read file
        std::ifstream stream( file_path );
        auto file_content =
            String( ( std::istreambuf_iterator<char>( stream ) ),
                    ( std::istreambuf_iterator<char>() ) );

        // Compile
        auto compile_pass = [&]() {
            auto lexer = make_lexer( state, file_content );
            if ( !state.success )
                return;

            auto root = make_parser( state, lexer );
            if ( !state.success )
                return;

            operator_transformation( state, root );
            basic_semantic_checks( state, root );
            if ( !state.success )
                return;

            auto mir = construct_mir( state, root );
            use_before_init_check( state, mir );
            if ( !state.success )
                return;

            analyze_liveness( state, mir );
            create_register_mapping( state, mir );
            if ( !state.success )
                return;

            assembly += "/* FILE " + file_path + " */\n";
            generate_code_x86( state, file_content, mir, assembly );
            assembly += "\n";
        };
        compile_pass();

        // Print messages
        for ( auto &m : state.messages ) {
            olog( generate_message_string( m, file_content ) );
        }
    }

    // Output into assembly file
    if ( state.success ) {
        std::ofstream os( conf.out_file + ".s" );
        os << assembly;
    }

    // Call assembler
    std::system(
        String( "gcc " + conf.out_file + ".s" + " -o " + conf.out_file )
            .c_str() );
}

} // namespace chc
