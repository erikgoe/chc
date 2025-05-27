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
            use_before_init_and_return_check( state, mir );
            drop_uninit_instrs( state, mir );
            if ( !state.success )
                return;
            type_checking( state, mir );
            if ( !state.success )
                return;

            analyze_liveness( state, mir );
            analyze_neededness( state, mir );
            trim_dead_code( state, mir );
            create_register_mapping( state, mir );
            if ( !state.success )
                return;

            EagerContainer<Assembly_x86> asm_code;
            generate_code_x86( state, file_content, mir, asm_code );
            assembly += "/* FILE " + file_path + " */\n";
            generate_asm_text_x86( state, asm_code, assembly );
            assembly += "\n";
        };
        compile_pass();

        // Print messages (max 30 messages)
        for ( size_t i = 0; i < state.messages.size() && i < 30; i++ ) {
            olog( generate_message_string( state.messages[i], file_content ) );
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
