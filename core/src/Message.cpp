#include "../include/chc/Message.hpp"
#include "../include/chc/Core.hpp"

namespace chc {

void make_error_msg( CompilerState &state, const String &message,
                     const InFileInfo &ifi, RetCode return_code ) {
    state.messages.push_back( Message{ Message::Type::Error, message, ifi } );
    state.success = false;
    if ( state.ret_code != RetCode::SyntaxError ) // Set highest priority
        state.ret_code = return_code;
}
void make_waring_msg( CompilerState &state, const String &message,
                      const InFileInfo &ifi ) {
    state.messages.push_back( Message{ Message::Type::Warning, message, ifi } );
}
void make_info_msg( CompilerState &state, const String &message,
                    const InFileInfo &ifi ) {
    state.messages.push_back( Message{ Message::Type::Info, message, ifi } );
}

String generate_message_string( const Message &mes,
                                const String &file_content ) {
    // Replace all tabs & newlines
    String clean_str = file_content;
    size_t itr = 0;
    while ( itr != clean_str.npos ) {
        itr = clean_str.find( "\t", itr );
        if ( itr != clean_str.npos ) {
            clean_str.erase( itr, 1 );
            clean_str.insert( itr, "    " );
            itr += 4;
        }
    }
    itr = 0;
    while ( itr != clean_str.npos ) {
        itr = clean_str.find( "\r\n", itr );
        if ( itr != clean_str.npos ) {
            clean_str.erase( itr, 1 );
        }
    }

    // Calculate ln & col
    size_t ln = 1;
    size_t col = 1;
    size_t line_idx = 0;

    for ( size_t i = 0; i < clean_str.size() && i < mes.ifi.offset; i++ ) {
        char c = clean_str[i];
        if ( c == '\n' ) {
            ++ln;
            col = 1;
            if ( i < mes.ifi.offset )
                line_idx = i + 1;
        } else {
            ++col;
        }
    }

    // Generate message text
    String mes_text;
    if ( mes.type == Message::Type::Error ) {
        mes_text = "Error";
    } else if ( mes.type == Message::Type::Warning ) {
        mes_text = "Warning";
    } else if ( mes.type == Message::Type::Info ) {
        mes_text = "Info";
    } else {
        mes_text = "Unknown message";
    }
    mes_text += " at " + to_string( ln ) + ":" + to_string( col ) + ": " +
                mes.message + "\n";

    // Generate code line
    String code_line =
        to_string( ln ) + " | " +
        clean_str.substr( line_idx,
                          clean_str.find( '\n', line_idx ) - line_idx ) +
        "\n";

    // Generate underline
    String underline = String( to_string( ln ).size() + 3 + col - 1, ' ' ) +
                       String( mes.ifi.size, '~' );

    // Return full message
    return mes_text + code_line + underline;
}

} // namespace chc
