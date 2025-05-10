#include "../include/chc/Lexer.hpp"
#include "../include/chc/Message.hpp"

namespace chc {

bool is_dec_int_token( const String &s ) {
    if ( s.size() != 1 )
        return false;
    return String( "0123456789" ).find( s.front() ) != String::npos;
}

bool is_hex_int_token( const String &s ) {
    if ( s.size() != 1 )
        return false;
    return String( "0123456789abcdefABCDEF" ).find( s.front() ) != String::npos;
}

bool is_ident_start_token( const String &s ) {
    if ( s.size() != 1 )
        return false;
    return String( "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
               .find( s.front() ) != String::npos;
}

bool is_ident_token( const String &s ) {
    return is_ident_start_token( s ) || is_dec_int_token( s );
}

bool is_operator_token( const String &s ) {
    if ( s.size() != 1 )
        return false;
    return String( "+-*/%=(){};" ).find( s.front() ) != String::npos;
}
bool is_merged_operator( const String &s ) {
    return s == "+=" || s == "-=" || s == "*=" || s == "/=" || s == "%=";
}

LazyIterator<Token> make_lexer( CompilerState &state, const String &text ) {
    size_t text_gen_idx = 0;
    LazyIterator<char> txt_raw( [&]() -> Opt<char> {
        if ( text_gen_idx < text.size() ) {
            return text[text_gen_idx++];
        } else {
            return {};
        }
    } );

    // Replace tabs
    String txt_clean_backlog;
    LazyIterator<char> txt_clean( [&]() -> Opt<char> {
        if ( !txt_clean_backlog.empty() ) {
            char r = txt_clean_backlog.front();
            txt_clean_backlog = txt_clean_backlog.substr( 1 );
            return r;
        }

        if ( txt_raw.curr_not_valid() )
            return {};
        auto c = txt_raw.consume();
        if ( c == '\t' ) {
            txt_clean_backlog += "   ";
            return ' ';
        }

        return c;
    } );

    // Insert position info
    struct FatChar {
        InFileInfo ifi;
        char c;
    };
    size_t file_cursor_idx = 0;
    auto txt_fat = txt_clean.map<FatChar>( [&]( const char &c ) {
        return FatChar{ InFileInfo{ file_cursor_idx++, 1 }, c };
    } );

    // Unify newlines
    LazyIterator<FatChar> txt_clean_newlines( [&]() -> Opt<FatChar> {
        if ( txt_fat.curr_not_valid() )
            return {};
        auto fc = txt_fat.consume();

        if ( fc.c == '\r' ) {
            if ( txt_fat.skip().consume_or( FatChar{ {}, 0 } ).c == '\n' ) {
                // Windows-style newlines
                txt_fat.consume();
                return FatChar{ InFileInfo{ fc.ifi.offset, 2 }, '\n' };
            } else {
                return FatChar{ fc.ifi, '\n' };
            }
        } else {
            return fc;
        }
    } );

    // Handle comments a strings
    LazyIterator<Token> txt_with_strings( [&]() -> Opt<Token> {
        if ( txt_clean_newlines.curr_not_valid() )
            return {};
        auto fc0 = txt_clean_newlines.consume();
        auto fc1 = txt_clean_newlines.get_or( FatChar{ {}, 0 } );

        InFileInfo ifi = fc0.ifi;
        if ( fc0.c == '/' ) {
            if ( fc1.c == '/' ) {
                // Line comment
                txt_clean_newlines.consume(); // second '/'
                ++ifi.size;
                while ( true ) {
                    fc0 = txt_clean_newlines.consume_or( FatChar{ {}, '\n' } );
                    ++ifi.size;
                    if ( fc0.c == '\n' )
                        break;
                }
                return Token{ Token::Type::None, " ",
                              ifi }; // Replace with single space.
            } else if ( fc1.c == '*' ) {
                // Block comment
                txt_clean_newlines.consume(); // '*'
                ++ifi.size;
                size_t nest_counter = 1; // Count nested block comments.
                while ( true ) {
                    fc0 = txt_clean_newlines.consume_or( FatChar{ {}, '*' } );
                    ++ifi.size;
                    fc1 = txt_clean_newlines.get_or( FatChar{ {}, '/' } );
                    if ( fc0.c == '*' && fc1.c == '/' ) {
                        --nest_counter;
                        txt_clean_newlines.consume(); // '/'
                        ++ifi.size;
                    } else if ( fc0.c == '/' && fc1.c == '*' ) {
                        ++nest_counter;
                        txt_clean_newlines.consume(); // '*'
                        ++ifi.size;
                    }
                    if ( nest_counter == 0 )
                        break;
                }
                return Token{ Token::Type::None, " ",
                              ifi }; // Replace with single space.
            }
            // TODO strings are not specified yet in L1!
            //} else if ( fc0.c == '"' ) {
            //    // Strings
            //    String content;
            //    while ( true ) {
            //        fc0 = txt_clean_newlines.consume_or( FatChar{ {}, '"' } );
            //        ++ifi.size;
            //        // TODO handle escape characters
            //        if ( fc0.c == '"' )
            //            break;
            //        content += fc0.c;
            //    }
            //    return Token{ Token::Type::String, content,
            //                  ifi }; // Produce string token.
        }

        // Other cases pass on normal characters
        return Token{ Token::Type::None, String( 1, fc0.c ), fc0.ifi };
    } );

    // Merge characters into tokens
    LazyIterator<Token> txt_separated( [&]() -> Opt<Token> {
        if ( txt_with_strings.curr_not_valid() )
            return {};
        auto t0 = txt_with_strings.consume();
        auto t1 =
            txt_with_strings.get_or( Token{ Token::Type::None, "\n", {} } );

        if ( t0.type != Token::Type::None )
            return t0; // Already categorized token, e g. strings.

        if ( t0.content == "0" ) {
            if ( t1.content == "x" || t1.content == "X" ) {
                // Potential hex integer
                auto t2 = txt_with_strings.skip( 1 ).get_or(
                    Token{ Token::Type::None, "\n", {} } );
                if ( is_hex_int_token( t2.content ) ) {
                    // Is a valid hex integer => Consume whole value.
                    Token tok{ Token::Type::HexInteger, "", t0.ifi };
                    txt_with_strings.consume(); // "x"
                    ++tok.ifi.size; // "x"
                    while ( is_hex_int_token( t2.content ) ) {
                        txt_with_strings.consume();
                        tok.content += t2.content;
                        ++tok.ifi.size;
                        t2 = txt_with_strings.get_or(
                            Token{ Token::Type::None, "\n", {} } );
                    }
                    return tok;
                } else {
                    // TODO
                    // For now just fall through
                }
            }

            // Must be the integer 0
            return Token{ Token::Type::DecInteger, "0", t0.ifi };
        } else if ( is_dec_int_token( t0.content ) ) {
            // Is an integer (but not 0)
            Token tok = t0;
            tok.type = Token::Type::DecInteger;
            t0 =
                txt_with_strings.get_or( Token{ Token::Type::None, "\n", {} } );
            while ( is_dec_int_token( t0.content ) ) {
                txt_with_strings.consume();
                tok.content += t0.content;
                ++tok.ifi.size;
                t0 = txt_with_strings.get_or(
                    Token{ Token::Type::None, "\n", {} } );
            }
            return tok;
        } else if ( is_ident_start_token( t0.content ) ) {
            // Identifier
            Token tok = t0;
            tok.type = Token::Type::Identifier;
            t0 =
                txt_with_strings.get_or( Token{ Token::Type::None, "\n", {} } );
            while ( is_ident_token( t0.content ) ) {
                txt_with_strings.consume();
                tok.content += t0.content;
                ++tok.ifi.size;
                t0 = txt_with_strings.get_or(
                    Token{ Token::Type::None, "\n", {} } );
            }
            return tok;
        } else if ( is_operator_token( t0.content ) ) {
            if ( is_merged_operator( t0.content + t1.content ) ) {
                // Double-char operator
                txt_with_strings.consume(); // second operator
                return Token{ Token::Type::Operator, t0.content + t1.content,
                              t0.ifi.merge( t1.ifi ) };
            } else {
                // Single-char operator
                return Token{ Token::Type::Operator, t0.content, t0.ifi };
            }
        } else if ( t0.content == " " || t0.content == "\n" ) {
            // Whitespace character
            return Token{ Token::Type::Whitespace, t0.content, t0.ifi };
        } else {
            make_error_msg( state, "Forbidden character", t0.ifi );
            return {};
        }
    } );

    // Detect keywords
    std::set<String> keywords = { "struct",  "if",       "else",  "while",
                                  "for",     "continue", "break", "return",
                                  "assert",  "true",     "false", "NULL",
                                  "print",   "read",     "alloc", "alloc_array",
                                  "int",     "bool",     "void ", " char ",
                                  " string " };
    LazyIterator<Token> txt_with_kw( [&, keywords]() -> Opt<Token> {
        if ( txt_separated.curr_not_valid() )
            return {};
        auto t0 = txt_separated.consume();
        if ( keywords.find( t0.content ) != keywords.end() )
            t0.type = Token::Type::Keyword;
        return t0;
    } );

    // Remove whitespace
    LazyIterator<Token> txt_no_ws = txt_with_kw.filter( []( const Token &t ) {
        return t.type != Token::Type::Whitespace;
    } );

    // Populate iterator cache (to circumvent iterator invalidation)
    txt_no_ws.end();

    // DEBUG
    if(true) {
        txt_no_ws.for_each( []( auto &&t ) {
            log( "Token " + to_string( (u32) t.type ) + ": " + t.content );
        } );
    }

    // Return final iterator
    return txt_no_ws;
}

} // namespace chc
