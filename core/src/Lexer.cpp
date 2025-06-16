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
    return String( "+-*/%=(){};!~&|^<>?:," ).find( s.front() ) != String::npos;
}
bool is_merged_operator( const String &s ) {
    return s == "&&" || s == "||" || s == "==" || s == "!=" || s == "<=" ||
           s == ">=" || s == "<<" || s == ">>" || s == "+=" || s == "-=" ||
           s == "*=" || s == "/=" || s == "%=" || s == "&=" || s == "|=" ||
           s == "^=" || s == "<<=" || s == ">>=";
}

EagerContainer<Token> make_lexer( CompilerState &state, const String &text ) {
    EagerContainer<char> txt_raw;
    txt_raw.fill( [&]( size_t idx ) -> Opt<char> {
        if ( idx < text.size() ) {
            return text[idx];
        } else {
            return {};
        }
    } );

    // Replace tabs
    String txt_clean_backlog;
    EagerContainer<char> txt_clean;
    txt_raw.for_each( [&]( const char &c ) {
        if ( c == '\t' ) {
            txt_clean.put( ' ' );
            txt_clean.put( ' ' );
            txt_clean.put( ' ' );
            txt_clean.put( ' ' );
        } else {
            txt_clean.put( c );
        }
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
    EagerContainer<FatChar> txt_clean_newlines;
    auto itr = txt_fat.itr();
    while ( itr ) {
        auto fc = itr.consume();

        if ( fc.c == '\r' ) {
            if ( itr.skip().consume_or( FatChar{ {}, 0 } ).c == '\n' ) {
                // Windows-style newlines
                txt_clean_newlines.put(
                    FatChar{ InFileInfo{ fc.ifi.offset, 2 }, '\n' } );
            } else {
                txt_clean_newlines.put( FatChar{ fc.ifi, '\n' } );
            }
        } else {
            txt_clean_newlines.put( fc );
        }
    }

    // Handle comments and strings
    EagerContainer<Token> txt_with_strings;
    itr = txt_clean_newlines.itr();
    while ( itr ) {
        auto fc0 = itr.consume();
        auto fc1 = itr.get_or( FatChar{ {}, 0 } );

        InFileInfo ifi = fc0.ifi;
        if ( fc0.c == '/' ) {
            if ( fc1.c == '/' ) {
                // Line comment
                itr.consume(); // second '/'
                ++ifi.size;
                while ( true ) {
                    fc0 = itr.consume_or( FatChar{ {}, '\n' } );
                    ++ifi.size;
                    if ( fc0.c == '\n' )
                        break;
                }
                txt_with_strings.put(
                    Token{ Token::Type::None, " ",
                           ifi } ); // Replace with single space.
                continue;
            } else if ( fc1.c == '*' ) {
                // Block comment
                itr.consume(); // '*'
                ++ifi.size;
                size_t nest_counter = 1; // Count nested block comments.
                while ( true ) {
                    fc0 = itr.consume_or( FatChar{ {}, '\n' } );
                    ++ifi.size;
                    fc1 = itr.get_or( FatChar{ {}, '\n' } );
                    if ( fc0.c == '*' && fc1.c == '/' ) {
                        --nest_counter;
                        itr.consume_opt(); // '/'
                        ++ifi.size;
                    } else if ( fc0.c == '/' && fc1.c == '*' ) {
                        ++nest_counter;
                        itr.consume_opt(); // '*'
                        ++ifi.size;
                    }
                    if ( nest_counter == 0 )
                        break;
                    if ( itr.curr_not_valid() ) {
                        // Reached end of file with unclosed block comment
                        make_error_msg( state, "Unclosed block comment",
                                        fc0.ifi, RetCode::SyntaxError );
                        break;
                    }
                }
                txt_with_strings.put(
                    Token{ Token::Type::None, " ",
                           ifi } ); // Replace with single space.
                continue;
            }
            // TODO strings are not specified yet in L1!
            //} else if ( fc0.c == '"' ) {
            //    // Strings
            //    String content;
            //    while ( true ) {
            //        fc0 = itr.consume_or( FatChar{ {}, '\n' } );
            //        ++ifi.size;
            //        // TODO handle escape characters
            //        if ( fc0.c == '"' )
            //            break;
            //        if ( itr.curr_not_valid() ) {
            //            // Reached end of file with unclosed string
            //            make_error_msg( state, "Unclosed string",
            //                            fc0.ifi, RetCode::SyntaxError );
            //            break;
            //        }
            //        content += fc0.c;
            //    }
            //    txt_with_strings.put( Token{ Token::Type::String, content,
            //                  ifi }; // Produce string token.
        }

        // Other cases pass on normal characters
        txt_with_strings.put(
            Token{ Token::Type::None, String( 1, fc0.c ), fc0.ifi } );
    };

    // Merge characters into tokens
    EagerContainer<Token> txt_tokenized;
    auto tok_itr = txt_with_strings.itr();
    while ( tok_itr ) {
        if ( tok_itr.curr_not_valid() ) {
            txt_tokenized.put( {} );
            continue;
        }
        auto t0 = tok_itr.consume();
        auto t1 = tok_itr.get_or( Token{ Token::Type::None, "\n", {} } );

        if ( t0.type != Token::Type::None ) {
            // Already categorized token, e g. strings.
            txt_tokenized.put( t0 );
            continue;
        }

        if ( t0.content == "0" ) {
            if ( t1.content == "x" || t1.content == "X" ) {
                // Potential hex integer
                auto t2 = tok_itr.skip( 1 ).get_or(
                    Token{ Token::Type::None, "\n", {} } );
                if ( is_hex_int_token( t2.content ) ) {
                    // Is a valid hex integer => Consume whole value.
                    Token tok{ Token::Type::HexInteger, "", t0.ifi };
                    tok_itr.consume(); // "x"
                    ++tok.ifi.size; // "x"
                    while ( is_hex_int_token( t2.content ) ) {
                        tok_itr.consume();
                        tok.content += t2.content;
                        ++tok.ifi.size;
                        t2 = tok_itr.get_or(
                            Token{ Token::Type::None, "\n", {} } );
                    }
                    txt_tokenized.put( tok );
                    continue;
                } else {
                    // TODO
                    // For now just fall through
                }
            }

            // Must be the integer 0
            txt_tokenized.put( Token{ Token::Type::DecInteger, "0", t0.ifi } );
        } else if ( is_dec_int_token( t0.content ) ) {
            // Is an integer (but not 0)
            Token tok = t0;
            tok.type = Token::Type::DecInteger;
            t0 = tok_itr.get_or( Token{ Token::Type::None, "\n", {} } );
            while ( is_dec_int_token( t0.content ) ) {
                tok_itr.consume();
                tok.content += t0.content;
                ++tok.ifi.size;
                t0 = tok_itr.get_or( Token{ Token::Type::None, "\n", {} } );
            }
            txt_tokenized.put( tok );
        } else if ( is_ident_start_token( t0.content ) ) {
            // Identifier
            Token tok = t0;
            tok.type = Token::Type::Identifier;
            t0 = tok_itr.get_or( Token{ Token::Type::None, "\n", {} } );
            while ( is_ident_token( t0.content ) ) {
                tok_itr.consume();
                tok.content += t0.content;
                ++tok.ifi.size;
                t0 = tok_itr.get_or( Token{ Token::Type::None, "\n", {} } );
            }
            txt_tokenized.put( tok );
        } else if ( is_operator_token( t0.content ) ) {
            auto t2 = tok_itr.skip( 1 ).get_or(
                Token{ Token::Type::None, "\n", {} } );
            if ( is_merged_operator( t0.content + t1.content + t2.content ) ) {
                // Triple-char operators
                tok_itr.consume(); // second operator
                tok_itr.consume(); // third operator
                txt_tokenized.put( Token{ Token::Type::Operator,
                                          t0.content + t1.content + t2.content,
                                          t0.ifi.merge( t2.ifi ) } );
            } else if ( is_merged_operator( t0.content + t1.content ) ) {
                // Double-char operator
                tok_itr.consume(); // second operator
                txt_tokenized.put( Token{ Token::Type::Operator,
                                          t0.content + t1.content,
                                          t0.ifi.merge( t1.ifi ) } );
            } else {
                // Single-char operator
                txt_tokenized.put(
                    Token{ Token::Type::Operator, t0.content, t0.ifi } );
            }
        } else if ( t0.content == " " || t0.content == "\n" ) {
            // Whitespace character
            txt_tokenized.put(
                Token{ Token::Type::Whitespace, t0.content, t0.ifi } );
        } else {
            make_error_msg( state, "Forbidden character.", t0.ifi,
                            RetCode::SyntaxError );
            txt_tokenized.put( {} );
        }
    }

    // Detect keywords
    std::set<String> keywords = { "struct",      "if",       "else",  "while",
                                  "for",         "continue", "break", "return",
                                  "assert",      "true",     "false", "NULL",
                                  "print",       "read",     "flush", "alloc",
                                  "alloc_array", "int",      "bool",  "void",
                                  "char",        "string" };
    EagerContainer<Token> txt_with_kw =
        txt_tokenized.map<Token>( [keywords]( const Token &tok ) {
            auto ret = tok;
            if ( keywords.find( ret.content ) != keywords.end() )
                ret.type = Token::Type::Keyword;
            return ret;
        } );

    // Remove whitespace
    EagerContainer<Token> txt_no_ws = txt_with_kw.filter(
        []( const Token &t ) { return t.type != Token::Type::Whitespace; } );

    // DEBUG
#ifndef NDEBUG
    if ( false ) {
        log( "== TOKENS ==" );
        txt_no_ws.for_each( []( auto &&t ) {
            log( "Token " + to_string( (u32) t.type ) + ": " + t.content );
        } );
    }
#endif

    // Return final iterator
    return txt_no_ws;
}

} // namespace chc
