#pragma once
#include "pch.hpp"
#include "Lexer.hpp"

namespace chc {

struct CompilerState;

/// Some message to be show to the user.
struct Message {
    enum class Type {
        None,
        Error,
        Warning,
        Info,

        count
    } type = Type::None;
    String message;
    InFileInfo ifi;
};

/// Prints an error message to the user
void make_error_msg( CompilerState &state, const String &message,
                     const InFileInfo &ifi );
/// Prints an warning message to the user
void make_waring_msg( CompilerState &state, const String &message,
                      const InFileInfo &ifi );
/// Prints an info message to the user
void make_info_msg( CompilerState &state, const String &message,
                    const InFileInfo &ifi );

String generate_message_string( const Message &mes,
                                const String &file_content );

} // namespace chc
