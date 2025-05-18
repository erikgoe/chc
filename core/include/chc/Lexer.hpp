#pragma once
#include "pch.hpp"
#include "EagerContainer.hpp"

namespace chc {

struct CompilerState;

struct InFileInfo {
    size_t offset = 0;
    size_t size = 0;

    InFileInfo merge( const InFileInfo &other ) const {
        if ( other.offset == 0 && other.size == 0 )
            return *this;
        if ( offset == 0 && size == 0 )
            return other;
        size_t start = std::min( offset, other.offset );
        size_t end = std::max( offset + size, other.offset + other.size );
        return InFileInfo{ start, end - start };
    }
};

struct Token {
    enum class Type {
        None,
        Whitespace,
        DecInteger,
        HexInteger,
        Identifier,
        Keyword,
        Operator,
        String,

        count
    } type = Type::None;
    String content;
    InFileInfo ifi;

    bool operator==( const Token &other ) const {
        return type == other.type && content == other.content;
    }
    bool operator!=( const Token &other ) const {
        return !( ( *this ) == other );
    }
};

EagerContainer<Token> make_lexer( CompilerState &state, const String &text );

} // namespace chc
