#pragma once
#include "pch.hpp"
#include "Iterator.hpp"

namespace chc {

struct CompilerState;

struct InFileInfo {
    size_t offset = 0;
    size_t size = 0;

    InFileInfo merge( const InFileInfo &other ) const {
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
};

LazyIterator<Token> make_lexer( CompilerState &state, const String &text );

} // namespace chc
