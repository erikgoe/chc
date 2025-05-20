#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

using AstCont = EagerContainer<AstNode>;
using AstItr = EagerContainer<AstNode>::Iterator;

/// \param func should return true when nodes was modified.
inline void apply_pass_recursively_from_left(
    CompilerState &state, AstCont &block, const AstNode &parent,
    std::function<bool( CompilerState &state, AstItr &nodes,
                        const AstNode &parent )>
        func ) {
    auto itr = block.begin();
    while ( itr ) {
        while ( func( state, itr, parent ) ) {
        }
        if ( itr && itr.get().nodes )
            apply_pass_recursively_from_left( state, *itr.get().nodes,
                                              itr.get(), func );
        itr.skip_self( 1 );
    }
}

/// \param func should return true when nodes was modified.
inline void apply_pass_recursively_from_right(
    CompilerState &state, AstCont &block, const AstNode &parent,
    std::function<bool( CompilerState &state, AstItr &nodes,
                        const AstNode &parent )>
        func ) {
    auto itr = block.end();
    while ( itr != block.begin() ) {
        itr.skip_self( -1 );
        while ( func( state, itr, parent ) ) {
        }
        if ( itr && itr.get().nodes )
            apply_pass_recursively_from_right( state, *itr.get().nodes,
                                               itr.get(), func );
    }
}

} // namespace chc
