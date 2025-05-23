#pragma once
#include "pch.hpp"
#include "Lexer.hpp"

namespace chc {

using SymbolId = size_t;

struct AstNode {
    enum class Type {
        None,

        Token,
        GlobalScope,
        FunctionDef,
        Stmt,
        Ret,
        ContinueStmt,
        BreakStmt,
        Decl,
        DeclUninit,
        Ident,
        Paren,
        Block,
        Type,
        IntConst,
        BoolConst,
        AsnOp,
        UniOp,
        BinOp,
        TernOp,
        IfStmt,
        IfElseStmt,
        WhileLoop,
        ForLoop,

        count
    } type = Type::None;

    Sptr<EagerContainer<AstNode>> nodes; // TODO could be Opt?
    Opt<Token> tok; // Leaves
    Opt<SymbolId> symbol_id; // Only for respective nodes

    InFileInfo ifi;

    bool matching_valid = true;
    /// Returns whether the parameter matches this node (as pattern).
    AstNode match( const AstNode instance ) {
        matching_valid = true;
        if ( type != Type::None )
            matching_valid = matching_valid && type == instance.type;
        if ( matching_valid && tok.has_value() )
            matching_valid = matching_valid && tok == instance.tok;
        if ( matching_valid && nodes ) {
            if ( !instance.nodes ||
                 nodes->length() > instance.nodes->length() ) {
                matching_valid = false;
            } else {
                matching_valid = instance.nodes->itr().match( nodes->itr() );
            }
        }
        return *this;
    }

    String get_type_name() const;
    operator bool() const { return matching_valid; }
};


AstNode make_parser( CompilerState &state, EagerContainer<Token> &tokens );

} // namespace chc
