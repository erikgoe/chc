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
        StructDef,
        Stmt,
        Ret,
        ContinueStmt,
        BreakStmt,
        Decl,
        DeclUninit,
        Ident,
        Paren,
        Block,
        Bracket,
        PrimType,
        PtrType,
        ArrayType,
        StructType,
        IntConst,
        BoolConst,
        NullConst,
        AsnOp,
        UniOp,
        BinOp,
        TernOp,
        IfStmt,
        IfElseStmt,
        WhileLoop,
        ForLoop,
        Call,
        AllocCall,
        CommaList,
        MemberAccess,
        IndirectAccess,
        ArrayAccess,
        PtrDeref,

        count
    } type = Type::None;

    Sptr<EagerContainer<AstNode>> nodes; // TODO could be Opt?
    Opt<Token> tok; // Leaves
    Opt<SymbolId> symbol_id; // Only for respective nodes

    InFileInfo ifi; // TODO replace with function which returns tok.ifi.

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

enum class ArithType {
    None,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,

    BInv,
    BAnd,
    BOr,
    BXor,

    Shl,
    Shr,

    Eq,
    UnEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    LNot,
    LAnd,
    LOr,

    Unknown,

    count
};

inline bool has_only_bool_params( ArithType t ) {
    switch ( t ) {
    case ArithType::LNot:
    case ArithType::LAnd:
    case ArithType::LOr:
        return true;
    default:
        return false;
    }
}
inline bool has_only_int_params( ArithType t ) {
    switch ( t ) {
    case ArithType::Add:
    case ArithType::Sub:
    case ArithType::Mul:
    case ArithType::Div:
    case ArithType::Mod:
    case ArithType::Neg:
    case ArithType::BInv:
    case ArithType::BAnd:
    case ArithType::BOr:
    case ArithType::BXor:
    case ArithType::Shl:
    case ArithType::Shr:
    case ArithType::Less:
    case ArithType::Greater:
    case ArithType::LessEq:
    case ArithType::GreaterEq:
        return true;
    default:
        return false;
    }
}
inline bool has_only_bool_ret( ArithType t ) {
    switch ( t ) {
    case ArithType::Eq:
    case ArithType::UnEq:
    case ArithType::Less:
    case ArithType::Greater:
    case ArithType::LessEq:
    case ArithType::GreaterEq:
    case ArithType::LNot:
    case ArithType::LAnd:
    case ArithType::LOr:
        return true;
    default:
        return false;
    }
}
inline bool has_only_int_ret( ArithType t ) {
    switch ( t ) {
    case ArithType::Add:
    case ArithType::Sub:
    case ArithType::Mul:
    case ArithType::Div:
    case ArithType::Mod:
    case ArithType::Neg:
    case ArithType::BInv:
    case ArithType::BAnd:
    case ArithType::BOr:
    case ArithType::BXor:
    case ArithType::Shl:
    case ArithType::Shr:
        return true;
    default:
        return false;
    }
}
inline bool has_any_type_ret( ArithType t ) {
    switch ( t ) {
    case ArithType::None:
    case ArithType::Eq:
    case ArithType::UnEq:
        return true;
    default:
        return false;
    }
}

AstNode make_parser( CompilerState &state, EagerContainer<Token> &tokens );

} // namespace chc
