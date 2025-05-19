#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {


namespace AstNodeFacades {

using AstCont = EagerContainer<AstNode>;

enum class ArithType {
    None,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Neg,

    Unknown,

    count
};

class FacadeBase {
protected:
    bool matches = false;

public:
    operator bool() const { return matches; }
};

const AstNode &unwrap_paren( const AstNode &node ) {
    if ( node.type == AstNode::Type::Paren ) {
        return node.nodes->first()->get();
    } else {
        return node;
    }
}

class FunctionDef : public FacadeBase {
public:
    FunctionDef( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::FunctionDef;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            auto decl_itr = itr.get().nodes->itr();
            type = itr.get().tok->content;
            fn_symbol = decl_itr.get().tok->content;
            fn_symbol_id = decl_itr.get().symbol_id;
            params = *itr.skip( 1 ).get().nodes;
            stmts = *itr.skip( 2 ).get().nodes;
        }
    }
    void update_symbol_id( AstNode &wrapped_node, SymbolId new_id ) {
        auto itr = wrapped_node.nodes->itr();
        auto decl_itr = itr.get().nodes->itr();
        decl_itr.get().symbol_id = new_id;
        fn_symbol_id = new_id;
    }

    String type;
    String fn_symbol;
    Opt<SymbolId> fn_symbol_id;
    AstCont params;
    AstCont stmts;
};

class RetStmt : public FacadeBase {
public:
    RetStmt( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Stmt &&
                  to_wrap.nodes->first()->get().type == AstNode::Type::Ret;
        if ( matches ) {
            auto itr = to_wrap.nodes->first()->get().nodes->itr();
            value = itr.get();
        }
    }

    AstNode value;
};

class DeclStmt : public FacadeBase {
public:
    DeclStmt( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Stmt &&
                  to_wrap.nodes->first()->get().type == AstNode::Type::Decl;
        if ( matches ) {
            type = to_wrap.nodes->first()->get().tok->content;
            auto itr = to_wrap.nodes->first()->get().nodes->itr();
            auto simp_itr = itr.get().nodes->itr();
            symbol = simp_itr.get().tok->content;
            symbol_id = simp_itr.get().symbol_id;
            init = simp_itr.skip( 1 ).get();
        }
    }
    void update_symbol_id( AstNode &wrapped_node, SymbolId new_id ) {
        auto itr = wrapped_node.nodes->first()->get().nodes->itr();
        auto simp_itr = itr.get().nodes->itr();
        simp_itr.get().symbol_id = new_id;
        symbol_id = new_id;
    }

    String type;
    String symbol;
    Opt<SymbolId> symbol_id;
    AstNode init;
};

class DeclUninitStmt : public FacadeBase {
public:
    DeclUninitStmt( AstNode &to_wrap ) {
        matches =
            to_wrap.type == AstNode::Type::Stmt &&
            to_wrap.nodes->first()->get().type == AstNode::Type::DeclUninit;
        if ( matches ) {
            type = to_wrap.nodes->first()->get().tok->content;
            auto itr = to_wrap.nodes->first()->get().nodes->itr();
            symbol = itr.get().tok->content;
            symbol_id = itr.get().symbol_id;
        }
    }
    void update_symbol_id( AstNode &wrapped_node, SymbolId new_id ) {
        auto itr = wrapped_node.nodes->first()->get().nodes->itr();
        itr.get().symbol_id = new_id;
        symbol_id = new_id;
    }

    String type;
    String symbol;
    Opt<SymbolId> symbol_id;
};

class SimpStmt : public FacadeBase {
public:
    SimpStmt( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Stmt &&
                  to_wrap.nodes->first()->get().type == AstNode::Type::Simp;
        if ( matches ) {
            auto itr = to_wrap.nodes->first()->get().nodes->itr();
            // TODO check that parenthesis has only one subnode
            lvalue = unwrap_paren( itr.get() );
            value = itr.skip( 1 ).get();
            auto &type_str = to_wrap.nodes->first()->get().tok->content;
            if ( type_str == "=" ) {
                type = ArithType::None;
            } else if ( type_str == "+=" ) {
                type = ArithType::Add;
            } else if ( type_str == "-=" ) {
                type = ArithType::Sub;
            } else if ( type_str == "*=" ) {
                type = ArithType::Mul;
            } else if ( type_str == "/=" ) {
                type = ArithType::Div;
            } else if ( type_str == "%=" ) {
                type = ArithType::Mod;
            } else {
                type = ArithType::Unknown;
            }
        }
    }
    void update_arith_type( AstNode &wrapped_node, ArithType new_type ) {
        auto &type_str = wrapped_node.nodes->itr().get().tok->content;
        if ( new_type == ArithType::None ) {
            type_str = "=";
        } else if ( new_type == ArithType::Add ) {
            type_str = "+=";
        } else if ( new_type == ArithType::Sub ) {
            type_str = "-=";
        } else if ( new_type == ArithType::Mul ) {
            type_str = "*=";
        } else if ( new_type == ArithType::Div ) {
            type_str = "/=";
        } else if ( new_type == ArithType::Mod ) {
            type_str = "%=";
        }
    }

    AstNode lvalue;
    AstNode value;
    ArithType type;
};

class Ident : public FacadeBase {
public:
    Ident( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Ident;
        if ( matches ) {
            symbol = to_wrap.tok->content;
            id = to_wrap.symbol_id;
        }
    }
    void update_symbol_id( AstNode &wrapped_node, SymbolId new_id ) {
        wrapped_node.symbol_id = new_id;
        id = new_id;
    }

    String symbol;
    Opt<SymbolId> id;
};

class Paren : public FacadeBase {
public:
    Paren( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Paren;
        if ( matches ) {
            if ( to_wrap.nodes )
                children = *to_wrap.nodes;
        }
    }

    AstCont children;
};

class Block : public FacadeBase {
public:
    Block( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Block;
        if ( matches ) {
            if ( to_wrap.nodes )
                children = *to_wrap.nodes;
        }
    }

    AstCont children;
};

class IntConst : public FacadeBase {
public:
    IntConst( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::IntConst;
        if ( matches ) {
            value = stoul( to_wrap.tok->content );
        }
    }

    i32 value;
};

class BinOp : public FacadeBase {
public:
    BinOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::BinOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = itr.get();
            rhs = itr.skip( 1 ).get();
            auto &type_str = to_wrap.tok->content;
            if ( type_str == "+" ) {
                type = ArithType::Add;
            } else if ( type_str == "-" ) {
                type = ArithType::Sub;
            } else if ( type_str == "*" ) {
                type = ArithType::Mul;
            } else if ( type_str == "/" ) {
                type = ArithType::Div;
            } else if ( type_str == "%" ) {
                type = ArithType::Mod;
            } else {
                type = ArithType::Unknown;
            }
        }
    }

    AstNode lhs;
    AstNode rhs;
    ArithType type;
};

class UniOp : public FacadeBase {
public:
    UniOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::UniOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            rhs = itr.get();
            auto &type_str = to_wrap.tok->content;
            if ( type_str == "-" ) {
                type = ArithType::Neg;
            } else {
                type = ArithType::Unknown;
            }
        }
    }

    AstNode rhs;
    ArithType type;
};

} // namespace AstNodeFacades

} // namespace chc
