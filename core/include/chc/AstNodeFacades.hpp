#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

namespace AstNodeFacades {

using AstCont = EagerContainer<AstNode>;

inline ArithType map_bin_arith( const String &str ) {
    if ( str == "" ) {
        return ArithType::None;
    } else if ( str == "+" ) {
        return ArithType::Add;
    } else if ( str == "-" ) {
        return ArithType::Sub;
    } else if ( str == "*" ) {
        return ArithType::Mul;
    } else if ( str == "/" ) {
        return ArithType::Div;
    } else if ( str == "%" ) {
        return ArithType::Mod;
    } else if ( str == "&" ) {
        return ArithType::BAnd;
    } else if ( str == "|" ) {
        return ArithType::BOr;
    } else if ( str == "^" ) {
        return ArithType::BXor;
    } else if ( str == "<<" ) {
        return ArithType::Shl;
    } else if ( str == ">>" ) {
        return ArithType::Shr;
    } else if ( str == "==" ) {
        return ArithType::Eq;
    } else if ( str == "!=" ) {
        return ArithType::UnEq;
    } else if ( str == "<" ) {
        return ArithType::Less;
    } else if ( str == ">" ) {
        return ArithType::Greater;
    } else if ( str == "<=" ) {
        return ArithType::LessEq;
    } else if ( str == ">=" ) {
        return ArithType::GreaterEq;
    } else if ( str == "&&" ) {
        return ArithType::LAnd;
    } else if ( str == "||" ) {
        return ArithType::LOr;
    } else {
        return ArithType::Unknown;
    }
}

inline String map_bin_arith( ArithType type ) {
    if ( type == ArithType::Add ) {
        return "+";
    } else if ( type == ArithType::Sub ) {
        return "-";
    } else if ( type == ArithType::Mul ) {
        return "*";
    } else if ( type == ArithType::Div ) {
        return "/";
    } else if ( type == ArithType::Mod ) {
        return "%";
    } else if ( type == ArithType::BAnd ) {
        return "&";
    } else if ( type == ArithType::BOr ) {
        return "|";
    } else if ( type == ArithType::BXor ) {
        return "^";
    } else if ( type == ArithType::Shl ) {
        return "<<";
    } else if ( type == ArithType::Shr ) {
        return ">>";
    } else if ( type == ArithType::Eq ) {
        return "==";
    } else if ( type == ArithType::UnEq ) {
        return "!=";
    } else if ( type == ArithType::Less ) {
        return "<";
    } else if ( type == ArithType::Greater ) {
        return ">";
    } else if ( type == ArithType::LessEq ) {
        return "<=";
    } else if ( type == ArithType::GreaterEq ) {
        return ">=";
    } else if ( type == ArithType::LAnd ) {
        return "&&";
    } else if ( type == ArithType::LOr ) {
        return "||";
    } else {
        return "";
    }
}

class FacadeBase {
protected:
    bool matches = false;

public:
    operator bool() const { return matches; }
};

AstNode &unwrap_paren( AstNode &node ) {
    if ( node.type == AstNode::Type::Paren ) {
        return node.nodes->itr().get();
    } else {
        return node;
    }
}

class TypeSpecifier : public FacadeBase {
public:
    TypeSpecifier() {}
    TypeSpecifier( const String &prim_name ) {
        type = Type::Prim;
        name = prim_name;
    }
    TypeSpecifier( AstNode &node ) {
        matches = true;
        if ( node.type == AstNode::Type::PrimType ) {
            type = Type::Prim;
            name = node.tok->content;
        } else if ( node.type == AstNode::Type::PtrType ) {
            type = Type::Ptr;
            sub = std::make_shared<TypeSpecifier>( node.nodes->itr().get() );
        } else if ( node.type == AstNode::Type::NullConst ) {
            type = Type::Nullptr;
        } else if ( node.type == AstNode::Type::ArrayType ) {
            type = Type::Array;
            sub = std::make_shared<TypeSpecifier>( node.nodes->itr().get() );
        } else if ( node.type == AstNode::Type::StructType ) {
            type = Type::Struct;
            name = node.tok->content;
            struct_symbol_id = &node.symbol_id;
        } else {
            matches = false;
        }
    }
    bool operator==( const TypeSpecifier &other ) const {
        return type == other.type && ( !sub || *sub == *other.sub ) &&
               name == other.name;
    }
    bool operator!=( const TypeSpecifier &other ) const {
        return !( *this == other );
    }
    int operator<( const TypeSpecifier &other ) const {
        if ( type != other.type )
            return type < other.type;
        if ( type == Type::Prim || type == Type::Struct ) {
            return name < other.name;
        } else if ( type == Type::Nullptr ) {
            return false;
        } else {
            return *sub < *other.sub;
        }
    }
    static std::shared_ptr<TypeSpecifier> make_pointer_to(
        const std::shared_ptr<TypeSpecifier> &sub ) {
        auto ret = std::make_shared<TypeSpecifier>();
        ret->type = Type::Ptr;
        ret->sub = sub;
        return ret;
    }
    static std::shared_ptr<TypeSpecifier> make_null_pointer() {
        auto ret = std::make_shared<TypeSpecifier>();
        ret->type = Type::Nullptr;
        return ret;
    }
    static std::shared_ptr<TypeSpecifier> make_array_of(
        const std::shared_ptr<TypeSpecifier> &sub ) {
        auto ret = std::make_shared<TypeSpecifier>();
        ret->type = Type::Array;
        ret->sub = sub;
        return ret;
    }
    static std::shared_ptr<TypeSpecifier> make_struct_type(
        const String &name ) {
        auto ret = std::make_shared<TypeSpecifier>();
        ret->type = Type::Struct;
        ret->name = name;
        return ret;
    }

    enum class Type {
        None,
        Prim,
        Ptr,
        Nullptr,
        Array,
        Struct,
        count
    } type = Type::None;
    std::shared_ptr<TypeSpecifier> sub;
    String name;
    Opt<SymbolId> *struct_symbol_id = nullptr;
};

class FunctionDef : public FacadeBase {
public:
    FunctionDef( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::FunctionDef;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            auto decl_itr = itr.get().nodes->itr();
            type = std::make_shared<TypeSpecifier>( decl_itr.get() );
            fn_symbol = decl_itr.skip( 1 ).get().tok->content;
            fn_symbol_id = &decl_itr.skip( 1 ).get().symbol_id;
            auto &paren_content = itr.skip( 1 ).get().nodes;
            if ( paren_content->empty() ||
                 paren_content->itr().get().type != AstNode::Type::CommaList ) {
                params = &*paren_content;
            } else {
                // Pass elements of the CommaList
                params = &*paren_content->itr().get().nodes;
            }
            stmts = &*itr.skip( 2 ).get().nodes;
        }
    }

    std::shared_ptr<TypeSpecifier> type;
    String fn_symbol;
    Opt<SymbolId> *fn_symbol_id = nullptr;
    AstCont *params = nullptr;
    AstCont *stmts = nullptr;
};

class StructDef : public FacadeBase {
public:
    StructDef( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::StructDef;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            struct_symbol = itr.get().tok->content;
            struct_symbol_id = &itr.get().symbol_id;
            fields = &*itr.skip( 1 ).get().nodes;
        }
    }

    String struct_symbol;
    Opt<SymbolId> *struct_symbol_id = nullptr;
    AstCont *fields = nullptr;
};

class Ret : public FacadeBase {
public:
    Ret( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Ret;
        if ( matches ) {
            value = &to_wrap.nodes->itr().get();
        }
    }

    AstNode *value = nullptr;
};

class Decl : public FacadeBase {
public:
    Decl( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Decl;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            type = std::make_shared<TypeSpecifier>( itr.get() );
            auto asnop_itr = itr.skip( 1 ).get().nodes->itr();
            symbol = asnop_itr.get().tok->content;
            symbol_id = &asnop_itr.get().symbol_id;
            init = &asnop_itr.skip( 1 ).get();
        }
    }

    std::shared_ptr<TypeSpecifier> type;
    String symbol;
    Opt<SymbolId> *symbol_id = nullptr;
    AstNode *init = nullptr;
};

class DeclUninit : public FacadeBase {
public:
    DeclUninit( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::DeclUninit;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            type = std::make_shared<TypeSpecifier>( itr.get() );
            symbol = itr.skip( 1 ).get().tok->content;
            symbol_id = &itr.skip( 1 ).get().symbol_id;
        }
    }

    std::shared_ptr<TypeSpecifier> type;
    String symbol;
    Opt<SymbolId> *symbol_id = nullptr;
};

class Ident : public FacadeBase {
public:
    Ident( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Ident;
        if ( matches ) {
            symbol = to_wrap.tok->content;
            id = &to_wrap.symbol_id;
        }
    }

    String symbol;
    Opt<SymbolId> *id = nullptr;
};

class Paren : public FacadeBase {
public:
    Paren( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Paren;
        if ( matches ) {
            assert( to_wrap.nodes );
            children = &*to_wrap.nodes;
        }
    }

    AstCont *children = nullptr;
};

class Block : public FacadeBase {
public:
    Block( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Block;
        if ( matches ) {
            assert( to_wrap.nodes );
            children = &*to_wrap.nodes;
        }
    }

    AstCont *children = nullptr;
};

class PrimType : public FacadeBase {
public:
    PrimType( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::PrimType;
        if ( matches ) {
            type_name = &to_wrap.tok->content;
        }
    }

    String *type_name = nullptr;
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

class BoolConst : public FacadeBase {
public:
    BoolConst( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::BoolConst;
        if ( matches ) {
            value = to_wrap.tok->content != "false" ? true : false;
        }
    }

    bool value;
};

class AsnOp : public FacadeBase {
public:
    AsnOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::AsnOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lvalue = &unwrap_paren( itr.get() );
            value = &itr.skip( 1 ).get();
            auto &type_str = to_wrap.tok->content;
            type = map_bin_arith( type_str.substr( 0, type_str.size() - 1 ) );
        }
    }
    void update_arith_type( AstNode &wrapped_node, ArithType new_type ) {
        auto &type_str = wrapped_node.tok->content;
        type_str = map_bin_arith( new_type ) + "=";
    }

    AstNode *lvalue = nullptr;
    AstNode *value = nullptr;
    ArithType type;
};

class UniOp : public FacadeBase {
public:
    UniOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::UniOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            rhs = &itr.get();
            auto &type_str = to_wrap.tok->content;
            if ( type_str == "-" ) {
                type = ArithType::Neg;
            } else if ( type_str == "!" ) {
                type = ArithType::LNot;
            } else if ( type_str == "~" ) {
                type = ArithType::BInv;
            } else {
                type = ArithType::Unknown;
            }
        }
    }

    AstNode *rhs = nullptr;
    ArithType type;
};

class BinOp : public FacadeBase {
public:
    BinOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::BinOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = &itr.get();
            rhs = &itr.skip( 1 ).get();
            auto &type_str = to_wrap.tok->content;
            type = map_bin_arith( type_str );
        }
    }
    void update_arith_type( AstNode &wrapped_node, ArithType new_type ) {
        auto &type_str = wrapped_node.tok->content;
        type_str = map_bin_arith( new_type );
    }

    AstNode *lhs = nullptr;
    AstNode *rhs = nullptr;
    ArithType type;
};

class TernOp : public FacadeBase {
public:
    TernOp( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::TernOp;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = &itr.get();
            mid = &itr.skip( 1 ).get();
            rhs = &itr.skip( 2 ).get();
        }
    }

    AstNode *lhs = nullptr;
    AstNode *mid = nullptr;
    AstNode *rhs = nullptr;
};

class IfStmt : public FacadeBase {
public:
    IfStmt( AstNode &to_wrap ) {
        if ( to_wrap.type == AstNode::Type::IfStmt ) {
            matches = true;
            auto itr = to_wrap.nodes->itr();
            cond = &itr.get().nodes->itr().get();
            true_stmt = itr.skip( 1 ).get();
            false_stmt = AstNode{ AstNode::Type::None };
        } else if ( to_wrap.type == AstNode::Type::IfElseStmt ) {
            matches = true;
            auto itr = to_wrap.nodes->itr();
            cond = &itr.get().nodes->itr().get();
            true_stmt = itr.skip( 1 ).get();
            false_stmt = itr.skip( 2 ).get();
        } else {
            matches = false;
        }
    }
    void write_back_true_stmt( AstNode &wrapped_node ) {
        auto itr = wrapped_node.nodes->itr();
        itr.skip( 1 ).get() = true_stmt;
    }
    void write_back_false_stmt( AstNode &wrapped_node ) {
        auto itr = wrapped_node.nodes->itr();
        itr.skip( 2 ).get() = false_stmt;
    }

    AstNode *cond = nullptr;
    AstNode true_stmt;
    AstNode false_stmt;
};

class WhileLoop : public FacadeBase {
public:
    WhileLoop( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::WhileLoop;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            cond = &itr.get().nodes->itr().get();
            body = &itr.skip( 1 ).get();
        }
    }

    AstNode *cond = nullptr;
    AstNode *body = nullptr;
};

class ForLoop : public FacadeBase {
public:
    ForLoop( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::ForLoop;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            init = &itr.get().nodes->itr().get();
            cond = &itr.get().nodes->itr().skip( 1 ).get();
            step = &itr.get().nodes->itr().skip( 2 ).get();
            body = &itr.skip( 1 ).get();
        }
    }

    AstNode *init = nullptr;
    AstNode *cond = nullptr;
    AstNode *step = nullptr;
    AstNode *body = nullptr;
};

class Call : public FacadeBase {
public:
    Call( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::Call;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            fn_symbol = itr.get().tok->content;
            fn_symbol_id = &itr.get().symbol_id;
            auto &paren_content = itr.skip( 1 ).get().nodes;
            if ( paren_content->empty() ||
                 paren_content->itr().get().type != AstNode::Type::CommaList ) {
                args = &*paren_content;
            } else {
                // Pass elements of the CommaList
                args = &*paren_content->itr().get().nodes;
            }
        }
    }

    String fn_symbol;
    Opt<SymbolId> *fn_symbol_id = nullptr;
    AstCont *args = nullptr;
};


class AllocCall : public FacadeBase {
public:
    AllocCall( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::AllocCall;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            fn_symbol = to_wrap.tok->content;
            auto &paren_content = itr.get().nodes;
            if ( paren_content->empty() ||
                 paren_content->itr().get().type != AstNode::Type::CommaList ) {
                args = &*paren_content;
            } else {
                // Pass elements of the CommaList
                args = &*paren_content->itr().get().nodes;
            }
        }
    }

    String fn_symbol;
    AstCont *args = nullptr;
};

class FieldAccess : public FacadeBase {
public:
    FieldAccess( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::FieldAccess;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = &itr.get();
            auto &rhs = itr.skip( 1 ).get();
            field_symbol = rhs.tok->content;
        }
    }

    AstNode *lhs = nullptr;
    String field_symbol;
};

class IndirectAccess : public FacadeBase {
public:
    IndirectAccess( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::IndirectAccess;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = &itr.get();
            auto &rhs = itr.skip( 1 ).get();
            field_symbol = rhs.tok->content;
        }
    }

    AstNode *lhs = nullptr;
    String field_symbol;
};

class ArrayAccess : public FacadeBase {
public:
    ArrayAccess( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::ArrayAccess;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            lhs = &itr.get();
            idx = &itr.skip( 1 ).get().nodes->itr().get();
        }
    }

    AstNode *lhs = nullptr;
    AstNode *idx = nullptr;
};

class PtrDeref : public FacadeBase {
public:
    PtrDeref( AstNode &to_wrap ) {
        matches = to_wrap.type == AstNode::Type::PtrDeref;
        if ( matches ) {
            auto itr = to_wrap.nodes->itr();
            ptr = &itr.get();
        }
    }

    AstNode *ptr = nullptr;
};

} // namespace AstNodeFacades

using ShrTypeS = std::shared_ptr<AstNodeFacades::TypeSpecifier>;
using TypeSpecifier = AstNodeFacades::TypeSpecifier;

} // namespace chc
