#include "../include/chc/Parser.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/ParserUtils.hpp"

namespace chc {

String AstNode::get_type_name() const {
    switch ( type ) {
    case AstNode::Type::None:
        return "None";
    case AstNode::Type::Token:
        return "Token";
    case AstNode::Type::GlobalScope:
        return "GlobalScope";
    case AstNode::Type::FunctionDef:
        return "FunctionDef";
    case AstNode::Type::StructDef:
        return "StructDef";
    case AstNode::Type::Stmt:
        return "Stmt";
    case AstNode::Type::Ret:
        return "Ret";
    case AstNode::Type::Decl:
        return "Decl";
    case AstNode::Type::DeclUninit:
        return "DeclUninit";
    case AstNode::Type::AsnOp:
        return "AsnOp";
    case AstNode::Type::Ident:
        return "Ident";
    case AstNode::Type::Paren:
        return "Paren";
    case AstNode::Type::Block:
        return "Block";
    case AstNode::Type::Bracket:
        return "Bracket";
    case AstNode::Type::IntConst:
        return "IntConst";
    case AstNode::Type::BoolConst:
        return "BoolConst";
    case AstNode::Type::NullConst:
        return "NullConst";
    case AstNode::Type::BinOp:
        return "BinOp";
    case AstNode::Type::UniOp:
        return "UniOp";
    case AstNode::Type::PrimType:
        return "PrimType";
    case AstNode::Type::PtrType:
        return "PtrType";
    case AstNode::Type::ArrayType:
        return "ArrayType";
    case AstNode::Type::StructType:
        return "StructType";
    case AstNode::Type::IfStmt:
        return "IfStmt";
    case AstNode::Type::IfElseStmt:
        return "IfElseStmt";
    case AstNode::Type::WhileLoop:
        return "WhileLoop";
    case AstNode::Type::ForLoop:
        return "ForLoop";
    case AstNode::Type::ContinueStmt:
        return "ContinueStmt";
    case AstNode::Type::BreakStmt:
        return "BreakStmt";
    case AstNode::Type::TernOp:
        return "TernOp";
    case AstNode::Type::Call:
        return "Call";
    case AstNode::Type::AllocCall:
        return "AllocCall";
    case AstNode::Type::CommaList:
        return "CommaList";
    case AstNode::Type::FieldAccess:
        return "FieldAccess";
    case AstNode::Type::IndirectAccess:
        return "IndirectAccess";
    case AstNode::Type::ArrayAccess:
        return "ArrayAccess";
    case AstNode::Type::PtrDeref:
        return "PtrDeref";
    default:
        return "Unknown";
    }
}

void print_ast( const AstNode &root, const String &title = "" ) {
    if ( !title.empty() )
        log( title );
    std::function<void( const AstNode &, size_t )> print_node;
    print_node = [&]( const AstNode &n, size_t indent ) {
        String str = String( indent, ' ' ) + n.get_type_name() +
                     ( n.tok ? ": " + n.tok->content : "" );
        olog( str );
        if ( n.nodes )
            n.nodes->for_each(
                [&]( auto &&sub ) { print_node( sub, indent + 1 ); } );
    };
    print_node( root, 0 );
}

AstNode ast( AstNode::Type type ) {
    return AstNode{ type };
}
template <typename... Pack>
AstNode ast( AstNode::Type type, Pack... pack ) {
    auto cont = std::vector<AstNode>{ pack... };
    auto nodes = std::make_shared<EagerContainer<AstNode>>();
    nodes->fill( [&cont]( size_t i ) -> Opt<AstNode> {
        if ( i >= cont.size() )
            return {};
        return cont[i];
    } );
    return AstNode{ type, nodes };
}
AstNode ast_tok( Token::Type type, const String &content = "" ) {
    return AstNode{ AstNode::Type::Token, {}, Token{ type, content } };
}
template <typename... Pack>
AstNode ast_with_tok( AstNode::Type type, Token::Type tok_type,
                      const String &content, Pack... pack ) {
    auto cont = std::vector<AstNode>{ pack... };
    auto nodes = std::make_shared<EagerContainer<AstNode>>();
    nodes->fill( [&cont]( size_t i ) -> Opt<AstNode> {
        if ( i >= cont.size() )
            return {};
        return cont[i];
    } );
    return AstNode{ type, nodes, Token{ tok_type, content } };
}
AstNode any() {
    return AstNode{ AstNode::Type::None };
}

/// Class that specifies that matching should "unwrap" the value into the
/// specified variable.
class unwrap {
    AstNode &ref;

public:
    unwrap( AstNode &node ) : ref( node ) {}
    AstNode match( const AstNode instance ) {
        return ref = ref.match( instance );
    }
    operator bool() const { return ref; }
};
using AT = AstNode::Type;
using TT = Token::Type;
using AstCont = EagerContainer<AstNode>;
using AstItr = EagerContainer<AstNode>::Iterator;

bool is_asnop_operator( const String &op ) {
    return op.substr( op.size() - 1 ) == "=" && op != "==" && op != "!=" &&
           op != "<=" && op != ">=";
}

bool is_expr( const AstNode &node ) {
    return node.type == AT::Paren || node.type == AT::IntConst ||
           node.type == AT::Ident ||
           ( node.type == AT::BinOp &&
             !is_asnop_operator( node.tok->content ) ) ||
           node.type == AT::UniOp || node.type == AT::BoolConst ||
           node.type == AT::NullConst || node.type == AT::TernOp ||
           node.type == AT::Call || node.type == AT::AllocCall ||
           node.type == AT::FieldAccess || node.type == AT::IndirectAccess ||
           node.type == AT::ArrayAccess || node.type == AT::PtrDeref;
}

bool is_stmt_body( const AstNode &node ) {
    return node.type == AT::Decl || node.type == AT::DeclUninit ||
           node.type == AT::AsnOp || node.type == AT::Ret ||
           node.type == AT::BreakStmt || node.type == AT::ContinueStmt ||
           node.type == AT::Call || node.type == AT::AllocCall;
}

bool is_lvalue( const AstNode &node ) {
    return node.type == AT::Ident || node.type == AT::FieldAccess ||
           node.type == AT::IndirectAccess || node.type == AT::ArrayAccess ||
           node.type == AT::PtrDeref ||
           ( node.type == AT::Paren && node.nodes->not_empty() &&
             is_lvalue( *node.nodes->first() ) );
}

bool is_type( const AstNode &node ) {
    return node.type == AT::PrimType || node.type == AT::PtrType ||
           node.type == AT::ArrayType || node.type == AT::StructType;
}

bool is_stmt( const AstNode &node ) {
    return node.type == AT::Stmt || node.type == AT::Block ||
           node.type == AT::IfStmt || node.type == AT::IfElseStmt ||
           node.type == AT::WhileLoop || node.type == AT::ForLoop;
}

bool is_function_body( const AstNode &node ) {
    return node.type == AT::Block && node.nodes &&
           node.nodes->all( []( const AstNode &n ) { return is_stmt( n ); } );
}

bool is_comma_list_element( const AstNode &node ) {
    return is_expr( node ) || node.type == AT::DeclUninit || is_type( node );
}

bool is_call_ident( const AstNode &node ) {
    std::vector<String> built_in_functions = { "print", "read", "flush",
                                               "alloc", "alloc_array" };
    return node.type == AT::Ident ||
           ( node.type == AT::Token && node.tok->type == TT::Keyword &&
             std::find( built_in_functions.begin(), built_in_functions.end(),
                        node.tok->content ) != built_in_functions.end() );
}

Opt<std::reference_wrapper<AstNode>> unwrap_paren( AstNode &node ) {
    if ( node.type == AT::Paren ) {
        if ( node.nodes->empty() )
            return {};
        return unwrap_paren( node.nodes->itr().get() );
    }
    return node;
}

EagerContainer<AstNode> &unwrap_comma_list_nodes( AstNode &node ) {
    assert( node.nodes );
    if ( node.type != AT::Paren )
        return *node.nodes;
    if ( node.nodes->length() != 1 ||
         node.nodes->itr().get().type != AT::CommaList )
        return *node.nodes;
    return unwrap_comma_list_nodes( node.nodes->itr().get() );
}

AstNode make_merged_node( AT type, Token main_token,
                          const std::initializer_list<AstNode> &children ) {
    auto node = ast( type );
    node.nodes = std::make_shared<AstCont>();
    node.tok = main_token;
    for ( auto &c : children ) {
        node.nodes->put( c );
        node.tok->ifi = node.tok->ifi.merge( c.ifi );
    }
    node.ifi = node.tok->ifi;
    return node;
}


AstNode make_parser( CompilerState &state, EagerContainer<Token> &tokens ) {
    // Translation into raw AstNodes
    AstCont raw_nodes = tokens.map<AstNode>( [&state]( const Token &t ) {
        if ( t.type == TT::DecInteger ) {
            auto new_t = t;
            try {
                u64 value = stoull( new_t.content );
                if ( value == 1ull << 31 ) {
                    // Modulo arithmetic
                    new_t.content = "-" + t.content; // This should work...
                } else if ( value > 1ull << 31 ) {
                    make_error_msg(
                        state, "Decimal constant does not fit into 32 bit.",
                        t.ifi, RetCode::SemanticError );
                }
            } catch ( ... ) {
                make_error_msg( state, "Invalid decimal constant.", new_t.ifi,
                                RetCode::SemanticError );
            }
            return AstNode{ AT::IntConst, {}, new_t, {}, new_t.ifi };
        } else if ( t.type == TT::HexInteger ) {
            auto new_t = t;

            try {
                u64 value = stoull( new_t.content, 0, 16 );
                if ( value > 0xffffffffull ) {
                    make_error_msg(
                        state, "Hexadecimal constant does not fit into 32 bit.",
                        new_t.ifi, RetCode::SemanticError );
                }
                new_t.content = to_string( value ); // Weird, but works
            } catch ( ... ) {
                make_error_msg( state, "Invalid hexadecimal constant.", t.ifi,
                                RetCode::SemanticError );
            }
            return AstNode{ AT::IntConst, {}, new_t, {}, new_t.ifi };
            // TODO strings not yet implemented
            //}else if(t.type == TT::String){
            //    return AstNode{ AT::String, {}, t, {}, t.ifi };
        } else if ( t.type == TT::Keyword &&
                    ( t.content == "true" || t.content == "false" ) ) {
            return AstNode{ AT::BoolConst, {}, t, {}, t.ifi };
        } else if ( t.type == TT::Keyword && t.content == "NULL" ) {
            return AstNode{ AT::NullConst, {}, t, {}, t.ifi };
        } else if ( t.type == TT::Identifier ) {
            return AstNode{ AT::Ident, {}, t, {}, t.ifi };
        } else {
            return AstNode{ AT::Token, {}, t, {}, t.ifi };
        }
    } );

    // Parenthesis-pass
    std::function<AstNode( AstItr &, const String &, InFileInfo )> parse_parens;
    parse_parens = [&]( AstItr &itr, const String &closing_bracket,
                        InFileInfo open_at ) {
        AstNode ret;
        ret.nodes = std::make_shared<AstCont>();
        ret.ifi = open_at;
        while ( itr ) {
            if ( itr.match( ast_tok( TT::Operator, "(" ) ) ) {
                auto start_ifi = itr.consume().ifi;
                auto child = parse_parens( itr, ")", start_ifi );
                child.type = AT::Paren;
                ret.nodes->put( child );
            } else if ( itr.match( ast_tok( TT::Operator, "{" ) ) ) {
                auto start_ifi = itr.consume().ifi;
                auto child = parse_parens( itr, "}", start_ifi );
                child.type = AT::Block;
                ret.nodes->put( child );
            } else if ( itr.match( ast_tok( TT::Operator, "[" ) ) ) {
                auto start_ifi = itr.consume().ifi;
                auto child = parse_parens( itr, "]", start_ifi );
                child.type = AT::Bracket;
                ret.nodes->put( child );
            } else if ( !closing_bracket.empty() &&
                        itr.match(
                            ast_tok( TT::Operator, closing_bracket ) ) ) {
                itr.consume();
                return ret; // Complete this paren
            } else {
                auto n = itr.consume();
                if ( n.tok &&
                     ( n.tok->content == ")" || n.tok->content == "}" ||
                       n.tok->content == "]" ) ) {
                    make_error_msg( state, "Unmatched closing bracket.", n.ifi,
                                    RetCode::SyntaxError );
                    return ret;
                }
                ret.nodes->put( n );
            }
        }
        if ( !closing_bracket.empty() && !itr ) {
            make_error_msg( state, "Unmatched opening bracket.", open_at,
                            RetCode::SyntaxError );
        }
        return ret;
    };
    auto itr = raw_nodes.itr();
    AstNode root_node = parse_parens( itr, "", InFileInfo{} );
    root_node.type = AT::GlobalScope;

    // print_ast( root_node, "=After parens" ); // DEBUG

    // Type literals
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto type_ident = itr.get();
            if ( parent.type == AT::PrimType )
                return false;
            if ( type_ident.match( ast_tok( TT::Keyword, "int" ) ) ||
                 type_ident.match( ast_tok( TT::Keyword, "bool" ) ) ) {
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::PrimType, *type_ident.tok, {} );
                return true;
            }
            return false;
        } );

    // Other types
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto rhs = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( parent.type != AT::GlobalScope &&
                 itr.match( ast_tok( TT::Keyword, "struct" ),
                            ast( AT::Ident ) ) ) {
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::StructType, *rhs.tok, { rhs } );
                return true;
            }
            if ( is_type( lhs ) && rhs.type == AT::Token &&
                 rhs.tok->content == "*" ) {
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::PtrType, *lhs.tok, { lhs } );
                return true;
            }
            if ( is_type( lhs ) && rhs.type == AT::Bracket &&
                 rhs.nodes->empty() ) {
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::ArrayType, *lhs.tok, { lhs } );
                return true;
            }
            return false;
        } );

    // "break" & "continue"
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto kw = itr.get();
            if ( parent.type != AT::BreakStmt &&
                 kw.match( ast_tok( TT::Keyword, "break" ) ) ) {
                // Replace with merged token
                itr.get() = make_merged_node( AT::BreakStmt, *kw.tok, {} );
                return true;
            } else if ( parent.type != AT::ContinueStmt &&
                        kw.match( ast_tok( TT::Keyword, "continue" ) ) ) {
                // Replace with merged token
                itr.get() = make_merged_node( AT::ContinueStmt, *kw.tok, {} );
                return true;
            }
            return false;
        } );

    // Function call & access-operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto mid = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_call_ident( lhs ) && mid.type == AT::Paren ) {
                // Is "<ident> ( ... )"
                if ( parent.type == AT::GlobalScope )
                    return false; // Don't interpret function definitions as
                                  // calls

                // Remove one consumed element.
                itr.erase_self();

                if ( lhs.type == AT::Token ) {
                    if ( lhs.tok->content == "alloc" ||
                         lhs.tok->content == "alloc_array" ) {
                        // Is special function with type-parameter
                        itr.get() = make_merged_node( AT::AllocCall, *lhs.tok,
                                                      { mid } );
                        return true;
                    } else {
                        // Special functions were parsed like keywords.
                        lhs.type = AT::Ident;
                    }
                }

                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::Call, *lhs.tok, { lhs, mid } );
                return true;
            } else if ( is_expr( lhs ) && mid.type == AT::Token &&
                        mid.tok->content == "." && rhs.type == AT::Ident ) {
                // Is "<expr>.<ident>"
                // Remove two consumed elements.
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::FieldAccess, *lhs.tok, { lhs, rhs } );
                return true;
            } else if ( is_expr( lhs ) && mid.type == AT::Token &&
                        mid.tok->content == "->" && rhs.type == AT::Ident ) {
                // Is "<expr>.<ident>"
                // Remove two consumed elements.
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::IndirectAccess, *lhs.tok,
                                              { lhs, rhs } );
                return true;
            } else if ( is_expr( lhs ) && mid.type == AT::Bracket ) {
                // Is "<expr>.<ident>"
                // Remove one consumed elements.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::ArrayAccess, *lhs.tok, { lhs, mid } );
                return true;
            }
            return false;
        } );


    // Prefix operators
    apply_pass_recursively_from_right(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.skip( -1 ).get_or( ast( AT::None ) );
            auto op = itr.get();
            auto rhs = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( !is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "-" ) ) ||
                     op.match( ast_tok( TT::Operator, "!" ) ) ||
                     op.match( ast_tok( TT::Operator, "~" ) ) ) {
                    // Remove one consumed element.
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() = make_merged_node( AT::UniOp, *op.tok, { rhs } );
                    return true;
                } else if ( op.match( ast_tok( TT::Operator, "*" ) ) ) {
                    // Remove one consumed element.
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::PtrDeref, *op.tok, { rhs } );
                    return true;
                }
            }
            return false;
        } );

    // Generic parsing function for binary operators
    auto bin_op_parsing = [&]( const std::vector<String> &ops ) {
        apply_pass_recursively_from_left(
            state, *root_node.nodes, root_node,
            [ops]( CompilerState &state, AstItr &itr, const AstNode &parent ) {
                auto lhs = itr.get();
                auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
                auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
                if ( is_expr( lhs ) && is_expr( rhs ) ) {
                    for ( auto &o : ops ) {
                        if ( op.match( ast_tok( TT::Operator, o ) ) ) {
                            // Remove two consumed elements.
                            itr.erase_self();
                            itr.erase_self();
                            // Replace with merged token
                            itr.get() = make_merged_node( AT::BinOp, *op.tok,
                                                          { lhs, rhs } );
                            return true;
                        }
                    }
                }
                return false;
            } );
    };

    bin_op_parsing( { "*", "/", "%" } );
    bin_op_parsing( { "+", "-" } );
    bin_op_parsing( { "<<", ">>" } );
    bin_op_parsing( { "<", ">", "<=", ">=" } );
    bin_op_parsing( { "==", "!=" } );
    bin_op_parsing( { "&" } );
    bin_op_parsing( { "^" } );
    bin_op_parsing( { "|" } );
    bin_op_parsing( { "&&" } );
    bin_op_parsing( { "||" } );

    // "? :" operator
    apply_pass_recursively_from_right(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op0 = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto mid = itr.skip( 2 ).get_or( ast( AT::None ) );
            auto op1 = itr.skip( 3 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 4 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( mid ) && is_expr( rhs ) ) {
                if ( op0.match( ast_tok( TT::Operator, "?" ) ) &&
                     op1.match( ast_tok( TT::Operator, ":" ) ) ) {
                    // Remove four consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() = make_merged_node( AT::TernOp, *op0.tok,
                                                  { lhs, mid, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // Assignment operators
    apply_pass_recursively_from_right(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_lvalue( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "=" ) ) ||
                     op.match( ast_tok( TT::Operator, "+=" ) ) ||
                     op.match( ast_tok( TT::Operator, "-=" ) ) ||
                     op.match( ast_tok( TT::Operator, "*=" ) ) ||
                     op.match( ast_tok( TT::Operator, "/=" ) ) ||
                     op.match( ast_tok( TT::Operator, "%=" ) ) ||
                     op.match( ast_tok( TT::Operator, "&=" ) ) ||
                     op.match( ast_tok( TT::Operator, "^=" ) ) ||
                     op.match( ast_tok( TT::Operator, "|=" ) ) ||
                     op.match( ast_tok( TT::Operator, "<<=" ) ) ||
                     op.match( ast_tok( TT::Operator, ">>=" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::AsnOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // print_ast( root_node, "=After ops" ); // DEBUG

    // Statements
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto opr = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( is_type( lhs ) &&
                 itr.skip( 1 ).match( ast_with_tok(
                     AT::AsnOp, TT::Operator, "=", ast( AT::Ident ) ) ) ) {
                // Is "<type> <ident> = <expr>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::Decl, *lhs.tok, { lhs, opr } );
                return true;
            } else if ( is_type( lhs ) && opr.type == AT::Ident ) {
                // Is "<type> <ident>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::DeclUninit, *lhs.tok, { lhs, opr } );
                return true;
            } else if ( itr.match( ast_tok( TT::Keyword, "return" ) ) &&
                        is_expr( opr ) ) {
                // Is "ret <expr>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::Ret, *lhs.tok, { opr } );
                return true;
            }
            return false;
        } );

    // print_ast( root_node, "=After statements"  ); // DEBUG

    // Comma lists
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto comma = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( comma.match( ast_tok( TT::Operator, "," ) ) ) {
                if ( is_comma_list_element( lhs ) &&
                     is_comma_list_element( rhs ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() = make_merged_node( AT::CommaList, *comma.tok,
                                                  { lhs, rhs } );
                    return true;
                } else if ( lhs.type == AT::CommaList &&
                            is_comma_list_element( rhs ) ) {
                    // Remove two consumed elements.
                    itr.skip( 1 ).erase_self();
                    itr.skip( 1 ).erase_self();
                    // Merge into lhs
                    itr.get().nodes->put( rhs );
                    return true;
                }
            }
            return false;
        } );

    // Semicolons
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto stmt = itr.get();
            auto semicolon = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( itr.skip( 1 ).match( ast_tok( TT::Operator, ";" ) ) &&
                 is_stmt_body( stmt ) && parent.type != AT::GlobalScope ) {
                // Is "<stmt> ;"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::Stmt, *semicolon.tok, { stmt } );
                return true;
            }
            return false;
        } );

    // print_ast( root_node, "=After semicolons" ); // DEBUG

    // Control flow
    apply_pass_recursively_from_right(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            // If & If-else statements
            {
                auto if_kw = itr.get();
                auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
                auto true_block = itr.skip( 2 ).get_or( ast( AT::None ) );
                auto else_kw = itr.skip( 3 ).get_or( ast( AT::None ) );
                auto false_block = itr.skip( 4 ).get_or( ast( AT::None ) );
                if ( itr.match( ast_tok( TT::Keyword, "if" ),
                                ast( AT::Paren ) ) &&
                     !paren.nodes->empty() && is_stmt( true_block ) ) {
                    if ( itr.skip( 3 ).match(
                             ast_tok( TT::Keyword, "else" ) ) &&
                         is_stmt( false_block ) ) {
                        // Is "if (...) { ... } else { ... }"
                        // if-else
                        // Remove four consumed elements.
                        itr.erase_self();
                        itr.erase_self();
                        itr.erase_self();
                        itr.erase_self();
                        // Replace with merged token
                        itr.get() = make_merged_node(
                            AT::IfElseStmt, *if_kw.tok,
                            { paren, true_block, false_block } );
                    } else {
                        // Is "if (...) { ... }" without else
                        // Remove two consumed elements.
                        itr.erase_self();
                        itr.erase_self();
                        // Replace with merged token
                        itr.get() = make_merged_node( AT::IfStmt, *if_kw.tok,
                                                      { paren, true_block } );
                    }
                    return true;
                }
            }

            // While loops
            {
                auto if_kw = itr.get();
                auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
                auto block = itr.skip( 2 ).get_or( ast( AT::None ) );
                if ( itr.match( ast_tok( TT::Keyword, "while" ),
                                ast( AT::Paren ) ) &&
                     !paren.nodes->empty() && is_stmt( block ) ) {
                    // Is "while (...) { ... }"
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() = make_merged_node( AT::WhileLoop, *if_kw.tok,
                                                  { paren, block } );
                    return true;
                }
            }

            // For loops
            {
                auto none = ast( AT::None );
                auto for_kw = itr.get();
                auto paren = itr.skip( 1 ).get_or( none );
                auto block = itr.skip( 2 ).get_or( none );
                // For loops have kind of a messy syntax...
                if ( itr.match( ast_tok( TT::Keyword, "for" ),
                                ast( AT::Paren ) ) &&
                     paren.nodes->length() <= 4 && is_stmt( block ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();

                    // Replace with merged token
                    itr.get() = make_merged_node( AT::ForLoop, *for_kw.tok,
                                                  { paren, block } );
                    return true;
                }
            }
            return false;
        } );

    // Struct definitions
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( parent.type != AT::GlobalScope )
                return false; // Only allow struct definitions in global scope
            auto keyword = itr.get();
            auto head = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto block = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( itr.match( ast_tok( TT::Keyword, "struct" ), ast( AT::Ident ),
                            ast( AT::Block ), ast_tok( TT::Operator, ";" ) ) ) {
                // Is "struct <ident> { ... }"
                // Remove three consumed elements.
                itr.erase_self();
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::StructDef, *head.tok,
                                              { head, block } );
                return true;
            }
            return false;
        } );

    // Function definitions
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( parent.type != AT::GlobalScope )
                return false; // Only allow function definitions in global
                              // scope
            auto head = itr.get();
            auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto block = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( itr.match( ast( AT::DeclUninit ), ast( AT::Paren ),
                            ast( AT::Block ) ) &&
                 head.nodes->itr().get().type != AT::StructDef ) {
                // Is "int <ident> ( ... ) { ... }"
                // Remove two consumed elements.
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node(
                    AT::FunctionDef, *head.nodes->itr().skip().get().tok,
                    { head, paren, block } );
                return true;
            }
            return false;
        } );

    // print_ast( root_node, "=After parsing" ); // DEBUG

    // Check for expressions in parenthesis
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( itr.get().type == AT::Paren && parent.type != AT::ForLoop &&
                 parent.type != AT::FunctionDef && parent.type != AT::Call &&
                 parent.type != AT::AllocCall ) {
                auto node = itr.get();
                if ( !node.nodes || node.nodes->length() != 1 ||
                     !is_expr( node.nodes->itr().get() ) )
                    make_error_msg( state,
                                    "Expected expression in parenthesis.",
                                    node.ifi, RetCode::SyntaxError );
            }
            if ( itr.get().type == AT::Paren &&
                 parent.type != AT::FunctionDef && parent.type != AT::Call &&
                 parent.type != AT::AllocCall ) {
                auto node = itr.get();
                if ( node.nodes->any(
                         []( auto &&n ) { return n.type == AT::CommaList; } ) )
                    make_error_msg(
                        state,
                        "Comma-separated list only allowed in function "
                        "definitions and function calls.",
                        itr.get().ifi, RetCode::SyntaxError );
            }
            return false;
        } );

    // Check for correct node type of elements of comma list
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( itr.get().type == AT::Paren &&
                 itr.get().nodes->length() > 0 ) {
                auto node = itr.get();
                if ( parent.type == AT::FunctionDef &&
                     unwrap_comma_list_nodes( node ).any(
                         []( const AstNode &n ) {
                             return n.type != AT::DeclUninit;
                         } ) ) {
                    make_error_msg( state,
                                    "Expected parameter declaration in "
                                    "function definition.",
                                    node.ifi, RetCode::SyntaxError );
                }
                if ( parent.type == AT::FunctionDef &&
                     unwrap_comma_list_nodes( node ).any(
                         []( const AstNode &n ) {
                             return n.type == AT::StructType;
                         } ) ) {
                    make_error_msg( state, "Parameters must be small types.",
                                    node.ifi, RetCode::SemanticError );
                }
                if ( parent.type == AT::Call &&
                     unwrap_comma_list_nodes( node ).any(
                         []( const AstNode &n ) { return !is_expr( n ); } ) ) {
                    make_error_msg( state,
                                    "Expected parameter declaration in "
                                    "function call.",
                                    node.ifi, RetCode::SyntaxError );
                }
                if ( parent.type == AT::AllocCall ) {
                    auto &comma_list = unwrap_comma_list_nodes( node );
                    auto fn_ident = parent.tok->content;
                    if ( fn_ident == "alloc_array" &&
                         ( comma_list.length() != 2 ||
                           !is_type( comma_list.itr().get() ) ||
                           !is_expr( comma_list.itr().skip( 1 ).get() ) ) ) {
                        make_error_msg( state,
                                        "Expected parameters '(type, expr)' in "
                                        "'alloc_array'"
                                        "function call.",
                                        node.ifi, RetCode::SyntaxError );
                    } else if ( fn_ident == "alloc" &&
                                ( comma_list.length() != 1 ||
                                  !is_type( comma_list.itr().get() ) ) ) {
                        make_error_msg(
                            state,
                            "Expected parameters '(type)' in 'alloc'"
                            "function call.",
                            node.ifi, RetCode::SyntaxError );
                    }
                }
            }
            return false;
        } );

    // Check parenthesis in for-loops
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( itr.get().type == AT::Paren && parent.type == AT::ForLoop ) {
                auto nodes = itr.get().nodes->itr();

                // Handle empty init
                auto &init = nodes.get();
                if ( nodes.match( ast_tok( TT::Operator, ";" ) ) ) {
                    // Replace with empty block
                    auto tmp_tok = init.tok;
                    init = ast( AT::Block );
                    init.tok = tmp_tok;
                    init.ifi = init.tok->ifi;
                    init.nodes = std::make_shared<AstCont>();
                } else if ( !is_stmt( init ) ) {
                    make_error_msg( state,
                                    "Expected statement as init in for-loop.",
                                    init.ifi, RetCode::SyntaxError );
                    return false;
                }

                // Handle condition
                auto cond = nodes.skip( 1 ).get();
                if ( !is_expr( cond ) ) {
                    make_error_msg(
                        state, "Expected expression as condition in for-loop.",
                        cond.ifi, RetCode::SyntaxError );
                    return false;
                }

                // Erase second semicolon
                auto semicolon_tok = nodes.skip( 2 ).get().tok;
                nodes.skip( 2 ).erase_self();

                // Handle empty step
                if ( nodes.skip( 2 ).curr_not_valid() ) {
                    // Add empty block
                    auto step = ast( AT::Block );
                    step.tok = semicolon_tok;
                    step.ifi = step.tok->ifi;
                    step.nodes = std::make_shared<AstCont>();
                    itr.get().nodes->put( step );
                }

                // Check step conditions
                auto &step = nodes.skip( 2 ).get();
                if ( !is_stmt_body( step ) &&
                     ( step.type != AT::Block || !step.nodes->empty() ) ) {
                    make_error_msg( state,
                                    "Expected statement as step in for-loop.",
                                    step.ifi, RetCode::SyntaxError );
                    return false;
                }
                if ( step.type == AT::Decl || step.type == AT::DeclUninit ) {
                    make_error_msg( state,
                                    "Declaration is not allowed as step "
                                    "statement of for-loop head.",
                                    step.ifi, RetCode::SemanticError );
                    return false;
                }
            }
            return false;
        } );

    // Check for direct dereferencing NULL
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto none_node = ast( AT::None );
            if ( parent.type == AT::PtrDeref ||
                 parent.type == AT::FieldAccess ) {
                auto &node = itr.get();
                if ( node.type == AT::NullConst ||
                     ( node.type == AT::Paren &&
                       unwrap_paren( node ).value_or( none_node ).get().type ==
                           AT::NullConst ) ) {
                    make_error_msg(
                        state, "Directly de-referencing NULL is not allowed!",
                        node.ifi, RetCode::SyntaxError );
                }
            }
            return false;
        } );

    // Check for Statements in blocks
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( parent.type == AT::Block ) {
                auto node = itr.get();
                if ( !is_stmt( node ) )
                    make_error_msg( state,
                                    "Expected statement in block. Did you "
                                    "forget a semicolon?",
                                    node.ifi, RetCode::SyntaxError );
            }
            return false;
        } );

    // Check for Fields in struct definitions
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( itr.get().type == AT::Block && parent.type == AT::StructDef ) {
                auto block_itr = itr.get().nodes->itr();
                while ( block_itr ) {
                    if ( block_itr.get().type != AT::Stmt ||
                         block_itr.get().nodes->itr().get().type !=
                             AT::DeclUninit ) {
                        make_error_msg(
                            state, "Expected field in struct block.",
                            block_itr.get().ifi, RetCode::SyntaxError );
                    }
                    block_itr.skip_self( 1 );
                }
            }
            return false;
        } );

    // Check against orphan AT::None or AT::Token
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto node = itr.get();
            if ( node.type == AT::None || node.type == AT::Token )
                make_error_msg( state, "Failed to match syntax around token.",
                                node.ifi, RetCode::SyntaxError );
            return false;
        } );

    AstCont full_graph = *root_node.nodes;

    // Check for functions at global scope and main function
    bool found_main = false;
    auto global_itr = full_graph.itr();
    while ( global_itr ) {
        auto node = global_itr.get();
        if ( node.type != AT::FunctionDef && node.type != AT::StructDef ) {
            make_error_msg(
                state,
                "Expected function or struct definition in global scope.",
                node.ifi, RetCode::SyntaxError );
            return {};
        }
        if ( node.type == AT::FunctionDef ) {
            auto fn_children = node.nodes->itr();
            if ( fn_children.get().nodes->itr().skip( 1 ).get().tok->content ==
                     "main" &&
                 fn_children.get().tok->content == "int" &&
                 fn_children.skip( 1 ).get().nodes->empty() )
                found_main = true;
        }
        global_itr.skip_self( 1 );
    }
    if ( !found_main ) {
        make_error_msg( state,
                        "Expected global function with signature 'int main()'.",
                        InFileInfo{}, RetCode::SemanticError );
    }

// DEBUG
#ifndef NDEBUG
    if ( true ) {
        print_ast( root_node, "== AST ==" );
    }
#endif

    // Return root of AST
    return root_node;
}


} // namespace chc
