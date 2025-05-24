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
    case AstNode::Type::IntConst:
        return "IntConst";
    case AstNode::Type::BinOp:
        return "BinOp";
    case AstNode::Type::UniOp:
        return "UniOp";
    case AstNode::Type::Type:
        return "Type";
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
    case AstNode::Type::BoolConst:
        return "BoolConst";
    case AstNode::Type::TernOp:
        return "TernOp";
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

bool is_expr( const AstNode &node ) {
    return ( node.type == AT::Paren && !node.nodes->empty() &&
             is_expr( node.nodes->first()->get() ) ) ||
           node.type == AT::IntConst || node.type == AT::Ident ||
           node.type == AT::BinOp || node.type == AT::UniOp ||
           node.type == AT::BoolConst || node.type == AT::TernOp;
}

bool is_stmt_body( const AstNode &node ) {
    return node.type == AT::Decl || node.type == AT::DeclUninit ||
           node.type == AT::AsnOp || node.type == AT::Ret ||
           node.type == AT::BreakStmt || node.type == AT::ContinueStmt;
}

bool is_lvalue( const AstNode &node ) {
    return node.type == AT::Ident ||
           ( node.type == AT::Paren && node.nodes->not_empty() &&
             is_lvalue( *node.nodes->first() ) );
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
            } else if ( !closing_bracket.empty() &&
                        itr.match(
                            ast_tok( TT::Operator, closing_bracket ) ) ) {
                itr.consume();
                return ret; // Complete this paren
            } else {
                auto n = itr.consume();
                if ( n.tok &&
                     ( n.tok->content == ")" || n.tok->content == "}" ) ) {
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
            auto ident = itr.get();
            if ( parent.type == AT::Type )
                return false;
            if ( ident.match( ast_tok( TT::Keyword, "int" ) ) ||
                 ident.match( ast_tok( TT::Keyword, "bool" ) ) ) {
                // Replace with merged token
                itr.get() = make_merged_node( AT::Type, *ident.tok, { ident } );
                return true;
            }
            return false;
        } );

    // "break" & "continue"
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto ident = itr.get();
            if ( parent.type != AT::BreakStmt &&
                 ident.match( ast_tok( TT::Identifier, "break" ) ) ) {
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::BreakStmt, *ident.tok, { ident } );
                return true;
            } else if ( parent.type != AT::ContinueStmt &&
                        ident.match( ast_tok( TT::Identifier, "continue" ) ) ) {
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::ContinueStmt, *ident.tok, { ident } );
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
                }
            }
            return false;
        } );

    // TODO Create a generic function for binops

    // "*", "/", "%" operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "*" ) ) ||
                     op.match( ast_tok( TT::Operator, "/" ) ) ||
                     op.match( ast_tok( TT::Operator, "%" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "+", "-" operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "+" ) ) ||
                     op.match( ast_tok( TT::Operator, "-" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "<<", ">>" operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "<<" ) ) ||
                     op.match( ast_tok( TT::Operator, ">>" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "<", ">", "<=", ">=" operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "<" ) ) ||
                     op.match( ast_tok( TT::Operator, ">" ) ) ||
                     op.match( ast_tok( TT::Operator, "<=" ) ) ||
                     op.match( ast_tok( TT::Operator, ">=" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "==", "!=" operators
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "==" ) ) ||
                     op.match( ast_tok( TT::Operator, "!=" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "&" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "&" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "^" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "^" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "|" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "|" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "&&" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "&&" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "||" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "||" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::BinOp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // "? :" operator
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto op0 = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto mid = itr.skip( 2 ).get_or( ast( AT::None ) );
            auto op1 = itr.skip( 3 ).get_or( ast( AT::None ) );
            auto rhs = itr.skip( 4 ).get_or( ast( AT::None ) );
            if ( is_expr( lhs ) && is_expr( mid ) && is_expr( rhs ) ) {
                if ( op0.match( ast_tok( TT::Operator, "?" ) ) &&
                     op0.match( ast_tok( TT::Operator, ":" ) ) ) {
                    // Remove four consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::TernOp, *op0.tok, { lhs, rhs } );
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
            if ( itr.match( ast( AT::Type ),
                            ast_with_tok( AT::AsnOp, TT::Operator, "=",
                                          ast( AT::Ident ) ) ) ) {
                // Is "int <ident> = <expr>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::Decl, *lhs.tok, { opr } );
                return true;
            } else if ( itr.match( ast( AT::Type ), ast( AT::Ident ) ) ) {
                // Is "<type> <ident>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() =
                    make_merged_node( AT::DeclUninit, *lhs.tok, { opr } );
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

    // Semicolons
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto stmt = itr.get();
            auto semicolon = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( itr.skip( 1 ).match( ast_tok( TT::Operator, ";" ) ) &&
                 is_stmt_body( stmt ) ) {
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

    // print_ast( root_node, "=After semicolons"  ); // DEBUG

    // If statements
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto if_kw = itr.get();
            auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto true_block = itr.skip( 2 ).get_or( ast( AT::None ) );
            auto else_kw = itr.skip( 3 ).get_or( ast( AT::None ) );
            auto false_block = itr.skip( 4 ).get_or( ast( AT::None ) );
            if ( itr.match( ast_tok( TT::Keyword, "if" ), ast( AT::Paren ),
                            ast( AT::Block ) ) &&
                 !paren.nodes->empty() ) {
                if ( itr.skip( 3 ).match( ast_tok( TT::Keyword, "else" ),
                                          ast( AT::Block ) ) ) {
                    // Is "if (...) { ... } else { ... }"
                    // if-else
                    // Remove four consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::IfElseStmt, *if_kw.tok,
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
            return false;
        } );

    // While loop
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto if_kw = itr.get();
            auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto block = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( itr.match( ast_tok( TT::Keyword, "while" ), ast( AT::Paren ),
                            ast( AT::Block ) ) ) {
                // Is "while (...) { ... }"
                // Remove two consumed elements.
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::WhileLoop, *if_kw.tok,
                                              { paren, block } );
                return true;
            }
            return false;
        } );

    // For loop
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto for_kw = itr.get();
            auto none = ast( AT::None );
            auto paren = itr.skip( 1 ).get_or( none );
            auto block = itr.skip( 2 ).get_or( none );
            // For loops have kind of a messy syntax...
            if ( itr.match( ast_tok( TT::Keyword, "for" ), ast( AT::Paren ),
                            ast( AT::Block ) ) &&
                 paren.nodes->length() <= 3 ) {
                auto pb = paren.nodes->itr(); // paren body
                auto &pb0 = pb.get();
                auto &pb1 =
                    pb.skip( 1 ).get_or( none ); // Always the inner expr
                auto &pb2 = pb.skip( 2 ).get_or( none );
                bool valid = false;
                if ( pb1.type == AT::Stmt &&
                     is_expr( pb1.nodes->first()->get() ) ) {
                    pb1 = pb1.nodes->first()->get(); // remove semicolon
                    if ( pb.match( ast( AT::Stmt ), ast( AT::Stmt ) ) &&
                         is_stmt_body( pb2 ) ) {
                        // Is "for (<stmt>; <expr>; <stmt>) { ... }"
                        pb0 = pb0.nodes->first()->get(); // remove semicolon
                        valid = true;
                    } else if ( pb.match( ast_tok( TT::Operator, ";" ),
                                          ast( AT::Stmt ) ) &&
                                is_stmt_body( pb2 ) ) {
                        // Is "for (; <expr>; <stmt>) { ... }"
                        // First argument is equivalent to an empty block
                        pb0 = ast( AT::Block );
                        // Dummy container is needed here
                        pb0.nodes = std::make_shared<AstCont>();
                        valid = true;
                    } else if ( pb.match( ast( AT::Stmt ), ast( AT::Stmt ) ) ) {
                        // Is "for (<stmt>; <expr>;) { ... }"
                        pb0 = pb0.nodes->first()->get(); // remove semicolon
                        // Add a third parameter as empty block
                        auto third_param = ast( AT::Block );
                        // Dummy container is needed here
                        pb0.nodes = std::make_shared<AstCont>();
                        paren.nodes->put( third_param );
                        valid = true;
                    } else if ( pb.match( ast_tok( TT::Operator, ";" ),
                                          ast( AT::Stmt ) ) ) {
                        // Is "for (; <expr>;) { ... }"
                        // First argument is equivalent to an empty block
                        pb0 = ast( AT::Block );
                        // Dummy container is needed here
                        pb0.nodes = std::make_shared<AstCont>();
                        // Add a third parameter as empty block
                        auto third_param = ast( AT::Block );
                        // Dummy container is needed here
                        pb0.nodes = std::make_shared<AstCont>();
                        paren.nodes->put( third_param );
                        valid = true;
                    }
                }
                auto step_type = paren.nodes->itr().skip( 2 ).get().type;
                if ( valid && step_type != AT::Decl &&
                     step_type != AT::DeclUninit ) {
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

    // Function definitions
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( parent.type != AT::GlobalScope )
                return false; // Only allow function definitions in global scope
            auto head = itr.get();
            auto paren = itr.skip( 1 ).get_or( ast( AT::None ) );
            auto block = itr.skip( 2 ).get_or( ast( AT::None ) );
            if ( itr.match( ast( AT::DeclUninit ), ast( AT::Paren ),
                            ast( AT::Block ) ) &&
                 paren.nodes->empty() ) {
                // Is "int <ident> ( ... ) { ... }"
                // Remove two consumed elements.
                itr.erase_self();
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::FunctionDef,
                                              *head.nodes->first()->get().tok,
                                              { head, paren, block } );
                return true;
            }
            return false;
        } );

    print_ast( root_node, "=After parsing" ); // DEBUG

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

    // Check for global main (and nothing else)
    if ( full_graph.length() != 1 || !full_graph.first().has_value() ||
         full_graph.first()->get().type != AT::FunctionDef ) {
        make_error_msg( state, "Expected single function at global scope.",
                        InFileInfo{}, RetCode::SyntaxError );
        return {};
    }
    auto main_fn = full_graph.first()->get();
    if ( main_fn.nodes->first()->get().nodes->first()->get().tok->content !=
         "main" ) {
        make_error_msg( state, "Expected global function with name 'main'.",
                        main_fn.ifi, RetCode::SyntaxError );
        return {};
    }

    // DEBUG
#ifndef NDEBUG
    if ( true ) {
        full_graph.for_each( [&]( auto &&n ) { print_ast( n, "== AST ==" ); } );
    }
#endif

    // Return root of AST
    return root_node;
}


} // namespace chc
