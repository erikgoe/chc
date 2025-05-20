#include "../include/chc/Parser.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/ParserUtils.hpp"

namespace chc {

String name_of_type( AstNode::Type type ) {
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
    case AstNode::Type::Simp:
        return "Simp";
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
    default:
        return "Unknown";
    }
}

void print_ast( const AstNode &root, const String &title = "" ) {
    if ( !title.empty() )
        log( title );
    std::function<void( const AstNode &, size_t )> print_node;
    print_node = [&]( const AstNode &n, size_t indent ) {
        String str = String( indent, ' ' ) + name_of_type( n.type ) +
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
    return ( node.type == AT::Paren &&
             ( node.nodes->empty() ||
               is_expr( node.nodes->first()->get() ) ) ) ||
           node.type == AT::IntConst || node.type == AT::Ident ||
           node.type == AT::BinOp || node.type == AT::UniOp;
}

bool is_stmt_body( const AstNode &node ) {
    return node.type == AT::Decl || node.type == AT::DeclUninit ||
           node.type == AT::Simp || node.type == AT::Ret;
}

bool is_lvalue( const AstNode &node ) {
    return node.type == AT::Ident ||
           ( node.type == AT::Paren && node.nodes->not_empty() &&
             is_lvalue( *node.nodes->first() ) );
}

bool is_function_body( const AstNode &node ) {
    return node.type == AT::Block && node.nodes &&
           node.nodes->all(
               []( const AstNode &n ) { return n.type == AT::Stmt; } );
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

    // Prefix "-" operator
    apply_pass_recursively_from_right(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.skip( -1 ).get_or( ast( AT::None ) );
            auto op = itr.get();
            auto rhs = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( !is_expr( lhs ) && is_expr( rhs ) ) {
                if ( op.match( ast_tok( TT::Operator, "-" ) ) ) {
                    // Remove one consumed element.
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() = make_merged_node( AT::UniOp, *op.tok, { rhs } );
                    return true;
                }
            }
            return false;
        } );

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
                     op.match( ast_tok( TT::Operator, "%=" ) ) ) {
                    // Remove two consumed elements.
                    itr.erase_self();
                    itr.erase_self();
                    // Replace with merged token
                    itr.get() =
                        make_merged_node( AT::Simp, *op.tok, { lhs, rhs } );
                    return true;
                }
            }
            return false;
        } );

    // print_ast( root_node, "After ops"  ); // DEBUG

    // Statements
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto lhs = itr.get();
            auto opr = itr.skip( 1 ).get_or( ast( AT::None ) );
            if ( itr.match( ast_tok( TT::Keyword, "int" ),
                            ast_with_tok( AT::Simp, TT::Operator, "=",
                                          ast( AT::Ident ) ) ) ) {
                // Is "int <ident> = <expr>"
                // Remove one consumed element.
                itr.erase_self();
                // Replace with merged token
                itr.get() = make_merged_node( AT::Decl, *lhs.tok, { opr } );
                return true;
            } else if ( itr.match( ast_tok( TT::Keyword, "int" ),
                                   ast( AT::Ident ) ) ) {
                // Is "int <ident>"
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

    // print_ast( root_node, "After statements"  ); // DEBUG

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

    // print_ast( root_node, "After semicolons"  ); // DEBUG

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

    // Check for AT::Stmt in blocks
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            if ( parent.type == AT::Block ) {
                auto node = itr.get();
                if ( node.type != AT::Stmt )
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
