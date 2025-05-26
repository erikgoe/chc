#include "../include/chc/Parser.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/AstNodeFacades.hpp"
#include "../include/chc/ParserUtils.hpp"
#include "../include/chc/Core.hpp"
#include "../include/chc/Mir.hpp"

namespace chc {

using AT = AstNode::Type;
using AstItr = EagerContainer<AstNode>::Iterator;

using namespace AstNodeFacades;

struct SymbolDecl {
    SymbolId id;
    InFileInfo ifi;
};
struct SymbolStackEntry {
    SymbolId first_symbol;
    std::vector<std::pair<String, SymbolDecl>> symbols;
    std::vector<String> missing_symbols;
};

Opt<String> find_similar_symbol(
    const String &to_search,
    const std::unordered_map<String, SymbolDecl> &map ) {
    if ( to_search.size() < 3 )
        return {}; // Minimum useful length for the algorithm

    // Generate similarity info.
    std::vector<String> search_stat;
    for ( size_t i = 0; i < to_search.size() - 1; i++ ) {
        search_stat.push_back( to_search.substr( i, 2 ) );
    }

    auto similarity = [&]( const String &ref ) -> ssize_t {
        ssize_t sim = 0;
        size_t stat_ctr = 0;
        for ( size_t i = 0; i < ref.size() - 1; i++ ) {
            String p = ref.substr( i, 2 );
            auto itr = std::find( search_stat.begin() + stat_ctr,
                                  search_stat.end(), p );
            if ( itr != search_stat.end() ) {
                // Found something
                stat_ctr = itr - search_stat.begin() + 1;
                sim++;
            } else {
                sim--;
            }
        }
        // TODO check this algorithm
        return sim;
    };
    String best_match = "";
    ssize_t best_match_val = 0;

    for ( auto &p : map ) {
        ssize_t sim = similarity( p.first );
        if ( sim > best_match_val ) {
            best_match = p.first;
            best_match_val = sim;
        }
    }
    return best_match.empty() ? Opt<String>{} : best_match;
}


void analyze_symbol_definitions( CompilerState &state, AstNode &root_node ) {
    std::unordered_map<String, SymbolDecl> symbol_map;
    SymbolId next_symbol = 1;
    std::deque<SymbolStackEntry> prev_stack;
    prev_stack.push_back( SymbolStackEntry{ next_symbol } );

    // Analyze all blocks
    std::function<void( AstNode & )> analyze_block;
    analyze_block = [&]( AstNode &node ) {
        auto &ps = prev_stack.back();

        // Utility functions
        auto match_new_symbol = [&]( const String &symbol, InFileInfo ifi ) {
            auto present_sym = symbol_map.find( symbol );
            if ( present_sym != symbol_map.end() ) {
                if ( present_sym->second.id >= ps.first_symbol ) {
                    // Already declared in this scop
                    make_error_msg( state,
                                    "Symbol already defined in this scope.",
                                    ifi, RetCode::SemanticError );
                    make_info_msg( state, "Previously defined here.",
                                   present_sym->second.ifi );
                } else {
                    // Copy existing symbols in buffer and create a new
                    // symbol
                    ps.symbols.push_back( *present_sym );
                    symbol_map[symbol] = SymbolDecl{ next_symbol++, ifi };
                }
            } else {
                // New symbol
                symbol_map[symbol] = SymbolDecl{ next_symbol++, ifi };
                ps.missing_symbols.push_back( symbol );
            }
            return next_symbol - 1; // Returns the new symbol's id (if any)
        };
        auto push_var_stack = [&]() {
            prev_stack.push_back( SymbolStackEntry{ next_symbol } );
        };
        auto pop_var_stack = [&]() {
            for ( auto &entry : prev_stack.back().symbols ) {
                symbol_map[entry.first] = entry.second;
            }
            for ( auto &entry : prev_stack.back().missing_symbols ) {
                symbol_map.erase( entry );
            }
            prev_stack.pop_back();
        };

        // Check node
        if ( auto decl = Decl( node ) ) {
            // Normal variable declaration
            analyze_block( *decl.init );
            SymbolId new_id = match_new_symbol( decl.symbol, node.ifi );
            *decl.symbol_id = new_id;
        } else if ( auto decl = DeclUninit( node ) ) {
            // Normal variable declaration
            SymbolId new_id = match_new_symbol( decl.symbol, node.ifi );
            *decl.symbol_id = new_id;
        } else if ( auto ident = Ident( node ) ) {
            if ( !*ident.id ) {
                if ( symbol_map.find( ident.symbol ) != symbol_map.end() ) {
                    node.symbol_id = symbol_map[ident.symbol].id;
                } else {
                    // Unknown symbol
                    make_error_msg( state, "Undefined identifier", node.ifi,
                                    RetCode::SemanticError );
                    auto sim = find_similar_symbol( ident.symbol, symbol_map );
                    if ( sim ) {
                        make_info_msg( state,
                                       "Do you mean '" + sim.value() +
                                           "' instead? Defined here.",
                                       symbol_map[sim.value()].ifi );
                    }
                }
            }
        } else if ( auto block = Block( node ) ) {
            // Blocks create new scopes
            push_var_stack();

            auto itr = block.children->itr();
            while ( itr ) {
                analyze_block( itr.get() );
                itr.skip_self( 1 );
            }

            pop_var_stack();
        } else if ( auto if_stmt = IfStmt( node ) ) {
            // New scope in true_stmt
            analyze_block( *if_stmt.cond );
            push_var_stack();
            analyze_block( if_stmt.true_stmt );
            if_stmt.write_back_true_stmt( node );
            pop_var_stack();

            // Optionally the same for the false_statement
            if ( if_stmt.false_stmt.type != AT::None ) {
                push_var_stack();
                analyze_block( if_stmt.false_stmt );
                if_stmt.write_back_false_stmt( node );
                pop_var_stack();
            }
        } else if ( auto while_loop = WhileLoop( node ) ) {
            // New scope in while body
            analyze_block( *while_loop.cond );
            push_var_stack();
            analyze_block( *while_loop.body );
            pop_var_stack();
        } else if ( auto for_loop = ForLoop( node ) ) {
            // For loops actually have two scopes
            push_var_stack();
            analyze_block( *for_loop.init );
            push_var_stack();
            analyze_block( *for_loop.cond );
            analyze_block( *for_loop.body );
            analyze_block( *for_loop.step );
            pop_var_stack();
            pop_var_stack();
        } else if ( auto fn_def = FunctionDef( node ) ) {
            analyze_block( node.nodes->itr().skip( 2 ).get() );
            SymbolId new_id = match_new_symbol( fn_def.fn_symbol, node.ifi );
            *fn_def.fn_symbol_id = new_id;
        } else {
            // Normal nodes
            // Simply recurse into subnodes
            if ( node.nodes ) {
                auto itr = node.nodes->itr();
                while ( itr ) {
                    analyze_block( itr.get() );
                    itr.skip_self( 1 );
                }
            }
        }
    };

    // Iterate root node
    analyze_block( root_node );
}

void basic_semantic_checks( CompilerState &state, AstNode &root_node ) {
    // First analyze symbols
    analyze_symbol_definitions( state, root_node );

    // Check if all symbols were matched
    if ( state.success ) {
        apply_pass_recursively_from_left(
            state, *root_node.nodes, root_node,
            []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
                auto node = itr.get();
                if ( node.type == AT::Ident && !node.symbol_id.has_value() )
                    make_error_msg( state, "Could not calculate id for symbol.",
                                    node.ifi, RetCode::SemanticError );
                return false;
            } );
    }

    // Check if return statement exists
    if ( state.success ) {
        bool found_return = false;
        apply_pass_recursively_from_left(
            state, *root_node.nodes, root_node,
            [&]( CompilerState &state, AstItr &itr, const AstNode &parent ) {
                auto node = itr.get();
                if ( node.type == AT::Ret )
                    found_return = true;
                return false;
            } );
        if ( !found_return ) {
            make_error_msg( state, "No return statement found.", InFileInfo{},
                            RetCode::SemanticError );
        }
    }
}

void print_ast( const AstNode &root, const String &title );

void operator_transformation( CompilerState &state, AstNode &root_node ) {
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        [&]( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto &node = itr.get();
            // TODO check if future specifications also only allow asnop as
            // statement and not as expression (which would not match here).
            if ( auto asnop = AsnOp( node );
                 asnop && parent.type != AT::Decl ) {
                // Translate combined asnop into explicit operation.
                if ( asnop.type == ArithType::Add ||
                     asnop.type == ArithType::Sub ||
                     asnop.type == ArithType::Mul ||
                     asnop.type == ArithType::Div ||
                     asnop.type == ArithType::Mod ||
                     asnop.type == ArithType::BAnd ||
                     asnop.type == ArithType::BOr ||
                     asnop.type == ArithType::BXor ||
                     asnop.type == ArithType::Shl ||
                     asnop.type == ArithType::Shr ) {
                    auto asnop_tok = node.tok;

                    // Inner operation
                    auto inner_node = AstNode{ AT::BinOp };
                    inner_node.nodes = std::make_shared<AstCont>();
                    inner_node.nodes->put( *asnop.lvalue );
                    inner_node.nodes->put( *asnop.value );
                    inner_node.tok = asnop_tok;
                    inner_node.tok->content = inner_node.tok->content.substr(
                        0, inner_node.tok->content.length() - 1 );
                    inner_node.ifi = inner_node.tok->ifi;

                    // Outer node
                    auto outer_node = AstNode{ AT::AsnOp };
                    outer_node.nodes = std::make_shared<AstCont>();
                    outer_node.nodes->put( *asnop.lvalue );
                    outer_node.nodes->put( inner_node );
                    outer_node.tok = asnop_tok;
                    outer_node.tok->content = outer_node.tok->content.substr(
                        outer_node.tok->content.length() - 1 );
                    outer_node.ifi = outer_node.tok->ifi;

                    // Replace
                    node = outer_node;
                    return true;
                }
            } else if ( auto uni_op = UniOp( node ) ) {
                if ( uni_op.type == ArithType::LNot ) {
                    // Translate logical not into ternary operator.
                    auto const_true = AstNode{ AT::BoolConst,
                                               {},
                                               Token{ Token::Type::Keyword,
                                                      "true", node.ifi },
                                               {},
                                               node.ifi };
                    auto const_false = AstNode{ AT::BoolConst,
                                                {},
                                                Token{ Token::Type::Keyword,
                                                       "false", node.ifi },
                                                {},
                                                node.ifi };

                    // Ternary operator
                    auto tern_op = AstNode{ AT::TernOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( *uni_op.rhs );
                    tern_op.nodes->put( const_false );
                    tern_op.nodes->put( const_true );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "?";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                } else if ( uni_op.type == ArithType::BInv ) {
                    // Translate bit inversion into Xor with all 1s
                    auto const_ones =
                        AstNode{ AT::IntConst,
                                 {},
                                 Token{ Token::Type::Keyword, "-1", node.ifi },
                                 {},
                                 node.ifi };

                    // Ternary operator
                    auto tern_op = AstNode{ AT::BinOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( *uni_op.rhs );
                    tern_op.nodes->put( const_ones );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "^";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                } else if ( uni_op.type == ArithType::Neg ) {
                    // Translate integer negation into 0-x
                    auto const_zero =
                        AstNode{ AT::IntConst,
                                 {},
                                 Token{ Token::Type::Keyword, "0", node.ifi },
                                 {},
                                 node.ifi };

                    // Ternary operator
                    auto tern_op = AstNode{ AT::BinOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( const_zero );
                    tern_op.nodes->put( *uni_op.rhs );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "-";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                }
            } else if ( auto bin_op = BinOp( node ) ) {
                // Bool constant
                auto const_true =
                    AstNode{ AT::BoolConst,
                             {},
                             Token{ Token::Type::Keyword, "true", node.ifi },
                             {},
                             node.ifi };
                auto const_false =
                    AstNode{ AT::BoolConst,
                             {},
                             Token{ Token::Type::Keyword, "false", node.ifi },
                             {},
                             node.ifi };
                if ( bin_op.type == ArithType::LAnd ) {
                    // Translate short-circuit "&&" to ternary operator.
                    auto tern_op = AstNode{ AT::TernOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( *bin_op.lhs );
                    tern_op.nodes->put( *bin_op.rhs );
                    tern_op.nodes->put( const_false );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "?";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                } else if ( bin_op.type == ArithType::LOr ) {
                    // Translate short-circuit "||" to ternary operator.
                    auto tern_op = AstNode{ AT::TernOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( *bin_op.lhs );
                    tern_op.nodes->put( const_true );
                    tern_op.nodes->put( *bin_op.rhs );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "?";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                } else if ( bin_op.type == ArithType::UnEq ||
                            bin_op.type == ArithType::Greater ||
                            bin_op.type == ArithType::GreaterEq ) {
                    // Translate comparison operators into corresponding
                    // inverted versions.

                    // Inverted operation
                    if ( bin_op.type == ArithType::UnEq ) {
                        bin_op.update_arith_type( node, ArithType::Eq );
                    } else if ( bin_op.type == ArithType::Greater ) {
                        bin_op.update_arith_type( node, ArithType::LessEq );
                    } else if ( bin_op.type == ArithType::GreaterEq ) {
                        bin_op.update_arith_type( node, ArithType::Less );
                    }

                    auto tern_op = AstNode{ AT::TernOp };
                    tern_op.nodes = std::make_shared<AstCont>();
                    tern_op.nodes->put( node );
                    tern_op.nodes->put( const_false );
                    tern_op.nodes->put( const_true );
                    tern_op.tok = node.tok;
                    tern_op.tok->content = "?";
                    tern_op.ifi = tern_op.tok->ifi;

                    // Replace
                    itr.get() = tern_op;
                    return true;
                }
            }
            return false;
        } );


    // DEBUG
#ifndef NDEBUG
    if ( false ) {
        std::function<void( const AstNode &, size_t )> print_node;
        print_node = [&]( const AstNode &n, size_t indent ) {
            String str = String( indent, ' ' ) + n.get_type_name() +
                         ( n.tok ? ": " + n.tok->content : "" );
            olog( str );
            if ( n.nodes )
                n.nodes->for_each(
                    [&]( auto &&sub ) { print_node( sub, indent + 1 ); } );
        };
        root_node.nodes->for_each(
            [&]( auto &&n ) { print_ast( n, "== TRANSFORMED AST ==" ); } );
    }
#endif
}

using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;

void use_before_init_check( CompilerState &state, Mir &mir ) {
    std::deque<EagerContainer<Mir::MirInstr>::Iterator> to_check;
    std::set<EagerContainer<Mir::MirInstr>::Iterator> already_checked;
    std::set<VarId> defs;
    to_check.push_back( mir.instrs.itr() );

    auto add_next_line = [&]( EagerContainer<Mir::MirInstr>::Iterator itr,
                              InFileInfo ifi ) {
        if ( itr.curr_not_valid() ) {
            make_error_msg( state, "Missing return statement.", ifi,
                            RetCode::SemanticError );
            return;
        }
        to_check.push_back( itr );
    };

    // Check only reachable code
    while ( !to_check.empty() ) {
        auto instr_itr = to_check.front();
        to_check.pop_front();
        if ( already_checked.find( instr_itr ) != already_checked.end() )
            continue;
        already_checked.insert( instr_itr );

        // Add successors
        if ( instr_itr.get().type == MT::Jmp ) {
            to_check.push_back( *mir.jump_table[instr_itr.get().imm] );
        } else if ( instr_itr.get().type == MT::JZero ) {
            add_next_line( instr_itr.skip( 1 ), instr_itr.get().ifi );
            to_check.push_back( *mir.jump_table[instr_itr.get().imm] );
        } else if ( instr_itr.get().type != MT::Ret ) {
            add_next_line( instr_itr.skip( 1 ), instr_itr.get().ifi );
        }

        // Check variable usage
        auto &instr = instr_itr.get();
        if ( instr.p0 != 0 && defs.find( instr.p0 ) == defs.end() ) {
            // p0 not defined
            make_error_msg( state, "Using undefined variable", instr.ifi,
                            RetCode::SemanticError );
        }
        if ( instr.p1 != 0 && defs.find( instr.p1 ) == defs.end() ) {
            // p0 not defined
            make_error_msg( state, "Using undefined variable", instr.ifi,
                            RetCode::SemanticError );
        }
        if ( instr.result != 0 ) {
            defs.insert( instr.result );
        }
    }
}

void type_checking( CompilerState &state, Mir &mir ) {
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        if ( instr.type == MT::Const ) {
            if ( mir.type_of( instr.result ) != 0 &&
                 mir.type_of( instr.result ) != instr.result_type ) {
                make_error_msg( state, "Type mismatch", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            mir.type_of( instr.result ) = instr.result_type;
        } else if ( instr.type == MT::BinOp ) {
            if ( mir.type_of( instr.p0 ) == 0 ||
                 mir.type_of( instr.p0 ) == 0 ) {
                make_error_msg( state, "Variable untyped", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            if ( mir.type_of( instr.p0 ) != mir.type_of( instr.p1 ) ) {
                make_error_msg( state, "Type mismatch", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            if ( ( has_only_int_params( instr.subtype ) &&
                   mir.type_of( instr.p0 ) != Mir::TYPE_INT ) ||
                 ( has_only_int_ret( instr.subtype ) &&
                   mir.type_of( instr.result ) != 0 &&
                   mir.type_of( instr.result ) != Mir::TYPE_INT ) ) {
                make_error_msg( state, "Expected type 'int'", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            if ( ( has_only_bool_params( instr.subtype ) &&
                   mir.type_of( instr.p0 ) != Mir::TYPE_BOOL ) ||
                 ( has_only_bool_ret( instr.subtype ) &&
                   mir.type_of( instr.result ) != 0 &&
                   mir.type_of( instr.result ) != Mir::TYPE_BOOL ) ) {
                make_error_msg( state, "Expected type 'bool'", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            if ( has_only_int_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.TYPE_INT;
            } else if ( has_only_bool_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.TYPE_BOOL;
            } else if ( has_any_type_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.type_of( instr.p0 );
            }
        } else if ( instr.type == MT::Ret ) {
            if ( mir.type_of( instr.p0 ) != Mir::TYPE_INT ) {
                // TODO generalize as soon as functions can have any type
                make_error_msg( state, "Expected return type 'int'", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
        } else if ( instr.type == MT::Mov ) {
            if ( instr.p0 != 0 ) {
                if ( mir.type_of( instr.p0 ) == 0 ) {
                    make_error_msg( state, "Variable untyped", instr.ifi,
                                    RetCode::SemanticError );
                    return;
                }
                if ( mir.type_of( instr.result ) != 0 &&
                     mir.type_of( instr.result ) != mir.type_of( instr.p0 ) ) {
                    make_error_msg( state, "Type mismatch", instr.ifi,
                                    RetCode::SemanticError );
                    return;
                }
                mir.type_of( instr.result ) = mir.type_of( instr.p0 );
            }
        }
    } );
}

} // namespace chc
