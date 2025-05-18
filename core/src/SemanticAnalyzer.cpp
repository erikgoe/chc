#include "../include/chc/Parser.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/AstNodeFacades.hpp"
#include "../include/chc/ParserUtils.hpp"
#include "../include/chc/Core.hpp"

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
        size_t sim = similarity( p.first );
        if ( sim > best_match_val ) {
            best_match = p.first;
            best_match_val = sim;
        }
    }
    return best_match.empty() ? Opt<String>{} : best_match;
}


void basic_semantic_checks( CompilerState &state, AstNode &root_node ) {
    std::unordered_map<String, SymbolDecl> symbol_map;
    SymbolId next_symbol = 1;
    std::deque<SymbolStackEntry> prev_stack;
    prev_stack.push_back( SymbolStackEntry{ next_symbol } );

    // Analyze all blocks
    std::function<void( AstNode & )> analyze_block;
    analyze_block = [&]( AstNode &node ) {
        auto &ps = prev_stack.back();

        auto match_new_symbol = [&]( const String &symbol, InFileInfo ifi ) {
            auto present_sym = symbol_map.find( symbol );
            if ( present_sym != symbol_map.end() ) {
                if ( present_sym->second.id >= ps.first_symbol ) {
                    // Already declared in this scop
                    make_error_msg(
                        state, "Symbol already defined in this scope.", ifi );
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

        // Check node
        if ( auto decl = DeclStmt( node ) ) {
            // Normal variable declaration
            analyze_block( decl.init );
            SymbolId new_id = match_new_symbol( decl.symbol, node.ifi );
            decl.update_symbol_id( node, new_id );
        } else if ( auto decl = DeclUninitStmt( node ) ) {
            // Normal variable declaration
            SymbolId new_id = match_new_symbol( decl.symbol, node.ifi );
            decl.update_symbol_id( node, new_id );
        } else if ( auto ident = Ident( node ) ) {
            if ( !ident.id ) {
                if ( symbol_map.find( ident.symbol ) != symbol_map.end() ) {
                    ident.update_symbol_id( node, symbol_map[ident.symbol].id );
                } else {
                    // Unknown symbol
                    make_error_msg( state, "Undefined identifier", node.ifi );
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
            prev_stack.push_back( SymbolStackEntry{ next_symbol } );

            auto itr = block.children.itr();
            while ( itr ) {
                analyze_block( itr.get() );
                itr.skip_self( 1 );
            }

            // Revert symbol table changes
            for ( auto &entry : prev_stack.back().symbols ) {
                symbol_map[entry.first] = entry.second;
            }
            for ( auto &entry : prev_stack.back().missing_symbols ) {
                symbol_map.erase( entry );
            }
            prev_stack.pop_back();

        } else if ( auto fn_def = FunctionDef( node ) ) {
            analyze_block( node.nodes->itr().skip( 2 ).get() );
            SymbolId new_id = match_new_symbol( fn_def.fn_symbol, node.ifi );
            fn_def.update_symbol_id( node, new_id );
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

    // Check if all symbols were matched
    if ( state.success ) {
        apply_pass_recursively_from_left(
            state, *root_node.nodes, root_node,
            []( CompilerState &state, AstItr &itr, const AstNode &parent ) {
                auto node = itr.get();
                if ( node.type == AT::Ident && !node.symbol_id.has_value() )
                    make_error_msg( state, "Could not calculate id for symbol.",
                                    node.ifi );
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
            } );
        if ( !found_return ) {
            make_error_msg( state, "No return statement found.", InFileInfo{} );
        }
    }
}
String name_of_type( AstNode::Type type );

void operator_transformation( CompilerState &state, AstNode &root_node ) {
    apply_pass_recursively_from_left(
        state, *root_node.nodes, root_node,
        [&]( CompilerState &state, AstItr &itr, const AstNode &parent ) {
            auto &node = itr.get();
            if ( auto simp = SimpStmt( node ) ) {
                // Translate combined simp into explicit operation.
                if ( simp.type == ArithType::Add ||
                     simp.type == ArithType::Sub ||
                     simp.type == ArithType::Mul ||
                     simp.type == ArithType::Div ||
                     simp.type == ArithType::Mod ) {
                    auto simp_tok = node.nodes->first().value().get().tok;

                    // Inner operation
                    auto inner_node = AstNode{ AT::BinOp };
                    inner_node.nodes = std::make_shared<AstCont>();
                    inner_node.nodes->put( simp.lvalue );
                    inner_node.nodes->put( simp.value );
                    inner_node.tok = simp_tok;
                    inner_node.tok->content =
                        inner_node.tok->content.substr( 0, 1 );
                    inner_node.ifi = inner_node.tok->ifi;

                    // Outer node
                    auto outer_node = AstNode{ AT::Simp };
                    outer_node.nodes = std::make_shared<AstCont>();
                    outer_node.nodes->put( simp.lvalue );
                    outer_node.nodes->put( inner_node );
                    outer_node.tok = simp_tok;
                    outer_node.tok->content =
                        outer_node.tok->content.substr( 1, 1 );
                    outer_node.ifi = outer_node.tok->ifi;

                    // Replace
                    itr.get().nodes->itr().get() = outer_node;
                }
            }
            // TODO check if future specifications also only allow simp as
            // statement and not as expression (which would not match here).
        } );


    // DEBUG
    if ( true ) {
        std::function<void( const AstNode &, size_t )> print_node;
        print_node = [&]( const AstNode &n, size_t indent ) {
            String str = String( indent, ' ' ) + name_of_type( n.type ) +
                         ( n.tok ? ": " + n.tok->content : "" );
            olog( str );
            if ( n.nodes )
                n.nodes->for_each(
                    [&]( auto &&sub ) { print_node( sub, indent + 1 ); } );
        };
        root_node.nodes->for_each( [&]( auto &&n ) { print_node( n, 0 ); } );
    }
}

} // namespace chc
