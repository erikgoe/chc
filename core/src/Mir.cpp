#include "../include/chc/Mir.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/AstNodeFacades.hpp"

namespace chc {

using AT = AstNode::Type;
using AstItr = EagerContainer<AstNode>::Iterator;
using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;
using RegId = Mir::RegId;


using namespace AstNodeFacades;

/// An efficient representation of an inference graph (runtime-wise; not
/// memory-wise).
class InferenceGraph {
    std::vector<bool> cont;
    size_t var_count = 0;

public:
    InferenceGraph( size_t var_count ) : var_count( var_count ) {
        cont.resize( var_count * var_count );
    }
    void set_inference( VarId a, VarId b ) {
        cont[a * var_count + b] = true;
        cont[b * var_count + a] = true;
    }
    size_t neighbor_count( VarId a ) {
        size_t count = 0;
        for ( size_t i = a * var_count; i < a * ( var_count + 1 ); i++ ) {
            if ( cont[i] )
                ++count;
        }
        return count;
    }
    void for_all_neighbors( VarId a,
                            std::function<void( VarId neighbor )> func ) const {
        for ( size_t i = a * var_count; i < a * ( var_count + 1 ); i++ ) {
            if ( cont[i] )
                func( i - a * var_count );
        }
    }
};

String name_of_instr( Mir::MirInstr::Type type ) {
    switch ( type ) {
    case Mir::MirInstr::Type::None:
        return "None";
    case Mir::MirInstr::Type::Nop:
        return "Nop";
    case Mir::MirInstr::Type::Const:
        return "Const";
    case Mir::MirInstr::Type::Mov:
        return "Mov";
    case Mir::MirInstr::Type::Add:
        return "Add";
    case Mir::MirInstr::Type::Sub:
        return "Sub";
    case Mir::MirInstr::Type::Mul:
        return "Mul";
    case Mir::MirInstr::Type::Div:
        return "Div";
    case Mir::MirInstr::Type::Mod:
        return "Mod";
    case Mir::MirInstr::Type::Ret:
        return "Ret";
    default:
        return "Unknown";
    }
}

SymbolId symbol_id_of_lvalue( CompilerState &state, const AstNode &n ) {
    if ( n.type == AT::Ident ) {
        return n.symbol_id.value();
    } else if ( n.type == AT::Paren && n.nodes->not_empty() ) {
        return symbol_id_of_lvalue( state, n.nodes->first()->get() );
    } else {
        make_error_msg(
            state,
            "LValue is of unknow type. This is probably in compiler bug.",
            n.ifi, RetCode::InternalError );
        return 0;
    }
}

void write_mir_instr( CompilerState &state, Mir &mir, AstNode &node,
                      Mir::VarId into_var ) {
    if ( auto fn_def = FunctionDef( node ) ) {
        // TODO params
        auto itr = fn_def.stmts.itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto ret = RetStmt( node ) ) {
        VarId tmp = mir.next_var++;
        write_mir_instr( state, mir, ret.value, tmp );
        mir.instrs.put( MI{ MT::Ret, 0, tmp, 0, 0, node.ifi } );
    } else if ( auto decl = DeclStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id.value()] = variable;
        write_mir_instr( state, mir, decl.init, variable );
    } else if ( auto decl = DeclUninitStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id.value()] = variable;
    } else if ( auto stmt = AsnOpStmt( node ) ) {
        assert( stmt.type == ArithType::None ); // Should already be handled in
                                                // operator_transformation()
        VarId variable = mir.var_map[symbol_id_of_lvalue( state, stmt.lvalue )];
        write_mir_instr( state, mir, stmt.value, variable );
    } else if ( auto ident = Ident( node ) ) {
        VarId variable = mir.var_map[ident.id.value()];
        mir.instrs.put( MI{ MT::Mov, into_var, variable, 0, 0, node.ifi } );
    } else if ( auto paren = Paren( node ) ) {
        if ( paren.children.length() != 1 ) {
            make_error_msg( state, "Expected only one element in parenthesis.",
                            node.ifi, RetCode::SyntaxError );
            return;
        }
        auto child = paren.children.first()->get();
        write_mir_instr( state, mir, child, into_var );
    } else if ( auto block = Block( node ) ) {
        auto itr = block.children.itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto int_const = IntConst( node ) ) {
        mir.instrs.put(
            MI{ MT::Const, into_var, 0, 0, int_const.value, node.ifi } );
    } else if ( auto bin_op = BinOp( node ) ) {
        VarId tmp_lhs = mir.next_var++;
        VarId tmp_rhs = mir.next_var++;
        write_mir_instr( state, mir, bin_op.lhs, tmp_lhs );
        write_mir_instr( state, mir, bin_op.rhs, tmp_rhs );
        MT op = MT::None;
        if ( bin_op.type == ArithType::Add ) {
            op = MT::Add;
        } else if ( bin_op.type == ArithType::Sub ) {
            op = MT::Sub;
        } else if ( bin_op.type == ArithType::Mul ) {
            op = MT::Mul;
        } else if ( bin_op.type == ArithType::Div ) {
            op = MT::Div;
        } else if ( bin_op.type == ArithType::Mod ) {
            op = MT::Mod;
        }
        mir.instrs.put( MI{ op, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
    } else if ( auto uni_op = UniOp( node ) ) {
        VarId tmp_lhs = mir.next_var++;
        VarId tmp_rhs = mir.next_var++;
        write_mir_instr( state, mir, uni_op.rhs, tmp_rhs );
        if ( uni_op.type == ArithType::Neg ) {
            mir.instrs.put( MI{ MT::Const, tmp_lhs, 0, 0, 0, node.ifi } );
            mir.instrs.put(
                MI{ MT::Sub, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
        } else {
            mir.instrs.put(
                MI{ MT::None, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
        }
    } else if ( node.type == AstNode::Type::GlobalScope ) {
        auto itr = node.nodes->itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    }
}

void analyze_liveness( CompilerState &state, Mir &mir ) {
    auto itr = mir.instrs.end();
    auto none = MI{};
    while ( itr != mir.instrs.begin() ) {
        itr.skip_self( -1 );
        auto &instr = itr.get();
        auto &next = itr.skip( 1 ).get_or( none );

        if ( instr.p0 != 0 ) {
            instr.life.insert( instr.p0 );
        }
        if ( instr.p1 != 0 ) {
            instr.life.insert( instr.p1 );
        }
        for ( auto &v : next.life ) {
            if ( v != instr.result ) {
                instr.life.insert( v );
            }
        }
        // TODO does not account for jumps yet! Needs another "successor"
        // detection pass. Then this can instead be implemented as forward pass.
    }
}

bool has_effect( Mir &mir, Mir::MirInstr &instr ) {
    return instr.type == MT::Ret || instr.type == MT::Div ||
           instr.type == MT::Mod;
}

void analyze_neededness( CompilerState &state, Mir &mir ) {
    auto itr = mir.instrs.end();
    auto none = MI{};
    while ( itr != mir.instrs.begin() ) {
        itr.skip_self( -1 );
        auto &instr = itr.get();
        auto &next = itr.skip( 1 ).get_or( none );

        if ( has_effect( mir, instr ) ) {
            // Operations with some effect make their parameters needed.
            if ( instr.p0 != 0 ) {
                instr.needed.insert( instr.p0 );
            }
            if ( instr.p1 != 0 ) {
                instr.needed.insert( instr.p1 );
            }
        }
        for ( auto &v : next.needed ) {
            // Transitivity of neededness from successors.
            if ( v != instr.result ) {
                instr.needed.insert( v );
            }
            if ( instr.result != 0 && v == instr.result ) {
                if ( instr.p0 != 0 ) {
                    instr.needed.insert( instr.p0 );
                }
                if ( instr.p1 != 0 ) {
                    instr.needed.insert( instr.p1 );
                }
            }
        }
        // TODO same as analyze_liveness() does not account for jumps yet!
    }
}

void trim_dead_code( CompilerState &state, Mir &mir ) {
    auto itr = mir.instrs.end();
    auto none = MI{};
    while ( itr != mir.instrs.begin() ) {
        itr.skip_self( -1 );
        auto &instr = itr.get();
        auto &next = itr.skip( 1 ).get_or( none );
        if ( !has_effect( mir, instr ) &&
             next.needed.find( instr.result ) == next.needed.end() ) {
            // Result is not needed and therefore this line is dead code.
            itr.erase_self();
        }
    }
    // TODO same as analyze_liveness() does not account for jumps yet!
}

void create_inference_graph( CompilerState &state, Mir &mir,
                             InferenceGraph &inference_graph ) {
    // Every pair is included twice, because it is an undirected graph.
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        for ( auto v0 : instr.life ) {
            for ( auto v1 : instr.life ) {
                if ( v0 != v1 )
                    inference_graph.set_inference( v0, v1 );
            }
        }
    } );
}

void create_simplicial_elimination_ordering(
    CompilerState &state, Mir &mir, const InferenceGraph &inference_graph,
    std::vector<VarId> &ordering ) {
    std::map<VarId, size_t> wv; // Weighted vertices TODO use make_heap
    for ( size_t i = 0; i < mir.next_var; i++ )
        wv[i] = 0; // Populate wv
    ordering.reserve( wv.size() );

    // Construct ordering
    while ( !wv.empty() ) {
        auto max_v_itr = std::max_element(
            wv.begin(), wv.end(),
            []( auto &&a, auto &&b ) { return a.second < b.second; } );
        ordering.push_back( max_v_itr->first );

        // Increment weight of remaining neighbors (if there are any)
        inference_graph.for_all_neighbors(
            max_v_itr->first, [&]( VarId neighbor ) {
                if ( wv.find( neighbor ) != wv.end() )
                    ++wv[neighbor];
            } );

        wv.erase( max_v_itr );
    }
}

void create_register_mapping( CompilerState &state, Mir &mir ) {
    InferenceGraph inference_graph( mir.next_var );
    std::vector<VarId> ordering;
    create_inference_graph( state, mir, inference_graph );
    create_simplicial_elimination_ordering( state, mir, inference_graph,
                                            ordering );
    mir.reg_mapping.resize(
        mir.next_var ); // Every variable could get a mapping.

    // Greedy register allocation
    std::vector<bool> reg_candidates_occupied;
    for ( size_t i = 0; i < ordering.size(); i++ ) {
        auto v = ordering[i];

        // Find smallest register (color) which no other neighbor uses
        size_t count = inference_graph.neighbor_count( v );
        RegId r =
            std::max<size_t>( 1, count ); // Zero would be an invalid register.
        reg_candidates_occupied.assign( count + 2, false );
        inference_graph.for_all_neighbors( v, [&]( auto &&neighbor ) {
            RegId r = mir.reg_mapping.at( neighbor );
            if ( r > 0 && r <= count + 1 )
                reg_candidates_occupied[r] = true;
        } );
        for ( size_t i = 1; i < reg_candidates_occupied.size(); i++ ) {
            if ( !reg_candidates_occupied[i] )
                r = i;
        }

        mir.reg_mapping[v] = r;
        if ( r + 1 > mir.reg_count )
            mir.reg_count = r + 1;
    }

    // DEBUG
#ifndef NDEBUG
    if ( true ) {
        log( "== MIR REGS ==" );
        size_t i = 0;
        for ( auto p : mir.reg_mapping ) {
            olog( to_string( i ) + " => " + to_string( p ) );
            i++;
        }
    }
#endif
}

Mir construct_mir( CompilerState &state, AstNode &root_node ) {
    Mir mir;

    write_mir_instr( state, mir, root_node, mir.next_var++ );

    // Check whether there are None ops
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        if ( instr.type == MT::None )
            make_error_msg( state,
                            "Found 'None' instruction in MIR. This is "
                            "probably a compiler bug.",
                            instr.ifi, RetCode::InternalError );
    } );

    // DEBUG
#ifndef NDEBUG
    if ( true ) {
        log( "== MIR INSTRS ==" );
        mir.instrs.for_each( []( const Mir::MirInstr &instr ) {
            String str = name_of_instr( instr.type ) + " " +
                         to_string( instr.result ) + " " +
                         to_string( instr.p0 ) + " " + to_string( instr.p1 ) +
                         " c" + to_string( instr.imm );
            olog( str );
        } );
    }
#endif

    return mir;
}

} // namespace chc
