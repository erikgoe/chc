#include "../include/chc/Mir.hpp"
#include "../include/chc/Message.hpp"

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

String Mir::MirInstr::type_name() const {
    switch ( type ) {
    case Mir::MirInstr::Type::None:
        return "None";
    case Mir::MirInstr::Type::Nop:
        return "Nop";
    case Mir::MirInstr::Type::Label:
        return "Label";
    case Mir::MirInstr::Type::Const:
        return "Const";
    case Mir::MirInstr::Type::Mov:
        return "Mov";
    case Mir::MirInstr::Type::BinOp:
        return "BinOp (" + map_bin_arith( subtype ) + ")";
    case Mir::MirInstr::Type::Ret:
        return "Ret";
    case Mir::MirInstr::Type::Jmp:
        return "Jmp";
    case Mir::MirInstr::Type::JZero:
        return "JZero";
    default:
        return "Unknown";
    }
}

Mir::TypeId str_to_type( CompilerState &state, const String &str,
                         InFileInfo ifi ) {
    if ( str == "int" ) {
        return Mir::TYPE_INT;
    } else if ( str == "bool" ) {
        return Mir::TYPE_BOOL;
    } else {
        make_error_msg( state, "Unknown type name", ifi,
                        RetCode::SemanticError );
        return 0;
    }
}

void add_jump_label_target(
    Mir &mir, i32 label_id,
    EagerContainer<Mir::MirInstr>::Iterator instr_itr ) {
    if ( mir.jump_table.size() <= static_cast<size_t>( label_id ) )
        mir.jump_table.resize( label_id + 1 );
    mir.jump_table[label_id] = instr_itr;
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
        auto itr = fn_def.stmts->itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto ret = RetStmt( node ) ) {
        VarId tmp = mir.next_var++;
        write_mir_instr( state, mir, *ret.value, tmp );
        mir.instrs.put( MI{ MT::Ret, 0, tmp, 0, 0, node.ifi } );
    } else if ( node.type == AT::Stmt &&
                node.nodes->itr().get().type == AT::ContinueStmt ) {
        if ( mir.continue_stack.empty() ) {
            make_error_msg( state, "Continue statement must be inside a loop.",
                            node.ifi, RetCode::SemanticError );
            return;
        }
        mir.instrs.put(
            MI{ MT::Jmp, 0, 0, 0, mir.continue_stack.back(), node.ifi } );
    } else if ( node.type == AT::Stmt &&
                node.nodes->itr().get().type == AT::BreakStmt ) {
        if ( mir.break_stack.empty() ) {
            make_error_msg( state, "Break statement must be inside a loop.",
                            node.ifi, RetCode::SemanticError );
            return;
        }
        mir.instrs.put(
            MI{ MT::Jmp, 0, 0, 0, mir.break_stack.back(), node.ifi } );
    } else if ( auto decl = DeclStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id->value()] = variable;
        write_mir_instr( state, mir, *decl.init, variable );
        mir.type_of( variable ) = str_to_type( state, decl.type, node.ifi );
    } else if ( auto decl = DeclUninitStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id->value()] = variable;
        mir.type_of( variable ) = str_to_type( state, decl.type, node.ifi );
    } else if ( auto ident = Ident( node ) ) {
        VarId variable = mir.var_map[ident.id->value()];
        mir.instrs.put( MI{ MT::Mov, into_var, variable, 0, 0, node.ifi } );
    } else if ( auto paren = Paren( node ) ) {
        if ( paren.children->length() != 1 ) {
            make_error_msg( state, "Expected only one element in parenthesis.",
                            node.ifi, RetCode::SyntaxError );
            return;
        }
        auto child = paren.children->first()->get();
        write_mir_instr( state, mir, child, into_var );
    } else if ( auto block = Block( node ) ) {
        auto itr = block.children->itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto int_const = IntConst( node ) ) {
        mir.instrs.put( MI{ MT::Const, into_var, 0, 0, int_const.value,
                            node.ifi, ArithType::None, Mir::TYPE_INT } );
    } else if ( auto bool_const = BoolConst( node ) ) {
        mir.instrs.put( MI{ MT::Const, into_var, 0, 0,
                            bool_const.value ? -1 : 0, node.ifi,
                            ArithType::None, Mir::TYPE_BOOL } );
    } else if ( auto stmt = AsnOpStmt( node ) ) {
        assert( stmt.type == ArithType::None ); // Should already be handled in
                                                // operator_transformation()
        VarId variable =
            mir.var_map[symbol_id_of_lvalue( state, *stmt.lvalue )];
        write_mir_instr( state, mir, *stmt.value, variable );
    } else if ( auto bin_op = BinOp( node ) ) {
        VarId tmp_lhs = mir.next_var++;
        VarId tmp_rhs = mir.next_var++;
        write_mir_instr( state, mir, *bin_op.lhs, tmp_lhs );
        write_mir_instr( state, mir, *bin_op.rhs, tmp_rhs );
        mir.instrs.put( MI{ MT::BinOp, into_var, tmp_lhs, tmp_rhs, 0, node.ifi,
                            bin_op.type } );
    } else if ( auto tern_op = TernOp( node ) ) {
        i32 else_lbl = mir.next_label++;
        i32 skip_lbl = mir.next_label++;
        VarId cond = mir.next_var++;
        mir.type_of( cond ) = Mir::TYPE_BOOL;
        // This is where it stops being real SSA (writing into into_var twice).
        write_mir_instr( state, mir, *tern_op.lhs, cond );
        mir.instrs.put( MI{ MT::JZero, 0, cond, 0, else_lbl, node.ifi } );
        write_mir_instr( state, mir, *tern_op.mid, into_var );
        mir.instrs.put( MI{ MT::Jmp, 0, 0, 0, skip_lbl, node.ifi } );
        add_jump_label_target( mir, else_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, else_lbl, node.ifi } );
        write_mir_instr( state, mir, *tern_op.rhs, into_var );
        add_jump_label_target( mir, skip_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, skip_lbl, node.ifi } );
    } else if ( auto if_stmt = IfStmt( node ) ) {
        bool has_else = if_stmt.false_stmt.type != AT::None;
        i32 else_lbl = mir.next_label++;
        i32 skip_lbl = mir.next_label++;
        VarId cond = mir.next_var++;
        mir.type_of( cond ) = Mir::TYPE_BOOL;
        VarId tmp_true = mir.next_var++;
        mir.type_of( tmp_true ) = Mir::TYPE_BOOL;
        VarId tmp_false = mir.next_var++;
        mir.type_of( tmp_false ) = Mir::TYPE_BOOL;
        write_mir_instr( state, mir, *if_stmt.cond, cond );
        mir.instrs.put( MI{ MT::JZero, 0, cond, 0, else_lbl, node.ifi } );
        write_mir_instr( state, mir, if_stmt.true_stmt, tmp_true );
        if ( has_else ) {
            mir.instrs.put( MI{ MT::Jmp, 0, 0, 0, skip_lbl, node.ifi } );
        }
        add_jump_label_target( mir, else_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, else_lbl, node.ifi } );
        if ( has_else ) {
            write_mir_instr( state, mir, if_stmt.false_stmt, tmp_false );
            add_jump_label_target( mir, skip_lbl, mir.instrs.end() );
            mir.instrs.put( MI{ MT::Label, 0, 0, 0, skip_lbl, node.ifi } );
        }
    } else if ( auto while_loop = WhileLoop( node ) ) {
        i32 loop_lbl = mir.next_label++;
        i32 skip_lbl = mir.next_label++;
        VarId cond = mir.next_var++;
        mir.type_of( cond ) = Mir::TYPE_BOOL;
        add_jump_label_target( mir, loop_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, loop_lbl, node.ifi } );
        write_mir_instr( state, mir, *while_loop.cond, cond );
        mir.instrs.put( MI{ MT::JZero, 0, cond, 0, skip_lbl, node.ifi } );
        mir.continue_stack.push_back( loop_lbl );
        mir.break_stack.push_back( skip_lbl );
        write_mir_instr( state, mir, *while_loop.body, mir.next_var++ );
        mir.break_stack.pop_back();
        mir.continue_stack.pop_back();
        mir.instrs.put( MI{ MT::Jmp, 0, 0, 0, loop_lbl, node.ifi } );
        add_jump_label_target( mir, skip_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, skip_lbl, node.ifi } );
    } else if ( auto for_loop = ForLoop( node ) ) {
        i32 loop_lbl = mir.next_label++;
        i32 skip_lbl = mir.next_label++;
        VarId cond = mir.next_var++;
        mir.type_of( cond ) = Mir::TYPE_BOOL;
        write_mir_instr( state, mir, *for_loop.init, mir.next_var++ );
        add_jump_label_target( mir, loop_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, loop_lbl, node.ifi } );
        write_mir_instr( state, mir, *for_loop.cond, cond );
        mir.instrs.put( MI{ MT::JZero, 0, cond, 0, skip_lbl, node.ifi } );
        mir.continue_stack.push_back( loop_lbl );
        mir.break_stack.push_back( skip_lbl );
        write_mir_instr( state, mir, *for_loop.body, mir.next_var++ );
        mir.break_stack.pop_back();
        mir.continue_stack.pop_back();
        write_mir_instr( state, mir, *for_loop.step, mir.next_var++ );
        mir.instrs.put( MI{ MT::Jmp, 0, 0, 0, loop_lbl, node.ifi } );
        add_jump_label_target( mir, skip_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, skip_lbl, node.ifi } );
    } else if ( node.type == AstNode::Type::GlobalScope ) {
        auto itr = node.nodes->itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    }
}

std::pair<MI *, MI *> get_successors(
    Mir &mir, EagerContainer<Mir::MirInstr>::Iterator instr_itr, MI &none ) {
    std::pair<MI *, MI *> ret;
    auto &instr = instr_itr.get();
    ret.first = &instr_itr.skip( 1 ).get_or( none );
    ret.second = &none;
    if ( instr.type == MT::Jmp )
        ret.first = &mir.jump_table[instr.imm]->get();
    if ( instr.type == MT::JZero )
        ret.second = &mir.jump_table[instr.imm]->get();
    return ret;
}

void analyze_liveness( CompilerState &state, Mir &mir ) {
    auto none = MI{};
    bool saturated = false;
    // TODO Maybe find a better way instead of dump repetition. Also for
    // analyze_neededness() and trim_dead_code().
    while ( !saturated ) {
        saturated = true;
        auto itr = mir.instrs.end();
        while ( itr != mir.instrs.begin() ) {
            itr.skip_self( -1 );
            auto &instr = itr.get();
            auto succ = get_successors( mir, itr, none );

            // Helper
            auto set_live = [&]( VarId v ) {
                if ( v != 0 && instr.live.find( v ) == instr.live.end() ) {
                    instr.live.insert( v );
                    saturated = false;
                }
            };

            // Parameters need to live
            set_live( instr.p0 );
            set_live( instr.p1 );

            // Live in successors is transitive, except if its this
            // instruction's result value.
            for ( auto &v : succ.first->live ) {
                if ( v != instr.result )
                    set_live( v );
            }
            for ( auto &v : succ.second->live ) {
                if ( v != instr.result )
                    set_live( v );
            }
        }
    }
}

bool has_effect( Mir &mir, Mir::MirInstr &instr ) {
    return instr.type == MT::Ret || instr.type == MT::Jmp ||
           instr.type == MT::JZero || instr.type == MT::Label ||
           ( instr.type == MT::BinOp && ( instr.subtype == ArithType::Div ||
                                          instr.subtype == ArithType::Mod ) );
}

void analyze_neededness( CompilerState &state, Mir &mir ) {
    auto none = MI{};
    bool saturated = false;
    while ( !saturated ) {
        saturated = true;
        auto itr = mir.instrs.end();
        while ( itr != mir.instrs.begin() ) {
            itr.skip_self( -1 );
            auto &instr = itr.get();
            auto succ = get_successors( mir, itr, none );

            // Helper
            auto set_need = [&]( VarId v ) {
                if ( v != 0 && instr.needed.find( v ) == instr.needed.end() ) {
                    instr.needed.insert( v );
                    saturated = false;
                }
            };

            if ( has_effect( mir, instr ) ) {
                // Operations with some effect make their parameters needed.
                set_need( instr.p0 );
                set_need( instr.p1 );
            }
            // Transitivity of neededness from successors.
            for ( auto &v : succ.first->needed ) {
                if ( v != instr.result )
                    set_need( v );
                if ( instr.result != 0 && v == instr.result ) {
                    set_need( instr.p0 );
                    set_need( instr.p1 );
                }
            }
            for ( auto &v : succ.second->needed ) {
                if ( v != instr.result )
                    set_need( v );
                if ( instr.result != 0 && v == instr.result ) {
                    set_need( instr.p0 );
                    set_need( instr.p1 );
                }
            }
        }
    }
}

void trim_dead_code( CompilerState &state, Mir &mir ) {
    auto itr = mir.instrs.end();
    auto none = MI{};
    std::vector<EagerContainer<Mir::MirInstr>::Iterator> to_erase;
    while ( itr != mir.instrs.begin() ) {
        itr.skip_self( -1 );
        auto &instr = itr.get();
        auto succ = get_successors( mir, itr, none );
        if ( !has_effect( mir, instr ) &&
             succ.first->needed.find( instr.result ) ==
                 succ.first->needed.end() &&
             succ.second->needed.find( instr.result ) ==
                 succ.second->needed.end() ) {
            // Result is not needed and therefore this line is dead code.
            to_erase.push_back( itr );
        }
    }

    // Now delete dead code
    for ( auto itr : to_erase ) {
        itr.erase_self();
    }
    mir.jump_table.clear(); // All iterators invalidated
    // TODO maybe find a better way, as soon as EagerContainer allows for safe
    // erasure.
}

void create_inference_graph( CompilerState &state, Mir &mir,
                             InferenceGraph &inference_graph ) {
    // Every pair is included twice, because it is an undirected graph.
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        for ( auto v0 : instr.live ) {
            for ( auto v1 : instr.live ) {
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
            String str = instr.type_name() + " " + to_string( instr.result ) +
                         " " + to_string( instr.p0 ) + " " +
                         to_string( instr.p1 ) + " c" + to_string( instr.imm );
            olog( str );
        } );
    }
#endif

    return mir;
}

} // namespace chc
