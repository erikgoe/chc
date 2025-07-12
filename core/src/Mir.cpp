#include "../include/chc/Mir.hpp"
#include "../include/chc/Message.hpp"

namespace chc {

using AT = AstNode::Type;
using AstItr = EagerContainer<AstNode>::Iterator;
using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;
using RegId = Mir::RegId;
using TypeId = Mir::TypeId;


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
    case Mir::MirInstr::Type::Uninit:
        return "Uninit";
    case Mir::MirInstr::Type::Func:
        return "Func";
    case Mir::MirInstr::Type::Param:
        return "Param";
    case Mir::MirInstr::Type::Arg:
        return "Arg";
    case Mir::MirInstr::Type::Call:
        return "Call";
    case Mir::MirInstr::Type::TypeCast:
        return "TypeCast";
    case Mir::MirInstr::Type::FieldRead:
        return "FieldRead";
    case Mir::MirInstr::Type::FieldWrite:
        return "FieldWrite";
    case Mir::MirInstr::Type::IndirectRead:
        return "IndirectRead";
    case Mir::MirInstr::Type::IndirectWrite:
        return "IndirectWrite";
    case Mir::MirInstr::Type::ArrayRead:
        return "ArrayRead";
    case Mir::MirInstr::Type::ArrayWrite:
        return "ArrayWrite";
    case Mir::MirInstr::Type::ReadMem:
        return "ReadMem";
    case Mir::MirInstr::Type::WriteMem:
        return "WriteMem";
    default:
        return "Unknown";
    }
}

Mir::TypeId &spec_to_type( Mir &mir, const TypeSpecifier &spec ) {
    if ( mir.map_to_type_id.find( spec ) == mir.map_to_type_id.end() ) {
        TypeId id = mir.next_type++;
        mir.map_to_type_id[spec] = id;
        mir.map_to_type_spec[id] = spec;
    }
    return mir.map_to_type_id[spec];
}

// Sift down used for std::make_heap heaps (as standard library does not provide
// this function). TODO not needed?
template <typename T>
void sift_down( std::vector<T> &heap, size_t idx,
                std::function<bool( const T &, const T & )> cmp ) {
    while ( idx * 2 + 1 < heap.size() ) {
        size_t lhs_child = idx * 2 + 1;
        size_t rhs_child = idx * 2 + 2;
        size_t curr_max = idx;

        if ( lhs_child < heap.size() &&
             cmp( heap[curr_max], heap[lhs_child] ) ) {
            curr_max = lhs_child;
        }
        if ( rhs_child < heap.size() &&
             cmp( heap[curr_max], heap[rhs_child] ) ) {
            curr_max = rhs_child;
        }
        if ( curr_max != idx ) {
            std::swap( heap[idx], heap[curr_max] );
            idx = curr_max;
        } else {
            break;
        }
    }
}

// Sift up used for std::make_heap heaps (as standard library does not provide
// this function).
template <typename T, typename Comp>
void sift_up( std::vector<T> &heap, size_t offset, size_t idx, Comp cmp ) {
    while ( idx > 0 ) {
        size_t parent = ( idx - 1 ) / 2;

        if ( cmp( heap[offset + parent], heap[offset + idx] ) ) {
            std::swap( heap[offset + parent], heap[offset + idx] );
            idx = parent;
        } else {
            break;
        }
    }
}

void add_jump_label_target(
    Mir &mir, i32 label_id,
    EagerContainer<Mir::MirInstr>::Iterator instr_itr ) {
    if ( mir.jump_table.size() <= static_cast<size_t>( label_id ) )
        mir.jump_table.resize( label_id + 1 );
    mir.jump_table[label_id] = instr_itr;
}

Mir::FunctionInfo &get_func_signature( Mir &mir, SymbolId fn_symbol_id ) {
    if ( mir.func_map.find( fn_symbol_id ) == mir.func_map.end() ) {
        mir.func_map[fn_symbol_id].label = mir.next_label++;
        mir.func_label_to_symbol[mir.next_label - 1] = fn_symbol_id;
    }
    return mir.func_map[fn_symbol_id];
}

Mir::StructInfo &get_struct_info( Mir &mir, SymbolId struct_symbol_id ) {
    return mir.struct_map[struct_symbol_id];
}

size_t get_struct_size( Mir &mir, SymbolId struct_symbol_id );

size_t get_type_size( Mir &mir, const TypeSpecifier &type_spec ) {
    if ( type_spec.type == TypeSpecifier::Type::Struct ) {
        return get_struct_size( mir, type_spec.struct_symbol_id->value() );
    } else if ( type_spec.type == TypeSpecifier::Type::Prim ) {
        return 4;
    } else {
        return 8;
    }
}

size_t get_struct_size( Mir &mir, SymbolId struct_symbol_id ) {
    auto &struct_info = get_struct_info( mir, struct_symbol_id );
    if ( !struct_info.cached_size ) {
        size_t size = 0;
        for ( auto &pair : struct_info.fields ) {
            size += get_type_size( mir, *pair.first );
        }
        struct_info.cached_size = size;
    }
    return struct_info.cached_size.value();
}

size_t get_struct_field_offset( Mir &mir, SymbolId struct_symbol_id,
                                const String &field_name ) {
    auto &struct_info = get_struct_info( mir, struct_symbol_id );
    size_t offset = 0;
    for ( auto &pair : struct_info.fields ) {
        if ( pair.second == field_name )
            break;
        offset += get_type_size( mir, *pair.first );
    }
    return offset;
}

void discover_all_signatures( CompilerState &state, Mir &mir,
                              SemanticData &semantic_data, AstNode &node ) {
    // Iterate global nodes
    if ( auto fn_def = FunctionDef( node ) ) {
        auto &fn_info = get_func_signature( mir, fn_def.fn_symbol_id->value() );

        // Return type
        fn_info.ret_type = spec_to_type( mir, *fn_def.type );

        // Params
        auto itr = fn_def.params->itr();
        while ( itr ) {
            auto decl = DeclUninit( itr.get() );
            fn_info.arg_types.push_back( spec_to_type( mir, *decl.type ) );
            itr.skip_self( 1 );
        }
    } else if ( auto struct_def = StructDef( node ) ) {
        auto &struct_info =
            get_struct_info( mir, struct_def.struct_symbol_id->value() );

        // Fields
        auto itr = struct_def.fields->itr();
        while ( itr ) {
            auto decl = DeclUninit( itr.get().nodes->itr().get() );
            struct_info.fields.push_back(
                std::make_pair( decl.type, decl.symbol ) );

            // Touch type at least once
            spec_to_type( mir, *decl.type );
            itr.skip_self( 1 );
        }

        // Allocate a new type
        auto type = TypeSpecifier::make_struct_type( struct_def.struct_symbol );
        type->struct_symbol_id = struct_def.struct_symbol_id;
        spec_to_type( mir, *type );

    } else if ( node.type == AT::GlobalScope ) {
        // Add built-in functions
        auto make_build_in_type = [&]( const String &symbol, TypeId ret_type,
                                       Opt<TypeId> param0 ) {
            auto &fn_info = get_func_signature(
                mir, semantic_data.build_in_symbols[symbol] );
            fn_info.ret_type = ret_type;
            if ( param0 )
                fn_info.arg_types.push_back( param0.value() );
        };
        make_build_in_type( "print", Mir::TYPE_INT, Mir::TYPE_INT );
        make_build_in_type( "read", Mir::TYPE_INT, {} );
        make_build_in_type( "flush", Mir::TYPE_INT, {} );

        // Match AllocCall-labels to new symbols
        mir.func_label_to_symbol[mir.alloc_label] = semantic_data.next_symbol++;
        auto &tmp_fn_info = get_func_signature(
            mir, mir.func_label_to_symbol[mir.alloc_label] );
        tmp_fn_info.label = mir.alloc_label;
        tmp_fn_info.arg_types.push_back( Mir::TYPE_INT );
        tmp_fn_info.arg_types.push_back( Mir::TYPE_INT );
        tmp_fn_info.ret_type = Mir::TYPE_INT;
        mir.func_label_to_symbol[mir.check_array_label] =
            semantic_data.next_symbol++;
        get_func_signature( mir,
                            mir.func_label_to_symbol[mir.check_array_label] )
            .label = mir.check_array_label;

        // Recurse into definitions
        auto itr = node.nodes->itr();
        while ( itr ) {
            discover_all_signatures( state, mir, semantic_data, itr.get() );
            itr.skip_self( 1 );
        }
    }
}

void write_mir_instr( CompilerState &state, Mir &mir, AstNode &node,
                      Mir::VarId into_var );

void write_mir_store_instr( CompilerState &state, Mir &mir, AstNode &node,
                            Mir::VarId to_store ) {
    if ( auto ident = Ident( node ) ) {
        VarId variable = mir.var_map[ident.id->value()];
        mir.instrs.put( MI{ MT::Mov, variable, to_store, 0, 0, node.ifi } );
    } else if ( node.type == AT::Paren && node.nodes->not_empty() ) {
        return write_mir_store_instr( state, mir, node.nodes->itr().get(),
                                      to_store );
    } else if ( auto access = FieldAccess( node ) ) {
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        mir.instrs.put( MI{ MT::FieldWrite, lhs_var, to_store, 0, 0, node.ifi,
                            ArithType::None, 0, access.field_symbol } );
    } else if ( auto access = IndirectAccess( node ) ) {
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        mir.instrs.put( MI{ MT::IndirectWrite, lhs_var, to_store, 0, 0,
                            node.ifi, ArithType::None, 0,
                            access.field_symbol } );
    } else if ( auto access = ArrayAccess( node ) ) {
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        VarId idx_var = mir.next_var++;
        write_mir_instr( state, mir, *access.idx, idx_var );
        mir.instrs.put(
            MI{ MT::ArrayWrite, lhs_var, idx_var, to_store, 0, node.ifi } );
    } else if ( auto deref = PtrDeref( node ) ) {
        VarId ptr_var = mir.next_var++;
        write_mir_instr( state, mir, *deref.ptr, ptr_var );
        mir.instrs.put( MI{ MT::WriteMem, ptr_var, to_store, 0, 0, node.ifi } );
    }
}
void write_mir_instr( CompilerState &state, Mir &mir, AstNode &node,
                      Mir::VarId into_var ) {
    if ( auto fn_def = FunctionDef( node ) ) {
        // Label
        Mir::FunctionInfo fn_info =
            get_func_signature( mir, fn_def.fn_symbol_id->value() );
        mir.instrs.put( MI{ MT::Func, 0, 0, 0, fn_info.label, node.ifi } );
        if ( fn_def.fn_symbol == "main" )
            mir.main_function_symbol = fn_def.fn_symbol_id->value();

        // Params
        auto itr = fn_def.params->itr();
        while ( itr ) {
            auto var = mir.next_var++;
            mir.instrs.put( MI{ MT::Param, var, 0, 0, 0, node.ifi } );
            auto decl = DeclUninit( itr.get() );
            if ( decl.symbol_id )
                mir.var_map[decl.symbol_id->value()] = var;
            mir.type_of( var ) = spec_to_type( mir, *decl.type );
            itr.skip_self( 1 );
        }

        // Body
        mir.curr_fn_return_type = fn_info.ret_type;
        itr = fn_def.stmts->itr();
        while ( itr ) {
            write_mir_instr( state, mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( node.type == AT::Stmt ) {
        if ( node.nodes->itr().get().type == AT::BreakStmt ) {
            if ( mir.break_stack.empty() ) {
                make_error_msg( state, "Break statement must be inside a loop.",
                                node.ifi, RetCode::SemanticError );
                return;
            }
            mir.instrs.put(
                MI{ MT::Jmp, 0, 0, 0, mir.break_stack.back(), node.ifi } );
        } else if ( node.nodes->itr().get().type == AT::ContinueStmt ) {
            if ( mir.continue_stack.empty() ) {
                make_error_msg( state,
                                "Continue statement must be inside a loop.",
                                node.ifi, RetCode::SemanticError );
                return;
            }
            mir.instrs.put(
                MI{ MT::Jmp, 0, 0, 0, mir.continue_stack.back(), node.ifi } );
        } else {
            write_mir_instr( state, mir, node.nodes->itr().get(),
                             mir.next_var++ );
        }
    } else if ( auto ret = Ret( node ) ) {
        VarId tmp = mir.next_var++;
        write_mir_instr( state, mir, *ret.value, tmp );
        mir.instrs.put( MI{ MT::Ret, 0, tmp, 0, 0, node.ifi, ArithType::None,
                            mir.curr_fn_return_type } );
    } else if ( auto decl = Decl( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id->value()] = variable;
        write_mir_instr( state, mir, *decl.init, variable );
        mir.type_of( variable ) = spec_to_type( mir, *decl.type );
    } else if ( auto decl = DeclUninit( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id->value()] = variable;
        mir.type_of( variable ) = spec_to_type( mir, *decl.type );
        mir.instrs.put( MI{ MT::Uninit, variable, 0, 0, 0, node.ifi } );
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
    } else if ( node.type == AT::NullConst ) {
        mir.instrs.put(
            MI{ MT::Const, into_var, 0, 0, 0, node.ifi, ArithType::None,
                spec_to_type( mir, *TypeSpecifier::make_null_pointer() ) } );
    } else if ( auto stmt = AsnOp( node ) ) {
        assert( stmt.type == ArithType::None ); // Should already be handled in
                                                // operator_transformation()
        // We can just use into_var here, as it will be discarded anyways
        write_mir_instr( state, mir, *stmt.value, into_var );
        write_mir_store_instr( state, mir, *stmt.lvalue, into_var );
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
        i32 continue_lbl = mir.next_label++;
        VarId cond = mir.next_var++;
        mir.type_of( cond ) = Mir::TYPE_BOOL;
        write_mir_instr( state, mir, *for_loop.init, mir.next_var++ );
        add_jump_label_target( mir, loop_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, loop_lbl, node.ifi } );
        write_mir_instr( state, mir, *for_loop.cond, cond );
        mir.instrs.put( MI{ MT::JZero, 0, cond, 0, skip_lbl, node.ifi } );
        mir.continue_stack.push_back( continue_lbl );
        mir.break_stack.push_back( skip_lbl );
        write_mir_instr( state, mir, *for_loop.body, mir.next_var++ );
        mir.break_stack.pop_back();
        mir.continue_stack.pop_back();
        add_jump_label_target( mir, continue_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, continue_lbl, node.ifi } );
        write_mir_instr( state, mir, *for_loop.step, mir.next_var++ );
        mir.instrs.put( MI{ MT::Jmp, 0, 0, 0, loop_lbl, node.ifi } );
        add_jump_label_target( mir, skip_lbl, mir.instrs.end() );
        mir.instrs.put( MI{ MT::Label, 0, 0, 0, skip_lbl, node.ifi } );
    } else if ( auto call = Call( node ) ) {
        // First evaluate arguments (in normal order) and pass as parameters
        // (in reverse order)
        auto itr = call.args->itr();
        auto fn_info = get_func_signature( mir, call.fn_symbol_id->value() );
        auto arg_type_itr = fn_info.arg_types.begin();
        std::deque<std::pair<VarId, TypeId>> rev_vars;
        while ( itr ) {
            auto var = mir.next_var++;
            write_mir_instr( state, mir, itr.get(), var );
            rev_vars.push_back( std::make_pair( var, *arg_type_itr ) );
            itr.skip_self( 1 );
            ++arg_type_itr;
        }
        // Put arguments in reverse order (later for the stack).
        while ( !rev_vars.empty() ) {
            mir.instrs.put( MI{ MT::Arg, 0, rev_vars.back().first, 0,
                                fn_info.label, node.ifi, ArithType::None,
                                rev_vars.back().second } );
            rev_vars.pop_back();
        }

        // Call function
        mir.instrs.put( MI{ MT::Call, into_var, 0, 0, fn_info.label, node.ifi,
                            ArithType::None, fn_info.ret_type } );
    } else if ( auto call = AllocCall( node ) ) {
        auto itr = call.args->itr();
        // Calculate type size
        i32 type_size = get_type_size( mir, TypeSpecifier( itr.get() ) );
        VarId type_size_var = mir.next_var++;
        mir.instrs.put( MI{ MT::Const, type_size_var, 0, 0,
                            std::max( 8, type_size ), node.ifi, ArithType::None,
                            Mir::TYPE_INT } );

        if ( call.fn_symbol == "alloc" ) {
            auto shr_type_spec = TypeSpecifier::make_pointer_to(
                std::make_shared<TypeSpecifier>( itr.get() ) );
            auto type_id = spec_to_type( mir, *shr_type_spec );

            // Allocate simply one element.
            VarId count_var = mir.next_var++;
            mir.instrs.put( MI{ MT::Const, count_var, 0, 0, 1, node.ifi,
                                ArithType::None, Mir::TYPE_INT } );
            // Put parameters in reverse order
            mir.instrs.put( MI{ MT::Arg, 0, type_size_var, 0, mir.alloc_label,
                                node.ifi, ArithType::None, mir.TYPE_INT } );
            mir.instrs.put( MI{ MT::Arg, 0, count_var, 0, mir.alloc_label,
                                node.ifi, ArithType::None, mir.TYPE_INT } );

            // Call allocation function
            VarId tmp = mir.next_var++;
            mir.instrs.put( MI{ MT::Call, tmp, 0, 0, mir.alloc_label, node.ifi,
                                ArithType::None } );

            // Cast to requested type
            mir.instrs.put( MI{ MT::TypeCast, into_var, tmp, 0, mir.alloc_label,
                                node.ifi, ArithType::None, type_id } );
        } else if ( call.fn_symbol == "alloc_array" ) {
            auto shr_type_spec = TypeSpecifier::make_array_of(
                std::make_shared<TypeSpecifier>( itr.get() ) );
            auto type_id = spec_to_type( mir, *shr_type_spec );

            // Evaluate element count argument
            VarId tmp_count = mir.next_var++;
            VarId tmp_one = mir.next_var++;
            write_mir_instr( state, mir, itr.skip( 1 ).get(), tmp_count );
            // Add 1 to count_var for the added pointer
            VarId count_var = mir.next_var++;
            mir.instrs.put( MI{ MT::Const, tmp_one, 0, 0, 1, node.ifi,
                                ArithType::None, Mir::TYPE_INT } );
            mir.instrs.put( MI{ MT::BinOp, count_var, tmp_count, tmp_one, 0,
                                node.ifi, ArithType::Add, Mir::TYPE_ANY } );

            // Put parameters in reverse order
            mir.instrs.put( MI{ MT::Arg, 0, type_size_var, 0, mir.alloc_label,
                                node.ifi, ArithType::None, mir.TYPE_INT } );
            mir.instrs.put( MI{ MT::Arg, 0, count_var, 0, mir.alloc_label,
                                node.ifi, ArithType::None, mir.TYPE_INT } );

            // Call allocation function
            VarId ptr = mir.next_var++;
            mir.instrs.put(
                MI{ MT::Call, ptr, 0, 0, mir.alloc_label, node.ifi } );

            // Assign dynamic array size and shift returned pointer
            mir.instrs.put( MI{ MT::WriteMem, ptr, tmp_count, 0, 0, node.ifi,
                                ArithType::None, Mir::TYPE_ANY } );
            VarId shifted_ptr = mir.next_var++;
            VarId tmp_eight = mir.next_var++;
            mir.instrs.put( MI{ MT::Const, tmp_eight, 0, 0, 8, node.ifi,
                                ArithType::None, Mir::TYPE_INT } );
            mir.instrs.put( MI{ MT::BinOp, shifted_ptr, ptr, tmp_eight, 0,
                                node.ifi, ArithType::Add, Mir::TYPE_ANY, "",
                                true } );

            // Cast to requested type
            mir.instrs.put( MI{ MT::TypeCast, into_var, shifted_ptr, 0,
                                mir.alloc_label, node.ifi, ArithType::None,
                                type_id } );
        }
    } else if ( auto access = FieldAccess( node ) ) {
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        mir.instrs.put( MI{ MT::FieldRead, into_var, lhs_var, 0, 0, node.ifi,
                            ArithType::None, 0, access.field_symbol } );
    } else if ( auto access = IndirectAccess( node ) ) {
        // Basically the same thing as normal field access
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        mir.instrs.put( MI{ MT::IndirectRead, into_var, lhs_var, 0, 0, node.ifi,
                            ArithType::None, 0, access.field_symbol } );
    } else if ( auto access = ArrayAccess( node ) ) {
        VarId lhs_var = mir.next_var++;
        write_mir_instr( state, mir, *access.lhs, lhs_var );
        VarId idx_var = mir.next_var++;
        write_mir_instr( state, mir, *access.idx, idx_var );
        mir.instrs.put(
            MI{ MT::ArrayRead, into_var, lhs_var, idx_var, 0, node.ifi } );
    } else if ( auto ptr_deref = PtrDeref( node ) ) {
        VarId addr = mir.next_var++;
        write_mir_instr( state, mir, *ptr_deref.ptr, addr );
        mir.instrs.put( MI{ MT::ReadMem, into_var, addr, 0, 0, node.ifi } );
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

bool result_is_actually_param( MT instr_type ) {
    return instr_type == MT::FieldWrite || instr_type == MT::IndirectWrite ||
           instr_type == MT::ArrayWrite || instr_type == MT::WriteMem;
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
            if ( result_is_actually_param( instr.type ) )
                set_live( instr.result );

            // Live in successors is transitive, except if it's this
            // instruction's result value.
            for ( auto &v : succ.first->live ) {
                if ( v != instr.result ||
                     result_is_actually_param( instr.type ) )
                    set_live( v );
            }
            for ( auto &v : succ.second->live ) {
                if ( v != instr.result ||
                     result_is_actually_param( instr.type ) )
                    set_live( v );
            }
        }
    }
}

bool has_effect( Mir &mir, Mir::MirInstr &instr ) {
    return instr.type == MT::Ret || instr.type == MT::Jmp ||
           instr.type == MT::JZero || instr.type == MT::Label ||
           instr.type == MT::Func || instr.type == MT::Arg ||
           instr.type == MT::Param || instr.type == MT::Call ||
           instr.type == MT::FieldWrite || instr.type == MT::IndirectWrite ||
           instr.type == MT::ArrayWrite || instr.type == MT::WriteMem ||
           instr.type == MT::IndirectRead || instr.type == MT::ArrayRead ||
           instr.type == MT::ReadMem ||
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
            auto set_needed = [&]( VarId v ) {
                if ( v != 0 && instr.needed.find( v ) == instr.needed.end() ) {
                    instr.needed.insert( v );
                    saturated = false;
                }
            };
            auto set_fn_args_needed = [&]() {
                // Make all prepended arguments needed
                auto arg_itr = itr;
                while ( arg_itr != mir.instrs.begin() ) {
                    arg_itr.skip_self( -1 );
                    auto arg = arg_itr.get();
                    if ( arg.type != MT::Arg )
                        break;
                    set_needed( arg.p0 );
                }
            };

            if ( has_effect( mir, instr ) ) {
                // Operations with some effect make their parameters needed.
                set_needed( instr.p0 );
                set_needed( instr.p1 );
                if ( instr.type == MT::Call )
                    set_fn_args_needed();
                if ( result_is_actually_param( instr.type ) )
                    set_needed( instr.result );
            }
            // Transitivity of neededness from successors.
            for ( auto &v : succ.first->needed ) {
                if ( v != instr.result ||
                     result_is_actually_param( instr.type ) )
                    set_needed( v );
                if ( instr.result != 0 && v == instr.result &&
                     !result_is_actually_param( instr.type ) ) {
                    set_needed( instr.p0 );
                    set_needed( instr.p1 );
                }
            }
            for ( auto &v : succ.second->needed ) {
                if ( v != instr.result ||
                     result_is_actually_param( instr.type ) )
                    set_needed( v );
                if ( instr.result != 0 && v == instr.result &&
                     !result_is_actually_param( instr.type ) ) {
                    set_needed( instr.p0 );
                    set_needed( instr.p1 );
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
        if ( succ.first->needed.find( instr.result ) ==
                 succ.first->needed.end() &&
             succ.second->needed.find( instr.result ) ==
                 succ.second->needed.end() ) {
            if ( !has_effect( mir, instr ) ) {
                // Result is not needed and therefore this line is dead code.
                to_erase.push_back( itr );
            } else if ( !result_is_actually_param( instr.type ) ) {
                // Not needed, but has an effect => don't write back result
                itr.get().result = 0;
            }
        }
    }

    // Now delete dead code (to_erase lists iterators from back to front).
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
    ordering.reserve( mir.next_var );
    std::vector<std::pair<VarId, size_t>> wv; // Weighted vertices
    std::vector<bool> scheduled_vars; // For faster lookup
    scheduled_vars.resize( mir.next_var );
    wv.resize( mir.next_var );
    for ( size_t i = 0; i < mir.next_var; i++ )
        wv[i] = std::make_pair( i, 0 ); // Populate wv
    size_t consumed = 0;

    auto cmp = []( auto &&a, auto &&b ) { return a.second <= b.second; };

    // Construct ordering
    while ( wv.size() - consumed > 0 ) {
        auto max_v = *( wv.begin() + consumed );
        consumed++;
        ordering.push_back( max_v.first );
        scheduled_vars[max_v.first] = true;

        // Increment weight of remaining neighbors (if there are any)
        inference_graph.for_all_neighbors( max_v.first, [&]( VarId neighbor ) {
            if ( !scheduled_vars[neighbor] ) {
                // Still to schedule => increase weight
                auto itr = std::find_if(
                    wv.begin() + consumed, wv.end(),
                    [=]( auto &&p ) { return p.first == neighbor; } );
                ++itr->second;
                sift_up( wv, consumed, itr - wv.begin() - consumed, cmp );
            }
        } );
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

void count_function_registers( CompilerState &state, Mir &mir ) {
    Mir::FunctionInfo *fn_info = nullptr;

    auto itr = mir.instrs.itr();
    while ( itr ) {
        auto instr = itr.get();
        if ( instr.type == MT::Func ) {
            fn_info = &mir.func_map[mir.func_label_to_symbol[instr.imm]];
        } else {
            if ( instr.result != 0 ) {
                // Register is used in this function
                fn_info->max_register_used = std::max(
                    fn_info->max_register_used, mir.reg_mapping[instr.result] );
            }
        }

        itr.skip_self( 1 );
    }
}

Mir construct_mir( CompilerState &state, SemanticData &semantic_data,
                   AstNode &root_node ) {
    Mir mir;

    discover_all_signatures( state, mir, semantic_data, root_node );
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
            if ( instr.type != MT::Func )
                str = "  " + str; // Intend function bodies slightly
            olog( str );
        } );
    }
#endif

    return mir;
}

} // namespace chc
