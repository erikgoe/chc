#include "../include/chc/MirAnalysis.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/Core.hpp"

namespace chc {

using AT = AstNode::Type;
using AstItr = EagerContainer<AstNode>::Iterator;

// using namespace AstNodeFacades;

using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;

void use_before_init_and_return_check( CompilerState &state, Mir &mir ) {
    std::vector<std::vector<VarId>> instr_uninits; // For each instr
    instr_uninits.resize( mir.instrs.length() + 1 );
    std::vector<Opt<bool>> instr_had_return; // For each instr
    instr_had_return.resize( mir.instrs.length() + 1 );
    Opt<Mir::MirInstr> prev_instr;

    // Check only reachable code
    size_t i = 0;
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        std::vector<VarId> uninits = instr_uninits[i];

        // Check variable usage
        if ( instr.p0 != 0 && std::find( uninits.begin(), uninits.end(),
                                         instr.p0 ) != uninits.end() ) {
            // p0 not defined
            make_error_msg( state, "Using undefined variable", instr.ifi,
                            RetCode::SemanticError );
        }
        if ( instr.p1 != 0 && std::find( uninits.begin(), uninits.end(),
                                         instr.p1 ) != uninits.end() ) {
            // p1 not defined
            make_error_msg( state, "Using undefined variable", instr.ifi,
                            RetCode::SemanticError );
        }

        if ( instr.type == MT::Uninit ) {
            // Memorize uninitialized variables
            uninits.push_back( instr.result );
        } else if ( instr.type == MT::Ret ) {
            // Some random specification rule, defines that this "defines" all
            // undefined variables ...
            uninits.clear();
        } else if ( instr.result != 0 ) {
            // Is defined
            uninits.erase(
                std::remove_if( uninits.begin(), uninits.end(),
                                [&]( auto &&u ) { return u == instr.result; } ),
                uninits.end() );
        }

        // Return statement
        instr_had_return[i] =
            instr_had_return[i].value_or( false ) || instr.type == MT::Ret;

        // Check return statements between functions
        if ( instr.type == MT::Func && prev_instr ) {
            if ( !instr_had_return[i - 1].has_value() ||
                 !instr_had_return[i - 1].value() ) {
                make_error_msg( state, "Found path without return statement",
                                prev_instr->ifi, RetCode::SemanticError );
            }
            // Reset, so that the state of the previous function does not leak
            // into the state of the new function.
            instr_had_return[i] = false;
        }

        // Add successors
        if ( instr.type != MT::Jmp ) {
            // Next instr is reached
            instr_uninits[i + 1].insert( instr_uninits[i + 1].end(),
                                         uninits.begin(), uninits.end() );
            instr_had_return[i + 1] =
                instr_had_return[i + 1].value_or( true ) &&
                instr_had_return[i].value();
        }
        if ( instr.type == MT::Jmp || instr.type == MT::JZero ) {
            // Next specified by imm
            ssize_t next_idx = *mir.jump_table[instr.imm] - mir.instrs.begin();
            instr_uninits[next_idx].insert( instr_uninits[next_idx].end(),
                                            uninits.begin(), uninits.end() );
            instr_had_return[next_idx] =
                instr_had_return[next_idx].value_or( true ) &&
                instr_had_return[i].value();
        }

        prev_instr = instr;
        i++;
    } );

    // Check for missing return statement
    size_t last_return_op = mir.instrs.length() - 1;
    if ( prev_instr && ( !instr_had_return[last_return_op].has_value() ||
                         !instr_had_return[last_return_op].value() ) ) {
        make_error_msg( state, "Found path without return statement",
                        prev_instr->ifi, RetCode::SemanticError );
    }
}

void drop_uninit_instrs( CompilerState &state, Mir &mir ) {
    mir.instrs.for_each( [&]( Mir::MirInstr &instr ) {
        if ( instr.type == MT::Uninit ) {
            instr.type = MT::Nop;
            instr.result = 0;
        }
    } );
}

void type_checking( CompilerState &state, Mir &mir ) {
    auto match_types = [&]( Mir::TypeId &lhs, Mir::TypeId &rhs,
                            const InFileInfo &ifi, const String &hint = "" ) {
        if ( lhs != rhs ) {
            if ( lhs == 0 ) {
                lhs = rhs;
            } else if ( rhs == 0 ) {
                rhs = lhs;
            } else {
                const auto &lhs_ts = mir.map_to_type_spec[lhs];
                const auto &rhs_ts = mir.map_to_type_spec[rhs];
                if ( !( lhs_ts.type == TypeSpecifier::Type::Ptr &&
                        rhs_ts.type == TypeSpecifier::Type::Nullptr ) &&
                     !( rhs_ts.type == TypeSpecifier::Type::Ptr &&
                        lhs_ts.type == TypeSpecifier::Type::Nullptr ) ) {
                    make_error_msg( state,
                                    "Type mismatch" +
                                        ( hint != "" ? " (" + hint + ")" : "" ),
                                    ifi, RetCode::SemanticError );
                    return false;
                }
            }
        }
        return true;
    };

    mir.instrs.for_each( [&]( Mir::MirInstr &instr ) {
        if ( instr.type == MT::Const ) {
            if ( !match_types( mir.type_of( instr.result ),
                               instr.type_constraint, instr.ifi ) )
                return;
        } else if ( instr.type == MT::BinOp ) {
            if ( ( mir.type_of( instr.p0 ) == 0 ||
                   mir.type_of( instr.p1 ) == 0 ) &&
                 instr.type_constraint != Mir::TYPE_ANY ) {
                make_error_msg( state, "Variable untyped", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            if ( !match_types( mir.type_of( instr.p0 ), mir.type_of( instr.p1 ),
                               instr.ifi ) )
                return;
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
            auto p0_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            auto p1_ts = mir.map_to_type_spec[mir.type_of( instr.p1 )];
            if ( p0_ts.type == TypeSpecifier::Type::Ptr ||
                 p0_ts.type == TypeSpecifier::Type::Nullptr ) {
                if ( instr.subtype != ArithType::Eq &&
                     instr.subtype != ArithType::UnEq ) {
                    make_error_msg( state, "Pointer arithmetic is not allowed.",
                                    instr.ifi, RetCode::SemanticError );
                    return;
                }
                if ( p0_ts.sub != p1_ts.sub &&
                     p0_ts.type != TypeSpecifier::Type::Nullptr &&
                     p1_ts.type != TypeSpecifier::Type::Nullptr ) {
                    make_error_msg(
                        state, "Pointer comparison requires type equality.",
                        instr.ifi, RetCode::SemanticError );
                    return;
                }
            }

            if ( has_only_int_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.TYPE_INT;
            } else if ( has_only_bool_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.TYPE_BOOL;
            } else if ( has_any_type_ret( instr.subtype ) ) {
                mir.type_of( instr.result ) = mir.type_of( instr.p0 );
            }
        } else if ( instr.type == MT::Ret ) {
            if ( !match_types( mir.type_of( instr.p0 ), instr.type_constraint,
                               instr.ifi, "return type" ) )
                return;
        } else if ( instr.type == MT::Arg ) {
            if ( !match_types( mir.type_of( instr.p0 ), instr.type_constraint,
                               instr.ifi, "argument type" ) )
                return;
        } else if ( instr.type == MT::Call ) {
            if ( !match_types( mir.type_of( instr.result ),
                               instr.type_constraint, instr.ifi,
                               "function result type" ) )
                return;
        } else if ( instr.type == MT::Mov ) {
            if ( instr.p0 != 0 ) {
                if ( mir.type_of( instr.p0 ) == 0 ) {
                    make_error_msg( state, "Variable untyped", instr.ifi,
                                    RetCode::SemanticError );
                    return;
                }
                if ( !match_types( mir.type_of( instr.result ),
                                   mir.type_of( instr.p0 ), instr.ifi ) )
                    return;
            }
        } else if ( instr.type == MT::TypeCast ) {
            if ( !match_types( mir.type_of( instr.result ),
                               instr.type_constraint, instr.ifi, "type cast" ) )
                return;
        } else if ( instr.type == MT::FieldRead ) {
            auto struct_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            if ( struct_ts.type != TypeSpecifier::Type::Struct ) {
                make_error_msg(
                    state,
                    "Field access on variable, which is not a struct-object.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            auto &fields =
                mir.struct_map[struct_ts.struct_symbol_id->value()].fields;
            auto field_itr = std::find_if(
                fields.begin(), fields.end(),
                [&]( auto &&pair ) { return pair.second == instr.name; } );
            if ( field_itr == fields.end() ) {
                make_error_msg( state,
                                "Identifier does not name field of struct.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            mir.var_struct_symbols[instr.p0] =
                struct_ts.struct_symbol_id->value();
            if ( !match_types( mir.type_of( instr.result ),
                               spec_to_type( mir, *field_itr->first.get() ),
                               instr.ifi, "field read" ) )
                return;
        } else if ( instr.type == MT::FieldWrite ) {
            auto struct_ts = mir.map_to_type_spec[mir.type_of( instr.result )];
            if ( struct_ts.type != TypeSpecifier::Type::Struct ) {
                make_error_msg(
                    state,
                    "Field access on variable, which is not a struct-object.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            auto &fields =
                mir.struct_map[struct_ts.struct_symbol_id->value()].fields;
            auto field_itr = std::find_if(
                fields.begin(), fields.end(),
                [&]( auto &&pair ) { return pair.second == instr.name; } );
            if ( field_itr == fields.end() ) {
                make_error_msg( state,
                                "Identifier does not name field of struct.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            mir.var_struct_symbols[instr.result] =
                struct_ts.struct_symbol_id->value();
            if ( !match_types( mir.type_of( instr.p0 ),
                               spec_to_type( mir, *field_itr->first.get() ),
                               instr.ifi, "field write" ) )
                return;
        } else if ( instr.type == MT::IndirectRead ) {
            auto ptr_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            if ( ptr_ts.type != TypeSpecifier::Type::Ptr ) {
                make_error_msg( state,
                                "Indirect field access on variable, which is "
                                "not a pointer.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            auto struct_ts = mir.complete_type_spec( *ptr_ts.sub );
            if ( struct_ts.type != TypeSpecifier::Type::Struct ) {
                make_error_msg(
                    state,
                    "Field access on variable, which is not a struct-object.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            auto &fields =
                mir.struct_map[struct_ts.struct_symbol_id->value()].fields;
            auto field_itr = std::find_if(
                fields.begin(), fields.end(),
                [&]( auto &&pair ) { return pair.second == instr.name; } );
            if ( field_itr == fields.end() ) {
                make_error_msg( state,
                                "Identifier does not name field of struct.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            mir.var_struct_symbols[instr.p0] =
                struct_ts.struct_symbol_id->value();
            if ( !match_types( mir.type_of( instr.result ),
                               spec_to_type( mir, *field_itr->first.get() ),
                               instr.ifi, "indirect read" ) )
                return;
        } else if ( instr.type == MT::IndirectWrite ) {
            auto ptr_ts = mir.map_to_type_spec[mir.type_of( instr.result )];
            if ( ptr_ts.type != TypeSpecifier::Type::Ptr ) {
                make_error_msg( state,
                                "Indirect field access on variable, which is "
                                "not a pointer.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            auto struct_ts = mir.complete_type_spec( *ptr_ts.sub );
            if ( struct_ts.type != TypeSpecifier::Type::Struct ) {
                make_error_msg(
                    state,
                    "Field access on variable, which is not a struct-object.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            auto &fields =
                mir.struct_map[struct_ts.struct_symbol_id->value()].fields;
            auto field_itr = std::find_if(
                fields.begin(), fields.end(),
                [&]( auto &&pair ) { return pair.second == instr.name; } );
            if ( field_itr == fields.end() ) {
                make_error_msg( state,
                                "Identifier does not name field of struct.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            mir.var_struct_symbols[instr.result] =
                struct_ts.struct_symbol_id->value();
            if ( !match_types( mir.type_of( instr.p0 ),
                               spec_to_type( mir, *field_itr->first.get() ),
                               instr.ifi, "indirect write" ) )
                return;
        } else if ( instr.type == MT::ArrayRead ) {
            auto arr_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            if ( arr_ts.type != TypeSpecifier::Type::Array ) {
                make_error_msg( state,
                                "Array access on variable, which is "
                                "not an array.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            auto idx_ts = mir.map_to_type_spec[mir.type_of( instr.p1 )];
            if ( idx_ts.type != TypeSpecifier::Type::Prim ||
                 idx_ts.name != "int" ) {
                make_error_msg( state,
                                "Array access not using an integer index.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            if ( !match_types( mir.type_of( instr.result ),
                               spec_to_type( mir, *arr_ts.sub ), instr.ifi,
                               "array read" ) )
                return;
        } else if ( instr.type == MT::ArrayWrite ) {
            auto arr_ts = mir.map_to_type_spec[mir.type_of( instr.result )];
            if ( arr_ts.type != TypeSpecifier::Type::Array ) {
                make_error_msg( state,
                                "Array access on variable, which is "
                                "not an array.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            auto idx_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            if ( idx_ts.type != TypeSpecifier::Type::Prim ||
                 idx_ts.name != "int" ) {
                make_error_msg( state,
                                "Array access not using an integer index.",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            if ( !match_types( mir.type_of( instr.p1 ),
                               spec_to_type( mir, *arr_ts.sub ), instr.ifi,
                               "array write" ) )
                return;
        } else if ( instr.type == MT::ReadMem ) {
            auto ptr_ts = mir.map_to_type_spec[mir.type_of( instr.p0 )];
            if ( ptr_ts.type != TypeSpecifier::Type::Ptr ) {
                make_error_msg(
                    state, "Pointer deref on variable, which is not a pointer.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            if ( !match_types( mir.type_of( instr.result ),
                               spec_to_type( mir, *ptr_ts.sub ), instr.ifi,
                               "pointer read" ) )
                return;
        } else if ( instr.type == MT::WriteMem &&
                    instr.type_constraint != Mir::TYPE_ANY ) {
            auto ptr_ts = mir.map_to_type_spec[mir.type_of( instr.result )];
            if ( ptr_ts.type != TypeSpecifier::Type::Ptr ) {
                make_error_msg(
                    state, "Pointer deref on variable, which is not a pointer.",
                    instr.ifi, RetCode::SemanticError );
                return;
            }
            if ( !match_types( mir.type_of( instr.p0 ),
                               spec_to_type( mir, *ptr_ts.sub ), instr.ifi,
                               "ptr write" ) )
                return;
        }
    } );
}

} // namespace chc
