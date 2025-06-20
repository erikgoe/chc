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

        i++;
    } );

    // Check for missing return statement
    if ( !instr_had_return.empty() && ( !instr_had_return.back().has_value() ||
                                        !instr_had_return.back().value() ) ) {
        make_error_msg( state, "Found path without return statement",
                        InFileInfo{}, RetCode::SemanticError );
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
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        if ( instr.type == MT::Const ) {
            if ( mir.type_of( instr.result ) != 0 &&
                 mir.type_of( instr.result ) != instr.type_constraint ) {
                make_error_msg( state, "Type mismatch", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
            mir.type_of( instr.result ) = instr.type_constraint;
        } else if ( instr.type == MT::BinOp ) {
            if ( mir.type_of( instr.p0 ) == 0 ||
                 mir.type_of( instr.p1 ) == 0 ) {
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
            if ( mir.type_of( instr.p0 ) != instr.type_constraint ) {
                make_error_msg( state, "Return type mismatch", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
        } else if ( instr.type == MT::Arg ) {
            if ( mir.type_of( instr.p0 ) != instr.type_constraint ) {
                make_error_msg( state, "Argument type mismatch", instr.ifi,
                                RetCode::SemanticError );
                return;
            }
        } else if ( instr.type == MT::Call ) {
            if ( mir.type_of( instr.result ) != 0 &&
                 mir.type_of( instr.result ) != instr.type_constraint ) {
                make_error_msg( state, "Type mismatch with function result",
                                instr.ifi, RetCode::SemanticError );
                return;
            }
            mir.type_of( instr.result ) = instr.type_constraint;
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
