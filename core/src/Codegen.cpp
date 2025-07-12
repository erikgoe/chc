#include "../include/chc/Codegen.hpp"
#include "../include/chc/Message.hpp"

namespace chc {

using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;
using RegId = Mir::RegId;
using AOC = Assembly_x86::OpCode;
using HwReg = Assembly_x86::HwReg;

#ifndef NDEBUG
#define AMS_CODE_LINE_COMMENTS
#endif

// Used for tracking register states regarding stack pushing.
struct RegisterState {
    bool written_to = false; // Whether it must be saved
    bool pushed_to_stack = false; // Whether it was saved
};

void split_into_lines( const String &str, std::vector<String> &lines ) {
    String tmp;
    for ( auto c : str ) {
        if ( c == '\n' ) {
            lines.push_back( tmp );
            tmp.clear();
        } else {
            tmp += c;
        }
    }
    lines.push_back( tmp ); // Last line
}

size_t line_from_offset( const String &str, size_t offset ) {
    size_t ln = 0;
    size_t i = 0;
    for ( auto c : str ) {
        if ( i >= offset )
            break;
        if ( c == '\n' ) {
            ln++;
        }
        i++;
    }
    return ln;
}

String get_fn_label( Mir &mir, SymbolId id ) {
    return "fn" + to_string( mir.func_map[id].label );
}

void generate_code_x86( CompilerState &state, const String &original_source,
                        Mir &mir, SemanticData &semantic_data,
                        EagerContainer<Assembly_x86> &assembly ) {
    constexpr HwReg no_reg = HwReg::None;
    RegId virtual_stack_start_reg = 12; // DEBUG
    RegId caller_saved_reg_count = 6;
    InFileInfo no_ifi;

    std::map<HwReg, RegisterState> reg_states;
    bool saved_caller_registers = false;
    size_t loaded_parm_count = 0;
    Mir::FunctionInfo curr_fn_info;
    size_t curr_frame_size = 0; // Used for alignment calculation.

    // Source code lines
#ifdef AMS_CODE_LINE_COMMENTS
    String clean_source;
    make_clean_code_str( original_source, clean_source );
    std::vector<String> source_lines;
    split_into_lines( clean_source, source_lines );
#endif

    // Functions to generate single instructions
    auto put_comment = [&]( const String &content, const InFileInfo &ifi ) {
        // Dirty-escape comments in the line
        String tmp = content;
        while ( tmp.find( "*/" ) != tmp.npos )
            tmp.insert( tmp.find( "*/" ) + 1, " " );

        assembly.put(
            Assembly_x86{ AOC::Comment, no_reg, no_reg, 0, tmp, ifi } );
    };
    (void) put_comment; // Ignore unused in release build.
    auto put_reg_reg = [&]( AOC opcode, HwReg dest, HwReg src,
                            const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, src, 0, "", ifi } );
    };
    auto put_reg = [&]( AOC opcode, HwReg dest, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, no_reg, 0, "", ifi } );
    };
    auto put_src_reg = [&]( AOC opcode, HwReg src, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, no_reg, src, 0, "", ifi } );
    };
    auto put_reg_imm = [&]( AOC opcode, HwReg dest, i32 imm,
                            const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, no_reg, imm, "", ifi } );
    };
    auto put_src_reg_imm = [&]( AOC opcode, HwReg src, i32 imm,
                                const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, no_reg, src, imm, "", ifi } );
    };
    auto put_imm = [&]( AOC opcode, i32 imm, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, no_reg, no_reg, imm, "", ifi } );
    };
    auto put_symbol = [&]( AOC opcode, HwReg dest, const String &str,
                           const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, no_reg, 0, str, ifi } );
    };
    auto put_str = [&]( AOC opcode, const String &str, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, no_reg, no_reg, 0, str, ifi } );
    };
    auto put_label = [&]( const String &str, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ AOC::Label, no_reg, no_reg, 0, str, ifi } );
    };
    auto put_empty = [&]( AOC opcode, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, no_reg, no_reg, 0, "", ifi } );
    };

    // Translation between virtual registers and hardware registers
    auto to_hw_reg = [&]( RegId reg ) -> HwReg {
        if ( reg >= virtual_stack_start_reg ) {
            return HwReg::r10d;
        } else {
            // Map normal registers
            switch ( reg ) {
            case 1:
                return HwReg::ecx; // caller saved
            case 2:
                return HwReg::esi; // caller saved
            case 3:
                return HwReg::edi; // caller saved
            case 4:
                return HwReg::r8d; // caller saved
            case 5:
                return HwReg::r9d; // caller saved
            case 6:
                return HwReg::r11d; // caller saved
            case 7:
                return HwReg::ebx;
            case 8:
                return HwReg::r12d;
            case 9:
                return HwReg::r13d;
            case 10:
                return HwReg::r14d;
            case 11:
                return HwReg::r15d;
            default:
                return HwReg::None;
            }
        }
    };

    // Get register and optionally load from stack
    auto make_available = [&]( VarId var, const InFileInfo &ifi ) -> HwReg {
        RegId reg = mir.reg_mapping[var];
        if ( reg < virtual_stack_start_reg ) {
            // Simply use available register
            return to_hw_reg( reg );
        }

        // Load from stack first
        size_t addr_offset = -8 * ( reg - virtual_stack_start_reg + 1 );
        put_reg_imm( AOC::MovFromStack64, HwReg::r10d, addr_offset, ifi );
        return HwReg::r10d;
    };
    // Copy to register and optionally load from stack
    auto make_available_in = [&]( VarId var, HwReg dest_reg,
                                  const InFileInfo &ifi ) {
        RegId reg = mir.reg_mapping[var];
        if ( reg < virtual_stack_start_reg ) {
            HwReg hw_reg = to_hw_reg( reg );
            if ( hw_reg != dest_reg ) {
                // Copy into destination register.
                put_reg_reg( AOC::Mov64, dest_reg, hw_reg, ifi );
                reg_states[dest_reg].written_to = true;
            }
            return;
        }

        // Load from stack first
        size_t addr_offset = -8 * ( reg - virtual_stack_start_reg + 1 );
        put_reg_imm( AOC::MovFromStack64, dest_reg, addr_offset, ifi );
        reg_states[dest_reg].written_to = true;
    };
    // Write back to stack if necessary
    auto writeback_opt = [&]( VarId to_var, HwReg src_reg,
                              const InFileInfo &ifi ) {
        if ( to_var == 0 ) {
            // Empty dest register, means that we are not interested in the
            // result, but in the effect.
            return;
        }
        RegId reg = mir.reg_mapping[to_var];
        if ( reg < virtual_stack_start_reg ) {
            if ( to_hw_reg( reg ) != src_reg ) {
                // Copy back into original register.
                put_reg_reg( AOC::Mov64, to_hw_reg( reg ), src_reg, ifi );
                reg_states[to_hw_reg( reg )].written_to = true;
            }
            return;
        }

        // Write back to stack
        size_t addr_offset = -8 * ( reg - virtual_stack_start_reg + 1 );
        put_src_reg_imm( AOC::MovToStack64, src_reg, addr_offset, ifi );
    };

    // Saving register to stack for function calls
    auto is_caller_saved = []( HwReg reg ) {
        return reg == HwReg::ecx || reg == HwReg::esi || reg == HwReg::edi ||
               reg == HwReg::r8d || reg == HwReg::r9d || reg == HwReg::r11d;
    };
    auto save_callee_registers = [&]( size_t used_regs_count, InFileInfo ifi ) {
        if ( used_regs_count > caller_saved_reg_count ) {
            size_t need_to_save =
                std::min( used_regs_count, virtual_stack_start_reg ) -
                caller_saved_reg_count;
            for ( size_t i = 0; i < need_to_save; i++ ) {
                put_src_reg( AOC::Push,
                             to_hw_reg( caller_saved_reg_count + i + 1 ), ifi );
                curr_frame_size += 8;
            }
        }
    };
    auto restore_callee_registers = [&]( size_t used_regs_count,
                                         InFileInfo ifi ) {
        if ( used_regs_count > caller_saved_reg_count ) {
            size_t need_to_save =
                std::min( used_regs_count, virtual_stack_start_reg ) -
                caller_saved_reg_count;
            for ( size_t i = 0; i < need_to_save; i++ ) {
                put_reg( AOC::Pop,
                         to_hw_reg( caller_saved_reg_count + need_to_save - i ),
                         ifi );
                curr_frame_size -= 8;
            }
        }
    };
    auto save_caller_registers = [&]( size_t used_regs_count, InFileInfo ifi ) {
        size_t count = static_cast<size_t>( HwReg::count );
        for ( size_t i = 0; i < count; i++ ) {
            if ( auto itr = reg_states.find( static_cast<HwReg>( i + 1 ) );
                 itr != reg_states.end() && itr->second.written_to &&
                 is_caller_saved( itr->first ) ) {
                put_src_reg( AOC::Push, itr->first, ifi );
                itr->second.pushed_to_stack = true;
                curr_frame_size += 8;
            }
        }
    };
    auto restore_caller_registers = [&]( size_t used_regs_count,
                                         InFileInfo ifi ) {
        size_t count = static_cast<size_t>( HwReg::count );
        for ( size_t i = 0; i < count; i++ ) {
            size_t idx = count - i;
            if ( auto itr = reg_states.find( static_cast<HwReg>( idx ) );
                 itr != reg_states.end() && itr->second.pushed_to_stack &&
                 is_caller_saved( itr->first ) ) {
                put_reg( AOC::Pop, itr->first, ifi );
                itr->second.pushed_to_stack = false;
                curr_frame_size -= 8;
            }
        }
    };
    auto save_before_params_on_demand = [&]( Mir::FunctionInfo callee_fn_info,
                                             InFileInfo ifi ) {
        if ( !saved_caller_registers ) {
            saved_caller_registers = true;
            save_caller_registers( callee_fn_info.max_register_used, ifi );

            // Alignment must be done before the parameters
            i32 align_delta =
                curr_frame_size + callee_fn_info.arg_types.size() * 8;
            put_imm( AOC::AddSp, -16 + ( align_delta % 16 ), ifi );
        }
    };

    // Add preamble
    auto main_fn_label = get_fn_label( mir, mir.main_function_symbol );
    put_str( AOC::Global, "main", no_ifi );
    put_empty( AOC::Text, no_ifi );
    put_label( "main", no_ifi );
    put_str( AOC::Call, main_fn_label, no_ifi );

    // Call flush after main. Push twice to keep alignment
    put_src_reg( AOC::Push, HwReg::eax, no_ifi );
    put_src_reg( AOC::Push, HwReg::eax, no_ifi );
    put_str( AOC::Call, get_fn_label( mir, semantic_data.func_map["flush"].id ),
             no_ifi );
    put_reg( AOC::Pop, HwReg::eax, no_ifi );
    put_reg( AOC::Pop, HwReg::eax, no_ifi );

    put_reg_reg( AOC::Mov64, HwReg::edi, HwReg::eax, no_ifi );
    put_reg_imm( AOC::MovConst, HwReg::eax, 0x3c, no_ifi );
    put_empty( AOC::Syscall, no_ifi );

    // Add wrappers for built-in functions
    put_str( AOC::Extern, "putchar", no_ifi );
    put_str( AOC::Extern, "getchar", no_ifi );
    put_str( AOC::Extern, "fflush", no_ifi );
    put_str( AOC::Extern, "stdout", no_ifi );
    put_str( AOC::Extern, "calloc", no_ifi );
    put_str( AOC::Extern, "abort", no_ifi );
    for ( auto b : semantic_data.build_in_symbols ) {
        SymbolId symbol = b.second;
        String label = get_fn_label( mir, b.second );

        // Preamble
        put_comment( "wrapper: " + b.first, no_ifi );
        put_label( label, no_ifi );
        put_imm( AOC::Enter, 0, no_ifi );
        bool requires_saving_edi = false;
        if ( mir.func_map[symbol].arg_types.size() == 1 || b.first == "flush" )
            requires_saving_edi = true;
        if ( requires_saving_edi ) {
            // One parameter (alignment cancels out with saved register)
            put_src_reg( AOC::Push, HwReg::edi, no_ifi );
        } else {
            // Zero parameters
            put_imm( AOC::AddSp, -8, no_ifi ); // alignment
        }

        // Parameters
        if ( mir.func_map[symbol].arg_types.size() == 1 ) {
            size_t p0_offset = 16;
            put_reg_imm( AOC::MovFromStack64, HwReg::edi, p0_offset, no_ifi );
        }
        if ( b.first == "flush" ) {
            // Add stdout as paremeter (which has value 1)
            put_symbol( AOC::MovSymbolWithRip64, HwReg::edi, "stdout", no_ifi );
        }

        // Actual call
        if ( b.first == "print" ) {
            put_str( AOC::Call, "putchar", no_ifi );
        } else if ( b.first == "read" ) {
            put_str( AOC::Call, "getchar", no_ifi );
        } else if ( b.first == "flush" ) {
            put_str( AOC::Call, "fflush", no_ifi );
        }

        // Restore saved register
        if ( requires_saving_edi ) {
            // One parameter
            put_reg( AOC::Pop, HwReg::edi, no_ifi );
        } else {
            // Zero parameters
            put_imm( AOC::AddSp, 8, no_ifi ); // alignment
        }

        // Return values
        if ( b.first == "print" || b.first == "flush" ) {
            put_reg_imm( AOC::MovConst, HwReg::eax, 0,
                         no_ifi ); // Always return 0
        }
        put_empty( AOC::Leave, no_ifi );
        put_empty( AOC::Ret, no_ifi );
    }

    // Explicit calloc wrapper
    put_comment( "wrapper: calloc", no_ifi );
    put_label( "fn" + to_string( mir.alloc_label ), no_ifi );
    put_imm( AOC::Enter, 0, no_ifi );
    // Does not need alignment adjustment
    put_src_reg( AOC::Push, HwReg::ecx, no_ifi ); // caller-saved
    put_src_reg( AOC::Push, HwReg::r8d, no_ifi ); // caller-saved
    put_src_reg( AOC::Push, HwReg::r9d, no_ifi ); // caller-saved
    put_src_reg( AOC::Push, HwReg::r11d, no_ifi ); // caller-saved
    put_src_reg( AOC::Push, HwReg::edi, no_ifi );
    put_src_reg( AOC::Push, HwReg::esi, no_ifi );
    size_t p0_offset = 16;
    put_reg_imm( AOC::MovFromStack64, HwReg::edi, p0_offset, no_ifi );
    put_reg_imm( AOC::MovFromStack64, HwReg::esi, p0_offset + 8, no_ifi );
    put_reg_imm( AOC::MovConst, HwReg::eax, 1, no_ifi );
    put_reg_reg( AOC::Cmp, HwReg::eax, HwReg::edi, no_ifi );
    put_str( AOC::Jnle, "sigabrt", no_ifi );
    put_str( AOC::Call, "calloc", no_ifi );
    put_reg( AOC::Pop, HwReg::esi, no_ifi );
    put_reg( AOC::Pop, HwReg::edi, no_ifi );
    put_reg( AOC::Pop, HwReg::r11d, no_ifi );
    put_reg( AOC::Pop, HwReg::r9d, no_ifi );
    put_reg( AOC::Pop, HwReg::r8d, no_ifi );
    put_reg( AOC::Pop, HwReg::ecx, no_ifi );
    put_empty( AOC::Leave, no_ifi );
    put_empty( AOC::Ret, no_ifi );

    // Simple wrapper to call abort
    put_label( "sigabrt", no_ifi );
    put_str( AOC::Call, "abort", no_ifi );


    // Add instructions for all functions
#ifdef AMS_CODE_LINE_COMMENTS
    ssize_t curr_line = -1;
    InFileInfo curr_ifi;
#endif
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
    // Printing source lines as comments for easier debugging
#ifdef AMS_CODE_LINE_COMMENTS
        size_t ln = line_from_offset( clean_source, instr.ifi.offset );
        if ( static_cast<ssize_t>( ln ) != curr_line ) {
            curr_line = ln;
            put_comment( "=> " + source_lines[ln], instr.ifi );
        }
        if ( instr.ifi != curr_ifi ) {
            curr_ifi = instr.ifi;
            put_comment( clean_source.substr( curr_ifi.offset, curr_ifi.size ),
                         instr.ifi );
        }
#endif

        // Actual instructions
        if ( instr.type == MT::Label ) {
            put_label( "l" + to_string( instr.imm ), instr.ifi );
        } else if ( instr.type == MT::Const ) {
            put_reg_imm( AOC::MovConst, HwReg::eax, instr.imm, instr.ifi );
            writeback_opt( instr.result, HwReg::eax, instr.ifi );
        } else if ( instr.type == MT::Mov ) {
            auto src_reg = make_available( instr.p0, instr.ifi );
            writeback_opt( instr.result, src_reg, instr.ifi );
        } else if ( instr.type == MT::BinOp ) {
            AOC aoc;
            switch ( instr.subtype ) {
            case ArithType::Add:
                aoc = instr.use_64bit ? AOC::Add64 : AOC::Add;
                break;
            case ArithType::Sub:
                aoc = AOC::Sub;
                break;
            case ArithType::Mul:
                aoc = AOC::IMul;
                break;
            case ArithType::Div:
                aoc = AOC::IDiv;
                break;
            case ArithType::Mod:
                aoc = AOC::IDiv;
                break;
            case ArithType::BAnd:
                aoc = AOC::And;
                break;
            case ArithType::BOr:
                aoc = AOC::Or;
                break;
            case ArithType::BXor:
                aoc = AOC::Xor;
                break;
            case ArithType::Shl:
                aoc = AOC::Shl;
                break;
            case ArithType::Shr:
                aoc = AOC::Shr;
                break;
            case ArithType::Eq:
                aoc = AOC::SetEq;
                break;
            case ArithType::Less:
                aoc = AOC::SetBelow;
                break;
            case ArithType::LessEq:
                aoc = AOC::SetBelowEq;
                break;
            default:
                aoc = AOC::None;
                break;
            }

            // Add the actual instructions
            make_available_in( instr.p0, HwReg::eax, instr.ifi );
            auto rhs_reg = make_available( instr.p1, instr.ifi );
            if ( instr.subtype == ArithType::Div ||
                 instr.subtype == ArithType::Mod ) {
                put_empty( AOC::Cltd, instr.ifi );
            }
            if ( instr.subtype == ArithType::Eq ||
                 instr.subtype == ArithType::Less ||
                 instr.subtype == ArithType::LessEq ) {
                put_reg_reg( AOC::Cmp, HwReg::eax, rhs_reg, instr.ifi );
                put_reg( aoc, HwReg::eax, instr.ifi );
                put_reg( AOC::MovZeroExtend, HwReg::eax, instr.ifi );
            } else if ( instr.subtype == ArithType::Shl ||
                        instr.subtype == ArithType::Shr ) {
                // Because shifts must go through ecx, temporarily swap ecx and
                // edx (which is always free).
                put_reg_reg( AOC::Mov64, HwReg::edx, HwReg::ecx, instr.ifi );
                put_reg_reg( AOC::Mov64, HwReg::ecx, rhs_reg, instr.ifi );
                put_reg( aoc, HwReg::eax, instr.ifi );
                put_reg_reg( AOC::Mov64, HwReg::ecx, HwReg::edx, instr.ifi );
            } else {
                put_reg_reg( aoc, HwReg::eax, rhs_reg, instr.ifi );
            }
            if ( instr.subtype == ArithType::Mod ) {
                writeback_opt( instr.result, HwReg::edx, instr.ifi );
            } else {
                writeback_opt( instr.result, HwReg::eax, instr.ifi );
            }
        } else if ( instr.type == MT::Ret ) {
            make_available_in( instr.p0, HwReg::eax, instr.ifi );
            restore_callee_registers( curr_fn_info.max_register_used,
                                      instr.ifi );
            put_empty( AOC::Leave, instr.ifi );
            put_empty( AOC::Ret, instr.ifi );
        } else if ( instr.type == MT::Jmp ) {
            put_str( AOC::Jmp, "l" + to_string( instr.imm ), instr.ifi );
        } else if ( instr.type == MT::JZero ) {
            auto val = make_available( instr.p0, instr.ifi );
            put_reg_imm( AOC::MovConst, HwReg::eax, 0, instr.ifi );
            put_reg_reg( AOC::Cmp, HwReg::eax, val, instr.ifi );
            put_str( AOC::Jz, "l" + to_string( instr.imm ), instr.ifi );
        } else if ( instr.type == MT::Func ) {
            // Assume clean state for all registers.
            put_label( get_fn_label( mir, mir.func_label_to_symbol[instr.imm] ),
                       instr.ifi );
            curr_fn_info = mir.func_map[mir.func_label_to_symbol[instr.imm]];
            size_t local_bytes = 8 * ( curr_fn_info.max_register_used + 1 -
                                       std::min( curr_fn_info.max_register_used,
                                                 virtual_stack_start_reg ) );
            put_imm( AOC::Enter, local_bytes, instr.ifi );
            curr_frame_size = local_bytes + 16; // also return address and bp
            reg_states.clear();
            loaded_parm_count = 0;
            save_callee_registers( curr_fn_info.max_register_used, instr.ifi );
        } else if ( instr.type == MT::Param ) {
            size_t addr_offset = 16 + 8 * ( loaded_parm_count++ );
            put_reg_imm( AOC::MovFromStack64, HwReg::eax, addr_offset,
                         instr.ifi );
            writeback_opt( instr.result, HwReg::eax, instr.ifi );
        } else if ( instr.type == MT::Arg ) {
            save_before_params_on_demand(
                mir.func_map[mir.func_label_to_symbol[instr.imm]], instr.ifi );

            // Put argument on stack. Reverse order is already ensured.
            auto val = make_available( instr.p0, instr.ifi );
            put_src_reg( AOC::Push, val, instr.ifi );
        } else if ( instr.type == MT::Call ) {
            auto callee_fn_info =
                mir.func_map[mir.func_label_to_symbol[instr.imm]];
            save_before_params_on_demand( callee_fn_info, instr.ifi );

            // Actual call with stack alignment
            put_str( AOC::Call,
                     get_fn_label( mir, mir.func_label_to_symbol[instr.imm] ),
                     instr.ifi );

            // Temporarily memorize result
            put_reg_reg( AOC::Mov64, HwReg::r10d, HwReg::eax, instr.ifi );

            // Pop arguments from stack by decrementing esp
            put_imm( AOC::AddSp, callee_fn_info.arg_types.size() * 8,
                     instr.ifi );

            // Remove alignment
            i32 align_delta =
                curr_frame_size + callee_fn_info.arg_types.size() * 8;
            put_imm( AOC::AddSp, 16 - ( align_delta % 16 ), instr.ifi );

            // Restore previously saved registers
            restore_caller_registers( callee_fn_info.max_register_used,
                                      instr.ifi );

            // Result after registers were restored to prevent overwriting
            writeback_opt( instr.result, HwReg::r10d, instr.ifi );
            saved_caller_registers = false;
        } else if ( instr.type == MT::TypeCast ) {
            // Basically the same as a move
            auto src_reg = make_available( instr.p0, instr.ifi );
            writeback_opt( instr.result, src_reg, instr.ifi );
        } else if ( instr.type == MT::FieldRead ||
                    instr.type == MT::IndirectRead ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.result )] );
            size_t offset = get_struct_field_offset(
                mir, mir.var_struct_symbols[instr.p0], instr.name );

            put_reg_imm( AOC::MovConst, HwReg::eax, offset, instr.ifi );
            auto base_reg = make_available( instr.p0, instr.ifi );
            put_reg_reg( AOC::Add64, HwReg::eax, base_reg, instr.ifi );

            if ( mir.map_to_type_spec[mir.type_of( instr.result )].type !=
                 TypeSpecifier::Type::Struct ) {
                // Small types must be loaded from memory
                if ( type_size > 4 ) {
                    put_reg_reg( AOC::MovIndrFrom64, HwReg::eax, HwReg::eax,
                                 instr.ifi );
                } else {
                    put_reg_reg( AOC::MovIndrFrom, HwReg::eax, HwReg::eax,
                                 instr.ifi );
                }
            }
            writeback_opt( instr.result, HwReg::eax, instr.ifi );
        } else if ( instr.type == MT::ArrayRead ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.result )] );
            type_size = std::max<size_t>( type_size, 1 ); // No zero alloc

            make_available_in( instr.p1, HwReg::eax, instr.ifi );
            make_available_in( instr.p0, HwReg::edx, instr.ifi );
            // eax now contains elem-offset and edx contains base address.

            // Do bounds checking
            put_reg_imm( AOC::Sub64Const, HwReg::edx, 8, no_ifi );
            put_reg_reg( AOC::MovIndrFrom, HwReg::edx, HwReg::edx, no_ifi );
            put_reg_reg( AOC::Cmp, HwReg::eax, HwReg::edx, no_ifi );
            put_str( AOC::Jnb, "sigabrt", no_ifi );
            put_reg_imm( AOC::MovConst, HwReg::edx, 0, no_ifi );
            put_reg_reg( AOC::Cmp, HwReg::edx, HwReg::eax, no_ifi );
            put_str( AOC::Jnle, "sigabrt", no_ifi );
            // Later need to reload base address, because edx was overwritten

            // Final address calculation
            put_reg_imm( AOC::MovConst, HwReg::eax, type_size, instr.ifi );
            auto count_reg = make_available( instr.p1, instr.ifi );
            put_reg_reg( AOC::IMul, HwReg::eax, count_reg, instr.ifi );
            auto base_reg = make_available( instr.p0, instr.ifi );
            put_reg_reg( AOC::Add64, HwReg::eax, base_reg, instr.ifi );

            if ( mir.map_to_type_spec[mir.type_of( instr.result )].type !=
                 TypeSpecifier::Type::Struct ) {
                // Small types must be loaded from memory
                if ( type_size > 4 ) {
                    put_reg_reg( AOC::MovIndrFrom64, HwReg::eax, HwReg::eax,
                                 instr.ifi );
                } else {
                    put_reg_reg( AOC::MovIndrFrom, HwReg::eax, HwReg::eax,
                                 instr.ifi );
                }
            }
            writeback_opt( instr.result, HwReg::eax, instr.ifi );
        } else if ( instr.type == MT::FieldWrite ||
                    instr.type == MT::IndirectWrite ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.p0 )] );
            // Here instr.result has a special role, as it is also a parameter.
            size_t offset = get_struct_field_offset(
                mir, mir.var_struct_symbols[instr.result], instr.name );

            put_reg_imm( AOC::MovConst, HwReg::eax, offset, instr.ifi );
            auto base_reg = make_available( instr.result, instr.ifi );
            put_reg_reg( AOC::Add64, HwReg::eax, base_reg, instr.ifi );

            auto to_store_reg = make_available( instr.p0, instr.ifi );
            if ( type_size > 4 ) {
                put_reg_reg( AOC::MovIndrTo64, HwReg::eax, to_store_reg,
                             instr.ifi );
            } else {
                put_reg_reg( AOC::MovIndrTo, HwReg::eax, to_store_reg,
                             instr.ifi );
            }
        } else if ( instr.type == MT::ArrayWrite ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.p1 )] );
            type_size = std::max<size_t>( type_size, 1 ); // No zero alloc

            make_available_in( instr.p0, HwReg::eax, instr.ifi );
            // Here instr.result has a special role, as it is also a parameter.
            make_available_in( instr.result, HwReg::edx, instr.ifi );
            // eax now contains elem-offset and edx contains base address.

            // Do bounds checking
            put_reg_imm( AOC::Sub64Const, HwReg::edx, 8, no_ifi );
            put_reg_reg( AOC::MovIndrFrom, HwReg::edx, HwReg::edx, no_ifi );
            put_reg_reg( AOC::Cmp, HwReg::eax, HwReg::edx, no_ifi );
            put_str( AOC::Jnb, "sigabrt", no_ifi );
            put_reg_imm( AOC::MovConst, HwReg::edx, 0, no_ifi );
            put_reg_reg( AOC::Cmp, HwReg::edx, HwReg::eax, no_ifi );
            put_str( AOC::Jnle, "sigabrt", no_ifi );
            // Later need to reload base address, because edx was overwritten

            // Final address calculation
            put_reg_imm( AOC::MovConst, HwReg::eax, type_size, instr.ifi );
            auto count_reg = make_available( instr.p0, instr.ifi );
            put_reg_reg( AOC::IMul, HwReg::eax, count_reg, instr.ifi );
            auto base_reg = make_available( instr.result, instr.ifi );
            put_reg_reg( AOC::Add64, HwReg::eax, base_reg, instr.ifi );

            auto to_store_reg = make_available( instr.p1, instr.ifi );
            if ( type_size > 4 ) {
                put_reg_reg( AOC::MovIndrTo64, HwReg::eax, to_store_reg,
                             instr.ifi );
            } else {
                put_reg_reg( AOC::MovIndrTo, HwReg::eax, to_store_reg,
                             instr.ifi );
            }
        } else if ( instr.type == MT::ReadMem ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.p0 )] );
            make_available_in( instr.p0, HwReg::eax, instr.ifi );

            if ( type_size > 4 ) {
                put_reg_reg( AOC::MovIndrFrom64, HwReg::eax, HwReg::eax,
                             instr.ifi );
            } else {
                put_reg_reg( AOC::MovIndrFrom, HwReg::eax, HwReg::eax,
                             instr.ifi );
            }
            writeback_opt( instr.result, HwReg::eax, instr.ifi );
        } else if ( instr.type == MT::WriteMem ) {
            size_t type_size = get_type_size(
                mir, mir.map_to_type_spec[mir.type_of( instr.p0 )] );
            auto val_reg = make_available( instr.p0, instr.ifi );
            make_available_in( instr.result, HwReg::eax, instr.ifi );

            if ( type_size > 4 ) {
                put_reg_reg( AOC::MovIndrTo64, HwReg::eax, val_reg, instr.ifi );
            } else {
                put_reg_reg( AOC::MovIndrTo, HwReg::eax, val_reg, instr.ifi );
            }
        } else if ( instr.type != MT::Nop ) {
            // Unknown instruction
            make_error_msg(
                state,
                "Unknown mir instruction. This is probably a compiler bug.",
                instr.ifi, RetCode::InternalError );
        }
    } );
}

void generate_asm_text_x86( CompilerState &state,
                            const EagerContainer<Assembly_x86> &assembly,
                            String &out ) {
    auto put_asm = [&]( const String &content ) { out += content + "\n"; };

    auto to_reg_str = []( HwReg reg ) -> String {
        switch ( reg ) {
        case HwReg::eax:
            return "%eax";
        case HwReg::ebx:
            return "%ebx";
        case HwReg::ecx:
            return "%ecx";
        case HwReg::edx:
            return "%edx";
        case HwReg::edi:
            return "%edi";
        case HwReg::esi:
            return "%esi";
        case HwReg::ebp:
            return "%ebp";
        case HwReg::esp:
            return "%esp";
        case HwReg::r8d:
            return "%r8d";
        case HwReg::r9d:
            return "%r9d";
        case HwReg::r10d:
            return "%r10d";
        case HwReg::r11d:
            return "%r11d";
        case HwReg::r12d:
            return "%r12d";
        case HwReg::r13d:
            return "%r13d";
        case HwReg::r14d:
            return "%r14d";
        case HwReg::r15d:
            return "%r15d";
        default:
            return "%noreg";
        }
    };
    auto to_reg_64_str = []( HwReg reg ) -> String {
        switch ( reg ) {
        case HwReg::eax:
            return "%rax";
        case HwReg::ebx:
            return "%rbx";
        case HwReg::ecx:
            return "%rcx";
        case HwReg::edx:
            return "%rdx";
        case HwReg::edi:
            return "%rdi";
        case HwReg::esi:
            return "%rsi";
        case HwReg::ebp:
            return "%rbp";
        case HwReg::esp:
            return "%rsp";
        case HwReg::r8d:
            return "%r8";
        case HwReg::r9d:
            return "%r9";
        case HwReg::r10d:
            return "%r10";
        case HwReg::r11d:
            return "%r11";
        case HwReg::r12d:
            return "%r12";
        case HwReg::r13d:
            return "%r13";
        case HwReg::r14d:
            return "%r14";
        case HwReg::r15d:
            return "%r15";
        default:
            return "%noreg";
        }
    };

    auto make_reg_reg_op = [&]( const String &cmd, const Assembly_x86 &op ) {
        put_asm( cmd + " " + to_reg_str( op.src ) + ", " +
                 to_reg_str( op.dest ) );
    };
    auto make_reg_reg_op_64 = [&]( const String &cmd, const Assembly_x86 &op ) {
        put_asm( cmd + " " + to_reg_64_str( op.src ) + ", " +
                 to_reg_64_str( op.dest ) );
    };

    // Generate assembly code
    assembly.for_each( [&]( const Assembly_x86 &op ) {
        if ( op.opcode == AOC::Raw ) {
            put_asm( op.str );
        } else if ( op.opcode == AOC::Comment ) {
            put_asm( "/* " + op.str + " */" );
        } else if ( op.opcode == AOC::Global ) {
            put_asm( ".global " + op.str );
        } else if ( op.opcode == AOC::Text ) {
            put_asm( ".text" );
        } else if ( op.opcode == AOC::Extern ) {
            put_asm( ".extern " + op.str );
        } else if ( op.opcode == AOC::Label ) {
            put_asm( op.str + ":" );
        } else if ( op.opcode == AOC::Nop ) {
            put_asm( "nop" );
        } else if ( op.opcode == AOC::Call ) {
            put_asm( "call " + op.str );
        } else if ( op.opcode == AOC::Mov ) {
            make_reg_reg_op( "movl", op );
        } else if ( op.opcode == AOC::Mov64 ) {
            make_reg_reg_op_64( "movq", op );
        } else if ( op.opcode == AOC::MovConst ) {
            put_asm( "movl $" + to_string( op.imm ) + ", " +
                     to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::MovConst64 ) {
            put_asm( "movq $" + to_string( op.imm ) + ", " +
                     to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::MovFromStack ) {
            put_asm( "movl " + to_string( op.imm ) + "(%rbp), " +
                     to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::MovFromStack64 ) {
            put_asm( "movq " + to_string( op.imm ) + "(%rbp), " +
                     to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::MovToStack ) {
            put_asm( "movl " + to_reg_str( op.src ) + ", " +
                     to_string( op.imm ) + "(%rbp)" );
        } else if ( op.opcode == AOC::MovToStack64 ) {
            put_asm( "movq " + to_reg_64_str( op.src ) + ", " +
                     to_string( op.imm ) + "(%rbp)" );
        } else if ( op.opcode == AOC::MovZeroExtend ) {
            put_asm( "movzbl %al, %eax" );
        } else if ( op.opcode == AOC::MovSymbolWithRip64 ) {
            put_asm( "movq " + op.str + "(%rip), " + to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::MovIndrTo ) {
            put_asm( "movl " + to_reg_str( op.src ) + ", 0(" +
                     to_reg_64_str( op.dest ) + ")" );
        } else if ( op.opcode == AOC::MovIndrFrom ) {
            put_asm( "movl 0(" + to_reg_64_str( op.src ) + "), " +
                     to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::MovIndrTo64 ) {
            put_asm( "movq " + to_reg_64_str( op.src ) + ", 0(" +
                     to_reg_64_str( op.dest ) + ")" );
        } else if ( op.opcode == AOC::MovIndrFrom64 ) {
            put_asm( "movq 0(" + to_reg_64_str( op.src ) + "), " +
                     to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::Syscall ) {
            put_asm( "syscall" );
        } else if ( op.opcode == AOC::Add ) {
            make_reg_reg_op( "addl", op );
        } else if ( op.opcode == AOC::Add64 ) {
            make_reg_reg_op_64( "addq", op );
        } else if ( op.opcode == AOC::Sub ) {
            make_reg_reg_op( "subl", op );
        } else if ( op.opcode == AOC::Sub64 ) {
            make_reg_reg_op_64( "subq", op );
        } else if ( op.opcode == AOC::Sub64Const ) {
            put_asm( "subq $" + to_string( op.imm ) + ", " +
                     to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::IMul ) {
            make_reg_reg_op( "imull", op );
        } else if ( op.opcode == AOC::IDiv ) {
            make_reg_reg_op( "idivl", op );
        } else if ( op.opcode == AOC::Cltd ) {
            put_asm( "cltd" );
        } else if ( op.opcode == AOC::And ) {
            make_reg_reg_op( "andl", op );
        } else if ( op.opcode == AOC::Or ) {
            make_reg_reg_op( "orl", op );
        } else if ( op.opcode == AOC::Xor ) {
            make_reg_reg_op( "xorl", op );
        } else if ( op.opcode == AOC::Shl ) {
            put_asm( "sall %cl, " + to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::Shr ) {
            put_asm( "sarl %cl, " + to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::SetEq ) {
            put_asm( "sete %al" );
        } else if ( op.opcode == AOC::SetBelow ) {
            put_asm( "setl %al" );
        } else if ( op.opcode == AOC::SetBelowEq ) {
            put_asm( "setle %al" );
        } else if ( op.opcode == AOC::Jmp ) {
            put_asm( "jmp " + op.str );
        } else if ( op.opcode == AOC::Jz ) {
            put_asm( "jz " + op.str );
        } else if ( op.opcode == AOC::Jnb ) {
            put_asm( "jnb " + op.str );
        } else if ( op.opcode == AOC::Jnle ) {
            put_asm( "jnle " + op.str );
        } else if ( op.opcode == AOC::Cmp ) {
            make_reg_reg_op( "cmp", op );
        } else if ( op.opcode == AOC::Ret ) {
            put_asm( "ret" );
        } else if ( op.opcode == AOC::Push ) {
            put_asm( "push " + to_reg_64_str( op.src ) );
        } else if ( op.opcode == AOC::Pop ) {
            put_asm( "pop " + to_reg_64_str( op.dest ) );
        } else if ( op.opcode == AOC::AddSp ) {
            put_asm( "addq $" + to_string( op.imm ) + ", %rsp" );
        } else if ( op.opcode == AOC::Enter ) {
            put_asm( "enter $" + to_string( op.imm ) + ", $0" );
        } else if ( op.opcode == AOC::Leave ) {
            put_asm( "leave" );
        } else {
            make_error_msg(
                state,
                "Found invalid x86 instruction. This is an internal error.",
                op.ifi, RetCode::InternalError );
        }
    } );
}

} // namespace chc
