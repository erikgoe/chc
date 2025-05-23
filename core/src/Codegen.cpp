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

void generate_code_x86( CompilerState &state, const String &original_source,
                        Mir &mir, EagerContainer<Assembly_x86> &assembly ) {
    constexpr HwReg no_reg = HwReg::None;
    RegId virtual_stack_start_reg = 12; // DEBUG
    InFileInfo no_ifi;

    // Source code lines
#ifdef AMS_CODE_LINE_COMMENTS
    String clean_source;
    make_clean_code_str( original_source, clean_source );
    std::vector<String> source_lines;
    split_into_lines( clean_source, source_lines );
#endif

    // Functions to generate single instructions
    auto put_comment = [&]( const String &content, const InFileInfo &ifi ) {
        assembly.put(
            Assembly_x86{ AOC::Comment, no_reg, no_reg, 0, content, ifi } );
    };
    (void) put_comment; // Ignore unused in release build.
    auto put_reg_reg = [&]( AOC opcode, HwReg dest, HwReg src,
                            const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, src, 0, "", ifi } );
    };
    auto put_reg = [&]( AOC opcode, HwReg dest, const InFileInfo &ifi ) {
        assembly.put( Assembly_x86{ opcode, dest, no_reg, 0, "", ifi } );
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
                return HwReg::ebx;
            case 2:
                return HwReg::ecx;
            case 3:
                return HwReg::esi;
            case 4:
                return HwReg::edi;
            case 5:
                return HwReg::r8d;
            case 6:
                return HwReg::r9d;
            case 7:
                return HwReg::r11d;
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
        size_t addr_offset = -4 * ( reg - virtual_stack_start_reg + 1 );
        put_reg_imm( AOC::MovFromStack, HwReg::r10d, addr_offset, ifi );
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
                put_reg_reg( AOC::Mov, dest_reg, hw_reg, ifi );
            }

            return;
        }

        // Load from stack first
        size_t addr_offset = -4 * ( reg - virtual_stack_start_reg + 1 );
        put_reg_imm( AOC::MovFromStack, dest_reg, addr_offset, ifi );
    };
    // Write back to stack if necessary
    auto writeback_opt = [&]( VarId to_var, HwReg src_reg,
                              const InFileInfo &ifi ) {
        RegId reg = mir.reg_mapping[to_var];
        if ( reg < virtual_stack_start_reg ) {
            if ( to_hw_reg( reg ) != src_reg ) {
                // Copy back into original register.
                put_reg_reg( AOC::Mov, to_hw_reg( reg ), src_reg, ifi );
            }

            return;
        }

        // Write back to stack
        size_t addr_offset = -4 * ( reg - virtual_stack_start_reg + 1 );
        put_src_reg_imm( AOC::MovToStack, src_reg, addr_offset, ifi );
    };

    // Add preamble
    put_str( AOC::Global, "main", no_ifi );
    put_str( AOC::Global, "_main", no_ifi );
    put_empty( AOC::Text, no_ifi );
    put_label( "main", no_ifi );
    put_str( AOC::Call, "_main", no_ifi );
    put_reg_reg( AOC::Mov, HwReg::edi, HwReg::eax, no_ifi );
    put_reg_imm( AOC::MovConst, HwReg::eax, 0x3c, no_ifi );
    put_empty( AOC::Syscall, no_ifi );
    put_label( "_main", no_ifi );

    // Add main "function" preface
    put_imm( AOC::Enter,
             4 * ( mir.reg_count -
                   std::min( mir.reg_count, virtual_stack_start_reg ) ),
             no_ifi );

    // Add all instructions
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
                aoc = AOC::Add;
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
            put_empty( AOC::Leave, instr.ifi );
            put_empty( AOC::Ret, instr.ifi );
        } else if ( instr.type == MT::Jmp ) {
            put_str( AOC::Jmp, "l" + to_string( instr.imm ), instr.ifi );
        } else if ( instr.type == MT::JZero ) {
            auto val = make_available( instr.p0, instr.ifi );
            put_reg_imm( AOC::MovConst, HwReg::eax, 0, instr.ifi );
            put_reg_reg( AOC::Cmp, HwReg::eax, val, instr.ifi );
            put_str( AOC::Jz, "l" + to_string( instr.imm ), instr.ifi );
        } else if ( instr.type != MT::Nop ) {
            // Unknown instruction
            make_error_msg(
                state,
                "Unknown mir instruction. This is probably an compiler bug.",
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

    auto make_reg_reg_op = [&]( const String &cmd, const Assembly_x86 &op ) {
        put_asm( cmd + " " + to_reg_str( op.src ) + ", " +
                 to_reg_str( op.dest ) );
    };

    // Generate assembly code
    assembly.for_each( [&]( const Assembly_x86 &op ) {
        if ( op.opcode == AOC::Comment ) {
            put_asm( "/* " + op.str + " */" );
        } else if ( op.opcode == AOC::Global ) {
            put_asm( ".global " + op.str );
        } else if ( op.opcode == AOC::Text ) {
            put_asm( ".text" );
        } else if ( op.opcode == AOC::Label ) {
            put_asm( op.str + ":" );
        } else if ( op.opcode == AOC::Nop ) {
            put_asm( "nop" );
        } else if ( op.opcode == AOC::Call ) {
            put_asm( "call " + op.str );
        } else if ( op.opcode == AOC::Mov ) {
            make_reg_reg_op( "movl", op );
        } else if ( op.opcode == AOC::MovConst ) {
            put_asm( "movl $" + to_string( op.imm ) + ", " +
                     to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::MovFromStack ) {
            put_asm( "movl " + to_string( op.imm ) + "(%rbp), " +
                     to_reg_str( op.dest ) );
        } else if ( op.opcode == AOC::MovToStack ) {
            put_asm( "movl " + to_reg_str( op.src ) + ", " +
                     to_string( op.imm ) + "(%rbp)" );
        } else if ( op.opcode == AOC::Syscall ) {
            put_asm( "syscall" );
        } else if ( op.opcode == AOC::Add ) {
            make_reg_reg_op( "addl", op );
        } else if ( op.opcode == AOC::Sub ) {
            make_reg_reg_op( "subl", op );
        } else if ( op.opcode == AOC::IMul ) {
            make_reg_reg_op( "imull", op );
        } else if ( op.opcode == AOC::IDiv ) {
            make_reg_reg_op( "idivl", op );
        } else if ( op.opcode == AOC::Cltd ) {
            put_asm( "cltd" );
        } else if ( op.opcode == AOC::And ) {
            put_asm( "and" );
        } else if ( op.opcode == AOC::Or ) {
            put_asm( "or" );
        } else if ( op.opcode == AOC::Xor ) {
            put_asm( "xor" );
        } else if ( op.opcode == AOC::Shl ) {
            put_asm( "sal" );
        } else if ( op.opcode == AOC::Shr ) {
            put_asm( "sar" );
        } else if ( op.opcode == AOC::SetEq ) {
            put_asm( "sete" );
        } else if ( op.opcode == AOC::SetBelow ) {
            put_asm( "setb" );
        } else if ( op.opcode == AOC::SetBelowEq ) {
            put_asm( "setbe" );
        } else if ( op.opcode == AOC::Jmp ) {
            put_asm( "jmp" );
        } else if ( op.opcode == AOC::Jz ) {
            put_asm( "jz" );
        } else if ( op.opcode == AOC::Cmp ) {
            put_asm( "cmp" );
        } else if ( op.opcode == AOC::Ret ) {
            put_asm( "ret" );
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
