#include "../include/chc/Codegen.hpp"
#include "../include/chc/Message.hpp"

namespace chc {

using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;
using RegId = Mir::RegId;

#ifndef NDEBUG
#define AMS_CODE_LINE_COMMENTS
#endif

constexpr char const *asm_preamble = R"(
.global main
.global _main
.text
main:
call _main
movq %rax, %rdi
movq $0x3C, %rax
syscall
_main:)";

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
                        Mir &mir, String &assembly ) {
    RegId stack_start_reg = 10;
    bool r10_curr_used =
        false; // whether r10 is already used for stack variables.

    // Source code lines
#ifdef AMS_CODE_LINE_COMMENTS
    String clean_source;
    make_clean_code_str( original_source, clean_source );
    std::vector<String> source_lines;
    split_into_lines( clean_source, source_lines );
#endif

    // General utility functions
    auto put_asm = [&]( const String &mnemonic ) {
        assembly += mnemonic + "\n";
    };

    // Utility functions to manage (spilled) registers
    auto stack_preface = [&]( RegId reg ) -> const String {
        if ( mir.reg_mapping[reg] >= stack_start_reg ) {
            // Must load register from stack
            size_t addr_offset = 4 * ( mir.reg_mapping[reg] - stack_start_reg );
            if ( !r10_curr_used ) {
                put_asm( "movl " + to_string( addr_offset ) + "(%esp), %r10d" );
                r10_curr_used = true;
                return "%r10d";
            } else {
                put_asm( "movl " + to_string( addr_offset ) + "(%esp), %r11d" );
                return "%r11d";
            }
        } else {
            // Map normal registers
            switch ( mir.reg_mapping[reg] ) {
            case 0:
                return "%ebx";
            case 1:
                return "%ecx";
            case 2:
                return "%esi";
            case 3:
                return "%edi";
            case 4:
                return "%r8d";
            case 5:
                return "%r9d";
            case 6:
                return "%r12d";
            case 7:
                return "%r13d";
            case 8:
                return "%r14d";
            case 9:
                return "%r15d";
            default:
                return "%noreg";
            }
        }
    };
    auto stack_epilog = [&]( RegId reg, const String &used_reg,
                             bool writeback ) {
        size_t addr_offset = 4 * ( mir.reg_mapping[reg] - stack_start_reg );
        if ( used_reg == "%r10d" ) {
            r10_curr_used = false;
            if ( writeback )
                put_asm( "movl %r10d, " + to_string( addr_offset ) + "(%esp)" );
        } else if ( used_reg == "%r11d" ) {
            if ( writeback )
                put_asm( "movl %r11d, " + to_string( addr_offset ) + "(%esp)" );
        }
    };

    // Utility functions to write assembly instructions
    auto put_asm_reg_reg = [&]( const String &mnemonic, RegId r0, RegId r1,
                                bool r0_writeback, bool r1_writeback ) {
        auto r0_name = stack_preface( r0 );
        auto r1_name = stack_preface( r1 );
        assembly += mnemonic + " " + r0_name + ", " + r1_name + "\n";
        stack_epilog( r1, r1_name, r1_writeback );
        stack_epilog( r0, r0_name, r0_writeback );
    };
    auto put_asm_reg_ex = [&]( const String &mnemonic, RegId r,
                               const String &ex, bool r_writeback ) {
        auto r_name = stack_preface( r );
        assembly += mnemonic + " " + r_name + ", " + ex + "\n";
        stack_epilog( r, r_name, r_writeback );
    };
    auto put_asm_ex_reg = [&]( const String &mnemonic, const String &ex,
                               RegId r, bool r_writeback ) {
        auto r_name = stack_preface( r );
        assembly += mnemonic + " " + ex + ", " + r_name + "\n";
        stack_epilog( r, r_name, r_writeback );
    };
    auto put_asm_reg = [&]( const String &mnemonic, RegId r,
                            bool r_writeback ) {
        auto r_name = stack_preface( r );
        assembly += mnemonic + " " + r_name + "\n";
        stack_epilog( r, r_name, r_writeback );
    };

    // Add preamble
    assembly += String( asm_preamble ) + "\n";

    // Add main "function" preface
    put_asm( "enter $" +
             to_string( 4 * ( mir.reg_count -
                              std::min( mir.reg_count, stack_start_reg ) ) ) +
             ", $0" );

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
            assembly += "/*=> " + source_lines[ln] + " */\n";
        }
        if ( instr.ifi != curr_ifi ) {
            curr_ifi = instr.ifi;
            assembly += "/* " +
                        clean_source.substr( curr_ifi.offset, curr_ifi.size ) +
                        " */\n";
        }
#endif

        // Actual instructions
        if ( instr.type == MT::Const ) {
            put_asm_ex_reg( "movl", "$" + to_string( instr.imm ), instr.result,
                            true );
        } else if ( instr.type == MT::Mov ) {
            put_asm_reg_reg( "movl", instr.p0, instr.result, false, true );
        } else if ( instr.type == MT::Add ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm_reg_ex( "addl", instr.p1, "%eax", false );
            put_asm_ex_reg( "movl", "%eax", instr.result, true );
        } else if ( instr.type == MT::Sub ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm_reg_ex( "subl", instr.p1, "%eax", false );
            put_asm_ex_reg( "movl", "%eax", instr.result, true );
        } else if ( instr.type == MT::Mul ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm_reg_ex( "imull", instr.p1, "%eax", false );
            put_asm_ex_reg( "movl", "%eax", instr.result, true );
        } else if ( instr.type == MT::Div ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm( "cltd" );
            put_asm_reg( "idivl", instr.p1, false );
            put_asm_ex_reg( "movl", "%eax", instr.result, true );
        } else if ( instr.type == MT::Mod ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm( "cltd" );
            put_asm_reg( "idivl", instr.p1, false );
            put_asm_ex_reg( "movl", "%edx", instr.result, true );
        } else if ( instr.type == MT::Ret ) {
            put_asm_reg_ex( "movl", instr.p0, "%eax", false );
            put_asm( "leave" );
            put_asm( "ret" );
        } else if ( instr.type != MT::Nop ) {
            // Unknown instruction
            make_error_msg(
                state,
                "Unknown mir instruction. This is probably an compiler bug.",
                instr.ifi, RetCode::InternalError );
        }
    } );
}

} // namespace chc
