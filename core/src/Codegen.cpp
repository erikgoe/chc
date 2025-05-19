#include "../include/chc/Codegen.hpp"
#include "../include/chc/Message.hpp"

namespace chc {

using MI = Mir::MirInstr;
using MT = Mir::MirInstr::Type;
using VarId = Mir::VarId;
using RegId = Mir::RegId;

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

void generate_code_x86( CompilerState &state, Mir &mir, String &assembly ) {
    RegId stack_start_reg = 10;
    bool r10_curr_used =
        false; // whether r10 is already used for stack variables.

    // General utility functions
    auto put_asm = [&]( const String &mnemonic ) {
        assembly += mnemonic + "\n";
    };

    // Utility functions to manage (spilled) registers
    auto stack_preface = [&]( RegId reg ) -> const String {
        if ( mir.reg_mapping[reg] >= stack_start_reg ) {
            // Must load register from stack
            size_t addr_offset = 8 * ( mir.reg_mapping[reg] - stack_start_reg );
            if ( !r10_curr_used ) {
                put_asm( "movq " + to_string( addr_offset ) + "(%rsp), %r10" );
                r10_curr_used = true;
                return "%r10";
            } else {
                put_asm( "movq " + to_string( addr_offset ) + "(%rsp), %r11" );
                return "%r11";
            }
        } else {
            // Map normal registers
            switch ( mir.reg_mapping[reg] ) {
            case 0:
                return "%rbx";
            case 1:
                return "%rcx";
            case 2:
                return "%rsi";
            case 3:
                return "%rdi";
            case 4:
                return "%r8";
            case 5:
                return "%r9";
            case 6:
                return "%r12";
            case 7:
                return "%r13";
            case 8:
                return "%r14";
            case 9:
                return "%r15";
            default:
                return "%noreg";
            }
        }
    };
    auto stack_epilog = [&]( RegId reg, const String &used_reg,
                             bool writeback ) {
        size_t addr_offset = 8 * ( mir.reg_mapping[reg] - stack_start_reg );
        if ( used_reg == "%r10" ) {
            r10_curr_used = false;
            if ( writeback )
                put_asm( "movq %r10, " + to_string( addr_offset ) + "(%rsp)" );
        } else if ( used_reg == "%r11" ) {
            if ( writeback )
                put_asm( "movq %r11, " + to_string( addr_offset ) + "(%rsp)" );
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
             to_string( 8 * ( mir.reg_count -
                              std::min( mir.reg_count, stack_start_reg ) ) ) +
             ", $0" );

    // Add all instructions
    mir.instrs.for_each( [&]( const Mir::MirInstr &instr ) {
        if ( instr.type == MT::Const ) {
            put_asm_ex_reg( "movq", "$" + to_string( instr.imm ), instr.result,
                            true );
        } else if ( instr.type == MT::Mov ) {
            put_asm_reg_reg( "movq", instr.p0, instr.result, false, true );
        } else if ( instr.type == MT::Add ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
            put_asm_reg_ex( "addq", instr.p1, "%rax", false );
            put_asm_ex_reg( "movq", "%rax", instr.result, true );
        } else if ( instr.type == MT::Sub ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
            put_asm_reg_ex( "subq", instr.p1, "%rax", false );
            put_asm_ex_reg( "movq", "%rax", instr.result, true );
        } else if ( instr.type == MT::Mul ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
            put_asm_reg_ex( "imulq", instr.p1, "%rax", false );
            put_asm_ex_reg( "movq", "%rax", instr.result, true );
        } else if ( instr.type == MT::Div ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
            put_asm( "cltd" );
            put_asm_reg( "idivq", instr.p1, false );
            put_asm_ex_reg( "movq", "%rax", instr.result, true );
        } else if ( instr.type == MT::Mod ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
            put_asm( "cltd" );
            put_asm_reg( "idivq", instr.p1, false );
            put_asm_ex_reg( "movq", "%rdx", instr.result, true );
        } else if ( instr.type == MT::Ret ) {
            put_asm_reg_ex( "movq", instr.p0, "%rax", false );
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
