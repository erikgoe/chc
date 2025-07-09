#pragma once
#include "pch.hpp"
#include "Mir.hpp"

namespace chc {

/// Slightly more abstract representation of an instruction in x86-64.
struct Assembly_x86 {
    enum class HwReg {
        None,

        eax,
        ebx,
        ecx,
        edx,
        edi,
        esi,
        ebp,
        esp,
        r8d,
        r9d,
        r10d, // currently used for stack variables
        r11d,
        r12d,
        r13d,
        r14d,
        r15d,

        count
    };

    enum class OpCode {
        None,

        Raw, // raw assembly line
        Comment, // block comment
        Global, // ".global <str>"
        Text, // ".text"
        Extern, // ".extern <str>"
        Label, // "<str>:"

        Nop,
        Call, // "call <str>"
        Mov, // "movl <src>, <dest>"
        Mov64, // "movq <src>, <dest>"
        MovConst, // "mov $<imm>, <dest>"
        MovConst64, // "mov $<imm>, <dest>"
        MovFromStack, // "movl $<imm>(%rbp), <dest>"
        MovFromStack64, // "movq $<imm>(%rbp), <dest>"
        MovToStack, // "movl <src>, $<imm>(%rbp)"
        MovZeroExtend, // "movzbl %al, %eax"
        MovSymbolWithRip64, // "movl <str>(%rip), <dest>"
        MovIndrTo, // "movq 0(<src>), <dest>"
        MovIndrFrom, // "movq <src>, 0(<dest>)"
        Syscall,
        Add,
        Add64,
        Sub,
        Sub64,
        Sub64Const, // "subq $<imm>, <dest>"
        IMul,
        IDiv,
        Cltd,
        And,
        Or,
        Xor,
        Shl,
        Shr,
        SetEq,
        SetBelow,
        SetBelowEq,
        Jmp,
        Jz,
        Jnb,
        Jnbe,
        Cmp,
        Ret,
        Push,
        Pop,
        AddSp, // "addq $<imm>, %rsp"

        Enter, // "enter $<imm>, $0"
        Leave,

        count
    } opcode;

    HwReg dest = HwReg::None;
    HwReg src = HwReg::None;
    i32 imm = 0; // Immediate values, but also label and function ids.

    String str;

    InFileInfo ifi;
};

void generate_code_x86( CompilerState &state, const String &original_source,
                        Mir &mir, SemanticData &semantic_data,
                        EagerContainer<Assembly_x86> &assembly );

void generate_asm_text_x86( CompilerState &state,
                            const EagerContainer<Assembly_x86> &assembly,
                            String &out );

} // namespace chc
