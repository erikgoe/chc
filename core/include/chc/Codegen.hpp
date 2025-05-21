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

        Comment, // block comment
        Global, // ".global <str>"
        Text, // ".text"
        Label, // "<str>:"

        Nop,
        Call, // "call <str>"
        Mov, // "mov <src>, <dest>"
        MovConst, // "mov $<imm>, <dest>"
        MovFromStack, // "movl $<imm>(%esp), <dest>"
        MovToStack, // "movl <src>, $<imm>(%esp)"
        Syscall,
        Add,
        Sub,
        IMul,
        IDiv,
        Cltd,
        Ret,

        Enter, // "enter $<imm>, $0"
        Leave,

        count
    } opcode;

    HwReg dest = HwReg::None;
    HwReg src = HwReg::None;
    i32 imm = 0;

    String str;

    InFileInfo ifi;
};

void generate_code_x86( CompilerState &state, const String &original_source,
                        Mir &mir, EagerContainer<Assembly_x86> &assembly );

void generate_asm_text_x86( CompilerState &state,
                            const EagerContainer<Assembly_x86> &assembly,
                            String &out );

} // namespace chc
