#pragma once
#include "pch.hpp"
#include "Mir.hpp"

namespace chc {

using RegId = size_t;

// TODO
/* struct Assembly_x86 {
    enum class OpCode {
        None,

        Global, // ".global <str>"
        Text, // ".text"
        Label, // "<str>:"

        Nop,
        Call,
        MovQ,
        Syscall,
        Add,
        Sub,
        IMul,
        IDiv,
        Ret,

        count
    };

    RegId reg0;
    RegId reg1;
    i32 imm;

    String str;
}; */

void generate_code_x86( CompilerState &state, Mir &mir, String &assembly );

} // namespace chc
