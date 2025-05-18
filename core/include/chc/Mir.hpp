#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

/// Intermediate language that is close to assembly
struct Mir {
    using VarId = size_t;

    struct MirInstr {
        enum class Type {
            None,

            Nop,  // Not the same as "None"
            Const,
            Mov,
            Add,
            Sub,
            Mul,
            Div,
            Mod,
            Ret,

            count
        } type = Type::None;

        VarId result;
        VarId p0;
        VarId p1;

        i32 imm;

        InFileInfo ifi;
    };

    EagerContainer<MirInstr> instrs;
    std::map<SymbolId, VarId> var_map;
    VarId next_var = 0;
};

Mir construct_mir( CompilerState &state, AstNode &root_node );

} // namespace chc
