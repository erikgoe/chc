#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

/// Intermediate language that is close to assembly
struct Mir {
    using VarId = size_t;
    using RegId = size_t; // Zero is defined as invalid register

    struct MirInstr {
        enum class Type {
            None,

            Nop, // Not the same as "None"
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

        std::set<VarId> life;
        std::set<VarId> needed; // Needed variables
    };

    EagerContainer<MirInstr> instrs;
    std::map<SymbolId, VarId> var_map;
    VarId next_var = 0;

    std::vector<RegId> reg_mapping;
    RegId reg_count = 0; // Maximum used registers
};

void analyze_liveness( CompilerState &state, Mir &mir );

void analyze_neededness( CompilerState &state, Mir &mir );

void trim_dead_code( CompilerState &state, Mir &mir );

void create_register_mapping( CompilerState &state, Mir &mir );

Mir construct_mir( CompilerState &state, AstNode &root_node );

} // namespace chc
