#pragma once
#include "pch.hpp"
#include "Parser.hpp"
#include "AstNodeFacades.hpp"

namespace chc {

/// Intermediate language that is close to assembly
struct Mir {
    using VarId = size_t;
    using RegId = size_t; // Zero is defined as invalid register
    using TypeId = size_t;

    struct MirInstr {
        enum class Type {
            None,

            Nop, // Not the same as "None"
            Label,
            Const,
            Mov,
            BinOp,
            Ret,
            Jmp,
            JZero,

            count
        } type = Type::None;

        VarId result = 0;
        VarId p0 = 0;
        VarId p1 = 0;

        i32 imm = 0; // Also used for labels and jumps

        InFileInfo ifi;

        ArithType subtype = ArithType::None;
        TypeId result_type = 0; // TODO

        std::set<VarId> live;
        std::set<VarId> needed; // Needed variables

        String type_name() const;
    };

    EagerContainer<MirInstr> instrs;
    std::map<SymbolId, VarId> var_map;
    VarId next_var = 1;
    VarId next_type = 1; // TODO

    i32 next_label = 1;
    std::deque<i32> continue_stack;
    std::deque<i32> break_stack;
    std::vector<Opt<decltype( instrs )::Iterator>>
        jump_table; // Maps label ids to instr indices.

    std::vector<RegId> reg_mapping;
    RegId reg_count = 0; // Maximum used registers

    static constexpr TypeId TYPE_INT = 1;
    static constexpr TypeId TYPE_BOOL = 2;
};

void analyze_liveness( CompilerState &state, Mir &mir );

void analyze_neededness( CompilerState &state, Mir &mir );

void trim_dead_code( CompilerState &state, Mir &mir );

void create_register_mapping( CompilerState &state, Mir &mir );

Mir construct_mir( CompilerState &state, AstNode &root_node );

} // namespace chc
