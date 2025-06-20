#pragma once
#include "pch.hpp"
#include "Parser.hpp"
#include "AstNodeFacades.hpp"
#include "AstAnalysis.hpp"

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
            Uninit,
            Func,
            Param,
            Arg,
            Call,

            count
        } type = Type::None;

        VarId result = 0;
        VarId p0 = 0;
        VarId p1 = 0;

        i32 imm = 0; // Also used for labels and jumps

        InFileInfo ifi;

        ArithType subtype = ArithType::None;
        TypeId type_constraint = 0;

        std::set<VarId> live;
        std::set<VarId> needed; // Needed variables

        bool reachable = false;

        String type_name() const;
    };
    struct FunctionInfo {
        TypeId ret_type;
        std::vector<TypeId> arg_types;
        i32 label;
        size_t max_register_used = 0;
    };

    EagerContainer<MirInstr> instrs;
    std::map<SymbolId, VarId> var_map;
    std::map<SymbolId, FunctionInfo> func_map;
    std::map<i32, SymbolId> func_label_to_symbol;
    VarId next_var = 1;
    VarId next_type = 1; // TODO

    i32 next_label = 1;
    std::deque<i32> continue_stack;
    std::deque<i32> break_stack;
    TypeId curr_fn_return_type;
    std::vector<Opt<decltype( instrs )::Iterator>>
        jump_table; // Maps label ids to instr indices.

    std::vector<RegId> reg_mapping;
    SymbolId main_function_symbol;

    std::vector<TypeId> types; // Maps variables to types
    TypeId &type_of( VarId var ) {
        if ( types.size() <= var )
            types.resize( var + 1 );
        return types[var];
    }

    static constexpr TypeId TYPE_INT = 1;
    static constexpr TypeId TYPE_BOOL = 2;
};

void analyze_liveness( CompilerState &state, Mir &mir );

void analyze_neededness( CompilerState &state, Mir &mir );

void trim_dead_code( CompilerState &state, Mir &mir );

void create_register_mapping( CompilerState &state, Mir &mir );

/// Calculates for every function how many registers are needed (i. e. written).
void count_function_registers( CompilerState &state, Mir &mir );

Mir construct_mir( CompilerState &state, SemanticData &semantic_data,
                   AstNode &root_node );

} // namespace chc
