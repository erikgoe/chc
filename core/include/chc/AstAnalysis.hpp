#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

struct SymbolDecl {
    SymbolId id;
    InFileInfo ifi;
};
struct SemanticData {
    std::map<String, SymbolId> build_in_symbols;

    std::unordered_map<String, SymbolDecl> symbol_map;
    std::unordered_map<String, SymbolDecl> func_map;
    SymbolId next_symbol = 1;
};

void basic_semantic_checks( CompilerState &state, SemanticData &semantic_data,
                            AstNode &root_node );

void operator_transformation( CompilerState &state, AstNode &root_node );

} // namespace chc
