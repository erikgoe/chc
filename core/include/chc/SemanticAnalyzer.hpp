#pragma once
#include "pch.hpp"
#include "Parser.hpp"
#include "Mir.hpp"

namespace chc {

void basic_semantic_checks( CompilerState &state, AstNode &root_node );

void operator_transformation( CompilerState &state, AstNode &root_node );

void use_before_init_and_return_check( CompilerState &state, Mir &mir );

void drop_uninit_instrs( CompilerState &state, Mir &mir );

void type_checking( CompilerState &state, Mir &mir );

} // namespace chc
