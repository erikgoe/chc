#pragma once
#include "pch.hpp"
#include "Parser.hpp"

namespace chc {

void basic_semantic_checks( CompilerState &state, AstNode &root_node );

void operator_transformation( CompilerState &state, AstNode &root_node );

} // namespace chc
