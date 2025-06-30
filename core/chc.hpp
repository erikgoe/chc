#pragma once

#include "include/chc/Core.hpp"

#ifdef CHC_IMPL
#include "src/Core.cpp"
#include "src/Message.cpp"
#include "src/Lexer.cpp"
#include "src/Parser.cpp"
#include "src/AstAnalysis.cpp"
#include "src/Mir.cpp"
#include "src/MirAnalysis.cpp"
#include "src/Codegen.cpp"
#include "src/AsmOptimizer.cpp"

#endif
