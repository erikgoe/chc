#pragma once
#include "pch.hpp"
#include "Mir.hpp"

namespace chc {

void use_before_init_and_return_check( CompilerState &state, Mir &mir );

void drop_uninit_instrs( CompilerState &state, Mir &mir );

void type_checking( CompilerState &state, Mir &mir );

} // namespace chc
