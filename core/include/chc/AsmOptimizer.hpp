#pragma once
#include "pch.hpp"
#include "Codegen.hpp"

namespace chc {

void optimize_asm( CompilerState &state,
                   EagerContainer<Assembly_x86> &assembly );

}
