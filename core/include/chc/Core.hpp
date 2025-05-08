#pragma once
#include "pch.hpp"

namespace chc {

/// Configuration of the compiler.
struct CompilerConf {
    std::vector<String> in_files;
    String out_file;
};

/// General compilation state.
struct CompilerState {
    i8 ret_code = 0;
};

/// The main class of the compiler.
class Core {
    CompilerConf conf;
    CompilerState state;

public:
    Core( const CompilerConf &conf ) : conf( conf ) {}

    /// Does the whole compilation including all passes.
    void compile();

    /// Returns the return code of the compiler.
    i8 get_ret_code() { return state.ret_code; }
};

} // namespace chc
