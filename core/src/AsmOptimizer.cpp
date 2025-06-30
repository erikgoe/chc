#include "../include/chc/AsmOptimizer.hpp"

namespace chc {

using AOC = Assembly_x86::OpCode;
using HwReg = Assembly_x86::HwReg;
using AItr = EagerContainer<Assembly_x86>::Iterator;

// Operations which have from-to-semantics with optionally an simple
// computation.
bool is_simple_operation( AOC opcode ) {
    const std::set simple_ops = { AOC::Nop,
                                  AOC::Mov,
                                  AOC::Mov64,
                                  AOC::MovConst,
                                  AOC::MovConst64,
                                  AOC::MovFromStack,
                                  AOC::MovFromStack64,
                                  AOC::MovToStack,
                                  AOC::MovZeroExtend,
                                  AOC::Add,
                                  AOC::Add64,
                                  AOC::Sub,
                                  AOC::Sub64,
                                  AOC::IMul,
                                  AOC::IDiv,
                                  AOC::And,
                                  AOC::Or,
                                  AOC::Xor,
                                  AOC::Shl,
                                  AOC::Shr };
    return simple_ops.find( opcode ) != simple_ops.end();
}

bool is_move_operation( AOC opcode ) {
    const std::set move_ops = { AOC::Mov,          AOC::Mov64,
                                AOC::MovConst,     AOC::MovConst64,
                                AOC::MovFromStack, AOC::MovFromStack64,
                                AOC::MovToStack,   AOC::MovZeroExtend };
    return move_ops.find( opcode ) != move_ops.end();
}

bool op_has_side_effect_on_reg( AOC op, HwReg reg ) {
    return ( reg == HwReg::eax && ( op == AOC::Call || op == AOC::Cltd ||
                                    op == AOC::IMul || op == AOC::IDiv ) ) ||
           ( reg == HwReg::edx && ( op == AOC::IDiv || op == AOC::Cltd ) );
}

/// Returns a list of all usages of the current register value, over all
/// following paths. Paths end when the register is overwritten.
std::vector<AItr> find_usages_of_register_value(
    const std::map<String, AItr> &jump_targets, HwReg reg, AItr start ) {
    assert( reg != HwReg::None );
    std::vector<AItr> ret;

    std::deque<AItr> to_check;
    to_check.push_back( start.skip( 1 ) );
    std::set<AItr> already_checked;
    already_checked.insert( start );

    // Iterate all paths
    while ( !to_check.empty() ) {
        auto itr = to_check.front();
        to_check.pop_front();
        if ( !itr || already_checked.find( itr ) != already_checked.end() )
            continue; // Cycle detected (or invalid iterator)

        auto instr = itr.get();

        if ( instr.opcode == AOC::Ret ) {
            continue;
        }

        // Check usage
        if ( instr.src == reg ) {
            ret.push_back( itr );
        }

        // Successor paths
        if ( instr.dest == reg ||
             op_has_side_effect_on_reg( instr.opcode, reg ) ) {
            // Overwritten in path
            continue;
        } else if ( instr.opcode == AOC::Jmp ) {
            auto target = jump_targets.at( instr.str );
            to_check.push_front( target );
            already_checked.insert( target );
        } else if ( instr.opcode == AOC::Jz ) {
            // Branch
            auto target = jump_targets.at( instr.str );
            to_check.push_front( target );
            already_checked.insert( target );
            to_check.push_front( itr.skip() );
            already_checked.insert( itr.skip() );
        } else {
            // Regular successor
            to_check.push_front( itr.skip() );
        }
    }

    return ret;
}

/// Returns one of the next positions at which the register is overwritten. If
/// \param end is reached the path is considered terminated.
Opt<AItr> find_next_write_to_register_before(
    const std::map<String, AItr> &jump_targets, HwReg reg, AItr start,
    AItr end ) {
    assert( reg != HwReg::None );

    std::deque<AItr> to_check;
    to_check.push_back( start.skip( 1 ) );
    std::set<AItr> already_checked;
    already_checked.insert( start );
    already_checked.insert( end );

    // Iterate all paths
    while ( !to_check.empty() ) {
        auto itr = to_check.front();
        to_check.pop_front();
        if ( !itr || already_checked.find( itr ) != already_checked.end() )
            continue; // Cycle detected (or invalid iterator)

        auto instr = itr.get();

        if ( instr.opcode == AOC::Ret ) {
            continue;
        }

        // Check usage
        if ( instr.dest == reg || instr.opcode == AOC::Label ||
             op_has_side_effect_on_reg( instr.opcode, reg ) ) {
            // Return only the first occurrence.
            // Labels are also bad. They imply loops => too hard for now.
            return itr;
        }

        // Successor paths
        if ( instr.opcode == AOC::Jmp ) {
            auto target = jump_targets.at( instr.str );
            if ( target < itr )
                return target; // Implies loops => too hard for now
            to_check.push_front( target );
            already_checked.insert( target );
        } else if ( instr.opcode == AOC::Jz ) {
            // Branch
            auto target = jump_targets.at( instr.str );
            if ( target < itr )
                return target; // Implies loops => too hard for now
            to_check.push_front( target );
            already_checked.insert( target );
            to_check.push_front( itr.skip() );
            already_checked.insert( itr.skip() );
        } else {
            // Regular successor
            to_check.push_front( itr.skip() );
        }
    }

    return {}; // Nothing found
}

void optimize_asm( CompilerState &state,
                   EagerContainer<Assembly_x86> &assembly ) {
    // Find all jump targets
    std::map<String, AItr> jump_targets;
    auto itr = assembly.begin();
    while ( itr ) {
        if ( itr.get().opcode == AOC::Label )
            jump_targets.insert_or_assign( itr.get().str, itr );
        itr.skip_self();
    }

    // Implement all the local optimizations
    bool made_changes = true;
    auto none_op = Assembly_x86{};
    while ( made_changes ) {
        made_changes = false;
        itr = assembly.begin();
        while ( itr ) {
            auto &instr0 = itr.get();

            if ( instr0.src != HwReg::None && instr0.dest != HwReg::None &&
                 is_move_operation( instr0.opcode ) ) {
                // Simple local copy propagation
                auto dest_reg_users = find_usages_of_register_value(
                    jump_targets, instr0.dest, itr );
                if ( dest_reg_users.size() == 1 ) {
                    // Potential candidate
                    auto &target_instr = dest_reg_users.front().get();
                    if ( is_simple_operation( target_instr.opcode ) ) {
                        auto src_writes = find_next_write_to_register_before(
                            jump_targets, instr0.src, itr,
                            dest_reg_users.front() );
                        if ( !src_writes.has_value() ) {
                            // Can propagate the register
                            target_instr.src = instr0.src;
                            instr0 = Assembly_x86{
                                AOC::Nop
                            }; // Also clear register values!
                            made_changes = true;
                            log( "did copy propagation" );
                        }
                    }
                }
            }

            itr.skip_self();
        }
    }
}

} // namespace chc
