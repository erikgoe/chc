#include "../include/chc/Mir.hpp"
#include "../include/chc/Message.hpp"
#include "../include/chc/AstNodeFacades.hpp"

namespace chc {

using AT = AstNode::Type;
using AstItr = EagerContainer<AstNode>::Iterator;

using namespace AstNodeFacades;

String name_of_instr( Mir::MirInstr::Type type ) {
    switch ( type ) {
    case Mir::MirInstr::Type::None:
        return "None";
    case Mir::MirInstr::Type::Nop:
        return "Nop";
    case Mir::MirInstr::Type::Const:
        return "Const";
    case Mir::MirInstr::Type::Mov:
        return "Mov";
    case Mir::MirInstr::Type::Add:
        return "Add";
    case Mir::MirInstr::Type::Sub:
        return "Sub";
    case Mir::MirInstr::Type::Mul:
        return "Mul";
    case Mir::MirInstr::Type::Div:
        return "Div";
    case Mir::MirInstr::Type::Mod:
        return "Mod";
    case Mir::MirInstr::Type::Ret:
        return "Ret";
    default:
        return "Unknown";
    }
}

void write_mir_instr( Mir &mir, AstNode &node, Mir::VarId into_var ) {
    using MI = Mir::MirInstr;
    using MT = Mir::MirInstr::Type;
    using VarId = Mir::VarId;

    if ( auto fn_def = FunctionDef( node ) ) {
        // TODO params
        auto itr = fn_def.stmts.itr();
        while ( itr ) {
            write_mir_instr( mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto ret = RetStmt( node ) ) {
        VarId tmp = mir.next_var++;
        write_mir_instr( mir, ret.value, tmp );
        mir.instrs.put( MI{ MT::Ret, tmp, 0, 0, 0, node.ifi } );
    } else if ( auto decl = DeclStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id.value()] = variable;
        write_mir_instr( mir, decl.init, variable );
    } else if ( auto decl = DeclUninitStmt( node ) ) {
        VarId variable = mir.next_var++;
        mir.var_map[decl.symbol_id.value()] = variable;
    } else if ( auto stmt = SimpStmt( node ) ) {
        assert( stmt.type == ArithType::None ); // Should already be handled in
                                                // operator_transformation()
        VarId variable = mir.var_map[stmt.lvalue.symbol_id.value()];
        write_mir_instr( mir, stmt.value, variable );
    } else if ( auto ident = Ident( node ) ) {
        VarId variable = mir.var_map[ident.id.value()];
        mir.instrs.put( MI{ MT::Mov, into_var, variable, 0, 0, node.ifi } );
    } else if ( auto paren = Paren( node ) ) {
        // TODO need to check that paren contains only one element
        auto child = paren.children.first()->get();
        write_mir_instr( mir, child, into_var );
    } else if ( auto block = Block( node ) ) {
        auto itr = block.children.itr();
        while ( itr ) {
            write_mir_instr( mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    } else if ( auto int_const = IntConst( node ) ) {
        mir.instrs.put(
            MI{ MT::Const, into_var, 0, 0, int_const.value, node.ifi } );
    } else if ( auto bin_op = BinOp( node ) ) {
        VarId tmp_lhs = mir.next_var++;
        VarId tmp_rhs = mir.next_var++;
        write_mir_instr( mir, bin_op.lhs, tmp_lhs );
        write_mir_instr( mir, bin_op.rhs, tmp_rhs );
        MT op = MT::None;
        if ( bin_op.type == ArithType::Add ) {
            op = MT::Add;
        } else if ( bin_op.type == ArithType::Sub ) {
            op = MT::Sub;
        } else if ( bin_op.type == ArithType::Mul ) {
            op = MT::Mul;
        } else if ( bin_op.type == ArithType::Div ) {
            op = MT::Div;
        } else if ( bin_op.type == ArithType::Mod ) {
            op = MT::Mod;
        }
        mir.instrs.put( MI{ op, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
    } else if ( auto uni_op = UniOp( node ) ) {
        VarId tmp_lhs = mir.next_var++;
        VarId tmp_rhs = mir.next_var++;
        write_mir_instr( mir, uni_op.rhs, tmp_rhs );
        if ( uni_op.type == ArithType::Neg ) {
            mir.instrs.put( MI{ MT::Const, tmp_lhs, 0, 0, 0, node.ifi } );
            mir.instrs.put(
                MI{ MT::Sub, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
        } else {
            mir.instrs.put(
                MI{ MT::None, into_var, tmp_lhs, tmp_rhs, 0, node.ifi } );
        }
    } else if ( node.type == AstNode::Type::GlobalScope ) {
        auto itr = node.nodes->itr();
        while ( itr ) {
            write_mir_instr( mir, itr.get(), mir.next_var++ );
            itr.skip_self( 1 );
        }
    }
}


Mir construct_mir( CompilerState &state, AstNode &root_node ) {
    Mir mir;

    write_mir_instr( mir, root_node, mir.next_var++ );

    // Check whether there are None ops TODO

    // DEBUG
    if ( true ) {
        auto itr = mir.instrs.itr();
        while ( itr ) {
            Mir::MirInstr instr = itr.get();
            String str = name_of_instr( instr.type ) + " " +
                         to_string( instr.result ) + " " +
                         to_string( instr.p0 ) + " " + to_string( instr.p1 ) +
                         " c" + to_string( instr.imm );
            itr.skip_self( 1 );
            olog( str );
        }
    }


    return mir;
}

} // namespace chc
