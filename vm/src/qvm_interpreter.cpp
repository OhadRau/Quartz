#include "qvm_interpreter.h"

#include "qvm_instrs.h"
#include "qvm_types.h"

#include <functional>

#define PUSH(X) ctx->stack[ctx->stack_ptr++] = QzDatum(X)
#define POP()   ((QzDatum) ctx->stack[--ctx->stack_ptr])
#define PEEK()  ((QzDatum) ctx->stack[ctx->stack_ptr - 1])

namespace qz { namespace vm {

void qz_run_local(std::shared_ptr<QzVm> vm,
                  std::shared_ptr<QzContext> ctx,
                  std::shared_ptr<std::queue<QzMessage>> msgs) {
  auto hash = std::hash<std::string>{};
  while (true) { // NOTE: This doesn't actually take thread_id, messages into account
    auto instr = *((Instruction *) &vm->heap[ctx->instr_ptr++]); // WARNING: INCREMENTS IP BY DEFAULT
    switch(instr.rator) {
    case NOP: {
      break;
    }
    case PUSH: {
      if (!instr.rand1) break;
      switch (instr.rand1->type) {
      case ILiteral:
        PUSH(instr.rand1->int_);
        break;
      case FLiteral:
        PUSH(instr.rand1->float_);
        break;
      case String:
        PUSH(instr.rand1->string);
        break;
      case Symbol:
        PUSH(hash(*instr.rand1->symbol));
        break;
      case FuncRef:
        PUSH(std::make_shared<QzFunction>(vm->function_table[*instr.rand1->funcref]));
        break;
      }
      break;
    }      
    case POP: {
      if (!instr.rand1) ctx->stack_ptr--;
      if (instr.rand1->type != Symbol) break;
      auto tmp = POP();
      if (tmp.type != QZ_DATUM_INT) break;
      if (*instr.rand1->symbol == "SP")
        ctx->stack_ptr = tmp.int_;
      else if (*instr.rand1->symbol == "FP")
        ctx->frame_ptr = tmp.int_;
      else if (*instr.rand1->symbol == "IP")
        ctx->instr_ptr = tmp.int_;
      break;
    }
    case ADD: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        if (tmp1.type == QZ_DATUM_INT && tmp2.type == QZ_DATUM_INT) {
          PUSH(tmp1.int_ + tmp2.int_);
        } else {
          PUSH(tmp1.float_ + tmp2.float_);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ + tmp.int_);
        } else {
          PUSH(instr.rand1->float_ + tmp.float_);
        }
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ + instr.rand2->int_);
        } else {
          PUSH(instr.rand1->float_ + instr.rand2->float_);
        }
      }
      break;
    }
    case SUB: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        if (tmp1.type == QZ_DATUM_INT && tmp2.type == QZ_DATUM_INT) {
          PUSH(tmp1.int_ - tmp2.int_);
        } else {
          PUSH(tmp1.float_ - tmp2.float_);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ - tmp.int_);
        } else {
          PUSH(instr.rand1->float_ - tmp.float_);
        }
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ - instr.rand2->int_);
        } else {
          PUSH(instr.rand1->float_ - instr.rand2->float_);
        }
      }
      break;
    }
    case MUL: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        if (tmp1.type == QZ_DATUM_INT && tmp2.type == QZ_DATUM_INT) {
          PUSH(tmp1.int_ * tmp2.int_);
        } else {
          PUSH(tmp1.float_ * tmp2.float_);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ * tmp.int_);
        } else {
          PUSH(instr.rand1->float_ * tmp.float_);
        }
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ * instr.rand2->int_);
        } else {
          PUSH(instr.rand1->float_ * instr.rand2->float_);
        }
      }
      break;
    }
    case DIV: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        if (tmp1.type == QZ_DATUM_INT && tmp2.type == QZ_DATUM_INT) {
          PUSH(tmp1.int_ / tmp2.int_);
        } else {
          PUSH(tmp1.float_ / tmp2.float_);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ / tmp.int_);
        } else {
          PUSH(instr.rand1->float_ / tmp.float_);
        }
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ / instr.rand2->int_);
        } else {
          PUSH(instr.rand1->float_ / instr.rand2->float_);
        }
      }
      break;
    }
    case MOD: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        PUSH(tmp1.int_ % tmp2.int_);
      } else if (!instr.rand2) {
        auto tmp = POP();
        PUSH(instr.rand1->int_ % tmp.int_);
      } else {
        PUSH(instr.rand1->int_ % instr.rand2->int_);
      }
      break;
    }
    case AND: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        PUSH(tmp1.int_ & tmp2.int_);
      } else if (!instr.rand2) {
        auto tmp = POP();
        PUSH(instr.rand1->int_ & tmp.int_);
      } else {
        PUSH(instr.rand1->int_ & instr.rand2->int_);
      }
      break;
    }    
    case OR: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        PUSH(tmp1.int_ | tmp2.int_);
      } else if (!instr.rand2) {
        auto tmp = POP();
        PUSH(instr.rand1->int_ | tmp.int_);
      } else {
        PUSH(instr.rand1->int_ | instr.rand2->int_);
      }
      break;
    }
    case XOR: {
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        PUSH(tmp1.int_ ^ tmp2.int_);
      } else if (!instr.rand2) {
        auto tmp = POP();
        PUSH(instr.rand1->int_ ^ tmp.int_);
      } else {
        PUSH(instr.rand1->int_ ^ instr.rand2->int_);
      }
      break;
    }
    case NOT: {
      if (!instr.rand1) {
        PUSH(~POP().int_);
      } else {
        PUSH(~instr.rand1->int_);
      }
      break;
    }
    case DUP: {
      PUSH(PEEK());
      break;
    }
    case SWAP: {
      auto tmp = PEEK();
      ctx->stack[ctx->stack_ptr - 1] = ctx->stack[ctx->stack_ptr - 2];
      ctx->stack[ctx->stack_ptr - 2] = tmp;
      break;
    }
    }
  }
}

} }
