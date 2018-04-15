#include "qvm_interpreter.h"

#include "qvm_instrs.h"
#include "qvm_types.h"

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <functional>

#define PUSH(X)     ctx->stack[ctx->stack_ptr++] = QzDatum(X)
#define PUSH_RAW(X) ctx->stack[ctx->stack_ptr++] = X
#define POP()       ((QzDatum) ctx->stack[--ctx->stack_ptr])
#define PEEK()      ((QzDatum) ctx->stack[ctx->stack_ptr - 1])

namespace qz { namespace vm {

void qz_run_local(std::shared_ptr<QzVm> vm,
                  std::shared_ptr<QzContext> ctx,
                  std::shared_ptr<std::queue<QzMessage>> msgs) {
  auto hash = std::hash<std::string>{};
  while (true) { // NOTE: This doesn't actually take thread_id, messages into account
    while (!ctx->thread_running)
      std::this_thread::sleep_for(std::chrono::milliseconds(5)); // TODO: HACK: Should use concurrency primitives

    auto instr = *((Instruction *) &vm->heap[ctx->instr_ptr]); // HACK: WARNING: INCREMENTS IP BY DEFAULT
    std::cout << "\tIP=" << ctx->instr_ptr << ", OP=" << instr.rator << std::endl;
    ctx->instr_ptr += sizeof(Instruction);
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
    case EXCHANGE: {
      auto tmp = ctx->stack[ctx->stack_ptr + instr.rand1->stackref - 1];
      ctx->stack[ctx->stack_ptr + instr.rand1->stackref - 1] = ctx->stack[ctx->stack_ptr + instr.rand2->stackref - 1];
      ctx->stack[ctx->stack_ptr + instr.rand1->stackref - 1] = tmp;
      break;
    }
    case CMP: { // TODO: ** Should ** allow for more than just int/float comparisons
      if (!instr.rand1) {
        auto tmp1 = POP();
        auto tmp2 = POP();
        if (tmp1.type == QZ_DATUM_INT && tmp2.type == QZ_DATUM_INT) {
          PUSH(tmp1.int_ - tmp2.int_);
        } else if (tmp1.type == QZ_DATUM_FLOAT && tmp2.type == QZ_DATUM_FLOAT) {
          PUSH(tmp1.float_ - tmp2.float_);
        } else if (tmp1.type == QZ_DATUM_SYMBOL && tmp2.type == QZ_DATUM_SYMBOL) {
          PUSH(tmp1.symbol - tmp2.symbol);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ - tmp.int_);
        } else if (instr.rand1->type == FLiteral) {
          PUSH(instr.rand1->float_ - tmp.float_);
        } else if (instr.rand1->type == Symbol) {
          PUSH(hash(*instr.rand1->symbol) - tmp.symbol);
        }
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ - instr.rand2->int_);
        } else if (instr.rand1->type == FLiteral) {
          PUSH(instr.rand1->float_ - instr.rand2->float_);
        } else if (instr.rand1->type == Symbol) {
          PUSH(hash(*instr.rand1->symbol) - hash(*instr.rand2->symbol));
        }
      }
      break;
    }
    case JEQ: {
      if (ctx->stack[ctx->stack_ptr - 1].int_ == 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JNE: {
      if (ctx->stack[ctx->stack_ptr - 1].int_ != 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JLT: {
      if (ctx->stack[ctx->stack_ptr - 1].int_ < 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JGT: {
      if (ctx->stack[ctx->stack_ptr - 1].int_ > 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JMP: {
      ctx->instr_ptr = instr.rand1->int_;
      break;
    }
    case CALL: {
      auto fn = vm->function_table[*instr.rand1->funcref];
      if (fn.lambda) {
        auto fn_lambda = *fn.lambda;
        fn_lambda(vm, ctx);
      } else {
        auto f_addr = fn.program_ptr * sizeof(Instruction);
        PUSH(ctx->frame_ptr);
        PUSH(ctx->instr_ptr);
        ctx->frame_ptr = ++ctx->stack_ptr;
        ctx->instr_ptr = f_addr;
      }
      break;
    }
    case RET: {
      if (!instr.rand1) {
        ctx->instr_ptr = ctx->stack[ctx->frame_ptr - 1].int_;
        ctx->stack_ptr = ctx->frame_ptr;
        POP(); // Pops old instr_ptr
        ctx->frame_ptr = POP().int_;
      } else {
        QzDatum d;
        switch (instr.rand1->type) {
        case ILiteral:
          d = QzDatum(instr.rand1->int_);
          break;
        case FLiteral:
          d = QzDatum(instr.rand1->float_);
          break;
        case String:
          d = QzDatum(instr.rand1->string);
          break;
        case Symbol:
          d = QzDatum(hash(*instr.rand1->symbol));
          break;
        case StackRef:
          d = ctx->stack[ctx->stack_ptr + instr.rand1->stackref - 1];
          break;
        case FuncRef:
          d = QzDatum(std::make_shared<QzFunction>(vm->function_table[*instr.rand1->funcref]));
          break;
        }
        ctx->instr_ptr = ctx->stack[ctx->frame_ptr - 1].int_;
        ctx->stack_ptr = ctx->frame_ptr;
        POP(); // Pops old instr_ptr
        ctx->frame_ptr = POP().int_;
        PUSH_RAW(d);
      }
      break;
    }
    case CLOSE: {
      return;
    }
    case AWAIT_MSG: {
      if (!instr.rand1) {
        while (msgs->empty()) {
          std::this_thread::sleep_for(std::chrono::milliseconds(5)); // TODO: HACK: Should use concurrency primitives
        }
        PUSH(msgs->front().message_symbol);
        msgs->pop();
      } else if (instr.rand1->type == StackRef) {
      } else if (instr.rand1->type == ILiteral) {
        if (!instr.rand2) {
        } else if (instr.rand2->type == StackRef) {
        }
      }
      break;
    }
    }
  }
}

} }
