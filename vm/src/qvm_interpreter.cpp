#include "qvm_interpreter.h"

#include "qvm_instrs.h"
#include "qvm_types.h"

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <functional>

#define PUSH(X)     ctx->stack[ctx->stack_ptr++] = QzDatum(X)
#define PUSH_RAW(X) ctx->stack[ctx->stack_ptr++] = X
#define POP()       ctx->stack[--ctx->stack_ptr]
#define PEEK()      ctx->stack[ctx->stack_ptr - 1]
#define DEREF(X)    ctx->stack[ctx->stack_ptr + X - 1]

namespace qz { namespace vm {

void qz_run_local(std::shared_ptr<QzVm> vm,
                  std::shared_ptr<QzContext> ctx,
                  std::shared_ptr<std::queue<QzMessage>> msgs) {
  auto hash = std::hash<std::string>{};
  auto idHash = std::hash<std::thread::id>{};
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
      DEREF(0) = DEREF(-1);
      DEREF(-1) = tmp;
      break;
    }
    case EXCHANGE: {
      auto tmp = DEREF(instr.rand1->stackref);
      DEREF(instr.rand1->stackref) = DEREF(instr.rand2->stackref);
      DEREF(instr.rand1->stackref) = tmp;
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
        } else if (tmp1.type == QZ_DATUM_STRING && tmp2.type == QZ_DATUM_STRING) {
          PUSH((std::int64_t) tmp1.string->compare(*tmp2.string));
        } else if (tmp1.type == QZ_DATUM_THREAD && tmp2.type == QZ_DATUM_THREAD) {
          PUSH(idHash(tmp1.thread) - idHash(tmp2.thread));
        } else { // QZ_DATUM_FUNCTION_POINTER, QZ_DATUM_INTERNAL, types not equal => false
          PUSH((std::int64_t) -1);
        }
      } else if (!instr.rand2) {
        auto tmp = POP();
        if (instr.rand1->type == ILiteral && tmp.type == QZ_DATUM_INT) {
          PUSH(instr.rand1->int_ - tmp.int_);
        } else if (instr.rand1->type == FLiteral && tmp.type == QZ_DATUM_FLOAT) {
          PUSH(instr.rand1->float_ - tmp.float_);
        } else if (instr.rand1->type == String && tmp.type == QZ_DATUM_STRING) {
          PUSH((std::int64_t) instr.rand1->string->compare(*tmp.string));
        } else if (instr.rand1->type == Symbol && tmp.type == QZ_DATUM_SYMBOL) {
          PUSH(hash(*instr.rand1->symbol) - tmp.symbol);
        } else if (instr.rand1->type == StackRef && tmp.type == QZ_DATUM_THREAD) {
          PUSH(idHash(DEREF(instr.rand1->stackref).thread) - idHash(tmp.thread));
        } else {
          PUSH((std::int64_t) -1);
        } // QZ_DATUM_FUNCTION_POINTER, QZ_DATUM_INTERNAL, types not equal => false
      } else {
        if (instr.rand1->type == ILiteral) {
          PUSH(instr.rand1->int_ - instr.rand2->int_);
        } else if (instr.rand1->type == FLiteral) {
          PUSH(instr.rand1->float_ - instr.rand2->float_);
        } else if (instr.rand1->type == String) {
          PUSH((std::int64_t) instr.rand1->string->compare(*instr.rand2->string));
        } else if (instr.rand1->type == Symbol) {
          PUSH(hash(*instr.rand1->symbol) - hash(*instr.rand2->symbol));
        } else { // QZ_DATUM_FUNCTION_POINTER, QZ_DATUM_THREAD, types not equal => false
          PUSH((std::int64_t) -1);
        }
      }
      break;
    }
    case TCHECK: {
      switch (instr.rand1->type) {
      case ILiteral:
        if (hash(*instr.rand2->symbol) == hash("INT")) {
          PUSH((std::int64_t) 0);
        } else {
          PUSH((std::int64_t) -1);
        }
        break;
      case FLiteral:
        if (hash(*instr.rand2->symbol) == hash("FLOAT")) {
          PUSH((std::int64_t) 0);
        } else {
          PUSH((std::int64_t) -1);
        }
        break;
      case String:
        if (hash(*instr.rand2->symbol) == hash("STRING")) {
          PUSH((std::int64_t) 0);
        } else {
          PUSH((std::int64_t) -1);
        }
        break;
      case Symbol:
        if (hash(*instr.rand2->symbol) == hash("SYMBOL")) {
          PUSH((std::int64_t) 0);
        } else {
          PUSH((std::int64_t) -1);
        }
        break;
      case StackRef: {
        auto tmp = DEREF(instr.rand1->stackref);
        switch (tmp.type) {
        case QZ_DATUM_INT:
          if (hash(*instr.rand2->symbol) == hash("INT")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_FLOAT:
          if (hash(*instr.rand2->symbol) == hash("FLOAT")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_STRING:
          if (hash(*instr.rand2->symbol) == hash("STRING")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_SYMBOL:
          if (hash(*instr.rand2->symbol) == hash("SYMBOL")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_FUNCTION_POINTER:
          if (hash(*instr.rand2->symbol) == hash("FUNCTION")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_THREAD:
          if (hash(*instr.rand2->symbol) == hash("THREAD")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        case QZ_DATUM_INTERNAL:
          if (hash(*instr.rand2->symbol) == hash("INTERNAL")) {
            PUSH((std::int64_t) 0);
          } else {
            PUSH((std::int64_t) -1);
          }
          break;
        }
        break;
      }
      case FuncRef:
        if (hash(*instr.rand2->symbol) == hash("FUNCTION")) {
          PUSH((std::int64_t) 0);
        } else {
          PUSH((std::int64_t) -1);
        }
        break;
      }
      break;
    }
    case JEQ: {
      if (DEREF(0).int_ == 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JNE: {
      if (DEREF(0).int_ != 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JLT: {
      if (DEREF(0).int_ < 0) {
        ctx->instr_ptr = instr.rand1->int_;
      }
      break;
    }
    case JGT: {
      if (DEREF(0).int_ > 0) {
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
          d = DEREF(instr.rand1->stackref);
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
    case CONSTRUCT_ASYNC: {
      std::cout << "Constructing" << std::endl;
      if (instr.rand1->type == FuncRef) {
        auto fn = instr.rand1->funcref;
        auto thread = instr.rand2 ? vm->thread_map[DEREF(instr.rand2->stackref).thread] : vm->thread_map[POP().thread];
        thread->exec_function(*fn);
        thread->resume();
      } else {
        auto param_count = instr.rand1->int_;
        auto fn = instr.rand2->funcref;
        auto thread = instr.rand3 ? vm->thread_map[DEREF(instr.rand3->stackref).thread] : vm->thread_map[DEREF(param_count).thread];
        if (thread->type == QZ_THREAD_LOCAL) {
          auto lctx = thread->local.ctx;
          for (auto i = 0; i < param_count; i++) {
            std::cout << "T[" << lctx->stack_ptr + 1 << "] <- [" << i - param_count << "]" << std::endl;
            lctx->stack[lctx->stack_ptr++] = DEREF(i - param_count);
          }
        }
        thread->exec_function(*fn);
        thread->resume();
      }
      break;
    }
    case SPAWN_EMPTY: {
      std::cout << "Spawning" << std::endl;
      auto thread = QzThread::create(vm);
      PUSH(thread->thread_id);
      break;
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
    case CLOSE: {
      return;
    }
    case CLOSE_ERR: {
      throw instr.rand1->string;
    }
    }
  }
}

} }
