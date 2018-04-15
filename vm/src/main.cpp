#include <string>
#include <vector>
#include <cstdint>
#include <iostream>
#include <functional>
#include "qvm_types.h"
#include "qvm_instrs.h"

using namespace qz::vm;

int main(int argc, char **argv) {
  // stack_size: 256 * 8 * 8 = 16KiB
  // heap_size: 131072 * 8 = 2MiB
  // stack_size: 256 * 8 = 2KiB
  // stack_size: 256 * 8 = 2KiB
  auto functions = std::vector<QzFunction> {
    QzFunction {
      .name = "<QZ%START>",
      .arity = 0,
      .program_ptr = 0
    },
    QzFunction {
      .name = "wait_for_hello",
      .arity = 0,
      .program_ptr = 3
    },
    QzFunction {
      .name = "std::print",
      .arity = 1,
      .program_ptr = 0,
      .lambda = [](std::shared_ptr<QzVm> vm, std::shared_ptr<QzContext> ctx) {
        auto str = *ctx->stack[ctx->stack_ptr - 1].string;
        std::cout << str << std::endl;
      }
    }
  };

  std::vector<Instruction> instrs = {
    Instruction(PUSH, Operand(TString {"Hello, world!"})),
    Instruction(CALL, Operand(TFuncRef {"std::print"})),
    Instruction(CLOSE),
    Instruction(PUSH, Operand(TString {"Awaiting messages!"})),
    Instruction(CALL, Operand(TFuncRef {"std::print"})),
    Instruction(AWAIT_MSG),
    Instruction(CMP, Operand(TSymbol {"Hello"})),
    Instruction(JEQ, Operand(TILiteral {0})),
    Instruction(CLOSE)
  };

  auto vm = QzVm::create_and_run(256, 131072, functions, instrs);

  QzDatum threadA(QzThread::create(vm));
  QzDatum threadB(QzThread::create(vm));

  threadB.thread->exec_function("wait_for_hello");
  threadB.thread->resume();

  if (threadA.type == QZ_DATUM_THREAD && threadB.type == QZ_DATUM_THREAD) {
    threadB.thread->enqueue_msg(
      QzMessage {
        .message_symbol = std::hash<std::string>{}("Hello"),
        .message_name   = "Hello",
        .message_params = std::vector<QzDatum>(),
        .sender_id      = threadA.thread->thread_id
      }
    );
  }

  while (true) {}
}
