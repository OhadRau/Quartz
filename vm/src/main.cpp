#include <string>
#include <vector>
#include <cstdint>
#include <iostream>
#include <functional>
#include "qvm_types.h"
#include "qvm_instrs.h"

using namespace qz::vm;

int main(int argc, char **argv) {
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
    Instruction(SPAWN_EMPTY),
    Instruction(PUSH, Operand(TString {"Hello from constructor 1"})),
    /* WEIRDEST FUCKING ERROR:
       Replace this with any other opcode and it works without a hitch. Comment it out and it works too.
       As in, the next CONSTRUCT_ASYNC works just fine as long as it's not right here in the vector.
       But if this is placed on this line, we get a segfault on QzThread::create(vm); in this file.
       Seems to somehow break the way the vm gets copied with this instruction vector. This makes literally
       0 sense and I'm convinced this is some kind of compiler/STL bug because I can't see how my code would
       actually affect what is happening on that line. */
    Instruction(CONSTRUCT_ASYNC, Operand(TILiteral {1}), Operand(TFuncRef {"std::print"})),
    Instruction(SPAWN_EMPTY),
    Instruction(PUSH, Operand(TString {"Hello from constructor 2"})),
    Instruction(CONSTRUCT_ASYNC, Operand(TILiteral {1}), Operand(TFuncRef {"std::print"}), Operand(TStackRef {-2})),
    Instruction(CLOSE),
  };

/*
<QZ%START>:
  push "Hello, world!"
  call @std::print
  close
wait_for_hello:
  push "Awaiting messages!"
  call @std::print
  await_msg
  cmp $Hello
  jeq <QZ%START> // print "Hello, world!"
  spawn_empty
  push "Hello from constructor 1"
  construct_async(1) @std::print
  spawn_empty
  push "Hello from constructor 2"
  construct_async(1) @std::print [-2]
  close
*/

  // stack_size: 512 * 8 * 8 = 32KiB
  // heap_size: 67108864 * 8 = 64MiB
  auto vm = QzVm::create_and_run(512, 67108864, functions, instrs);

  auto threadA = QzThread::create(vm);
  auto threadB = QzThread::create(vm);

  threadB->exec_function("wait_for_hello");
  threadB->resume();

  threadB->enqueue_msg(
    QzMessage {
      .message_symbol = std::hash<std::string>{}("Hello"),
      .message_name   = "Hello",
      .message_params = std::vector<QzDatum>(),
      .sender_id      = threadA->thread_id
    }
  );

  while (true) {}
}
