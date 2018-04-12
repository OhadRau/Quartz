#include <string>
#include <vector>
#include <iostream>
#include <functional>
#include "qvm_types.h"

using namespace qz::vm;

int main(int argc, char **argv) {
  std::cout << "Sizeof QzThread:   " << sizeof(QzThread)   << std::endl;
  std::cout << "Sizeof QzDatum:    " << sizeof(QzDatum)    << std::endl;
  std::cout << "Sizeof QzFunction: " << sizeof(QzFunction) << std::endl;

  // stack_size: 256 * 8 * 8 = 16KiB
  // heap_size: 131072 * 8 = 1MiB
  // stack_size: 256 * 8 = 2KiB
  // stack_size: 256 * 8 = 2KiB
  auto functions = std::vector<QzFunction> {
    QzFunction {
      .name = "<QZ%START>",
      .arity = 0,
      .program_ptr = 0
    }
  };

  auto vm = QzVm::create_and_run(256, 131072, functions, std::vector<QzInstruction>());

  QzDatum threadA(QzThread::create(vm));
  QzDatum threadB(QzThread::create(vm));

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
}
