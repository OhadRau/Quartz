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

  QzDatum threadA(QzThread::create());
  QzDatum threadB(QzThread::create(
    std::make_shared<QzFunction>(QzFunction {
      .name = "threadB",
      .arity = 0,
      .program = std::vector<QzInstruction>()
    }
  )));

  if (threadA.type == QZ_DATUM_THREAD && threadB.type == QZ_DATUM_THREAD) {
    threadB.thread->enqueue_msg(
      QzMessage {
        .message_symbol = std::hash<std::string>{}("Hello"),
        .message_name   = "Hello",
        .sender_id      = threadA.thread->thread_id
      }
    );
  }
}
