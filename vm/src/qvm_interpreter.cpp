#include "qvm_interpreter.h"

namespace qz { namespace vm

void qz_eval(std::shared_ptr<QzVm> vm, std::shared_ptr<QzContext> ctx, QzInstruction i) {

}

void qz_run_local_thread(std::shared_ptr<QzVm> vm,
                         std::shared_ptr<QzContext> ctx,
                         std::thread::id thread_id,
                         std::shared_ptr<std::queue<QzMessage>> msgs) {
  while (true) { // NOTE: This doesn't actually take thread_id, messages into account
    qz_eval(vm, ctx, vm->heap[ctx->instr_ptr]);
  }
}

} }
