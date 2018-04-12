#ifndef QVM_INTERPRETER_H
#define QVM_INTERPRETER_H

#include "qvm_types.h"

namespace qv { namespace vm {

void qz_eval(std::shared_ptr<QzVm> vm, std::shared_ptr<QzContext> ctx, QzInstruction i);

void qz_run_local(std::shared_ptr<QzVm> vm,
                  std::shared_ptr<QzContext> ctx,
                  std::thread::id thread_id,
                  std::queue<QzMessage> message_queue);

} }

#endif
