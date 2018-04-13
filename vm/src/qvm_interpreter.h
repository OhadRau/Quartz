#ifndef QVM_INTERPRETER_H
#define QVM_INTERPRETER_H

#include "qvm_types.h"

namespace qz { namespace vm {

void qz_run_local(std::shared_ptr<QzVm> vm,
                  std::shared_ptr<QzContext> ctx,
                  std::shared_ptr<std::queue<QzMessage>> message_queue);

} }

#endif
