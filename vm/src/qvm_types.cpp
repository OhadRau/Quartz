#include "qvm_types.h"
#include <iostream>

namespace qz { namespace vm {

QzVm::QzVm(std::size_t stack_size, std::size_t heap_size, std::vector<QzFunction> program, std::vector<QzInstruction> instrs) {
  this->stack_size = stack_size;
  this->heap_size = heap_size;

  this->heap = std::vector<std::int8_t>(heap_size);
  // TODO: Push instructions onto heap

  for (auto fn : program) {
    this->function_table[fn.name] = fn;
  }

}

std::shared_ptr<QzVm> QzVm::create_and_run(std::size_t stack_size, std::size_t heap_size, std::vector<QzFunction> program, std::vector<QzInstruction> instrs) {
  std::shared_ptr<QzVm> vm = std::make_shared<QzVm>(stack_size, heap_size, program, instrs);

  auto main_thread = QzThread::create(vm);
  main_thread->exec_function("<QZ%START>");

  return vm;
}

QzContext::QzContext(std::size_t stack_size) {
  this->stack = std::vector<std::size_t>(stack_size);
  this->stack_ptr = this->frame_ptr = 0;
  this->instr_ptr = 0;
}

QzDatum::QzDatum(std::int64_t i) {
  this->type = QZ_DATUM_INT;
  this->int_ = i;
}

QzDatum::QzDatum(double f) {
  this->type = QZ_DATUM_FLOAT;
  this->float_ = f;
}

QzDatum::QzDatum(std::size_t s) {
  this->type = QZ_DATUM_SYMBOL;
  this->symbol = s;
}

QzDatum::QzDatum(std::string s) {
  this->type = QZ_DATUM_STRING;
  this->string = s;
}

QzDatum::QzDatum(std::shared_ptr<QzFunction> f) {
  this->type = QZ_DATUM_FUNCTION_POINTER;
  this->function = f;
}

QzDatum::QzDatum(std::shared_ptr<QzThread> t) {
  this->type = QZ_DATUM_THREAD;
  this->thread = t;
}

QzDatum::QzDatum(const QzDatum &d) {
  this->type = d.type;
  switch (d.type) {
  case QZ_DATUM_INT:
    this->int_ = d.int_;
    break;
  case QZ_DATUM_FLOAT:
    this->float_ = d.float_;
    break;
  case QZ_DATUM_SYMBOL:
    this->symbol = d.symbol;
    break;
  case QZ_DATUM_STRING:
    this->string = d.string;
    break;
  case QZ_DATUM_FUNCTION_POINTER:
    this->function = d.function;
    break;
  case QZ_DATUM_THREAD:
    this->thread = d.thread;
    break;
  }
}

QzDatum::~QzDatum() {}

QzThread::QzThread(std::shared_ptr<QzVm> vm) {
  this->type = QZ_THREAD_LOCAL;
  this->local.vm = vm;
  this->local.ctx = QzContext(vm->stack_size);
  this->local.thread = std::make_shared<std::thread>([vm = this->local.vm, ctx = this->local.ctx]() {
    std::cout << "TODO: Write thread execution function!" << std::endl;
    std::cout.flush();
  });
  this->local.thread->join();
  this->local.message_queue = std::queue<QzMessage>();

  this->thread_id = this->local.thread->get_id();
}

std::shared_ptr<QzThread> QzThread::create(std::shared_ptr<QzVm> vm) {
  std::shared_ptr<QzThread> qt = std::make_shared<QzThread>(vm);

  if (qt->type == QZ_THREAD_LOCAL)
    vm->thread_map[qt->thread_id] = qt;

  return qt;
}

void QzThread::kill() {
  if (this->type == QZ_THREAD_LOCAL)
    this->local.vm->thread_map.erase(this->thread_id);
}

void QzThread::enqueue_msg(QzMessage m) {
  if (this->type == QZ_THREAD_LOCAL)
    this->local.message_queue.push(m);
}

void QzThread::clear_msg_queue() {
  if (this->type == QZ_THREAD_LOCAL)
    this->local.message_queue = std::queue<QzMessage>();
}

void QzThread::exec_function(std::string name) { // Assumes you already pushed the params
  if (this->type == QZ_THREAD_LOCAL) {
    this->pause();
    auto found = this->local.vm->function_table.find(name);
    if (found != this->local.vm->function_table.end()) {
      this->local.ctx.instr_ptr = found->second.program_ptr;
    }
    this->resume();
  }
}

void QzThread::pause() {
  // TODO: Implement thread pausing
}

void QzThread::resume() {
  // TODO: Implement thread resuming
}

QzThread::~QzThread() {
  this->kill();
}

} }
