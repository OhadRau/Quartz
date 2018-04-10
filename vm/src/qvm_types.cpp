#include "qvm_types.h"
#include <iostream>

namespace qz { namespace vm {

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

QzDatum::~QzDatum() {}

std::map<std::thread::id, std::shared_ptr<QzThread>> QzThread::thread_map;

QzThread::QzThread() {
  std::shared_ptr<QzFunction> fn = std::make_shared<QzFunction>(QzFunction {
    .name = "<QZ%EMPTY_THREAD>",
    .arity = 0,
    .program = std::vector<QzInstruction>()
  });
  this->type = QZ_THREAD_LOCAL;
  this->local.program = fn;
  this->local.thread = std::make_shared<std::thread>([program = fn]() {
    std::cout << "TODO: Write thread execution function!" << std::endl;
    std::cout << "Thread running " << program->name << std::endl;
    std::cout.flush();
  });
  this->local.thread->join();
  this->local.message_queue = std::queue<QzMessage>();

  this->thread_id = this->local.thread->get_id();
}

std::shared_ptr<QzThread> QzThread::create() {
  std::shared_ptr<QzThread> qt = std::make_shared<QzThread>();

  QzThread::thread_map[qt->thread_id] = qt;

  return qt;
}

QzThread::QzThread(std::shared_ptr<QzFunction> prog) {
  this->type = QZ_THREAD_LOCAL;
  this->local.program = prog;
  this->local.thread = std::make_shared<std::thread>([program = prog]() {
    std::cout << "TODO: Write thread execution function!" << std::endl;
    std::cout << "Thread running " << program->name << std::endl;
    std::cout.flush();
  });
  this->local.thread->join();
  this->local.message_queue = std::queue<QzMessage>();

  this->thread_id = this->local.thread->get_id();
}

std::shared_ptr<QzThread> QzThread::create(std::shared_ptr<QzFunction> prog) {
  std::shared_ptr<QzThread> qt = std::make_shared<QzThread>(prog);

  QzThread::thread_map[qt->thread_id] = qt;

  return qt;
}

void QzThread::enqueue_msg(QzMessage m) {
  this->local.message_queue.push(m);
}

QzThread::~QzThread() {
  QzThread::thread_map.erase(this->thread_id);
}

} }
