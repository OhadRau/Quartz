#ifndef QVM_TYPES_H
#define QVM_TYPES_H

#include <map>
#include <array>
#include <mutex>
#include <queue>
#include <memory>
#include <thread>
#include <vector>
#include <cstdint>
#include <functional>

#include "qvm_instrs.h"

namespace qz { namespace vm {

struct QzVm;

struct QzContext;
struct QzDatum;
struct QzFunction;
struct QzThread;

struct QzMessage;

struct QzVm {
  std::mutex vm_lock;

  std::size_t stack_size;
  std::size_t heap_size;

  std::vector<std::int8_t> heap;

  std::map<std::string, QzFunction> function_table;
  std::map<std::thread::id, std::shared_ptr<QzThread>> thread_map;

  QzVm(std::size_t stack_size,
       std::size_t heap_size,
       std::vector<QzFunction> program,
       std::vector<Instruction> instrs);

  static std::shared_ptr<QzVm> create_and_run(std::size_t stack_size, std::size_t heap_size, std::vector<QzFunction> program, std::vector<Instruction> instrs);
};

// Should be thread-safe. The assumption is that the only time this is accessed from another thread
// is when the thread holding the context has yielded control, for example when executing a native
// function (wherein the thread pauses, and the native function can deal with threading issues if
// that is necessary).
struct QzContext {
  bool thread_running;

  std::size_t stack_ptr;
  std::size_t frame_ptr;
  std::size_t instr_ptr;

  std::vector<QzDatum> stack;

  QzContext(std::size_t stack_size);
};

enum QzDatumType {
  QZ_DATUM_INT,
  QZ_DATUM_FLOAT,
  QZ_DATUM_SYMBOL,
  QZ_DATUM_STRING,
  QZ_DATUM_FUNCTION_POINTER,
  QZ_DATUM_THREAD,
  QZ_DATUM_INTERNAL
}; // TODO: Product types + sum types (or lists, arrays, etc.)

struct QzDatum {
  QzDatumType type;
  union {
    std::int64_t                 int_;
    double                       float_;
    std::size_t                  symbol;
    std::shared_ptr<std::string> string;
    std::shared_ptr<QzFunction>  function;
    std::thread::id              thread;
    std::array<int8_t, 16>       internal;
  };

  QzDatum();
  QzDatum(const QzDatum &d);

  QzDatum(std::int64_t i);
  QzDatum(double d);
  QzDatum(std::size_t s);
  QzDatum(std::string s);
  QzDatum(std::shared_ptr<std::string> s);
  QzDatum(std::shared_ptr<QzFunction> f);
  QzDatum(std::thread::id);
  QzDatum(std::array<std::int8_t, 16> a);

  QzDatum &operator=(const QzDatum &d);

  ~QzDatum();
};

struct QzFunction {
  std::string name;
  std::size_t arity;
  std::size_t program_ptr;
  std::optional<std::function<void(std::shared_ptr<QzVm>, std::shared_ptr<QzContext>)>> lambda;
};

struct QzMailbox {
  std::mutex mail_lock;
  std::queue<QzMessage> message_queue;
};

enum QzThreadType {
  QZ_THREAD_LOCAL,
  QZ_THREAD_FOREIGN
};

struct QzLocalThread {
  std::shared_ptr<QzVm> vm;
  std::shared_ptr<QzContext> ctx;
  std::shared_ptr<std::thread> thread;
  std::shared_ptr<QzMailbox> mailbox;
};

struct QzThread {
  std::thread::id thread_id;

  QzThreadType type;
  union {
    QzLocalThread local;
    // or

    // Socket
  };

  QzThread(std::shared_ptr<QzVm> vm);

  // FUTURE: We could switch to a single unique_ptr to the thread and
  // simply return the thread ID here. However, this would require us
  // to move all thread related function outside the QzThread class.
  // So for example we could do qz::vm::send(thread, "kill"); or even
  // thread_ref.kill(), which would wrap that. However, it's not clear
  // whether this is really worth adding or not so I'll leave it for
  // now. The one nice thing is I could move away from a union and
  // instead have a QzThread class that Local and Foreign both inherit
  // from.
  static std::shared_ptr<QzThread> create(std::shared_ptr<QzVm> vm);

  ~QzThread();

  void kill();
  void enqueue_msg(QzMessage m);
  void clear_msg_queue();
  void exec_function(std::string name);
  std::shared_ptr<QzThread> fork();
  void migrate(QzThreadType t);
  void pause();
  void resume();
};

struct QzMessage {
  std::size_t          message_symbol;
  std::string          message_name;
  std::vector<QzDatum> message_params;
  std::thread::id      sender_id;
};

} }

#endif
