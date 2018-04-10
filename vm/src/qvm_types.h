#ifndef QVM_TYPES_H
#define QVM_TYPES_H

#include <map>
#include <queue>
#include <memory>
#include <thread>
#include <vector>
#include <cstdint>

namespace qz { namespace vm {

struct QzDatum;
struct QzFunction;
struct QzThread;

struct QzMessage;
struct QzInstruction;

enum QzDatumType {
  QZ_DATUM_INT,
  QZ_DATUM_FLOAT,
  QZ_DATUM_SYMBOL,
  QZ_DATUM_STRING,
  QZ_DATUM_FUNCTION_POINTER,
  QZ_DATUM_THREAD,
}; // TODO: Product types + sum types (or lists, arrays, etc.)

struct QzDatum {
  QzDatumType type;
  union {
    std::int64_t                int_;
    double                      float_;
    std::size_t                 symbol;
    std::string                 string;
    std::shared_ptr<QzFunction> function;
    std::shared_ptr<QzThread>   thread;
  };

  QzDatum(std::int64_t i);
  QzDatum(double d);
  QzDatum(std::size_t s);
  QzDatum(std::string s);
  QzDatum(std::shared_ptr<QzFunction> f);
  QzDatum(std::shared_ptr<QzThread> t);

  ~QzDatum();
};

struct QzFunction {
  std::string name;
  std::size_t arity;
  std::vector<QzInstruction> program;
};

enum QzThreadType {
  QZ_THREAD_LOCAL,
  QZ_THREAD_FOREIGN
};

struct QzLocalThread {
  std::shared_ptr<std::thread> thread;
  std::queue<QzMessage> message_queue;
  // VM Context (do we really need this?)
  std::shared_ptr<QzFunction> program;
};

struct QzThread {
private:
  static std::map<std::thread::id, std::shared_ptr<QzThread>> thread_map;
public:
  std::thread::id thread_id;

  QzThreadType type;
  union {
    QzLocalThread local;
    // or

    // Socket
  };

  QzThread();
  QzThread(std::shared_ptr<QzFunction>);

  static std::shared_ptr<QzThread> create();
  static std::shared_ptr<QzThread> create(std::shared_ptr<QzFunction> f);

  ~QzThread();

  void kill();
  void enqueue_msg(QzMessage m);
  void clear_msg_queue();
  void exec_program(std::shared_ptr<QzFunction> f);
  std::shared_ptr<QzThread> fork();
  void migrate(QzThreadType t);
  void pause();
  void resume();
};

struct QzMessage {
  std::size_t message_symbol;
  std::string message_name;
  std::thread::id sender_id;
};

struct QzInstruction {
  std::int64_t instr;
};

} }

#endif
