The Quartz VM:
-------------

* Probably needs some fancy rock-related name (SiO2, Silicon/Silica, Oxygen, Mineral, Amethyst)
* Exceptions are first-class
* Concurrency/parallelism are first-class
* Message passing is first-class (w/ message queues)
  + All threads have implicit mailboxes
  + Thread type can be defined as:
    ```ocaml
    type qthread = {
      address : int;
      mailbox : message_queue; (* What happens if the thread is on a different computer? *)
      proc : [ `Process of native_process | `Thread of native_thread | `Socket of native_socket ];
    }
    ```
  + Thread objects have the following (external) operations:
    - `kill`         => Forcibly terminate the thread
    - `enqueue_msg`  => Add a message to a thread's message queue
    - `clear_queue`  => Clear a thread's message queue
    - `exec_program` => Set the program (function/session) to run on that thread
    - `fork`         => Duplicate the thread
    - `migrate`      => Move a thread to a different native thread/process or a different computer and change the socket
    - `pause_exec`   => Temporarily pause execution of the thread's program
    - `resume_exec`  => Resume execution of the thread's program
* Built-in GC
* Stack-based probably
* Doesn't need to be extremely low-level
* Maybe sessions should mimic classes in JVM bytecode
  + Top-level export with type signatures
* Somewhat dynamically typed
  + When code hops between computers, we need some checks to make sure everything is okay
  + E.g. if one server goes AWOL, we need dynamic chceks in place to respond
  + We can assume that all local threads are safe at runtime

## Ideas:

* OCaml bytecode
* JVM bytecode
* BEAM bytecode

## Examples:

```
session Timer(s)
  on Start from s
    let now = std::get_time_ms()
    on Stop from s
      let new = std::get_time_ms()
      s!Took(new - now)
    end
  end
end

session P(q)
  t = spawn Timer(self)
  t!Start
  q!Ping
  on Pong from q
    t!Stop
    on Took(ms)
      std::print(ms)
    end
  end
end

session Q(p)
  on Ping from p
    std::wait(10)
    p!Pong
  end
end

fun main()
  let p = spawn P(q)
  and q = spawn Q(p)
end
```

```
session Timer < exists S. [s : S] -> ?s{Start ~> ?s{Stop ~> !s{Took(int) ~> End}}} >:
  # push s
  await_msg [0]
  cmp $Start _nomatch0
  call_function @std::get_time_ms
  await_msg [-1]
  cmp $Stop _nomatch0
  pop
  call_function @std::get_time_ms
  sub
  send_msg(1) [-1] $Took
  close

  _nomatch0:
  close_err "Session Timer received unmatched message from ?S" # Better example would use exceptions

session P < exists Q. (q : Q) -> [t : Timer] -> !t{Start ~> !q{Ping ~> ?q{Pong ~> !t{Stop ~> ?t{Took(int) ~> End}}}}} >:
  # push q
  push @self
  call_constructor_spawn @Timer # Call and push spawn Timer(self)
  send_msg [0] $Start
  send_msg [-1] $Ping
  await_msg [-1]
  cmp $Pong _nomatch0
  send_msg [-1] $Stop
  await_msg(1) [-1]    # Takes an additional arg (goes argN, ..., arg0, msg)
  cmp $Took _nomatch1
  pop
  call_function @std::print
  close

  _nomatch0:
  close_err "Session P received unmatched message from Q" # Better example would use exceptions

  _nomatch1:
  close_err "Session P received unmatched message from Timer" # Better example would use exceptions

session Q < exists P. (p : P) -> ?p{Ping ~> !p{Pong ~> End}} >:
  # push p
  await_msg [0]            # Await a message from p
  cmp $Ping _nomatch0  # Compare last item on stack (the message) to $Ping
  push 10
  call_function @std::wait
  send_msg [-1] $Pong      # Send $Pong to p
  close
  
  _nomatch0:
  close_err "Session Q received unmatched message from P" # Better example would use exceptions

function main < () -> void >:
  spawn_empty                    # Push thread p onto stack
  spawn_empty                    # Push thread q onto stack
  call_constructor_async @P [-1] # Takes 1 off the stack (q) and runs on thread p
  swap
  call_constructor_async @Q [-1] # Takes 1 off the stack (p) and runs on thread q
```

## The QuartzVM Protocol

* VMs will often be running either in separate processes or on separate systems
* We can use network sockets to communicate between these on some protocol
  + Most likely TCP-based, UDP would be nice but safety is more important => maybe compiler flag?
* Can occur at macro level (entire VMs communicating) or micro level (sessions communicating across servers)

### Definition:

```
[QVM_OPERATION][QVM_NUM_PARAMS][QVM_PARAMS...]

QVM_OPERATION =
  | "qvm_immigrate" -- (thread_id, context, code_pointer) -> status   => Take an "immigrant" thread
  | "qvm_emigrate"  -- (thread_id) -> (status, context, code_pointer) => Give thread information
  | "qvm_kill"      -- (thread_id) -> status                          => Kill a thread
```
