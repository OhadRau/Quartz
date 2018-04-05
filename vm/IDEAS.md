The Quartz VM:
-------------

* Probably needs some fancy rock-related name (SiO2, Silicon/Silica, Oxygen, Mineral, Amethyst)
* Exceptions are first-class
* Concurrency/parallelism are first-class
* Message passing is first-class
* Built-in GC
* Stack-based probably
* Doesn't need to be extremely low-level
* Maybe sessions should mimic classes in JVM bytecode
  + Top-level export with type signatures
* Somewhat dynamically typed
  + When code hops between computers, we need some checks to make sure everything is okay
  + E.g. if one server goes AWOL, we need dynamic chceks in place to respond

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