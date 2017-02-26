# Quartz

Quartz is a statically typed programming concurrent language for the BEAM VM based on the actor model with a sleek, Ruby-based syntax and support for functional and object-oriented programming.

## Features

* Familiar Ruby-like syntax
* First-class concurrency primitives
* Actor model message passing à la Erlang
* Static typing for messages based on session types
* Duality checking
* First-class functions

## Examples

```
session Server
  branch Start
    case File.open "file.txt"
    when Success f
      sender!Ok
      File.read_into f sender!Value
    else
      sender!Err
    end
  end
end

session Client (target : Server)
  initialize
    target!Start
  end

  branch Ok
    branch Value status
      print status
      close
    end
  end

  branch Err
    print "Server-side error when opening file"
    close
  end
end

fun main
  let server = spawn Server
  spawn Client server
end
```
