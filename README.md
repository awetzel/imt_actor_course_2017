# IMT Atlantique : Elixir live coding for the erlang Actor Model

Unprepared live-coding session to explain concepts during the course.

Please :
- first install elixir to really test the files :
[http://elixir-lang.github.io/install.html](http://elixir-lang.github.io/install.html).
- Then read the course slides : [slides in this repo](slides.pdf)
- Then clone this repo to test : `git clone https://github.com/awetzel/imt_2017`
- Then read this README and each part `course_part_x.exs` files
- Then and only then (actor principles are more important than the given
  language) read the Elixir language documentation
  [http://elixir-lang.github.io/getting-started](http://elixir-lang.github.io/getting-started)
- If you want to read the same in the erlang language : read
  [http://learnyousomeerlang.com/content](http://learnyousomeerlang.com/content)

The use case is to manage a bank account 
- with an initial credit hold of `4` €
- then credit 5€
- then credit 2€
- then debit 3€
- then show current credit hold : should be 8€

This course try to "show" the principles of the actor model, how it is
applied in practice in a functional language (Erlang/Elixir).
The idea is to build the "account application" incrementally :
- part0: from scratch first : with only a raw functional language
- part1: then with a micro-process/coroutine system sending message
- part2: then splitting the generic server logic from the business account logic
- part3: then with the Erlang/Elixir libraries to create the coroutine
- part4: then with a supervisor to handle error and restart the coroutine
- part5: then with the Erlang/Elixir libraries to create the supervisor
- part6: then with the Erlang-OTP/Elixir-tooling way to create the concept of "application" 

## Part 0 : Functional programming

```
elixir course_part_0.exs
```

standard functionnal programming : the function transform immutable
data : stateless.

Problem : is you want the system to manage the bank account state,
and not let an external stateful system (database, filesystem) to
manage it... You need your software to be a permanent server with
account in the server RAM : see `course_part_1`

## Part 1 : Server as a linked processes with message communication

```
elixir course_part_1.exs
```

Use `spawn_link`, `receive` and `send` (core low level functions of erlang)
to create the account server.

## Part 2 : functional split : the same with a generic server

```
elixir course_part_2.exs
```

Create 2 different modules : 
- one to handle a generic server.
- one other to handle the way you want the server to behave

Put the second module (behaviour module) in the generic server state.

## Part 3 : generic server IS in standard library

```
elixir course_part_3.exs
```

Show that a complete and featurefull generic server is in the
standard Library : GenServer : to build server nodes in a supervision
tree.
Show that it behaves exactly the same as the custom generic server
developed in part 2.

## Part 4 : error management : let it crash ! but supervised

```
elixir course_part_4.exs
```

Develop a special simple type of server, which role is to start child
servers, and restart them when they fail.
Introduce a bug to make `credit 2` fail, then show that the code is
still "working" - but the account server came back to it's initial working state.

## Part 5 : supervisor IS in standard library

```
elixir course_part_5.exs
```

Show that a complete and featurefull supervisor server is in the
standard Library : Supervisor : to build supervisor nodes in a supervision
tree.
Show that it behaves pretty much the same as the custom simple
supervisor developed in part 4.

## Part 6 : put the supervision tree into an OTP application

```
cd course_part_6
mix test
```

Erlang OTP applications includes : a configuration system, a standard
way to layout your code, a standard way to start a named "root
supervisor".
Elixir gives you a way to build these applications with nice tooling :

- config/config.exs manamement
- mix.exs to describe application
- test/*_test.exs files for unit testing

`mix test` launches the application then unit tests where the test cases has
been put.

`iex -S mix` launches the application then give you an elixir shell connected
to this application. You can try to call `GenServer.call(:account_server,:get)`
in this shell to communicate "in live" with the account server.
