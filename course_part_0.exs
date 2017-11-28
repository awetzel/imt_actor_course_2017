# Basic use case - part 0
# Elixir is a functionnal language, function handles immutable (not
# modifiable) values
# The use case is a bank account which can be retreived, credited or
# debited

# `elixir course_part_0.exs` to execute it

defmodule AccountFunction do
  def credit(amount,to_credit), do: amount + to_credit
  def debit(amount,to_debit), do: amount + to_debit
end

mon_compte0 = 4
mon_compte1 = AccountFunction.credit(mon_compte0,5)
mon_compte2 = AccountFunction.credit(mon_compte1,2)
mon_compte3 = AccountFunction.debit(mon_compte2,3)
amount = mon_compte3
IO.puts "current credit hold is #{amount}"

## First, this could also be written in elixir with
# mon_compte = AccountFunction.credit(mon_compte,5)
# because Elixir allows this "rebinding" of the name "mon_compte" with another value
# I did not write it like this to avoid confusion : "mon_compte" is NOT a variable, it is a "value"

# If you are in a web request to credit the account, and that the 
# account "state" (the amount) is handled by an external "stateful"
# system (database, filesystem), then the code above is sufficient
# ; stateless : your code has no state : it is only a computation/transformation

# BUT what if you are building a low level "account" server for a
# bank, and want the account state to be managed by your software
# (so manually handle the transactional properties of the banking
# system, the exact transaction time, etc.)
# You need that each account amount to be global : it should be
# stored in live memory (RAM) of the software.

# - The "Actor oriented" way to handle this kind of state is to
# handle it in a separated micro process and communicate with it
# through messages : see course_part_1.exs for basic implementation

# - But the main part of this implementation is generic and 
# not specific with the "bank account" use case, so we need to create
# a kind of "polimorphism" where the "server" code can be 
# used to develop multiple type of servers. This can be done in a
# functional language : you need to transmit to the generic function
# parameter either a specific anonymous function, or a module
# providing specific functions.
# : see course_part_2.exs for implementation 

# - But since the "actor oriented" way is the core of Erlang, a
# "generic" server implementation is provided by the standard library
# so you can directly use it
# : see course_part_3.exs for implementation

# - The "Erlang/Elixir way" to handle exceptions of theses processes
# is to created "supervisor" processes. These supervisor must be
# linked with child server processes  and restart them on failure.
# An application is organised as a tree of supervisors where leafs are servers.
# : see course_part_4.exs for basic supervisor implementation

# - But since this "supervisor tree" approach of error handling is
# the core of Erlang, a more complete implementation, handling
# maximum restart rate, is provided by the standard library so you
# can directly use it
# : see course_part_5.exs for implementation

# - But since this "supervisor tree" approach of error handling is
# the core of Erlang, a more complete implementation, handling
# maximum restart rate, is provided by the standard library so you
# can directly use it
# : see course_part_5.exs for implementation

# - In erlang, the root supervisor of all a supervisor tree is called
# an OTP Application. Erlang handles OTP applications with a special
# "application supervisor" process handled by the virtual machine.
# see "course_part_6" folder for inclusion of the previously developped
# supervision tree in the OTP application model.


# usage:
# elixir course_part_0.exs
# elixir course_part_1.exs
# elixir course_part_2.exs
# elixir course_part_3.exs
# elixir course_part_4.exs
# elixir course_part_5.exs
# cd course_part_6; mix test
