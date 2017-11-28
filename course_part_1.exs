# Live coded during course - part 1
# thanks to `spawn_link`, `send`, `receive`, create a linked 
# server process to handle a bank account (state is the bank account
# credit hold), with a protocol of messages to credit, debit, or
# retrieve the account amount.

# `elixir course_part_1.exs` to execute it

defmodule AccountServer do
  # since this function is called recursively and block on "receive", 
  # this "amount" parameter is actually the process state in memory
  def loop(amount) do 
    receive do # 
      {:credit,c}->
        loop(amount + c)
      {:debit,c}->
        loop(amount - c)
      {:get,sender}->
        send(sender,amount)
        loop(amount)
    end
  end

  def start_link(initial_amount) do
    pid = spawn_link(fn->
      loop(initial_amount)
    end)
    {:ok,pid}
  end
  def credit(pid,c), do: send(pid,{:credit,c})
  def debit(pid,c), do: send(pid,{:debit,c})
  def get(pid) do
    send(pid,{:get,self()})
    receive do amount-> amount end
  end
end

{:ok,mon_compte} = AccountServer.start_link(4)
AccountServer.credit(mon_compte,5)
AccountServer.credit(mon_compte,2)
AccountServer.debit(mon_compte,3)
amount = AccountServer.get(mon_compte)
IO.puts "current credit hold is #{amount}"
