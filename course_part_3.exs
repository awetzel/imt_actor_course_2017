# Live coded during course - part 3
# GenericServer exists actually in Erlang libraries: named GenServer
# replace our GenericServer with erlang GenServer and observe that it works the same
# `elixir course_part_3.exs` to execute it

defmodule AccountServer do
  def handle_cast({:credit,c},amount), do: {:noreply,amount+c}
  def handle_cast({:debit,c},amount), do: {:noreply,amount-c}
  def handle_call(:get,_,amount), do: {:reply,amount,amount}
  def init(amount), do: {:ok,amount}

  def start_link(initial_amount) do
    GenServer.start_link(AccountServer,initial_amount)
  end
end

{:ok,mon_compte} = AccountServer.start_link(4)

GenServer.cast(mon_compte,{:credit,5})
GenServer.cast(mon_compte,{:credit,2})
GenServer.cast(mon_compte,{:debit,3})
amount = GenServer.call(mon_compte,:get)

IO.puts "current credit hold is #{amount}"
