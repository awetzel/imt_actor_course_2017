# Bonus After course, not had time for this part - part 5
# like GenServer, a more complete Supervisor is already coded into
# Erlang/Elixir;  use it to do the same as the previous part.
# `elixir course_part_5.exs` to execute it

defmodule AccountServer do
  # introduce a bug the server does not allow any credit of an amount <=3, in order to test bug and restart
  def handle_cast({:credit,c},amount) when c > 3, do: {:noreply,amount+c}
  def handle_cast({:debit,c},amount), do: {:noreply,amount-c}
  def handle_call(:get,_,amount), do: {:reply,amount,amount}
  def init(amount), do: {:ok,amount}

  def start_link(initial_amount) do
    GenServer.start_link(AccountServer,initial_amount)
  end
end

defmodule AccountSupervisor do
  use Supervisor
  def init(initial_amount) do
    supervise([
      # more complex child spec, start_link is the default function to start Child, and second parameter is start_link parameter list
      # so here : AccountServer.start_link(initial_amount) is called to start the child
      worker(AccountServer,[initial_amount])
    ], strategy: :one_for_one)
    # strategy is supervisor restart strategy : 
    # one_for_one means when a single child dies, it is restarted alone
    # one_for_all means when a single child dies, all the other one are killed and restarted
  end
  def start_link(initial_amount) do
    Supervisor.start_link(AccountSupervisor,initial_amount)
  end
end

mon_compte = fn sup_pid-> # func to get mon_compte pid from supervisor
  [{_,single_child_pid,_,_}] = Supervisor.which_children(sup_pid)
  single_child_pid
end

{:ok,sup} = AccountSupervisor.start_link(4)
GenServer.cast(mon_compte.(sup),{:credit,5})
amount = GenServer.call(mon_compte.(sup),:get)
IO.puts "current credit hold is #{amount}"

GenServer.cast(mon_compte.(sup),{:credit,2})
receive do after 10-> :ok end # wait the server to be re-started, wait 10 milliseconds
GenServer.cast(mon_compte.(sup),{:debit,3})
amount = GenServer.call(mon_compte.(sup),:get)
IO.puts "current credit hold is #{amount}"
