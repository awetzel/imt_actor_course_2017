defmodule AccountServer do
  # introduce a bug the server does not allow any credit of an amount <=3, in order to test bug and restart
  def handle_cast({:credit,c},amount) when c > 3, do: {:noreply,amount+c}
  def handle_cast({:debit,c},amount), do: {:noreply,amount-c}
  def handle_call(:get,_,amount), do: {:reply,amount,amount}
  def init(amount), do: {:ok,amount}

  def start_link(initial_amount) do
    # the process ids (pid) can be "named" : ie. stored in a global
    # register in order to simplify queries
    # so here the server can be called with GenServer.call(:account_server,:get) for instance
    GenServer.start_link(AccountServer,initial_amount, name: :account_server)
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

# The additional module (compared to part5) to include the supervision tree in
# an OTP application `start/2` should return {:ok,root_pid} where `root_pid` is
# the root supervisor of the application.  This module is referenced in the
# `mix.exs` file in order to define the starting point of the application.
defmodule AccountApp do
  use Application
  def start(_type,_args) do
    AccountSupervisor.start_link( # initial account credit hold is taken from the configuration system
      Application.get_env(:course_part_6,:initial_account_amount))
  end
end
