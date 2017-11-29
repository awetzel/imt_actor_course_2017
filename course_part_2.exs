# Live coded during course - part 2
# Inject the specific behaviour of the "account" server in a more
# generic server. To do that, put the account server module name in
# the state of the server (callback), and call `handle_cast` and `handle_call` functions.

# `elixir course_part_2.exs` to execute it

defmodule GenericServer do
  # the process "state" contain the module name (callback) where to take the
  # functions defining the way to change the state or answer when a given message is received
  # and transmit a generic "state" value : so process state is {callback,state}
  def loop({callback,state}) do 
    receive do
      {:cast,msg}-> 
        # a cast is an asynchronious message passing which changes the server state
        # handle_cast describe how the state changes when a given message/event arrives
        loop({callback,callback.handle_cast(msg,state)})
      {:call,msg,pid}->
        # a call is a synchronious call to send some information back to the calling process (RPC call)
        # handle_call compute the response from the query message and the current process state
        send(pid,callback.handle_call(msg,state))
        loop({callback,state})
    end
  end

  def cast(pid,msg), do: send(pid,{:cast,msg})
  def call(pid,msg) do 
     send(pid,{:call,msg,self()})
     receive do msg-> msg end
  end

  def start_link(callback,initial_state) do
    server_pid = spawn_link(fn-> loop({callback,initial_state}) end)
    {:ok,server_pid}
  end
end

# implement our account server logic as a callback module implementing handle_cast and handle_call
defmodule AccountServer do
  def handle_cast({:credit,c},amount), do: amount+c
  def handle_cast({:debit,c},amount), do: amount-c
  def handle_call(:get,amount), do: amount

  def start_link(initial_amount) do
    GenericServer.start_link(AccountServer,initial_amount)
  end
end

{:ok,mon_compte} = AccountServer.start_link(4)
GenericServer.cast(mon_compte,{:credit,5})
GenericServer.cast(mon_compte,{:credit,2})
GenericServer.cast(mon_compte,{:debit,3})
amount = GenericServer.call(mon_compte,:get)
IO.puts "current credit hold is #{amount}"
