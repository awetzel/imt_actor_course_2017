# Live coded during course - part 4
# from an account Server process created in previous parts
# try to create a very simple Supervisor by hand in order to explain how it works.
# introduce a bug : server only handle credit > 3, then observe
# result with same operations as previous parts
# `elixir course_part_4.exs` to execute it

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

defmodule SimpleSupervisor do
  def loop(childs_spec) do # childs_spec = [{pid,func}]
    receive do
      {:EXIT,this_pid_died,_reason}-> # as we are in trap_exit mode, this message is sent on "link" dead signal
        {_,func_to_launch} = Enum.find(childs_spec, fn {pid,_func}-> 
          pid == this_pid_died
        end) # find the function to restart the child
        {:ok,new_pid} = func_to_launch.() # start the function to relaunch the child
        new_childs_spec = Enum.map(childs_spec, fn {pid,func}->
          if pid == this_pid_died do
            {new_pid,func}
          else
            {pid,func}
          end
        end) ## change in the supervisor state the "pid" of the child
        loop(new_childs_spec)
      {:get_children,sender}-> ## message for another process to "get" the current children list
        send(sender,childs_spec)
        loop(childs_spec)
    end
  end

  def start_link(child_funcs) do
    {:ok,spawn_link(fn->
      # when a linked process dies, by default, it kills the other linked process
      # the :trap_exit flag make the "death signal" to be converted into a
      # simple message passing of {:EXIT,the_dead_linked_process,the_death_reason}
      :erlang.process_flag(:trap_exit,true) 
      # first the supervisor starts all its childs (each child_fun is a "start_link" function)
      initial_childs_spec = Enum.map(child_funcs, fn child_fun->
        {:ok,pid} = child_fun.()
        {pid,child_fun}
      end)
      # then go to the server loop in order to wait death of childs to restart them
      loop(initial_childs_spec)
    end)}
  end
end

mon_compte = fn sup_pid-> # func to get mon_compte pid from supervisor children
  send(sup_pid,{:get_children,self()})
  receive do
    [{single_child_pid,_single_child_fn}]-> single_child_pid
  end
end

{:ok,sup} = SimpleSupervisor.start_link([fn->
   AccountServer.start_link(4)
end])
GenServer.cast(mon_compte.(sup),{:credit,5})
amount = GenServer.call(mon_compte.(sup),:get)
IO.puts "current credit hold is #{amount}"

GenServer.cast(mon_compte.(sup),{:credit,2})
receive do after 10-> :ok end # wait the server to be re-started, wait 10 milliseconds
GenServer.cast(mon_compte.(sup),{:debit,3})
amount = GenServer.call(mon_compte.(sup),:get)
IO.puts "current credit hold is #{amount}"
