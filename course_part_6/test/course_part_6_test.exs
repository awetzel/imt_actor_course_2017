defmodule CoursePart6Test do
  use ExUnit.Case

  test "The use case test" do
    GenServer.cast(:account_server,{:credit,5})
    amount = GenServer.call(:account_server,:get)
    IO.puts "current credit hold is #{amount}"

    GenServer.cast(:account_server,{:credit,2})
    receive do after 10-> :ok end # wait the server to be re-started, wait 10 milliseconds
    GenServer.cast(:account_server,{:debit,3})
    amount = GenServer.call(:account_server,:get)
    IO.puts "current credit hold is #{amount}"
  end
end
