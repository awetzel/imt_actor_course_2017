defmodule CoursePart6Test do
  use ExUnit.Case

  test "The use case test" do
    GenServer.cast(:account_server,{:credit,5})
    amount = GenServer.call(:account_server,:get)
    IO.puts "current credit hold is #{amount}"
    assert amount == 9

    GenServer.cast(:account_server,{:credit,2})
    receive do after 10-> :ok end # wait the server to be re-started, wait 10 milliseconds
    GenServer.cast(:account_server,{:debit,3})
    amount = GenServer.call(:account_server,:get)
    IO.puts "current credit hold is #{amount}"
    assert amount == 1
  end
end
