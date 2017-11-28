# CoursePart6

The part 6 of the "Actor with Elixir/erlang" course for IMT Atlantique.

The "application" model of erlang provide additional features to `course_part_5` :
- a global "register" which saves a map of `name=>process_id` in
  order to call servers by name. (usage register(:some_name,pid) or GenServer.start_link(Callback,args, name: :some_name)
- a "configuration" server which saves a map of configuration values,
  - they are listed in `config/config.exs`
  - they can be retrieved by `Application.get_env(:application_name,:conf_name)`
- the definition of an application: in `mix.exs : def application`
  - containing the OTP application dependencies (what application to start BEFORE the current one)
  - containing which module launches the app root supervisor (start function)

The idea of "part6" is to : 
- identify the Account server with a registered name: `:account_server`
- put the root supervisor (AccountSup) in the `start` application function (see mix.exs + AccountApp module)
- set the initial account amount in the configuration : see config/config.exs and AccountApp module
- put the use case in a unit test (see `test/course_part_6_test.exs`)

## Usage

To do the same as `elixir course_part_5.exs` with this part 6, go in
`course_part_6` directory then :

```
mix test
```

to execute the test in `test/course_part_6_test.exs`
