-module(rebar3_perc_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_perc_plugin).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create(
          [
           {name, ?PROVIDER},            % The 'user friendly' name of the task
           {module, ?MODULE},            % The module implementation of the task
           {bare, true},                 % The task can be run by the user
           {deps, ?DEPS},                % The list of dependencies
           {opts, perc:get_optspec()},   % Options understood by the plugin
           {short_desc, "Compile perc codecs with rebar"},
           {desc, "Compile perc codecs with rebar"}
          ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
