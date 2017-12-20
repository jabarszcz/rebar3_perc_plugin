-module(rebar3_perc_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, perc).
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
           {opts, perc_opts:optspec_nodefaults()},   % Options understood by
                                                     % the plugin
           {short_desc, "Compile perc codecs with rebar"},
           {desc, "Compile perc codecs with rebar"}
          ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    {Args, _} = rebar_state:command_parsed_args(State),
    [compile_app(App, Args) || App <- Apps],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile_app(rebar_app_info:t(), perc_opts:options()) -> ok | no_return().
compile_app(AppInfo, GeneralOpts) ->
    Opts = rebar_app_info:opts(AppInfo),
    PercOpts = rebar_opts:get(Opts, perc_opts, []),
    CodecsOpts = proplists:get_value(codecs, PercOpts, []),
    CodecsOptsNew =
        case proplists:is_defined(in, PercOpts) of
            true -> [PercOpts | CodecsOpts];
            _ -> CodecsOpts
        end,
    [compile_codec(AppInfo, GeneralOpts ++ O) || O <- CodecsOptsNew],
    ok.

-spec compile_codec(rebar_app_info:t(), perc_opts:options()) ->
                           ok | no_return().
compile_codec(AppInfo, Opts) ->
    Dir = rebar_app_info:dir(AppInfo),
    BinDir = rebar_app_info:ebin_dir(AppInfo),
    So = proplists:get_value(so, Opts, filename:join(Dir, "./priv/")),
    NewOpts =
        [case O of
             {beam, Beam} ->
                 %% Beam path relative to ebin dir
                 {beam, filename:join(BinDir, Beam)};
             Other ->
                 Other
         end || O <- Opts] ++
        [{so, So},
         {appname, binary_to_atom(rebar_app_info:name(AppInfo), latin1)},
         {lib_dir, Dir}
        ],
    ok = perc:generate_codecs(NewOpts).
