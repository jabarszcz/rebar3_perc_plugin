rebar3_perc_plugin
=====

Compile perc codecs with rebar

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_perc_plugin, ".*",
            {git, "https://github.com/jabarszcz/rebar3_perc_plugin.git",
                {branch, "master"}
            }
        }
    ]}.

Then also add the pre-compile hook and the configuration options:

    {provider_hooks, [
        {pre, [{compile, perc}]}
      ]}.

    {perc_opts, [
        {in, "myrecord.hrl"},
        {record, "myrecordname"},
        {erl_out, "encoder"}
      ]}.
