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
        { rebar3_perc_plugin, ".*", {git, "git@host:user/rebar3_perc_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_perc_plugin
    ===> Fetching rebar3_perc_plugin
    ===> Compiling rebar3_perc_plugin
    <Plugin Output>
