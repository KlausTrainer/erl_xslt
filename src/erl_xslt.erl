-module(erl_xslt).
-export([transform/2]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
    {error, _} ->
        EbinDir = filename:dirname(code:which(?MODULE)),
        AppPath = filename:dirname(EbinDir),
        filename:join(AppPath, "priv");
    Path ->
        Path
    end,
    erlang:load_nif(filename:join(PrivDir, "erl_xslt"), 0).

%% @spec transform(XslFilename::binary(), Xml::binary()) -> {ok, binary()} | {error, Reason}
transform(_XslFilename, _Xml) ->
    "NIF library not loaded".
