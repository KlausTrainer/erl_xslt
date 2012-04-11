-module(erl_xslt).
-on_load(init/0).

-export([transform/2]).

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
    erlang:nif_error("NIF library not loaded", [{module, ?MODULE}, {line, ?LINE}]).
