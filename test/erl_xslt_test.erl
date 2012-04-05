-module(erl_xslt_test).
-include_lib("eunit/include/eunit.hrl").

transform_test() ->
    TestDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "test"]),
    Xslt = list_to_binary(filename:join(TestDir, "xsl.xsl")),
    {ok, Xml} = file:read_file(filename:join(TestDir, "xml.xml")),
    {ok, ExpectedResult} = file:read_file(filename:join(TestDir, "result.xml")),
    {ok, ExpectedResult} = erl_xslt:transform(Xslt, Xml).
