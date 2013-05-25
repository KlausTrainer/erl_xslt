-module(erl_xslt_test).
-include_lib("eunit/include/eunit.hrl").

transform_test() ->
    TestDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "test"]),
    Xslt = list_to_binary(filename:join(TestDir, "xsl.xsl")),

    {ok, Xml} = file:read_file(filename:join(TestDir, "xml.xml")),
    {ok, ExpectedResult} = file:read_file(filename:join(TestDir, "result.xml")),
    {ok, Result} = erl_xslt:transform(Xslt, Xml),
    ?assertEqual(ExpectedResult, Result),
    {ok, Result} = erl_xslt:transform(Xslt, Xml),
    ?assertEqual(ExpectedResult, Result),
    {ok, Result} = erl_xslt:transform(Xslt, Xml),
    ?assertEqual(ExpectedResult, Result),

    ?assertEqual({error, no_stylesheet}, erl_xslt:transform(<<"foo.xsl">>, Xml)),

    {ok, InvalidXml} = file:read_file(filename:join(TestDir, "invalid_xml.xml")),
    ?assertEqual({error, invalid_xml}, erl_xslt:transform(Xslt, InvalidXml)).
