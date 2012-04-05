# erl_xslt

erl_xslt provides an Erlang NIF for [libxslt](http://xmlsoft.org/xslt/).

## Features

erl_xslt provides an Erlang function for transforming XSLT stylesheets.
Simply specify a stylesheet's filename and the XML to be transformed as binary,
respectively. As of now, XSLT parameters are not supported.

Once a stylesheet has been read from the filesystem and is parsed, the
stylesheet's parsed representation is cached in memory to be reused for
speeding up subsequent transformations.

## Requirements

* [Erlang](https://github.com/erlang/otp) >= R14B
* [rebar](https://github.com/basho/rebar)
* [libxml2](http://xmlsoft.org/)
* [libxslt](http://xmlsoft.org/xslt/)

## Building

  ```Bash
    make # build
    make check # run testsuite to verify that everything works correctly
  ```

## Usage

  ```Erlang
    {ok, Xml} = file:read_file("foo.xml"),
    {ok, Result} = erl_xslt:transform(<<"stylesheet.xsl">>, Xml).
  ```
