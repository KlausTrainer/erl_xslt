# erl_xslt

erl_xslt provides an Erlang NIF binding for
[libxslt](http://xmlsoft.org/xslt/) that can be used for
[XSL Transformations](http://www.w3.org/TR/xslt).

## Functionality

Simply specify an XSLT stylesheet's filename and the XML to be transformed as
binary, respectively. XSLT parameters are currently not supported.

Once a stylesheet has been read from the filesystem and is parsed, the
stylesheet's parsed representation is cached in memory to be reused for
speeding up subsequent transformations.

## Requirements

* [Erlang](https://github.com/erlang/otp) >= R14B (with header files)
* [rebar](https://github.com/basho/rebar)
* [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config)
* [libxml2](http://xmlsoft.org/) (with header files)
* [libxslt](http://xmlsoft.org/xslt/) (with header files)

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
