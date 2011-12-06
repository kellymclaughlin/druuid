## Druuid

### Overview

An Erlang NIF wrapper around the
[OSSP-UUID](http://www.ossp.org/pkg/lib/uuid/) library.

Currently only V4 UUIDs are supported, but support for other versions
is coming.

### Building

The druuid repo contains version 1.6.2 of the OSSP uuid libary. To
build just clone the repo and type the following:

<pre>
<code>
make
</code>
</pre>

### Usage

#### Generate a v4 uuid

<pre>
<code>
druuid:v4().
</code>
</pre>

