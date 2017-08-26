# edgar #

Erlang Does Gnu AR.

## Overview ##

``edgar`` is a module inspired of ``erl_tar`` and doing ``GNU ar`` archive format.
Same API than ``erl_tar`` is exported, except ``add/3`` and ``add/4`` (to be done in a next version).


## Documentation ##

A complete documentation is available.

Simply run `make docs` and open `doc/index.html` in your favorite browser, this will insure you having the documentation related to your version.

## Quick Start ##

```
git clone git://github.com/crownedgrouse/edgar.git
cd edgar
make
erl -pa `pwd`/ebin
```

## Running tests caveat ##
When cloning project, references `ar` files have to be created first, as reference files coming with new timestamps and UID/GID.

To do so simply run `make init_test` once.

You can then run `make tests`.

Note : Test initialization need `ar` tool to be installed. Running tests on some platform/OS may not be possible, then.
But this does not mean `edgar` cannot be used on those platform/OS.



## Contributing ##

Contributions are welcome. Please use pull-requests.
