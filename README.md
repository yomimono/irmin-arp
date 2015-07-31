# Caution: Sharp Edges

This library is intended as a test/prototype library and exploration of the possibility space around testing networking code in the MirageOS library operating systems project.  It is usable as a drop-in replacement for the stock MirageOS ARP module supplied by `tcpip` -- for an example, see [this example unikernel](https://github.com/yomimono/example-unikernels/tree/mirarp/irmin-arp-node).  For limitations, please see [a more thorough explanation of the project](http://somerandomidiot.com/blog/2015/04/24/what-a-distributed-version-controlled-ARP-cache-gets-you/).

## To Build

The following dependencies are needed from the default opam repository:

```
opam install irmin irmin-unix alcotest ezjsonm lwt ipaddr ounit mirage-clock-unix mirage-unix tcpip mirage mirage-types
```

`make` should now conclude successfully; if not, please [let me know!](https://github.com/yomimono/irmin-arp/issues/new)

## What Does This Thing Do?

[A more thorough explanation of this code's aims and actions](https://somerandomidiot.com/blog/2015/04/24/what-a-distributed-version-controlled-ARP-cache-gets-you/) is available on my blog.

The ARP implementation given uses Irmin to store a Map representing the ARP cache.  This is functorized and can be either an in-memory Irmin store or a Git-backed store on a filesystem.  (Currently, only the in-memory store is available to MirageOS unikernels running on Xen.)

Tests are included which use Magnus Skjegstad's [mirage-vnetif](https://github.com/magnuss/mirage-vnetif) library to mock out communications between network devices using the [mirage-tcpip](https://github.com/mirage/mirage-tcpip) Ethernet protocol implementation.

Test code is included which uses the Git-backed filesystem store to make assertions about the operations performed on the cache as a result of certain packet inputs.

To see something interesting happen, build the tests:

```
ocaml setup.ml -configure --enable-tests
make test
```

Git repositories reflecting test state will be generated in the subdirectory `test_results`.  For example, to see commits and merges made by the "speaker" interface during the `entries_aged_out` test:

```
cd test_results/entries_aged_out/speaker
git log --all --graph --oneline
```

Alternately, interacting with the state of the repository via the graphical git repository browser `gitk` is also of potential interest.  Be sure to invoke `gitk --all` if you're interested in seeing all the branches (you probably are!).

There is also a demo which simulates multiple network nodes communicating over a TCP built on top of Irmin-ARP with the in-memory backend.  Build and run it with:

```
ocaml setup.ml -configure --enable-demo
make
./demo_network.native
```
