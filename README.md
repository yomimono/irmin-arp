# Caution: Sharp Edges

This library is intended as a test/prototype library and exploration of the possibility space around testing networking code in the MirageOS library operating systems project.  Currently, it is not usable as a drop-in replacement for the stock MirageOS ARP module supplied by `tcpip`.

## To Build

The following dependencies are needed from the default opam repository:

```
opam install irmin irmin-unix alcotest ezjsonm lwt ipaddr ounit mirage-clock-unix mirage-unix
```

Pinned versions of `tcpip` and `mirage-types` are currently required:

```
opam pin add mirage https://github.com/yomimono/mirage#separate_arp
opam pin add tcpip https://github.com/yomimono/tcpip#separate_arp
```

`make` should now conclude successfully; if not, please let me know via a GitHub issue :)

## What Does This Thing Do?

[A more thorough explanation of this code's aims and actions](http://somerandomidiot.com/blog/2015/04/24/what-a-distributed-version-controlled-ARP-cache-gets-you/) is available on my blog.

The Arp implementation given uses Irmin to store a Map representing the ARP cache.  This is functorized and can be either an in-memory Irmin store or a Git-backed store on a filesystem.  

Tests are included which use Magnus Skjegstad's [mirage-vnetif](https://github.com/magnuss/mirage-vnetif) library to mock out communications between network devices.

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
