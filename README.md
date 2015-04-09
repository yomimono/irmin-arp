# Caution: Sharp Edges

This library is intended as a test/prototype library and exploration of the possibility space around testing networking code in the MirageOS library operating systems project.  Currently, it is not usable as a drop-in replacement for the stock MirageOS ARP library.

## What Does This Thing Do?

The Arp implementation given uses Irmin to store a Map representing the ARP cache.  This is functorized and can be either an in-memory Irmin store or a Git-backed store on a filesystem.  

Tests are included which use Magnus Skjegstad's [mirage-vnetif](https://github.com/magnuss/mirage-vnetif) library to mock out communications between network devices.

Test code is included which uses the Git-backed filesystem store to make assertions about the operations performed on the cache as a result of certain packet inputs.
