1. Introduction
---------------

This repository contains Ada source code and complete sample GNAT projects for
selected bare-board platforms supported by GNAT.  Initially the repository
contains software for ARM platforms from a specific vendor, but we intend this
to be a location for both AdaCore and the community in general to contribute
support for additional processors, platforms, and vendors.


2. Quick install
----------------

To use this library, you need:

* an Ada2012 compiler, targeting arm-eabi (for example [GNAT GPL 2016](http://libre.adacore.com/download/configurations#))
* [run-times](https://github.com/AdaCore/embedded-runtimes) for the target you want to use.
* please install first the runtimes before using the library
* try it out with the examples, or with some [demos](https://github.com/lambourg/Ada_Bare_Metal_Demos)


3. License
----------

All files are provided under a 3-clause Berkeley Software Distribution (BSD)
license. As such, and within the conditions required by the license, the files
are available both for proprietary ("commercial") and non-proprietary use.

For details, see the "LICENSE" file in the root directory.

4. Requirements
---------------

The software is written in Ada 2012 and uses, for example, preconditions,
postconditions, and the high-level iterator form of for-loops.

In addition, a GNAT implementation-defined pragma is used extensively. This
pragma makes it possible to avoid explicit temporary copies when assigning
components of types representing hardware registers requiring full word or full
half-word accesses. The pragma is named Volatile_Full_Access. Those persons
wishing to submit additions to the library should see the GNAT Reference Manual
for details.

Therefore, building with the sources requires a compiler supporting both Ada
2012 and the GNAT-defined pragma Volatile_Full_Access. The "GNAT GPL 2016"
compiler for ARM ELF is one such compiler [(Download it
here)](http://libre.adacore.com/download/configurations). A recent GNAT Pro
compiler for that target will also suffice.
