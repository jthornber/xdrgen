Status
======

This project is not mature enough for general use yet.  Initially I'm
aiming to provide generators for Haskell and C, Ruby will probably
follow at some point.

Introduction
============

XDR is a little language to specify binary formats.  The full
specification can be found [here](http://www.faqs.org/rfcs/rfc1832.html).

Confusingly, the language has a lot of similarity with the C syntax
for data types.  However the two serve very different purposes.  Make
sure you get this difference clear in your head, and in particular
note the difference in the semantics of '*'.

This project consists of two elements:

- A small Haskell library for parsing XDR files.

- A program (xdrgen) that generates code for various languages:

    - language specific internal representations of the XDR objects
    - serialisers to move between internal and binary format

I've found in the past that a tool like this can make it trivial to
write code for network protocols, storing data on disk, transfering
data between languages etc.

A little rant
=============

I'm a big believer in having explicitly specified protocols between
components in large systems.

All too often people use serialisation libraries which have a more
emergent behaviour - often based on the data declarations for a
particular language.  For example, Java binary objects, or C++ classes
that all have individual serialisation methods.  This makes it
incredibly hard for other languages to interface to these components
(e.g, for functional testing).

Quite often far too much data ends up getting serialised.  I've worked
on projects where the size of messages passed over the network has
shrunk by an order of magnitude when switching to an explicitly
specified protocol.
