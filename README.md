# lenteja

## what's this?

Suppose you have a strongly-typed Haskell data structure, and you want to explore and dig into it using lensy-like expressions entered interactively at runtime.

The program would have to parse the lensy-like expressions and assemble an optic from them, but how? What would be required? Ain't that too much dynamicity for Haskell?

This repo is a minimal proof-of-concept.

## How does it work?

Basically, there's a `HasLentejas` typeclass. For types that are instances of this typeclass, a map of existentially quantified optics can be produced. The optics go from the type to each of its (existentially quantified) fields.

## What could be improved?

Right now the `HasLentejas` instances are manually declared; perhaps they could be generically derived.

Support for setters and traversals could be added (working over a record value kept in a mutable reference).

Also Ix- and At- like functionality could be added, to allow indexing over list fields. 

Right now the "end representation" uses `Show`, but perhaps it could be configurable and allow using something like `ToJSON`.

Support for listing available optics. Also, autocompletion. 

## example of use

    Enter lensy exp:
    name
    A single result:
    "John"
    Enter lensy exp:
    partner . name
    A single result:
    "Sara"
    Enter lensy exp:
    pets . folded
    Multiple results:
    Pet {petName = "Fido", petAge = 4}
    Pet {petName = "Fifi", petAge = 3}
    Enter lensy exp:
    pets . folded . petName
    Multiple results:
    "Fido"
    "Fifi"

