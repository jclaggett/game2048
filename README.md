# game2048

A Clojure implementation of the 2048 game plus various players that
play the game. The notable feature of this implementation is it is written
using "Updater Style" as described in this README.

## Usage

FIXME

## Updater Style Code

The goal of Updater Style is to encourage composible functions plus modular
state. The strategy is to limit state changes and side effects to a specific
class of functions known as 'updater functions'.

Here are the specific rules of this style:

1. An Updater Function can always accept as its first argument any value it could return.
2. Updater Functions may perform side effects. No other functions may perform
   side effects or call functions that do.
3. Updater Functions may mutate their first argument and no other argument. No
   other functions may mutate their arguments or call functions that do.

## For ClojureScript dev:

lein cljx auto < /dev/null &
lein cljsbuild auto

## License

Copyright © 2014 Jonathan Claggett & Chris Houser

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
