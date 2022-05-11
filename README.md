# リーチ計算機 Riichi Keisanki

A graphical user interface with tools for Riichi Mahjong.

It can compute the list of yakus and scores from a given hand.

## Usage

Currently there are 2 GUIs. The original one is made with [cljfx], written in
Clojure and should be easy to run on your PC.
The other one is made with [reagent], written in ClojureScript and is meant for
the web (this version should run even on Android).

1. Clone this repo with submodules:
`git clone --recurse-submodules https://github.com/Elinvention/riichi-keisanki.git`
2. Depending on which GUI you want:
    - Run `clj -Xmain` to launch the cljfx GUI.
    - Run `clj -M --main cljs.main --compile reagent.core --repl` to launch an
    interactive REPL and a web browser with the reagent GUI.

It can take a while to start as it has to compile initially.

[cljfx]: https://github.com/cljfx/cljfx/
[reagent]: https://github.com/reagent-project/reagent

## License

Copyright © 2021 Elia Argentieri

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
