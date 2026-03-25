# リーチ計算機 Riichi Keisanki

A graphical user interface with tools for Riichi Mahjong.

It can compute the list of yakus and scores from a given hand.

You can try the WebApp version at https://riichi.elinvention.ovh/.

## Building

Currently there are 2 GUIs. The original one is made with [cljfx], written in
Clojure and should be easy to run on your PC.
The other one is made with [reagent], written in ClojureScript and is meant for
the web (this version should run even on Android).

1. Clone this repo with submodules:
`git clone --recurse-submodules https://github.com/Elinvention/riichi-keisanki.git`
2. Depending on which GUI you want:
    - Run `clj -Xmain` to launch the cljfx GUI (old GUI, not recommended).
    - Run `npm install && npx shadow-cljs watch app` to launch an
    interactive REPL and a web browser with the reagent GUI.

[cljfx]: https://github.com/cljfx/cljfx/
[reagent]: https://github.com/reagent-project/reagent

## Testing

Run `npx shadow-cljs compile test` to build and automatically launch tests.

## Contributing

Pull requests are always welcome.
For major changes, please open an issue first to discuss what you would like to
change.

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
