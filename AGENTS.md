# AGENTS.md - riichi-calc Development Guidelines

## Project Overview

A Riichi (Japanese Mahjong) hand/score calculator. The maintained UI is a
ClojureScript/Reagent web app (`riichi-calc.ui.reagent.*`) using Bulma plus an
on-device ONNX tile scanner. `riichi-calc.ui.fx.*` is a legacy, unmaintained
cljfx desktop GUI (`clj -Xmain`) - do not extend it.

Naming is inconsistent and easy to guess wrong: `package.json` name is
`riichi-keisanki`, namespaces are `riichi-calc.*`, but the source directory is
`src/riichi_calc/` (underscores). Follow the surrounding file for paths.

## Commands

```bash
npm install
npx shadow-cljs watch app        # dev server on :8020, hot reload + REPL
npx shadow-cljs compile app test # compile web app and run tests (autorun)
npm run build                    # production: shadow release + fingerprint -> dist/
```

- `npm test` is a stub that always exits 1. Run tests with shadow-cljs instead.
- Tests are `test/riichi_calc/*_test.cljc`, matched by `:ns-regexp "-test$"`;
  the `:test` build is `:node-test` with `:autorun true`.
- There is no lint/format script; `.clj-kondo/` is only consumed by the editor LSP.
- Clean rebuild: `rm -rf target/ resources/public/assets/js/* && npx shadow-cljs compile app test`.

## Architecture

- Pure, platform-independent logic lives in `.cljc` files under `src/riichi_calc/`
  (`tile`, `hand`, `game`, `group`, `state`, `vision`, `vision.yolo`). Put new
  scoring/detection logic here so it stays unit-testable; it compiles for both the
  JVM and JavaScript.
- Browser-only code (DOM/React/ONNX) lives in `.cljs` under
  `src/riichi_calc/ui/reagent/`.
- `onnxruntime-web` is NOT imported as a module. `index.html` loads
  `assets/ort/ort.wasm.min.js`, which sets a global `ort` read via `js/window.ort`
  in `ui/reagent/onnx.cljs`.

## Build pipeline (`npm run build`)

The build script does more than `shadow-cljs release`:
1. `shadow-cljs release app` with `--config-merge` injecting `VERSION` (from
   package.json), `BUILD-SHA` and `BUILD-DATE` (`goog-define`s in
   `ui/reagent/main.cljs`). `BUILD_SHA` env var overrides the git SHA.
2. `scripts/fingerprint.mjs` copies `resources/public` -> `dist/`, content-hashes
   CSS/models, rewrites `index.html`, prunes stale `main.*.js`, generates `sw.js`.
   It errors if no hashed `main.<hash>.js` exists, so the release step must run first.

## Detection models & assets

- Models live in `resources/public/assets/models/`, named
  `riichi.v<N>.<model>.<quant>.onnx`; the name drives newest-first sort order.
- `resources/public/index.html` holds `window.RIICHI_MODELS`; its order is the
  picker order and the first entry is the default. To add a model, drop the file in
  and add an entry (the dev server auto-lists files; release rewrites the list).
- `.onnx` and `.wasm` are git-lfs tracked; `resources/public/assets/tiles` is a git
  submodule. Clone with `--recurse-submodules` and install `git-lfs` first.
- Dev-only: `riichi-calc.dev/handler` (wired in `shadow-cljs.edn` `:dev-http`)
  serves a live model manifest at `/assets/models/dev-models.json`.

## Deployment

Pushing to the remote triggers `post-receive` (copy at repo root): per branch it
runs `npm install && BUILD_SHA=<newrev> npm run build` and copies `dist/` under
`/srv/riichi/<branch>/`. Pushing therefore deploys - be deliberate.
