# AGENTS.md - riichi-calc Development Guidelines

## Project Overview

A Clojure/ClojureScript Riichi (Japanese Mahjong) calculator with Reagent-based UI.
Built with `deps` and `shadow-cljs`.
There is also an outdated `cljfx` GUI.

---

## Build & Test Commands

### Setup

```bash
# Install npm dependencies
npm install

# Compile cljs to JS once
npx shadow-cljs build

# Start dev server on :app build (port 8020)
npx shadow-cljs watch app
```

### Versioning & Production Builds

The project uses semantic versioning and injects build information (SHA and Date) into the UI.

```bash
# Production build with versioning
npm run build
```

This command uses `shadow-cljs release` with `--config-merge` to inject:
- `VERSION`: From `package.json`
- `BUILD-SHA`: Current git commit SHA (can be overridden by `BUILD_SHA` env var)
- `BUILD-DATE`: Current date

In ClojureScript, these are accessed via `goog-define` in `riichi-calc.ui.reagent.main`:
```clojure
(goog-define VERSION "0.0.0")
(goog-define BUILD-SHA "UNKNOWN")
(goog-define BUILD-DATE "UNKNOWN")
```

### Testing

```bash
npx shadow-cljs compile test
```

### Build & Lint

```bash
# Compile all builds
npx shadow-cljs build

# Watch and compile
npx shadow-cljs watch

# Clean rebuild
rm -rf target/ resource/public/assets/js/*
npx shadow-cljs build
```

---

## Code Style Guidelines

### Imports

```clojure
; Place imports in dedicated namespace or use :require-macros
(require '[clojure.string :as str])
; Prefer explicit namespace over :default
(require '[cljsjs.react.client.react.core :refer [React]])
```

### Formatting

- Two spaces for indentation - NO tabs
- Line width: 80 chars minimum, 120 max

### Types

```clojure
; All public functions with args have metadata docstrings
(defn ^{:arglists "[]" :doc "description"} my-func [x y] ...)
; Use type hints when performance critical or for cljs interoperop
#^{:long-name true} (defn long-name-var [] ...)
```

### Naming Conventions

- Functions: `kebab-case` verbs (`get-hand-scores`)
- Namespaces: `snake_case` (`riichi_calc.hand`)
- Variables: `camelCase` for locals, `upper_case` constants (`HAND_SCORES`)
- React components: PascalCase (e.g., `ScoreWidgetComponent`)

### ClojureScript-specific

- Import `cljs.core` functions explicitly instead of relying on implicit bindings
- Use `js/*` for JS globals in interop code: `(/ js.Math.max x y)`
- For React interop, use `cljsjs.react.client.react.core.ns/...`

---

## Reagent/React Guidelines

### Component Structure

```clojure
(ns riichi-calc.ui.reagent.wizard
  (:require [reagent.core :as r]
            [reagent.dom :as dom]))

(defn WizardComponent []
  [:div "Wizard content"])

; Expose for SSR if needed:
(dom/render
  (comp [[#WizardComponent]]) (.getElementById js/document "app"))
```

### Component Props Convention

- All props defined as destructuring pattern: `{:as opts :keys [title score]} [opts]`
- Use `r/atom` for internal state, pass to subcomponents via `refers`/context
