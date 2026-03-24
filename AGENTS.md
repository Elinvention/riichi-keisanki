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
shadow build

# Start dev server on :app build (port 8020)
shadow watch app
```

### Testing

```bash
npx shadow-cljs compile test
```

### Build & Lint

```bash
# Compile all builds
shadow build

# Watch and compile
shadow watch

# Clean rebuild
rm -rf target/ resource/public/assets/js/*
shadow build
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
