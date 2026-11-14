import { createHash } from "node:crypto";
import {
  cpSync,
  existsSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { execFileSync } from "node:child_process";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("..", import.meta.url));
const publicDir = join(root, "resources", "public");
const jsDir = join(publicDir, "assets", "js");
const distDir = join(root, "dist");

const pkg = JSON.parse(readFileSync(join(root, "package.json"), "utf8"));
const version = pkg.version ?? "0.0.0";
const sha =
  process.env.BUILD_SHA ??
  (() => {
    try {
      return execFileSync("git", ["rev-parse", "--short", "HEAD"], {
        cwd: root,
        encoding: "utf8",
      }).trim();
    } catch {
      return "UNKNOWN";
    }
  })();

function shortHash(file) {
  return createHash("sha256").update(readFileSync(file)).digest("hex").slice(0, 12);
}

const modelQualifiers = { fp16: "fp16", int8: "int8", int8s_conv: "int8" };

function modelLabel(name) {
  const parts = name.replace(/^riichi\.v\d+\./, "").replace(/\.onnx$/, "").split(".");
  const [model, ...qualifiers] = parts;
  if (qualifiers.length === 0) qualifiers.push("fp32");
  return [model, ...qualifiers.map((q) => modelQualifiers[q] ?? q)].join(" ");
}

function findMainJs() {
  const manifestPath = join(jsDir, "manifest.edn");
  if (existsSync(manifestPath)) {
    const match = readFileSync(manifestPath, "utf8").match(/main\.[0-9A-Za-z]+\.js/);
    if (match && existsSync(join(jsDir, match[0]))) return match[0];
  }
  const candidates = readdirSync(jsDir)
    .filter((f) => /^main\.[0-9A-Za-z]+\.js$/.test(f))
    .map((f) => ({ f, mtime: statSync(join(jsDir, f)).mtimeMs }))
    .sort((a, b) => b.mtime - a.mtime);
  if (candidates.length === 0) {
    throw new Error(
      `no hashed main.<hash>.js found in ${jsDir}; run "shadow-cljs release app" first`,
    );
  }
  return candidates[0].f;
}

const mainJs = findMainJs();

for (const f of readdirSync(jsDir)) {
  if (f !== mainJs && /^main\.[0-9A-Za-z]+\.js$/.test(f)) {
    rmSync(join(jsDir, f), { force: true });
  }
}

const cssSource = join(publicDir, "assets", "css");
const bulmaHash = shortHash(join(cssSource, "bulma.css"));
const bulmaStepsHash = shortHash(join(cssSource, "bulma-steps.css"));
const bulmaOut = `bulma.${bulmaHash}.css`;
const bulmaStepsOut = `bulma-steps.${bulmaStepsHash}.css`;

rmSync(distDir, { recursive: true, force: true });
cpSync(publicDir, distDir, { recursive: true, dereference: true });

const distCss = join(distDir, "assets", "css");
const distJs = join(distDir, "assets", "js");

renameSync(join(distCss, "bulma.css"), join(distCss, bulmaOut));
renameSync(join(distCss, "bulma-steps.css"), join(distCss, bulmaStepsOut));
for (const entry of readdirSync(distJs)) {
  if (entry !== mainJs) rmSync(join(distJs, entry), { recursive: true, force: true });
}

const indexPath = join(distDir, "index.html");
let index = readFileSync(indexPath, "utf8");
index = index.replace("assets/js/main.js", `assets/js/${mainJs}`);
index = index.replace("assets/css/bulma-steps.css", `assets/css/${bulmaStepsOut}`);
index = index.replace("assets/css/bulma.css", `assets/css/${bulmaOut}`);

// Content-hash every model under assets/models and inject the list into
// index.html (window.RIICHI_MODELS) so the UI can offer a model picker while
// the files can be served with a long cache lifetime. Models keep the order
// declared in index.html (the first one is the default); files not listed there
// are appended.
const declaredOrder = (() => {
  const match = index.match(/window\.RIICHI_MODELS = (\[[\s\S]*?\]);/);
  if (!match) return [];
  return [...match[1].matchAll(/key:\s*"([^"]+)"/g)].map((m) => m[1]);
})();

const qualifierRank = { int8: 0, int8s_conv: 0, fp16: 1, fp32: 2 };

function modelSortKey(name) {
  const match = name.match(/^riichi\.v(\d+)\.(.+)\.onnx$/);
  if (!match) return { version: 0, rank: 9, name };
  const parts = match[2].split(".");
  const qualifier = parts.length > 1 ? parts[parts.length - 1] : "fp32";
  return {
    version: Number(match[1]),
    rank: qualifierRank[qualifier] ?? 9,
    name,
  };
}

// Declared models keep the order from index.html (the first one is the default);
// any undeclared file is appended by newest version, then fastest variant.
function compareModels(a, b) {
  const ia = declaredOrder.indexOf(a);
  const ib = declaredOrder.indexOf(b);
  if (ia !== -1 && ib !== -1) return ia - ib;
  if (ia !== -1) return -1;
  if (ib !== -1) return 1;
  const ka = modelSortKey(a);
  const kb = modelSortKey(b);
  if (ka.version !== kb.version) return kb.version - ka.version;
  if (ka.rank !== kb.rank) return ka.rank - kb.rank;
  return a.localeCompare(b);
}

const modelsDir = join(distDir, "assets", "models");
const models = existsSync(modelsDir)
  ? readdirSync(modelsDir)
      .filter((f) => f.endsWith(".onnx"))
      .sort(compareModels)
      .map((name) => {
        const path = join(modelsDir, name);
        const bytes = statSync(path).size;
        const hashed = `${name.replace(/\.onnx$/, "")}.${shortHash(path)}.onnx`;
        renameSync(path, join(modelsDir, hashed));
        return { key: name, name: modelLabel(name), size: bytes, url: `assets/models/${hashed}` };
      })
  : [];

index = index.replace(
  /window\.RIICHI_MODELS = \[[\s\S]*?\];/,
  `window.RIICHI_MODELS = ${JSON.stringify(models)};`,
);

writeFileSync(indexPath, index);

const precache = [
  "./",
  "./index.html",
  "./manifest.webmanifest",
  "./assets/icons/icon.svg",
  "./assets/icons/icon.png",
  "./assets/icons/icon.webp",
  "./assets/sounds/klick4.ogg",
  `./assets/js/${mainJs}`,
  `./assets/css/${bulmaOut}`,
  `./assets/css/${bulmaStepsOut}`,
];

const swTemplate = readFileSync(join(root, "scripts", "sw.template.js"), "utf8");
const sw = swTemplate
  .replace("__CACHE_VERSION__", `${version}-${sha}`)
  .replace("__PRECACHE__", JSON.stringify(precache, null, 2));
writeFileSync(join(distDir, "sw.js"), sw);

console.log(`dist/ generated: ${mainJs}, ${bulmaOut}, ${bulmaStepsOut} (cache ${version}-${sha})`);
