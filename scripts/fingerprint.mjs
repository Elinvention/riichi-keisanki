import { createHash } from "node:crypto";
import {
  cpSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
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

function findMainJs() {
  const hashed = readdirSync(jsDir).filter((f) => /^main\.[0-9A-Za-z]+\.js$/.test(f));
  if (hashed.length === 0) {
    throw new Error(
      `no hashed main.<hash>.js found in ${jsDir}; run "shadow-cljs release app" first`,
    );
  }
  if (hashed.length > 1) {
    throw new Error(`multiple hashed main bundles found: ${hashed.join(", ")}`);
  }
  return hashed[0];
}

const mainJs = findMainJs();

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
