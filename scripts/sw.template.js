// Generated at build time by scripts/fingerprint.mjs. Do not edit dist/sw.js.
const VERSION = "__CACHE_VERSION__";
const PRECACHE = "riichi-precache-" + VERSION;
const RUNTIME = "riichi-runtime-" + VERSION;
const PRECACHE_URLS = __PRECACHE__;

const SCOPE = new URL("./", self.location).pathname;

self.addEventListener("install", (event) => {
  event.waitUntil(
    (async () => {
      const cache = await caches.open(PRECACHE);
      await cache.addAll(PRECACHE_URLS);
      await self.skipWaiting();
    })(),
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    (async () => {
      const keys = await caches.keys();
      await Promise.all(
        keys
          .filter((key) => key !== PRECACHE && key !== RUNTIME)
          .map((key) => caches.delete(key)),
      );
      await self.clients.claim();
    })(),
  );
});

async function cacheFirst(cacheName, request) {
  const cache = await caches.open(cacheName);
  const cached = await cache.match(request);
  if (cached) return cached;
  const response = await fetch(request);
  if (response && response.ok) cache.put(request, response.clone());
  return response;
}

async function networkFirst(request) {
  const cache = await caches.open(PRECACHE);
  try {
    const response = await fetch(request);
    if (response && response.ok) cache.put(request, response.clone());
    return response;
  } catch (err) {
    const cached = (await cache.match(request)) || (await cache.match("./index.html"));
    if (cached) return cached;
    throw err;
  }
}

self.addEventListener("fetch", (event) => {
  const request = event.request;
  if (request.method !== "GET") return;

  const url = new URL(request.url);
  if (url.origin !== self.location.origin) return;
  if (!url.pathname.startsWith(SCOPE)) return;

  const path = url.pathname;

  if (request.mode === "navigate" || path === SCOPE || path.endsWith("/index.html")) {
    event.respondWith(networkFirst(request));
    return;
  }

  if (
    /\/assets\/js\/main\.[^/]+\.js$/.test(path) ||
    /\/assets\/css\/[^/]+\.[0-9a-f]{8,}\.css$/.test(path)
  ) {
    event.respondWith(cacheFirst(PRECACHE, request));
    return;
  }

  if (path.includes("/assets/tiles/")) {
    event.respondWith(cacheFirst(RUNTIME, request));
  }
});
