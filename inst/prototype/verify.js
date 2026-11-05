// Headless check of the M2 prototype (node >= 22: global fetch + WebSocket).
// Usage: node verify.js http://127.0.0.1:<port>/
// Fetches the page and every script, then runs shim.js + the real
// igvShiny.js in a vm with a stub DOM/igv browser, connected to the live R
// server. No real browser, no igv.js rendering.
const vm = require("vm");
const base = process.argv[2];
const fail = (m) => { console.error("FAIL: " + m); process.exit(1); };
const ok = (m) => console.log("ok   " + m);
(async () => {
  const html = await (await fetch(base)).text();
  const srcs = [...html.matchAll(/<script src="([^"]+)"/g)].map((m) => m[1]);
  ok("page scripts: " + srcs.join(", "));
  const shimAt = srcs.indexOf("shim.js");
  const bindingAt = srcs.findIndex((s) => s.endsWith("igvShiny.js"));
  if (shimAt < 0 || bindingAt < 0 || shimAt > bindingAt) fail("shim must load before igvShiny.js");
  if (!/<script type="application\/json" data-for="igvR">/.test(html)) fail("widget JSON spec missing");
  ok("shim precedes igvShiny.js; widget spec present");
  const code = {};
  for (const s of srcs) {
    const r = await fetch(new URL(s, base));
    if (r.status !== 200) fail(s + " -> " + r.status);
    code[s] = await r.text();
  }
  ok("all " + srcs.length + " scripts served (200)");

  const loaded = [];
  const el = { id: "igvR", igvBrowser: {
    trackViews: [],
    loadTrack: (config) => { loaded.push(config); return Promise.resolve(); },
    removeTrack: () => {} } };
  const window = { location: { href: base } };
  const ctx = vm.createContext({
    window, console, WebSocket, setTimeout, clearTimeout,
    document: { getElementById: (id) => (id === "igvR" ? el : null) },
    HTMLWidgets: { widget: () => {} }, igv: {} });
  window.WebSocket = WebSocket;
  ctx.location = window.location;
  vm.runInContext(code["shim.js"], ctx);
  ctx.Shiny = ctx.window.Shiny;
  vm.runInContext(code[srcs[bindingAt]], ctx);
  ok("shim + igvShiny.js evaluated; Shiny shim present: " + typeof ctx.Shiny.setInputValue);

  const deadline = Date.now() + 10000;
  while (loaded.length === 0 && Date.now() < deadline) await new Promise((r) => setTimeout(r, 50));
  if (loaded.length === 0) fail("no track reached igvBrowser.loadTrack");
  const cfg = loaded[0];
  ok("igvShiny handler called loadTrack: " + JSON.stringify(cfg));
  const bed = await fetch(cfg.url);
  const body = await bed.text();
  if (bed.status !== 200 || !/chr1\t/.test(body)) fail("bed url " + cfg.url + " -> " + bed.status);
  ok("bed file served: " + body.trim().split("\n").length + " lines");
  ctx.Shiny.setInputValue("igvReady", "igvR", { priority: "event" });
  ctx.Shiny.setInputValue("protoDone", loaded.length, { priority: "event" });
  setTimeout(() => process.exit(0), 300);
})().catch((e) => fail(e.stack));
