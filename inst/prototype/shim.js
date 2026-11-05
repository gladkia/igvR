// igvR M2 prototype transport shim (not wired into igvR's exported API).
// Gives igvShiny.js the two Shiny calls it uses, over one WebSocket to the
// igvR httpuv server. No Shiny.outputBindings, so htmlwidgets stays in
// static (non-Shiny) mode and renders the widget from its JSON spec.
(function() {
  var handlers = {};
  var outbox = [];
  var url = window.location.href.split("#")[0].split("?")[0].replace(/^http/, "ws");
  var ws = new WebSocket(url);
  function post(frame) {
    var text = JSON.stringify(frame);
    if (ws.readyState === 1) ws.send(text); else outbox.push(text);
  }
  ws.onopen = function() {
    outbox.forEach(function(text) { ws.send(text); });
    outbox = [];
  };
  ws.onmessage = function(event) {
    var frame = JSON.parse(event.data);
    var handler = handlers[frame.type];
    if (handler) handler(frame.message);
    else console.warn("igvR shim: no handler for " + frame.type);
  };
  window.Shiny = {
    addCustomMessageHandler: function(type, handler) { handlers[type] = handler; },
    setInputValue: function(name, value, opts) { post({name: name, value: value}); }
  };
})();
