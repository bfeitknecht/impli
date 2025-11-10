function logger(str) {
  console.log(str);
}

function warner(str) {
  console.warn(str);
}

function exporter(content) {
  if (typeof content !== "string") {
    console.error("[FFI] exporter: function expects string argument!");
    return;
  }

  const blob = new Blob([content], { type: "text/plain" });
  const url = URL.createObjectURL(blob);

  globalThis.open(url, "_blank");
  setTimeout(() => URL.revokeObjectURL(url), 60_0000);
}
