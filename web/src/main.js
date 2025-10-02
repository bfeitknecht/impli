import { Terminal } from "xterm";
import { openpty } from "pty";
import { FitAddon } from "fit";
import Module from "./impli.js";

const div = document.getElementById("terminal");
const terminal = new Terminal({
  cursorBlink: true,
  fontFamily: '"CommitMono", "Courier New Bold", monospace',
  fontSize: 13,
});
terminal.open(div);

const { master, slave } = openpty();
terminal.loadAddon(master);

const fitter = new FitAddon();
terminal.loadAddon(fitter);
fitter.fit();
new ResizeObserver(() => fitter.fit()).observe(div);

Module({ pty: slave });

// TODO: mount directory with example IMP files
