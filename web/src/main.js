import { Terminal } from "xterm";
import { openpty } from "xterm-pty";
import { FitAddon as Fitter } from "xterm-fit";
import Module from "impli";

const div = document.getElementById("terminal");
const terminal = new Terminal({
  cursorBlink: true,
  fontFamily: '"CommitMono", "Courier New Bold", monospace',
  fontSize: 13,
});
terminal.open(div);

const { master, slave } = openpty();
terminal.loadAddon(master);

const fitter = new Fitter();
terminal.loadAddon(fitter);
fitter.fit();
new ResizeObserver(() => fitter.fit()).observe(div);

const setup = {
  pty: slave,
};
const module = await Module(setup);

globalThis.module = module; // FIXME: don't expose module in production

// TODO: mount directory with example IMP files
