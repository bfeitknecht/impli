import { WASI, PreopenDirectory, ConsoleStdout } from "shim";
import { Terminal } from "xterm";
import { openpty } from "pty";
import { FitAddon } from "fit";
import { Module } from "./module.js";

const div = document.getElementById("terminal");
const terminal = new Terminal();
terminal.open(div);

const { master, slave } = openpty();
terminal.loadAddon(master);

return new Module({ pty: slave });
