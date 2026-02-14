/**
 * Constants for SharedArrayBuffer communication
 */
export const CAPACITY = 4096;

/**
 * State values for the atomic lock
 */
export const IDLE = 0;
export const WAITING = 1;

/**
 * Context-aware logger for debug mode
 */
const DEBUG = true;
export function log(context: string, ...args: any[]) {
  if (DEBUG) {
    console.log(`[DEBUG] ${context}:`, ...args);
  }
}

/**
 * Dedent template literal helper
 */
export function dedent(strings: TemplateStringsArray, ...values: any[]) {
  let raw = "";
  for (let i = 0; i < strings.length; i++) {
    raw += strings[i];
    if (i < values.length) raw += values[i];
  }
  const lines = raw.split("\n");
  const indent = lines
    .filter((line) => line.trim())
    .reduce((min, line) => {
      const match = line.match(/^(\s*)/);
      return match ? Math.min(min, match[1].length) : min;
    }, Infinity);

  if (indent === Infinity) return raw;

  return lines
    .map((line) => line.slice(indent))
    .join("\n")
    .replace(/\n/g, "\r\n");
}
