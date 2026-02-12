export const DEBUG = true;

/**
 * Constants for SharedArrayBuffer communication
 */
export const DATA_BUFFER_SIZE = 4096;

/**
 * State values for the atomic lock
 */
export const STATE_IDLE = 0;
export const STATE_WAITING = 1;

/**
 * Context-aware logger for debug mode
 * @param context The component or module name (e.g., "Impli", "Worker", "SW")
 * @param args Additional arguments to log
 */
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
