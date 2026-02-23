/**
 * Context-aware logger for debug mode
 */
const DEBUG = false;
export function log(context: string, ...args: any[]) {
  if (DEBUG) {
    console.log(`[DEBUG] ${context}:`, ...args);
  }
}

/**
 * Dedent template literal
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
