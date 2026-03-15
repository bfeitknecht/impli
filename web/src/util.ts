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
  const raw = strings.reduce(
    (acc, str, i) => {
      let val = String(values[i] ?? "");
      if (val.includes("\n")) {
        const match = (acc + str).match(/(?:^|\n)( *)$/);
        if (match) {
          val = val.split("\n").map((l, j) => j === 0 ? l : match[1] + l).join(
            "\n",
          );
        }
      }
      return acc + str + val;
    },
    "",
  );

  const lines = raw.split("\n");

  const indent = lines
    .filter((line) => line.trim().length > 0)
    .reduce((min, line) => {
      const match = line.match(/^(\s*)/);
      return match ? Math.min(min, match[1].length) : min;
    }, Infinity);

  if (indent === Infinity) return raw;

  return lines
    .map((line) => line.slice(indent))
    .join("\n");
}
