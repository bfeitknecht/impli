/**
 * Adjustments to ghostty-web, which as of 0.4 neither applies theme changes
 * after `open()` nor supports OSC 8 hyperlinks: the URI lookup is a stub and
 * every hyperlink cell shares id 1.
 *
 * Cells carry the colors resolved by the VT core with the theme at creation,
 * so they are translated to the current theme while drawing. Links are
 * underlined labels instead, overlaid with real anchors so the browser treats
 * them as links: URL preview, context menu, modifier clicks.
 */
import type { ITheme, Terminal } from "ghostty-web";

/** Cell fields the renderer reads colors from. */
interface Cell {
  fg_r: number;
  fg_g: number;
  fg_b: number;
  bg_r: number;
  bg_g: number;
  bg_b: number;
}

/** Renderer internals relied on, see ghostty-web's CanvasRenderer. */
interface Renderer {
  charWidth: number;
  charHeight: number;
  lastViewportY: number;
  getCanvas(): HTMLCanvasElement;
  setTheme(theme: ITheme): void;
  renderCellBackground(cell: Cell, x: number, y: number): void;
  renderCellText(cell: Cell, x: number, y: number): void;
}

/** Renderer of opened terminal. */
const renderer = (terminal: Terminal) => (terminal as unknown as { renderer: Renderer }).renderer;

/** Packed RGB of hex color. */
const rgb = (hex: string) => parseInt(hex.replace("#", ""), 16);

/**
 * Theme for creating the terminal. The VT core reads color 0 as unset and
 * substitutes its default, so black is nudged to the next blue.
 */
export function creation(theme: ITheme): ITheme {
  const result = { ...theme };
  for (const key of Object.keys(result) as (keyof ITheme)[]) {
    if (typeof result[key] === "string" && rgb(result[key]) === 0) result[key] = "#000001";
  }
  return result;
}

/**
 * Patch renderer of opened terminal, created with `initial` theme. Returns
 * function to switch to another theme.
 */
export function adjust(terminal: Terminal, initial: ITheme) {
  const target = renderer(terminal);
  const background = target.renderCellBackground;
  const text = target.renderCellText;
  // INFO: Cells only carry these, cursor and selection may coincide with them
  const keys = (Object.keys(initial) as (keyof ITheme)[]).filter((key) => !/^(cursor|selection)/.test(key));
  let translation = new Map<number, number>();

  const translate = (cell: Cell): Cell => {
    const fg = translation.get((cell.fg_r << 16) | (cell.fg_g << 8) | cell.fg_b);
    const bg = translation.get((cell.bg_r << 16) | (cell.bg_g << 8) | cell.bg_b);
    if (fg === undefined && bg === undefined) return cell;
    const result = { ...cell };
    if (fg !== undefined) [result.fg_r, result.fg_g, result.fg_b] = [fg >> 16, (fg >> 8) & 255, fg & 255];
    if (bg !== undefined) [result.bg_r, result.bg_g, result.bg_b] = [bg >> 16, (bg >> 8) & 255, bg & 255];
    return result;
  };

  target.renderCellBackground = function (cell, x, y) {
    background.call(this, translate(cell), x, y);
  };
  target.renderCellText = function (cell, x, y) {
    text.call(this, translate(cell), x, y);
  };

  return (theme: ITheme) => {
    translation = new Map();
    for (const key of keys) {
      const from = initial[key], to = theme[key];
      if (typeof from === "string" && typeof to === "string" && from !== to) translation.set(rgb(from), rgb(to));
    }
    target.setTheme(theme);
    target.lastViewportY = NaN; // INFO: Forces a full redraw on the next frame
  };
}

/** Underlined label, linked by `link` when visible. */
export const label = (text: string) => `\x1b[4m${text}\x1b[24m`;

/**
 * Overlay visible underlined labels of the opened terminal with anchors to
 * their URI, kept in place as content, scroll position and size change.
 */
export function link(terminal: Terminal, container: HTMLElement, uris: Record<string, string>) {
  const target = renderer(terminal);
  const overlay = document.createElement("div");
  overlay.className = "links";
  overlay.contentEditable = "false"; // INFO: Links in editable content aren't followed
  container.appendChild(overlay);

  let previous = "";
  const update = () => {
    requestAnimationFrame(update);
    const buffer = terminal.buffer.active;
    const top = buffer.length - terminal.rows - Math.floor(terminal.getViewportY());
    const canvas = target.getCanvas();
    const key = [buffer.length, top, buffer.cursorX, buffer.cursorY, terminal.cols, target.charWidth].join();
    if (key === previous) return;
    previous = key;

    const anchors: HTMLAnchorElement[] = [];
    for (let row = 0; row < terminal.rows; row++) {
      const line = buffer.getLine(top + row);
      for (let x = 0; line && x < line.length; x++) {
        if (!line.getCell(x)?.isUnderline()) continue;
        const start = x;
        while (x + 1 < line.length && line.getCell(x + 1)?.isUnderline()) x++;
        const text = line.translateToString(false, start, x + 1);
        if (!(text in uris)) continue;
        const anchor = document.createElement("a");
        anchor.href = uris[text];
        anchor.target = "_blank";
        anchor.rel = "noopener";
        anchor.textContent = text;
        anchor.style.left = `${canvas.offsetLeft + start * target.charWidth}px`;
        anchor.style.top = `${canvas.offsetTop + row * target.charHeight}px`;
        anchor.style.width = `${(x + 1 - start) * target.charWidth}px`;
        anchor.style.height = `${target.charHeight}px`;
        anchor.style.lineHeight = `${target.charHeight}px`;
        // INFO: Keep keyboard focus in the terminal, the click itself is untouched
        anchor.addEventListener("mousedown", (event) => event.preventDefault());
        anchors.push(anchor);
      }
    }
    overlay.replaceChildren(...anchors);
  };
  update();
}
