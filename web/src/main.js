import { Impli } from "./impli.js";

(async () => {
  const impli = await new Impli().init();
  globalThis.impli = impli;
  impli.serve();
})();
