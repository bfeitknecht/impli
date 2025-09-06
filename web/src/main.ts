import { Impli } from "./impli.ts";

(async () => {
  const impli = await new Impli().init();
  impli.serve();
})();
