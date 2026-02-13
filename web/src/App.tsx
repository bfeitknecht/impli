import { useEffect, useRef } from "preact/hooks";
import { Impli } from "@/impli.ts";

export function App() {
  const terminalElementRef = useRef<HTMLDivElement>(null);
  const impliRef = useRef<Impli | null>(null);

  useEffect(() => {
    if (!terminalElementRef.current) return;

    // Initialize the Impli terminal application
    const impli = new Impli(terminalElementRef.current);
    impliRef.current = impli;

    // Start the application (Service Worker, WASM, etc.)
    impli.start();

    // Cleanup on unmount
    return () => {
      if (impliRef.current) {
        impliRef.current.dispose();
        impliRef.current = null;
      }
    };
  }, []);

  return <div ref={terminalElementRef} id="terminal" />;
}
