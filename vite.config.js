import { defineConfig } from "vite";
import rollup from "rollup"

// https://vitejs.dev/config/
export default defineConfig({
  base: "/pcurveAppTest/",
  plugins: [],
  optimizeDeps: {
    esbuildOptions: {
      target: 'esnext'
    }
  },
  build: {
    outDir: "build",
    target: "esnext"	
  },
  server: {
    host:"0.0.0.0",
    port:3000,
    strictPort: true,
    hmr: {
      clientPort: 443 // Run the websocket server on the SSL port
    },
    headers: {
      "Cross-Origin-Embedder-Policy": "require-corp",
  		"Cross-Origin-Opener-Policy": "same-origin",
  		"Cross-Origin-Resource-Policy": "cross-origin"
		}
  }
});
