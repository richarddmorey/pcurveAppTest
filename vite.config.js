import { defineConfig } from "vite";
import rollup from "rollup"

// https://vitejs.dev/config/
export default defineConfig({
  base: "/pcurveAppTest/",
  plugins: [],
  optimizeDeps: {
    esbuildOptions: {
      target: 'es2022'
    }
  },
  build: {
    outDir: "build",
    target: "es2022"	
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
