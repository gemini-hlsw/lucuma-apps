import react from '@vitejs/plugin-react';
import path from 'path';
import Unfonts from 'unplugin-fonts/vite';
import { defineConfig, UserConfig } from 'vite';
import mkcert from 'vite-plugin-mkcert';

const fontImport = Unfonts({
  fontsource: {
    families: ['Lato'],
  },
});

// https://vitejs.dev/config/
export default defineConfig(async ({ mode }) => {
  const scalaClassesDir = path.resolve(__dirname, 'target/scala-3.8.3-RC3');
  const isProduction = mode == 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, 'observe_web_client-opt')
    : path.resolve(scalaClassesDir, 'observe_web_client-fastopt');
  const common = __dirname;
  const webappCommon = path.resolve(common, 'src/main/webapp/');
  const imagesCommon = path.resolve(webappCommon, 'images');
  const resourceDir = path.resolve(scalaClassesDir, 'classes');
  const lucumaCss = path.resolve(__dirname, 'target/lucuma-css');

  return {
    // TODO Remove this if we get EnvironmentPlugin to work.
    root: 'src/main/webapp',
    envPrefix: ['VITE_', 'CATS_EFFECT_'],
    resolve: {
      dedupe: ['react-is'],
      alias: [
        {
          find: '@sjs',
          replacement: sjs,
        },
        {
          find: '/common',
          replacement: webappCommon,
        },
        {
          find: '/images',
          replacement: imagesCommon,
        },
        {
          find: '@resources',
          replacement: resourceDir,
        },
        {
          find: '/lucuma-css',
          replacement: lucumaCss,
        },
      ],
    },
    css: {
      transformer: 'lightningcss',
      preprocessorOptions: {
        scss: {
          charset: false,
        },
      },
      lightningcss: {
        visitor: {
          Selector(selector) {
            // Filter out :root selectors that are not the first rule
            if (selector.find((v, i) => v.type === 'pseudo-class' && v.kind === 'root' && i > 0)) {
              return selector.filter(
                (v, i) => i < 1 || !(v.type === 'pseudo-class' && v.kind === 'root'),
              );
            }
          },
        },
      },
    },
    server: {
      strictPort: true,
      fs: {
        strict: true,
      },
      host: '0.0.0.0',
      port: 8081,
      proxy: {
        '/api': {
          target: 'https://127.0.0.1:7070',
          changeOrigin: true,
          secure: false,
          ws: true,
        },
      },
      watch: {
        ignored: [
          function ignoreThisPath(_path) {
            const sjsIgnored =
              _path.includes('/target/stream') ||
              _path.includes('/zinc/') ||
              _path.includes('/classes') ||
              _path.endsWith('.tmp');
            return sjsIgnored;
          },
        ],
      },
    },
    build: {
      emptyOutDir: true,
      chunkSizeWarningLimit: 20000,
      outDir: path.resolve(__dirname, 'deploy'),
    },
    plugins: [mkcert({ hosts: ['localhost', 'local.lucuma.xyz'] }), fontImport, react()],
  } satisfies UserConfig;
});
