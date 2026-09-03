import react from '@vitejs/plugin-react';
import type { PathLike } from 'fs';
import fs from 'fs/promises';
import path from 'path';
import Unfonts from 'unplugin-fonts/vite';
import { defineConfig, PluginOption, UserConfig } from 'vite';
import mkcert from 'vite-plugin-mkcert';
import { VitePWA } from 'vite-plugin-pwa';
import type { RuntimeCaching } from 'workbox-build';

const scalaVersion = '3.9.0';

const fontImport = Unfonts({
  fontsource: {
    families: ['Lato'],
  },
});

/**
 * Configuration to cache aladin images
 */
const imageCache = ({
  name,
  pattern,
}: {
  name: string;
  pattern: RuntimeCaching['urlPattern'];
}): RuntimeCaching => ({
  urlPattern: pattern,
  handler: 'CacheFirst',
  options: {
    cacheName: name,
    expiration: {
      purgeOnQuotaError: true,
      maxEntries: 2500,
      maxAgeSeconds: 60 * 60 * 24 * 14, // 1week
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

/**
 * Factory for StaleWhileRevalidate cache with 1-hour TTL and 1 entry limit
 */
const metadataCache = (pathEnding: string, cacheName: string): RuntimeCaching => ({
  urlPattern: new RegExp(`${pathEnding.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`),
  handler: 'StaleWhileRevalidate',
  options: {
    cacheName,
    expiration: {
      maxAgeSeconds: 60 * 60, // 1 hour
      maxEntries: 1,
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

const environmentsCache = () => metadataCache('/environments.conf.json', 'environments-config');

/**
 * Check if a file or directory exists
 */
const pathExists = async (path: PathLike) => {
  try {
    await fs.access(path, fs.constants.F_OK);
    return true;
  } catch (err) {
    return false;
  }
};

/**
 * The dev (fastopt) worker bundle is a single huge file shared by all workers
 * (ITC, AGS, Catalog, Plot, Horizons), each independently importing it at page
 * load.
 * Force `no-store` so the browser never tries to cache it.
 */
const noStoreForWorkersBundlePlugin = (): PluginOption => ({
  name: 'no-store-for-workers-bundle',
  configureServer(server) {
    // Vite's own static/@fs middleware sets Cache-Control after ours would run,
    // so patch res.setHeader for this request to force the value it lands on.
    server.middlewares.use((req, res, next) => {
      if (req.url?.includes('-fastopt/exploreworkers.js')) {
        const originalSetHeader = res.setHeader.bind(res);
        res.setHeader = ((name: string, value: unknown) =>
          name.toLowerCase() === 'cache-control'
            ? originalSetHeader(name, 'no-store')
            : originalSetHeader(name, value as never)) as typeof res.setHeader;
      }
      next();
    });
  },
});

/**
 * Vite plugin to reload the page when environment configuration changes
 */
const reloadEnvPlugin = (publicDirProd: string, publicDirDev: string): PluginOption => ({
  name: 'reload-on-environments-change',
  configureServer(server) {
    const { ws, watcher } = server;

    const sourceFiles = [
      path.resolve(publicDirProd, 'environments.conf.json'),
      path.resolve(publicDirProd, 'local.conf.json'),
    ];

    watcher.add(sourceFiles);

    watcher.on('change', async (file: string) => {
      if (sourceFiles.includes(file)) {
        // Copy the updated file to dev directory
        const localConf = path.resolve(publicDirProd, 'local.conf.json');
        const devConf = path.resolve(publicDirProd, 'environments.conf.json');

        try {
          await fs.copyFile(
            (await pathExists(localConf)) ? localConf : devConf,
            path.resolve(publicDirDev, 'environments.conf.json'),
          );
          console.log('Configuration updated, triggering reload...');
          ws.send({ type: 'full-reload' });
        } catch (error) {
          console.error('Failed to update configuration:', error);
        }
      }
    });
  },
});

// https://vitejs.dev/config/
export default defineConfig(async ({ mode }) => {
  const _dirname = import.meta.dirname;
  const scalaClassesDir = path.resolve(_dirname, `app/target/scala-${scalaVersion}`);
  const isProduction = mode === 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, `explore_app-opt`)
    : path.resolve(scalaClassesDir, `explore_app-fastopt`);
  const workersScalaClassesDir = path.resolve(_dirname, `workers/target/scala-${scalaVersion}`);
  const workersSjs = isProduction
    ? path.resolve(workersScalaClassesDir, 'explore_workers-opt')
    : path.resolve(workersScalaClassesDir, 'explore_workers-fastopt');
  const common = path.resolve(_dirname, 'common/');
  const webappCommon = path.resolve(common, 'src/main/webapp/');
  const imagesCommon = path.resolve(webappCommon, 'images');
  const publicDirProd = path.resolve(common, 'src/main/public');
  const publicDirDev = path.resolve(common, 'src/main/publicdev');
  const lucumaCss = path.resolve(_dirname, `app/target/lucuma-css`);

  if (!(await pathExists(publicDirDev))) {
    await fs.mkdir(publicDirDev);
  }
  const localConf = path.resolve(publicDirProd, 'local.conf.json');
  const devConf = path.resolve(publicDirProd, 'environments.conf.json');

  const publicDirProdFiles = (await fs.readdir(publicDirProd)).filter(
    (file) =>
      !file.endsWith('local.conf.json') &&
      !file.endsWith('environments.conf.json') &&
      !file.endsWith('README.txt'),
  );

  await Promise.all([
    fs.copyFile(
      (await pathExists(localConf)) ? localConf : devConf,
      path.resolve(publicDirDev, 'environments.conf.json'),
    ),
    ...publicDirProdFiles.map((file) =>
      fs.copyFile(path.resolve(publicDirProd, file), path.resolve(publicDirDev, file)),
    ),
  ]);

  const publicDir = mode === 'production' ? publicDirProd : publicDirDev;

  return {
    // TODO Remove this if we get EnvironmentPlugin to work.
    root: 'app/src/main/webapp',
    publicDir: publicDir,
    envPrefix: ['VITE_', 'CATS_EFFECT_', 'EXPLORE_'],
    // "process is not defined" on drag/resize start in the browser breaking resizing
    // https://github.com/react-grid-layout/react-draggable/issues/806
    define: {
      'process.env.DRAGGABLE_DEBUG': 'false',
    },
    resolve: {
      dedupe: ['react-is'],
      alias: [
        {
          find: '@sjs',
          replacement: sjs,
        },
        {
          find: '@workers',
          replacement: workersSjs,
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
      port: 8080,
      cors: { origin: '*' },
      hmr: {
        port: 8080,
        host: 'local.lucuma.xyz',
        clientPort: 8080,
        // Reduce HMR overhead
        overlay: false,
      },
      // Force HTTP/1.1: Node's http2 server truncates large responses to Firefox
      // (NS_ERROR_NET_PARTIAL_TRANSFER on multi-MB modules), breaking page load.
      // Vite ≤5 fell back to HTTP/1.1 when a proxy was configured, but Vite 8
      // always creates an http2 server, so restrict the negotiated protocol instead.
      // (ALPNCallback rather than ALPNProtocols: Node's http2 server overrides the
      // latter unconditionally.)
      https: {
        ALPNCallback: () => 'http/1.1',
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
      // https://vitejs.dev/guide/performance.html#warm-up-frequently-used-files
      warmup: {
        clientFiles: [
          path.resolve(sjs, '*.js'),
          path.resolve(webappCommon, 'sass/*.scss'),
          path.resolve(lucumaCss, '*.scss'),
        ],
      },
    },
    build: {
      emptyOutDir: true,
      chunkSizeWarningLimit: 20000,
      outDir: path.resolve(_dirname, 'heroku/static'),
      rollupOptions: {
        onwarn(warning, warn) {
          // The Scala.js bundles import Node built-ins (crypto, tls, zlib, ...)
          // from cross-compiled code whose Node paths never run in the browser.
          if (warning.message.includes('has been externalized for browser compatibility')) {
            return;
          }
          warn(warning);
        },
      },
    },
    worker: {
      format: 'es', // We need this for workers to be able to do dynamic imports.
    },
    plugins: [
      noStoreForWorkersBundlePlugin(),
      reloadEnvPlugin(publicDirProd, publicDirDev),
      mkcert({ hosts: ['localhost', 'local.lucuma.xyz', 'local.gemini.edu'] }),
      fontImport,
      VitePWA({
        injectRegister: 'inline',
        selfDestroying: false,
        workbox: {
          globPatterns: ['**/*.{js,css,html,wasm,woff2,woff,ttf,otf,csv,dat}'],
          globIgnores: ['**/uninstall.html'],
          maximumFileSizeToCacheInBytes: 30000000, // sjs produce large ffiles
          navigateFallbackDenylist: [/\/uninstall\.html$/],
          // Cache aladin images
          runtimeCaching: [
            environmentsCache(),
            imageCache({
              pattern: /^https:\/\/simbad.u-strasbg.fr\/simbad\/sim-id/,
              name: 'simbad',
            }),
            imageCache({
              pattern: /^https:\/\/alasky.u-strasbg.fr\/DSS/,
              name: 'aladin-images',
            }),
            imageCache({
              pattern: /^https:\/\/alasky.cds.unistra.fr\/DSS/,
              name: 'cds-dss',
            }),
            imageCache({
              pattern: /^https:\/\/alaskybis.cds.unistra.fr\/2MASS/,
              name: 'cds-2mass',
            }),
          ],
        },
      }),
      react(),
    ],
  } satisfies UserConfig;
});
