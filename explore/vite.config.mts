import react from '@vitejs/plugin-react';
import type { PathLike } from 'fs';
import fs from 'fs/promises';
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import path from 'path';
import type { PluginCreator } from 'postcss';
import Unfonts from 'unplugin-fonts/vite';
import { defineConfig, UserConfig } from 'vite';
import env from 'vite-plugin-env-compatible';
import mkcert from 'vite-plugin-mkcert';
import { VitePWA } from 'vite-plugin-pwa';
import type { RuntimeCaching } from 'workbox-build';

const scalaVersion = '3.7.4';

const fixCssRoot: PluginCreator<void> = () => {
  return {
    postcssPlugin: 'postcss-fix-nested-root',
    Once(root) {
      root.walkRules((rule) => {
        if (rule.selector.includes(' :root')) {
          rule.selector = rule.selector.replace(' :root', '');
        }
      });
    },
  };
};
fixCssRoot.postcss = true;

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

const enumMetadataCache = () => metadataCache('/export/enumMetadata', 'enum-metadata');
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
 * Vite plugin to cache enum metadata from ODB with 1-hour TTL
 */
const enumMetadataPlugin = (publicDirDev: string) => ({
  name: 'enum-metadata-cache',
  configureServer(server: any) {
    let cachedMetadata: { data: string; timestamp: number } | null = null;
    const CACHE_TTL = 60 * 60 * 1000; // 1 hour in milliseconds

    const isCacheValid = () => cachedMetadata && Date.now() - cachedMetadata.timestamp < CACHE_TTL;

    // get environments and enumMetadata and cache locally
    const fetchEnumMetadata = async (host: string) => {
      try {
        const configPath = path.resolve(publicDirDev, 'environments.conf.json');
        const configText = await fs.readFile(configPath, 'utf-8');
        const environments = JSON.parse(configText);

        const getODBRestURL = (h: string) =>
          environments.find((e: any) => e.hostName === h)?.odbRestURI ??
          environments.find((e: any) => e.hostName === '*')?.odbRestURI;

        const url = getODBRestURL(host);

        if (!url) {
          throw new Error(`No ODB URL found`);
        }

        const response = await fetch(`${url}/export/enumMetadata`);

        const module = await response.text();
        cachedMetadata = { data: module, timestamp: Date.now() };

        return module;
      } catch (error) {
        console.error('[enum-metadata-cache] Failed to fetch:', error);
        throw error;
      }
    };

    server.middlewares.use(async (req: any, res: any, next: any) => {
      if (req.url?.startsWith('/api/enumMetadata')) {
        try {
          let metadata: string;

          if (isCacheValid()) {
            metadata = cachedMetadata!.data;
          } else {
            metadata = await fetchEnumMetadata('local.lucuma.xyz');
          }

          res.setHeader('Content-Type', 'application/javascript');
          res.setHeader('Cache-Control', 'no-cache');
          res.end(metadata);
        } catch (error) {
          res.statusCode = 500;
          res.end('Failed to fetch enum metadata');
        }
      } else {
        next();
      }
    });

    server.__enumMetadataCache = {
      clear: () => {
        cachedMetadata = null;
      },
    };
  },
});

/**
 * Vite plugin to reload the page when environment configuration changes
 */
const reloadEnvPlugin = (publicDirProd: string, publicDirDev: string) => ({
  name: 'reload-on-environments-change',
  configureServer(server: any) {
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
          // Clear enum metadata cache since ODB URL might have changed
          if (server.__enumMetadataCache) {
            server.__enumMetadataCache.clear();
          }
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
  const _dirname =
    typeof __dirname !== 'undefined' ? __dirname : dirname(fileURLToPath(import.meta.url));
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
    envPrefix: ['VITE_', 'CATS_EFFECT_'],
    resolve: {
      dedupe: ['react-is'],
      alias: [
        {
          find: 'process',
          replacement: 'process/browser',
        },
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
      preprocessorOptions: {
        scss: {
          charset: false,
        },
      },
      postcss: {
        plugins: [fixCssRoot],
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
    },
    worker: {
      format: 'es', // We need this for workers to be able to do dynamic imports.
    },
    plugins: [
      env(),
      enumMetadataPlugin(publicDirDev),
      reloadEnvPlugin(publicDirProd, publicDirDev),
      mkcert({ hosts: ['localhost', 'local.lucuma.xyz', 'local.gemini.edu'] }),
      fontImport,
      VitePWA({
        injectRegister: 'inline',
        selfDestroying: false,
        workbox: {
          globPatterns: ['**/*.{js,css,html,wasm,woff2,woff,ttf,otf}'],
          globIgnores: ['**/uninstall.html'],
          maximumFileSizeToCacheInBytes: 30000000, // sjs produce large ffiles
          navigateFallbackDenylist: [/\/uninstall\.html$/],
          // Cache aladin images
          runtimeCaching: [
            enumMetadataCache(),
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
