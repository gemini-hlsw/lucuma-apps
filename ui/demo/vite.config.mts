import react from '@vitejs/plugin-react';
import path from 'path';
import { defineConfig, type UserConfig } from 'vite';

const scalaVersion = '3.8.3';

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const sjs =
    mode == 'production'
      ? path.resolve(__dirname, `target/scala-${scalaVersion}/ui_demo-opt/`)
      : path.resolve(__dirname, `target/scala-${scalaVersion}/ui_demo-fastopt/`);
  return {
    root: 'src/main/webapp',
    resolve: {
      alias: [
        {
          find: '@sjs',
          replacement: sjs,
        },
        {
          find: '@lucuma-css',
          replacement: path.resolve(__dirname, 'target/lucuma-css/'),
        },
      ],
    },
    server: {
      host: '0.0.0.0',
      allowedHosts: ['local.lucuma.xyz'],
      watch: {
        ignored: [
          function ignoreThisPath(_path) {
            const sjsIgnored =
              _path.includes('/target/stream') ||
              _path.includes('/zinc/') ||
              _path.includes('/classes');
            return sjsIgnored;
          },
        ],
      },
    },
    build: {
      outDir: path.resolve(__dirname, '../docs'),
    },
    plugins: [react()],
  } satisfies UserConfig;
});
