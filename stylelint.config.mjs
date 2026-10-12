import fs from 'node:fs';
import { createRequire } from 'node:module';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import postcssScss from 'postcss-scss';

const rootDir = path.dirname(fileURLToPath(import.meta.url));
const lucumaCssDir = path.join(rootDir, 'ui/lib/src/main/resources/lucuma-css');
// lucuma-prime-styles is built on arya-blue and keeps its variable names, so the stock
// theme from the app's own primereact version defines the same set.
const primeThemeCss = createRequire(path.join(rootDir, 'explore/package.json')).resolve(
  'primereact/resources/themes/arya-blue/theme.css',
);

const lucumaCustomProperties = {};
for (const file of fs.readdirSync(lucumaCssDir).filter((f) => f.endsWith('.scss'))) {
  postcssScss
    .parse(fs.readFileSync(path.join(lucumaCssDir, file), 'utf8'))
    .walkDecls(/^--/, (decl) => {
      lucumaCustomProperties[decl.prop] = {};
    });
}

/** @type {import("stylelint").Config} */
export default {
  extends: ['stylelint-config-standard-scss'],
  plugins: ['stylelint-value-no-unknown-custom-properties', 'stylelint-color-format'],
  rules: {
    'color-format/format': {
      format: 'hsla',
    },
    'csstools/value-no-unknown-custom-properties': [
      true,
      {
        importFrom: [{ customProperties: lucumaCustomProperties }, primeThemeCss],
      },
    ],
    'no-descending-specificity': null,
  },

  reportInvalidScopeDisables: true,
  reportNeedlessDisables: true,
  reportUnscopedDisables: true,
};
