// Vite resolves some imports that node can't load. Tests only need those modules to load, so:
// - asset imports such as '/images/flags/uh.png' export their own path;
// - PrimeReact ESM builds such as 'primereact/tooltip/tooltip.esm' load their CJS build;
// - browser-only modules load as an inert stub.
//
// CAVEAT: the inert stub never throws. Every property read, call or `new` on it silently returns
// another stub, so a test that exercises one of the modules listed in `browserOnlyModules` runs
// against a do-nothing placeholder and can pass without testing anything. Such a test needs a
// real implementation (e.g. jsdom plus the actual module), not this stub.
const Module = require('module');

const assetPattern = /\.(png|svg|jpe?g|gif|webp)$/;
const primeReactEsmPattern = /^(primereact\/[^/]+)\/[^/]+\.esm$/;

// Listed one by one, not caught generically, so that a new unloadable import fails loudly.
const browserOnlyModules = new Set([
  // Vite worker import.
  '/exploreworker.js?worker',
  // Imports CSS.
  '@atlaskit/pragmatic-drag-and-drop-react-drop-indicator/list-item',
  // Ships no CommonJS build.
  'aladin-lite',
  // Reads `matchMedia` on load.
  'inputmask',
]);

const originalLoad = Module._load;

// Any property read, call or construction yields another stub. See the caveat at the top.
function inertStub() {
  return new Proxy(function () {}, {
    get: (_, key) => (key === '__esModule' || typeof key === 'symbol' ? undefined : inertStub()),
    apply: () => inertStub(),
    construct: () => inertStub(),
  });
}

Module._load = function (request, parent, isMain) {
  if (assetPattern.test(request)) return { default: request };
  if (browserOnlyModules.has(request)) return inertStub();
  const cjsRequest = request.replace(primeReactEsmPattern, '$1');
  return originalLoad.call(this, cjsRequest, parent, isMain);
};
