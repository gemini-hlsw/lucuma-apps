// Vite resolves some imports that node can't load. Tests only need those modules to load, so:
// - asset imports such as '/images/flags/uh.png' export their own path;
// - PrimeReact ESM builds such as 'primereact/tooltip/tooltip.esm' load their CJS build.
const Module = require('module');

const assetPattern = /\.(png|svg|jpe?g|gif|webp)$/;
const primeReactEsmPattern = /^(primereact\/[^/]+)\/[^/]+\.esm$/;
const originalLoad = Module._load;

Module._load = function (request, parent, isMain) {
  if (assetPattern.test(request)) return { default: request };
  const cjsRequest = request.replace(primeReactEsmPattern, '$1');
  return originalLoad.call(this, cjsRequest, parent, isMain);
};
