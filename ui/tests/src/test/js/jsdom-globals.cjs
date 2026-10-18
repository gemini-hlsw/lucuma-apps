// Installs a jsdom window as the Node globals, so that tests can exercise libraries that need a
// DOM (Inputmask). Loaded with `node --require` before the test bundle, see build.sbt.
const { JSDOM } = require('jsdom');

const dom = new JSDOM('<!doctype html><html><body></body></html>', { url: 'http://localhost/' });
const win = dom.window;

const define = (name, value) =>
  Object.defineProperty(globalThis, name, { value, configurable: true, writable: true });

define('window', win);
define('document', win.document);
define('navigator', win.navigator);

for (const name of Object.getOwnPropertyNames(win)) {
  if (!(name in globalThis) && /^[A-Z]/.test(name)) define(name, win[name]);
}
define('getComputedStyle', win.getComputedStyle.bind(win));

// jsdom has no matchMedia; a stub that never matches is enough for module initialization.
const matchMedia = (query) => ({
  matches: false,
  media: query,
  onchange: null,
  addListener() {},
  removeListener() {},
  addEventListener() {},
  removeEventListener() {},
  dispatchEvent() {
    return false;
  },
});
win.matchMedia = matchMedia;
define('matchMedia', matchMedia);
