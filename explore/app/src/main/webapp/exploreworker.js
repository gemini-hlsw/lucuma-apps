// Single entry point for all web workers. The server to run is selected via the `name`
// passed to the Worker constructor (see WorkerClients.scala). Using a single entry (and
// thus a single URL) keeps Vite from bundling a separate copy of the whole shared
// workers bundle for each worker.
// The import is dynamic for side effect only. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/import#import_a_module_for_its_side_effects_only
(async () => {
  const workers = await import('@workers/exploreworkers.js');
  const server = {
    ags: workers.AgsServer,
    catalog: workers.CatalogServer,
    horizons: workers.HorizonsServer,
    itc: workers.ItcServer,
    plot: workers.PlotServer,
  }[self.name];
  server.runWorker();
})();
