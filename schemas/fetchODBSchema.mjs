#!/usr/bin/env node
import { writeFile } from 'fs/promises';
import { buildClientSchema, getIntrospectionQuery, printSchema } from 'graphql';

//
// fetchODBSchema.mjs
//
// Usage: fetchODBSchema.mjs [local|dev|staging]
//
// Fetches the current version of the ODB schema from either a 'local' ODB
// instance running on localhost, or else from the 'dev' or 'staging' ODB.  Places it
// in the proper place in `lucuma-schemas` for use in generating code.
//

const RED = '\x1b[0;31m';
const NC = '\x1b[0m';

function usage() {
  console.error(`${RED}Usage: ${process.argv[1]} [local|dev|staging]${NC}`);
  process.exit(1);
}

let url;

// [2] is the first arg (after node and the script name)
switch (process.argv[2]) {
  case 'local':
    url = 'http://localhost:8082/odb';
    break;
  case 'dev':
    url = 'https://lucuma-postgres-odb-dev.herokuapp.com/odb';
    break;
  case 'staging':
    url = 'https://lucuma-postgres-odb-staging.herokuapp.com/odb';
    break;

  default:
    usage();
    break;
}

const response = await fetch(new URL(url), {
  headers: {
    'Content-Type': 'application/json',
  },
  method: 'POST',
  body: JSON.stringify({
    query: getIntrospectionQuery({
      specifiedByUrl: true,
      oneOf: true,
      inputValueDeprecation: true,
      directiveIsRepeatable: true,
    }),
  }),
});

if (!response.ok) {
  throw new Error(`Failed to fetch introspection query: ${response.statusText}`);
}

console.log(`Fetched ODB schema from ${url}`);

const json = await response.json();

if (json.errors) {
  throw new Error('Introspection query returned errors', { cause: json.errors });
}

const schema = printSchema(buildClientSchema(json.data));

const outputFile = 'lib/src/clue/resources/lucuma/schemas/ObservationDB.graphql';

await writeFile(outputFile, schema);
console.log(`Wrote ODB schema to ${outputFile}.`);
