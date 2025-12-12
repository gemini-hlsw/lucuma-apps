
alter table "public"."lucumaUserPreferences" alter column "lastOpenPrograms" set default '{}'::jsonb;

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "lastOpenPrograms" JSONB
--  not null default '{}';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "logLevel" Text
--  not null default 'error';

DELETE FROM "public"."exploreLogLevel" WHERE "id" = 'error';

DELETE FROM "public"."exploreLogLevel" WHERE "id" = 'warn';

DELETE FROM "public"."exploreLogLevel" WHERE "id" = 'trace';

DELETE FROM "public"."exploreLogLevel" WHERE "id" = 'debug';

DELETE FROM "public"."exploreLogLevel" WHERE "id" = 'info';

DROP TABLE "public"."exploreLogLevel";
