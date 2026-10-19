
alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_theme_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "theme" text
--  not null default 'dark';

alter table "public"."lucumaTheme" rename to "lucumaThem";

DELETE FROM "public"."lucumaThem" WHERE "id" IN ('light', 'system');

DELETE FROM "public"."lucumaThem" WHERE "id" = 'dark';

DROP TABLE "public"."lucumaThem";
