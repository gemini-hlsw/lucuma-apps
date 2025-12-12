
CREATE TABLE "public"."exploreLogLevel" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."exploreLogLevel"("id") VALUES (E'info');

INSERT INTO "public"."exploreLogLevel"("id") VALUES (E'debug');

INSERT INTO "public"."exploreLogLevel"("id") VALUES (E'trace');

INSERT INTO "public"."exploreLogLevel"("id") VALUES (E'warn');

INSERT INTO "public"."exploreLogLevel"("id") VALUES (E'error');

alter table "public"."lucumaUserPreferences" add column "logLevel" Text
 not null default 'error';

alter table "public"."lucumaUserPreferences" add column "lastOpenPrograms" JSONB
 not null default '{}';

alter table "public"."lucumaUserPreferences" alter column "lastOpenPrograms" set default '[]'::json;
