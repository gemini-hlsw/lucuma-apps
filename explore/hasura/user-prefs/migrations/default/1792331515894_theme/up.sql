CREATE TABLE "public"."lucumaTheme" ("id" text NOT NULL, PRIMARY KEY ("id"), UNIQUE ("id"));

INSERT INTO "public"."lucumaTheme"("id") VALUES (E'dark'), (E'light'), (E'system');

alter table "public"."lucumaUserPreferences" add column "theme" text
 not null default 'dark';

alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_theme_fkey"
  foreign key ("theme")
  references "public"."lucumaTheme"
  ("id") on update restrict on delete restrict;
