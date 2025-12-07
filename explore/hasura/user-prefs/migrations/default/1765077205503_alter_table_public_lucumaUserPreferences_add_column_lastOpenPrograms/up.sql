alter table "public"."lucumaUserPreferences" add column "lastOpenPrograms" JSONB
 not null default '{}';
