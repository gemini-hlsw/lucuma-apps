alter table "public"."lucumaUserPreferences" add column "spectroscopyModesTableFilters" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" add column "imagingModesTableFilters" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" add column "archiveDuplicationTableFilters" boolean
 not null default 'false';
