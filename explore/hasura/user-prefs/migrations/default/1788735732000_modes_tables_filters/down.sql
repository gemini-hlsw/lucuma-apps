alter table "public"."lucumaUserPreferences" drop column if exists "archiveDuplicationTableFilters";

alter table "public"."lucumaUserPreferences" drop column if exists "imagingModesTableFilters";

alter table "public"."lucumaUserPreferences" drop column if exists "spectroscopyModesTableFilters";
