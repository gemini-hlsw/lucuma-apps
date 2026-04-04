
alter table "public"."lucumaUserPreferences" add column "observationTableFilters" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" add column "programsTableFilters" boolean
 not null default 'false';

alter table "public"."lucumaTableColumnPreferences" add column "filter" text
 null;
