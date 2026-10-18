alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_theme_fkey";

alter table "public"."lucumaUserPreferences" drop column "theme";

DROP TABLE "public"."lucumaTheme";
