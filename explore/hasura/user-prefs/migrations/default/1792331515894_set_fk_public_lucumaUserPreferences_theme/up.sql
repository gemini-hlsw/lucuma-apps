alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_theme_fkey"
  foreign key ("theme")
  references "public"."lucumaTheme"
  ("id") on update restrict on delete restrict;
