DROP TABLE "public"."lucumaTableColumnPreferences";

DROP TABLE "public"."lucumaSortDirection";

CREATE TABLE "public"."lucumaTablePreferences" ("userId" text NOT NULL, "tableId" text NOT NULL, "preferences" jsonb NOT NULL, PRIMARY KEY ("userId", "tableId") , FOREIGN KEY ("tableId") REFERENCES "public"."lucumaTableIds"("id") ON UPDATE cascade ON DELETE cascade);
