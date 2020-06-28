CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE CloudCover AS ENUM (
  'Percent50',
  'Percent70',
  'Percent80',
  'Any'
);

CREATE TYPE ImageQuality AS ENUM (
  'Percent20',
  'Percent70',
  'Percent85',
  'Any'
);

CREATE TYPE SkyBackground AS ENUM (
  'Percent20',
  'Percent50',
  'Percent80',
  'Any'
);

CREATE TYPE WaterVapor AS ENUM (
  'Percent20',
  'Percent50',
  'Percent80',
  'Any'
);

CREATE TYPE TargetObjectType AS ENUM (
  'Sidereal',
  'NonSidereal'
);

CREATE TYPE ObsStatus AS ENUM (
  'New',
  'Included',
  'Proposed',
  'Approved',
  'ForReview',
  'Ready',
  'Ongoing',
  'Observed'
);

CREATE TABLE constraints (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  name VARCHAR NOT NULL,
  cloud_cover CloudCover DEFAULT 'Any' NOT NULL,
  image_quality ImageQuality DEFAULT 'Any' NOT NULL,
  sky_background SkyBackground DEFAULT 'Any' NOT NULL,
  water_vapor WaterVapor DEFAULT 'Any' NOT NULL
);

CREATE TABLE targets (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  object_type TargetObjectType NOT NULL,
  ra VARCHAR NOT NULL,
  dec VARCHAR NOT NULL,
  name VARCHAR NOT NULL
);

CREATE TABLE observations (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  target_id UUID REFERENCES targets(id),
  constraints_id UUID REFERENCES constraints(id),
  status ObsStatus NOT NULL,
  configuration VARCHAR NOT NULL,
  duration_seconds INT NOT NULL
);

INSERT INTO constraints VALUES(uuid('608c8407-63a5-4d26-970c-587486af57da'), 'Testing', 'Any', 'Any', 'Any', 'Any');
INSERT INTO constraints VALUES(uuid('182926f3-998f-4a61-9df4-0bd318f7b677'), '<0.8" <0.3 mag Gray', 'Any', 'Any', 'Any', 'Any');
INSERT INTO constraints VALUES(uuid('5d03a604-90d8-4791-b7e8-1b4d084e8889'), '<0.7" <0.3 mag Bright', 'Any', 'Any', 'Any', 'Any');

INSERT INTO targets VALUES(uuid('9be5789c-3ffe-48cd-8e8e-24fe3e4067ee'), 'Sidereal', '09:55:33.173061', '+69:03:55.060919', 'M81');
INSERT INTO targets VALUES(uuid('b9acf8b4-79e9-4c69-9a96-904746e127ab'), 'Sidereal', '02:41:45.232999', '+00:26:35.450016', 'NGC 1055');
INSERT INTO targets VALUES(uuid('165cc9d7-0430-46a7-bebd-377bad83c184'), 'Sidereal', '23:46:58.557000', '+29:27:32.169996', 'NGC 7752');
INSERT INTO targets VALUES(uuid('6d38c349-5189-4953-b337-ca8d72b61f2c'), 'Sidereal', '11:30:07.456000', '+09:16:35.870015', 'NGC 3705');
INSERT INTO targets VALUES(uuid('6830dbd3-f530-48c0-b36e-722efd36277f'), 'Sidereal', '02:42:40.771000', '-00:00:47.840004', 'NGC 1068');
INSERT INTO targets VALUES(uuid('68f56259-c09d-4553-b6bc-d999205aeb59'), 'Sidereal', '02:46:25.154457', '-00:29:55.449960', 'NGC 1087');

INSERT INTO observations VALUES(uuid('6c011b18-091a-431d-b48d-1ba14ea86a0c'), uuid('b9acf8b4-79e9-4c69-9a96-904746e127ab'), uuid('182926f3-998f-4a61-9df4-0bd318f7b677'), 'New', 'GMOS-N R831 1x300"', 7200);
INSERT INTO observations VALUES(uuid('54a32730-2849-4173-92ed-11bb377ce7e7'), uuid('165cc9d7-0430-46a7-bebd-377bad83c184'), uuid('182926f3-998f-4a61-9df4-0bd318f7b677'), 'New', 'GMOS-N R831 1x300"', 4920);
INSERT INTO observations VALUES(uuid('e892547a-8a9c-4fed-b676-cbb1d6a0241d'), uuid('6830dbd3-f530-48c0-b36e-722efd36277f'), uuid('182926f3-998f-4a61-9df4-0bd318f7b677'), 'New', 'GMOS-N R831 1x300"', 6300);
INSERT INTO observations VALUES(uuid('b320d288-b26d-4893-b2ca-4e57eca182e7'), uuid('6830dbd3-f530-48c0-b36e-722efd36277f'), uuid('5d03a604-90d8-4791-b7e8-1b4d084e8889'), 'New', 'GMOS-N R831 1x300"', 5520);

