CREATE TABLE "resource_reference"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"provider" VARCHAR NOT NULL,"reference" BLOB NOT NULL,CONSTRAINT "unique_resource_reference" UNIQUE ("provider","name"));

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Declops.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.
