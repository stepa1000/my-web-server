SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;

ALTER TABLE "news" 
  ADD COLUMN "uuidNews" uuid;

ALTER TABLE "category" 
  Add COLUMN "uuidCategory" uuid;

