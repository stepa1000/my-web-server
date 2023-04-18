-- CAUTION: beam-postgres currently escapes postgres string literals somewhat
--          haphazardly when generating scripts (but not when generating commands)
--          This is due to technical limitations in libPq that require a Postgres
--          Connection in order to correctly escape strings. Please verify that the
--          generated migration script is correct before running it on your database.
--          If you feel so called, please contribute better escaping support to beam-postgres

-- Set connection encoding to UTF-8
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;

CREATE TABLE "news" 
  ( "newsName" TEXT NOT NULL UNIQUE, 
    "loginAuthor" TEXT NOT NULL UNIQUE, 
    "nameAuthor" TEXT NOT NULL, 
    "dateCreation" DATE NOT NULL, 
    "category" TEXT NOT NULL, 
    "content" TEXT NOT NULL, 
    "photo" BYTEA NOT NULL, 
    "makeNews" BOOLEAN NOT NULL, 
    PRIMARY KEY("newsName")) ;
