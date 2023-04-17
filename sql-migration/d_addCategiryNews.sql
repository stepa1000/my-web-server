SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;

CREATE TABLE "category" 
  ( "categoryName" TEXT NOT NULL UNIQUE, 
    "parent" TEXT NOT NULL,
    "child" TEXT[],
    PRIMARY KEY("uuid")) ;
