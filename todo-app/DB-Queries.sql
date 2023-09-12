-- create database and add  database name in env , by default its todo-db
--  Create database todo-db;
-- Todos Table
Create Table if not exists "Todos" (
  "id" varchar Primary key not null,
  "task" varchar unique not null,
  "description" varchar ,
  "status" varchar not null,
  "active" boolean not null,
  "createdAt" timestamp with time zone not null,
  "updatedAt" timestamp with time zone not null,
  "udf" json
);

-- User Table