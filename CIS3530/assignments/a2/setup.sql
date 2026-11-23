-- Reset and load the A2 database. Safe to re-run any time.
-- Run from this folder:  psql -d cis3530_a2 -f setup.sql
\set ON_ERROR_STOP on
SET client_min_messages = warning;

DROP TABLE IF EXISTS degree_offered, recruits_from, agents, country, degree, university CASCADE;

\i create_icru.sql

\copy university     FROM 'A2_csv_files/university.csv'     DELIMITER ',' CSV HEADER;
\copy degree         FROM 'A2_csv_files/degree.csv'         DELIMITER ',' CSV HEADER;
\copy country        FROM 'A2_csv_files/country.csv'        DELIMITER ',' CSV HEADER;
\copy agents         FROM 'A2_csv_files/agents.csv'         DELIMITER ',' CSV HEADER;
\copy recruits_from  FROM 'A2_csv_files/recruits_from.csv'  DELIMITER ',' CSV HEADER;
\copy degree_offered FROM 'A2_csv_files/degree_offered.csv' DELIMITER ',' CSV HEADER;

SELECT 'university' AS tbl, count(*) FROM university
UNION ALL SELECT 'degree',         count(*) FROM degree
UNION ALL SELECT 'country',        count(*) FROM country
UNION ALL SELECT 'agents',         count(*) FROM agents
UNION ALL SELECT 'recruits_from',  count(*) FROM recruits_from
UNION ALL SELECT 'degree_offered', count(*) FROM degree_offered;
