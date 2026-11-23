-- Example calls from Assignment2.pdf. Load your solution files first.
-- Run:  psql -d cis3530_a2 -f test.sql
-- Each part runs in its own transaction and is rolled back, so the data
-- is never changed and one failing part doesn't stop the others.

\echo '--- (a) numberAgents(''Toronto'')  expect 3'
SELECT numberAgents('Toronto');

\echo '--- (b) findCommission(1)  expect 8.00'
SELECT findCommission(1) AS "Commission earned";

\echo '--- (b) findCommission(10)  expect NOTICE list then ERROR: Invalid agent id'
SELECT findCommission(10);

\echo '--- (c) generateEmail(1)  expect mahia.jain.india@uoguelph.ca'
SELECT generateEmail(1);

\echo '--- (d) update Guelph url  expect NOTICE: University of Guelph is changing its URL'
BEGIN;
UPDATE university SET url = 'https://www.uoguelph.ca/new' WHERE uid = (SELECT uid FROM university WHERE upper(name) = 'GUELPH');
ROLLBACK;

\echo '--- (e) insert agent 13  expect RITU / CHATURVEDI'
BEGIN;
INSERT INTO Agents (fname, lname, aid) VALUES ('Ritu', 'chaturvedi', 13);
SELECT fname, lname FROM agents WHERE aid = 13;
ROLLBACK;
