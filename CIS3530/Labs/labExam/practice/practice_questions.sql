-- CIS3530 LAB EXAM PRACTICE
-- Write your query under each question, then compare with the expected output.
-- Answers are in solutions.sql.
--
-- Setup, following Tutorial 1 (off campus: run the VPN first):
--   ssh linux.socs.uoguelph.ca
--   cd 3530                        (the folder holding the labExam scripts)
--   psql -h db
-- Then inside psql:
--   \i create_LE1.sql
--   \i insert_LE1.sql
--   \i create_and_insert_LE2.sql
--   \d+                            (should list manusupp, items, proj, combine,
--                                   movie, reviewer, rating)
--   select * from combine;         (verify: 24 rows)
--   select * from rating;          (verify: 20 rows)
--   \q                             (exit)
-- Optional, to keep these tables apart from your lab tables, before the \i lines:
--   CREATE SCHEMA LE;
--   SET SEARCH_PATH TO LE;         (redo this SET each time you reconnect)
-- To run a file of your own answers:  \i myAnswers.sql
--
-- Mock exams (2 queries each, give yourself ~25 min per pair):
--   Mock 1: A1 + A5      Mock 4: B1 + B4
--   Mock 2: A3 + A9      Mock 5: B2 + B10
--   Mock 3: A6 + A10     Mock 6: B6 + B11
--
-- Difficulty: [1] joins  [2] subquery / grouping  [3] division, nested aggregates

------------------------------------------------------------------------
-- PART A: LE1
--   manuSupp(manuSupitemNo, sname, status, city)      -- the suppliers
--   items(itemNo, iname, color, weight, city)
--   proj(projNo, projname, city)
--   combine(manuSupitemNo, itemNo, projNo, qty)       -- supplier ships item to project
------------------------------------------------------------------------

-- A1 [1] Names of suppliers who ship at least one RED item to a project
--        located in LONDON. No duplicates, ordered by name.
-- Expected: CLARK



-- A2 [1] Project number and name of every project that is supplied by at least
--        one supplier located in the same city as the project. Order by projNo.
-- Expected: (J1, SORTER) (J4, CONSOLE) (J7, TAPE)



-- A3 [2] Number and name of suppliers who do not ship anything to any project
--        located in ATHENS. Order by supplier number.
-- Expected: (M3, BLAKE)



-- A4 [1] All pairs of different items that have the same colour. List each pair
--        once (smaller itemNo first) with the colour. Order by the two item numbers.
-- Expected: (I1,I4,RED) (I1,I6,RED) (I3,I5,BLUE) (I4,I6,RED)



-- A5 [2] For each project that receives more than 1000 units in total: project
--        number, name, total quantity (totalqty) and the number of different
--        suppliers shipping to it (numsuppliers). Order by totalqty descending,
--        then projNo.
-- Expected: (J4,CONSOLE,3300,3) (J2,DISPLAY,1200,3) (J7,TAPE,1200,3) (J5,RAID,1100,2)



-- A6 [2] For EVERY supplier, the number of shipments they make to projects in
--        LONDON (londonshipments). Suppliers with none must show 0.
--        Order by supplier number.
-- Expected: (M1,SMITH,0) (M2,JONES,2) (M3,BLAKE,0) (M4,CLARK,1) (M5,ADAMS,2)



-- A7 [2] The largest single shipment(s): supplier name, item name, project name
--        and quantity. Ties must all be shown. Order by supplier name.
-- Expected: (ADAMS,SCREW,CONSOLE,800) (JONES,SCREW,TAPE,800)



-- A8 [2] Number and name of suppliers who ship at least one RED item but never
--        ship a BLUE item. Order by supplier number.
-- Expected: (M1, SMITH) (M4, CLARK)



-- A9 [3] Number and name of suppliers, other than M2, who ship every item that
--        M2 ships.
-- Expected: (M5, ADAMS)



-- A10 [3] Number and name of projects that are supplied by every supplier
--         located in PARIS. Order by projNo.
-- Expected: (J1, SORTER) (J2, DISPLAY)



-- A11 [3] Suppliers whose total shipped quantity is greater than the average
--         total shipped quantity per supplier. Show supplier number and totalqty.
-- Expected: (M2, 3200) (M5, 3100)



------------------------------------------------------------------------
-- PART B: LE2
--   Movie(mID, title, year, director)
--   Reviewer(rID, name)
--   Rating(rID, mID, stars, ratingDate)
------------------------------------------------------------------------

-- B1 [1] Names of reviewers who rated 'Gone with the Wind'. No duplicates,
--        ordered by name.
-- Expected: Chris Jackson, Mike Anderson, Sarah Martinez



-- B2 [2] Titles of movies that were rated by neither Sarah Martinez nor
--        Michael Walsh. Order by title.
-- Expected: Avatar, E.T., Snow White, Star Wars, Titanic



-- B3 [1] Names of reviewers who have at least one rating with no rating date.
--        Order by name.
-- Expected: Chris Jackson, Daniel Lewis



-- B4 [2] For each movie with at least 3 ratings: title, number of ratings
--        (numratings) and average stars rounded to 2 decimals (avgstars).
--        Order by avgstars descending, then title.
-- Expected: (Snow White,3,3.67) (Avatar,4,3.50)
--           (Raiders of the Lost Ark,3,3.33) (Gone with the Wind,4,3.00)



-- B5 [2] Directors who directed more than one movie, with the number of movies
--        (nummovies). Ignore movies with no director. Order by director.
-- Expected: (James Cameron, 2) (Steven Spielberg, 2)



-- B6 [2] For every case where the same reviewer rated the same movie twice and
--        gave it more stars the second time: reviewer name and movie title.
-- Expected: (Sarah Martinez, Gone with the Wind)



-- B7 [2] All pairs of reviewers who rated the same movie. Show both names, each
--        pair once, with the alphabetically earlier name first. Order by both names.
-- Expected: 11 rows, first (Ashley White, Chris Jackson),
--           last (Mike Anderson, Sarah Martinez)



-- B8 [3] The movie(s) with the largest rating spread (highest stars minus lowest
--        stars). Show title and spread.
-- Expected: (Snow White, 3)



-- B9 [2] For EVERY reviewer, the number of ratings of 2 stars or fewer they gave
--        (lowratings). Reviewers with none must show 0. Order by rID.
-- Expected: 201:1  202:0  203:2  204:0  205:3  206:0  207:0  208:0



-- B10 [3] Names of reviewers who rated every movie directed by James Cameron.
-- Expected: Chris Jackson



-- B11 [3] The movie(s) with the lowest average rating: title and average stars
--         rounded to 2 decimals.
-- Expected: (Star Wars, 2.00)


