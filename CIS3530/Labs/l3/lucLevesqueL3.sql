-- CIS 3530 Lab 3
-- Luc Levesque

-- a. Find all CIS courses offered in 2016. Display them in order of course number, term.
SELECT DISTINCT o.cNum, o.term, c.name
FROM Offering o
JOIN Course c ON o.cNum = c.cNum AND o.dept = c.dept
WHERE o.dept = 'CIS' AND o.year = 2016
ORDER BY o.cNum, o.term;

-- b. Find all courses offered by Ritu. Display them in order of course number.
SELECT DISTINCT c.cNum, c.name
FROM Course c
JOIN Offering o ON o.cNum = c.cNum AND o.dept = c.dept
WHERE o.instructor = 'Ritu'
ORDER BY c.cNum;

-- c. Find all students who do not take course CIS3530.
SELECT *
FROM Student s
WHERE s.sID NOT IN (
    SELECT t.sID
    FROM Took t
    JOIN Offering o ON t.oID = o.oID
    WHERE o.dept = 'CIS' AND o.cNum = 3530
);

-- d. Find all courses that have the same number but are offered by different departments.
SELECT c1.cNum, c1.dept, c2.cNum, c2.dept
FROM Course c1
JOIN Course c2 ON c1.cNum = c2.cNum AND c1.dept < c2.dept
ORDER BY c1.cNum;

-- e. Find all courses for the term F 2017 and the current enrollment.
--    Display them in order of course number.
SELECT c.cNum, c.name, COUNT(*) AS enrollment
FROM Course c
JOIN Offering o ON o.cNum = c.cNum AND o.dept = c.dept
JOIN Took t ON t.oID = o.oID
WHERE o.year = 2017 AND o.term = 'F'
GROUP BY c.cNum, c.dept, c.name
ORDER BY c.cNum, c.name;

-- f. Find all students who have taken CIS3530 multiple times.
--    Display them in order of lastName, firstName.
SELECT s.sID, s.lastName, s.firstName, COUNT(*) AS timestaken
FROM Student s
JOIN Took t ON t.sID = s.sID
JOIN Offering o ON t.oID = o.oID
WHERE o.dept = 'CIS' AND o.cNum = 3530
GROUP BY s.sID, s.lastName, s.firstName
HAVING COUNT(*) > 1
ORDER BY s.lastName, s.firstName;
