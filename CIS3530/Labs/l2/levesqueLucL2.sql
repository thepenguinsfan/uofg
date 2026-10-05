-- Suppliers of at least one red part, using a Cartesian product
SELECT DISTINCT S.SNAME
FROM S, SP, P
WHERE S.SNO = SP.SNO
  AND SP.PNO = P.PNO
  AND P.COLOR = 'RED'
ORDER BY S.SNAME ASC;

-- Suppliers of at least one red part, using joins
SELECT DISTINCT S.SNAME
FROM S
JOIN SP ON S.SNO = SP.SNO
JOIN P ON SP.PNO = P.PNO
WHERE P.COLOR = 'RED'
ORDER BY S.SNAME ASC;

-- Pairs of supplier numbers for suppliers in a city containing N
SELECT S1.SNO AS "SuppNo#1", S2.SNO AS "SuppNo#2"
FROM S S1, S S2
WHERE S1.CITY LIKE '%N%'
  AND S2.CITY LIKE '%N%'
  AND S1.SNO < S2.SNO
ORDER BY S1.SNO ASC, S2.SNO ASC;

-- Supplier names for suppliers who supply at least one part supplied by S2
SELECT S.SNAME
FROM S
WHERE S.SNO IN (
    SELECT SP.SNO
    FROM SP
    WHERE SP.PNO IN (
        SELECT SP.PNO
        FROM SP
        WHERE SP.SNO = 'S2'
    )
)
ORDER BY S.SNAME ASC;

-- Suppliers who do not supply any red parts
SELECT S.SNO, S.SNAME
FROM S
WHERE S.SNO NOT IN (
    SELECT SP.SNO
    FROM SP
    WHERE SP.PNO IN (
        SELECT P.PNO
        FROM P
        WHERE P.COLOR = 'RED'
    )
)
ORDER BY S.SNO ASC;

-- Suppliers who do not supply any red parts, using EXCEPT
SELECT S.SNO, S.SNAME
FROM S
EXCEPT
SELECT S.SNO, S.SNAME
FROM S, SP, P
WHERE S.SNO = SP.SNO
  AND SP.PNO = P.PNO
  AND P.COLOR = 'RED'
ORDER BY SNO ASC;

-- Suppliers who supply at least one red part and no green part
SELECT S.SNAME
FROM S
WHERE S.SNO IN (
    SELECT SP.SNO
    FROM SP, P
    WHERE SP.PNO = P.PNO
      AND P.COLOR = 'RED'
)
AND S.SNO NOT IN (
    SELECT SP.SNO
    FROM SP, P
    WHERE SP.PNO = P.PNO
      AND P.COLOR = 'GREEN'
)
ORDER BY S.SNAME ASC;




