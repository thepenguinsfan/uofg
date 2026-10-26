-- a
SELECT s.sid, s.name
FROM students s
JOIN rsvp r ON s.sid = r.sid
JOIN events e ON r.eid = e.eid
WHERE r.status = 'going'
  AND e.title = 'Winter Beats Night'
ORDER BY s.sid;

-- b
SELECT e.eid, e.title, COUNT(r.sid) AS goingCount
FROM events e
JOIN rsvp r ON e.eid = r.eid
WHERE r.status = 'going'
GROUP BY e.eid, e.title
ORDER BY e.eid;

-- c
SELECT e.eid, e.title, COALESCE(g.goingCount, 0) AS goingCount
FROM events e
LEFT JOIN (
  SELECT eid, COUNT(*) AS goingCount
  FROM rsvp
  WHERE status = 'going'
  GROUP BY eid
) g ON e.eid = g.eid
ORDER BY e.eid;

-- d
SELECT s1.name AS name1, s2.name AS name2, e.title
FROM friend_of f
JOIN friend_of f2 ON f.sid1 = f2.sid2 AND f.sid2 = f2.sid1
JOIN students s1 ON f.sid1 = s1.sid
JOIN students s2 ON f.sid2 = s2.sid
JOIN rsvp r1 ON r1.sid = f.sid1 AND r1.status = 'going'
JOIN rsvp r2 ON r2.sid = f.sid2 AND r2.eid = r1.eid AND r2.status = 'going'
JOIN events e ON e.eid = r1.eid
WHERE f.sid1 < f.sid2
ORDER BY s1.name, s2.name, e.title;

-- e
SELECT s.sid, s.name
FROM students s
WHERE NOT EXISTS (
  SELECT e.eid
  FROM events e
  WHERE EXTRACT(YEAR FROM e.eventDate) = 2026
    AND EXTRACT(MONTH FROM e.eventDate) = 2
    AND NOT EXISTS (
      SELECT 1
      FROM rsvp r
      WHERE r.sid = s.sid
        AND r.eid = e.eid
        AND r.status = 'going'
    )
)
ORDER BY s.sid;
