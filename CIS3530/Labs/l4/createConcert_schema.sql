-- create
DROP TABLE IF EXISTS rsvp, friend_of, events, students;

CREATE TABLE students(
  sid INT PRIMARY KEY,
  name VARCHAR(80) NOT NULL,
  year INT NOT NULL
);

CREATE TABLE friend_of(
  sid1 INT NOT NULL REFERENCES students (sid),
  sid2 INT NOT NULL REFERENCES students (sid),
  PRIMARY KEY(sid1, sid2)
);

CREATE TABLE events(
  eid INT PRIMARY KEY,
  title VARCHAR(120) NOT NULL,
  venue VARCHAR(120) NOT NULL,
  eventDate DATE NOT NULL
);

CREATE TABLE rsvp(
  sid INT NOT NULL REFERENCES students (sid),
  eid INT NOT NULL REFERENCES events (eid),
  status VARCHAR(20) NOT NULL,
  PRIMARY KEY(sid, eid)
);