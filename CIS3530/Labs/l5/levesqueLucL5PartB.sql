-- a
insert into student values (101, 'Maya', 'Singh', 'msingh@uoguelph.ca', 'CIS');
insert into student values (102, 'Ethan', 'Chen', 'echen@uoguelph.ca', 'SENG');
insert into student values (103, 'Sofia', 'Martin', 'smartin@uoguelph.ca', 'CIS');
insert into student values (104, 'Noah', 'Patel', 'npatel@uoguelph.ca', 'MATH');
insert into student values (105, 'Ava', 'Wilson', 'awilson@uoguelph.ca', 'CIS');

-- b
insert into club (clubID, clubName, category) values (10, 'Computing Club', 'Academic');
insert into club (clubID, clubName, category) values (20, 'Photography Club', 'Arts');
insert into club (clubID, clubName, category) values (30, 'Board Game Club', 'Recreation');

-- c
insert into membership values (101, 10, '2026-09-05', 'President');
insert into membership values (102, 10, '2026-09-06', 'Member');
insert into membership values (103, 10, '2026-09-06', 'Executive');
insert into membership values (103, 20, '2026-09-10', 'President');
insert into membership values (104, 20, '2026-09-11', 'Member');
insert into membership values (105, 30, '2026-09-12', 'President');

-- d
update club set presidentID = 101 where clubID = 10;
update club set presidentID = 103 where clubID = 20;
update club set presidentID = 105 where clubID = 30;

-- e
insert into event values (501, 10, 'Hack Night', '2026-10-15', 'Reynolds 1101', 103);
insert into event values (502, 20, 'Fall Photo Walk', '2026-10-20', 'Johnston Green', 103);

-- f
insert into registration values (101, 501, '2026-10-01', 'Registered');
insert into registration values (102, 501, '2026-10-01', 'Registered');
insert into registration values (104, 502, '2026-10-01', 'Registered');

-- g
update event
set location = 'Reynolds 1103'
where eventID = 501;

-- h
delete from registration
where studentID = 104
  and eventID = 502;
