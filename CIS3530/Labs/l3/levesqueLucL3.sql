-- a
select distinct o.cnum, o.term, c.name
from offering o
join course c on o.cnum = c.cnum and o.dept = c.dept
where o.dept = 'CIS'
  and o.year = 2016
order by o.cnum, o.term;

-- b
select distinct c.cnum, c.name
from course c
join offering o on o.cnum = c.cnum and o.dept = c.dept
where o.instructor = 'Ritu'
order by c.cnum;

-- c
select *
from student s
where s.sid not in (
    select t.sid
    from took t
    join offering o on t.oid = o.oid
    where o.dept = 'CIS'
      and o.cnum = 3530
);

-- d
select c1.cnum, c1.dept, c2.cnum, c2.dept
from course c1
join course c2 on c1.cnum = c2.cnum and c1.dept < c2.dept
order by c1.cnum;

-- e
select c.cnum, c.name, count(*) as enrollment
from course c
join offering o on o.cnum = c.cnum and o.dept = c.dept
join took t on t.oid = o.oid
where o.year = 2017
  and o.term = 'F'
group by c.cnum, c.dept, c.name
order by c.cnum, c.name;

-- f
select s.sid, s.lastname, s.firstname, count(*) as timestaken
from student s
join took t on t.sid = s.sid
join offering o on t.oid = o.oid
where o.dept = 'CIS'
  and o.cnum = 3530
group by s.sid, s.lastname, s.firstname
having count(*) > 1
order by s.lastname, s.firstname;
