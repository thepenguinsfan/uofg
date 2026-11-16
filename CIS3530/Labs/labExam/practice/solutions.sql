-- CIS3530 lab exam practice: SOLUTIONS
-- Don't open until you've attempted the question. Other correct answers exist;
-- compare your output against the expected output in practice_questions.sql.

------------------------------------------------------------------------
-- PART A: LE1 (manuSupp, items, proj, combine)
------------------------------------------------------------------------

-- A1
select distinct m.sname
from manusupp m
join combine c using (manusupitemno)
join items i using (itemno)
join proj p on p.projno = c.projno
where i.color = 'RED'
  and p.city = 'LONDON'
order by m.sname;

-- A2
select distinct p.projno, p.projname
from proj p
join combine c on c.projno = p.projno
join manusupp m on m.manusupitemno = c.manusupitemno
where m.city = p.city
order by p.projno;

-- A3
select manusupitemno, sname
from manusupp
where manusupitemno not in (
    select c.manusupitemno
    from combine c
    join proj p on p.projno = c.projno
    where p.city = 'ATHENS'
)
order by manusupitemno;

-- A4
select i1.itemno as item1, i2.itemno as item2, i1.color
from items i1
join items i2 on i1.color = i2.color and i1.itemno < i2.itemno
order by i1.itemno, i2.itemno;

-- A5
select p.projno, p.projname, sum(c.qty) as totalqty,
       count(distinct c.manusupitemno) as numsuppliers
from proj p
join combine c on c.projno = p.projno
group by p.projno, p.projname
having sum(c.qty) > 1000
order by totalqty desc, p.projno;

-- A6
select m.manusupitemno, m.sname, count(l.projno) as londonshipments
from manusupp m
left join (
    select c.manusupitemno, c.projno
    from combine c
    join proj p on p.projno = c.projno
    where p.city = 'LONDON'
) l on l.manusupitemno = m.manusupitemno
group by m.manusupitemno, m.sname
order by m.manusupitemno;

-- A7
select m.sname, i.iname, p.projname, c.qty
from combine c
join manusupp m on m.manusupitemno = c.manusupitemno
join items i on i.itemno = c.itemno
join proj p on p.projno = c.projno
where c.qty = (select max(qty) from combine)
order by m.sname;

-- A8
select m.manusupitemno, m.sname
from manusupp m
where m.manusupitemno in (
    select c.manusupitemno
    from combine c join items i on i.itemno = c.itemno
    where i.color = 'RED'
)
and m.manusupitemno not in (
    select c.manusupitemno
    from combine c join items i on i.itemno = c.itemno
    where i.color = 'BLUE'
)
order by m.manusupitemno;

-- A9  (division: no item of M2's that this supplier doesn't supply)
select m.manusupitemno, m.sname
from manusupp m
where m.manusupitemno <> 'M2'
  and not exists (
    select c2.itemno
    from combine c2
    where c2.manusupitemno = 'M2'
    except
    select c.itemno
    from combine c
    where c.manusupitemno = m.manusupitemno
)
order by m.manusupitemno;

-- A10  (division: no PARIS supplier that doesn't supply this project)
select p.projno, p.projname
from proj p
where not exists (
    select *
    from manusupp m
    where m.city = 'PARIS'
      and not exists (
        select *
        from combine c
        where c.manusupitemno = m.manusupitemno
          and c.projno = p.projno
    )
)
order by p.projno;

-- A11
select manusupitemno, sum(qty) as totalqty
from combine
group by manusupitemno
having sum(qty) > (
    select avg(t.total)
    from (select sum(qty) as total from combine group by manusupitemno) t
)
order by manusupitemno;

------------------------------------------------------------------------
-- PART B: LE2 (Movie, Reviewer, Rating)
------------------------------------------------------------------------

-- B1
select distinct r.name
from reviewer r
join rating ra on ra.rid = r.rid
join movie m on m.mid = ra.mid
where m.title = 'Gone with the Wind'
order by r.name;

-- B2
select title
from movie
where mid not in (
    select ra.mid
    from rating ra
    join reviewer r on r.rid = ra.rid
    where r.name = 'Sarah Martinez'
       or r.name = 'Michael Walsh'
)
order by title;

-- B3
select distinct r.name
from reviewer r
join rating ra on ra.rid = r.rid
where ra.ratingdate is null
order by r.name;

-- B4
select m.title, count(*) as numratings, round(avg(ra.stars), 2) as avgstars
from movie m
join rating ra on ra.mid = m.mid
group by m.mid, m.title
having count(*) >= 3
order by avgstars desc, m.title;

-- B5
select director, count(*) as nummovies
from movie
where director is not null
group by director
having count(*) > 1
order by director;

-- B6
select r.name, m.title
from rating r1
join rating r2 on r1.rid = r2.rid and r1.mid = r2.mid
join reviewer r on r.rid = r1.rid
join movie m on m.mid = r1.mid
where r1.ratingdate < r2.ratingdate
  and r1.stars < r2.stars;

-- B7
select distinct r1.name as reviewer1, r2.name as reviewer2
from rating a
join rating b on a.mid = b.mid
join reviewer r1 on r1.rid = a.rid
join reviewer r2 on r2.rid = b.rid
where r1.name < r2.name
order by reviewer1, reviewer2;

-- B8
select m.title, max(ra.stars) - min(ra.stars) as spread
from movie m
join rating ra on ra.mid = m.mid
group by m.mid, m.title
having max(ra.stars) - min(ra.stars) = (
    select max(t.spread)
    from (select max(stars) - min(stars) as spread from rating group by mid) t
)
order by m.title;

-- B9
select r.rid, r.name, count(ra.mid) as lowratings
from reviewer r
left join rating ra on ra.rid = r.rid and ra.stars <= 2
group by r.rid, r.name
order by r.rid;

-- B10  (division)
select r.name
from reviewer r
where not exists (
    select *
    from movie m
    where m.director = 'James Cameron'
      and not exists (
        select *
        from rating ra
        where ra.rid = r.rid
          and ra.mid = m.mid
    )
)
order by r.name;

-- B11
select m.title, round(avg(ra.stars), 2) as avgstars
from movie m
join rating ra on ra.mid = m.mid
group by m.mid, m.title
having avg(ra.stars) <= all (
    select avg(stars) from rating group by mid
)
order by m.title;
