-- CIS3530 lab exam practice: SOLUTIONS
-- Don't open until you've attempted the question. Other correct answers exist;
-- compare your output against the expected output in practice_questions.sql.
-- Every query uses only what is in the Week 3 / Week 4 SQL slides.
--
-- Joins use JOIN ... USING (column) wherever both tables share the column name.
-- Aliases (like i1, i2) only appear where they are required: when the same
-- table is joined to itself, or to name a subquery in the FROM clause.

------------------------------------------------------------------------
-- PART A: LE1 (manuSupp, items, proj, combine)
------------------------------------------------------------------------

-- A1
select distinct sname
from manusupp
join combine using (manusupitemno)
join items using (itemno)
join proj using (projno)
where color = 'RED'
  and proj.city = 'LONDON'
order by sname;

-- A2  (city is in both tables, so write the table name in front of it)
select distinct projno, projname
from proj
join combine using (projno)
join manusupp using (manusupitemno)
where manusupp.city = proj.city
order by projno;

-- A3
select manusupitemno, sname
from manusupp
where manusupitemno not in (
    select manusupitemno
    from combine
    join proj using (projno)
    where city = 'ATHENS'
)
order by manusupitemno;

-- A4  (self-join: items twice, so the two copies need names i1 and i2)
select i1.itemno, i2.itemno, i1.color
from items i1
join items i2 on i1.color = i2.color
where i1.itemno < i2.itemno
order by i1.itemno, i2.itemno;

-- A5
select projno, projname, sum(qty) as totalqty,
       count(distinct manusupitemno) as numsuppliers
from proj
join combine using (projno)
group by projno, projname
having sum(qty) > 1000
order by totalqty desc, projno;

-- A6  (left joins keep every supplier, even those with no London shipments.
--      The LONDON test has to sit in the ON, so this last join can't be USING.
--      count(proj.projno) skips the rows where no London project matched.)
select manusupitemno, sname, count(proj.projno) as londonshipments
from manusupp
left join combine using (manusupitemno)
left join proj on proj.projno = combine.projno and proj.city = 'LONDON'
group by manusupitemno, sname
order by manusupitemno;

-- A7
select sname, iname, projname, qty
from combine
join manusupp using (manusupitemno)
join items using (itemno)
join proj using (projno)
where qty = (select max(qty) from combine)
order by sname;

-- A8
select manusupitemno, sname
from manusupp
where manusupitemno in (
    select manusupitemno
    from combine
    join items using (itemno)
    where color = 'RED'
)
and manusupitemno not in (
    select manusupitemno
    from combine
    join items using (itemno)
    where color = 'BLUE'
)
order by manusupitemno;

-- A9  (division, count method: of M2's items, they ship as many as M2 does)
select manusupitemno, sname
from manusupp
where manusupitemno <> 'M2'
  and manusupitemno in (
    select manusupitemno
    from combine
    where itemno in (select itemno from combine where manusupitemno = 'M2')
    group by manusupitemno
    having count(distinct itemno) = (
        select count(distinct itemno) from combine where manusupitemno = 'M2'
    )
);

-- A10  (division, count method: number of PARIS suppliers shipping to the
--       project equals the number of PARIS suppliers)
select projno, projname
from proj
where projno in (
    select projno
    from combine
    join manusupp using (manusupitemno)
    where city = 'PARIS'
    group by projno
    having count(distinct manusupitemno) = (
        select count(*) from manusupp where city = 'PARIS'
    )
)
order by projno;

-- A11  (average total per supplier = overall total / number of suppliers)
select manusupitemno, sum(qty) as totalqty
from combine
group by manusupitemno
having sum(qty) > (
    select sum(qty) / count(distinct manusupitemno) from combine
)
order by manusupitemno;

------------------------------------------------------------------------
-- PART B: LE2 (Movie, Reviewer, Rating)
------------------------------------------------------------------------

-- B1
select distinct name
from reviewer
join rating using (rid)
join movie using (mid)
where title = 'Gone with the Wind'
order by name;

-- B2
select title
from movie
where mid not in (
    select mid
    from rating
    join reviewer using (rid)
    where name in ('Sarah Martinez', 'Michael Walsh')
)
order by title;

-- B3
select distinct name
from reviewer
join rating using (rid)
where ratingdate is null
order by name;

-- B4
select title, count(*) as numratings, round(avg(stars), 2) as avgstars
from movie
join rating using (mid)
group by title
having count(*) >= 3
order by avgstars desc, title;

-- B5
select director, count(*) as nummovies
from movie
where director is not null
group by director
having count(*) > 1
order by director;

-- B6  (self-join: rating twice, r1 = earlier rating, r2 = later rating.
--      USING (rid, mid) matches the same reviewer and the same movie.)
select name, title
from rating r1
join rating r2 using (rid, mid)
join reviewer using (rid)
join movie using (mid)
where r1.ratingdate < r2.ratingdate
  and r1.stars < r2.stars;

-- B7  (self-join: two ratings of the same movie, each with its own reviewer,
--      so every copy needs a name)
select distinct rev1.name, rev2.name
from rating a
join rating b using (mid)
join reviewer rev1 on rev1.rid = a.rid
join reviewer rev2 on rev2.rid = b.rid
where rev1.name < rev2.name
order by rev1.name, rev2.name;

-- B8  (inner query: every movie's spread; outer keeps the movies matching the max.
--      A subquery in FROM must be given a name, here s.)
select title, max(stars) - min(stars) as spread
from movie
join rating using (mid)
group by title
having max(stars) - min(stars) = (
    select max(spread)
    from (select max(stars) - min(stars) as spread
          from rating
          group by mid) s
);

-- B9  (the stars test goes in the ON, not WHERE, so reviewers with none stay.
--      That extra condition is why this join can't be USING.)
select reviewer.rid, name, count(rating.mid) as lowratings
from reviewer
left join rating on rating.rid = reviewer.rid and stars <= 2
group by reviewer.rid, name
order by reviewer.rid;

-- B10  (division, count method)
select name
from reviewer
where rid in (
    select rid
    from rating
    join movie using (mid)
    where director = 'James Cameron'
    group by rid
    having count(distinct mid) = (
        select count(*) from movie where director = 'James Cameron'
    )
);

-- B11  (same shape as B8, with the lowest average)
select title, round(avg(stars), 2) as avgstars
from movie
join rating using (mid)
group by title
having avg(stars) = (
    select min(avgstars)
    from (select avg(stars) as avgstars
          from rating
          group by mid) a
);
