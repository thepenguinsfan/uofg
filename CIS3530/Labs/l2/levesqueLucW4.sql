-- 1.1
-- Error: sno is selected next to COUNT(*) but there is no GROUP BY sno.
select sno, count(*)
from sp
group by sno;

-- 1.2
-- Error: use IN instead of = as the subquery returns more than one sno.
select sname
from s
where sno in (
    select sno
    from sp
    where pno in ('P2', 'P3')
);

-- 1.3
-- Error: no join condition, sname is not in the GROUP BY.
select sname, count(*)
from s
join sp on s.sno = sp.sno
group by s.sno, sname;

-- 1.4
-- Error: MAX can't be used in WHERE. Put MAX(qty) in a subquery.
select sname
from s
join sp on s.sno = sp.sno
where qty = (select max(qty) from sp);

-- 1.5
-- Error: aggregates can't be used in WHERE, so use HAVING after GROUP BY.
select sno
from sp
group by sno
having count(pno) > 2;

-- 1.6
-- Error: COUNT(sno) counts a supplier once per shipment, so use COUNT(DISTINCT sno).
select count(distinct sno)
from sp;

-- 1.7
-- Error: S and SP don't have the same columns, so they aren't union compatible and can't be used with INTERSECT.
select *
from s
where sno in (select sno from sp);

-- 2.1
-- Names of suppliers who do not supply P2:
-- SMITH, JONES, BLAKE, CLARK, HENRY
-- (Note: the curly quotes around 'P2' must be straight quotes to run.)

-- 2.2
-- Names of suppliers who supply P2 or P3:
-- JONES, BLAKE, ADAMS

-- 2.3
-- Names of suppliers who supply every part that S2 supplies (P3, P5):
-- JONES, ADAMS

-- 3.1
select sname
from s
where not exists (
    select sp1.pno
    from sp sp1
    join s s1 on sp1.sno = s1.sno
    where s1.city = 'PARIS'
    except
    select sp2.pno
    from sp sp2
    where sp2.sno = s.sno
);

-- 3.2
select sno
from sp
group by sno
having sum(qty) > (select avg(qty) from sp);
