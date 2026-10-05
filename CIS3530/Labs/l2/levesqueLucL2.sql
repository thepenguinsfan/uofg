-- Suppliers of at least one red part, using a Cartesian product
select distinct s.sname from s, sp, p
where s.sno = sp.sno
  and sp.pno = p.pno
  and p.color = 'RED'
order by s.sname;

-- Suppliers of at least one red part, using joins
select distinct sname from s
join sp using (sno)
join p using (pno)
where color = 'RED'
order by sname;

-- Pairs of supplier numbers for suppliers in a city containing N
select s1.sno as "SuppNo#1", s2.sno as "SuppNo#2"
from s s1, s s2
where s1.city like '%N%'
  and s2.city like '%N%'
  and s1.sno < s2.sno
order by s1.sno, s2.sno;

-- Supplier names for suppliers who supply at least one part supplied by S2
select sname from s
where sno in (
    select sno
    from sp
    where pno in (
        select pno
        from sp
        where sno = 'S2'
    )
)
order by sname;

-- Suppliers who do not supply any red parts
select sno, sname from s
where sno not in (
    select sno
    from sp
    join p using (pno)
    where color = 'RED'
)
order by sno;

-- Suppliers who do not supply any red parts, using EXCEPT
select sno, sname from s
except select sno, sname
from s
join sp using (sno)
join p using (pno)
where color = 'RED'
order by sno;

-- Suppliers who supply at least one red part and no green part
select sname from s
where sno in (
    select sno
    from sp
    join p using (pno)
    where color = 'RED'
)
and sno not in (
    select sno
    from sp
    join p using (pno)
    where color = 'GREEN'
)
order by sname;
