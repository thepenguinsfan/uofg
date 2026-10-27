-- a
select s.sid, s.name
from students s
join rsvp r on s.sid = r.sid
join events e on r.eid = e.eid
where r.status = 'going'
  and e.title = 'Winter Beats Night'
order by s.sid;

-- b
select e.eid, e.title, count(r.sid) as goingcount
from events e
join rsvp r on e.eid = r.eid
where r.status = 'going'
group by e.eid, e.title
order by e.eid;

-- c
select e.eid, e.title, coalesce(g.goingcount, 0) as goingcount
from events e
left join (
  select eid, count(*) as goingcount
  from rsvp
  where status = 'going'
  group by eid
) g on e.eid = g.eid
order by e.eid;

-- d
select s1.name as name1, s2.name as name2, e.title
from friend_of f
join friend_of f2 on f.sid1 = f2.sid2 and f.sid2 = f2.sid1
join students s1 on f.sid1 = s1.sid
join students s2 on f.sid2 = s2.sid
join rsvp r1 on r1.sid = f.sid1
join rsvp r2 on r2.sid = f.sid2 and r2.eid = r1.eid
join events e on e.eid = r1.eid
where f.sid1 < f.sid2
  and r1.status = 'going'
  and r2.status = 'going'
order by s1.name, s2.name, e.title;

-- e
select s.sid, s.name
from students s
where not exists (
  select *
  from events e
  where e.eventdate between '2026-02-01' and '2026-02-28'
    and not exists (
      select *
      from rsvp r
      where r.sid = s.sid
        and r.eid = e.eid
        and r.status = 'going'
    )
)
order by s.sid;
