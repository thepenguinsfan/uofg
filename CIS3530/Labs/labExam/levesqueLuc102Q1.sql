select distinct itemNo
from combine
join manuSUpp using (manuSupitemNo)
join proj using (projNo)
where manuSupp.city = proj.city
order by itemNo;
