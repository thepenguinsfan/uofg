select sname, projNo, count(itemNo) as numberOfItems
from manuSupp
join combine using (manuSupitemNo)
group by manuSupitemNo, sname, projNo
having count(itemNo) = 2
order by sname;
