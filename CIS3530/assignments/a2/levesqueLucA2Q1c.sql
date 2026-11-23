/******************** levesqueLucA2Q1c.sql **************
Student Name: Luc Levesque Email Id: TODO_EMAIL_ID

Due Date: November TODO Course Number: CIS 3530

I have exclusive control over this submission via my password.
By including this statement in this header comment, I certify that:

I have read and understood the University policy on academic integrity.

I assert that this work is my own. I have appropriately acknowledged any and
all material that I have used, whether directly quoted or paraphrased.
Furthermore, I certify that this assignment was prepared by me specifically for
this course.

********************************************************/

create or replace function generateEmail(agentid integer)
returns varchar as $$
declare
  email varchar;
begin
  select lower(a.fname || '.' || a.lname || '.' || c.name || '@' ||
         case upper(u.name)
           when 'BROCK' then 'brocku.ca'
           when 'TRENT' then 'trentu.ca'
           when 'WINDSOR' then 'uwindsor.ca'
           when 'SHERBROOKE' then 'usherbrooke.ca'
           when 'TORONTO' then 'utoronto.ca'
           when 'GUELPH' then 'uoguelph.ca'
         end)
  into email
  from agents a
  join country c on a.cid = c.cid
  join university u on a.uid = u.uid
  where a.aid = agentid;

  return email;
end;
$$ language plpgsql;
