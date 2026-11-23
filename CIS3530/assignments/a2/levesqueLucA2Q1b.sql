/******************** levesqueLucA2Q1b.sql **************
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

create or replace function findCommission(agentid integer)
returns decimal as $$
declare
  comm decimal;
  r record;
begin
  select commission into comm from agents where aid = agentid;

  if not found then
    raise notice 'Invalid agent id. Valid agent ids are:';
    for r in select aid, fname, lname from agents order by aid loop
      raise notice '%: % %', r.aid, r.fname, r.lname;
    end loop;
    raise exception 'Invalid agent id';
  end if;

  return comm;
end;
$$ language plpgsql;
