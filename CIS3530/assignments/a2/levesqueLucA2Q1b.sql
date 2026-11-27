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

create or replace function findCommission(agentId integer)
returns decimal as $$
declare
  agentCommission decimal;
  agent record;
begin
  if not exists (select * from agents where aid = agentId) then
    raise notice 'Invalid agent id. Valid agent ids are:';
    for agent in select aid, fname, lname from agents order by aid loop
      raise notice '%: % %', agent.aid, agent.fname, agent.lname;
    end loop;
    raise exception 'Invalid agent id';
  end if;

  select commission into agentCommission
  from agents
  where aid = agentId;

  return agentCommission;
end;
$$ language plpgsql;
