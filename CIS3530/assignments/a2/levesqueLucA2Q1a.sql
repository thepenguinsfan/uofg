/******************** levesqueLucA2Q1a.sql **************
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

create or replace function numberAgents(uniName varchar)
returns integer as $$
begin
  return (select count(*)
          from agents
          join university using (uid)
          where upper(name) = upper(uniName));
end;
$$ language plpgsql;
