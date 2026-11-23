/******************** levesqueLucA2Q1e.sql **************
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

create or replace function uppercase_names()
returns trigger as $$
begin
  new.fname := upper(new.fname);
  new.lname := upper(new.lname);
  return new;
end;
$$ language plpgsql;

create or replace trigger ensure_case
before insert or update of fname, lname on agents
for each row
execute function uppercase_names();
