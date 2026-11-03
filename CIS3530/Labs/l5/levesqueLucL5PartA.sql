drop table if exists registration, event, membership, club, student cascade;

create table student(
  studentID integer primary key,
  firstName varchar(30) not null,
  lastName varchar(30) not null,
  email varchar(50) not null unique,
  program varchar(20)
);

create table club(
  clubID integer primary key,
  clubName varchar(50) not null unique,
  category varchar(30) not null,
  presidentID integer
);

create table membership(
  studentID integer references student (studentID),
  clubID integer references club (clubID),
  joinDate date not null,
  role varchar(20) check (role in ('Member', 'Executive', 'President')),
  primary key (studentID, clubID)
);

-- club and membership reference each other, so this fk is added once membership exists
alter table club
  add constraint fk_club_president
  foreign key (presidentID, clubID) references membership (studentID, clubID);

create table event(
  eventID integer primary key,
  clubID integer references club (clubID),
  eventName varchar(80) not null,
  eventDate date not null,
  location varchar(80) not null,
  organizerID integer references student (studentID)
);

create table registration(
  studentID integer references student (studentID),
  eventID integer references event (eventID),
  registrationDate date not null,
  status varchar(20) check (status in ('Registered', 'Attended', 'Cancelled')),
  primary key (studentID, eventID)
);
