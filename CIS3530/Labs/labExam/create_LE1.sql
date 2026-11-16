------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- Creating tables for the database ...

DROP TABLE IF EXISTS manuSupp;
CREATE TABLE manuSupp (
	 manuSupitemNo	VARCHAR(3)	PRIMARY KEY 
	,SNAME VARCHAR(10)
	,STATUS INTEGER
	,CITY VARCHAR(10)
);

DROP TABLE IF EXISTS items;
CREATE TABLE items (
	 itemNo	VARCHAR(3)	PRIMARY KEY 
	,INAME VARCHAR(10)
	,COLOR VARCHAR(5)
	,WEIGHT NUMERIC(5,2)
	,CITY VARCHAR(10)
);

DROP TABLE IF EXISTS proj;
CREATE TABLE proj (
	 proJNo	VARCHAR(3)	 PRIMARY KEY 
	,proJNAME VARCHAR(10)
	,CITY  VARCHAR(10)
);

DROP TABLE IF EXISTS combine;
CREATE TABLE combine (
	 manuSupitemNo	VARCHAR(3)  
	,itemNo	VARCHAR(3) 
	,projNo	VARCHAR(3) 
	,QTY  INTEGER
    , PRIMARY KEY(manuSupitemNo,itemNo,projNo)
    , FOREIGN KEY (manuSupitemNo) REFERENCES manuSupp
    , FOREIGN KEY (itemNo) REFERENCES items
    , FOREIGN KEY (proJNo) REFERENCES proj
);




