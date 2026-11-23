CREATE TABLE OnlineExam (
    examNo           INT,
    examTitle        VARCHAR(100),
    examTimeAllowed  INT,

    questionNo       INT,
    questionText     VARCHAR(500),
    correctAnswer    VARCHAR(10),

    optionID         VARCHAR(10),
    optionText       VARCHAR(250),

    userId           INT,
    userEmail        VARCHAR(100),
    userPassword     VARCHAR(100),
    userFname        VARCHAR(50),
    userLname        VARCHAR(50),
    userAddress      VARCHAR(150),
    userCity         VARCHAR(50),
    userState        VARCHAR(50),
    userZip          VARCHAR(20),

    startTime        TIMESTAMP,
    finishTime       TIMESTAMP,
    userResponse     VARCHAR(10),

    PRIMARY KEY (examNo, questionNo, optionID, userId)
);