       ID Division.
      * 
      * Copyright (C) 2021 Craig Schneiderwent.  All rights reserved.
      * 
      * I accept no liability for damages of any kind resulting 
      * from the use of this software.  Use at your own risk.
      *
      * This software may be modified and distributed under the terms
      * of the MIT license. See the LICENSE file for details.
      *
      * This doesn't give the right answer for the full data set, but
      * does for the small data set.  I don't know why, and I guess
      * I never will.
      *
       Program-ID. cs10b.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(4096).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)      VALUE 'cs10b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  STACK-PTR          PIC 9(009) COMP    VALUE 0.
           05  STACK-PTR1         PIC 9(009) COMP    VALUE 0.
           05  STACK-MAX          PIC 9(009) COMP    VALUE 0.
           05  STACK-CURR-LVL     PIC 9(009) COMP    VALUE 0.
           05  STACK-MAX-LVL      PIC 9(009) COMP    VALUE 0.
           05  CHAR-PTR           PIC 9(009) COMP    VALUE 0.
           05  COMPLETION-PTR     PIC 9(009) COMP    VALUE 1.
           05  COMPLETION-MAX     PIC 9(009) COMP    VALUE 0.
           05  FILE-SCORE         PIC 9(009) COMP    VALUE 0.
           05  TOTAL-SCORE-MAX    PIC 9(018) COMP    VALUE 0.
           05  TOTAL-SUB          PIC 9(009) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  COMPLETION-CHAR    PIC X(001)         VALUE SPACE.
           05  CLOSE-CHAR         PIC X(001)         VALUE SPACE.
           05  THE-CHAR           PIC X(001)         VALUE SPACE.
               88  THE-CHAR-IS-OPEN                  VALUES
                                                     '(' '[' '{' '<'.
               88  THE-CHAR-IS-CLOSE                 VALUES
                                                     ')' ']' '}' '>'.
           05  COMPLETION-LIST    PIC X(100)         VALUE SPACES.
           05  TOTAL-SCORE-TBL.
               10  TOTAL-SCORE
                   OCCURS 100     PIC 9(018).

       01  WS-INPT-DATA GLOBAL.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  BAD-RECORD-SW      PIC X(001)         VALUE 'N'.
               88  BAD-RECORD                        VALUE 'Y'
                                                     FALSE 'N'.

       01  STACK-TABLE.
           05  STACK-TBL OCCURS 256.
               10  STACK          PIC X(001).
               10  STACK-LVL      PIC 9(009)  COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           INITIALIZE TOTAL-SCORE-TBL
           MOVE 0 TO TOTAL-SUB

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-PROCESS-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           SORT TOTAL-SCORE DESCENDING TOTAL-SCORE

           PERFORM VARYING TOTAL-SUB FROM 1 BY 1
           UNTIL TOTAL-SUB > TOTAL-SCORE-MAX
             DISPLAY MYNAME
             ' ' TOTAL-SUB
             ' total score ' TOTAL-SCORE(TOTAL-SUB)
           END-PERFORM

           COMPUTE TOTAL-SUB = TOTAL-SCORE-MAX / 2 + 1

           DISPLAY MYNAME ' file score      ' FILE-SCORE
           DISPLAY MYNAME ' total score     ' TOTAL-SCORE(TOTAL-SUB)
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           GOBACK.

       1000-PROCESS-INPUT.
           SET BAD-RECORD TO FALSE
           PERFORM VARYING CHAR-PTR FROM 1 BY 1
           UNTIL WS-INPT(CHAR-PTR:1) = SPACE
           OR BAD-RECORD
             MOVE WS-INPT(CHAR-PTR:1) TO THE-CHAR
             IF THE-CHAR-IS-OPEN
                 ADD 1 TO STACK-PTR
                 MOVE WS-INPT(CHAR-PTR:1) TO STACK(STACK-PTR)
             ELSE
                 IF STACK-PTR = 0
                     DISPLAY MYNAME ' stack pointer 0'
                     SET BAD-RECORD TO TRUE
                 ELSE
                     IF PROCESS-TEST
                         DISPLAY 
                           MYNAME SPACE THE-CHAR SPACE STACK(STACK-PTR)
                     END-IF
                     EVALUATE STACK(STACK-PTR) ALSO THE-CHAR
                       WHEN   '('              ALSO ')'
                       WHEN   '['              ALSO ']'
                       WHEN   '{'              ALSO '}'
                       WHEN   '<'              ALSO '>'
                              SUBTRACT 1 FROM STACK-PTR
                       WHEN OTHER
                            SET BAD-RECORD TO TRUE
                     END-EVALUATE
                 END-IF
             END-IF
           END-PERFORM

           IF BAD-RECORD
               IF PROCESS-TEST
                   DISPLAY MYNAME ' expected close for '
                     STACK(STACK-PTR) ' but found ' THE-CHAR ' instead'
               END-IF
               EVALUATE THE-CHAR
                 WHEN ')' ADD 3     TO FILE-SCORE
                 WHEN ']' ADD 57    TO FILE-SCORE
                 WHEN '}' ADD 1197  TO FILE-SCORE
                 WHEN '>' ADD 25137 TO FILE-SCORE
               END-EVALUATE
           ELSE
               PERFORM 2000-COMPLETE-THE-LINE
           END-IF

           PERFORM 8010-READ-INPT-DATA
           .

       2000-COMPLETE-THE-LINE.
           IF PROCESS-TEST
               DISPLAY MYNAME ' record ' WS-INPT(1:CHAR-PTR)
           END-IF

           INITIALIZE
               STACK-PTR
               STACK-MAX-LVL
               STACK-CURR-LVL
               COMPLETION-PTR
               COMPLETION-LIST
               COMPLETION-MAX
             ALL TO VALUE

           PERFORM VARYING CHAR-PTR FROM 1 BY 1
           UNTIL WS-INPT(CHAR-PTR:1) = SPACE
             MOVE WS-INPT(CHAR-PTR:1) TO THE-CHAR
             IF THE-CHAR-IS-OPEN
                 ADD 1 TO STACK-CURR-LVL
             END-IF
             IF STACK-CURR-LVL < 1
                 DISPLAY
                   MYNAME
                   ' logic error STACK-CURR-LVL '
                   STACK-CURR-LVL
                 DISPLAY MYNAME ' record ' WS-REC-COUNT
             END-IF
             ADD 1 TO STACK-PTR
             MOVE THE-CHAR TO STACK(STACK-PTR)
             MOVE STACK-CURR-LVL TO STACK-LVL(STACK-PTR)
             IF STACK-CURR-LVL > STACK-MAX-LVL
                 MOVE STACK-CURR-LVL TO STACK-MAX-LVL
             END-IF
             IF THE-CHAR-IS-CLOSE
                 SUBTRACT 1 FROM STACK-CURR-LVL
             END-IF
           END-PERFORM

           MOVE STACK-PTR TO STACK-MAX
           PERFORM VARYING STACK-PTR FROM 1 BY 1
           UNTIL STACK-PTR > STACK-MAX
             MOVE STACK(STACK-PTR) TO THE-CHAR
             IF THE-CHAR-IS-OPEN
                 EVALUATE STACK(STACK-PTR)
                   WHEN '(' MOVE ')' TO CLOSE-CHAR
                   WHEN '[' MOVE ']' TO CLOSE-CHAR
                   WHEN '{' MOVE '}' TO CLOSE-CHAR
                   WHEN '<' MOVE '>' TO CLOSE-CHAR
                 END-EVALUATE
                 PERFORM VARYING STACK-PTR1 FROM STACK-PTR BY 1
                 UNTIL STACK(STACK-PTR1) = SPACE
                 OR (STACK(STACK-PTR1) = CLOSE-CHAR
                 AND STACK-LVL(STACK-PTR1) = STACK-LVL(STACK-PTR))
                   CONTINUE
                 END-PERFORM
                 IF STACK(STACK-PTR1) = SPACE
                     PERFORM 2010-ADD-TO-COMPLETION-LIST
                 END-IF
             END-IF
           END-PERFORM

           IF PROCESS-TEST
               DISPLAY MYNAME ' completion list ' COMPLETION-LIST
           END-IF

           ADD 1 TO TOTAL-SUB
           COMPUTE COMPLETION-MAX = COMPLETION-PTR - 1
           PERFORM VARYING COMPLETION-PTR FROM COMPLETION-MAX BY -1
           UNTIL COMPLETION-PTR = 0
             MULTIPLY 5 BY TOTAL-SCORE(TOTAL-SUB)
             EVALUATE COMPLETION-LIST(COMPLETION-PTR:1)
               WHEN ')' ADD 1 TO TOTAL-SCORE(TOTAL-SUB)
               WHEN ']' ADD 2 TO TOTAL-SCORE(TOTAL-SUB)
               WHEN '}' ADD 3 TO TOTAL-SCORE(TOTAL-SUB)
               WHEN '>' ADD 4 TO TOTAL-SCORE(TOTAL-SUB)
             END-EVALUATE
           END-PERFORM

           ADD 1 TO TOTAL-SCORE-MAX
           .

       2010-ADD-TO-COMPLETION-LIST.
           EVALUATE STACK(STACK-PTR)
             WHEN '(' MOVE ')' TO COMPLETION-CHAR
             WHEN '[' MOVE ']' TO COMPLETION-CHAR
             WHEN '{' MOVE '}' TO COMPLETION-CHAR
             WHEN '<' MOVE '>' TO COMPLETION-CHAR
           END-EVALUATE

           STRING COMPLETION-CHAR
             INTO COMPLETION-LIST
             POINTER COMPLETION-PTR
           END-STRING
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .


