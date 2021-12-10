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
      *
       Program-ID. cs10a.
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
           05  MYNAME             PIC X(008)      VALUE 'cs10a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  STACK-PTR          PIC 9(009) COMP    VALUE 0.
           05  CHAR-PTR           PIC 9(009) COMP    VALUE 0.
           05  FILE-SCORE         PIC 9(009) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  THE-CHAR           PIC X(001)         VALUE SPACE.
               88  THE-CHAR-IS-OPEN                  VALUES
                                                     '(' '[' '{' '<'.
               88  THE-CHAR-IS-CLOSE                 VALUES
                                                     ')' ']' '}' '>'.

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
           05  STACK OCCURS 256   PIC X(001).

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-PROCESS-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           DISPLAY MYNAME ' file score      ' FILE-SCORE
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
               continue
           END-IF

           PERFORM 8010-READ-INPT-DATA
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .


