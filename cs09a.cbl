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
       Program-ID. cs09a.
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
           05  MYNAME             PIC X(008) VALUE 'cs09a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  ROW-COUNT          PIC 9(009) COMP    VALUE 0.
           05  ROW-MAX            PIC 9(009) COMP    VALUE 0.
           05  COL-MAX            PIC 9(009) COMP    VALUE 0.
           05  RISK-SUM           PIC 9(009) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  IN-SIGNAL-PATTERNS PIC X(060)         VALUE SPACES.
           05  IN-FOUR-DIGITS     PIC X(032)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  LOW-POINT-SW       PIC X(001)         VALUE 'N'.
               88  A-NEW-LOW                         VALUE 'Y'
                                                     FALSE 'N'.

       01  HEIGHT-MAP.
           05  HEIGHT-ROW
               OCCURS 100
               INDEXED ROW-I1 ROW-I2.
               10  HEIGHT-COL
                   OCCURS 100
                   INDEXED COL-I1 COL-I2.
                   15  HEIGHT     PIC X(001).

       01  ADJACENT-LOCATIONS.
           05  ALOC-MAX           PIC 9(004)  COMP   VALUE 0.
           05  ALOC-SUB           PIC 9(004)  COMP   VALUE 0.
           05  ALOC-VAL OCCURS 4  PIC X(001).

       01  LOW-POINT-TABLE.
           05  LOW-POINT-MAX      PIC 9(009)  COMP   VALUE 0.
           05  LOW-POINT-SUB      PIC 9(009)  COMP   VALUE 0.
           05  LOW-POINT OCCURS 10000 PIC X(001).

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           INITIALIZE HEIGHT-MAP

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           DISPLAY MYNAME ' row max         ' ROW-MAX
           DISPLAY MYNAME ' col max         ' COL-MAX
           
           PERFORM 2000-PROCESS-HEIGHT-MAP

           DISPLAY MYNAME ' sum of all risk ' RISK-SUM
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE
           GOBACK.

       1000-LOAD-INPUT.
           ADD 1 TO ROW-COUNT ROW-MAX

           IF ROW-COUNT = 1
               INSPECT WS-INPT
                 TALLYING COL-MAX 
                   FOR ALL CHARACTERS BEFORE INITIAL SPACE
           END-IF

           SET ROW-I1 TO ROW-COUNT
           MOVE WS-INPT(1:COL-MAX) TO HEIGHT-ROW(ROW-I1)

           PERFORM 8010-READ-INPT-DATA
           .

       2000-PROCESS-HEIGHT-MAP.
           PERFORM 2010-FIND-LOW-POINTS
             VARYING ROW-I1 FROM 1 BY 1
             UNTIL ROW-I1 > ROW-MAX
             AFTER COL-I1 FROM 1 BY 1
             UNTIL COL-I1 > COL-MAX

           display myname ' low point max ' low-point-max
           PERFORM VARYING LOW-POINT-SUB FROM 1 BY 1
           UNTIL LOW-POINT-SUB > LOW-POINT-MAX
             COMPUTE RISK-SUM =
               RISK-SUM + 1 + FUNCTION NUMVAL(LOW-POINT(LOW-POINT-SUB))
           END-PERFORM
           .

       2010-FIND-LOW-POINTS.
           PERFORM 8020-DETERMINE-ADJACENT
           SET A-NEW-LOW TO TRUE
           IF PROCESS-TEST OR (ROW-I1 = 100 AND COL-I1 = 1)
               DISPLAY MYNAME ' HEIGHT('ROW-I1','COL-I1') '
                 HEIGHT(ROW-I1,COL-I1)
           END-IF
           PERFORM VARYING ALOC-SUB FROM 1 BY 1
           UNTIL ALOC-SUB > ALOC-MAX
           OR NOT A-NEW-LOW
             IF PROCESS-TEST OR (ROW-I1 = 100 AND COL-I1 = 1)
                 DISPLAY MYNAME SPACE
                   ALOC-VAL(ALOC-SUB)
             END-IF
             IF ALOC-VAL(ALOC-SUB) <= HEIGHT(ROW-I1,COL-I1)
                 SET A-NEW-LOW TO FALSE
                 IF PROCESS-TEST
                 OR (ROW-I1 = 100 AND COL-I1 = 1)
                     DISPLAY MYNAME SPACE
                       'ALOC-VAL('ALOC-SUB') ' ALOC-VAL(ALOC-SUB)
                       ' <= HEIGHT('ROW-I1','COL-I1') '
                       HEIGHT(ROW-I1,COL-I1)
                 END-IF
             END-IF
           END-PERFORM

           IF A-NEW-LOW
               ADD 1 TO LOW-POINT-MAX
               ADD 1 TO LOW-POINT-SUB
               MOVE HEIGHT(ROW-I1,COL-I1) TO LOW-POINT(LOW-POINT-SUB)
               IF PROCESS-TEST 
               OR (ROW-I1 = 100 AND COL-I1 = 1)
               OR (ROW-I1 = 89  AND COL-I1 = 1)
               OR (ROW-I1 = 15  AND COL-I1 = 99)
               OR (ROW-I1 = 23  AND COL-I1 = 100)
               OR (ROW-I1 = 1   AND COL-I1 = 20)
                 DISPLAY MYNAME ' new low ' LOW-POINT(LOW-POINT-SUB)
                 DISPLAY MYNAME ' found ' ROW-I1 ',' COL-I1
               END-IF
           END-IF
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ
           .

       8020-DETERMINE-ADJACENT.
           SET ROW-I2 TO ROW-I1
           SET COL-I2 TO COL-I1
           INITIALIZE
             ALOC-MAX
             ALOC-SUB

           IF ROW-I1 > 1
               ADD 1 TO ALOC-SUB
               ADD 1 TO ALOC-MAX
               SET ROW-I2 DOWN BY 1
               MOVE HEIGHT(ROW-I2,COL-I2) TO ALOC-VAL(ALOC-SUB)
               SET ROW-I2 TO ROW-I1
           END-IF

           IF ROW-I1 < ROW-MAX
               ADD 1 TO ALOC-SUB
               ADD 1 TO ALOC-MAX
               SET ROW-I2 UP BY 1
               MOVE HEIGHT(ROW-I2,COL-I2) TO ALOC-VAL(ALOC-SUB)
               SET ROW-I2 TO ROW-I1
           END-IF

           IF COL-I1 > 1
               ADD 1 TO ALOC-SUB
               ADD 1 TO ALOC-MAX
               SET COL-I2 DOWN BY 1
               MOVE HEIGHT(ROW-I2,COL-I2) TO ALOC-VAL(ALOC-SUB)
               SET COL-I2 TO COL-I1
           END-IF

           IF COL-I1 < COL-MAX
               ADD 1 TO ALOC-SUB
               ADD 1 TO ALOC-MAX
               SET COL-I2 UP BY 1
               MOVE HEIGHT(ROW-I2,COL-I2) TO ALOC-VAL(ALOC-SUB)
               SET COL-I2 TO COL-I1
           END-IF

           IF ALOC-MAX < 2
               DISPLAY
                 MYNAME ' error in determining adjacent locations '
                 ALOC-MAX SPACE
                 ROW-I1 SPACE
                 COL-I1 SPACE
           END-IF
           .
