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
       Program-ID. cs14a.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(080).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'cs14a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  RULE-COUNT         PIC 9(009) COMP    VALUE 0.
           05  NB-STEPS           PIC 9(009) COMP    VALUE 0.
           05  POLY-IDX           PIC 9(009) COMP    VALUE 0.
           05  POLY-LEN           PIC 9(009) COMP    VALUE 0.
           05  POLY-PTR           PIC 9(009) COMP    VALUE 1.
           05  LMNT-MAX           PIC 9(009) COMP    VALUE 0.
           05  LMNT-MIN           PIC 9(009) COMP    VALUE 999999999.
           05  LMNT-COUNT         PIC 9(009) COMP    VALUE 0.
           05  LMNT-DIF           PIC 9(009) COMP    VALUE 0.
           05  NB-STEPS-X         PIC X(004)         VALUE SPACES.
           05  HOLD-LMNT          PIC X(001)         VALUE SPACES.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(080)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  RULES-NOW-SW       PIC X(001)         VALUE 'N'.
               88  RULES-NOW                         VALUE 'Y'.

       01  RULE-TABLE.
           05  RULE-TBL
               OCCURS 100
               ASCENDING KEY PAIR
               INDEXED RULE-IDX.
               10  PAIR           PIC X(002).
               10  LMNT           PIC X(001).

       01  POLYMER                PIC X(100000)       VALUE SPACES.
       01  POLYMER-R
           REDEFINES POLYMER.
           05  POLY-TBL OCCURS 100000 PIC X(001).
       01  POLYMER-NEW            PIC X(100000)       VALUE SPACES.

       Procedure Division.
           DISPLAY MYNAME SPACE CURRENT-DATE

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE NB-STEPS-X
           END-UNSTRING

           MOVE UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW
           MOVE NUMVAL(NB-STEPS-X) TO NB-STEPS

           DISPLAY MYNAME ' nb steps  ' NB-STEPS

           MOVE HIGH-VALUES TO RULE-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           INSPECT POLYMER
             TALLYING POLY-PTR FOR CHARACTERS BEFORE INITIAL SPACE
           ADD 1 TO POLY-PTR

           DISPLAY MYNAME ' initial polymer ' POLYMER(1:POLY-PTR)
           DISPLAY MYNAME ' number of rules ' RULE-COUNT

           SORT RULE-TBL ASCENDING KEY PAIR

           PERFORM 2000-CONSTRUCT-POLYMER NB-STEPS TIMES

           IF NB-STEPS < 5
               DISPLAY MYNAME ' ' POLYMER(1:POLY-PTR)
           END-IF

           SORT POLY-TBL DESCENDING KEY POLY-TBL

           PERFORM 3000-COUNT-ELEMENTS

           DISPLAY MYNAME ' most common     ' LMNT-MAX
           DISPLAY MYNAME ' least common    ' LMNT-MIN
           DISPLAY MYNAME ' difference      ' LMNT-DIF
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE CURRENT-DATE

           GOBACK.

       1000-LOAD-INPUT.
           EVALUATE TRUE
             WHEN RULES-NOW
                  ADD 1 TO RULE-COUNT
                  UNSTRING WS-INPT DELIMITED ' -> '
                    INTO PAIR(RULE-COUNT) LMNT(RULE-COUNT)
                  END-UNSTRING
             WHEN WS-REC-COUNT = 1
                  MOVE WS-INPT TO POLYMER
             WHEN WS-INPT = SPACES
                  SET RULES-NOW TO TRUE
           END-EVALUATE

           INITIALIZE WS-INPT
           PERFORM 8010-READ-INPT-DATA
           .

       2000-CONSTRUCT-POLYMER.
           COMPUTE POLY-LEN = POLY-PTR - 1
           MOVE 1 TO POLY-PTR
           MOVE SPACES TO POLYMER-NEW

           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL POLY-IDX > POLY-LEN
           OR POLYMER(POLY-IDX:1) = SPACE
             EVALUATE TRUE
               WHEN POLYMER(POLY-IDX + 1:1) = SPACE
                    STRING
                        POLYMER(POLY-IDX:1)
                      INTO POLYMER-NEW
                      POINTER POLY-PTR
                    END-STRING
               WHEN OTHER
                    PERFORM 2010-SEARCH-RULES
                    STRING
                        POLYMER(POLY-IDX:1)
                        LMNT(RULE-IDX)
                      INTO POLYMER-NEW
                      POINTER POLY-PTR
                    END-STRING
             END-EVALUATE
           END-PERFORM

           IF POLY-PTR > LENGTH(POLYMER-NEW) - 100
               DISPLAY MYNAME ' POLY-PTR = ' POLY-PTR
               GOBACK
           END-IF

           MOVE POLYMER-NEW TO POLYMER
           .

       2010-SEARCH-RULES.
           SEARCH ALL RULE-TBL
             WHEN PAIR(RULE-IDX) = POLYMER(POLY-IDX:2) CONTINUE
           END-SEARCH
           .

       3000-COUNT-ELEMENTS.
           COMPUTE POLY-LEN = POLY-PTR
           MOVE POLY-TBL(1) TO HOLD-LMNT
           IF NB-STEPS < 5
               DISPLAY MYNAME ' POLY-LEN ' POLY-LEN
               DISPLAY MYNAME ' POLYMER-R ' POLYMER-R(1:POLY-LEN)
           END-IF

           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL POLY-IDX > POLY-LEN
             IF POLY-TBL(POLY-IDX) NOT = HOLD-LMNT
                 MOVE POLY-TBL(POLY-IDX) TO HOLD-LMNT
                 IF LMNT-COUNT < LMNT-MIN
                     MOVE LMNT-COUNT TO LMNT-MIN
                 END-IF
                 IF LMNT-COUNT > LMNT-MAX
                     MOVE LMNT-COUNT TO LMNT-MAX
                 END-IF
                 MOVE 0 TO LMNT-COUNT
             END-IF
             ADD 1 TO LMNT-COUNT
           END-PERFORM

           SUBTRACT LMNT-MIN FROM LMNT-MAX GIVING LMNT-DIF
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .


       END PROGRAM cs14a.


