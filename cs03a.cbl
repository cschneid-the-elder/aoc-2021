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
       Program-ID. cs03a.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(024).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'cs03a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  BIT-EXPONENT       PIC 9(008) COMP    VALUE 0.
           05  NB-BITS            PIC 9(008) COMP    VALUE 1.
           05  BIT-TO-COUNT       PIC 9(008) COMP    VALUE 0.
           05  CURR-PRODUCT       PIC 9(018) COMP    VALUE 0.
           05  GAMMA-RATE         PIC 9(018) COMP    VALUE 0.
           05  EPSILON-RATE       PIC 9(018) COMP    VALUE 0.
           05  GAMMA-RATE-X       PIC X(016)         VALUE SPACES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  EPSILON-RATE-X     PIC X(016)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(024)         VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  BIT-COUNTS.
           05  BIT-COUNT-TABLE
               OCCURS 16.
               10  COUNT-0        PIC 9(008) COMP.
               10  COUNT-1        PIC 9(008) COMP.

       Procedure Division.
           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM UNTIL INPT-DATA-EOF
             PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
             UNTIL BIT-TO-COUNT > NB-BITS
               EVALUATE WS-INPT(BIT-TO-COUNT:1)
                 WHEN '0'
                      ADD 1 TO COUNT-0(BIT-TO-COUNT)
                 WHEN '1'
                      ADD 1 TO COUNT-1(BIT-TO-COUNT)
                 WHEN OTHER
                      DISPLAY
                        MYNAME
                       ' bad bit '
                       BIT-TO-COUNT
                       ' in record '
                       WS-REC-COUNT
                       ' '
                       WS-INPT 
               END-EVALUATE
             END-PERFORM
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           IF PROCESS-TEST
               RESET TRACE
               PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
               UNTIL BIT-TO-COUNT > NB-BITS
                 DISPLAY
                   MYNAME
                   ' COUNT-0(' BIT-TO-COUNT ') '
                   COUNT-0(BIT-TO-COUNT)
                 DISPLAY
                   MYNAME
                   ' COUNT-1(' BIT-TO-COUNT ') '
                   COUNT-1(BIT-TO-COUNT)
               END-PERFORM
               READY TRACE
           END-IF

           PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
           UNTIL BIT-TO-COUNT > NB-BITS
             IF COUNT-0(BIT-TO-COUNT) > COUNT-1(BIT-TO-COUNT)
                 MOVE '0' TO GAMMA-RATE-X(BIT-TO-COUNT:1)
                 MOVE '1' TO EPSILON-RATE-X(BIT-TO-COUNT:1)
             ELSE
                 MOVE '1' TO GAMMA-RATE-X(BIT-TO-COUNT:1)
                 MOVE '0' TO EPSILON-RATE-X(BIT-TO-COUNT:1)
             END-IF
           END-PERFORM

           DISPLAY MYNAME ' gamma rate   ' GAMMA-RATE-X
           DISPLAY MYNAME ' epsilon rate ' EPSILON-RATE-X

           PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
           UNTIL BIT-TO-COUNT > NB-BITS
             COMPUTE BIT-EXPONENT = NB-BITS - BIT-TO-COUNT
             IF GAMMA-RATE-X(BIT-TO-COUNT:1) = '1'
                 COMPUTE GAMMA-RATE =
                   GAMMA-RATE + 2 ** BIT-EXPONENT
             END-IF
             IF EPSILON-RATE-X(BIT-TO-COUNT:1) = '1'
                 COMPUTE EPSILON-RATE =
                   EPSILON-RATE + 2 ** BIT-EXPONENT
             END-IF
           END-PERFORM

           DISPLAY MYNAME ' gamma rate   ' GAMMA-RATE
           DISPLAY MYNAME ' epsilon rate ' EPSILON-RATE

           COMPUTE CURR-PRODUCT = GAMMA-RATE * EPSILON-RATE
           DISPLAY
            MYNAME
            ' product of gamma and epsilon '
            CURR-PRODUCT

           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
               IF WS-REC-COUNT = 1
                   PERFORM 
                   UNTIL WS-INPT(NB-BITS:1) NOT NUMERIC
                     ADD 1 TO NB-BITS
                   END-PERFORM
                   SUBTRACT 1 FROM NB-BITS
                   DISPLAY MYNAME ' number of bits ' NB-BITS
               END-IF
           END-READ

           .


