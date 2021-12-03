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
       Program-ID. cs03b.
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
           05  MYNAME             PIC X(008) VALUE 'cs03b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  O2-COUNT           PIC 9(008) COMP    VALUE 9.
           05  CO2-COUNT          PIC 9(008) COMP    VALUE 9.
           05  O2-INDX            PIC 9(009) COMP    VALUE 0.
           05  CO2-INDX           PIC 9(009) COMP    VALUE 0.
           05  BIT-EXPONENT       PIC 9(008) COMP    VALUE 0.
           05  NB-BITS            PIC 9(008) COMP    VALUE 1.
           05  BIT-TO-COUNT       PIC 9(008) COMP    VALUE 0.
           05  CURR-PRODUCT       PIC 9(018) COMP    VALUE 0.
           05  O2-RATING          PIC 9(018) COMP    VALUE 0.
           05  CO2-RATING         PIC 9(018) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.

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
               10  MOST-COMMON    PIC X(001).
               10  LEAST-COMMON   PIC X(001).

       01  INPT-VALUES.
           05  INPT-TABLE
               OCCURS 1 TO 2000
               DEPENDING WS-REC-COUNT
               INDEXED INPT-INDX.
               10  INPT-BITS      PIC X(024).
               10  INPT-O2-FLAG   PIC X(001).
                   88  INPT-O2                       VALUE 'Y'
                                                     FALSE 'N'.
               10  INPT-CO2-FLAG  PIC X(001).
                   88  INPT-CO2                      VALUE 'Y'
                                                     FALSE 'N'.

       Procedure Division.
           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           MOVE 1 TO BIT-TO-COUNT
           PERFORM UNTIL INPT-DATA-EOF
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
           UNTIL BIT-TO-COUNT > NB-BITS
           OR O2-COUNT = 1
             MOVE 0 TO COUNT-0(BIT-TO-COUNT)
             MOVE 0 TO COUNT-1(BIT-TO-COUNT)
             PERFORM VARYING INPT-INDX FROM 1 BY 1
             UNTIL INPT-INDX > WS-REC-COUNT
               IF INPT-O2(INPT-INDX)
                   PERFORM 8020-COUNT-BITS
               END-IF
             END-PERFORM
             PERFORM 8030-MOST-AND-LEAST
             MOVE 0 TO O2-COUNT
             PERFORM VARYING INPT-INDX FROM 1 BY 1
             UNTIL INPT-INDX > WS-REC-COUNT
               IF INPT-BITS(INPT-INDX)(BIT-TO-COUNT:1) =
                   MOST-COMMON(BIT-TO-COUNT)
               AND INPT-O2(INPT-INDX)
                    ADD 1 TO O2-COUNT
                    MOVE INPT-INDX TO O2-INDX
               ELSE
                   SET INPT-O2(INPT-INDX) TO FALSE
               END-IF
             END-PERFORM
             IF PROCESS-TEST
                 RESET TRACE
                 PERFORM 9010-DUMP-TABLES
                 READY TRACE
             END-IF
           END-PERFORM

           PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
           UNTIL BIT-TO-COUNT > NB-BITS
           OR CO2-COUNT = 1
             MOVE 0 TO COUNT-0(BIT-TO-COUNT)
             MOVE 0 TO COUNT-1(BIT-TO-COUNT)
             PERFORM VARYING INPT-INDX FROM 1 BY 1
             UNTIL INPT-INDX > WS-REC-COUNT
               IF INPT-CO2(INPT-INDX)
                   PERFORM 8020-COUNT-BITS
               END-IF
             END-PERFORM
             PERFORM 8030-MOST-AND-LEAST
             MOVE 0 TO CO2-COUNT
             PERFORM VARYING INPT-INDX FROM 1 BY 1
             UNTIL INPT-INDX > WS-REC-COUNT
               IF INPT-BITS(INPT-INDX)(BIT-TO-COUNT:1) =
                  LEAST-COMMON(BIT-TO-COUNT)
                  AND INPT-CO2(INPT-INDX)
                   ADD 1 TO CO2-COUNT
                   MOVE INPT-INDX TO CO2-INDX
               ELSE
                   SET INPT-CO2(INPT-INDX) TO FALSE
               END-IF
             END-PERFORM
             IF PROCESS-TEST
                 RESET TRACE
                 PERFORM 9010-DUMP-TABLES
                 READY TRACE
             END-IF
           END-PERFORM

           IF O2-COUNT NOT = 1
               DISPLAY MYNAME ' O2-COUNT = ' O2-COUNT
           END-IF

           IF CO2-COUNT NOT = 1
               DISPLAY MYNAME ' CO2-COUNT = ' CO2-COUNT
           END-IF

           DISPLAY MYNAME ' O2 rating   ' INPT-BITS(O2-INDX)
           DISPLAY MYNAME ' CO2 rating  ' INPT-BITS(CO2-INDX)

           PERFORM VARYING BIT-TO-COUNT FROM 1 BY 1
           UNTIL BIT-TO-COUNT > NB-BITS
             COMPUTE BIT-EXPONENT = NB-BITS - BIT-TO-COUNT
             IF INPT-BITS(O2-INDX)(BIT-TO-COUNT:1) = '1'
                 COMPUTE O2-RATING =
                   O2-RATING + 2 ** BIT-EXPONENT
             END-IF
             IF INPT-BITS(CO2-INDX)(BIT-TO-COUNT:1) = '1'
                 COMPUTE CO2-RATING =
                   CO2-RATING + 2 ** BIT-EXPONENT
             END-IF
           END-PERFORM

           DISPLAY MYNAME ' O2 rating   ' O2-RATING
           DISPLAY MYNAME ' CO2 rating  ' CO2-RATING

           COMPUTE CURR-PRODUCT = O2-RATING * CO2-RATING
           DISPLAY
            MYNAME
            ' product of O2 and CO2 '
            CURR-PRODUCT

           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
               MOVE WS-INPT TO INPT-BITS(WS-REC-COUNT)
               SET INPT-O2(WS-REC-COUNT) TO TRUE
               SET INPT-CO2(WS-REC-COUNT) TO TRUE
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

       8020-COUNT-BITS.
           EVALUATE INPT-BITS(INPT-INDX)(BIT-TO-COUNT:1)
             WHEN '0'
                  ADD 1 TO COUNT-0(BIT-TO-COUNT)
             WHEN '1'
                  ADD 1 TO COUNT-1(BIT-TO-COUNT)
             WHEN OTHER
                  DISPLAY
                    MYNAME
                   ' bad bit '
                   BIT-TO-COUNT
                   ' '
                   INPT-INDX
                   ' |'
                   INPT-BITS(INPT-INDX)
                   '|' 
           END-EVALUATE
           .

       8030-MOST-AND-LEAST.
           EVALUATE TRUE
             WHEN COUNT-0(BIT-TO-COUNT) > COUNT-1(BIT-TO-COUNT)
               MOVE '0' TO MOST-COMMON(BIT-TO-COUNT)
               MOVE '1' TO LEAST-COMMON(BIT-TO-COUNT)
             WHEN COUNT-0(BIT-TO-COUNT) <= COUNT-1(BIT-TO-COUNT)
               MOVE '1' TO MOST-COMMON(BIT-TO-COUNT)
               MOVE '0' TO LEAST-COMMON(BIT-TO-COUNT)
           END-EVALUATE
           .

       9010-DUMP-TABLES.
           DISPLAY
             MYNAME
             BIT-TO-COUNT
             ' COUNT-0 '
             COUNT-0(BIT-TO-COUNT)
             ' COUNT-1 '
             COUNT-1(BIT-TO-COUNT)
             ' MOST-COMMON '
             MOST-COMMON(BIT-TO-COUNT)
             ' LEAST-COMMON '
             LEAST-COMMON(BIT-TO-COUNT)
           PERFORM VARYING INPT-INDX FROM 1 BY 1
           UNTIL INPT-INDX > WS-REC-COUNT
             DISPLAY
               MYNAME
               ' '
               INPT-INDX
               ' '
               INPT-BITS(INPT-INDX)
               ' '
               INPT-O2-FLAG(INPT-INDX)
               ' '
               INPT-CO2-FLAG(INPT-INDX)
           END-PERFORM
           .

