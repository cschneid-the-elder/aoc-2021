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
       Program-ID. cs08b.
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
           05  MYNAME             PIC X(008) VALUE 'cs08b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  ENTRY-COUNT        PIC 9(009) COMP    VALUE 0.
           05  UNSTRING-PTR       PIC 9(009) COMP    VALUE 0.
           05  STRING-PTR         PIC 9(009) COMP    VALUE 0.
           05  SEGMENT-COUNT      PIC 9(009) COMP    VALUE 0.
           05  X-COUNT            PIC 9(009) COMP    VALUE 0.
           05  SUB1               PIC 9(009) COMP    VALUE 0.
           05  SUB2               PIC 9(009) COMP    VALUE 0.
           05  SUB3               PIC 9(009) COMP    VALUE 0.
           05  TOTAL-DIGITS       PIC 9(018) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  IN-SIGNAL-PATTERNS PIC X(060)         VALUE SPACES.
           05  IN-FOUR-DIGITS     PIC X(032)         VALUE SPACES.
           05  CODED-VALUE-TBL.
               10  CODED-VALUE OCCURS 8 PIC X(001).
           05  ONE.
               10  ONE-TBL   OCCURS 8 PIC X(001).
           05  FOUR.
               10  FOUR-TBL  OCCURS 8 PIC X(001).
           05  SEVEN.
               10  SEVEN-TBL OCCURS 8 PIC X(001).
           05  EIGHT.
               10  EIGHT-TBL OCCURS 8 PIC X(001).
           05  ZER0.
               10  ZER0-TBL  OCCURS 8 PIC X(001).
           05  SIX.
               10  SIX-TBL   OCCURS 8 PIC X(001).
           05  NINE.
               10  NINE-TBL  OCCURS 8 PIC X(001).
           05  TWO.
               10  TWO-TBL   OCCURS 8 PIC X(001).
           05  THREE.
               10  THREE-TBL OCCURS 8 PIC X(001).
           05  FIVE.
               10  FIVE-TBL  OCCURS 8 PIC X(001).
           05  MAP-A              PIC X(001)         VALUE SPACE.
           05  MAP-B              PIC X(001)         VALUE SPACE.
           05  MAP-C              PIC X(001)         VALUE SPACE.
           05  MAP-D              PIC X(001)         VALUE SPACE.
           05  MAP-E              PIC X(001)         VALUE SPACE.
           05  MAP-F              PIC X(001)         VALUE SPACE.
           05  MAP-G              PIC X(001)         VALUE SPACE.
           05  CDE-TABLE.
               10  CDE
               OCCURS 3
               INDEXED CDE-INDX
                                  PIC X(001)         VALUE SPACE.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  ENTRY-TABLE.
           05  ENTRY-TBL
               OCCURS 200
               INDEXED ENTRY-INDX.
               10  SIGNAL-PATTERN-TBL
                   OCCURS 10
                   INDEXED SIGNAL-INDX.
                   15  SIGNAL-PATTERN        PIC X(008).
               10  DIGIT-TBL
                   OCCURS 4
                   INDEXED DIGIT-INDX.
                   15  DIGIT                 PIC X(008).
                   15  DIGIT-VAL             PIC X(001).
               10  ALL-DIGITS                PIC X(004).

       01  SIXES-TABLE.
           05  SIXES OCCURS 3                PIC X(008).

       01  FIVES-TABLE.
           05  FIVES OCCURS 3                PIC X(008).

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           INITIALIZE
             ENTRY-TABLE
             SIXES-TABLE
             FIVES-TABLE
             CDE-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           PERFORM 2000-PROCESS-ENTRIES
             VARYING ENTRY-INDX FROM 1 BY 1
             UNTIL ENTRY-INDX > ENTRY-COUNT

           DISPLAY MYNAME ' total digits    ' TOTAL-DIGITS
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE
           GOBACK.

       1000-LOAD-INPUT.
           ADD 1 TO ENTRY-COUNT
           SET ENTRY-INDX TO ENTRY-COUNT
           UNSTRING WS-INPT DELIMITED ' | '
             INTO
               IN-SIGNAL-PATTERNS
               IN-FOUR-DIGITS
           END-UNSTRING

           MOVE 1 TO UNSTRING-PTR
           PERFORM VARYING SIGNAL-INDX FROM 1 BY 1
           UNTIL SIGNAL-INDX > 10
             UNSTRING IN-SIGNAL-PATTERNS DELIMITED SPACE
               INTO SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX)
               POINTER UNSTRING-PTR
             END-UNSTRING
           END-PERFORM

           MOVE 1 TO UNSTRING-PTR
           PERFORM VARYING DIGIT-INDX FROM 1 BY 1
           UNTIL DIGIT-INDX > 4
             UNSTRING IN-FOUR-DIGITS DELIMITED SPACE
               INTO DIGIT(ENTRY-INDX,DIGIT-INDX)
               POINTER UNSTRING-PTR
             END-UNSTRING
           END-PERFORM

           PERFORM 8010-READ-INPT-DATA
           .

       2000-PROCESS-ENTRIES.
           IF PROCESS-TEST
               RESET TRACE
           END-IF          

           INITIALIZE
             SIXES-TABLE
             CDE-TABLE

           PERFORM 2010-PROCESS-SIGNALS

           PERFORM 2020-0-6-9

           PERFORM 2030-2-3-5

           SORT ONE-TBL   ASCENDING KEY ONE-TBL
           SORT TWO-TBL   ASCENDING KEY TWO-TBL
           SORT THREE-TBL ASCENDING KEY TWO-TBL
           SORT FOUR-TBL  ASCENDING KEY TWO-TBL
           SORT FIVE-TBL  ASCENDING KEY TWO-TBL
           SORT SIX-TBL   ASCENDING KEY TWO-TBL
           SORT SEVEN-TBL ASCENDING KEY TWO-TBL
           SORT EIGHT-TBL ASCENDING KEY TWO-TBL
           SORT NINE-TBL  ASCENDING KEY TWO-TBL
           SORT ZER0-TBL  ASCENDING KEY TWO-TBL

           PERFORM 2040-PROCESS-DIGITS

           .

       2010-PROCESS-SIGNALS.
           INITIALIZE SUB1 SUB2 SUB3
           PERFORM VARYING SIGNAL-INDX FROM 1 BY 1
           UNTIL SIGNAL-INDX > 10
             INITIALIZE SEGMENT-COUNT
             INSPECT SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX)
               TALLYING SEGMENT-COUNT
                 FOR ALL CHARACTERS BEFORE SPACE
             EVALUATE SEGMENT-COUNT
               WHEN 2
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX) TO ONE
               WHEN 3
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX) TO SEVEN
               WHEN 4
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX) TO FOUR
               WHEN 7
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX) TO EIGHT
               WHEN 6
                    ADD 1 TO SUB1
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX)
                      TO SIXES(SUB1)
               WHEN 5
                    ADD 1 TO SUB2
                    MOVE SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX)
                      TO FIVES(SUB2)
             END-EVALUATE
           END-PERFORM

          PERFORM 3000-MAP

          IF PROCESS-TEST
          DISPLAY ' A ' MAP-A
          DISPLAY ' B ' MAP-B
          DISPLAY ' C ' MAP-C
          DISPLAY ' D ' MAP-D
          DISPLAY ' E ' MAP-E
          DISPLAY ' F ' MAP-F
          DISPLAY ' G ' MAP-G
          END-IF
          .

       2020-0-6-9.
           INITIALIZE ZER0 SIX NINE

           PERFORM VARYING SUB1 FROM 1 BY 1
           UNTIL SUB1 > 3
             IF PROCESS-TEST
                 DISPLAY MYNAME ' SIXES(' SUB1 ') ' SIXES(SUB1)
             END-IF
             IF ZER0 = SPACES
                 PERFORM 2021-0
             END-IF
             IF SIX = SPACES
                 PERFORM 2022-6
             END-IF
             IF NINE = SPACES
                 PERFORM 2023-9
             END-IF
           END-PERFORM
           .

       2021-0.
           INITIALIZE X-COUNT
           INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-B
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-C
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT SIXES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-E
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT SIXES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-F
                           IF X-COUNT > 0
                               INITIALIZE X-COUNT
                               INSPECT SIXES(SUB1)
                                 TALLYING X-COUNT FOR ALL MAP-G
                               IF X-COUNT > 0
                                   MOVE SIXES(SUB1) TO ZER0
                                   IF PROCESS-TEST
                                       DISPLAY MYNAME
                                         ' SIXES(' SUB1 ') is ZER0'
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2022-6.
           INITIALIZE X-COUNT
           INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-B
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-D
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT SIXES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-E
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT SIXES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-F
                           IF X-COUNT > 0
                               INITIALIZE X-COUNT
                               INSPECT SIXES(SUB1)
                                 TALLYING X-COUNT FOR ALL MAP-G
                               IF X-COUNT > 0
                                   MOVE SIXES(SUB1) TO SIX
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2023-9.
           INITIALIZE X-COUNT
           INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-B
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT SIXES(SUB1) TALLYING X-COUNT FOR ALL MAP-C
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT SIXES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-D
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT SIXES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-F
                           IF X-COUNT > 0
                               INITIALIZE X-COUNT
                               INSPECT SIXES(SUB1)
                                 TALLYING X-COUNT FOR ALL MAP-G
                               IF X-COUNT > 0
                                   MOVE SIXES(SUB1) TO NINE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2030-2-3-5.
           INITIALIZE TWO THREE FIVE

           PERFORM VARYING SUB1 FROM 1 BY 1
           UNTIL SUB1 > 3
             IF TWO = SPACES
                 PERFORM 2031-2
             END-IF
             IF THREE = SPACES
                 PERFORM 2032-3
             END-IF
             IF FIVE = SPACES
                 PERFORM 2033-5
             END-IF
           END-PERFORM
           .

       2031-2.
           INITIALIZE X-COUNT
           INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-C
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-D
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT FIVES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-E
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT FIVES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-G
                           IF X-COUNT > 0
                               MOVE FIVES(SUB1) TO TWO
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2032-3.
           INITIALIZE X-COUNT
           INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-C
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-D
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT FIVES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-F
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT FIVES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-G
                           IF X-COUNT > 0
                               MOVE FIVES(SUB1) TO THREE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2033-5.
           INITIALIZE X-COUNT
           INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-A
           IF X-COUNT > 0
               INITIALIZE X-COUNT
               INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-B
               IF X-COUNT > 0
                   INITIALIZE X-COUNT
                   INSPECT FIVES(SUB1) TALLYING X-COUNT FOR ALL MAP-D
                   IF X-COUNT > 0
                       INITIALIZE X-COUNT
                       INSPECT FIVES(SUB1)
                         TALLYING X-COUNT FOR ALL MAP-F
                       IF X-COUNT > 0
                           INITIALIZE X-COUNT
                           INSPECT FIVES(SUB1)
                             TALLYING X-COUNT FOR ALL MAP-G
                           IF X-COUNT > 0
                               MOVE FIVES(SUB1) TO FIVE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       2040-PROCESS-DIGITS.
           IF PROCESS-TEST
           DISPLAY MYNAME ' zero  ' ZER0
           DISPLAY MYNAME ' one   ' ONE
           DISPLAY MYNAME ' two   ' TWO
           DISPLAY MYNAME ' three ' THREE
           DISPLAY MYNAME ' four  ' FOUR
           DISPLAY MYNAME ' five  ' FIVE
           DISPLAY MYNAME ' six   ' SIX
           DISPLAY MYNAME ' seven ' SEVEN
           DISPLAY MYNAME ' eight ' EIGHT
           DISPLAY MYNAME ' nine  ' NINE
           END-IF

           MOVE 1 TO STRING-PTR
           PERFORM VARYING DIGIT-INDX FROM 1 BY 1
           UNTIL DIGIT-INDX > 4
             MOVE DIGIT(ENTRY-INDX,DIGIT-INDX) TO CODED-VALUE-TBL
             SORT CODED-VALUE ASCENDING KEY CODED-VALUE
             EVALUATE CODED-VALUE-TBL
               WHEN ZER0  MOVE '0' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN ONE   MOVE '1' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN TWO   MOVE '2' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN THREE MOVE '3' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN FOUR  MOVE '4' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN FIVE  MOVE '5' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN SIX   MOVE '6' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN SEVEN MOVE '7' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN EIGHT MOVE '8' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN NINE  MOVE '9' TO DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               WHEN OTHER
                    DISPLAY MYNAME ' digit encoding failure for '
                      'DIGIT('ENTRY-INDX','DIGIT-INDX') '
                      DIGIT(ENTRY-INDX,DIGIT-INDX)
             END-EVALUATE
             STRING DIGIT-VAL(ENTRY-INDX,DIGIT-INDX)
               INTO ALL-DIGITS(ENTRY-INDX)
               POINTER STRING-PTR
             END-STRING
           END-PERFORM

           DISPLAY MYNAME ' ALL-DIGITS ' ALL-DIGITS(ENTRY-INDX)
           ADD FUNCTION NUMVAL(ALL-DIGITS(ENTRY-INDX)) TO TOTAL-DIGITS
           .

       3000-MAP.
           PERFORM 3010-MAP-A
           PERFORM 3020-CDE
           PERFORM 3030-MAP-E
           PERFORM 3040-MAP-D
           PERFORM 3050-MAP-C
           PERFORM 3060-MAP-F
           PERFORM 3070-MAP-B
           PERFORM 3080-MAP-G
           .

       3010-MAP-A.
           INITIALIZE MAP-A X-COUNT
           PERFORM VARYING SUB3 FROM 1 BY 1
           UNTIL SUB3 > FUNCTION LENGTH(SEVEN)
           OR MAP-A NOT = SPACE
             INITIALIZE X-COUNT
             INSPECT ONE TALLYING X-COUNT FOR ALL SEVEN(SUB3:1)
             IF X-COUNT = 0
                 MOVE SEVEN(SUB3:1) TO MAP-A
             END-IF
           END-PERFORM

           IF MAP-A = SPACE
               DISPLAY MYNAME ' failed to map a'
               DISPLAY MYNAME ' ONE   ' ONE
               DISPLAY MYNAME ' SEVEN ' SEVEN
           END-IF
           .

       3020-CDE.
           INITIALIZE CDE-TABLE
           SET CDE-INDX TO 0

           PERFORM VARYING SUB1 FROM 1 BY 1
           UNTIL SUB1 > 3
             PERFORM VARYING SUB2 FROM 1 BY 1
             UNTIL SUB2 > 3
               IF SUB1 NOT = SUB2
                   PERFORM VARYING SUB3 FROM 1 BY 1
                   UNTIL SUB3 > FUNCTION LENGTH(SIXES(SUB2))
                     INITIALIZE X-COUNT
                     INSPECT SIXES(SUB1)
                       TALLYING X-COUNT FOR ALL SIXES(SUB2)(SUB3:1)
                     IF X-COUNT = 0
                         INITIALIZE X-COUNT
                         INSPECT CDE-TABLE
                           TALLYING X-COUNT FOR ALL SIXES(SUB2)(SUB3:1)
                         IF X-COUNT = 0
                             SET CDE-INDX UP BY 1
                             MOVE SIXES(SUB2)(SUB3:1) TO CDE(CDE-INDX)
                         END-IF
                     END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
           END-PERFORM

           IF PROCESS-TEST
               DISPLAY MYNAME ' CDE-TABLE ' CDE-TABLE
           END-IF
           .

       3030-MAP-E.
           INITIALIZE MAP-E X-COUNT

           PERFORM VARYING CDE-INDX FROM 1 BY 1
           UNTIL CDE-INDX > 3
           OR MAP-E NOT = SPACE
             INITIALIZE X-COUNT
             INSPECT FOUR TALLYING X-COUNT FOR ALL CDE(CDE-INDX)
             IF X-COUNT = 0
                 MOVE CDE(CDE-INDX) TO MAP-E
             END-IF
           END-PERFORM

           IF MAP-E = SPACE
               DISPLAY MYNAME ' failed to map e'
               DISPLAY MYNAME ' FOUR ' FOUR
               DISPLAY MYNAME ' CDE  ' CDE-TABLE
           END-IF
           .

       3040-MAP-D.
           INITIALIZE MAP-D X-COUNT

           PERFORM VARYING CDE-INDX FROM 1 BY 1
           UNTIL CDE-INDX > 3
           OR MAP-D NOT = SPACE
             IF PROCESS-TEST
                 DISPLAY MYNAME ' CDE(' CDE-INDX ') ' CDE(CDE-INDX)
             END-IF
             IF CDE(CDE-INDX) = MAP-E
                 CONTINUE
             ELSE
                 INITIALIZE X-COUNT
                 INSPECT ONE TALLYING X-COUNT FOR ALL CDE(CDE-INDX)
                 IF X-COUNT = 0
                     MOVE CDE(CDE-INDX) TO MAP-D
                 END-IF
             END-IF
           END-PERFORM

           IF MAP-D = SPACE
               DISPLAY MYNAME ' failed to map d'
           END-IF
           .

       3050-MAP-C.
           INITIALIZE MAP-C

           PERFORM VARYING CDE-INDX FROM 1 BY 1
           UNTIL CDE-INDX > 3
           OR MAP-C NOT = SPACE
             IF CDE(CDE-INDX) = MAP-E
             OR CDE(CDE-INDX) = MAP-D
                 CONTINUE
             ELSE
                 MOVE CDE(CDE-INDX) TO MAP-C
             END-IF
           END-PERFORM

           IF MAP-C = SPACE
               DISPLAY MYNAME ' failed to map c'
           END-IF
           .

       3060-MAP-F.
           INITIALIZE MAP-F

           IF ONE(1:1) = MAP-C
               MOVE ONE(2:1) TO MAP-F
           ELSE
               MOVE ONE(1:1) TO MAP-F
           END-IF

           IF MAP-F = SPACE
               DISPLAY MYNAME ' failed to map f'
           END-IF
           .

       3070-MAP-B.
           INITIALIZE MAP-B

           PERFORM VARYING SUB1 FROM 1 BY 1
           UNTIL SUB1 > 4
           OR MAP-B NOT = SPACE
             EVALUATE FOUR(SUB1:1)
               WHEN MAP-C
               WHEN MAP-D
               WHEN MAP-F
                    CONTINUE
               WHEN OTHER
                    MOVE FOUR(SUB1:1) TO MAP-B
             END-EVALUATE
           END-PERFORM

           IF MAP-B = SPACE
               DISPLAY MYNAME ' failed to map b'
           END-IF
           .

       3080-MAP-G.
           INITIALIZE MAP-G

           PERFORM VARYING SUB1 FROM 1 BY 1
           UNTIL SUB1 > 7
           OR MAP-G NOT = SPACE
             EVALUATE EIGHT(SUB1:1)
               WHEN MAP-A
               WHEN MAP-B
               WHEN MAP-C
               WHEN MAP-D
               WHEN MAP-E
               WHEN MAP-F
                    CONTINUE
               WHEN OTHER
                    MOVE EIGHT(SUB1:1) TO MAP-G
             END-EVALUATE
           END-PERFORM

           IF MAP-G = SPACE
               DISPLAY MYNAME ' failed to map g'
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


