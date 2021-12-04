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
       Program-ID. cs04a.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(512).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'cs04a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  BINGO-COUNT        PIC 9(008) COMP    VALUE 0.
           05  BINGO-COL          PIC 9(009) COMP    VALUE 0.
           05  BINGO-ROW          PIC 9(009) COMP    VALUE 0.
           05  BOARDS-MAX         PIC 9(008) COMP    VALUE 0.
           05  BOARDS-PTR         PIC 9(008) COMP    VALUE 1.
           05  WINNING-BOARD      PIC 9(009) COMP    VALUE 1.
           05  WINNING-SUM        PIC 9(009) COMP    VALUE 0.
           05  NUMBERS-MAX        PIC 9(008) COMP    VALUE 0.
           05  NUMBERS-PTR        PIC 9(008) COMP    VALUE 1.
           05  CURR-PRODUCT       PIC 9(016) COMP    VALUE 0.
           05  OPERATION-ARG      PIC 9(008) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  WINNING-NUMBER     PIC X(002)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(512)         VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  BINGO-SW           PIC X(008)         VALUE 'N'.
               88  SOMEONE-YELLS-BINGO               VALUE 'Y'
                                                     FALSE 'N'.

       01  DRAW-NUMBERS-TABLE.
           05  DRAW-NUMBER
               OCCURS 200
               INDEXED NUMBERS-INDX
               PIC X(002) JUST RIGHT.

       01  BINGO-BOARD-TABLES.
           05  BINGO-BOARD OCCURS 200 INDEXED BOARD-INDX.
               10  BOARD-ROW OCCURS 5 INDEXED ROW-INDX ROW-INDX-B.
                   15  BOARD-COL OCCURS 5 INDEXED COL-INDX COL-INDX-B.
                       20  BOARD-VAL  PIC X(002) JUST RIGHT.
                       20  BINGO-MARK PIC X(001).
                           88  BINGO-MARKED           VALUE 'Y'
                                                      FALSE 'N'.

       Procedure Division.
           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           PERFORM 1000-READ-AND-PARSE-INPT

           PERFORM 2000-PLAY-THE-GAME
             VARYING NUMBERS-INDX FROM 1 BY 1
             UNTIL NUMBERS-INDX > NUMBERS-MAX
             OR SOMEONE-YELLS-BINGO

           IF SOMEONE-YELLS-BINGO
               DISPLAY
                 MYNAME
                ' bingo is declared for board '
                WINNING-BOARD
                PERFORM 9010-DUMP-BOARDS
                DISPLAY MYNAME ' winning number ' WINNING-NUMBER
                DISPLAY MYNAME ' bingo row ' BINGO-ROW
                DISPLAY MYNAME ' bingo col ' BINGO-COL
                PERFORM 3000-SUM-WINNING-BOARD
                COMPUTE CURR-PRODUCT = 
                  FUNCTION NUMVAL(WINNING-NUMBER) * WINNING-SUM
                DISPLAY MYNAME ' final score ' CURR-PRODUCT
           ELSE
               DISPLAY MYNAME ' bingo remains elusive'
           END-IF

           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       1000-READ-AND-PARSE-INPT.
           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM UNTIL INPT-DATA-EOF
             EVALUATE TRUE
               WHEN WS-REC-COUNT = 1
                    PERFORM 1100-PARSE-NUMBERS
               WHEN WS-INPT = SPACES
                    IF BOARDS-MAX = 0
                        SET BOARD-INDX TO 1
                    ELSE
                        SET BOARD-INDX UP BY 1
                    END-IF
                    SET ROW-INDX TO 1
                    ADD 1 TO BOARDS-MAX
                    INITIALIZE BINGO-BOARD(BOARD-INDX)
               WHEN OTHER
                    PERFORM 1200-PARSE-BOARD
                    IF ROW-INDX < 5
                        SET ROW-INDX UP BY 1
                    END-IF
             END-EVALUATE
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           DISPLAY
            MYNAME
            ' drawn numbers '
            NUMBERS-MAX

           DISPLAY
            MYNAME
            ' boards '
            BOARDS-MAX

           IF PROCESS-TEST
               PERFORM 9010-DUMP-BOARDS
           END-IF
           .

       1100-PARSE-NUMBERS.
           INITIALIZE DRAW-NUMBERS-TABLE
           SET NUMBERS-PTR TO 1

           PERFORM VARYING NUMBERS-INDX FROM 1 BY 1
           UNTIL WS-INPT-DATA(NUMBERS-PTR:1) = SPACE
             UNSTRING WS-INPT-DATA DELIMITED ',' OR SPACE
               INTO DRAW-NUMBER(NUMBERS-INDX)
               POINTER NUMBERS-PTR
             END-UNSTRING
             ADD 1 TO NUMBERS-MAX
           END-PERFORM
           .

       1200-PARSE-BOARD.
           MOVE 1 TO BOARDS-PTR

           PERFORM VARYING COL-INDX FROM 1 BY 1
           UNTIL COL-INDX > 5
             IF WS-INPT-DATA(1:1) = SPACE
                 UNSTRING WS-INPT-DATA(2:) DELIMITED ALL SPACE
                   INTO BOARD-VAL(BOARD-INDX,ROW-INDX,COL-INDX)
                   POINTER BOARDS-PTR
                 END-UNSTRING
             ELSE
                 UNSTRING WS-INPT-DATA DELIMITED ALL SPACE
                   INTO BOARD-VAL(BOARD-INDX,ROW-INDX,COL-INDX)
                   POINTER BOARDS-PTR
                 END-UNSTRING
             END-IF
             SET BINGO-MARKED(BOARD-INDX,ROW-INDX,COL-INDX) TO FALSE
           END-PERFORM
           .

       2000-PLAY-THE-GAME.
           PERFORM VARYING BOARD-INDX FROM 1 BY 1
           UNTIL BOARD-INDX > BOARDS-MAX OR SOMEONE-YELLS-BINGO
             PERFORM VARYING ROW-INDX FROM 1 BY 1
             UNTIL ROW-INDX > 5 OR SOMEONE-YELLS-BINGO
               PERFORM VARYING COL-INDX FROM 1 BY 1
               UNTIL COL-INDX > 5
                 IF BOARD-VAL(BOARD-INDX,ROW-INDX,COL-INDX)
                 = DRAW-NUMBER(NUMBERS-INDX)
                     SET BINGO-MARKED(BOARD-INDX,ROW-INDX,COL-INDX)
                      TO TRUE
                 END-IF
               END-PERFORM
             END-PERFORM
             PERFORM 2100-DID-SOMEONE-YELL-BINGO
             IF SOMEONE-YELLS-BINGO
                 MOVE BOARD-INDX TO WINNING-BOARD
                 MOVE DRAW-NUMBER(NUMBERS-INDX) TO WINNING-NUMBER
             END-IF
           END-PERFORM
           .

       2100-DID-SOMEONE-YELL-BINGO.
           PERFORM VARYING ROW-INDX-B FROM 1 BY 1
           UNTIL ROW-INDX-B > 5 OR BINGO-COUNT = 5
             MOVE 0 TO BINGO-COUNT
             PERFORM VARYING COL-INDX-B FROM 1 BY 1
             UNTIL COL-INDX-B > 5 OR BINGO-COUNT = 5
               IF BINGO-MARKED(BOARD-INDX,ROW-INDX-B,COL-INDX-B)
                   ADD 1 TO BINGO-COUNT
               END-IF
             END-PERFORM
           END-PERFORM

           IF BINGO-COUNT = 5
               SET SOMEONE-YELLS-BINGO TO TRUE
               SET ROW-INDX-B DOWN BY 1
               MOVE ROW-INDX-B TO BINGO-ROW
           ELSE
               PERFORM VARYING COL-INDX-B FROM 1 BY 1
               UNTIL COL-INDX-B > 5 OR BINGO-COUNT = 5
                 MOVE 0 TO BINGO-COUNT
                 PERFORM VARYING ROW-INDX-B FROM 1 BY 1
                 UNTIL ROW-INDX-B > 5 OR BINGO-COUNT = 5
                   IF BINGO-MARKED(BOARD-INDX,ROW-INDX-B,COL-INDX-B)
                       ADD 1 TO BINGO-COUNT
                   END-IF
                 END-PERFORM
               END-PERFORM
               IF BINGO-COUNT = 5
                   SET SOMEONE-YELLS-BINGO TO TRUE
                   SET COL-INDX-B DOWN BY 1
                   MOVE COL-INDX-B TO BINGO-COL
               END-IF
           END-IF
           .

       3000-SUM-WINNING-BOARD.
           PERFORM VARYING ROW-INDX FROM 1 BY 1
           UNTIL ROW-INDX > 5
           AFTER COL-INDX FROM 1 BY 1
           UNTIL COL-INDX > 5
      *       IF ROW-INDX = BINGO-ROW OR COL-INDX = BINGO-COL
             IF BINGO-MARKED(WINNING-BOARD,ROW-INDX,COL-INDX)
                 CONTINUE
             ELSE
                 DISPLAY MYNAME ' adding BOARD-VAL('
                   WINNING-BOARD ',' ROW-INDX ',' COL-INDX ') '
                   BOARD-VAL(WINNING-BOARD,ROW-INDX,COL-INDX)
                 ADD FUNCTION NUMVAL(
                     BOARD-VAL(WINNING-BOARD,ROW-INDX,COL-INDX))
                  TO WINNING-SUM
             END-IF
           END-PERFORM

           DISPLAY MYNAME ' winning sum ' WINNING-SUM
           .
       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ
           .

       9010-DUMP-BOARDS.
           IF PROCESS-TEST
               RESET TRACE
           END-IF

           PERFORM VARYING BOARD-INDX FROM 1 BY 1
           UNTIL BOARD-INDX > BOARDS-MAX
             PERFORM VARYING ROW-INDX FROM 1 BY 1
             UNTIL ROW-INDX > 5
               DISPLAY MYNAME ' ' BOARD-INDX WITH NO ADVANCING
               PERFORM VARYING COL-INDX FROM 1 BY 1
               UNTIL COL-INDX > 5
                 DISPLAY
                   SPACE
                   BOARD-VAL(BOARD-INDX,ROW-INDX,COL-INDX)
                   SPACE
                   BINGO-MARK(BOARD-INDX,ROW-INDX,COL-INDX)
                   SPACE
                   WITH NO ADVANCING
               END-PERFORM
               DISPLAY ' '
             END-PERFORM
             DISPLAY MYNAME
           END-PERFORM

           IF PROCESS-TEST
               READY TRACE
           END-IF
           .

