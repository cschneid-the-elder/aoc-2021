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
      * This program serves as the solution to both parts 1 and 2
      * for day 13.
      *
       Program-ID. cs13a.
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
           05  MYNAME             PIC X(008)         VALUE 'cs13a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  DOT-COUNT          PIC 9(009) COMP    VALUE 0.
           05  NB-FOLDS           PIC 9(009) COMP    VALUE 0.
           05  FOLD-MAX           PIC 9(009) COMP    VALUE 0.
           05  Y-MAX              PIC 9(009) COMP    VALUE 0.
           05  X-MAX              PIC 9(009) COMP    VALUE 0.
           05  FOLD-IDX           PIC 9(009) COMP    VALUE 0.
           05  FOLD-START         PIC 9(009) COMP    VALUE 0.
           05  Y-IDX              PIC 9(009) COMP    VALUE 0.
           05  X-IDX              PIC 9(009) COMP    VALUE 0.
           05  T-IDX              PIC 9(009) COMP    VALUE 0.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  FOLD-HDR           PIC X(012)         VALUE SPACES.
           05  FOLD-LINE-X        PIC X(004)         VALUE SPACES.
           05  X-X                PIC X(004)         VALUE SPACES.
           05  Y-X                PIC X(004)         VALUE SPACES.
           05  NB-FOLDS-X         PIC X(004)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(080)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  FOLDING-NOW-SW     PIC X(001)         VALUE 'N'.
               88  FOLDING-NOW                       VALUE 'Y'.

       01  POINT-TABLE EXTERNAL.
           05  POINT-Y OCCURS 2000.
               10  POINT-X OCCURS 2000.
                   15  POINT      PIC X(001).

       01  POINT-TRANSFORM-TABLE.
           05  POINT-Y-T OCCURS 2000.
               10  POINT-X-T OCCURS 2000.
                   15  POINT-T    PIC X(001).

       01  FOLD-LIST.
           05  FOLDS OCCURS 20.
               10  FOLD-AXIS      PIC X(001).
               10  FOLD-LINE      PIC 9(009) COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE CURRENT-DATE

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE NB-FOLDS-X
           END-UNSTRING

           MOVE UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW
           MOVE NUMVAL(NB-FOLDS-X) TO NB-FOLDS

           DISPLAY MYNAME ' nb folds  ' NB-FOLDS

           INITIALIZE
             POINT-TRANSFORM-TABLE
             FOLD-LIST

           MOVE ALL '.' TO POINT-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           DISPLAY MYNAME ' initial x max ' X-MAX
           DISPLAY MYNAME ' initial y max ' Y-MAX

           IF NB-FOLDS > 0
               PERFORM 2000-PROCESS-INPUT
                 VARYING FOLD-IDX FROM 1 BY 1
                 UNTIL FOLD-IDX > FOLD-MAX
                 OR FOLD-IDX > NB-FOLDS
           ELSE
               PERFORM 2000-PROCESS-INPUT
                 VARYING FOLD-IDX FROM 1 BY 1
                 UNTIL FOLD-IDX > FOLD-MAX
               CALL 'pointdmp' USING
                   Y-MAX
                   X-MAX
               END-CALL
           END-IF

           PERFORM VARYING Y-IDX FROM 1 BY 1
           UNTIL Y-IDX > Y-MAX
           AFTER X-IDX FROM 1 BY 1
           UNTIL X-IDX > X-MAX
             IF POINT(Y-IDX,X-IDX) = '#'
                 ADD 1 TO DOT-COUNT
             END-IF
           END-PERFORM

           DISPLAY MYNAME ' dot count       ' DOT-COUNT
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           GOBACK.

       1000-LOAD-INPUT.
           EVALUATE TRUE
             WHEN FOLDING-NOW
                  UNSTRING WS-INPT DELIMITED '='
                    INTO FOLD-HDR FOLD-LINE-X
                  END-UNSTRING
                  ADD 1 TO FOLD-MAX
                  MOVE FOLD-HDR(12:1) TO FOLD-AXIS(FOLD-MAX)
                  COMPUTE FOLD-LINE(FOLD-MAX) = NUMVAL(FOLD-LINE-X) + 1
                  IF FOLD-AXIS(FOLD-MAX) = 'x'
                      IF FOLD-LINE(FOLD-MAX) * 2 > X-MAX
                          COMPUTE X-MAX = FOLD-LINE(FOLD-MAX) * 2
                      END-IF
                  END-IF
             WHEN WS-INPT = SPACES
                  SET FOLDING-NOW TO TRUE
             WHEN OTHER
                  UNSTRING WS-INPT DELIMITED ','
                    INTO X-X Y-X
                  END-UNSTRING
                  COMPUTE X-IDX = NUMVAL(X-X) + 1
                  COMPUTE Y-IDX = NUMVAL(Y-X) + 1
                  IF PROCESS-TEST
                      DISPLAY MYNAME ' ' X-IDX ',' Y-IDX
                  END-IF
                  MOVE '#' TO POINT(Y-IDX,X-IDX)
                  IF Y-IDX > Y-MAX
                      MOVE Y-IDX TO Y-MAX
                  END-IF
                  IF X-IDX > X-MAX
                      MOVE X-IDX TO X-MAX
                  END-IF
           END-EVALUATE

           INITIALIZE WS-INPT
           PERFORM 8010-READ-INPT-DATA
           .

       2000-PROCESS-INPUT.
           IF PROCESS-TEST
               DISPLAY MYNAME ' before'
               CALL 'pointdmp' USING
                   Y-MAX
                   X-MAX
               END-CALL
           END-IF

           IF PROCESS-TEST OR NB-FOLDS > 0
               DISPLAY
                 MYNAME
                 SPACE FOLD-AXIS(FOLD-IDX)
                 SPACE FOLD-LINE(FOLD-IDX)
           END-IF

           MOVE ALL '.' TO POINT-TRANSFORM-TABLE
           COMPUTE FOLD-START = FOLD-LINE(FOLD-IDX) + 1
           IF FOLD-AXIS(FOLD-IDX) = 'x'
               PERFORM 2100-FOLD-X
                 VARYING Y-IDX FROM 1 BY 1
                 UNTIL Y-IDX > Y-MAX
                 AFTER X-IDX FROM FOLD-START BY 1
                 UNTIL X-IDX > X-MAX
               COMPUTE X-MAX = FOLD-LINE(FOLD-IDX) - 1
               IF PROCESS-TEST
                   DISPLAY MYNAME ' x max ' X-MAX
               END-IF
           ELSE
               PERFORM 2200-FOLD-Y
                 VARYING Y-IDX FROM FOLD-START BY 1
                 UNTIL Y-IDX > Y-MAX
                 AFTER X-IDX FROM 1 BY 1
                 UNTIL X-IDX > X-MAX
               COMPUTE Y-MAX = FOLD-LINE(FOLD-IDX) - 1
               IF PROCESS-TEST
                   DISPLAY MYNAME ' y max ' Y-MAX
               END-IF
           END-IF

           MOVE POINT-TRANSFORM-TABLE TO POINT-TABLE

           IF PROCESS-TEST
               DISPLAY MYNAME ' after'
               CALL 'pointdmp' USING
                   Y-MAX
                   X-MAX
               END-CALL
           END-IF
           .

       2100-FOLD-X.
           COMPUTE T-IDX = 
             FOLD-LINE(FOLD-IDX) - (X-IDX - FOLD-LINE(FOLD-IDX))
           EVALUATE POINT(Y-IDX,T-IDX) ALSO POINT(Y-IDX,X-IDX)
             WHEN   '.'                ALSO '#'
             WHEN   '#'                ALSO '.'
             WHEN   '#'                ALSO '#'
                    MOVE '#' TO POINT-T(Y-IDX,T-IDX)
             WHEN   OTHER
                    CONTINUE
           END-EVALUATE
           .

       2200-FOLD-Y.
           COMPUTE T-IDX = 
             FOLD-LINE(FOLD-IDX) - (Y-IDX - FOLD-LINE(FOLD-IDX))
           EVALUATE POINT(T-IDX,X-IDX) ALSO POINT(Y-IDX,X-IDX)
             WHEN   '.'                ALSO '#'
             WHEN   '#'                ALSO '.'
             WHEN   '#'                ALSO '#'
                    MOVE '#' TO POINT-T(T-IDX,X-IDX)
             WHEN   OTHER
                    CONTINUE
           END-EVALUATE
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .

       END PROGRAM cs13a.

       ID Division.
       Program-ID. pointdmp.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'pointdmp'.

       01  WORK-AREAS.
           05  Y-IDX              PIC S9(009) COMP   VALUE 0.
           05  X-IDX              PIC S9(009) COMP   VALUE 0.

       01  POINT-TABLE EXTERNAL.
           05  POINT-Y OCCURS 2000.
               10  POINT-X OCCURS 2000.
                   15  POINT      PIC X(001).

       Linkage Section.
       01  Y-MAX                  PIC S9(009) COMP.
       01  X-MAX                  PIC S9(009) COMP.
       Procedure Division Using
           Y-MAX X-MAX
         .
           PERFORM VARYING Y-IDX FROM 1 BY 1
           UNTIL Y-IDX > Y-MAX
             DISPLAY MYNAME SPACE WITH NO ADVANCING
             PERFORM VARYING X-IDX FROM 1 BY 1
             UNTIL X-IDX > X-MAX
               DISPLAY POINT(Y-IDX,X-IDX)
                 WITH NO ADVANCING
             END-PERFORM
             DISPLAY SPACE
           END-PERFORM
           .

       END PROGRAM pointdmp.

