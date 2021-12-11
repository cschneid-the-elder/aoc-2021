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
      * This solves both the first and second puzzles for day 11. 
      * 
      * 
       Program-ID. cs11a.
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
           05  MYNAME             PIC X(008)         VALUE 'cs11a'.
           05  WS-INIT            PIC X(010)         VALUE 'initial'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  NB-STEPS           PIC 9(009) COMP    VALUE 0.
           05  ROW-IDX            PIC S9(009) COMP   VALUE 0.
           05  COL-IDX            PIC S9(009) COMP   VALUE 0.
           05  STEP-COUNT         PIC 9(018)  COMP   VALUE 0.
           05  NB-STEPS-X         PIC X(004)         VALUE LOW-VALUES.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  ALL-FLASHED-SW     PIC X(001)         VALUE 'N'.
               88  ALL-FLASHED                       VALUE 'Y'
                                                     FALSE 'N'.

       01  OCTOPUS-TABLE EXTERNAL.
           05  OCTO-ROW OCCURS 10 TIMES.
               10  OCTO-COL OCCURS 10 TIMES.
                   15  OCTO-E-LVL      PIC 9(004) COMP.
                   15  OCTO-FLASHED-SW PIC X(001).
                       88  OCTO-FLASHED              VALUE 'Y'
                                                     FALSE 'N'.
       77  TOTAL-FLASHES EXTERNAL PIC 9(018).
       77  PROCESS-TYPE  EXTERNAL PIC X(004).

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           INITIALIZE TOTAL-FLASHES

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE NB-STEPS-X
           END-UNSTRING

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           MOVE FUNCTION NUMVAL(NB-STEPS-X) TO NB-STEPS

           DISPLAY MYNAME ' number of steps ' NB-STEPS

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           PERFORM 2000-PROCESS-INPUT NB-STEPS TIMES

           DISPLAY MYNAME ' total flashes   ' TOTAL-FLASHES
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           GOBACK.

       1000-LOAD-INPUT.
           ADD 1 TO ROW-IDX
           MOVE 0 TO COL-IDX
           PERFORM UNTIL COL-IDX > 10
             ADD 1 TO COL-IDX
             MOVE FUNCTION NUMVAL(WS-INPT(COL-IDX:1))
               TO OCTO-E-LVL(ROW-IDX,COL-IDX)
             SET  OCTO-FLASHED(ROW-IDX,COL-IDX) TO FALSE
           END-PERFORM

           PERFORM 8010-READ-INPT-DATA
           .

       2000-PROCESS-INPUT.
           PERFORM VARYING ROW-IDX FROM 1 BY 1
           UNTIL ROW-IDX > 10
           AFTER COL-IDX FROM 1 BY 1
           UNTIL COL-IDX > 10
             ADD 1 TO OCTO-E-LVL(ROW-IDX,COL-IDX)
           END-PERFORM

           IF PROCESS-TEST
               DISPLAY ' intermediate after adding 1'
               CALL 'octodump' USING MYNAME END-CALL
           END-IF

           PERFORM VARYING ROW-IDX FROM 1 BY 1
           UNTIL ROW-IDX > 10
           AFTER COL-IDX FROM 1 BY 1
           UNTIL COL-IDX > 10
             IF OCTO-E-LVL(ROW-IDX,COL-IDX) > 9
                 CALL 'flasher' USING
                   ROW-IDX
                   COL-IDX
                   WS-INIT
                 END-CALL
             END-IF
           END-PERFORM

           IF PROCESS-TEST
               DISPLAY MYNAME ' intermediate after calling flasher'
               CALL 'octodump' USING MYNAME END-CALL
           END-IF

           ADD 1 TO STEP-COUNT
           SET ALL-FLASHED TO TRUE
           PERFORM VARYING ROW-IDX FROM 1 BY 1
           UNTIL ROW-IDX > 10
           AFTER COL-IDX FROM 1 BY 1
           UNTIL COL-IDX > 10
             IF OCTO-FLASHED(ROW-IDX,COL-IDX)
                 MOVE 0 TO OCTO-E-LVL(ROW-IDX,COL-IDX)
                 SET  OCTO-FLASHED(ROW-IDX,COL-IDX) TO FALSE
             ELSE
                 SET ALL-FLASHED TO FALSE
             END-IF
           END-PERFORM

           IF ALL-FLASHED
               DISPLAY MYNAME ' all flashed on step ' STEP-COUNT
           END-IF

           IF PROCESS-TEST
               DISPLAY MYNAME
               CALL 'octodump' USING MYNAME END-CALL
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

       END PROGRAM cs11a.

       ID Division.
       Program-ID. flasher Recursive.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'flasher'.
           05  WS-NORTH           PIC X(010) VALUE 'north'.
           05  WS-NORTHEAST       PIC X(010) VALUE 'northeast'.
           05  WS-EAST            PIC X(010) VALUE 'east'.
           05  WS-SOUTHEAST       PIC X(010) VALUE 'southeast'.
           05  WS-SOUTH           PIC X(010) VALUE 'south'.
           05  WS-SOUTHWEST       PIC X(010) VALUE 'southwest'.
           05  WS-WEST            PIC X(010) VALUE 'west'.
           05  WS-NORTHWEST       PIC X(010) VALUE 'northwest'.

       01  OCTOPUS-TABLE EXTERNAL.
           05  OCTO-ROW OCCURS 10 TIMES.
               10  OCTO-COL OCCURS 10 TIMES.
                   15  OCTO-E-LVL      PIC 9(004) COMP.
                   15  OCTO-FLASHED-SW PIC X(001).
                       88  OCTO-FLASHED              VALUE 'Y'
                                                     FALSE 'N'.
       77  TOTAL-FLASHES EXTERNAL PIC 9(018).
       77  PROCESS-TYPE  EXTERNAL PIC X(004).

       Local-Storage Section.
       01  WORK-AREAS.
           05  INIT-ROW-IDX       PIC S9(009) COMP   VALUE 0.
           05  INIT-COL-IDX       PIC S9(009) COMP   VALUE 0.
           05  ROW-IDX            PIC S9(009) COMP   VALUE 0.
           05  COL-IDX            PIC S9(009) COMP   VALUE 0.

       Linkage Section.
       01  LS-ROW                 PIC S9(009) COMP.
       01  LS-COL                 PIC S9(009) COMP.
       01  LS-DIR                 PIC X(010).

       Procedure Division Using
           LS-ROW
           LS-COL
           LS-DIR
         .

           MOVE LS-ROW TO INIT-ROW-IDX
           MOVE LS-COL TO INIT-COL-IDX

           IF PROCESS-TYPE = 'test'
               DISPLAY MYNAME ' called with '
                 INIT-ROW-IDX ' ' INIT-COL-IDX
                 ' ' LS-DIR
           END-IF

           IF INIT-ROW-IDX < 1  OR INIT-COL-IDX < 1
           OR INIT-ROW-IDX > 10 OR INIT-COL-IDX > 10
               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' out of range goback'
               END-IF
               GOBACK
           END-IF

           ADD 1 TO OCTO-E-LVL(INIT-ROW-IDX,INIT-COL-IDX)
           IF OCTO-E-LVL(INIT-ROW-IDX,INIT-COL-IDX) > 9
               IF OCTO-FLASHED(INIT-ROW-IDX,INIT-COL-IDX)
                   IF PROCESS-TYPE = 'test'
                       DISPLAY MYNAME ' flashed goback'
                   END-IF
                   GOBACK
               ELSE
                   SET OCTO-FLASHED(INIT-ROW-IDX,INIT-COL-IDX) TO TRUE
                   ADD 1 TO TOTAL-FLASHES
               END-IF
           ELSE
               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' e-lvl <= 9 goback'
               END-IF
               GOBACK
           END-IF

      *    north
               COMPUTE ROW-IDX = INIT-ROW-IDX - 1
               COMPUTE COL-IDX = INIT-COL-IDX
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-NORTH
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after north with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    northeast
               COMPUTE ROW-IDX = INIT-ROW-IDX - 1
               COMPUTE COL-IDX = INIT-COL-IDX + 1
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-NORTHEAST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after northeast with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    east
               COMPUTE ROW-IDX = INIT-ROW-IDX
               COMPUTE COL-IDX = INIT-COL-IDX + 1
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-EAST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after east with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    southeast
               COMPUTE ROW-IDX = INIT-ROW-IDX + 1
               COMPUTE COL-IDX = INIT-COL-IDX + 1
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-SOUTHEAST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after southeast with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    south
               COMPUTE ROW-IDX = INIT-ROW-IDX + 1
               COMPUTE COL-IDX = INIT-COL-IDX
               CALL 'flasher' USING
                 ROW-IDX 
                 COL-IDX
                 WS-SOUTH
               END-CALL
 
               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after south with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    southwest
               COMPUTE ROW-IDX = INIT-ROW-IDX + 1
               COMPUTE COL-IDX = INIT-COL-IDX - 1
               CALL 'flasher' USING
                 ROW-IDX 
                 COL-IDX
                 WS-SOUTHWEST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after southwest with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    west
               COMPUTE ROW-IDX = INIT-ROW-IDX
               COMPUTE COL-IDX = INIT-COL-IDX - 1
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-WEST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after west with '
                     INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

      *    northwest
               COMPUTE ROW-IDX = INIT-ROW-IDX - 1
               COMPUTE COL-IDX = INIT-COL-IDX - 1
               CALL 'flasher' USING
                 ROW-IDX
                 COL-IDX
                 WS-NORTHWEST
               END-CALL

               IF PROCESS-TYPE = 'test'
                   DISPLAY MYNAME ' after northwest with ' 
                    INIT-ROW-IDX ' ' INIT-COL-IDX
                   CALL 'octodump' USING MYNAME END-CALL
               END-IF

           IF PROCESS-TYPE = 'test'
               DISPLAY MYNAME ' goback with '
                 INIT-ROW-IDX ' ' INIT-COL-IDX
           END-IF
           GOBACK.

       END PROGRAM flasher.

       ID Division.
       Program-ID. octodump.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'octodump'.

       01  WORK-AREAS.
           05  ROW-IDX            PIC S9(009) COMP   VALUE 0.
           05  COL-IDX            PIC S9(009) COMP   VALUE 0.

       01  OCTOPUS-TABLE EXTERNAL.
           05  OCTO-ROW OCCURS 10 TIMES.
               10  OCTO-COL OCCURS 10 TIMES.
                   15  OCTO-E-LVL      PIC 9(004) COMP.
                   15  OCTO-FLASHED-SW PIC X(001).
                       88  OCTO-FLASHED              VALUE 'Y'
                                                     FALSE 'N'.
       77  TOTAL-FLASHES EXTERNAL PIC 9(018).

       Linkage Section.
       01  LS-NAME                PIC X(008).
       Procedure Division Using
           LS-NAME
         .
           PERFORM VARYING ROW-IDX FROM 1 BY 1
           UNTIL ROW-IDX > 10
             DISPLAY LS-NAME WITH NO ADVANCING
             PERFORM VARYING COL-IDX FROM 1 BY 1
             UNTIL COL-IDX > 10
               DISPLAY SPACE OCTO-E-LVL(ROW-IDX,COL-IDX)
                 WITH NO ADVANCING
             END-PERFORM
             DISPLAY SPACE
           END-PERFORM
           .

       END PROGRAM octodump.

