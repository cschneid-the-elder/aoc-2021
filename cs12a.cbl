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
      * Still doesn't work, but at least I got past the compile errors.
      * 
       Function-ID. caveidx.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'caveidx'.

       01  EXTERNAL-WORK-AREAS EXTERNAL.
           05  CONNECTION-COUNT   PIC 9(009) COMP.
           05  PATH-COUNT         PIC 9(009) COMP.
           05  CAVE-COUNT         PIC 9(009) COMP.
           05  CURR-PATH          PIC 9(009) COMP.

       77  PROCESS-SW  EXTERNAL   PIC X(004).
           88  PROCESS-TEST                          VALUE 'TEST'.

       01  CAVE-TABLE EXTERNAL.
           05  CAVE-TBL OCCURS 25.
               10  CAVE           PIC X(005).
               10  CAVE-SW        PIC X(001).
                   88  CAVE-IS-BIG               VALUE 'B'.
                   88  CAVE-IS-SMALL             VALUE 'S'.
               10  CAVE-MAX-VISIT PIC 9(009) COMP.
               10  CAVE-CONN-COUNT PIC 9(009) COMP.
               10  CAVE-CONN-TBL OCCURS 10.
                   15  CAVE-CONN     PIC X(005).
                   15  CAVE-CONN-IDX PIC 9(009) COMP.

       Local-Storage Section.
       01  WORK-AREAS.
           05  FOUND-SW           PIC X(001)         VALUE 'N'.
               88  FOUND-IT                          VALUE 'Y'.

       Linkage Section.
       01  LS-CAVE                PIC X(005).
       01  CAVE-IDX               PIC S9(009) COMP   VALUE 0.
       Procedure Division Using
           LS-CAVE
         Returning CAVE-IDX
         .

           PERFORM VARYING CAVE-IDX FROM 1 BY 1
           UNTIL CAVE-IDX > CAVE-COUNT
             IF CAVE(CAVE-IDX) = LS-CAVE
                 SET FOUND-IT TO TRUE
                 EXIT PERFORM
             END-IF
           END-PERFORM

           IF FOUND-IT
               CONTINUE
           ELSE
               MOVE 9999 TO CAVE-IDX
           END-IF

           GOBACK
           .

       END FUNCTION caveidx.

       ID Division.
       Program-ID. cs12a.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION caveidx
           FUNCTION ALL INTRINSIC.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(4096).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'cs12a'.
           05  START-CAVE         PIC X(005)         VALUE 'start'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.

       01  FIND-PATH-AREAS.
           05  CONN-IDX           PIC 9(009) COMP    VALUE 0.
           05  PATH-IDX           PIC 9(009) COMP    VALUE 0.
           05  CAVE-IDX           PIC 9(009) COMP    VALUE 0.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.

       01  EXTERNAL-WORK-AREAS EXTERNAL.
           05  CONNECTION-COUNT   PIC 9(009) COMP.
           05  PATH-COUNT         PIC 9(009) COMP.
           05  CAVE-COUNT         PIC 9(009) COMP.
           05  CURR-PATH          PIC 9(009) COMP.

       77  PROCESS-SW  EXTERNAL   PIC X(004).
           88  PROCESS-TEST                          VALUE 'TEST'.

       01  CONNECTION-TABLE EXTERNAL.
           05  CONN-TBL OCCURS 25.
               10  CONN-CAVE-1    PIC X(005).
               10  CONN-CAVE-2    PIC X(005).

       01  CAVE-TABLE EXTERNAL.
           05  CAVE-TBL OCCURS 25.
               10  CAVE           PIC X(005).
               10  CAVE-SW        PIC X(001).
                   88  CAVE-IS-BIG               VALUE 'B'.
                   88  CAVE-IS-SMALL             VALUE 'S'.
               10  CAVE-MAX-VISIT PIC 9(009) COMP.
               10  CAVE-CONN-COUNT PIC 9(009) COMP.
               10  CAVE-CONN-TBL OCCURS 10.
                   15  CAVE-CONN     PIC X(005).
                   15  CAVE-CONN-IDX PIC 9(009) COMP.

       01  PATH-TABLE EXTERNAL.
           05  PATH OCCURS 1000.
               10  PATH-LN            PIC 9(009).
               10  PATH-CAVE
                   OCCURS 10
                                      PIC X(005).

       Procedure Division.
           DISPLAY MYNAME SPACE CURRENT-DATE

           INITIALIZE 
             CONNECTION-TABLE 
             PATH-TABLE 
             CAVE-TABLE
             EXTERNAL-WORK-AREAS

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE
           END-UNSTRING

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           PERFORM 2000-CONNECT-CAVES
             VARYING CONN-IDX FROM 1 BY 1
             UNTIL CONN-IDX > CONNECTION-COUNT

           CALL 'cavedump'

           PERFORM 3000-PROCESS-INPUT

           PERFORM VARYING PATH-IDX FROM 1 BY 1
           UNTIL PATH-IDX > PATH-COUNT
             DISPLAY MYNAME ' path ' PATH-IDX
               WITH NO ADVANCING
             PERFORM VARYING CAVE-IDX FROM 1 BY 1
             UNTIL CAVE-IDX > PATH-LN(PATH-IDX)
               DISPLAY SPACE PATH-CAVE(PATH-IDX,CAVE-IDX)
                 WITH NO ADVANCING
             END-PERFORM
             DISPLAY SPACE
           END-PERFORM

           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           GOBACK.

       1000-LOAD-INPUT.
           ADD 1 TO CONNECTION-COUNT
           ADD 1 TO CONN-IDX

           UNSTRING WS-INPT DELIMITED SPACE OR '-'
             INTO 
               CONN-CAVE-1(CONN-IDX)
               CONN-CAVE-2(CONN-IDX)
           END-UNSTRING

           PERFORM 1100-ADD-CAVES

           PERFORM 8010-READ-INPT-DATA
           .

       1100-ADD-CAVES.
           MOVE caveidx(CONN-CAVE-1(CONN-IDX)) TO CAVE-IDX
           IF CAVE-IDX = 9999
               ADD 1 TO CAVE-COUNT
               MOVE CONN-CAVE-1(CONN-IDX) TO CAVE(CAVE-COUNT)
               PERFORM 1010-SET-CAVE-SIZE
           END-IF

           MOVE caveidx(CONN-CAVE-2(CONN-IDX)) TO CAVE-IDX
           IF CAVE-IDX = 9999
               ADD 1 TO CAVE-COUNT
               MOVE CONN-CAVE-2(CONN-IDX) TO CAVE(CAVE-COUNT)
               PERFORM 1010-SET-CAVE-SIZE
           END-IF
           .

       1010-SET-CAVE-SIZE.
           IF CAVE(CAVE-COUNT) = FUNCTION UPPER-CASE(CAVE(CAVE-COUNT))
               SET CAVE-IS-BIG(CAVE-COUNT) TO TRUE
               MOVE 9999 TO CAVE-MAX-VISIT(CAVE-COUNT)
           ELSE
               SET CAVE-IS-SMALL(CAVE-COUNT) TO TRUE
               MOVE 1 TO CAVE-MAX-VISIT(CAVE-COUNT)
           END-IF
           .

       2000-CONNECT-CAVES.
           MOVE caveidx(CONN-CAVE-1(CONN-IDX)) TO CAVE-IDX
           IF CONN-CAVE-2(CONN-IDX) NOT = 'start'
              ADD  1 TO CAVE-CONN-COUNT(CAVE-IDX)
               MOVE CONN-CAVE-2(CONN-IDX)
                 TO CAVE-CONN(CAVE-IDX,CAVE-CONN-COUNT(CAVE-IDX))
              MOVE caveidx(CONN-CAVE-2(CONN-IDX)) 
                 TO CAVE-CONN-IDX(CAVE-IDX,CAVE-CONN-COUNT(CAVE-IDX))
           END-IF

           MOVE caveidx(CONN-CAVE-2(CONN-IDX)) TO CAVE-IDX
           IF CONN-CAVE-1(CONN-IDX) NOT = 'start'
               ADD  1 TO CAVE-CONN-COUNT(CAVE-IDX)
               MOVE CONN-CAVE-1(CONN-IDX)
                 TO CAVE-CONN(CAVE-IDX,CAVE-CONN-COUNT(CAVE-IDX))
               MOVE caveidx(CONN-CAVE-1(CONN-IDX)) 
                 TO CAVE-CONN-IDX(CAVE-IDX,CAVE-CONN-COUNT(CAVE-IDX))
           END-IF
           .

       3000-PROCESS-INPUT.
           INITIALIZE FIND-PATH-AREAS
           MOVE 1 TO PATH-IDX
           MOVE caveidx(START-CAVE) TO CAVE-IDX

           CALL 'findpath' USING
             FIND-PATH-AREAS
           END-CALL

           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .

       END PROGRAM cs12a.

       ID Division.
       Program-ID. findpath Recursive.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION caveidx
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'findpath'.
           05  START-CAVE         PIC X(005)         VALUE 'start'.
           05  END-CAVE           PIC X(005)         VALUE 'end  '.

       01  EXTERNAL-WORK-AREAS EXTERNAL.
           05  CONNECTION-COUNT   PIC 9(009) COMP.
           05  PATH-COUNT         PIC 9(009) COMP.
           05  CAVE-COUNT         PIC 9(009) COMP.
           05  CURR-PATH          PIC 9(009) COMP.

       77  PROCESS-SW  EXTERNAL   PIC X(004).
           88  PROCESS-TEST                          VALUE 'TEST'.

       01  CONNECTION-TABLE EXTERNAL.
           05  CONN-TBL OCCURS 25.
               10  CONN-CAVE-1    PIC X(005).
               10  CONN-CAVE-2    PIC X(005).

       01  CAVE-TABLE EXTERNAL.
           05  CAVE-TBL OCCURS 25.
               10  CAVE           PIC X(005).
               10  CAVE-SW        PIC X(001).
                   88  CAVE-IS-BIG               VALUE 'B'.
                   88  CAVE-IS-SMALL             VALUE 'S'.
               10  CAVE-MAX-VISIT PIC 9(009) COMP.
               10  CAVE-CONN-COUNT PIC 9(009) COMP.
               10  CAVE-CONN-TBL OCCURS 10.
                   15  CAVE-CONN     PIC X(005).
                   15  CAVE-CONN-IDX PIC 9(009) COMP.

       01  PATH-TABLE EXTERNAL.
           05  PATH OCCURS 1000.
               10  PATH-LN            PIC 9(009).
               10  PATH-CAVE
                   OCCURS 10
                                      PIC X(005).

       Local-Storage Section.
       01  FIND-PATH-AREAS.
           05  CONN-IDX           PIC 9(009) COMP    VALUE 0.
           05  PATH-IDX           PIC 9(009) COMP    VALUE 0.
           05  CAVE-IDX           PIC 9(009) COMP    VALUE 0.

       01  WORK-AREAS.
           05  TEMP-IDX           PIC 9(009) COMP    VALUE 0.

       01  CAVE-IN-PATH-SW        PIC X(001).
           88  CAVE-IN-PATH                          VALUE 'Y'
                                                     FALSE 'N'.

       Linkage Section.
       01  INPT-AREAS.
           05  LS-CONN-IDX        PIC 9(009) COMP.
           05  LS-PATH-IDX        PIC 9(009) COMP.
           05  LS-CAVE-IDX        PIC 9(009) COMP.

       Procedure Division Using
           INPT-AREAS
           .

           MOVE INPT-AREAS TO FIND-PATH-AREAS

           IF CAVE-CONN(CAVE-IDX,CONN-IDX) = END-CAVE
               DISPLAY MYNAME ' path ' PATH-IDX
                 WITH NO ADVANCING
               PERFORM VARYING TEMP-IDX FROM 1 BY 1
               UNTIL TEMP-IDX > PATH-LN(PATH-IDX)
                 DISPLAY SPACE PATH-CAVE(PATH-IDX,TEMP-IDX)
               END-PERFORM
               DISPLAY ' '
               ADD 1 TO PATH-IDX
               GOBACK
           END-IF

           PERFORM 1000-PROCESS-CONNECTIONS
             VARYING CONN-IDX FROM 1 BY 1
             UNTIL CONN-IDX > CAVE-CONN-COUNT(CAVE-IDX)

           GOBACK
           .

       1000-PROCESS-CONNECTIONS.
           MOVE caveidx(CAVE-CONN(CAVE-IDX,CONN-IDX))
             TO TEMP-IDX
 
           CALL 'cavinpth' USING
               PATH-IDX
               CAVE-CONN(CAVE-IDX,CONN-IDX)
               CAVE-IN-PATH-SW
           END-CALL

           EVALUATE TRUE                    ALSO TRUE
             WHEN   CAVE-IS-BIG(TEMP-IDX)   ALSO ANY
             WHEN   CAVE-IS-SMALL(TEMP-IDX) ALSO NOT CAVE-IN-PATH
                    ADD 1 TO PATH-LN(PATH-IDX)
                    MOVE CAVE(CAVE-IDX)
                      TO PATH-CAVE(PATH-IDX,PATH-LN(PATH-IDX))
                    CALL 'findpath' USING
                        FIND-PATH-AREAS
                    END-CALL
           END-EVALUATE
           .

       END PROGRAM findpath.

       ID Division.
       Program-ID. cavinpth.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'cavinpth'.

       01  WORK-AREAS.
           05  CAVE-IDX           PIC 9(009) COMP    VALUE 0.
           05  CONN-IDX           PIC 9(009) COMP    VALUE 0.
           05  PATH-CAVE-IDX      PIC 9(009) COMP    VALUE 0.

       77  PROCESS-SW  EXTERNAL   PIC X(004).
           88  PROCESS-TEST                          VALUE 'TEST'.

       01  PATH-TABLE EXTERNAL.
           05  PATH OCCURS 1000.
               10  PATH-LN            PIC 9(009).
               10  PATH-CAVE
                   OCCURS 10
                                      PIC X(005).

       Linkage Section.
       01  LS-PATH-IDX            PIC 9(009) COMP.
       01  LS-CAVE                PIC X(005).
       01  LS-FOUND-SW            PIC X(001).
           88  FOUND-IT                              VALUE 'Y'
                                                     FALSE 'N'.

       Procedure Division Using
           LS-PATH-IDX
           LS-CAVE
           LS-FOUND-SW
           .

           SET FOUND-IT TO FALSE
           PERFORM VARYING PATH-CAVE-IDX FROM 1 BY 1
           UNTIL PATH-CAVE-IDX > PATH-LN(LS-PATH-IDX)
             IF PATH-CAVE(LS-PATH-IDX,PATH-CAVE-IDX) = LS-CAVE
                 SET FOUND-IT TO TRUE
                 EXIT PERFORM
             END-IF
           END-PERFORM

           GOBACK
           .

       END PROGRAM cavinpth.

       ID Division.
       Program-ID. cavedump.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'cavedump'.

       01  WORK-AREAS.
           05  CAVE-IDX           PIC 9(009) COMP    VALUE 0.
           05  CONN-IDX           PIC 9(009) COMP    VALUE 0.

       01  EXTERNAL-WORK-AREAS EXTERNAL.
           05  CONNECTION-COUNT   PIC 9(009) COMP.
           05  PATH-COUNT         PIC 9(009) COMP.
           05  CAVE-COUNT         PIC 9(009) COMP.
           05  CURR-PATH          PIC 9(009) COMP.

       77  PROCESS-SW  EXTERNAL   PIC X(004).
           88  PROCESS-TEST                          VALUE 'TEST'.

       01  CONNECTION-TABLE EXTERNAL.
           05  CONN-TBL OCCURS 25.
               10  CONN-CAVE-1    PIC X(005).
               10  CONN-CAVE-2    PIC X(005).

       01  CAVE-TABLE EXTERNAL.
           05  CAVE-TBL OCCURS 25.
               10  CAVE           PIC X(005).
               10  CAVE-SW        PIC X(001).
                   88  CAVE-IS-BIG               VALUE 'B'.
                   88  CAVE-IS-SMALL             VALUE 'S'.
               10  CAVE-MAX-VISIT PIC 9(009) COMP.
               10  CAVE-CONN-COUNT PIC 9(009) COMP.
               10  CAVE-CONN-TBL OCCURS 10.
                   15  CAVE-CONN     PIC X(005).
                   15  CAVE-CONN-IDX PIC 9(009) COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE CURRENT-DATE

           PERFORM VARYING CAVE-IDX FROM 1 BY 1
           UNTIL CAVE-IDX > CAVE-COUNT
             DISPLAY 
               MYNAME ' ' 
               CAVE-IDX ' ' 
               CAVE(CAVE-IDX) ' '
               CAVE-SW(CAVE-IDX) ' '
               CAVE-MAX-VISIT(CAVE-IDX) ' '
               WITH NO ADVANCING
             PERFORM VARYING CONN-IDX FROM 1 BY 1
             UNTIL CONN-IDX > CAVE-CONN-COUNT(CAVE-IDX)
               DISPLAY CAVE-CONN(CAVE-IDX,CONN-IDX) ' '
                 WITH NO ADVANCING
             END-PERFORM
             DISPLAY ' '
           END-PERFORM

           DISPLAY MYNAME SPACE CURRENT-DATE
           GOBACK
           .

       END PROGRAM cavedump.

