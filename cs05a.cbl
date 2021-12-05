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
       Program-ID. cs05a.
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
           05  MYNAME             PIC X(008) VALUE 'cs05a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  X1                 PIC 9(009) COMP    VALUE 0.
           05  Y1                 PIC 9(009) COMP    VALUE 0.
           05  X2                 PIC 9(009) COMP    VALUE 0.
           05  Y2                 PIC 9(009) COMP    VALUE 0.
           05  OVERLAP-COUNT      PIC 9(008) COMP    VALUE 0.
           05  CURR-AIM           PIC 9(008) COMP    VALUE 0.
           05  CURR-PRODUCT       PIC 9(016) COMP    VALUE 0.
           05  OPERATION-ARG      PIC 9(008) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  COORD-1            PIC X(008)         VALUE SPACES.
           05  COORD-2            PIC X(008)         VALUE SPACES.
           05  COORD-X1           PIC X(004)         VALUE SPACES.
           05  COORD-Y1           PIC X(004)         VALUE SPACES.
           05  COORD-X2           PIC X(004)         VALUE SPACES.
           05  COORD-Y2           PIC X(004)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(024)         VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  THE-OCEAN-FLOOR.
           05  Y-VAL OCCURS 1000 INDEXED Y-INDX1 Y-INDX2.
               10  X-VAL OCCURS 1000 INDEXED X-INDX1 X-INDX2.
                   15  POINT      PIC 9(004).

       Procedure Division.
           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           INITIALIZE THE-OCEAN-FLOOR

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT
             UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           PERFORM 9010-DUMP-THE-OCEAN-FLOOR

           PERFORM 2000-COUNT-INTERSECTIONS

           DISPLAY MYNAME ' overlap count ' OVERLAP-COUNT
           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       1000-LOAD-INPUT.
           UNSTRING WS-INPT
             DELIMITED '->'
             INTO COORD-1 COORD-2
           END-UNSTRING
           UNSTRING COORD-1
             DELIMITED ','
             INTO COORD-X1 COORD-Y1
           END-UNSTRING
           UNSTRING COORD-2
             DELIMITED ','
             INTO COORD-X2 COORD-Y2
           END-UNSTRING

           COMPUTE X1 = FUNCTION NUMVAL(COORD-X1) + 1
           COMPUTE Y1 = FUNCTION NUMVAL(COORD-Y1) + 1
           COMPUTE X2 = FUNCTION NUMVAL(COORD-X2) + 1
           COMPUTE Y2 = FUNCTION NUMVAL(COORD-Y2) + 1

           EVALUATE TRUE ALSO TRUE
             WHEN X1 = X2 ALSO Y1 < Y2
                  SET X-INDX1 TO X1
                  PERFORM 1100-PLOT-THE-LINE
                    VARYING Y-INDX1 FROM Y1 BY 1
                    UNTIL Y-INDX1 > Y2
             WHEN X1 = X2 ALSO Y1 > Y2
                  SET X-INDX1 TO X1
                  PERFORM 1100-PLOT-THE-LINE
                    VARYING Y-INDX1 FROM Y2 BY 1
                    UNTIL Y-INDX1 > Y1
             WHEN Y1 = Y2 ALSO X1 < X2
                  SET Y-INDX1 TO Y1
                  PERFORM 1100-PLOT-THE-LINE
                    VARYING X-INDX1 FROM X1 BY 1
                    UNTIL X-INDX1 > X2
             WHEN Y1 = Y2 ALSO X1 > X2
                  SET Y-INDX1 TO Y1
                  PERFORM 1100-PLOT-THE-LINE
                    VARYING X-INDX1 FROM X2 BY 1
                    UNTIL X-INDX1 > X1
             WHEN OTHER
                  DISPLAY MYNAME ' ignoring ' WS-INPT
           END-EVALUATE

           PERFORM 8010-READ-INPT-DATA
           .

       1100-PLOT-THE-LINE.
           ADD 1 TO POINT(Y-INDX1,X-INDX1)
           .

       2000-COUNT-INTERSECTIONS.
           IF PROCESS-TEST
               RESET TRACE
           END-IF          

           PERFORM VARYING Y-INDX1 FROM 1 BY 1 UNTIL Y-INDX1 > 1000
           AFTER X-INDX1 FROM 1 BY 1 UNTIL X-INDX1 > 1000
             IF POINT(Y-INDX1,X-INDX1) > 1
                 ADD 1 TO OVERLAP-COUNT
             END-IF
           END-PERFORM

           IF PROCESS-TEST
               READY TRACE
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

       9010-DUMP-THE-OCEAN-FLOOR.
           IF PROCESS-TEST
               RESET TRACE
               PERFORM VARYING Y-INDX1 FROM 1 BY 1
               UNTIL Y-INDX1 > 10
                 DISPLAY MYNAME SPACE WITH NO ADVANCING
                 PERFORM VARYING X-INDX1 FROM 1 BY 1
                 UNTIL X-INDX1 > 10
                   DISPLAY POINT(Y-INDX1,X-INDX1) SPACE
                     WITH NO ADVANCING
                 END-PERFORM
                 DISPLAY SPACE
               END-PERFORM
               READY TRACE
           END-IF
           .

