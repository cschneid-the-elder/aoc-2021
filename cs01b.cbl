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
       Program-ID. cs01b.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(008).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'cs01b'.

       01  WORK-AREAS.
           05  WS-COUNT           PIC 9(008) COMP    VALUE 0.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  DEPTH-LEN          PIC 9(008) COMP    VALUE 0.
           05  DEPTH-INDX-MAX     PIC 9(008) COMP    VALUE 0.
           05  SLIDING-COUNT      PIC 9(008) COMP    VALUE 0.
           05  WHICH-INDX         PIC 9(008) COMP    VALUE 0.
           05  WHICH-INDX-R       PIC 9(008) COMP    VALUE 0.
           05  HOLD-DEPTH         PIC 9(008)         VALUE 0.
           05  CURR-DEPTH         PIC 9(008)         VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT-DEPTH      PIC X(008)         VALUE SPACES.
           05  WS-INPT-DEPTH-R
               REDEFINES
               WS-INPT-DEPTH.
               10                 PIC X(003).
               10  WS-INPT-BYTE-4 PIC X(001).
               10                 PIC X(004).

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  SLIDING-WINDOW-TABLE.
          05  DEPTH-TBL
              OCCURS 2000
              INDEXED
                DEPTH-INDX
                DEPTH-INDX1
                DEPTH-INDX2
                DEPTH-INDX3.
              10  SLIDING-DEPTH   PIC 9(008) COMP.

       Procedure Division.
           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA
           SET DEPTH-INDX1 TO 1
           SET DEPTH-INDX2 TO 2
           SET DEPTH-INDX3 TO 3

           PERFORM UNTIL INPT-DATA-EOF
             ADD 1 TO SLIDING-COUNT
             EVALUATE SLIDING-COUNT
               WHEN 1 PERFORM 1010-INCREMENT-GROUP-1
               WHEN 2 PERFORM 1020-INCREMENT-GROUP-2
               WHEN 3 PERFORM 1030-INCREMENT-GROUP-3
                      ADD 1 TO DEPTH-INDX-MAX
               WHEN OTHER
                          ADD 1 TO DEPTH-INDX-MAX
                          DIVIDE SLIDING-COUNT BY 3
                            GIVING WHICH-INDX
                            REMAINDER WHICH-INDX-R
                          EVALUATE WHICH-INDX-R
                            WHEN 1 SET DEPTH-INDX1 UP BY 3
                            WHEN 2 SET DEPTH-INDX2 UP BY 3
                            WHEN 0 SET DEPTH-INDX3 UP BY 3
                          END-EVALUATE
                          IF DEPTH-INDX1 > 2000
                              DISPLAY MYNAME ' internal table overflow'
                              CLOSE INPT-DATA
                              MOVE 8 TO RETURN-CODE
                              GOBACK
                          END-IF
                          PERFORM 1030-INCREMENT-GROUP-3
             END-EVALUATE
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           MOVE SLIDING-DEPTH(1) TO HOLD-DEPTH

           PERFORM VARYING DEPTH-INDX FROM 1 BY 1
           UNTIL DEPTH-INDX > DEPTH-INDX-MAX
             IF SLIDING-DEPTH(DEPTH-INDX) > HOLD-DEPTH
                 ADD 1 TO WS-COUNT
             END-IF
             MOVE SLIDING-DEPTH(DEPTH-INDX) TO HOLD-DEPTH
           END-PERFORM

           DISPLAY
            MYNAME
            ' measurements larger than the previous measurement '
             WS-COUNT

           DISPLAY MYNAME ' records read ' WS-REC-COUNT
           DISPLAY MYNAME ' sliding window groups ' DEPTH-INDX-MAX

           IF PROCESS-TEST
               RESET TRACE
               PERFORM VARYING DEPTH-INDX FROM 1 BY 1
               UNTIL DEPTH-INDX > DEPTH-INDX-MAX
                 DISPLAY DEPTH-INDX ' ' SLIDING-DEPTH(DEPTH-INDX)
               END-PERFORM
           END-IF

           GOBACK.

       1010-INCREMENT-GROUP-1.
           ADD CURR-DEPTH TO SLIDING-DEPTH(DEPTH-INDX1)
           .

       1020-INCREMENT-GROUP-2.
           PERFORM 1010-INCREMENT-GROUP-1
           ADD CURR-DEPTH TO SLIDING-DEPTH(DEPTH-INDX2)
           .

       1030-INCREMENT-GROUP-3.
           PERFORM 1020-INCREMENT-GROUP-2
           ADD CURR-DEPTH TO SLIDING-DEPTH(DEPTH-INDX3)
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
               IF WS-INPT-BYTE-4 NUMERIC
                   MOVE 4 TO DEPTH-LEN
               ELSE
                   MOVE 3 TO DEPTH-LEN
               END-IF
               MOVE FUNCTION NUMVAL(WS-INPT-DEPTH(1:DEPTH-LEN))
                 TO CURR-DEPTH
           END-READ

           .


