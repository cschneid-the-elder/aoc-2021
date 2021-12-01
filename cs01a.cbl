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
       Program-ID. cs01a.
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
           05  MYNAME             PIC X(008) VALUE 'cs01a'.

       01  WORK-AREAS.
           05  WS-COUNT           PIC 9(008) COMP    VALUE 0.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  DEPTH-LEN          PIC 9(008) COMP    VALUE 0.
           05  HOLD-DEPTH         PIC 9(008)         VALUE 0.
           05  CURR-DEPTH         PIC 9(008)         VALUE 0.

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

       Procedure Division.

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA
           MOVE CURR-DEPTH TO HOLD-DEPTH

           PERFORM UNTIL INPT-DATA-EOF
             IF CURR-DEPTH > HOLD-DEPTH
                 ADD 1 TO WS-COUNT
             END-IF
             MOVE CURR-DEPTH TO HOLD-DEPTH
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           DISPLAY
            MYNAME
            ' measurements larger than the previous measurement '
             WS-COUNT

           DISPLAY MYNAME ' records read ' WS-REC-COUNT
           GOBACK.

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


