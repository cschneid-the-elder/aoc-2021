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
       Program-ID. cs02b.
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
           05  MYNAME             PIC X(008) VALUE 'cs02b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(008) COMP    VALUE 0.
           05  CURR-DEPTH         PIC 9(008) COMP    VALUE 0.
           05  CURR-HPOSN         PIC 9(008) COMP    VALUE 0.
           05  CURR-AIM           PIC 9(008) COMP    VALUE 0.
           05  CURR-PRODUCT       PIC 9(016) COMP    VALUE 0.
           05  OPERATION-ARG      PIC 9(008) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  OPERATION          PIC X(008)         VALUE LOW-VALUES.
           05  OPERATION-ARG-X    PIC X(008)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(024)         VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.
           05  OPERATION-SW       PIC X(008)         VALUE LOW-VALUES.
               88  OPERATION-FORWARD                 VALUE 'FORWARD'.
               88  OPERATION-DOWN                    VALUE 'DOWN'.
               88  OPERATION-UP                      VALUE 'UP'.

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
             EVALUATE TRUE
               WHEN OPERATION-FORWARD
                    ADD OPERATION-ARG TO CURR-HPOSN
                    COMPUTE CURR-DEPTH =
                      CURR-DEPTH + (OPERATION-ARG * CURR-AIM)
               WHEN OPERATION-DOWN
                    ADD OPERATION-ARG TO CURR-AIM
               WHEN OPERATION-UP
                    SUBTRACT OPERATION-ARG FROM CURR-AIM
               WHEN OTHER
                          DISPLAY
                            MYNAME ' invalid operation ' OPERATION-SW
             END-EVALUATE
             PERFORM 8010-READ-INPT-DATA
           END-PERFORM

           CLOSE INPT-DATA

           DISPLAY
            MYNAME
            ' current horizontal position '
            CURR-HPOSN

           DISPLAY
            MYNAME
            ' current depth '
            CURR-DEPTH

           COMPUTE CURR-PRODUCT = CURR-HPOSN * CURR-DEPTH
           DISPLAY
            MYNAME
            ' product of position and depth '
            CURR-PRODUCT

           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
               UNSTRING WS-INPT
                 DELIMITED ALL SPACE OR ALL LOW-VALUE
                 INTO OPERATION OPERATION-ARG-X
               END-UNSTRING
               MOVE FUNCTION NUMVAL(OPERATION-ARG-X)
                 TO OPERATION-ARG
               MOVE FUNCTION UPPER-CASE(OPERATION)
                 TO OPERATION-SW
           END-READ

           .


