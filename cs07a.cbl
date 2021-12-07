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
       Program-ID. cs07a.
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
           05  MYNAME             PIC X(008) VALUE 'cs07a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  NB-DAYS            PIC 9(009) COMP    VALUE 0.
           05  UNSTRING-PTR       PIC 9(009) COMP    VALUE 1.
           05  NB-CRABS           PIC 9(009) COMP    VALUE 0.
           05  TOTAL-FUEL         PIC 9(018) COMP    VALUE 0.
           05  LOW-TOTAL-FUEL     PIC 9(018) COMP    VALUE 999999999999.
           05  HIGH-CRAB          PIC 9(004) COMP    VALUE 0.
           05  LOW-CRAB           PIC 9(004) COMP    VALUE 9999.
           05  HPOSN-DIFF         PIC S9(004) COMP   VALUE +0.
           05  HPOSN              PIC 9(004) COMP    VALUE 0.
           05  LOW-HPOSN          PIC 9(004) COMP    VALUE 0.
           05  CRAB-X             PIC X(004)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  CRAB-TABLE.
           05  CRAB-TBL
               OCCURS 1000
               INDEXED CRAB-INDX.
               10  CRAB           PIC 9(004) COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           INITIALIZE CRAB-TABLE
           SET CRAB-INDX TO 1

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL CRAB-X = SPACES

           CLOSE INPT-DATA

           DISPLAY MYNAME ' low crab        ' LOW-CRAB
           DISPLAY MYNAME ' high crab       ' HIGH-CRAB
           DISPLAY MYNAME ' nb crabs        ' NB-CRABS

           PERFORM 2000-PROCESS
             VARYING HPOSN FROM 0 BY 1
             UNTIL HPOSN > HIGH-CRAB + 1

           DISPLAY MYNAME ' low fuel hposn  ' LOW-HPOSN
           DISPLAY MYNAME ' low fuel amount ' LOW-TOTAL-FUEL
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE
           GOBACK.

       1000-LOAD-INPUT.
           MOVE SPACES TO CRAB-X
           UNSTRING WS-INPT
             DELIMITED ','
             INTO CRAB-X
             POINTER UNSTRING-PTR
           END-UNSTRING

           IF CRAB-X NOT = SPACES
               ADD 1 TO NB-CRABS
               SET CRAB-INDX TO NB-CRABS
               MOVE FUNCTION NUMVAL(CRAB-X) TO CRAB(CRAB-INDX)
               IF CRAB(CRAB-INDX) > HIGH-CRAB
                   MOVE CRAB(CRAB-INDX) TO HIGH-CRAB
               END-IF
               IF CRAB(CRAB-INDX) < LOW-CRAB
                   MOVE CRAB(CRAB-INDX) TO LOW-CRAB
               END-IF
           END-IF
           .

       2000-PROCESS.
           IF PROCESS-TEST
               RESET TRACE
               DISPLAY ' hposn ' HPOSN
           END-IF          

           MOVE 0 TO TOTAL-FUEL
           PERFORM VARYING CRAB-INDX FROM 1 BY 1
           UNTIL CRAB-INDX > NB-CRABS
             COMPUTE HPOSN-DIFF = CRAB(CRAB-INDX) - HPOSN
             IF PROCESS-TEST
                 DISPLAY MYNAME ' diff  ' FUNCTION ABS(HPOSN-DIFF)
             END-IF
             COMPUTE TOTAL-FUEL = 
               TOTAL-FUEL + FUNCTION ABS(HPOSN-DIFF)
           END-PERFORM

           IF TOTAL-FUEL < LOW-TOTAL-FUEL
               MOVE TOTAL-FUEL TO LOW-TOTAL-FUEL
               MOVE HPOSN TO LOW-HPOSN
           END-IF

           IF PROCESS-TEST
               DISPLAY ' total ' TOTAL-FUEL
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


