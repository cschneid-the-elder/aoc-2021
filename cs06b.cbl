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
       Program-ID. cs06b.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(1024).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'cs06b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  DAY-COUNT          PIC 9(009) COMP    VALUE 0.
           05  NB-DAYS            PIC 9(009) COMP    VALUE 0.
           05  UNSTRING-PTR       PIC 9(009) COMP    VALUE 1.
           05  FISH-SUB           PIC 9(009) COMP    VALUE 0.
           05  TOTAL-FISH         PIC 9(018) COMP    VALUE 0.
           05  FISH-SWAP          PIC 9(018) COMP    VALUE 0.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  FISH-X             PIC X(001)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  NB-DAYS-X          PIC X(004)         VALUE LOW-VALUES.
           05  FISH-SUB-OUT       PIC 9.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(1024)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  FISH-TABLE.
           05  FISH-TBL
               OCCURS 10.
               10  FISH           PIC 9(018) COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE NB-DAYS-X
           END-UNSTRING

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           MOVE FUNCTION NUMVAL(NB-DAYS-X) TO NB-DAYS

           DISPLAY MYNAME ' number of days ' NB-DAYS

           IF PROCESS-TEST
               READY TRACE
           END-IF

           INITIALIZE FISH-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL FISH-X = SPACES

           CLOSE INPT-DATA

           PERFORM VARYING FISH-SUB FROM 2 BY 1
           UNTIL FISH-SUB > 9
             ADD FISH(FISH-SUB) TO TOTAL-FISH
           END-PERFORM
           DISPLAY MYNAME ' initial number of fish ' TOTAL-FISH

           PERFORM 9010-DUMP-FISH-TABLE

           PERFORM 2000-PROCESS-ONE-DAY
             VARYING DAY-COUNT FROM 1 BY 1
             UNTIL DAY-COUNT > NB-DAYS

           MOVE 0 TO TOTAL-FISH
           PERFORM VARYING FISH-SUB FROM 1 BY 1
           UNTIL FISH-SUB > 9
             ADD FISH(FISH-SUB) TO TOTAL-FISH
           END-PERFORM
           SUBTRACT 1 FROM DAY-COUNT
           DISPLAY MYNAME ' number of fish ' TOTAL-FISH
             ' after ' DAY-COUNT ' days'
           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE
           GOBACK.

       1000-LOAD-INPUT.
           MOVE SPACES TO FISH-X
           UNSTRING WS-INPT
             DELIMITED ','
             INTO FISH-X
             POINTER UNSTRING-PTR
           END-UNSTRING

           IF FISH-X NOT = SPACES
               COMPUTE FISH-SUB = FUNCTION NUMVAL(FISH-X) + 1
               ADD 1 TO FISH(FISH-sub)
           END-IF
           .

       2000-PROCESS-ONE-DAY.
           IF PROCESS-TEST
               RESET TRACE
           END-IF          

           MOVE FISH(1) TO FISH-SWAP
           PERFORM VARYING FISH-SUB FROM 2 BY 1
           UNTIL FISH-SUB > 9
             MOVE FISH(FISH-SUB) TO FISH(FISH-SUB - 1)
           END-PERFORM
           MOVE 0 TO FISH(9)
           ADD FISH-SWAP TO FISH(7) FISH(9) 

           IF PROCESS-TEST
               PERFORM 9010-DUMP-FISH-TABLE
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

       9010-DUMP-FISH-TABLE.
               DISPLAY MYNAME ' day ' DAY-COUNT SPACE WITH NO ADVANCING
               PERFORM VARYING FISH-SUB FROM 1 BY 1
               UNTIL FISH-SUB > 9
                 COMPUTE FISH-SUB-OUT = FISH-SUB - 1
                 DISPLAY FISH-SUB-OUT SPACE FISH(FISH-SUB) ','
                   WITH NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           .
