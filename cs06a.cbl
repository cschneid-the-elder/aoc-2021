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
       Program-ID. cs06a.
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
           05  MYNAME             PIC X(008) VALUE 'cs06a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  DAY-COUNT          PIC 9(009) COMP    VALUE 0.
           05  NB-DAYS            PIC 9(009) COMP    VALUE 0.
           05  MAX-FISH           PIC 9(009) COMP    VALUE 1000000.
           05  NB-FISH            PIC 9(009) COMP    VALUE 0.
           05  UNSTRING-PTR       PIC 9(009) COMP    VALUE 1.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  FISH-X             PIC X(001)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  NB-DAYS-X          PIC X(004)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(1024)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  FISH-TABLE.
           05  FISH-TBL
               OCCURS 1000000
               INDEXED FISH-INDX.
               10  FISH           PIC S9(004) COMP.
               10  NEW-FISH-SW    PIC X(001).
                   88  NEW-FISH                      VALUE 'Y'
                                                     FALSE 'N'.

       Procedure Division.
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

           DISPLAY MYNAME ' initial number of fish ' NB-FISH

           PERFORM 2000-PROCESS-ONE-DAY
             VARYING DAY-COUNT FROM 1 BY 1
             UNTIL DAY-COUNT > NB-DAYS

           SUBTRACT 1 FROM DAY-COUNT
           DISPLAY MYNAME ' number of fish ' NB-FISH
             ' after ' DAY-COUNT ' days'
           DISPLAY MYNAME ' records read ' WS-REC-COUNT

           GOBACK.

       1000-LOAD-INPUT.
           MOVE SPACES TO FISH-X
           UNSTRING WS-INPT
             DELIMITED ','
             INTO FISH-X
             POINTER UNSTRING-PTR
           END-UNSTRING

           IF FISH-X NOT = SPACES
               ADD 1 TO NB-FISH
               SET FISH-INDX TO NB-FISH
               MOVE FUNCTION NUMVAL(FISH-X) TO FISH(FISH-INDX)
               SET NEW-FISH(FISH-INDX) TO FALSE
           END-IF
           .

       2000-PROCESS-ONE-DAY.
           IF PROCESS-TEST
               RESET TRACE
           END-IF          

           PERFORM VARYING FISH-INDX FROM 1 BY 1
           UNTIL FISH-INDX > NB-FISH
             EVALUATE FISH(FISH-INDX)
               WHEN 0
                    PERFORM 2010-RESET-FISH
               WHEN OTHER
                    IF NEW-FISH(FISH-INDX)
                        SET NEW-FISH(FISH-INDX) TO FALSE
                    ELSE
                        SUBTRACT 1 FROM FISH(FISH-INDX)
                    END-IF
             END-EVALUATE
           END-PERFORM

           IF PROCESS-TEST
               DISPLAY MYNAME ' day ' DAY-COUNT SPACE WITH NO ADVANCING
               PERFORM VARYING FISH-INDX FROM 1 BY 1
               UNTIL FISH-INDX > NB-FISH
                 DISPLAY FISH(FISH-INDX) ',' WITH NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
               READY TRACE
           END-IF          
           .

       2010-RESET-FISH.
           MOVE 6 TO FISH(FISH-INDX)
           ADD 1 TO NB-FISH
           IF NB-FISH > MAX-FISH
               DISPLAY
                 MYNAME
                 ' internal fish table overflow on day '
                 DAY-COUNT
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           MOVE 8 TO FISH(NB-FISH)
           SET NEW-FISH(NB-FISH) TO TRUE
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .


