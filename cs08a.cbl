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
       Program-ID. cs08a.
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
           05  MYNAME             PIC X(008) VALUE 'cs08a'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  ENTRY-COUNT        PIC 9(009) COMP    VALUE 0.
           05  UNSTRING-PTR       PIC 9(009) COMP    VALUE 0.
           05  SEGMENT-COUNT      PIC 9(009) COMP    VALUE 0.
           05  UNIQUE-COUNT       PIC 9(009) COMP    VALUE 0.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  IN-SIGNAL-PATTERNS PIC X(060)         VALUE SPACES.
           05  IN-FOUR-DIGITS     PIC X(032)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(4096)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  ENTRY-TABLE.
           05  ENTRY-TBL
               OCCURS 200
               INDEXED ENTRY-INDX.
               10  SIGNAL-PATTERN
                   OCCURS 10
                   INDEXED SIGNAL-INDX
                   PIC X(008).
               10  DIGIT
                   OCCURS 4
                   INDEXED DIGIT-INDX
                   PIC X(008).

       Procedure Division.
           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE

           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           IF PROCESS-TEST
               READY TRACE
           END-IF

           INITIALIZE ENTRY-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           PERFORM 2000-PROCESS-ENTRIES
             VARYING ENTRY-INDX FROM 1 BY 1
             UNTIL ENTRY-INDX > ENTRY-COUNT

           DISPLAY MYNAME ' unique count    ' UNIQUE-COUNT
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE FUNCTION CURRENT-DATE
           GOBACK.

       1000-LOAD-INPUT.
           ADD 1 TO ENTRY-COUNT
           SET ENTRY-INDX TO ENTRY-COUNT
           UNSTRING WS-INPT DELIMITED ' | '
             INTO
               IN-SIGNAL-PATTERNS
               IN-FOUR-DIGITS
           END-UNSTRING

           MOVE 1 TO UNSTRING-PTR
           PERFORM VARYING SIGNAL-INDX FROM 1 BY 1
           UNTIL SIGNAL-INDX > 10
             UNSTRING IN-SIGNAL-PATTERNS DELIMITED SPACE
               INTO SIGNAL-PATTERN(ENTRY-INDX,SIGNAL-INDX)
               POINTER UNSTRING-PTR
             END-UNSTRING
           END-PERFORM

           MOVE 1 TO UNSTRING-PTR
           PERFORM VARYING DIGIT-INDX FROM 1 BY 1
           UNTIL DIGIT-INDX > 4
             UNSTRING IN-FOUR-DIGITS DELIMITED SPACE
               INTO DIGIT(ENTRY-INDX,DIGIT-INDX)
               POINTER UNSTRING-PTR
             END-UNSTRING
           END-PERFORM

           PERFORM 8010-READ-INPT-DATA
           .

       2000-PROCESS-ENTRIES.
           IF PROCESS-TEST
               RESET TRACE
           END-IF          

           PERFORM VARYING DIGIT-INDX FROM 1 BY 1
           UNTIL DIGIT-INDX > 4
             INITIALIZE SEGMENT-COUNT
             INSPECT DIGIT(ENTRY-INDX,DIGIT-INDX)
               TALLYING SEGMENT-COUNT
                 FOR ALL CHARACTERS BEFORE SPACE
             EVALUATE SEGMENT-COUNT
               WHEN 2
               WHEN 3
               WHEN 4
               WHEN 7
                    ADD 1 TO UNIQUE-COUNT
               WHEN OTHER
                    CONTINUE
             END-EVALUATE
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


