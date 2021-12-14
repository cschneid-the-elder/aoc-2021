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
       Function-ID. lmntidx.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'lmntidx'.

       01  EXTERNAL-SWITCHES EXTERNAL.
           05  PROCESS-SW         PIC X(004).
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  ELEMENT-TABLE EXTERNAL.
           05  LMNT-TBL
               OCCURS 26.
               10  LMNT-VAL       PIC X(001). 
               10  LMNT-CT        PIC 9(018) COMP.

       Linkage Section.
       01  LS-LMNT                PIC X(001).
       01  LMNT-IDX               PIC S9(009) COMP   VALUE 0.
       Procedure Division Using
           LS-LMNT
         Returning LMNT-IDX
         .
           DISPLAY MYNAME ' LS-LMNT ' LS-LMNT
           PERFORM VARYING LMNT-IDX FROM 1 BY 1
           UNTIL LMNT-IDX > 26
           OR LMNT-VAL(LMNT-IDX) = LS-LMNT
           OR LMNT-VAL(LMNT-IDX) = SPACE
             CONTINUE
           END-PERFORM

           IF LMNT-VAL(LMNT-IDX) = SPACE
               MOVE LS-LMNT TO LMNT-VAL(LMNT-IDX)
               IF PROCESS-TEST
                   DISPLAY MYNAME ' adding ' LS-LMNT ' @ ' LMNT-IDX
               END-IF
           END-IF

           GOBACK
           .

       END FUNCTION lmntidx.

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
      * The correct algorithm escapes me.  This doesn't work for 
      * part 2 of day 14.
      *
       Program-ID. cs14b.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION lmntidx
           FUNCTION ALL INTRINSIC.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(080).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008)         VALUE 'cs14b'.

       01  WORK-AREAS.
           05  WS-REC-COUNT       PIC 9(009) COMP    VALUE 0.
           05  RULE-COUNT         PIC 9(009) COMP    VALUE 0.
           05  NB-STEPS           PIC 9(009) COMP    VALUE 0.
           05  STEP-COUNT         PIC 9(009) COMP    VALUE 0.
           05  NB-LMNT            PIC 9(009) COMP    VALUE 0.
           05  NEW-IDX            PIC 9(009) COMP    VALUE 0.
           05  POLY-IDX           PIC 9(009) COMP    VALUE 0.
           05  POLY-LEN           PIC 9(009) COMP    VALUE 0.
           05  POLY-PTR           PIC 9(009) COMP    VALUE 1.
           05  LMNT-IDX           PIC 9(009) COMP    VALUE 0.
           05  LMNT-MAX           PIC 9(018) COMP    VALUE 0.
           05  LMNT-MIN           PIC 9(018) COMP    VALUE 9999999999.
           05  LMNT-COUNT         PIC 9(009) COMP    VALUE 0.
           05  LMNT-DIF           PIC 9(018) COMP    VALUE 0.
           05  NB-STEPS-X         PIC X(004)         VALUE SPACES.
           05  HOLD-LMNT          PIC X(001)         VALUE SPACES.
           05  CLI-ARGS           PIC X(080)         VALUE LOW-VALUES.
           05  PROCESS-TYPE       PIC X(004)         VALUE LOW-VALUES.
           05  INITIAL-POLYMER    PIC X(080)         VALUE SPACES.
           05  NEW-PAIR           PIC X(002)         VALUE SPACES.

       01  WS-INPT-DATA.
           05  WS-INPT            PIC X(080)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  RULES-NOW-SW       PIC X(001)         VALUE 'N'.
               88  RULES-NOW                         VALUE 'Y'.

       01  EXTERNAL-SWITCHES EXTERNAL.
           05  PROCESS-SW         PIC X(004).
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  RULE-TABLE.
           05  RULE-TBL
               OCCURS 100
               ASCENDING KEY PAIR
               INDEXED RULE-IDX.
               10  PAIR           PIC X(002).
               10  LMNT           PIC X(001).

       01  POLYMER-TABLE.
           05  POLY-TBL
               OCCURS 676.
               10  POLY-PAIR      PIC X(002).
               10  POLY-PAIR-CT   PIC 9(018) COMP.
               10  POLY-PAIR-SW   PIC X(001).
                   88  POLY-PAIR-ACTIVE              VALUE 'Y'
                                                     FALSE 'N'.

       01  NEW-POLYMER-TABLE.
           05  NEW-POLY-TBL
               OCCURS 676.
               10  POLY-PAIR      PIC X(002).
               10  POLY-PAIR-CT   PIC 9(018) COMP.
               10  POLY-PAIR-SW   PIC X(001).
                   88  POLY-PAIR-ACTIVE              VALUE 'Y'
                                                     FALSE 'N'.

       01  ELEMENT-TABLE EXTERNAL.
           05  LMNT-TBL
               OCCURS 26.
               10  LMNT-VAL       PIC X(001). 
               10  LMNT-CT        PIC 9(018) COMP.

       Procedure Division.
           DISPLAY MYNAME SPACE CURRENT-DATE

           ACCEPT CLI-ARGS FROM COMMAND-LINE
           UNSTRING CLI-ARGS DELIMITED SPACE OR LOW-VALUE
             INTO PROCESS-TYPE NB-STEPS-X
           END-UNSTRING

           MOVE UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW
           MOVE NUMVAL(NB-STEPS-X) TO NB-STEPS

           DISPLAY MYNAME ' nb steps  ' NB-STEPS

           MOVE HIGH-VALUES TO RULE-TABLE

           OPEN INPUT INPT-DATA

           PERFORM 8010-READ-INPT-DATA

           PERFORM 1000-LOAD-INPUT UNTIL INPT-DATA-EOF

           CLOSE INPT-DATA

           DISPLAY MYNAME ' number of rules ' RULE-COUNT

           SORT RULE-TBL ASCENDING KEY PAIR

           PERFORM 2000-CONSTRUCT-POLYMER NB-STEPS TIMES

           IF PROCESS-TEST
               PERFORM VARYING POLY-IDX FROM 1 BY 1
               UNTIL POLY-PAIR OF POLYMER-TABLE(POLY-IDX) = SPACES
               OR POLY-IDX > 676
                 DISPLAY MYNAME ' ' POLY-IDX
                   ' ' POLY-PAIR OF POLYMER-TABLE(POLY-IDX)
                   ' ' POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
                   ' ' POLY-PAIR-SW OF POLYMER-TABLE(POLY-IDX)
               END-PERFORM
           END-IF

           PERFORM 3000-COUNT-ELEMENTS

           DISPLAY MYNAME ' most common     ' LMNT-MAX
           DISPLAY MYNAME ' least common    ' LMNT-MIN
           DISPLAY MYNAME ' difference      ' LMNT-DIF
           DISPLAY MYNAME ' records read    ' WS-REC-COUNT

           DISPLAY MYNAME SPACE CURRENT-DATE

           GOBACK.

       1000-LOAD-INPUT.
           EVALUATE TRUE
             WHEN RULES-NOW
                  ADD 1 TO RULE-COUNT
                  UNSTRING WS-INPT DELIMITED ' -> '
                    INTO PAIR(RULE-COUNT) LMNT(RULE-COUNT)
                  END-UNSTRING
             WHEN WS-REC-COUNT = 1
                  PERFORM 1010-INIT-POLYMER-TABLE
                  PERFORM 1020-INIT-ELEMENT-TABLE
             WHEN WS-INPT = SPACES
                  SET RULES-NOW TO TRUE
           END-EVALUATE

           INITIALIZE WS-INPT
           PERFORM 8010-READ-INPT-DATA
           .

       1010-INIT-POLYMER-TABLE.
           MOVE WS-INPT TO INITIAL-POLYMER
           INITIALIZE POLYMER-TABLE
           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL INITIAL-POLYMER(POLY-IDX + 1:1) = SPACE
             MOVE INITIAL-POLYMER(POLY-IDX:2)
               TO POLY-PAIR OF POLYMER-TABLE(POLY-IDX)
             MOVE 1 TO POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
             SET POLY-PAIR-ACTIVE OF POLYMER-TABLE(POLY-IDX) TO TRUE
           END-PERFORM
           .

       1020-INIT-ELEMENT-TABLE.
           INITIALIZE ELEMENT-TABLE
           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL INITIAL-POLYMER(POLY-IDX:1) = SPACE
             MOVE lmntidx(INITIAL-POLYMER(POLY-IDX:1))
               TO LMNT-IDX
             ADD 1 TO LMNT-CT(LMNT-IDX)
           END-PERFORM
           .

       2000-CONSTRUCT-POLYMER.
           MOVE POLYMER-TABLE TO NEW-POLYMER-TABLE
           ADD 1 TO STEP-COUNT
           DISPLAY MYNAME ' STEP ' STEP-COUNT
           CALL 'lmntdump'
           DISPLAY MYNAME ' POLYMER-TABLE'
           CALL 'polydump' USING POLYMER-TABLE

           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL POLY-IDX > 676
           OR POLY-PAIR OF POLYMER-TABLE(POLY-IDX) = SPACES
             IF POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX) > 0
                 PERFORM 2010-SEARCH-RULES
                 MOVE lmntidx(LMNT(RULE-IDX)) TO LMNT-IDX
                 IF PROCESS-TEST
                     DISPLAY MYNAME
                       ' lmnt    ' LMNT-VAL(LMNT-IDX)
                       ' lmnt-ct ' LMNT-CT(LMNT-IDX)
                       ' poly-pair-ct ' 
                       POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
                     DISPLAY MYNAME
                       ' rule ' PAIR(RULE-IDX) ' -> ' LMNT(RULE-IDX)
                 END-IF
                 COMPUTE LMNT-CT(LMNT-IDX) =
                     LMNT-CT(LMNT-IDX)
                   + POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
                 SUBTRACT
                      POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
                   FROM
                      POLY-PAIR-CT OF NEW-POLYMER-TABLE(POLY-IDX)
                 PERFORM 2020-CREATE-NEW-PAIRS
      *           MOVE 0 TO POLY-PAIR-CT OF NEW-POLYMER-TABLE(POLY-IDX)
             END-IF 
           END-PERFORM

           DISPLAY MYNAME ' NEW-POLYMER-TABLE'
           CALL 'polydump' USING NEW-POLYMER-TABLE
           MOVE NEW-POLYMER-TABLE TO POLYMER-TABLE
           .

       2010-SEARCH-RULES.
           SEARCH ALL RULE-TBL
             WHEN PAIR(RULE-IDX) = POLY-PAIR OF POLYMER-TABLE(POLY-IDX)
                  CONTINUE
           END-SEARCH
           .

       2020-CREATE-NEW-PAIRS.

           MOVE POLY-PAIR OF POLYMER-TABLE (POLY-IDX)(1:1)
             TO NEW-PAIR(1:1)
           MOVE LMNT(RULE-IDX)           TO NEW-PAIR(2:1)
           PERFORM 2021-SET-NEW-PAIR-ACTIVE

           MOVE LMNT(RULE-IDX)           TO NEW-PAIR(1:1)
           MOVE POLY-PAIR OF POLYMER-TABLE (POLY-IDX)(2:1)
             TO NEW-PAIR(2:1)
           PERFORM 2021-SET-NEW-PAIR-ACTIVE
           .

       2021-SET-NEW-PAIR-ACTIVE.
           CALL 'pairidx' USING
               NEW-PAIR
               NEW-IDX
               NEW-POLYMER-TABLE
               POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
           END-CALL

      *     MOVE POLY-PAIR-CT OF POLYMER-TABLE(POLY-IDX)
      *       TO POLY-PAIR-CT OF NEW-POLYMER-TABLE(NEW-IDX)
           .

       3000-COUNT-ELEMENTS.
           CALL 'lmntdump'
           PERFORM VARYING LMNT-IDX FROM 1 BY 1
           UNTIL LMNT-IDX > 26
           OR LMNT-VAL(LMNT-IDX) = SPACE
             IF LMNT-CT(LMNT-IDX) < LMNT-MIN
                 MOVE LMNT-CT(LMNT-IDX) TO LMNT-MIN
             END-IF
             IF LMNT-CT(LMNT-IDX) > LMNT-MAX
                 MOVE LMNT-CT(LMNT-IDX) TO LMNT-MAX
             END-IF
           END-PERFORM

           SUBTRACT LMNT-MIN FROM LMNT-MAX GIVING LMNT-DIF
           .

       8010-READ-INPT-DATA.
           INITIALIZE WS-INPT-DATA
           READ INPT-DATA INTO WS-INPT-DATA
             AT END SET INPT-DATA-EOF TO TRUE
             NOT AT END
               ADD 1 TO WS-REC-COUNT
           END-READ

           .


       END PROGRAM cs14b.

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
       Program-ID. pairidx.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'pairidx'.

       01  EXTERNAL-SWITCHES EXTERNAL.
           05  PROCESS-SW         PIC X(004).
               88  PROCESS-TEST                      VALUE 'TEST'.

       Linkage Section.
       01  LS-POLY-PAIR           PIC X(002).
       01  POLY-IDX               PIC S9(009) COMP   VALUE 0.
       01  NEW-POLYMER-TABLE.
           05  NEW-POLY-TBL
               OCCURS 676.
               10  POLY-PAIR      PIC X(002).
               10  POLY-PAIR-CT   PIC 9(018) COMP.
               10  POLY-PAIR-SW   PIC X(001).
                   88  POLY-PAIR-ACTIVE              VALUE 'Y'
                                                     FALSE 'N'.

       01  OLD-POLY-PAIR-CT       PIC 9(018) COMP.
       Procedure Division Using
           LS-POLY-PAIR
           POLY-IDX
           NEW-POLYMER-TABLE
           OLD-POLY-PAIR-CT
         .

           DISPLAY MYNAME ' POLY-PAIR ' LS-POLY-PAIR
           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL POLY-IDX > 676
           OR POLY-PAIR OF NEW-POLYMER-TABLE(POLY-IDX) = SPACES
             IF POLY-PAIR OF NEW-POLYMER-TABLE(POLY-IDX) = LS-POLY-PAIR
                 ADD 1 TO POLY-PAIR-CT(POLY-IDX)
                 EXIT PERFORM
             END-IF
           END-PERFORM

           IF POLY-PAIR OF NEW-POLYMER-TABLE(POLY-IDX) = SPACES
               MOVE LS-POLY-PAIR
                 TO POLY-PAIR OF NEW-POLYMER-TABLE(POLY-IDX)
               MOVE OLD-POLY-PAIR-CT
                 TO POLY-PAIR-CT OF NEW-POLYMER-TABLE(POLY-IDX)
               IF PROCESS-TEST
                   DISPLAY MYNAME ' adding ' LS-POLY-PAIR ' @ ' POLY-IDX
               END-IF
           END-IF

           GOBACK
           .

       END PROGRAM pairidx.

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
       Program-ID. lmntdump.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'lmntdump'.

       01  EXTERNAL-SWITCHES EXTERNAL.
           05  PROCESS-SW         PIC X(004).
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  ELEMENT-TABLE EXTERNAL.
           05  LMNT-TBL
               OCCURS 26.
               10  LMNT-VAL       PIC X(001). 
               10  LMNT-CT        PIC 9(018) COMP.

       Local-Storage Section.
       01  WORK-AREAS.
           05  LMNT-IDX           PIC 9(009) COMP.

       Procedure Division.

           PERFORM VARYING LMNT-IDX FROM 1 BY 1
           UNTIL LMNT-IDX > 26
           OR LMNT-VAL(LMNT-IDX) = SPACE
             IF PROCESS-TEST
                 DISPLAY MYNAME ' element ' LMNT-VAL(LMNT-IDX)
                   ' ' LMNT-CT(LMNT-IDX)
             END-IF
           END-PERFORM

           GOBACK
           .

       END PROGRAM lmntdump.

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
       Program-ID. polydump.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'polydump'.

       01  EXTERNAL-SWITCHES EXTERNAL.
           05  PROCESS-SW         PIC X(004).
               88  PROCESS-TEST                      VALUE 'TEST'.

       01  ELEMENT-TABLE EXTERNAL.
           05  LMNT-TBL
               OCCURS 26.
               10  LMNT-VAL       PIC X(001). 
               10  LMNT-CT        PIC 9(018) COMP.

       Local-Storage Section.
       01  WORK-AREAS.
           05  POLY-IDX           PIC 9(009) COMP.

       Linkage Section.
       01  POLYMER-TABLE.
           05  POLY-TBL
               OCCURS 676.
               10  POLY-PAIR      PIC X(002).
               10  POLY-PAIR-CT   PIC 9(018) COMP.
               10  POLY-PAIR-SW   PIC X(001).
                   88  POLY-PAIR-ACTIVE              VALUE 'Y'
                                                     FALSE 'N'.

       Procedure Division Using
           POLYMER-TABLE
           .

           PERFORM VARYING POLY-IDX FROM 1 BY 1
           UNTIL POLY-IDX > 676
           OR POLY-PAIR(POLY-IDX) = SPACES
             IF PROCESS-TEST
                 DISPLAY MYNAME
                   ' ' POLY-IDX
                   ' pair ' POLY-PAIR(POLY-IDX)
                   ' ' POLY-PAIR-CT(POLY-IDX)
                   ' ' POLY-PAIR-SW(POLY-IDX)
             END-IF
           END-PERFORM

           GOBACK
           .

       END PROGRAM polydump.

