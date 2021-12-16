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
      * As far as I got on day 16.  I understand how to parse the
      * packets, and I can do it by hand, but don't see an automatable
      * pattern for dealing with the type 4 literals.
      *
       Function-ID. srl.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  myname             PIC X(008)         VALUE 'sll'.

       Local-Storage Section.
       01  WORK-AREAS.
           05  divisor            PIC 9(009) COMP-5  Value 0.

       Linkage Section.
       01  in-4-byte-int          PIC 9(009) COMP-5.
       01  in-nb-bits             PIC 9(009) COMP-5.
       01  out-4-byte-int         PIC 9(009) COMP-5.

       Procedure Division Using
           in-4-byte-int
           in-nb-bits
         Returning
           out-4-byte-int
           .

           Compute out-4-byte-int = in-4-byte-int / (2 ** in-nb-bits)
             On Size Error
               Display
                 myname ' size error '
                 in-4-byte-int ' ' in-nb-bits
           End-Compute

           Goback.

       END FUNCTION srl.

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
       Function-ID. sll.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  myname             PIC X(008)         VALUE 'srl'.

       Local-Storage Section.
       01  WORK-AREAS.
           05  divisor            PIC 9(009) COMP-5  Value 0.

       Linkage Section.
       01  in-4-byte-int          PIC 9(009) COMP-5.
       01  in-nb-bits             PIC 9(009) COMP-5.
       01  out-4-byte-int         PIC 9(009) COMP-5.

       Procedure Division Using
           in-4-byte-int
           in-nb-bits
         Returning
           out-4-byte-int
           .

           Compute out-4-byte-int = in-4-byte-int * (2 ** in-nb-bits)
             On Size Error
               Display
                 myname ' size error '
                 in-4-byte-int ' ' in-nb-bits
           End-Compute

           Goback.

       END FUNCTION sll.

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
       Function-ID. bittest.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  myname             PIC X(008)         VALUE 'bittest'.

       Local-Storage Section.
       01  WORK-AREAS.
           05  eight-byte-int1    PIC 9(018) COMP-5  Value 0.
           05  eight-byte-int2    PIC 9(018) COMP-5  Value 0.

       Linkage Section.
       01  in-4-byte-int          PIC 9(009) COMP-5.
       01  in-bit-to-test         PIC 9(009) COMP-5.
       01  out-4-byte-int         PIC 9(009) COMP-5.

       Procedure Division Using
           in-4-byte-int
           in-bit-to-test
         Returning
           out-4-byte-int
           .

           Move in-4-byte-int To eight-byte-int2
           Compute eight-byte-int1 = 2 ** in-bit-to-test
             On Size Error
               Display
                 myname ' size error '
                 in-bit-to-test
           End-Compute

           Call 'CBL_AND' Using
               eight-byte-int1
               eight-byte-int2
               By Value 1
           End-Call

           If eight-byte-int1 = eight-byte-int2
               Move 1 To out-4-byte-int
           Else
               Move 0 To out-4-byte-int
           End-If

           Goback.

       END FUNCTION bittest.

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
       Program-ID. cs16a.
       Environment Division.
       Configuration Section.
       Repository.
           Function srl
           Function sll
           FUNCTION ALL INTRINSIC.
       Input-Output Section.
       File-Control.
           Select INPT-DATA            Assign Keyboard.
       Data Division.
       File Section.
       FD  INPT-DATA.
       01  INPT-DATA-REC-MAX PIC X(2048).

       Working-Storage Section.
       01  CONSTANTS.
           05  myname             PIC X(008)         VALUE 'cs16a'.

       01  WORK-AREAS.
           05  ws-rec-count       PIC 9(009) COMP    VALUE 0.
           05  inst-ptr           Pic 9(009) Comp    Value 1.
           05  vrsn-tot           Pic 9(009) Comp    Value 0.
           05  cli-args           PIC X(080)         VALUE LOW-VALUES.
           05  process-type       PIC X(004)         VALUE LOW-VALUES.

       01  WS-INPT-DATA.
           05  ws-inpt            PIC X(2048)        VALUE SPACES.

       01  SWITCHES.
           05  INPT-DATA-EOF-SW   PIC X(001)         VALUE 'N'.
               88  INPT-DATA-EOF                     VALUE 'Y'.
           05  PROCESS-SW         PIC X(004)         VALUE LOW-VALUES.
               88  process-test                      VALUE 'TEST'.

       01  Instruction-Table.
           05  inst-tbl occurs 1000.
               10  inst-vrsn      Pic 9(009) Comp-5.
               10  inst-type      Pic 9(009) comp-5.

       Procedure Division.
           Display myname Space Current-Date

           Accept cli-args From Command-Line
           Unstring cli-args Delimited Space Or Low-Value
             Into process-type
           End-UnstrinG

           Move Upper-Case(process-type)
             To process-sw

           Open Input inpt-data

           Perform 8010-Read-Inpt-Data

           Close inpt-data

           Perform 1000-Parse-Input
             Until ws-inpt(inst-ptr:1) = Space

           Display myname ' records read    ' WS-REC-COUNT

           Display myname Space CURRENT-DATE

           GOBACK.

       1000-Parse-Input.
           Call 'parseit' Using
               WS-INPT
               inst-ptr
               vrsn-tot
           End-Call
           .

       8010-READ-INPT-DATA.
           Initialize ws-inpt-data
           Read Inpt-Data Into ws-inpt-data
             At End Set inpt-data-eof To True
             Not At End
               Add 1 To ws-rec-count
           End-Read

           .


       END PROGRAM cs16a.

       ID Division.
       Program-ID. parseit Recursive.
       Environment Division.
       Configuration Section.
       Repository.
           FUNCTION srl
           FUNCTION sll
           Function bittest
           FUNCTION ALL INTRINSIC.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  myname             PIC X(008)         VALUE 'cs16a'.
           05  zero-bit           Pic 9(009) Comp-5  Value 1.
           05  four-bit           Pic 9(009) Comp-5  Value 4.
           05  bits-2-3-4         Pic 9(018) Comp-5  Value 28.
           05  bit-tbl-val.
               10  .
                   15             Pic X(001)         Value '0'.
                   15             Pic 9(009) Comp-5  Value 0.
               10  .
                   15             Pic X(001)         Value '1'.
                   15             Pic 9(009) Comp-5  Value 1.
               10  .
                   15             Pic X(001)         Value '2'.
                   15             Pic 9(009) Comp-5  Value 2.
               10  .
                   15             Pic X(001)         Value '3'.
                   15             Pic 9(009) Comp-5  Value 3.
               10  .
                   15             Pic X(001)         Value '4'.
                   15             Pic 9(009) Comp-5  Value 4.
               10  .
                   15             Pic X(001)         Value '5'.
                   15             Pic 9(009) Comp-5  Value 5.
               10  .
                   15             Pic X(001)         Value '6'.
                   15             Pic 9(009) Comp-5  Value 6.
               10  .
                   15             Pic X(001)         Value '7'.
                   15             Pic 9(009) Comp-5  Value 7.
               10  .
                   15             Pic X(001)         Value '8'.
                   15             Pic 9(009) Comp-5  Value 8.
               10  .
                   15             Pic X(001)         Value '9'.
                   15             Pic 9(009) Comp-5  Value 9.
               10  .
                   15             Pic X(001)         Value 'A'.
                   15             Pic 9(009) Comp-5  Value 10.
               10  .
                   15             Pic X(001)         Value 'B'.
                   15             Pic 9(009) Comp-5  Value 11.
               10  .
                   15             Pic X(001)         Value 'C'.
                   15             Pic 9(009) Comp-5  Value 12.
               10  .
                   15             Pic X(001)         Value 'D'.
                   15             Pic 9(009) Comp-5  Value 13.
               10  .
                   15             Pic X(001)         Value 'E'.
                   15             Pic 9(009) Comp-5  Value 14.
               10  .
                   15             Pic X(001)         Value 'F'.
                   15             Pic 9(009) Comp-5  Value 15.
           05  bit-tbl
               Redefines bit-tbl-val
               Occurs 16
               Ascending Key hex-digit
               Indexed bit-tbl-idx.
               10  hex-digit      Pic X(001).
               10  hex-bit-val    Pic 9(009) Comp-5.


       Local-Storage Section.
       01  work-areas.
           05  nb-bits            Pic 9(009) Comp-5.
           05  first-four-bits    Pic 9(009) Comp-5.
           05  second-four-bits   Pic 9(009) Comp-5.
           05  bit-string1        Pic 9(009) Comp-5.
           05  bit-string2        Pic 9(009) Comp-5.
           05  bit-string3        Pic 9(009) Comp-5.
           05  bit-string4        Pic 9(009) Comp-5.
           05  bit-string5        Pic 9(018) Comp-5.
           05  bit-string6        Pic 9(018) Comp-5.
           05  inst-vers          Pic 9(018) Comp-5.

       01  switches.
           05  continue-sw        Pic 9(009)         Value 9.
               88  we-must-continue                  Value 1.
               88  we-are-done                       Value 0.

       Linkage Section.
       01  inst-string            Pic X(2048).
       01  inst-ptr               Pic 9(009).
       01  vrsn-tot               Pic 9(009).

       Procedure Division Using
           inst-string
           inst-ptr
           vrsn-tot
           .

           Search All bit-tbl
             When inst-string(inst-ptr:1) = hex-digit(bit-tbl-idx)
                  Move hex-bit-val(bit-tbl-idx)
                    To bit-string1 first-four-bits
           End-Search

           Move 1 To nb-bits
           Move srl(bit-string1,nb-bits) to bit-string2
           Add  bit-string2 to vrsn-tot

           Add 1 to inst-ptr
           Search All bit-tbl
             When inst-string(inst-ptr:1) = hex-digit(bit-tbl-idx)
                  Move hex-bit-val(bit-tbl-idx)
                    To bit-string6 second-four-bits
           End-Search

           Move 4 To nb-bits
           Move sll(first-four-bits,nb-bits) To bit-string5
           Add bit-string5 To bit-string6

           Call 'CBL_AND' Using
               bits-2-3-4
               bit-string6
               By Value 1
           End-Call

           Move bit-string6 To bit-string1
           Move 1 To nb-bits
           Move srl(bit-string1,nb-bits) To bit-string2

           If bit-string2 = four-bit
               Move bittest(zero-bit,second-four-bits) To continue-sw
               Perform 2000-Literal
                 Until we-are-done
           End-If

           If inst-string(inst-ptr:1) Not = Space
               Call 'parseit' Using
                   inst-string
                   inst-ptr
                   vrsn-tot
               End-Call
           End-If

           Goback
           .

       2000-Literal.
           Add 1 to inst-ptr
           Search All bit-tbl
             When inst-string(inst-ptr:1) = hex-digit(bit-tbl-idx)
                  Move hex-bit-val(bit-tbl-idx)
                    To bit-string1
           End-Search

           Add 1 to inst-ptr
           Search All bit-tbl
             When inst-string(inst-ptr:1) = hex-digit(bit-tbl-idx)
                  Move hex-bit-val(bit-tbl-idx)
                    To bit-string2
           End-Search

           Move 4 to nb-bits
           Move bit-string6 to bit-string2
           Move sll(bit-string2,nb-bits) To bit-string3

           Move bittest(zero-bit,second-four-bits) To continue-sw
           .

