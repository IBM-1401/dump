               JOB  Card 1 of core dump routine
     * SET WORD MARKS FOR THE SECOND CARD
               ORG  1
               SW   A1,A2
     A1        SW   A3,A4
     A2        NOP  0,0,0
     A3        SW   A5,A6
     A4        NOP  0,0,0
     A5        SW   A7
     A6        SW   A8,A9
     A7        SW   A10
     A8        SW   A11
     A9           N0
     A10       SW   A12
     A11       SW   A13
     A12       R    BLOOP            READ THE NEXT CARD
     A13       DCW  #1
               JOB  Card 2 of core dump routine
     * Replace group marks in 81-399 by ).
               ORG  1
     BLOOP     MCW  BADDR,BTESTG&6   SET TEST ADDRESS
               MCW  BADDR,BREPLG&6   SET REPLACEMENT ADDR
     BTESTG    BCE  BREPLG,5777&X1,}  GROUP MARK? (B REPLACED) 
     BBUMP     A    BINCR,BADDR      UPDATE TEST ADDRESS
               BCE  BDONE,BADDR-2,4  DONE WITH SCAN?
               B    BLOOP            AROUND AGAIN
     BREPLG    MCW  BLOZ,5777&X1     REPLACE GROUP MARK (B REPLACED)
               B    BBUMP            UPDATE TEST ADDRESS
     BADDR     DCW  @081@
               DC   @ @
     BDONE     CC   K                SKIP TWO LINES
               CS   *-3              NEED NEW WORD MARKS
               SW   CSTART           READY FOR A NEW CARD
     BINCR     R    CSTART           READ A NEW CARD
     BLOZ      DCW  @)@              LOZENGE ON THE  A  CHAIN
               JOB  Card 3 of core dump routine
     * Set word marks for the next card, print the print
     * area, then print the word marks.
               ORG  1
     CSTART    SW   C1,C2
     C1        SW   C3
     C2        SW   C4
     C3        SW   C5,C6
     C4        SW   C7,C8
     C5        SW   C9
     C6        SW   C10,C11
     C7        SW   DLOAD,C14
     C8        SW
     C9        SW   C15,C16
     C10       SW   C17,C12
     C11       CW   C4
     C12       SW   DLOAD2
     C13       W
     C14          2)
     C15          N0
     C16       R    DSTART
     C17       NOP
               JOB  Card 4 of core dump routine
     * Clear 300-332 to make sure 330 isn't a zero.  Fill
     * in the dots and numbers 210-330, print them.
               ORG  1
               DCW  @.......@
     DNUM      DCW  @210@
               DC   #1
     DSTART    CS   332              ENSURE 330 IS NOT @0@
                  N00
               NOP
     DLOAD     LCA  DNUM,210         LOAD NUM TO PRINT
     DLOAD2    LCA                   LOAD DOTS
               CW   C8
               A    DREAD&1,DNUM     ADD 10 T0 NUM
               BCE  DDONE,330,0      DONE?
               A    DREAD&1,DLOAD&6  ADD 10 TO LOAD PLACE
               LCA  2,332            LAST TWO DOTS TO PRINT
               B    DLOAD            AROUND AGAIN
     DDONE     CW   DLOAD2
               W
                  N0
               CC   J                SKIP ONE LINE
     DREAD     R    ESTART           READ THE NEXT CARD
               DCW  #1
               JOB  Card 5 of core dump routine
     * Clear 300-332. Remember whether 101 had a word mark.
     * Set one so we can load from 101 upward.  Load 101-200
     * and its word marks to 201-300.  Clear (or don't
     * clear) the word mark in 201.  Print.  Print word marks.
               ORG  1
     EHAVWM    MCW  ENOP,ECW         CHANGE CW TO NOP
               B    ELOOP
     ESTART    SW   E1
               NOP
     E1        CS   332
               NOP
               BW   EHAVWM,101       WM IN 101?
               SW   101              NO, SET ONE
     ELOOP     LCA  101,201          LOAD TO PRINT AREA
               BCE  ECW,ELOOP&4,3    STORING AT 300 YET?
               A    EREAD,ELOOP&3    UPDATE FROM ADDRESS
               A    EREAD,ELOOP&6    UPDATE TO ADDRESS
               B    ELOOP            NO
     ECW       CW   201              OR MAYBE NOT
               W
                  2)                 PRINT THE WORD MARKS
                  N0
     EREAD     R    FSTART           READ THE NEXT CARD
     ENOP      NOP
               JOB  Card 6 of core dump routine
     * Clear 300 to make sure it isn't zero.  Fill in the
     * dots and numbers 110-200.  Print them.
               ORG  1
               DCW  @.......@
     FNUM      DCW  @110@
               DC   #1
     FSTART    CS   300              ENSURE 300 ISN'T 0
               NOP
               SW   FLOAD2
               NOP
     FLOAD     LCA  FNUM,210         LOAD NUM TO PRINT
     FLOAD2    LCA                   LOAD THE DOTS
               NOP  0
               A    FREAD&1,FNUM     ADD 10 T0 NUM
               BCE  FDONE,300,0      DONE?
               A    FREAD&1,FLOAD&6  ADD 10 TO LOAD PLACE
               NOP  0,0
               B    FLOAD            AROUND AGAIN
     FDONE     CW   FLOAD2
               W
                  N0
               CC   J                SKIP ONE LINE
     FREAD     R    GSTART           READ THE NEXT CARD
               NOP
               JOB  Card 7 of core dump routine
     * Clear 300.  Remember whether 81 had a word mark.  Set
     * one.  Load 81-99 and its word marks to 281-299.
     * Clear (or don't clear) the word mark in 281.  Print.
     * Print word marks.
               ORG  1
     GHAVWM    MCW  GNOP,GCW         CHANGE CW TO NOP
               B    GLOOP
               DCW  #4
               DCW  #1
     GSTART    CS   300
               CS
               BW   GHAVWM,81        WM IN 81?
               SW   81               NO, SET ONE
     GLOOP     LCA  81,201           LOAD TO PRINT AREA
               BCE  GCW,GLOOP&4,3    STORING AT 300 YET?
               A    GREAD,GLOOP&3    UPDATE FROM ADDRESS
               A    GREAD,GLOOP&6    UPDATE TO ADDRESS
               B    GLOOP            NO
     GCW       CW   281              OR MAYBE NOT
               W
                  2)                 PRINT THE WORD MARKS
                  N0
     GREAD     R    HSTART           READ THE NEXT CARD
     GNOP      NOP
               JOB  Card 8 of core dump routine
     * Load index register identification to 281-300.
               ORG  1
     HDOTS     DCW  @......*@
     HINDX1    DCW  @1*..@
               DCW  @*2*.@
     H1        DCW  @.@
     HINDX     DCW  @*3*.@
               DC   #1
     H2        DCW  #1
     H3        DC   #7
     HSTART    SW   H4
               CW   HDOTS&1,HINDX&1
               CW   H2,H1&1
     H4        CW
               LCA  HINDX,300        MOVE XR ID TO 300
               SW   H1,H2&1
               R    ISTART           READ THE NEXT CARD
               DCW  #4
               DCW  #1
               DCW  #2
               DCW  #2
               DCW  #4
               DCW  #1
               JOB  Card 9 of core dump routine
     * Load sense switch identifiers to 201-234.
               ORG  1
     ISSTXT    DCW  @SENSE SWS   ON@
               DC   #1
               DCW  #7
               DCW  #7
     IOFF      DCW  @OFF@
               DC   #1
               DCW  #7
               DCW  #7
               DCW  #1
     ISTART    LCA  ISSTXT,214       LOAD SS ON TEXT TO 214
               LCA  IOFF,234         LOAD SS OFF
               CW   C15
               CW   C12
               NOP
               SW   JSTART
               R    JSTART           READ THE NEXT CARD
               DCW  #1
               JOB  Card 10 of core dump routine
     * Put list of on and off sense switches in 236... (on)
     * or 216... (off).
               ORG  1
     JTWO      DCW  2
               DC   #9
     JSTART    BSS  JON,B            SWITCH ON?
     JOFF      M    JB,236           NO, MOVE SS ID TO OFF
               A    JTWO,JOFF&6      INCR OFF ID SPOT
               B    JBOTH
     JON       M    JB,216           YES, MOVE SS ID TO ON
               A    JTWO,JON&6
               NOP
     JBOTH     A    JREAD,JSTART&4   INCR SWITCH TEST
               A    JREAD,JB         INCR SS ID
               BCE  JREAD,JB,H       DONE?
               NOP
               B    JSTART           AROUND AGAIN
     JREAD     R    KSTART           READ THE NEXT CARD
     JB        DCW  @B@              SS ID TO PRINT
               JOB  Card 11 of core dump routine
     * Save comparison indicators in 100..125.
               ORG  1
     KUNEQ     DCW  @   UNEQUAL@
     KEQUAL    DCW  @EQUAL@
     KHIGH     DCW  @ HIGH@
               DC   #2
     KLOW      DCW  @LOW@
               DC   #4
               DCW  #4
     KSTART    LCA  KUNEQ,110        SAVE  UNEQUAL
               LCA  KEQUAL,115       SAVE  EQUAL
               NOP
               LCA  KHIGH,120        SAVE  HIGH
               LCA  KLOW,125         SAVE  LOW
               NOP  0,0,0
               NOP
               NOP  0
               R    LSTART
               DCW  #1
               JOB  Card 12 of core dump routine
     * Move appropriate comparison indicators to 247..265.
               ORG  1
     LINC      DCW  5003             INC FOR TWO ADDRS
               DC   #6
     LSTART    BU   LINDON           INDICATOR ON?
               NOP  0,0
               NOP  0,0
               B    LINDOF           NO
               DCW  #7
     LINDON    MCW  110,256          MOVE INDICATOR TO PR
               NOP
     LINDOF    A    LREAD,LSTART&4   INCR INDICATOR TEST
               A    LINC,LINDON&6    INCR BOTH ADDRS
               BCE  LREAD,LSTART&4,V  DONE?
               NOP
               B    LSTART           AROUND AGAIN
     LREAD     R    MSTART           READ THE NEXT CARD
               DCW  #1
               JOB  Card 13 of core dump routine
     * Construct overflow off (or on) indicator.
     * Move it to 268..277.
               ORG  1
     MOVFF     DCW  @OVFLO OFF@
               DC   #1
     MSTART    BAV  MOVFL            OVERFLOW?
     MOVON     DCW  @N @             NOP, AND  N  FOR MSG
               DC   @00000@          REST OF THE NOP INSTR
               NOP  0,0
               B    MOVFON
               DCW  #7
     MOVFL     MCW  MOVON,MOVFF      CHANGE  OFF  TO  ON
               NOP
     MOVFON    MCW  MOVFF,277        MOVE MSG TO PRINT AREA
               NOP  0,0
               NOP  0,0,0
               NOP
               NOP  0
               R    NSTART
               DCW  #1
               JOB  Card 14 of core dump routine
     * Move clear routines for 200-299 and 0-80 to
     * 101-116.  Print indicators. Clear 200-299 and 0-80.
     * Set word mark in 1, read a card and branch to 1.
               ORG  1
               DCW  @/299/080,0@     CLEAR STORAGE ROUTINE
               DCW  @01100@          SEE SOURCE CODE BELOW
     NCLEAR    DCW  @1@
               DC   #6
               DCW  #7
               DCW  #4
     NSTART    SW   N1,N2
               CW   JSTART,NCLEAR
               CW
               LCA  NCLEAR,NCLEND-1
               SW   NCLEND,NCL3
               SW   NCL2
     N1        SW   NCL1
               W
               CC   L                SKIP THREE LINES
     N2        CC   K                SKIP TWO LINES
               B    NCLBEG
               DCW  #1
     * Routine to clear 200-299 and 0-80, then set a word
     * mark at 1, read a card and branch to 1.  The text of
     * this routine is punched in cc 1-16 of card N.
               ORG  101
     NCLBEG    CS   299
     NCL1      CS   80
     NCL2      SW   1
     NCL3      R    OSTART
     NCLEND    DCW  #1
               JOB  Move data and word marks to print area
     * Routine to move data and word marks to print area.
     * Convert group marks to ).
     * Read a card when done.
               ORG  78
     DWSTRT    BU   WSTART
     DW1       B    SWITCH
     X1        DCW  @X00@
     DFF       DCW  @FF@
     X2        DCW  333
     DW3       DCW  01
     X3        DC   033
     DWPRNT    W
     DW5       CS   332
               CS
     DWLOOP    SW   212&X3           ASSUME DATA HAS WM
     DW7       MCW  0&X2,212&X3      MOVE THE DATA
     DW8       BW   GOTWM,0&X2       DID IT HAVE WM?
     LOZ       CW   212&X3           NO, CLEAR ASSUMED WM
     GOTWM     BCE  GOTGM,0&X3,}     GROUP MARK?
     DW9       B    NOGM             NO
     GOTGM     MCW  LOZ,212&X3       REPL GROUP MARK WITH )
     NOGM      A    DWREAD,X3        BUMP X3
               A                     BUMP X2 -- MA FOR BIG CORE
     DW12      BCE  DWLOOP,X3-2,0    AROUND AGAIN
               CW   DWSTRT           CHANGED LATER TO W 060
     SWITCH    NOP  080              CHANGED TO N OR /
     DW14      SW   PSTART
     DWREAD    R    PSTART           READ THE NEXT CARD
     DWDOTS    DCW  @........@
     DWLOW     DCW  49               LOW ORDER TWO DIGITS
     DWBEGN    DCW  00333            BEGIN OF LINE
     DWAREA    DC   @-AREA @
               JOB  Card 15 of core dump routine
     * Set word marks, move some code to 164-200.
               ORG  1
     OSTART    SW   O1,O2
     O1        SW   O3,O4
     O2        SW   O5,O6
     O3        NOP
     O4        SW   O7
     O5        SW   O8
     O6        LCA  O9,DWAREA
     O7        R    PSTART
     O8        DCW  @)@              CW JUST BEFORE SWITCH
     O9        DC   @078N080,0011001........4900333-AREA @
               JOB  Card 16 of core dump routine
     * Move some code to 125-163, set some word marks.
               ORG  1
     PSTART    LCA  P1,DW12&7
               SW   DWBEGN-4,DWLOW-1
               SW   DW12,DW12
               SW
               SW   DWDOTS-7
               SW   DWREAD
               SW   DW14,SWITCH
               R    QSTART
               DCW  @)2A2B1410!0}B148M1252A2A176099A@
     P1        DC   @B1060970@
               JOB  Card 17 of core dump routine
     * Move some code to 87-124, set some word marks.
               ORG  1
     QSTART    LCA  Q1,DW8&7
               SW   NOGM,GOTGM
               SW   DW5,DWLOOP
               SW
               SW   DW9
               SW   GOTWM
               SW   DW8,DW7
               R    RSTART
               DCW  @X00FF333010332/332/,2A2M0!02A2@
     Q1        DC   @V1290!01@
               JOB  Card 18 of core dump routine
     * Set some word marks, move some code to 78-86
     * (actually part of an instruction)
               ORG  1
     RSTART    SW   R1,SDONE
               SW   S1,SLOW-6
               LCA  R5,DW1&3
               NOP
               CW   O8
               CW   O5
               SW   X2&1,X2-2
               SW   X1&1,DW1
     R1        R    SSTART
               DCW  #11
     R5        DCW  @   1/B168@
               JOB  Card 19 of core dump routine
     * Move "00333-AREA " with zero suppression to
     * 201-211.  Update "xxxxx-AREA " to 00400.  Put
     * .....39 - ........99 in print area.  Go to print it
     * and to set up to print 333-399 area.
               ORG  1
     SSTART    MCS  DWAREA,211
               MCW  SADDR,DWBEGN     REPLACE 333 WITH 400
     SLOOP     LCA  DWLOW,261        FIRST DOTS OFFSET 49
               LCA                   MOVE THE DOTS
               BCE  SDONE,DWLOW-1,9  DONE FILLING DOTS?
               A    DWREAD,DWLOW-1   BUMP DOTS ADDR BY 10
               A    DWREAD,SLOOP&5   BUMP DOTS POINT BY 10
               B    SLOOP            AROUND AGAIN
     SDONE     LCA  SLOW,251         PUT ......39 IN PRINT
     S1        B    DWPRNT           GO PRINT IT
     SLOW      DCW  @.....39@
     SADDR     DC   00400
               JOB  Test for a blank x00-x99 area
     * Reset ........xx to ........09.  Set starting
     * position for dots to 221.  Check for a blank line
     * without word marks.
               ORG  333
     TBSTRT    MN   DW14&1,DWLOW-1   ZERO TO ........x9
     TB1       MN   WLOOP-1,WLOOP&4  322 BACK TO 222
     TB2       MCW  X2,X1
     TB3       SW   323
     TBLOOP    C    9&X1,332         BLANK AREA?
               BU   DWPRNT           NO, PRINT
     TB4       BW   DWPRNT,0&X1      WORDMARK? NO, PRINT
     TB5       BCE  WRET,X1-1,9      DONE?
     TB6       A    X3-2,X1          BUMP X1 BY 10 -- MA FOR BIG CORE
     TB7       B    TBLOOP           AROUND AGAIN
     MSIZ      DCW  @014@            MEMORY SIZE / 100
               JOB  Card 20 of core dump routine
     * Move some code to 333-364.  Set some word marks.
               ORG  1
     TSTART    CW   S1,SLOW-6
               LCA  T2,TBLOOP&6
               SW   TB1,TB2
               NOP
               NOP  0,0,0
     T1        SW   TB3,TBLOOP
               CW   T1,T1
               R    USTART
     T2        DCW  @D173188D014019M094089,323C0'9332@
               JOB  Card 21 of core dump routine
     * Move some code to 365-396.  Set some word marks.
               ORG  1
     USTART    LCA  U2,TB7&3
               SW   U1,V1
               SW   TB4,TB5
               NOP
               SW   TB6,TB7
     U1        NOP  0,0,0
               SW   DWSTRT,DWSTRT
               R    VSTART
               DCW  @B@              B OF  BU DWPRNT
     U2        DC   @100/V1000'01B0490889A097089B358@
               JOB  Card 22 of core dump routine
     * Set some word marks.  Change SWITCH to 2060N
     * Change first ........x9 to ........09.
     * Move the core size to 396-399.
               ORG  1
     VSTART    SW   XRET,W4
               SW   W5,WTEST
               M    VSWICH,SWITCH&3  CHANGE SWITCH TO 2060
               M
     V1        MN   DWREAD&1,DWLOW-1  X9 TO ........09
               NOP  0,0,0
               LCA  VCORE,MSIZ       SAVE CORE SIZE
               R    WSTART           READ THE NEXT CARD
               W    XRET             NEW FOR SWITCH
     VSWICH    NOP
               DC   @  @
     VCORE     DCW  014              CORE SIZE / 100
               DC   00               TENS DIGIT OF CORE
               DCW  @  CORE SIZE@    JUST A COMMENT
               JOB  Card 23 of core dump routine
     * Move "xxxxx-AREA " with zero supression to 201-211.
     * Update xxxxx by 100.
     * Put ........09 - ........99 to 212-311.
     * Go put data and word marks in the print area and
     * print the data.  Print the word marks on return.
               ORG  1
     WSTART    MCS  DWAREA,211
               A    DWREAD,DWBEGN-2  BUMP ADDR BY 100
     WLOOP     LCA  DWLOW,221        ........X9 TO PRINT
               LCA
               A    DWREAD,WLOOP&5   BUMP ........X9 POSN
               BCE  TBSTRT,DWLOW-1,9  DONE WITH ........X9?
               A    DWREAD,DWLOW-1   BUMP X IN ........X9
               B    WLOOP            AROUND AGAIN
     WRET      A    DWREAD&2,X2      BUMP CORE START BY 100 - MA IF BIG
               B    WTEST
     XRET         2)                 PRINT THE WORD MARKS
     W4        CC   J                SKIP ONE LINE
     W5        MN   DW14&1,X3-2      ZERO TO HIGH DIGIT
     WTEST     C    MSIZ,DWBEGN-2    DONE?
               BU   WSTART           DOESN'T FIT, BUT OK
     *                               (rest is on card R)
               JOB  Card 24 of core dump routine
     * Print whether Sense switch A is on.
               ORG  1
     YSTART    SW   Y1,Y3
               CW   XRET,WTEST
               SW   Y2,YHALT
               SW
               CW   W4,DWSTRT
               NOP
     Y1        CS   332
               CS
     Y2           N0
               LCA  YSWA,213
               SW   YHALT&4
               BSS  YPRINT,A         SS A ON?
     Y3           N0
               MCW  DFF,214          CHANGE MSG TO  OFF
     YPRINT    W
     YHALT     H    YHALT            ALL DONE
     YSWA      DCW  @SENSE SW A ON@
               JOB  Alternative card 12 of core dump routine
     * Clear routine that gets moved to 81-92
               ORG  81
               CS   80
     SETWM2    SW   1
     READX2    R    1
     *
     * First card of two-card alternative sequence for cards 12-14
     * Move appropriate comparison indicators to 247..265.  Move
     * R 001  to 92.
               ORG  1
               SFX  L
     INC       DCW  5003             INC FOR TWO ADDRS
               DC   #6
     START     BU   INDON            INDICATOR ON?
               LCA  MREAD&3,READX2&3  SOME INDICATOR WILL BE OFF
               B    INDOF
               DC   #3               B WITH BLANK D NEEDS NO WM
     MREAD     R    001              GETS MOVED TO 89-92
               DCW  #7
     INDON     MCW  110,256          MOVE INDICATOR TO PR
               NOP
     INDOF     A    READ,START&4    INCR INDICATOR TEST
               A    INC,INDON&6     INCR BOTH ADDRS
               BCE  READ,START&4,V  DONE?
               NOP
               B    START            AROUND AGAIN
     READ      R    STARTM           READ THE NEXT CARD
               DCW  #1
               JOB  Alternative card 13 of core dump routine
     * Second card of two-card alternative sequence for cards 12-14
     * Construct overflow off (or on) indicator.  Move it to 268..277.
     * Move CS 80, SW 1 to 81-88.  Set word marks for it.
     * Print indicators. Clear 200-299 and 0-80.  Set word mark in 1,
     * read a card and branch to 1.
               SFX  M
               ORG  1
     OVMSG     DCW  @OVFLO ON @
               DC   #1
     START     BAV  OVON             OVERFLOW ON?
               MCW  OVFF,OVMSG       NO, CHANGE ON TO OFF
     OVON      MCW  OVMSG,277        MOVE MESSAGE TO PRINT AREA
               SW   OVCC             NEED A WM
               LCA  OVCLR,SETWM2&3   MOVE PART OF CLEAR ROUTINE
               SW   SETWM2,READX2&4  IT NEEDS A WM
               NOP
               B    OVFIN
               DCW  #3               BRANCH WITH BLANK D NEEDS NO WM
     OVFF      DCW  @FF@
               DC   #5
               CS   80               GETS MOVED TO 81-84
     OVCLR     DC   @,001@           GETS MOVED TO 85-88
     OVFIN     W
               CC   L                SKIP 3 LINES
     OVCC      CC   K                SKIP 2 LINES
               CS   299
               NOP
               END
