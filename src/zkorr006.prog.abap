REPORT ZKORR006 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Report listing cost centres showing a cc group only
*               (no individual cc's) with related cost element group
*               totals also (for for each cc). - Union Energy Only

*  Column Headings for columns 1-35 (Head1, Head2, Head3, Head4, etc.)
*    If parameter entered, print parameter
*    If only 1 entry and no range, print that cost centre
*    If only 1 entry and a range, print cost centre with *
*    If more than 1 entry, print first cost centre with *
*
* memory id on parameters/select-options initializes fields with user
*           parameters


************************************************************************
* 2001/02/14 mdemeest 4.6B Changed function calls for Groups
*                          CE - '0H' --> '0101' COAT + CE Group
*                          CC - '0H' --> '0102' 10 + 2 spaces + CC Grp

* 2000/06/15 gymana   4.6B Changed a couple of invalid text elements
*                          and added an intermediate work field to split
*                          the report header for the Excel spreadsheet
*                          since the report header is a text element and
*                          cannot be split itself.

* 1999/08/09 mdemeest #--- Test for plan version in CHECK_CENTRES_*
* 1999/07/14 mdemeest #487 Original request - used ZKORR005 as template
*
************************************************************************
TABLES: BKPF,
        T800S,
        T800Y,
        COSP,                             "External Postings
        COSS,
        RKMA1,
        RKMA4,                            "Cost Elements
        CSKU.                             "Cost Elements Description

FIELD-SYMBOLS: <F1>,
               <F2>,
               <FS>.
DATA: SKOST(8) TYPE C,
      PKOST LIKE RKMA1-KOSTL,
      KOSTL LIKE RGSBS-SETNR,
      KOST(23) TYPE C,
      FIELD(5).
DATA: NO_OF_LINES TYPE I.

DATA:  SETNAME     LIKE RGSBS-SETNR.

DATA: VALUE   LIKE COSP-WKG001.

DATA: TTLWRITE(1) TYPE C. "Determines group which has the totals
DATA:  GROUP(1)   TYPE C. "35 columns divided into 7 groups of 5.
DATA:  HEAD1(25)  TYPE C.
DATA:  HEAD2(25)  TYPE C.
DATA:  HEAD3(25)  TYPE C.
DATA:  HEAD4(25)  TYPE C.
DATA:  HEAD5(25)  TYPE C.
DATA:  HEAD6(25)  TYPE C.
DATA:  HEAD7(25)  TYPE C.
DATA:  HEAD8(25)  TYPE C.
DATA:  HEAD9(25)  TYPE C.
DATA:  HEAD10(25) TYPE C.
DATA:  HEAD11(25) TYPE C.
DATA:  HEAD12(25) TYPE C.
DATA:  HEAD13(25) TYPE C.
DATA:  HEAD14(25) TYPE C.
DATA:  HEAD15(25) TYPE C.
DATA:  HEAD16(25) TYPE C.
DATA:  HEAD17(25) TYPE C.
DATA:  HEAD18(25) TYPE C.
DATA:  HEAD19(25) TYPE C.
DATA:  HEAD20(25) TYPE C.
DATA:  HEAD21(25) TYPE C.
DATA:  HEAD22(25) TYPE C.
DATA:  HEAD23(25) TYPE C.
DATA:  HEAD24(25) TYPE C.
DATA:  HEAD25(25) TYPE C.
DATA:  HEAD26(25) TYPE C.
DATA:  HEAD27(25) TYPE C.
DATA:  HEAD28(25) TYPE C.
DATA:  HEAD29(25) TYPE C.
DATA:  HEAD30(25) TYPE C.
DATA:  HEAD31(25) TYPE C.
DATA:  HEAD32(25) TYPE C.
DATA:  HEAD33(25) TYPE C.
DATA:  HEAD34(25) TYPE C.
DATA:  HEAD35(25) TYPE C.
DATA:  HD_TITLE(36) TYPE C.


DATA:   BEGIN OF NAME_TAB OCCURS 10.               "Cost Elements
           INCLUDE STRUCTURE RGSB4.
DATA:   END OF NAME_TAB.

DATA:   BEGIN OF CC_TAB OCCURS 10.                 "Cost Centres
           INCLUDE STRUCTURE RGSB4.
DATA:   END OF CC_TAB.

DATA:   PREV_POSITION TYPE I.
DATA:   PREV_SETNR      LIKE NAME_TAB-SETNR.

DATA:   BEGIN OF BIG_TABLE OCCURS 100,
           POSITION TYPE I,           "Order accordingly to structure
           SETNR   LIKE NAME_TAB-SETNR,
           KSTAR   LIKE COSP-KSTAR,                "Cost Element
           LTEXT   LIKE CSKU-LTEXT,                "Description

           AMTA1   LIKE COSP-WKG001,               "Actuals
           AMTA2   LIKE COSP-WKG001,
           AMTA3   LIKE COSP-WKG001,
           AMTA4   LIKE COSP-WKG001,
           AMTA5   LIKE COSP-WKG001,
           AMTA6   LIKE COSP-WKG001,
           AMTA7   LIKE COSP-WKG001,
           AMTA8   LIKE COSP-WKG001,
           AMTA9   LIKE COSP-WKG001,
           AMTA10  LIKE COSP-WKG001,
           AMTA11  LIKE COSP-WKG001,
           AMTA12  LIKE COSP-WKG001,
           AMTA13  LIKE COSP-WKG001,
           AMTA14  LIKE COSP-WKG001,
           AMTA15  LIKE COSP-WKG001,
           AMTA16  LIKE COSP-WKG001,
           AMTA17  LIKE COSP-WKG001,
           AMTA18  LIKE COSP-WKG001,
           AMTA19  LIKE COSP-WKG001,
           AMTA20  LIKE COSP-WKG001,
           AMTA21  LIKE COSP-WKG001,
           AMTA22  LIKE COSP-WKG001,
           AMTA23  LIKE COSP-WKG001,
           AMTA24  LIKE COSP-WKG001,
           AMTA25  LIKE COSP-WKG001,
           AMTA26  LIKE COSP-WKG001,
           AMTA27  LIKE COSP-WKG001,
           AMTA28  LIKE COSP-WKG001,
           AMTA29  LIKE COSP-WKG001,
           AMTA30  LIKE COSP-WKG001,
           AMTA31  LIKE COSP-WKG001,
           AMTA32  LIKE COSP-WKG001,
           AMTA33  LIKE COSP-WKG001,
           AMTA34  LIKE COSP-WKG001,
           AMTA35  LIKE COSP-WKG001,

           AMTP1   LIKE COSP-WKG001,                "Plans
           AMTP2   LIKE COSP-WKG001,
           AMTP3   LIKE COSP-WKG001,
           AMTP4   LIKE COSP-WKG001,
           AMTP5   LIKE COSP-WKG001,
           AMTP6   LIKE COSP-WKG001,
           AMTP7   LIKE COSP-WKG001,
           AMTP8   LIKE COSP-WKG001,
           AMTP9   LIKE COSP-WKG001,
           AMTP10  LIKE COSP-WKG001,
           AMTP11  LIKE COSP-WKG001,
           AMTP12  LIKE COSP-WKG001,
           AMTP13  LIKE COSP-WKG001,
           AMTP14  LIKE COSP-WKG001,
           AMTP15  LIKE COSP-WKG001,
           AMTP16  LIKE COSP-WKG001,
           AMTP17  LIKE COSP-WKG001,
           AMTP18  LIKE COSP-WKG001,
           AMTP19  LIKE COSP-WKG001,
           AMTP20  LIKE COSP-WKG001,
           AMTP21  LIKE COSP-WKG001,
           AMTP22  LIKE COSP-WKG001,
           AMTP23  LIKE COSP-WKG001,
           AMTP24  LIKE COSP-WKG001,
           AMTP25  LIKE COSP-WKG001,
           AMTP26  LIKE COSP-WKG001,
           AMTP27  LIKE COSP-WKG001,
           AMTP28  LIKE COSP-WKG001,
           AMTP29  LIKE COSP-WKG001,
           AMTP30  LIKE COSP-WKG001,
           AMTP31  LIKE COSP-WKG001,
           AMTP32  LIKE COSP-WKG001,
           AMTP33  LIKE COSP-WKG001,
           AMTP34  LIKE COSP-WKG001,
           AMTP35  LIKE COSP-WKG001,

           AMTA36   LIKE COSP-WKG001,
           AMTP36   LIKE COSP-WKG001,
           AMTV36   LIKE COSP-WKG001.
DATA:   END OF BIG_TABLE.

*------------------- Excel Spreadsheet Internal Table ------------------
DATA:   BEGIN OF XLS_NAME OCCURS 100,
           KSTAR(12)   TYPE C,                    "Cost Element
           LTEXT(40)   TYPE C,                    "Description

           AMTA1(12)   TYPE C,                    "Col 1 Actual
           AMTP1(12)   TYPE C,                    "Col 1 Plan

           AMTA2(12)   TYPE C,                    "Col 2 Actual
           AMTP2(12)   TYPE C,                    "Col 2 Plan

           AMTA3(12)   TYPE C,                    "Col 3 Actual
           AMTP3(12)   TYPE C,                    "Col 3 Plan

           AMTA4(12)   TYPE C,                    "Col 4 Actual
           AMTP4(12)   TYPE C,                    "Col 4 Plan

           AMTA5(12)   TYPE C,                    "Col 5 Actual
           AMTP5(12)   TYPE C,                    "Col 5 Plan

           AMTA6(12)   TYPE C,                    "Col 6 Actual
           AMTP6(12)   TYPE C,                    "Col 6 Plan

           AMTA7(12)   TYPE C,                    "Col 7 Actual
           AMTP7(12)   TYPE C,                    "Col 7 Plan

           AMTA8(12)   TYPE C,                    "Col 8 Actual
           AMTP8(12)   TYPE C,                    "Col 8 Plan

           AMTA9(12)   TYPE C,                    "Col 9 Actual
           AMTP9(12)   TYPE C,                    "Col 9 Plan

           AMTA10(12)   TYPE C,                    "Col10 Actual
           AMTP10(12)   TYPE C,                    "Col10 Plan

           AMTA11(12)   TYPE C,                    "Col11 Actual
           AMTP11(12)   TYPE C,                    "Col11 Plan

           AMTA12(12)   TYPE C,                    "Col12 Actual
           AMTP12(12)   TYPE C,                    "Col12 Plan

           AMTA13(12)   TYPE C,                    "Col13 Actual
           AMTP13(12)   TYPE C,                    "Col13 Plan

           AMTA14(12)   TYPE C,                    "Col14 Actual
           AMTP14(12)   TYPE C,                    "Col14 Plan

           AMTA15(12)   TYPE C,                    "Col15 Actual
           AMTP15(12)   TYPE C,                    "Col15 Plan

           AMTA16(12)   TYPE C,                    "Col16 Actual
           AMTP16(12)   TYPE C,                    "Col16 Plan

           AMTA17(12)   TYPE C,                    "Col17 Actual
           AMTP17(12)   TYPE C,                    "Col17 Plan

           AMTA18(12)   TYPE C,                    "Col18 Actual
           AMTP18(12)   TYPE C,                    "Col18 Plan

           AMTA19(12)   TYPE C,                    "Col19 Actual
           AMTP19(12)   TYPE C,                    "Col19 Plan

           AMTA20(12)   TYPE C,                    "Col20 Actual
           AMTP20(12)   TYPE C,                    "Col20 Plan

           AMTA21(12)   TYPE C,                    "Col21 Actual
           AMTP21(12)   TYPE C,                    "Col21 Plan

           AMTA22(12)   TYPE C,                    "Col22 Actual
           AMTP22(12)   TYPE C,                    "Col22 Plan

           AMTA23(12)   TYPE C,                    "Col23 Actual
           AMTP23(12)   TYPE C,                    "Col23 Plan

           AMTA24(12)   TYPE C,                    "Col24 Actual
           AMTP24(12)   TYPE C,                    "Col24 Plan

           AMTA25(12)   TYPE C,                    "Col25 Actual
           AMTP25(12)   TYPE C,                    "Col25 Plan

           AMTA26(12)   TYPE C,                    "Col26 Actual
           AMTP26(12)   TYPE C,                    "Col26 Plan

           AMTA27(12)   TYPE C,                    "Col27 Actual
           AMTP27(12)   TYPE C,                    "Col27 Plan

           AMTA28(12)   TYPE C,                    "Col28 Actual
           AMTP28(12)   TYPE C,                    "Col28 Plan

           AMTA29(12)   TYPE C,                    "Col29 Actual
           AMTP29(12)   TYPE C,                    "Col29 Plan

           AMTA30(12)   TYPE C,                    "Col30 Actual
           AMTP30(12)   TYPE C,                    "Col30 Plan

           AMTA31(12)   TYPE C,                    "Col31 Actual
           AMTP31(12)   TYPE C,                    "Col31 Plan

           AMTA32(12)   TYPE C,                    "Col32 Actual
           AMTP32(12)   TYPE C,                    "Col32 Plan

           AMTA33(12)   TYPE C,                    "Col33 Actual
           AMTP33(12)   TYPE C,                    "Col33 Plan

           AMTA34(12)   TYPE C,                    "Col34 Actual
           AMTP34(12)   TYPE C,                    "Col34 Plan

           AMTA35(12)   TYPE C,                    "Col35 Actual
           AMTP35(12)   TYPE C,                    "Col35 Plan

           AMTA36(12)   TYPE C,                   "Col 36 Actual
           AMTP36(12)   TYPE C,                   "Col 36 Plan
           AMTV36(12)   TYPE C.                   "Col 36 Variance

DATA:   END OF XLS_NAME.


*-----------------------------------------------------------------------

*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-127.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_KOKRS  LIKE COEP-KOKRS      OBLIGATORY     MEMORY ID  CAC,
            P_SEARCH LIKE T800S-SEARCHFLD OBLIGATORY     MEMORY ID  KPL,
            P_BUKRS(4)                    OBLIGATORY     MEMORY ID  BUK,
            P_GJAHR  LIKE COSP-GJAHR   OBLIGATORY DEFAULT SY-DATUM(4).
SELECT-OPTIONS:
            S_MTH    FOR BKPF-MONAT    OBLIGATORY DEFAULT '01' TO '12'.
PARAMETERS: P_VERS   LIKE TKVST-VERSI  OBLIGATORY DEFAULT '000'.
PARAMETERS: P_COSTGR LIKE KKB0-KST MEMORY ID KAG. "Cost Element Group

SELECT-OPTIONS:
            S_KSTAR  FOR RKMA4-KSTAR MATCHCODE OBJECT KART. "Cost Elemnt
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-128.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            S_KOST1 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST. "Cost Centre
PARAMETER:  P_KOST1 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST2 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST2 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST3 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST3 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST4 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST4 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST5 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST5 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST6 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST6 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST7 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST7 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST8 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST8 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST9 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST9 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST10 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST10 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST11 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST11 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST12 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST12 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST13 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST13 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST14 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST14 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST15 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST15 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST16 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST16 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST17 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST17 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST18 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST18 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST19 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST19 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST20 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST20 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST21 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST21 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST22 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST22 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST23 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST23 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST24 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST24 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST25 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST25 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST26 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST26 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST27 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST27 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST28 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST28 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST29 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST29 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST30 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST30 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST31 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST31 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST32 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST32 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST33 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST33 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST34 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST34 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOST35 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOST35 LIKE RGSB4-SETNR.


SELECTION-SCREEN END OF BLOCK BOX1.
SKIP 1.

*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, 15 SY-REPID INTENSIFIED ON,                "Title
      110 TEXT-HD1,
      220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-HD2 UNDER TEXT-HD1,                            "Title
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
WRITE: / TEXT-CMP UNDER TEXT-RPT, P_BUKRS  UNDER SY-REPID.   "Company Cd
WRITE: / TEXT-VER UNDER TEXT-RPT, P_VERS   UNDER SY-REPID,   "Version
         TEXT-034 UNDER TEXT-HD1.
WRITE:   132 S_MTH+3(2).
IF S_MTH+5(2) = '00'.                         "Entered from period only
         WRITE: 135 P_GJAHR.
ELSE.                                         "Entered from & to periods
         WRITE: 137 S_MTH+5(2), 140 P_GJAHR.
ENDIF.

IF P_COSTGR IS INITIAL.                       "Cost Element Group
   WRITE: / TEXT-035, 'N/A' UNDER SY-REPID.
ELSE.
   WRITE: / TEXT-035, P_COSTGR UNDER SY-REPID.
ENDIF.

ULINE.
PERFORM WRITE_VERT.
WRITE:  2 TEXT-001,  81 TEXT-004,  108 TEXT-009,  135 TEXT-014,
      162 TEXT-019, 189 TEXT-024, 216 TEXT-030.
CASE GROUP.
  WHEN ' '.
    WRITE: HEAD1  UNDER TEXT-004, HEAD2  UNDER TEXT-009,
           HEAD3  UNDER TEXT-014, HEAD4  UNDER TEXT-019,
           HEAD5  UNDER TEXT-024.
  WHEN '2'.
    WRITE: HEAD6  UNDER TEXT-004, HEAD7  UNDER TEXT-009,
           HEAD8  UNDER TEXT-014, HEAD9  UNDER TEXT-019,
           HEAD10 UNDER TEXT-024.
  WHEN '3'.
    WRITE: HEAD11 UNDER TEXT-004, HEAD12 UNDER TEXT-009,
           HEAD13 UNDER TEXT-014, HEAD14 UNDER TEXT-019,
           HEAD15 UNDER TEXT-024.
  WHEN '4'.
    WRITE: HEAD16 UNDER TEXT-004, HEAD17 UNDER TEXT-009,
           HEAD18 UNDER TEXT-014, HEAD19 UNDER TEXT-019,
           HEAD20 UNDER TEXT-024.
  WHEN '5'.
    WRITE: HEAD21 UNDER TEXT-004, HEAD22 UNDER TEXT-009,
           HEAD23 UNDER TEXT-014, HEAD24 UNDER TEXT-019,
           HEAD25 UNDER TEXT-024.
  WHEN '6'.
    WRITE: HEAD26 UNDER TEXT-004, HEAD27 UNDER TEXT-009,
           HEAD28 UNDER TEXT-014, HEAD29 UNDER TEXT-019,
           HEAD30 UNDER TEXT-024.
  WHEN '7'.
    WRITE: HEAD31 UNDER TEXT-004, HEAD32 UNDER TEXT-009,
           HEAD33 UNDER TEXT-014, HEAD34 UNDER TEXT-019,
           HEAD35 UNDER TEXT-024.
ENDCASE.
PERFORM WRITE_VERT.
WRITE: TEXT-002 UNDER TEXT-001,
       TEXT-005 UNDER TEXT-004,  94 TEXT-006,           "col 1
       TEXT-010 UNDER TEXT-009, 121 TEXT-011,           "col 2
       TEXT-015 UNDER TEXT-014, 148 TEXT-016,           "col 3
       TEXT-020 UNDER TEXT-019, 175 TEXT-021,           "col 4
       TEXT-025 UNDER TEXT-024, 202 TEXT-026,
       TEXT-031 UNDER TEXT-030, 229 TEXT-032, 242 TEXT-033.
ULINE.


* Either a cost element group is entered or at least one cost element
AT SELECTION-SCREEN.
  IF ( S_KSTAR  IS INITIAL   AND  P_COSTGR IS INITIAL ) OR
     ( S_KSTAR(1) <> SPACE   AND  P_COSTGR(1) <> SPACE ).
     MESSAGE E001.                                   "Message table T100
  ENDIF.

*-----------------------------------------------------------------------
* Either a cost centre group is entered or at least one cost centre
* for column 1.  Columns 2 to 5 are optional.  However, if information
* is entered, it can be in only the cost centre group or the cost
* centre, not both.
*-----------------------------------------------------------------------

  DO 35 TIMES.
     CLEAR: SKOST, PKOST.
     SKOST = 's_kost'.
     PKOST = 'p_kost'.
     SKOST+6(2) = SY-INDEX.
     PKOST+6(2) = SY-INDEX.
     ASSIGN (SKOST) TO <F1>.
     ASSIGN (PKOST) TO <F2>.

    IF ( <F1>  IS INITIAL   AND   <F2> IS INITIAL ) OR
       ( <F1> <> SPACE      AND   <F2> <> SPACE ).
       IF SY-INDEX = 1.
          MESSAGE E002.
       ENDIF.
  ELSE.
     IF <F2> <> SPACE.                "Entry is cost centre group
        CASE SY-INDEX.
          WHEN 01.  HEAD1+9(12) = <F2>.     "Column 1 Heading
          WHEN 02.  HEAD2+9(12) = <F2>.     "Column 2 Heading
          WHEN 03.  HEAD3+9(12) = <F2>.     "Column 3 Heading
          WHEN 04.  HEAD4+9(12) = <F2>.     "Column 4 Heading
          WHEN 05.  HEAD5+9(12) = <F2>.     "Column 5 Heading
          WHEN 06.  HEAD6+9(12) = <F2>.     "Column 6 Heading
          WHEN 07.  HEAD7+9(12) = <F2>.     "Column 7 Heading
          WHEN 08.  HEAD8+9(12) = <F2>.     "Column 8 Heading
          WHEN 09.  HEAD9+9(12) = <F2>.     "Column 9 Heading
          WHEN 10.  HEAD10+9(12) = <F2>.    "Column 10 Heading
          WHEN 11.  HEAD11+9(12) = <F2>.    "Column 11 Heading
          WHEN 12.  HEAD12+9(12) = <F2>.    "Column 12 Heading
          WHEN 13.  HEAD13+9(12) = <F2>.    "Column 13 Heading
          WHEN 14.  HEAD14+9(12) = <F2>.    "Column 14 Heading
          WHEN 15.  HEAD15+9(12) = <F2>.    "Column 15 Heading
          WHEN 16.  HEAD16+9(12) = <F2>.    "Column 16 Heading
          WHEN 17.  HEAD17+9(12) = <F2>.    "Column 17 Heading
          WHEN 18.  HEAD18+9(12) = <F2>.    "Column 18 Heading
          WHEN 19.  HEAD19+9(12) = <F2>.    "Column 19 Heading
          WHEN 20.  HEAD20+9(12) = <F2>.    "Column 20 Heading
          WHEN 21.  HEAD21+9(12) = <F2>.    "Column 21 Heading
          WHEN 22.  HEAD22+9(12) = <F2>.    "Column 22 Heading
          WHEN 23.  HEAD23+9(12) = <F2>.    "Column 23 Heading
          WHEN 24.  HEAD24+9(12) = <F2>.    "Column 24 Heading
          WHEN 25.  HEAD25+9(12) = <F2>.    "Column 25 Heading
          WHEN 26.  HEAD26+9(12) = <F2>.    "Column 26 Heading
          WHEN 27.  HEAD27+9(12) = <F2>.    "Column 27 Heading
          WHEN 28.  HEAD28+9(12) = <F2>.    "Column 28 Heading
          WHEN 29.  HEAD29+9(12) = <F2>.    "Column 29 Heading
          WHEN 30.  HEAD30+9(12) = <F2>.    "Column 30 Heading
          WHEN 31.  HEAD31+9(12) = <F2>.    "Column 31 Heading
          WHEN 32.  HEAD32+9(12) = <F2>.    "Column 32 Heading
          WHEN 33.  HEAD33+9(12) = <F2>.    "Column 33 Heading
          WHEN 34.  HEAD34+9(12) = <F2>.    "Column 34 Heading
          WHEN 35.  HEAD35+9(12) = <F2>.    "Column 35 Heading
        ENDCASE.
     ELSE.                     "Entry is cost centre
       CASE SY-INDEX.
        WHEN 01. CONCATENATE: S_KOST1+3(10) '*' INTO HEAD1+9(12).
                 DESCRIBE TABLE S_KOST1 LINES NO_OF_LINES.
                 IF ( S_KOST1+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD1+9(12) = S_KOST1+3(10).
                 ENDIF.
        WHEN 02. CONCATENATE: S_KOST2+3(10) ' *' INTO HEAD2+9(12).
                 DESCRIBE TABLE S_KOST2 LINES NO_OF_LINES.
                 IF ( S_KOST2+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD2+9(12) = S_KOST2+3(10).
                 ENDIF.
        WHEN 03. CONCATENATE: S_KOST3+3(10) ' *' INTO HEAD3+9(12).
                 DESCRIBE TABLE S_KOST3 LINES NO_OF_LINES.
                 IF ( S_KOST3+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD3+9(12) = S_KOST3+3(10).
                 ENDIF.
        WHEN 04. CONCATENATE: S_KOST4+3(10) ' *' INTO HEAD4+9(12).
                 DESCRIBE TABLE S_KOST4 LINES NO_OF_LINES.
                 IF ( S_KOST4+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD4+9(12) = S_KOST4+3(10).
                 ENDIF.
        WHEN 05. CONCATENATE: S_KOST5+3(10) ' *' INTO HEAD5+9(12).
                 DESCRIBE TABLE S_KOST5 LINES NO_OF_LINES.
                 IF ( S_KOST5+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                   HEAD5+9(12) = S_KOST5+3(10).
                 ENDIF.
        WHEN 06. CONCATENATE: S_KOST6+3(10) ' *' INTO HEAD6+9(12).
                 DESCRIBE TABLE S_KOST6 LINES NO_OF_LINES.
                 IF ( S_KOST6+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD6+9(12) = S_KOST6+3(10).
                 ENDIF.
        WHEN 07. CONCATENATE: S_KOST7+3(10) ' *' INTO HEAD7+9(12).
                 DESCRIBE TABLE S_KOST7 LINES NO_OF_LINES.
                 IF ( S_KOST7+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD7+9(12) = S_KOST7+3(10).
                 ENDIF.
        WHEN 08. CONCATENATE: S_KOST8+3(10) ' *' INTO HEAD8+9(12).
                 DESCRIBE TABLE S_KOST8 LINES NO_OF_LINES.
                 IF ( S_KOST8+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD8+9(12) = S_KOST8+3(10).
                 ENDIF.
        WHEN 09. CONCATENATE: S_KOST9+3(10) ' *' INTO HEAD9+9(12).
                 DESCRIBE TABLE S_KOST9 LINES NO_OF_LINES.
                 IF ( S_KOST9+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD9+9(12) = S_KOST9+3(10).
                 ENDIF.
        WHEN 10. CONCATENATE: S_KOST10+3(10) ' *' INTO HEAD10+9(12).
                 DESCRIBE TABLE S_KOST10 LINES NO_OF_LINES.
                 IF ( S_KOST10+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD10+9(12) = S_KOST10+3(10).
                 ENDIF.
        WHEN 11.  CONCATENATE: S_KOST11+3(10) ' *' INTO HEAD11+9(12).
                 DESCRIBE TABLE S_KOST11 LINES NO_OF_LINES.
                 IF ( S_KOST11+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD11+9(12) = S_KOST11+3(10).
                 ENDIF.
        WHEN 12.  CONCATENATE: S_KOST12+3(10) ' *' INTO HEAD12+9(12).
                 DESCRIBE TABLE S_KOST12 LINES NO_OF_LINES.
                 IF ( S_KOST12+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD12+9(12) = S_KOST12+3(10).
                 ENDIF.
        WHEN 13.  CONCATENATE: S_KOST13+3(10) ' *' INTO HEAD13+9(12).
                 DESCRIBE TABLE S_KOST13 LINES NO_OF_LINES.
                 IF ( S_KOST13+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD13+9(12) = S_KOST13+3(10).
                 ENDIF.
        WHEN 14.  CONCATENATE: S_KOST14+3(10) ' *' INTO HEAD14+9(12).
                 DESCRIBE TABLE S_KOST14 LINES NO_OF_LINES.
                 IF ( S_KOST14+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD14+9(12) = S_KOST14+3(10).
                 ENDIF.
        WHEN 15.  CONCATENATE: S_KOST15+3(10) ' *' INTO HEAD15+9(12).
                 DESCRIBE TABLE S_KOST15 LINES NO_OF_LINES.
                 IF ( S_KOST15+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD15+9(12) = S_KOST15+3(10).
                 ENDIF.
        WHEN 16.  CONCATENATE: S_KOST16+3(10) ' *' INTO HEAD16+9(12).
                 DESCRIBE TABLE S_KOST16 LINES NO_OF_LINES.
                 IF ( S_KOST16+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD16+9(12) = S_KOST16+3(10).
                 ENDIF.
        WHEN 17.  CONCATENATE: S_KOST17+3(10) ' *' INTO HEAD17+9(12).
                 DESCRIBE TABLE S_KOST17 LINES NO_OF_LINES.
                 IF ( S_KOST17+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD17+9(12) = S_KOST17+3(10).
                 ENDIF.
        WHEN 18.  CONCATENATE: S_KOST18+3(10) ' *' INTO HEAD18+9(12).
                 DESCRIBE TABLE S_KOST18 LINES NO_OF_LINES.
                 IF ( S_KOST18+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD18+9(12) = S_KOST18+3(10).
                 ENDIF.
        WHEN 19.  CONCATENATE: S_KOST19+3(10) ' *' INTO HEAD19+9(12).
                 DESCRIBE TABLE S_KOST19 LINES NO_OF_LINES.
                 IF ( S_KOST19+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD19+9(12) = S_KOST19+3(10).
                 ENDIF.
        WHEN 20.  CONCATENATE: S_KOST20+3(10) ' *' INTO HEAD20+9(12).
                 DESCRIBE TABLE S_KOST20 LINES NO_OF_LINES.
                 IF ( S_KOST20+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD20+9(12) = S_KOST20+3(10).
                 ENDIF.
        WHEN 21.  CONCATENATE: S_KOST21+3(10) ' *' INTO HEAD21+9(12).
                 DESCRIBE TABLE S_KOST21 LINES NO_OF_LINES.
                 IF ( S_KOST21+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD21+9(12) = S_KOST21+3(10).
                 ENDIF.
        WHEN 22.  CONCATENATE: S_KOST22+3(10) ' *' INTO HEAD22+9(12).
                 DESCRIBE TABLE S_KOST22 LINES NO_OF_LINES.
                 IF ( S_KOST22+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD22+9(12) = S_KOST22+3(10).
                 ENDIF.
        WHEN 23.  CONCATENATE: S_KOST23+3(10) ' *' INTO HEAD23+9(12).
                 DESCRIBE TABLE S_KOST23 LINES NO_OF_LINES.
                 IF ( S_KOST23+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD23+9(12) = S_KOST23+3(10).
                 ENDIF.
        WHEN 24.  CONCATENATE: S_KOST24+3(10) ' *' INTO HEAD24+9(12).
                 DESCRIBE TABLE S_KOST24 LINES NO_OF_LINES.
                 IF ( S_KOST24+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD24+9(12) = S_KOST24+3(10).
                 ENDIF.
        WHEN 25.  CONCATENATE: S_KOST25+3(10) ' *' INTO HEAD25+9(12).
                 DESCRIBE TABLE S_KOST25 LINES NO_OF_LINES.
                 IF ( S_KOST25+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD25+9(12) = S_KOST25+3(10).
                 ENDIF.
        WHEN 26.  CONCATENATE: S_KOST26+3(10) ' *' INTO HEAD26+9(12).
                 DESCRIBE TABLE S_KOST26 LINES NO_OF_LINES.
                 IF ( S_KOST26+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD26+9(12) = S_KOST26+3(10).
                 ENDIF.
        WHEN 27.  CONCATENATE: S_KOST27+3(10) ' *' INTO HEAD27+9(12).
                 DESCRIBE TABLE S_KOST27 LINES NO_OF_LINES.
                 IF ( S_KOST27+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD27+9(12) = S_KOST27+3(10).
                 ENDIF.
        WHEN 28.  CONCATENATE: S_KOST28+3(10) ' *' INTO HEAD28+9(12).
                 DESCRIBE TABLE S_KOST28 LINES NO_OF_LINES.
                 IF ( S_KOST28+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD28+9(12) = S_KOST28+3(10).
                 ENDIF.
        WHEN 29.  CONCATENATE: S_KOST29+3(10) ' *' INTO HEAD29+9(12).
                 DESCRIBE TABLE S_KOST29 LINES NO_OF_LINES.
                 IF ( S_KOST29+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD29+9(12) = S_KOST29+3(10).
                 ENDIF.
        WHEN 30.  CONCATENATE: S_KOST30+3(10) ' *' INTO HEAD30+9(12).
                 DESCRIBE TABLE S_KOST30 LINES NO_OF_LINES.
                 IF ( S_KOST30+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD30+9(12) = S_KOST30+3(10).
                 ENDIF.
        WHEN 31.  CONCATENATE: S_KOST31+3(10) ' *' INTO HEAD31+9(12).
                 DESCRIBE TABLE S_KOST31 LINES NO_OF_LINES.
                 IF ( S_KOST31+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD31+9(12) = S_KOST31+3(10).
                 ENDIF.
        WHEN 32.  CONCATENATE: S_KOST32+3(10) ' *' INTO HEAD32+9(12).
                 DESCRIBE TABLE S_KOST32 LINES NO_OF_LINES.
                 IF ( S_KOST32+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD32+9(12) = S_KOST32+3(10).
                 ENDIF.
        WHEN 33.  CONCATENATE: S_KOST33+3(10) ' *' INTO HEAD33+9(12).
                 DESCRIBE TABLE S_KOST33 LINES NO_OF_LINES.
                 IF ( S_KOST33+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD33+9(12) = S_KOST33+3(10).
                 ENDIF.
        WHEN 34.  CONCATENATE: S_KOST34+3(10) ' *' INTO HEAD34+9(12).
                 DESCRIBE TABLE S_KOST34 LINES NO_OF_LINES.
                 IF ( S_KOST34+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD34+9(12) = S_KOST34+3(10).
                 ENDIF.
        WHEN 35.  CONCATENATE: S_KOST35+3(10) ' *' INTO HEAD35+9(12).
                 DESCRIBE TABLE S_KOST35 LINES NO_OF_LINES.
                 IF ( S_KOST35+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
                 ELSE.
                    HEAD35+9(12) = S_KOST35+3(10).
                 ENDIF.
     ENDCASE.
     ENDIF.
  ENDIF.

 ENDDO.

IF HEAD31 <> SPACE OR HEAD32 <> SPACE OR HEAD33 <> SPACE OR
   HEAD34 <> SPACE OR HEAD35 <> SPACE.
   TTLWRITE = '7'.
ELSEIF HEAD26 <> SPACE OR HEAD27 <> SPACE OR HEAD28 <> SPACE OR
       HEAD29 <> SPACE OR HEAD30 <> SPACE.
   TTLWRITE = '6'.
ELSEIF HEAD21 <> SPACE OR HEAD22 <> SPACE OR HEAD23 <> SPACE OR
       HEAD24 <> SPACE OR HEAD25 <> SPACE.
   TTLWRITE = '5'.
ELSEIF HEAD16 <> SPACE OR HEAD17 <> SPACE OR HEAD18 <> SPACE OR
       HEAD19 <> SPACE OR HEAD20 <> SPACE.
   TTLWRITE = '4'.
ELSEIF HEAD11 <> SPACE OR HEAD12 <> SPACE OR HEAD13 <> SPACE OR
       HEAD14 <> SPACE OR HEAD15 <> SPACE.
   TTLWRITE = '3'.
ELSEIF HEAD6  <> SPACE OR HEAD7  <> SPACE OR HEAD8  <> SPACE OR
       HEAD9  <> SPACE OR HEAD10 <> SPACE.
   TTLWRITE = '2'.
ELSE. TTLWRITE = ' '.
   ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_COSTGR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0102'   "2001/02/14
            FIELD_NAME         = 'KSTAR'
            SEARCHFLD          = P_SEARCH
            SEARCHFLD_REQUIRED = SPACE
            START_COLUMN       = 10
            START_ROW          = 5
            TABLE              = 'CCSS'
            TYPELIST           = 'BS'
       IMPORTING
            SET_NAME           = P_COSTGR
       EXCEPTIONS
            NO_SET_PICKED      = 1
            OTHERS             = 2.

* Pull down for COST CENTRE GROUPS. *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST1.        "Column 1
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST2.        "Column 2
    PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST3.        "Column 3
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST4.        "Column 4
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST5.        "Column 5
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST5.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST6.        "Column 6
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST6.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST7.        "Column 7
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST7.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST8.        "Column 8
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST8.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST9.        "Column 9
  PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST9.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST10.       "Column 10
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST10.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST11.       "Column 11
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST11.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST12.       "Column 12
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST12.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST13.       "Column 13
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST13.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST14.       "Column 14
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST14.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST15.       "Column 15
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST15.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST16.       "Column 16
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST16.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST17.       "Column 17
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST17.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST18.       "Column 18
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST18.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST19.       "Column 19
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST19.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST20.       "Column 20
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST20.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST21.       "Column 21
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST21.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST22.       "Column 22
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST22.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST23.       "Column 23
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST23.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST24.       "Column 24
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST24.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST25.       "Column 25
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST25.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST26.       "Column 26
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST26.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST27.       "Column 27
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST27.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST28.       "Column 28
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST28.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST29.       "Column 29
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST29.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST30.       "Column 30
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST30.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST31.       "Column 31
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST31.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST32.       "Column 32
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST32.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST33.       "Column 33
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST33.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST34.       "Column 34
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST34.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOST35.       "Column 35
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST35.


*---------------------- START-of-SELECTION -----------------------------
START-OF-SELECTION.
* Get all cost elements in cost element group and append to the
* cost element variant (s_kstar)
  PERFORM GET_COST_ELEMENTS USING P_COSTGR.

  LOOP AT NAME_TAB.
    S_KSTAR(1)     = 'I'.
    S_KSTAR+1(2)   = 'BT'.
    S_KSTAR+3(10)  = NAME_TAB-FROM.
    S_KSTAR+13(10) = NAME_TAB-TO.
    APPEND S_KSTAR.
  ENDLOOP.

* Get all cost centres in cost centre group and append to the
* cost centre variant (s_kost1).
 DO 35 TIMES.
     CLEAR: PKOST, SKOST.
     PKOST = 'p_kost'.
     SKOST = 's_kost'.
     PKOST+6(2) = SY-INDEX.
     SKOST+6(2) = SY-INDEX.
     ASSIGN (SKOST) TO <F1>.
     ASSIGN (PKOST) TO <F2>.
     MOVE  <F2> TO KOSTL.
  PERFORM GET_COST_CENTRES USING KOSTL.

  LOOP AT CC_TAB.
    KOST(1)     = 'I'.
    KOST+1(2)   = 'BT'.
    KOST+3(10) = CC_TAB-FROM.
    KOST+13(10) = CC_TAB-TO.
    MOVE KOST TO <F1>.
    CASE SY-INDEX.
      WHEN '01'.  APPEND S_KOST1.
      WHEN '02'.  APPEND S_KOST2.
      WHEN '03'.  APPEND S_KOST3.
      WHEN '04'.  APPEND S_KOST4.
      WHEN '05'.  APPEND S_KOST5.
      WHEN '06'.  APPEND S_KOST6.
      WHEN '07'.  APPEND S_KOST7.
      WHEN '08'.  APPEND S_KOST8.
      WHEN '09'.  APPEND S_KOST9.
      WHEN '10'.  APPEND S_KOST10.
      WHEN '11'.  APPEND S_KOST11.
      WHEN '12'.  APPEND S_KOST12.
      WHEN '13'.  APPEND S_KOST13.
      WHEN '14'.  APPEND S_KOST14.
      WHEN '15'.  APPEND S_KOST15.
      WHEN '16'.  APPEND S_KOST16.
      WHEN '17'.  APPEND S_KOST17.
      WHEN '18'.  APPEND S_KOST18.
      WHEN '19'.  APPEND S_KOST19.
      WHEN '20'.  APPEND S_KOST20.
      WHEN '21'.  APPEND S_KOST21.
      WHEN '22'.  APPEND S_KOST22.
      WHEN '23'.  APPEND S_KOST23.
      WHEN '24'.  APPEND S_KOST24.
      WHEN '25'.  APPEND S_KOST25.
      WHEN '26'.  APPEND S_KOST26.
      WHEN '27'.  APPEND S_KOST27.
      WHEN '28'.  APPEND S_KOST28.
      WHEN '29'.  APPEND S_KOST29.
      WHEN '30'.  APPEND S_KOST30.
      WHEN '31'.  APPEND S_KOST31.
      WHEN '32'.  APPEND S_KOST32.
      WHEN '33'.  APPEND S_KOST33.
      WHEN '34'.  APPEND S_KOST34.
      WHEN '35'.  APPEND S_KOST35.
    ENDCASE.
  ENDLOOP.
ENDDO.

CONCATENATE 'KS' P_KOKRS '%' INTO FIELD.
ASSIGN FIELD TO <FS>.

SELECT * FROM COSP                               "Get external postings
    WHERE OBJNR LIKE <FS>
      AND GJAHR = P_GJAHR
      AND WRTTP IN ('01', '04')
      AND VERSN IN ('000', P_VERS)
      AND KSTAR IN S_KSTAR.
  ADD COSP-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING VALUE.
  PERFORM CHECK_CENTRES_COSP.
ENDSELECT.

SELECT * FROM COSS                               "Get internal postings
    WHERE OBJNR LIKE <FS>
      AND GJAHR = P_GJAHR
      AND WRTTP IN ('01', '04')
      AND VERSN IN ('000', P_VERS)
      AND KSTAR IN S_KSTAR.
  ADD COSS-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING VALUE.
  PERFORM CHECK_CENTRES_COSS.
ENDSELECT.

PERFORM DISPLAY_REPORT.

AT PF8.
 PERFORM CREATE_EXCEL_SPREADSHEET.
*-----------------------------------------------------------------------

************************************************************************
*                Subroutines used by program
************************************************************************
*----------------------- DISPLAY_REPORT --------------------------------
* Displays report with subtotals & grand total for report
*-----------------------------------------------------------------------
FORM DISPLAY_REPORT.
 LOOP AT BIG_TABLE.
*  assign relative position for sorting purposes
   LOOP AT NAME_TAB.
     IF BIG_TABLE-KSTAR BETWEEN NAME_TAB-FROM AND NAME_TAB-TO.
        BIG_TABLE-SETNR = NAME_TAB-SETNR.
        BIG_TABLE-POSITION = SY-TABIX.
     ENDIF.
   ENDLOOP.

*  round amounts in table.
   COMPUTE BIG_TABLE-AMTA1  = TRUNC( BIG_TABLE-AMTA1 ).
   COMPUTE BIG_TABLE-AMTA2  = TRUNC( BIG_TABLE-AMTA2 ).
   COMPUTE BIG_TABLE-AMTA3  = TRUNC( BIG_TABLE-AMTA3  ).
   COMPUTE BIG_TABLE-AMTA4  = TRUNC( BIG_TABLE-AMTA4  ).
   COMPUTE BIG_TABLE-AMTA5  = TRUNC( BIG_TABLE-AMTA5  ).
   COMPUTE BIG_TABLE-AMTA6  = TRUNC( BIG_TABLE-AMTA6  ).
   COMPUTE BIG_TABLE-AMTA7  = TRUNC( BIG_TABLE-AMTA7  ).
   COMPUTE BIG_TABLE-AMTA8  = TRUNC( BIG_TABLE-AMTA8  ).
   COMPUTE BIG_TABLE-AMTA9  = TRUNC( BIG_TABLE-AMTA9  ).
   COMPUTE BIG_TABLE-AMTA10 = TRUNC( BIG_TABLE-AMTA10 ).
   COMPUTE BIG_TABLE-AMTA11 = TRUNC( BIG_TABLE-AMTA11 ).
   COMPUTE BIG_TABLE-AMTA12 = TRUNC( BIG_TABLE-AMTA12 ).
   COMPUTE BIG_TABLE-AMTA13 = TRUNC( BIG_TABLE-AMTA13 ).
   COMPUTE BIG_TABLE-AMTA14 = TRUNC( BIG_TABLE-AMTA14 ).
   COMPUTE BIG_TABLE-AMTA15 = TRUNC( BIG_TABLE-AMTA15 ).
   COMPUTE BIG_TABLE-AMTA16 = TRUNC( BIG_TABLE-AMTA16 ).
   COMPUTE BIG_TABLE-AMTA17 = TRUNC( BIG_TABLE-AMTA17 ).
   COMPUTE BIG_TABLE-AMTA18 = TRUNC( BIG_TABLE-AMTA18 ).
   COMPUTE BIG_TABLE-AMTA19 = TRUNC( BIG_TABLE-AMTA19 ).
   COMPUTE BIG_TABLE-AMTA20 = TRUNC( BIG_TABLE-AMTA20 ).
   COMPUTE BIG_TABLE-AMTA21 = TRUNC( BIG_TABLE-AMTA21 ).
   COMPUTE BIG_TABLE-AMTA22 = TRUNC( BIG_TABLE-AMTA22 ).
   COMPUTE BIG_TABLE-AMTA23 = TRUNC( BIG_TABLE-AMTA23 ).
   COMPUTE BIG_TABLE-AMTA24 = TRUNC( BIG_TABLE-AMTA24 ).
   COMPUTE BIG_TABLE-AMTA25 = TRUNC( BIG_TABLE-AMTA25 ).
   COMPUTE BIG_TABLE-AMTA26 = TRUNC( BIG_TABLE-AMTA26 ).
   COMPUTE BIG_TABLE-AMTA27 = TRUNC( BIG_TABLE-AMTA27 ).
   COMPUTE BIG_TABLE-AMTA28 = TRUNC( BIG_TABLE-AMTA28 ).
   COMPUTE BIG_TABLE-AMTA29 = TRUNC( BIG_TABLE-AMTA29 ).
   COMPUTE BIG_TABLE-AMTA30 = TRUNC( BIG_TABLE-AMTA30 ).
   COMPUTE BIG_TABLE-AMTA31 = TRUNC( BIG_TABLE-AMTA31 ).
   COMPUTE BIG_TABLE-AMTA32 = TRUNC( BIG_TABLE-AMTA32 ).
   COMPUTE BIG_TABLE-AMTA33 = TRUNC( BIG_TABLE-AMTA33 ).
   COMPUTE BIG_TABLE-AMTA34 = TRUNC( BIG_TABLE-AMTA34 ).
   COMPUTE BIG_TABLE-AMTA35 = TRUNC( BIG_TABLE-AMTA35 ).

   COMPUTE BIG_TABLE-AMTP1  = TRUNC( BIG_TABLE-AMTP1 ).
   COMPUTE BIG_TABLE-AMTP2  = TRUNC( BIG_TABLE-AMTP2 ).
   COMPUTE BIG_TABLE-AMTP3  = TRUNC( BIG_TABLE-AMTP3  ).
   COMPUTE BIG_TABLE-AMTP4  = TRUNC( BIG_TABLE-AMTP4  ).
   COMPUTE BIG_TABLE-AMTP5  = TRUNC( BIG_TABLE-AMTP5  ).
   COMPUTE BIG_TABLE-AMTP6  = TRUNC( BIG_TABLE-AMTP6  ).
   COMPUTE BIG_TABLE-AMTP7  = TRUNC( BIG_TABLE-AMTP7  ).
   COMPUTE BIG_TABLE-AMTP8  = TRUNC( BIG_TABLE-AMTP8  ).
   COMPUTE BIG_TABLE-AMTP9  = TRUNC( BIG_TABLE-AMTP9  ).
   COMPUTE BIG_TABLE-AMTP10 = TRUNC( BIG_TABLE-AMTP10 ).
   COMPUTE BIG_TABLE-AMTP11 = TRUNC( BIG_TABLE-AMTP11 ).
   COMPUTE BIG_TABLE-AMTP12 = TRUNC( BIG_TABLE-AMTP12 ).
   COMPUTE BIG_TABLE-AMTP13 = TRUNC( BIG_TABLE-AMTP13 ).
   COMPUTE BIG_TABLE-AMTP14 = TRUNC( BIG_TABLE-AMTP14 ).
   COMPUTE BIG_TABLE-AMTP15 = TRUNC( BIG_TABLE-AMTP15 ).
   COMPUTE BIG_TABLE-AMTP16 = TRUNC( BIG_TABLE-AMTP16 ).
   COMPUTE BIG_TABLE-AMTP17 = TRUNC( BIG_TABLE-AMTP17 ).
   COMPUTE BIG_TABLE-AMTP18 = TRUNC( BIG_TABLE-AMTP18 ).
   COMPUTE BIG_TABLE-AMTP19 = TRUNC( BIG_TABLE-AMTP19 ).
   COMPUTE BIG_TABLE-AMTP20 = TRUNC( BIG_TABLE-AMTP20 ).
   COMPUTE BIG_TABLE-AMTP21 = TRUNC( BIG_TABLE-AMTP21 ).
   COMPUTE BIG_TABLE-AMTP22 = TRUNC( BIG_TABLE-AMTP22 ).
   COMPUTE BIG_TABLE-AMTP23 = TRUNC( BIG_TABLE-AMTP23 ).
   COMPUTE BIG_TABLE-AMTP24 = TRUNC( BIG_TABLE-AMTP24 ).
   COMPUTE BIG_TABLE-AMTP25 = TRUNC( BIG_TABLE-AMTP25 ).
   COMPUTE BIG_TABLE-AMTP26 = TRUNC( BIG_TABLE-AMTP26 ).
   COMPUTE BIG_TABLE-AMTP27 = TRUNC( BIG_TABLE-AMTP27 ).
   COMPUTE BIG_TABLE-AMTP28 = TRUNC( BIG_TABLE-AMTP28 ).
   COMPUTE BIG_TABLE-AMTP29 = TRUNC( BIG_TABLE-AMTP29 ).
   COMPUTE BIG_TABLE-AMTP30 = TRUNC( BIG_TABLE-AMTP30 ).
   COMPUTE BIG_TABLE-AMTP31 = TRUNC( BIG_TABLE-AMTP31 ).
   COMPUTE BIG_TABLE-AMTP32 = TRUNC( BIG_TABLE-AMTP32 ).
   COMPUTE BIG_TABLE-AMTP33 = TRUNC( BIG_TABLE-AMTP33 ).
   COMPUTE BIG_TABLE-AMTP34 = TRUNC( BIG_TABLE-AMTP34 ).
   COMPUTE BIG_TABLE-AMTP35 = TRUNC( BIG_TABLE-AMTP35 ).

*  calculate totals for actuals, plans, variance  (col 36)
   ADD BIG_TABLE-AMTA1 FROM 1 TO 35 GIVING BIG_TABLE-AMTA36.
   ADD BIG_TABLE-AMTP1 FROM 1 TO 35 GIVING BIG_TABLE-AMTP36.

   BIG_TABLE-AMTV36 = BIG_TABLE-AMTA36 - BIG_TABLE-AMTP36.

*  pick up cost element description
   SELECT SINGLE * FROM CSKU
     WHERE SPRAS = SY-LANGU
       AND KTOPL = P_SEARCH
       AND KSTAR = BIG_TABLE-KSTAR.
   IF SY-SUBRC = '0'.
      MOVE CSKU-LTEXT TO BIG_TABLE-LTEXT.
   ENDIF.

   MODIFY BIG_TABLE.
 ENDLOOP.

 SORT BIG_TABLE BY POSITION SETNR KSTAR.
* adjust position so that 1 group summarizes together.
 PREV_SETNR = SPACE.
 PREV_POSITION = 0.
 LOOP AT BIG_TABLE.
   IF BIG_TABLE-SETNR = PREV_SETNR.
      BIG_TABLE-POSITION = PREV_POSITION.
      MODIFY BIG_TABLE.
   ENDIF.
   MOVE BIG_TABLE-POSITION TO PREV_POSITION.
   MOVE BIG_TABLE-SETNR    TO PREV_SETNR.
 ENDLOOP.

* display columns 1 - 5
 FORMAT RESET.
 PERFORM PRINT_BIG_TABLE USING GROUP.

* display columns 6 to 10
 IF HEAD6 <> SPACE OR HEAD7 <> SPACE OR HEAD8 <> SPACE OR
    HEAD9 <> SPACE OR HEAD10 <> SPACE.
    GROUP = '2'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.

* display columns 11 to 15
 IF HEAD11 <> SPACE OR HEAD12 <> SPACE OR HEAD13 <> SPACE OR
    HEAD14 <> SPACE OR HEAD15 <> SPACE.
    GROUP = '3'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.

* display columns 16 to 20
 IF HEAD16 <> SPACE OR HEAD17 <> SPACE OR HEAD18 <> SPACE OR
    HEAD19 <> SPACE OR HEAD20 <> SPACE.
    GROUP = '4'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.

* display columns 21 to 25
 IF HEAD21 <> SPACE OR HEAD22 <> SPACE OR HEAD23 <> SPACE OR
    HEAD24 <> SPACE OR HEAD25 <> SPACE.
    GROUP = '5'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.

* display columns 26 to 30
 IF HEAD26 <> SPACE OR HEAD27 <> SPACE OR HEAD28 <> SPACE OR
    HEAD29 <> SPACE OR HEAD30 <> SPACE.
    GROUP = '6'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.

* display columns 31 to 35
 IF HEAD31 <> SPACE OR HEAD32 <> SPACE OR HEAD33 <> SPACE OR
    HEAD34 <> SPACE OR HEAD35 <> SPACE.
    GROUP = '7'.
    NEW-PAGE.
    FORMAT RESET.
    PERFORM PRINT_BIG_TABLE USING GROUP.
 ENDIF.
ENDFORM.

FORM PRINT_BIG_TABLE USING GROUP.
 LOOP AT BIG_TABLE.
    CASE GROUP.                                       "Print Detail Info
       WHEN ' '. PERFORM PRINT_INFO.
       WHEN '2'. PERFORM PRINT_INFO_6_10.
       WHEN '3'. PERFORM PRINT_INFO_11_15.
       WHEN '4'. PERFORM PRINT_INFO_16_20.
       WHEN '5'. PERFORM PRINT_INFO_21_25.
       WHEN '6'. PERFORM PRINT_INFO_26_30.
       WHEN '7'. PERFORM PRINT_INFO_31_35.
    ENDCASE.

  WRITE:   20  BIG_TABLE-LTEXT.

  AT END OF SETNR.
    ULINE.
    SUM.
    FORMAT COLOR COL_TOTAL.
    CASE GROUP.                           "Print Subtotals by CE Group
       WHEN ' '. PERFORM PRINT_INFO.
       WHEN '2'. PERFORM PRINT_INFO_6_10.
       WHEN '3'. PERFORM PRINT_INFO_11_15.
       WHEN '4'. PERFORM PRINT_INFO_16_20.
       WHEN '5'. PERFORM PRINT_INFO_21_25.
       WHEN '6'. PERFORM PRINT_INFO_26_30.
       WHEN '7'. PERFORM PRINT_INFO_31_35.
    ENDCASE.
    WRITE: 2 BIG_TABLE-SETNR+8(10).

    LOOP AT NAME_TAB.                          "Description for CE Group
       IF  BIG_TABLE-SETNR = NAME_TAB-SETNR.
           WRITE: 20 NAME_TAB-TITLE.
       ENDIF.
    ENDLOOP.

    FORMAT RESET.
    ULINE.
    PERFORM WRITE_VERT.
  ENDAT.

  AT LAST.
    ULINE.
    SUM.
    FORMAT COLOR COL_GROUP.
    CASE GROUP.                                     "Print Report Totals
       WHEN ' '. PERFORM PRINT_INFO.
       WHEN '2'. PERFORM PRINT_INFO_6_10.
       WHEN '3'. PERFORM PRINT_INFO_11_15.
       WHEN '4'. PERFORM PRINT_INFO_16_20.
       WHEN '5'. PERFORM PRINT_INFO_21_25.
       WHEN '6'. PERFORM PRINT_INFO_26_30.
       WHEN '7'. PERFORM PRINT_INFO_31_35.
    ENDCASE.
     WRITE: 2 'TOTAL REPORT'.
     FORMAT RESET.
  ENDAT.
 ENDLOOP.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA1 DECIMALS 0 UNDER TEXT-005,      "Column 1
          (12) BIG_TABLE-AMTP1 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA2 DECIMALS 0 UNDER TEXT-010,      "Column 2
          (12) BIG_TABLE-AMTP2 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA3 DECIMALS 0 UNDER TEXT-015,      "Column 3
          (12) BIG_TABLE-AMTP3 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA4 DECIMALS 0 UNDER TEXT-020,      "Column 4
          (12) BIG_TABLE-AMTP4 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA5 DECIMALS 0 UNDER TEXT-025,      "Column 5
          (12) BIG_TABLE-AMTP5 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = ' '.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_6_10.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA6  DECIMALS 0 UNDER TEXT-005,    "Column 6
          (12) BIG_TABLE-AMTP6  DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA7  DECIMALS 0 UNDER TEXT-010,    "Column 7
          (12) BIG_TABLE-AMTP7  DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA8  DECIMALS 0 UNDER TEXT-015,    "Column 8
          (12) BIG_TABLE-AMTP8  DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA9  DECIMALS 0 UNDER TEXT-020,    "Column 9
          (12) BIG_TABLE-AMTP9  DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA10 DECIMALS 0 UNDER TEXT-025,    "Column 10
          (12) BIG_TABLE-AMTP10 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = '2'.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_11_15.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA11 DECIMALS 0 UNDER TEXT-005,    "Column 6
          (12) BIG_TABLE-AMTP11 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA12 DECIMALS 0 UNDER TEXT-010,    "Column 7
          (12) BIG_TABLE-AMTP12 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA13 DECIMALS 0 UNDER TEXT-015,    "Column 8
          (12) BIG_TABLE-AMTP13 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA14 DECIMALS 0 UNDER TEXT-020,    "Column 9
          (12) BIG_TABLE-AMTP14 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA15 DECIMALS 0 UNDER TEXT-025,    "Column 10
          (12) BIG_TABLE-AMTP15 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = '3'.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_16_20.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA16 DECIMALS 0 UNDER TEXT-005,    "Column 6
          (12) BIG_TABLE-AMTP16 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA17 DECIMALS 0 UNDER TEXT-010,    "Column 7
          (12) BIG_TABLE-AMTP17 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA18 DECIMALS 0 UNDER TEXT-015,    "Column 8
          (12) BIG_TABLE-AMTP18 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA19 DECIMALS 0 UNDER TEXT-020,    "Column 9
          (12) BIG_TABLE-AMTP19 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA20 DECIMALS 0 UNDER TEXT-025,    "Column 10
          (12) BIG_TABLE-AMTP20 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = '4'.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_21_25.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA21 DECIMALS 0 UNDER TEXT-005,    "Column 6
          (12) BIG_TABLE-AMTP21 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA22 DECIMALS 0 UNDER TEXT-010,    "Column 7
          (12) BIG_TABLE-AMTP22 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA23 DECIMALS 0 UNDER TEXT-015,    "Column 8
          (12) BIG_TABLE-AMTP23 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA24 DECIMALS 0 UNDER TEXT-020,    "Column 9
          (12) BIG_TABLE-AMTP24 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA25 DECIMALS 0 UNDER TEXT-025,    "Column 10
          (12) BIG_TABLE-AMTP25 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = '5'.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_26_30.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA26 DECIMALS 0 UNDER TEXT-005,    "Column 6
          (12) BIG_TABLE-AMTP26 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA27 DECIMALS 0 UNDER TEXT-010,    "Column 7
          (12) BIG_TABLE-AMTP27 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA28 DECIMALS 0 UNDER TEXT-015,    "Column 8
          (12) BIG_TABLE-AMTP28 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA29 DECIMALS 0 UNDER TEXT-020,    "Column 9
          (12) BIG_TABLE-AMTP29 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA30 DECIMALS 0 UNDER TEXT-025,    "Column 10
          (12) BIG_TABLE-AMTP30 DECIMALS 0 UNDER TEXT-026.
  IF TTLWRITE = '6'.
   WRITE: (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,     "Column 6
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
  ENDIF.
ENDFORM.

*---------------------------  PRINT_INFO -------------------------------
* Displays columnar information - cost element, actuals, plans, variance
*-----------------------------------------------------------------------
FORM PRINT_INFO_31_35.

   PERFORM WRITE_VERT.
   WRITE:      BIG_TABLE-KSTAR UNDER TEXT-001,
          (12) BIG_TABLE-AMTA31 DECIMALS 0 UNDER TEXT-005,    "Column 31
          (12) BIG_TABLE-AMTP31 DECIMALS 0 UNDER TEXT-006,
          (12) BIG_TABLE-AMTA32 DECIMALS 0 UNDER TEXT-010,    "Column 32
          (12) BIG_TABLE-AMTP32 DECIMALS 0 UNDER TEXT-011,
          (12) BIG_TABLE-AMTA33 DECIMALS 0 UNDER TEXT-015,    "Column 33
          (12) BIG_TABLE-AMTP33 DECIMALS 0 UNDER TEXT-016,
          (12) BIG_TABLE-AMTA34 DECIMALS 0 UNDER TEXT-020,    "Column 34
          (12) BIG_TABLE-AMTP34 DECIMALS 0 UNDER TEXT-021,
          (12) BIG_TABLE-AMTA35 DECIMALS 0 UNDER TEXT-025,    "Column 35
          (12) BIG_TABLE-AMTP35 DECIMALS 0 UNDER TEXT-026,
          (12) BIG_TABLE-AMTA36 DECIMALS 0 UNDER TEXT-031,    "Column 36
          (12) BIG_TABLE-AMTP36 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV36 DECIMALS 0 UNDER TEXT-033.
ENDFORM.

*--------------------  PRINT_VERT  -------------------------------------
*  Vertical Lines
*-----------------------------------------------------------------------
FORM WRITE_VERT.
 WRITE: /1 SY-VLINE,
        15 SY-VLINE,
        79 SY-VLINE,  80 SY-VLINE,  93 SY-VLINE,
       106 SY-VLINE, 107 SY-VLINE, 120 SY-VLINE,
       133 SY-VLINE, 134 SY-VLINE, 147 SY-VLINE,

       160 SY-VLINE, 161 SY-VLINE, 174 SY-VLINE,
       187 SY-VLINE, 188 SY-VLINE, 201 SY-VLINE,
       214 SY-VLINE, 215 SY-VLINE, 228 SY-VLINE, 241 SY-VLINE,
       255 SY-VLINE.
ENDFORM.

*-----------------------  GET_COST_ELEMENTS ----------------------------
* Using function module, get all cost centre ranges for the cost
* centre group   --> SETNAME = name of cost element group
*---------------------------------------------------------------------*

FORM GET_COST_ELEMENTS USING SETNAME.
* 2001/02/14 mdemeest - change for 4.6b
data:  setname_pass(30)   type c.
  move setname to setname_pass+4(12).
  move p_search to setname_pass(4).
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0102'
            SETNR         = SETNAME_PASS
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = NAME_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

*-------------------- GET_COST_CENTRES ---------------------------------
FORM GET_COST_CENTRES USING SETNAME.
* 2001/02/14 mdemeest - change for 4.6b
data: setname_pass(30) type c.
  move setname to setname_pass+4(12).
  move p_kokrs  to setname_pass(4).
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0101'
            SETNR         = SETNAME_PASS
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = CC_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

*----------------------- GET_PARAMETERS_DEFAULT ------------------------
* Get user default value (These are values the user has set to default
*-----------------------------------------------------------------------
FORM GET_PARAMETERS_DEFAULT USING PARID USERPAR.


  CALL FUNCTION 'G_GET_USER_PARAMETER'
       EXPORTING
             PARAMETER_ID    = PARID
       IMPORTING
             PARAMETER_VALUE = USERPAR.

ENDFORM.

*------------------------ CHECK_CENTRES_COSP  --------------------------
* This routine releases the dollar value only if the entry from the
* table satisfies both the cost element group and cost centre group
* criteria.
*-----------------------------------------------------------------------
FORM  CHECK_CENTRES_COSP.
 CLEAR BIG_TABLE.

 MOVE COSP-KSTAR TO BIG_TABLE-KSTAR.
 IF COSP-WRTTP = '01'.
  IF COSP-VERSN = P_VERS.
    IF COSP-OBJNR+6(10) IN S_KOST1 AND S_KOST1 <> SPACE.
       MOVE VALUE TO BIG_TABLE-AMTP1.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST2 AND S_KOST2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP2.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST3 AND S_KOST3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP3.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST4 AND S_KOST4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP4.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST5 AND S_KOST5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP5.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST6 AND S_KOST6 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP6.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST7 AND S_KOST7 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP7.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST8 AND S_KOST8 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP8.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST9 AND S_KOST9 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP9.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST10 AND S_KOST10 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP10.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST11 AND S_KOST11 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP11.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST12 AND S_KOST12 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP12.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST13 AND S_KOST13 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP13.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST14 AND S_KOST14 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP14.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST15 AND S_KOST15 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP15.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST16 AND S_KOST16 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP16.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST17 AND S_KOST17 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP17.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST18 AND S_KOST18 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP18.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST19 AND S_KOST19 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP19.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST20 AND S_KOST20 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP20.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST21 AND S_KOST21 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP21.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST22 AND S_KOST22 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP22.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST23 AND S_KOST23 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP23.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST24 AND S_KOST24 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP24.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST25 AND S_KOST25 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP25.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST26 AND S_KOST26 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP26.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST27 AND S_KOST27 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP27.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST28 AND S_KOST28 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP28.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST29 AND S_KOST29 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP29.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST30 AND S_KOST30 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP30.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST31 AND S_KOST31 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP31.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST32 AND S_KOST32 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP32.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST33 AND S_KOST33 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP33.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST34 AND S_KOST34 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP34.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOST35 AND S_KOST35 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP35.
    ENDIF.
 ENDIF.
 ELSEIF COSP-WRTTP = '04'.                                 "ACTUALS
     IF COSP-OBJNR+6(10) IN S_KOST1 AND S_KOST1(1) <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA1.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST2 AND S_KOST2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA2.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST3 AND S_KOST3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA3.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST4 AND S_KOST4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA4.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST5 AND S_KOST5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA5.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST6 AND S_KOST6 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA6.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST7 AND S_KOST7 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA7.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST8 AND S_KOST8 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA8.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST9 AND S_KOST9 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA9.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST10 AND S_KOST10 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA10.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST11 AND S_KOST11 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA11.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST12 AND S_KOST12 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA12.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST13 AND S_KOST13 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA13.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST14 AND S_KOST14 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA14.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST15 AND S_KOST15 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA15.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST16 AND S_KOST16 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA16.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST17 AND S_KOST17 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA17.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST18 AND S_KOST18 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA18.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST19 AND S_KOST19 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA19.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST20 AND S_KOST20 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA20.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST21 AND S_KOST21 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA21.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST22 AND S_KOST22 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA22.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST23 AND S_KOST23 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA23.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST24 AND S_KOST24 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA24.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST25 AND S_KOST25 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA25.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST26 AND S_KOST26 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA26.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST27 AND S_KOST27 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA27.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST28 AND S_KOST28 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA28.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST29 AND S_KOST29 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA29.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST30 AND S_KOST30 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA30.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST31 AND S_KOST31 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA31.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST32 AND S_KOST32 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA32.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST33 AND S_KOST33 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA33.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST34 AND S_KOST34 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA34.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOST35 AND S_KOST35 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA35.
     ENDIF.
  ENDIF.
  COLLECT BIG_TABLE.
ENDFORM.

*------------------------ CHECK_CENTRES_COSS ---------------------------
* This routine releases the dollar value only if the entry from the
* table satisfies both the cost element group and cost centre group
* criteria.
*-----------------------------------------------------------------------
FORM  CHECK_CENTRES_COSS.
 CLEAR BIG_TABLE.
 MOVE COSS-KSTAR TO BIG_TABLE-KSTAR.
 IF COSS-VERSN = P_VERS.
   IF COSS-WRTTP = '01'.                                   "PLANS
     IF COSS-OBJNR+6(10) IN S_KOST1 AND S_KOST1 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP1.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST2 AND S_KOST2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP2.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST3 AND S_KOST3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP3.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST4 AND S_KOST4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP4.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST5 AND S_KOST5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP5.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST6 AND S_KOST6 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP6.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST7 AND S_KOST7 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP7.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST8 AND S_KOST8 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP8.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST9 AND S_KOST9 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP9.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST10 AND S_KOST10 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP10.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST11 AND S_KOST11 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP11.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST12 AND S_KOST12 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP12.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST13 AND S_KOST13 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP13.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST14 AND S_KOST14 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP14.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST15 AND S_KOST15 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP15.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST16 AND S_KOST16 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP16.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST17 AND S_KOST17 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP17.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST18 AND S_KOST18 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP18.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST19 AND S_KOST19 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP19.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST20 AND S_KOST20 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP20.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST21 AND S_KOST21 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP21.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST22 AND S_KOST22 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP22.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST23 AND S_KOST23 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP23.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST24 AND S_KOST24 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP24.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST25 AND S_KOST25 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP25.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST26 AND S_KOST26 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP26.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST27 AND S_KOST27 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP27.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST28 AND S_KOST28 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP28.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST29 AND S_KOST29 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP29.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST30 AND S_KOST30 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP30.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST31 AND S_KOST31 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP31.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST32 AND S_KOST32 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP32.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST33 AND S_KOST33 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP33.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST34 AND S_KOST34 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP34.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST35 AND S_KOST35 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP35.
     ENDIF.
  ENDIF.
  ELSEIF COSS-WRTTP = '04'.                               "ACTUALS
     IF COSS-OBJNR+6(10) IN S_KOST1 AND S_KOST1 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA1.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST2 AND S_KOST2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA2.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST3 AND S_KOST3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA3.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST4 AND S_KOST4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA4.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST5 AND S_KOST5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA5.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST6 AND S_KOST6 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA6.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST7 AND S_KOST7 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA7.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST8 AND S_KOST8 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA8.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST9 AND S_KOST9 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA9.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST10 AND S_KOST10 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA10.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST11 AND S_KOST11 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA11.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST12 AND S_KOST12 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA12.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST13 AND S_KOST13 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA13.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST14 AND S_KOST14 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA14.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST15 AND S_KOST15 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA15.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST16 AND S_KOST16 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA16.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST17 AND S_KOST17 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA17.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST18 AND S_KOST18 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA18.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST19 AND S_KOST19 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA19.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST20 AND S_KOST20 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA20.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST21 AND S_KOST21 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA21.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST22 AND S_KOST22 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA22.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST23 AND S_KOST23 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA23.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST24 AND S_KOST24 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA24.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST25 AND S_KOST25 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA25.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST26 AND S_KOST26 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA26.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST27 AND S_KOST27 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA27.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST28 AND S_KOST28 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA28.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST29 AND S_KOST29 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA29.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST30 AND S_KOST30 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA30.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST31 AND S_KOST31 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA31.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST32 AND S_KOST32 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA32.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST33 AND S_KOST33 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA33.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST34 AND S_KOST34 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA34.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOST35 AND S_KOST35 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA35.
     ENDIF.
  ENDIF.
   COLLECT BIG_TABLE.
ENDFORM.


*-------------------- COST_CENTRE_GROUP_PULLDOWN -----------------------
FORM COST_CENTRE_GROUP_PULLDOWN USING P_KOST.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  O_SEARCHFLD = P_KOKRS.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0101'  "2001/02/14
            FIELD_NAME         = 'KOSTL'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = P_KOST
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    P_KOST1 = O_TMPSET.
  ENDIF.
ENDFORM.

************************************************************************
* Excel Spreadsheet Routines
************************************************************************
*------------------------ CREATE_ESCEL_SPREADSHEET ---------------------
* This routine copied from "Developing SAP's R/3 Applications pg. 566
*-----------------------------------------------------------------------
FORM CREATE_EXCEL_SPREADSHEET.
 INCLUDE OLE2INCL.
 DATA: ZKORR006 TYPE OLE2_OBJECT,
       WORKBOOK TYPE OLE2_OBJECT,
       SHEET    TYPE OLE2_OBJECT,
       CELLS    TYPE OLE2_OBJECT,
       INDEX    TYPE I,
       ROW_MAX  TYPE I VALUE 256,
       F        TYPE I.
 FIELD-SYMBOLS: <NAME>.

* This routine moves info to internal table for download to Excel
  PERFORM EXCEL_TITLES.
  LOOP AT BIG_TABLE.
    MOVE BIG_TABLE-KSTAR       TO XLS_NAME-KSTAR.
    MOVE BIG_TABLE-LTEXT       TO XLS_NAME-LTEXT.
    PERFORM MOVE_AMTS_TO_EXCEL.
    APPEND XLS_NAME.

    AT END OF SETNR.                                "Totals for CE Group
       SUM.
       MOVE BIG_TABLE-SETNR+2(10) TO XLS_NAME-KSTAR.
       LOOP AT NAME_TAB.
         IF BIG_TABLE-SETNR = NAME_TAB-SETNR.
            MOVE NAME_TAB-TITLE TO XLS_NAME-LTEXT.
         ENDIF.
       ENDLOOP.
       PERFORM MOVE_AMTS_TO_EXCEL.
       APPEND XLS_NAME.
       CLEAR XLS_NAME.            "Blank line on spreadsheet after total
       APPEND XLS_NAME.
    ENDAT.

    AT LAST.                                       "Report Total
       SUM.
       CLEAR  XLS_NAME-KSTAR.
       MOVE 'TOTAL REPORT'        TO XLS_NAME-LTEXT.
       PERFORM MOVE_AMTS_TO_EXCEL.
       APPEND XLS_NAME.
    ENDAT.
ENDLOOP.

* This section creates the spreadsheet, workbooks, worksheets & cells
* and moves the entries from the internal table to the CELLS

 CREATE OBJECT ZKORR006 'excel.application'.
 SET PROPERTY OF ZKORR006 'visible' = 1.
 CALL METHOD OF ZKORR006 'Workbooks' = WORKBOOK.
 CALL METHOD OF WORKBOOK 'Add'.
 CALL METHOD OF ZKORR006 'Worksheets' = SHEET
                  EXPORTING #1 = 1.
 CALL METHOD OF SHEET 'Activate'.

 LOOP AT XLS_NAME.
   INDEX = ROW_MAX * ( SY-TABIX - 1 ) + 1.
   DO 75 TIMES.
     ASSIGN COMPONENT SY-INDEX OF STRUCTURE XLS_NAME TO <NAME>.
     CALL METHOD OF SHEET 'Cells' = CELLS
                 EXPORTING #1 = INDEX.
     SET PROPERTY OF CELLS 'Value' = <NAME>.
     ADD 1 TO INDEX.
   ENDDO.
 ENDLOOP.

ENDFORM.

*--------------------------  EXCEL_TITLES ------------------------------
* This routines moves the titles to the internal table for download
* into EXCEL
*-----------------------------------------------------------------------
FORM EXCEL_TITLES.
 XLS_NAME-KSTAR = TEXT-RPT.
 XLS_NAME-LTEXT = SY-REPID.
 HD_TITLE = TEXT-HD1.
 XLS_NAME-AMTA3 = HD_TITLE+0(12).
 XLS_NAME-AMTP3 = HD_TITLE+12(12).
 XLS_NAME-AMTA4 = HD_TITLE+24(12).
 XLS_NAME-AMTA6 = TEXT-DTE.
 XLS_NAME-AMTP6 = SY-DATUM.
 APPEND XLS_NAME.

 CLEAR XLS_NAME.
 XLS_NAME-KSTAR = TEXT-CLT.
 CONCATENATE SY-MANDT '  -  ' SY-SYSID INTO XLS_NAME-LTEXT.
 XLS_NAME-AMTA3 = TEXT-133.
 XLS_NAME-AMTP3 = S_MTH+3(2).
 XLS_NAME-AMTA4 = S_MTH+5(2).
 XLS_NAME-AMTP4 = P_GJAHR.
 APPEND XLS_NAME.

 CLEAR XLS_NAME.
 XLS_NAME-KSTAR = TEXT-CMP.
 XLS_NAME-LTEXT = P_BUKRS.
 APPEND XLS_NAME.

 XLS_NAME-KSTAR = TEXT-VER.
 XLS_NAME-LTEXT = P_VERS.
 APPEND XLS_NAME.

 CLEAR XLS_NAME.                        "Blank line
 APPEND XLS_NAME.

 XLS_NAME-KSTAR = '  Cost'.
 MOVE HEAD1+9(12) TO:  XLS_NAME-AMTA1,  XLS_NAME-AMTP1.
 MOVE HEAD2+9(12) TO:  XLS_NAME-AMTA2,  XLS_NAME-AMTP2.
 MOVE HEAD3+9(12) TO:  XLS_NAME-AMTA3,  XLS_NAME-AMTP3.
 MOVE HEAD4+9(12) TO:  XLS_NAME-AMTA4,  XLS_NAME-AMTP4.
 MOVE HEAD5+9(12) TO:  XLS_NAME-AMTA5,  XLS_NAME-AMTP5.
 MOVE HEAD6+9(12) TO:  XLS_NAME-AMTA6,  XLS_NAME-AMTP6.
 MOVE HEAD7+9(12) TO:  XLS_NAME-AMTA7,  XLS_NAME-AMTP7.
 MOVE HEAD8+9(12) TO:  XLS_NAME-AMTA8,  XLS_NAME-AMTP8.
 MOVE HEAD9+9(12) TO:  XLS_NAME-AMTA9,  XLS_NAME-AMTP9.
 MOVE HEAD10+9(12) TO: XLS_NAME-AMTA10, XLS_NAME-AMTP10.
 MOVE HEAD11+9(12) TO: XLS_NAME-AMTA11, XLS_NAME-AMTP11.
 MOVE HEAD12+9(12) TO: XLS_NAME-AMTA12, XLS_NAME-AMTP12.
 MOVE HEAD13+9(12) TO: XLS_NAME-AMTA13, XLS_NAME-AMTP13.
 MOVE HEAD14+9(12) TO: XLS_NAME-AMTA14, XLS_NAME-AMTP14.
 MOVE HEAD15+9(12) TO: XLS_NAME-AMTA15, XLS_NAME-AMTP15.
 MOVE HEAD16+9(12) TO: XLS_NAME-AMTA16, XLS_NAME-AMTP16.
 MOVE HEAD17+9(12) TO: XLS_NAME-AMTA17, XLS_NAME-AMTP17.
 MOVE HEAD18+9(12) TO: XLS_NAME-AMTA18, XLS_NAME-AMTP18.
 MOVE HEAD19+9(12) TO: XLS_NAME-AMTA19, XLS_NAME-AMTP19.
 MOVE HEAD20+9(12) TO: XLS_NAME-AMTA20, XLS_NAME-AMTP20.
 MOVE HEAD21+9(12) TO: XLS_NAME-AMTA21, XLS_NAME-AMTP21.
 MOVE HEAD22+9(12) TO: XLS_NAME-AMTA22, XLS_NAME-AMTP22.
 MOVE HEAD23+9(12) TO: XLS_NAME-AMTA23, XLS_NAME-AMTP23.
 MOVE HEAD24+9(12) TO: XLS_NAME-AMTA24, XLS_NAME-AMTP24.
 MOVE HEAD25+9(12) TO: XLS_NAME-AMTA25, XLS_NAME-AMTP25.
 MOVE HEAD26+9(12) TO: XLS_NAME-AMTA26, XLS_NAME-AMTP26.
 MOVE HEAD27+9(12) TO: XLS_NAME-AMTA27, XLS_NAME-AMTP27.
 MOVE HEAD28+9(12) TO: XLS_NAME-AMTA28, XLS_NAME-AMTP28.
 MOVE HEAD29+9(12) TO: XLS_NAME-AMTA29, XLS_NAME-AMTP29.
 MOVE HEAD30+9(12) TO: XLS_NAME-AMTA30, XLS_NAME-AMTP30.
 MOVE HEAD31+9(12) TO: XLS_NAME-AMTA31, XLS_NAME-AMTP31.
 MOVE HEAD32+9(12) TO: XLS_NAME-AMTA32, XLS_NAME-AMTP32.
 MOVE HEAD33+9(12) TO: XLS_NAME-AMTA33, XLS_NAME-AMTP33.
 MOVE HEAD34+9(12) TO: XLS_NAME-AMTA34, XLS_NAME-AMTP34.
 MOVE HEAD35+9(12) TO: XLS_NAME-AMTA35, XLS_NAME-AMTP35.
 MOVE 'TOTAL'     TO: XLS_NAME-AMTA36, XLS_NAME-AMTP36, XLS_NAME-AMTV36.

 APPEND XLS_NAME.

 XLS_NAME-KSTAR = 'Element'.
 XLS_NAME-LTEXT = 'Description'.

 MOVE 'Actual' TO: XLS_NAME-AMTA1,  XLS_NAME-AMTA2,  XLS_NAME-AMTA3,
                   XLS_NAME-AMTA4,  XLS_NAME-AMTA5,  XLS_NAME-AMTA6,
                   XLS_NAME-AMTA7,  XLS_NAME-AMTA8,  XLS_NAME-AMTA9,
                   XLS_NAME-AMTA10, XLS_NAME-AMTA11, XLS_NAME-AMTA12,
                   XLS_NAME-AMTA13, XLS_NAME-AMTA14, XLS_NAME-AMTA15,
                   XLS_NAME-AMTA16, XLS_NAME-AMTA17, XLS_NAME-AMTA18,
                   XLS_NAME-AMTA19, XLS_NAME-AMTA20, XLS_NAME-AMTA21,
                   XLS_NAME-AMTA22, XLS_NAME-AMTA23, XLS_NAME-AMTA24,
                   XLS_NAME-AMTA25, XLS_NAME-AMTA26, XLS_NAME-AMTA27,
                   XLS_NAME-AMTA28, XLS_NAME-AMTA29, XLS_NAME-AMTA30,
                   XLS_NAME-AMTA31, XLS_NAME-AMTA32, XLS_NAME-AMTA33,
                   XLS_NAME-AMTA34, XLS_NAME-AMTA35, XLS_NAME-AMTA36.

 MOVE 'Plan  ' TO: XLS_NAME-AMTP1,  XLS_NAME-AMTP2,  XLS_NAME-AMTP3,
                   XLS_NAME-AMTP4,  XLS_NAME-AMTP5,  XLS_NAME-AMTP6,
                   XLS_NAME-AMTP7,  XLS_NAME-AMTP8,  XLS_NAME-AMTP9,
                   XLS_NAME-AMTP10, XLS_NAME-AMTP11, XLS_NAME-AMTP12,
                   XLS_NAME-AMTP13, XLS_NAME-AMTP14, XLS_NAME-AMTP15,
                   XLS_NAME-AMTP16, XLS_NAME-AMTP17, XLS_NAME-AMTP18,
                   XLS_NAME-AMTP19, XLS_NAME-AMTP20, XLS_NAME-AMTP21,
                   XLS_NAME-AMTP22, XLS_NAME-AMTP23, XLS_NAME-AMTP24,
                   XLS_NAME-AMTP25, XLS_NAME-AMTP26, XLS_NAME-AMTP27,
                   XLS_NAME-AMTP28, XLS_NAME-AMTP29, XLS_NAME-AMTP30,
                   XLS_NAME-AMTP31, XLS_NAME-AMTP32, XLS_NAME-AMTP33,
                   XLS_NAME-AMTP34, XLS_NAME-AMTP35, XLS_NAME-AMTP36.


 XLS_NAME-AMTV36 = 'Variance'.
 APPEND XLS_NAME.

 CLEAR XLS_NAME.       "Blank line on spreadsheet after column headings
 APPEND XLS_NAME.
ENDFORM.

*-------------------------- MOVE_AMTS_TO_EXCEL -------------------------
* Used a write statement rather than a move because the write
* allows the data to be rounded.  A move statement keeps the decimal
* positions.  Jan Mieras requested the amounts be rounded to whole
* numbers.
*-----------------------------------------------------------------------
FORM MOVE_AMTS_TO_EXCEL.
*  move big_table-amta1       to xls_name-amta1.      "Keeps decimals
  WRITE BIG_TABLE-AMTA1 TO XLS_NAME-AMTA1 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA1.
  WRITE BIG_TABLE-AMTP1 TO XLS_NAME-AMTP1 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP1.
  WRITE BIG_TABLE-AMTA2 TO XLS_NAME-AMTA2 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA2.
  WRITE BIG_TABLE-AMTP2 TO XLS_NAME-AMTP2 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP2.
  WRITE BIG_TABLE-AMTA3 TO XLS_NAME-AMTA3 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA3.
  WRITE BIG_TABLE-AMTP3 TO XLS_NAME-AMTP3 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP3.
  WRITE BIG_TABLE-AMTA4 TO XLS_NAME-AMTA4 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA4.
  WRITE BIG_TABLE-AMTP4 TO XLS_NAME-AMTP4 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP4.

  WRITE BIG_TABLE-AMTA5 TO XLS_NAME-AMTA5 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA5.
  WRITE BIG_TABLE-AMTP5 TO XLS_NAME-AMTP5 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP5.

  WRITE BIG_TABLE-AMTA6 TO XLS_NAME-AMTA6 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA6.
  WRITE BIG_TABLE-AMTP6 TO XLS_NAME-AMTP6 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP6.

  WRITE BIG_TABLE-AMTA7 TO XLS_NAME-AMTA7 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA7.
  WRITE BIG_TABLE-AMTP7 TO XLS_NAME-AMTP7 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP7.

  WRITE BIG_TABLE-AMTA8 TO XLS_NAME-AMTA8 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA8.
  WRITE BIG_TABLE-AMTP8 TO XLS_NAME-AMTP8 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP8.

  WRITE BIG_TABLE-AMTA9 TO XLS_NAME-AMTA9 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA9.
  WRITE BIG_TABLE-AMTP9 TO XLS_NAME-AMTP9 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP9.

  WRITE BIG_TABLE-AMTA10 TO XLS_NAME-AMTA10 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA10.
  WRITE BIG_TABLE-AMTP10 TO XLS_NAME-AMTP10 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP10.

  WRITE BIG_TABLE-AMTA11 TO XLS_NAME-AMTA11 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA11.
  WRITE BIG_TABLE-AMTP11 TO XLS_NAME-AMTP11 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP11.

  WRITE BIG_TABLE-AMTA12 TO XLS_NAME-AMTA12 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA12.
  WRITE BIG_TABLE-AMTP12 TO XLS_NAME-AMTP12 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP12.

  WRITE BIG_TABLE-AMTA13 TO XLS_NAME-AMTA13 DECIMALS 0.  "Columns 13
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA13.
  WRITE BIG_TABLE-AMTP13 TO XLS_NAME-AMTP13 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP13.

  WRITE BIG_TABLE-AMTA14 TO XLS_NAME-AMTA14 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA14.
  WRITE BIG_TABLE-AMTP14 TO XLS_NAME-AMTP14 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP14.

  WRITE BIG_TABLE-AMTA15 TO XLS_NAME-AMTA15 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA15.
  WRITE BIG_TABLE-AMTP15 TO XLS_NAME-AMTP15 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP15.

  WRITE BIG_TABLE-AMTA16 TO XLS_NAME-AMTA16 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA16.
  WRITE BIG_TABLE-AMTP16 TO XLS_NAME-AMTP16 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP16.

  WRITE BIG_TABLE-AMTA17 TO XLS_NAME-AMTA17 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA17.
  WRITE BIG_TABLE-AMTP17 TO XLS_NAME-AMTP17 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP17.

  WRITE BIG_TABLE-AMTA18 TO XLS_NAME-AMTA18 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA18.
  WRITE BIG_TABLE-AMTP18 TO XLS_NAME-AMTP18 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP18.

  WRITE BIG_TABLE-AMTA19 TO XLS_NAME-AMTA19 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA19.
  WRITE BIG_TABLE-AMTP19 TO XLS_NAME-AMTP19 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP19.

  WRITE BIG_TABLE-AMTA20 TO XLS_NAME-AMTA20 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA20.
  WRITE BIG_TABLE-AMTP20 TO XLS_NAME-AMTP20 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP20.

  WRITE BIG_TABLE-AMTA21 TO XLS_NAME-AMTA21 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA21.
  WRITE BIG_TABLE-AMTP21 TO XLS_NAME-AMTP21 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP21.

  WRITE BIG_TABLE-AMTA22 TO XLS_NAME-AMTA22 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA22.
  WRITE BIG_TABLE-AMTP22 TO XLS_NAME-AMTP22 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP22.

  WRITE BIG_TABLE-AMTA23 TO XLS_NAME-AMTA23 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA23.
  WRITE BIG_TABLE-AMTP23 TO XLS_NAME-AMTP23 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP23.

  WRITE BIG_TABLE-AMTA24 TO XLS_NAME-AMTA24 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA24.
  WRITE BIG_TABLE-AMTP24 TO XLS_NAME-AMTP24 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP24.

  WRITE BIG_TABLE-AMTA25 TO XLS_NAME-AMTA25 DECIMALS 0.   "Column 25
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA25.
  WRITE BIG_TABLE-AMTP25 TO XLS_NAME-AMTP25 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP25.

  WRITE BIG_TABLE-AMTA26 TO XLS_NAME-AMTA26 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA26.
  WRITE BIG_TABLE-AMTP26 TO XLS_NAME-AMTP26 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP26.

  WRITE BIG_TABLE-AMTA27 TO XLS_NAME-AMTA27 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA27.
  WRITE BIG_TABLE-AMTP27 TO XLS_NAME-AMTP27 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP27.

  WRITE BIG_TABLE-AMTA28 TO XLS_NAME-AMTA28 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA28.
  WRITE BIG_TABLE-AMTP28 TO XLS_NAME-AMTP28 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP28.

  WRITE BIG_TABLE-AMTA29 TO XLS_NAME-AMTA29 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA29.
  WRITE BIG_TABLE-AMTP29 TO XLS_NAME-AMTP29 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP29.

  WRITE BIG_TABLE-AMTA30 TO XLS_NAME-AMTA30 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA30.
  WRITE BIG_TABLE-AMTP30 TO XLS_NAME-AMTP30 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP30.

  WRITE BIG_TABLE-AMTA31 TO XLS_NAME-AMTA31 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA31.
  WRITE BIG_TABLE-AMTP31 TO XLS_NAME-AMTP31 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP31.

  WRITE BIG_TABLE-AMTA32 TO XLS_NAME-AMTA32 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA32.
  WRITE BIG_TABLE-AMTP32 TO XLS_NAME-AMTP32 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP32.

  WRITE BIG_TABLE-AMTA33 TO XLS_NAME-AMTA33 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA33.
  WRITE BIG_TABLE-AMTP33 TO XLS_NAME-AMTP33 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP33.

  WRITE BIG_TABLE-AMTA34 TO XLS_NAME-AMTA34 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA34.
  WRITE BIG_TABLE-AMTP34 TO XLS_NAME-AMTP34 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP34.

  WRITE BIG_TABLE-AMTA35 TO XLS_NAME-AMTA35 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA35.
  WRITE BIG_TABLE-AMTP35 TO XLS_NAME-AMTP35 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP35.

  WRITE BIG_TABLE-AMTA36 TO XLS_NAME-AMTA36 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTA36.
  WRITE BIG_TABLE-AMTP36 TO XLS_NAME-AMTP36 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTP36.
  WRITE BIG_TABLE-AMTV36 TO XLS_NAME-AMTV36 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTV36.
ENDFORM.

*---------------------- SHIFT_NEGATIVE ---------------------------------
* This routine checks for a trailing negative sign in the amount field
* and if found, moves it as a leading sign.  This is required because
* fields with trailing signs are treated as characters in Excel but
* leading signs are treated as positives.
*-----------------------------------------------------------------------
FORM SHIFT_NEGATIVE USING AMT.
  SEARCH AMT FOR '-'.
  IF SY-SUBRC = '0'.
     SHIFT AMT UP TO '-' CIRCULAR.
  ENDIF.
ENDFORM.
