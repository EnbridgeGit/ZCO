REPORT ZKORR005 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Report listing cost centres showing a cc group only
*               (no individual cc's) with related cost element group
*               totals also (for for each cc). - Union Energy Only

*  Column Headings for columns 1 - 5 (Head1, Head2, Head3, Head4, Head5)
*    If parameter entered, print parameter
*    If only 1 entry and no range, print that cost centre
*    If only 1 entry and a range, print cost centre with *
*    If more than 1 entry, print first cost centre with *
*
* memory id on parameters/select-options initializes fields with user
*           parameters
************************************************************************
* 99/08/09 mdemeest #--- Test for plan version in CHECK_CENTRES_*
* 99/06/17 mdemeest #487 Original request
*
* 2000/06/15> Correct syntax errors and added intermediate work field
* gymana    > to allow splitting of text element HD1 for excel
* 46BUpgrade> spreadsheet.
************************************************************************
TABLES: BKPF,
        T800S,
        T800Y,
        COSP,                             "External Postings
        COSS,
        RKMA1,
        RKMA4,                            "Cost Elements
        CSKU.                             "Cost Elements Description

FIELD-SYMBOLS <FS>.
DATA: FIELD(5).
DATA: NO_OF_LINES TYPE I.

DATA:  SETNAME     LIKE RGSBS-SETNR.

DATA: VALUE   LIKE COSP-WKG001.

DATA:  HEAD1(25) TYPE C.
DATA:  HEAD2(25) TYPE C.
DATA:  HEAD3(25) TYPE C.
DATA:  HEAD4(25) TYPE C.
DATA:  HEAD5(25) TYPE C.
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
           POSITION TYPE I,            "Order accordingly to structure
           SETNR   LIKE NAME_TAB-SETNR,
           KSTAR   LIKE COSP-KSTAR,                "Cost Element
           LTEXT   LIKE CSKU-LTEXT,                "Description

           AMTA1   LIKE COSP-WKG001,
           AMTP1   LIKE COSP-WKG001,

           AMTA2   LIKE COSP-WKG001,
           AMTP2   LIKE COSP-WKG001,

           AMTA3   LIKE COSP-WKG001,
           AMTP3   LIKE COSP-WKG001,

           AMTA4   LIKE COSP-WKG001,
           AMTP4   LIKE COSP-WKG001,

           AMTA5   LIKE COSP-WKG001,
           AMTP5   LIKE COSP-WKG001,

           AMTA6   LIKE COSP-WKG001,
           AMTP6   LIKE COSP-WKG001,
           AMTV6   LIKE COSP-WKG001.
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
           AMTV6(12)   TYPE C.                    "Col 6 Variance

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
PARAMETERS: P_COSTGR LIKE KKB0-KST MEMORY ID KAG.    "Cost Element Group
SELECT-OPTIONS:
            S_KSTAR  FOR RKMA4-KSTAR MATCHCODE OBJECT KART. "Cost Elemnt
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-128.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            S_KOSTL1 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST. "Cost Centre
PARAMETER:  P_KOSTL1 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOSTL2 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOSTL2 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOSTL3 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOSTL3 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOSTL4 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOSTL4 LIKE RGSB4-SETNR.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            S_KOSTL5 FOR RKMA1-KOSTL MATCHCODE OBJECT KOST.
PARAMETER:  P_KOSTL5 LIKE RGSB4-SETNR.

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
WRITE: HEAD1 UNDER TEXT-004, HEAD2 UNDER TEXT-009,
       HEAD3 UNDER TEXT-014, HEAD4 UNDER TEXT-019,
       HEAD5 UNDER TEXT-024.
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
  IF ( S_KOSTL1  IS INITIAL   AND   P_KOSTL1 IS INITIAL ) OR
     ( S_KOSTL1(1) <> SPACE   AND   P_KOSTL1 <> SPACE ).
     MESSAGE E002.
  ELSE.
     IF P_KOSTL1 <> SPACE.                  "Column 1 Heading
        HEAD1+9(12) = P_KOSTL1.
     ELSE.
        CONCATENATE: S_KOSTL1+3(10) ' *' INTO HEAD1+9(12).
        DESCRIBE TABLE S_KOSTL1 LINES NO_OF_LINES.
        IF ( S_KOSTL1+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
        ELSE.
           HEAD1+9(12) = S_KOSTL1+3(10).
        ENDIF.
     ENDIF.
  ENDIF.

  IF     ( S_KOSTL2 IS INITIAL   AND   P_KOSTL2 IS INITIAL ).
  ELSEIF ( S_KOSTL2(1) <> SPACE  AND   P_KOSTL2 <> SPACE ).
     MESSAGE E002.
     ELSE.
     IF P_KOSTL2 <> SPACE.                  "Column 2 Heading
        HEAD2+9(12) = P_KOSTL2.
     ELSE.
        CONCATENATE: S_KOSTL2+3(10) ' *' INTO HEAD2+9(12).
        DESCRIBE TABLE S_KOSTL2 LINES NO_OF_LINES.
        IF ( S_KOSTL2+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
        ELSE.
           HEAD2+9(12) = S_KOSTL2+3(10).
        ENDIF.
     ENDIF.
  ENDIF.

  IF     ( S_KOSTL3 IS INITIAL   AND   P_KOSTL3 IS INITIAL ).
  ELSEIF ( S_KOSTL3(1) <> SPACE  AND   P_KOSTL3 <> SPACE ).
     MESSAGE E002.
     ELSE.
     IF P_KOSTL3 <> SPACE.                  "Column 3 Heading
        HEAD3+9(12) = P_KOSTL3.
     ELSE.
        CONCATENATE: S_KOSTL3+3(10) ' *' INTO HEAD3+9(12).
        DESCRIBE TABLE S_KOSTL3 LINES NO_OF_LINES.
        IF ( S_KOSTL3+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
        ELSE.
           HEAD3+9(12) = S_KOSTL3+3(10).
        ENDIF.
     ENDIF.
  ENDIF.

  IF     ( S_KOSTL4 IS INITIAL   AND   P_KOSTL4 IS INITIAL ).
  ELSEIF ( S_KOSTL4(1) <> SPACE  AND   P_KOSTL4 <> SPACE ).
     MESSAGE E002.
     ELSE.
     IF P_KOSTL4 <> SPACE.                  "Column 4 Heading
        HEAD4+9(12) = P_KOSTL4.
     ELSE.
        CONCATENATE: S_KOSTL4+3(10) ' *' INTO HEAD4+9(12).
        DESCRIBE TABLE S_KOSTL4 LINES NO_OF_LINES.
        IF ( S_KOSTL4+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
        ELSE.
           HEAD4+9(12) = S_KOSTL4+3(10).
        ENDIF.
     ENDIF.
  ENDIF.

  IF     ( S_KOSTL5 IS INITIAL   AND   P_KOSTL5 IS INITIAL ).
  ELSEIF ( S_KOSTL5(1) <> SPACE  AND   P_KOSTL5 <> SPACE ).
     MESSAGE E002.
     ELSE.
     IF P_KOSTL5 <> SPACE.                  "Column 5 Heading
        HEAD5+9(12) = P_KOSTL5.
     ELSE.
        CONCATENATE: S_KOSTL5+3(10) ' *' INTO HEAD5+9(12).
        DESCRIBE TABLE S_KOSTL5 LINES NO_OF_LINES.
        IF ( S_KOSTL5+13(10) <> SPACE ) OR ( NO_OF_LINES > 1 ).
        ELSE.
           HEAD5+9(12) = S_KOSTL5+3(10).
        ENDIF.
     ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_COSTGR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0H'
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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOSTL1.        "Column 1
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOSTL2.        "Column 2
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOSTL3.        "Column 3
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOSTL4.        "Column 4
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KOSTL5.        "Column 5
   PERFORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL5.


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
* cost centre variant (s_kostl1).
  PERFORM GET_COST_CENTRES USING P_KOSTL1.
  LOOP AT CC_TAB.
    S_KOSTL1(1)     = 'I'.
    S_KOSTL1+1(2)   = 'BT'.
    S_KOSTL1+3(10)  = CC_TAB-FROM.
    S_KOSTL1+13(10) = CC_TAB-TO.
    APPEND S_KOSTL1.
  ENDLOOP.


* Get all cost centres in cost centre group and append to the
* cost centre variant (s_kostl2).
  PERFORM GET_COST_CENTRES USING P_KOSTL2.
  LOOP AT CC_TAB.
    S_KOSTL2(1)     = 'I'.
    S_KOSTL2+1(2)   = 'BT'.
    S_KOSTL2+3(10)  = CC_TAB-FROM.
    S_KOSTL2+13(10) = CC_TAB-TO.
    APPEND S_KOSTL2.
  ENDLOOP.

* Get all cost centres in cost centre group and append to the
* cost centre variant (s_kostl3).
  PERFORM GET_COST_CENTRES USING P_KOSTL3.
  LOOP AT CC_TAB.
    S_KOSTL3(1)     = 'I'.
    S_KOSTL3+1(2)   = 'BT'.
    S_KOSTL3+3(10)  = CC_TAB-FROM.
    S_KOSTL3+13(10) = CC_TAB-TO.
    APPEND S_KOSTL3.
  ENDLOOP.

* Get all cost centres in cost centre group and append to the
* cost centre variant (s_kostl4).
  PERFORM GET_COST_CENTRES USING P_KOSTL4.
  LOOP AT CC_TAB.
    S_KOSTL4(1)     = 'I'.
    S_KOSTL4+1(2)   = 'BT'.
    S_KOSTL4+3(10)  = CC_TAB-FROM.
    S_KOSTL4+13(10) = CC_TAB-TO.
    APPEND S_KOSTL4.
  ENDLOOP.

* Get all cost centres in cost centre group and append to the
* cost centre variant (s_kostl5).
  PERFORM GET_COST_CENTRES USING P_KOSTL5.
  LOOP AT CC_TAB.
    S_KOSTL5(1)     = 'I'.
    S_KOSTL5+1(2)   = 'BT'.
    S_KOSTL5+3(10)  = CC_TAB-FROM.
    S_KOSTL5+13(10) = CC_TAB-TO.
    APPEND S_KOSTL5.
  ENDLOOP.


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

*  calculate totals for actuals, plans, variance  (col 6)
   BIG_TABLE-AMTA6 = BIG_TABLE-AMTA1 + BIG_TABLE-AMTA2
                   + BIG_TABLE-AMTA3 + BIG_TABLE-AMTA4
                   + BIG_TABLE-AMTA5.

   BIG_TABLE-AMTP6 = BIG_TABLE-AMTP1 + BIG_TABLE-AMTP2
                   + BIG_TABLE-AMTP3 + BIG_TABLE-AMTP4
                   + BIG_TABLE-AMTP5.

   BIG_TABLE-AMTV6 = BIG_TABLE-AMTA6 - BIG_TABLE-AMTP6.

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

 FORMAT RESET.
 LOOP AT BIG_TABLE.

  PERFORM PRINT_INFO.                         "Detail Info
  WRITE:   20  BIG_TABLE-LTEXT.


  AT END OF SETNR.
    ULINE.
    SUM.
    FORMAT COLOR COL_TOTAL.
    PERFORM PRINT_INFO.
    WRITE: 2 BIG_TABLE-SETNR+2(10).

    LOOP AT NAME_TAB.                         "Description for CE Group
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
     PERFORM PRINT_INFO.
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
          (12) BIG_TABLE-AMTP5 DECIMALS 0 UNDER TEXT-026,
          (12) BIG_TABLE-AMTA6 DECIMALS 0 UNDER TEXT-031,      "Column 6
          (12) BIG_TABLE-AMTP6 DECIMALS 0 UNDER TEXT-032,
          (12) BIG_TABLE-AMTV6 DECIMALS 0 UNDER TEXT-033.
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
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = NAME_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

*-------------------- GET_COST_CENTRES ---------------------------------
FORM GET_COST_CENTRES USING SETNAME.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = CC_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

*----------------------- GET_PARAMETERS_DEFAULT ------------------------
* Get user default value (These are values the user has set to default
*-----------------------------------------------------------------------
*form get_parameters_default using parid userpar.
*
*
*  call function 'G_GET_USER_PARAMETER'
*       exporting
*             parameter_id    = parid
*       importing
*             parameter_value = userpar.
*
*endform.

*------------------------ CHECK_CENTRES_COSP ---------------------------
* This routine releases the dollar value only if the entry from the
* table satisfies both the cost element group and cost centre group
* criteria.
*-----------------------------------------------------------------------
form  check_centres_cosp.
 CLEAR BIG_TABLE.
 MOVE COSP-KSTAR TO BIG_TABLE-KSTAR.
 IF COSP-WRTTP = '01'.                                "PLANS
  IF COSP-VERSN = P_VERS.
     IF COSP-OBJNR+6(10) IN S_KOSTL1 AND S_KOSTL1(1) <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP1.
     ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOSTL2 AND S_KOSTL2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP2.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOSTL3 AND S_KOSTL3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP3.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOSTL4 AND S_KOSTL4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP4.
    ENDIF.
    IF COSP-OBJNR+6(10) IN S_KOSTL5 AND S_KOSTL5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP5.
    ENDIF.
  ENDIF.
 ELSEIF COSP-WRTTP  = '04'.                   "ACTUALS
     IF COSP-OBJNR+6(10) IN S_KOSTL1 AND S_KOSTL1(1) <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA1.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOSTL2 AND S_KOSTL2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA2.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOSTL3 AND S_KOSTL3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA3.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOSTL4 AND S_KOSTL4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA4.
     ENDIF.
     IF COSP-OBJNR+6(10) IN S_KOSTL5 AND S_KOSTL5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA5.
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
  IF COSS-WRTTP = '01'.
    IF COSS-VERSN = P_VERS.
     IF COSS-OBJNR+6(10) IN S_KOSTL1 AND S_KOSTL1 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP1.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL2 AND S_KOSTL2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP2.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL3 AND S_KOSTL3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP3.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL4 AND S_KOSTL4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP4.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL5 AND S_KOSTL5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTP5.
     ENDIF.
   ENDIF.
   ELSEIF COSS-WRTTP = '04'.                                  "ACTUALS
     IF COSS-OBJNR+6(10) IN S_KOSTL1 AND S_KOSTL1 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA1.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL2 AND S_KOSTL2 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA2.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL3 AND S_KOSTL3 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA3.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL4 AND S_KOSTL4 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA4.
     ENDIF.
     IF COSS-OBJNR+6(10) IN S_KOSTL5 AND S_KOSTL5 <> SPACE.
        MOVE VALUE TO BIG_TABLE-AMTA5.
     ENDIF.
  ENDIF.
   COLLECT BIG_TABLE.
ENDFORM.


*-------------------- COST_CENTRE_GROUP_PULLDOWN -----------------------
FORM COST_CENTRE_GROUP_PULLDOWN USING P_KOSTL.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  O_SEARCHFLD = P_KOKRS.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0H'
            FIELD_NAME         = 'KOSTL'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = P_KOSTL
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    P_KOSTL1 = O_TMPSET.
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
 DATA: ZKORR005 TYPE OLE2_OBJECT,
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

 CREATE OBJECT ZKORR005 'excel.application'.
 SET PROPERTY OF ZKORR005 'visible' = 1.
 CALL METHOD OF ZKORR005 'Workbooks' = WORKBOOK.
 CALL METHOD OF WORKBOOK 'Add'.
 CALL METHOD OF ZKORR005 'Worksheets' = SHEET
                  EXPORTING #1 = 1.
 CALL METHOD OF SHEET 'Activate'.

 LOOP AT XLS_NAME.
   INDEX = ROW_MAX * ( SY-TABIX - 1 ) + 1.
   DO 15 TIMES.
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
 HD_TITLE       = TEXT-HD1.
 XLS_NAME-AMTA3 = HD_TITLE(12).
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
 MOVE HEAD1+9(12) TO: XLS_NAME-AMTA1, XLS_NAME-AMTP1.
 MOVE HEAD2+9(12) TO: XLS_NAME-AMTA2, XLS_NAME-AMTP2.
 MOVE HEAD3+9(12) TO: XLS_NAME-AMTA3, XLS_NAME-AMTP3.
 MOVE HEAD4+9(12) TO: XLS_NAME-AMTA4, XLS_NAME-AMTP4.
 MOVE HEAD5+9(12) TO: XLS_NAME-AMTA5, XLS_NAME-AMTP5.
 MOVE 'TOTAL'     TO: XLS_NAME-AMTA6, XLS_NAME-AMTP6, XLS_NAME-AMTV6.

 APPEND XLS_NAME.

 XLS_NAME-KSTAR = 'Element'.
 XLS_NAME-LTEXT = 'Description'.

 MOVE 'Actual' TO: XLS_NAME-AMTA1, XLS_NAME-AMTA2, XLS_NAME-AMTA3,
                   XLS_NAME-AMTA4, XLS_NAME-AMTA5, XLS_NAME-AMTA6.

 MOVE 'Plan  ' TO: XLS_NAME-AMTP1, XLS_NAME-AMTP2, XLS_NAME-AMTP3,
                   XLS_NAME-AMTP4, XLS_NAME-AMTP5, XLS_NAME-AMTP6.

 XLS_NAME-AMTV6 = 'Variance'.
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
  WRITE BIG_TABLE-AMTV6 TO XLS_NAME-AMTV6 DECIMALS 0.
  PERFORM SHIFT_NEGATIVE USING XLS_NAME-AMTV6.
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
