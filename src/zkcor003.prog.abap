REPORT ZKCOR003 NO STANDARD PAGE HEADING
LINE-SIZE 132 LINE-COUNT 58 MESSAGE-ID KO.
************************************************************************
* PROGRAM: ZKCOR003
* Cost of Service by Internal Order Group
* AUTHOR : M DeMeester
* Date   : April 6th, 2000
* This report will calculate the cost of service based on the various
* internal order groups.
* Details include I/O Group, Description, Labour Charges, Total $
* percentages.
*
************************************************************************
* 01/09/02  mokhan   Issue Log: 925
*                    Program changes to adapt for the new set logic to
*                    get cost center group and it's corresponding cost
*                    centers. REF: R/3 NOTE NO. 92029
* 00/11/29  mdemeest 4.6B Text doesn't print on report - fixed using
*                         G_SET_GET_INFO
* 00/04/27  mdemeest #123 WBS Settlements should be treated as capital.
* 00/04/06  mdemeest #123 New request
************************************************************************

TABLES: AUFK,              "Order master data
        COSS,              "CO Object:  Cost Totals - Internal
        COSP,              "CO Object:  Cost Totals - External
        T001,              "Company Code
        COBRB,             "Distribution Rules Settlement Rule
        RGSBS,
        KKB0,
        RGSB4.

DATA:   WRTTP      LIKE COSP-WRTTP,
        VERSN      LIKE COSP-VERSN,
        AVORG      LIKE COBRB-AVORG,
        PERCENT    LIKE COSP-WKG001,
        OBJNR      LIKE COSP-OBJNR,
        KTEXT      LIKE AUFK-KTEXT,
        FROMDATE(6) TYPE C,
        TODATE(6)   TYPE C,
        VARDATE(6)  TYPE C.

DATA:   BEGIN OF VALTAB OCCURS 10.           "Orders for a group
        INCLUDE STRUCTURE RGSB4.
DATA:   END OF VALTAB.

DATA:   BEGIN OF LABR_COSTS  OCCURS 1000,
          SETNAME LIKE VALTAB-SETNR,
          FROM    LIKE VALTAB-FROM,
          TO      LIKE VALTAB-TO.
DATA:   END OF LABR_COSTS.

DATA:   BEGIN OF TOT_COSTS  OCCURS 1000,
          SETNAME LIKE VALTAB-SETNR,
          FROM    LIKE VALTAB-FROM,
          TO      LIKE VALTAB-TO.
DATA:   END OF TOT_COSTS.

DATA:   BEGIN OF CAP_COSTS  OCCURS 1000,
          SETNAME LIKE VALTAB-SETNR,
          FROM    LIKE VALTAB-FROM,
          TO      LIKE VALTAB-TO.
DATA:   END OF CAP_COSTS.

DATA:   BEGIN OF ORDER_TAB  OCCURS 1000,     "Info for output
          TITLE   LIKE VALTAB-TITLE,
          AUFNR   LIKE AUFK-AUFNR,
          PPER    LIKE COSP-WKG001,                         "PLANS
          PLABR   LIKE COSP-WKG001,
          PCAP    LIKE COSP-WKG001,
          PTOT    LIKE COSP-WKG001,
          APER    LIKE COSP-WKG001,                         "ACTUALS
          ALABR   LIKE COSP-WKG001,
          ATOT    LIKE COSP-WKG001,
          ACAP    LIKE COSP-WKG001,
          KTEXT   LIKE VALTAB-TITLE,
          OBJNR   LIKE COSP-OBJNR,
          KSTAR   LIKE COSP-KSTAR.
DATA:   END OF ORDER_TAB.



*-------------------------- Variant Screen Inputs ----------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_BUKRS LIKE T001-BUKRS       OBLIGATORY MEMORY ID BUK,
                P_SEARCH LIKE CSKU-KTOPL      OBLIGATORY MEMORY ID KPL,
                P_GJAHR  LIKE COSP-GJAHR      DEFAULT SY-DATUM(4).
SELECT-OPTIONS: S_PER  FOR SY-DATUM+4(2)      DEFAULT '01' TO '12',
                S_ORDGR FOR RGSB4-SETNR       OBLIGATORY MEMORY ID KOG
                                                    NO INTERVALS,
                S_TOTGRP FOR RGSB4-SETNR      OBLIGATORY MEMORY ID KAG
                                                    NO INTERVALS,
                S_ELMGRP FOR RGSB4-SETNR      OBLIGATORY MEMORY ID KAG
                                                    NO INTERVALS,
                S_CCGRP FOR RGSB4-SETNR       OBLIGATORY NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.                             "Actuals
PARAMETER:       P_ACTUAL   RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 05(25) TEXT-008.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                             "Plans
PARAMETER:      P_PLAN      RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT 05(25) TEXT-009.
PARAMETER:      P_VERSN  LIKE COSP-VERSN      DEFAULT '0'.
SELECTION-SCREEN COMMENT 37(25) TEXT-010.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.


SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-011.
SELECTION-SCREEN BEGIN OF LINE.                             "Report
PARAMETER:      P_RPT      RADIOBUTTON GROUP RADA DEFAULT 'X'.
SELECTION-SCREEN COMMENT 05(40) TEXT-012.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                           "Create File
PARAMETER:      P_FILE     RADIOBUTTON GROUP RADA.
SELECTION-SCREEN COMMENT 05(40) TEXT-013.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.

*-------------- PULLDOWNS for VARIANTS ---------------------------------
* Internal Order Groups, Cost Element Groups and Cost Centre Groups
* require this function call to create the pulldown selections
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDGR-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0103'
            FIELD_NAME         = 'AUFNR'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = S_ORDGR-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    S_CCGRP = O_TMPSET.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CCGRP-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0101'
            FIELD_NAME         = 'KOSTL'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = S_CCGRP-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    S_CCGRP = O_TMPSET.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ELMGRP-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0102'
            FIELD_NAME         = 'KSTAR'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = O_SEARCHFLD
       IMPORTING
            SET_NAME           = S_ELMGRP-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    S_CCGRP = O_TMPSET.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TOTGRP-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0102'
            FIELD_NAME         = 'KSTAR'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = O_SEARCHFLD
       IMPORTING
            SET_NAME           = S_TOTGRP-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    S_CCGRP = O_TMPSET.
  ENDIF.
*-----------------------  ERRORS on Screen -----------------------------
*at selection-screen.               "Set values for selection of ACTUALS

*-------------------------  START of SELECTION -------------------------
*  Main processing module
*-----------------------------------------------------------------------
START-OF-SELECTION.

  VARDATE(4) = P_GJAHR.
  IF S_PER+5(2) = '00'.
    VARDATE+4(2) = S_PER+3(2).
  ELSE.
    VARDATE+4(2) = S_PER+5(2).
  ENDIF.
  IF P_ACTUAL = 'X'.
    VERSN = '0'.
    WRTTP = '04'.
    AVORG = 'KOAO'.
  ENDIF.

  IF P_PLAN = 'X'.                "Set values for selection of PLANS
    VERSN = P_VERSN.
    WRTTP = '01'.
    AVORG = 'KOAP'.
  ENDIF.

  SELECT SINGLE * FROM T001         "Company Name for Report Title
      WHERE BUKRS = P_BUKRS.


  PERFORM LOOP_THRU_PRIMARY.
  PERFORM LOOP_THRU_LABOUR.
  PERFORM LOOP_THRU_CAPITALIZATION.
  PERFORM LOOP_THRU_ORDERS.
  PERFORM DETERMINE_CHARGES.
  SORT ORDER_TAB BY TITLE AUFNR.
  PERFORM PRINT_TOTAL.

*------------------------ End of Main Processing -----------------------

*----------------------------  SUBROUTINES  ----------------------------
*---------------------------- LOOP_THRU_ORDERS -------------------------
* Get info for each order
*-----------------------------------------------------------------------
FORM LOOP_THRU_ORDERS.
  LOOP AT S_ORDGR.
    PERFORM GET_ORDERS_INFO.
  ENDLOOP.
ENDFORM.

*----------------------  LOOP_THRU_LABOUR ------------------------------
* Routine takes all the entered Labour CE Groups and determines the
* cost elements or cost element ranges.
FORM LOOP_THRU_LABOUR.
  DATA: SETNAME(30)  TYPE C.

  LOOP AT S_ELMGRP.
    MOVE P_SEARCH       TO SETNAME(4).
    MOVE S_ELMGRP+3(12) TO SETNAME+4(12).
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
         EXPORTING
              CLASS         = '0102'
              SETNR         = SETNAME
              TABLE         = 'CCSS'
         TABLES
              SET_VALUES    = VALTAB
         EXCEPTIONS
              SET_NOT_FOUND = 1.

    LOOP AT VALTAB.
      MOVE SETNAME       TO LABR_COSTS-SETNAME.
      MOVE VALTAB-FROM   TO LABR_COSTS-FROM.
      MOVE VALTAB-TO     TO LABR_COSTS-TO.
      APPEND LABR_COSTS.
    ENDLOOP.                                     "End of VALTAB loop
  ENDLOOP.                                      "End of S_ELMGRP loop

ENDFORM.

*----------------------  LOOP_THRU_PRIMARY -----------------------------
* Routine takes all the entered "TOTAL" CE Groups and determines the
* cost elements or cost element ranges.
FORM LOOP_THRU_PRIMARY.
  DATA: SETNAME(30) TYPE C.

  LOOP AT S_TOTGRP.
    MOVE P_SEARCH       TO SETNAME(4).
    MOVE S_TOTGRP+3(12) TO SETNAME+4(12).
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
         EXPORTING
              CLASS         = '0102'
              SETNR         = SETNAME
              TABLE         = 'CCSS'
         TABLES
              SET_VALUES    = VALTAB
         EXCEPTIONS
              SET_NOT_FOUND = 1.

    LOOP AT VALTAB.
      MOVE SETNAME       TO TOT_COSTS-SETNAME.
      MOVE VALTAB-FROM   TO TOT_COSTS-FROM.
      MOVE VALTAB-TO     TO TOT_COSTS-TO.
      APPEND TOT_COSTS.
    ENDLOOP.                                     "End of VALTAB loop
  ENDLOOP.                                      "End of S_TOTGRP loop

ENDFORM.

*---------------START OF ISSUE LOG 925 CHANGES ----------------------
*
*----------  LOOP_THRU_CAPITALIZATION  ---------------------
* Routine takes all the entered Capitalization Cost Centre Groups and
* determines the cost centres or cost centre ranges.
FORM LOOP_THRU_CAPITALIZATION.
* Local working variables
  DATA: P_SET LIKE SETHIER-SHORTNAME.
  DATA: P_SETID    LIKE SETHIER-SETID,   "Internal ID of set to be read
        FIND_SETID LIKE SETHIER-SETID,   "Internal ID of set to be found
        MY_INT    TYPE I,
        MY_TABIX  LIKE SY-TABIX.

* Internal tables to hold set hierarchy, values, pointers and sorts
  DATA: SET_HIERARCHY    LIKE SETHIER     OCCURS 0 WITH HEADER LINE,
        SET_VALUES       LIKE SETVALUES   OCCURS 0 WITH HEADER LINE,
        SET_HIER_PTR     LIKE SETHIERPTR  OCCURS 0 WITH HEADER LINE,
        SET_VAL_PTR      LIKE SETVALPTR   OCCURS 0 WITH HEADER LINE,
        SET_HIER_SORT    LIKE SETHIERSRT  OCCURS 0 WITH HEADER LINE,
        SET_VAL_SORT     LIKE SETVALSRT   OCCURS 0 WITH HEADER LINE,
        FOUND_POINTERS   LIKE SETVALSRT   OCCURS 0 WITH HEADER LINE.

* Include to draw lines in lists
  INCLUDE <LINE>.

  DATA: SETNAME LIKE RKMAH-HNAM2.

  LOOP AT S_CCGRP.
    MOVE S_CCGRP+3(12) TO P_SET.
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
         EXPORTING
              SHORTNAME = P_SET
              OLD_SETID = P_SETID
         IMPORTING
              NEW_SETID = P_SETID.

    CALL FUNCTION 'G_SET_TREE_IMPORT'
         EXPORTING
              SETID              = P_SETID  "Set-ID, not the Setname!!!
              NO_TABLE_BUFFERING = ' '
         TABLES
              SET_HIERARCHY      = SET_HIERARCHY
              SET_VALUES         = SET_VALUES.


    CALL FUNCTION 'G_SET_TREE_ADD_POINTERS'
         TABLES
              SET_HIERARCHY = SET_HIERARCHY
              SET_VALUES    = SET_VALUES
              SET_HIER_PTR  = SET_HIER_PTR
              SET_VAL_PTR   = SET_VAL_PTR
              SET_HIER_SORT = SET_HIER_SORT
              SET_VAL_SORT  = SET_VAL_SORT
         EXCEPTIONS
              OTHERS        = 1.

    CHECK SY-SUBRC = 0.


    LOOP AT SET_VAL_PTR.
      MOVE P_SET              TO CAP_COSTS-SETNAME.
      MOVE SET_VAL_PTR-FROM   TO CAP_COSTS-FROM.
      MOVE SET_VAL_PTR-TO     TO CAP_COSTS-TO.
      APPEND CAP_COSTS.
    ENDLOOP.                                    "End of SET_VAL_PTR loop
  ENDLOOP.                                      "End of S_CCGRP loop

ENDFORM.
*----------------------  LOOP_THRU_CAPITALIZATION  ---------------------
* Routine takes all the entered Capitalization Cost Centre Groups and
* determines the cost centres or cost centre ranges.
**FORM LOOP_THRU_CAPITALIZATION.
**DATA: SETNAME LIKE RKMAH-HNAM2.

**LOOP AT S_CCGRP.
**  MOVE S_CCGRP+3(12) TO SETNAME.
**  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
**       EXPORTING
**            CLASS         = '0101'
**            SETNR         = SETNAME
**            TABLE         = 'CCSS'
**       TABLES
**            SET_VALUES    = VALTAB
**       EXCEPTIONS
**            SET_NOT_FOUND = 1.
**
** LOOP AT VALTAB.
**   MOVE SETNAME       TO CAP_COSTS-SETNAME.
**   MOVE VALTAB-FROM   TO CAP_COSTS-FROM.
**   MOVE VALTAB-TO     TO CAP_COSTS-TO.
**   APPEND CAP_COSTS.
** ENDLOOP.                                     "End of VALTAB loop
**ENDLOOP.                                      "End of S_ELMGRP loop
**
**ENDFORM.

*---------------------------END OF ISSUE LOG 925 CHANGES ---------------

TOP-OF-PAGE.
  IF P_RPT = 'X' OR
     P_FILE = 'X' AND SY-PAGNO = 00001.
    FORMAT RESET.
    FORMAT COLOR 1.
    WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,    "Title
           50 T001-BUTXT COLOR 4 INTENSIFIED ON,
          100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

    WRITE: / TEXT-CLT UNDER TEXT-RPT,
             SY-MANDT UNDER SY-REPID, SY-SYSID,             "Client
             TEXT-TTL UNDER T001-BUTXT COLOR 4 INTENSIFIED ON,
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM."Page
    WRITE: / TEXT-VRN UNDER TEXT-RPT, P_VERSN,
             TEXT-GJA UNDER T001-BUTXT, P_GJAHR,
             TEXT-016, S_PER+3(2).
    IF S_PER+5(2) <> '00'.
      WRITE: 77 '-', S_PER+5(2).
    ENDIF.

  IF P_ACTUAL = 'X'.                              "Plan / Actual Report?
      WRITE: / TEXT-014 UNDER T001-BUTXT.
    ELSEIF P_PLAN = 'X'.
      WRITE: / TEXT-015 UNDER T001-BUTXT.
    ENDIF.

    SKIP 1.
    ULINE.                                                  "Fiscal yr

    FORMAT COLOR 1.
    WRITE: /2 TEXT-002, 33  TEXT-003,  47 TEXT-004,         "Columns
           93 TEXT-005, 109 TEXT-006, 125 TEXT-007.
    FORMAT RESET.
    ULINE.
  ENDIF.
*----------------------- end of page header ----------------------------


*--------------------- GET_ORDERS_INFO---------------------------------*
*   Function module used to retrieve order number ranges.
*---------------------------------------------------------------------*
*   ranges for selected order group.  These ranges are used to get
*   all order numbers.  The next step is to obtain numbers of cost
*   elements assosiated with orders together with fiscal data.
*   Order numbers, cost elements and fiscal data are stored in ORDER_TAB
*   table.
*---------------------------------------------------------------------*
*  -->  SETNAME - name of the order group
*---------------------------------------------------------------------*
FORM GET_ORDERS_INFO.
  DATA: SETNAME LIKE RKMAH-HNAM2.
*----------------------------------------------------------------------
* 4.6b Gets the internal order group description from SAP
*----------------------------------------------------------------------
  DATA:  WA_SETNAME(30)  TYPE C.
  DATA:  WA_INFO         LIKE SETINFO.

  MOVE S_ORDGR+3(12) TO WA_SETNAME.
  CALL FUNCTION 'G_SET_GET_INFO'
       EXPORTING
            CLASS         = '0103'
            SETNAME       = WA_SETNAME
       IMPORTING
            INFO          = WA_INFO
       EXCEPTIONS
            SET_NOT_FOUND = 1.
*-----------------------------------------------------------------------

  MOVE S_ORDGR+3(12) TO SETNAME.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0103'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = VALTAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.

  LOOP AT VALTAB.
   SELECT * FROM AUFK                       "Cross reference of IO to CE
        WHERE AUFNR GE VALTAB-FROM
        AND   AUFNR LE VALTAB-TO.
      SELECT * FROM COSS
         WHERE OBJNR = AUFK-OBJNR
           AND GJAHR = P_GJAHR
           AND ( VERSN = VERSN OR VERSN = '000' )
           AND WRTTP = WRTTP.
        IF COSS-WRTTP = '01'.
          CHECK COSS-VERSN = P_VERSN.
          MOVE WA_INFO-TITLE  TO ORDER_TAB-TITLE.
          MOVE AUFK-AUFNR   TO ORDER_TAB-AUFNR.
          MOVE AUFK-KTEXT   TO ORDER_TAB-KTEXT.
          MOVE COSS-OBJNR   TO ORDER_TAB-OBJNR.
          MOVE COSS-KSTAR   TO ORDER_TAB-KSTAR.
          ADD COSS-WKG001  FROM S_PER+3(2) TO S_PER+5(2)
                                                 GIVING ORDER_TAB-PPER.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.

        IF COSS-WRTTP = '04'.
          CHECK COSS-VERSN = '000'.
          MOVE WA_INFO-TITLE  TO ORDER_TAB-TITLE.
          MOVE AUFK-AUFNR     TO ORDER_TAB-AUFNR.
          MOVE AUFK-KTEXT     TO ORDER_TAB-KTEXT.
          MOVE COSS-OBJNR     TO ORDER_TAB-OBJNR.
          MOVE COSS-KSTAR     TO ORDER_TAB-KSTAR.
          ADD COSS-WKG001 FROM S_PER+3(2) TO S_PER+5(2)
                                                GIVING ORDER_TAB-APER.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
      ENDSELECT.

      SELECT * FROM COSP
          WHERE OBJNR = AUFK-OBJNR
          AND GJAHR = P_GJAHR.
        CHECK COSP-WRTTP = '01' OR COSP-WRTTP = '04'.
        IF COSP-WRTTP = '01'.
          CHECK COSP-VERSN = P_VERSN.
          MOVE WA_INFO-TITLE  TO ORDER_TAB-TITLE.
          MOVE AUFK-AUFNR   TO ORDER_TAB-AUFNR.
          MOVE AUFK-KTEXT   TO ORDER_TAB-KTEXT.
          MOVE COSP-OBJNR   TO ORDER_TAB-OBJNR.
          MOVE COSP-KSTAR   TO ORDER_TAB-KSTAR.
          ADD COSP-WKG001 FROM S_PER+3(2) TO S_PER+5(2)
                                                GIVING ORDER_TAB-PPER.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.

        IF COSP-WRTTP = '04'.
          CHECK COSP-VERSN = '000'.
          MOVE WA_INFO-TITLE TO ORDER_TAB-TITLE.
          MOVE AUFK-AUFNR    TO ORDER_TAB-AUFNR.
          MOVE AUFK-KTEXT    TO ORDER_TAB-KTEXT.
          MOVE COSP-OBJNR    TO ORDER_TAB-OBJNR.
          MOVE COSP-KSTAR    TO ORDER_TAB-KSTAR.
          ADD COSP-WKG001 FROM S_PER+3(2) TO S_PER+5(2)
                                                GIVING ORDER_TAB-APER.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
      ENDSELECT.
    ENDSELECT.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRINT_TOTAL                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_TOTAL.
  LOOP AT ORDER_TAB.
    MOVE ORDER_TAB-OBJNR TO OBJNR.
    MOVE ORDER_TAB-KTEXT TO KTEXT.

    AT NEW TITLE.                                 "Order Group
      IF P_RPT = 'X'.
        NEW-PAGE.
      ENDIF.
    ENDAT.

AT END OF AUFNR.                                  "Sum by internal order
      MOVE 100 TO PERCENT.
      SUM.
* calculate O&M percentage
      PERFORM DETERMINE_OANDM_PERCENT.
      PERFORM PRINT_VLINE.
      WRITE:   ORDER_TAB-TITLE UNDER TEXT-002.
      WRITE:   ORDER_TAB-AUFNR UNDER TEXT-003,
                         KTEXT UNDER TEXT-004.
      IF P_ACTUAL = 'X'.
        WRITE: (15) ORDER_TAB-ATOT  UNDER TEXT-005,
               (15) ORDER_TAB-ALABR UNDER TEXT-006,
               (7)  PERCENT         UNDER TEXT-007.
      ELSE.
        WRITE: (15) ORDER_TAB-PTOT  UNDER TEXT-005,
               (15) ORDER_TAB-PLABR UNDER TEXT-006,
               (7)  PERCENT         UNDER TEXT-007.
      ENDIF.
    ENDAT.

  ENDLOOP.

  WRITE: / TEXT-017 UNDER T001-BUTXT.
ENDFORM.
*-------------------------  DETERMINE_LABOUR_CHARGES  ------------------
*  Loop thru project table and any items that have labour cost elements
*  should have their labour charge column updated.
*-----------------------------------------------------------------------
FORM DETERMINE_CHARGES.
  LOOP AT ORDER_TAB.
    LOOP AT LABR_COSTS.
      IF ORDER_TAB-KSTAR >= LABR_COSTS-FROM AND
                                   ORDER_TAB-KSTAR <= LABR_COSTS-TO.
        MOVE ORDER_TAB-APER          TO ORDER_TAB-ALABR.
        MOVE ORDER_TAB-PPER          TO ORDER_TAB-PLABR.
        MODIFY ORDER_TAB.
        EXIT.
      ENDIF.
    ENDLOOP.

    LOOP AT TOT_COSTS.
      IF ORDER_TAB-KSTAR >= TOT_COSTS-FROM AND
                                    ORDER_TAB-KSTAR <= TOT_COSTS-TO.
        MOVE ORDER_TAB-APER          TO ORDER_TAB-ATOT.
        MOVE ORDER_TAB-PPER          TO ORDER_TAB-PTOT.
        MODIFY ORDER_TAB.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.
ENDFORM.

*-------------------------  DETERMINE_TOTAL_CHARGES  ------------------
*  Loop thru project table and any items that have labour cost elements
*  should have their labour charge column updated.
*-----------------------------------------------------------------------
FORM DETERMINE_LABOUR_CHARGES.
  LOOP AT ORDER_TAB.
    LOOP AT LABR_COSTS.
      IF ORDER_TAB-KSTAR >= LABR_COSTS-FROM AND
                                   ORDER_TAB-KSTAR <= LABR_COSTS-TO.
        MOVE ORDER_TAB-APER          TO ORDER_TAB-ALABR.
        MOVE ORDER_TAB-PPER          TO ORDER_TAB-PLABR.
        MODIFY ORDER_TAB.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DETERMINE_OANDM_PERCENT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DETERMINE_OANDM_PERCENT.

  IF P_ACTUAL = 'X'.
     PERFORM ACTUAL_OANDM_PERCENT.
  ELSE.
     PERFORM PLAN_OANDM_PERCENT.
*                                         "Mohammad Khan Dec.6, 2005.
*     IF PERCENT = 100.                   "Wilma asked for this change.
*        PERFORM ACTUAL_OANDM_PERCENT.    "TR # is not availabe as it's
*     ENDIF.                              "less than half day work.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM ACTUAL_OANDM_PERCENT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
FORM ACTUAL_OANDM_PERCENT.

  SELECT * FROM COBRB
   WHERE OBJNR = OBJNR
     AND AVORG = 'KOAO'.             "Actual

    MOVE COBRB-GABJA      TO FROMDATE(4).
    MOVE COBRB-GABPE+1(2) TO FROMDATE+4(2).
    MOVE COBRB-GBISJ      TO TODATE(4).
    MOVE COBRB-GBISP+1(2) TO TODATE+4(2).

    LOOP AT CAP_COSTS.
      IF  COBRB-KOSTL >= CAP_COSTS-FROM AND COBRB-KOSTL <= CAP_COSTS-TO
           AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
                 OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
                 OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
*                 OR ( FROMDATE = '000000' AND TODATE = '000000' ) ).
        COMPUTE PERCENT = PERCENT - COBRB-PROZS.
      ENDIF.
    ENDLOOP.

    IF  ( COBRB-KONTY = 'PR' OR COBRB-KONTY = 'SK' )
    AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
             OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
             OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
*             OR ( FROMDATE = '000000' AND TODATE = '000000' ) ).
      COMPUTE PERCENT = PERCENT - COBRB-PROZS.
    ENDIF.

  ENDSELECT.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM PLAN_OANDM_PERCENT                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
FORM PLAN_OANDM_PERCENT.

  SELECT * FROM COBRB
   WHERE OBJNR = OBJNR
     AND AVORG = 'KOAP'
     AND VERSN = P_VERSN.

    MOVE COBRB-GABJA      TO FROMDATE(4).
    MOVE COBRB-GABPE+1(2) TO FROMDATE+4(2).
    MOVE COBRB-GBISJ      TO TODATE(4).
    MOVE COBRB-GBISP+1(2) TO TODATE+4(2).

    LOOP AT CAP_COSTS.
      IF  COBRB-KOSTL >= CAP_COSTS-FROM AND COBRB-KOSTL <= CAP_COSTS-TO
           AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
                 OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
                 OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
        COMPUTE PERCENT = PERCENT - COBRB-PROZS.
      ENDIF.
    ENDLOOP.

    IF  ( COBRB-KONTY = 'PR' OR COBRB-KONTY = 'SK' )
    AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
             OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
             OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
      COMPUTE PERCENT = PERCENT - COBRB-PROZS.
    ENDIF.

  ENDSELECT.


ENDFORM.
*-----------------------------------------------------------------------


*FORM DETERMINE_OANDM_PERCENT.
*  SELECT * FROM COBRB
*   WHERE OBJNR = OBJNR
*     AND AVORG = AVORG
*     AND VERSN = VERSN.                       "CHANGE FEB. 4
*    MOVE COBRB-GABJA      TO FROMDATE(4).
*    MOVE COBRB-GABPE+1(2) TO FROMDATE+4(2).
*    MOVE COBRB-GBISJ      TO TODATE(4).
*    MOVE COBRB-GBISP+1(2) TO TODATE+4(2).
*
*    LOOP AT CAP_COSTS.
*      IF  COBRB-KOSTL >= CAP_COSTS-FROM AND COBRB-KOSTL <= CAP_COSTS-TO
*           AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
*                 OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
*                 OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
*        COMPUTE PERCENT = PERCENT - COBRB-PROZS.
*      ENDIF.
*    ENDLOOP.
*
*    IF  COBRB-KONTY = 'PR'                                     "WBS
*        AND (  ( VARDATE BETWEEN FROMDATE AND TODATE )
*             OR ( VARDATE >= FROMDATE AND TODATE = '000000' )
*             OR ( VARDATE <= TODATE AND FROMDATE = '000000' ) ).
*      COMPUTE PERCENT = PERCENT - COBRB-PROZS.
*    ENDIF.
*
*  ENDSELECT.
*ENDFORM.
*
*---------------------------------------------------------------------*
*       FORM PRINT_VLINE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_VLINE.
  WRITE: /1 SY-VLINE,   32 SY-VLINE,  45 SY-VLINE, 92 SY-VLINE,
         108 SY-VLINE, 124 SY-VLINE, 132 SY-VLINE.
ENDFORM.
