REPORT ZKORR001 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.

************************************************************************
*  Author:   Dorothy Bialkowska
*  Brief Description:
*     - The purpose of this program is to produce a report listing
*       actual and plan values for selected group orders and cost
*       elements groups.
************************************************************************
* 00/11/16 mdemeest 4.6B Titles no longer printing
* 00/06/14 mdemeest 4.6B Changed itema, itemno, itemnoa from I to N
*                        to fix sort
* 00/04/27 mdemeest Orders should appear in same sequence as in order
*                   group
* 00/03/21 mdemeest Add a summary at a level higher (costgr1)
* 00/02/29 mdemeest Fix sort of cost elements so that items summarize
*                   properly
* 98/12/04 MD7140 #Separation Make chart of accounts variable
* 98/09/28 md7140 #--- Add variants to page title header
* 97/04/24 md7140 move plan from actual column to plan column
***** ***** ***** ***** ***** ****** ***** ***** ***** ***** ***** *****
*  Modified by Nancy Gilligan, OmniLogic  98/10   D30K906170           *
*    - standardized headers, removed hard coding from variables        *
************************************************************************

TABLES:   AUFK, COSP, COSS, CSKA, CSKU, T800Y.

DATA:     BEGIN OF ORDER_TAB OCCURS 100,
             ITEMA(4)  TYPE N,
             ORDERNR     LIKE    AUFK-AUFNR,
             ITEMNO(4) TYPE N,
             COSTGR1    LIKE    RGSB4-SETNR,        "mdemeest 2000/02/29
             ITEMNOA(4) TYPE N,
             COSTGR     LIKE   RGSBS-SETNR,        "mdemeest 2000/03/21
             COSTEL      LIKE    COSS-KSTAR,
             GRNAME      LIKE    RGSB4-TITLE,
             OBJNR       LIKE    COSS-OBJNR,
             DESCRIPT    LIKE    CSKU-LTEXT,
             PPER        LIKE    COSP-WKG001,  "Sum of Plans
             APER        LIKE    COSP-WKG001,  "Sum of Actuals
          END OF ORDER_TAB.

DATA:    BEGIN OF NAME_TAB OCCURS 10.
        INCLUDE STRUCTURE RGSB4.
DATA:    END OF NAME_TAB.

DATA:    BEGIN OF TITLE_TAB OCCURS 10.
        INCLUDE STRUCTURE RGSBS.
DATA:    END OF TITLE_TAB.

DATA:    BEGIN OF ALLCOST_TAB OCCURS 1000,
             COSTEL     LIKE    CSKA-KSTAR,
             DESCRIPT   LIKE    CSKU-LTEXT,
             GRNAME     LIKE    RGSB4-TITLE,
             COSTGR     LIKE    RGSB4-SETNR,
          END OF ALLCOST_TAB.


  DATA:     BEGIN OF VALTAB OCCURS 10.
          INCLUDE STRUCTURE RGSB4.
  DATA:     END OF VALTAB.

data: begin of wa_setinfo  occurs 0.
      include  structure setinfo.
data: end of wa_setinfo.

data:  setid like sethier-setid.

DATA:      BEGIN OF KSTAR_STRUCTURE OCCURS 100.
             INCLUDE STRUCTURE SETHIER.
DATA:      END OF KSTAR_STRUCTURE.

DATA:    PLAN           LIKE    COSP-WKG001,
                                "plan value from period1 to period2
         ACTUAL         LIKE    COSP-WKG001,
                                "actual value from period1 to period2
         DELTA1         LIKE    COSP-WKG001,
                                       "formula: DELTA1 = ACTUAL - PLAN
         DELTA2         TYPE P  DECIMALS 2,
         CTR            TYPE I,
             COSTGR1     LIKE   RGSBS-SETNR.        "mdemeest 2000/03/21


DATA:  SETNR LIKE RGSBS-SETNR.
DATA:  SETTITLE LIKE RGSBS-TITLE.

*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_ORDGR     LIKE    RKMAH-HNAM2   OBLIGATORY,
            P_COSTGR    LIKE    KKB0-KST      OBLIGATORY MEMORY ID KAG
                                              DEFAULT 'PRIMARY',
            P_VERS      LIKE    TKVST-VERSI   DEFAULT '0',
            P_FYEAR     LIKE    COSP-GJAHR    DEFAULT SY-DATUM(4),
            P_START     LIKE    SY-DATUM+4(2) DEFAULT '01',
            P_END       LIKE    SY-DATUM+4(2) DEFAULT '12',
            P_SEARCH    LIKE   T800S-SEARCHFLD OBLIGATORY MEMORY ID KPL
                                          DEFAULT 'COAT'.
SELECTION-SCREEN END OF BLOCK BOX.
*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ORDGR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0103'             "mdemeest 2001/02/19
            FIELD_NAME         = 'AUFNR'
            SEARCHFLD_REQUIRED = SPACE
            START_COLUMN       = 10
            START_ROW          = 5
            TABLE              = 'CCSS'
            TYPELIST           = 'BS'
       IMPORTING
            SET_NAME           = P_ORDGR
       EXCEPTIONS
            NO_SET_PICKED      = 1
            OTHERS             = 2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_COSTGR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0102'             "mdemeest 2001/02/19
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

*-----------------------------------------------------------------------
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
        105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: 45 TEXT-TTL.                                   "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
  WRITE: / TEXT-012 UNDER TEXT-RPT, P_VERS UNDER SY-MANDT.
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10

   WRITE:  20 TEXT-ODR, P_ORDGR, TEXT-CEG, P_COSTGR.
  IF P_END = SPACE.
     WRITE:  P_FYEAR NO-GAP, TEXT-SLH NO-GAP, P_START.
  ELSE.
     WRITE:  P_FYEAR NO-GAP, TEXT-SLH NO-GAP, P_START,
              TEXT-DSH, P_FYEAR NO-GAP, TEXT-SLH NO-GAP, P_END.
  ENDIF.

* write: / text-011, sy-datum, 117 text-012, sy-pagno.
* write: / sy-uzeit under sy-datum, sy-repid under text-012.
* write: /1 text-013, sy-mandt, 50 text-000.
  ULINE.
  FORMAT INTENSIFIED ON.
*-----------------------------------------------------------------------

START-OF-SELECTION.
  PERFORM GET_ORDERS_INFO USING P_ORDGR.
  PERFORM GET_COST_RANGE_NAME USING P_COSTGR.
  PERFORM GET_ALL_COST_EL.
  PERFORM ASSIGN_COST_GROUP_TO_ORDERS.
  PERFORM GET_TREE_STRUCTURE USING P_COSTGR.
  PERFORM ASSIGN_HIGHER_GROUP.                "mdemeest 2000/03/21
  PERFORM FINAL_STEP.

***********************************************************************
*                Subroutines used by program                          *

*---------------------------------------------------------------------*
*       FORM GET_ORDERS_INFO                                        *
*---------------------------------------------------------------------*
*   Function module used in this subroutine retrives order number
*   ranges for selected order group.  These ranges are used to get
*   all order numbers.  The next step is to obtain numbers of cost
*   elements assosiated with orders together with fiscal data.
*   Order numbers, cost elements and fiscal data are stored in ORDER_TAB
*   table.
*---------------------------------------------------------------------*
*  -->  SETNAME - name of the order group
*---------------------------------------------------------------------*
FORM GET_ORDERS_INFO USING SETNAME.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0103'          "mdemeest 2001/02/17
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = VALTAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.

  LOOP AT VALTAB.
    SELECT * FROM AUFK
      WHERE AUFNR GE VALTAB-FROM
      AND   AUFNR LE VALTAB-TO.
      SELECT * FROM COSS
         WHERE OBJNR = AUFK-OBJNR
         AND GJAHR = P_FYEAR.
        CHECK COSS-WRTTP = '01' OR COSS-WRTTP = '04'.

        IF COSS-WRTTP = '01'.
          CHECK COSS-VERSN = P_VERS.
          MOVE AUFK-AUFNR TO ORDER_TAB-ORDERNR.
          MOVE COSS-OBJNR TO ORDER_TAB-OBJNR.
          MOVE COSS-KSTAR TO ORDER_TAB-COSTEL.

          ADD COSS-WKG001 FROM P_START TO P_END GIVING ORDER_TAB-PPER.
          PERFORM FIND_ORDER_POSITION.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
        IF COSS-WRTTP = '04'.
          MOVE AUFK-AUFNR TO ORDER_TAB-ORDERNR.
          MOVE COSS-OBJNR TO ORDER_TAB-OBJNR.
          MOVE COSS-KSTAR TO ORDER_TAB-COSTEL.

          ADD COSS-WKG001 FROM P_START TO P_END GIVING ORDER_TAB-APER.
          PERFORM FIND_ORDER_POSITION.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
      ENDSELECT.

      SELECT * FROM COSP
          WHERE OBJNR = AUFK-OBJNR
          AND GJAHR = P_FYEAR.
        CHECK COSP-WRTTP = '01' OR COSP-WRTTP = '04'.
        IF COSP-WRTTP = '01'.
          CHECK COSP-VERSN = P_VERS.

          ADD COSP-WKG001 FROM P_START TO P_END GIVING ORDER_TAB-PPER.

          MOVE AUFK-AUFNR TO ORDER_TAB-ORDERNR.
          MOVE COSP-OBJNR TO ORDER_TAB-OBJNR.
          MOVE COSP-KSTAR TO ORDER_TAB-COSTEL.
          PERFORM FIND_ORDER_POSITION.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
        IF COSP-WRTTP = '04'.
          ADD COSP-WKG001 FROM P_START TO P_END GIVING ORDER_TAB-APER.


          MOVE AUFK-AUFNR TO ORDER_TAB-ORDERNR.
          MOVE COSP-OBJNR TO ORDER_TAB-OBJNR.
          MOVE COSP-KSTAR TO ORDER_TAB-COSTEL.
          PERFORM FIND_ORDER_POSITION.
          COLLECT ORDER_TAB.
          CLEAR ORDER_TAB.
        ENDIF.
      ENDSELECT.
    ENDSELECT.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_COST_RANGE_NAME                                      *
*---------------------------------------------------------------------*
*   This time the same (as before) function module is used to get cost
*   element ranges for a selected cost element group
*---------------------------------------------------------------------*
*  -->  SETNAME - name of cost element group                          *
*---------------------------------------------------------------------*
FORM GET_COST_RANGE_NAME USING SETNAME.
data:  setname_func(30) type c.                     "mdemeest 2001/02/19
  move p_search  to setname_func(4).                "mdemeest 2001/02/19
  move setname   to setname_func+4(26).             "mdemeest 2001/02/19
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0102'                  "mdemeest 2001/02/19
            SETNR         = SETNAME_FUNC
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = NAME_TAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_ALL_COST_EL                                          *
*---------------------------------------------------------------------*
*   This subroutine retrives all cost elements for a selected cost
*   element group.  All cost elements together with their description
*   are stored in ALLCOST_TAB table.
*---------------------------------------------------------------------*
FORM GET_ALL_COST_EL.
  LOOP AT NAME_TAB.
    SELECT * FROM CSKA
      WHERE KTOPL = P_SEARCH
*     where ktopl = 'COAT'
      AND KSTAR GE NAME_TAB-FROM
      AND KSTAR LE NAME_TAB-TO.
      SELECT SINGLE * FROM CSKU
        WHERE KTOPL = CSKA-KTOPL
        AND SPRAS = SY-LANGU
        AND KSTAR = CSKA-KSTAR.
      MOVE CSKA-KSTAR TO ALLCOST_TAB-COSTEL.
*      MOVE NAME_TAB-TITLE TO ALLCOST_TAB-GRNAME.
      perform get_group_name.
      move wa_setinfo-title  to allcost_tab-grname.
      MOVE NAME_TAB-SETNR TO ALLCOST_TAB-COSTGR.    "mdemeest 2000/02/29
      IF CSKU-LTEXT NE SPACE.
        MOVE CSKU-LTEXT TO ALLCOST_TAB-DESCRIPT.
      ELSE.
        MOVE CSKU-KTEXT TO ALLCOST_TAB-DESCRIPT.
      ENDIF.
      APPEND ALLCOST_TAB.
      CLEAR ALLCOST_TAB.

    ENDSELECT.
  ENDLOOP.
  SORT ALLCOST_TAB BY COSTEL.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ASSIGN_COST_GROUP_TO_ORDERS                              *
*---------------------------------------------------------------------*
*   Here ORDER_TAB table is checked aganist ALLCOST_TAB.  If cost
*   element from ORDER_TAB is not listed in ALLCOST_TAB, the entry is
*   deleted form ORDER_TAB.  If cost element from ORDER_TAB does exist
*   in ALLCOST_TAB, the description of cost element and the name of
*   cost group is entered into ORDER_TAB.
*---------------------------------------------------------------------*
FORM ASSIGN_COST_GROUP_TO_ORDERS.

  DATA: L_INDEX   LIKE SY-SUBRC.
  LOOP AT ORDER_TAB.
    MOVE SY-TABIX TO L_INDEX.
    READ TABLE ALLCOST_TAB WITH KEY ORDER_TAB-COSTEL BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE ALLCOST_TAB-GRNAME TO ORDER_TAB-GRNAME.
      MOVE ALLCOST_TAB-DESCRIPT TO ORDER_TAB-DESCRIPT.
      MOVE ALLCOST_TAB-COSTGR   TO ORDER_TAB-COSTGR.       "2000/03/01
      MODIFY ORDER_TAB INDEX L_INDEX.
    ELSE.
      DELETE ORDER_TAB INDEX L_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FINAL_STEP                                               *
*---------------------------------------------------------------------*
*  This subroutine displayes info from ORDER_TAB in the form of the
*  requested report.
*---------------------------------------------------------------------*
FORM FINAL_STEP.


  DATA:  TEMP(12).
  DATA:  DESCRIPT   LIKE ORDER_TAB-DESCRIPT.
  DATA:  GRNAME     LIKE ORDER_TAB-DESCRIPT.
  DATA:  DESC_OR    LIKE ORDER_TAB-DESCRIPT.
  DATA:  ORDERNR    LIKE ORDER_TAB-ORDERNR.


*  sort order_tab by ordernr itemno costgr1 costgr costel.   "2000/03/01
*   sort order_tab by ordernr itema itemno costel.  "2000/03/01
*    sort order_tab by itema  ordernr itemno costel.  "2000/04/27
    SORT ORDER_TAB BY ITEMA ORDERNR ITEMNO  COSTGR1 ITEMNOA
                                            COSTGR COSTEL.  "2000/05/05

  LOOP AT ORDER_TAB.
    CLEAR ORDER_TAB-ITEMNO.
    MODIFY ORDER_TAB.
  ENDLOOP.

  CONCATENATE '0H' P_ORDGR INTO SETNR.
    PERFORM GET_DESCRIPTION.
  TRANSLATE T800Y-TITLE TO UPPER CASE.
  WRITE: / TEXT-001, P_ORDGR, T800Y-TITLE.
  SKIP 1.

  LOOP AT ORDER_TAB.

    MOVE ORDER_TAB-DESCRIPT TO DESCRIPT.
    MOVE ORDER_TAB-GRNAME   TO GRNAME.
    MOVE ORDER_TAB-COSTGR1  TO COSTGR1.
    MOVE ORDER_TAB-ORDERNR  TO ORDERNR.

    AT NEW ORDERNR.
      SELECT SINGLE * FROM AUFK
        WHERE AUFNR EQ ORDERNR.
      FORMAT INTENSIFIED OFF.
      WRITE: /2 ORDERNR, AUFK-KTEXT.
      FORMAT INTENSIFIED ON.
      PERFORM PRINT_HEADINGS.
      FORMAT RESET.
    ENDAT.

    AT END OF COSTEL.
       SUM.
       DELTA1 = ORDER_TAB-APER - ORDER_TAB-PPER.


    WRITE: /10 ORDER_TAB-COSTEL, DESCRIPT.
    PERFORM PRINT_VERT.
    IF ORDER_TAB-APER NE 0.
      DELTA2 = DELTA1 * 100 / ORDER_TAB-APER.
    ENDIF.
    WRITE: 69(15) ORDER_TAB-APER,
          101(15) DELTA1,
          117(15) DELTA2.
    IF ORDER_TAB-PPER <> 0.                  "write non-zero plans only
       WRITE: 85(15) ORDER_TAB-PPER.
    ENDIF.
    ENDAT.

     AT END OF COSTGR.
       PERFORM PRINT_VERT.
       WRITE: /6(132) SY-ULINE.
       SUM.
       DELTA1 = ORDER_TAB-APER - ORDER_TAB-PPER.

    WRITE: /30 GRNAME COLOR COL_NEGATIVE.
    IF ORDER_TAB-APER NE 0.
      DELTA2 = DELTA1 * 100 / ORDER_TAB-APER.
    ENDIF.
    WRITE: 69(15) ORDER_TAB-APER COLOR COL_NEGATIVE,
           85(15) ORDER_TAB-PPER COLOR COL_NEGATIVE,
          101(15) DELTA1         COLOR COL_NEGATIVE,
          117(15) DELTA2         COLOR COL_NEGATIVE.
    FORMAT RESET.
    PERFORM PRINT_VERT.
    WRITE: /6(132) SY-ULINE.
    ENDAT.

    AT END OF COSTGR1.
       SUM.
       DELTA1 = ORDER_TAB-APER - ORDER_TAB-PPER.

*   concatenate '0H' order_tab-costgr1 into setnr.
    CONCATENATE '0H' COSTGR1+8(10) INTO SETNR.
    PERFORM GET_DESCRIPTION.


    WRITE: /25  T800Y-TITLE COLOR COL_POSITIVE.
    PERFORM PRINT_VERT.
    IF ORDER_TAB-APER NE 0.
      DELTA2 = DELTA1 * 100 / ORDER_TAB-APER.
    ENDIF.
    WRITE: 69(15) ORDER_TAB-APER COLOR COL_POSITIVE,
           85(15) ORDER_TAB-PPER COLOR COL_POSITIVE,
          101(15) DELTA1         COLOR COL_POSITIVE,
          117(15) DELTA2         COLOR COL_POSITIVE.
    WRITE: /6(132) SY-ULINE.
    ENDAT.

    AT END OF ORDERNR.
       SUM.
       DELTA1 = ORDER_TAB-APER - ORDER_TAB-PPER.


    FORMAT COLOR COL_GROUP.
    WRITE: /20  AUFK-KTEXT.
    PERFORM PRINT_VERT.
    IF ORDER_TAB-APER NE 0.
      DELTA2 = DELTA1 * 100 / ORDER_TAB-APER.
    ENDIF.
    WRITE: 69(15) ORDER_TAB-APER,
           85(15) ORDER_TAB-PPER,
          101(15) DELTA1,
          117(15) DELTA2.
    WRITE: /6 SY-ULINE(132).
    WRITE: /.
    FORMAT RESET.
    ENDAT.

    AT LAST.
       SUM.
       DELTA1 = ORDER_TAB-APER - ORDER_TAB-PPER.

    SKIP 1.
    WRITE: /6 SY-ULINE(132).
    FORMAT COLOR COL_TOTAL.
    WRITE: /20  TEXT-010.
    PERFORM PRINT_VERT.
    IF ORDER_TAB-APER NE 0.
      DELTA2 = DELTA1 * 100 / ORDER_TAB-APER.
    ENDIF.
    WRITE: 69(15) ORDER_TAB-APER,
           85(15) ORDER_TAB-PPER,
          101(15) DELTA1,
          117(15) DELTA2.
    WRITE: /6 SY-ULINE(132).
    WRITE: /.
    FORMAT RESET.
    ENDAT.
ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRINT_HEADINGS                                           *
*---------------------------------------------------------------------*
*   This subroutine prints columns headings.
*---------------------------------------------------------------------*
FORM PRINT_HEADINGS.
  WRITE: /6(127) SY-ULINE.
  FORMAT INTENSIFIED OFF.
  WRITE: /7(125) TEXT-002," color col_normal on,
          21(51) TEXT-003," color col_normal on,
          74 TEXT-004," color col_normal on,
          91 TEXT-005," color col_normal on,
          106 TEXT-006," color col_normal on,
          122 TEXT-007." color col_normal on.
  PERFORM PRINT_VERT.
  WRITE: /6(127) SY-ULINE.
  FORMAT RESET.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRINT_VERT                                               *
*---------------------------------------------------------------------*
*   This is used to print vertical lines.
*---------------------------------------------------------------------*
FORM PRINT_VERT.
  WRITE: 6 SY-VLINE, 19 SY-VLINE, 68 SY-VLINE, 84 SY-VLINE,
         100 SY-VLINE, 116 SY-VLINE, 132 SY-VLINE.
ENDFORM.

*------------------------ ASSIGN_HIGHER_GROUP   ------------------------
*   Assigns a hierarchical number to maintain proper order for printing
*-----------------------------------------------------------------------
FORM ASSIGN_HIGHER_GROUP.
DATA:  SETNAME  LIKE RGSBS-SETNR.

DATA:  BEGIN OF COSTGR_TAB OCCURS 10.
         INCLUDE STRUCTURE SETLIST.
DATA:  END OF COSTGR_TAB.

  LOOP AT ORDER_TAB.
    MOVE ORDER_TAB-COSTGR+8(10) TO SETNAME.
    CALL FUNCTION 'G_SET_GET_SUPERSETS'
        EXPORTING
             CLIENT        = SY-MANDT
             SETCLASS      = '0103'                "mdemeest 2001/02/19
             SETNAME        = SETNAME
             TABNAME        = 'CCSS'
        TABLES
             SUPERSETS     = COSTGR_TAB.

* A cost element group can be linked into more than 1 tree.
* This loop determines the correct tree.
      LOOP AT COSTGR_TAB.
         LOOP AT KSTAR_STRUCTURE.
         IF COSTGR_TAB-SETNAME = KSTAR_STRUCTURE-SETID.
            MOVE COSTGR_TAB-SETNAME TO ORDER_TAB-COSTGR1.
            MODIFY ORDER_TAB.
         ENDIF.
         ENDLOOP.
      ENDLOOP.
   ENDLOOP.

* To sort according to cost element group, assign index of item number
* to order group.
LOOP AT KSTAR_STRUCTURE.
  CTR = CTR + 1.

  IF KSTAR_STRUCTURE-TYPE = 'B'.
     LOOP AT ORDER_TAB.
        IF KSTAR_STRUCTURE-SETID = ORDER_TAB-COSTGR.
           MOVE CTR TO: ORDER_TAB-ITEMNO,  ORDER_TAB-ITEMNOA.
           MODIFY ORDER_TAB.
        ENDIF.
     ENDLOOP.
  ELSE.
*     loop at order_tab.
*        if kstar_structure-setid+2(10) = order_tab-costgr1.
*           move ctr to order_tab-itema.
*           modify order_tab.
*        endif.
*     endloop.
  ENDIF.
ENDLOOP.

ENDFORM.

FORM FIND_ORDER_POSITION.
  MOVE 0 TO CTR.
  LOOP AT VALTAB.
      ADD 1 TO CTR.
      IF ORDER_TAB-ORDERNR BETWEEN VALTAB-FROM AND VALTAB-TO.
         MOVE CTR TO ORDER_TAB-ITEMA.
      ENDIF.
  ENDLOOP.
ENDFORM.

FORM GET_DESCRIPTION.

  SELECT SINGLE * FROM T800Y
     WHERE LANGU = SY-LANGU
       AND TAB   = 'CCSS'
       AND SETNR = SETNR.
ENDFORM.

FORM GET_TREE_STRUCTURE USING P_COSTGR.
DATA: SETID  LIKE SETHIER-SETID.
  CONCATENATE '0H' P_COSTGR INTO SETID.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
       EXPORTING
            FIELDNAME     = 'KSTAR'            "Optional
            LANGU         = SY-LANGU           "Optional
            SETID         = SETID              "Mandatory  ie 'OHDO-OM'
            TABNAME       = 'CCSS '
       TABLES
            SET_HIERARCHY = KSTAR_STRUCTURE
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

form get_group_name.
data: setid_func(30)  type c.                      "mdemeest 2001/02/19
  move p_search             to setid_func(4).      "mdemeest 2001/02/19
  move name_tab-setnr+8(12) to setid_func+4(26).   "mdemeest 2001/02/19
* move name_tab-setnr+8(12) to setid.              "mdemeest 2001/02/19
  call function 'G_SET_GET_INFO'
      exporting
          class         = '0102'                   "mdemeest 2001/02/19
          setname       = setid_func
          table         = 'CCSS'
          fieldname     = 'KSTAR'
          client        = sy-mandt
      importing
          info          = wa_setinfo
      exceptions
          set_not_found = 1.
endform.

















