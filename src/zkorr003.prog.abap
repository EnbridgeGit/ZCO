REPORT ZKORR003 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.

************************************************************************
*  Author:      M DeMeester
*  Description: Report listing the Gross O&M Cost Analysis by District
*               4 sections: O&M,  Capitalization Expenses,
*                           Direct Capital Expenses & the TOTAL
*               Each group of columns contains the Actual, Plan,
*               variance and % variance
************************************************************************
* 99/03/17 mdemeest #629 Original request
************************************************************************
TABLES: BKPF, T800S, T800Y,
        T247,                             "Calendar month names
        COSP,                             "External Postings
        COSS.                             "Internal Postings

DATA:  SETID       LIKE SETHIER-SETID.
DATA:  SETNAME     LIKE RGSBS-SETNR.
DATA:  CLASS(2)    TYPE C.
DATA:  ACTUALGR    LIKE COSP-WKG001.
DATA:  PLANGR      LIKE COSP-WKG001.
DATA:  VAROM       LIKE COSP-WKG001.
DATA:  VAROH       LIKE COSP-WKG001.
DATA:  VARDC       LIKE COSP-WKG001.
DATA:  VARGR       LIKE COSP-WKG001.
DATA:      VALUE       LIKE COSP-WKG001.
*data:      typecode(2) type c.
*data:      keyvalue    like t800s-setnr.
DATA:      VAROMP      LIKE COSP-WKG001.                "O&M Variance %
DATA:      VAROHP      LIKE COSP-WKG001.                "O&H Variance %
DATA:      VARDCP      LIKE COSP-WKG001.                "DC Variance %
DATA:      VARGRP      LIKE COSP-WKG001.                "Total Var  %
DATA:      DATERANGE(30) TYPE C.

DATA:  BEGIN OF KOSTL_ENTRIES  OCCURS 100,
        LEVEL0     LIKE SETHIER-SETID,
        LEVEL1     LIKE SETHIER-SETID,
        LEVEL2     LIKE SETHIER-SETID,
        LEVEL3     LIKE SETHIER-SETID,
        LEVEL4     LIKE SETHIER-SETID,
        ACTUALOM   LIKE COSP-WKG001,
        PLANOM     LIKE COSP-WKG001,
        ACTUALOH   LIKE COSP-WKG001,
        PLANOH     LIKE COSP-WKG001,
        ACTUALDC   LIKE COSP-WKG001,
        PLANDC     LIKE COSP-WKG001.
DATA:  END OF KOSTL_ENTRIES.

DATA:      BEGIN OF NAME_TAB OCCURS 10.
              INCLUDE STRUCTURE RGSB4.
DATA:      END OF NAME_TAB.

DATA:      BEGIN OF NAME_TABLE OCCURS 100,
             TITLE      LIKE RGSB4-TITLE,
             SETNR      LIKE RGSB4-SETNR,
             FROM       LIKE RGSB4-FROM,
             TO         LIKE RGSB4-TO,
             GROUP(2)   TYPE C,                      "99/03/30
             ACTUALOM   LIKE COSP-WKG001,
             PLANOM     LIKE COSP-WKG001,
             ACTUALOH   LIKE COSP-WKG001,
             PLANOH     LIKE COSP-WKG001,
             ACTUALDC   LIKE COSP-WKG001,
             PLANDC     LIKE COSP-WKG001.
DATA:      END OF NAME_TABLE.
*-----------------------------------------------------------------------



DATA:      BEGIN OF KOSTL_STRUCTURE OCCURS 100.
             INCLUDE STRUCTURE SETHIER.
DATA:      END OF KOSTL_STRUCTURE.


data:      grtitle like rgsbs-title.


*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-027.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_BUKRS(4)                 OBLIGATORY DEFAULT 'UGL',
            P_GJAHR  LIKE COSP-GJAHR   OBLIGATORY DEFAULT SY-DATUM(4).
SELECT-OPTIONS:
            S_MTH    FOR BKPF-MONAT    OBLIGATORY DEFAULT '01' TO '12'.
PARAMETERS: P_VERS   LIKE TKVST-VERSI  OBLIGATORY DEFAULT '000'.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-028.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            S_KOSTOM FOR T800S-SETNR NO INTERVALS OBLIGATORY,
            S_KOSTOH FOR T800S-SETNR NO INTERVALS OBLIGATORY,
            S_KOSTDC FOR T800S-SETNR NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-031.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            S_KSTROM FOR COSP-KSTAR OBLIGATORY,
            S_KSTROH FOR COSP-KSTAR OBLIGATORY,
            S_KSTRDC FOR COSP-KSTAR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX2.
*-----------------------------------------------------------------------


*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,                "Title
      110 TEXT-001,
      220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-002 UNDER TEXT-001,                           "Title
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
WRITE: / TEXT-VER UNDER TEXT-RPT, P_VERS,                   "Version
          DATERANGE UNDER TEXT-001, P_GJAHR.                     "Period
*         text-003 under text-001,  p_gjahr.                 "Period
WRITE: / TEXT-026 UNDER TEXT-RPT, P_BUKRS.                  "Company
ULINE.
PERFORM WRITE_VERT.
WRITE: 51 TEXT-004, 102 TEXT-009, 153 TEXT-014, 204 TEXT-019.
ULINE.
PERFORM WRITE_VERT.
PERFORM WRITE_VERT.
WRITE: 51 TEXT-005,  65 TEXT-006,  79 TEXT-007,  93 TEXT-008,
      102 TEXT-010, 116 TEXT-011, 130 TEXT-012, 144 TEXT-013,
      153 TEXT-015, 167 TEXT-016, 181 TEXT-017, 195 TEXT-018,
      204 TEXT-020, 218 TEXT-021, 232 TEXT-022, 246 TEXT-023.
ULINE.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
SELECT SINGLE * FROM T247
  WHERE SPRAS = SY-LANGU
    AND MNR   = S_MTH+3(2).
DATERANGE = TEXT-003.
DATERANGE+4(10) = T247-LTX.

IF ( S_MTH+5(2) IS INITIAL OR S_MTH+5(2) = '00' ).
   DATERANGE+14(15) = SPACE.
ELSE.
 SELECT SINGLE * FROM T247
  WHERE SPRAS = SY-LANGU
    AND MNR   = S_MTH+5(2).
  DATERANGE+19(10) = T247-LTX.
ENDIF.

START-OF-SELECTION.
*-----------------------------------------------------------------------
* Expand structure into all its levels based on variant
*-----------------------------------------------------------------------
LOOP AT S_KOSTOM.                                  "O&M Structure
  CONCATENATE '0H' S_KOSTOM+3(12) INTO SETID.
  PERFORM GET_TREE_STRUCTURE.
  PERFORM POPULATE_KOSTL_STRUCTURE USING 'OM'.
ENDLOOP.

LOOP AT S_KOSTOH.                                  "O&H Structure
  CONCATENATE '0H' S_KOSTOH+3(12) INTO SETID.
  PERFORM GET_TREE_STRUCTURE.
  PERFORM POPULATE_KOSTL_STRUCTURE USING 'OH'.
ENDLOOP.

LOOP AT S_KOSTDC.                                 "DC Structure
  CONCATENATE '0H' S_KOSTDC+3(12) INTO SETID.
  PERFORM GET_TREE_STRUCTURE.
  PERFORM POPULATE_KOSTL_STRUCTURE USING 'DC'.
ENDLOOP.

SELECT * FROM COSP                                "Get external postings
    WHERE OBJNR LIKE 'KS10%'
      AND GJAHR = P_GJAHR
      AND WRTTP IN ('01', '04')
      AND VERSN IN ('000', P_VERS).
   PERFORM CALCULATE_AMT_COSP.
   perform check_centres_cosp.
ENDSELECT.

SELECT * FROM COSS                                "Get internal postings
    WHERE OBJNR LIKE 'KS10%'
      AND GJAHR = P_GJAHR
      AND WRTTP IN ('01', '04')
      AND VERSN IN ('000', P_VERS).
   PERFORM CALCULATE_AMT_COSS.
   PERFORM CHECK_CENTRES_COSS.
ENDSELECT.


* Moves individual amounts from NAME_TABLE to KOSTL_ENTRIES so that
* info can be displayed with the various levels.
LOOP AT NAME_TABLE.
  LOOP AT KOSTL_ENTRIES.
    IF NAME_TABLE-GROUP = 'OM'.
*      if ( name_table-setnr+2(10) = kostl_entries-level2 ) or
       IF ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL0 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL1 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL2 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL3 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL4 ).
          KOSTL_ENTRIES-ACTUALOM =  KOSTL_ENTRIES-ACTUALOM
                                 +  NAME_TABLE-ACTUALOM.
          KOSTL_ENTRIES-PLANOM   =  KOSTL_ENTRIES-PLANOM
                                 +  NAME_TABLE-PLANOM.
          MODIFY KOSTL_ENTRIES.
       ENDIF.
    ELSEIF NAME_TABLE-GROUP = 'OH'.
       IF ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL0 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL1 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL2 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL3 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL4 ).
          KOSTL_ENTRIES-ACTUALOH = KOSTL_ENTRIES-ACTUALOH
                                 + NAME_TABLE-ACTUALOH.
          KOSTL_ENTRIES-PLANOH   = KOSTL_ENTRIES-PLANOH
                                 + NAME_TABLE-PLANOH.
          MODIFY KOSTL_ENTRIES.
       ENDIF.
    ELSEIF NAME_TABLE-GROUP = 'DC'.
       IF ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL0 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL1 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL2 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL3 ) OR
          ( NAME_TABLE-SETNR+2(10) = KOSTL_ENTRIES-LEVEL4 ).
          KOSTL_ENTRIES-ACTUALDC = KOSTL_ENTRIES-ACTUALDC
                                 + NAME_TABLE-ACTUALDC.
          KOSTL_ENTRIES-PLANDC   = KOSTL_ENTRIES-PLANDC
                                 + NAME_TABLE-PLANDC.
          MODIFY KOSTL_ENTRIES.
       ENDIF.
              ENDIF.
  ENDLOOP.
ENDLOOP.

LOOP AT KOSTL_ENTRIES.
  REPLACE 'OM' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL0,
                                KOSTL_ENTRIES-LEVEL1,
                                KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3.
  REPLACE 'OH' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL0,
                                KOSTL_ENTRIES-LEVEL1,
                                KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3.
  REPLACE 'DC' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL0,
                                KOSTL_ENTRIES-LEVEL1,
                                KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3.
  TRANSLATE KOSTL_ENTRIES-LEVEL0 USING '- '.
  TRANSLATE KOSTL_ENTRIES-LEVEL1 USING '- '.
  TRANSLATE KOSTL_ENTRIES-LEVEL2 USING '- '.
  TRANSLATE KOSTL_ENTRIES-LEVEL3 USING '- '.
  MODIFY KOSTL_ENTRIES.
ENDLOOP.

SORT KOSTL_ENTRIES BY LEVEL1 LEVEL2 LEVEL3.
PERFORM DISPLAY_KOSTL_ENTRIES.



************************************************************************
*                Subroutines used by program
************************************************************************
*--------------------  PRINT_VERT  -------------------------------------
*  Vertical Lines
*-----------------------------------------------------------------------
FORM WRITE_VERT.
 WRITE: /1 SY-VLINE,
        50 SY-VLINE,  64 SY-VLINE,  78 SY-VLINE,  92 SY-VLINE,
       101 SY-VLINE, 115 SY-VLINE, 129 SY-VLINE, 143 SY-VLINE,
       152 SY-VLINE, 166 SY-VLINE, 180 SY-VLINE, 194 SY-VLINE,
       203 SY-VLINE, 217 SY-VLINE, 231 SY-VLINE, 245 SY-VLINE,
       254 SY-VLINE.
ENDFORM.

*form get_group_name using keyvalue.
*
*data:  field   like t800y-setnr.
*  clear grtitle.
*  concatenate '0H' keyvalue into field.
*  select single * from t800y
*      where langu = sy-langu
*        and tab   = 'CCSS'
*        and setnr = field.
*  if sy-subrc = '0'.
*     grtitle = t800y-title.
*     replace '- DC'  with '    '  into grtitle.
*     replace '- OH'  with '    '  into grtitle.
*     replace '- O&M' with '     ' into grtitle.
*  endif.
*
**endform.
*-------------------  GET_TREE_STRUCTURE  ------------------------------
* Routine takes all cost element groups and expands them to their lowest
* level
*-----------------------------------------------------------------------
FORM GET_TREE_STRUCTURE.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
       EXPORTING
            FIELDNAME     = 'KOSTL'            "Optional
            LANGU         = SY-LANGU           "Optional
            SETID         = SETID              "Mandatory  ie 'OHDO-OM'
            TABNAME       = 'CCSS '
       TABLES
            SET_HIERARCHY = KOSTL_STRUCTURE
       EXCEPTIONS
            SET_NOT_FOUND = 1.
ENDFORM.

FORM POPULATE_KOSTL_STRUCTURE USING CLASS.
 LOOP AT KOSTL_STRUCTURE.
*   replace 'DC'  with '    '  into kostl_structure-setid.
*   replace 'OH'  with '    '  into kostl_structure-setid.
*   replace 'OM' with '     '  into kostl_structure-setid.
*   replace '-'   with ' '     into kostl_structure-setid.
   CASE KOSTL_STRUCTURE-LEVEL.
     WHEN '0'.
       CLEAR KOSTL_ENTRIES.
       KOSTL_ENTRIES-LEVEL0 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
     WHEN '1'.
       CLEAR: KOSTL_ENTRIES-LEVEL2, KOSTL_ENTRIES-LEVEL3,
              KOSTL_ENTRIES-LEVEL4.
       KOSTL_ENTRIES-LEVEL1 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
     WHEN '2'.
       CLEAR: KOSTL_ENTRIES-LEVEL3, KOSTL_ENTRIES-LEVEL4.
       KOSTL_ENTRIES-LEVEL2 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
     WHEN '3'.
       CLEAR: KOSTL_ENTRIES-LEVEL4.
       KOSTL_ENTRIES-LEVEL3 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
     WHEN '4'.
       KOSTL_ENTRIES-LEVEL4 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
     WHEN OTHERS.
       KOSTL_ENTRIES-LEVEL4 = KOSTL_STRUCTURE-SETID+2(32).
       PERFORM CHECK-IF-BASIC USING CLASS.
   ENDCASE.
 ENDLOOP.

ENDFORM.

*-------------------- CHECK-IF-BASIC -----------------------------------
* Routine creates internal table with the various levels
*-----------------------------------------------------------------------
FORM CHECK-IF-BASIC USING CLASS.
 IF KOSTL_STRUCTURE-TYPE = 'B'.
    COLLECT KOSTL_ENTRIES.
    PERFORM GET_COST_CENTRES.                        "99/03/30
    LOOP AT NAME_TAB.
      NAME_TABLE-TITLE   = NAME_TAB-TITLE.
      NAME_TABLE-SETNR   = NAME_TAB-SETNR.
      NAME_TABLE-FROM    = NAME_TAB-FROM.
      NAME_TABLE-TO      = NAME_TAB-TO.
      NAME_TABLE-GROUP   = CLASS.
      APPEND NAME_TABLE.
    ENDLOOP.
 ENDIF.
ENDFORM.

*-----------------------  GET_COST_CENTRES  ----------------------------
* Using function module, get all cost centre ranges for the cost
* centre group   --> SETNAME = name of cost element group
*---------------------------------------------------------------------*
FORM GET_COST_CENTRES.
  IF KOSTL_ENTRIES-LEVEL4 <> SPACE.
     SETNAME = KOSTL_ENTRIES-LEVEL4.
  ELSEIF KOSTL_ENTRIES-LEVEL3 <> SPACE.
     SETNAME = KOSTL_ENTRIES-LEVEL3.
  ELSEIF KOSTL_ENTRIES-LEVEL2 <> SPACE.
     SETNAME = KOSTL_ENTRIES-LEVEL2.
  ELSEIF KOSTL_ENTRIES-LEVEL1 <> SPACE.
     SETNAME = KOSTL_ENTRIES-LEVEL1.
  ELSEIF KOSTL_ENTRIES-LEVEL0 <> SPACE.
     SETNAME = KOSTL_ENTRIES-LEVEL0.
  ENDIF.

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

*------------------------ CHECK_CENTRES_COSP ---------------------------
* This routine releases the dollar value only if the entry from the
* table satisfies both the cost element group and cost centre group
* criteria.
*-----------------------------------------------------------------------
form  check_centres_cosp.
 LOOP AT NAME_TABLE.
   IF COSP-OBJNR+6(10) BETWEEN NAME_TABLE-FROM AND NAME_TABLE-TO.
      IF ( NAME_TABLE-GROUP = 'OM' AND COSP-KSTAR IN S_KSTROM ).
         IF COSP-WRTTP = '04' AND COSP-VERSN = '000'.
            NAME_TABLE-ACTUALOM = NAME_TABLE-ACTUALOM + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSP-WRTTP = '01' AND COSP-VERSN = P_VERS.
            NAME_TABLE-PLANOM   = NAME_TABLE-PLANOM   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ELSEIF ( NAME_TABLE-GROUP = 'OH' AND COSP-KSTAR IN S_KSTROH ).
         IF COSP-WRTTP = '04' AND COSP-VERSN = '000'.
            NAME_TABLE-ACTUALOH = NAME_TABLE-ACTUALOH + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSP-WRTTP = '01' AND COSP-VERSN = P_VERS.
            NAME_TABLE-PLANOH   = NAME_TABLE-PLANOH   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ELSEIF ( NAME_TABLE-GROUP = 'DC' AND COSP-KSTAR IN S_KSTRDC ).
         IF COSP-WRTTP = '04' AND COSP-VERSN = '000'.
            NAME_TABLE-ACTUALDC = NAME_TABLE-ACTUALDC + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSP-WRTTP = '01' AND COSP-VERSN = P_VERS.
            NAME_TABLE-PLANDC   = NAME_TABLE-PLANDC   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ENDIF. ENDIF.
 ENDLOOP.

ENDFORM.
*------------------------ CHECK_CENTRES_COSS ---------------------------
* This routine releases the dollar value only if the entry from the
* table satisfies both the cost element group and cost centre group
* criteria.
*-----------------------------------------------------------------------
FORM  CHECK_CENTRES_COSS.
 LOOP AT NAME_TABLE.
   IF COSS-OBJNR+6(10) BETWEEN NAME_TABLE-FROM AND NAME_TABLE-TO.
      IF ( NAME_TABLE-GROUP = 'OM' AND COSS-KSTAR IN S_KSTROM ).
         IF COSS-WRTTP = '04' AND COSS-VERSN = '000'.
            NAME_TABLE-ACTUALOM = NAME_TABLE-ACTUALOM + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSS-WRTTP = '01' AND COSS-VERSN = P_VERS.
            NAME_TABLE-PLANOM   = NAME_TABLE-PLANOM   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ELSEIF ( NAME_TABLE-GROUP = 'OH' AND COSS-KSTAR IN S_KSTROH ).
         IF COSS-WRTTP = '04' AND COSS-VERSN = '000'.
            NAME_TABLE-ACTUALOH = NAME_TABLE-ACTUALOH + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSS-WRTTP = '01' AND COSS-VERSN = P_VERS.
            NAME_TABLE-PLANOH   = NAME_TABLE-PLANOH   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ELSEIF ( NAME_TABLE-GROUP = 'DC' AND COSS-KSTAR IN S_KSTRDC ).
         IF COSS-WRTTP = '04' AND COSS-VERSN = '000'.
            NAME_TABLE-ACTUALDC = NAME_TABLE-ACTUALDC + VALUE.
            MODIFY NAME_TABLE.
          ELSEIF COSS-WRTTP = '01' AND COSS-VERSN = P_VERS.
            NAME_TABLE-PLANDC   = NAME_TABLE-PLANDC   + VALUE.
            MODIFY NAME_TABLE.
          ENDIF.
      ENDIF. ENDIF.
 ENDLOOP.

ENDFORM.


*------------------------- CALCULATE_AMT_COSP --------------------------
* Routine calculates total postings for the specified period
*-----------------------------------------------------------------------
FORM CALCULATE_AMT_COSP.
   ADD COSP-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING VALUE.
ENDFORM.

*------------------------- CALCULATE_AMT_COSS --------------------------
* Routine calculates total postings for the specified period
*-----------------------------------------------------------------------
FORM CALCULATE_AMT_COSS.
   ADD COSS-WKG001 FROM S_MTH+3(2) TO S_MTH+5(2) GIVING VALUE.
ENDFORM.
FORM DISPLAY_KOSTL_ENTRIES.
* remove the grouping (ie OM, OH, DC) from the level2, level3 entries
LOOP AT KOSTL_ENTRIES.
  REPLACE 'OM' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3,
                                KOSTL_ENTRIES-LEVEL4.
  REPLACE 'OH' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3,
                                KOSTL_ENTRIES-LEVEL4.
  REPLACE 'DC' WITH '   ' INTO: KOSTL_ENTRIES-LEVEL2,
                                KOSTL_ENTRIES-LEVEL3,
                                KOSTL_ENTRIES-LEVEL4.
  TRANSLATE KOSTL_ENTRIES-LEVEL2 USING '- '.
  TRANSLATE KOSTL_ENTRIES-LEVEL3 USING '- '.
  TRANSLATE KOSTL_ENTRIES-LEVEL4 USING '- '.
  MODIFY KOSTL_ENTRIES.
ENDLOOP.

SORT KOSTL_ENTRIES BY LEVEL1 LEVEL2 LEVEL3.

LOOP AT KOSTL_ENTRIES.
 AT END OF LEVEL4.
   SUM.
   IF KOSTL_ENTRIES-LEVEL4 <> SPACE.
      PERFORM WRITE_VERT.
      WRITE: 20 KOSTL_ENTRIES-LEVEL4(15).
      PERFORM WRITE_DETAIL.
*      perform write_vert.
*      write: 20 sy-uline(235).
   ENDIF.
 ENDAT.

 AT END OF LEVEL3.
   SUM.
   IF KOSTL_ENTRIES-LEVEL3 <> SPACE.
      PERFORM WRITE_VERT.
      WRITE: 15 KOSTL_ENTRIES-LEVEL3.
      PERFORM WRITE_DETAIL.
      PERFORM WRITE_VERT.
      WRITE: 15 SY-ULINE(240).
   ENDIF.
 ENDAT.

 AT END OF LEVEL2.
   SUM.
   IF KOSTL_ENTRIES-LEVEL2 <> SPACE.
      PERFORM WRITE_VERT.
      WRITE: 10 KOSTL_ENTRIES-LEVEL2.
      PERFORM WRITE_DETAIL.
   ENDIF.
 ENDAT.

 AT END OF LEVEL1.
   SUM.
   IF KOSTL_ENTRIES-LEVEL1 <> SPACE.
      ULINE.
      PERFORM WRITE_VERT.
      WRITE: 5 KOSTL_ENTRIES-LEVEL1.
      PERFORM WRITE_DETAIL.
      ULINE.
      PERFORM WRITE_VERT.
   ENDIF.
 ENDAT.

 AT END OF LEVEL0.
   SUM.
   ULINE.
   PERFORM WRITE_VERT.
   WRITE: 5 KOSTL_ENTRIES-LEVEL0.
   PERFORM WRITE_DETAIL.
   ULINE.
   PERFORM WRITE_VERT.
 ENDAT.

 AT LAST.                                     "Company Total
   SUM.
   ULINE.
   PERFORM WRITE_VERT.
   WRITE: 3 'TOTAL'.
   PERFORM WRITE_DETAIL.
   ULINE.
 ENDAT.
ENDLOOP.
ENDFORM.

FORM WRITE_DETAIL.
 COMPUTE ACTUALGR = KOSTL_ENTRIES-ACTUALOM
                  + KOSTL_ENTRIES-ACTUALOH
                  + KOSTL_ENTRIES-ACTUALDC.
 COMPUTE PLANGR   = KOSTL_ENTRIES-PLANOM
                  + KOSTL_ENTRIES-PLANOH
                  + KOSTL_ENTRIES-PLANDC.
 WRITE: (12) KOSTL_ENTRIES-ACTUALOM DECIMALS 0 UNDER TEXT-005,
        (12) KOSTL_ENTRIES-PLANOM   DECIMALS 0 UNDER TEXT-006,
        (12) KOSTL_ENTRIES-ACTUALOH DECIMALS 0 UNDER TEXT-010,
        (12) KOSTL_ENTRIES-PLANOH   DECIMALS 0 UNDER TEXT-011,
        (12) KOSTL_ENTRIES-ACTUALDC DECIMALS 0 UNDER TEXT-015,
        (12) KOSTL_ENTRIES-PLANDC   DECIMALS 0 UNDER TEXT-016,
         (12)             ACTUALGR DECIMALS 0 UNDER TEXT-020,
         (12)             PLANGR   DECIMALS 0 UNDER TEXT-021.

* Variances for O&M, OH, DC and Gross
 COMPUTE VAROM = KOSTL_ENTRIES-ACTUALOM - KOSTL_ENTRIES-PLANOM.
 COMPUTE VAROH = KOSTL_ENTRIES-ACTUALOH - KOSTL_ENTRIES-PLANOH.
 COMPUTE VARDC = KOSTL_ENTRIES-ACTUALDC - KOSTL_ENTRIES-PLANDC.
 COMPUTE VARGR = VAROM + VAROH + VARDC.
 WRITE: (12) VAROM DECIMALS 0 UNDER TEXT-007,
        (12) VAROH DECIMALS 0 UNDER TEXT-012,
        (12) VARDC DECIMALS 0 UNDER TEXT-017,
        (12) VARGR DECIMALS 0 UNDER TEXT-022.

* calculate variance percents & display them on report
 CLEAR: VAROMP, VAROHP, VARDCP, VARGRP.
 IF KOSTL_ENTRIES-PLANOM <> 0.
    COMPUTE VAROMP = VAROM  * 100 / KOSTL_ENTRIES-PLANOM.
 ENDIF.
 IF KOSTL_ENTRIES-PLANOH <> 0.
    COMPUTE VAROHP = VAROH * 100 / KOSTL_ENTRIES-PLANOH.
 ENDIF.
 IF KOSTL_ENTRIES-PLANDC <> 0.
    COMPUTE VARDCP = VARDC * 100 / KOSTL_ENTRIES-PLANDC.
 ENDIF.
 IF PLANGR <> 0.
    COMPUTE VARGRP = VARGR * 100 / PLANGR.
 ENDIF.

WRITE: (8) VAROMP  DECIMALS 2 UNDER TEXT-008,
       (8) VAROHP  DECIMALS 2 UNDER TEXT-013,
       (8) VARDCP  DECIMALS 2 UNDER TEXT-018,
       (8) VARGRP  DECIMALS 2 UNDER TEXT-023.
ENDFORM.
