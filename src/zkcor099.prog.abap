REPORT ZKCOR099 MESSAGE-ID ZK LINE-SIZE 132 LINE-COUNT 65
                                              NO STANDARD PAGE HEADING.
********************************************************************
*      Owner: Union Gas Ltd. - BIS                                 *
* Programmer: MFD                                                  *
*       Date: January 22nd, 1997                                   *
* Request ID:                                                      *
*                                                                  *
* The following program will generate a Cost Centre Group/Cost     *
* Centres/Cost Element Group/Cost Elements Report control breaking *
* in that order.                                                   *
********************************************************************
*                                          *
********************************************************************
TABLES: COSP,      "CO Object:  Cost Totals - External Postings
        COSS,      "CO Object:  Cost Totals - Internal Postings
        CSKB,      "Cost elements (data dependent on controlling area)
        CSKS,      "Cost center master
        CSKT,      "Cost center texts
        CSKU,      "Cost element texts
        T800Y,     "FI-SL Set Texts
        TKKB2,
        T800S,
        T001.      "Company Code

DATA:  ACTCST(11)  TYPE P DECIMALS 2,            "Actual Cost
       PLNCST      LIKE ACTCST,                  "Plan Cost
       ABSVAR(10)  TYPE P DECIMALS 2,            "Abs. var.
       VAR%(4)     TYPE P DECIMALS 2,            "Variance %
       ACTTOT(11)  TYPE P DECIMALS 2,            "Actual total
       PLNTOT      LIKE ACTTOT,                  "Plan total
       ABSTOT      LIKE ABSVAR,                  "Abs. var. total
       VARTOT      LIKE VAR%,                    "Variance total
       ACT_EL2     LIKE ACTCST,
       PLN_EL2     LIKE ACTCST,
       VAR_EL2     LIKE VAR%,
       ABS_EL2     LIKE ABSVAR,
       ACT_EL      LIKE ACTCST,
       PLN_EL      LIKE ACTCST,
       VAR_EL      LIKE VAR%,
       ABS_EL      LIKE ABSVAR,
       GRACT       LIKE ACTTOT,                  "Grand total actuals
       GRPLN       LIKE ACTTOT,                  "Grand total plan
       GRABS       LIKE ABSVAR,                  "Grand total abs. var.
       GRVAR       LIKE VAR%,                    "Grand total var. %
       LN_CNTR     TYPE I,                       "Line counter
       P_TO(2)     TYPE N value 12,
       P_FROM(2)   TYPE N value 01,
       MONTH_ACT   LIKE ABSVAR,
       MONTH_EL    LIKE ABSVAR,
       MONTH_EL2   LIKE ABSVAR,
       MONTHTOT    LIKE ABSVAR,
       MONTHGTOT   LIKE ABSVAR,
       LTEXT       LIKE CSKT-LTEXT,              "Cost centre text
       GRPTEXT     LIKE CSKT-LTEXT.              "Cost centre Group txt
DATA: BEGIN OF SETVAL OCCURS 1000.               "Table for centre group
        INCLUDE STRUCTURE RGSB4.
DATA: END OF SETVAL.

DATA: BEGIN OF SETVLUES OCCURS 1000.              "Table for element grp
        INCLUDE STRUCTURE RGSB4.
DATA: END OF SETVLUES.

DATA: BEGIN OF ITAB1 OCCURS 1000,                 "Main internal table
        KOSTL    LIKE CSKS-KOSTL,                 "Cost centre
        ELMGRP   LIKE RGSB4-TITLE,                "Element group1
        KTITLE   LIKE T800Y-TITLE,                "Element group text
        KTEXT    LIKE CSKU-KTEXT,                 "Cost centre text
        KSTARGRP LIKE RGSB4-TITLE,                "Element group2
        KSTAR    LIKE COSP-KSTAR,                 "Cost element
        CSTGRP   LIKE CSKS-KHINR,                 "Centre group
        OBJNR    LIKE COSP-OBJNR,                 "Object number
        WRTTP    LIKE COSP-WRTTP,                 "Value type
        VERSN    LIKE COSP-VERSN,                 "Plan version
        ACOST1   LIKE COSP-WKG001,                "Actual cost - Jan
        ACOST2   LIKE COSP-WKG002,                "Actual cost - Feb
        ACOST3   LIKE COSP-WKG003,                "Actual cost - Mar
        ACOST4   LIKE COSP-WKG004,                "Actual cost - Apr
        ACOST5   LIKE COSP-WKG005,                "Actual cost - May
        ACOST6   LIKE COSP-WKG006,                "Actual cost - Jun
        ACOST7   LIKE COSP-WKG007,                "Actual cost - Jul
        ACOST8   LIKE COSP-WKG008,                "Actual cost - Aug
        ACOST9   LIKE COSP-WKG009,                "Actual cost - Sep
        ACOST10  LIKE COSP-WKG010,                "Actual cost - Oct
        ACOST11  LIKE COSP-WKG011,                "Actual cost - Nov
        ACOST12  LIKE COSP-WKG012,                "Actual cost - Dec
        PCOST1   LIKE COSP-WKG001,                "Plan cost - Jan
        PCOST2   LIKE COSP-WKG002,                "Plan cost - Feb
        PCOST3   LIKE COSP-WKG003,                "Plan cost - Mar
        PCOST4   LIKE COSP-WKG004,                "Plan cost - Apr
        PCOST5   LIKE COSP-WKG005,                "Plan cost - May
        PCOST6   LIKE COSP-WKG006,                "Plan cost - Jun
        PCOST7   LIKE COSP-WKG007,                "Plan cost - Jul
        PCOST8   LIKE COSP-WKG008,                "Plan cost - Aug
        PCOST9   LIKE COSP-WKG009,                "Plan cost - Sep
        PCOST10  LIKE COSP-WKG010,                "Plan cost - Oct
        PCOST11  LIKE COSP-WKG011,                "Plan cost - Nov
        PCOST12  LIKE COSP-WKG012,                "Plan cost - Dec
       END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 1000,                 "Table for element grp
        ELMGRP   LIKE RGSB4-TITLE,                "Element group title
        KSTARGRP LIKE RGSB4-SETNR,                "Element group
        KTITLE   LIKE T800Y-TITLE,                "Element group text
        KSTAR    LIKE CSKB-KSTAR,                 "Cost element
      END OF ITAB2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 10(63) TEXT-002 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME TITLE TEXT-004.

PARAMETERS:  P_CTRGRP LIKE RGSB4-SETNR OBLIGATORY MODIF ID ABC,
             P_ELMGRP LIKE RGSB4-SETNR OBLIGATORY MODIF ID ABC,
             P_TOTALS TYPE C MODIF ID ABC.

SELECTION-SCREEN END OF BLOCK INTRO.

SELECTION-SCREEN BEGIN OF BLOCK PARAM WITH FRAME TITLE TEXT-005.
PARAMETERS: P_YEAR   LIKE COSP-GJAHR DEFAULT SY-DATUM(4) MODIF ID ABC,
**            P_FROM(2) TYPE N DEFAULT '01' MODIF ID ABC,
              P_MONTH(2) TYPE N MODIF ID ABC,
**            P_TO(2)   TYPE N DEFAULT '12' MODIF ID ABC,
            P_VERSN  LIKE COSP-VERSN DEFAULT '000' MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK PARAM.
*
* Company specific parameters
SELECTION-SCREEN BEGIN OF BLOCK COMPANY WITH FRAME TITLE TEXT-021.
PARAMETERS: P_BUKRS    LIKE CSKS-BUKRS MODIF ID ABC, "Company Code
            P_KOKRS    LIKE CSKB-KOKRS MODIF ID ABC, "Controlling Area
            P_KTOPL    LIKE CSKU-KTOPL MODIF ID ABC.
                                                  "Chart of Accounts
SELECTION-SCREEN END OF BLOCK COMPANY.

************************************************************************
* Screen appearance *
AT SELECTION-SCREEN OUTPUT.
  IF SY-MANDT+2(1) = '0'.             "Union Gas
      MOVE 'UGL'          TO P_BUKRS.
      MOVE '10'           TO P_KOKRS.
      MOVE 'COAT'         TO P_KTOPL.
      P_TOTALS = 'N'.
"omning
  ELSE.
  IF SY-MANDT+2(1) = '1'.             "Union Energy
     MOVE 'UEC'           TO P_BUKRS.
     MOVE 'UE'            TO P_KOKRS.
     MOVE 'ACCT'          TO P_KTOPL.
     P_ELMGRP = 'ALL-KSTAR'.                                     "omning
  ENDIF.  ENDIF.

  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED  = '1'.
    MODIFY SCREEN.
  ENDLOOP.

* Pull down for COST CENTRE GROUPS. *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CTRGRP.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  O_SEARCHFLD = P_KOKRS.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0H'
            FIELD_NAME         = 'KOSTL'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*' "o_searchfld
       IMPORTING
            SET_NAME           = P_CTRGRP
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    P_CTRGRP = O_TMPSET.
  ENDIF.

* Pull down for COST ELEMENT GROUPS. *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ELMGRP.
DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  O_SEARCHFLD = P_KTOPL.


CALL FUNCTION 'K_GROUP_SELECT'
     EXPORTING
          CLASS         = '0H'
          FIELD_NAME    = 'KSTAR'
          SEARCHFLD     = O_SEARCHFLD
    IMPORTING
          SET_NAME      = P_ELMGRP
     EXCEPTIONS
           NO_SET_PICKED = 1.

 IF SY-SUBRC = 1.
   P_ELMGRP = O_TMPSET.
 ENDIF.

************************************************************************

START-OF-SELECTION.
* housekeeping
  SELECT SINGLE * FROM T001           "Company Name
     WHERE BUKRS = P_BUKRS.

  PERFORM INITIALIZE.
  PERFORM GET_COSTGROUP.
  PERFORM GET_ELEMENT_GRP.
  PERFORM PROC_COSTGROUP.
  PERFORM WRT_HDG.
  PERFORM PROC_MAINTAB.
END-OF-SELECTION.

* Routine to initialize internal tables and line counter.              *
FORM INITIALIZE.
  REFRESH: ITAB1, ITAB2, SETVAL, SETVLUES.
  CLEAR:   ITAB1, ITAB2, SETVAL, SETVLUES.
  MOVE 60 TO LN_CNTR.
ENDFORM.

*  Call function to get all appropriate data for cost centre group.    *
FORM GET_COSTGROUP.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = P_CTRGRP
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = SETVAL
       EXCEPTIONS
            SET_NOT_FOUND = 01.
  SORT SETVAL BY TITLE.
  LOOP AT SETVAL.
    SELECT * FROM CSKS WHERE KOSTL BETWEEN SETVAL-FROM AND
             SETVAL-TO ORDER BY KOSTL.
      SELECT * FROM COSP WHERE OBJNR = CSKS-OBJNR     "External Postings
               AND GJAHR = P_YEAR
               ORDER BY OBJNR KSTAR.
        CLEAR ITAB1.
        PERFORM MOVE_COSP.
        IF COSP-WRTTP = '04'.
          PERFORM MOVE_ACT.
        ELSEIF COSP-WRTTP = '01'.
          CHECK COSP-VERSN = P_VERSN.
          PERFORM MOVE_PLAN.
        ENDIF.
        APPEND ITAB1.
      ENDSELECT.

      SELECT * FROM COSS WHERE OBJNR = CSKS-OBJNR     "Internal Postings
               AND GJAHR = P_YEAR
               ORDER BY OBJNR KSTAR.
        CLEAR ITAB1.
        PERFORM MOVE_COSS.
        IF COSS-WRTTP = '04'.
          PERFORM MOVE_ACT_COSS.
        ELSEIF COSS-WRTTP = '01'.
          CHECK COSS-VERSN = P_VERSN.
          PERFORM MOVE_PLAN_COSS.
        ENDIF.
        APPEND ITAB1.
      ENDSELECT.

    ENDSELECT.
  ENDLOOP.
ENDFORM.

* Call function to get all appropriate data for cost element group.    *
FORM GET_ELEMENT_GRP.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = P_ELMGRP
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = SETVLUES
       EXCEPTIONS
            SET_NOT_FOUND = 01.
  SORT SETVLUES BY TITLE.
  LOOP AT SETVLUES.
    SELECT * FROM CSKB WHERE KOKRS = P_KOKRS
                         AND KSTAR BETWEEN SETVLUES-FROM
                         AND SETVLUES-TO ORDER BY KSTAR.
      CLEAR ITAB2.
      MOVE: SETVLUES-TITLE TO ITAB2-ELMGRP,
            CSKB-KSTAR     TO ITAB2-KSTAR PERCENTAGE 100 RIGHT.
      APPEND ITAB2.
    ENDSELECT.
  ENDLOOP.
ENDFORM.

*  Routine to join cost elements to cost centres. *
FORM PROC_COSTGROUP.
  LOOP AT ITAB1.
    READ TABLE ITAB2 WITH KEY KSTAR = ITAB1-KSTAR.
    IF ITAB2-KSTAR = ITAB1-KSTAR.
      SELECT SINGLE * FROM CSKU WHERE KSTAR = ITAB2-KSTAR
                                  AND KTOPL = P_KTOPL
                                  AND SPRAS = SY-LANGU.
      PERFORM MOVE_ELM.
      SELECT * FROM T800Y WHERE LANGU = SY-LANGU
               AND TAB = 'CCSS' AND SETNR = LTEXT.
        MOVE: T800Y-TITLE TO GRPTEXT.
      ENDSELECT.
      MODIFY ITAB1.
    ELSE.
      DELETE ITAB1.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*     FORM PROC_MAINTAB                                               *
*---------------------------------------------------------------------*
*  This routine control breaks on Cost centres, Cost element Groups   *
*  and Cost elements.  It calculates and writes all appropriate data. *
*---------------------------------------------------------------------*
FORM PROC_MAINTAB.
SORT ITAB1 BY KOSTL ELMGRP KSTARGRP KSTAR.
  LOOP AT ITAB1.
    ON CHANGE OF ITAB1-KOSTL.
      PERFORM CHK_LCNT.
      PERFORM WRT_COL.
      SELECT * FROM CSKT WHERE SPRAS = SY-LANGU
                           AND KOKRS = P_KOKRS
                           AND KOSTL = ITAB1-KOSTL.
        PERFORM WRT_CSTGRP.
      ENDSELECT.
    ENDON.
    AT END OF KSTAR.
      SUM.
      PERFORM CAL_COSTS.
      PERFORM CHK_LCNT.
      PERFORM ACCUM.
      PERFORM WRT_DET.
            CLEAR: VAR%.
    ENDAT.
    AT END OF KSTARGRP.
      PERFORM CAL_ELMGRP2.
      PERFORM CHK_LCNT.
      PERFORM WRT_ELMGRP2.
    ENDAT.
    AT END OF ELMGRP.
      PERFORM CAL_ELMGRP.
      PERFORM CHK_LCNT.
      PERFORM WRT_ELMGRP.
    ENDAT.
    AT END OF KOSTL.
      PERFORM CAL_CSTGRP.
      PERFORM CHK_LCNT.
      PERFORM WRT_CSTOT.
    ENDAT.
    AT LAST.
      PERFORM CHK_LCNT.
      ULINE: /1(132).
      PERFORM CHK_LCNT.
      PERFORM CAL_GRABS.
      PERFORM CHK_LCNT.
      PERFORM WRT_GRABS.
    ENDAT.
  ENDLOOP.
ENDFORM.

*  Routine to move specific data to be appended into itab1 table.      *
FORM MOVE_COSP.
  MOVE: SETVAL-TITLE TO ITAB1-CSTGRP,
        CSKS-KOSTL   TO ITAB1-KOSTL,
        CSKS-OBJNR   TO ITAB1-OBJNR,
        COSP-VERSN   TO ITAB1-VERSN.
ENDFORM.
**************************** MOVE_COSS *********************************
*  Routine to move specific data to be appended into itab1 table.      *
************************************************************************
FORM MOVE_COSS.
  MOVE: SETVAL-TITLE TO ITAB1-CSTGRP,
        CSKS-KOSTL   TO ITAB1-KOSTL,
        CSKS-OBJNR   TO ITAB1-OBJNR,
        COSS-VERSN   TO ITAB1-VERSN.
ENDFORM.

*  Routine to move specific data to be appended into itab1 table.      *
FORM MOVE_ACT.
  MOVE: COSP-WRTTP   TO ITAB1-WRTTP,
        COSP-KSTAR   TO ITAB1-KSTAR,
        COSP-WKG001  TO ITAB1-ACOST1,
        COSP-WKG002  TO ITAB1-ACOST2,
        COSP-WKG003  TO ITAB1-ACOST3,
        COSP-WKG004  TO ITAB1-ACOST4,
        COSP-WKG005  TO ITAB1-ACOST5,
        COSP-WKG006  TO ITAB1-ACOST6,
        COSP-WKG007  TO ITAB1-ACOST7,
        COSP-WKG008  TO ITAB1-ACOST8,
        COSP-WKG009  TO ITAB1-ACOST9,
        COSP-WKG010  TO ITAB1-ACOST10,
        COSP-WKG011  TO ITAB1-ACOST11,
        COSP-WKG012  TO ITAB1-ACOST12.
ENDFORM.
************************ MOVE_ACT_COSS *********************************
*  Routine to move specific data to be appended into itab1 table.      *
************************************************************************
FORM MOVE_ACT_COSS.
  MOVE: COSS-WRTTP   TO ITAB1-WRTTP,
        COSS-KSTAR   TO ITAB1-KSTAR,
        COSS-WKG001  TO ITAB1-ACOST1,
        COSS-WKG002  TO ITAB1-ACOST2,
        COSS-WKG003  TO ITAB1-ACOST3,
        COSS-WKG004  TO ITAB1-ACOST4,
        COSS-WKG005  TO ITAB1-ACOST5,
        COSS-WKG006  TO ITAB1-ACOST6,
        COSS-WKG007  TO ITAB1-ACOST7,
        COSS-WKG008  TO ITAB1-ACOST8,
        COSS-WKG009  TO ITAB1-ACOST9,
        COSS-WKG010  TO ITAB1-ACOST10,
        COSS-WKG011  TO ITAB1-ACOST11,
        COSS-WKG012  TO ITAB1-ACOST12.
ENDFORM.

*   Routine to move specific data to be appended into itab1 table.     *
FORM MOVE_PLAN.
  MOVE: COSP-WRTTP   TO ITAB1-WRTTP,
        COSP-KSTAR   TO ITAB1-KSTAR,
        COSP-WKG001  TO ITAB1-PCOST1,
        COSP-WKG002  TO ITAB1-PCOST2,
        COSP-WKG003  TO ITAB1-PCOST3,
        COSP-WKG004  TO ITAB1-PCOST4,
        COSP-WKG005  TO ITAB1-PCOST5,
        COSP-WKG006  TO ITAB1-PCOST6,
        COSP-WKG007  TO ITAB1-PCOST7,
        COSP-WKG008  TO ITAB1-PCOST8,
        COSP-WKG009  TO ITAB1-PCOST9,
        COSP-WKG010  TO ITAB1-PCOST10,
        COSP-WKG011  TO ITAB1-PCOST11,
        COSP-WKG012  TO ITAB1-PCOST12.
ENDFORM.
*************************** MOVE_PLAN_COSS *****************************
*   Routine to move specific data to be appended into itab1 table.     *
************************************************************************
FORM MOVE_PLAN_COSS.
  MOVE: COSS-WRTTP   TO ITAB1-WRTTP,
        COSS-KSTAR   TO ITAB1-KSTAR,
        COSS-WKG001  TO ITAB1-PCOST1,
        COSS-WKG002  TO ITAB1-PCOST2,
        COSS-WKG003  TO ITAB1-PCOST3,
        COSS-WKG004  TO ITAB1-PCOST4,
        COSS-WKG005  TO ITAB1-PCOST5,
        COSS-WKG006  TO ITAB1-PCOST6,
        COSS-WKG007  TO ITAB1-PCOST7,
        COSS-WKG008  TO ITAB1-PCOST8,
        COSS-WKG009  TO ITAB1-PCOST9,
        COSS-WKG010  TO ITAB1-PCOST10,
        COSS-WKG011  TO ITAB1-PCOST11,
        COSS-WKG012  TO ITAB1-PCOST12.
ENDFORM.

* Routine to move specific data to be appended into itab1 table.       *
FORM MOVE_ELM.
  MOVE: ITAB2-ELMGRP   TO ITAB1-ELMGRP,
        ITAB2-KSTARGRP TO ITAB1-KSTARGRP,
        CSKU-KTEXT     TO ITAB1-KTEXT.
  CONCATENATE:  '0H' P_CTRGRP INTO LTEXT.
ENDFORM.

* Routine to write the cost centre group to the report.                *
FORM WRT_CSTGRP.
  FORMAT COLOR COL_GROUP ON INTENSIFIED OFF.
  WRITE: /1 SY-VLINE, 3 ITAB1-KOSTL, 10 SY-VLINE,
         11(33) CSKT-LTEXT, 45 SY-VLINE, 68 SY-VLINE,
         91 SY-VLINE, 115 SY-VLINE, 132 SY-VLINE.
  FORMAT COLOR COL_GROUP OFF INTENSIFIED ON.
  LN_CNTR = LN_CNTR + 1.
ENDFORM.

*  Routine to calculate the costs for specified periods.               *
FORM CAL_COSTS.
  ADD ITAB1-ACOST1 FROM P_FROM TO P_MONTH GIVING ACTCST.
  ADD ITAB1-PCOST1 FROM P_FROM TO P_TO GIVING PLNCST.
  ADD ITAB1-ACOST1 FROM P_MONTH TO P_MONTH GIVING MONTH_ACT.
  ABSVAR = ACTCST - PLNCST.
  IF PLNCST NE 0.
    COMPUTE: VAR% = ( ABSVAR / PLNCST ) * 100.
  ENDIF.
ENDFORM.

* Routine to write the detail data to the report.                      *
FORM WRT_DET.
 IF P_TOTALS = 'N'.
  WRITE: /1 SY-VLINE, 10 SY-VLINE.
  FORMAT COLOR COL_KEY ON.
  WRITE:  16 ITAB1-KSTAR, 24 ITAB1-KTEXT.
  FORMAT COLOR COL_KEY OFF.
  FORMAT COLOR COL_NORMAL ON INTENSIFIED OFF.
  WRITE:  45 SY-VLINE.
  IF MONTH_ACT NE 0.
    WRITE:  46 MONTH_ACT.
  ENDIF.
  IF ACTCST NE 0.
    WRITE:  69 ACTCST.
  ENDIF.
  IF PLNCST NE 0.
    WRITE: 92 PLNCST.
  ENDIF.
  WRITE: 91 SY-VLINE.
  IF ABSVAR NE 0.
    WRITE: 116 ABSVAR.
  ENDIF.
  WRITE: 68 SY-VLINE, 115 SY-VLINE, 132 SY-VLINE.
  IF VAR% NE 0.
*    WRITE: 122 VAR%.
  ENDIF.
  FORMAT COLOR COL_NORMAL OFF INTENSIFIED ON.
  LN_CNTR = LN_CNTR + 1.
 ENDIF.
ENDFORM.

FORM CAL_ELMGRP2.
   ABS_EL2 = ACT_EL2 - PLN_EL2.
   IF PLN_EL2 NE 0.
     COMPUTE: VAR% = ( ABS_EL2 / PLN_EL2 ) * 100.
   ENDIF.
 ENDFORM.

FORM WRT_ELMGRP2.

WRITE: /1 SY-VLINE, 10 SY-VLINE.
  FORMAT COLOR COL_NORMAL ON INTENSIFIED ON.
  WRITE: 13 TEXT-TTX, ITAB1-KSTARGRP, 45 SY-VLINE.
  IF MONTH_EL NE 0.
    WRITE: 46 MONTH_EL2.
  ENDIF.
  IF ACT_EL NE 0.
    WRITE: 69 ACT_EL2.
  ENDIF.
  IF PLN_EL NE 0.
    WRITE: 92 PLN_EL2.
  ENDIF.
  WRITE:   68 SY-VLINE,  91 SY-VLINE.
  IF ABS_EL NE 0.
    WRITE: 116 ABS_EL2.
  ENDIF.
  IF VAR% NE 0.
*    WRITE: 122 VAR%.
  ENDIF.
  WRITE: 115 SY-VLINE, 132 SY-VLINE.
  FORMAT COLOR COL_NORMAL OFF.
  LN_CNTR = LN_CNTR + 1.
  CLEAR: VAR%, MONTH_EL2, ACT_EL2, PLN_EL2, ABS_EL2.
  ENDFORM.

*  Routine to calculate the cost element group sub totals.             *
FORM CAL_ELMGRP.
*  SUM.
*  ADD ITAB1-ACOST1 FROM P_FROM TO P_MONTH GIVING ACTCST.
*  ADD ITAB1-PCOST1 FROM P_FROM TO P_TO GIVING PLNCST.
*  ADD ITAB1-ACOST1 FROM P_MONTH TO P_MONTH GIVING MONTH_ACT.
  ABS_EL = ACT_EL - PLN_EL.
  IF PLN_EL NE 0.
    COMPUTE: VAR% = ( ABS_EL / PLN_EL ) * 100.
  ENDIF.
ENDFORM.

* Routine to write the cost element group to the report.               *
FORM WRT_ELMGRP.
  WRITE: /1 SY-VLINE, 10 SY-VLINE.
  FORMAT COLOR COL_NORMAL ON INTENSIFIED ON.
  WRITE: 13 TEXT-TTT, ITAB1-ELMGRP, 45 SY-VLINE.
  IF MONTH_EL NE 0.
    WRITE: 46 MONTH_EL.
  ENDIF.
  IF ACT_EL NE 0.
    WRITE: 69 ACT_EL.
  ENDIF.
  IF PLN_EL NE 0.
    WRITE: 92 PLN_EL.
  ENDIF.
  WRITE:   68 SY-VLINE,  91 SY-VLINE.
  IF ABS_EL NE 0.
    WRITE: 116 ABS_EL.
  ENDIF.
  IF VAR% NE 0.
*    WRITE: 122 VAR%.
  ENDIF.
  WRITE: 115 SY-VLINE, 132 SY-VLINE.
  FORMAT COLOR COL_NORMAL OFF.
  LN_CNTR = LN_CNTR + 1.
  CLEAR: VAR%, MONTH_EL, ACT_EL, PLN_EL, ABS_EL2.
  ENDFORM.

* Routine to accumulate all total variables.                           *
FORM ACCUM.
  ACTTOT = ACTTOT + ACTCST.
  PLNTOT = PLNTOT + PLNCST.
  MONTHTOT = MONTHTOT + MONTH_ACT.
  ACT_EL2 = ACT_EL2 + ACTCST.
  PLN_EL2 = PLN_EL2 + PLNCST.
  MONTH_EL2 = MONTH_EL2 + MONTH_ACT.
  ACT_EL = ACT_EL + ACTCST.
  PLN_EL = PLN_EL + PLNCST.
  MONTH_EL = MONTH_EL + MONTH_ACT.
  GRACT = GRACT + ACTCST.
  GRPLN = GRPLN + PLNCST.
  MONTHGTOT = MONTHGTOT + MONTH_ACT.
ENDFORM.

*  Routine to calculate the cost centre group sub totals.              *
FORM CAL_CSTGRP.
  ABSTOT = ACTTOT - PLNTOT.
  IF PLNTOT NE 0.
    COMPUTE: VARTOT = ( ABSTOT / PLNTOT ) * 100.
  ENDIF.
ENDFORM.

*  Routine to write the cost centre total to the report.              *
FORM WRT_CSTOT.
  WRITE: /1 SY-VLINE, 10 SY-VLINE.
  FORMAT COLOR COL_TOTAL ON.
  WRITE: 20 TEXT-015.
  IF MONTHTOT NE 0.
    WRITE: 46 MONTHTOT.
  ENDIF.
  IF ACTTOT NE 0.
    WRITE: 69 ACTTOT.
  ENDIF.
  IF PLNTOT NE 0.
    WRITE: 92 PLNTOT.
  ENDIF.
  WRITE: 45 SY-VLINE,  68 SY-VLINE,  91 SY-VLINE.
  IF ABSTOT NE 0.
    WRITE: 115 ABSTOT.
  ENDIF.
  IF VARTOT NE 0.
*    WRITE: 122 VARTOT.
  ENDIF.
  WRITE:  115 SY-VLINE, 132 SY-VLINE.
  LN_CNTR = LN_CNTR + 1.
  ULINE: /1(132).
  CLEAR: ACTTOT, PLNTOT.
  FORMAT COLOR COL_TOTAL OFF.
  CLEAR: VARTOT, MONTHTOT, ACTTOT, PLNTOT.
  IF P_TOTALS = 'N'.
     NEW-PAGE.
  ENDIF.
ENDFORM.

*  Routine to calculate the Grand totals.                              *
FORM CAL_GRABS.
  GRABS = GRACT - GRPLN.
  IF GRPLN NE 0.
    COMPUTE: GRVAR = ( GRABS / GRPLN ) * 100.
  ENDIF.
ENDFORM.

*  Routine to write the Grand totals to the report.                    *
FORM WRT_GRABS.
  SKIP.
  FORMAT COLOR COL_TOTAL ON.
  ULINE: /1(132).
  WRITE: /1 SY-VLINE, 2 P_CTRGRP,
         16 TEXT-016, 45 SY-VLINE.
  IF GRACT NE 0.
    WRITE: 69 GRACT.
  ENDIF.
  IF GRPLN NE 0.
    WRITE: 92 GRPLN.
  ENDIF.
  WRITE: 68 SY-VLINE,  91 SY-VLINE.
  IF GRABS NE 0.
    WRITE: 115 GRABS.
  ENDIF.
  IF MONTHGTOT NE 0.
    WRITE: 46 MONTHGTOT.
  ENDIF.
  IF GRVAR NE 0.
*    WRITE: 122 GRVAR.
  ENDIF.
  WRITE: 115 SY-VLINE, 132 SY-VLINE.
  CLEAR GRVAR.
  ULINE: /1(132).
  FORMAT COLOR COL_TOTAL OFF.
  LN_CNTR = LN_CNTR + 4.
ENDFORM.

*  Routine to check the line count.                                    *
FORM CHK_LCNT.
  IF LN_CNTR >= 58.
    ULINE: /1(132).
    PERFORM WRT_HDG.
  ENDIF.
ENDFORM.

*  Routine to write the main headings.                                 *
FORM WRT_HDG.
  FORMAT INTENSIFIED OFF.
  NEW-PAGE.
  ULINE: /1(132).
  WRITE: /1 SY-VLINE, 2 TEXT-RPT, SY-REPID, 54 TEXT-TTL,
          105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT, 132 SY-VLINE.
  WRITE: /1 SY-VLINE, TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           T001-BUTXT UNDER TEXT-TTL,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO, 132 SY-VLINE.
  WRITE: /1 SY-VLINE, TEXT-VRS UNDER TEXT-RPT, P_VERSN,
                      61 TEXT-TT1, 132 SY-VLINE.
  WRITE: /1 SY-VLINE, 40 TEXT-TT2, 132 SY-VLINE.
  WRITE: /1 SY-VLINE, 49 TEXT-TT3, 68 P_MONTH, 70 '/',
              71 P_YEAR, 132 SY-VLINE.
  uline: /1(132).
  FORMAT COLOR COL_TOTAL ON INTENSIFIED OFF.
  WRITE: /1 SY-VLINE, 2 TEXT-006, 25 P_CTRGRP, 40 GRPTEXT,
            TEXT-025, P_ELMGRP, 132 SY-VLINE.
  FORMAT COLOR COL_TOTAL OFF INTENSIFIED ON.
  ULINE: /1(132).
  MOVE: 10 TO LN_CNTR.
  FORMAT INTENSIFIED ON.
ENDFORM.

*  Routine to write the column headings.                               *
FORM WRT_COL.
  ULINE: /1(132).
  FORMAT INTENSIFIED OFF.
  WRITE: /1 SY-VLINE, 3 TEXT-007, 10 SY-VLINE, 11 TEXT-010, 45 SY-VLINE,
         48 TEXT-011, 68 SY-VLINE, 72 TEXT-017, 91 SY-VLINE,
        95 TEXT-012, 115 SY-VLINE, 119 TEXT-013,
        132 SY-VLINE.
  ULINE: /1(132).
  LN_CNTR = LN_CNTR + 3.
  FORMAT INTENSIFIED ON.
ENDFORM.
