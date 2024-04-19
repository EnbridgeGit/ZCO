REPORT ZKORR011.   " LINE-SIZE 250 LINE-COUNT 65.
TYPE-POOLS: SLIS.

************************************************************************
*                                                                      *
*   PROGRAM:    ZKORR011                                               *
*   PROGRAMMER: Mohammad Khan                                          *
*   DATE:       May, 2006.                                             *
*   Track #:    TR61                                                   *
*   The purpose of this program is to produce the Orders list with the *
*   latest settlement rules.                                           *
*   Settlement Category (COBRB-KONTY) used are:                        *
*        'PR' ------- WBS                                              *
*        'OR' ------- ORD                                              *
*        'KS' ------- CTR                                              *
*        'SK' ------- G/L                                              *
*                                                                      *
*                                                                      *
************************************************************************
* CHANGES:                                                             *
* 17/07/2009 TR667 M KHAN  Following changes are made:                 *
*                          1- Optionally select Actual or Plan rules.  *
*                          2- Optionally select settlement type        *
*                             Periodical or Full.                      *
*                          3- Apply the latest settlement rules.       *
************************************************************************

TABLES:   AUFK,                        " Order master
          CSKT,                        " Cost Center Texts
          COBRB,                       " Settlement rules
        SETHIER.        "STRUCTURE -Interface Structure: Set Hierarchy

DATA: BEGIN OF STRUC1,
            BUKRS       LIKE AUFK-BUKRS,       " Company code
            AUFNR       LIKE AUFK-AUFNR,       " Order Number
            PERBZ       LIKE COBRB-PERBZ,      " Settlement Type
            KONTY       LIKE COBRB-KONTY,      " Settlement Category
            RECVR(12)   TYPE C,                " Settlement Receiver
            GABJA       LIKE COBRB-GABJA,      " Valid from year
            GABPE       LIKE COBRB-GABPE,      " Valid from period
            PROZS       LIKE COBRB-PROZS,      " Percentage
            RESCC       LIKE AUFK-KOSTV,       " Responsible CC
            KTEXT       LIKE AUFK-KTEXT,       " Order Description
            URZUO       LIKE COBRB-URZUO,      " Source Assignment
            EXTNR       LIKE COBRB-EXTNR,      " Sequence number
            STATUS(9)   TYPE C,                " Order Status
      END OF STRUC1.

DATA: BEGIN OF STRUC2,
            BUKRS       LIKE AUFK-BUKRS,           " Company code
            AUFNR       LIKE AUFK-AUFNR,           " Order Number
            PERBZ       LIKE COBRB-PERBZ,          " Settlement Type
            KONTY       LIKE COBRB-KONTY,          " Settlement Category
            GABJA       LIKE COBRB-GABJA,          " Valid from year
            GABPE       LIKE COBRB-GABPE,          " Valid from period
      END OF STRUC2.

DATA:     BEGIN OF ITAB1 OCCURS 0.
                INCLUDE STRUCTURE STRUC1.
DATA:     END OF ITAB1.

DATA:     BEGIN OF ITAB2 OCCURS 0.
                INCLUDE STRUCTURE STRUC1.
DATA:     END OF ITAB2.

DATA:     BEGIN OF DSPTAB OCCURS 0,
*            ORDGRP      LIKE SETHIER-SHORTNAME,    "Order Group
            AUFNR       LIKE AUFK-AUFNR,           " Order Number
            KTEXT       LIKE AUFK-KTEXT,           " Order Description
            STATUS(9)   TYPE C,                    " Order Status
            BUKRS       LIKE AUFK-BUKRS,           " Company code
            RESCC       LIKE AUFK-KOSTL,           " Responsible CC
            PERBZ       LIKE COBRB-PERBZ,          " Settlement Type
            URZUO       LIKE COBRB-URZUO,          " Source Assignment
            EXTNR       LIKE COBRB-EXTNR,          " Sequence number
            PROZS       LIKE COBRB-PROZS,          " Percentage
            KOSTL       LIKE COBRB-KOSTL,          " Cost Center
            CCDES       LIKE CSKT-KTEXT,           " Cost Center Text
            PS_PSP_PNR       LIKE COBRB-PS_PSP_PNR,     " WBS
            HKONT       LIKE COBRB-HKONT,          " G/L Account
            ORNUM       LIKE COBRB-AUFNR,          " Receiver Order #
            GABPE       LIKE COBRB-GABPE,          " Valid from period
            GABJA       LIKE COBRB-GABJA,          " Valid from year
          END OF DSPTAB.
DATA:     BEGIN OF SRTAB OCCURS 0,
            PERBZ       LIKE COBRB-PERBZ,
            KONTY       LIKE COBRB-KONTY,
            GBISJ       LIKE COBRB-GBISJ,
            GBISP       LIKE COBRB-GBISP,
            GABJA       LIKE COBRB-GABJA,
            GABPE       LIKE COBRB-GABPE,
            PROZS       LIKE COBRB-PROZS,
            KOSTL       LIKE COBRB-KOSTL,
            PS_PSP_PNR       LIKE COBRB-PS_PSP_PNR,
            HKONT       LIKE COBRB-HKONT,
            AUFNR       LIKE COBRB-AUFNR,
            URZUO       LIKE COBRB-URZUO,
            EXTNR       LIKE COBRB-EXTNR,
          END OF SRTAB.

DATA:     W_HEAD01(60)  TYPE C,
          W_HEAD03(60)  TYPE C,
          NEW_CCYYMM(6) TYPE C,
          OLD_CCYYMM(6) TYPE C.

DATA: BEGIN OF WHERE_CLAUSE  OCCURS 10,                         "TR667
      COND(30)  TYPE C,                                         "TR667
      END OF WHERE_CLAUSE.                                      "TR667

DATA: PREV_GBISJ_GBISP(7) TYPE C,                               "TR667
      CURR_GBISJ_GBISP(7) TYPE C.                               "TR667
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X'.                      "TR667
FIELD-SYMBOLS: <FS1>.

************************************************************************
*              selection screen                                        *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: S_ORDGRP FOR SETHIER-SHORTNAME NO INTERVALS.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(3) TEXT-003.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS: S_AUFNR FOR AUFK-AUFNR.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: S_BUKRS  FOR AUFK-BUKRS OBLIGATORY
                         NO INTERVALS DEFAULT 'UGL'.
SELECTION-SCREEN SKIP 1.                                        "TR667
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-004. "TR667
PARAMETERS:                                                     "TR667
  B_ACTUAL RADIOBUTTON GROUP RBCR DEFAULT 'X' USER-COMMAND RAD, "TR667
  B_PLAN   RADIOBUTTON GROUP RBCR.                "Plan rules   "TR667
PARAMETERS:                                                     "TR667
  P_VERSN LIKE COBRB-VERSN.                                     "TR667
SELECTION-SCREEN END OF BLOCK BOX1.                             "TR667

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-005. "TR667
PARAMETERS:                                   "Settlement Type  "TR667
  B_PER    RADIOBUTTON GROUP PRFL DEFAULT 'X',    "Periodical   "TR667
  B_FULL   RADIOBUTTON GROUP PRFL.                "Full         "TR667
SELECTION-SCREEN END OF BLOCK BOX2.                             "TR667

*SELECTION-SCREEN SKIP 2.                                       "TR667
*   SELECTION-SCREEN COMMENT  1(28) TEXT-002.                   "TR667
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002. "TR667
      PARAMETERS: P_PHASE0   AS CHECKBOX DEFAULT 'X',
                  P_PHASE1   AS CHECKBOX DEFAULT 'X',
                  P_PHASE2   AS CHECKBOX DEFAULT 'X',
                  P_PHASE3   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX3.                             "TR667
SELECTION-SCREEN END OF BLOCK BOX.
************************************************************************
************************* INITIALIZATION ******************************
************************************************************************
INITIALIZATION.
  PERFORM RESTRICT_RANGES.

************************************************************************
**************** SELECTION-SCREEN ON VALUE-REQUEST *********************
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDGRP-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0103'
            FIELD_NAME         = 'AUFNR'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = S_ORDGRP-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
*  IF SY-SUBRC = 1.
*    S_CCGRP = O_TMPSET.
*  ENDIF.

************************************************************************
**************** SELECTION-SCREEN OUTPUT **********************   TR667
************************************************************************
AT SELECTION-SCREEN OUTPUT.

*IF SY-SLSET = SPACE.
   CASE TRUE.
        WHEN B_ACTUAL.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_VERSN'.
                     SCREEN-INPUT = '0'.
                     CLEAR P_VERSN.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.

        WHEN B_PLAN.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_VERSN'.
                     SCREEN-INPUT = '1'.
*                     CLEAR P_VERSN.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.
   ENDCASE.
*ENDIF.

************************************************************************
********************** START-OF-SELECTION ******************************
************************************************************************

START-OF-SELECTION.
    PERFORM VALIDATE_SCREEN_DATA_PLUS.
    IF NOT S_ORDGRP[] IS INITIAL.
       LOOP AT S_ORDGRP.
            PERFORM GET_HIERARCHY_DATA USING S_ORDGRP-LOW.
       ENDLOOP.
    ENDIF.
    PERFORM COLLECT_DB_DATA.
    PERFORM DISPLAY_ALV_GRID_DATA.
END-OF-SELECTION.

************************************************************************
***                       COLLECT_DB_DATA                           ****
************************************************************************
FORM COLLECT_DB_DATA.

*Start of TR667 Changes
*Build Dynamic Where clause depending upon Selection Screen
     CONCATENATE 'OBJNR' ' = ' 'AUFK-OBJNR' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
  IF B_PER = 'X'.
     CONCATENATE 'AND' 'PERBZ' ' = ' '''PER''' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
  ELSE.
     CONCATENATE 'AND' 'PERBZ' ' = ' '''GES''' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
  ENDIF.
  IF B_ACTUAL = 'X'.
     CONCATENATE 'AND' 'AVORG' ' = ' '''KOAO''' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
  ELSE.
     CONCATENATE 'AND' 'AVORG' ' = ' '''KOAP''' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
     CONCATENATE 'AND' 'VERSN' ' = ' 'P_VERSN' INTO WHERE_CLAUSE-COND
     SEPARATED BY SPACE.
     APPEND WHERE_CLAUSE.
     CLEAR WHERE_CLAUSE-COND.
  ENDIF.
*End of TR667 Changes

 SELECT BUKRS AUFNR AUART KTEXT PHAS0 PHAS1 PHAS2 PHAS3 OBJNR KOSTV
   INTO (AUFK-BUKRS, AUFK-AUFNR, AUFK-AUART, AUFK-KTEXT, AUFK-PHAS0,
         AUFK-PHAS1, AUFK-PHAS2, AUFK-PHAS3, AUFK-OBJNR, AUFK-KOSTV)
   FROM AUFK
  WHERE AUFNR IN S_AUFNR
    AND BUKRS IN S_BUKRS
    AND LOEKZ NE 'X'.

 IF ( P_PHASE0 = 'X' AND AUFK-PHAS0 = 'X' )  OR
    ( P_PHASE1 = 'X' AND AUFK-PHAS1 = 'X' )  OR
    ( P_PHASE2 = 'X' AND AUFK-PHAS2 = 'X' )  OR
    ( P_PHASE3 = 'X' AND AUFK-PHAS3 = 'X' ).

    CLEAR: PREV_GBISJ_GBISP, SRTAB.
    REFRESH SRTAB.
*Start of TR667 changes
*    SELECT PERBZ KONTY GABJA GABPE PROZS KOSTL PS_PSP_PNR HKONT AUFNR
*           URZUO EXTNR
*      INTO (COBRB-PERBZ, COBRB-KONTY, COBRB-GABJA, COBRB-GABPE,
*            COBRB-PROZS, COBRB-KOSTL, COBRB-PS_PSP_PNR,
*            COBRB-HKONT, COBRB-AUFNR, COBRB-URZUO, COBRB-EXTNR)
*      FROM COBRB
*     WHERE OBJNR = AUFK-OBJNR
*       AND AVORG = 'KOAO'.              "Actual

    SELECT PERBZ KONTY GBISJ GBISP GABJA GABPE PROZS KOSTL PS_PSP_PNR
           HKONT AUFNR URZUO EXTNR
      INTO TABLE SRTAB
*      INTO (COBRB-PERBZ, COBRB-KONTY, COBRB-GBISJ, COBRB-GBISP,
*            COBRB-GABJA, COBRB-GABPE, COBRB-PROZS, COBRB-KOSTL,
*            COBRB-PS_PSP_PNR,COBRB-HKONT, COBRB-AUFNR,
*            COBRB-URZUO, COBRB-EXTNR)
      FROM COBRB
      WHERE (WHERE_CLAUSE).
*      ORDER BY GBISJ DESCENDING GBISP DESCENDING.

    LOOP AT SRTAB.
         IF SRTAB-GBISJ = SPACE.
            MOVE '9999'   TO SRTAB-GBISJ.
            MOVE '16'     TO SRTAB-GBISP.
         ENDIF.
         MODIFY SRTAB.
    ENDLOOP.
    SORT SRTAB DESCENDING BY GBISJ GBISP .

    LOOP AT SRTAB.
      CONCATENATE SRTAB-GBISJ SRTAB-GBISP INTO CURR_GBISJ_GBISP.
      IF CURR_GBISJ_GBISP < PREV_GBISJ_GBISP.
         CONTINUE.
      ELSE.
         MOVE CURR_GBISJ_GBISP TO PREV_GBISJ_GBISP.
         CLEAR: CURR_GBISJ_GBISP.
*End of TR667 changes

         MOVE: AUFK-BUKRS  TO ITAB1-BUKRS,
               AUFK-AUFNR  TO ITAB1-AUFNR,
               AUFK-KTEXT  TO ITAB1-KTEXT,
               AUFK-KOSTV  TO ITAB1-RESCC,
               SRTAB-PERBZ TO ITAB1-PERBZ,
               SRTAB-KONTY TO ITAB1-KONTY,
               SRTAB-GABJA TO ITAB1-GABJA,
               SRTAB-GABPE TO ITAB1-GABPE,
               SRTAB-URZUO TO ITAB1-URZUO,
               SRTAB-EXTNR TO ITAB1-EXTNR,
               SRTAB-PROZS TO ITAB1-PROZS.
               CASE SRTAB-KONTY.
                    WHEN 'KS'.                                    "CTR
                          MOVE  SRTAB-KOSTL TO ITAB1-RECVR.
                    WHEN 'PR'.                                    "WBS
                          MOVE  SRTAB-PS_PSP_PNR TO ITAB1-RECVR.
                    WHEN 'SK'.                                    "G/L
                          MOVE  SRTAB-HKONT TO ITAB1-RECVR.
                    WHEN 'OR'.                                    "ORD
                          MOVE  SRTAB-AUFNR TO ITAB1-RECVR.
                    WHEN  OTHERS.
                          WRITE: /1 TEXT-007, TEXT-008, SRTAB-KONTY,
*                             TEXT-009, SRTAB-AUFNR.
                             TEXT-009, AUFK-AUFNR.
                          STOP.
               ENDCASE.

*        IF COBRB-KOSTL <> SPACE.              "keep this code
*           MOVE  COBRB-KOSTL TO ITAB1-RECVR.
*        ELSEIF COBRB-HKONT <> SPACE.
*           MOVE  COBRB-HKONT TO ITAB1-RECVR.
*        ELSEIF COBRB-PS_PSP_PNR <> SPACE.
*           MOVE  COBRB-PS_PSP_PNR TO ITAB1-RECVR.
*        ELSE.
*           MOVE  COBRB-AUFNR      TO ITAB1-RECVR.
*        ENDIF.

        IF AUFK-PHAS0 = 'X'.
           MOVE TEXT-100 TO ITAB1-STATUS.
        ELSEIF AUFK-PHAS1 = 'X'.
           MOVE TEXT-101 TO ITAB1-STATUS.
        ELSEIF AUFK-PHAS2 = 'X'.
           MOVE TEXT-102 TO ITAB1-STATUS.
        ELSEIF AUFK-PHAS3 = 'X'.
           MOVE TEXT-103 TO ITAB1-STATUS.
        ENDIF.

        APPEND ITAB1.
        CLEAR  ITAB1.
   ENDIF.
   ENDLOOP.
*   ENDSELECT.
  ENDIF.
ENDSELECT.

 SORT ITAB1 BY BUKRS AUFNR PERBZ KONTY RECVR GABJA GABPE.

* WRITE: / '********ITAB1 DISPLAY***********'.
* LOOP AT ITAB1.
*      WRITE: / ITAB1-BUKRS, ITAB1-AUFNR, ITAB1-PERBZ, ITAB1-KONTY,
*               ITAB1-RECVR, ITAB1-GABJA, ITAB1-GABPE, ITAB1-PROZS,
*               ITAB1-KTEXT, ITAB1-STATUS.
*      AT END OF AUFNR.
*         SKIP 1.
*      ENDAT.
*
* ENDLOOP.
 ITAB2[] = ITAB1[].

* LOOP AT ITAB1.  TR61
*      CONCATENATE ITAB1-GABJA ITAB1-GABPE INTO NEW_CCYYMM.
*      IF NEW_CCYYMM > OLD_CCYYMM.
*         MOVE-CORRESPONDING ITAB1 TO STRUC2.
*            MOVE NEW_CCYYMM TO OLD_CCYYMM.
*      ENDIF.
*      AT END OF PERBZ.
*         DELETE ITAB2 WHERE BUKRS = STRUC2-BUKRS AND
*                             AUFNR = STRUC2-AUFNR AND
*                             PERBZ = STRUC2-PERBZ AND
**                             KONTY = STRUC2-KONTY AND
*                             GABJA < STRUC2-GABJA.
*         DELETE ITAB2 WHERE BUKRS = STRUC2-BUKRS AND
*                             AUFNR = STRUC2-AUFNR AND
*                             PERBZ = STRUC2-PERBZ AND
**                             KONTY = STRUC2-KONTY AND
*                             GABPE < STRUC2-GABPE.
*
*         CLEAR: OLD_CCYYMM, STRUC2.
*      ENDAT.
* ENDLOOP.

* SKIP 2.
* WRITE: / '********ITAB2 DISPLAY***********'.
* LOOP AT ITAB2.
*      WRITE: / ITAB2-BUKRS, ITAB2-AUFNR, ITAB2-PERBZ, ITAB2-KONTY,
*               ITAB2-RECVR, ITAB2-GABJA, ITAB2-GABPE, ITAB2-PROZS,
*               ITAB2-KTEXT, ITAB2-STATUS.
*      AT END OF AUFNR.
*         SKIP 1.
*      ENDAT.
* ENDLOOP.

 LOOP AT ITAB2.
      MOVE-CORRESPONDING ITAB2 TO DSPTAB.
      CASE ITAB2-KONTY.
             WHEN 'KS'.                                    "CTR
                   MOVE  ITAB2-RECVR  TO DSPTAB-KOSTL.
                   PERFORM GET_CC_TEXT.
             WHEN 'PR'.                                    "WBS
                   MOVE  ITAB2-RECVR  TO DSPTAB-PS_PSP_PNR.
             WHEN 'SK'.                                    "G/L
                   MOVE  ITAB2-RECVR  TO DSPTAB-HKONT.
             WHEN 'OR'.                                    "ORD
                   MOVE  ITAB2-RECVR  TO DSPTAB-ORNUM.
             WHEN  OTHERS.
      ENDCASE.
      APPEND DSPTAB.
      CLEAR DSPTAB.
 ENDLOOP.

* SKIP 2.
* WRITE: / '********DSPTAB DISPLAY***********'.
* LOOP AT DSPTAB.
*      WRITE: / DSPTAB-AUFNR, DSPTAB-KTEXT, DSPTAB-STATUS,
*               DSPTAB-BUKRS, DSPTAB-RESCC, DSPTAB-PERBZ, DSPTAB-PROZS,
*               DSPTAB-KOSTL, DSPTAB-PS_PSP_PNR, DSPTAB-HKONT,
*               DSPTAB-ORNUM, DSPTAB-GABPE, DSPTAB-GABJA.
*
*      AT END OF AUFNR.
*         SKIP 1.
*      ENDAT.
* ENDLOOP.


ENDFORM.

************************************************************************
************************* GET_CC_TEXT **********************************
************************************************************************
FORM GET_CC_TEXT.

DATA: MAX_DATBI LIKE CSKT-DATBI.

     SELECT SINGLE MAX( DATBI ) INTO MAX_DATBI
       FROM CSKT
      WHERE SPRAS = 'EN'
        AND KOKRS = '10'
        AND KOSTL = DSPTAB-KOSTL.

     SELECT SINGLE KTEXT INTO CSKT-KTEXT
       FROM CSKT
      WHERE SPRAS = 'EN'
        AND KOKRS = '10'
        AND KOSTL = DSPTAB-KOSTL
        AND DATBI = MAX_DATBI.

     IF SY-SUBRC = 0.
        MOVE CSKT-KTEXT TO DSPTAB-CCDES.
     ENDIF.

ENDFORM.
************************************************************************
************************* RESTRICT_RANGES ******************************
************************************************************************
FORM RESTRICT_RANGES.
TYPE-POOLS: SSCR.
DATA: L_RESTRICT TYPE SSCR_RESTRICT.
DATA: L_OPT_LIST TYPE SSCR_OPT_LIST.
DATA: L_ASS TYPE SSCR_ASS.
  CLEAR L_OPT_LIST.
  L_OPT_LIST-NAME = 'S_ORDGRP'.
  L_OPT_LIST-OPTIONS-EQ = 'X'.
  APPEND L_OPT_LIST TO L_RESTRICT-OPT_LIST_TAB.

  CLEAR L_ASS.
  L_ASS-KIND = 'S'.
  L_ASS-NAME = 'S_ORDGRP'.
  L_ASS-SG_MAIN = 'I'.
  L_ASS-OP_MAIN = 'S_ORDGRP'.
  APPEND L_ASS TO L_RESTRICT-ASS_TAB.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
       EXPORTING
            RESTRICTION = L_RESTRICT
       EXCEPTIONS
            OTHERS      = 1.
ENDFORM.

********************************************************************
FORM GET_HIERARCHY_DATA USING P_SET.
* Local working variables
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
INCLUDE <line>.

  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
       EXPORTING
            SHORTNAME = P_SET
            OLD_SETID = P_SETID
       IMPORTING
            NEW_SETID = P_SETID.

*----------------------------------------------------------------------*
* Read the complete hierarchy (set and all subsets)
*----------------------------------------------------------------------*
  CALL FUNCTION 'G_SET_TREE_IMPORT'
       EXPORTING
            SETID              = P_SETID  "Set-ID, not the Setname!!!
            NO_TABLE_BUFFERING = ' '
       TABLES
            SET_HIERARCHY      = SET_HIERARCHY
            SET_VALUES         = SET_VALUES.


*----------------------------------------------------------------------*
* Add pointers to the imported set hierarchy. This is not needed if you
* just want to list all nodes or all values in the hierarchy!
*----------------------------------------------------------------------*
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
      IF SET_VAL_PTR-FROM =  SET_VAL_PTR-TO.
         S_AUFNR-SIGN     =  'I'.
         S_AUFNR-OPTION   =  'EQ'.
         S_AUFNR-LOW      =  SET_VAL_PTR-FROM.
         APPEND: S_AUFNR.
         CLEAR:  S_AUFNR.
      ELSE.
         S_AUFNR-SIGN     =  'I'.
         S_AUFNR-OPTION   =  'BT'.
         S_AUFNR-LOW      =  SET_VAL_PTR-FROM.
         S_AUFNR-HIGH     =  SET_VAL_PTR-TO.
         APPEND S_AUFNR.
         CLEAR S_AUFNR.
      ENDIF.
    ENDLOOP.                             "End of SET_VAL_PTR loop

*Temporary code
*loop at s_aufnr.
*     write: /5 s_aufnr-low, s_aufnr-high.
*endloop.
**stop.
ENDFORM.
************************************************************************
*********************** VALIDATE_SCREEN_DATA ***************************
************************************************************************
FORM VALIDATE_SCREEN_DATA_PLUS.
     IF ( S_ORDGRP[] IS INITIAL AND S_AUFNR[] IS INITIAL ) OR
        ( NOT S_ORDGRP[] IS INITIAL AND NOT S_AUFNR[] IS INITIAL ).
           CALL FUNCTION 'POPUP_FOR_INTERACTION'
             EXPORTING
             HEADLINE   = '!! ERROR !!'
             TEXT1      = 'PLEASE ENTER DATA IN:'
             TEXT2      = 'Order Group or Order Number'
             TEXT3      = ' '
             TEXT4      = 'Press OK Button to Correct Variants'
             BUTTON_1   = 'OK'.
           STOP.
     ENDIF.
ENDFORM.
************************************************************************
***                    DISPLAY_ALV_GRID_DATA                         ***
************************************************************************
*
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
*      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV.
*      SORT_STR TYPE SLIS_SORTINFO_ALV.

  MOVE TEXT-CLT  TO W_HEAD01+0(7).
  MOVE SY-SYSID  TO W_HEAD01+8(5).
  MOVE SY-MANDT  TO W_HEAD01+14(4).
  MOVE TEXT-DTE  TO W_HEAD01+21(5).
  WRITE SY-DATUM TO W_HEAD01+27(10).
  MOVE TEXT-TME  TO W_HEAD01+40(5).
  WRITE SY-UZEIT TO W_HEAD01+46(10).

*Build Report Heading
*  IF S_GJAHR-HIGH = SPACE.
*     CONCATENATE TEXT-001 S_GJAHR-LOW INTO W_HEAD03 SEPARATED BY SPACE.
*  ELSE.
*     CONCATENATE TEXT-001 S_GJAHR-LOW TEXT-002 S_GJAHR-HIGH
*                          INTO W_HEAD03 SEPARATED BY SPACE.
*  ENDIF.
*
  REPID = SY-REPID.
*  W_HEAD03 = SY-TITLE.                                         "TR667
  IF B_ACTUAL = 'X'.                                            "TR667
     CONCATENATE SY-TITLE TEXT-104 INTO W_HEAD03.               "TR667
  ELSE.                                                         "TR667
     CONCATENATE SY-TITLE TEXT-105 P_VERSN+1(2) ')'             "TR667
              INTO W_HEAD03 SEPARATED BY SPACE.                 "TR667
  ENDIF.                                                        "TR667

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
*  VARIANT-VARIANT = PVARIANT.

* create field catalog
  REFRESH FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'DSPTAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'AUFNR'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'KTEXT'.
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'STATUS'.
          FC_STR-SELTEXT_L = TEXT-C03.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'BUKRS'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative colheader
          FC_STR-KEY     = ' '.                " Key columns -not first
          FC_STR-DDICTXT = 'L'.
     WHEN 'RESCC'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'PERBZ'.
          FC_STR-SELTEXT_L = TEXT-C06.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -notfirst
     WHEN 'URZUO'.
          FC_STR-SELTEXT_L = TEXT-C6A.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'EXTNR'.
          FC_STR-SELTEXT_L = TEXT-C6B.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
           FC_STR-KEY     = ' '.                " Key columns -not first
    WHEN 'PROZS'.
          FC_STR-SELTEXT_L = TEXT-C07.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
*          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'KOSTL'.
          FC_STR-SELTEXT_L = TEXT-C08.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'CCDES'.
          FC_STR-SELTEXT_L = TEXT-C8A.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'PS_PSP_PNR'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 " Do Sum
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'HKONT'.
          FC_STR-SELTEXT_L = TEXT-C10.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'ORNUM'.
          FC_STR-SELTEXT_L = TEXT-C11.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'GABPE'.
          FC_STR-SELTEXT_L = TEXT-C12.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN 'GABJA'.
          FC_STR-SELTEXT_L = TEXT-C13.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY     = ' '.                " Key columns -not first
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          IT_FIELDCAT  = FIELDCAT
          IS_LAYOUT    = LAYOUT
          I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = REPID
          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = DSPTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

************************************************************************
***                    ALV_TOP_OF_PAGE                               ***
************************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: F_LENGTH     TYPE I,
        KOUNT        TYPE  I.

  DATA: BEGIN OF HDATA,
        HEAD03(60) TYPE C,
        HEAD04(60) TYPE C,
        HEAD05(60) TYPE C,
        END OF HDATA.

IF S_ORDGRP[] IS INITIAL.
   KOUNT = 1.
   ASSIGN COMPONENT KOUNT OF STRUCTURE HDATA TO <FS1>.
   MOVE TEXT-090 TO <FS1>.
   LOOP AT S_AUFNR.
        F_LENGTH = STRLEN( <FS1> ) + 1.
        MOVE S_AUFNR-LOW+6(6) TO <FS1>+F_LENGTH(6).
        IF S_AUFNR-OPTION = 'BT'.
           F_LENGTH = STRLEN( <FS1> ).
           MOVE '-' TO <FS1>+F_LENGTH(1).
           ADD 1 TO F_LENGTH.
           MOVE S_AUFNR-HIGH+6(6) TO <FS1>+F_LENGTH(6).
        ENDIF.
        F_LENGTH = STRLEN( <FS1> ).
        MOVE ',' TO <FS1>+F_LENGTH(1).
        F_LENGTH = STRLEN( <FS1> ).
        IF F_LENGTH > 53.
           KOUNT = KOUNT + 1.
           IF KOUNT < 4.
              ASSIGN COMPONENT KOUNT OF STRUCTURE HDATA TO <FS1>.
           ELSE.
              EXIT.
           ENDIF.
        ENDIF.
        AT LAST.
           F_LENGTH = F_LENGTH - 1.
           MOVE '.' TO <FS1>+F_LENGTH(1).
        ENDAT.
   ENDLOOP.
ELSE.
   KOUNT = 1.
   ASSIGN COMPONENT KOUNT OF STRUCTURE HDATA TO <FS1>.
   MOVE TEXT-091 TO <FS1>.
   LOOP AT S_ORDGRP.
        F_LENGTH = STRLEN( <FS1> ) + 1.
        MOVE S_ORDGRP-LOW TO <FS1>+F_LENGTH(10).
        F_LENGTH = STRLEN( <FS1> ).
        MOVE ',' TO <FS1>+F_LENGTH(1).
        F_LENGTH = STRLEN( <FS1> ).
        IF F_LENGTH > 48.
           KOUNT = KOUNT + 1.
           IF KOUNT < 4.
              ASSIGN COMPONENT KOUNT OF STRUCTURE HDATA TO <FS1>.
           ELSE.
              EXIT.
           ENDIF.
        ENDIF.
        AT LAST.
           F_LENGTH = F_LENGTH - 1.
           MOVE '.' TO <FS1>+F_LENGTH.
        ENDAT.
   ENDLOOP.
ENDIF.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.


*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO  = W_HEAD03.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*line 2:
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Transaction code line 1.
  IF HDATA-HEAD03 <> SPACE.
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = HDATA-HEAD03.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

*Transaction code line 2
  IF HDATA-HEAD04 <> SPACE.
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = HDATA-HEAD04.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

*Transaction code line 3
  IF HDATA-HEAD05 <> SPACE.
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = HDATA-HEAD05.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.
************************************************************************
***                    END OF PROGRAM                                ***
************************************************************************
