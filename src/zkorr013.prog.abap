REPORT ZKORR013 NO STANDARD PAGE HEADING MESSAGE-ID ZS.

************************************************************************
*                                                                      *
*   PROGRAM:    ZKORR013                                               *
*   PROGRAMMER: Mohammad Khan                                          *
*   DATE:       May, 2007.                                             *
*                                                                      *
*   Track #:    TR407                                                  *
*                                                                      *
*   The purpose of this program is to produce:                         *
*          1- File with I/O data                                       *
*          2- File with WBS data                                       *
*                                                                      *
*                                                                      *
************************************************************************

TABLES:   AUFK,     " Order master
          PRPS.     " WBS (Work Breakdown Structure) Element Master Data

DATA: BEGIN OF ORDERS OCCURS 0,
             IORDER(6)   TYPE N,
*            AUFNR       LIKE AUFK-AUFNR,       " Order Number
      END OF ORDERS.

DATA: BEGIN OF PROJECTS OCCURS 0,
            PRJ_PRT1(2) TYPE C,
            PRJ_PRT2(2) TYPE C,
            PRJ_PRT3(3) TYPE C,
            PRJ_PRT4(4) TYPE C,
*            POSID       LIKE PRPS-POSID,       " Project Number
      END OF PROJECTS.

DATA: BEGIN OF PRPSTAB OCCURS 0,
            OBJNR       LIKE PRPS-OBJNR,
            POSID       LIKE PRPS-POSID,       " Project Number
      END OF PRPSTAB.

DATA:   MSG(80)    TYPE C.
CONSTANTS:  W_DELIMTR  TYPE C VALUE ','.

RANGES: R_STAT FOR JEST-STAT.

************************************************************************
*              Selection Screen                                        *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: P_BUKRS  FOR AUFK-BUKRS OBLIGATORY
                         NO INTERVALS DEFAULT 'UGL'.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_AUFNR FOR AUFK-AUFNR.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT  1(28) TEXT-004.
PARAMETERS: P_PHASE0   AS CHECKBOX,
            P_PHASE1   AS CHECKBOX DEFAULT 'X',
            P_PHASE2   AS CHECKBOX DEFAULT 'X ',
            P_PHASE3   AS CHECKBOX.
PARAMETERS:     FNAME1   TYPE RLGRAP-FILENAME DEFAULT TEXT-003.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS: S_PSPHI FOR PRPS-PSPHI.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT  1(28) TEXT-006.
PARAMETERS: P_CREATE   AS CHECKBOX,
            P_RELEAS   AS CHECKBOX DEFAULT 'X',
            P_TECO     AS CHECKBOX DEFAULT 'X',
            P_CLOSE    AS CHECKBOX.
PARAMETERS:     FNAME2   TYPE RLGRAP-FILENAME DEFAULT TEXT-011.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.

************************************************************************
********************** START-OF-SELECTION ******************************
************************************************************************

START-OF-SELECTION.
  PERFORM COLLECT_ORDERS_DATA.
  PERFORM COLLECT_WBS_DATA.
  REFRESH PRPSTAB.
  PERFORM CREATE_ORDERS_FILE.
  PERFORM CREATE_WBS_FILE.

END-OF-SELECTION.

************************************************************************
***                       COLLECT_ORDERS_DATA                       ****
************************************************************************
FORM COLLECT_ORDERS_DATA.

  SELECT AUFNR PHAS0 PHAS1 PHAS2 PHAS3
    INTO (AUFK-AUFNR, AUFK-PHAS0, AUFK-PHAS1, AUFK-PHAS2, AUFK-PHAS3)
    FROM AUFK
   WHERE AUFNR IN S_AUFNR
     AND BUKRS IN P_BUKRS
     AND LOEKZ EQ SPACE.

    IF ( P_PHASE0 = 'X' AND AUFK-PHAS0 = 'X' )  OR
       ( P_PHASE1 = 'X' AND AUFK-PHAS1 = 'X' )  OR
       ( P_PHASE2 = 'X' AND AUFK-PHAS2 = 'X' )  OR
       ( P_PHASE3 = 'X' AND AUFK-PHAS3 = 'X' ).

      MOVE AUFK-AUFNR+6  TO ORDERS-IORDER.

      APPEND ORDERS.
      CLEAR  ORDERS.
    ENDIF.
  ENDSELECT.

ENDFORM.                    "COLLECT_ORDERS_DATA

************************************************************************
***                       COLLECT_WBS_DATA                          ****
************************************************************************
FORM COLLECT_WBS_DATA.

  DATA: STATUS(40)    TYPE C.               "All valid status of object

  SELECT OBJNR POSID
    INTO TABLE PRPSTAB
    FROM PRPS
   WHERE PSPHI IN S_PSPHI
     AND PBUKR IN P_BUKRS
     AND LOEVM =  SPACE
     AND BELKZ =  'X'
   ORDER BY OBJNR.

  LOOP AT PRPSTAB.

    IF PRPSTAB-POSID+5(2)  CO '1234567890'.    "Eliminates templates
       CALL FUNCTION 'AIP9_STATUS_READ'        "Retrieve Status Info
        EXPORTING
             I_OBJNR = PRPSTAB-OBJNR
             I_SPRAS = SY-LANGU
        IMPORTING
             E_SYSST = STATUS
        EXCEPTIONS
             OTHERS = 1.

      IF SY-SUBRC = 0.
        IF STATUS CS 'DLFL'.
          CONTINUE.
        ELSEIF STATUS CS 'CLSD' AND P_CLOSE = 'X'.
          PERFORM FILL_PROJECT_TABLE.
        ELSEIF STATUS CS 'TECO' AND P_TECO = 'X'.
          PERFORM FILL_PROJECT_TABLE.
        ELSEIF STATUS CS 'REL' AND P_RELEAS = 'X'.
          PERFORM FILL_PROJECT_TABLE.
        ELSEIF STATUS CS 'CRTD' AND P_CREATE = 'X'.
          PERFORM FILL_PROJECT_TABLE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "COLLECT_WBS_DATA

************************************************************************
***                    FILL_PROJECT_TABLE.                          ****
************************************************************************
FORM FILL_PROJECT_TABLE.
  MOVE: PRPSTAB-POSID+0(2)  TO  PROJECTS-PRJ_PRT1,
        PRPSTAB-POSID+2(2)  TO  PROJECTS-PRJ_PRT2,
        PRPSTAB-POSID+4(3)  TO  PROJECTS-PRJ_PRT3,
        PRPSTAB-POSID+7(4)  TO  PROJECTS-PRJ_PRT4.

  APPEND PROJECTS.
  CLEAR PROJECTS.
ENDFORM.                    "FILL_PROJECT_TABLE
************************************************************************
***                    BUILD_STATUS_RANGE                           ****
************************************************************************
FORM BUILD_STATUS_RANGE USING W_STAT.
  R_STAT-SIGN   = 'I'.
  R_STAT-OPTION = 'EQ'.
  R_STAT-LOW    = W_STAT.
  APPEND R_STAT.
  CLEAR  R_STAT.
ENDFORM.                    "BUILD_STATUS_RANGE
************************************************************************
***                    CREATE_ORDERS_FILE.                          ****
************************************************************************
FORM  CREATE_ORDERS_FILE.

  OPEN DATASET FNAME1 FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE
       MESSAGE MSG.
  IF SY-SUBRC NE '0'.
    MESSAGE E002 WITH FNAME1 MSG.
    STOP.
  ENDIF.

  SORT ORDERS.
  LOOP AT ORDERS.
    TRANSFER ORDERS TO FNAME1.
*           WRITE: /1 ORDERS-AUFNR.
  ENDLOOP.

  CLOSE DATASET FNAME1.
  IF SY-SUBRC NE '0'.
    MESSAGE E019 WITH 'unsuccessful close' FNAME1 MSG.
    STOP.
  ENDIF.
ENDFORM.                    "CREATE_ORDERS_FILE

************************************************************************
***                      CREATE_WBS_FILE.                           ****
************************************************************************
FORM  CREATE_WBS_FILE.

DATA: W_PROJECT(14) TYPE C.
  OPEN DATASET FNAME2 FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE
       MESSAGE MSG.
  IF SY-SUBRC NE '0'.
    MESSAGE E002 WITH FNAME2 MSG.
    STOP.
  ENDIF.

  SORT PROJECTS.
  LOOP AT PROJECTS.
       CONCATENATE PROJECTS-PRJ_PRT1 PROJECTS-PRJ_PRT2 PROJECTS-PRJ_PRT3
                PROJECTS-PRJ_PRT4 INTO W_PROJECT SEPARATED BY W_DELIMTR.
    TRANSFER W_PROJECT TO FNAME2.
    CLEAR W_PROJECT.
*           WRITE: /1 PROJECTS-POSID.
  ENDLOOP.

  CLOSE DATASET FNAME2.
  IF SY-SUBRC NE '0'.
    MESSAGE E019 WITH 'unsuccessful close' FNAME2 MSG.
    STOP.
  ENDIF.

ENDFORM.                    "CREATE_WBS_FILE
