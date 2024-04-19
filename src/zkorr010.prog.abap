REPORT ZKORR010 NO STANDARD PAGE HEADING LINE-SIZE 80
                LINE-COUNT 60 MESSAGE-ID ZS.

*---------------------------------------------------------------------*
*       REPORT ZKORR010                                               *
*       AUTHOR M. Khan                                                *
*       DATE   March, 2003.                                           *
*---------------------------------------------------------------------*
*                                                                     *
*   The purpose of this program is to copy the settlement rules from  *
*   one version to another one within the same year or another year.  *
*                                                                     *
*---------------------------------------------------------------------*

TABLES:
  COAS,      "Generated Table for COAS View
  COBRB,     "Distribution Rules Settlement Rule Order Settlement
  TKVST,     "CO Version Texts
  RGSB4.     "Set Values (Structure)

DATA:
  FIELD_NAME      LIKE BDCDATA-FNAM,
  LAST_VERSN      LIKE COBRB-VERSN,
  W_PROZS         LIKE COBRB-PROZS,
  FIRST_TIME      TYPE C VALUE 'Y',
  TOTLINE         LIKE COBRB-EXTNR,
  W_SEQUENCE(2)   TYPE N,              "Sequence of Version
  TKVST_SEQUENCE  TYPE N,              "Seq. of version in TKVST
  LAST_AUFNR      LIKE COAS-AUFNR,
  CURSOR_POSITION(5)   TYPE C,
  HEAD01(45)           TYPE C.


DATA:  BEGIN OF ITAB   OCCURS 0,
        AUFNR  LIKE  COAS-AUFNR,           "Internal Order Number
        OBJNR  LIKE  COAS-OBJNR,           "Object Number
        KOSTL  LIKE  COBRB-KOSTL,          "Settlement Receiver
        PS_PSP_PNR LIKE COBRB-PS_PSP_PNR,  "Settlement Receiver
        VERSN  LIKE  COBRB-VERSN,          "Plan Version
        PROZS  LIKE  COBRB-PROZS,          "Percentage
        GABPE  LIKE  COBRB-GABPE,          "From Period
        GABJA  LIKE  COBRB-GABJA,          "From year
        GBISP  LIKE  COBRB-GBISP,          "To Period
        GBISJ  LIKE  COBRB-GBISJ,          "To Year
        KONTY  LIKE  COBRB-KONTY,          "Acct. Assign Category
        EXTNR  LIKE  COBRB-EXTNR,          "Dist. Rule Seq. Number
       END  OF  ITAB.

DATA:  BEGIN OF ITAB2 OCCURS 0,
         AUFNR  LIKE  COAS-AUFNR,           "Internal Order Number
         KOSTL  LIKE  COBRB-KOSTL,          "Settlement Receiver
         PS_PSP_PNR(14) TYPE N,             "Settlement Receiver
         PROZS  LIKE  COBRB-PROZS,          "Percentage
         KONTY(3) TYPE C,                   "Acct. Assign Category
         EXTNR  LIKE  COBRB-EXTNR,          "Seq# of Settlement Receiver
         SEQUENCE(2)  TYPE  N,              "Sequence of Plan Version
         OCCURANCE(2) TYPE  N,              "Occurance of Sett. Receiver
         RECTYP,                            "Record Type
       END  OF  ITAB2.

DATA:  BEGIN OF EXCP OCCURS 0,
         EXTYP  TYPE  N,                    "Exception Type
         AUFNR  LIKE  COAS-AUFNR,           "Internal Order Number
         VERSN  LIKE  COBRB-VERSN,          "Version
         FYEAR  LIKE  COBRB-GBISJ,          "Fiscal year
         PROZS  LIKE  COBRB-PROZS,          "%
       END  OF  EXCP.

DATA:   BEGIN OF VALTAB OCCURS 10.           "Orders for a group
        INCLUDE STRUCTURE RGSB4.
DATA:   END OF VALTAB.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

RANGES: R_AUFNR FOR COAS-AUFNR.             "Order Number

*------------------------ Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-090.
SELECT-OPTIONS:
    SORDGR  FOR RGSB4-SETNR NO INTERVALS.               "Order Group
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) TEXT-102.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS:
    SAUFNR  FOR  COAS-AUFNR.                            "Internal Orders
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-091.
PARAMETERS: B_ACTU RADIOBUTTON GROUP BUTN.             "Copy from Actual
PARAMETERS: B_PLAN RADIOBUTTON GROUP BUTN DEFAULT 'X'. "Copy from Plan
PARAMETERS: FVERSN  LIKE COBRB-VERSN,                  "Plan Version
            FPERIOD LIKE COBRB-GBISP OBLIGATORY,       "From Period
            FYEAR   LIKE COBRB-GBISJ OBLIGATORY.       "From Year
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-092.
PARAMETERS:     TVERSN  LIKE COBRB-VERSN OBLIGATORY,     "To Period
                TYEAR   LIKE COBRB-GBISJ OBLIGATORY.     "To Year
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BDC WITH FRAME TITLE TEXT-093.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) TEXT-094.   " FOR FIELD PBDC.
SELECTION-SCREEN POSITION 32.
PARAMETERS: PBDC  AS CHECKBOX DEFAULT ' '.        "BDC Session-Y/N
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BDC.

SELECTION-SCREEN END OF BLOCK BOX.

*-------------- PULLDOWNS for VARIANTS ---------------------------------
* Int. Order Groups require function call to create pulldown Selection *
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR SORDGR-LOW.
  DATA: O_TMPSET LIKE RGSB4-SETNR.
  DATA: O_SEARCHFLD LIKE T800S-SEARCHFLD.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0103'
            FIELD_NAME         = 'AUFNR'
            SEARCHFLD_REQUIRED = SPACE
            SEARCHFLD          = '*'
       IMPORTING
            SET_NAME           = SORDGR-LOW
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    SORDGR = O_TMPSET.
  ENDIF.

AT SELECTION-SCREEN.
  IF SORDGR-LOW <> SPACE AND SAUFNR-LOW <> SPACE.
     MESSAGE E019 WITH TEXT-006.
  ENDIF.
  IF B_PLAN = 'X' AND FVERSN = SPACE.
     MESSAGE E019 WITH TEXT-007.
  ENDIF.
  IF B_PLAN = 'X'.
     PERFORM CHECK_VERSION USING FVERSN.
     IF SY-SUBRC <> 0.
        MESSAGE E019 WITH TEXT-011.
     ENDIF.
  ENDIF.
  PERFORM CHECK_VERSION USING TVERSN.
  IF SY-SUBRC <> 0.
     MESSAGE E019 WITH TEXT-012.
  ENDIF.
*---------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
  IF SORDGR-LOW <> SPACE.
     PERFORM GO_BY_ORDER_GROUP.
  ELSE.
     PERFORM BUILD_DATA_TABLE.
  ENDIF.
  PERFORM GET_EXCEPTIONS_DATA.
  PERFORM GET_VERSION_SEQ_FROM_TKVST.
  PERFORM PROCESS_DATA.
  PERFORM WRITE_EXCEPTION_REPORT.
  IF NOT ITAB2[] IS INITIAL AND PBDC = 'X'.
     PERFORM OPEN_BDC.
     PERFORM BUILD_BDC_TABLE.
     PERFORM CLOSE_BDC.
  ENDIF.

************************************************************************
*                       GO_BY_ORDER_GROUP                              *
************************************************************************
FORM GO_BY_ORDER_GROUP.

DATA: SETNAME LIKE RKMAH-HNAM2,
      WA_SETNAME(30)    TYPE C,
      WA_INFO     LIKE SETINFO.

LOOP AT SORDGR.

  MOVE SORDGR+3(12) TO WA_SETNAME.
  CALL FUNCTION 'G_SET_GET_INFO'
       EXPORTING
            CLASS         = '0103'
            SETNAME       = WA_SETNAME
       IMPORTING
            INFO          = WA_INFO
       EXCEPTIONS
            SET_NOT_FOUND = 1.

 IF SY-SUBRC <> 0.
    SKIP 1.
    WRITE: /10 '**** ORDER GROUP NOT FOUND ****', WA_SETNAME.
    SKIP 1.
 ENDIF.
*-----------------------------------------------------------------------

  MOVE SORDGR+3(12) TO SETNAME.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0103'
            SETNR         = SETNAME
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = VALTAB
       EXCEPTIONS
            SET_NOT_FOUND = 1.
 IF SY-SUBRC <> 0.
    SKIP 1.
    WRITE: /10 '**** ORDER GROUP NOT FOUND ****', WA_SETNAME.
    SKIP 1.
 ELSE.
    LOOP AT VALTAB.
         REFRESH SAUFNR.
         IF VALTAB-FROM = VALTAB-TO.
            SAUFNR-SIGN   = 'I'.
            SAUFNR-OPTION = 'EQ'.
            SAUFNR-LOW    = VALTAB-FROM.
         ELSE.
            SAUFNR-SIGN   = 'I'.
            SAUFNR-OPTION = 'BT'.
            SAUFNR-LOW    = VALTAB-FROM.
            SAUFNR-HIGH   = VALTAB-TO.
         ENDIF.
         APPEND SAUFNR.
         CLEAR  SAUFNR.
         PERFORM BUILD_DATA_TABLE.
    ENDLOOP.                                     "End of VALTAB loop
  ENDIF.
ENDLOOP.                                   "End of order groups
ENDFORM.
************************************************************************
*                     GET_EXCEPTIONS_DATA                              *
************************************************************************
FORM GET_EXCEPTIONS_DATA.
SORT ITAB BY AUFNR.

*Exception type 3 - Existing Orders with no records copied
SELECT AUFNR INTO COAS-AUFNR
  FROM COAS
 WHERE PHAS1 = 'X' AND          "Release
       PHAS2 = ' ' AND
       PHAS3 = ' '.

 READ TABLE ITAB WITH KEY AUFNR = COAS-AUFNR BINARY SEARCH.
 IF SY-SUBRC <> 0.
    MOVE 3 TO EXCP-EXTYP.
    MOVE COAS-AUFNR TO EXCP-AUFNR.
    APPEND EXCP.
    CLEAR  EXCP.
 ENDIF.
ENDSELECT.
ENDFORM.
************************************************************************
*                         PROCESS_DATA                                 *
************************************************************************
FORM PROCESS_DATA.


  IF ITAB[] IS INITIAL.
     WRITE: /21 'NO DATA SELECTED FOR PROCESSING'.
     STOP.
  ENDIF.

 SORT ITAB BY AUFNR OBJNR VERSN.
   LOOP AT ITAB.

    IF ITAB-AUFNR <> LAST_AUFNR.
        MOVE 'H'         TO  ITAB2-RECTYP.
        MOVE ITAB-AUFNR  TO  ITAB2-AUFNR.
        MOVE ITAB-AUFNR  TO  LAST_AUFNR.
        CLEAR: LAST_VERSN.
        PERFORM DETERMINE_SEQ_OF_TO_VERSION.
        ITAB2-SEQUENCE = W_SEQUENCE.
        APPEND ITAB2.
        CLEAR:  ITAB2, W_SEQUENCE, TOTLINE.

        SELECT MAX( EXTNR ) INTO TOTLINE
          FROM COBRB
         WHERE OBJNR = ITAB-OBJNR
           AND VERSN = TVERSN.
    ENDIF.

     MOVE 'D'         TO  ITAB2-RECTYP.
     TOTLINE = TOTLINE + 1.
     MOVE ITAB-AUFNR  TO  ITAB2-AUFNR.
     MOVE ITAB-KOSTL  TO  ITAB2-KOSTL.
     WRITE ITAB-PS_PSP_PNR TO ITAB2-PS_PSP_PNR.
     MOVE ITAB-PROZS  TO  ITAB2-PROZS.
     MOVE TOTLINE     TO  ITAB2-EXTNR.
     ADD ITAB-PROZS   TO  W_PROZS.       "w_prozs = w_prozs + itab-prozs
     CASE ITAB-KONTY.
          WHEN 'KS'.   MOVE 'CTR' TO ITAB2-KONTY.
          WHEN 'PR'.   MOVE 'WBS' TO ITAB2-KONTY.
          WHEN 'OR'.   MOVE 'ORD' TO ITAB2-KONTY.
          WHEN 'SK'.   MOVE 'G/L' TO ITAB2-KONTY.
          WHEN OTHERS. MOVE 'CTR' TO ITAB2-KONTY.
     ENDCASE.
     APPEND ITAB2.
     CLEAR  ITAB2.

   AT END OF AUFNR.
     IF W_PROZS <> 100.                  "More than 100%
        MOVE 1          TO EXCP-EXTYP.
        MOVE ITAB-AUFNR TO EXCP-AUFNR.
        MOVE FVERSN     TO EXCP-VERSN.
        MOVE FYEAR      TO EXCP-FYEAR.
        MOVE W_PROZS    TO EXCP-PROZS.
        APPEND EXCP.
        CLEAR  EXCP.
*                                      Delete all records for this order
        DELETE ITAB2 WHERE AUFNR = ITAB-AUFNR.
        CLEAR ITAB2.
     ENDIF.
     CLEAR: W_PROZS.
   ENDAT.
  ENDLOOP.
ENDFORM.
************************************************************************
*                DETERMINE_SEQ_OF_TO_VERSION                           *
************************************************************************
FORM DETERMINE_SEQ_OF_TO_VERSION.

DATA: VERSN_COUNT TYPE I.
*1* Check if the "to version" exists in COBRB.
   SELECT COUNT(*) INTO VERSN_COUNT
     FROM COBRB
    WHERE OBJNR = ITAB-OBJNR AND
          VERSN = TVERSN.
   IF VERSN_COUNT = 0.
      PERFORM GET_VERSION_FROM_COBRB_N_TKVST.
   ELSE.
      PERFORM GET_VERSION_FROM_COBRB.
   ENDIF.

ENDFORM.
************************************************************************
*                GET_VERSION_FROM_COBRB_N_TKVST                      *
************************************************************************
FORM GET_VERSION_FROM_COBRB_N_TKVST.

     SELECT VERSN INTO COBRB-VERSN
       FROM COBRB
      WHERE OBJNR = ITAB-OBJNR AND
            VERSN GE TVERSN
      ORDER BY VERSN.
      IF COBRB-VERSN <> LAST_VERSN.
         W_SEQUENCE =  W_SEQUENCE + 1.
         LAST_VERSN =  COBRB-VERSN.
      ENDIF.
    ENDSELECT.

    W_SEQUENCE = W_SEQUENCE + TKVST_SEQUENCE.

ENDFORM.
*
************************************************************************
*                GET_VERSION_FROM_COBRB                                *
************************************************************************
FORM GET_VERSION_FROM_COBRB.

     SELECT VERSN INTO COBRB-VERSN
       FROM COBRB
      WHERE OBJNR = ITAB-OBJNR AND
            VERSN LE TVERSN
      ORDER BY VERSN.
      IF COBRB-VERSN <> LAST_VERSN.
         W_SEQUENCE =  W_SEQUENCE + 1.
         LAST_VERSN =  COBRB-VERSN.
      ENDIF.
      ENDSELECT.

ENDFORM.

************************************************************************
*           GET_VERSION_SEQ_FROM_TKVST. (One time)                     *
************************************************************************
FORM GET_VERSION_SEQ_FROM_TKVST.

     SELECT COUNT(*) INTO TKVST_SEQUENCE
       FROM TKVST
      WHERE SPRAS = 'EN'
        AND VERSI LE TVERSN.

ENDFORM.
************************************************************************
*                      BUILD_DATA_TABLE                                *
************************************************************************
FORM BUILD_DATA_TABLE.

DATA: W_AVORG LIKE COBRB-AVORG.

  IF B_PLAN = 'X'.
     MOVE 'KOAP' TO W_AVORG.        "Copy from Plan Version
  ELSE.
     MOVE 'KOAO' TO W_AVORG.        "Copy from Actual Version
     CLEAR FVERSN.
  ENDIF.

  SELECT COAS~AUFNR  COAS~OBJNR  COBRB~KOSTL COBRB~PS_PSP_PNR
         COBRB~VERSN COBRB~PROZS COBRB~GABPE COBRB~GABJA
         COBRB~GBISP COBRB~GBISJ COBRB~KONTY
         COBRB~EXTNR
    APPENDING TABLE ITAB
    FROM COAS INNER JOIN COBRB ON COAS~OBJNR = COBRB~OBJNR
    WHERE COAS~AUFNR  IN  SAUFNR        "Order Number
      AND COBRB~VERSN EQ  FVERSN        "From Version
      AND COBRB~GBISJ EQ  FYEAR         "From Year
      AND COBRB~GBISP EQ  FPERIOD       "From Period
      AND COBRB~AVORG EQ  W_AVORG.      "Plan/Act Settltment Transaction

*Exception type 2 - Orders Provided with rules already existing
  SELECT COAS~AUFNR  COBRB~VERSN COBRB~GBISJ
    INTO (COAS-AUFNR, COBRB-VERSN, COBRB-GBISJ)
    FROM COAS INNER JOIN COBRB ON COAS~OBJNR = COBRB~OBJNR
    WHERE COAS~AUFNR  IN  SAUFNR        "Order Number
      AND COBRB~VERSN EQ  TVERSN        "From Version
      AND ( COBRB~GBISJ EQ TYEAR OR COBRB~GBISJ EQ SPACE )   "From Year
      AND COBRB~AVORG EQ  W_AVORG.      "Plan/Act Settltment Transaction

    MOVE 2 TO EXCP-EXTYP.
    MOVE COAS-AUFNR TO EXCP-AUFNR.
    MOVE COBRB-VERSN TO EXCP-VERSN.
    MOVE COBRB-GBISJ TO EXCP-FYEAR.
    APPEND EXCP.
    CLEAR  EXCP.
  ENDSELECT.

ENDFORM.
************************************************************************
*                         CHECK_VERSION                                *
************************************************************************
FORM CHECK_VERSION USING ID_VERSN.

  SELECT SINGLE * FROM TKVST
   WHERE SPRAS = 'EN'
     AND VERSI = ID_VERSN.

ENDFORM.
************************************************************************
*                      FORM   BUILD_BDC_TABLE                          *
************************************************************************
FORM BUILD_BDC_TABLE.
LOOP AT ITAB2.
  IF ITAB2-RECTYP = 'H'.
    PERFORM BDC_SCREEN USING 'SAPMKAUF'    '110'.
    PERFORM BDC_FIELD  USING 'COAS-AUFNR'   ITAB2-AUFNR.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/6'.       "Settlement Rule
    PERFORM BDC_SCREEN USING 'SAPLKOBS'    '130'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/28'.      "Plan Settlement
    PERFORM BDC_SCREEN USING 'SAPMSSY0'    '120'.
    ITAB2-SEQUENCE = ITAB2-SEQUENCE + 2.
    CONCATENATE ITAB2-SEQUENCE '/' '02' INTO CURSOR_POSITION.
    PERFORM BDC_FIELD USING 'BDC_CURSOR' CURSOR_POSITION.    "(Row,col)
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/2'.       "Double Click
  ELSE.
    IF FIRST_TIME = 'Y'.
       PERFORM BDC_SCREEN USING 'SAPLKOBS'    '130'.
       FIRST_TIME = 'N'.
    ENDIF.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-KONTY(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING FIELD_NAME  ITAB2-KONTY.
    CLEAR FIELD_NAME.

    CONCATENATE 'DKOBR-EMPGE(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    IF ITAB2-KOSTL = SPACE.
       PERFORM BDC_FIELD  USING FIELD_NAME  ITAB2-PS_PSP_PNR.
    ELSE.
       PERFORM BDC_FIELD  USING FIELD_NAME  ITAB2-KOSTL.
    ENDIF.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-PROZS(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD2 USING FIELD_NAME  ITAB2-PROZS.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-GABPE(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING FIELD_NAME  '001'.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-GABJA(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING FIELD_NAME  TYEAR.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-GBISP(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING FIELD_NAME  '012'.
    CLEAR FIELD_NAME.

    CONCATENATE 'COBRB-GBISJ(' ITAB2-EXTNR ')' INTO FIELD_NAME.
    PERFORM BDC_FIELD  USING FIELD_NAME  TYEAR.
    CLEAR FIELD_NAME.
  ENDIF.

  AT END OF AUFNR.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.       "Save
    PERFORM INSERT_BDC.
    REFRESH BDCDATA.
    FIRST_TIME = 'Y'.
  ENDAT.

ENDLOOP.
ENDFORM.
*
*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

FORM BDC_FIELD2 USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  MOVE FVAL TO BDCDATA-FVAL+0(7).
*  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM Open_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = 'ZCO_RULECOPY'
            KEEP                = 'X'
            USER                = sy-uname
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM Insert_BDC.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'KO02'
       TABLES
            DYNPROTAB      = BDCData
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
FORM CLOSE_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
     SKIP 2.
     WRITE: /25 '***** BDC SESSION CREATED *****'.
ENDFORM.
*---------------------------------------------------------------------*
*       WRITE_EXCEPTION.REPORT.                                       *
*---------------------------------------------------------------------*
FORM WRITE_EXCEPTION_REPORT.

 SORT EXCP BY EXTYP AUFNR.

 LOOP AT EXCP.
      AT NEW EXTYP.
         CASE EXCP-EXTYP.
              WHEN 1. MOVE TEXT-014  TO  HEAD01.
              WHEN 2. MOVE TEXT-015  TO  HEAD01.
              WHEN 3. MOVE TEXT-016  TO  HEAD01.
         ENDCASE.
         PERFORM EXCEPTION_HEADING.
      ENDAT.
  WRITE: /15 EXCP-AUFNR, 33 EXCP-VERSN, 49 EXCP-FYEAR,
          62 EXCP-PROZS NO-ZERO.
 ENDLOOP.
ENDFORM.
*-------------------------  TOP-OF-PAGE  -------------------------------
TOP-OF-PAGE.
   WRITE: /25 TEXT-002.
   WRITE: /25 TEXT-001.
   WRITE: /25 TEXT-002.

FORM EXCEPTION_HEADING.
  SKIP 1.
  WRITE: /20 HEAD01.
  WRITE: /20 TEXT-010.
  WRITE: /15 TEXT-003, 30 TEXT-021, 45 TEXT-028, 62 TEXT-008.
ENDFORM.

*---------------------END OF PROGRAM ----------------------------------*

