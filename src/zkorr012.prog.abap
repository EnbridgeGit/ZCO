REPORT ZKORR012 NO STANDARD PAGE HEADING LINE-SIZE 120
                LINE-COUNT 90 MESSAGE-ID ZS.

*----------------------------------------------------------------------*
*       REPORT: ZKORR012                                               *
*       AUTHOR: Mohammad Khan                                          *
*       DATE:   July, 2006.                                            *
*       ISSUE LOG: 880                                                 *
*----------------------------------------------------------------------*
* The purpose of this program is to close the settlement rules for     *
* existing internal orders and add new settlement rules for the same   *
* orders. It will create a BDC session to update settlement rules.     *
* The excel sheet will be used for input data.                         *
*----------------------------------------------------------------------*
*Changes:                                                              *
*Date     Track By       Description                                   *
*-------- ----- -------- ----------------------------------------------*
*10/07/12  995  M Khan  Change C: drive to H: drive with               *
*                           directory, file selection using F4         *
*10/04/08 TR555 Mohammad Program change to upload plan settlement rules*
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************

TABLES:   AUFK,                        " Order master
          COBRB.                       " Settlement rules

DATA:     BEGIN OF INREC OCCURS 0,
            AUFNR       LIKE AUFK-AUFNR,           " Order Number
            URZUO       LIKE COBRB-URZUO,          " Source Assignment
            PERBZ       LIKE COBRB-PERBZ,          " Settlement Type
            PROZS       LIKE COBRB-PROZS,          " Percentage
            KOSTL       LIKE COBRB-KOSTL,          " Cost Center
            POSID       LIKE COBL-PS_POSID,        " WBS
            HKONT       LIKE COBRB-HKONT,          " G/L Account
            ORNUM       LIKE COBRB-AUFNR,          " Receiver Order #
          END OF INREC.

DATA:     BEGIN OF CLOSRULE OCCURS 0,
          GBISP LIKE COBRB-GBISP,
          GBISJ LIKE COBRB-GBISJ,
          EXTNR LIKE COBRB-EXTNR,
          URZUO LIKE COBRB-URZUO,
          END OF CLOSRULE.
DATA:

  SRULE_COUNT(4) TYPE N VALUE 0,
  WRK_PROZS(3)   TYPE N,
  PREV_AUFNR     LIKE AUFK-AUFNR,
  BDC_FLAG       TYPE C VALUE 'Y',
  W_AVORG        LIKE COBRB-AVORG.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

TYPES: BEGIN OF KCDE_INTERN_STRUC.
          INCLUDE STRUCTURE  KCDE_CELLS.
TYPES: END OF KCDE_INTERN_STRUC.

DATA: EXCELTAB TYPE KCDE_INTERN_STRUC OCCURS 0 with header line.

*------------------------ Selection Screen  ---------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECTION-SCREEN COMMENT 20(50) TEXT-017.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
     FILE_IN LIKE RLGRAP-FILENAME
             DEFAULT 'H:\saptemp\newrules.xls'. "TR995
*            DEFAULT 'C:\saptemp\newrules.xls'. "TR995
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-007.
PARAMETERS:
   P_GBISP  LIKE COBRB-GBISP OBLIGATORY,            "To Period
   P_GBISJ  LIKE COBRB-GBISJ OBLIGATORY.            "To Year
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-008.
PARAMETERS:
   P_GABPE  LIKE COBRB-GABPE OBLIGATORY,            "From Period
   P_GABJA  LIKE COBRB-GABJA OBLIGATORY.            "From Year
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-021. "TR555
PARAMETER: B_ACTUAL RADIOBUTTON GROUP BUTN.     "Actual Rules   "TR555
PARAMETER: B_PLAN   RADIOBUTTON GROUP BUTN.     "Plan Rules     "TR555
PARAMETER: P_VERSN  LIKE COBRB-VERSN.                           "TR555
SELECTION-SCREEN END OF BLOCK BOX4.                             "TR555

SELECTION-SCREEN END OF BLOCK BOX.

*----------------------------------------------------------------------*
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE_IN.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      FILE_IN = WIT_FILENAME_TAB.
    ELSE.
      CLEAR FILE_IN.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON FILE_IN.
  PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*----------------------------- MAINLINE -------------------------------*
START-OF-SELECTION.

IF SY-BATCH = 'X'.
   MESSAGE E019 WITH TEXT-018.
   STOP.
ENDIF.

IF B_PLAN = 'X' AND P_VERSN = SPACE.
   MESSAGE E019 WITH TEXT-019.
   STOP.
ENDIF.

PERFORM UPLOAD_EXCEL_DATA.
IF NOT INREC[] IS INITIAL.
   IF B_ACTUAL = 'X'.                        "TR555
      MOVE 'KOAO' TO W_AVORG.                "TR555
   ELSE.                                     "TR555
      MOVE 'KOAP' TO W_AVORG.                "TR555
   ENDIF.                                    "TR555
   PERFORM DISPLAY_ERROR_RECORDS.
   PERFORM OPEN_BDC_SESSION.
   PERFORM FORMAT_BDC_DATA.
   PERFORM CLOSE_BDC_SESSION.
ENDIF.
*----------------------------------------------------------------------*
*             FORM UPLOAD_EXCEL_DATA.                                  *
*----------------------------------------------------------------------*
FORM UPLOAD_EXCEL_DATA.

CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
  EXPORTING
    FILENAME                      = FILE_IN
    I_BEGIN_COL                   = 1
    I_BEGIN_ROW                   = 2
    I_END_COL                     = 16
    I_END_ROW                     = 9999
  TABLES
    INTERN                        = EXCELTAB
 EXCEPTIONS
   INCONSISTENT_PARAMETERS       = 1
   UPLOAD_OLE                    = 2
   OTHERS                        = 3
          .
*IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

 IF SY-SUBRC <> 0.
    SKIP 2.
       WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL UPLOAD'.
    SKIP 2.
    STOP.
 ENDIF.

 LOOP AT EXCELTAB.
      CASE EXCELTAB-COL.
          WHEN 1.  CONCATENATE '000000' EXCELTAB-VALUE INTO INREC-AUFNR.
          WHEN 6.  MOVE EXCELTAB-VALUE TO INREC-PERBZ.
          WHEN 7.  MOVE EXCELTAB-VALUE TO INREC-URZUO.
          WHEN 9. MOVE EXCELTAB-VALUE TO INREC-KOSTL.
          WHEN 11. MOVE EXCELTAB-VALUE TO INREC-POSID.
          WHEN 12. MOVE EXCELTAB-VALUE TO INREC-HKONT.
          WHEN 13. MOVE EXCELTAB-VALUE TO INREC-ORNUM.
          WHEN 16.  MOVE EXCELTAB-VALUE TO INREC-PROZS.
          WHEN OTHERS.
      ENDCASE.
      AT END OF ROW.
         APPEND INREC.
         CLEAR  INREC.
      ENDAT.
 ENDLOOP.

 ENDFORM.
*---------------------------------------------------------------------*
*                 FORMAT BDC DATA                                     *
*---------------------------------------------------------------------*

FORM FORMAT_BDC_DATA.

SORT INREC BY AUFNR URZUO PERBZ.
LOOP AT INREC.
 AT NEW PERBZ.
    SELECT SINGLE OBJNR INTO AUFK-OBJNR
      FROM AUFK
     WHERE AUFNR = INREC-AUFNR.

    IF SY-SUBRC = 0.
*How many settlement rules already exist
       CLEAR SRULE_COUNT.
       SELECT COUNT(*) INTO SRULE_COUNT
         FROM COBRB
        WHERE OBJNR = AUFK-OBJNR
          AND AVORG = W_AVORG                             "TR555
          AND versn = p_versn.                            "TR555
*         AND AVORG = 'KOAO'.      "Actual   "TR555
*Which of the existing settlement rules need to be closed
       SELECT GBISP GBISJ EXTNR URZUO INTO TABLE CLOSRULE
         FROM COBRB
        WHERE OBJNR = AUFK-OBJNR
          AND AVORG = W_AVORG                             "TR555
*         AND AVORG = 'KOAO'.      "Actual                "TR555
          AND URZUO = INREC-URZUO                         "TR555
          AND versn = p_versn.                            "TR555

    ENDIF.

  PERFORM BDC_SCREEN USING 'SAPMKAUF'   '0110'.
  PERFORM BDC_FIELD  USING 'COAS-AUFNR'  INREC-AUFNR.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'UEBS'.      "Master Data Button

  PERFORM BDC_SCREEN USING 'SAPMKAUF'   '0600'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ABVO'.      "Settlement Rule

*START OF TR555 CHANGES
IF B_PLAN = 'X'.
  PERFORM BDC_SCREEN USING 'SAPLKOBS'   '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'KOAP'.         "Plan S Rules
  PERFORM BDC_SCREEN USING 'SAPLSPO4'   '0300'.
  PERFORM BDC_FIELD  USING 'SVALD-VALUE(01)' P_VERSN.   "Version
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'FURT'.
ENDIF.
*END OF TR555 CHANGES

  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'DETA'.      "GoTo --> Details
 ENDAT.

*First, close the existing rules
  IF ( INREC-AUFNR <> PREV_AUFNR ) OR
     ( INREC-AUFNR = PREV_AUFNR AND INREC-URZUO <> SPACE ).
     MOVE INREC-AUFNR TO PREV_AUFNR.
     PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.

     LOOP AT CLOSRULE.
        IF CLOSRULE-GBISP = SPACE AND CLOSRULE-GBISJ = SPACE.
           IF CLOSRULE-URZUO = INREC-URZUO.
              IF CLOSRULE-EXTNR = 0.
                 MOVE 1 TO CLOSRULE-EXTNR.
              ENDIF.
              PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'POSI'. "Which Rule
              PERFORM BDC_SCREEN USING 'SAPLSPO4' '0300'.
              PERFORM BDC_FIELD  USING 'SVALD-VALUE(1)' CLOSRULE-EXTNR.
              PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.   "Enter
              PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.
              PERFORM BDC_FIELD  USING 'COBRB-GBISP' P_GBISP.
              PERFORM BDC_FIELD  USING 'COBRB-GBISJ' P_GBISJ.
           ENDIF.
        ENDIF.

     ENDLOOP.
     CLEAR:  CLOSRULE.
     REFRESH CLOSRULE.
  ENDIF.

*Second, create new settlement rules
  SRULE_COUNT = SRULE_COUNT + 1.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/03'.        "Go Back
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'NEUR'.        "New Rule
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.

  IF INREC-KOSTL <> SPACE.
     PERFORM BDC_FIELD  USING 'COBL-KOSTL' INREC-KOSTL.   "Receiver CC
  ELSEIF INREC-POSID > '00000000'.                        "Rec WBS
     PERFORM BDC_FIELD  USING 'COBL-PS_POSID' INREC-POSID.
  ELSEIF INREC-HKONT <> SPACE.
     PERFORM BDC_FIELD  USING 'COBL-SAKNR' INREC-HKONT.  "Receiver G/L
  ELSEIF INREC-ORNUM <> SPACE.
     PERFORM BDC_FIELD  USING 'COBL-AUFNR' INREC-ORNUM.  "Receiver Order
  ENDIF.

  IF B_ACTUAL = 'X'.                                     "TR555
     PERFORM BDC_FIELD  USING 'COBRB-PERBZ' INREC-PERBZ.
  ENDIF.                                                 "TR555
  MOVE INREC-PROZS TO WRK_PROZS.
  PERFORM BDC_FIELD  USING 'COBRB-PROZS' WRK_PROZS.
  IF INREC-URZUO <> SPACE.
     PERFORM BDC_FIELD  USING 'COBRB-URZUO' INREC-URZUO.
  ENDIF.

  PERFORM BDC_FIELD  USING 'COBRB-GABPE' P_GABPE.
  PERFORM BDC_FIELD  USING 'COBRB-GABJA' P_GABJA.

  AT END OF PERBZ.
  IF B_PLAN = 'X'.                                       "TR555
     PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/03'.        "TR555
     PERFORM BDC_SCREEN USING 'SAPLKOBS'  '0130'.        "TR555
  ENDIF.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.        "Save
    PERFORM INSERT_BDC.
    REFRESH BDCDATA.
  ENDAT.
 ENDLOOP.
ENDFORM.

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

*---------------------------------------------------------------------*
*       FORM OPEN_BDC                                                 *
*---------------------------------------------------------------------*
FORM OPEN_BDC_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = TEXT-011
*            GROUP               = 'ZCO_UPDT_RUL'
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

  REFRESH BDCDATA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM INSERT_BDC                                               *
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
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CLOSE_BDC_SESSION.
DATA: POP_TEXT(35).
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.

  CONCATENATE TEXT-013 TEXT-011 INTO POP_TEXT SEPARATED BY SPACE.
  CALL FUNCTION 'POPUP_FOR_INTERACTION'
       EXPORTING
            HEADLINE = TEXT-012
            TEXT1    = ' '
            TEXT2    = POP_TEXT
            TEXT3    = ' '
            TEXT4    = TEXT-014
            BUTTON_1 = TEXT-015.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DISPLAY_ERROR_RECORDS                                    *
*---------------------------------------------------------------------*
FORM DISPLAY_ERROR_RECORDS.

  SORT INREC BY AUFNR URZUO PERBZ.
  LOOP AT INREC.
       IF INREC-PROZS = 0  OR INREC-PROZS = SPACE.
          MOVE 'N' TO BDC_FLAG.
          WRITE: /5 INREC-AUFNR, INREC-PROZS.
       ENDIF.

       AT END OF PERBZ.
          SUM.
          IF INREC-PROZS <> 100.
             MOVE 'N' TO BDC_FLAG.
             WRITE: /5 INREC-AUFNR, INREC-PROZS.
          ENDIF.
       ENDAT.
  ENDLOOP.
  IF BDC_FLAG = 'N'.
     STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      lv_bol TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = FILE_IN
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = sep_path
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
*-------------------------  TOP-OF-PAGE  -----------------------------*
TOP-OF-PAGE.
  WRITE: /6 TEXT-100.
  ULINE.
  WRITE: /5 TEXT-101, 15 TEXT-102.

*&---------------------------------------------------------------------*
