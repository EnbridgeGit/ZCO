REPORT ZKORR009 NO STANDARD PAGE HEADING LINE-SIZE 120
                LINE-COUNT 60 MESSAGE-ID ZS.

*---------------------------------------------------------------------*
*  PROGRAM:   ZKORR015                                                *
*  AUTHOR:    Mohammad Khan                                           *
*  DATE:      JAN , 2007.                                             *
*  ISSUE LOG: TR372.                                                  *
*  DESCRIPTION:                                                       *
*---------------------------------------------------------------------*

TABLES:   AUFK,                        " Order master
          COBRB.                       " Settlement rules

DATA:     BEGIN OF ITAB OCCURS 0,
          AUFNR LIKE AUFK-AUFNR,
          VERSN LIKE COBRB-VERSN,
          EXTNR LIKE COBRB-EXTNR,
          OBJNR LIKE COBRB-OBJNR,
          GBISP LIKE COBRB-GBISP,
          GBISJ LIKE COBRB-GBISJ,
          END OF ITAB.
DATA:
  PREV_AUFNR     LIKE AUFK-AUFNR.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*------------------------ Selection Screen  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECT-OPTIONS:  SAUFNR  FOR  AUFK-AUFNR.           "Internal Orders

PARAMETERS:
   P_VERSN  LIKE COBRB-VERSN OBLIGATORY,            "Version
   P_GBISP  LIKE COBRB-GBISP OBLIGATORY,            "To Period
   P_GBISJ  LIKE COBRB-GBISJ OBLIGATORY.            "To Year

SELECTION-SCREEN END OF BLOCK BOX.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.

PERFORM GET_DATA_TOBE_UPDATED.
IF NOT ITAB[] IS INITIAL.
   PERFORM OPEN_BDC_SESSION.
   PERFORM FORMAT_BDC_DATA.
   PERFORM CLOSE_BDC_SESSION.
ELSE.
   MESSAGE E019 WITH TEXT-018.
ENDIF.
*----------------------------------------------------------------------*
*             FORM GET_DATA_TOBE_UPDATED.                              *
*----------------------------------------------------------------------*
FORM GET_DATA_TOBE_UPDATED.

  SELECT AUFK~AUFNR  COBRB~VERSN COBRB~EXTNR
         COBRB~OBJNR COBRB~GBISP COBRB~GBISJ
    INTO TABLE ITAB
    FROM AUFK INNER JOIN COBRB ON AUFK~OBJNR = COBRB~OBJNR
    WHERE AUFK~AUFNR  IN  SAUFNR        "Order Number
      AND AUFK~PHAS3   =  SPACE         "Phase - order closed
      AND AUFK~LOEKZ   =  SPACE         "Deletion Indicator
      AND COBRB~AVORG  =  'KOAP'        "Plan Settltment Transaction
      AND COBRB~GBISJ  =  SPACE         "To Year
      AND COBRB~GBISP  =  SPACE         "To Period.
      AND COBRB~VERSN  =  P_VERSN.      "Version

ENDFORM.

*---------------------------------------------------------------------*
*                 FORMAT BDC DATA                                     *
*---------------------------------------------------------------------*

FORM FORMAT_BDC_DATA.

SORT ITAB BY AUFNR VERSN.
LOOP AT ITAB.
IF ITAB-AUFNR <> PREV_AUFNR.
   MOVE ITAB-AUFNR TO PREV_AUFNR.

  PERFORM BDC_SCREEN USING 'SAPMKAUF'   '0110'.
  PERFORM BDC_FIELD  USING 'COAS-AUFNR'  ITAB-AUFNR. "Order Number
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'UEBS'.      "Master Data Button

  PERFORM BDC_SCREEN USING 'SAPMKAUF'   '0600'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ABVO'.         "Settlement Rule

  PERFORM BDC_SCREEN USING 'SAPLKOBS'   '0130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'KOAP'.         "Plan S Rules
  PERFORM BDC_SCREEN USING 'SAPLSPO4'   '0300'.
  PERFORM BDC_FIELD  USING 'SVALD-VALUE(01)' ITAB-VERSN. "Version
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'FURT'.

  PERFORM BDC_SCREEN USING 'SAPLKOBS'    '130'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'DETA'.      "GoTo --> Details
  PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.
ENDIF.

IF ITAB-GBISP = SPACE AND ITAB-GBISJ = SPACE.
   IF ITAB-EXTNR = 0.
      MOVE 1 TO ITAB-EXTNR.
   ENDIF.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'POSI'.         "Which Rule
   PERFORM BDC_SCREEN USING 'SAPLSPO4' '0300'.
   PERFORM BDC_FIELD  USING 'SVALD-VALUE(1)' ITAB-EXTNR. "Rule number
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.          "Enter
   PERFORM BDC_SCREEN USING 'SAPLKOBS' '0100'.
   PERFORM BDC_FIELD  USING 'COBRB-GBISP' P_GBISP.       "Closing Period
   PERFORM BDC_FIELD  USING 'COBRB-GBISJ' P_GBISJ.       "Closing year
ENDIF.

AT END OF AUFNR.
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
  MESSAGE S019 WITH pop_TEXT.

ENDFORM.
