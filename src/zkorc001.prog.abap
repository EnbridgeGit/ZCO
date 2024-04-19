REPORT ZKORC001 NO STANDARD PAGE HEADING LINE-SIZE 70 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZKORC001
*    PROGRAMMER  :  Marv Radsma - OMNILOGIC SYSTEMS GROUP
*    Client      :  Union Gas Limited
*    Date        :  March 1998
*
* This ABAP will transfer the open internal order balances from CGO to
* UGL.  The CGO (sending) internal orders may not be closed and the
* UGL (receiving) orders must be present and have a status of released.
* If the receiving order is not present or has an invalid status, an
* error message will be displayed.
************************************************************************

TABLES:  COAS,                          " Controlling: Order Master
         COEP,                          " Controlling: Line Items
         COSS,                          " CO object: internal postings
         COSP.                          " CO object: external postings

FIELD-SYMBOLS: <F1> , <F2>.

DATA:    ACTIVITY(1) TYPE C,
         CHAR(11)    TYPE C,
         T_CODE(4)   TYPE C,
         T_MODE(1)   TYPE C VALUE 'N',
         ERRIND(1)   TYPE C VALUE 'N',
         OPENIND(1)  TYPE C VALUE 'N',
         SEG_CNT(3)  TYPE C VALUE 0,
         CYC_CNT(3)  TYPE C VALUE 0,
         CYC_DAT     LIKE SY-DATUM,
         XAUFNR      LIKE COAS-AUFNR,
         PRV_AUFNR   LIKE COAS-AUFNR,
         AMOUNT      LIKE COSS-WKG001,
         TOTAMT      LIKE COSS-WKG001.

DATA:    BEGIN OF ICOAS OCCURS 5000,
           AUFNR     LIKE COAS-AUFNR,                 " UGL int order no
           AUFEX     LIKE COAS-AUFEX,                 " CGO int order no
         END OF ICOAS.

DATA  : BEGIN OF BDCDATA OCCURS 100.                   "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

DATA:   BEGIN OF MESSTAB OCCURS 10.                    "Trx messages
          INCLUDE STRUCTURE BDCMSGCOLL.
DATA:   END OF MESSTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(30) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECT-OPTIONS: S_AUART    FOR COAS-AUART OBLIGATORY.
    SELECT-OPTIONS: S_AUFNR    FOR COAS-AUFNR.
    PARAMETERS: P_ERDAT        LIKE COAS-ERDAT DEFAULT '19980225'.
    PARAMETERS: P_PERIO        LIKE COEP-PERIO DEFAULT '001'.
    PARAMETERS: P_GJAHR        LIKE COSS-GJAHR DEFAULT '1998'.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-011.
      PARAMETERS: P_NOBDC      RADIOBUTTON GROUP RADI DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-015.
      PARAMETERS: P_BDC        RADIOBUTTON GROUP RADI.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-016.
      PARAMETERS: P_BDCTST     RADIOBUTTON GROUP RADI.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-018.
      PARAMETERS: P_CYCLE      LIKE RKAL1-KSCYC DEFAULT 'COM001'.
      SELECTION-SCREEN COMMENT 40(30) TEXT-019.
    SELECTION-SCREEN END OF LINE.
    PARAMETERS: P_MAXSEG(3)    TYPE N DEFAULT 100.
    PARAMETERS: P_ELEGRP       LIKE KGALK-SETNAME DEFAULT 'ALLTOTAL'.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* set up the printing of report headers with correct company name
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

*   Extract required data
START-OF-SELECTION.

* set the CO table value fields (by period) to be accumulated
  IF P_PERIO < 0
  OR P_PERIO > 12.
    MESSAGE E100 WITH 'Invalid period, please correct'.
  ENDIF.

  CHAR(11) = 'COSS-WKG000'.
  IF P_PERIO+1(1) <> SPACE.
    CHAR+9(1) = P_PERIO+1(1).
  ENDIF.
  CHAR+10(1) = P_PERIO+2(1).
  ASSIGN (CHAR) TO <F1>.

  CHAR+3(1) = 'P'.
  ASSIGN (CHAR) TO <F2>.

* get open CGO internal orders
  SELECT AUFNR AUFEX
  FROM   COAS
  INTO   TABLE ICOAS
  WHERE  AUFNR IN S_AUFNR
  AND    AUART IN S_AUART
  AND    ERDAT GE P_ERDAT
  AND    AUTYP EQ '1'
  AND    BUKRS EQ 'UGL'
  AND    PHAS3 NE 'X'
  AND    AUFEX NE SPACE.

* set some defaults
  WRITE '19980101' TO CYC_DAT.
  CYC_CNT = P_CYCLE+3(3).

* sort table by CGO order number to check for duplicates
  SORT ICOAS BY AUFEX.

* verify UGL orders and if OK, process transfer
  LOOP AT ICOAS.

    XAUFNR = ICOAS-AUFEX+8(12).

    IF XAUFNR = PRV_AUFNR.                             "duplicate orders
      WRITE: /03  ICOAS-AUFNR
           , 018  XAUFNR
           , 033  TEXT-013.
      PERFORM SHOWVLINE.
      CONTINUE.
    ENDIF.
    PRV_AUFNR = XAUFNR.

    SELECT SINGLE * FROM COAS
    WHERE  AUFNR EQ XAUFNR
    AND    BUKRS EQ 'CGO'.

    IF SY-SUBRC <> 0.                                  "order not found
      WRITE: /03  ICOAS-AUFNR
           , 018  XAUFNR
           , 033  TEXT-007.
      PERFORM SHOWVLINE.
      CONTINUE.
    ENDIF.

    IF COAS-PHAS1 <> 'X'.                              "ord not released
      WRITE: /03  ICOAS-AUFNR
           , 018  XAUFNR
           , 033  TEXT-008.
      PERFORM SHOWVLINE.
      CONTINUE.
    ENDIF.

    ACTIVITY = 'N'.
    PERFORM GET_ORDER_AMT.

    IF P_NOBDC <> 'X'
    AND ACTIVITY = 'Y'.
      IF SEG_CNT = 0.
        P_CYCLE+3(3) = CYC_CNT.
        PERFORM OPEN_BATCH_SESSION.
        OPENIND = 'Y'.
        T_CODE = 'OKKS'.
        PERFORM SET_CONTROLLING_AREA.
        T_CODE = 'KSW2'.
        PERFORM DELETE_REPOST_CYCLE.
        T_CODE = 'KSW1'.
        PERFORM CREATE_REPOST_CYCLE.
      ELSE.
        T_CODE = 'KSW2'.
        PERFORM CHANGE_REPOST_CYCLE.
      ENDIF.
      SEG_CNT = SEG_CNT + 1.
      IF SEG_CNT > P_MAXSEG.
        SEG_CNT = 0.
        T_CODE = 'KSW5'.
        PERFORM EXECUTE_REPOST_CYCLE.
        PERFORM CLOSE_BATCH_SESSION.
        OPENIND = 'N'.
        CYC_CNT = CYC_CNT + 1.
      ENDIF.
    ENDIF.
    WRITE: /03  ICOAS-AUFNR
         , 018  XAUFNR
         , 049  AMOUNT.
    PERFORM SHOWVLINE.

  ENDLOOP.

* process last cycle and close BDC session
  IF P_NOBDC <> 'X'
  AND OPENIND = 'Y'.
    T_CODE = 'KSW5'.
    PERFORM EXECUTE_REPOST_CYCLE.
    PERFORM CLOSE_BATCH_SESSION.
  ENDIF.
  ULINE.

* output report total line
  WRITE: /03  TEXT-017
       , 049  TOTAMT.
  PERFORM SHOWVLINE.
  ULINE.

END-OF-SELECTION.

*-----------------------------------------------------------------------
*   FORM GET_ORDER_AMT
*-----------------------------------------------------------------------
*  This section accumulates all the actual amounts for the order
*-----------------------------------------------------------------------
FORM GET_ORDER_AMT.

    AMOUNT = 0.

    SELECT * FROM COSS                            " internal postings
    WHERE  OBJNR EQ COAS-OBJNR
    AND    VERSN EQ '000'
    AND    GJAHR EQ P_GJAHR
    AND    VERSN EQ '000'
    AND    WRTTP EQ '04'
    AND  ( NOT KSTAR EQ '491001'
    OR     NOT KSTAR EQ '491002' ).
      IF <F1> NE 0.
        ACTIVITY = 'Y'.
        AMOUNT = AMOUNT + <F1>.
      ENDIF.
    ENDSELECT.

    SELECT * FROM COSP                            " external postings
    WHERE  OBJNR EQ COAS-OBJNR
    AND    VERSN EQ '000'
    AND    GJAHR EQ P_GJAHR
    AND    WRTTP EQ '04'
    AND  ( NOT KSTAR EQ '491001'
    OR     NOT KSTAR EQ '491002' ).
      IF <F2> NE 0.
        ACTIVITY = 'Y'.
        AMOUNT = AMOUNT + <F2>.
      ENDIF.
    ENDSELECT.

    TOTAMT = TOTAMT + AMOUNT.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM SET_CONTROLLING_AREA
*-----------------------------------------------------------------------
*  This section sets the Controlliong area
*-----------------------------------------------------------------------
FORM SET_CONTROLLING_AREA.

  PERFORM BDC_SCREEN USING 'SAPLSPO4'         '0100'.
  PERFORM BDC_FIELD  USING 'SVALD-VALUE(1)'   '10'.         " controling
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       'FURT'.       " <continue>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM DELETE_REPOST_CYCLE
*-----------------------------------------------------------------------
*  This section deletes the existing cycle if there is one
*-----------------------------------------------------------------------
FORM DELETE_REPOST_CYCLE.

* perform bdc_screen using 'SAPMKAL1'         '0103'.
* perform bdc_field  using 'RKAL1-KSCYC'      p_cycle.      " Cycle

* perform bdc_screen using 'SAPMKAL1'         '0201'.
* perform bdc_field  using 'BDC_OKCODE'       'DEL'.        " <delete>

* perform bdc_screen using 'SAPLSPO1'         '0100'.
* perform bdc_field  using 'BDC_CURSOR'       'SPOP-OPTION1'. " <yes>

* perform bdc_screen using 'SAPMKAL1'         '0103'.
* perform bdc_field  using 'BDC_OKCODE'       '/3'.         " <return>

* perform insert_session.
* refresh bdcdata.
* clear bdcdata.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM CREATE_REPOST_CYCLE
*-----------------------------------------------------------------------
*  This section creates the cycle for the transfer of internal orders
*  from CGO to UGL.  Multiple segments (orders) are inserted per cycle.
*-----------------------------------------------------------------------
FORM CREATE_REPOST_CYCLE.

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0102'.
  PERFORM BDC_FIELD  USING 'RKAL1-KSCYC'      P_CYCLE.     " Cycle
  PERFORM BDC_FIELD  USING 'T811C-SDATE'      CYC_DAT.     " Cycle date

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0201'.
  PERFORM BDC_FIELD  USING 'RKAL1-CTXT'       TEXT-003.    " text
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       'SQAP'.      " <attach sg>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0301'.
  PERFORM BDC_FIELD  USING 'KGALS-NAME'       SEG_CNT.      " segment
  PERFORM BDC_FIELD  USING 'KGALS-RRULE'      '3'.          " trace rule
  PERFORM BDC_FIELD  USING 'KGALK-VALMIN(2)'  XAUFNR.       " CGO order
  PERFORM BDC_FIELD  USING 'KGALK-SETNAME(5)' P_ELEGRP.     " elemnt grp
  PERFORM BDC_FIELD  USING 'KGALK-VALMIN(7)'  ICOAS-AUFNR.  " UGL order
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/8'.         " <tracing>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0421'.
  PERFORM BDC_FIELD  USING 'KGALF-PERCENT(1)' '100'.        " portion
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/11'.        " <save>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0421'.       " handle msg
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       'END'.        " <return>

  PERFORM INSERT_SESSION.

  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM CHANGE_REPOST_CYCLE
*-----------------------------------------------------------------------
*  This section changes the cycle for the transfer of internal orders
*  from CGO to UGL by adding segments (orders) to the cycle.
*-----------------------------------------------------------------------
FORM CHANGE_REPOST_CYCLE.

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0103'.
  PERFORM BDC_FIELD  USING 'RKAL1-KSCYC'      P_CYCLE.     " Cycle

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0201'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       'SQAP'.      " <attach sg>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0301'.
  PERFORM BDC_FIELD  USING 'KGALS-NAME'       SEG_CNT.      " segment
  PERFORM BDC_FIELD  USING 'KGALS-RRULE'      '3'.          " trace rule
  PERFORM BDC_FIELD  USING 'KGALK-VALMIN(2)'  XAUFNR.       " CGO order
  PERFORM BDC_FIELD  USING 'KGALK-SETNAME(5)' P_ELEGRP.     " elemnt grp
  PERFORM BDC_FIELD  USING 'KGALK-VALMIN(7)'  ICOAS-AUFNR.  " UGL order
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/8'.         " <tracing>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0421'.
  PERFORM BDC_FIELD  USING 'KGALF-PERCENT(1)' '100'.        " portion
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/11'.        " <save>

  PERFORM BDC_SCREEN USING 'SAPMKAL1'         '0421'.       " handle msg
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       'END'.        " <return>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM EXECUTE_REPOST_CYCLE
*-----------------------------------------------------------------------
*  This section executes the cycle which will transfer the actual
*  internal orders from CGO to UGL.
*-----------------------------------------------------------------------
FORM EXECUTE_REPOST_CYCLE.

  PERFORM BDC_SCREEN USING 'SAPMKGA2'         '0101'.
  PERFORM BDC_FIELD  USING 'RKGA2U-FROM'      P_PERIO.      " period frm
  PERFORM BDC_FIELD  USING 'RKGA2U-TO'        P_PERIO.      " period to
  PERFORM BDC_FIELD  USING 'RKGA2U-GJAHR'     '1998'.       " fiscal yr
  IF P_BDCTST = 'X'.
    PERFORM BDC_FIELD  USING 'RKGA2U-TEST'    'X'.          " test mode
  ELSE.
    PERFORM BDC_FIELD  USING 'RKGA2U-TEST'    ' '.          " test mode
  ENDIF.
  PERFORM BDC_FIELD  USING 'RKGA2U-LIST'      'X'.          " detail lst
  PERFORM BDC_FIELD  USING 'RKGA2-KSCYC(1)'   P_CYCLE.      " cycle
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/8'.         " <execute>

  PERFORM BDC_SCREEN USING 'SAPMSSY0'         '0120'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/3'.         " <return>

  PERFORM BDC_SCREEN USING 'SAPMKGA2'         '0101'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'       '/3'.         " <return>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*  this section outputs the report headings
*-----------------------------------------------------------------------
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 028 TEXT-001
            , 059 TEXT-002,  SY-PAGNO
            , 070 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 023 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 070 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     TEXT-020 UNDER SY-DATUM
            ,     P_PERIO
            , 070 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-004
            , 018 TEXT-005
            , 033 TEXT-006
            , 063 TEXT-014.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

*-----------------------------------------------------------------------
*  this section outputs vertical lines to the report
*-----------------------------------------------------------------------
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 016 SY-VLINE
            , 031 SY-VLINE
            , 070 SY-VLINE.
ENDFORM.

*-------------------------  BDC_SCREEN  --------------------------------
* This routine adds an entry to the table BDCDATA with screen
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  PROGRAM - Program name of the screen
*      DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*-------------------------  BDC_FIELD  ---------------------------------
* This routine adds an entry to the table BDCDATA with field
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  fnam - name of the field on the screen
*      fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*------------------------  OPEN_BATCH_SESSION --------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
          CLIENT              = SY-MANDT
          GROUP               = 'ZCO_INTORDER'
          KEEP                = 'X'
          USER                = SY-UNAME
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
  IF SY-SUBRC NE 0.
    MESSAGE E001.
  ENDIF.
ENDFORM.

*-------------------------  INSERT_SESSION  ----------------------------
*-----------------------------------------------------------------------
FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
          TCODE               = T_CODE
      TABLES
          DYNPROTAB           = BDCDATA
      EXCEPTIONS
          INTERNAL_ERROR      = 1
          NOT_OPEN            = 2
          QUEUE_ERROR         = 3
          TCODE_INVALID       = 4
          OTHERS              = 5.
  IF SY-SUBRC NE 0.
    MESSAGE I002 WITH 'KSW2'.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    CLOSE_BATCH_SESSION
*-----------------------------------------------------------------------
*  - This closes the batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
          NOT_OPEN            = 1
          QUEUE_ERROR         = 2
          OTHERS              = 3.
  IF SY-SUBRC NE 0.
    MESSAGE I003 WITH 'ZCO_INTORDER'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END, MY FREIND                                    *
*---------------------------------------------------------------------*
