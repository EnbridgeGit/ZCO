REPORT ZKPAA002 MESSAGE-ID ZS.

*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - CO-PA Reversal                                                    *
*----------------------------------------------------------------------*
* 2000/06/02 modified program to populate branch, division,            *
*     GY3628 & region fields on Screen RK1M1100 0130.                  *
*                                                                      *
*----------------------------------------------------------------------*
TABLES: CE11100.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: BELNR   FOR  CE11100-BELNR.
PARAMETERS    : P_GROUP LIKE APQI-GROUPID DEFAULT 'ZPA_EXTREV'.
SELECTION-SCREEN END OF BLOCK BLOCK1.

DATA: BEGIN OF BDCDATA  OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END   OF BDCDATA.

DATA: TEMPDATE      LIKE SY-DATUM,     "Posting date
      TEMPCHR(15).                     "To hold Volume & Revenue

START-OF-SELECTION.
  PERFORM OPEN_SESSION.                "Open BDC session

* PERFORM SET_CONTROLLING_AREA.
* PERFORM CLOSE_SESSION.
* STOP.

  SELECT * FROM CE11100 WHERE BELNR IN BELNR ORDER BY BELNR.
    WRITE: / CE11100-BELNR.
    PERFORM CREATE_SESSION.
  ENDSELECT.

  PERFORM CLOSE_SESSION.               "Close BDC session

END-OF-SELECTION.


*-----------------------------------------------------------------------
*     FORM CREATE_SESSION           "Submit BDC data to session
*-----------------------------------------------------------------------
FORM CREATE_SESSION.
  CLEAR BDCDATA.
  REFRESH BDCDATA.
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0100'.
  PERFORM BDC_FIELD  USING 'CEST1-POSNR'   CE11100-POSNR.   "Item#
  WRITE CE11100-BUDAT TO TEMPDATE.                          "Post.Dt
  PERFORM BDC_FIELD  USING 'CEST1-BUDAT'   TEMPDATE.
  PERFORM BDC_FIELD  USING 'CEST1-PERDE'   CE11100-PERDE.   "Period
  PERFORM BDC_FIELD  USING 'CEST1-VRGAR'   '2'.             "Rec type
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/13'.           "Origin Data
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0108'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/5'.
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0110'.
  PERFORM BDC_FIELD  USING 'CEST1-KNDNR'   CE11100-KNDNR.   "Customer
  PERFORM BDC_FIELD  USING 'CEST1-VKORG'   CE11100-VKORG.   "Sales Org
  PERFORM BDC_FIELD  USING 'CEST1-VTWEG'   CE11100-VTWEG.   "Dist.Chanl
  PERFORM BDC_FIELD  USING 'CEST1-BUKRS'   CE11100-BUKRS.   "Comp.Cd
* WRITE CE11100-FADAT TO TEMPDATE.                          "Inv.dt
* PERFORM BDC_FIELD  USING 'CEST1-FADAT'   TEMPDATE.
  PERFORM BDC_FIELD  USING 'CEST1-FRWAE'   CE11100-FRWAE.   "Curr.key
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0120'.
  PERFORM BDC_FIELD  USING 'CEST1-ARTNR'   CE11100-ARTNR.   "Product
  PERFORM BDC_FIELD  USING 'CEST1-WERKS'   CE11100-WERKS.   "Plant
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0130'.
  PERFORM BDC_FIELD  USING 'CE11100-WWDVN' CE11100-WWDVN.   "Div/Dist
  PERFORM BDC_FIELD  USING 'CE11100-WWSCT' CE11100-WWSCT.   "Sector
  PERFORM BDC_FIELD  USING 'CE11100-WWSEG' CE11100-WWSEG.   "Segment
  PERFORM BDC_FIELD  USING 'CE11100-WWRAT' CE11100-WWRAT.   "Rate/Class
  PERFORM BDC_FIELD  USING 'CE11100-WWSER' CE11100-WWSER.   "Service cl
  PERFORM BDC_FIELD  USING 'CE11100-WWSID' CE11100-WWSID.   "Syst of or
  PERFORM BDC_FIELD  USING 'CE11100-WWBRN' CE11100-WWBRN.   "Branch
  PERFORM BDC_FIELD  USING 'CE11100-WWDVS' CE11100-WWDVS.   "District
  PERFORM BDC_FIELD  USING 'CE11100-WWRGN' CE11100-WWRGN.   "Region_
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0150'.

  CE11100-VVBVL = - CE11100-VVBVL.
  MOVE  CE11100-VVBVL TO TEMPCHR.
  PERFORM BDC_FIELD  USING 'CE11100-VVBVL' TEMPCHR.         "Billed Vol
  PERFORM BDC_FIELD  USING 'CE11100-VVBVL_ME' CE11100-VVBVL_ME."UOM

  CE11100-VVUVL = - CE11100-VVUVL.
  MOVE  CE11100-VVUVL TO TEMPCHR.
  PERFORM BDC_FIELD  USING 'CE11100-VVUVL' TEMPCHR.         "Unbilled V
  PERFORM BDC_FIELD  USING 'CE11100-VVUVL_ME' CE11100-VVUVL_ME."UOM

  CE11100-VVBRV = - CE11100-VVBRV.
  MOVE  CE11100-VVBRV TO TEMPCHR.
  PERFORM BDC_FIELD  USING 'CE11100-VVBRV' TEMPCHR.         "Billed Rev

  CE11100-VVURV = - CE11100-VVURV.
  MOVE  CE11100-VVURV TO TEMPCHR.
  PERFORM BDC_FIELD  USING 'CE11100-VVURV' TEMPCHR.         "Unbilled R
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0151'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'    'NEXT'.          "Enter
*
  PERFORM BDC_SCREEN USING 'RK1M1100'      '0152'.
  CE11100-VVCUT = - CE11100-VVCUT.
  MOVE  CE11100-VVCUT TO TEMPCHR.
  PERFORM BDC_FIELD  USING 'CE11100-VVCUT' TEMPCHR.         "# of Custo
  PERFORM BDC_FIELD  USING 'CE11100-VVCUT_ME' CE11100-VVCUT_ME."UOM

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'    '/11'.            "Save

  PERFORM INSERT_SESSION.
ENDFORM.


*-----------------------------------------------------------------------
*   FORM OPEN_SESSION
*-----------------------------------------------------------------------
* - This routine opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GROUP
*           HOLDDATE          =
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERROR       = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALID      = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine closes a batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  IF SY-SUBRC <> 0.
    WRITE: / 'BDC Close Group Error. rc=', SY-SUBRC.
    EXIT.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'KE21'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error inserting data into session.'.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM BDC_SCREEN
*-----------------------------------------------------------------------
*   Description:
*   - This routine adds an entry to the table BDCDATA with screen
*     information from a particular transaction.  This is used as part
*     of the process for creating data for batch input.
*
*   Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM BDC_FIELD
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

*----------------------------------------------------------------------*
*       Form  SET_CONTROLLING_AREA
*----------------------------------------------------------------------*
FORM SET_CONTROLLING_AREA.
  CLEAR BDCDATA.
  REFRESH BDCDATA.
*
  PERFORM BDC_SCREEN USING 'SAPMSYST'    '40'.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'KEBC'.
  PERFORM BDC_SCREEN USING 'SAPLKEA3'    '0200'.
  PERFORM BDC_FIELD  USING 'RKEA2-ERKRS' '1100'.         "Op.concern
  PERFORM BDC_FIELD  USING 'RKEA2-PA_TYPE_1' 'X'.        "Costing based
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'ENTE'.

  PERFORM BDC_SCREEN USING 'SAPMSYST'    '40'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'OKKS'.
  PERFORM BDC_SCREEN USING 'SAPLSPO4'    '0100'.
  PERFORM BDC_FIELD  USING 'TKA01-KOKRS' '10'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'ENTE'.         "Controlg area

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'KEMN'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error inserting data into session.'.
  ENDIF.

ENDFORM.
