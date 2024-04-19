REPORT ZKPAA004 MESSAGE-ID ZF.
************************************************************************
*  Programmer: Lee Haire
*              OmniLogic Systems Group
*  Brief Description:
*  - This program is used for the assignment of FI revenue postings
*  to profitability segments in COPA.  This is accomplished through a
*  a G/L posting (transaction F-02).
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2006/06/16 mdemeest TRxxx - Added KURSF for ConTrax Payments
* 2006/04/05 gymana   4.7  Removed inrec46 layout coding since the
*                          Mercator maps are being replaced by ABAPs
*                          and the BDC layouts will now be in sync with
*                          the BBKPF/BGR00/ZBSEG structures.
* - Nov/98 - Added code for new branch field and deleted unused fields
*            for populating PA characteristic group.  This is for the
*            Separation project.
************************************************************************
* generic input record from file
DATA: INREC(717).                                "input record

* input record structure - session header
DATA: ZBGR00 LIKE BGR00.
* input record structure - document header
DATA: ZBBKPF LIKE BBKPF.
* input record structure - document line item
DATA: CBSEG LIKE ZCOPA_BSEG.
*------------------ SPECIAL NOTE ---------------------------------------
* The structure ZCOPA_BSEG was created as a copy of ZZBSEG with the
* additional fields for COPA information.  The choice of fields to
* add are taken from table TKEP8.   Any changes to this table (through
* config) directly affects the transaction that is used for creating
* the BDC in this program.  Hence, changes to the table must also be
* made manually to this structure here.
* This also may require additions or deletions to the BDC
* transaction mapping in this program.  Any affected sections of this
* program will be noted accordingly.
*
* Additional fields in ZCOPA_BSEG for COPA:
*
*        KNDNR LIKE CE11100-KNDNR,                  "Customer
*        VKORG LIKE CE11100-VKORG,                  "Sales Org.
*        VTWEG LIKE CE11100-VTWEG,                  "Dist. channel
*        SPART LIKE CE11100-SPART,                  "Division
*        WWDVN LIKE CE11100-WWDVN,                  "Division/District
*        WWSCT LIKE CE11100-WWSCT,                  "Sector
*        WWSEG LIKE CE11100-WWSEG,                  "Segment
*        WWRAT LIKE CE11100-WWRAT,                  "Rate Class
*        WWSER LIKE CE11100-WWSER,                  "Service Class
*        ARTNR LIKE CE11100-ARTNR,                  "Product number
*        PRDHA LIKE CE11100-PRDHA,                  "Product hierarchy
*        WWSUB LIKE CE11100-WWSUB,                  "Sub-Program
*        WWPRG LIKE CE11100-WWPRG,                  "Program
*        WWLOB LIKE CE11100-WWLOB,                  "Line of business
*        WWREG LIKE CE11100-WWREG,                  "Region
*-----------------------------------------------------------------------

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.
* real name of input file
DATA: INFILE(70).
* transaction code for batch input was F-02
CONSTANTS: G_TCODE LIKE TSTC-TCODE VALUE 'FB01'.
* name of batch input session.
DATA: BDCNAME LIKE ZBGR00-GROUP.
* flag: cancel processing current document?
DATA: CANCEL(1).

* logical file name
PARAMETERS: LGCLFILE LIKE FILENAME-FILEINTERN
                     DEFAULT 'ZBIS20?'.

*=======================================================================
AT SELECTION-SCREEN.
  PERFORM CHECK_FILE.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  PERFORM CREATE_BATCH_INPUT.


*-----------------------------------------------------------------------
*     FORM CHECK_FILE
*-----------------------------------------------------------------------
*  - Routine to check logical file and convert it to physical file.
*  It then attempts to open the physical file to determine if there
*  are any errors reading it.
*-----------------------------------------------------------------------
FORM CHECK_FILE.

  DATA: MSG(100).                           "open file - system message

  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = LGCLFILE
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = INFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 01.
  IF ( SY-SUBRC = 1 ).
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '001' WITH LGCLFILE.
  ENDIF.
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '002' WITH INFILE MSG.
  ENDIF.
*  CLOSE DATASET INFILE.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
* - This is the main routine of the program which reads each record
*   from the input file and creates the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.

  DATA: MSG(100),                           "open file - system message
        FIRST_DOC(1),                       "flag - first document?
        FIRST_LI(1).                        "first line item?

  REFRESH BDCDATA.

  FIRST_DOC = 'X'.
* do until we have read in the complete file.
  DO.
    READ DATASET INFILE INTO INREC.
    IF ( SY-SUBRC <> 0 ).
      EXIT.
    ENDIF.
    CASE INREC(1).
*     session header record....
      WHEN '0'.
        ZBGR00 = INREC.
        PERFORM OPEN_BATCH_SESSION.
*     document header record....
      WHEN '1'.
        CLEAR: CANCEL.
        FIRST_LI = 'X'.
*       process rest of previous document (if not first one)
        IF ( FIRST_DOC = SPACE ).
*         select F14 - Document overview
*          PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
          PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.       "F11 - post
*         pop-up window "Coding block" appears - select F8 to continue
          PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
          PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
*          PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
*          PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
          PERFORM INSERT_SESSION.
        ENDIF.
        REFRESH BDCDATA.
        ZBBKPF = INREC.
        PERFORM START_NEW_TRANSACTION.
        CLEAR: FIRST_DOC.
*     document line item record....
      WHEN '2'.

        CHECK CANCEL = SPACE.
        CBSEG = INREC.
        PERFORM START_NEXT_ITEM USING CBSEG-NEWBS CBSEG-HKONT FIRST_LI.
        PERFORM ADD_GL_LINE_ITEM.
        CLEAR: FIRST_LI.
    ENDCASE.
  ENDDO.

* select F14 - Document overview
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.             "F11 - post
* pop-up window "Coding block" appears - select F8 to continue
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
*  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
*  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
* process rest of very last document
  PERFORM INSERT_SESSION.

  CLOSE DATASET INFILE.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC = 0.
    MESSAGE ID 'ZS' TYPE 'I' NUMBER '003' WITH ZBGR00-GROUP.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.

* check system ID
  IF ( SY-MANDT <> ZBGR00-MANDT ).
     MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH
        'Input client number differs from the system client'.
  ENDIF.
* if XKEEP flag is not set to 'X' or ' ' - set it to an 'X'
  IF ( ZBGR00-XKEEP <> 'X' ) OR ( ZBGR00-XKEEP <> SPACE ).
    ZBGR00-XKEEP = 'X'.
  ENDIF.
  COMPUTE ZBGR00-START = SY-DATUM - 1.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = ZBGR00-GROUP
            HOLDDATE          = ZBGR00-START
            KEEP              = ZBGR00-XKEEP
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
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '004' WITH ZBGR00-GROUP.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = G_TCODE
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '013' WITH SY-SUBRC.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM START_NEW_TRANSACTION
*-----------------------------------------------------------------------
* - This routine provides the BDC mapping for the initial screen in
* the transaction.
*-----------------------------------------------------------------------
FORM START_NEW_TRANSACTION.

  DATA: TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                    "date in user format
        ZBUDAT(10).

* convert dates to user-default format
  TBLDAT = ZBBKPF-BLDAT.
  TBUDAT = ZBBKPF-BUDAT.
  WRITE TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE TBUDAT TO ZBUDAT DD/MM/YYYY.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' ZBBKPF-BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' ZBBKPF-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' ZBBKPF-WAERS.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' ZBBKPF-XBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-KURSF' ZBBKPF-KURSF.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' ZBBKPF-BKTXT.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM START_NEXT_ITEM
*-----------------------------------------------------------------------
* - This routine enters the posting key and account for the next line
* item.  This was put in a seperate routine for clarity as only these
* 2 fields appear on the previous screen.  The rest of the line item
* information appears on a subsequent screen in the transaction.
*
*  Parameters
*     --> F_NEWBS - Posting key
*         F_NEWKO - account number
*-----------------------------------------------------------------------
FORM START_NEXT_ITEM USING F_NEWBS F_NEWKO F_ITEM.

  PERFORM BDC_FIELD  USING 'RF05A-NEWBS' F_NEWBS.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' F_NEWKO.
* coding block appears except if it's the first line item
  IF ( F_ITEM = SPACE ).
    PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_GL_LINE_ITEM
*-----------------------------------------------------------------------
* - This is the BDC mapping for the screen when entering a G/L line
* item.
*-----------------------------------------------------------------------
FORM ADD_GL_LINE_ITEM.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

  IF ZBBKPF-WAERS = 'USD'.
    PERFORM BDC_FIELD  USING 'BSEG-DMBTR' CBSEG-WRBTR.
  ELSE.
    PERFORM BDC_FIELD  USING 'BSEG-WRBTR' CBSEG-WRBTR.
  ENDIF.

  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' CBSEG-MWSKZ.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' CBSEG-SGTXT.
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
* fill in fields on coding block screen if first time; otherwise accept
* the defaults
  PERFORM BDC_FIELD  USING 'COBL-KOSTL' CBSEG-KOSTL.
  PERFORM BDC_FIELD  USING 'COBL-MATNR' CBSEG-MATNR.
  PERFORM BDC_FIELD  USING 'COBL-AUFNR' CBSEG-AUFNR.
*   Sales Org means COPA data is required...
  IF ( CBSEG-VKORG <> SPACE ) AND ( CBSEG-VKORG <> '/' ).
*   fill in dummy profitability segment number
*   perform bdc_field  using 'COBL-PAOBJNR' '0'.
    PERFORM BDC_FIELD  USING 'DKACB-XERGO' 'X'.
*    perform bdc_field using 'BDC_okcode' 'PICK'.     "mdemeest


    PERFORM PROF_SEGMENT.
  ENDIF.
* continue - F8
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM PROF_SEGMENT
*-----------------------------------------------------------------------
* - BDC mapping for the screen(s) to enter the prof. segment info.
*-----------------------------------------------------------------------
FORM PROF_SEGMENT.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ANRE'.      "F18 - Derivation

  PERFORM BDC_SCREEN USING 'SAPLKEAK' '300'.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(1)' CBSEG-KNDNR.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(2)' CBSEG-VKORG.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(3)' CBSEG-WWBRN.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(4)' CBSEG-WWSCT.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(5)' CBSEG-WWSEG.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(6)' CBSEG-WWRAT.
  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(7)' CBSEG-WWSER.
*  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(11)'  CBSEG-WWPRG.
*  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(12)'  CBSEG-WWLOB.
*  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(13)'  CBSEG-WWDVS.
*  PERFORM BDC_FIELD  USING 'RKEAK-FIELD(14)'  CBSEG-WWREG.

* PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/18'.         "F18 - Derivation
* PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ANRE'.        "F18 - Derivation
* PERFORM BDC_SCREEN USING 'SAPLKEAK' '400'.
* PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.          "F8 - continue
 PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
 PERFORM BDC_FIELD USING 'BDC_OKCODE' 'ANRE'.


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

  CHECK FVAL <> '/' AND FVAL <> SPACE.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
