REPORT ZKPSR005 NO STANDARD PAGE HEADING
LINE-SIZE 170 LINE-COUNT 58 MESSAGE-ID ZS.

************************************************************************
* PROGRAM: ZKPSR005
* Detailed Warehouse Activity Report: PROJECTS
* AUTHOR : NESH N. LAURENCIC   - OMNILOGIC SYSTEMS GROUP
*          MARVIN J. RADSMA    - OMNILOGIC SYSTEMS GROUP
************************************************************************
* 10/08/25 M Khan  TR850 Remove the hardcoded cost element from the
*                        program and add it to the variant screen.
*
* 00/11/15 mdemeest 4.6B Changed REFBZ to ZLENR for proper indexing into
*                        BSEG.
*
* 98/09/24 md7140   #--- Quantity field is always being processed as a
*                        positive.  Should be +/-
************************************************************************

TABLES: PROJ,                          "Project definition
        PRPS,                          "WBS element description
        COBK,                          "CO Object: Document header
        COEP,                          "CO object:  period-related line
        MSEG,                          "Document Segment: Material
        MKPF,                          "Header: Material Document
        BKPF,                          "Accounting document header
        BSEG,                          "Accounting document segment
        MAKT,                          "Material Descriptions
        SKAT,                          "G/L account master record
        EKPO.                          "Purchasing Document Item

RANGES: WBS_RNG FOR PRPS-POSID.        " WBS element range

DATA: FLAG TYPE C,                     " 1 time run subroutine flag
      PSPID LIKE MSEG-PROJN,           " Because of diff. in length
      I_AWORG(10) TYPE C,              " Function AC_document_record
      TEXT(40) TYPE C,                 " report save area
      UOM(5) TYPE C,                   " report save area
      CURRENCY(3) TYPE C,              " report save area
      POST1 LIKE PRPS-POST1,           " report save area
      ERRORFLAG TYPE C,                " error flag
      LIN TYPE D,                      " lines in table
      TMPMEINS LIKE BSEG-MEINS,        " To avoid stars when do the sum
      LINEITEM LIKE MSEG-BUZEI,        " Diff. format
      BSEGITEM LIKE MSEG-BUZEI,        " Line item in accounting doc
      MSEGITEM LIKE MSEG-BUZEI.        " This one is guesing ...?

* EXPORT TABLE FROM FUNCTION
DATA: BEGIN OF T_DOCUMENTS OCCURS 100.
        INCLUDE STRUCTURE ACC_DOC.     " Used by function
DATA: END OF T_DOCUMENTS.

DATA: BEGIN OF ITAB OCCURS 500,
        PSPID LIKE PROJ-PSPID,         " Project
        POSKI LIKE PRPS-POSKI,         " WBS ELEMENT
        MATNR LIKE MSEG-MATNR,         " Material number
        BELNR LIKE BSEG-BELNR,         " Document number
        KTEXT LIKE AUFK-KTEXT,         " Line item desc or G/L desc
        POST1 LIKE PRPS-POST1,         " WBS element description
        WERKS LIKE MSEG-WERKS,         " Plant
        LGORT LIKE MSEG-LGORT,         " Storage location
        MENGE LIKE BSEG-MENGE,         " Quantity issued or ordered
        MEINS LIKE BSEG-MEINS,         " Unit of measurment
        PSWBT LIKE BSEG-PSWBT,         " Value in CND
        PSWSL LIKE BSEG-PSWSL,         " Currency
        HKONT LIKE BSEG-HKONT,         " G/L number
        BUDAT LIKE BKPF-BUDAT,         " Document date
        USNAM LIKE BKPF-USNAM,         " User name
        BELNR_TEMP LIKE BSEG-BELNR,    " Always accounting doc. number
        BUZEI LIKE      BSEG-BUZEI,    " Line item number acc doc
      END OF ITAB.
DATA: TEMP_ITAB LIKE ITAB.

*ata: begin of iprps occurs 300,       " PRPS/COBK/COEP structure
*       kokrs like cobk-kokrs,
*       belnr like cobk-belnr,
*       refbn like cobk-refbn,
*       awtyp like cobk-awtyp,
*       buzei like coep-buzei,
*       objnr like coep-objnr,
*       refbz like coep-refbz,
*       bukrs like coep-bukrs,
*       pspnr like prps-pspnr,
*       posid like prps-posid,
*       poski like prps-poski,
*       pspid like proj-pspid,
*     end of iprps.

DATA: BEGIN OF IPRPS OCCURS 300,       " PRPS/COBK/COEP structure
        PSPID LIKE PROJ-PSPID,
        PSPNR LIKE PRPS-PSPNR,
        POSID LIKE PRPS-POSID,
        OBJNR LIKE PRPS-OBJNR,
        POSKI LIKE PRPS-POSKI,
        PKOKR LIKE PRPS-PKOKR,
        PBUKR LIKE PRPS-PBUKR,
        BELNR LIKE COEP-BELNR,
        BUZEI LIKE COEP-BUZEI,
*       refbz LIKE COEP-REFBZ,     "mdemeest 4.6b
        zlenr like coep-zlenr,     "mdemeest 4.6b
        REFBN LIKE COBK-REFBN,
        AWTYP LIKE COBK-AWTYP,
      END OF IPRPS.

DATA: BEGIN OF IMSEG,                  " MSEG structure
        MBLNR LIKE MSEG-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
        MATNR LIKE MSEG-MATNR,
        WERKS LIKE MSEG-WERKS,
        LGORT LIKE MSEG-LGORT,
        DMBTR LIKE MSEG-DMBTR,
        EBELN LIKE MSEG-EBELN,
        EBELP LIKE MSEG-EBELP,
        SAKTO LIKE MSEG-SAKTO,
      END OF IMSEG.

DATA: BEGIN OF IBSEG,                  " BSEG structure
        BELNR LIKE BSEG-BELNR,
        BUZEI LIKE BSEG-BUZEI,
        SHKZG LIKE BSEG-SHKZG,
        PSWBT LIKE BSEG-PSWBT,
        PSWSL LIKE BSEG-PSWSL,
        SGTXT LIKE BSEG-SGTXT,
        HKONT LIKE BSEG-HKONT,
        WERKS LIKE BSEG-WERKS,
        MENGE LIKE BSEG-MENGE,
        MEINS LIKE BSEG-MEINS,
      END OF IBSEG.

DATA: BEGIN OF IMKPF,                  " MKPF structure
        BUDAT LIKE MKPF-BUDAT,
        USNAM LIKE MKPF-USNAM,
        AWSYS LIKE MKPF-AWSYS,
      END OF IMKPF.

DATA: BEGIN OF IBKPF ,                  " BKPF structure
        BUDAT LIKE BKPF-BUDAT,
        USNAM LIKE BKPF-USNAM,
      END OF IBKPF.

*************************   Selection Screen ************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  SELECT-OPTIONS: SPSPID FOR PROJ-PSPID
                   MATCHCODE OBJECT PRSM OBLIGATORY.
  SELECT-OPTIONS: S_KSTAR FOR COEP-KSTAR OBLIGATORY. "TR850
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) TEXT-980.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN ULINE /1(13).
  PARAMETERS: FPOPER LIKE T009B-POPER OBLIGATORY. " Post period (from)
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-991.
    PARAMETERS:  LPOPER LIKE T009B-POPER.         " Posting period (to)
    SELECTION-SCREEN COMMENT 40(38) TEXT-990.
  SELECTION-SCREEN END OF LINE.
* Fiscal variant is hidden, default value can be easily changed
* PARAMETERS: BDATJ LIKE  T009B-BDATJ DEFAULT '1997' OBLIGATORY, " Year
  PARAMETERS: BDATJ LIKE  T009B-BDATJ DEFAULT SY-DATUM(4) OBLIGATORY,
                          PERIV LIKE  T009B-PERIV
                          DEFAULT 'K1' NO-DISPLAY. "Fiscal year variant
  SELECTION-SCREEN ULINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-992.
    PARAMETERS: P_SUMRPT    AS CHECKBOX.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

DATA: FDATE LIKE SY-DATUM,
      LDATE LIKE SY-DATUM.
*********************END-of-selection screen****************************
START-OF-SELECTION.
  PERFORM FIRST_AND_LAST_DAY_IN_PERIOD.
  MOVE BDATJ TO I_AWORG.

* Assign last period in that fiscal year
  IF LPOPER EQ SPACE.
    IF LDATE(4) EQ FDATE(4).
      MOVE  LDATE+4(2) TO LPOPER.
    ELSE.
      LPOPER = '12' + LDATE+4(2).
    ENDIF.
  ENDIF.

* get all required projects/WBS elements
  SELECT PSPID
  FROM   PROJ
  INTO   CORRESPONDING FIELDS OF IPRPS
  WHERE  PSPID IN SPSPID
  AND    LOEVM EQ SPACE.

    CONCATENATE IPRPS-PSPID(7) '0000' INTO WBS_RNG-LOW.
    CONCATENATE IPRPS-PSPID(7) '9999' INTO WBS_RNG-HIGH.

    SELECT PSPNR POSID OBJNR POSKI PKOKR PBUKR
    FROM   PRPS
    INTO   CORRESPONDING FIELDS OF IPRPS
    WHERE  POSID GE WBS_RNG-LOW
    AND    POSID LE WBS_RNG-HIGH
    AND    BELKZ EQ 'X'.

*     SELECT BELNR BUZEI REFBZ     "mdemeest 4.6b
      select belnr buzei zlenr     "mdemeest 4.6b
        FROM   COEP
        INTO   CORRESPONDING FIELDS OF IPRPS
        WHERE  OBJNR EQ IPRPS-OBJNR
*        AND  ( KSTAR BETWEEN '0000420001' AND '0000420015'    "TR850
*         OR    KSTAR BETWEEN '0000420500' AND '0000420502'    "TR850
*         OR    KSTAR BETWEEN '0000421001' AND '0000421009' )  "TR850
        AND    KSTAR IN S_KSTAR                                "TR850
        AND    GJAHR EQ BDATJ
        AND    PERIO GE FPOPER
        AND    KOKRS EQ IPRPS-PKOKR
        AND    BUKRS EQ IPRPS-PBUKR
        AND    OBJNR EQ IPRPS-OBJNR
        AND    WRTTP EQ '04'
        AND    PERIO LE LPOPER.

        SELECT SINGLE REFBN AWTYP
        FROM   COBK
        INTO   CORRESPONDING FIELDS OF IPRPS
        WHERE  KOKRS EQ IPRPS-PKOKR
        AND    BELNR EQ IPRPS-BELNR.

        APPEND IPRPS.

      ENDSELECT.
    ENDSELECT.
  ENDSELECT.
  COMMIT WORK.

* get all required projects/WBS elements
* select kokrs belnr refbn awtyp
* from   cobk
* into   corresponding fields of iprps
* where  gjahr eq bdatj.
*
*   select buzei objnr refbz bukrs
*   from   coep
*   into   corresponding fields of iprps
*   where  kokrs eq iprps-kokrs
*   and    belnr eq iprps-belnr
*   and    wrttp eq '04'
*   and    perio ge fpoper
*   and    perio le lpoper
*   and  ( kstar between '0000420001' and '0000420015'
*   or     kstar between '0000420500' and '0000420502'
*   or     kstar between '0000421001' and '0000421009' ).
*   check iprps-objnr(2) = 'PR'.
*
*     select pspnr posid poski
*     from   prps
*     into   corresponding fields of iprps
*     where  objnr eq iprps-objnr
*     and    pbukr eq iprps-bukrs
*     and    pkokr eq iprps-kokrs
*     and    belkz eq 'X'.
*
*       concatenate iprps-posid(7) '0000' into wbs_rng-low.
*       select pspid
*       from   proj
*       into   corresponding fields of iprps
*       where  pspid in spspid
*       and    pspid eq wbs_rng-low
*       and    loevm eq space.
*
*         append iprps.
*
*       endselect.
*     endselect.
*   endselect.
* endselect.
* commit work.

* Process all data currently selected
  LOOP AT IPRPS.
    LINEITEM = IPRPS-BUZEI.                       " CO doc line item
*    BSEGITEM = IPRPS-REFBZ.   "4.6B mdemeest     " BSEG line item
    bsegitem = iprps-zlenr.    "4.6B mdemeest
    MSEGITEM = LINEITEM + 1.                      " when items not match
    CASE IPRPS-AWTYP.
      WHEN 'MKPF '.
        CLEAR: MSEG, IMSEG.
        SELECT SINGLE MBLNR MJAHR MATNR WERKS LGORT
                      DMBTR EBELN EBELP SAKTO
        INTO   IMSEG
        FROM   MSEG
        WHERE  MBLNR EQ IPRPS-REFBN
        AND    MJAHR EQ BDATJ
        AND    ZEILE EQ LINEITEM
        AND    PS_PSP_PNR EQ IPRPS-PSPNR.
        IF SY-SUBRC <> 0.
          SELECT SINGLE MBLNR MJAHR MATNR WERKS LGORT
                        DMBTR EBELN EBELP SAKTO
          INTO   IMSEG
          FROM   MSEG
          WHERE  MBLNR EQ IPRPS-REFBN
          AND    MJAHR EQ BDATJ
          AND    ZEILE EQ MSEGITEM
          AND    PS_PSP_PNR EQ IPRPS-PSPNR.
        ENDIF.
        CHECK SY-SUBRC = 0.

        CLEAR: MKPF, IMKPF.
        SELECT SINGLE BUDAT USNAM AWSYS
        INTO   IMKPF
        FROM   MKPF
        WHERE  MBLNR EQ IMSEG-MBLNR
        AND    MJAHR EQ IMSEG-MJAHR.
        CHECK SY-SUBRC = 0.

        REFRESH T_DOCUMENTS.
        FLAG = 1.
        CALL FUNCTION 'AC_DOCUMENT_RECORD'
             EXPORTING
                  I_AWTYP      = 'MKPF '
                  I_AWREF      = IMSEG-MBLNR     " Material document nbr
                  I_AWORG      = I_AWORG "Year
                  I_AWSYS      = IMKPF-AWSYS
*                 I_AWTYP_INCL = ' '
*                 I_AWTYP_EXCL = ' '
                  I_BUKRS      = IPRPS-PBUKR
                  X_DIALOG     = ' '
             TABLES
                  T_DOCUMENTS  = T_DOCUMENTS
             EXCEPTIONS
                  NO_REFERENCE = 1
                  NO_DOCUMENT  = 2
                  OTHERS       = 3.

        IF SY-SUBRC = 0.
          LOOP AT T_DOCUMENTS
            WHERE AWTYP EQ 'BKPF'
            AND   BUKRS EQ IPRPS-PBUKR.
          ENDLOOP.
          IF SY-SUBRC = 0.
            PERFORM MAT_AND_AC_DOCUMENTS.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
      WHEN 'BKPF '.
        PERFORM FIND_REST_OF_AC_DOCUMENTS.
    ENDCASE.

    CLEAR: MSEG, FLAG, LINEITEM, MSEGITEM, BSEGITEM.
  ENDLOOP.

  DESCRIBE TABLE ITAB LINES LIN.
  IF LIN = 0.
    MESSAGE E019(ZS) WITH TEXT-887.                     "4.6 B Changes
*   MESSAGE E999(AT) WITH TEXT-887.
  ENDIF.

* Check currency and conver into canadian dollars
  LOOP AT ITAB.
    IF ITAB-PSWSL NE 'CAD'.
      PERFORM CONVERT_INTO_CANADIAN.
    ENDIF.
  ENDLOOP.

  SORT ITAB BY PSPID POSKI MATNR BELNR.


  FORMAT COLOR 2 INTENSIFIED OFF.
  IF P_SUMRPT = SPACE.
    LOOP AT ITAB.
      FORMAT COLOR 2 INTENSIFIED OFF.
      WRITE: /  ITAB-POSKI UNDER TEXT-222,
                ITAB-HKONT UNDER TEXT-002,
                ITAB-MATNR UNDER TEXT-003,
                ITAB-BUDAT UNDER TEXT-004,
                ITAB-BELNR UNDER TEXT-005,
                ITAB-KTEXT UNDER TEXT-006,
                ITAB-MENGE UNDER TEXT-007 NO-GAP,
                ITAB-MEINS,
                ITAB-PSWBT UNDER TEXT-008 NO-GAP,
                ITAB-PSWSL ,
                ITAB-WERKS UNDER TEXT-009,
                ITAB-LGORT UNDER TEXT-010,
                ITAB-USNAM UNDER TEXT-011.
      FORMAT RESET.
      MOVE ITAB-MEINS TO TMPMEINS.
      AT END OF MATNR.
        SUM.
        WRITE: / SY-ULINE(19) UNDER TEXT-008.
        FORMAT INTENSIFIED ON.
        WRITE: / TEXT-200 COLOR 4 UNDER TEXT-006, ITAB-MATNR COLOR 4.

        IF ITAB-MATNR NE SPACE.          " I want  sum in this case
          WRITE: ITAB-MENGE COLOR 3 UNDER TEXT-007 NO-GAP,
                 TMPMEINS COLOR 3.
        ENDIF.
        WRITE: ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP,
               TEXT-300  COLOR 3.
        SKIP.
        FORMAT RESET.
        CLEAR TMPMEINS.
      ENDAT.
      AT END OF POSKI.
        FORMAT INTENSIFIED ON.
        SUM.
        WRITE: / SY-ULINE(19) UNDER TEXT-008.
        SKIP.
        WRITE: / TEXT-202 COLOR 3  INTENSIFIED ON UNDER TEXT-222 ,
                   ITAB-POSKI COLOR 3 INTENSIFIED ON.
        WRITE:  ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP INTENSIFIED ON,
                TEXT-300  COLOR 3 INTENSIFIED ON.
        SKIP.
        FORMAT RESET.
      ENDAT.
      AT END OF PSPID.
        SUM.
        WRITE: / SY-ULINE(19) UNDER TEXT-008.
        SKIP.
        WRITE: /62 TEXT-203 COLOR 5 INTENSIFIED ON,
                   ITAB-PSPID COLOR 5 INTENSIFIED ON.
        WRITE:  ITAB-PSWBT COLOR 5 INTENSIFIED ON UNDER TEXT-008 NO-GAP,
                TEXT-300  COLOR 5 INTENSIFIED ON.
        NEW-PAGE.
      ENDAT.
    ENDLOOP.
  ELSE.
    LOOP AT ITAB.
      MOVE :  ITAB-KTEXT TO TEXT,
              ITAB-MEINS TO UOM,
              ITAB-PSWSL TO CURRENCY,
              ITAB-POST1 TO POST1.
      AT NEW POSKI.
        WRITE:  / ITAB-POSKI COLOR 4 INTENSIFIED ON UNDER TEXT-222.
        WRITE:  / ITAB-POST1 COLOR 4 INTENSIFIED ON.
      ENDAT.
      AT NEW MATNR.
        SUM.
        WRITE:  / ITAB-MATNR UNDER TEXT-003,
                  TEXT UNDER TEXT-006,
                  ITAB-MENGE UNDER TEXT-007,
                  UOM UNDER TEXT-666,
                  ITAB-PSWBT UNDER TEXT-008,
                  CURRENCY UNDER TEXT-889.
      ENDAT.
      AT END OF POSKI.
        SUM.
        WRITE: / SY-ULINE(20) UNDER TEXT-008.
        SKIP.
        WRITE: /62 TEXT-202 COLOR 7 INTENSIFIED ON,
                   ITAB-POSKI COLOR 7 INTENSIFIED ON.
        WRITE:  ITAB-PSWBT COLOR 3 INTENSIFIED ON UNDER TEXT-008 NO-GAP,
                TEXT-300  COLOR 3 INTENSIFIED ON.
        SKIP.
      ENDAT.
      AT END OF PSPID.
        SUM.
        WRITE: / SY-ULINE(20) UNDER TEXT-008.
        SKIP.
        WRITE: /62 TEXT-203 COLOR 5,
                   ITAB-PSPID COLOR 5 INTENSIFIED ON.
        WRITE:  ITAB-PSWBT COLOR 5 INTENSIFIED ON UNDER TEXT-008 NO-GAP,
                TEXT-300  COLOR 5 INTENSIFIED ON.
        NEW-PAGE.
      ENDAT.
    ENDLOOP.
  ENDIF.

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         57 SY-TITLE COLOR 4 INTENSIFIED ON,
        144 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID, SY-SYSID.

  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  FORMAT INTENSIFIED ON.
  WRITE:   'PERIOD:' UNDER SY-TITLE,
           FPOPER COLOR 4 INTENSIFIED ON, 'TO' COLOR 4 INTENSIFIED ON.
  IF LPOPER NE SPACE.
    WRITE: LPOPER COLOR 4 INTENSIFIED ON, BDATJ COLOR 4 INTENSIFIED ON.
  ELSE.
    WRITE: TEXT-555 COLOR 4 INTENSIFIED ON, BDATJ COLOR 4
                                                         INTENSIFIED ON.
  ENDIF.

  IF P_SUMRPT = SPACE.
    WRITE: / TEXT-001 COLOR 4 INTENSIFIED ON,
             ITAB-PSPID COLOR 4 INTENSIFIED ON UNDER SY-MANDT. "PROJECT
    ULINE.
    WRITE: /1 TEXT-222,                             "WBS-element
           19 TEXT-003,                             "MATERIAL
           29 TEXT-006,                             "DESCRIPTION
           70 TEXT-007,                             "QUANTITY
           97 TEXT-008,                             "$VALUE
          118 TEXT-002,                             "G/L
          128 TEXT-004,                             "DATE
          140 TEXT-005,                             "DOC_#
          152 TEXT-009,                             "PLnt
          158 TEXT-010,                             "STlc
          164 TEXT-011.                             "USER
    ULINE.
    SKIP 3.
    FORMAT RESET.
  ELSE.
    WRITE:  /  TEXT-001, ITAB-PSPID COLOR 4 INTENSIFIED ON
               UNDER SY-REPID,                     "PROJECT
            23 POST1 COLOR 4 INTENSIFIED ON.       " Project description
    ULINE.
    WRITE:  /1 TEXT-222,                           "WBC-element
            19 TEXT-003,                           "MATERIAL
            37 TEXT-006,                           "MATERIAL DESCRIPTION
            79 TEXT-007,                           "QUANTITY
            97 TEXT-666,                           " U/M
           103 TEXT-008,                           "TOTAL ACTUALS
           120 TEXT-889.                           " Currency
    WRITE: /21 TEXT-223.                           " No.
    ULINE.
    SKIP 3.
    FORMAT RESET.
  ENDIF.

END-OF-PAGE.

***************************FORMS****************************************
*&---------------------------------------------------------------------*
*&      Form  MAT_AND_AC_DOCUMENTS
*&---------------------------------------------------------------------*
*       Finding material document and assoc.  accounting document      *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAT_AND_AC_DOCUMENTS.
  ITAB-PSPID = IPRPS-PSPID.            " PROJECT
  ITAB-POSKI = IPRPS-POSKI.            " WBS ELEMENT
  ITAB-MATNR = IMSEG-MATNR.            " Material number
  ITAB-POST1 = PROJ-POST1.             "PS: Short description
* Material description from material master or purchase order.
  IF ITAB-MATNR NE SPACE.              " If there is material number
    SELECT SINGLE * FROM MAKT
    WHERE  MATNR EQ ITAB-MATNR
    AND    SPRAS EQ SY-LANGU.
    ITAB-KTEXT = MAKT-MAKTX.
  ELSE.                         " No matnr number , I am taking from PO
    SELECT SINGLE * FROM EKPO
    WHERE  EBELN EQ IMSEG-EBELN
    AND    EBELP EQ IMSEG-EBELP.
    ITAB-KTEXT = EKPO-TXZ01.
  ENDIF.
  ITAB-WERKS = IMSEG-WERKS.            " Plant
  ITAB-LGORT = IMSEG-LGORT.            " Storage location
  ITAB-BELNR = IMSEG-MBLNR.            " Material document number

  SELECT SINGLE * FROM BSEG
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ T_DOCUMENTS-DOCNR
  AND    GJAHR EQ BDATJ
  AND    BUZEI EQ BSEGITEM
  AND    PSWBT EQ IMSEG-DMBTR
  AND    HKONT EQ IMSEG-SAKTO
  AND    MATNR EQ IMSEG-MATNR
  AND    PROJK EQ IPRPS-PSPNR.
  CHECK SY-SUBRC = 0.

  ITAB-HKONT = BSEG-HKONT.             " G/L number
*  itab-menge = bseg-menge.             " Quantity       MD7140 98/09/24
  ITAB-MEINS = BSEG-MEINS.             " Unit od measurment
  CASE BSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = BSEG-PSWBT.         " Value in CAD
      ITAB-MENGE = BSEG-MENGE.         " Quantity        MD7140 98/09/24
    WHEN 'H'.
      ITAB-PSWBT = BSEG-PSWBT * ( -1 )." Credit
      ITAB-MENGE = BSEG-MENGE * ( -1 )."                 MD7140 98/09/24
  ENDCASE.
  ITAB-PSWSL = BSEG-PSWSL.             " Currency
  ITAB-BELNR_TEMP = BSEG-BELNR.        " Accounting document number
  ITAB-BUZEI = BSEG-BUZEI.             " Line item
  ITAB-BUDAT = IMKPF-BUDAT.            "Posting date
  ITAB-USNAM = IMKPF-USNAM.            "User name
  APPEND ITAB.
  CLEAR ITAB.

ENDFORM.                               " MAT_AND_AC_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  FIND_REST_OF_AC_DOCUMENTS
*&---------------------------------------------------------------------*
*       Find rest of acc docs                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_REST_OF_AC_DOCUMENTS.
* Just valid documents, for enetered period
  SELECT SINGLE BUDAT USNAM
  INTO   IBKPF
  FROM   BKPF
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ IPRPS-REFBN
  AND    GJAHR EQ BDATJ
  AND    BUDAT BETWEEN FDATE AND LDATE
  AND    BSTAT EQ SPACE.
  CHECK SY-SUBRC = 0.

  SELECT SINGLE BELNR BUZEI SHKZG PSWBT PSWSL
                SGTXT HKONT WERKS MENGE MEINS
  INTO   IBSEG
  FROM   BSEG
  WHERE  BUKRS EQ IPRPS-PBUKR
  AND    BELNR EQ IPRPS-REFBN
  AND    GJAHR EQ BDATJ
  AND    BUZEI EQ BSEGITEM
  AND    PROJK EQ IPRPS-PSPNR.
  CHECK SY-SUBRC = 0.

  CLEAR ITAB.
  ITAB-PSPID = IPRPS-PSPID.            " PROJECT
  ITAB-POSKI = IPRPS-POSKI.            " WBS ELEMENT
  ITAB-POST1 = PROJ-POST1.             "PS: Short description
  ITAB-BELNR = IPRPS-BELNR.            " Acc doc #
  ITAB-BELNR_TEMP = IBSEG-BELNR.       " Acc doc #
  ITAB-HKONT = IBSEG-HKONT.            " G/L number
  CASE IBSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = IBSEG-PSWBT.        " Value in CAD
    WHEN 'H'.
      ITAB-PSWBT = IBSEG-PSWBT * ( -1 ) .
  ENDCASE.
  ITAB-PSWSL = IBSEG-PSWSL.            " Currency
  ITAB-MENGE = IBSEG-MENGE.            " Quantity
  ITAB-MEINS = IBSEG-MEINS.            " Unit of measure
  ITAB-WERKS = IBSEG-WERKS.            " Plant
  ITAB-BUZEI = IBSEG-BUZEI.            " Line itam in acc. document
  ITAB-BUDAT = IBKPF-BUDAT.            " Document date
  ITAB-USNAM = IBKPF-USNAM.            " User name
* Description for G/L
  IF IBSEG-SGTXT EQ SPACE.
    SELECT SINGLE * FROM SKAT
    WHERE  SPRAS EQ SY-LANGU
    AND    KTOPL EQ 'COAT'
    AND    SAKNR EQ ITAB-HKONT.
    ITAB-KTEXT = SKAT-TXT20.
  ELSE.
    ITAB-KTEXT = IBSEG-SGTXT.
  ENDIF.

  MOVE ITAB TO TEMP_ITAB.              " Temp stored header-line.

  CLEAR ITAB.
  READ TABLE ITAB WITH KEY
       PSPID      = TEMP_ITAB-PSPID
       POSKI      = TEMP_ITAB-POSKI
       HKONT      = TEMP_ITAB-HKONT
       BELNR_TEMP = TEMP_ITAB-BELNR_TEMP
       BUZEI      = TEMP_ITAB-BUZEI.

  IF SY-SUBRC <> 0.
    MOVE TEMP_ITAB TO ITAB.
    APPEND ITAB.
  ENDIF.
  CLEAR ITAB.
  CLEAR IBSEG.
ENDFORM.                               " FIND_REST_OF_AC_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  FIRST_AND_LAST_DAY_IN_PERIOD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_AND_LAST_DAY_IN_PERIOD.
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR        = BDATJ
*         I_MONMIT       = 00
            I_PERIV        = PERIV
            I_POPER        = FPOPER
       IMPORTING
            E_DATE         = FDATE
       EXCEPTIONS
            INPUT_FALSE    = 1
            T009_NOTFOUND  = 2
            T009B_NOTFOUND = 3
            OTHERS         = 4.
  IF SY-SUBRC NE 0.
    MESSAGE E019(ZS) WITH TEXT-999.                       "4.6 B Changes
*   MESSAGE E912(AT) WITH TEXT-999.
  ENDIF.
  IF LPOPER NE SPACE.
    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
         EXPORTING
              I_GJAHR        = BDATJ
*         I_MONMIT       = 00
              I_PERIV        = PERIV
              I_POPER        = LPOPER
         IMPORTING
              E_DATE         = LDATE
         EXCEPTIONS
              INPUT_FALSE    = 1
              T009_NOTFOUND  = 2
              T009B_NOTFOUND = 3
              OTHERS         = 4.
    IF SY-SUBRC <> 0.
      MESSAGE E019(ZS) WITH TEXT-999.                     "4.6 B Changes
*     MESSAGE E912(AT) WITH TEXT-999.
    ENDIF.
  ELSE.
    CALL FUNCTION 'LAST_DAY_IN_YEAR_GET'
         EXPORTING
              I_DATE         = FDATE
              I_PERIV        = PERIV
         IMPORTING
              E_DATE         = LDATE
         EXCEPTIONS
              INPUT_FALSE    = 1
              T009_NOTFOUND  = 2
              T009B_NOTFOUND = 3
              OTHERS         = 4.

  ENDIF.
ENDFORM.                               " FIRST_AND_LAST_DAY_IN_PERIOD

*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_CANADIAN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERT_INTO_CANADIAN.
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = ITAB-BUDAT
            FOREIGN_AMOUNT   = ITAB-PSWBT
            FOREIGN_CURRENCY = ITAB-PSWSL
            LOCAL_CURRENCY   = 'CAD'
*         RATE             = 0
            TYPE_OF_RATE     = 'M'
       IMPORTING
*          exchange_rate    =
*          foreign_factor   =
             LOCAL_AMOUNT     = ITAB-PSWBT
*          local_factor     =
*          exchange_ratex   =
       EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            OTHERS           = 4.
  IF SY-SUBRC = 0.
    ITAB-PSWSL = 'CAD'.
    MODIFY ITAB.
  ELSE.
    EXIT.
  ENDIF.
ENDFORM.                               " CONVERT_INTO_CANADIAN
