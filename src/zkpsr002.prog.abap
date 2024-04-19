REPORT ZKPSR002 NO STANDARD PAGE HEADING
                LINE-SIZE 170 LINE-COUNT 58 MESSAGE-ID AT.
************************************************************************
* PROGRAM: ZKPSR002
* Detailed Warehouse Activity Report: Cost Centres
* AUTHOR : NESH N. LAURENCIC   - OMNILOGIC SYSTEMS GROUP
************************************************************************
* 98/09/24 md7140 #--- Added sy-sysid to top-of-page
*                      Credit qty reporting as debit qty. Fixed
************************************************************************

TABLES:
        MSEG,                          "Document Segment: Material
        MKPF,                          "Header: Material Document
        BKPF,                          "Accounting document header
        BSEG,                          "Accounting document segment
        MAKT,                          "Material Descriptions
        SKAT,                          "G/L account master record
        CSKS,                          " Cost center master
        EKPO,                          "Purchasing Document Item
        COEP,                          "CO object:  period-related line
        COBK.                          "CO Object: Document header
DATA: FLAG TYPE C.
* EXPORT TABLE FROM FUNCTION
DATA: BEGIN OF T_DOCUMENTS OCCURS 100.
        INCLUDE STRUCTURE ACC_DOC.     " Used by function
DATA: END OF T_DOCUMENTS.

DATA: I_AWORG(10) TYPE C.              " Function AC_document_record

DATA: BEGIN OF ITAB OCCURS 100,
      KOSTL LIKE CSKS-KOSTL,           "Cost center
      HKONT LIKE BSEG-HKONT,           " G/L number
      MATNR LIKE MSEG-MATNR,           " Material number
      KTEXT LIKE AUFK-KTEXT,          "Line item description or G/L desc
      WERKS LIKE MSEG-WERKS,           " Plant
      LGORT LIKE MSEG-LGORT,           " Storage location
      MENGE LIKE BSEG-MENGE,           " Quantity issued or ordered
      MEINS LIKE BSEG-MEINS,           " Unit of measurment
      PSWBT LIKE BSEG-PSWBT,           " Value in CND
      PSWSL LIKE BSEG-PSWSL,           " Currency
      BUDAT LIKE BKPF-BUDAT,           " Document date
      BELNR LIKE BSEG-BELNR,           " Document number
      USNAM LIKE BKPF-USNAM,           " User name
      BELNR_TEMP LIKE BSEG-BELNR,      " Always accounting doc. number
      BUZEI LIKE      BSEG-BUZEI,      " Line item number acc doc
      END OF ITAB.
DATA: TEMP_ITAB LIKE ITAB.
DATA: TMPMEINS LIKE ITAB-MEINS.        " To avoid stars when do the sum

DATA: FDATE LIKE SY-DATUM,             " First and Last date
      LDATE LIKE SY-DATUM.

* The next one is for Cost elements group functions
DATA: BEGIN OF SETVLUES OCCURS 1000.   "Table for element grp
        INCLUDE STRUCTURE RGSB4.
DATA: END OF SETVLUES.

* Ranges table
RANGES STABLE FOR BSEG-HKONT.

* To improve the perfomance we added this working areas
DATA: BEGIN OF IMSEG,
      MBLNR LIKE MSEG-MBLNR,
      MJAHR LIKE MSEG-MJAHR,
      ZEILE LIKE MSEG-ZEILE,
      MATNR LIKE MSEG-MATNR,
      WERKS LIKE MSEG-WERKS,
      LGORT LIKE MSEG-LGORT,
      DMBTR LIKE MSEG-DMBTR,
      EBELN LIKE MSEG-EBELN,
      EBELP LIKE MSEG-EBELP,
      SAKTO LIKE MSEG-SAKTO,
      END OF IMSEG.

* To improve the perfomance we added this working areas
DATA: BEGIN OF IMKPF,
      MBLNR LIKE MKPF-MBLNR,
      MJAHR LIKE MKPF-MJAHR,
      BUDAT LIKE MKPF-BUDAT,
      USNAM LIKE MKPF-USNAM,
      AWSYS LIKE MKPF-AWSYS,
      END OF IMKPF.
* To improve the perfomance we added this working areas
DATA: BEGIN OF IBSEG,
      BUKRS LIKE BSEG-BUKRS,
      BELNR LIKE BSEG-BELNR,
      GJAHR LIKE BSEG-GJAHR,
      BUZEI LIKE BSEG-BUZEI,
      SHKZG LIKE BSEG-SHKZG,
      PSWBT LIKE BSEG-PSWBT,
      PSWSL LIKE BSEG-PSWSL,
      SGTXT LIKE BSEG-SGTXT,
      HKONT LIKE BSEG-HKONT,
      MATNR LIKE BSEG-MATNR,
      WERKS LIKE BSEG-WERKS,
      MENGE LIKE BSEG-MENGE,
      MEINS LIKE BSEG-MEINS,
      END OF IBSEG.
* To improve the perfomance we added this working areas

DATA: BEGIN OF IBKPF ,
      BUKRS LIKE BKPF-BUKRS,
      BELNR LIKE BKPF-BELNR,
      GJAHR LIKE BKPF-GJAHR,
      BUDAT LIKE BKPF-BUDAT,
      USNAM LIKE BKPF-USNAM,
      END OF IBKPF.
* To improve the perfomance we added this working areas

DATA: BEGIN OF ICOEP,
      BELNR LIKE COEP-BELNR,
      BUZEI LIKE COEP-BUZEI,
      KSTAR LIKE COEP-KSTAR,
      REFBZ LIKE COEP-REFBZ,
      WERKS LIKE COEP-WERKS,
      MATNR LIKE COEP-MATNR,
      EBELP LIKE COEP-EBELP,
      END OF ICOEP.

DATA: LINEITEM LIKE MSEG-BUZEI,        " Diff. format
      BSEGITEM   LIKE MSEG-BUZEI,      " Line item in accounting doc
      MSEGITEM   LIKE MSEG-BUZEI.      " This one is guesing ...?



*************************   Selection Screen ***************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: SKOSTL FOR CSKS-KOSTL MATCHCODE OBJECT KOST OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_ELMGRP LIKE RGSB4-SETNR DEFAULT 'MAT_TOTAL'.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) TEXT-980.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE /1(13).
PARAMETERS: FPOPER LIKE T009B-POPER OBLIGATORY. " Posting period (from)

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) TEXT-991.
PARAMETERS:  LPOPER LIKE T009B-POPER.  " Posting period (to)
SELECTION-SCREEN COMMENT 40(43) TEXT-990.
SELECTION-SCREEN END OF LINE.
* Fiscal variant is hidden, default value can be easily changed
PARAMETERS: BDATJ LIKE  T009B-BDATJ DEFAULT '1997' OBLIGATORY, " Year
                           PERIV LIKE  T009B-PERIV
                           DEFAULT 'K1' NO-DISPLAY."Fiscal year variant

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
SELECT-OPTIONS SHKONT FOR BSEG-HKONT MATCHCODE OBJECT SAKO.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT 1(53) TEXT-333 MODIF ID TTT. " See loop at scr.
PARAMETER: P1 RADIOBUTTON GROUP R1 DEFAULT 'X',
           P2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END OF BLOCK B2.
*********************END-of-selection screen****************************
* Modify screen
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'TTT'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.
* Pull down for cost element groups. *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ELMGRP.
  DATA: O_TMPSET LIKE RGSB4-SETNR  VALUE 'MAT_TOTAL'.


  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS         = '0H'
            FIELD_NAME    = 'KSTAR'
            SEARCHFLD     = 'COAT'
       IMPORTING
            SET_NAME      = P_ELMGRP
       EXCEPTIONS
            NO_SET_PICKED = 1.

  IF SY-SUBRC = 1.
    P_ELMGRP = O_TMPSET.
  ENDIF.

***********************START OF SELECTION*******************************

START-OF-SELECTION.
  PERFORM GET_GL_RANGES.
  PERFORM FIRST_AND_LAST_DAY_IN_PERIOD.
  MOVE BDATJ TO I_AWORG.               "Year
  SELECT * FROM  CSKS WHERE
                          KOSTL IN SKOSTL AND     " Cost Centre
                          DATAB  LE FDATE.

    CLEAR: MSEG, FLAG.
*  Selection criteria G/L
    PERFORM MAKE_2_IN_1.   " stable and shkont into one ranges int table
* Assign last period in that fiscal year
    IF LPOPER EQ SPACE.
      IF LDATE(4) EQ FDATE(4).
        MOVE  LDATE+4(2) TO LPOPER.
      ELSE.
        LPOPER = '12' + LDATE+4(2).
      ENDIF.
    ENDIF.
* CO document first
    SELECT BELNR BUZEI KSTAR REFBZ WERKS MATNR EBELP
                            FROM COEP
                            INTO ICOEP
                            WHERE
                                        KOKRS EQ CSKS-KOKRS AND
                                        PERIO >= FPOPER AND
                                        PERIO <= LPOPER AND
                                        OBJNR EQ CSKS-OBJNR AND
                                        GJAHR EQ BDATJ AND
                                        WRTTP = '04' AND
                                        KSTAR IN STABLE AND
                                        BUKRS EQ CSKS-BUKRS.

      MOVE: ICOEP-BUZEI TO LINEITEM,   "Line item in co doc
            ICOEP-REFBZ TO BSEGITEM.   " Line item in bseg
      MSEGITEM = LINEITEM + 1.          "When lineitem doesnt match
      CLEAR COBK.
      SELECT SINGLE * FROM COBK WHERE
                                 KOKRS EQ CSKS-KOKRS AND
                                 BELNR EQ ICOEP-BELNR AND
                                 GJAHR EQ BDATJ.
      CHECK SY-SUBRC = 0.
      CASE COBK-AWTYP.           "Check document type
        WHEN 'MKPF '.
          CLEAR: MSEG, IMSEG.

          SELECT  SINGLE MBLNR MJAHR ZEILE MATNR WERKS LGORT DMBTR
                                                    EBELN EBELP SAKTO
                  INTO IMSEG
                  FROM MSEG WHERE

                                       MBLNR EQ COBK-REFBN AND
                                       MJAHR EQ BDATJ AND
                                       ZEILE EQ LINEITEM AND
                                       KOSTL EQ CSKS-KOSTL.
          IF SY-SUBRC <> 0.
            CLEAR SY-SUBRC.
            SELECT  SINGLE MBLNR MJAHR ZEILE MATNR WERKS LGORT DMBTR
                                                      EBELN EBELP SAKTO
                    INTO IMSEG
                    FROM MSEG WHERE

                                         MBLNR EQ COBK-REFBN AND
                                         MJAHR EQ BDATJ AND
                                         ZEILE EQ MSEGITEM AND
                                         KOSTL EQ CSKS-KOSTL.
            CHECK SY-SUBRC = 0.
          ENDIF.

          CLEAR: MKPF, IMKPF.
          SELECT SINGLE MBLNR MJAHR BUDAT USNAM AWSYS
                      INTO IMKPF
                      FROM MKPF WHERE
                                       MBLNR EQ IMSEG-MBLNR AND
                                       MJAHR EQ IMSEG-MJAHR.
          CHECK SY-SUBRC = 0.


          REFRESH T_DOCUMENTS.
          FLAG = 1.
          CALL FUNCTION 'AC_DOCUMENT_RECORD'
               EXPORTING
                    I_AWTYP      = 'MKPF '
                  I_AWREF      = IMSEG-MBLNR" Material document number
                    I_AWORG      = I_AWORG "Year
              I_AWSYS      = IMKPF-AWSYS
*         I_AWTYP_INCL = ' '
*         I_AWTYP_EXCL = ' '
                    I_BUKRS      = CSKS-BUKRS
                    X_DIALOG     = ' '
               TABLES
                    T_DOCUMENTS  = T_DOCUMENTS
               EXCEPTIONS
                    NO_REFERENCE = 1
                    NO_DOCUMENT  = 2
                    OTHERS       = 3.

          IF SY-SUBRC = 0.
            LOOP AT T_DOCUMENTS WHERE AWTYP EQ 'BKPF' AND
                                      BUKRS EQ CSKS-BUKRS.
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


      CLEAR: ICOEP, LINEITEM, BSEGITEM, MSEGITEM.
    ENDSELECT.

    CHECK SY-SUBRC = 0.

  ENDSELECT.
    IF SY-SUBRC <> 0.
      WRITE:/ TEXT-MMM, CSKS-KOSTL.
    ENDIF.

* Check currency and conver into canadian dollars
  LOOP AT ITAB.
    IF ITAB-PSWSL NE 'CAD'.
      PERFORM CONVERT_INTO_CANADIAN.
    ENDIF.
  ENDLOOP.

  SORT ITAB BY KOSTL HKONT MATNR BELNR.

* OUTPUT
  FORMAT COLOR 2 INTENSIFIED OFF.
  LOOP AT ITAB.
    FORMAT COLOR 2 INTENSIFIED OFF.
    WRITE:  /  ITAB-KOSTL UNDER TEXT-001,
               ITAB-HKONT UNDER TEXT-002,
               ITAB-MATNR UNDER TEXT-003,
               ITAB-BUDAT UNDER TEXT-004,
               ITAB-BELNR UNDER TEXT-005,
               ITAB-KTEXT UNDER TEXT-006,
               ITAB-MENGE UNDER TEXT-007 NO-GAP,
               ITAB-MEINS,
               ITAB-PSWBT UNDER TEXT-008 NO-GAP,
               ITAB-PSWSL,
               ITAB-WERKS UNDER TEXT-009,
               ITAB-LGORT UNDER TEXT-010,
               ITAB-USNAM UNDER TEXT-011.
    FORMAT RESET.
    MOVE ITAB-MEINS TO TMPMEINS.
    AT END OF MATNR.
      SUM.

      WRITE: / SY-ULINE(19) UNDER TEXT-008.

*     skip.
*TOTAL FOR MATERIAL
      FORMAT INTENSIFIED ON.

      WRITE: /60 TEXT-100 COLOR 4, ITAB-MATNR COLOR 4.
      WRITE: ITAB-MENGE COLOR 3 UNDER TEXT-007 NO-GAP,
             TMPMEINS COLOR 3.
      WRITE: ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP,
             TEXT-200  COLOR 3.
      SKIP.
      FORMAT RESET.
      CLEAR TMPMEINS.
    ENDAT.

    AT END OF HKONT.
      SUM.
      WRITE: / SY-ULINE(19) UNDER TEXT-008.
      SKIP.
*TOTAL_FOR_G/L
      FORMAT INTENSIFIED ON.
      WRITE: /55 TEXT-101 COLOR 6, ITAB-HKONT COLOR 6.
      WRITE: ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP,
             TEXT-200  COLOR 3.
      FORMAT RESET.
      SKIP.
    ENDAT.

    AT END OF KOSTL.
      SUM.
      WRITE: / SY-ULINE(19) UNDER TEXT-008.
      SKIP.
*TOTAL FOR COST CENTRE
      FORMAT INTENSIFIED ON.
      WRITE: /50 TEXT-102 COLOR 5,
                 ITAB-KOSTL COLOR 5 .
      WRITE:  ITAB-PSWBT COLOR 5  UNDER TEXT-008 NO-GAP,
              TEXT-200  COLOR 5 .
      NEW-PAGE.
    ENDAT.

  ENDLOOP.

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         57 SY-TITLE COLOR 4 INTENSIFIED ON,
        144 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID, SY-SYSID.
  WRITE: TEXT-222 UNDER SY-TITLE, P_ELMGRP COLOR 4 INTENSIFIED ON.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  FORMAT INTENSIFIED ON.
  WRITE: / 'PERIOD:' UNDER SY-TITLE,
           FPOPER COLOR 4 INTENSIFIED ON, 'TO' COLOR 4 INTENSIFIED ON.
  IF LPOPER NE SPACE.
    WRITE: LPOPER COLOR 4 INTENSIFIED ON, BDATJ COLOR 4 INTENSIFIED ON.
  ELSE.
    WRITE: TEXT-555 COLOR 4 INTENSIFIED ON, BDATJ COLOR 4
                                                         INTENSIFIED ON.
  ENDIF.
  ULINE.

  WRITE:  /1 TEXT-001,                 "COST Cntr.
          13 TEXT-002,                 "G/L
          23 TEXT-003,                 "MATERIAL
          36 TEXT-004,                 "DATE
          48 TEXT-005,                 "DOC_#
          60 TEXT-006,                 "DESCRIPTION
         105 TEXT-007,                 "QUANTITY
         126 TEXT-008,                 "___$VALUE
         151 TEXT-009,                 "PLnt
         157 TEXT-010,                 "STlc
         163 TEXT-011.                 "USER
  ULINE.
  SKIP 3.
  FORMAT RESET.

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
  ITAB-KOSTL = CSKS-KOSTL.             " Cost centre
  ITAB-MATNR = IMSEG-MATNR.            " Material number
* Mterial description from material master or purchase order
  IF ITAB-MATNR NE SPACE.              " If there is material number

    SELECT SINGLE * FROM MAKT WHERE
                                    MATNR EQ ITAB-MATNR AND
                                    SPRAS EQ SY-LANGU.
    IF SY-SUBRC <> 0 OR MAKT-MAKTX EQ SPACE.
      ITAB-KTEXT = CSKS-ABTEI.         " Material description
    ELSE.
      ITAB-KTEXT = MAKT-MAKTX.
    ENDIF.

  ELSE.                         " No matnr number , I am taking from PO
    SELECT SINGLE * FROM EKPO WHERE
                                    EBELN EQ IMSEG-EBELN AND
                                    EBELP EQ IMSEG-EBELP.
    ITAB-KTEXT = EKPO-TXZ01.
  ENDIF.

  ITAB-WERKS = IMSEG-WERKS.            " Plant
  ITAB-LGORT = IMSEG-LGORT.            " Storage location
  ITAB-BELNR = IMSEG-MBLNR.            " Material document number

*  select * from bseg up to 1 rows where
  SELECT SINGLE * FROM BSEG  WHERE
                                  BUKRS EQ CSKS-BUKRS AND
                                  BELNR EQ T_DOCUMENTS-DOCNR AND
                                  GJAHR EQ BDATJ AND
                                  BUZEI EQ BSEGITEM AND
                                  PSWBT EQ IMSEG-DMBTR AND
                                  KOSTL EQ ITAB-KOSTL AND
                                  HKONT EQ IMSEG-SAKTO AND
                                  MATNR EQ IMSEG-MATNR.
*  endselect.
  CHECK SY-SUBRC = 0.
  ITAB-HKONT = BSEG-HKONT.             " G/L number
*  itab-menge = bseg-menge.            " Quantity               98/09/24
  ITAB-MEINS = BSEG-MEINS.             " Unit od measurment
  CASE BSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = BSEG-PSWBT.         " Value in CAD
      ITAB-MENGE = BSEG-MENGE.         " Quantity               98/09/24
    WHEN 'H'.
      ITAB-PSWBT = BSEG-PSWBT * ( -1 )." Credit
      ITAB-MENGE = BSEG-MENGE * ( -1 )." Quantity               98/09/24
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
*&      Form  READ_AC_DOCUMENTS_ONLY
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM READ_AC_DOCUMENTS_ONLY.
** Just valid documents, for enetered period
*  SELECT  * FROM BKPF WHERE
*                                     BUKRS EQ CSKS-BUKRS AND
*                                     GJAHR EQ BSEG-GJAHR AND
*                                     BUDAT BETWEEN FDATE AND LDATE AND
*                                     BSTAT EQ SPACE.
*
*
** To change the range of G/L numbers do that here
*    SELECT BUKRS BELNR GJAHR BUZEI SHKZG PSWBT PSWSL SGTXT HKONT
*           MATNR WERKS MENGE MEINS
*            INTO IBSEG
*            FROM BSEG
*                     WHERE
*
*                     BUKRS EQ BKPF-BUKRS AND
*                     BELNR EQ BKPF-BELNR AND
*                     GJAHR EQ BDATJ AND
*                     KOSTL EQ CSKS-KOSTL AND
*                     HKONT IN STABLE.
**                   (
**                   HKONT BETWEEN '0000420001' AND '0000420015' OR
**                   HKONT BETWEEN '0000420500' AND '0000420502' OR
**                   HKONT BETWEEN '0000421001' AND '0000421009' ).
*
*      ITAB-KOSTL = CSKS-KOSTL.         "Cost centre
*      ITAB-BELNR = IBSEG-BELNR.        " Acc doc #
*      ITAB-HKONT = IBSEG-HKONT.        " G/L number
*      CASE IBSEG-SHKZG.
*        WHEN 'S'.
*          ITAB-PSWBT = IBSEG-PSWBT.    " Value in CAD
*        WHEN 'H'.
*          ITAB-PSWBT = IBSEG-PSWBT * ( -1 ) .
*      ENDCASE.
*      ITAB-PSWSL = IBSEG-PSWSL.        " Currency
*      ITAB-MENGE = IBSEG-MENGE.        "Quantity
*      ITAB-MEINS = IBSEG-MEINS.        "Unit of measure
*      ITAB-WERKS = IBSEG-WERKS.        " Plant
*      ITAB-BUDAT = BKPF-BUDAT.         " Document date
*      ITAB-USNAM = BKPF-USNAM.         " User name
*
** Description for G/L
*      IF IBSEG-SGTXT EQ SPACE.
*        SELECT SINGLE * FROM SKAT WHERE
*                                       SPRAS EQ SY-LANGU AND
*                                       KTOPL EQ 'COAT' AND
*                                       SAKNR EQ ITAB-HKONT.
*        ITAB-KTEXT = SKAT-TXT20.
*      ELSE.
*        ITAB-KTEXT = IBSEG-SGTXT.
*      ENDIF.
*
*      APPEND ITAB.
*      CLEAR ITAB.
*      CLEAR IBSEG.
*    ENDSELECT.
*    CHECK SY-SUBRC = 0.
*  ENDSELECT.
*  CHECK SY-SUBRC = 0.
*ENDFORM.                               " READ_AC_DOCUMENTS_ONLY
*&---------------------------------------------------------------------*
*&      Form  FIND_REST_OF_AC_DOCUMENTS
*&---------------------------------------------------------------------*
*       Find rest of acc docs                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_REST_OF_AC_DOCUMENTS.
  SELECT SINGLE BUKRS BELNR GJAHR BUDAT USNAM
          INTO IBKPF
          FROM BKPF WHERE
                                    BUKRS EQ CSKS-BUKRS AND
                                    BELNR EQ COBK-REFBN AND
                                    GJAHR EQ BDATJ AND
                                    BUDAT BETWEEN FDATE AND LDATE AND
                                    BSTAT EQ SPACE.
  CHECK SY-SUBRC = 0.

  SELECT SINGLE BUKRS BELNR GJAHR BUZEI SHKZG PSWBT PSWSL SGTXT HKONT
         MATNR WERKS MENGE MEINS
         INTO IBSEG
         FROM BSEG
                     WHERE

                              BUKRS EQ IBKPF-BUKRS AND
                              BELNR EQ IBKPF-BELNR AND
                              GJAHR EQ BDATJ AND
                              BUZEI EQ BSEGITEM AND
                              KOSTL EQ CSKS-KOSTL AND
                              HKONT IN STABLE.

  IF SY-SUBRC <> 0.
    SELECT SINGLE BUKRS BELNR GJAHR BUZEI SHKZG PSWBT PSWSL SGTXT HKONT
           MATNR WERKS MENGE MEINS
           INTO IBSEG
           FROM BSEG
                       WHERE

                                BUKRS EQ IBKPF-BUKRS AND
                                BELNR EQ IBKPF-BELNR AND
                                GJAHR EQ BDATJ AND
                                BUZEI EQ LINEITEM AND
                                KOSTL EQ CSKS-KOSTL AND
                                HKONT IN STABLE.
    CHECK SY-SUBRC = 0.
  ENDIF.


  CLEAR ITAB.
  ITAB-KOSTL = CSKS-KOSTL.             "Cost centre
  ITAB-BELNR = IBSEG-BELNR.            " Acc doc #
  ITAB-BELNR_TEMP = IBSEG-BELNR.       "Acc doc #
  ITAB-HKONT = IBSEG-HKONT.            " G/L number
  CASE IBSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = IBSEG-PSWBT.        " Value in CAD
      ITAB-MENGE = IBSEG-MENGE.        "Quantity              98/09/24
    WHEN 'H'.
      ITAB-PSWBT = IBSEG-PSWBT * ( -1 ).
      ITAB-MENGE = IBSEG-MENGE * ( -1 )."Quantity              98/09/24
  ENDCASE.
  ITAB-PSWSL = IBSEG-PSWSL.            " Currency
* itab-menge = ibseg-menge.            "Quantity              98/09/24
  ITAB-MEINS = IBSEG-MEINS.            "Unit of measure
  ITAB-WERKS = IBSEG-WERKS.            " Plant
  ITAB-BUZEI = IBSEG-BUZEI.            " Line itam in acc. document
  ITAB-BUDAT = IBKPF-BUDAT.            " Document date
  ITAB-USNAM = IBKPF-USNAM.            " User name
* Description for G/L
  IF IBSEG-SGTXT EQ SPACE.
    SELECT SINGLE * FROM SKAT WHERE
                                   SPRAS EQ SY-LANGU AND
                                   KTOPL EQ 'COAT' AND
                                   SAKNR EQ ITAB-HKONT.
    ITAB-KTEXT = SKAT-TXT20.
  ELSE.
    ITAB-KTEXT = IBSEG-SGTXT.
  ENDIF.

  MOVE ITAB TO TEMP_ITAB.              " Temp stored header-line.

  CLEAR ITAB.
  READ TABLE ITAB WITH KEY
                           KOSTL      = TEMP_ITAB-KOSTL
                           HKONT      = TEMP_ITAB-HKONT
                           PSWBT      = TEMP_ITAB-PSWBT
                           BELNR_TEMP = TEMP_ITAB-BELNR_TEMP
                           BUZEI      = TEMP_ITAB-BUZEI.

  IF SY-SUBRC <> 0.
    MOVE TEMP_ITAB TO ITAB.
    APPEND ITAB.
  ENDIF.
  CLEAR ITAB.
  CHECK SY-SUBRC = 0.
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
    MESSAGE E912(AT) WITH TEXT-999.
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
      MESSAGE E912(AT) WITH TEXT-999.
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
*&---------------------------------------------------------------------*
*&      Form  GET_G/L_RANGES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GL_RANGES.
  REFRESH SETVLUES.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
            CLASS         = '0H'
            SETNR         = P_ELMGRP
            TABLE         = 'CCSS'
       TABLES
            SET_VALUES    = SETVLUES
       EXCEPTIONS
            SET_NOT_FOUND = 1
            OTHERS        = 2.

  LOOP AT SETVLUES.
    MOVE: SETVLUES-FROM TO STABLE-LOW,
          SETVLUES-TO TO STABLE-HIGH,
          'I' TO STABLE-SIGN,
          'BT' TO STABLE-OPTION.
    APPEND STABLE.
  ENDLOOP.
ENDFORM.                               " GET_G/L_RANGES
*&---------------------------------------------------------------------*
*&      Form  MAKE_2_IN_1
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_2_IN_1.
  CASE P1.
    WHEN 'X'.
      LOOP AT SHKONT.
        MOVE-CORRESPONDING SHKONT TO STABLE.
        APPEND STABLE.
      ENDLOOP.
    WHEN SPACE.
      REFRESH STABLE.
      LOOP AT SHKONT.
        MOVE-CORRESPONDING SHKONT TO STABLE.
        APPEND STABLE.
      ENDLOOP.
  ENDCASE.
  SORT STABLE BY LOW HIGH.
ENDFORM.                               " MAKE_2_IN_1
