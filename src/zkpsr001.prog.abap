REPORT ZKPSR001 NO STANDARD PAGE HEADING
LINE-SIZE 170 LINE-COUNT 58 MESSAGE-ID AT.
************************************************************************
* PROGRAM: ZKPSR001
* Detailed Warehouse Activity Report: Internal Orders
* AUTHOR : NESH N. LAURENCIC   - OMNILOGIC SYSTEMS GROUP
************************************************************************
* 98/09/24 md7140 #--- Added sy-sysid to top-of-page
*                      All quantities were coming out positive even
*                      though some of them were negative
************************************************************************

TABLES: AUFK,                          "Order master data
        EKPO,                          "Purchasing Document Item
        MSEG,                          "Document Segment: Material
        MKPF,                          "Header: Material Document
        BKPF,                          "Accounting document header
        BSEG,                          "Accounting document segment
        MAKT,                          "Material Descriptions
        SKAT,                          "G/L account master record
        COEP,                          "CO object:  period-related line
        COBK.                          "CO Object: Document header
*CSKA,                         "Cost elements (data dependent on
* CSKU,                        "Cost element texts
*CSKB,                         "Cost elements (data dependent on
* TKA01,                                "Controlling areas
* T800S,                                "FI-GLX Set Table CE Group
* T800Y,                                "FI-SL Set Texts
* T800A,                                "FI-SL tables
* SETCLS,                               "Set Classes
* T004T.                                "Chart of account names

DATA: FLAG TYPE C.
* EXPORT TABLE FROM FUNCTION
DATA: BEGIN OF T_DOCUMENTS OCCURS 100.
        INCLUDE STRUCTURE ACC_DOC.     " Used by function
DATA: END OF T_DOCUMENTS.

DATA: I_AWORG(10) TYPE C.              " Function AC_document_record

DATA: BEGIN OF ITAB OCCURS 100,
      AUFNR LIKE AUFK-AUFNR,           " ITERNAL ORDER NUMBER
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
DATA: TMPMEINS LIKE ITAB-MEINS.
DATA: TEMP_ITAB LIKE ITAB.

* Ranges table
RANGES STABLE FOR BSEG-HKONT.

DATA: FDATE LIKE SY-DATUM,             " We are using this for function
      LDATE LIKE SY-DATUM.

* The next one is for Cost elements group function
*DATA: BEGIN OF SETVLUES OCCURS 1000.   "Table for element grp
*        INCLUDE STRUCTURE RGSB4.
*DATA: END OF SETVLUES.

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
DATA: BEGIN OF IBKPF ,
      BUKRS LIKE BKPF-BUKRS,
      BELNR LIKE BKPF-BELNR,
      GJAHR LIKE BKPF-GJAHR,
      BUDAT LIKE BKPF-BUDAT,
      USNAM LIKE BKPF-USNAM,
      END OF IBKPF.

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


*************************   Selection Screen ************

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: SAUFNR FOR AUFK-AUFNR MATCHCODE OBJECT ORDE OBLIGATORY.
SELECTION-SCREEN SKIP 2.
PARAMETERS: P_SET LIKE sethier-shortname MEMORY ID gse DEFAULT
            'MAT_TOTAL'.
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
PARAMETERS: BDATJ LIKE     T009B-BDATJ OBLIGATORY, " Year
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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_SET.
  DATA: O_TMPSET LIKE RGSB4-SETNR  VALUE 'MAT_TOTAL'.


  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS         = '0H'
            FIELD_NAME    = 'KSTAR'
            SEARCHFLD     = 'COAT'
       IMPORTING
            SET_NAME      = P_SET
       EXCEPTIONS
            NO_SET_PICKED = 1.

  IF SY-SUBRC = 1.
    P_SET = O_TMPSET.
  ENDIF.
***********************START OF SELECTION*******************************
START-OF-SELECTION.
  PERFORM GET_GL_RANGES.
  PERFORM FIRST_AND_LAST_DAY_IN_PERIOD.
  MOVE BDATJ TO I_AWORG.               "Year
  SELECT * FROM AUFK WHERE
                          AUFNR IN SAUFNR AND     " Order number
                          LOEKZ EQ SPACE.         " Deletion factor
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
    SELECT BELNR BUZEI KSTAR REFBZ WERKS MATNR EBELP
                            FROM COEP
                            INTO ICOEP
                            WHERE
                                        KOKRS EQ AUFK-KOKRS AND
                                        PERIO >= FPOPER AND
                                        PERIO <= LPOPER AND
                                        OBJNR EQ AUFK-OBJNR AND
                                        GJAHR EQ BDATJ AND
                                        WRTTP = '04' AND
                                        KSTAR IN STABLE AND
                                        BUKRS EQ AUFK-BUKRS.

      MOVE: ICOEP-BUZEI TO LINEITEM,   "Line item in co doc
            ICOEP-REFBZ TO BSEGITEM.   " Line item in bseg document
      MSEGITEM = LINEITEM + 1.         " When lineitem doesnt match

      CLEAR COBK.
      SELECT SINGLE * FROM COBK WHERE
                                 KOKRS EQ AUFK-KOKRS AND
                                 BELNR EQ ICOEP-BELNR AND
                                 GJAHR EQ BDATJ.

      CHECK SY-SUBRC = 0.
      CASE COBK-AWTYP.                 "Document type
        WHEN 'MKPF '.
          CLEAR: MSEG, IMSEG.

          SELECT  SINGLE MBLNR MJAHR ZEILE MATNR WERKS LGORT DMBTR
                                                    EBELN EBELP SAKTO
                  INTO IMSEG
                  FROM MSEG WHERE

                                       MBLNR EQ COBK-REFBN AND
                                       MJAHR EQ BDATJ AND
                                       ZEILE EQ LINEITEM AND
                                       AUFNR EQ AUFK-AUFNR.
          IF SY-SUBRC <> 0.
            CLEAR SY-SUBRC.
            SELECT  SINGLE MBLNR MJAHR ZEILE MATNR WERKS LGORT DMBTR
                                                      EBELN EBELP SAKTO
                    INTO IMSEG
                    FROM MSEG WHERE

                                         MBLNR EQ COBK-REFBN AND
                                         MJAHR EQ BDATJ AND
                                         ZEILE EQ MSEGITEM AND
                                         AUFNR EQ AUFK-AUFNR.
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
                    I_BUKRS      = AUFK-BUKRS
                    X_DIALOG     = ' '
               TABLES
                    T_DOCUMENTS  = T_DOCUMENTS
               EXCEPTIONS
                    NO_REFERENCE = 1
                    NO_DOCUMENT  = 2
                    OTHERS       = 3.

          IF SY-SUBRC = 0.
            LOOP AT T_DOCUMENTS WHERE AWTYP EQ 'BKPF' AND
                                      BUKRS EQ AUFK-BUKRS.
             IF SY-SUBRC = 0.
              PERFORM MAT_AND_AC_DOCUMENTS.
            ELSE.
              CONTINUE.
            ENDIF.
            ENDLOOP.
*            IF SY-SUBRC = 0.
*              PERFORM MAT_AND_AC_DOCUMENTS.
*            ELSE.
*              CONTINUE.
*            ENDIF.
          ENDIF.
        WHEN 'BKPF '.
          PERFORM FIND_REST_OF_AC_DOCUMENTS.
      ENDCASE.
      CLEAR ICOEP.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      WRITE:/ TEXT-MMM, AUFK-AUFNR.
    ENDIF.
    CHECK SY-SUBRC = 0.
  ENDSELECT.
* Check currency and conver into canadian dollars
  LOOP AT ITAB.
    IF ITAB-PSWSL NE 'CAD'.
      PERFORM CONVERT_INTO_CANADIAN.
    ENDIF.
  ENDLOOP.

  SORT ITAB BY AUFNR HKONT MATNR BELNR.


  FORMAT COLOR 2 INTENSIFIED OFF.
  LOOP AT ITAB.

    FORMAT COLOR 2 INTENSIFIED OFF.
    WRITE:  /  ITAB-AUFNR UNDER TEXT-001,
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

    MOVE ITAB-MEINS TO TMPMEINS.       " To avoid stars when do the sum.
    AT END OF MATNR.
      SUM.

      WRITE: / SY-ULINE(19) UNDER TEXT-008.
*     skip.
*TOTAL FOR MATERIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /60 TEXT-100 COLOR 4, ITAB-MATNR COLOR 4.
      WRITE: ITAB-MENGE COLOR 3 UNDER TEXT-007 NO-GAP,
             TMPMEINS COLOR 3.
      WRITE: ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP,
             TEXT-200  COLOR 3.
      SKIP.
      CLEAR TMPMEINS.

      FORMAT RESET.
    ENDAT.

    AT END OF HKONT.
      SUM.
      WRITE: / SY-ULINE(19) UNDER TEXT-008.
      SKIP.
*TOTAL FOR G/L
      WRITE: /55 TEXT-201 COLOR 6 INTENSIFIED ON,
                 ITAB-HKONT COLOR 6 INTENSIFIED ON.
      WRITE: ITAB-PSWBT COLOR 3 UNDER TEXT-008 NO-GAP INTENSIFIED ON,
             TEXT-200  COLOR 3 INTENSIFIED ON.
      SKIP.
    ENDAT.

    AT END OF AUFNR.
      SUM.
      WRITE: / SY-ULINE(19) UNDER TEXT-008.
      SKIP.
*TOTAL FOR ORDER
      WRITE: /50 TEXT-202 COLOR 5 INTENSIFIED ON,
                 ITAB-AUFNR COLOR 5 INTENSIFIED ON.
      WRITE:  ITAB-PSWBT COLOR 5  INTENSIFIED ON UNDER TEXT-008 NO-GAP,
              TEXT-200  COLOR 5 INTENSIFIED ON.
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
  WRITE: TEXT-222 UNDER SY-TITLE, P_SET COLOR 4 INTENSIFIED ON.
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
  WRITE:  /1 TEXT-001,
          13 TEXT-002,
          23 TEXT-003,
          36 TEXT-004,
          48 TEXT-005,
          60 TEXT-006,
         102 TEXT-007,
         129 TEXT-008,
         152 TEXT-009,
         157 TEXT-010,
         162 TEXT-011.
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
  ITAB-AUFNR = AUFK-AUFNR.             " Order number
  ITAB-MATNR = IMSEG-MATNR.            " Material number
* Mterial description from material master or purchase order
  IF ITAB-MATNR NE SPACE.              " If there is material number

    SELECT SINGLE * FROM MAKT WHERE
                                    MATNR EQ ITAB-MATNR AND
                                    SPRAS EQ SY-LANGU.
    IF SY-SUBRC <> 0 OR MAKT-MAKTX EQ SPACE.
      ITAB-KTEXT = AUFK-KTEXT.         " Material description
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

  SELECT SINGLE * FROM BSEG  WHERE
                                  BUKRS EQ AUFK-BUKRS AND
                                  BELNR EQ T_DOCUMENTS-DOCNR AND
                                  GJAHR EQ BDATJ AND
*                                BUZEI EQ BSEGITEM AND  "CMCCOY 02/04/03
                                  PSWBT EQ IMSEG-DMBTR AND
                                  AUFNR EQ ITAB-AUFNR AND  " Order
                                  HKONT EQ IMSEG-SAKTO AND " G/L number
                                  MATNR EQ IMSEG-MATNR.    " Material

  CHECK SY-SUBRC = 0.
  ITAB-HKONT = BSEG-HKONT.             " G/L number
* itab-menge = bseg-menge.             " Quantity              98/09/24
  ITAB-MEINS = BSEG-MEINS.             " Unit od measurment
  CASE BSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = BSEG-PSWBT.         " Value in CAD
      ITAB-MENGE = BSEG-MENGE.         " Quantity              98/09/24
    WHEN 'H'.
      ITAB-PSWBT = BSEG-PSWBT * ( -1 )." Credit
      ITAB-MENGE = BSEG-MENGE * ( -1 )." Quantity              98/09/24
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

  SELECT SINGLE BUKRS BELNR GJAHR BUDAT USNAM
          INTO IBKPF
          FROM BKPF WHERE
                                    BUKRS EQ AUFK-BUKRS AND
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
                              AUFNR EQ AUFK-AUFNR AND
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
                                AUFNR EQ AUFK-AUFNR AND
                                HKONT IN STABLE.
    CHECK SY-SUBRC = 0.
  ENDIF.

  CLEAR ITAB.
  ITAB-AUFNR = AUFK-AUFNR.             "order number
  ITAB-BELNR = IBSEG-BELNR.            " Acc doc #
  ITAB-BELNR_TEMP = IBSEG-BELNR.       "Acc doc #
  ITAB-HKONT = IBSEG-HKONT.            " G/L number
  CASE IBSEG-SHKZG.
    WHEN 'S'.
      ITAB-PSWBT = IBSEG-PSWBT.        " Value in CAD
      ITAB-MENGE = IBSEG-MENGE.            "Quantity
    WHEN 'H'.
      ITAB-PSWBT = IBSEG-PSWBT * ( -1 ) .
      ITAB-MENGE = IBSEG-MENGE * ( -1 ).   "Quantity
  ENDCASE.
  ITAB-PSWSL = IBSEG-PSWSL.            " Currency
*  itab-menge = ibseg-menge.            "Quantity
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
    ITAB-KTEXT = IBSEG-SGTXT.          "Line item text
  ENDIF.

  MOVE ITAB TO TEMP_ITAB.              " Temp stored header-line.

  CLEAR ITAB.
  READ TABLE ITAB WITH KEY
                           AUFNR      = TEMP_ITAB-AUFNR
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

*&-------START OF CHANGES FOR CO SET VALUES ---------------------------*
*&      Form  GET_GL_RANGES
*&---------------------------------------------------------------------*
FORM GET_GL_RANGES.

* Local working variables
DATA: p_setid    LIKE sethier-setid,   "Internal ID of set to be read
      find_setid LIKE sethier-setid,   "Internal ID of set to be found
      my_int    TYPE i,
      my_tabix  LIKE sy-tabix.

* Internal tables to hold set hierarchy, values, pointers and sorts
DATA: set_hierarchy    LIKE sethier     OCCURS 0 WITH HEADER LINE,
      set_values       LIKE setvalues   OCCURS 0 WITH HEADER LINE,
      set_hier_ptr     LIKE sethierptr  OCCURS 0 WITH HEADER LINE,
      set_val_ptr      LIKE setvalptr   OCCURS 0 WITH HEADER LINE,
      set_hier_sort    LIKE sethiersrt  OCCURS 0 WITH HEADER LINE,
      set_val_sort     LIKE setvalsrt   OCCURS 0 WITH HEADER LINE,
      found_pointers   LIKE setvalsrt   OCCURS 0 WITH HEADER LINE.

* Include to draw lines in lists
INCLUDE <line>.

*----------------------------------------------------------------------*
* Get the internal Set-ID from the external name of the set
* Selection of sets can be restricted to a specific set class or
* to sets matching a specific table field.
* See documentation of function module G_SET_GET_ID_FROM_NAME
* If the set class, subclass and setname is known, function module
* G_SET_ENCRYPT_SETID should be used alternatively (see RGSEX020)
*----------------------------------------------------------------------*
  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
       EXPORTING
            shortname = p_set
            old_setid = p_setid
       IMPORTING
            new_setid = p_setid.


*----------------------------------------------------------------------*
* Read the complete hierarchy (set and all subsets)
*----------------------------------------------------------------------*
  CALL FUNCTION 'G_SET_TREE_IMPORT'
       EXPORTING
            setid              = p_setid  "Set-ID, not the Setname!!!
            no_table_buffering = ' '
       TABLES
            set_hierarchy      = set_hierarchy
            set_values         = set_values.


*----------------------------------------------------------------------*
* Add pointers to the imported set hierarchy. This is not needed if you
* just want to list all nodes or all values in the hierarchy!
*----------------------------------------------------------------------*
  CALL FUNCTION 'G_SET_TREE_ADD_POINTERS'
       TABLES
            set_hierarchy = set_hierarchy
            set_values    = set_values
            set_hier_ptr  = set_hier_ptr
            set_val_ptr   = set_val_ptr
            set_hier_sort = set_hier_sort
            set_val_sort  = set_val_sort
       EXCEPTIONS
            OTHERS        = 1.

  CHECK sy-subrc = 0.

  LOOP AT SET_VALUES.
    MOVE: SET_VALUES-FROM TO STABLE-LOW,
          SET_VALUES-TO TO STABLE-HIGH,
          'I' TO STABLE-SIGN,
          'BT' TO STABLE-OPTION.
    APPEND STABLE.
  ENDLOOP.
ENDFORM.                               " GET_GL_RANGES

*&---------------------------------------------------------------------*
*&      Form  GET_G/L_RANGES
*&---------------------------------------------------------------------*
*       THIS OLD PROCEEDURE REPLACED WITH ABOVE

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*FORM GET_G/L_RANGES.
*  REFRESH SETVLUES.
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*       EXPORTING
*            CLASS         = '0H'
*            SETNR         = P_ELMGRP
*            TABLE         = 'CCSS'
*       TABLES
*            SET_VALUES    = SETVLUES
*       EXCEPTIONS
*            SET_NOT_FOUND = 1
*            OTHERS        = 2.
*
*  LOOP AT SETVLUES.
*    MOVE: SETVLUES-FROM TO STABLE-LOW,
*          SETVLUES-TO TO STABLE-HIGH,
*          'I' TO STABLE-SIGN,
*          'BT' TO STABLE-OPTION.
*    APPEND STABLE.
*  ENDLOOP.
*ENDFORM.                               " GET_G/L_RANGES
**&---------------------------------------------------------------------
*
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
