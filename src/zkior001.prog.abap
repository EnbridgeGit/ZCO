REPORT ZKORI001 LINE-COUNT 65 LINE-SIZE 132 NO STANDARD PAGE HEADING
                MESSAGE-ID ZK.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - CO Internal Order Report                                          *
*----------------------------------------------------------------------*
* CHANGES
* 2001/04/04 mdemeest #--- added version selection on COBRB            *
* 2001/02/19 mdemeest 4.6B Function call change from '0H' to '0103'    *
* 98/10/13 Nancy Gilligan, OmniLogic      D30K906162                   *
*             - standardized headers
* 98/02/23 md7140 added OSA field - selection option & on report       *
*----------------------------------------------------------------------*
TABLES: COAS,                          "Order Master
        JEST,                          "Object status
        TJ01T,                         "Business Transaction Texts
        TJ02T,                         "System status texts
        COBRB,                       "Distribution Rules Settlement Rule
        T001.                        "Company Code       ngilligan 98/10

DATA:   W_SETNR   LIKE RGSB4-SETNR,
        FOUND.

DATA:   BEGIN OF SETHIER OCCURS 1000.
        INCLUDE STRUCTURE SETHIER.
DATA:   END OF SETHIER.

DATA:   BEGIN OF SETVALUES OCCURS 1000.
        INCLUDE STRUCTURE RGSB4.
DATA:   END OF SETVALUES.

DATA:   BEGIN OF ITAB OCCURS 1000,
          AUFNR LIKE COAS-AUFNR,
          SETNR LIKE RGSB4-SETNR.
DATA:   END OF ITAB.


SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-011.
PARAMETERS    : SETNR LIKE CODIA-AUFGR."Order Group
SELECT-OPTIONS: AUFNR FOR  COAS-AUFNR MATCHCODE OBJECT ORDE   "Order #
                                      NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-012.
SELECT-OPTIONS: AUART      FOR  COAS-AUART,      "Order type
                BUKRS      FOR  COAS-BUKRS,      "Company code
                KALSM      FOR  COAS-KALSM,      "Costing Sheet
                KONTY      FOR  COBRB-KONTY,     "Acct Asignmt Category
                SURZUO     FOR  COBRB-URZUO.     "OSA field

SELECTION-SCREEN ULINE.
SELECT-OPTIONS: KOSTL      FOR  COBRB-KOSTL,     "Receiver - cost center
                HKONT      FOR  COBRB-HKONT,     "Receiver - G/L acct
                PS_PSP_P   FOR  COBRB-PS_PSP_PNR."Receiver - WBS

SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX1.


* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
     BUKRS-LOW = 'UEC'.                                 "ngilligan 98/10
     BUKRS-SIGN = 'I'.                                  "ngilligan 98/10
     BUKRS-OPTION = 'EQ'.                               "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
     BUKRS-LOW = 'UGL'.                                 "ngilligan 98/10
     BUKRS-SIGN = 'I'.                                  "ngilligan 98/10
     BUKRS-OPTION = 'EQ'.                               "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10
APPEND   BUKRS.                                         "ngilligan 98/10



AT SELECTION-SCREEN.
* Check for either Order Group or Order #'s
  IF NOT ( SETNR IS INITIAL ) AND NOT ( AUFNR IS INITIAL ).
    MESSAGE E004.
  ENDIF.

* Check for Receiver (only one field has to be entered)
  IF NOT KOSTL IS INITIAL.
    IF ( NOT ( HKONT IS INITIAL ) OR NOT ( PS_PSP_P IS INITIAL ) ).
      MESSAGE E005.
    ENDIF.
  ENDIF.

  IF NOT HKONT IS INITIAL.
    IF ( NOT ( KOSTL IS INITIAL ) OR NOT ( PS_PSP_P IS INITIAL ) ).
      MESSAGE E005.
    ENDIF.
  ENDIF.

  IF NOT PS_PSP_P IS INITIAL.
    IF ( NOT ( KOSTL IS INITIAL ) OR NOT ( HKONT IS INITIAL ) ).
      MESSAGE E005.
    ENDIF.
  ENDIF.


*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR SETNR.
  DATA: OLD_AUFGR LIKE CODIA-AUFGR.
  OLD_AUFGR = SETNR.
  CALL FUNCTION 'K_GROUP_SELECT'
       EXPORTING
            CLASS              = '0103'             "mdemeest 2001/02/19
            FIELD_NAME         = 'AUFNR'
            SEARCHFLD_REQUIRED = SPACE
*           SELECT_SETS_WITHOUT_CLASS = 'X'
       IMPORTING
            SET_NAME           = SETNR
       EXCEPTIONS
            NO_SET_PICKED      = 1.
  IF SY-SUBRC = 1.
    SETNR = OLD_AUFGR.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
* get company code text                                  ngilligan 98/10
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS IN BUKRS.


   IF NOT SETNR IS INITIAL.             "Order Group based selects
*    W_SETNR(2) = '0H'.
*    W_SETNR(2) = '0103'.
*    CONDENSE SETNR.
*    W_SETNR+2 = SETNR.
     w_setnr = setnr.

    CLEAR ITAB.
    REFRESH ITAB.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
         EXPORTING
              class         = '0103'
              SETNR         = W_SETNR
              TABLE         = 'CCSS'
         TABLES
              SET_VALUES    = SETVALUES
         EXCEPTIONS
              SET_NOT_FOUND = 01.

    LOOP AT SETVALUES.
      SELECT * FROM COAS WHERE AUFNR BETWEEN SETVALUES-FROM AND
                                             SETVALUES-TO.
        ITAB-AUFNR = COAS-AUFNR.
        ITAB-SETNR = SETVALUES-SETNR.
        APPEND ITAB.
      ENDSELECT.
    ENDLOOP.

  ELSE.
    IF NOT AUFNR IS INITIAL.           "Order Number based selects
      LOOP AT AUFNR.
        IF AUFNR-OPTION = 'EQ'.
          MOVE AUFNR-LOW TO ITAB-AUFNR.
          APPEND ITAB.
        ELSE.
          IF AUFNR-OPTION = 'BT'.
            SELECT * FROM COAS WHERE AUFNR BETWEEN AUFNR-LOW  AND
                                                   AUFNR-HIGH.
              ITAB-AUFNR = COAS-AUFNR.
              APPEND ITAB.
            ENDSELECT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF SETNR IS INITIAL AND AUFNR IS INITIAL.
    SELECT * FROM COAS WHERE
                             AUART IN AUART      AND
                             BUKRS IN BUKRS      AND
                             KALSM IN KALSM
                                  ORDER BY AUFNR.
      MOVE COAS-AUFNR TO ITAB-AUFNR.
      APPEND ITAB.
    ENDSELECT.
  ENDIF.


*----------------------------------------------------------------------*
  LOOP AT ITAB.
    SELECT * FROM COAS WHERE AUFNR = ITAB-AUFNR  AND
                             AUART IN AUART      AND
                             BUKRS IN BUKRS      AND
                             KALSM IN KALSM
                                  ORDER BY AUFNR.
      FOUND = SPACE.
      IF KOSTL IS INITIAL AND
            HKONT IS INITIAL AND
            PS_PSP_P IS INITIAL AND
            KONTY IS INITIAL AND
            SURZUO IS INITIAL.
        FOUND = 'X'.
      ENDIF.

      IF NOT KOSTL IS INITIAL AND FOUND = SPACE.
         SELECT * FROM COBRB
            WHERE OBJNR = COAS-OBJNR
              AND KOSTL IN KOSTL
              AND URZUO IN SURZUO.                             "98/02/23
            FOUND = 'X'.
            EXIT.
         ENDSELECT.
      ENDIF.

      IF NOT KONTY IS INITIAL AND FOUND = SPACE.
         SELECT * FROM COBRB
           WHERE OBJNR = COAS-OBJNR
             AND KONTY IN KONTY
             AND URZUO IN SURZUO.                              "98/02/23
          FOUND = 'X'.
          EXIT.
        ENDSELECT.
      ENDIF.

      IF NOT HKONT IS INITIAL AND FOUND = SPACE.
         SELECT * FROM COBRB
           WHERE OBJNR = COAS-OBJNR
             AND HKONT IN HKONT
             AND URZUO IN SURZUO.                              "98/02/23
          FOUND = 'X'.
          EXIT.
        ENDSELECT.
      ENDIF.

      IF NOT PS_PSP_P IS INITIAL AND FOUND = SPACE.
         SELECT * FROM COBRB
           WHERE OBJNR = COAS-OBJNR
             AND PS_PSP_PNR IN PS_PSP_P
             AND URZUO IN SURZUO.                              "98/02/23
          FOUND = 'X'.
          EXIT.
        ENDSELECT.
      ENDIF.

      IF NOT SURZUO IS INITIAL AND FOUND = SPACE.              "98/02/24
         SELECT * FROM COBRB
           WHERE OBJNR = COAS-OBJNR
             AND URZUO IN SURZUO.
          FOUND = 'X'.
          EXIT.
        ENDSELECT.
      ENDIF.


      IF FOUND = 'X'.
        RESERVE 8 LINES.
        FORMAT INTENSIFIED ON.
        WRITE: / COAS-AUFNR UNDER TEXT-111,    "Order #
                 COAS-KTEXT UNDER TEXT-112,    "Short Text
                 COAS-BUKRS UNDER TEXT-113,    "Company code
                 COAS-KALSM UNDER TEXT-114,    "Costing Sheet
                 ITAB-SETNR UNDER TEXT-115.    "Order Group (immediate)
        FORMAT INTENSIFIED OFF.
        SELECT * FROM JEST WHERE OBJNR = COAS-OBJNR  AND
                                 INACT = SPACE.
          SELECT SINGLE * FROM TJ02T WHERE ISTAT = JEST-STAT  AND
                                           SPRAS = SY-LANGU.
          IF SY-SUBRC = 0.
            WRITE: / TJ02T-TXT04 UNDER TEXT-112,
                     TJ02T-TXT30.
          ENDIF.
        ENDSELECT.

        WRITE: / SY-ULINE(40) UNDER TEXT-105.
        SELECT * FROM COBRB WHERE OBJNR = COAS-OBJNR
*                             and urzuo in surzuo
                                  ORDER BY AVORG.

          WRITE: / COBRB-LFDNR UNDER TEXT-112,    "Sequence #
                   COBRB-KONTY.             "Account assignment category

          IF NOT COBRB-HKONT IS INITIAL.
            WRITE:  COBRB-HKONT UNDER TEXT-124.       "G/L account
          ENDIF.
          IF NOT COBRB-KOSTL IS INITIAL.
            WRITE:  COBRB-KOSTL UNDER TEXT-124.       "Cost center
          ENDIF.
          IF NOT COBRB-AUFNR IS INITIAL.
            WRITE:  COBRB-AUFNR UNDER TEXT-124.       "Order
          ENDIF.
          IF NOT COBRB-PS_PSP_PNR IS INITIAL.
            WRITE:  COBRB-PS_PSP_PNR UNDER TEXT-124.  "WBS element
          ENDIF.

          WRITE: COBRB-PROZS UNDER TEXT-125 USING EDIT MASK 'RR___.__',
             (6) COBRB-AQZIF UNDER TEXT-127,  "Equival number for Order
                 COBRB-PERBZ UNDER TEXT-128,  "Settlement Type
                 COBRB-GABPE UNDER TEXT-130,  "Valid-from period
                 COBRB-GABJA,                 "Valid-from year
                 COBRB-GBISP UNDER TEXT-132,  "Valid-to period
                 COBRB-GBISJ,                 "Valid-to year
                 COBRB-URZUO UNDER TEXT-134,  "OSA field
             105 cobrb-versn.                "Version number
*          write: /.
*         write: 39  cobrb-prozs using edit mask 'RR___.__', "percentage
*                    cobrb-aqzif,      "Equivalence number for order
*                    cobrb-perbz,      "Settlement type
*                    cobrb-gabpe,      "Valid-from period
*                    cobrb-gabja,      "Valid-from year
*                    cobrb-gbisp,      "Valid to
*                    cobrb-gbisj.      "Valid-to year
*                    COBRB-AVORG.      "Sttlmt Transaction
          SELECT SINGLE * FROM TJ01T WHERE VRGNG = COBRB-AVORG AND
                                           SPRAS = SY-LANGU.
          IF SY-SUBRC = 0.
            WRITE: TJ01T-TXT UNDER TEXT-133.
          ENDIF.
        ENDSELECT.
        ULINE.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

  IF SY-LINNO = 1.
    MESSAGE I006.
  ENDIF.


* top of page
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         37 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
        105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10




  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-111, 14 TEXT-112, 54 TEXT-113, 60 TEXT-114,     "Order#
         67 TEXT-115.
  WRITE: / TEXT-116 UNDER TEXT-113, TEXT-117 UNDER TEXT-114,
           TEXT-118 UNDER TEXT-115.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-105 UNDER TEXT-112,                             "Status
         / SY-ULINE(40) UNDER TEXT-105.
  WRITE: / TEXT-120 UNDER TEXT-112, 18 TEXT-122,
           22 TEXT-124, 40 TEXT-125, 55 TEXT-127, 65 TEXT-128,
           70 TEXT-134, 80 TEXT-130, 90 TEXT-132, 110 TEXT-133,
          100 text-136.
  WRITE: / TEXT-121 UNDER TEXT-120, TEXT-123 UNDER TEXT-122,
           TEXT-126 UNDER TEXT-125, TEXT-129 UNDER TEXT-128,
           TEXT-135 UNDER TEXT-134, TEXT-131 UNDER TEXT-130,
           TEXT-131 UNDER TEXT-132.
  ULINE.
