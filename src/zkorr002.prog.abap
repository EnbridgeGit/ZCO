REPORT ZKORR002 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZKORR002
*    Programmer  :  Ric Aarssen
*    Client      :  Centra Union Gas Limited
*    Date        :  September 17, 1997
*
* This ABAP will display material charges for an internal order
* by cost element and material.
* The user will have the choice of entering company code, internal order
* a range of cost elements (or order group) and period.
************************************************************************
TABLES  : ENT1027, MARC, MARD, MBEW, T001L, T001W, T023T.
DATA    : QTYHAND         TYPE I,
          CHECK1(1)       TYPE C VALUE 'N',
          CHECK2          LIKE CHECK1 VALUE 'Y',
          TOTAL_VALUE(8)  TYPE P DECIMALS 2.

DATA    : BEGIN OF TABLE1 OCCURS 5000,
              WERKS        LIKE MARD-WERKS,
              LGORT        LIKE MARD-LGORT,
              MATKL        LIKE ENT1027-MATKL,
              MATNR        LIKE MARA-MATNR,
              LVORM        LIKE MARD-LVORM,
              QTYHAND      LIKE QTYHAND,
              VALUE        LIKE TOTAL_VALUE,
              MAKTX        LIKE ENT1027-MAKTX,
              VERPR        LIKE TOTAL_VALUE,
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_WERKS         FOR   MARC-WERKS,
     S_LGORT         FOR   MARD-LGORT,
     S_MATKL         FOR   ENT1027-MATKL,
     S_MATNR         FOR   ENT1027-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-008.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-009.
INCLUDE <ICON>.
*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: /1 TEXT-019, SY-REPID COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
       50 TEXT-002, 105 TEXT-024, SY-DATUM, TEXT-025, SY-UZEIT.
WRITE: / TEXT-022 UNDER TEXT-019, SY-MANDT UNDER SY-REPID,
         TEXT-026 UNDER TEXT-024, (3) SY-PAGNO.
ULINE.

WRITE: /44 TEXT-021.
WRITE: /45 TEXT-012, 80 TEXT-006, 89 TEXT-013.
WRITE: /33 TEXT-011,                                  "Quantity
           TEXT-017 UNDER TEXT-012,                   "AVG Moving Price
        62 TEXT-027,                                  "Value
           TEXT-018 UNDER TEXT-006,                   "Material Number
           TEXT-015 UNDER TEXT-013.                   "Description
ULINE.
*write: /.

************************************************************************
START-OF-SELECTION.
CLEAR TABLE1.
REFRESH TABLE1.

    SELECT * FROM MARD
        WHERE LVORM NE 'X'
          AND LGORT IN S_LGORT
          AND WERKS IN S_WERKS
          AND MATNR IN S_MATNR
          ORDER BY WERKS LGORT MATNR.

          SELECT SINGLE * FROM ENT1027 WHERE MATNR = MARD-MATNR
                                         AND SPRAS = SY-LANGU
                                         AND MATKL IN S_MATKL.
          IF SY-SUBRC = 0.
             QTYHAND = QTYHAND + MARD-LABST + MARD-UMLME +
                       MARD-INSME + MARD-SPEME + MARD-EINME.

             SELECT SINGLE * FROM MBEW WHERE MATNR = MARD-MATNR
                                         AND BWKEY = MARD-WERKS
                                         AND BWTAR = SPACE.

                TOTAL_VALUE = MBEW-VERPR * QTYHAND.
                MOVE MBEW-VERPR    TO TABLE1-VERPR.
                MOVE ENT1027-MATKL TO TABLE1-MATKL.
                MOVE ENT1027-MAKTX TO TABLE1-MAKTX.
                MOVE MARD-WERKS    TO TABLE1-WERKS.
                MOVE MARD-LGORT    TO TABLE1-LGORT.
                MOVE MARD-MATNR    TO TABLE1-MATNR.
                MOVE QTYHAND       TO TABLE1-QTYHAND.
                MOVE TOTAL_VALUE   TO TABLE1-VALUE.
                APPEND TABLE1.
                CLEAR: TABLE1, QTYHAND.
          ENDIF.
   ENDSELECT.

SORT TABLE1 BY WERKS  ASCENDING
               LGORT  ASCENDING
               MATKL  ASCENDING
               MATNR  ASCENDING.

LOOP AT TABLE1.
  AT NEW WERKS.                                 "Plant Name
     NEW-PAGE.
     SELECT SINGLE * FROM T001W WHERE WERKS = TABLE1-WERKS.
     WRITE: /1 TEXT-020, 19 TABLE1-WERKS COLOR COL_NEGATIVE INVERSE ON,
               TEXT-023, T001W-NAME1.
  ENDAT.
  AT NEW LGORT.                                 "Storage Location Name
     SELECT SINGLE * FROM T001L WHERE WERKS = TABLE1-WERKS
                                  AND LGORT = TABLE1-LGORT.
     WRITE: / TEXT-010 UNDER TEXT-020,
              19 TABLE1-LGORT COLOR COL_NEGATIVE
              INVERSE ON,
              TEXT-023 UNDER TEXT-023,
              T001L-LGOBE UNDER T001W-NAME1.
  ENDAT.
  AT NEW MATKL.
     SELECT SINGLE * FROM T023T WHERE MATKL = TABLE1-MATKL
                                  AND SPRAS = SY-LANGU.
     WRITE: / TEXT-004 UNDER TEXT-020,
              19 TABLE1-MATKL COLOR COL_NEGATIVE
              INVERSE ON,
              TEXT-023 UNDER TEXT-023,
              T023T-WGBEZ UNDER T001W-NAME1.
  ENDAT.
        WRITE: / TABLE1-QTYHAND UNDER TEXT-011.           "Quantity
        WRITE: TABLE1-VERPR   UNDER TEXT-012.               "AVG Price
        WRITE: TABLE1-VALUE COLOR COL_TOTAL UNDER TEXT-027. "Value
        WRITE: TABLE1-MATNR UNDER TEXT-006.                 "Material#
        WRITE: TABLE1-MAKTX UNDER TEXT-013.                 "Description

  AT END OF MATKL.                                 "Material Group Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS, TABLE1-LGORT, TABLE1-MATKL,
              TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT END OF LGORT.                                 "Storage Loc Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS, TABLE1-LGORT,
              TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT END OF WERKS.                                  "Plant Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS,
               TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT LAST.                                           "Report Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-007,
               TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
  ENDAT.
ENDLOOP.
