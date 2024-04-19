REPORT ZKCOR002 NO STANDARD PAGE HEADING
LINE-SIZE 132 LINE-COUNT 58 MESSAGE-ID AT.
************************************************************************
* PROGRAM: ZKCOR002
* BreakDown of Cost Elements into Capitalised vs. Expensed
* AUTHOR : NESH N. LAURENCIC   - OMNILOGIC SYSTEMS GROUP
* Date   : April 30th, 1998
* This report will show how much has gone to CAPITAL versus Expense per
* cost element.
* In detail report you have the option to see all corresponding
* orders and percentages.
* Warning: watch for FORM -> load_reference_cc_table
* This form contains list of Cost Centers, if you need to maintain do
* that here.
*
************************************************************************
* YY/MM/DD  Programmer  Description
* --------  ----------  -----------------------------------------------
* 98/05/27  MRadsma     - use CO currency (WKGnnn instead of WTGnnn)
*                       - add year and company code to header
*                       - update settlement rule to take rule that was
*                         active at report yearend instead of just the
*                         current active settlement rule
*                       - add new cost centers to internal table
************************************************************************

TABLES: AUFK,                          "Order master data
        SKAT,                          "G/L account master record
        COSS,                        "CO Object:  Cost Totals - Internal
        COSP,                        "CO Object:  Cost Totals - External
        COBRB,                       "Distribution Rules Settlement Rule
        CSKU,                        "Cost element texts
        RKMA4.
DATA:     BEGIN OF AUFK_TAB OCCURS 100," To hold orders
          AUFNR LIKE AUFK-AUFNR,       "Order number
          BUKRS LIKE AUFK-BUKRS,       "Company code
          OBJNR LIKE COSP-OBJNR,       "Object
          GJAHR LIKE COSP-GJAHR,       " Year
          KSTAR LIKE COSP-KSTAR,       "Cost element
          TOTAL TYPE P DECIMALS 2,      "Total
          CAPITAL TYPE P DECIMALS 2,    "capital
          EXPENSE TYPE P DECIMALS 2,    "expense
          WKG001 LIKE COSP-WKG001,     "Period 1 money
          WKG002 LIKE COSP-WKG002,                          "
          WKG003 LIKE COSP-WKG003,                          "
          WKG004 LIKE COSP-WKG004,                          "
          WKG005 LIKE COSP-WKG005,                          "
          WKG006 LIKE COSP-WKG006,                          "
          WKG007 LIKE COSP-WKG007,                          "
          WKG008 LIKE COSP-WKG008,                          "
          WKG009 LIKE COSP-WKG009,                          "
          WKG010 LIKE COSP-WKG010,                          "
          WKG011 LIKE COSP-WKG011,                          "
          WKG012 LIKE COSP-WKG012,     "Period 12 money
          KTEXT  LIKE AUFK-KTEXT,      "Description
          IPEXIST TYPE C,              "Internal postings exist
*         percent like cobrb-prozs,    "Setl. Perc
          PERCENT TYPE P DECIMALS 2,   "Setl. Perc
          END OF AUFK_TAB.
DATA:     OUTPUT(14) TYPE C.           "Print format totals

DATA:     PERCENT LIKE COBRB-PROZS,    "Settlement percentage rate
          PERCTEXT(6) TYPE C.          "Print format


DATA:     TEMPTEXT(40) TYPE C.         "Description
* Internal table with balnces
DATA:     BEGIN OF ITAB OCCURS 100.
        INCLUDE STRUCTURE AUFK_TAB.
DATA:     END OF ITAB.

DATA: BEGIN OF PRINT_TAB OCCURS 50,
      KSTAR LIKE COSP-KSTAR,
      AUFNR LIKE AUFK-AUFNR,
      TOTAL LIKE COSP-WKG001,      "Total
      CAPITAL LIKE COSP-WKG001,    "capital
      EXPENSE LIKE COSP-WKG001,    "expense
*     percent like cobrb-prozs,    "Setl. Perc
      PERCENT TYPE P DECIMALS 2, "Setl. Perc
      BUKRS   LIKE AUFK-BUKRS,
      KTEXT   LIKE AUFK-KTEXT,
      END OF PRINT_TAB.

*Reference table ( Cost Centers that are excluded )
DATA: BEGIN OF CC_TABLE OCCURS 60,
      KOSTL LIKE COBRB-KOSTL,
      END OF CC_TABLE.

* settlement rules, required for active rule at yearend    MRadsma 98/05
DATA: BEGIN OF ICOBRB OCCURS 50,                          "MRadsma 98/05
        KOSTL      LIKE COBRB-KOSTL,                      "MRadsma 98/05
        LFDNR      LIKE COBRB-LFDNR,                      "MRadsma 98/05
        CCIND(1)   TYPE C,                                "MRadsma 98/05
        PERFROM(7) TYPE C,                                "MRadsma 98/05
        PERTO(7)   TYPE C,                                "MRadsma 98/05
        PROZS      LIKE COBRB-PROZS,                      "MRadsma 98/05
      END OF ICOBRB.                                      "MRadsma 98/05
DATA: PRVKOSTL     LIKE COBRB-KOSTL,                      "MRadsma 98/05
      PEREND(7)    TYPE C,                                "MRadsma 98/05
      PCTFOUND(1)  TYPE C.                                "MRadsma 98/05

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: SAUFNR FOR AUFK-AUFNR MODIF ID AA MATCHCODE OBJECT ORDE,
                SKSTAR  FOR RKMA4-KSTAR MATCHCODE
                OBJECT KART OBLIGATORY.
SELECTION-SCREEN SKIP 2.
PARAMETERS: PBUKRS LIKE AUFK-BUKRS DEFAULT 'CGO' OBLIGATORY,
            PGJAHR LIKE COSS-GJAHR DEFAULT '1998'.
SELECTION-SCREEN SKIP 3.
PARAMETERS:     P1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

* At selection screen
AT SELECTION-SCREEN OUTPUT.
SAUFNR-LOW = '1'.
SAUFNR-HIGH = '999999'.
SAUFNR-SIGN = 'I'.
SAUFNR-OPTION = 'BT'.
APPEND SAUFNR.
* Modify screen

  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'AA'.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM LOAD_REFERENCE_CC_TABLE.
* Order master
  SELECT * FROM AUFK WHERE
                          AUFNR IN SAUFNR AND  " Order number
                          BUKRS EQ PBUKRS AND
                          LOEKZ EQ SPACE." Deletion factor


*CO Object:  Cost Totals - External Postings
    CLEAR COSP.
    SELECT * FROM COSP WHERE           " fIND Plan
                             LEDNR EQ '00' AND
                             OBJNR EQ AUFK-OBJNR AND
                             GJAHR EQ PGJAHR AND
                             WRTTP EQ '04' AND
                             KSTAR IN SKSTAR.
      CLEAR AUFK_TAB.
      MOVE: AUFK-AUFNR TO AUFK_TAB-AUFNR,"Order #
            AUFK-BUKRS TO AUFK_TAB-BUKRS,"Company code
            AUFK-KTEXT TO AUFK_TAB-KTEXT."Description

      MOVE: COSP-OBJNR TO AUFK_TAB-OBJNR,
            COSP-GJAHR TO AUFK_TAB-GJAHR,
            COSP-KSTAR TO AUFK_TAB-KSTAR,
            COSP-WKG001 TO AUFK_TAB-WKG001,
            COSP-WKG002 TO AUFK_TAB-WKG002,
            COSP-WKG003 TO AUFK_TAB-WKG003,
            COSP-WKG004 TO AUFK_TAB-WKG004,
            COSP-WKG005 TO AUFK_TAB-WKG005,
            COSP-WKG006 TO AUFK_TAB-WKG006,
            COSP-WKG007 TO AUFK_TAB-WKG007,
            COSP-WKG008 TO AUFK_TAB-WKG008,
            COSP-WKG009 TO AUFK_TAB-WKG009,
            COSP-WKG010 TO AUFK_TAB-WKG010,
            COSP-WKG011 TO AUFK_TAB-WKG011,
            COSP-WKG012 TO AUFK_TAB-WKG012.

      AUFK_TAB-TOTAL = AUFK_TAB-WKG001 + AUFK_TAB-WKG002 +
                       AUFK_TAB-WKG003 + AUFK_TAB-WKG004 +
                       AUFK_TAB-WKG005 + AUFK_TAB-WKG006 +
                       AUFK_TAB-WKG007 + AUFK_TAB-WKG008 +
                       AUFK_TAB-WKG009 + AUFK_TAB-WKG010 +
                       AUFK_TAB-WKG011 + AUFK_TAB-WKG012.

      APPEND AUFK_TAB.
      CLEAR AUFK_TAB.
    ENDSELECT.
*CO Object:  Cost Totals - Internal Postings
    CLEAR COSS.
    SELECT * FROM COSS WHERE           " fIND Plan
                             LEDNR EQ '00' AND
                             OBJNR EQ AUFK-OBJNR AND
                             GJAHR EQ PGJAHR AND
                             WRTTP EQ '04' AND
                             KSTAR IN SKSTAR.
      MOVE: AUFK-AUFNR TO AUFK_TAB-AUFNR,   "Order #
            AUFK-BUKRS TO AUFK_TAB-BUKRS,   "Company code
            AUFK-KTEXT TO AUFK_TAB-KTEXT.   "Description

      MOVE: COSS-OBJNR TO AUFK_TAB-OBJNR,
            COSS-GJAHR TO AUFK_TAB-GJAHR,
            COSS-KSTAR TO AUFK_TAB-KSTAR,
            COSS-WKG001 TO AUFK_TAB-WKG001,
            COSS-WKG002 TO AUFK_TAB-WKG002,
            COSS-WKG003 TO AUFK_TAB-WKG003,
            COSS-WKG004 TO AUFK_TAB-WKG004,
            COSS-WKG005 TO AUFK_TAB-WKG005,
            COSS-WKG006 TO AUFK_TAB-WKG006,
            COSS-WKG007 TO AUFK_TAB-WKG007,
            COSS-WKG008 TO AUFK_TAB-WKG008,
            COSS-WKG009 TO AUFK_TAB-WKG009,
            COSS-WKG010 TO AUFK_TAB-WKG010,
            COSS-WKG011 TO AUFK_TAB-WKG011,
            COSS-WKG012 TO AUFK_TAB-WKG012.

      AUFK_TAB-TOTAL = AUFK_TAB-WKG001 + AUFK_TAB-WKG002 +
                       AUFK_TAB-WKG003 + AUFK_TAB-WKG004 +
                       AUFK_TAB-WKG005 + AUFK_TAB-WKG006 +
                       AUFK_TAB-WKG007 + AUFK_TAB-WKG008 +
                       AUFK_TAB-WKG009 + AUFK_TAB-WKG010 +
                       AUFK_TAB-WKG011 + AUFK_TAB-WKG012.
      AUFK_TAB-IPEXIST = 'Y'.
      APPEND AUFK_TAB.
      CLEAR AUFK_TAB.

    ENDSELECT.
    CHECK SY-SUBRC = 0.

  ENDSELECT.
*break-point.
*  perform clean_duplicate_records.

* Find balance
  SORT AUFK_TAB BY AUFNR KSTAR.
  LOOP AT AUFK_TAB.
    MOVE:
              AUFK_TAB-AUFNR TO ITAB-AUFNR,       "Order number
              AUFK_TAB-BUKRS TO ITAB-BUKRS,       "Company code
              AUFK_TAB-OBJNR TO ITAB-OBJNR,       "Object
              AUFK_TAB-GJAHR TO ITAB-GJAHR,       " Year
              AUFK_TAB-KSTAR TO ITAB-KSTAR,       "Cost element
              AUFK_TAB-KTEXT TO ITAB-KTEXT,      "Description
              AUFK_TAB-IPEXIST  TO ITAB-IPEXIST. "Internal postings

    AT END OF KSTAR.
      SUM.
      MOVE:
                AUFK_TAB-TOTAL TO ITAB-TOTAL.      "Total
      APPEND ITAB.
    ENDAT.
  ENDLOOP.
  FORMAT COLOR 2.
  SKIP.

  LOOP AT ITAB.
    PERFORM CHECK_THE_SETTLEMENT_RULE.
  ENDLOOP.
  SORT ITAB BY KSTAR AUFNR.
*  break-point.
  IF P1 EQ 'X'.
  PERFORM PRINTING_REGULAR.
  ELSE.
  PERFORM PRINTING_SUMMARY.
  ENDIF.


TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         30 SY-TITLE COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID.
  WRITE: 30 TEXT-GJA COLOR 4 INTENSIFIED ON               "MRadsma 98/05
       ,    PGJAHR   COLOR 4 INTENSIFIED ON               "MRadsma 98/05
       , 75 TEXT-001 COLOR 4 INTENSIFIED ON               "MRadsma 98/05
       ,    PBUKRS   COLOR 4 INTENSIFIED ON.              "MRadsma 98/05
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  FORMAT COLOR 1.
* write:/1 text-auf, 10 text-001,                         "MRadsma 98/05
*                         22 text-002, 35 text-prc,       "MRadsma 98/05
  WRITE:/1 TEXT-AUF,      12 TEXT-002, 30 TEXT-PRC,       "MRadsma 98/05
                                       41 TEXT-003, 70 TEXT-004,
                          90 TEXT-005, 105 TEXT-DES.
  FORMAT RESET.
*&---------------------------------------------------------------------*
*&      Form  CHECK_THE_SETTLEMENT_RULE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_THE_SETTLEMENT_RULE.
* select * from cobrb where                               "MRadsma 98/05
*                           objnr eq itab-objnr and       "MRadsma 98/05
*                           gbisj eq '0' and              "MRadsma 98/05
*                           gbisp eq '0' and              "MRadsma 98/05
*                           avorg = 'KOAO' and            "MRadsma 98/05
*                           (                             "MRadsma 98/05
*                           kostl ne space or             "MRadsma 98/05
*                           ps_psp_pnr ne space ) .       "MRadsma 98/05
*                                                         "MRadsma 98/05
*   if cobrb-kostl ne space.                              "MRadsma 98/05
*     read table cc_table with key kostl = cobrb-kostl.   "MRadsma 98/05
*     if sy-subrc = 0.                                    "MRadsma 98/05
*       percent = percent + cobrb-prozs.                  "MRadsma 98/05
*       continue.                                         "MRadsma 98/05
*     else.                                               "MRadsma 98/05
*       continue.                                         "MRadsma 98/05
*     endif.                                              "MRadsma 98/05
*   endif.                                                "MRadsma 98/05
*   percent = percent + cobrb-prozs.                      "MRadsma 98/05
* endselect.                                              "MRadsma 98/05

  REFRESH ICOBRB.                                         "MRadsma 98/05
  SELECT * FROM COBRB                                     "MRadsma 98/05
  WHERE  OBJNR EQ ITAB-OBJNR                              "MRadsma 98/05
  AND    AVORG EQ 'KOAO'                                  "MRadsma 98/05
  AND  ( KOSTL NE SPACE                                   "MRadsma 98/05
  OR     PS_PSP_PNR NE SPACE ) .                          "MRadsma 98/05
    IF COBRB-KOSTL <> SPACE.                              "MRadsma 98/05
      ICOBRB-KOSTL      = COBRB-KOSTL.                    "MRadsma 98/05
      ICOBRB-CCIND      = 'X'.                            "MRadsma 98/05
    ELSE.                                                 "MRadsma 98/05
      ICOBRB-KOSTL      = COBRB-PS_PSP_PNR.               "MRadsma 98/05
    ENDIF.                                                "MRadsma 98/05
    ICOBRB-LFDNR        = COBRB-LFDNR.                    "MRadsma 98/05
    ICOBRB-PERFROM(4)   = COBRB-GABJA.                    "MRadsma 98/05
    ICOBRB-PERFROM+4(3) = COBRB-GABPE.                    "MRadsma 98/05
    ICOBRB-PERTO(4)     = COBRB-GBISJ.                    "MRadsma 98/05
    ICOBRB-PERTO+4(3)   = COBRB-GBISP.                    "MRadsma 98/05
    ICOBRB-PROZS        = COBRB-PROZS.                    "MRadsma 98/05
    APPEND ICOBRB.                                        "MRadsma 98/05
    CLEAR ICOBRB.                                         "MRadsma 98/05
  ENDSELECT.                                              "MRadsma 98/05
                                                          "MRadsma 98/05
* sort the table and initialize fields                    "MRadsma 98/05
  SORT ICOBRB BY KOSTL LFDNR DESCENDING.                  "MRadsma 98/05
  PEREND(4)   = PGJAHR.                                   "MRadsma 98/05
  PEREND+4(3) = '012'.                                    "MRadsma 98/05
  PCTFOUND    = 'N'.                                      "MRadsma 98/05
  PRVKOSTL    = SPACE.                                    "MRadsma 98/05
                                                          "MRadsma 98/05
* determine the percent of capital investment             "MRadsma 98/05
  LOOP AT ICOBRB.                                         "MRadsma 98/05
    IF ICOBRB-KOSTL = PRVKOSTL.                           "MRadsma 98/05
      IF PCTFOUND = 'Y'.                                  "MRadsma 98/05
        CONTINUE.                                         "MRadsma 98/05
      ENDIF.                                              "MRadsma 98/05
    ELSE.                                                 "MRadsma 98/05
      PRVKOSTL = ICOBRB-KOSTL.                            "MRadsma 98/05
      PCTFOUND = 'N'.                                     "MRadsma 98/05
    ENDIF.                                                "MRadsma 98/05
    IF ICOBRB-PERFROM   > PEREND                          "MRadsma 98/05
    OR  ( ICOBRB-PERTO <> '0000000'                       "MRadsma 98/05
    AND   ICOBRB-PERTO  < PEREND ).                       "MRadsma 98/05
      CONTINUE.                                           "MRadsma 98/05
    ELSE.                                                 "MRadsma 98/05
      PCTFOUND = 'Y'.                                     "MRadsma 98/05
      IF ICOBRB-CCIND = 'X'.                              "MRadsma 98/05
        READ TABLE CC_TABLE                               "MRadsma 98/05
             WITH KEY KOSTL = ICOBRB-KOSTL.               "MRadsma 98/05
        IF SY-SUBRC = 0.                                  "MRadsma 98/05
          PERCENT = PERCENT + ICOBRB-PROZS.               "MRadsma 98/05
          CONTINUE.                                       "MRadsma 98/05
        ELSE.                                             "MRadsma 98/05
          CONTINUE.                                       "MRadsma 98/05
        ENDIF.                                            "MRadsma 98/05
      ENDIF.                                              "MRadsma 98/05
      PERCENT = PERCENT + ICOBRB-PROZS.                   "MRadsma 98/05
    ENDIF.                                                "MRadsma 98/05
  ENDLOOP.                                                "MRadsma 98/05

  ITAB-PERCENT = PERCENT.
  ITAB-CAPITAL = ITAB-TOTAL * PERCENT / 100.
  ITAB-EXPENSE = ITAB-TOTAL - ITAB-CAPITAL.
  MODIFY ITAB.
  CLEAR PERCENT.

ENDFORM.                               " CHECK_THE_SETTLEMENT_RULE
*&---------------------------------------------------------------------*
*&      Form  LOAD_REFERENCE_CC_TABLE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_REFERENCE_CC_TABLE.
  CC_TABLE-KOSTL = '0000010002'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010010'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010033'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010037'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010050'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010053'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010070'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010073'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010090'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010095'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010110'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010113'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010130'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010133'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010160'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010163'.                          "MRadsma 98/05
  APPEND CC_TABLE.                                        "MRadsma 98/05

  CC_TABLE-KOSTL = '0000010180'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010192'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010201'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010205'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010206'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010213'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010222'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010232'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010243'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010252'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010262'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010272'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010282'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010292'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000010340'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020022'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020042'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020060'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020080'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020100'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020120'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020140'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020160'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020180'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020200'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020220'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020280'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020292'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020308'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020322'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020339'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020342'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020358'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020362'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020372'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020382'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020392'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020402'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020480'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020602'.
  APPEND CC_TABLE.

  CC_TABLE-KOSTL = '0000020603'.
  APPEND CC_TABLE.

ENDFORM.                               " LOAD_REFERENCE_CC_TABLE
*&---------------------------------------------------------------------*
*&      Form  CLEAN_DUPLICATE_RECORDS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAN_DUPLICATE_RECORDS.
  SORT AUFK_TAB.
ENDFORM.                               " CLEAN_DUPLICATE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  PRINTING_REGULAR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINTING_REGULAR.
LOOP AT ITAB.
MOVE-CORRESPONDING ITAB TO PRINT_TAB.
APPEND PRINT_TAB.
ENDLOOP.

  LOOP AT PRINT_TAB.
    WRITE: / PRINT_TAB-AUFNR UNDER TEXT-AUF,
*            print_tab-bukrs under text-001,              "MRadsma 98/05

             PRINT_TAB-PERCENT TO PERCTEXT,
             PERCTEXT UNDER  TEXT-PRC RIGHT-JUSTIFIED,

             PRINT_TAB-TOTAL TO OUTPUT,
             OUTPUT UNDER TEXT-003,

             PRINT_TAB-KSTAR UNDER TEXT-002,

             PRINT_TAB-CAPITAL TO OUTPUT,
             OUTPUT UNDER TEXT-004,

             PRINT_TAB-EXPENSE TO OUTPUT,
             OUTPUT UNDER TEXT-005,

             PRINT_TAB-KTEXT UNDER TEXT-DES.

    AT END OF KSTAR.
    SUM.
    WRITE: / SY-ULINE(11) UNDER TEXT-003,
             SY-ULINE(11) UNDER TEXT-004,
             SY-ULINE(11) UNDER TEXT-005.

    WRITE:   PRINT_TAB-TOTAL TO OUTPUT,
           / OUTPUT UNDER TEXT-003 COLOR 3,

             PRINT_TAB-CAPITAL TO OUTPUT,
             OUTPUT UNDER TEXT-004 COLOR 3,

             PRINT_TAB-EXPENSE TO OUTPUT,
             OUTPUT UNDER TEXT-005 COLOR 3.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " PRINTING_REGULAR
*&---------------------------------------------------------------------*
*&      Form  PRINTING_SUMMARY
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINTING_SUMMARY.
LOOP AT ITAB.
MOVE-CORRESPONDING ITAB TO PRINT_TAB.
SELECT SINGLE * FROM CSKU WHERE
                                SPRAS = 'E' AND
                                KTOPL = 'COAT' AND
                                KSTAR = ITAB-KSTAR.

MOVE CSKU-LTEXT TO PRINT_TAB-KTEXT.

APPEND PRINT_TAB.
ENDLOOP.

  LOOP AT PRINT_TAB.
  MOVE PRINT_TAB-KTEXT TO TEMPTEXT.
    AT END OF KSTAR.
    WRITE: /  TEMPTEXT UNDER TEXT-DES.
    SUM.
    WRITE:   PRINT_TAB-KSTAR UNDER TEXT-002,

             PRINT_TAB-TOTAL TO OUTPUT,
             OUTPUT UNDER TEXT-003,

             PRINT_TAB-CAPITAL TO OUTPUT,
             OUTPUT UNDER TEXT-004,

             PRINT_TAB-EXPENSE TO OUTPUT,
             OUTPUT UNDER TEXT-005.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " PRINTING_SUMMARY
