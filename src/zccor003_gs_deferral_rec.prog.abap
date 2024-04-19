*&---------------------------------------------------------------------*
*& Report  ZCCOR003_GS_DEFERRAL_REC
*&
*&---------------------------------------------------------------------*
* AUTHOR:      Mohammad T. Khan                                        *
* DATE:        March 2011.                                          *
* PROJECT:     Cost of Gas.                                            *
* ISSUE LOG:   TR804                                                   *
* DESCRIPTION:                                                         *
* This program will retrieve the Gas Supply Receipts for an Accounting *
* Period and compare the value against the Alberta Border Price        *
* Component of the Standard Cost Estimate.                             *
*&---------------------------------------------------------------------*
* Changes:
* 2011/12/02 gymana tr804 - Correct the +/- logic for the amount and
*                           quantity fields.
* 2012/06/25 btboundy - Fixed BWDAT date selection, added BLDAT
*                       Added ZLOC to agreement types
*
*2013/09/12  M Khan SDP54955 - Remove duplicate rows
*2016/02/02  G Ymana ACR-578 - Modify MKPF/MSEG Select statement
*2016/11/17  G Ymana ACR-1346 - Modify to add component 80 lookup
*2018/08/03  AKMADASU  CHG0119991 - Modify to selection screen
*                                   and MKPF/MSEG Select statement
*2021/12/01  BIRUDURD COG changes to get MSA,Locations and Tradin Buyer
*&--------------------------------------------------------------------*

REPORT  ZCCOR003_GS_DEFERRAL_REC.
TYPE-POOLS: SLIS.

TABLES:
   MKPF,       "Header: Material Document
   MSEG,       "Document Segment: Material
   EKKO,       "Purchasing Document Header
   CKIS.       "Items Unit Costing/Itemization Product Costing


DATA: BEGIN OF ITAB OCCURS 1,
      MJAHR  LIKE MKPF-MJAHR,        "Material Document Year
      VGART  LIKE MKPF-VGART,        "Transaction/Event Type
      BUDAT  LIKE MKPF-BUDAT,        "Posting Date in the Document
      BLDAT  LIKE MKPF-BLDAT,        "Document Date
      MBLNR  LIKE MKPF-MBLNR,        "Number of Material Document
      XBLNR  LIKE MKPF-XBLNR,        "Reference Document Number
      FRBNR  LIKE MKPF-FRBNR,        "Number of Bill of Lading
      BSART  LIKE EKKO-BSART,        "Agreement Type
      SGTXT  LIKE MSEG-SGTXT,        "Item Text
      MATNR  LIKE MSEG-MATNR,        "Material Number
      WERKS  LIKE MSEG-WERKS,        "Plant
      BWART  LIKE MSEG-BWART,        "Movement Type (Inventory Mgmt)
      LGORT  LIKE MSEG-LGORT,        "Storage Location
      LIFNR  LIKE MSEG-LIFNR,        "Vendor Account Number
      EBELN  LIKE MSEG-EBELN,        "Purchase Order Number
      DMBTR  LIKE MSEG-DMBTR,        "Amount in Local Currency
      BUALT  LIKE MSEG-BUALT,        "Amt Posted in Alt. Price Control
      MENGE  LIKE MSEG-MENGE,        "Quantity
      KALNR  LIKE KEKO-KALNR,        "Cost Estimate # - Product Costing
      KADKY  LIKE KEKO-KADKY,        "Costing Date (Key)
*  Start of COG Change
      ZZPARTY_AGMT_ID like ekko-ZZPARTY_AGMT_ID,
      ZZEKGRP     like ekko-ZZEKGRP,
      zzparty     like ekko-zzparty,
      zztrloc1    like ekko-zztrloc1,
      zztrloc2    like ekko-zztrloc2,
      zztrloc3    like ekko-zztrloc3,
      zztrloc4    like ekko-zztrloc4,
      zzoldealid  like ekko-zzoldealid,
*  End of COG Change
      END OF ITAB.

DATA: BEGIN OF REPTAB OCCURS 1,
      MJAHR  LIKE MKPF-MJAHR,        "Material Document Year
      VGART  LIKE MKPF-VGART,        "Transaction/Event Type
      BUDAT  LIKE MKPF-BUDAT,        "Posting Date in the Document
      BLDAT  LIKE MKPF-BLDAT,        "Document Date
      MBLNR  LIKE MKPF-MBLNR,        "Number of Material Document
      XBLNR  LIKE MKPF-XBLNR,        "Reference Document Number
      FRBNR  LIKE MKPF-FRBNR,        "Bill of Lading
      BSART  LIKE EKKO-BSART,        "Agreement Type
      SGTXT  LIKE MSEG-SGTXT,        "Item Text
      MATNR  LIKE MSEG-MATNR,        "Material Number
      WERKS  LIKE MSEG-WERKS,        "Plant
      BWART  LIKE MSEG-BWART,        "Movement Type (Inventory Mgmt)
      LGORT  LIKE MSEG-LGORT,        "Storage Location
      LIFNR  LIKE MSEG-LIFNR,        "Vendor Account Number
      EBELN  LIKE MSEG-EBELN,        "Purchase Order Number
      DMBTR  LIKE MSEG-DMBTR,        "Amount in Local Currency
      BUALT  LIKE MSEG-BUALT,        "Amt Posted in Alt. Price Control
      MENGE  LIKE MSEG-MENGE,        "Quantity
      GPREIS10 LIKE CKIS-GPREIS,     "ALB BR PRICE
      GPREIS20 LIKE CKIS-GPREIS,     "TRN DMD
      GPREIS30 LIKE CKIS-GPREIS,     "TRN COMM
      GPREIS40 LIKE CKIS-GPREIS,     "TRN FUEL
      GPREIS80 LIKE CKIS-GPREIS,     "DAWN Ref Price            ACR1346
      VALAMT LIKE  MSEG-DMBTR,       "Alberta Border Price Amount
      VALAMT1 LIKE MSEG-DMBTR,       "WACOG: Tran Demand Amount
      VALAMT2 LIKE MSEG-DMBTR,       "WACOG: Tran Commidity Amount
      VALAMT3 LIKE MSEG-DMBTR,       "WACOG: Tran Fuel Amount
      VALAMT4 LIKE MSEG-DMBTR,       "Dawn Reference Price Amt  ACR1346
      DIFF    LIKE MSEG-DMBTR,       "Difference
*  Start of COG Change
      "ZZPARTY_AGMT_ID like ekko-ZZPARTY_AGMT_ID, "MSA ID
      ZZMSA       like ZMMT_MASTAGREE-ZZMSA,     "MSA
      ZZEKGRP     like ekko-ZZEKGRP,             "Trading Buyer
      EKNAM       like T024-EKNAM,               "Trading buyer Name
      zzparty     like ekko-zzparty,             "Pipeline ID
      zztrloc1    like ekko-zztrloc1,            "Location 1
      zztrloc2    like ekko-zztrloc2,            "Location 2
      zztrloc3    like ekko-zztrloc3,            "Location 3
      zztrloc4    like ekko-zztrloc4,            "Location 4
      zzoldealid  like ekko-zzoldealid,          "OpnLink Deal ID
*  End of COG Change
      END OF REPTAB.
*  Start of COG Change
TYPES: BEGIN OF ty_msa,
         zzparty_agmt_id type zmmt_mastagree-zzparty_agmt_id,
         zzmsa type zmmt_mastagree-zzmsa,
       END OF ty_msa,

       BEGIN OF ty_tb,
         ekgrp type t024-ekgrp,
         eknam type t024-eknam,
       END OF ty_tb.
DATA: ls_msa type ty_msa,
      ls_tb type ty_tb,
      lt_msa type standard table of ty_msa,
      lt_tb type standard table of ty_tb.
*  End of COG Change

DATA: BEGIN OF CKISTAB OCCURS 1,
      KALNR LIKE CKIS-KALNR,          "Cost Estimate Number
      KADKY LIKE CKIS-KADKY,          "Costing Date (Key)
      ELEMT LIKE CKIS-ELEMT,          "Cost Component Number
      GPREIS LIKE CKIS-GPREIS,        "Price in Controlling Area Curr
      END OF CKISTAB.

DATA: W_HEAD01(60)  TYPE  C,
      W_HEAD02(60)  TYPE  C,
      ES_VARIANT    LIKE  DISVARIANT,
      IS_VARIANT    LIKE  DISVARIANT.
.

*Data Fields to Restrict ranges
TYPE-POOLS: SSCR.
DATA: L_RESTRICT TYPE SSCR_RESTRICT,
      L_OPT_LIST TYPE SSCR_OPT_LIST,
      L_ASS TYPE SSCR_ASS.
*------------------------  Selection Screen  --------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETERS:
    P_MJAHR  LIKE MKPF-MJAHR   DEFAULT '2011'   OBLIGATORY,
    P_MATNR  LIKE MSEG-MATNR   DEFAULT 'NATGAS' OBLIGATORY.
SELECT-OPTIONS:
    S_BUDAT  FOR  MKPF-BUDAT   DEFAULT SY-DATUM OBLIGATORY,
    S_BWART  FOR  MSEG-BWART   DEFAULT '101'    OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.         "Display Variant

SELECTION-SCREEN END OF BLOCK BOX.
**-- START OF CHANGES BY AKMADASU CHG0119991
SELECTION-SCREEN BEGIN OF BLOCK B11 WITH FRAME TITLE TEXT-111.
PARAMETERS:P_BUDATS TYPE BUDAT,
           P_BUDATE TYPE BUDAT.
SELECTION-SCREEN END OF BLOCK B11.
**-- END OF CHANGES BY AKMADASU CHG0119991

***********************************************************************
*                      INITIALIZATION                                 *
***********************************************************************
INITIALIZATION.

  CLEAR L_OPT_LIST.
  L_OPT_LIST-NAME = 'S_BWART'.
  L_OPT_LIST-OPTIONS-EQ = 'X'.
  APPEND L_OPT_LIST TO L_RESTRICT-OPT_LIST_TAB.

  CLEAR L_ASS.
  L_ASS-KIND = 'S'.
  L_ASS-NAME = 'S_BWART'.
  L_ASS-SG_MAIN = 'I'.
  L_ASS-OP_MAIN = 'S_BWART'.
  APPEND L_ASS TO L_RESTRICT-ASS_TAB.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      RESTRICTION = L_RESTRICT
    EXCEPTIONS
      OTHERS      = 1.

  MOVE '102' TO S_BWART-LOW.
  APPEND S_BWART.

*Select Display variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZCCOR003_GS_DEFERRAL_REC'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT          = IS_VARIANT
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      I_SAVE              = 'A'
    IMPORTING
*     E_EXIT              =
      ES_VARIANT          = ES_VARIANT
    EXCEPTIONS
      NOT_FOUND           = 1
      PROGRAM_ERROR       = 2
      OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.


***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

  PERFORM GET_DB_DATA.
  PERFORM BUILD_REPORT_TABLE.
  PERFORM DISPLAY_ALV_GRID_DATA.

***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
FORM GET_DB_DATA.

  IF S_BUDAT-HIGH = '00000000'.
    S_BUDAT-HIGH = S_BUDAT-LOW.
  ENDIF.
**-- START OF CHANGES BY AKMADASU FOR CHG0119991
  IF P_BUDATS IS NOT INITIAL AND P_BUDATE IS NOT INITIAL.
    SELECT MKPF~MJAHR MKPF~VGART MKPF~BUDAT MKPF~BLDAT MKPF~MBLNR
           MKPF~XBLNR
           MKPF~FRBNR EKKO~BSART MSEG~SGTXT MSEG~MATNR MSEG~WERKS
           MSEG~BWART MSEG~LGORT MSEG~LIFNR MSEG~EBELN MSEG~DMBTR
           MSEG~BUALT MSEG~MENGE KEKO~KALNR KEKO~KADKY
    " Start of COG Change
           EKKO~ZZPARTY_AGMT_ID EKKO~ZZEKGRP ekko~zzparty
           ekko~zztrloc1 ekko~zztrloc2 ekko~zztrloc3 ekko~zztrloc4
           ekko~zzoldealid
     " End of COG Change
      INTO TABLE ITAB
      FROM    ( ( ( MKPF INNER JOIN MSEG
                        ON MSEG~MBLNR = MKPF~MBLNR AND
                           MSEG~MJAHR = MKPF~MJAHR )
                        INNER JOIN KEKO
                        ON KEKO~MATNR = MSEG~MATNR   AND
                           KEKO~WERKS = MSEG~WERKS )
                        INNER JOIN EKKO
                        ON EKKO~EBELN = MSEG~EBELN )

      WHERE  MKPF~MJAHR =  P_MJAHR
        AND  MKPF~BUDAT IN S_BUDAT
        AND  MSEG~BWART IN S_BWART
        AND  MSEG~MATNR =  P_MATNR
        AND  KEKO~BWDAT =< P_BUDATS
        AND  KEKO~BIDAT => P_BUDATE
        AND  KEKO~FEH_STA = 'FR'.

    IF SY-SUBRC <> 0.
      WRITE: /1 'NO DATA SELECTED'.
      STOP.
    ENDIF.
  ELSE.
**-- END OF CHANGES BY AKMADASU FOR CHG0119991
SELECT MKPF~MJAHR MKPF~VGART MKPF~BUDAT MKPF~BLDAT MKPF~MBLNR MKPF~XBLNR
           MKPF~FRBNR EKKO~BSART MSEG~SGTXT MSEG~MATNR MSEG~WERKS
           MSEG~BWART MSEG~LGORT MSEG~LIFNR MSEG~EBELN MSEG~DMBTR
           MSEG~BUALT MSEG~MENGE KEKO~KALNR KEKO~KADKY
    " Start of COG Change
           EKKO~ZZPARTY_AGMT_ID EKKO~ZZEKGRP ekko~zzparty
           ekko~zztrloc1 ekko~zztrloc2 ekko~zztrloc3 ekko~zztrloc4
           ekko~zzoldealid
     " End of COG Change
      INTO TABLE ITAB
      FROM    ( ( ( MKPF INNER JOIN MSEG
                        ON MSEG~MBLNR = MKPF~MBLNR AND
                           MSEG~MJAHR = MKPF~MJAHR )
                        INNER JOIN KEKO
                        ON KEKO~MATNR = MSEG~MATNR   AND
                           KEKO~WERKS = MSEG~WERKS )
                        INNER JOIN EKKO
                        ON EKKO~EBELN = MSEG~EBELN )

      WHERE  MKPF~MJAHR =  P_MJAHR
        AND  MKPF~BUDAT IN S_BUDAT
        AND  MSEG~BWART IN S_BWART
        AND  MSEG~MATNR =  P_MATNR
        AND  KEKO~BWDAT =< MKPF~BUDAT
        AND  KEKO~BIDAT => MKPF~BUDAT
        AND  KEKO~FEH_STA = 'FR'.

    IF SY-SUBRC <> 0.
      WRITE: /1 'NO DATA SELECTED'.
      STOP.
    ENDIF.

  ENDIF. " ADDED BY AKMADASU CHG0119991
  " Start of COG Change
  IF ITAB[] IS NOT INITIAL.
    select ZZPARTY_AGMT_ID ZZMSA
       FROM zmmt_mastagree into table lt_msa
       FOR ALL ENTRIES IN itab
       WHERE ZZPARTY_AGMT_ID = itab-ZZPARTY_AGMT_ID.

    SELECT ekgrp eknam
      FROM t024 into table lt_tb
      FOR ALL ENTRIES IN itab
      WHERE ekgrp = itab-zzekgrp.
  ENDIF.
  " End of COG Change
  SELECT KALNR KADKY ELEMT GPREIS
    FROM CKIS
    INTO TABLE CKISTAB.

  SORT CKISTAB BY KALNR KADKY ELEMT.
ENDFORM.
***********************************************************************
*                        BUILD_REPORT_TABLE.                          *
***********************************************************************
FORM BUILD_REPORT_TABLE.

  DATA: W_ELEMT LIKE CKIS-ELEMT.

  LOOP AT ITAB.

* Adjust amounts depending on movement type.                     "TR804
    IF ITAB-BWART = 102 OR ITAB-BWART = 104 OR                   "TR804
       ITAB-BWART = 122 OR ITAB-BWART = 522 OR                   "TR804
       ITAB-BWART = 512.                                         "TR804
      ITAB-MENGE = ITAB-MENGE * -1.                              "TR804
      ITAB-DMBTR = ITAB-DMBTR * -1.                              "TR804
      ITAB-BUALT = ITAB-BUALT * -1.                              "TR804
    ENDIF.                                                       "TR804

    MOVE-CORRESPONDING ITAB TO REPTAB.
* Start of COG Change
    clear ls_msa.
    READ TABLE lt_msa into ls_msa
      with KEY zzparty_agmt_id = itab-zzparty_agmt_id.
    IF sy-subrc = 0.
      REPTAB-zzmsa = ls_msa-zzmsa.
    ENDIF.

    clear ls_tb.
    READ TABLE lt_tb into ls_tb
      with KEY ekgrp = itab-zzekgrp.
    IF sy-subrc = 0.
      REPTAB-eknam = ls_tb-eknam.
    ENDIF.
* End of COG Change
    CLEAR W_ELEMT.
    DO 10 TIMES.                                            "ACR1346
      ADD 10 TO W_ELEMT.
      READ TABLE CKISTAB WITH KEY KALNR = ITAB-KALNR
                                  KADKY = ITAB-KADKY
                                  ELEMT = W_ELEMT.
      IF SY-SUBRC EQ 0.
        IF W_ELEMT = 10.
          MOVE CKISTAB-GPREIS TO REPTAB-GPREIS10.
          REPTAB-VALAMT = ( CKISTAB-GPREIS / 1000 ) * ITAB-MENGE.
        ELSEIF W_ELEMT = 20.
          MOVE CKISTAB-GPREIS TO REPTAB-GPREIS20.
          REPTAB-VALAMT1 = ( CKISTAB-GPREIS / 1000 ) * ITAB-MENGE.
        ELSEIF W_ELEMT = 30.
          MOVE CKISTAB-GPREIS TO REPTAB-GPREIS30.
          REPTAB-VALAMT2 = ( CKISTAB-GPREIS / 1000 ) * ITAB-MENGE.
        ELSEIF W_ELEMT = 80.
          MOVE CKISTAB-GPREIS TO REPTAB-GPREIS80.
          REPTAB-VALAMT4 = ( CKISTAB-GPREIS / 1000 ) * ITAB-MENGE.
        ELSE.
          MOVE CKISTAB-GPREIS TO REPTAB-GPREIS40.
          REPTAB-VALAMT3 = ( CKISTAB-GPREIS / 1000 ) * ITAB-MENGE.
        ENDIF.
      ENDIF.
    ENDDO.

    IF ITAB-BSART = 'DAWN' OR ITAB-BSART = 'ZLOC'.
      REPTAB-DIFF = ITAB-BUALT - ( REPTAB-VALAMT  + REPTAB-VALAMT1 +
                                   REPTAB-VALAMT2 + REPTAB-VALAMT3 +
                                   REPTAB-VALAMT4 ).        "ACR1346
    ELSE.
      REPTAB-DIFF = ITAB-BUALT -                            "ACR1346
                    ( REPTAB-VALAMT + REPTAB-VALAMT4 ).     "ACR1346
    ENDIF.

    APPEND REPTAB.
    CLEAR  REPTAB.
  ENDLOOP.
  SORT REPTAB BY BUDAT.
* SORT REPTAB BY MJAHR VGART BUDAT MBLNR XBLNR MATNR WERKS BWART LGORT.
ENDFORM.

***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM DISPLAY_ALV_GRID_DATA.

  DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        FC_STR   TYPE SLIS_FIELDCAT_ALV,
        LAYOUT   TYPE SLIS_LAYOUT_ALV,
        TITLE    TYPE LVC_TITLE,
        REPID    LIKE SY-REPID,
        VARIANT  LIKE DISVARIANT,
        SORT     TYPE SLIS_T_SORTINFO_ALV,
        SORT_STR TYPE SLIS_SORTINFO_ALV.

  DATA: W_DATE_FROM(10) TYPE C,
        W_DATE_TO(10)   TYPE C.

  CONCATENATE S_BUDAT-LOW(4) '/' S_BUDAT-LOW+4(2) '/' S_BUDAT-LOW+6(2)
                                                     INTO W_DATE_FROM.
CONCATENATE S_BUDAT-HIGH(4) '/' S_BUDAT-HIGH+4(2) '/' S_BUDAT-HIGH+6(2)
                                                     INTO W_DATE_TO.

  CONCATENATE TEXT-004 P_MJAHR TEXT-005 W_DATE_FROM TEXT-006 W_DATE_TO
              INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = REPID
      I_INTERNAL_TABNAME     = 'REPTAB'
      I_INCLNAME             = REPID
    CHANGING
      CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
  LOOP AT FIELDCAT INTO FC_STR.

    CASE FC_STR-FIELDNAME.
      WHEN 'MBLNR'.
        FC_STR-KEY    = ' '.               " Key columns-not first
      WHEN 'ELEHK'.
        FC_STR-KEY    = ' '.               " Key columns-not first
      WHEN 'MENGE'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'DMBTR'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'BUALT'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'VALAMT'.
        FC_STR-SELTEXT_L = TEXT-C19.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'DIFF'.
        FC_STR-SELTEXT_L = TEXT-C20.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'GPREIS10'.
        FC_STR-SELTEXT_L = TEXT-C21.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'GPREIS20'.
        FC_STR-SELTEXT_L = TEXT-C22.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'GPREIS30'.
        FC_STR-SELTEXT_L = TEXT-C23.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'GPREIS40'.
        FC_STR-SELTEXT_L = TEXT-C24.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum VALAMT
* ACR-1346
      WHEN 'GPREIS80'.
        FC_STR-SELTEXT_L = TEXT-C30.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum VALAMT
      WHEN 'VALAMT'.
        FC_STR-SELTEXT_L = TEXT-C25.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'VALAMT1'.
        FC_STR-SELTEXT_L = TEXT-C26.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'VALAMT2'.
        FC_STR-SELTEXT_L = TEXT-C27.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'VALAMT3'.
        FC_STR-SELTEXT_L = TEXT-C28.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
* ACR-1346
      WHEN 'VALAMT4'.
        FC_STR-SELTEXT_L = TEXT-C29.        " Alternative col header
        FC_STR-DDICTXT = 'L'.
        FC_STR-DO_SUM  = 'X'.               " Do Sum
      WHEN 'EKNAM'.                         " COG Addition
        FC_STR-seltext_l = 'Trd Buyer Name'(C31).
        FC_STR-seltext_m = 'Trd Buyer Name'(C31).
        FC_STR-seltext_s = 'TBy Name'(C32).
      WHEN OTHERS.
* FC_STR-NO_OUT = 'X'.           " hide column
* FC_STR-KEY    = ' '.           " Key columns-not first
* FC_STR-DO_SUM = 'X'.           " Do Sum

    ENDCASE.
*
    MODIFY FIELDCAT FROM FC_STR.
  ENDLOOP.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IT_FIELDCAT             = FIELDCAT
      IS_LAYOUT               = LAYOUT
      I_CALLBACK_TOP_OF_PAGE  = 'ALV_TOP_OF_PAGE'
      I_CALLBACK_PROGRAM      = repid
      I_SAVE                  = 'A'
      IS_VARIANT              = variant
      IT_SORT                 = sort
*     I_GRID_TITLE            = TITLE
*     I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
      T_OUTTAB                = REPTAB
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
