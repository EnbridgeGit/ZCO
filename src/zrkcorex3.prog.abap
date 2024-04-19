
REPORT  ZRKCOREX3.
************************************************************************
* THIS REPORT ELIMINATES DOUBLE EXTERNAL NUMBERS OF SETTLEMENT RULES   *
* COBRB-EXTNR. IT SETS ALL EXTERNAL NUMBERS EQUAL TO THE CURRENT       *
* NUMBER COBRB-LFDNR OF THE SETTLEMENT RULE!                           *
* ATTENTION: IF YOU USE EXTERNAL NUMBERS WHICH ARE DIFFERENT FROM THE  *
* CURRENT NUMBERS (THESE ARE DETERMINED AUTOMATICALLY BY THE SYSTEM)   *
* DO NOT USE THE REPORT IF YOU NEED TO KEEP THE EXTERNAL               *
* NUMBERS.                                                             *
* PARAMETER REP_ONLY:                                                  *
*          'X'        ONLY DUPLICATE EXTERNAL NUMBERS WILL BE CHANGED  *
*                     FIRST DOUBLE EXTNR WILL STAY AS IT IS, ALL OTHER *
*                     DUPLICATES WILL BE REPLACED BY COBRB-LFDNR       *
*                                                                      *
*          ' '        ALL EXTERNAL NUMBERS WILL BE REPLACED BY         *
*                     COBRB-LFDNR                                      *
************************************************************************
TABLES: COBRB.
DATA: IT_COBRB LIKE COBRB OCCURS 0,
      IT_COBRB_REP LIKE COBRB OCCURS 0,
      IS_COBRB LIKE COBRB,
      I_OBJNR LIKE COBRB-OBJNR,
      I_AVORG LIKE COBRB-AVORG,
      I_VERSN LIKE COBRB-VERSN,
      I_EXTNR LIKE COBRB-EXTNR,
      ZEILEN TYPE N.
************************************************************************
PARAMETERS: TESTRUN TYPE C DEFAULT 'X',
            REP_ONLY TYPE C DEFAULT 'X',
            PLAN TYPE C DEFAULT 'X'.

SELECT-OPTIONS: OBJNR FOR COBRB-OBJNR OBLIGATORY,
                VERSION FOR COBRB-VERSN.
************************************************************************
* read settlement rules
IF PLAN = 'X'.
 I_AVORG = 'KOAP'.
ELSE.
 I_AVORG = 'KOAO'.
ENDIF.

SELECT * FROM COBRB INTO TABLE IT_COBRB
                    WHERE OBJNR IN OBJNR
                      AND AVORG = I_AVORG
                      AND VERSN IN VERSION.
  CHECK SY-SUBRC EQ 0.
  IF REP_ONLY NE ' '.
    SORT IT_COBRB BY OBJNR AVORG VERSN EXTNR.
    LOOP AT IT_COBRB INTO IS_COBRB.
      IF IS_COBRB-OBJNR NE I_OBJNR.
        CLEAR I_EXTNR.
      ENDIF.
      IF IS_COBRB-VERSN NE I_VERSN.
        CLEAR I_EXTNR.
      ENDIF.
      IF IS_COBRB-EXTNR = I_EXTNR.  "Duplikat!
        APPEND IS_COBRB TO IT_COBRB_REP.
      ENDIF.
      I_OBJNR = IS_COBRB-OBJNR.
      I_AVORG = IS_COBRB-AVORG.
      I_VERSN = IS_COBRB-VERSN.
      I_EXTNR = IS_COBRB-EXTNR.
    ENDLOOP.
    IT_COBRB[] = IT_COBRB_REP[].
  ENDIF.
  DESCRIBE TABLE IT_COBRB LINES ZEILEN.
  IF ZEILEN = 0.
    WRITE: /'No double EXTNR found, nothing to repair'.
  ENDIF.
* set external numbers equal to internal numbers
  LOOP AT IT_COBRB INTO IS_COBRB.
    CHECK IS_COBRB-EXTNR NE IS_COBRB-LFDNR.
    IS_COBRB-EXTNR = IS_COBRB-LFDNR.
    MODIFY IT_COBRB FROM IS_COBRB.
    WRITE: /'Objektnumber:', IS_COBRB-OBJNR.
    WRITE: /'  Update COBRB: rule no.', IS_COBRB-LFDNR,
               ' External: ', IS_COBRB-EXTNR,
               ' Bureg: ', IS_COBRB-BUREG.
  ENDLOOP.
  IF TESTRUN <> 'X'.
    UPDATE COBRB FROM TABLE IT_COBRB.
    COMMIT WORK.
    WRITE: /'Update posted'.
  ELSE.
    WRITE:/'Testrun finished'.
  ENDIF.
