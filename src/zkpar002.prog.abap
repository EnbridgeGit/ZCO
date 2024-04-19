REPORT ZKPAR002 MESSAGE-ID ZZ NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 65.
************************************************************************
*
*   PROGRAM:    ZKPAR002
*   REQUEST ID: DRCO0161
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Centra - Union
*   DATE:       March 22, 1997.
*
*   The purpose of this report is to produce a product hierarchy report
*   listing assigned material numbers, sub program, program, line of
*   busines and texts.
*
************************************************************************

TABLES:   T179, T179T, MARA, MAKT, K9078.

DATA:     BEGIN OF PRODTAB  OCCURS 100,
          PRODH      LIKE T179-PRODH,
          STUFE      LIKE T179-STUFE,
          ZFELD1     LIKE K9078-ZFELD1,
          ZFELD2     LIKE K9078-ZFELD2,
          ZFELD3     LIKE K9078-ZFELD3,
          MATNR      LIKE MARA-MATNR,
          END OF PRODTAB.

DATA:     PRV_PRODH  LIKE T179-PRODH,
          PRV_STUFE  LIKE T179-STUFE,
          PRV_ZFELD1 LIKE K9078-ZFELD1,
          PRV_ZFELD2 LIKE K9078-ZFELD2,
          PRV_ZFELD3 LIKE K9078-ZFELD3,
          PRV_MATNR  LIKE MARA-MATNR,
          MATNR_READ TYPE I.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(30) TEXT-001.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS S_PRODH     FOR T179-PRODH
                           NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS S_STUFE     FOR T179-STUFE
                           NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_incl      RADIOBUTTON GROUP RADI.
SELECTION-SCREEN COMMENT 05(25) TEXT-005.
PARAMETERS: P_excl      RADIOBUTTON GROUP RADI.
SELECTION-SCREEN COMMENT 35(19) TEXT-006.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 23(51) TEXT-007.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX5.

*   End of selection screen.
************************************************************************

TOP-OF-PAGE.
  PERFORM PrintHeader.

START-OF-SELECTION.

  SELECT * FROM T179.

    SELECT SINGLE * FROM K9078
    WHERE  ERKRS      = '1100'
    AND    QFELD3_BIS = T179-PRODH
    AND    DAT2       > SY-DATUM
    AND    DAT1       < SY-DATUM.

    CLEAR MATNR_READ.
    IF P_INCL NE SPACE.
      SELECT * FROM MARA
      WHERE PRDHA = T179-PRODH
      AND ( MTART = 'NLAG'
      OR    MTART = 'DIEN' ).
        MATNR_READ = MATNR_READ + 1.
        MOVE T179-PRODH   TO PRODTAB-PRODH.
        MOVE T179-STUFE   TO PRODTAB-STUFE.
        MOVE K9078-ZFELD1 TO PRODTAB-ZFELD1.
        MOVE K9078-ZFELD2 TO PRODTAB-ZFELD2.
        MOVE K9078-ZFELD3 TO PRODTAB-ZFELD3.
        MOVE MARA-MATNR   TO PRODTAB-MATNR.
        PERFORM APPENDTAB.
      ENDSELECT.
    ELSE.
      MOVE T179-PRODH   TO PRODTAB-PRODH.
      MOVE T179-STUFE   TO PRODTAB-STUFE.
      MOVE K9078-ZFELD1 TO PRODTAB-ZFELD1.
      MOVE K9078-ZFELD2 TO PRODTAB-ZFELD2.
      MOVE K9078-ZFELD3 TO PRODTAB-ZFELD3.
      PERFORM APPENDTAB.
    ENDIF.
    IF MATNR_READ EQ 0.
      MOVE T179-PRODH   TO PRODTAB-PRODH.
      MOVE T179-STUFE   TO PRODTAB-STUFE.
      MOVE K9078-ZFELD1 TO PRODTAB-ZFELD1.
      MOVE K9078-ZFELD2 TO PRODTAB-ZFELD2.
      MOVE K9078-ZFELD3 TO PRODTAB-ZFELD3.
      PERFORM APPENDTAB.
    ENDIF.
  ENDSELECT.

* sort the product hierarchy table.
  SORT PRODTAB BY PRODH STUFE ZFELD1 ZFELD2 ZFELD3 MATNR.

* Process the table, looping thru and outputing detail lines
  LOOP AT PRODTAB.
    IF PRODTAB-PRODH NE PRV_PRODH.
      MOVE PRODTAB-PRODH  TO PRV_PRODH.
      MOVE PRODTAB-STUFE  TO PRV_STUFE.
      MOVE PRODTAB-ZFELD1 TO PRV_ZFELD1.
      MOVE PRODTAB-ZFELD2 TO PRV_ZFELD2.
      MOVE PRODTAB-ZFELD3 TO PRV_ZFELD3.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      SELECT SINGLE * FROM T179T
      WHERE  PRODH = PRODTAB-PRODH
      AND    SPRAS = SY-LANGU.
      MOVE SPACE TO MAKT-MAKTX.
      IF P_INCL NE SPACE.
        SELECT SINGLE * FROM MAKT
        WHERE  MATNR = PRODTAB-MATNR
        AND    SPRAS = SY-LANGU.
      ENDIF.
      WRITE: /002 PRODTAB-PRODH
             ,021 T179T-VTEXT
             ,046 PRODTAB-STUFE
             ,056 PRODTAB-ZFELD1
             ,066 PRODTAB-ZFELD2
             ,075 PRODTAB-ZFELD3
             ,083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    IF PRODTAB-STUFE NE PRV_STUFE.
      MOVE PRODTAB-STUFE  TO PRV_STUFE.
      MOVE PRODTAB-ZFELD1 TO PRV_ZFELD1.
      MOVE PRODTAB-ZFELD2 TO PRV_ZFELD2.
      MOVE PRODTAB-ZFELD3 TO PRV_ZFELD3.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      MOVE SPACE TO MAKT-MAKTX.
      IF P_INCL NE SPACE.
        SELECT SINGLE * FROM MAKT
        WHERE  MATNR = PRODTAB-MATNR
        AND    SPRAS = SY-LANGU.
      ENDIF.
      WRITE: /046 PRODTAB-STUFE
             ,056 PRODTAB-ZFELD1
             ,066 PRODTAB-ZFELD2
             ,075 PRODTAB-ZFELD3
             ,083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    IF PRODTAB-ZFELD1 NE PRV_ZFELD1.
      MOVE PRODTAB-ZFELD1 TO PRV_ZFELD1.
      MOVE PRODTAB-ZFELD2 TO PRV_ZFELD2.
      MOVE PRODTAB-ZFELD3 TO PRV_ZFELD3.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      MOVE SPACE TO MAKT-MAKTX.
      IF P_INCL NE SPACE.
        SELECT SINGLE * FROM MAKT
        WHERE  MATNR = PRODTAB-MATNR
        AND    SPRAS = SY-LANGU.
      ENDIF.
      WRITE: /056 PRODTAB-ZFELD1
             ,066 PRODTAB-ZFELD2
             ,075 PRODTAB-ZFELD3
             ,083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    IF PRODTAB-ZFELD2 NE PRV_ZFELD2.
      MOVE PRODTAB-ZFELD2 TO PRV_ZFELD2.
      MOVE PRODTAB-ZFELD3 TO PRV_ZFELD3.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      MOVE SPACE TO MAKT-MAKTX.
      IF P_INCL NE SPACE.
        SELECT SINGLE * FROM MAKT
        WHERE  MATNR = PRODTAB-MATNR
        AND    SPRAS = SY-LANGU.
      ENDIF.
      WRITE: /066 PRODTAB-ZFELD2
             ,075 PRODTAB-ZFELD3
             ,083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    IF PRODTAB-ZFELD3 NE PRV_ZFELD3.
      MOVE PRODTAB-ZFELD3 TO PRV_ZFELD3.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      MOVE SPACE TO MAKT-MAKTX.
      IF P_INCL NE SPACE.
        SELECT SINGLE * FROM MAKT
        WHERE  MATNR = PRODTAB-MATNR
        AND    SPRAS = SY-LANGU.
      ENDIF.
      WRITE: /075 PRODTAB-ZFELD3
             ,083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    IF PRODTAB-MATNR NE PRV_MATNR.
      MOVE PRODTAB-MATNR  TO PRV_MATNR.
      SELECT SINGLE * FROM MAKT
      WHERE  MATNR = PRODTAB-MATNR
      AND    SPRAS = SY-LANGU.
      WRITE: /083 PRODTAB-MATNR
             ,091 MAKT-MAKTX.
      PERFORM SHOWVLINE.
    ENDIF.
    AT END OF PRODH.
      ULINE.
    ENDAT.
  ENDLOOP.
  ULINE.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by a program.
************************************************************************

FORM PrintSysInfo.
  FORMAT INTENSIFIED OFF.
  WRITE: /001 SY-DATUM
         ,117 TEXT-008, SY-PAGNO.
  WRITE: /    SY-UZEIT UNDER SY-DATUM
         ,    SY-REPID UNDER TEXT-008.
  FORMAT INTENSIFIED ON.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM AppendTab                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM APPENDTAB.
  IF S_PRODH EQ SPACE.
    IF S_STUFE EQ SPACE.
      APPEND PRODTAB.
      CLEAR PRODTAB.
    ELSE.
      IF PRODTAB-STUFE IN S_STUFE.
        APPEND PRODTAB.
        CLEAR PRODTAB.
      ENDIF.
    ENDIF.
  ELSE.
    IF S_STUFE EQ SPACE.
      IF PRODTAB-PRODH IN S_PRODH.
        APPEND PRODTAB.
        CLEAR PRODTAB.
      ENDIF.
    ELSE.
      IF  PRODTAB-PRODH IN S_PRODH
      AND PRODTAB-STUFE IN S_STUFE.
        APPEND PRODTAB.
        CLEAR PRODTAB.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintTitle                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintTitle.
  WRITE: /050 TEXT-001.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ShowVline                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ShowVline.
  WRITE:  001 SY-VLINE
         ,020 SY-VLINE
         ,042 SY-VLINE
         ,052 SY-VLINE
         ,062 SY-VLINE
         ,072 SY-VLINE
         ,082 SY-VLINE
         ,090 SY-VLINE
         ,132 SY-VLINE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintColHead                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintColHead.
  FORMAT INTENSIFIED ON.
  ULINE.
  WRITE: /002  TEXT-009
         ,021  TEXT-010
         ,043  TEXT-010
         ,056  TEXT-011
         ,074  TEXT-012
         ,084  TEXT-013
         ,091  TEXT-013.
  PERFORM ShowVline.
  WRITE: /002  TEXT-010
         ,021  TEXT-014
         ,044  TEXT-015
         ,054  TEXT-016
         ,064  TEXT-016
         ,074  TEXT-017
         ,083  TEXT-018
         ,091  TEXT-014.
  PERFORM ShowVline.
  ULINE.
  FORMAT INTENSIFIED OFF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintHeader                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintHeader.
  PERFORM PrintSysInfo.
  PERFORM PrintTitle.
  SKIP 1.
  PERFORM PrintColHead.
ENDFORM.
