REPORT ZKALER02.

TABLES: AUFK,
        COBK,
        COEP,
        T000.

DATA: T_COBK  LIKE COBK OCCURS 1 WITH HEADER LINE,
      T_COEPO LIKE COEP OCCURS 1 WITH HEADER LINE,
      T_COEPP LIKE COEP OCCURS 1 WITH HEADER LINE,
      T_COEPQ LIKE COEP OCCURS 1 WITH HEADER LINE.

DATA: BEGIN OF T_AUFK OCCURS 0,
        AUFNR     LIKE AUFK-AUFNR,
        OBJNR     LIKE AUFK-OBJNR,
        LOGSYSTEM LIKE AUFK-LOGSYSTEM,
      END OF T_AUFK.

DATA: BEGIN OF T_SUMBZ OCCURS 0,
        KOKRS LIKE COBK-KOKRS,
        BELNR LIKE COBK-BELNR,
        COUNT LIKE SY-TABIX,
      END OF T_SUMBZ.

DATA: O_CNT LIKE SY-TABIX,
      P_CNT LIKE SY-TABIX.

PARAMETERS: P_UPDATE LIKE MKAUF-MARKE.

START-OF-SELECTION.

  SELECT SINGLE * FROM T000 WHERE MANDT = SY-MANDT.

  SELECT * FROM AUFK INTO CORRESPONDING FIELDS OF TABLE T_AUFK
                   WHERE LOGSYSTEM NE SPACE
                   AND   LOGSYSTEM NE T000-LOGSYS.

  DESCRIBE TABLE T_AUFK LINES SY-TABIX.
  WRITE: / 'Number of orders with LOGSYSTEM <>', T000-LOGSYS,
           ' : ', SY-TABIX COLOR COL_NEGATIVE.
  SKIP 1.
  FORMAT COLOR COL_HEADING.
  WRITE: / 'Order', 14 'Logsystem', 26 'Action'.
  FORMAT COLOR COL_NORMAL.

  LOOP AT T_AUFK.

    WRITE: / T_AUFK-AUFNR,
             14 T_AUFK-LOGSYSTEM.
    SELECT * FROM COEP INTO TABLE T_COEPO
                       WHERE LEDNR   =  '00'
                       AND   OBJNR   =  T_AUFK-OBJNR
                       AND   LOGSYSO NE SPACE
                       AND   LOGSYSO NE T000-LOGSYS.
    DESCRIBE TABLE T_COEPO LINES SY-TABIX.
    WRITE: 26 'correcting', (3) SY-TABIX,
              'line items with wrong LOGSYSO'.
    ADD SY-TABIX TO O_CNT.

    IF SY-TABIX > 0.


      REFRESH T_SUMBZ.
      T_SUMBZ-COUNT = 1.
      LOOP AT T_COEPO.
        T_SUMBZ-KOKRS = T_COEPO-KOKRS.
        T_SUMBZ-BELNR = T_COEPO-BELNR.
        COLLECT T_SUMBZ.
        T_COEPO-LOGSYSO = SPACE.
        MODIFY T_COEPO.
      ENDLOOP.
      REFRESH T_COBK.
      LOOP AT T_SUMBZ.
        SELECT SINGLE * FROM COBK WHERE KOKRS = T_SUMBZ-KOKRS
                                  AND   BELNR = T_SUMBZ-BELNR.


        IF SY-SUBRC <> 0.
          RAISE MISSING_COBK.
        ENDIF.
        ADD T_SUMBZ-COUNT TO COBK-SUMBZ.
        SUBTRACT T_SUMBZ-COUNT FROM COBK-ALEBZ.
        APPEND COBK TO T_COBK.
      ENDLOOP.
      SORT T_COBK BY KOKRS BELNR.

      IF P_UPDATE = 'X'.
        UPDATE COEP FROM TABLE T_COEPO.
        UPDATE COBK FROM TABLE T_COBK.
        CALL FUNCTION 'K_DOCUMENT_UPDATE'
             EXPORTING
                  I_TOTALS_UPDATE = 'X'
                  I_RCL_UPDATE    = 'X'
                  I_ITEMS_INSERT  = ' '
                  I_SAVE_DIRECTLY = 'X'
             TABLES
                  T_COBK          = T_COBK
                  T_COEP          = T_COEPO.
      ENDIF.

      SELECT * FROM COEP INTO TABLE T_COEPP
                         FOR ALL ENTRIES IN T_COEPO
                         WHERE KOKRS   =  T_COEPO-KOKRS
                         AND   BELNR   =  T_COEPO-BELNR
                         AND   OBJNR   =  T_COEPO-PAROB1
                         AND   PAROB1  =  T_AUFK-OBJNR
                       AND   LOGSYSP NE SPACE
                       AND   LOGSYSP NE T000-LOGSYS.
    DESCRIBE TABLE T_COEPP LINES SY-TABIX.
    WRITE: /26 'correcting', (3) SY-TABIX,
                'line items with wrong LOGSYSP'.
    ADD SY-TABIX TO P_CNT.

    IF SY-TABIX > 0.

      REFRESH T_COEPQ.
      REFRESH T_SUMBZ.
      T_SUMBZ-COUNT = 1.
      LOOP AT T_COEPP.
        T_SUMBZ-KOKRS = T_COEPP-KOKRS.
        T_SUMBZ-BELNR = T_COEPP-BELNR.
        COLLECT T_SUMBZ.
        IF T_COEPP-LOGSYSO = SPACE
        OR T_COEPP-LOGSYSO = T000-LOGSYS.
          T_COEPQ = T_COEPP.
          MULTIPLY T_COEPQ-WTGBTR BY -1.
          MULTIPLY T_COEPQ-WOGBTR BY -1.
          MULTIPLY T_COEPQ-WKGBTR BY -1.
          MULTIPLY T_COEPQ-WKFBTR BY -1.
          MULTIPLY T_COEPQ-PAGBTR BY -1.
          MULTIPLY T_COEPQ-PAFBTR BY -1.
          MULTIPLY T_COEPQ-MEGBTR BY -1.
          MULTIPLY T_COEPQ-MEFBTR BY -1.
          APPEND T_COEPQ.
        ENDIF.


        T_COEPP-LOGSYSP = SPACE.
        MODIFY T_COEPP.
        IF T_COEPP-LOGSYSO = SPACE
        OR T_COEPP-LOGSYSO = T000-LOGSYS.
            APPEND T_COEPP TO T_COEPQ.
          ENDIF.
        ENDLOOP.

        IF T_SUMBZ[] IS INITIAL.
          RAISE PROGRAM_ERROR.
        ENDIF.

        SELECT * FROM COBK INTO TABLE T_COBK
                           FOR ALL ENTRIES IN T_SUMBZ
                           WHERE KOKRS = T_SUMBZ-KOKRS
                           AND   BELNR = T_SUMBZ-BELNR.
        IF T_COBK[] IS INITIAL.
          RAISE MISSING_COBK.
        ENDIF.
        SORT T_COBK BY KOKRS BELNR.

        IF P_UPDATE = 'X'.
          UPDATE COEP FROM TABLE T_COEPP.
          IF NOT T_COEPQ[] IS INITIAL.
            CALL FUNCTION 'K_DOCUMENT_UPDATE'
                 EXPORTING
                      I_TOTALS_UPDATE = 'X'
                      I_RCL_UPDATE    = 'X'
                      I_ITEMS_INSERT  = ' '
                      I_SAVE_DIRECTLY = 'X'
                 TABLES
                      T_COBK          = T_COBK
                      T_COEP          = T_COEPQ.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    WRITE: /26 'correcting wrong LOGSYSTEM in AUFK'.
  IF P_UPDATE = 'X'.
    UPDATE AUFK SET LOGSYSTEM = SPACE WHERE AUFNR = T_AUFK-AUFNR.
    WRITE: /26 'correcting wrong LOGSYSTEM in master data'.
  ENDIF.

  COMMIT WORK.

ENDLOOP.

SKIP.
WRITE: / 'Total number of line items with wrong LOGSYSO:',
         O_CNT COLOR COL_NEGATIVE.
WRITE: / 'Total number of line items with wrong LOGSYSP:',
         P_CNT COLOR COL_NEGATIVE.
