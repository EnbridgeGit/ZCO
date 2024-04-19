REPORT ZKCAR001 LINE-SIZE 120.
* Program: zkcar001

* Author: Nesh N. Laurencic -    Omnilogic Systems Group

* This program generates report. No data for input. Result is list of
* account numbers that exists in CGO or UGL, not in both, and if they
* are Cosat Elements or not.
TABLES: SKA1,          "G/L accounts master (chart of accounts)
        SKAT,          "G/L account master record (chart of accounts
        CSKU,                          "Cost element texts
        SKB1,          "G/L account master (company code)
        CSKA,          "Cost elements (data dependent on chart of accou
        CSKB.          "Cost elements (data dependent on controlling ar
DATA: BEGIN OF ITAB OCCURS 100,
      SAKNR LIKE SKB1-SAKNR,     "Cost elements (data dependent on chart
      KTEXT LIKE CSKU-KTEXT,           "Description
      TEXT(3) TYPE C,                  " Yes/NO      CGO
      TEXT1(3) TYPE C,                 " Yes/NO      UGL
      TEXT2(3) TYPE C,                 " Yes/NO      Cost Element
      FSTAG LIKE SKB1-FSTAG,
      KATYP LIKE CSKB-KATYP,
      END OF ITAB.

DATA: BEGIN OF ITAB1 OCCURS 100.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 100.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB2.
*break-point.

START-OF-SELECTION.
*break-point.
  SELECT * FROM SKA1 WHERE
                             KTOPL EQ 'COAT'.
    SELECT * FROM SKB1 WHERE
                             ( BUKRS EQ 'CGO' OR
                               BUKRS EQ 'UGL' ) AND
                               SAKNR EQ SKA1-SAKNR.
      CHECK SY-SUBRC = 0.

      CASE SKB1-BUKRS.
        WHEN 'CGO '.

          MOVE: SKA1-SAKNR TO ITAB1-SAKNR,
                SKB1-FSTAG TO ITAB1-FSTAG.  "Field status group

          ITAB1-TEXT = 'YES'.
* Find Description
          CLEAR SKAT.
          SELECT SINGLE * FROM SKAT WHERE
                                         SPRAS EQ SY-LANGU AND
                                         KTOPL EQ SKA1-KTOPL AND
                                         SAKNR EQ SKA1-SAKNR.
          MOVE SKAT-TXT20 TO ITAB1-KTEXT.

* Find if G/L is Cost Centre
          CLEAR CSKA.
          SELECT SINGLE * FROM CSKA WHERE
                                         KTOPL EQ SKA1-KTOPL AND
                                         KSTAR EQ SKA1-SAKNR.
          IF SY-SUBRC = 0.
            ITAB1-TEXT2 = 'YES'.
            SELECT SINGLE * FROM CSKB WHERE
                                             KOKRS    = '10' AND
                                             KSTAR    = CSKA-KSTAR AND
                                             DATBI    GT SY-DATUM.
            IF SY-SUBRC = 0.
              MOVE CSKB-KATYP TO ITAB1-KATYP. "Field status group
              MOVE ITAB1-KATYP TO ITAB1-TEXT2. "Just for printing
            ELSE.
              ITAB1-TEXT2 = 'No rec. CSKB'.
            ENDIF.
          ELSE.
            ITAB1-TEXT2 = 'N/A'.
          ENDIF.

          APPEND ITAB1.
          CLEAR ITAB1.


        WHEN 'UGL '.
          MOVE: SKA1-SAKNR TO ITAB2-SAKNR,
                SKB1-FSTAG TO ITAB2-FSTAG.  "Field status group


          ITAB2-TEXT1 = 'YES'.
* Find Description
          CLEAR SKAT.
          SELECT SINGLE * FROM SKAT WHERE
                                         SPRAS EQ SY-LANGU AND
                                         KTOPL EQ SKA1-KTOPL AND
                                         SAKNR EQ SKA1-SAKNR.
          MOVE SKAT-TXT20 TO ITAB2-KTEXT.
* Find if G/L is Cost Centre
          SELECT SINGLE * FROM CSKA WHERE
                                         KTOPL EQ SKA1-KTOPL AND
                                         KSTAR EQ SKA1-SAKNR.
          IF SY-SUBRC = 0.
            ITAB2-TEXT2 = 'YES'.
            SELECT SINGLE * FROM CSKB WHERE
                                             KOKRS    = '10' AND
                                            KSTAR    = CSKA-KSTAR AND
                                             DATBI    GT SY-DATUM.
            IF SY-SUBRC =  0.
              MOVE CSKB-KATYP TO ITAB2-KATYP. "Field status group
              MOVE ITAB2-KATYP TO ITAB2-TEXT2.
            ELSE.
              ITAB2-TEXT2 = 'No rec in CSKB'.
            ENDIF.
          ELSE.
            ITAB2-TEXT2 = 'N/A'.
          ENDIF.
          APPEND ITAB2.
          CLEAR ITAB2.
      ENDCASE.
    ENDSELECT.
    CHECK SY-SUBRC = 0.
  ENDSELECT.
  CHECK SY-SUBRC = 0.
*break-point.
  LOOP AT ITAB1.

    CLEAR ITAB2.
    READ TABLE ITAB2 WITH KEY SAKNR = ITAB1-SAKNR.
    IF SY-SUBRC = 0.
      DELETE ITAB2 INDEX SY-TABIX.
      CONTINUE.                        " Go to next itab1
    ENDIF.
    APPEND ITAB1 TO ITAB.
    CLEAR ITAB.
  ENDLOOP.
  LOOP AT ITAB2.
    APPEND ITAB2 TO ITAB.
  ENDLOOP.
  SORT ITAB BY SAKNR.
  FORMAT COLOR 2.
  SKIP 1.
*  break-point.
  LOOP AT ITAB.
    WRITE: / ITAB-SAKNR UNDER TEXT-001,
             ITAB-KTEXT UNDER TEXT-002,
             ITAB-FSTAG UNDER TEXT-100,
             ITAB-TEXT  UNDER TEXT-003,
             ITAB-TEXT1 UNDER TEXT-004,
             ITAB-TEXT2 UNDER TEXT-005,
*             itab-katyp under text-005,
             SY-TABIX   UNDER  TEXT-006.

  ENDLOOP.
  FORMAT RESET.

END-OF-SELECTION.

TOP-OF-PAGE.
  FORMAT COLOR 1.
  WRITE:/ TEXT-555, SY-MANDT, TEXT-556, SY-REPID.
  WRITE:  67 TEXT-557, SY-UZEIT.
  WRITE: / TEXT-001, 15 TEXT-002, 40 TEXT-100,
        52 TEXT-003, 57 TEXT-004, 67 TEXT-005,
        81 TEXT-006.
  WRITE: / TEXT-101 UNDER TEXT-005.
  FORMAT RESET.
  SKIP.
