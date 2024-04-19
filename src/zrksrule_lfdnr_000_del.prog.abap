REPORT ZRKSRULE_LFDNR_000_DEL .

PARAMETERS: TESTRUN AS CHECKBOX DEFAULT 'X'.

DATA: XCOBRB       LIKE STANDARD TABLE OF COBRB WITH HEADER LINE,
      OBJECT(25)   TYPE C,
      AUFNR        LIKE IONRA-AUFNR,
      PSPNR        LIKE IONRA-PSPNR,
      LINES        TYPE I.

SELECT * FROM COBRB INTO TABLE XCOBRB WHERE LFDNR = '000'.
SORT XCOBRB BY OBJNR.

WRITE: ' ', AT 20 'Object'.
ULINE.

LOOP AT XCOBRB.
  CLEAR OBJECT.
  CASE XCOBRB-OBJNR+0(2).
    WHEN 'OR'.
      CALL FUNCTION 'OBJECT_KEY_GET_OR'
           EXPORTING
                OBJNR       = XCOBRB-OBJNR
           IMPORTING
                AUFNR       = AUFNR
           EXCEPTIONS
                NOT_FOUND   = 1
                WRONG_OBART = 2
                OTHERS      = 3.
      IF SY-SUBRC <> 0.
        OBJECT = XCOBRB-OBJNR.
      ELSE.
        OBJECT = AUFNR.
      ENDIF.
      WRITE:/ 'Internal order', AT 20 OBJECT.

    WHEN 'PR'.
      CALL FUNCTION 'OBJECT_KEY_GET_PR'
           EXPORTING
                OBJNR       = XCOBRB-OBJNR
           IMPORTING
                PSPNR       = PSPNR
           EXCEPTIONS
                NOT_FOUND   = 1
                WRONG_OBART = 2
                OTHERS      = 3.
      IF SY-SUBRC <> 0.
        OBJECT = XCOBRB-OBJNR.
      ELSE.
        WRITE PSPNR TO OBJECT.
      ENDIF.
      WRITE:/ 'WBS-element', AT 20 OBJECT.

    WHEN 'NP'.
      CALL FUNCTION 'OBJECT_KEY_GET_NP'
           EXPORTING
                OBJNR       = XCOBRB-OBJNR
           IMPORTING
                AUFNR       = AUFNR
           EXCEPTIONS
                NOT_FOUND   = 1
                WRONG_OBART = 2
                OTHERS      = 3.
      IF SY-SUBRC <> 0.
        OBJECT = XCOBRB-OBJNR.
      ELSE.
        WRITE AUFNR TO OBJECT.
      ENDIF.
      WRITE:/ 'Network', AT 20 OBJECT.

    WHEN OTHERS.
      OBJECT = XCOBRB-OBJNR.
      WRITE:/ ' ', AT 20 OBJECT.
  ENDCASE.

ENDLOOP.
ULINE.
DESCRIBE TABLE XCOBRB LINES LINES.
IF TESTRUN IS INITIAL.
  WRITE:/ LINES, ' entries with LFDNR = ''000'' have been deleted'.
ELSE.
  WRITE:/ LINES, ' entries with LFDNR = ''000'' detected'.
ENDIF.

IF TESTRUN IS INITIAL.
  DELETE COBRB FROM TABLE XCOBRB.
  COMMIT WORK.
ENDIF.
