REPORT ZKPAR001
       LINE-SIZE 80 LINE-COUNT 65 NO STANDARD PAGE HEADING.

************************************************************************
*  Author:     Dorothy Bilakowska
*  Brief Description:
*  - The purpose of this program is to count the number of records in
*    selected tables.
************************************************************************

TABLES:    CE11100, CE21100, CE31100, CE41100, CE71100.

DATA:     BEGIN OF ITAB OCCURS 20,
                PERIOD     LIKE     CE11100-PERIO,
                COUNT      TYPE     I.
DATA:     END OF ITAB.

DATA:     COUNT1     TYPE     I,
          COUNT2     LIKE     COUNT1,
          COUNT3     LIKE     COUNT1,
          COUNT4     LIKE     COUNT1,
          COUNT      LIKE     COUNT1.

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-008, SY-DATUM, 69 TEXT-010, SY-PAGNO.
  WRITE: / TEXT-009, SY-UZEIT UNDER SY-DATUM, SY-REPID UNDER TEXT-010.
  WRITE: /34 TEXT-011.
  ULINE.
  FORMAT INTENSIFIED ON.

START-OF-SELECTION.
  CLEAR COUNT.

  SELECT * FROM CE11100
  ORDER BY PERIO.
    MOVE CE11100-PERIO TO ITAB-PERIOD.
    APPEND ITAB.
    CLEAR ITAB.
    COUNT1 = COUNT1 + 1.
  ENDSELECT.

  SELECT * FROM CE21100.
    COUNT2 = COUNT2 + 1.
  ENDSELECT.

  SELECT * FROM CE31100.
    COUNT3 = COUNT3 + 1.
  ENDSELECT.

  SELECT * FROM CE41100.
    COUNT4 = COUNT4 + 1.
  ENDSELECT.

END-OF-SELECTION.

  SORT ITAB BY PERIOD.

  FORMAT INTENSIFIED OFF.
  WRITE: /5 TEXT-005, 20 TEXT-006, 50 TEXT-012, 70 TEXT-007.
  FORMAT INTENSIFIED ON.
  ULINE.
  SKIP.

  WRITE: /5 'CE11100', 20 TEXT-001.
  SKIP TO LINE 8.
  LOOP AT ITAB.
    COUNT = COUNT + 1.
    AT END OF PERIOD.
      WRITE: /50 ITAB-PERIOD, 65 COUNT.
      CLEAR COUNT.
    ENDAT.
  ENDLOOP.
  WRITE: /50(30) SY-ULINE.
  WRITE: /50 TEXT-013, 65 COUNT1.

  FORMAT INTENSIFIED ON.
  WRITE: /5 'CE21100', 20 TEXT-002, 65 COUNT2.
  WRITE: /5 'CE31100', 20 TEXT-003, 65 COUNT3.
  WRITE: /5 'CE41100', 20 TEXT-004, 65 COUNT4.