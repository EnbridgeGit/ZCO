REPORT ZKAPLINT.
*=======================================================================
* Repair    OSS note: 34927 Repair Request: D30K900603     DB  07/16/96
*=======================================================================
INCLUDE RBONRART.
TABLES: AUFKV, AUFK.
SELECT-OPTIONS: S_AUFNR FOR AUFKV-AUFNR.

*
DATA: BEGIN OF IT_AUFKV OCCURS 0.
      INCLUDE STRUCTURE AUFKV.
DATA: END OF IT_AUFKV.
*
DATA: GD_ANZPL     LIKE SY-TABIX,
      GD_ANZEQ     LIKE SY-TABIX,
      GD_ANZUP     LIKE SY-TABIX.

START-OF-SELECTION.
*
   CHECK NOT S_AUFNR IS INITIAL.
   SELECT * FROM AUFKV INTO TABLE IT_AUFKV WHERE AUFNR IN S_AUFNR.

*
   CLEAR IT_AUFKV.
   LOOP AT IT_AUFKV.
      IF IT_AUFKV-PLINT IS INITIAL.
         CALL FUNCTION 'ENQUEUE_ESORDER'
              EXPORTING
                 AUFNR = IT_AUFKV-AUFNR
              EXCEPTIONS
                 FOREIGN_LOCK = 01
                 SYSTEM_FAILURE = 02.
          IF NOT SY-SUBRC IS INITIAL.
*
             WRITE:/(40) TEXT-001, OBJEKTART_OR, IT_AUFKV-AUFNR.
             ADD 1 TO GD_ANZEQ.
          ELSE.
             UPDATE AUFKV SET PLINT = 'X' WHERE AUFNR = IT_AUFKV-AUFNR.
             ADD 1 TO GD_ANZUP.
             CALL FUNCTION 'DEQUEUE_ALL'.
          ENDIF.
       ELSE.
*
          ADD 1 TO GD_ANZPL.
       ENDIF.
    ENDLOOP.
*
    IF NOT SY-SUBRC IS INITIAL.
       WRITE: /(40) TEXT-002.
    ELSE.
       WRITE: /(40) TEXT-003, GD_ANZUP.
       WRITE: /(40) TEXT-004, GD_ANZEQ.
       WRITE: /(40) TEXT-005, GD_ANZPL.
    ENDIF.
