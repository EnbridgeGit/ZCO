REPORT ZYVAM001 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-COUNT 65
                                                       LINE-SIZE  80.
************************************************************************
*  Programmer: Lee Haire
*              OmniLogic Systems Group
*  Brief Description:
*    - This program updates the TVARV variables for the ABAP RKEVEXT0.
*  This variable represents a physical file which is to be used by
*  RKEVEXT0.  The physical file will have the current date and a
*  sequential number attached to the end of it.  This sequential number
*  is incremented if the value has already been updated previously on
*  the same day.  For each new day, the date is changed and the seq.
*  number is reset to 1.
************************************************************************
*----------------------- CHANGE LOG ------------------------------------
*
*
************************************************************************

TABLES: TVARV.                         "Table of variables
* working variables
DATA: ZFILE(60),
      TVAL LIKE TVARV-LOW,
      LEN TYPE I,
      SEQN(1) TYPE N.

DATA: BEGIN OF ENQ_KEY,                "Key for locking record
        NAME(30),
        TYPE LIKE TVARV-TYPE,
      END OF ENQ_KEY.

*=======================================================================
* SELECTION SCREEN
*=======================================================================
*SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_TVNAME LIKE TVARV-NAME DEFAULT 'ZCO_RKEVEXT0_FILENAME_?'
                                     OBLIGATORY,
            P_FILE(60) DEFAULT '/usr/P01/?'
                       LOWER CASE OBLIGATORY.
PARAMETERS :   P_TESTRN AS CHECKBOX.   "For test run only
*SELECTION-SCREEN END OF BLOCK BOX1.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.

* ensure that TVARV variable has already been created
  SELECT SINGLE FOR UPDATE * FROM TVARV WHERE NAME = P_TVNAME AND
                                              TYPE = 'P' AND
                                              NUMB = '0000'.
  IF SY-SUBRC <> 0.
    MESSAGE E024(ZS) WITH P_TVNAME.
  ENDIF.
* start building the new value of the physical file for TVARV....
  ZFILE = P_FILE.
  CONCATENATE P_FILE '.' INTO ZFILE.
  CONCATENATE ZFILE SY-DATUM INTO ZFILE.
* get current value from TVARV
  LEN = STRLEN( TVARV-LOW ) - 1.
  TVAL = TVARV-LOW(LEN).
* if TVARV contains value from today, increment the sequence by one
  IF ( TVAL = ZFILE ).
    SEQN = TVARV-LOW+LEN(1).
    SEQN = SEQN + 1.
* otherwise reset to '1'.
  ELSE.
    SEQN = '1'.
  ENDIF.
* new value of TVARV variable:
  CONCATENATE ZFILE SEQN INTO ZFILE.
  TVARV-LOW = ZFILE.
*  PERFORM ENQ_TVARV USING TVARV-NAME 'P'.
*  IF SY-SUBRC <> 0.
*    EXIT.
*  ENDIF.
*  WRITE: / TVARV-NAME, TVARV-LOW.
  MESSAGE I025(ZS) WITH TVARV-NAME TVARV-LOW.
  IF P_TESTRN <> 'X'.
    MODIFY TVARV.
  ELSE.
    MESSAGE I026(ZS).
  ENDIF.
*  PERFORM DEQ_TVARV USING TVARV-NAME TVARV-TYPE.


*----------------------------------------------------------------------*
*       FORM ENQ_TVARV                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM ENQ_TVARV USING
            P_NAME       LIKE TVARV-NAME
            P_TYPE       LIKE TVARV-TYPE.
  ENQ_KEY-NAME = P_NAME.
  ENQ_KEY-TYPE = P_TYPE.
  CALL FUNCTION 'ENQUEUE_ESVARV'
       EXPORTING
            NAME           = P_NAME
            TYPE           = P_TYPE
       EXCEPTIONS
            FOREIGN_LOCK   = 4
            SYSTEM_FAILURE = 8.
  CASE SY-SUBRC.
    WHEN 4.
      WRITE: P_NAME,  TEXT-310.
    WHEN 8.
      WRITE: ENQ_KEY, TEXT-320.
  ENDCASE.
ENDFORM.


*----------------------------------------------------------------------*
*       FORM DEQ_TVARV                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM DEQ_TVARV USING
            P_NAME       LIKE TVARV-NAME
            P_TYPE       LIKE TVARV-TYPE.
  ENQ_KEY-NAME = P_NAME.
  ENQ_KEY-TYPE = P_TYPE.
  CALL FUNCTION 'DEQUEUE_ESVARV'
       EXPORTING
            NAME = P_NAME
            TYPE = P_TYPE.
ENDFORM.


*---------------------------------------------------------------------*
*       top-of-page                                                   *
*---------------------------------------------------------------------*
*TOP-OF-PAGE.
*  FORMAT INTENSIFIED OFF.
*  WRITE: / SY-REPID, SY-SYSID, 32 TEXT-001.
*  WRITE:  63 TEXT-004, SY-DATUM.
*  WRITE: /63 TEXT-006, SY-UZEIT.
*  WRITE: /63 TEXT-005, SY-PAGNO.
*
*  IF P_TESTRN = 'X'.
*    WRITE: /32 TEXT-205.
*  ENDIF.
*  ULINE.
*  WRITE: / TEXT-201, 32 TEXT-202.
*  ULINE.
*  FORMAT INTENSIFIED ON.
