REPORT RKEVEXT0 MESSAGE-ID KE.

TYPE-POOLS: RKEA1.

TABLES: T258Z,
        CEST1,
        TKEVS,
        TKVS,
        TKEB,
        T258E,
        TKEPA.

DATA: REPORTNAME(8) VALUE 'RKEEXXXX'.
DATA: H_ZAE LIKE T258E-ZAEHLER.
DATA: H_PACK(5) TYPE P.
DATA: LIN TYPE P.

DATA: FLAG_ON(1) TYPE C VALUE 'X',
      FLAG_OFF(1) TYPE C VALUE ' '.

DATA: ERR_FILE_FLAG(1) TYPE C VALUE ' ', "= FLAG_ON, wenn Fehlerdaten
                                       "in Fehlerfile
      BDCGROUP_FLAG(1) TYPE C VALUE ' '. "= FLAG_ON, wenn Fehlerdaten
                                       "in Batch-Input-Mappe

DATA: FORMAPPLCLASS LIKE CEFORMS-APPLCLASS VALUE 'KE',      "rke
      FORMSUBCLASS LIKE CEFORMS-SUBCLASS VALUE '02'.    "planung

DATA: BEGIN OF INT_TKEL OCCURS 1.
        INCLUDE STRUCTURE TKEL.
DATA: END OF INT_TKEL.

DATA: GT_V_TKEBL TYPE RKEA1_T_V_TKEBL,
      GS_V_TKEBL TYPE LINE OF RKEA1_T_V_TKEBL,
      GT_V_TKELT TYPE RKEA1_T_V_TKELT,
      GS_V_TKELT TYPE LINE OF RKEA1_T_V_TKELT,
      GS_V_TKEB TYPE RKEA1_V_TKEB.

PARAMETERS: GROUP LIKE T258W-COPAGROUP OBLIGATORY,
            FILE LIKE CEPL4-FILENM
*           FILE(60) TYPE C DEFAULT '/usr/sap/K11/D47/work/hohe23'
              OBLIGATORY,
            ERR_FILE LIKE CEPL4-FILENM,
*           ERR_FILE(60) TYPE C DEFAULT '/usr/sap/K11/D47/work/hohe53'
            BDCGROUP LIKE APQI-GROUPID MODIF ID IST.

SELECTION-SCREEN ULINE.

PARAMETERS: TEXT AS CHECKBOX.
SELECTION-SCREEN ULINE.

PARAMETERS: TEST DEFAULT 'X' LIKE CEPL4-TEST,
            SHOWTRAC AS CHECKBOX,
            TRACFROM TYPE I DEFAULT 1,
            TRACETO TYPE I DEFAULT 2.
SELECTION-SCREEN ULINE.
PARAMETERS: PLIKZ LIKE CEST1-PLIKZ MODIF ID TCO,
            VERSI LIKE TKEVS-VERSI MODIF ID PLN,
            ALTPERIO LIKE CEPL4-WOCHEN MODIF ID PLN,
            REVAL LIKE TKEPA-REVAL MODIF ID PLN,
            BEWERT LIKE CEPL4-BEWERT MODIF ID PLN.

DATA: UPD_LOC VALUE 'X'.

DATA: ERKRS LIKE TKEB-ERKRS,
      ERKRS2 LIKE TKEB-ERKRS,
      OKCODE LIKE SY-PFKEY,
      PA_TYPE LIKE CEDDB-PA_TYPE,
      I_TKEBL LIKE TKEBL.

INITIALIZATION.
  IF SY-TCODE EQ 'KE4XO' OR SY-TCODE EQ 'KE1XO'.

    CALL FUNCTION 'COPA_GET_ERK'
         EXPORTING
              MODE    = 'F'
         IMPORTING
              ERKRS   = ERKRS2
              OK_CODE = OKCODE
              PA_TYPE = PA_TYPE.

    IF OKCODE NE 'ENTE'.
      CLEAR ERKRS2.
      SET SCREEN 0. LEAVE SCREEN.
    ENDIF.
    IF PA_TYPE EQ '2'.
      MESSAGE E071(KG).
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF SY-TCODE EQ 'KE4XO'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'PLN' OR SCREEN-GROUP1 EQ 'TCO'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    PLIKZ = '0'.
  ELSEIF SY-TCODE EQ 'KE1XO'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'IST' OR SCREEN-GROUP1 EQ 'TCO'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    PLIKZ = '1'.
  ENDIF.

AT SELECTION-SCREEN ON GROUP.
  SELECT SINGLE * FROM T258Z WHERE COPAGROUP = GROUP.
  IF SY-SUBRC NE 0.
    MESSAGE E331 WITH GROUP.
  ENDIF.

AT SELECTION-SCREEN ON ERR_FILE.
  IF NOT ERR_FILE IS INITIAL.
    ERR_FILE_FLAG = FLAG_ON.
  ELSE.
    ERR_FILE_FLAG = FLAG_OFF.
  ENDIF.

AT SELECTION-SCREEN ON BDCGROUP.
  IF NOT BDCGROUP IS INITIAL.
    BDCGROUP_FLAG = FLAG_ON.
  ELSE.
    BDCGROUP_FLAG = FLAG_OFF.
  ENDIF.

AT SELECTION-SCREEN ON TEST.
  IF TEST CN 'X. '.
    MESSAGE E327.
  ENDIF.

AT SELECTION-SCREEN ON PLIKZ.
  IF NOT ( PLIKZ EQ '0' OR PLIKZ EQ '1' ).
    MESSAGE E394.
  ENDIF.

AT SELECTION-SCREEN ON ALTPERIO.
  IF ALTPERIO CN 'X. '.
    MESSAGE E327.
  ENDIF.
  IF ALTPERIO EQ '.'.
    CLEAR ALTPERIO.
  ENDIF.

AT SELECTION-SCREEN ON BEWERT.
  IF BEWERT CN 'X. '.
    MESSAGE E327.
  ENDIF.

AT SELECTION-SCREEN.
  IF PLIKZ EQ '1' AND BDCGROUP_FLAG = FLAG_ON.
    MESSAGE E266.
  ENDIF.
  IF PLIKZ EQ '1' AND ERR_FILE_FLAG = FLAG_OFF.
    MESSAGE E267.
  ENDIF.
  IF BDCGROUP_FLAG = ERR_FILE_FLAG.
    IF BDCGROUP_FLAG =  FLAG_ON.
      MESSAGE E265.
    ELSE.
      MESSAGE E264.
    ENDIF.
  ENDIF.
  IF ( NOT REVAL IS INITIAL
    OR BEWERT EQ 'X' )
    AND PLIKZ NE '1'.
    MESSAGE E381.                      " nur bei Plandaten
  ENDIF.
  IF NOT ALTPERIO IS INITIAL
    AND PLIKZ NE '1'.
    MESSAGE E393.                      " nur bei Plandaten
  ENDIF.

* get ERKRS
* CALL FUNCTION 'COPA_GET_ERK'
*       IMPORTING ERKRS = ERKRS
*                 OK_CODE = OKCODE.
* IF OKCODE = 'NDE' OR OKCODE = 'ABB'
*           OR OKCODE = 'RW'.
*   SET SCREEN 0.
*   LEAVE SCREEN.
* ENDIF.

  ERKRS = T258Z-TAB_INT+3(4).
  IF SY-TCODE EQ 'KE4XO' OR SY-TCODE EQ 'KE1XO'.
    IF ERKRS NE ERKRS2.
      MESSAGE E443 WITH GROUP ERKRS ERKRS2.
    ENDIF.
  ENDIF.

* Wochenplanung erlaubt?
  IF PLIKZ EQ '1' AND ALTPERIO EQ 'X'.
    CALL FUNCTION 'RKE_READ_ERKRS_MESSAGE'
         EXPORTING
              I_ERKRS    = ERKRS
         IMPORTING
              E_V_TKEB   = GS_V_TKEB
              ET_V_TKEBL = GT_V_TKEBL
              ET_V_TKELT = GT_V_TKELT.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
                 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF GS_V_TKEB-USEPER2P IS INITIAL.
      MESSAGE E419 WITH ERKRS.         "keine Wochenplanung erlaubt
    ENDIF.
  ENDIF.

* Version pruefen
  IF PLIKZ NE '1'.
    IF NOT ( VERSI IS INITIAL ).
      MESSAGE E361.                    "Versi nur bei Plandaten
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM TKVS
     WHERE VERSI = VERSI.
    IF SY-SUBRC GT 0.
      MESSAGE E357 WITH VERSI.
    ENDIF.
    SELECT SINGLE * FROM TKEVS
      WHERE VERSI = VERSI
       AND ERKRS = ERKRS.
    IF SY-SUBRC GT 0.
      MESSAGE E358 WITH VERSI ERKRS.
    ENDIF.
    IF TKEVS-BEPLBA NE 'X'.
      MESSAGE E030(KG) WITH VERSI.
    ENDIF.
    IF NOT ALTPERIO IS INITIAL.
      IF TKEVS-ZERATR <> '1' AND TKEVS-ZERATR <> '2'.
*        Transformationsregel nicht unterstützt oder nicht gepflegt
*        ersatzweise wird nach Kalendertagen ('1') verteilt.
        MESSAGE I032(KG) WITH TKEVS-ZERATR.
        TKEVS-ZERATR = '1'.
      ENDIF.
    ENDIF.
  ENDIF.

* Umwertungstabelle pruefen.
  IF PLIKZ EQ '1'.
    TKEPA-APPLCLASS = FORMAPPLCLASS.
    TKEPA-SUBCLASS = FORMSUBCLASS.
    TKEPA-TABNAME = T258Z-TAB_INT.
    IF NOT REVAL IS INITIAL.
      SELECT SINGLE * FROM TKEPA
                    WHERE     APPLCLASS = TKEPA-APPLCLASS
                          AND SUBCLASS  = TKEPA-SUBCLASS
                          AND TABNAME   = TKEPA-TABNAME
                          AND REVAL     = REVAL.
      IF SY-SUBRC <> 0.
*    Die Umwertungsreihe & ist nicht vorgesehen
        MESSAGE E521(KG) WITH REVAL.
      ENDIF.
    ENDIF.
  ENDIF.

* Fehlerfile darf nicht gleich dem Quellfile sein.
  IF FILE EQ ERR_FILE.
    MESSAGE E284.
  ENDIF.

* Überprüfen, ob File schon einmal eingespielt wurde
  CLEAR T258E.
  SELECT * FROM T258E WHERE COPAGROUP = GROUP AND
                            FILENAME  = FILE.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC = 0.
    IF T258E-FILE_OK = 'X'.
      MESSAGE E388 WITH FILE
                        T258E-DATUM
                        T258E-USNAM.
    ELSE.
      H_ZAE = T258E-ZAEHLER + 1.
      H_PACK = H_ZAE.
      MESSAGE I389 WITH FILE H_PACK.
    ENDIF.
  ENDIF.

*.................. Open auf Inputdatei .............................. *
  IF NOT TEXT IS INITIAL.
    OPEN DATASET FILE FOR INPUT IN TEXT MODE.
  ELSE.
    OPEN DATASET FILE FOR INPUT.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE E316 WITH FILE.
  ENDIF.

*.................. Open auf Fehlerdatei.............................. *
  IF FILE NE ERR_FILE AND ERR_FILE_FLAG = FLAG_ON.
    IF NOT TEXT IS INITIAL.
      OPEN DATASET ERR_FILE FOR APPENDING IN TEXT MODE.
    ELSE.
      OPEN DATASET ERR_FILE FOR APPENDING.
    ENDIF.
    IF SY-SUBRC NE 0.
      MESSAGE E316 WITH ERR_FILE.
    ENDIF.
  ENDIF.


START-OF-SELECTION.
  IF SY-TCODE EQ 'KE4XO' OR SY-TCODE EQ 'KE1XO'.
* Bei Abbrechen auf dem Ergebnisbereichspopup sonst Dump.
    CHECK NOT ERKRS2 IS INITIAL.
  ENDIF.
  REPORTNAME+4(4) = GROUP.
  IF TRACETO = 0.
    TRACETO = 99999999.
  ENDIF.

  CALL FUNCTION 'RKE_GENERATE_INTERFACE_EXT'
       EXPORTING
            I_COPAGROUP = GROUP.

  PERFORM FILE_VERARBEITEN IN PROGRAM (REPORTNAME) USING FILE
                                                         ERR_FILE
                                                         BDCGROUP
                                                         TEST
                                                         PLIKZ
                                                         VERSI
                                                         ALTPERIO
                                                         REVAL
                                                         BEWERT
                                                         H_ZAE
                                                         GROUP
                                                        T258E-GROUP_NUM
                                                        UPD_LOC
                                                        SHOWTRAC
                                                        TRACFROM
                                                        TRACETO.

*---------------------------------------------------------------------*
*       FORM PROTOKOLL_AUSGABE                                        *
*---------------------------------------------------------------------*
*       Ausgabe des Protokoll bei externer Datenübernahme             *
*---------------------------------------------------------------------*
* -->   TEST            Testmodus                                     *
* -->   CNT_READ        Anzahl gelesener Records                      *
* -->   CNT_INSERT      Anzahl ins COPA geschriebener Records         *
* -->   CNT-ERROR       Anzahl fehlerhafter Sätze                     *
*---------------------------------------------------------------------*
FORM PROTOKOLL_AUSGABE USING I_TEST
                             I_CNT_READ
                             I_CNT_INSERT
                             I_CNT_ERROR.

  FORMAT COLOR COL_KEY INTENSIFIED.
  WRITE: / SY-ULINE(60).

  IF NOT I_TEST IS INITIAL.
    WRITE: /01(1) '|',
            02(58) 'Testlauf:'(080),
            60(1) '|'.
  ELSE.
    WRITE: /01(1) '|',
            02(58) 'Verbuchungslauf:'(090),
            60(1) '|'.
  ENDIF.

  FORMAT COLOR COL_HEADING INTENSIFIED.
  WRITE: / SY-ULINE(60).

  WRITE: /01(1) '|',
          02(47) 'Anzahl gelesener Sätze aus externem File:'(001),
          49 '|',
          50 I_CNT_READ COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:  60(1) '|'.

  WRITE: /01(1) '|',
          02(47) 'Anzahl geschriebener Einzelposten:'(002),
          49 '|',
          50 I_CNT_INSERT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:  60(1) '|'.

  WRITE: /01(1) '|',
          02(47) 'Anzahl fehlerhafter Sätze:'(003),
          49 '|',
          50 I_CNT_ERROR COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:  60(1) '|'.

  WRITE: / SY-ULINE(60).

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FEHLER_AUSGABE                                           *
*---------------------------------------------------------------------*
*       Ausgabe der Fehler bei externer Datenübernahme                *
*---------------------------------------------------------------------*
FORM FEHLER_AUSGABE.

  CALL FUNCTION 'MESSAGES_SHOW'
       EXPORTING
            OBJECT             = TEXT-010
            SHOW_LINNO         = ' '
       EXCEPTIONS
            INCONSISTENT_RANGE = 1
            NO_MESSAGES        = 2.

ENDFORM.
