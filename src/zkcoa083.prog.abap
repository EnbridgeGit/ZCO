REPORT ZZKEN083.

*---------------------------------------------------------------------*
*       REPORT ZZKEN083                                               *
*       AUTHOR Ken Kaine                                              *
*---------------------------------------------------------------------*
*   This report is used to balance the shared service costs for Union *
*   Gas and Centra Gas.  It uses the CSKS table to get both Intermal  *
*   costs (table COSS) and External cost (table COSS) for all cost    *
*   centres in the ZKA01 file.                                        *
*---------------------------------------------------------------------*
*           Please Keep 5 most recent changes here,
*           Move others down to the bottom.
* 98/06/22 Janet Reid-renaming bdc session
*---------------------------------------------------------------------*
* YY/MM/DD   *  USERID  *  MOD#  * DESCRIPTION                        *
*            *          *        *                                    *
*            *          *        *                                    *
*            *          *        *                                    *
*            *          *        *                                    *
*---------------------------------------------------------------------*


TABLES:
  CSKS,
  COSS,
  COSP,
  ZKA01,
  ZKA02,
  ZKA03.

DATA:
  MSG001(100),
  INFILE(70),
  BDC_NUMBER(15),
  CREATE_BDC(1) VALUE 'Y',
  GROUP           LIKE ZKA01-SHRCG,
  SS_TOTAL_AMT    LIKE COSP-WTG001,
  TEMP_AMT        LIKE COSP-WTG001,
  VAR_AMT_COSP    LIKE COSP-WTG001,
  VAR_AMT_COSS    LIKE COSP-WTG001,
  UNION_TOTAL     LIKE COSP-WTG001,
  COMPN_TOTAL     LIKE COSP-WTG001,
  LAST_COMPN      LIKE ZKA01-COMPN,
  PERIOD_DATE     LIKE SY-DATUM,
  INPUTSEC        LIKE APQI-GROUPID VALUE 'ZCO_KKTEST',
  BEGIN OF REC,
    GROUP         LIKE ZKA02-SHRNM,
    COMPN1        LIKE ZKA01-COMPN,
    UNION_CC      LIKE ZKA01-KOSTL,
    UNION_CE      LIKE ZKA01-KOSTL,
    UNION_SGN(6),
    UNION_AMT     LIKE COSP-WTG001,
    COMPN2        LIKE ZKA01-COMPN,
    CENTRA_CC     LIKE ZKA01-KOSTL,
    CENTRA_CE     LIKE ZKA01-KOSTL,
    CENTRA_SGN(6),
    CENTRA_AMT    LIKE COSP-WTG001,
  END OF REC.
DATA: BEGIN OF BDC_TAB OCCURS 5.
        INCLUDE STRUCTURE BDCDATA.
DATA:END OF BDC_TAB.

* input of period number
PARAMETERS:
  PERIOD LIKE BKPF-MONAT.

* MAINLINE
SELECT * FROM ZKA02.
  GROUP = ZKA02-SHRCG.
  LAST_COMPN = ZKA01-COMPN.
  PERFORM ZZ_GET_GL_INFO.
  WRITE:
    /5 'union total ', UNION_TOTAL,
    /5 'centra total ', COMPN_TOTAL.
  PERFORM ZZ_GET_POSTING_DATE.
  PERFORM ZZ_CALC_PERCENT.
  IF CREATE_BDC = 'Y'.
     PERFORM CREATE_BDC_INPUT.
     WRITE:
          /5 'BDC session created for this Shared Service Group'.
  ELSE.
     WRITE:
          /5 'No BDC session created for this Shared Service Group',
          /5 'because shared cost are properly distrubuted'.
  ENDIF.
ENDSELECT.

*---------------------------------------------------------------------*
*       FORM ZZ_GET_GL_INFO                                           *
*---------------------------------------------------------------------*
*   This function reads the CSKS, COSP and COSS files to determine    *
*   the total costs charged to Union and Centra for a given cost      *
*   centre.                                                           *
*---------------------------------------------------------------------*
FORM ZZ_GET_GL_INFO.

  SELECT * FROM ZKA01
    WHERE SHRCG = GROUP.
    IF ZKA01-COMPN <> LAST_COMPN.
      IF LAST_COMPN = ' '.
         REC-COMPN1 = ZKA01-COMPN.
      ENDIF.
      UNION_TOTAL = COMPN_TOTAL.
      COMPN_TOTAL = 0.
      LAST_COMPN  = ZKA01-COMPN.
      REC-COMPN2  = LAST_COMPN.
    ENDIF.
    SELECT * FROM CSKS
      WHERE KOSTL = ZKA01-KOSTL.
      IF ZKA01-STRT = ' '.
         ZKA01-STRT = ZKA01-KSTAR.
      ENDIF.
      SELECT * FROM COSP
        WHERE OBJNR = CSKS-OBJNR
          AND KSTAR >= ZKA01-KSTAR
          AND KSTAR <= ZKA01-STRT.
        PERFORM ZZ_GET_VAR_COSP.
        WRITE: /5 CSKS-KOSTL,
                  COSP-KSTAR,
                  VAR_AMT_COSP.
        COMPUTE COMPN_TOTAL = COMPN_TOTAL + VAR_AMT_COSP.
      ENDSELECT.
    ENDSELECT.
    SELECT * FROM CSKS
      WHERE KOSTL = ZKA01-KOSTL.
      SELECT * FROM COSS
        WHERE OBJNR = CSKS-OBJNR
          AND KSTAR >= ZKA01-KSTAR
          AND KSTAR <= ZKA01-STRT.
        PERFORM ZZ_GET_VAR_COSS.
        WRITE: /5 CSKS-KOSTL,
                  COSS-KSTAR,
                  VAR_AMT_COSS.
        COMPUTE COMPN_TOTAL = COMPN_TOTAL + VAR_AMT_COSS.
      ENDSELECT.
    ENDSELECT.
  ENDSELECT.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZZ_CALC_PERCENT                                          *
*---------------------------------------------------------------------*
*   This function determines the difference between the current post  *
*   amount for a cost centre and the proper percentage for the shared *
*   service.  The percentage is stored in the ZKA03-pcent field.      *
*---------------------------------------------------------------------*
FORM ZZ_CALC_PERCENT.

  SELECT * FROM ZKA03
    WHERE SHRCG = ZKA01-SHRCG.
    COMPUTE:
     SS_TOTAL_AMT = UNION_TOTAL + COMPN_TOTAL.
     TEMP_AMT    = SS_TOTAL_AMT * ( ZKA03-PCENT / 100 ).
     REC-UNION_AMT = TEMP_AMT - UNION_TOTAL.
     REC-CENTRA_AMT = SS_TOTAL_AMT - TEMP_AMT - COMPN_TOTAL.
     WRITE: /5 'Percent amt allocated Centra', REC-UNION_AMT,
            /5 'percent amt allocated Union',  REC-CENTRA_AMT.
  ENDSELECT.

* check to see if cost are already properly allocated and
* not bdc session is required
  IF REC-UNION_AMT = 0 AND REC-CENTRA_AMT = 0.
     CREATE_BDC = 'N'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CREATE_BDC_INPUT                                         *
*---------------------------------------------------------------------*
*     create input session to post adjustments for shared services    *
*     for union and centra.  (both a post and offset will be created  *
*     in this function                                                *
*---------------------------------------------------------------------*
FORM CREATE_BDC_INPUT.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = INPUTSEC
            KEEP   = 'X'
            USER   = SY-UNAME.

* post union adjustment entry
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' PERIOD_DATE.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' PERIOD_DATE.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' 'SA'.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' REC-COMPN1.
  PERFORM BDC_FIELD  USING 'BKPF-MONAT' PERIOD.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' 'CAD'.
  IF REC-UNION_AMT > 0.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' ZKA03-PCELE.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  BDC_NUMBER = ABS( REC-UNION_AMT ).
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' BDC_NUMBER.
  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' 'I0'.
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'COBL-KOSTL' ZKA03-PCCUN.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
  PERFORM BDC_SCREEN USING 'SAPLkACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

* post offset entry for union
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
  IF REC-UNION_AMT > 0.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' '145160'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' BDC_NUMBER.
  PERFORM BDC_SCREEN USING 'SAPLkACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
* insert entry in bdc session
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = 'F-02'
       TABLES
            DYNPROTAB = BDC_TAB.
  REFRESH BDC_TAB.

* post centra adjustment entry
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' PERIOD_DATE.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' PERIOD_DATE.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' 'SA'.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' REC-COMPN2.
  PERFORM BDC_FIELD  USING 'BKPF-MONAT' PERIOD.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' 'CAD'.
  IF REC-CENTRA_AMT > 0.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ENDIF.
  BDC_NUMBER = ABS( REC-CENTRA_AMT ).
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' ZKA03-PCELE.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' BDC_NUMBER.
  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' 'I0'.
  PERFORM BDC_SCREEN USING 'SAPLkACB' '0002'.
  PERFORM BDC_FIELD  USING 'COBL-KOSTL' ZKA03-PCCCT.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

* post offset entry for centra
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
  IF REC-CENTRA_AMT > 0.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' '145110'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' BDC_NUMBER.
  PERFORM BDC_SCREEN USING 'SAPLkACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

*insert centra posting to bdc session
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = 'F-02'
       TABLES
            DYNPROTAB = BDC_TAB.
  REFRESH BDC_TAB.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZZ_GET_VAR_COSP                                          *
*---------------------------------------------------------------------*
*  This function, given the period number, will move the proper       *
*  shared services total for external costs to the output field       *
*---------------------------------------------------------------------*
FORM ZZ_GET_VAR_COSP.

  CASE PERIOD.
    WHEN 1.
      VAR_AMT_COSP = COSP-WTG001.
    WHEN 2.
      VAR_AMT_COSP = COSP-WTG002.
    WHEN 3.
      VAR_AMT_COSP = COSP-WTG003.
    WHEN 4.
      VAR_AMT_COSP = COSP-WTG004.
    WHEN 5.
      VAR_AMT_COSP = COSP-WTG005.
    WHEN 6.
      VAR_AMT_COSP = COSP-WTG006.
    WHEN 7.
      VAR_AMT_COSP = COSP-WTG007.
    WHEN 8.
      VAR_AMT_COSP = COSP-WTG008.
    WHEN 9.
      VAR_AMT_COSP = COSP-WTG009.
    WHEN 10.
      VAR_AMT_COSP = COSP-WTG010.
    WHEN 11.
      VAR_AMT_COSP = COSP-WTG011.
    WHEN 12.
      VAR_AMT_COSP = COSP-WTG012.
    WHEN 13.
      VAR_AMT_COSP = COSP-WTG013.
    WHEN 14.
      VAR_AMT_COSP = COSP-WTG014.
    WHEN 15.
      VAR_AMT_COSP = COSP-WTG015.
    WHEN 16.
      VAR_AMT_COSP = COSP-WTG016.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZZ_GET_VAR_COSS                                          *
*---------------------------------------------------------------------*
*  This function, given the period number, will move the proper       *
*  shared services total for internal costs to the output field       *
*---------------------------------------------------------------------*
FORM ZZ_GET_VAR_COSS.

  CASE PERIOD.
    WHEN 1.
      VAR_AMT_COSS = COSS-WTG001.
    WHEN 2.
      VAR_AMT_COSS = COSS-WTG002.
    WHEN 3.
      VAR_AMT_COSS = COSS-WTG003.
    WHEN 4.
      VAR_AMT_COSS = COSS-WTG004.
    WHEN 5.
      VAR_AMT_COSS = COSS-WTG005.
    WHEN 6.
      VAR_AMT_COSS = COSS-WTG006.
    WHEN 7.
      VAR_AMT_COSS = COSS-WTG007.
    WHEN 8.
      VAR_AMT_COSS = COSS-WTG008.
    WHEN 9.
      VAR_AMT_COSS = COSS-WTG009.
    WHEN 10.
      VAR_AMT_COSS = COSS-WTG010.
    WHEN 11.
      VAR_AMT_COSS = COSS-WTG011.
    WHEN 12.
      VAR_AMT_COSS = COSS-WTG012.
    WHEN 13.
      VAR_AMT_COSS = COSS-WTG013.
    WHEN 14.
      VAR_AMT_COSS = COSS-WTG014.
    WHEN 15.
      VAR_AMT_COSS = COSS-WTG015.
    WHEN 16.
      VAR_AMT_COSS = COSS-WTG016.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_SCREEN                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDC_TAB.
  BDC_TAB-PROGRAM = PROGRAM.
  BDC_TAB-DYNPRO = DYNPRO.
  BDC_TAB-DYNBEGIN = 'X'.
  APPEND BDC_TAB.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDC_TAB.
  BDC_TAB-FNAM = FNAM.
  BDC_TAB-FVAL = FVAL.
  APPEND BDC_TAB.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZZ_GET_POSTING_DATE                                      *
*---------------------------------------------------------------------*
*  This functions uses the current date to calculate the posting      *
*  date for the intercompany postings.  This date will be the last    *
*  day of the pervious month.                                         *
*---------------------------------------------------------------------*
FORM ZZ_GET_POSTING_DATE.

  PERIOD_DATE = SY-DATUM.
  IF PERIOD_DATE+4(2) = '01'.
    COMPUTE:
      PERIOD_DATE+4(2) = 12.
  ENDIF.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = PERIOD_DATE
       IMPORTING
            LAST_DAY_OF_MONTH = PERIOD_DATE
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.

ENDFORM.
