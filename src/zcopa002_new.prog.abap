REPORT zcopa002_new NO STANDARD PAGE HEADING.
*
************************************************************************
*
*  AUTHOR:      MOHAMMAD KHAN
*  DATE:        FEBRUARY, 2002.
*  Description:
*     - The purpose of this program is to produce consolidated data
*       from the table CE11100. This program is using ALV technique.
*
*TR804 - 2010/11/29 Brian Boundy - updates for COG.
*
************************************************************************
* DB-Table
TABLES ce11100.

* Type-pool of ALV
TYPE-POOLS: slis.

* Report Selections
PARAMETERS:     p_bukrs LIKE ce11100-bukrs OBLIGATORY    DEFAULT 'UGL',
                p_kokrs LIKE ce11100-kokrs OBLIGATORY    DEFAULT '10',
                p_palgr LIKE ce11100-paledger OBLIGATORY,
                p_plikz LIKE ce11100-plikz    OBLIGATORY DEFAULT '0'.


SELECT-OPTIONS: s_perio FOR  ce11100-perio    OBLIGATORY,
                s_vkorg FOR  ce11100-vkorg    OBLIGATORY,
                s_vrgar FOR  ce11100-vrgar    OBLIGATORY,
                s_prdha FOR  ce11100-prdha OBLIGATORY DEFAULT
                               '00150000100010' TO '00179999999999',
                "s_werks for ce11100-werks OBLIGATORY DEFAULT 'GSTH'. "TR804
                s_werks for ce11100-werks. "TR804

SELECTION-SCREEN SKIP 1.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.

* Data to be displayed
DATA:
  BEGIN OF itable OCCURS 0,
        perio   LIKE ce11100-perio,
        kndnr   LIKE ce11100-kndnr,
        wwsbu   LIKE ce11100-wwsbu,
        wwprg   like ce11100-wwprg,
        wwsub   like ce11100-wwsub,
        wwrat   LIKE ce11100-wwrat,
        wwser   LIKE ce11100-wwser,
        werks   LIKE ce11100-werks,
        wwsld   like ce11100-wwsld,
        wwsct   LIKE ce11100-wwsct,
        wwseg   LIKE ce11100-wwseg,
        wwdvs   LIKE ce11100-wwdvs,
        wwrgn   LIKE ce11100-wwrgn,
        artnr   LIKE ce11100-artnr,
        prdha   LIKE ce11100-prdha,
        vvbrv   LIKE ce11100-vvbrv,
        vvbvl   LIKE ce11100-vvbvl,
        vvcut   LIKE ce11100-vvcut,

  END   OF itable.

DATA:
  BEGIN OF ftable OCCURS 0,
        perio   LIKE ce11100-perio,
        blank1(5),
        kndnr   LIKE ce11100-kndnr,
        wwsbu   LIKE ce11100-wwsbu,
        wwprg   like ce11100-wwprg,
        wwsub   like ce11100-wwsub,
        wwrat   LIKE ce11100-wwrat,
        wwser   LIKE ce11100-wwser,
        werks   LIKE ce11100-werks,
        wwsld   like ce11100-wwsld,
        wwsct   LIKE ce11100-wwsct,
        wwseg   LIKE ce11100-wwseg,
        wwdvs   LIKE ce11100-wwdvs,
        wwrgn   LIKE ce11100-wwrgn,
        artnr   LIKE ce11100-artnr,
        prdha   LIKE ce11100-prdha,
        vvbrv   LIKE ce11100-vvbrv,
        vvbvl   LIKE ce11100-vvbvl,
        vvcut   LIKE ce11100-vvcut,

  END   OF ftable.

DATA: gs_extract1 LIKE sy-repid.
DATA: gs_extract2 LIKE disextract.
*---------------------------------------------------------------------*
*
INITIALIZATION.

  PERFORM fieldcat_init  USING gt_fieldcat[].

START-OF-SELECTION.

* DB Selection
  SELECT  perio kndnr wwsbu wwsct wwseg wwrat wwser werks
          wwdvs wwrgn artnr prdha vvbrv vvbvl vvcut
          wwprg wwsub wwsld  "TR804
    INTO  CORRESPONDING FIELDS OF TABLE itable
    FROM  ce11100
   WHERE  paledger = p_palgr
     AND  vrgar   IN s_vrgar
     AND  versi   = ''
     AND  perio   IN s_perio

     AND  kokrs   = p_kokrs
     AND  bukrs   = p_bukrs
     AND  plikz   = p_plikz
     AND  vkorg   IN s_vkorg
     AND  prdha   IN s_prdha
     and  werks   in s_werks
     .

  IF itable[] IS INITIAL.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
  ENDIF.

  SORT itable.

  LOOP AT itable.
    MOVE-CORRESPONDING itable TO ftable.
    COLLECT ftable.
    CLEAR   ftable.
  ENDLOOP.

* Call ABAP/4 List Viewer
  MOVE sy-repid TO gs_extract1.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = gs_extract1
*            I_STRUCTURE_NAME = 'CE11100'
            it_fieldcat              = gt_fieldcat[]
            i_save           = 'A'
       TABLES
            t_outtab         = ftable.

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname     = 'PERIO'.
  ls_fieldcat-col_pos       = 1.
  ls_fieldcat-reptext_ddic  = 'PERIOD'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'BLANK1'.
  ls_fieldcat-col_pos       = 2.
  ls_fieldcat-reptext_ddic  = 'BLANK1'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'KNDNR'.
  ls_fieldcat-col_pos       = 3.
  ls_fieldcat-reptext_ddic  = 'CUSTOMER'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWSBU'.
  ls_fieldcat-col_pos       = 4.
  ls_fieldcat-reptext_ddic  = 'S.B.UNIT'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWPRG'. "TR804
  ls_fieldcat-col_pos       = 5.
  ls_fieldcat-reptext_ddic  = 'Program'. "TR804
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWSUB'. "TR804
  ls_fieldcat-col_pos       = 6.
  ls_fieldcat-reptext_ddic  = 'Sub-Program'. "TR804
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWRAT'.
  ls_fieldcat-col_pos       = 7.
  ls_fieldcat-reptext_ddic  = 'RATE CLASS'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWSER'.
  ls_fieldcat-col_pos       = 8.
  ls_fieldcat-reptext_ddic  = 'S.CLASS'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WERKS'.
  ls_fieldcat-col_pos       = 9.
  ls_fieldcat-reptext_ddic  = 'PLANT'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWSLD'. "TR804
  ls_fieldcat-col_pos       = 10.
  ls_fieldcat-reptext_ddic  = 'Sales District'. "TR804
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWDVS'.
  ls_fieldcat-col_pos       = 11.
  ls_fieldcat-reptext_ddic  = 'DIVISION'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'WWRGN'.
  ls_fieldcat-col_pos       = 12.
  ls_fieldcat-reptext_ddic  = 'REGION'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'ARTNR'.
  ls_fieldcat-col_pos       = 13.
  ls_fieldcat-reptext_ddic  = 'PROD #'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'PRDHA'.
  ls_fieldcat-col_pos       = 14.
  ls_fieldcat-reptext_ddic  = ' PRODUCT HIERARCHY '.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'VVBRV'.
  ls_fieldcat-col_pos       = 16.
  ls_fieldcat-reptext_ddic  = 'BILLED REVEVNUE'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'VVBVL'.
  ls_fieldcat-col_pos       = 17.
  ls_fieldcat-reptext_ddic  = 'BILLED VOLUME'.
  APPEND ls_fieldcat TO  rt_fieldcat.

  ls_fieldcat-fieldname     = 'VVCUT'.
  ls_fieldcat-col_pos       = 18.
  ls_fieldcat-reptext_ddic  = '# OF CUSTOMERS'.
  APPEND ls_fieldcat TO  rt_fieldcat.

ENDFORM.                    "FIELDCAT_INIT
