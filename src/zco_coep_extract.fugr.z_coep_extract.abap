FUNCTION Z_COEP_EXTRACT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(QUERY_TABLE) TYPE  DD02L-TABNAME
*"     VALUE(DELIMITER) TYPE  SONV-FLAG OPTIONAL
*"     VALUE(NO_DATA) TYPE  SONV-FLAG OPTIONAL
*"     VALUE(ROWSKIPS) TYPE  SOID-ACCNT OPTIONAL
*"     VALUE(ROWCOUNT) TYPE  SOID-ACCNT OPTIONAL
*"  EXPORTING
*"     VALUE(OUT_TABLE) TYPE  COEP
*"  TABLES
*"      OPTIONS STRUCTURE  /SAPDS/RFC_DB_OPT
*"      FIELDS STRUCTURE  RFC_DB_FLD
*"      OUT_TABLE1 STRUCTURE  COEP
*"      TBLOUT128 STRUCTURE  /SAPDS/TAB128
*"      TBLOUT512 STRUCTURE  /SAPDS/TAB512
*"      TBLOUT2048 STRUCTURE  /SAPDS/TAB2048
*"      TBLOUT8192 STRUCTURE  /SAPDS/TAB8192
*"      TBLOUT30000 STRUCTURE  /SAPDS/TAB30K
*"  EXCEPTIONS
*"      TABLE_NOT_AVAILABLE
*"      TABLE_WITHOUT_DATA
*"      OPTION_NOT_VALID
*"      FIELD_NOT_VALID
*"      NOT_AUTHORIZEC
*"      DATA_BUFFER_EXCEEDED
*"----------------------------------------------------------------------
   tables: coep .

   types: begin of ty_belnr,
           belnr type co_belnr,
          end of ty_belnr .

   data: gf_kokrs type kokrs,
         gf_belnr type co_belnr,
         gf_buzei type co_buzei .

   data: gf_wtgbtr(16) type c,
         gf_wogbtr(16) type c,
         gf_wkgbtr(16) type c,
         gf_wfkbtr(16) type c,
         gf_pagbtr(16) type c,
         gf_pafbtr(16) type c,
         gf_megbtr(16) type c,
         gf_mefbtr(16) type c,
         gf_mbgbtr(16) type c,
         gf_mbfbtr(16) type c,
         gf_timestmp(16) type c .

   data: gf_string type string .

   data: gf_prior_day type d .

   data: gs_input type RFC_DB_FLD .

   data: gw_belnr type ty_belnr,
         gt_belnr type STANDARD TABLE OF ty_belnr .

   constants: c_pipe type c VALUE '|' .

*   select a~fieldname a~intlen a~leng a~inttype
*          b~ddtext
*     into table fields
*     from dd03l as a
**    inner join dd04l as b
**       on b~tabname = a~tabname
**      and b~ddlanguage = syst-langu
**      and b~as4local = a~as4local
**      and b~fieldname = a~fieldname
*    inner join dd04t as b
*       on b~rollname = a~rollname
*      and b~ddlanguage = syst-langu
*      and b~as4local = a~as4local
*      and b~as4vers  = a~as4vers
*    where a~tabname = query_table .
*
*   gf_prior_day =  syst-datum - 1 .  " '20220128' .

  clear: gf_kokrs, gf_belnr, gf_buzei .

  read TABLE fields INTO gs_input with key fieldname = 'KOKRS' .
  gf_kokrs = gs_input-FIELDTEXT .

  read TABLE fields INTO gs_input with key fieldname = 'BELNR' .
  gf_belnr = gs_input-FIELDTEXT .

  read TABLE fields INTO gs_input with key fieldname = 'BUZEI' .
  gf_buzei = gs_input-FIELDTEXT .

   clear: gt_belnr[] .
   select belnr
     into table gt_belnr
     from cobk
    where cpudt = gf_prior_day .

   clear: out_table1[] .

   if not gf_kokrs is initial and
          gf_belnr is not INITIAL and
      not gf_buzei is INITIAL .

   select  * UP TO 1 rows
     into out_table
     from coep
*     CLIENT SPECIFIED
*    for ALL ENTRIES IN gt_belnr
    where kokrs = gf_kokrs
      and belnr = gf_belnr
      and buzei = gf_buzei .
*      and kokrs = '10' .

     ENDSELECT .

  else .

     raise TABLE_WITHOUT_DATA .

   endif .

     coep = out_table1 .

     clear: gf_wtgbtr, gf_wogbtr, gf_wkgbtr, gf_wfkbtr, gf_pagbtr,
            gf_pafbtr, gf_megbtr, gf_mefbtr, gf_mbgbtr, gf_mbfbtr,
            gf_timestmp, gf_string .

     write: coep-wtgbtr to gf_wtgbtr,
            coep-wogbtr to gf_wogbtr,
            coep-wkgbtr to gf_wkgbtr,
            coep-wkfbtr to gf_wfkbtr,
            coep-pagbtr to gf_pagbtr,
            coep-pafbtr to gf_pafbtr,
            coep-megbtr to gf_megbtr,
            coep-mefbtr to gf_mefbtr,
            coep-mbgbtr to gf_mbgbtr,
            coep-mbfbtr to gf_mbfbtr,
            coep-timestmp to gf_timestmp .

*   concatenate coep-kokrs  coep-belnr  coep-buzei  coep-perio
*               gf_wtgbtr gf_wogbtr gf_wkgbtr gf_wfkbtr
*               gf_pagbtr gf_pafbtr gf_megbtr gf_mefbtr
*               gf_mbgbtr gf_mbfbtr coep-lednr  coep-objnr
*               coep-gjahr  coep-wrttp  coep-versn  coep-kstar
*               coep-hrkft  coep-vrgng  coep-parob  coep-parob1
*               coep-uspob  coep-vbund  coep-pargb  coep-beknz
*               coep-twaer  coep-owaer  coep-meinh  coep-meinb
*               coep-mvflg  coep-sgtxt  coep-refbz  coep-zlenr
*               coep-bw_refbz coep-gkont coep-gkoar coep-werks
*               coep-matnr  coep-rbest  coep-ebeln  coep-ebelp
*               coep-zekkn  coep-erlkz  coep-pernr  coep-btrkl
*               coep-objnr_n1 coep-objnr_n2 coep-objnr_n3 coep-paobjnr
*               coep-beltp  coep-bukrs  coep-gsber  coep-fkber
*               coep-scope  coep-logsysO coep-pkstar coep-pbukrs
*               coep-pfkber coep-pscope coep-logsysp coep-dabrz
*               coep-bwstrat coep-objnr_hk gf_timestmp coep-qmnum
*               coep-geber  coep-pgeber coep-grant_nbr coep-pgrant_nbr
*               coep-refbz_fi coep-segment coep-psegment coep-budget_pd
*               coep-pbudget_pd coep-zzloc coep-zzref
*          into gf_string SEPARATED BY c_comma .
*
*  append gf_string to tblout512 .

ENDFUNCTION.
