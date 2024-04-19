*&---------------------------------------------------------------------*
*& Report  ZCCOI013_COBK_COEP_EXTRACT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zccoi013_cobk_coep_extract.

TABLES: cobk,          " CO Object: Document Header
        coep .         " CO Object: Line Items (by Period)

TYPES: BEGIN OF ty_belnr,
        belnr TYPE co_belnr,
       END OF ty_belnr,

       BEGIN OF ty_scobk,
         cobk(321) TYPE c,
       END OF ty_scobk,

       BEGIN OF ty_scoep,
         cobk(899) TYPE c,
       END OF ty_scoep .

DATA: gf_timestmp(16) TYPE c .

DATA: gf_wtgbtr(16) TYPE c,
      gf_wogbtr(16) TYPE c,
      gf_wkgbtr(16) TYPE c,
      gf_wfkbtr(16) TYPE c,
      gf_pagbtr(16) TYPE c,
      gf_pafbtr(16) TYPE c,
      gf_megbtr(16) TYPE c,
      gf_mefbtr(16) TYPE c,
      gf_mbgbtr(16) TYPE c,
      gf_mbfbtr(16) TYPE c,
      gf_itimestmp(16) TYPE c .

DATA: gf_string TYPE string .

DATA: gf_prior_day TYPE d .

DATA: gt_cobk TYPE STANDARD TABLE OF cobk,
      gt_coep TYPE STANDARD TABLE OF coep .

DATA: gw_cobk TYPE cobk,
      gw_coep TYPE coep .

DATA: gs_cobk(321) TYPE c,
      gs_coep(899) TYPE c .

DATA: gt_scobk TYPE STANDARD TABLE OF ty_scobk,
      gs_scobk TYPE ty_scobk .

DATA: gt_scoep TYPE STANDARD TABLE OF ty_scoep,
      gs_scoep TYPE ty_scoep .

DATA: gf_hfilename TYPE localfile,
      gf_ifilename TYPE localfile .

CONSTANTS: c_pipe TYPE c VALUE '|',
           c_error TYPE c VALUE 'E' .

SELECTION-SCREEN BEGIN OF BLOCK dte WITH FRAME TITLE text-t01 .
SELECT-OPTIONS: so_cpudt FOR cobk-cpudt DEFAULT syst-datum .
SELECTION-SCREEN END OF BLOCK dte .

SELECTION-SCREEN BEGIN OF BLOCK fpt WITH FRAME TITLE text-t02 .
PARAMETERS: p_path TYPE FILEEXTERN
  DEFAULT '/usr/sap/interfaces/D30/TEST/' .
SELECTION-SCREEN END OF BLOCK fpt .

CLEAR: gt_cobk[] .
SELECT *
  INTO TABLE gt_cobk
  FROM cobk
 WHERE cpudt IN so_cpudt .

CLEAR: gt_coep[] .

IF NOT gt_cobk[] IS INITIAL .
  SELECT *
    INTO TABLE gt_coep
    FROM coep
    CLIENT SPECIFIED
   FOR ALL ENTRIES IN gt_cobk
   WHERE belnr = gt_cobk-belnr
     AND mandt = syst-mandt
     AND kokrs = '10' .

ELSE .

  MESSAGE s000(zfi01) WITH 'No Data found!' DISPLAY LIKE c_error .

ENDIF .

CLEAR: gf_wtgbtr, gf_wogbtr, gf_wkgbtr, gf_wfkbtr, gf_pagbtr,
       gf_pafbtr, gf_megbtr, gf_mefbtr, gf_mbgbtr, gf_mbfbtr,
       gf_timestmp, gf_string .

LOOP AT gt_cobk
   INTO gw_cobk .

  WRITE: gw_cobk-timestmp TO gf_timestmp .

  CONCATENATE gw_cobk-mandt gw_cobk-kokrs gw_cobk-belnr
              gw_cobk-gjahr gw_cobk-versn gw_cobk-vrgng
              gf_timestmp gw_cobk-perab gw_cobk-perbi
              gw_cobk-bldat gw_cobk-budat gw_cobk-cpudt
              gw_cobk-usnam gw_cobk-bltxt gw_cobk-stflg
              gw_cobk-stokz gw_cobk-refbt gw_cobk-refbn
              gw_cobk-refbk gw_cobk-refgj gw_cobk-blart
              gw_cobk-orgvg gw_cobk-sumbz gw_cobk-delbz
              gw_cobk-wsdat gw_cobk-kurst gw_cobk-varnr
              gw_cobk-kwaer gw_cobk-ctyp1 gw_cobk-ctyp2
              gw_cobk-ctyp3 gw_cobk-ctyp4 gw_cobk-awtyp
              gw_cobk-aworg gw_cobk-logsystem gw_cobk-cputm
              gw_cobk-alebz gw_cobk-alebn gw_cobk-awsys
              gw_cobk-awref_rev gw_cobk-aworg_rev gw_cobk-valdt
         INTO gs_cobk SEPARATED BY c_pipe .

  APPEND gs_cobk TO gt_scobk .

ENDLOOP .

LOOP AT gt_coep
 INTO gw_coep .

  WRITE: gw_coep-wtgbtr TO gf_wtgbtr,
     gw_coep-wogbtr TO gf_wogbtr,
     gw_coep-wkgbtr TO gf_wkgbtr,
     gw_coep-wkfbtr TO gf_wfkbtr,
     gw_coep-pagbtr TO gf_pagbtr,
     gw_coep-pafbtr TO gf_pafbtr,
     gw_coep-megbtr TO gf_megbtr,
     gw_coep-mefbtr TO gf_mefbtr,
     gw_coep-mbgbtr TO gf_mbgbtr,
     gw_coep-mbfbtr TO gf_mbfbtr,
     gw_coep-timestmp TO gf_itimestmp .

  CONCATENATE gw_coep-mandt gw_coep-kokrs gw_coep-belnr
              gw_coep-buzei gw_coep-perio gf_wtgbtr gf_wogbtr
              gf_wkgbtr gf_wfkbtr gf_pagbtr gf_pafbtr
              gf_megbtr gf_mefbtr gf_mbgbtr gf_mbfbtr
              gw_coep-lednr gw_coep-objnr gw_coep-gjahr
              gw_coep-wrttp gw_coep-versn gw_coep-kstar
              gw_coep-hrkft gw_coep-vrgng gw_coep-parob
              gw_coep-parob1 gw_coep-uspob gw_coep-vbund
              gw_coep-pargb gw_coep-beknz gw_coep-twaer
              gw_coep-owaer gw_coep-meinh gw_coep-meinb
              gw_coep-mvflg gw_coep-sgtxt gw_coep-refbz
              gw_coep-zlenr gw_coep-bw_refbz gw_coep-gkont
              gw_coep-gkoar gw_coep-werks gw_coep-matnr
              gw_coep-rbest gw_coep-ebeln gw_coep-ebelp
              gw_coep-zekkn gw_coep-erlkz gw_coep-pernr
              gw_coep-btrkl gw_coep-objnr_n1 gw_coep-objnr_n2
              gw_coep-objnr_n3 gw_coep-paobjnr gw_coep-beltp
              gw_coep-bukrs gw_coep-gsber gw_coep-fkber
              gw_coep-scope gw_coep-logsyso gw_coep-pkstar
              gw_coep-pbukrs gw_coep-pfkber gw_coep-pscope
              gw_coep-logsysp gw_coep-dabrz gw_coep-bwstrat
              gw_coep-objnr_hk gf_itimestmp gw_coep-qmnum
              gw_coep-geber gw_coep-pgeber gw_coep-grant_nbr
              gw_coep-pgrant_nbr gw_coep-refbz_fi gw_coep-segment
              gw_coep-psegment gw_coep-budget_pd gw_coep-pbudget_pd
              gw_coep-zzloc gw_coep-zzref
         INTO gs_coep SEPARATED BY c_pipe .

  APPEND gs_coep TO gt_scoep .

ENDLOOP .

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

CONCATENATE: p_path
             'COBK' syst-datum syst-uzeit '.dat'
        INTO gf_hfilename,

             p_path 'COEP' syst-datum syst-uzeit '.dat'
        INTO gf_ifilename .

IF gt_scobk[] IS NOT INITIAL .
  OPEN DATASET gf_hfilename
    FOR OUTPUT
    IN TEXT MODE
    ENCODING DEFAULT .

  LOOP AT gt_scobk
     INTO gs_scobk .

    TRANSFER gs_scobk TO gf_hfilename .

  ENDLOOP .


  CLOSE DATASET gf_hfilename .
ENDIF .


IF gt_scoep[] IS NOT INITIAL .
  OPEN DATASET gf_ifilename
    FOR OUTPUT
    IN TEXT MODE
    ENCODING DEFAULT .

  LOOP AT gt_scoep
     INTO gs_scoep .

    TRANSFER gs_scoep TO gf_ifilename .

  ENDLOOP .


  CLOSE DATASET gf_ifilename .

ENDIF .
