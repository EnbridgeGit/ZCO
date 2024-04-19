REPORT  zbpci016_st MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract S&T data for the BPC application.         *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue    Description                              *
*2012/07/31 M Khan   TR995    Change C: drive to H: drive with         *
*                             directory, file selection using F4       *
*2017/01/13 G YMana  ACR-2583 Change text message 'PATHS_' to 'PATH_'  *
*2017/01/18 G YMana  ACR-2583 Change text message 'CONTRAX_ST_' to     *
*                             'SAP_ST_'                                *
*2017/01/26 G YMana  ACR-2583 Fix amount check and remove '_CT' string *
*                             from all text.                            *
************************************************************************
TABLES: ce11100.

TYPES:  BEGIN OF ty_ce11100,
          paledger    LIKE ce11100-paledger,
          gjahr       LIKE ce11100-gjahr,
          perde       LIKE ce11100-perde,
          rbeln       LIKE ce11100-rbeln,
          bukrs       LIKE ce11100-bukrs,
          artnr       LIKE ce11100-artnr,
          wwsno       LIKE ce11100-wwsno,
          blart       LIKE bkpf-blart,
          vvbvl_me    LIKE ce11100-vvbvl_me,
          wwltl       LIKE ce11100-wwltl,
          wwrsn       LIKE ce11100-wwrsn,
          kstar       LIKE ce11100-kstar, "Moved for Sort
          wwpce       LIKE ce11100-wwpce,
          vvord       LIKE ce11100-vvord,
          vvori       LIKE ce11100-vvori,
          vvbrv       LIKE ce11100-vvbrv,
          vvbvl       LIKE ce11100-vvbvl,
        END OF ty_ce11100.

TYPES:  BEGIN OF ty_coep,
          kokrs       LIKE coep-kokrs,
          objnr       LIKE coep-objnr,
          belnr       LIKE coep-belnr,
          wrttp       LIKE coep-wrttp,
          gjahr       LIKE coep-gjahr,
          perio       LIKE coep-perio,
          kstar       LIKE coep-kstar,
          wkgbtr      LIKE coep-wkgbtr,
        END OF ty_coep.

TYPES:  BEGIN OF ty_output,
          account(15)     TYPE c,
          category(6)     TYPE c,
          datasrc(20)     TYPE c,
          entity(5)       TYPE c,
          intco(8)        TYPE c,
          rptcurrency(2)  TYPE c,
          time(8)         TYPE c,
          customer(16)    TYPE c,
          dso(6)          TYPE c,
          paths(10)       TYPE c,
          projectst(12)   TYPE c,
          service(50)     TYPE c, "Moved for sort
          amount(15)      TYPE p DECIMALS 2,
        END OF ty_output.

DATA: lv_local      TYPE integer,
      wa_ce11100    LIKE coep,
      s_ce11100     TYPE ty_ce11100,
      t_ce11100     LIKE TABLE OF s_ce11100,
      t_ce11100_sum LIKE TABLE OF s_ce11100,
      s_coep        TYPE ty_coep,
      t_coep        LIKE TABLE OF s_coep,
      t_coep_sum    LIKE TABLE OF s_coep,
      s_stlookup    TYPE zbpcstlookup,
      s_output      TYPE ty_output,
      t_output      LIKE TABLE OF s_output,
      t_output_sum  LIKE TABLE OF s_output,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec,
      lv_blart      TYPE cobk-blart.



DATA: msg(80)           TYPE c,
      lv_account(15)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(20)    TYPE c,
      lv_entity(5)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(16)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(50)    TYPE c,
      lv_projectst(12)  TYPE c,
      lv_paths(10)      TYPE c,
      lv_amount(15)     TYPE c,
      wa_amount(15)     TYPE p DECIMALS 2.


DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_paledg    LIKE ce11100-paledger   OBLIGATORY DEFAULT '02',
p_perio     LIKE ce11100-perio      OBLIGATORY.

SELECT-OPTIONS:
s_vrgar     FOR ce11100-vrgar      OBLIGATORY.

SELECT-OPTIONS:
s_kstar     FOR ce11100-kstar     OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:
s_kstar2     FOR ce11100-kstar     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\STActual.csv', "TR995
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\STActual.csv', "TR995
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.


SELECTION-SCREEN END OF BLOCK c1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.


*************************************************************************
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  IF p_local = 'X'.
    PERFORM check_file_path.
  ENDIF.
*End of TR995 changes
*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-ST.csv' INTO csvfile.


  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.


  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.

*----------------------------------------------------------------------*
FORM get_db_data.
  SELECT ce11100~paledger ce11100~gjahr ce11100~perde
         ce11100~rbeln    ce11100~bukrs ce11100~artnr
         ce11100~wwsno    ce11100~kstar ce11100~vvord
         ce11100~vvori    ce11100~vvbrv bkpf~blart
         ce11100~wwpce    ce11100~vvbvl ce11100~vvbvl_me
         ce11100~wwltl    ce11100~wwrsn
    INTO CORRESPONDING FIELDS OF TABLE t_ce11100
    FROM ce11100 LEFT JOIN bkpf
      ON    ce11100~rbeln = bkpf~belnr
        AND ce11100~gjahr = bkpf~gjahr
        AND ce11100~bukrs = bkpf~bukrs
    WHERE ce11100~paledger  =  p_paledg
      AND ce11100~vrgar     IN s_vrgar
      AND ce11100~versi     = ''
      AND ce11100~perio     = p_perio
      AND ( ce11100~kstar     IN s_kstar
        OR  ce11100~wwpce     IN s_kstar )
    .

  LOOP AT t_ce11100 INTO s_ce11100.

    IF s_ce11100-kstar IS INITIAL.
      s_ce11100-kstar = s_ce11100-wwpce.
      MODIFY t_ce11100 FROM s_ce11100.
    ENDIF.

  ENDLOOP.


  SELECT kstar kokrs objnr belnr wrttp perio gjahr wkgbtr
    FROM coep
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    WHERE kokrs = '10'
      AND gjahr = p_perio(4)
      AND perio = p_perio+5(2)
      AND kstar IN s_kstar2
      AND wrttp = '04'
      AND objnr LIKE 'KS%'
  .

  LOOP AT t_coep INTO s_coep.
    s_coep-objnr = s_coep-objnr+6.
    SHIFT s_coep-objnr LEFT DELETING LEADING '0'.
    SHIFT s_coep-kstar LEFT DELETING LEADING '0'.
    s_coep-wkgbtr = s_coep-wkgbtr * -1.

    IF s_coep-kstar = '390686'.
      SELECT SINGLE blart
        FROM cobk
        INTO lv_blart
        WHERE kokrs = s_coep-kokrs
        AND   belnr = s_coep-belnr
      .

      IF sy-subrc = 0.
        IF lv_blart = 'WE'.
          DELETE t_coep INDEX sy-tabix.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY t_coep FROM s_coep.
  ENDLOOP.


ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_ce11100 ASCENDING BY gjahr perde wwsno artnr blart vvbvl_me
                              wwltl wwrsn kstar.

  CLEAR t_ce11100_sum.

  LOOP AT t_ce11100 INTO s_ce11100.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_ce11100 TO t_ce11100_sum.
    ENDAT.
  ENDLOOP.

  SORT t_coep ASCENDING BY gjahr perio objnr kstar.

  CLEAR t_coep_sum.

  LOOP AT t_coep INTO s_coep.
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.


ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.

  DATA: ls_zacctnew TYPE zacctnew,
        lv_glacct TYPE saknr.

  CLEAR t_output.

*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_ST'.
  lv_entity       = 'UGL'.
  lv_dso          = 'No_DSO'.
  lv_paths        = 'No_Paths'.



  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  LOOP AT t_ce11100_sum INTO s_ce11100.
    CLEAR:  st_datarec, lv_account, lv_intco,lv_time, lv_customer, lv_projectst, lv_service, lv_amount.



    CASE s_ce11100-blart.
      WHEN 'S5' OR 'R5' OR 'ST' OR 'TS' OR 'S3' OR 'WE'.
        CONTINUE.
      WHEN OTHERS.
        "Do Nothing.
    ENDCASE.

    IF s_ce11100-rbeln >= '0090000000' AND s_ce11100-rbeln <= '0090009999'.
      CONTINUE.
    ENDIF.

    lv_glacct = s_ce11100-kstar+4.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_glacct
      IMPORTING
        output = lv_glacct.

    CLEAR ls_zacctnew.
    SELECT SINGLE * FROM zacctnew INTO ls_zacctnew
      WHERE glacct = lv_glacct.
    IF sy-subrc <> 0.
      lv_intco = 'NO_IntCo'.
    ELSE.
      IF ls_zacctnew-affil is INITIAL.
         lv_intco = 'NO_IntCo'.
      ELSE.
         CONCATENATE 'IC_' ls_zacctnew-affil INTO lv_intco.
      ENDIF.
    ENDIF.
*    CASE s_ce11100-kstar+4.
*      WHEN '390220'.
*        lv_intco = 'IC_10816'.
*      WHEN '390221'.
*        lv_intco = 'IC_11768'.
*      WHEN '390270'.
*        lv_intco = 'IC_10816'.
*      WHEN '390271'.
*        lv_intco = 'IC_11768'.
*      WHEN OTHERS.
*        lv_intco = 'NO_IntCo'.
*    ENDCASE.

    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.


    lv_projectst    = 'No_ProjectST'.

    IF s_ce11100-wwsno = ''.
      lv_customer   = 'No_Cust_ST_CAD'.
    ELSEIF s_ce11100-wwsno(3) = 'OPT'.
      lv_customer   = 'No_Cust_ST_CAD'.
      "lv_projectst  = s_ce11100-wwsno.
      CONCATENATE 'ST' s_ce11100-wwsno INTO lv_customer.
    ELSEIF s_ce11100-wwsno(3) = 'ADJ'.
      lv_customer   = 'No_Cust_ST_CAD'.
      IF s_ce11100-kstar = '390080'.
        CONCATENATE 'ST_' s_ce11100-wwsno '_S' INTO lv_account.
      ELSEIF s_ce11100-kstar = '390085'.
        CONCATENATE 'ST_' s_ce11100-wwsno '_T' INTO lv_account.
      ENDIF.
    ELSEIF s_ce11100-wwsno CO '0123456789. '.
      CONCATENATE 'SA' s_ce11100-wwsno INTO lv_customer.
    ELSE.
      lv_customer = s_ce11100-wwsno.
    ENDIF.


**Revenue
    lv_rptcurrency  = 'LC'.

    lv_amount = s_ce11100-vvord + s_ce11100-vvori + s_ce11100-vvbrv.

    CLEAR s_stlookup.
    SELECT SINGLE *
      FROM zbpcstlookup
      INTO s_stlookup
      WHERE artnr = s_ce11100-artnr
        AND kstar = s_ce11100-kstar .

    IF sy-subrc <> 4.
      lv_service = s_stlookup-service.
      lv_account = s_stlookup-account.
    ELSE.
      "Record not found use defaults.
      CLEAR s_stlookup.
      SELECT SINGLE *
        FROM zbpcstlookup
        INTO s_stlookup
        WHERE artnr = s_ce11100-artnr
          AND kstar = ''
        .
      IF sy-subrc <> 4.
        lv_service = s_stlookup-service.
        lv_account = s_ce11100-kstar.
        SHIFT lv_account LEFT DELETING LEADING '0'.
      ELSE.
        CONCATENATE 'UNK' s_ce11100-artnr INTO lv_service.
        CONCATENATE 'UNK' s_ce11100-kstar INTO lv_account.
      ENDIF.
    ENDIF.

    IF lv_service = 'ONTARIO_PRODUCERS'.
      lv_customer = 'STONTPROD'.
    ENDIF.
    "-----------------
    IF s_ce11100-wwrsn = 'ZAC'.
      lv_datasrc      = 'SAP_ST_Act'.
    ELSEIF s_ce11100-wwrsn = 'ZES'.
      lv_datasrc      = 'SAP_ST_Est'.
    ELSEIF s_ce11100-wwrsn = 'ZRV'.
      lv_datasrc      = 'SAP_ST_Prior'.
    ELSE.
      lv_datasrc      = 'SAP_ST'.
    ENDIF.
    "-----------------
    CLEAR s_output.
    s_output-account      = lv_account.
    s_output-category     = lv_category.
    s_output-datasrc      = lv_datasrc.
    s_output-entity       = lv_entity.
    s_output-intco        = lv_intco.
    s_output-rptcurrency  = lv_rptcurrency.
    s_output-time         = lv_time.
    s_output-customer     = lv_customer.
    s_output-dso          = lv_dso.
    s_output-service      = lv_service.
    "-----------------
    IF s_ce11100-wwltl IS INITIAL.
      s_output-paths        = lv_paths.
    ELSE.
      CONCATENATE 'PATH_' s_ce11100-wwltl INTO s_output-paths.
    ENDIF.
    "----------------
    s_output-projectst    = lv_projectst.
    s_output-amount       = lv_amount.
    APPEND s_output TO t_output.
**Volume
    IF s_ce11100-vvbvl_me = 'GJ1'.
      lv_rptcurrency  = 'GJ'.

      lv_amount = s_ce11100-vvbvl.

      CLEAR s_stlookup.
      SELECT SINGLE *
        FROM zbpcstlookup
        INTO s_stlookup
        WHERE artnr = s_ce11100-artnr
          AND kstar = s_ce11100-kstar   .

      IF sy-subrc <> 4.
        lv_service = s_stlookup-service.
        lv_account = s_stlookup-account_vol.
      ELSE.
        "Record not found use defaults.
        CLEAR s_stlookup.
        SELECT SINGLE *
          FROM zbpcstlookup
          INTO s_stlookup
          WHERE artnr = s_ce11100-artnr
            AND kstar = ''
          .
        IF sy-subrc <> 4.
          lv_service = s_stlookup-service.
          lv_account = s_ce11100-kstar.
          SHIFT lv_account LEFT DELETING LEADING '0'.
        ELSE.
          CONCATENATE 'UNK' s_ce11100-artnr INTO lv_service.
          CONCATENATE 'UNK' s_ce11100-kstar INTO lv_account.
        ENDIF.
      ENDIF.

      IF lv_service = 'ONTARIO_PRODUCERS'.
        lv_customer = 'STONTPROD'.
      ENDIF.
      "-----------------
      IF s_ce11100-wwrsn = 'ZAC'.
        lv_datasrc      = 'SAP_ST_Act'.
      ELSEIF s_ce11100-wwrsn = 'ZES'.
        lv_datasrc      = 'SAP_ST_Est'.
      ELSEIF s_ce11100-wwrsn = 'ZRV'.
        lv_datasrc      = 'SAP_ST_Prior'.
      ELSE.
        lv_datasrc      = 'SAP_ST'.
      ENDIF.
      "-----------------
      CLEAR s_output.
      s_output-account      = lv_account.
      s_output-category     = lv_category.
      s_output-datasrc      = lv_datasrc.
      s_output-entity       = lv_entity.
      s_output-intco        = lv_intco.
      s_output-rptcurrency  = lv_rptcurrency.
      s_output-time         = lv_time.
      s_output-customer     = lv_customer.
      s_output-dso          = lv_dso.
      s_output-service      = lv_service.
      "------------------------
      IF s_ce11100-wwltl IS INITIAL.
        s_output-paths        = lv_paths.
      ELSE.
        CONCATENATE 'PATH_' s_ce11100-wwltl INTO s_output-paths.
      ENDIF.
      "------------------------
      s_output-projectst    = lv_projectst.
      s_output-amount       = lv_amount.
      APPEND s_output TO t_output.

      "Duplicate these accouts
      IF lv_account = 'D_3002_T' OR lv_account = 'D_3002_S' OR lv_account = 'D_3010_S'.
        CASE lv_account.
          WHEN 'D_3002_T'.
            lv_account = 'D_3005_T'.
          WHEN 'D_3002_S'.
            lv_account = 'D_3005_S'.
          WHEN 'D_3010_S'.
            lv_account = 'D_3005_S'.
        ENDCASE.

        s_output-account = lv_account.
        APPEND s_output TO t_output.
      ENDIF.
    ENDIF.

  ENDLOOP.
*COEP Data

  LOOP AT t_coep_sum INTO s_coep.
    lv_category     = 'Actual'.
    lv_datasrc      = 'SAP_ST'.
    lv_rptcurrency  = 'LC'.
    lv_dso          = 'No_DSO'.
    lv_paths        = 'No_Paths'.
    lv_intco        = 'NO_IntCo'.
    lv_customer     = 'No_Cust_ST_CAD'.
    lv_projectst    = 'No_ProjectST'.

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.
    lv_account = s_coep-kstar.
    lv_entity  = s_coep-objnr.
    lv_amount = s_coep-wkgbtr.

    IF lv_entity = '76511'.
      lv_service = 'YCR_Serv'.
    ELSEIF lv_entity = '77507'.
      lv_service = 'Exchanges'.
    ELSEIF lv_entity = '77515'.
      lv_service = 'LT_Peak_Storage'.
    ELSE.
      CONCATENATE 'UNK' lv_entity INTO lv_service.
    ENDIF.

    IF lv_account = '390686'.
      lv_service = 'YCR_Serv'.
    ENDIF.

    IF lv_service = 'YCR_Serv'.
      lv_customer = 'STYCR'.
    ENDIF.

    CLEAR s_output.
    s_output-account      = lv_account.
    s_output-category     = lv_category.
    s_output-datasrc      = lv_datasrc.
    s_output-entity       = lv_entity.
    s_output-intco        = lv_intco.
    s_output-rptcurrency  = lv_rptcurrency.
    s_output-time         = lv_time.
    s_output-customer     = lv_customer.
    s_output-dso          = lv_dso.
    s_output-service      = lv_service.
    s_output-paths        = lv_paths.
    s_output-projectst    = lv_projectst.
    s_output-amount       = lv_amount.
    APPEND s_output TO t_output.

  ENDLOOP.
*Summarize the final data.
  SORT t_output ASCENDING BY account time intco customer rptcurrency service.

  CLEAR t_output_sum.

  LOOP AT t_output INTO s_output.
*Last sorted field is service
    AT END OF service.
      SUM.
      APPEND s_output TO t_output_sum.
    ENDAT.
  ENDLOOP.


*Add output table to the csvfile.
  LOOP AT t_output_sum INTO s_output.

    IF s_output-amount < 0.
      s_output-amount = s_output-amount * -1.
      lv_amount = s_output-amount.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_output-amount.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE   s_output-account s_output-category s_output-datasrc
                  s_output-entity  s_output-intco s_output-rptcurrency
                  s_output-time s_output-customer s_output-dso
                  s_output-service s_output-paths s_output-projectst
                  lv_amount
                  INTO st_datarec SEPARATED BY delimtr.

    MOVE lv_amount TO wa_amount.
    IF wa_amount <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.
  ENDLOOP.
  IF lv_local = 0.

    PERFORM open_csvfile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO csvfile.
    ENDLOOP.

    PERFORM close_csvfile.
    WRITE: 'File Outputed Successfully to: ', csvfile.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: 'File Outputed Successfully to: ', p_file.
  ENDIF.



ENDFORM.                    "print_report



*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET csvfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET csvfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES


*&---------------------------------------------------------------------*
*Create Touch File.
*----------------------------------------------------------------------*
FORM create_touch_file.
  OPEN DATASET tuchfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH tuchfile msg.
    STOP.
  ENDIF.

  TRANSFER 'This is a touch file' TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "create_touch_file

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
