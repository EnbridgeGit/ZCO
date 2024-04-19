REPORT  zbpci005_internalorderactual MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for internal orders actuals for      *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By      Issue    Description                               *
*2012/07/31 M Khan  TR995    Change C: drive to H: drive with          *
*                            directory, file selection using F4        *
*2012/08/05 GYMANA  SDP58326 Replace object number with work order     *
*                   EAM      type in column IO_WBS for all Labour acct *
*                            types if they types match between COEP &  *
*                            AUFK                                      *
************************************************************************
TABLES: coep, aufk.

TYPES:  BEGIN OF ty_coep,
          perio   LIKE coep-perio,
          objnr   LIKE coep-objnr,
          gjahr   LIKE coep-gjahr,
          bukrs   LIKE coep-bukrs,
          kstar   LIKE coep-kstar,
          wrttp   LIKE coep-wrttp,
          versn   LIKE coep-versn,
          wogbtr  LIKE coep-wogbtr,
        END OF ty_coep.

DATA: lv_local    TYPE integer,
      s_coep      TYPE ty_coep,
      wa_coep     LIKE TABLE OF s_coep WITH HEADER LINE,
      wa_coep2    LIKE TABLE OF s_coep WITH HEADER LINE,    "SDP58326
      t_coep      LIKE TABLE OF s_coep,
      t_coep_sum  LIKE TABLE OF s_coep,
      st_datarec  TYPE string,
      t_data      LIKE TABLE OF st_datarec,
      w_auart     LIKE aufk-auart,                          "SDP58326
      w_kstar     LIKE coep-kstar.                          "SDP58326


DATA: msg(80)           TYPE c,
      lv_account(6)     TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(6)     TYPE c,
      lv_entity(4)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(10)    TYPE c,
      lv_ioswbs(20)     TYPE c,
      lv_division(11)   TYPE c,
      lv_amount(20)     TYPE c.


DATA: tuchfile          LIKE rfpdo-rfbifile.


CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


PARAMETERS:
p_perio   LIKE coep-perio OBLIGATORY.

SELECT-OPTIONS:
s_objnr   FOR coep-objnr OBLIGATORY
          DEFAULT 'OR0000000000000000' TO 'OR9999999999999999'.

PARAMETERS:
p_gjahr   LIKE coep-gjahr OBLIGATORY.

SELECT-OPTIONS:
s_wrttp   FOR coep-wrttp OBLIGATORY DEFAULT '4',
s_kstar   FOR coep-kstar,
s_versn   FOR coep-versn,
s_ortyp   FOR aufk-auart OBLIGATORY.                        "SDP58326

SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\IOActual.csv', "TR995
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\IOActual.csv', "TR995
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/capex/' INTO csvfile.


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
*End of TR995 changes*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'capex.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_gjahr '-' p_perio+1(2) '-IO.csv' INTO csvfile.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.

  PERFORM get_db_data_new.
*  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.


*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
    WHERE perio  = p_perio
      AND objnr IN s_objnr
      AND gjahr  = p_gjahr
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn.

  IF sy-subrc = 0.                                          "SDP58326
    LOOP AT t_coep INTO wa_coep.                            "SDP58326
                                                            "SDP58326
      CLEAR w_kstar.                                        "SDP58326
      MOVE wa_coep-kstar TO w_kstar.                        "SDP58326
                                                            "SDP58326
      IF w_kstar+4(2) = '71'.                               "SDP58326
                                                            "SDP58326
        SELECT * FROM aufk                                  "SDP58326
         WHERE objnr = wa_coep-objnr.                       "SDP58326
                                                            "SDP58326
          IF sy-subrc = 0.                                  "SDP58326
            IF aufk-auart IN s_ortyp.                       "SDP58326
              MOVE aufk-auart TO wa_coep-objnr.             "SDP58326
            ENDIF.                                          "SDP58326
          ENDIF.                                            "SDP58326
                                                            "SDP58326
        ENDSELECT.                                          "SDP58326
      ENDIF.                                                "SDP58326
      APPEND wa_coep TO wa_coep2.                           "SDP58326
                                                            "SDP58326
    ENDLOOP.                                                "SDP58326
  ENDIF.                                                    "SDP58326

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT wa_coep2 ASCENDING BY gjahr perio objnr kstar.

  LOOP AT wa_coep2 INTO s_coep.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.


*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_OM'.
  lv_rptcurrency  = 'LC'.
  lv_project      = 'No_Project'.
  lv_division     = 'No_Division'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_intco, lv_time, lv_ioswbs, lv_amount.

    lv_account = s_coep-kstar+4.

    lv_entity  = s_coep-bukrs.

    SELECT SINGLE affil
      INTO lv_intco
      FROM zacctnew
      WHERE glacct  = s_coep-kstar
        AND cocode  = s_coep-bukrs.
    IF lv_intco = ''.
      SELECT SINGLE affil
        INTO lv_intco
        FROM zacctnew
        WHERE glacct  = s_coep-kstar
          AND cocode  = ''.
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.
    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.
    lv_ioswbs = s_coep-objnr+2.
    SHIFT lv_ioswbs LEFT DELETING LEADING '0'.
    CONCATENATE 'IO_' lv_ioswbs INTO lv_ioswbs.
    IF s_coep-wogbtr < 0.
*If negative convert to positive and add '-' infront.
      s_coep-wogbtr = s_coep-wogbtr * -1.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.
    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_project lv_ioswbs lv_division
                lv_amount
                INTO st_datarec SEPARATED BY delimtr.
    IF s_coep-wogbtr <> 0.
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

ENDFORM.                    "CREATE_TOUCH_FILE

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
*&---------------------------------------------------------------------*
*&      Form  GET_DB_DATA_NEW
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_db_data_new .

  DATA: lt_aufk TYPE TABLE OF aufk,
        ls_aufk TYPE aufk.


  PERFORM gui_in_progress USING 5
                       'COEP data extraction is in progress. '.
  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
   INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
   WHERE objnr IN s_objnr
     AND kstar IN s_kstar
     AND gjahr  = p_gjahr
     AND perio  = p_perio
     AND wrttp IN s_wrttp
     AND versn IN s_versn.
  IF t_coep[] IS NOT INITIAL.
    PERFORM gui_in_progress USING 50
                  'Order Header data extraction is in progress. '.
    SELECT * FROM aufk INTO TABLE lt_aufk
        FOR ALL ENTRIES IN t_coep
        WHERE objnr = t_coep-objnr
          AND auart IN s_ortyp.
  ENDIF.
  PERFORM gui_in_progress USING 75
                   'Data preparation for output is in progress. '.
  LOOP AT t_coep INTO wa_coep.
    CLEAR: w_kstar,
           ls_aufk.
    w_kstar = wa_coep-kstar.
    IF w_kstar+4(2) = '71'.
      READ TABLE lt_aufk INTO ls_aufk WITH KEY
                         objnr = wa_coep-objnr.
      IF ls_aufk-auart IN s_ortyp.
          wa_coep-objnr = ls_aufk-auart.
      ENDIF.
    ENDIF.
    APPEND wa_coep TO wa_coep2.
  ENDLOOP.

ENDFORM.                    " GET_DB_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  GUI_IN_PROGRESS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM gui_in_progress  USING    iv_perc TYPE i
                               iv_msg  TYPE char100.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = iv_perc
      text       = iv_msg.

ENDFORM.                    " GUI_IN_PROGRESS
