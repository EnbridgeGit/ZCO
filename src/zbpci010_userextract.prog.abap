REPORT  zbpci010_userextract MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for Unbilled actuFals for            *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:
*Date       By       Issue    Description
*2014/08/06 GYMANA   SDP58326 -Modify code to remove activity type from
*                    EAM      Entity column
*                             -Bypass all work orders ('EM*' or 'PM*')
*                              in the report.
*                             -fix negative hours display.
*2013/12/06 GYMANA   SDP60031 Corrected a bug that was preventing the
*                             proper selection of wage type
************************************************************************
TABLES: coep, aufk.

TYPES:  BEGIN OF ty_coep,
          perio   LIKE coep-perio,
          objnr   LIKE coep-objnr,
          gjahr   LIKE coep-gjahr,
          bukrs   LIKE coep-bukrs,
          kstar   LIKE coep-kstar,
          sgtxt   LIKE coep-sgtxt,
          pernr   LIKE coep-pernr,
          mbgbtr  LIKE coep-mbgbtr,
          wogbtr  LIKE coep-wogbtr,
        END OF ty_coep.
*{   INSERT         D30K925887                                        1
types :begin of  ty_aufk,
  aufnr type aufk-aufnr,
  auart type aufk-auart,
  objnr type aufk-objnr,
  end of ty_aufk.
data : git_aufk type STANDARD TABLE OF ty_aufk,
      gwa_aufk type ty_aufk.
*}   INSERT

DATA: lv_local    TYPE integer,
*      wa_coep     LIKE coep,
      s_coep      TYPE ty_coep,
      wa_coep     LIKE TABLE OF s_coep WITH HEADER LINE,
      wa_coep2    LIKE TABLE OF s_coep WITH HEADER LINE,      "SDP58326
      t_coep      LIKE TABLE OF s_coep,
      t_coep_sum  LIKE TABLE OF s_coep,
      st_datarec  TYPE string,
      t_data      LIKE TABLE OF st_datarec,
      w_auart     LIKE aufk-auart,                            "SDP58326
      w_kstar     LIKE coep-kstar.                            "SDP58326


DATA: msg(80)           TYPE c,
      lv_account(10)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(7)     TYPE c,
      lv_entity(22)     TYPE c,
      lv_rptcurrency(3) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(16)    TYPE c,
      lv_ioswbs(16)     TYPE c,
      lv_empnum(12)     TYPE c,
      lv_wagetype(7)    TYPE c,
      lv_hours(20)      TYPE c,
      lv_amount(20)     TYPE c,
      lv_division(22)   TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.


CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


PARAMETERS:
p_perio   LIKE coep-perio OBLIGATORY,
p_gjahr   LIKE coep-gjahr OBLIGATORY.

SELECT-OPTIONS:
s_bukrs   FOR coep-bukrs,
s_wrttp   FOR coep-wrttp OBLIGATORY DEFAULT '4',
s_kstar   FOR coep-kstar,
s_pernr   FOR coep-pernr.

SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE   string
                             DEFAULT 'H:\SAPTEMP\EMPActual.csv', "TR995
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.


SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/empdetail/'
              INTO csvfile.
*{   INSERT         D30K925887                                        1
refresh git_aufk.
select aufnr auart objnr into table git_aufk
  from aufk.
  SORT git_aufk by objnr.
*}   INSERT

*************************************************************************
*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'empdetail.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_gjahr '-' p_perio+1(2) '-EMP.csv'
              INTO csvfile.


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

*{   REPLACE        D30K925873                                        1
*\  SELECT gjahr perio bukrs objnr kstar sgtxt pernr mbgbtr wogbtr
*\    INTO CORRESPONDING FIELDS OF TABLE t_coep
*\    FROM coep
*\    WHERE perio =  p_perio
*\      AND gjahr =  p_gjahr
*\      AND bukrs IN s_bukrs
*\      AND wrttp IN s_wrttp
*\      AND kstar IN s_kstar
*\      AND pernr IN s_pernr

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 5
   TEXT             = 'In Progress '
          .

Refresh t_coep.
  SELECT perio objnr gjahr  bukrs  kstar sgtxt pernr mbgbtr wogbtr
    INTO table t_coep
    FROM coep
    WHERE perio =  p_perio
      AND gjahr =  p_gjahr
      AND bukrs IN s_bukrs
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND pernr IN s_pernr
    ORDER BY PRIMARY KEY.


*}   REPLACE
    .

  IF sy-subrc = 0.                                            "SDP58326
    LOOP AT t_coep INTO wa_coep.                              "SDP58326
                                                              "SDP58326
*{   REPLACE        D30K925887                                        2
*\        SELECT * FROM aufk                                    "SDP58326
*\         WHERE objnr = wa_coep-objnr.                         "SDP58326
*\                                                              "SDP58326
*\        ENDSELECT.
clear gwa_aufk.
read table git_aufk into gwa_aufk with KEY objnr = wa_coep-objnr BINARY SEARCH.
   IF sy-subrc = 0.
          IF gwa_aufk-auart(2) = 'EM' OR
             gwa_aufk-auart(2) = 'PM'.
            SKIP.
          ELSE.
            APPEND wa_coep TO wa_coep2.
          ENDIF.
   ELSE.
   APPEND wa_coep TO wa_coep2.
  ENDIF.
endloop.
endif.
*}   REPLACE

*{   DELETE         D30K925887                                        3
*\        IF sy-subrc = 0.                                      "SDP58326
*\          IF aufk-auart(2) = 'EM' OR                          "SDP58326
*\             aufk-auart(2) = 'PM'.                            "SDP58326
*\            SKIP.                                             "SDP58326
*\          ELSE.                                               "SDP58326
*\            APPEND wa_coep TO wa_coep2.                       "SDP58326
*\          ENDIF.                                              "SDP58326
*\        ELSE.
*\          APPEND wa_coep TO wa_coep2.                         "SDP58326
*\        ENDIF.                                                "SDP58326
*\                                                              "SDP58326
*\    ENDLOOP.                                                  "SDP58326
*\  ENDIF.                                                      "SDP58326
*}   DELETE

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  LOOP AT wa_coep2 INTO s_coep.                               "SDP58326
    s_coep-sgtxt = s_coep-sgtxt(4).                           "SDP58326
    MODIFY wa_coep2 FROM s_coep.                              "SDP58326
  ENDLOOP.

  SORT wa_coep2                                               "SDP58326
  ASCENDING BY gjahr perio bukrs objnr kstar sgtxt pernr.     "SDP58326

  LOOP AT wa_coep2 INTO s_coep.                               "SDP58326
*Last sorted field is kstar
    AT END OF pernr.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.


*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_EMP'.



  CONCATENATE text-001 text-002 text-003 text-004
              text-006 text-007 text-008 text-009
              text-011 text-013 text-012 text-014
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_time, lv_amount.

    DATA: lv_poski(16) TYPE c.

    IF s_coep-pernr <> 0.
      CONCATENATE 'Emp_' s_coep-pernr INTO lv_empnum.
    ELSE.
      lv_empnum = 'No_Employee'.
    ENDIF.

    IF s_coep-kstar+4(1) = '7'.                             "SDP58326
      lv_empnum = 'No_Employee'.                            "SDP58326
    ENDIF.                                                  "SDP58326

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.


    IF s_coep-objnr(2) = 'OR'.
      lv_ioswbs = s_coep-objnr+2.
      SHIFT lv_ioswbs LEFT DELETING LEADING '0'.
      CONCATENATE 'IO_' lv_ioswbs INTO lv_ioswbs.
      lv_project      = 'No_Project'.
      lv_entity       = s_coep-bukrs.
      lv_division     = lv_ioswbs.
    ELSEIF s_coep-objnr(2) = 'PR'.
      SELECT SINGLE poski
        INTO lv_poski
        FROM prps
        WHERE pspnr = s_coep-objnr+2
      .

      lv_project = lv_poski(9).
      REPLACE ALL OCCURRENCES OF '-' IN lv_project WITH '.'.

      lv_ioswbs = lv_poski+10.
      CONCATENATE 'WBS_' lv_ioswbs INTO lv_ioswbs.

      lv_entity  = s_coep-bukrs.
      lv_division = 'DivProject'.
    ELSE.
      "No project, not Order
      lv_project      = 'No_Project'.
      lv_ioswbs       = 'No_IO'.

      lv_entity  = s_coep-objnr+6(10).                      "SDP58326
      SHIFT lv_entity LEFT DELETING LEADING '0'.
      REPLACE ALL OCCURRENCES OF '-' IN lv_entity WITH '_'.
      lv_division = lv_entity.


* Change no longer required - GYMANA                        "SDP58326
*      lv_empnum = s_coep-objnr+16(6).                      "SDP58326
    ENDIF.

    IF s_coep-sgtxt EQ ' '.                                 "SDP60031
      lv_wagetype = 'No_WT'.                                "SDP60031
    ELSEIF s_coep-sgtxt CO '1234567890 '.                   "SDP60031
      CONCATENATE 'WT_' s_coep-sgtxt INTO lv_wagetype.      "SDP60031
    ELSE.                                                   "SDP60031
      lv_wagetype = 'No_WT'.                                "SDP60031
    ENDIF.                                                  "SDP60031

    "Amount
    IF s_coep-wogbtr <> 0.

      lv_account = s_coep-kstar+4.


      IF s_coep-wogbtr < 0.
        "If negative convert to positive and add '-' infront.
        s_coep-wogbtr = s_coep-wogbtr * -1.
        lv_amount = s_coep-wogbtr.
        SHIFT lv_amount LEFT DELETING LEADING ' '.
        CONCATENATE '-' lv_amount INTO lv_amount.
      ELSE.
        lv_amount = s_coep-wogbtr.
        SHIFT lv_amount LEFT DELETING LEADING ' '.
      ENDIF.

      lv_rptcurrency  = 'LC'.

      CONCATENATE lv_account lv_category lv_datasrc lv_entity
                  lv_rptcurrency lv_time lv_project lv_ioswbs
                  lv_empnum lv_wagetype lv_amount lv_division
                  INTO st_datarec SEPARATED BY delimtr.


      APPEND st_datarec TO t_data.
    ENDIF.

    "Hours
    IF s_coep-mbgbtr <> 0.

      lv_account = s_coep-kstar+4.
      IF s_coep-mbgbtr < 0.                                   "SDP58326
        "If negative convert to positive and add '-' infront. "SDP58326
        s_coep-mbgbtr = s_coep-mbgbtr * -1.                   "SDP58326
        lv_amount = s_coep-mbgbtr.                            "SDP58326
        SHIFT lv_amount LEFT DELETING LEADING ' '.            "SDP58326
        CONCATENATE '-' lv_amount INTO lv_amount.             "SDP58326
      ELSE.                                                   "SDP58326
        lv_amount = s_coep-mbgbtr.                            "SDP58326
        SHIFT lv_amount LEFT DELETING LEADING ' '.            "SDP58326
      ENDIF.                                                  "SDP58326

      lv_rptcurrency = 'HRS'.

      CONCATENATE lv_account lv_category lv_datasrc lv_entity
            lv_rptcurrency lv_time lv_project lv_ioswbs
            lv_empnum lv_wagetype lv_amount lv_division
            INTO st_datarec SEPARATED BY delimtr.


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
