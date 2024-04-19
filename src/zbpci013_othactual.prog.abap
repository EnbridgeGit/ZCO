REPORT  zbpci013_othactual MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for Other Revenue actuals for        *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue Description                                 *
*2012/08/20 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
*                                                                      *
************************************************************************
TABLES: ce11100.

TYPES:  BEGIN OF ty_ce11100,
          peledger    LIKE ce11100-perio,
          gjahr       LIKE ce11100-gjahr,
          perde       LIKE ce11100-bukrs,
          belnr       LIKE ce11100-belnr,
          bukrs       LIKE ce11100-bukrs,
          wwsub       LIKE ce11100-wwsub,
          wwser       LIKE ce11100-wwser,
          wwprg       LIKE ce11100-wwprg,
          wwrat       LIKE ce11100-wwrat,
          werks       LIKE ce11100-werks,
          artnr       LIKE ce11100-artnr,
          wwpce       like ce11100-wwpce, "Moved for Sort
          vvord       LIKE ce11100-vvord,
          vvori       LIKE ce11100-vvori,
          vvbrv       LIKE ce11100-vvbrv,
        END OF ty_ce11100.

TYPES:  BEGIN OF ty_output,
          account(15)     TYPE c,
          category(6)     TYPE c,
          datasrc(12)     TYPE c,
          entity(3)       TYPE c,
          intco(8)        TYPE c,
          rptcurrency(2)  TYPE c,
          time(8)         TYPE c,
          customer(16)    TYPE c,
          dso(6)          TYPE c,
          service(13)     TYPE c,
          projectst(12)   TYPE c,
          paths(10)        TYPE c,
          amount(15)      TYPE p DECIMALS 2,
        END OF ty_output.

DATA: lv_local      TYPE integer,
      wa_ce11100    LIKE coep,
      s_ce11100     TYPE ty_ce11100,
      t_ce11100     LIKE TABLE OF s_ce11100,
      t_ce11100_sum LIKE TABLE OF s_ce11100,
      s_output      TYPE ty_output,
      t_output      LIKE TABLE OF s_output,
      t_output_sum  LIKE TABLE OF s_output,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.



DATA: msg(80)           TYPE c,
      lv_account(15)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(12)    TYPE c,
      lv_entity(3)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(16)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(13)    TYPE c,
      lv_projectst(12)  TYPE c,
      lv_paths(10)       TYPE c,
      lv_amount(15)     TYPE c.


DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_paledg    LIKE ce11100-paledger OBLIGATORY,
p_perio     LIKE ce11100-perio     OBLIGATORY.

SELECT-OPTIONS:
s_vrgar     FOR ce11100-vrgar     OBLIGATORY,
s_wwpce     FOR ce11100-wwpce     OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 default 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\OTHActual.csv', "TR995
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\OTHActual.csv', "TR995
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.


SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.


*************************************************************************
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILE.
  IF p_local = 'X'.
  PERFORM CHECK_FILE_PATH.
  ENDIF.
*End of TR995 changes
*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-Oth.csv' INTO csvfile.


  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.


  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.
  if lv_local = 0.
    PERFORM create_touch_file.
  endif.


*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT paledger artnr gjahr perde rbeln bukrs wwsub wwser wwprg wwrat werks vvord vvori vvbrv belnr wwpce
    INTO CORRESPONDING FIELDS OF TABLE t_ce11100
    FROM ce11100
    WHERE paledger  = p_paledg
      AND vrgar     IN s_vrgar
      AND versi     = ''
      AND perio     =  p_perio
      AND bukrs     = 'UGL'
      and wwpce     in s_wwpce
    .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_ce11100 ASCENDING BY gjahr perde wwser wwprg wwrat werks wwpce.

  CLEAR t_ce11100_sum.

  LOOP AT t_ce11100 INTO s_ce11100.
*Last sorted field is wwpce
    AT END OF wwpce.
      SUM.
      APPEND s_ce11100 TO t_ce11100_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.

  CLEAR t_output.

*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_OtherRev'.
  lv_entity       = 'UGL'.
  lv_intco        = 'No_IntCo'.
  lv_rptcurrency  = 'LC'.
  lv_dso          = 'No_DSO'.
  lv_paths        = 'No_DelArea'.
  lv_projectst    = 'No_ProjectST'.


  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  DATA: lv_blart(2) TYPE c.

  LOOP AT t_ce11100_sum INTO s_ce11100.
    CLEAR:  st_datarec, lv_blart, lv_account, lv_customer, lv_service, lv_time, lv_amount.

    CONCATENATE s_ce11100-wwpce+4 '_' s_ce11100-wwsub '_rev' INTO lv_account.

    IF s_ce11100-wwpce+4 = '380014'.
      CASE s_ce11100-artnr+12.
        WHEN '900803' OR '900804'.
          lv_account = '380014_ABCT'.
        WHEN '900805'.
          lv_account = '380014_DP'.
        WHEN '900806'.
          lv_account = '380014_ABCAdmin'.
        WHEN '902814'.
          lv_account = '380014_IVA'.
        WHEN OTHERS.
          lv_account = '380014'.
      ENDCASE.
    ELSE.
      lv_account = s_ce11100-wwpce+4.
    ENDIF.

    CASE lv_account.
      WHEN '390598' OR '390599'.
        lv_customer     = 'No_Customer_GS'.
        lv_service      = 'No_Service_GS'.
      WHEN OTHERS.
        lv_customer     = 'No_Customer'.
        lv_service      = 'No_Service'.
    ENDCASE.


    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.


    lv_amount = s_ce11100-vvord + s_ce11100-vvori + s_ce11100-vvbrv.

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

*    CONCATENATE lv_account lv_category lv_datasrc
*                lv_entity  lv_intco lv_rptcurrency
*                lv_time lv_customer lv_dso
*                lv_service lv_paths lv_projectst
*                lv_amount
*                INTO st_datarec SEPARATED BY delimtr.

*    APPEND st_datarec TO t_data.

  ENDLOOP.

*Summarize the final data.
  SORT t_output ASCENDING BY account time paths .

  CLEAR t_output_sum.

  LOOP AT t_output INTO s_output.
*Last sorted field is paths
    AT END OF paths.
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

    IF lv_amount <> 0.
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
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      lv_bol TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = P_FILE
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
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
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
