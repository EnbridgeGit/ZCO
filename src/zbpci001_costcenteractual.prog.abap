REPORT  zbpci001_costcenteractual MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for cost center actuals for          *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue    Description                              *
*2014/08/05 G Ymana  SDP58326 Remove activity type from COEP-OBJNR     *
*                    EAM      before populating Entity column          *
*2012/07/31 M Khan   TR995    Change C: drive to H: drive with         *
*                             directory, file selection using F4       *
*                                                                      *
************************************************************************
TABLES: coep.
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
      wa_coep     LIKE coep,
      s_coep      TYPE ty_coep,
      t_coep      LIKE TABLE OF s_coep,
      t_coep_sum  LIKE TABLE OF s_coep,
      st_datarec  TYPE string,
      t_data      LIKE TABLE OF st_datarec.


DATA: msg(80)           TYPE c,
      lv_account(6)     TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(6)     TYPE c,
      lv_entity(22)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(16)    TYPE c,
      lv_ioswbs(16)     TYPE c,
      lv_division(16)   TYPE c,
      lv_amount(20)     TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.
CONSTANTS: delimtr  TYPE c VALUE ','.
"*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


PARAMETERS:
p_perio   LIKE coep-perio OBLIGATORY.

SELECT-OPTIONS:
s_objnr   FOR coep-objnr OBLIGATORY
          DEFAULT 'KS00000000000000' TO 'KS99999999999999'.
PARAMETERS:
p_gjahr   LIKE coep-gjahr OBLIGATORY.
SELECT-OPTIONS:
s_wrttp   FOR coep-wrttp OBLIGATORY DEFAULT '4',
s_kstar   FOR coep-kstar,
s_versn   FOR coep-versn.

SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
*p_file    TYPE   string DEFAULT 'C:\SAPTEMP\CCActual.csv',    "TR995
 p_file    TYPE   string DEFAULT 'H:\SAPTEMP\CCActual.csv',    "TR995
 p_server  RADIOBUTTON GROUP rad1,
           csvfile   LIKE        rfpdo-rfbifile.


SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/capex/' INTO csvfile.

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

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
*    EXPORTING
*      mask      = ',CSV File,*.csv'
*      static    = 'X'
*    CHANGING
*      file_name = p_file.
*
*************************************************************************
*                         START-OF-SELECTION                            *
*************************************************************************
START-OF-SELECTION.
  CONCATENATE csvfile 'capex.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_gjahr '-' p_perio+1(2) '-CC.csv' INTO csvfile.


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
*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT perio objnr gjahr wrttp bukrs kstar versn wogbtr
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
    WHERE perio =  p_perio
      AND objnr IN s_objnr
      AND gjahr =  p_gjahr
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn
    .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_coep ASCENDING BY gjahr perio objnr kstar.

  LOOP AT t_coep INTO s_coep.
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
  lv_ioswbs       = 'No_IO'.
  lv_division     = 'No_Division'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_intco, lv_time, lv_amount.

    lv_account = s_coep-kstar+4.

    lv_entity  = s_coep-objnr+6(10).                          "SDP58326
    SHIFT lv_entity LEFT DELETING LEADING '0'.
    REPLACE ALL OCCURRENCES OF '-' IN lv_entity WITH '_'.

    SELECT SINGLE affil
      INTO lv_intco
      FROM zacctnew
      WHERE cocode  = s_coep-bukrs
        AND glacct  = s_coep-kstar
      .

    IF lv_intco = ''.
      SELECT SINGLE affil
        INTO lv_intco
        FROM zacctnew
        WHERE cocode  = ''
          AND glacct  = s_coep-kstar
        .
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.


    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.


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
