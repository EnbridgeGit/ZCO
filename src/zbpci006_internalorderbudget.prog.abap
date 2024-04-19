REPORT  zbpci006_internalorderbudget MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for internal order budget for        *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue Description                                 *
*2012/07/31 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
*                                                                      *
*2013/12/10 M Khan SDP57444 Change logic for Category and file name    *
*                           depending upon the version #.              *
*                                                                      *
************************************************************************
TABLES: coej.

TYPES:  BEGIN OF ty_coej,
          objnr   LIKE coej-objnr,
          gjahr   LIKE coej-gjahr,
          kstar   LIKE coep-kstar,
          wrttp   LIKE coej-wrttp,
          versn   LIKE coej-versn,
          wog001  LIKE coej-wog001,
          wog002  LIKE coej-wog002,
          wog003  LIKE coej-wog003,
          wog004  LIKE coej-wog004,
          wog005  LIKE coej-wog005,
          wog006  LIKE coej-wog006,
          wog007  LIKE coej-wog007,
          wog008  LIKE coej-wog008,
          wog009  LIKE coej-wog009,
          wog010  LIKE coej-wog010,
          wog011  LIKE coej-wog011,
          wog012  LIKE coej-wog012,
        END OF ty_coej.

DATA: lv_local    TYPE integer,
      wa_coej     LIKE coej,
      s_coej      TYPE ty_coej,
      t_coej      LIKE TABLE OF s_coej,
      t_coej_sum  LIKE TABLE OF s_coej,
      st_datarec  TYPE string,
      t_data      LIKE TABLE OF st_datarec.


DATA: msg(80)           TYPE c,
      lv_account(6)     TYPE c,
      lv_category(10)   TYPE c,
      lv_datasrc(6)     TYPE c,
      lv_entity(12)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(10)    TYPE c,
      lv_ioswbs(22)     TYPE c,
      lv_division(11)   TYPE c,
      lv_amount(20)     TYPE c.


DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


SELECT-OPTIONS:
s_objnr   FOR coej-objnr OBLIGATORY
          DEFAULT 'OR0000000000000000' TO 'OR9999999999999999'.
PARAMETERS:
p_gjahr   LIKE coej-gjahr OBLIGATORY.

SELECT-OPTIONS:
s_wrttp   FOR coej-wrttp OBLIGATORY DEFAULT 1,
s_kstar   FOR coej-kstar.
*s_versn   FOR coej-versn OBLIGATORY DEFAULT 0.     "SDP57444
PARAMETERS:                                         "SDP57444
p_versn   LIKE coej-versn OBLIGATORY DEFAULT 0.     "SDP57444
SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\IOBudget.csv', "TR995
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\IOBudget.csv', "TR995
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
*End of TR995 changes*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'capex.tch' INTO tuchfile.
  IF p_versn = '000'.                                                 "SDP57444
     CONCATENATE csvfile 'SAP-' p_gjahr '-IO-Bud.csv' INTO csvfile.
  ELSE.                                                               "SDP57444
     CONCATENATE csvfile 'SAP-' p_gjahr '-IO-OMv' p_versn             "SDP57444
                                      '.csv' INTO csvfile.            "SDP57444
  ENDIF.                                                              "SDP57444

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

  SELECT objnr gjahr wrttp kstar versn
         wog001 wog002 wog003 wog004 wog005 wog006
         wog007 wog008 wog009 wog010 wog011 wog012
    INTO CORRESPONDING FIELDS OF TABLE t_coej
    FROM coej
    WHERE objnr IN s_objnr
      AND gjahr =  p_gjahr
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn EQ p_versn          "SDP57444
    .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_coej ASCENDING BY gjahr objnr kstar.

  LOOP AT t_coej INTO s_coej.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coej TO t_coej_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.


*Set the hard-coded values.

  lv_datasrc      = 'SAP_OM'.
  lv_rptcurrency  = 'LC'.
  lv_project      = 'No_Project'.
  lv_division     = 'No_Division'.


  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  DATA: lv_period(10) TYPE c,
        lv_curcol(13)  TYPE c.

  FIELD-SYMBOLS <wogcol> TYPE ANY. "Name of current WOGperiod

  LOOP AT t_coej_sum INTO s_coej.
    CLEAR:  st_datarec, lv_account, lv_category, lv_entity, lv_intco, lv_time,
            lv_ioswbs, lv_amount,
            lv_period, lv_curcol.

    lv_account = s_coej-kstar+4.

    IF p_versn = '000'.                                    "SDP57444
       CONCATENATE 'Budget' s_coej-gjahr INTO lv_category.
    ELSE.                                                  "SDP57444
       CONCATENATE 'OMv' p_versn INTO lv_category.         "SDP57444
    ENDIF.                                                 "SDP57444

*Fields moved below for loop.

    SELECT SINGLE bukrs
      INTO lv_entity
      FROM aufk
      WHERE aufnr = s_coej-objnr+2
      .



*lv_entity is like coej-bukrs *if it existed....

    SELECT SINGLE affil
  INTO lv_intco
  FROM zacctnew
  WHERE cocode  = lv_entity
    AND glacct  = s_coej-kstar
  .

    IF lv_intco = ''.
      SELECT SINGLE affil
              INTO lv_intco
              FROM zacctnew
              WHERE cocode  = ''
                AND glacct  = s_coej-kstar
              .
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.


    lv_ioswbs = s_coej-objnr+2.
    SHIFT lv_ioswbs LEFT DELETING LEADING '0'.
    CONCATENATE 'IO_' lv_ioswbs INTO lv_ioswbs.


    DO 12 TIMES.
      CLEAR:  st_datarec.
      lv_period = sy-index.
      SHIFT lv_period LEFT DELETING LEADING ' '.

      IF sy-index < 10.
*Always have 2 characters.
        CONCATENATE '0' lv_period INTO lv_period.
      ENDIF.

      CONCATENATE s_coej-gjahr lv_period '00' INTO lv_time.

      CONCATENATE 's_coej-WOG0' lv_period INTO lv_curcol.

      ASSIGN (lv_curcol) TO <wogcol>.

      IF <wogcol> < 0.
*If negative convert to positive and add '-' infront.
        <wogcol> = <wogcol> * -1.
        lv_amount = <wogcol>.
        SHIFT lv_amount LEFT DELETING LEADING ' '.
        CONCATENATE '-' lv_amount INTO lv_amount.
      ELSE.
        lv_amount = <wogcol>.
        SHIFT lv_amount LEFT DELETING LEADING ' '.
      ENDIF.


      CONCATENATE lv_account lv_category lv_datasrc lv_entity
                  lv_intco lv_rptcurrency lv_time lv_project lv_ioswbs lv_division
                  lv_amount
                  INTO st_datarec SEPARATED BY delimtr.

      IF <wogcol> <> 0.
        APPEND st_datarec TO t_data.
      ENDIF.

    ENDDO.

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
