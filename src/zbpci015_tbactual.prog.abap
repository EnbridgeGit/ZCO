REPORT  zbpci015_tbactual MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for trial balances for               *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue Description                                 *
*2012/07/31 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
*                                                                      *
************************************************************************
TABLES: faglflext.

TYPES:  BEGIN OF ty_faglflext,
          ryear   LIKE faglflext-ryear,
          rldnr   LIKE faglflext-rldnr,
          racct   LIKE faglflext-racct,
          rbukrs  LIKE faglflext-rbukrs,
          hslvt   LIKE faglflext-hslvt,
          hsl01   LIKE faglflext-hsl01,
          hsl02   LIKE faglflext-hsl02,
          hsl03   LIKE faglflext-hsl03,
          hsl04   LIKE faglflext-hsl04,
          hsl05   LIKE faglflext-hsl05,
          hsl06   LIKE faglflext-hsl06,
          hsl07   LIKE faglflext-hsl07,
          hsl08   LIKE faglflext-hsl08,
          hsl09   LIKE faglflext-hsl09,
          hsl10   LIKE faglflext-hsl10,
          hsl11   LIKE faglflext-hsl11,
          hsl12   LIKE faglflext-hsl12,
          hsl13   LIKE faglflext-hsl13,
          hsl14   LIKE faglflext-hsl14,
          hsl15   LIKE faglflext-hsl15,
          hsl16   LIKE faglflext-hsl16,
        END OF ty_faglflext.

DATA: lv_local          TYPE integer,
      lv_int            TYPE integer,
      wa_faglflext      LIKE faglflext,
      s_faglflext       TYPE ty_faglflext,
      t_faglflext       LIKE TABLE OF s_faglflext,
      t_faglflext_sum   LIKE TABLE OF s_faglflext,
      st_datarec        TYPE string,
      t_data            LIKE TABLE OF st_datarec.


DATA: msg(80)           TYPE c,
      lv_account(6)    TYPE c,
      lv_category(10)   TYPE c,
      lv_datasrc(25)    TYPE c,
      lv_entity(12)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(10)    TYPE c,
      lv_ioswbs(22)     TYPE c,
      lv_division(11)   TYPE c,
      lv_iamount        LIKE faglflext-hsl01,
      lv_amount(20)     TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_ryear   LIKE faglflext-ryear OBLIGATORY,
p_period  LIKE faglflext-rpmax,
p_rldnr   LIKE faglflext-rldnr OBLIGATORY.

SELECT-OPTIONS:
s_racct   FOR faglflext-racct,
s_rbukrs  FOR faglflext-rbukrs.

SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\TBACTUAL', "TR995
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\TBACTUAL', "TR995
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile
            .


SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/finance/' INTO csvfile.


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

  CONCATENATE p_file  '-' p_ryear '-' p_period+1(2) '-' p_rldnr  '.csv' INTO p_file.

  CONCATENATE csvfile 'finance.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' p_ryear '-' p_period+1(2) '-TB' p_rldnr  '.csv' INTO csvfile.


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

  SELECT  ryear racct rldnr rbukrs  hslvt hsl01
          hsl02 hsl03 hsl04 hsl05   hsl06 hsl07
          hsl08 hsl09 hsl10 hsl11   hsl12 hsl13
          hsl14 hsl15 hsl16
    INTO CORRESPONDING FIELDS OF TABLE t_faglflext
    FROM faglflext
    WHERE ryear   =  p_ryear
      AND rldnr   =  p_rldnr
      AND racct   IN s_racct
      AND rbukrs  IN s_rbukrs
    .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_faglflext ASCENDING BY ryear racct rbukrs.

  LOOP AT t_faglflext INTO s_faglflext.
*Last sorted field is rbukrs
    AT END OF rbukrs.
      SUM.
      APPEND s_faglflext TO t_faglflext_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.


*Set the hard-coded values.

  lv_category     = 'Actual'.
  lv_rptcurrency  = 'LC'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.

  DATA: lv_period(10) TYPE c,
        lv_curcol(17)  TYPE c.

  FIELD-SYMBOLS <hslcol> TYPE ANY. "Name of current wkgperiod

  LOOP AT t_faglflext_sum INTO s_faglflext.
    CLEAR:  st_datarec, lv_account, lv_datasrc, lv_entity, lv_intco, lv_time,
            lv_amount,  lv_period, lv_curcol.

    SHIFT s_faglflext-racct LEFT DELETING LEADING '0'.
    lv_account = s_faglflext-racct.


    "lv_datasrc
    CASE s_faglflext-rldnr.
      WHEN '0L'.
        IF s_faglflext-rbukrs <> 'UGL'.
          IF s_faglflext-racct >= 100000 AND s_faglflext-racct <= 299999.
            lv_datasrc = 'SAP_TB0L_Affil'.
          else.
            lv_datasrc = 'YTD_SAP_TB0L_Affil'.
          endif.
        ELSE.
          IF s_faglflext-racct >= 100000 AND s_faglflext-racct <= 299999.
            lv_datasrc = 'SAP_TB0L_BS'.
          ELSEIF s_faglflext-racct >= 300000 AND s_faglflext-racct <= 302999.
            lv_datasrc = 'YTD_SAP_TB0L_RevCOG'.
          ELSEIF s_faglflext-racct >= 350000 AND s_faglflext-racct <= 399999.
            lv_datasrc = 'YTD_SAP_TB0L_RevCOG'.
          ELSEIF s_faglflext-racct >= 303000 AND s_faglflext-racct <= 349999.
            lv_datasrc = 'YTD_SAP_TB0L_Expense'.
          ELSEIF s_faglflext-racct >= 400000 AND s_faglflext-racct <= 999999.
            lv_datasrc = 'YTD_SAP_TB0L_Expense'.
          ENDIF.
        ENDIF.
      WHEN 'NL'.
        IF s_faglflext-rbukrs <> 'UGL'.
          IF s_faglflext-racct >= 100000 AND s_faglflext-racct <= 299999.
            lv_datasrc = 'SAP_TBNL_Affil'.
          else.
            lv_datasrc = 'YTD_SAP_TBNL_Affil'.
          endif.
        ELSE.
          IF s_faglflext-racct >= 100000 AND s_faglflext-racct <= 299999.
            lv_datasrc = 'SAP_TBNL_BS'.
          ELSEIF s_faglflext-racct >= 300000 AND s_faglflext-racct <= 302999.
            lv_datasrc = 'YTD_SAP_TBNL_RevCOG'.
          ELSEIF s_faglflext-racct >= 350000 AND s_faglflext-racct <= 399999.
            lv_datasrc = 'YTD_SAP_TBNL_RevCOG'.
          ELSEIF s_faglflext-racct >= 303000 AND s_faglflext-racct <= 349999.
            lv_datasrc = 'YTD_SAP_TBNL_Expense'.
          ELSEIF s_faglflext-racct >= 400000 AND s_faglflext-racct <= 999999.
            lv_datasrc = 'YTD_SAP_TBNL_Expense'.
          ENDIF.
        ENDIF.
    ENDCASE.


    lv_entity = s_faglflext-rbukrs.


    lv_int = STRLEN( s_faglflext-racct ).

    WHILE lv_int < 10.
      CONCATENATE '0' s_faglflext-racct INTO s_faglflext-racct.
      lv_int = lv_int + 1.
    ENDWHILE.


    "lv_IntCo
    SELECT SINGLE affil
            INTO lv_intco
            FROM zacctnew
            WHERE cocode  = s_faglflext-rbukrs
              AND glacct  = s_faglflext-racct
            .

    IF lv_intco = ''.
      SELECT SINGLE affil
              INTO lv_intco
              FROM zacctnew
              WHERE cocode  = ''
                AND glacct  = s_faglflext-racct
              .
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.


    CONCATENATE s_faglflext-ryear p_period+1(2) '00' INTO lv_time.


    "Loop through all the periods up to 16.
    lv_iamount = 0.
    DO 16 TIMES.
      IF sy-index > p_period.
        EXIT.
      ENDIF.

      lv_period = sy-index.
      SHIFT lv_period LEFT DELETING LEADING ' '.
      IF sy-index < 10.
        "Always have 2 characters.
        CONCATENATE '0' lv_period INTO lv_period.
      ENDIF.

      CONCATENATE 's_faglflext-HSL' lv_period INTO lv_curcol.

      ASSIGN (lv_curcol) TO <hslcol>.
      ADD <hslcol> TO lv_iamount.
    ENDDO.

    "Add the carried forward balance.
    ADD s_faglflext-hslvt TO lv_iamount.


    IF lv_iamount < 0.
      "If negative convert to positive and add '-' infront.
      lv_amount = lv_iamount * -1.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = lv_iamount.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.


    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time
                lv_amount
                INTO st_datarec SEPARATED BY delimtr.

    IF lv_iamount <> 0.
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
