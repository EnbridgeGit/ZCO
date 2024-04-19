REPORT  zccor002_mthlyskfposting MESSAGE-ID zs.

************************************************************************
*  Client:    Spectra Energy.                                          *
*  Author:    Brian Boundy                                             *
*  Date:      January, 2011.                                           *
*  Track #:   TR804.                                                   *
*                                                                      *
*  Description:                                                        *
*     - The purpose of this program is to create a BDC session for     *
*       posting skf  from the EXCEL sheet using                        *
*       bapi_acc_stat_key_fig_post                                     *
*                                                                      *
************************************************************************
* ---------------------- CHANGE LOG -----------------------------------*
*Date       By       Issue Description                                 *
*2012/07/31 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
*                                                                      *
************************************************************************
*Formated Input record
TYPES:
  BEGIN OF ty_header,
    docdate     LIKE bapidochdrp-docdate,
    postgdate   LIKE bapidochdrp-postgdate,
    doc_hdr_tx  LIKE bapidochdrp-doc_hdr_tx,
  END OF ty_header,

  BEGIN OF ty_line,
    rec_cctr    LIKE bapiskfitm-rec_cctr,
    statkeyfig  LIKE bapiskfitm-statkeyfig,
    stat_qty    LIKE bapiskfitm-stat_qty,
    seg_text    LIKE bapiskfitm-seg_text,
  END OF ty_line.


DATA:  BEGIN OF exceltab OCCURS 0.
        INCLUDE STRUCTURE kcde_cells.
DATA:  END OF exceltab.

* Working Data
DATA: s_header    TYPE ty_header,
      s_line      TYPE ty_line,
      t_line      LIKE TABLE OF s_line,
      bapi_header LIKE bapidochdrp,
      bapi_item   LIKE bapiskfitm,
      bapi_lines  LIKE TABLE OF bapi_item,
      bapi_rc     LIKE bapiret2,
      bapi_return LIKE TABLE OF bapi_rc,
      lv_error(1) TYPE c,
      lv_count    TYPE i.

CONSTANTS: c_num(10) TYPE c VALUE '0123456789'.

*======================================================================*
*              SELECTION SCREEN                                        *
*======================================================================*
*
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
PARAMETERS:  p_kokrs   LIKE coepr-kokrs     OBLIGATORY DEFAULT '10',
             p_filein  LIKE rlgrap-filename OBLIGATORY DEFAULT
                        'H:\saptemp\SKFPOSTINGS.xlsx'. "TR995
*                       'C:\saptemp\SKFPOSTINGS.xlsx'. "TR995
SELECTION-SCREEN END OF BLOCK box.

*======================================================================*
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filein.
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
      p_filein = wit_filename_tab.
    ELSE.
      CLEAR p_filein.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_filein.
  PERFORM check_file_path.
*End of TR995 changes                                   *
*======================================================================*

*======================================================================*
*              Main Processing Block                                   *
*======================================================================*

START-OF-SELECTION.

  PERFORM upload_exce_to_internal_tab.
  IF t_line IS INITIAL.
    STOP.
  ELSE.
    PERFORM data_checks.
    IF NOT lv_error = 'X'.
      PERFORM create_bapi.
    ENDIF.
  ENDIF.
*======================================================================*
*              Upload EXCEL Data                                       *
*======================================================================*
FORM upload_exce_to_internal_tab.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = p_filein
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 32
      i_end_row               = 999
    TABLES
      intern                  = exceltab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline = '!! ERROR !!'
        text1    = 'Unsuccessful EXCEL Upload '
        text2    = 'Please check the file path/name and try again'
        text3    = ' '
        text4    = 'Press OK Button to Continue'
        button_1 = 'OK'.
    STOP.
  ENDIF.
  CLEAR: s_header,
         s_line, t_line.
  LOOP AT exceltab.
    IF exceltab-row < 5.
      IF exceltab-col = 2.
        CASE exceltab-row.
          WHEN 1. MOVE exceltab-value TO s_header-docdate.
          WHEN 2. MOVE exceltab-value TO s_header-postgdate.
          WHEN 3. MOVE exceltab-value TO s_header-doc_hdr_tx.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      CONTINUE.
    ENDIF.
    CASE exceltab-col.
      WHEN 1.  MOVE exceltab-value TO s_line-rec_cctr.
      WHEN 2.  MOVE exceltab-value TO s_line-statkeyfig.
      WHEN 3.  MOVE exceltab-value TO s_line-stat_qty.
      WHEN 4.  MOVE exceltab-value TO s_line-seg_text.
      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      APPEND s_line TO t_line.
      CLEAR  s_line.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "UPLOAD_EXCE_TO_INTERNAL_TAB
*======================================================================*
*              Perform Data Check                                      *
*======================================================================*
FORM data_checks.

  lv_error = ' '.

  IF NOT s_header-docdate CO c_num.
    WRITE: 'Date format must be: YYYYMMDD'.
    lv_error = 'X'.
  ENDIF.

  IF NOT s_header-postgdate CO c_num.
    WRITE: 'Date format must be: YYYYMMDD'.
    lv_error = 'X'.
  ENDIF.

  lv_count = strlen( s_header-docdate ).

  IF NOT lv_count = 8.
    WRITE: 'Date format must be: YYYYMMDD'.
    lv_error = 'X'.
  ENDIF.

  lv_count = strlen( s_header-postgdate ).

  IF NOT lv_count = 8.
    WRITE: 'Date format must be: YYYYMMDD'.
    lv_error = 'X'.
  ENDIF.

ENDFORM.                    "data_checks
*======================================================================*
*              Create Bapi                                             *
*======================================================================*
FORM create_bapi.

  CLEAR: bapi_header, bapi_lines, bapi_return.

  DATA: lv_count    TYPE i,
        lc_flg(1)   TYPE c.

  CONSTANTS c_colen TYPE i VALUE 10.

  bapi_header-co_area     = p_kokrs.
  bapi_header-docdate     = s_header-docdate.
  bapi_header-postgdate   = s_header-postgdate.
  bapi_header-doc_hdr_tx  = s_header-doc_hdr_tx.
  bapi_header-username    = sy-uname.

  LOOP AT t_line INTO s_line.
    CLEAR bapi_item.
    lv_count = strlen( s_line-rec_cctr ).
    WHILE lv_count < c_colen.
      CONCATENATE '0' s_line-rec_cctr INTO s_line-rec_cctr.
      lv_count = lv_count + 1.
    ENDWHILE.
    bapi_item-statkeyfig  = s_line-statkeyfig.
    bapi_item-stat_qty    = s_line-stat_qty.
    bapi_item-seg_text    = s_line-seg_text.
    bapi_item-rec_cctr    = s_line-rec_cctr.
    APPEND bapi_item TO bapi_lines.
  ENDLOOP.
  CALL FUNCTION 'BAPI_ACC_STAT_KEY_FIG_POST'
    EXPORTING
      doc_header      = bapi_header
      ignore_warnings = 'X'
    TABLES
      doc_items       = bapi_lines
      return          = bapi_return.
  IF bapi_return[] IS NOT INITIAL.
    WRITE: / 'Return Log from BAPI'.
    SKIP 2.
  ENDIF.
  LOOP AT bapi_return INTO bapi_rc.
*    WRITE: bapi_rc-message.
    IF bapi_rc-type = 'A' OR bapi_rc-type = 'E'.
      lc_flg = 'X'.
    ENDIF.
    WRITE: / bapi_rc-type,
             bapi_rc-id,
             bapi_rc-number,
             bapi_rc-message.
  ENDLOOP.
  IF lc_flg = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

ENDFORM.                    "create_bapi

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
      full_name     = p_filein
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
*  Check if directory path exist or not.
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
