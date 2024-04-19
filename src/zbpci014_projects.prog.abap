REPORT  zbpci014_projects MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       December 2010                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract data for newly created or RECO'd projects *
*  for the BPC application.                                            *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       By       Issue Description                                 *
*2012/07/31 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 17-APR-2019  KBANERJEE   D30K929782   ENHC0025213 - Asset category   *
*                                       addition to Parenth2 field in  *
*                                       dimension file output          *
* 08-MAY-2019  KBANERJEE   D30K929817   QA Defect changes              *
* 09-MAY-2019  KBANERJEE   D30K929819   Add asset description          *
************************************************************************
TYPES:  ty_prps TYPE prps,
        BEGIN OF ty_jcds,
          objnr       LIKE jcds-objnr,
          stat        LIKE jcds-stat,
          inact       LIKE jcds-inact,
          udate       LIKE jcds-udate,
        END OF ty_jcds,

        BEGIN OF ty_project,
          pbukr       LIKE prps-pbukr,
          stat        LIKE jcds-stat,
          projobjnr   LIKE proj-objnr,
          objnr       LIKE prps-objnr,
          prart       LIKE prps-prart,
          pspri       LIKE prps-pspri,
          psphi       LIKE prps-psphi,
          pspid       LIKE proj-pspid,
          posid       LIKE prps-posid,
          post1       LIKE proj-post1,
          atwrt       LIKE ausp-atwrt,
          udate       LIKE jcds-udate,
        END OF ty_project,
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
        BEGIN OF gty_wbs,
         pspnr TYPE ps_posnr,
         objnr TYPE j_objnr ,
         psphi TYPE ps_psphi,
         stufe TYPE ps_stufe,
         objek TYPE objnum,
       END OF gty_wbs,
       BEGIN OF gty_asset_cat,
         objek TYPE objnum,
         atinn TYPE atinn,
         atzhl TYPE atzhl,
         klart TYPE klassenart,
         adzhl TYPE adzhl,
         atwrt TYPE atwrt,
       END OF gty_asset_cat,
         BEGIN OF gty_asset_desc,
         atinn TYPE atinn,
         atzhl TYPE atzhl,
         adzhl TYPE adzhl,
         atwtb TYPE atwtb,
       END OF gty_asset_desc.

TYPES:tt_asset_desc TYPE STANDARD TABLE OF gty_asset_desc
                    INITIAL SIZE 0,
      tt_asset_cat  TYPE STANDARD TABLE OF gty_asset_cat
                    INITIAL SIZE 0,
      tt_wbs        TYPE STANDARD TABLE OF gty_wbs
                    INITIAL SIZE 0.
DATA:gt_asset_desc  TYPE STANDARD TABLE OF gty_asset_desc
                    INITIAL SIZE 0,
     gt_asset_cat   TYPE STANDARD TABLE OF gty_asset_cat
                    INITIAL SIZE 0,
     gt_wbs         TYPE STANDARD TABLE OF gty_wbs
                    INITIAL SIZE 0.
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
DATA: lv_local      TYPE boolean,
      lv_new        TYPE boolean,
      lv_teco       TYPE boolean,
      lv_date       LIKE jcds-udate,
      msg(80)       TYPE c,
      s_jcds        TYPE ty_jcds,
      t_jcds        LIKE TABLE OF s_jcds,
      s_prps        TYPE ty_prps,
      t_prps        LIKE TABLE OF s_prps,
      s_project     TYPE ty_project,
      t_project     LIKE TABLE OF s_project,
      st_dimrec     TYPE string,
      t_dim         LIKE TABLE OF st_dimrec,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.

DATA: lv_account(10)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(13)    TYPE c,
      lv_entity(22)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_project(9)     TYPE c,
      lv_ioswbs(6)      TYPE c,
      lv_division(6)    TYPE c,
      lv_amount(8)      TYPE c,
      lv_prj_blanket(1) TYPE c.


DATA: lv_id(24)     TYPE c,
      lv_newid(1)   TYPE c,
      lv_evdesc(40) TYPE c,
      lv_h1(25)     TYPE c,
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
      "lv_h2(1)      TYPE c,
      lv_h2         TYPE atwtb,
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
      lv_h3(34)     TYPE c,
      lv_scaling(1) TYPE c,
      lv_div(3)     TYPE c,
      lv_type(3)    TYPE c,
      lv_prior(1)   TYPE c,
      lv_year(4)    TYPE c.


DATA: tuchfile     LIKE       rfpdo-rfbifile,
      lv_sfile     LIKE       rfpdo-rfbifile,
      lv_lfile     TYPE       string,
      lv_premonths LIKE       sy-datum,
      lv_premonthe LIKE       sy-datum.

CONSTANTS: delimtr  TYPE c VALUE ',',
           true     TYPE i VALUE 1,
           false    TYPE i VALUE 0.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

SELECT-OPTIONS  s_date      FOR lv_date OBLIGATORY NO-EXTENSION.

PARAMETERS:     p_new       RADIOBUTTON GROUP rad1 DEFAULT 'X',
                p_teco      RADIOBUTTON GROUP rad1.

SELECTION-SCREEN SKIP 1.

PARAMETERS:     p_tecom     RADIOBUTTON GROUP rad1,
                p_period(2) TYPE c,
                p_year(4)   TYPE c.


SELECTION-SCREEN END OF BLOCK a1.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local     RADIOBUTTON GROUP rad2 DEFAULT 'X',
*           p_lfile     TYPE        string DEFAULT 'H:\SAPTEMP\Projects', "TR995
            p_lfile     TYPE        string DEFAULT 'H:\SAPTEMP\Projects', "TR995
            p_server    RADIOBUTTON GROUP rad2,
            p_sfile     LIKE        rfpdo-rfbifile.



SELECTION-SCREEN END OF BLOCK b1.


*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/capex/' INTO p_sfile.

  IF s_date[] IS INITIAL.
    s_date-low = sy-datum - 1.
    APPEND s_date.
  ENDIF.

  CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
    EXPORTING
      i_date_old = sy-datum
    IMPORTING
      e_date_new = lv_premonthe.

  p_period  = lv_premonthe+4(2).
  p_year    = lv_premonthe+0(4).



*************************************************************************
*Start Of TR995 changes
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
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
      p_lfile = wit_filename_tab.
    ELSE.
      CLEAR p_lfile.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_lfile.
  IF p_local = 'X'.
    PERFORM check_file_path.
  ENDIF.
*End of TR995 changes
*************************************************************************
START-OF-SELECTION.
  CONCATENATE p_sfile 'capex.tch' INTO tuchfile.
  CONCATENATE p_sfile 'SAP-Project' INTO p_sfile.

  IF p_local = 'X'.
    lv_local = true.
  ELSE.
    lv_local = false.
  ENDIF.


  IF p_new = 'X'.
    lv_teco     = false.
    lv_new      = true.
    CONCATENATE p_lfile 'ND-' s_date-low '.csv' INTO lv_lfile.
    CONCATENATE p_sfile 'ND-' s_date-low '.csv' INTO lv_sfile.
    CONCATENATE p_lfile 'N-' s_date-low '.csv' INTO p_lfile.
    CONCATENATE p_sfile 'N-' s_date-low '.csv' INTO p_sfile.
  ELSEIF p_teco = 'X'.
    lv_teco     = true.
    lv_new      = false.
    CONCATENATE p_lfile 'C-' s_date-low '.csv' INTO p_lfile.
    CONCATENATE p_sfile 'C-' s_date-low '.csv' INTO p_sfile.
  ELSEIF p_tecom = 'X'.
    lv_teco     = true.
    lv_new      = false.
    CONCATENATE p_lfile 'C-' p_year p_period '01' '.csv' INTO p_lfile.
    CONCATENATE p_sfile 'C-' p_year p_period '01' '.csv' INTO p_sfile.
  ENDIF.

  "If monthly manualy populate the selection variant.
  IF p_tecom = 'X'.
    "Clear table and header.
    REFRESH s_date.
    CLEAR s_date.


    CONCATENATE p_year p_period '01' INTO lv_premonths.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_premonths
      IMPORTING
        last_day_of_month = lv_premonthe.

    s_date-sign   = 'I'.
    s_date-option = 'BT'.
    s_date-low    = lv_premonths.
    s_date-high   = lv_premonthe.

    APPEND s_date.

  ENDIF.

  PERFORM get_jcds_data.

  IF lv_new = true.
    PERFORM get_prps_data.
    PERFORM sumarize_data.
    PERFORM get_proj_data.
    PERFORM get_ausp_data.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
    PERFORM f_get_asset_cat CHANGING gt_wbs gt_asset_cat gt_asset_desc.
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
    PERFORM print_created_report.
    IF lv_local = 0.
      PERFORM create_touch_file.
    ENDIF.
  ENDIF.

  IF lv_teco = true.
    PERFORM get_proj_number.
    PERFORM print_teco_report.
    IF lv_local = 0.
      PERFORM create_touch_file.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
FORM get_jcds_data.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 1
      text       = 'Getting Changes to Projects'.

  CLEAR t_jcds.

  IF lv_new = true.
    SELECT objnr stat inact udate
      INTO CORRESPONDING FIELDS OF TABLE t_jcds
      FROM jcds
      WHERE objnr LIKE 'PR%'
        AND stat  = 'I0001'
        AND chgnr = '001'
       "AND inact = ''
        AND udate IN s_date
      .

  ELSEIF lv_teco = true.
    "TECO and Closed
    SELECT objnr stat udate
      INTO CORRESPONDING FIELDS OF TABLE t_jcds
      FROM jcds
      WHERE objnr LIKE 'PR%'
        AND ( stat  = 'I0045' OR stat  = 'I0046' )
        AND chgnr = '001'
        AND udate IN s_date
      .
  ENDIF.

ENDFORM.                    "get_jcds_data


*----------------------------------------------------------------------*
FORM get_prps_data.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Getting Project Info'.

  CLEAR t_project.

  SELECT objnr prart posid psphi pspri pbukr
    INTO CORRESPONDING FIELDS OF TABLE t_prps
    FROM prps.

  SORT t_prps ASCENDING BY objnr.


  LOOP AT t_jcds INTO s_jcds.
    CLEAR s_prps.
    READ TABLE t_prps INTO s_prps
              WITH KEY objnr = s_jcds-objnr.

    s_project-prart = s_prps-prart.
    s_project-psphi = s_prps-psphi.
    s_project-pspri = s_prps-pspri.

    s_project-udate = s_jcds-udate.
    s_project-pbukr = s_prps-pbukr.
    s_project-stat  = s_jcds-stat.
    s_project-posid = s_prps-posid.
    s_project-objnr = s_jcds-objnr.

    CONCATENATE 'PD' s_project-psphi INTO s_project-projobjnr.
    REPLACE ALL OCCURRENCES OF '-' IN s_project-posid WITH ''.
    APPEND s_project TO t_project.
  ENDLOOP.

ENDFORM.                    "get_prps_data


*----------------------------------------------------------------------*
FORM get_proj_number.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Getting Project Info'.

  CLEAR t_project.
  SELECT objnr psphi pbukr posid
    INTO CORRESPONDING FIELDS OF TABLE t_prps
    FROM prps.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Sorting Data'.

  SORT t_prps ASCENDING BY objnr.

  LOOP AT t_jcds INTO s_jcds.
    CLEAR s_prps.
    READ TABLE t_prps INTO s_prps
              WITH KEY objnr = s_jcds-objnr.

    s_project-psphi = s_prps-psphi.
    s_project-udate = s_jcds-udate.
    s_project-pbukr = s_prps-pbukr.
    s_project-stat  = s_jcds-stat.
    s_project-posid = s_prps-posid.

    CONCATENATE 'PD' s_project-psphi INTO s_project-projobjnr.
    s_project-objnr = s_jcds-objnr.
    REPLACE ALL OCCURRENCES OF '-' IN s_project-posid WITH ''.
    APPEND s_project TO t_project.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 60
      text       = 'Getting Additional Data'.

  LOOP AT t_project INTO s_project.
    IF s_project-posid+7(4) <> 0.
      DELETE TABLE t_project FROM s_project.
    ELSE.
      SELECT SINGLE pspid
        INTO CORRESPONDING FIELDS OF s_project
        FROM proj
        WHERE objnr = s_project-projobjnr
      .
      MODIFY t_project FROM s_project.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Getting Additional Data'.

*  LOOP AT t_project INTO s_project.
*    IF s_project-stat = 'I0046'.
*      s_project-stat = 'I0045'.
*      APPEND s_project TO t_project.
*    ENDIF.
*  ENDLOOP.


  SORT t_project BY stat pbukr posid udate.
  DELETE ADJACENT DUPLICATES FROM t_project COMPARING stat pbukr posid.

ENDFORM.                    "get_proj_number


*----------------------------------------------------------------------*
FORM sumarize_data.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Sorting Data'.

  LOOP AT t_project INTO s_project.
    IF s_project-posid+7(4) <> 0.
      DELETE TABLE t_project FROM s_project.
    ENDIF.
  ENDLOOP.

  SORT t_project BY projobjnr pspri prart psphi.
  DELETE ADJACENT DUPLICATES FROM t_project COMPARING ALL FIELDS.
ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM get_proj_data.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 60
      text       = 'Getting Additional Data'.

  LOOP AT t_project INTO s_project.
    SELECT SINGLE pspid post1
      INTO CORRESPONDING FIELDS OF s_project
      FROM proj
      WHERE objnr = s_project-projobjnr
    .

    "Remove , from post1 description.
    REPLACE ALL OCCURRENCES OF ',' IN s_project-post1 WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN s_project-pspid WITH ''.

    MODIFY t_project FROM s_project.
  ENDLOOP.

ENDFORM.                    "get_proj_data



*----------------------------------------------------------------------*
FORM get_ausp_data.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Getting Additional Data'.

  LOOP AT t_project INTO s_project.
    SELECT SINGLE atwrt
      INTO CORRESPONDING FIELDS OF s_project
      FROM ausp
      WHERE objek = s_project-objnr
        AND atinn = '0000000631'
        AND klart = '014'
    .

    MODIFY t_project FROM s_project.
  ENDLOOP.
ENDFORM.                    "get_ausp_data



*----------------------------------------------------------------------*
FORM print_created_report.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
  DATA: ls_wbs         TYPE gty_wbs,
        ls_asset_cat   TYPE gty_asset_cat,
        ls_asset_desc  TYPE gty_asset_desc.
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Generating Report'.

  CONCATENATE   text-001 text-002 text-003 text-004 text-005 text-006
                text-007 text-008 text-009 text-010 text-024 text-025

                INTO st_dimrec SEPARATED BY delimtr.

  APPEND st_dimrec TO t_dim.


  CONCATENATE   text-013 text-014 text-015 text-016 text-017 text-018
               text-019 text-020 text-021 text-022 text-023

               INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.



  lv_newid    = ''.
  lv_h2       = ''.
  lv_scaling  = ''.


  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_PrjStatus'.
  lv_intco        = 'No_Intco'.
  lv_rptcurrency  = 'LC'.
  lv_ioswbs       = 'No_WBS'.

  lv_account = 'Prj_Closed'.
  lv_amount  = '1'.

  LOOP AT t_project INTO s_project.

    IF s_project-pspid CN '0123456789 '.
      CONTINUE.
    ENDIF.

    lv_evdesc = s_project-post1.
    CONCATENATE '''' s_project-pspid(2) INTO lv_div.
    CONCATENATE '''' s_project-prart INTO lv_type.
    lv_prior  = s_project-pspri.

    CONCATENATE s_project-pspid(2) '.' s_project-pspid+2(2) '.' s_project-pspid+4(3) INTO lv_id.


    "Long list of exceptions for H1 Text
    CONCATENATE 'T' s_project-prart '_Prty_' s_project-pspri INTO lv_h1.

    "Type = prart
    "Priority = pspri
    "Division = pspid+4(3)


    IF s_project-pspid+4(3) = '064'.
      lv_h1 = 'NBScat_MRI_Inst'.

    ELSEIF s_project-pspri = '6'.
      IF s_project-pspid+4(3) = '37'.
        lv_h1 = 'Prj_Exp_Other'.
      ELSE.
        lv_h1 = 'Prj_Exp_Other'.
      ENDIF.

    ELSEIF s_project-pspid(2) = '49'.
      IF s_project-pspid+4(3) < 500.
        lv_h1 = 'Prj_IT_ISMisc'.
      ELSEIF s_project-pspid+4(3) < 700.
        lv_h1 = 'Prj_IT_DmdMgmt'.
      ELSEIF s_project-pspid+4(3) < 900.
        lv_h1 = 'Prj_IT_ISMisc'.
      ELSE.
        lv_h1 = 'Prj_IT_DmdMgmt'.
      ENDIF.
    ELSEIF s_project-pspid(2) = '52'.
      lv_h1 = 'Prj_IT_GasSupply'.
    ELSEIF s_project-pspid(2) = '79'.
      lv_h1 = 'Prj_IT_InfoTech'.

    ELSEIF s_project-prart = '1' OR s_project-prart = '01'.
      IF s_project-pspri = '2'.
        CASE s_project-pspid+4(3).
          WHEN '005' OR '010' OR '011' OR '012' OR '013' OR '014' OR '015'
                     OR '020' OR '030' OR '034' OR '040' OR '042' OR '046'.
            lv_h1 = 'NBScat_Services'.
          WHEN '008'.
            lv_h1 = 'NBScat_Additions'.
          WHEN '044'.
            lv_h1 = 'NBScat_Services'.
          WHEN '052'.
            lv_h1 = 'NBScat_LandRights'.
          WHEN '060'.
            lv_h1 = 'NBScat_MR_Purch'.
          WHEN '062'.
            lv_h1 = 'NBScat_MRI_Inst'.
          WHEN '068' OR '073'.
            lv_h1 = 'NBScat_Indirect'.
          WHEN '070'.
            lv_h1 = 'NBScat_OH'.
          WHEN '071'.
            lv_h1 = 'NBScat_Insp'.
          WHEN '072' OR '078'.
            lv_h1 = 'NBScat_Cleanup'.
          WHEN '304'.
            lv_h1 = 'NBScat_Prop'.
        ENDCASE.

      ELSEIF s_project-pspri = '7'.
        CASE s_project-pspid+4(3).
          WHEN '061'.
            lv_h1 = 'Repl_MR'.
          WHEN '074'.
            lv_h1 = 'Repl_OH'.
          WHEN '055' OR '026'.
            lv_h1 = 'Repl_Other'.
          WHEN '007'.
            lv_h1 = 'Repl_SMC'.
          WHEN '009'.
            lv_h1 = 'Repl_General'.
          WHEN '063'.
            lv_h1 = 'Repl_MRI'.
          WHEN '077'.
            lv_h1 = 'Repl_Indirect'.
          WHEN '053'.
            lv_h1 = 'Repl_LandRights'.
          WHEN '065'.
            lv_h1 = 'Repl_MRI'.
          WHEN '075'.
            lv_h1 = 'Repl_SiteVisits'.
          WHEN '076'.
            lv_h1 = 'Repl_Cleanup'.
          WHEN OTHERS.
            "Check if it is 700 series
            IF s_project-pspid+4(1) = '7'.
              lv_h1 = 'Repl_SMC'.
            ELSE.
              lv_h1 = 'Repl_Other'.
            ENDIF.
        ENDCASE.

      ENDIF.

    ELSEIF s_project-prart = '7' OR s_project-prart = '07'.
      IF s_project-pspri = '5'.
        CASE s_project-pspid(2).
          WHEN '54'.
            lv_h1 = 'Prj_Dist_Oth_Ovhd'.
        ENDCASE.
      ENDIF.



    ELSE.
      "CONCATENATE 'T' s_project-prart '_Prty_' s_project-pspri INTO lv_h1.
    ENDIF.




    IF s_project-atwrt <> ''.
      CONCATENATE 'PFA_' s_project-atwrt INTO lv_h3.
    ELSE.
      lv_h3 = 'PFA_unassigned'.
    ENDIF.



    CONCATENATE '20' s_project-pspid+2(2) INTO lv_year.




    "Data Record
    lv_entity = s_project-pbukr.
    CONCATENATE s_project-udate(6) '00' INTO lv_time.
    CONCATENATE s_project-pspid(2) '.' s_project-pspid+2(2) '.' s_project-pspid+4(3) INTO lv_project.
    CONCATENATE 'DIV_' s_project-pspid(2) INTO lv_division.



    "Set project blanket field
    IF s_project-pspid+4(3) < 200.
      lv_prj_blanket = 'Y'.
    ELSE.
      lv_prj_blanket = ''.
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
*Print Parenth2 data - Insert Asset category for level1 WBS
    READ TABLE gt_wbs INTO ls_wbs
                      WITH KEY psphi = s_project-psphi
                      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_asset_cat INTO ls_asset_cat
                              WITH KEY objek = ls_wbs-objek
                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE gt_asset_desc INTO ls_asset_desc
                                 WITH KEY atinn = ls_asset_cat-atinn
                                          atzhl = ls_asset_cat-atzhl
                                          adzhl = ls_asset_cat-adzhl
                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_h2 = ls_asset_desc-atwtb."Parenth2
        ENDIF.
      ENDIF.
    ENDIF.
*END OF CHANGES BY KBANERJEE FOR ENHC0025213

    "Output

    CONCATENATE   lv_id lv_newid lv_evdesc lv_h1 lv_h2 lv_h3 lv_scaling
                  lv_div lv_type lv_prior lv_year lv_prj_blanket
                  INTO st_dimrec SEPARATED BY delimtr.


    CONCATENATE lv_account lv_category    lv_datasrc lv_entity
                lv_intco   lv_rptcurrency lv_time    lv_project
                lv_ioswbs  lv_division    lv_amount

                INTO st_datarec SEPARATED BY delimtr.





    APPEND st_datarec TO t_data.
    APPEND st_dimrec TO t_dim.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
    CLEAR:ls_wbs,ls_asset_cat,ls_asset_desc.
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
  ENDLOOP.


  PERFORM output_file.

ENDFORM.                    "print_new_report


*----------------------------------------------------------------------*
FORM print_teco_report.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Generating Report'.
  DATA: lv_objnr LIKE s_project-objnr.

  CONCATENATE   text-013 text-014 text-015 text-016 text-017 text-018
                text-019 text-020 text-021 text-022 text-023

                INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.



  lv_category     = 'Actual'.
  lv_datasrc      = 'SAP_PrjStatus'.
  lv_intco        = 'No_Intco'.
  lv_rptcurrency  = 'LC'.
  lv_ioswbs       = 'No_WBS'.

  "Delete projects that are not the main project
  DELETE t_project WHERE posid+7(4) <> 0.

  "Sort by each object/update and then by time
  "Delete duplicates based on time.
  SORT t_project BY pbukr objnr pspid stat udate.
  DELETE ADJACENT DUPLICATES FROM t_project COMPARING pbukr objnr pspid stat.

  LOOP AT t_project INTO s_project.
    CLEAR:  lv_account, lv_entity, lv_time, lv_project,
            lv_division,lv_amount.

    IF s_project-pspid CN '0123456789 '.
      CONTINUE.
    ENDIF.

    lv_entity = s_project-pbukr.
    CONCATENATE s_project-udate(6) '00' INTO lv_time.
    CONCATENATE s_project-pspid(2) '.' s_project-pspid+2(2) '.' s_project-pspid+4(3) INTO lv_project.
    CONCATENATE 'DIV_' s_project-pspid(2) INTO lv_division.


    IF s_project-stat = 'I0046'.
      lv_account = 'Prj_Closed'.
      lv_amount  = '0'.

      IF lv_time+4(2) = '01'.
        lv_time+4(2) = '02'.
      ENDIF.


      "When closed, check if we sent a TECO, if not then send one now
      CLEAR t_jcds.
      "Check this period and earlier.
      SELECT *
        FROM jcds
        INTO CORRESPONDING FIELDS OF TABLE t_jcds
        WHERE objnr = s_project-objnr
          AND stat  = 'I0045'
          AND chgnr = '001'
          AND udate < s_date-high
      .

      IF t_jcds IS INITIAL.
        "We never sent TECO.
        "Send duplicate entry but teco it.
        CONCATENATE 'Prj_TECO' lv_category    lv_datasrc lv_entity
                    lv_intco   lv_rptcurrency lv_time    lv_project
                    lv_ioswbs  lv_division    s_project-udate
              INTO st_datarec SEPARATED BY delimtr.

        APPEND st_datarec TO t_data.
      ENDIF.

    ELSEIF s_project-stat = 'I0045'.
      "Check that this hasn't already happend prior
      CLEAR t_jcds.
      SELECT *
        FROM jcds
        INTO CORRESPONDING FIELDS OF TABLE t_jcds
        WHERE objnr = s_project-objnr
          AND stat  = 'I0045'
          AND chgnr = '001'
          AND udate < s_date-low
      .

      IF t_jcds IS INITIAL.
        lv_account = 'Prj_TECO'.
        lv_amount = s_project-udate.
        "lv_amount  = '1'.
      ENDIF.
    ENDIF.




    CONCATENATE lv_account lv_category    lv_datasrc lv_entity
                lv_intco   lv_rptcurrency lv_time    lv_project
                lv_ioswbs  lv_division    lv_amount

                INTO st_datarec SEPARATED BY delimtr.

    APPEND st_datarec TO t_data.
  ENDLOOP.

  PERFORM output_file.
ENDFORM.                    "print_new_report




*----------------------------------------------------------------------*
FORM output_file.
  IF lv_local = 0.

    PERFORM open_datafile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO p_sfile.
    ENDLOOP.

    PERFORM close_datafile.

    WRITE:/ 'Data File Outputed Successfully to: ', p_sfile.

  ELSE.


    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_lfile
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
    WRITE:/ 'Data File Outputed Successfully to: ', p_lfile.
  ENDIF.


  "Output Dimension
  IF lv_new = true.
    IF lv_local = 0.

      PERFORM open_dimfile.

      LOOP AT t_dim INTO st_dimrec.
        TRANSFER st_dimrec TO lv_sfile.
      ENDLOOP.

      PERFORM close_dimfile.

      WRITE:/ 'Dimension File Outputed Successfully to: ', p_sfile.

    ELSE.


      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = lv_lfile
        TABLES
          data_tab                = t_dim
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
      WRITE:/ 'Dimension File Outputed Successfully to: ', lv_lfile.
    ENDIF.

  ENDIF.

ENDFORM.                    "output_file


*----------------------------------------------------------------------*
FORM open_datafile.
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_sfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_datafile.
  CLOSE DATASET p_sfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' p_sfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "CLOSE_ALL_FILES


*----------------------------------------------------------------------*
FORM open_dimfile.
  OPEN DATASET lv_sfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH lv_sfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_dimfile.
  CLOSE DATASET lv_sfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' lv_sfile msg.
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
      full_name     = p_lfile
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
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025213
*&---------------------------------------------------------------------*
*&      Form  F_GET_ASSET_CAT
*&---------------------------------------------------------------------*
*      fetch asset category for each level 1  WBS of projects
*----------------------------------------------------------------------*
FORM f_get_asset_cat CHANGING pct_wbs        TYPE tt_wbs
                              pct_asset_cat  TYPE tt_asset_cat
                              pct_asset_desc TYPE tt_asset_desc.
  DATA:lt_wbs_objnr  TYPE STANDARD TABLE OF gty_wbs       INITIAL SIZE 0,
       lt_project    TYPE STANDARD TABLE OF ty_project    INITIAL SIZE 0.
  FIELD-SYMBOLS:<lfs_wbs>       TYPE gty_wbs,
                <lfs_asset_cat> TYPE gty_asset_cat.
  CONSTANTS:lc_level1 TYPE ps_stufe   VALUE '1',
            lc_usr00  TYPE char20     VALUE 'SAP_KKR_PRUSR00',
            lc_014    TYPE klassenart VALUE '014'.
  DATA:lv_atinn TYPE atinn.

*Get all level1 WBS data from PRPS
  lt_project = t_project.
  SORT lt_project BY psphi.
  DELETE ADJACENT DUPLICATES FROM lt_project COMPARING psphi.
  IF lt_project IS NOT INITIAL.
    SELECT pspnr objnr psphi stufe
      FROM prps
      INTO CORRESPONDING FIELDS OF TABLE pct_wbs
      FOR ALL ENTRIES IN lt_project
      WHERE  psphi = lt_project-psphi
        AND  stufe = lc_level1.
    IF sy-subrc IS INITIAL.
      SORT pct_wbs BY psphi.
      LOOP AT pct_wbs ASSIGNING <lfs_wbs>.
        <lfs_wbs>-objek = <lfs_wbs>-objnr.
      ENDLOOP.
      lt_wbs_objnr = pct_wbs.
      SORT lt_wbs_objnr BY objnr.
      DELETE ADJACENT DUPLICATES FROM lt_wbs_objnr COMPARING objnr.
      IF lt_wbs_objnr IS NOT INITIAL.
*Convert characteristic number to internal format
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = lc_usr00 "SAP_KKR_PRUSR00
          IMPORTING
            output = lv_atinn.
*Get asset category from AUSP
        SELECT objek atinn klart adzhl atwrt
          FROM ausp
          INTO CORRESPONDING FIELDS OF TABLE pct_asset_cat
          FOR ALL ENTRIES IN lt_wbs_objnr
          WHERE objek = lt_wbs_objnr-objek
            AND atinn = lv_atinn
            AND klart = lc_014. "'014'.
        IF sy-subrc IS INITIAL.
          SORT pct_asset_cat BY atinn atzhl adzhl.
          LOOP AT pct_asset_cat ASSIGNING <lfs_asset_cat>.
            <lfs_asset_cat>-atzhl = <lfs_asset_cat>-atwrt.
          ENDLOOP.
*Get asset category description from CAWNT
          SELECT atinn atzhl adzhl atwtb
            FROM cawnt
            INTO TABLE pct_asset_desc
            FOR ALL ENTRIES IN pct_asset_cat
            WHERE atinn = pct_asset_cat-atinn
              AND atzhl = pct_asset_cat-atzhl
              AND spras = sy-langu
              AND adzhl = pct_asset_cat-adzhl.
          IF sy-subrc IS INITIAL.
            SORT pct_asset_desc BY atinn atzhl adzhl.
          ENDIF.
          SORT pct_asset_cat BY objek.
        ENDIF."endif sy-subrc check on AUSP table
      ENDIF."endif lt_wbs_objnr IS NOT INITIAL
    ENDIF."sy-subrc check on prps
  ENDIF."lt_project IS NOT INITIAL
ENDFORM.                    " F_GET_ASSET_CAT
*END OF CHANGES BY KBANERJEE FOR ENHC0025213
