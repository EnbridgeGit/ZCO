******************************************************************
*                                                                *
*   PROGRAM: ZCOC003                                             *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/02/26                                          *
*                                                                *
*   DESCRIPTION: This program will delete COBRB entries based    *
*                on order status.                                *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/02/26 LRITCHIE TR692 New program for Terry Smith          *
******************************************************************

report zcoc003 line-size 132 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: cobrb,                          "Settlement rules
        jest,                           "Object status
        tj02t.                          "Status texts

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* original data
data: tbl_cobrb like cobrb occurs 0 with header line.

* unique object numbers
data: begin of tbl_objnr occurs 0,
        objnr        like cobrb-objnr,
      end of tbl_objnr.

* old entries
data: begin of tbl_old occurs 0,
        objnr        like cobrb-objnr,
        bureg       like cobrb-bureg,
        lfdnr       like cobrb-lfdnr,
      end of tbl_old.

* object status
data: begin of tbl_jest occurs 0,
         objnr       like jest-objnr,
         stat        like jest-stat,
         inact       like jest-inact,
         chgnr       like jest-chgnr,
     end of tbl_jest.

* status texts
data: begin of tbl_tj02t occurs 0,
         istat      like tj02t-istat,
         txt04      like tj02t-txt04,
         txt30      like tj02t-txt30,
      end of tbl_tj02t.

data: begin of tbl_stat occurs 0,
         stat       like jest-stat,
      end of tbl_stat.

* objects to be deleted
*data: tbl_delete like cobrb occurs 0 with header line.
data: begin of tbl_delete occurs 0,
         objnr       like cobrb-objnr,
         bureg       like cobrb-bureg,
         lfdnr       like cobrb-lfdnr,
         letja       like cobrb-letja,
         avorg       like cobrb-avorg,
         txt30       like tj02t-txt30,
      end of tbl_delete.

* status values
ranges r_stat       for jest-stat.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_read_cnt         type i,
      v_objnr_cnt        type i,
      v_potential_del    type i,
      v_actual_del       type i,
      v_error(1)         type c,
      v_prev_objnr       like cobrb-objnr,
      v_count            type i.

******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

selection-screen skip 1.
selection-screen begin of block b1 with frame title text-001.
selection-screen skip 1.
select-options s_objnr for cobrb-objnr.              "object number
selection-screen skip 1.
select-options s_avorg for cobrb-avorg.              "Tcode
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-004.
selection-screen skip 1.
parameter p_year like cobrb-letja obligatory default '1990'.
selection-screen skip 1.
parameter p_closed as checkbox.
parameter p_delete as checkbox.
parameter p_teco   as checkbox.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-002.
selection-screen skip 1.
parameters p_detail radiobutton group r3.        "Print details
parameters p_total  radiobutton group r3.        "Print only the totals
selection-screen end of block b3.

selection-screen begin of block b4 with frame title text-003.
selection-screen skip 1.
parameters p_report radiobutton group r4.        "Report only
parameters p_update radiobutton group r4.        "Report & update COBRB
selection-screen end of block b4.


******************************************************************
*                   INITIALIZATION                               *
******************************************************************
initialization.

******************************************************************
*                   START OF SELECTION                           *
******************************************************************
start-of-selection.

  clear v_error.

  if p_closed = ' ' and p_delete = ' ' and p_teco = ' '.
    skip 5.
    write:/10 'YOU MUST PICK AT LEAST ONE STATUS'.
    v_error = 'X'.
    stop.
  endif.

  perform get_cobrb_entries.

  perform identify_old_entries.

  perform get_object_status_entries.

  perform get_status_texts.

  perform setup_entries_for_deletion.

  if p_detail = 'X'.
    write:/ '     OBJECT NUMBER      DIST RULE   SEQ#  Last Used Year    Trans    Status'.
    perform create_detail_report.
  endif.

  if p_update = 'X'.
    perform delete_cobrb.
  endif.

end-of-selection.

  if v_error = ' '.

    skip 4.

    write:/ 'Total COBRB records read: ', v_read_cnt.
    skip 1.
    write:/ 'Potential deletes:        ', v_potential_del.
    skip 1.
    write:/ 'Actual deletes:           ', v_actual_del.

  endif.

******************************************************************
*                   SUBROUTINES                                  *
******************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_COBRB_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_cobrb_entries .

  refresh tbl_cobrb.

  select *
     into table tbl_cobrb
     from cobrb
     where objnr in s_objnr
       and avorg in s_avorg.

  sort tbl_cobrb by objnr bureg lfdnr.

  describe table tbl_cobrb lines v_read_cnt.

endform.                    " GET_COBRB_ENTRIES

*&---------------------------------------------------------------------*
*&      Form  delete_cobrb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form delete_cobrb .

  clear v_actual_del.

  loop at tbl_delete.

    v_actual_del = v_actual_del + 1.
    delete from cobrb where objnr = tbl_delete-objnr
                        and bureg = tbl_delete-bureg
                        and lfdnr = tbl_delete-lfdnr.
    if sy-subrc <> 0.
      write:/ 'Delete failed for ', tbl_delete-objnr,
               tbl_delete-bureg, tbl_delete-lfdnr.
      stop.
    endif.

  endloop.

  commit work.

endform.                    " delete_cobrb
*&---------------------------------------------------------------------*
*&      Form  identify_old_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form identify_old_entries .

* The old entries can be in any sequence.  If it has a lasted used
* year <= the deletion year from the selection screen, it is a candidate
* for deletion.

  refresh: tbl_objnr, tbl_old.

  loop at tbl_cobrb.

    if tbl_cobrb-letja <= p_year.
      tbl_old-objnr = tbl_cobrb-objnr.
      tbl_old-bureg = tbl_cobrb-bureg.
      tbl_old-lfdnr = tbl_cobrb-lfdnr.
      append tbl_old.

      tbl_objnr-objnr = tbl_cobrb-objnr.
      append tbl_objnr.

    endif.

  endloop.

  sort tbl_old.
  sort tbl_objnr.
  delete adjacent duplicates from tbl_objnr.

endform.                    " identify_old_entries
*&---------------------------------------------------------------------*
*&      Form  create_detail_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_detail_report .

  loop at tbl_delete.
    if sy-tabix = 1.
      v_prev_objnr = tbl_delete-objnr.
    endif.
    if v_prev_objnr <> tbl_delete-objnr.
      skip 1.
      v_prev_objnr = tbl_delete-objnr.
    endif.

    write:/5 tbl_delete-objnr, tbl_delete-bureg, 37 tbl_delete-lfdnr,
          48 tbl_delete-letja, 61 tbl_delete-avorg, 70 tbl_delete-txt30.

  endloop.

endform.                    " create_detail_report
*&---------------------------------------------------------------------*
*&      Form  get_object_status_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_object_status_entries .

  refresh: r_stat, tbl_jest.

  if p_closed = 'X'.
    r_stat-sign = 'I'.
    r_stat-option = 'EQ'.
    r_stat-low = 'I0046'.
    append r_stat.
  endif.

  if p_delete = 'X'.
    r_stat-sign = 'I'.
    r_stat-option = 'EQ'.
    r_stat-low = 'I0076'.
    append r_stat.
  endif.

  if p_teco = 'X'.
    r_stat-sign = 'I'.
    r_stat-option = 'EQ'.
    r_stat-low = 'I0045'.
    append r_stat.
  endif.

  if not tbl_objnr[] is initial.
    select objnr stat inact
           into table tbl_jest
           from jest
           for all entries in tbl_objnr
           where objnr = tbl_objnr-objnr
             and stat in r_stat
             and inact = ' '.

    sort tbl_jest by objnr stat.
  endif.

endform.                    " get_object_status_entries
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_status_texts .

  refresh: tbl_stat, tbl_tj02t.

  loop at tbl_jest.
    tbl_stat-stat = tbl_jest-stat.
    append tbl_stat.
  endloop.

  sort tbl_stat.
  delete adjacent duplicates from tbl_stat.

  if not tbl_stat[] is initial.
    select istat txt04 txt30
       into table tbl_tj02t
       from tj02t
       for all entries in tbl_stat
       where istat = tbl_stat-stat
         and spras = 'E'.
    sort tbl_tj02t by istat.
  endif.

endform.                    " GET_STATUS_TEXTS
*&---------------------------------------------------------------------*
*&      Form  REPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form report_data .

  loop at tbl_objnr.

    write:/5 tbl_objnr-objnr.

    loop at tbl_jest where objnr = tbl_objnr-objnr.
      clear tbl_tj02t.
      read table tbl_tj02t with key istat = tbl_jest-stat
                                    binary search.
      write:/20 tbl_jest-stat, tbl_jest-inact, tbl_jest-chgnr,
                tbl_tj02t-txt04, tbl_tj02t-txt30.
    endloop.

  endloop.

endform.                    " REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  SETUP_ENTRIES_FOR_DELETION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_entries_for_deletion .

  refresh tbl_delete.

  loop at tbl_cobrb.

* must be <= deletion year from selection screen
    read table tbl_old with key objnr = tbl_cobrb-objnr
                                bureg = tbl_cobrb-bureg
                                lfdnr = tbl_cobrb-lfdnr
                                binary search.
    if sy-subrc <> 0.
      continue.
    endif.

* must be one of the status values from the selection screen
    read table tbl_jest with key objnr = tbl_cobrb-objnr
                                 binary search.
    if sy-subrc = 0.
      tbl_delete-objnr = tbl_cobrb-objnr.
      tbl_delete-bureg = tbl_cobrb-bureg.
      tbl_delete-lfdnr = tbl_cobrb-lfdnr.
      tbl_delete-letja = tbl_cobrb-letja.
      tbl_delete-avorg = tbl_cobrb-avorg.
      clear tbl_tj02t.
      read table tbl_tj02t with key istat = tbl_jest-stat
                                    binary search.
      tbl_delete-txt30 = tbl_tj02t-txt30.
      append tbl_delete.
    endif.

  endloop.

  describe table tbl_delete lines v_potential_del.

endform.                    " SETUP_ENTRIES_FOR_DELETION
