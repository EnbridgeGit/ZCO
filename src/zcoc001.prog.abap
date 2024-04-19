******************************************************************
*                                                                *
*   PROGRAM: ZCOC001                                             *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/02/24                                          *
*                                                                *
*   DESCRIPTION: This program will resequence entries in the SAP *
*                table COBRB.  The sequence number can only go   *
*                up to 999 and then it rolls over to zero.  The  *
*                program will remove gaps in the sequence and    *
*                put the zero entry at the end.                  *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/02/24 LRITCHIE TR692 New program for Terry Smith          *
******************************************************************

report zcoc001 line-size 132 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: cobrb.                          "Settlement rules

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* original data
data: tbl_all_cobrb like cobrb occurs 0 with header line.

* original data that is out of sequence
data: tbl_old_cobrb like cobrb occurs 0 with header line.

* old values
data: begin of tbl_old occurs 0,
         objnr    like cobrb-objnr,
         lfdnr    like cobrb-lfdnr,
      end of tbl_old.

* new values
data: begin of tbl_new occurs 0,
         objnr    like cobrb-objnr,
         lfdnr    like cobrb-lfdnr,
      end of tbl_new.

* objects out of sequence
data: begin of tbl_objnr occurs 0,
         objnr   like cobrb-objnr,
      end of tbl_objnr.

* resequencing table
data: begin of tbl_old_new occurs 0,
         objnr      like cobrb-objnr,
         old_lfdnr  like cobrb-lfdnr,
         old_action(10) type c,     "no change, add, update, delete
         new_lfdnr(6) type n,
         new_action(10) type c,     "no change, add, update, delete
      end of tbl_old_new.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_read_cnt         type i,
      v_objnr_cnt        type i,
      v_lfdnr_cnt        type i,
      v_no_change        type i,
      v_potential_add    type i,
      v_potential_upd    type i,
      v_potential_del    type i,
      v_actual_add       type i,
      v_actual_upd       type i,
      v_actual_del       type i,
      v_prev_objnr       like cobrb-objnr,
      v_cobrb            like cobrb,
      v_count            type i.

******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

selection-screen skip 1.
selection-screen begin of block b1 with frame title text-001.
selection-screen skip 1.
select-options s_objnr for cobrb-objnr.              "object number
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
selection-screen skip 1.
parameters p_detail radiobutton group r2.        "Print details
parameters p_sum    radiobutton group r2.        "Print summary
parameters p_total  radiobutton group r2.        "Print only the totals
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
selection-screen skip 1.
parameters p_report radiobutton group r3.        "Report only
parameters p_update radiobutton group r3.        "Report & update COBRB
selection-screen end of block b3.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
initialization.

******************************************************************
*                   START OF SELECTION                           *
******************************************************************
start-of-selection.

  perform get_cobrb_entries.

  perform identify_bad_objects.

  perform setup_old_new_xref_table.

  perform create_report.

  if p_update = 'X'.
    perform update_cobrb.
  endif.

end-of-selection.

  skip 4.

  write:/ 'Total COBRB records read:    ', v_read_cnt.
  skip 1.
  write:/ 'Objects out of sequence:     ', v_objnr_cnt.
  write:/ 'Object items:                ', v_lfdnr_cnt.
  skip 1.
  write:/ 'Object items not changing:   ', v_no_change.
  skip 1.
  write:/ 'Potential adds:              ', v_potential_add.
  write:/ 'Potential deletes:           ', v_potential_del.
  write:/ 'Potential updates:           ', v_potential_upd.
  skip 1.
  write:/ 'Actual adds:                 ', v_actual_add.
  write:/ 'Actual deletes:              ', v_actual_del.
  write:/ 'Actual updates:              ', v_actual_upd.


******************************************************************
*                   SUBROUTINES                                  *
******************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_COBRB_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_cobrb_entries .

  select *
     into table tbl_all_cobrb
     from cobrb
     where objnr in s_objnr.

  sort tbl_all_cobrb by objnr bureg lfdnr.

  describe table tbl_all_cobrb lines v_read_cnt.

endform.                    " GET_COBRB_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  IDENTIFY_BAD_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form identify_bad_objects .

* identify the object numbers which have entries that are out of sequence
  loop at tbl_all_cobrb.

    if sy-tabix = 1.
      v_prev_objnr = tbl_all_cobrb-objnr.
      v_count = 0.
    endif.

    if v_prev_objnr <> tbl_all_cobrb-objnr.
      v_prev_objnr = tbl_all_cobrb-objnr.
      v_count = 0.
    endif.

    v_count = v_count + 1.

    if v_count <> tbl_all_cobrb-lfdnr.
      if tbl_objnr-objnr <> tbl_all_cobrb-objnr.
        tbl_objnr-objnr = tbl_all_cobrb-objnr.
        append tbl_objnr.
      endif.
    endif.

  endloop.

  describe table tbl_objnr lines v_objnr_cnt.

* save all the entries for the objects that are out of sequence
  clear v_prev_objnr.
  loop at tbl_all_cobrb.

    if tbl_all_cobrb-objnr <> v_prev_objnr.
      read table tbl_objnr with key objnr = tbl_all_cobrb-objnr
                                binary search.
      v_prev_objnr = tbl_all_cobrb-objnr.
    endif.
    if tbl_objnr-objnr = tbl_all_cobrb-objnr.
      tbl_old_cobrb = tbl_all_cobrb.
      append tbl_old_cobrb.
    endif.

  endloop.

  describe table tbl_old_cobrb lines v_lfdnr_cnt.

endform.                    " IDENTIFY_BAD_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  SETUP_OLD_NEW_XREF_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_old_new_xref_table .



  loop at tbl_old_cobrb.
    if sy-tabix = 1.
      v_prev_objnr = tbl_old_cobrb-objnr.
      v_count = 0.
    endif.
    if v_prev_objnr <> tbl_old_cobrb-objnr.
      v_prev_objnr = tbl_old_cobrb-objnr.
      v_count = 0.
    endif.

    v_count = v_count + 1.

    tbl_old_new-objnr = tbl_old_cobrb-objnr.
    tbl_old_new-old_lfdnr = tbl_old_cobrb-lfdnr.
    if tbl_old_cobrb-lfdnr = 0.                    "put the zero entry at the end
      tbl_old_new-new_lfdnr = 999999.
      v_count = v_count - 1.
    else.
      tbl_old_new-new_lfdnr = v_count.
    endif.

    append tbl_old_new.

  endloop.

  sort tbl_old_new by objnr new_lfdnr.

* change the 999999 sequence number to the last open sequence number
  loop at tbl_old_new.
    if sy-tabix = 1.
      v_prev_objnr = tbl_old_new-objnr.
      v_count = 0.
    endif.
    if v_prev_objnr <> tbl_old_new-objnr.
      v_prev_objnr = tbl_old_new-objnr.
      v_count = 0.
    endif.
    v_count = v_count + 1.
    if tbl_old_new-new_lfdnr = 999999.
      tbl_old_new-new_lfdnr = v_count.
      modify tbl_old_new.
    endif.
  endloop.

   sort tbl_old_new by objnr old_lfdnr.

* split it into old & new sections in order to determine what type of action will be needed
  loop at tbl_old_new.
    tbl_old-objnr = tbl_old_new-objnr.
    tbl_old-lfdnr = tbl_old_new-old_lfdnr.
    append tbl_old.
    tbl_new-objnr = tbl_old_new-objnr.
    tbl_new-lfdnr = tbl_old_new-new_lfdnr.
    append tbl_new.
  endloop.

  sort tbl_new.
  sort tbl_old.

  clear: v_no_change, v_potential_del, v_potential_upd,
         v_potential_add.

* determine if this is a no change, new, update or delete situation
  loop at tbl_old_new.
    if tbl_old_new-old_lfdnr = tbl_old_new-new_lfdnr.
      tbl_old_new-old_action = 'No Change'.
      tbl_old_new-new_action = 'No Change'.
      v_no_change = v_no_change + 1.
      modify tbl_old_new.
      continue.
    endif.

    read table tbl_new with key objnr = tbl_old_new-objnr
                                lfdnr = tbl_old_new-old_lfdnr
                                binary search.
    if sy-subrc <> 0.
      tbl_old_new-old_action = 'Delete'.
      v_potential_del = v_potential_del + 1.
    else.
      tbl_old_new-old_action = 'Update'.
      v_potential_upd = v_potential_upd + 1.
    endif.

    read table tbl_old with key objnr = tbl_old_new-objnr
                                lfdnr = tbl_old_new-new_lfdnr
                                binary search.
    if sy-subrc <> 0.
      tbl_old_new-new_action = 'New'.
      v_potential_add =  v_potential_add + 1.
    else.
      tbl_old_new-new_action = 'Update'.
    endif.

    modify tbl_old_new.

  endloop.

endform.                    " SETUP_OLD_NEW_XREF_TABLE
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_report .

  if p_detail = 'X'.
    loop at tbl_old_new.
      if tbl_old_new-objnr <> v_prev_objnr.
        skip 1.
      endif.
      write:/5 tbl_old_new-objnr, tbl_old_new-old_action,
               tbl_old_new-old_lfdnr no-zero, '====>',  tbl_old_new-new_action,
               tbl_old_new-new_lfdnr no-zero.
      v_prev_objnr = tbl_old_new-objnr.
    endloop.
  endif.

  if p_sum = 'X'.
    loop at tbl_objnr.
      write:/5 tbl_objnr-objnr.
    endloop.
  endif.

endform.                    " CREATE_REPORT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_COBRB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form update_cobrb .

  clear: v_actual_del, v_actual_add, v_actual_upd.

  loop at tbl_old_cobrb.
    if sy-tabix = 1.
      v_prev_objnr = tbl_old_cobrb-objnr.
    endif.
    if v_prev_objnr <> tbl_old_cobrb-objnr.
      commit work.
      v_prev_objnr = tbl_old_cobrb-objnr.
    endif.

    read table tbl_old_new with key objnr = tbl_old_cobrb-objnr
                                    old_lfdnr = tbl_old_cobrb-lfdnr
                                    binary search.
    if sy-subrc <> 0.
      write:/ 'Program error, searching for ', tbl_old_cobrb-objnr,
              tbl_old_cobrb-lfdnr.
      stop.
    endif.

    if tbl_old_new-old_action = 'No Change'.
      continue.
    endif.

* delete
    if tbl_old_new-old_action = 'Delete'.
      v_actual_del = v_actual_del + 1.
      delete from cobrb where objnr = tbl_old_cobrb-objnr
                          and bureg = '000'
                          and lfdnr = tbl_old_cobrb-lfdnr.
      if sy-subrc <> 0.
        write:/ 'Delete failed for ', tbl_old_cobrb-objnr,
                tbl_old_cobrb-lfdnr.
        stop.
      endif.
    endif.

* new
    if tbl_old_new-new_action = 'New'.
      v_actual_add = v_actual_add + 1.
      v_cobrb = tbl_old_cobrb.
      v_cobrb-lfdnr = tbl_old_new-new_lfdnr.
      modify cobrb from v_cobrb.
      if sy-subrc <> 0.
        write:/ 'New entry failed for ', v_cobrb-objnr,
                v_cobrb-lfdnr.
        stop.
      endif.
    endif.

* update
    if tbl_old_new-new_action = 'Update'.
      v_actual_upd = v_actual_upd + 1.
      v_cobrb = tbl_old_cobrb.
      v_cobrb-lfdnr = tbl_old_new-new_lfdnr.
      modify cobrb from v_cobrb.
      if sy-subrc <> 0.
        write:/ 'Update failed for ', v_cobrb-objnr,
                v_cobrb-lfdnr.
        stop.
      endif.
    endif.

  endloop.

  commit work.

endform.                    " UPDATE_COBRB
