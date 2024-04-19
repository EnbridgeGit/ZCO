******************************************************************
*                                                                *
*   PROGRAM: ZCOC002                                             *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/02/25                                          *
*                                                                *
*   DESCRIPTION: This program will delete COBRB entries when     *
*                the settlement transaction (AVORG) is either    *
*                KOAP or blank and the version (VERSN) is blank. *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/02/25 LRITCHIE TR692 New program for Terry Smith          *
******************************************************************

report zcoc002 line-size 132 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: cobrb.                          "Settlement rules

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* original data
data: tbl_cobrb like cobrb occurs 0 with header line.


* resequencing table
data: begin of tbl_delete occurs 0,
         objnr      like cobrb-objnr,
         bureg      like cobrb-bureg,
         lfdnr      like cobrb-lfdnr,
         avorg      like cobrb-avorg,
         versn      like cobrb-versn,
      end of tbl_delete.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_read_cnt         type i,
      v_no_change        type i,
      v_potential_del    type i,
      v_actual_del       type i,
      v_prev_objnr       like cobrb-objnr,
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
parameters p_total  radiobutton group r2.        "Print only the totals
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
selection-screen skip 1.
parameters p_report radiobutton group r3.        "Report only
parameters p_update radiobutton group r3.        "Report & update COBRB
selection-screen end of block b3.

selection-screen begin of block b4 with frame title text-004.
selection-screen skip 1.
selection-screen: begin of line, comment 1(79) text-005.
selection-screen end of line.
selection-screen end of block b4.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
initialization.

******************************************************************
*                   START OF SELECTION                           *
******************************************************************
start-of-selection.

  perform get_cobrb_entries.

  perform identify_entries_to_delete.

  if p_detail = 'X'.
    write:/ '     OBJECT NUMBER      DIST RULE   SEQ#  TCODE'.
    perform create_detail_report.
  endif.

  if p_update = 'X'.
    perform delete_cobrb.
  endif.

end-of-selection.

  skip 4.

  write:/ 'Total COBRB records read: ', v_read_cnt.
  skip 1.
  write:/ 'Potential deletes:        ', v_potential_del.
  skip 1.
  write:/ 'Actual deletes:           ', v_actual_del.


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
     into table tbl_cobrb
     from cobrb
     where objnr in s_objnr.

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
*&      Form  IDENTIFY_ENTRIES_TO_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form identify_entries_to_delete .

  loop at tbl_cobrb.

    if tbl_cobrb-avorg <> 'KOAO' and tbl_cobrb-versn = ' '.
      tbl_delete-objnr = tbl_cobrb-objnr.
      tbl_delete-bureg = tbl_cobrb-bureg.
      tbl_delete-lfdnr = tbl_cobrb-lfdnr.
      tbl_delete-avorg = tbl_cobrb-avorg.
      tbl_delete-versn = tbl_cobrb-versn.
      append tbl_delete.
    endif.

  endloop.

  describe table tbl_delete lines v_potential_del.

endform.                    " IDENTIFY_ENTRIES_TO_DELETE
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
             44 tbl_delete-avorg.

  endloop.

endform.                    " create_detail_report
