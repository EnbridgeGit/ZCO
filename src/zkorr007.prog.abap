REPORT ZKORR007 .

************************************************************************
*  Author:      Dr.Monika Ahrens
*  Description: Create Plan Rules As a Copy of Actual or Plan Rules
*


* KASS-Event Framework and general fields
include rkassefw.
include rko74000.

* order part
include rkosel10.

* general part
types: yt_cobrb like cobrb occurs 0,
       yt_objnr like jsto_pre occurs 0.
types: begin of ys_buper,
       gjahr like auak-gjahr,
       perio like auak-perio,
       end of ys_buper.

* parameters:  kokrs like auak-kokrs no-display.
selection-screen begin of block from with frame title text-001.
selection-screen begin of line.
parameters: plan default 'X' radiobutton group fkt user-command fkt.
selection-screen comment 4(40) text-a01.
selection-screen end of line.
parameters: versn_fr like lko74-versn.
selection-screen comment 50(30) txt_fr for field versn_fr.
selection-screen begin of line.
parameters: actual radiobutton group fkt.
selection-screen comment 4(40) text-a02.
selection-screen end of line.
parameters: ful type c.
selection-screen end of block from.

selection-screen begin of block to with frame title text-002.
parameters: versn_to like lko74-versn obligatory.
selection-screen comment 50(30) txt_to for field versn_to.
selection-screen end of block to.

selection-screen begin of block control with frame title text-003.
parameters: gjahr like auak-gjahr memory id gjr obligatory,
            testrun like lko74-testlauf default 'X'.
selection-screen end of block control.


* events
initialization.
  perform initialization.

start-of-selection.
  perform start_of_selection.

at selection-screen on block from.
  perform check_versn using kokrs gjahr versn_fr
                   changing txt_fr.

at selection-screen on block to.
  perform check_versn using kokrs gjahr versn_to
                   changing txt_to.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
form initialization.

  perform kass_fw_init
     using 'KOAO' kass_fw_default_init_options.

  perform kass_fw_event_register using
     kass_fw_event-selection_screen_output
     'AT_SELSCREEN_OUTPUT'
     kass_fw_priority-main
     kass_fw_default_reg_options.

  perform sender_initialization.

  perform check_versn using kokrs gjahr versn_fr
                       changing txt_fr.
  perform check_versn using kokrs gjahr versn_to
                      changing txt_to.

endform.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  at_selscreen_output
*&---------------------------------------------------------------------*
form at_selscreen_output.
  loop at screen.
    if screen-name = 'VERSN_FR'.
      if plan = 'X'.
        screen-required = 1.
      else.
        clear screen-required.
      endif.
      modify screen.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_VERSN
*&---------------------------------------------------------------------*
form check_versn using    id_kokrs like lko74-kokrs
                          id_gjahr like lko74-gjahr
                          id_versn like lko74-versn
                 changing cd_text.

  data: ls_tkt09 like tkt09,
        ls_tka07 like tka07.

  clear cd_text.
  check not id_versn is initial.
  check not id_kokrs is initial.
  check not id_gjahr is initial.
  call function 'K_VERSN_READ'
       exporting
            i_gjahr = id_gjahr
            i_kokrs = id_kokrs
            i_versn = id_versn
       importing
            e_tka07 = ls_tka07
            e_tkt09 = ls_tkt09.

  cd_text = ls_tkt09-txt.
  if ls_tka07-plicc <> 'X'.
    message e268(kd) with ls_tka07-versn.
  endif.

endform.                               " VERSN_TEXT_GET

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
form start_of_selection.

  data: lt_objnr type yt_objnr,
        ls_objnr like jsto_pre.

  call function 'MESSAGES_INITIALIZE'.

* selection of senders
  perform select_senders changing lt_objnr.

* process senders individually
  loop at lt_objnr into ls_objnr.
    perform process_one_sender using ls_objnr-objnr
                                     kokrs
                                     gjahr
                                     actual
                                     versn_fr
                                     versn_to
                                     ful
                                     testrun.
  endloop.

* show messages
  call function 'MESSAGES_SHOW'
       exporting
            batch_list_type = 'L'.
  call function 'MESSAGES_INITIALIZE'.
  call function 'MESSAGES_STOP'.

endform.                               " START_OF_SELECTION

*&---------------------------------------------------------------------*
*&      Form  PROCESS_ONE_SENDER
*&---------------------------------------------------------------------*
form process_one_sender using    id_objnr like auak-objnr
                                 id_kokrs like auak-kokrs
                                 id_gjahr like auak-gjahr
                                 ib_actual type c
                                 id_versn_fr like auak-versn
                                 id_versn_to like auak-versn
                                 ib_ful type c
                                 ib_testrun type c.

  data: ld_subrc like sy-subrc,
        lt_cobrb type yt_cobrb,
        lt_cobrb_ins type yt_cobrb,
        ls_new_valid_from type ys_buper,
        ls_new_valid_to type ys_buper.

* check if sender is planintegrated and not statistical
  perform check_sender using id_objnr id_kokrs
                    changing ld_subrc.
  check ld_subrc = 0.

* read plan rules
  perform read_rules using id_objnr id_gjahr ib_actual
                           id_versn_fr id_versn_to ib_ful
                  changing lt_cobrb.

* check if rule already exists
  perform check_rule_exist using lt_cobrb
                                 id_versn_to id_kokrs id_gjahr
                        changing ls_new_valid_from ls_new_valid_to
                                 ld_subrc.
  check ld_subrc = 0.

* copy rules
  if ib_actual = space.
    perform copy_rules_from_plan using id_objnr id_versn_fr id_versn_to
                                       ls_new_valid_from ls_new_valid_to
                                       lt_cobrb
                              changing ld_subrc lt_cobrb_ins.
  else.
    perform copy_rules_from_actual using id_objnr id_versn_to ib_ful
                                       ls_new_valid_from ls_new_valid_to
                                       lt_cobrb
                              changing ld_subrc lt_cobrb_ins.
  endif.

* update
  if ib_testrun is initial and not lt_cobrb_ins is initial.
    insert cobrb from table lt_cobrb_ins.
    commit work and wait.
    if sy-subrc <> 0.
      perform message_store using 'KA' 'I' '600'
             id_objnr ': Update failed.' space space.       "#EC NOTEXT
    endif.
  endif.

endform.                               " PROCESS_ONE_SENDER

*&---------------------------------------------------------------------*
*&      Form  CHECK_SENDER
*&---------------------------------------------------------------------*
form check_sender using    id_objnr like auak-objnr
                           id_kokrs like auak-kokrs
                  changing cd_subrc like sy-subrc.

  data: lb_astkz type c,
        ld_objnr like auak-objnr,
        lb_plint type c.

* read senders
  ld_objnr = id_objnr.
  case ld_objnr(2).
    when 'OR'.
      perform read_sender_or using ld_objnr id_kokrs
                          changing lb_astkz lb_plint cd_subrc.
    when 'PR'.
      perform read_sender_pr using ld_objnr
                          changing lb_astkz lb_plint cd_subrc.
    when others.
      perform message_store using 'KA' 'E' '600'
        id_objnr ':Object type not supported.' space space. "#EC NOTEXT
      cd_subrc = 4.
  endcase.

* special checks
  check cd_subrc = 0.
  if not lb_astkz is initial.
    perform message_store using 'KA' 'E' '600'
            id_objnr ':Object statistical.' space space.    "#EC NOTEXT
    cd_subrc = 4.
    exit.
  endif.
  if lb_plint is initial.
    perform message_store using 'KA' 'E' '600'
          id_objnr ':not planintegrated.' space space.      "#EC NOTEXT
    cd_subrc = 4.
    exit.
  endif.

endform.                               " CHECK_SENDER
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_STORE
*&---------------------------------------------------------------------*
form message_store using value(p_msgid) like sy-msgid
                         value(p_msgty) like sy-msgty
                         value(p_msgno) like sy-msgno
                         value(p_msgv1)
                         value(p_msgv2)
                         value(p_msgv3)
                         value(p_msgv4).

  data: ld_txtnr like mesg-txtnr.

  sy-msgv1 = p_msgv1.
  sy-msgv2 = p_msgv2.
  sy-msgv3 = p_msgv3.
  sy-msgv4 = p_msgv4.
  ld_txtnr = p_msgno.
  call function 'MESSAGE_STORE'
       exporting
            arbgb                   = p_msgid
            exception_if_not_active = ' '
            msgty                   = p_msgty
            txtnr                   = ld_txtnr
            msgv1                   = sy-msgv1
            msgv2                   = sy-msgv2
            msgv3                   = sy-msgv3
            msgv4                   = sy-msgv4.
endform.                               " MESSAGE_STORE

*&---------------------------------------------------------------------*
*&      Form  RULES_READ
*&---------------------------------------------------------------------*
form read_rules using    id_objnr like auak-objnr
                         id_gjahr like auak-gjahr
                         ib_actual type c
                         id_versn_fr like auak-versn
                         id_versn_to like auak-versn
                         ib_ful_actual type c
                changing ct_cobrb type yt_cobrb.

  data: ld_gjahr_init like auak-gjahr,
        ld_avorg_init like cobrb-avorg.

  refresh ct_cobrb.

*  copy plan rules -> read only plan rules for the two versions
  if ib_actual = space.
    select * from cobrb into table ct_cobrb
                    where objnr = id_objnr
                    and
                 ( ( perbz = 'PER' or perbz = 'VOR' )
                   and avorg = 'KOAP'
                   and versn = id_versn_to
                   or
                   ( perbz = 'PER' or perbz = 'VOR' )
                   and avorg = 'KOAP'
                   and versn = id_versn_fr
                   and gabja <= id_gjahr
                   and ( gbisj >= id_gjahr or gbisj = ld_gjahr_init ) ).
* copy actual FUL to plan -> read FUL actual and plan for one version
  elseif ib_ful_actual = 'X'.
    select * from cobrb into table ct_cobrb
             where objnr = id_objnr
                   and
                 ( ( perbz = 'PER' or perbz = 'VOR' )
                   and avorg = 'KOAP'
                   and versn = id_versn_to
                   or
                   perbz = 'GES'
                   and ( avorg = 'KOAO' or avorg = ld_avorg_init )
                   and gabja <= id_gjahr
                   and ( gbisj >= id_gjahr or gbisj = ld_gjahr_init ) ).
* copy actual PER to plan -> read PER actual and plan for one version
  else.
    select * from cobrb into table ct_cobrb
             where objnr = id_objnr
                   and
                 ( ( perbz = 'PER' or perbz = 'VOR' )
                   and avorg = 'KOAP'
                   and versn = id_versn_to
                   or
                   ( perbz = 'PER' or perbz = 'VOR' )
                   and ( avorg = 'KOAO' or avorg = ld_avorg_init )
                   and gabja <= id_gjahr
                   and ( gbisj >= id_gjahr or gbisj = ld_gjahr_init ) ).
  endif.

endform.                               " RULES_READ
*&---------------------------------------------------------------------*
*&      Form  COPY_RULES_FROM_PLAN
*&---------------------------------------------------------------------*
form copy_rules_from_plan using    id_objnr like auak-objnr
                                   id_versn_fr like auak-versn
                                   id_versn_to like auak-versn
                                   is_new_valid_from type ys_buper
                                   is_new_valid_to type ys_buper
                                   it_cobrb type yt_cobrb
                          changing cd_subrc like sy-subrc
                                   ct_cobrb_ins type yt_cobrb.

  data: ls_cobrb like cobrb,
        lb_append type c,
        ld_max_lfdnr like cobrb-lfdnr,
        ld_max_lfdnr_versn like cobrb-lfdnr,
        ls_tkb1a like tkb1a.

  perform copy_rule_prepare using id_objnr id_versn_to
                        changing ld_max_lfdnr
                                 ld_max_lfdnr_versn
                                 ls_tkb1a.

  loop at it_cobrb into ls_cobrb where avorg = 'KOAP'
                                   and versn = id_versn_fr.
    perform copy_one using id_versn_to
                           is_new_valid_from is_new_valid_to
                           ls_tkb1a
                  changing ls_cobrb
                           ld_max_lfdnr ld_max_lfdnr_versn
                           cd_subrc.
    append ls_cobrb to ct_cobrb_ins.
    lb_append = 'X'.
  endloop.

* no rule found
  if sy-subrc <> 0 or lb_append is initial.
    perform message_store using 'KA' 'I' '600'
         id_objnr ':No rule in versn' id_versn_fr space.    "#EC NOTEXT
    cd_subrc = 4.
  endif.
* success message
  if sy-subrc = 0 and lb_append = 'X'.
    perform message_store using 'KA' 'S' '600'
         id_objnr ': Rule copied.' space space.             "#EC NOTEXT
  endif.

endform.                               " COPY_RULES_FROM_PLAN

*&---------------------------------------------------------------------*
*&      Form  COPY_RULE_PREPARE
*&---------------------------------------------------------------------*
form copy_rule_prepare using    id_objnr like auak-objnr
                                id_versn_to like auak-versn
                       changing cd_max_lfdnr like cobrb-lfdnr
                                cd_max_lfdnr_versn like cobrb-lfdnr
                                cs_tkb1a like tkb1a.

  statics: ss_tkb1a like tkb1a.

  clear: cd_max_lfdnr, cd_max_lfdnr_versn, cs_tkb1a.
  select max( lfdnr ) from cobrb into cd_max_lfdnr
                           where objnr = id_objnr
                             and bureg = 0.
  select count( * ) into cd_max_lfdnr_versn from cobrb
                           where objnr = id_objnr
                             and avorg = 'KOAP'
                             and versn = id_versn_to
                             and bureg = 0.
  select single aprof from cobra into cs_tkb1a-aprof
                          where objnr = id_objnr.
  if sy-subrc = 0.
    if ss_tkb1a-aprof <> cs_tkb1a-aprof.
      select single * from tkb1a into ss_tkb1a
                where aprof = cs_tkb1a-aprof.
    endif.
    cs_tkb1a = ss_tkb1a.
  endif.
endform.                               " COPY_RULE_PREPARE

*&---------------------------------------------------------------------*
*&      Form  COPY_RULES_FROM_ACTUAL
*&---------------------------------------------------------------------*
form copy_rules_from_actual using    id_objnr like auak-objnr
                                     id_versn_to like auak-versn
                                     ib_ful type c
                                     is_new_valid_from type ys_buper
                                     is_new_valid_to type ys_buper
                                     it_cobrb type yt_cobrb
                            changing cd_subrc like sy-subrc
                                     ct_cobrb_ins type yt_cobrb.

  data: ls_cobrb like cobrb,
        lb_append type c,
        ld_max_lfdnr like cobrb-lfdnr,
        ld_max_lfdnr_versn like cobrb-lfdnr,
        ls_tkb1a like tkb1a.

  perform copy_rule_prepare using id_objnr id_versn_to
                        changing ld_max_lfdnr
                                 ld_max_lfdnr_versn
                                 ls_tkb1a.

  loop at it_cobrb into ls_cobrb where avorg = 'KOAO'
                                    or avorg is initial.
*   special checks
    check not ls_cobrb-prozs is initial or
          not ls_cobrb-aqzif is initial.
    if ib_ful = 'X'.
      check ls_cobrb-perbz = 'GES'.
    else.
      check ls_cobrb-perbz = 'PER' or ls_cobrb-perbz = 'VOR'.
    endif.
    check ls_cobrb-konty = 'KS' or
          ls_cobrb-konty = 'BP' or
          ls_cobrb-konty = 'OR' or
          ls_cobrb-konty = 'PR' or
          ls_cobrb-konty = 'EO'.
*   copy rule
    perform copy_one using id_versn_to
                           is_new_valid_from is_new_valid_to
                           ls_tkb1a
                  changing ls_cobrb
                           ld_max_lfdnr ld_max_lfdnr_versn
                           cd_subrc.
    if not cd_subrc is initial.
      exit.
    endif.
    append ls_cobrb to ct_cobrb_ins.
    lb_append = 'X'.
  endloop.
* no rule found
  if sy-subrc <> 0 or lb_append is initial.
    perform message_store using 'KA' 'I' '600'
         id_objnr ':No rule to copy.' space space.          "#EC NOTEXT
    cd_subrc = 4.
  endif.
* success message
  if sy-subrc = 0 and lb_append = 'X'.
    perform message_store using 'KA' 'S' '600'
         id_objnr ': Rule copied.' space space.             "#EC NOTEXT
  endif.

endform.                               " COPY_RULES_FROM_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  read_sender_or
*&---------------------------------------------------------------------*
form read_sender_or using    id_objnr like auak-objnr
                             id_kokrs like auak-kokrs
                    changing cb_astkz type c
                             cb_plint type c
                             cd_subrc like sy-subrc.

  data: ls_aufkv like aufkv.
  clear: cb_astkz, cb_plint, cd_subrc.
  call function 'OBJECT_KEY_GET_OR'
       exporting
            objnr  = id_objnr
       importing
            aufnr  = ls_aufkv-aufnr
       exceptions
            others = 1.
  if sy-subrc <> 0.
    perform message_store using sy-msgid sy-msgty sy-msgno
                                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    cd_subrc = 4.
    exit.
  endif.
  call function 'K_ORDER_CHECK'
       exporting
            aufnr      = ls_aufkv-aufnr
            test_kokrs = id_kokrs
            test_vrgng = 'KOAP'
       importing
            i_aufkv    = ls_aufkv
       exceptions
            others     = 1.
  if sy-subrc <> 0.
    perform message_store using sy-msgid sy-msgty sy-msgno
                                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    cd_subrc = 4.
    exit.
  endif.
  cb_astkz = ls_aufkv-astkz.
  cb_plint = ls_aufkv-plint.

endform.                               " read_sender_or
*&---------------------------------------------------------------------*
*&      Form  read_sender_pr
*&---------------------------------------------------------------------*
form read_sender_pr using    id_objnr like auak-objnr
                    changing cb_astkz type c
                             cb_plint type c
                             cd_subrc like sy-subrc.

  data: ls_prps like prps.
  clear: cb_astkz, cb_plint, cd_subrc.
  call function 'PS_STAT_INTEGRATED_ACT_CHECK'
       exporting
            check_only    = 'X'
            i_objnr       = id_objnr
            i_vrgng       = 'KOAP'
            no_popup      = 'X'
       importing
            e_prps        = ls_prps
       exceptions
            error_message = 1
            others        = 2.
  if sy-subrc <> 0.
    perform message_store using sy-msgid sy-msgty sy-msgno
                                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    cd_subrc = 4.
    exit.
  endif.
  cb_astkz = ls_prps-xstat.
  cb_plint = ls_prps-plint.

endform.                               " read_sender_pr
*&---------------------------------------------------------------------*
*&      Form  check_rule_exist
*&---------------------------------------------------------------------*
form check_rule_exist using    it_cobrb type yt_cobrb
                               id_versn_to like auak-versn
                               id_kokrs like auak-kokrs
                               id_gjahr like auak-gjahr
                      changing cs_new_valid_from type ys_buper
                               cs_new_valid_to type ys_buper
                               cd_subrc like sy-subrc.

  data: lb_rule_exist type c,
        ls_buper_from type ys_buper,
        ls_buper_to type ys_buper,
        ls_act_from type ys_buper,
        ls_act_to type ys_buper,
        ls_max_buper type ys_buper,
        ls_min_buper type ys_buper,
        ls_cobrb like cobrb.

  clear: cs_new_valid_from, cs_new_valid_to, cd_subrc.

  ls_act_from-gjahr = id_gjahr.
  ls_act_from-perio = '001'.
  ls_act_to-gjahr = id_gjahr.
  ls_act_to-perio = '012'.
  loop at it_cobrb into ls_cobrb
                   where versn = id_versn_to
                     and avorg = 'KOAP'.
    ls_buper_from-gjahr = ls_cobrb-gabja.
    ls_buper_from-perio = ls_cobrb-gabpe.
    ls_buper_to-gjahr = ls_cobrb-gbisj.
    ls_buper_to-perio = ls_cobrb-gbisp.
    if ( ls_buper_to >= ls_act_to or                      "#EC PORTABLE
         ls_buper_to is initial ) and
         ls_buper_from <= ls_act_from.
      lb_rule_exist = 'X'.
      exit.
    else.
      if ls_max_buper < ls_buper_to.                      "#EC PORTABLE
        ls_max_buper = ls_buper_to.
      endif.
      if ls_min_buper > ls_buper_from                     "#EC PORTABLE
         or ls_min_buper is initial.
        ls_min_buper = ls_buper_from.
      endif.
    endif.
  endloop.

* rule exists
  if lb_rule_exist = 'X'.
    perform message_store using 'KA' 'I' '600'
      ls_cobrb-objnr ':Rule already exists.' space space.   "#EC NOTEXT
    cd_subrc = 4.
    exit.
  endif.
  if ( ls_min_buper <= ls_act_from and ls_max_buper >= ls_act_from )
   or ( ls_min_buper <= ls_act_to and ls_max_buper >= ls_act_to )
   or ( ls_min_buper > ls_act_from and ls_max_buper < ls_act_to ).
    perform message_store using 'KA' 'I' '600'
      ls_cobrb-objnr ':Rule cannot be created.' space space."#EC NOTEXT
    cd_subrc = 4.
    exit.
  endif.

  if ls_max_buper <= ls_act_to.
    clear ls_min_buper.
  elseif ls_min_buper >= ls_act_from.
    clear ls_max_buper.
  endif.

* get next period
  if not ls_max_buper is initial.
    call function 'K_PERIOD_CHECK_IN_KOKRS'
         exporting
              i_perio          = ls_max_buper-perio
              i_gjahr          = ls_max_buper-gjahr
              i_kokrs          = id_kokrs
         importing
              e_next_perio     = cs_new_valid_from-perio
              e_next_gjahr     = cs_new_valid_from-gjahr
         exceptions
              period_not_valid = 1
              kokrs_not_found  = 2
              others           = 3.
    if sy-subrc <> 0.
      perform message_store using sy-msgid sy-msgty sy-msgno
                                  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      cd_subrc = 4.
    endif.
  endif.
* get previous period
  if not ls_min_buper is initial.
    call function 'K_PERIOD_CHECK_IN_KOKRS'
         exporting
              i_perio          = ls_min_buper-perio
              i_gjahr          = ls_min_buper-gjahr
              i_kokrs          = id_kokrs
         importing
              e_previous_perio = cs_new_valid_to-perio
              e_previous_gjahr = cs_new_valid_to-gjahr
         exceptions
              period_not_valid = 1
              kokrs_not_found  = 2
              others           = 3.
    if sy-subrc <> 0.
      perform message_store using sy-msgid sy-msgty sy-msgno
                                  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      cd_subrc = 4.
    endif.
  endif.

endform.                               " check_rule_exist
*&---------------------------------------------------------------------*
*&      Form  copy_one
*&---------------------------------------------------------------------*
form copy_one using    id_versn_to like auak-versn
                       is_new_valid_from type ys_buper
                       is_new_valid_to type ys_buper
                       is_tkb1a like tkb1a
              changing cs_cobrb like cobrb
                       cd_max_lfdnr like cobrb-lfdnr
                       cd_max_lfdnr_versn like cobrb-lfdnr
                       cd_subrc like sy-subrc.

  data: ls_from_buper type ys_buper,
        ls_to_buper type ys_buper.

  cs_cobrb-avorg = 'KOAP'.
  cs_cobrb-versn = id_versn_to.
  clear: cs_cobrb-ersja, cs_cobrb-erspe,
         cs_cobrb-letja, cs_cobrb-letpe.
  ls_from_buper-gjahr = cs_cobrb-gabja.
  ls_from_buper-perio = cs_cobrb-gabpe.
  if is_new_valid_from > ls_from_buper.                   "#EC PORTABLE
    cs_cobrb-gabja = is_new_valid_from-gjahr.
    cs_cobrb-gabpe = is_new_valid_from-perio.
  endif.
  ls_to_buper-gjahr = cs_cobrb-gbisj.
  ls_to_buper-perio = cs_cobrb-gbisp.
  if is_new_valid_to > ls_from_buper.                     "#EC PORTABLE
    cs_cobrb-gbisj = is_new_valid_to-gjahr.
    cs_cobrb-gbisp = is_new_valid_to-perio.
  endif.
  add 1 to cd_max_lfdnr.
  add 1 to cd_max_lfdnr_versn.
  if cd_max_lfdnr_versn > is_tkb1a-maxbr
     or cd_max_lfdnr > 999 or cd_max_lfdnr is initial.
    perform message_store using 'KD' 'E' '292'
      is_tkb1a-maxbr is_tkb1a-aprof space space.
    cd_subrc = 4.
  endif.
  cs_cobrb-lfdnr = cd_max_lfdnr.
  cs_cobrb-extnr = cd_max_lfdnr_versn.

endform.                               " copy_one
*&---------------------------------------------------------------------*
*&      Form  select_senders
*&---------------------------------------------------------------------*
form select_senders changing ct_objnr type yt_objnr.

  refresh ct_objnr.

  perform sender_selection tables    it_sender
                           changing  hlp_selart
                                     hlp_variant
                                     hlp_one.

  call function 'K_OBJECT_SELECTION_CRIT_FILL'
       exporting
            selart    = hlp_selart
            oneobject = hlp_one
            variant   = hlp_variant
       tables
            it_cosel2 = it_sender.

  call function 'K_OBJECT_SELECTION_RUN'
       exporting
            i_kokrs  = kokrs
            i_vrgng  = 'KOAO'
       tables
            it_objnr = ct_objnr.
  if ct_objnr[] is initial.
    perform message_store using 'KD' 'I' '212' kokrs space space space.
  endif.
  sort ct_objnr by objnr.

endform.                               " select_senders

