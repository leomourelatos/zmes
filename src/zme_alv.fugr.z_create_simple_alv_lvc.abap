FUNCTION z_create_simple_alv_lvc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PROG) TYPE  SYCPROG DEFAULT SY-REPID
*"     REFERENCE(I_TABNAME) TYPE  TABNAME
*"     REFERENCE(I_INTTAB) TYPE  TABNAME
*"     REFERENCE(I_SEL) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_CELLCOL) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_NOT_OPTIMIZE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DATA_CHANGE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_BUTTON) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_CALLER_EXIT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_NO_TOTLINE) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_TOTALS_BEFORE) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_NO_ZERO) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_ZEBRA) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(I_BACKGROUND_ID) TYPE  SDYDO_KEY DEFAULT SPACE
*"     REFERENCE(I_VARIANT) TYPE  DISVARIANT OPTIONAL
*"     REFERENCE(I_GRID_TITLE) TYPE  LVC_TITLE OPTIONAL
*"     REFERENCE(I_CHANGE_FORM) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_USER_COMMAND) TYPE  SLIS_FORMNAME DEFAULT
*"       'USER_COMMAND'
*"     REFERENCE(I_PF_STATUS) TYPE  SLIS_FORMNAME DEFAULT
*"       'R0_SET_PF_STATUS'
*"     REFERENCE(I_TOP_OF_PAGE) TYPE  CHAR30 DEFAULT 'TOP_OF_PAGE'
*"     REFERENCE(I_SCREEN_START_COLUMN) DEFAULT 0
*"     REFERENCE(I_SCREEN_START_LINE) DEFAULT 0
*"     REFERENCE(I_SCREEN_END_COLUMN) DEFAULT 0
*"     REFERENCE(I_SCREEN_END_LINE) DEFAULT 0
*"     REFERENCE(I_EVENT_EXIT) TYPE  SLIS_T_EVENT_EXIT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_EXIT_CAUSED_BY_USER) TYPE  SLIS_EXIT_BY_USER
*"  TABLES
*"      T_INTTAB_D
*"      T_INTTAB_HEAD TYPE  SLIS_T_LISTHEADER OPTIONAL
*"      T_INTTAB_SUM TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_SORT TYPE  LVC_T_SORT OPTIONAL
*"      T_INTTAB_SFIELDS TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_HIDE TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_HEAD_T TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_EDIT TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_CONCAT TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_EXCLUDE TYPE  SLIS_T_EXTAB OPTIONAL
*"      T_INTTAB_INCLUDE TYPE  SLIS_T_EXTAB OPTIONAL
*"      T_INTTAB_INCLUDEV TYPE  ZME_EXTAB_TBL OPTIONAL
*"      T_INTTAB_HOTSPOT TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_CHECKBOX TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_REPLACE TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_FIXED TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_NOEDIT TYPE  ZME_ALV_NO_EDIT_TBL OPTIONAL
*"      T_INTTAB_HYPE TYPE  LVC_T_HYPE OPTIONAL
*"      T_INTTAB_NOSUM TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"      T_INTTAB_ORDER TYPE  SLIS_T_FIELDCAT_ALV OPTIONAL
*"----------------------------------------------------------------------

  TYPE-POOLS:
    abap.

  DATA:
    lvc_fcat        TYPE lvc_t_fcat,
    lta_fcat        TYPE slis_t_fieldcat_alv,
    wa_fcat         TYPE slis_fieldcat_alv,
    i_grid_settings TYPE lvc_s_glay,
    lst_layout      TYPE lvc_s_layo,
*    co_rout_pf_stat TYPE char30    VALUE 'SET_PF_STATUS',
    ltp_title       TYPE lvc_title,
    i_events        TYPE slis_alv_event OCCURS 0 WITH HEADER LINE,
    it_events       TYPE slis_t_event,
    lta_sort_alv    TYPE slis_t_sortinfo_alv,
    extab           TYPE slis_t_extab.

  DATA:
    fcat TYPE LINE OF lvc_t_fcat.

  DATA:
    cellline   TYPE lvc_s_styl,
    celltab    TYPE lvc_t_styl,
    celltab_in TYPE lvc_t_styl,
    cell       TYPE lvc_s_styl OCCURS 0 WITH HEADER LINE,
    dp_index   LIKE sy-tabix.

  FIELD-SYMBOLS:
    <inttab_d>,
    <test_field1>,
    <test_field2>,
    <test_field3>,
    <test_field5>.
  DATA:
    test_field1(100) VALUE '<INTTAB_D>-ZCHECK',
    test_field2(100) VALUE '<INTTAB_D>-CELLTAB',
    test_field3(100) VALUE '<INTTAB_D>-',
    test_field4(100) VALUE 'BUTTON*',
    test_field5(100).

  DATA:
    p_noedit LIKE zme_alv_no_edit.

  RANGES:
    r_noedit FOR zme_alv_no_edit-name.

  LOOP AT t_inttab_noedit INTO p_noedit.
    CLEAR r_noedit.
    r_noedit-sign = 'I'.
    r_noedit-option = 'EQ'.
    r_noedit-low = p_noedit-name.
    APPEND r_noedit.
  ENDLOOP.
  IF r_noedit IS INITIAL.
    CLEAR r_noedit.
    r_noedit-sign = 'I'.
    r_noedit-option = 'EQ'.
    r_noedit-low = ' '.
    APPEND r_noedit.
  ENDIF.

  IF i_prog = ' '.
    ASSIGN sy-repid TO <prog_name>.
  ELSE.
    ASSIGN i_prog TO <prog_name>.
  ENDIF.

  ltp_title = i_grid_title.

  FREE lta_fcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = i_tabname
      i_bypassing_buffer     = abap_true
    CHANGING
      ct_fieldcat            = lta_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lta_fcat INTO wa_fcat
      WHERE no_out = 'X'.
      CLEAR wa_fcat-no_out.
      MODIFY lta_fcat FROM wa_fcat.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lta_fcat
    IMPORTING
      et_fieldcat_lvc = lvc_fcat
    TABLES
      it_data         = t_inttab_sum.
  IF sy-subrc <> 0.

  ENDIF.

  DATA: col_pos        TYPE sy-cucol,
        order_lines(5) TYPE n.

  CLEAR order_lines.
  DESCRIBE TABLE t_inttab_order LINES order_lines.

  IF order_lines > 0.

    SORT t_inttab_order BY col_pos ASCENDING.

    LOOP AT t_inttab_order.

      SORT lvc_fcat        BY col_pos ASCENDING.

      LOOP AT lvc_fcat INTO fcat
        WHERE col_pos >= t_inttab_order-col_pos.
        fcat-col_pos = fcat-col_pos + 1.
        MODIFY lvc_fcat FROM fcat.
      ENDLOOP.

      LOOP AT lvc_fcat INTO fcat
        WHERE fieldname = t_inttab_order-fieldname.
        fcat-col_pos = t_inttab_order-col_pos.
        MODIFY lvc_fcat FROM fcat.
      ENDLOOP.
    ENDLOOP.

*    sort lv_inttab_order by col_pos descending.
*
*    read table lv_inttab_order index 1.
*    col_pos = lv_inttab_order-col_pos.
*
*    sort lvc_fcat by col_pos ascending.
*
*    loop at lvc_fcat into fcat
**      where col_pos > col_pos
*      .
*      read table lv_inttab_order
*        with key fieldname = fcat-fieldname.
*      if sy-subrc <> '0'.
*        col_pos = col_pos + 1.
*        fcat-col_pos = col_pos.
*      endif.
*      modify lvc_fcat from fcat.
*    endloop.
  ENDIF.


*  LOOP AT lvc_fcat INTO fcat WHERE fieldname = 'ZURL'.
*    fcat-web_field = 'ZURL'.
*    MODIFY lvc_fcat FROM fcat.
*  ENDLOOP.

*  LOOP AT lvc_fcat INTO fcat WHERE fieldname = '/BIC/FILE'.
*    fcat-web_field = 'ZURL'.
*    MODIFY lvc_fcat FROM fcat.
*  ENDLOOP.

  CLEAR   i_grid_settings .
  i_grid_settings-coll_top_p = ' '.
  i_grid_settings-coll_end_l = 'X'.
  i_grid_settings-edt_cll_cb = 'X'.

  lst_layout-no_totline      = i_no_totline.
  lst_layout-zebra           = i_zebra.
  lst_layout-numc_total      = abap_false.
  IF i_not_optimize = ' '.
    lst_layout-cwidth_opt      = abap_true.
  ELSEIF i_not_optimize = 'X'.
    lst_layout-cwidth_opt      = abap_false.
  ENDIF.
  lst_layout-totals_bef = i_totals_before.
*  lst_layout-box_fname       = lv_inttab.
  lst_layout-stylefname = 'CELLTAB'.
  lst_layout-info_fname = 'ROWCOL'.
*  lst_layout-sel_mode = 'D'.
  IF i_cellcol = 'X'.
    lst_layout-ctab_fname = 'CELLCOL'.
  ENDIF.

  IF i_sel = 'X'.
    lst_layout-box_fname = 'SEL'.
  ENDIF.

*  lst_layout-confirmation_prompt = 'X'.

  IF i_no_zero = 'X'.
    LOOP AT lvc_fcat INTO fcat WHERE inttype = 'P' OR inttype = 'T'.
      fcat-no_zero = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDIF.

  LOOP AT t_inttab_sum.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_sum-fieldname.
      fcat-do_sum = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_sfields.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_sfields-fieldname.
      fcat-txt_field = t_inttab_sfields-text_fieldname.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_hide.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_hide-fieldname.
      fcat-no_out = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_head_t.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_head_t-fieldname.
      fcat-colddictxt = t_inttab_head_t-ddictxt.
      fcat-icon = t_inttab_head_t-icon.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_edit.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_edit-fieldname.
      fcat-edit = 'X'.
*      fcat-f4availabl = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_checkbox.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_checkbox-fieldname.
      fcat-checkbox = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_hotspot.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_hotspot-fieldname.
      fcat-hotspot = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_nosum.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_nosum-fieldname.
      fcat-no_sum = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_replace.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_replace-fieldname.
      IF t_inttab_replace-seltext_l <> ' '.
        fcat-scrtext_l  = t_inttab_replace-seltext_l.
        fcat-scrtext_m  = t_inttab_replace-seltext_m.
        fcat-scrtext_s  = t_inttab_replace-seltext_s.
        fcat-reptext  = t_inttab_replace-seltext_l.
      ENDIF.
      IF t_inttab_replace-ddictxt <> ' '.
        fcat-colddictxt = t_inttab_replace-ddictxt.
      ENDIF.
      IF t_inttab_replace-outputlen <> ' '.
        fcat-outputlen  = t_inttab_replace-outputlen.
      ENDIF.
      IF t_inttab_replace-icon <> ' '.
        fcat-icon  = t_inttab_replace-icon.
      ENDIF.
      IF t_inttab_replace-just <> ' '.
        fcat-just  = t_inttab_replace-just.
      ENDIF.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_concat.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_concat-fieldname.
      CONCATENATE fcat-scrtext_l t_inttab_concat-seltext_l
      INTO fcat-scrtext_l
      SEPARATED BY space.
      CONCATENATE fcat-scrtext_m t_inttab_concat-seltext_m
      INTO fcat-scrtext_m
      SEPARATED BY space.
      CONCATENATE fcat-scrtext_s t_inttab_concat-seltext_s
      INTO fcat-scrtext_s
      SEPARATED BY space.
      fcat-colddictxt = t_inttab_concat-ddictxt.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

  LOOP AT t_inttab_fixed.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname = t_inttab_fixed-fieldname.
      fcat-key = 'X'.
      MODIFY lvc_fcat FROM fcat.
    ENDLOOP.
  ENDLOOP.

*    loop at lvc_fcat into fcat
*      where fieldname = 'FILE_DESCR'.
*      fcat-web_field = 'FILE_HANDLE'.
*      modify lvc_fcat from fcat.
*    endloop.


*  FREE extab.
*  CLEAR extab.
*  PERFORM make_extab TABLES t_inttab_include
*                            t_inttab_exclude
*                            t_inttab_includev
*                            extab.

  IF i_button = 'X'.
    SORT lvc_fcat BY fieldname.
    LOOP AT lvc_fcat INTO fcat
      WHERE fieldname CP test_field4.

      CONCATENATE test_field3 fcat-fieldname INTO test_field5.
      FREE celltab.
      cellline-fieldname = fcat-fieldname.
      cellline-style = cl_gui_alv_grid=>mc_style_button.
      APPEND cellline TO celltab.
      LOOP AT t_inttab_d ASSIGNING <inttab_d>.
        FREE celltab_in.
        ASSIGN (test_field2) TO <test_field2>.
        celltab_in = <test_field2>.
        APPEND LINES OF celltab TO celltab_in.
        ASSIGN (test_field5) TO <test_field5>.
        IF <test_field5> <> ' '.
          <test_field2> = celltab_in.
        ELSE.
*          CLEAR <test_field2>.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT lvc_fcat.
  ENDIF.

  IF i_caller_exit = 'X'.
    i_events-name = 'CALLER_EXIT'.
    i_events-form = 'CALLER_EXIT'.
    APPEND i_events.
  ENDIF.


  i_events-name = 'AFTER_REFRESH'.
  i_events-form = 'AFTER_REFRESH'.
  APPEND i_events.
*
*  i_events-name = 'END_OF_LIST'.
*  i_events-form = 'END_OF_LIST'.
*  APPEND i_events.

  i_events-name = 'DATA_CHANGED'.
  i_events-form = i_change_form.
  APPEND i_events.

  FREE celltab.

  IF i_data_change = 'X'.
    cellline-style = cl_gui_alv_grid=>mc_style_disabled.
    SORT t_inttab_edit.
    LOOP AT t_inttab_edit.
      cellline-fieldname = t_inttab_edit-fieldname.
      APPEND cellline TO celltab.
    ENDLOOP.

    LOOP AT t_inttab_d ASSIGNING <inttab_d>.

      CLEAR: celltab_in, celltab_in[].

      CLEAR: cell, cell[].

      ASSIGN (test_field1) TO <test_field1>.
      CHECK  <test_field1> IS NOT INITIAL.
      ASSIGN (test_field2) TO <test_field2>.

      IF sy-subrc = 0.
        cell[] = <test_field2>.
      ENDIF.

      APPEND LINES OF celltab TO cell.

      SORT cell.
      DELETE ADJACENT DUPLICATES FROM cell.
      celltab_in = cell[].

      IF <test_field1> IN r_noedit.
        <test_field2> = celltab_in.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = <prog_name>
      i_background_id          = i_background_id
      i_callback_pf_status_set = i_pf_status
      i_callback_top_of_page   = i_top_of_page
      i_callback_user_command  = i_user_command
      i_grid_settings          = i_grid_settings
      i_grid_title             = ltp_title
      is_layout_lvc            = lst_layout
      it_events                = i_events[]
      it_event_exit            = i_event_exit
      it_fieldcat_lvc          = lvc_fcat[]
      it_sort_lvc              = t_inttab_sort[]
      it_excluding             = extab[]
      it_hyperlink             = t_inttab_hype[]
      i_default                = abap_true
      i_save                   = 'A' "x=global,u=user,a=all,' '=nosave
      is_variant               = i_variant
      i_screen_start_column    = i_screen_start_column
      i_screen_start_line      = i_screen_start_line
      i_screen_end_column      = i_screen_end_column
      i_screen_end_line        = i_screen_end_line
    IMPORTING
      es_exit_caused_by_user   = e_exit_caused_by_user
    TABLES
      t_outtab                 = t_inttab_d[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  make_extab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_extab TABLES l_inttab TYPE slis_t_extab
                       l_exctab  TYPE slis_t_extab
                       l_inttabv STRUCTURE zme_extab
                       l_extab.
  DATA:
    wa_extab TYPE slis_extab.

*  CLEAR:
*    lv_scr1,
*    lv_scr2,
*    lv_scr3,
*    lv_scr4,
*    lv_scr5,
*    lv_scr6,
*    lv_scr7,
*    lv_scr8,
*    lv_scr9,
*    lv_scr10.

  READ TABLE l_inttab WITH KEY fcode = 'COMPLETE'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'COMPLETE'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'POST'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'POST'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'CREATE'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'CREATE'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'EDIT'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'EDIT'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'PRINT'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'PRINT'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'INSERT'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'INSERT'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'REVERCE'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'REVERCE'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'DELETE'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'DELETE'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'PROCEED'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'PROCEED'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'REFRESH'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'REFRESH'.
    APPEND wa_extab TO l_extab.
  ENDIF.

  READ TABLE l_inttab WITH KEY fcode = 'EXPORT'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'EXPORT'.
    APPEND wa_extab TO l_extab.
  ENDIF.


  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON1'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON1'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr1-text      = l_inttabv-fcode_name.
    lv_scr1-icon_id   = l_inttabv-icon_id.
    lv_scr1-icon_text = l_inttabv-icon_text.
*    lv_scr1-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON2'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON2'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr2-text      = l_inttabv-fcode_name.
    lv_scr2-icon_id   = l_inttabv-icon_id.
    lv_scr2-icon_text = l_inttabv-icon_text.
*    lv_scr2-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON3'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON3'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr3-text      = l_inttabv-fcode_name.
    lv_scr3-icon_id   = l_inttabv-icon_id.
    lv_scr3-icon_text = l_inttabv-icon_text.
*    lv_scr3-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON4'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON4'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr4-text      = l_inttabv-fcode_name.
    lv_scr4-icon_id   = l_inttabv-icon_id.
    lv_scr4-icon_text = l_inttabv-icon_text.
*    lv_scr4-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON5'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON5'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr5-text      = l_inttabv-fcode_name.
    lv_scr5-icon_id   = l_inttabv-icon_id.
    lv_scr5-icon_text = l_inttabv-icon_text.
*    lv_scr5-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON6'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON6'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr6-text      = l_inttabv-fcode_name.
    lv_scr6-icon_id   = l_inttabv-icon_id.
    lv_scr6-icon_text = l_inttabv-icon_text.
*    lv_scr6-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON7'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON7'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr7-text      = l_inttabv-fcode_name.
    lv_scr7-icon_id   = l_inttabv-icon_id.
    lv_scr7-icon_text = l_inttabv-icon_text.
*    lv_scr7-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON8'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON8'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr8-text      = l_inttabv-fcode_name.
    lv_scr8-icon_id   = l_inttabv-icon_id.
    lv_scr8-icon_text = l_inttabv-icon_text.
*    lv_scr8-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON9'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON9'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr9-text      = l_inttabv-fcode_name.
    lv_scr9-icon_id   = l_inttabv-icon_id.
    lv_scr9-icon_text = l_inttabv-icon_text.
*    lv_scr9-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  READ TABLE l_inttabv WITH KEY fcode = 'BUTTON10'.
  IF sy-subrc <> 0.
    wa_extab-fcode = 'BUTTON10'.
    APPEND wa_extab TO l_extab.
  ELSE.
    lv_scr10-text      = l_inttabv-fcode_name.
    lv_scr10-icon_id   = l_inttabv-icon_id.
    lv_scr10-icon_text = l_inttabv-icon_text.
*    lv_scr10-QUICKINFO = l_inttabv-QUICKINFO.
  ENDIF.

  LOOP AT l_exctab.
    wa_extab-fcode = l_exctab-fcode.
    APPEND wa_extab TO l_extab.
  ENDLOOP.

ENDFORM.                    "make_extab
