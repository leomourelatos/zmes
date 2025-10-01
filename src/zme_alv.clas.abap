class ZME_ALV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_buttons,
        f01 TYPE rsfunc_txt,
        f02 TYPE rsfunc_txt,
        f03 TYPE rsfunc_txt,
        f04 TYPE rsfunc_txt,
        f05 TYPE rsfunc_txt,
        f06 TYPE rsfunc_txt,
        f07 TYPE rsfunc_txt,
        f08 TYPE rsfunc_txt,
        f09 TYPE rsfunc_txt,
        f10 TYPE rsfunc_txt,
        f11 TYPE rsfunc_txt,
        f12 TYPE rsfunc_txt,
        f13 TYPE rsfunc_txt,
        f14 TYPE rsfunc_txt,
        f15 TYPE rsfunc_txt,
        f16 TYPE rsfunc_txt,
        f17 TYPE rsfunc_txt,
        f18 TYPE rsfunc_txt,
        f19 TYPE rsfunc_txt,
*             f20 TYPE rsfunc_txt,
*             f21 TYPE rsfunc_txt,
*             f22 TYPE rsfunc_txt,
*             f23 TYPE rsfunc_txt,
*             f24 TYPE rsfunc_txt,
*             f25 TYPE rsfunc_txt,
*             f26 TYPE rsfunc_txt,
*             f27 TYPE rsfunc_txt,
*             f28 TYPE rsfunc_txt,
*             f29 TYPE rsfunc_txt,
*             f30 TYPE rsfunc_txt,
*             f31 TYPE rsfunc_txt,
*             f32 TYPE rsfunc_txt,
*             f33 TYPE rsfunc_txt,
*             f34 TYPE rsfunc_txt,
*             f35 TYPE rsfunc_txt,
      END OF t_buttons .
  types:
    BEGIN OF t_allowed_but,
        function TYPE sy-ucomm,
      END OF t_allowed_but .
  types:
    tt_excluded_but TYPE STANDARD TABLE OF sy-ucomm .
  types:
    tt_allowed_but TYPE STANDARD TABLE OF t_allowed_but .

  constants B_COMPLETE type SY-UCOMM value 'COMPLETE' ##NO_TEXT.
  constants B_ETA type SY-UCOMM value '&ETA' ##NO_TEXT.
  constants B_ALL type SY-UCOMM value '&ALL' ##NO_TEXT.
  constants B_SAL type SY-UCOMM value '&SAL' ##NO_TEXT.
  constants B_OUP type SY-UCOMM value '&OUP' ##NO_TEXT.
  constants B_ODN type SY-UCOMM value '&ODN' ##NO_TEXT.
  constants B_ILT type SY-UCOMM value '&ILT' ##NO_TEXT.
  constants B_UMC type SY-UCOMM value '&UMC' ##NO_TEXT.
  constants B_SUM type SY-UCOMM value '&SUM' ##NO_TEXT.
  constants B_VEXCEL type SY-UCOMM value '&VEXCEL' ##NO_TEXT.
  constants B_XXL type SY-UCOMM value '&XXL' ##NO_TEXT.
  constants B_PC type SY-UCOMM value '%PC' ##NO_TEXT.
  constants B_SL type SY-UCOMM value '%SL' ##NO_TEXT.
  constants B_OL0 type SY-UCOMM value '&OL0' ##NO_TEXT.
  constants B_OAD type SY-UCOMM value '&OAD' ##NO_TEXT.
  constants B_AVE type SY-UCOMM value '&AVE' ##NO_TEXT.
  constants B_01 type SY-UCOMM value 'F01' ##NO_TEXT.
  constants B_02 type SY-UCOMM value 'F02' ##NO_TEXT.
  constants B_03 type SY-UCOMM value 'F03' ##NO_TEXT.
  constants B_04 type SY-UCOMM value 'F04' ##NO_TEXT.
  constants B_05 type SY-UCOMM value 'F05' ##NO_TEXT.
  constants B_06 type SY-UCOMM value 'F06' ##NO_TEXT.
  constants B_07 type SY-UCOMM value 'F07' ##NO_TEXT.
  constants B_08 type SY-UCOMM value 'F08' ##NO_TEXT.
  constants B_09 type SY-UCOMM value 'F09' ##NO_TEXT.
  constants B_10 type SY-UCOMM value 'F10' ##NO_TEXT.
  constants B_11 type SY-UCOMM value 'F11' ##NO_TEXT.
  constants B_12 type SY-UCOMM value 'F12' ##NO_TEXT.
  constants B_13 type SY-UCOMM value 'F13' ##NO_TEXT.
  constants B_14 type SY-UCOMM value 'F14' ##NO_TEXT.
  constants B_15 type SY-UCOMM value 'F15' ##NO_TEXT.
  constants B_16 type SY-UCOMM value 'F16' ##NO_TEXT.
  constants B_17 type SY-UCOMM value 'F17' ##NO_TEXT.
  constants B_18 type SY-UCOMM value 'F18' ##NO_TEXT.
  constants B_19 type SY-UCOMM value 'F19' ##NO_TEXT.
  constants PROGRAM_NAME type PROGNAME value 'ZME_DYNAMIC_GUI_STATUS' ##NO_TEXT.
  class-data ALLOWED_BUTTONS type TT_ALLOWED_BUT .
  class-data BUTTONS type T_BUTTONS .
  class-data EXCLUDED_BUTTONS type TT_EXCLUDED_BUT .
  constants B_SEPARATOR type CHAR1 value '|' ##NO_TEXT.

  class-methods Z_ALV_BUTTON_CREATE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods Z_ALV_BUTTON_DELETE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods Z_ALV_COLOR_CREATE
    importing
      !I_COLOR type LVC_COL
      !I_INT type LVC_INT default '0'
      !I_INV type LVC_INV default '0'
      !I_COLTAB type LVC_T_SCOL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_COLTAB type LVC_T_SCOL .
  class-methods Z_ALV_COLOR_DELETE
    importing
      !I_COLTAB type LVC_T_SCOL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_COLTAB type LVC_T_SCOL .
  class-methods Z_ALV_EDIT_CREATE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods Z_ALV_EDIT_DELETE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods Z_ALV_HOTSPOT_CREATE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods Z_ALV_HOTSPOT_DELETE
    importing
      !I_CELLTAB type LVC_T_STYL
      !I_FIELDNAME type LVC_FNAME
    exporting
      !E_CELLTAB type LVC_T_STYL .
  class-methods CLASS_CONSTRUCTOR .
  class-methods ADD_BUTTON
    importing
      value(IV_BUTTON) type SY-UCOMM
      value(IV_TEXT) type SMP_DYNTXT-TEXT optional
      value(IV_ICON) type SMP_DYNTXT-ICON_ID optional
      value(IV_QINFO) type SMP_DYNTXT-QUICKINFO optional
      value(IV_ALLOWED) type ABAP_BOOL default ABAP_TRUE
    exceptions
      BUTTON_ALREADY_FILLED
      BUTTON_DOES_NOT_EXISTS
      ICON_AND_TEXT_EMPTY .
  class-methods HIDE_BUTTON
    importing
      value(IV_BUTTON) type SY-UCOMM .
  class-methods SHOW_BUTTON
    importing
      value(IV_BUTTON) type SY-UCOMM .
  class-methods GET_TOOLBAR
    exporting
      !E_TOOLBAR type T_BUTTONS .
  class-methods ADD_SEPARATOR
    importing
      value(IV_BUTTON) type SY-UCOMM .
  class-methods SHOW_TITLE
    importing
      value(IV_TEXT1) type STRING
      value(IV_TEXT2) type STRING optional
      value(IV_TEXT3) type STRING optional
      value(IV_TEXT4) type STRING optional
      value(IV_TEXT5) type STRING optional .
  class-methods SHOW_GUI_STATUS
    importing
      !I_STATUS type STRING default 'DEFAULT'
      !I_EXCLUDE type SLIS_T_EXTAB optional .
  class-methods TOP_OF_PAGE
    importing
      value(TA_COMMENT) type SLIS_T_LISTHEADER .
  class-methods Z_UPLOAD_FILE
    importing
      !I_FILE type STRING
      !I_HEADER type CHAR1 default 'X'
      !I_FILETYPE type CHAR3 default 'XLS'
    changing
      !C_INPUT type STANDARD TABLE .
  class-methods Z_UPLOAD_FILE_V2
    importing
      !I_FILE type STRING
      !I_HEADER type CHAR1 default 'X'
      !I_FILETYPE type CHAR3 default 'XLS'
      !I_COLUMNS type I optional
    exporting
      !E_FILENAME type STRING
    changing
      !C_INPUT type STANDARD TABLE .
  class-methods SHOW_ALV
    importing
      !I_REPID type SY-REPID default SY-REPID
      !I_TABNAME type TABNAME
      !I_NOZERO type CHAR1 optional
      !I_NO_TOTLINE type CHAR1 optional
      !I_ZEBRA type CHAR1 default 'X'
      !I_OPTIMIZE type CHAR1 default 'X'
      !I_TOTALS_BEFORE type CHAR1 default ' '
      !T_HYPE type LVC_T_HYPE optional
      !T_ORDER type SLIS_T_FIELDCAT_ALV optional
    changing
      !T_DATA type STANDARD TABLE
      !T_HEADER type SLIS_T_LISTHEADER optional
      !T_SUM type SLIS_T_FIELDCAT_ALV optional
      !T_NOSUM type SLIS_T_FIELDCAT_ALV optional
      !T_SORT type LVC_T_SORT optional
      !T_SFIELDS type SLIS_T_FIELDCAT_ALV optional
      !T_HIDE type SLIS_T_FIELDCAT_ALV optional
      !T_HEAD_T type SLIS_T_FIELDCAT_ALV optional
      !T_EDIT type SLIS_T_FIELDCAT_ALV optional
      !T_CONCAT type SLIS_T_FIELDCAT_ALV optional
      !T_EXCLUDE type SLIS_T_EXTAB optional
      !T_INCLUDE type ZME_INCLUDE_FCODE_TBL optional
      !T_HOTSPOT type SLIS_T_FIELDCAT_ALV optional
      !T_CHECKBOX type SLIS_T_FIELDCAT_ALV optional
      !T_REPLACE type SLIS_T_FIELDCAT_ALV optional
      !T_FIXED type SLIS_T_FIELDCAT_ALV optional .
  class-methods HANDLE_BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO
      !SENDER .
  class-methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  class-methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  class-methods TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  class-methods HANDLE_DATA_CHANGE
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM .
  PROTECTED SECTION.
private section.

  class-data LV_SCR1 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR2 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR3 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR4 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR5 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR6 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR7 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR8 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR9 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_SCR10 type SMP_DYNTXT value ' ' ##NO_TEXT.
  class-data LV_INTTAB_HEAD type SLIS_T_LISTHEADER .
  class-data TA_COMMENT type SLIS_T_LISTHEADER .
ENDCLASS.



CLASS ZME_ALV IMPLEMENTATION.


  METHOD add_button.
    DATA button TYPE smp_dyntxt.
    CHECK iv_button IS NOT INITIAL.

    IF iv_text IS INITIAL AND iv_icon IS INITIAL.
      RAISE icon_and_text_empty.
      RETURN.
    ENDIF.

    button-icon_id   = iv_icon.
    button-icon_text = iv_text.
    button-text      = iv_text.
    button-quickinfo = iv_qinfo.

    ASSIGN COMPONENT iv_button OF STRUCTURE buttons TO FIELD-SYMBOL(<bt>).
    IF <bt> IS ASSIGNED.
      IF <bt> IS INITIAL.
        <bt> = button.
        IF iv_allowed EQ abap_true.
          show_button( iv_button = iv_button ).
        ENDIF.
      ELSE.
        RAISE button_already_filled.
      ENDIF.
    ELSE.
      RAISE button_does_not_exists.
    ENDIF.
  ENDMETHOD.


  METHOD add_separator.

    add_button(
      EXPORTING
        iv_button              = iv_button
*        iv_text                = |{ cl_abap_char_utilities=>minchar }|
        iv_text                = |{ zme_alv=>b_separator }|
*        iv_icon                = iv_icon
*        iv_qinfo               = iv_qinfo
         iv_allowed             = abap_true
      EXCEPTIONS
        button_already_filled  = 1
        button_does_not_exists = 2
        icon_and_text_empty    = 3
        OTHERS                 = 4
    ).
    IF sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    excluded_buttons = VALUE #(
                                ( b_01 )
                                ( b_02 )
                                ( b_03 )
                                ( b_04 )
                                ( b_05 )
                                ( b_06 )
                                ( b_07 )
                                ( b_08 )
                                ( b_09 )
                                ( b_10 )
                                ( b_11 )
                                ( b_12 )
                                ( b_13 )
                                ( b_14 )
                                ( b_15 )
                                ( b_16 )
                                ( b_17 )
                                ( b_18 )
                                ( b_19 )
*                                ( b_complete )
*                                ( b_eta )
*                                ( b_all )
*                                ( b_sal )
*                                ( b_oup )
*                                ( b_odn )
*                                ( b_ilt )
*                                ( b_umc )
*                                ( b_sum )
*                                ( b_vexcel )
*                                ( b_xxl )
*                                ( b_pc )
*                                ( b_sl )
*                                ( b_ol0 )
*                                ( b_oad )
*                                ( b_ave )
                                          ).
  ENDMETHOD.


  METHOD get_toolbar.
    e_toolbar = buttons.
  ENDMETHOD.


  METHOD handle_button_click.

    BREAK lmourelatos.

  ENDMETHOD.


  method HANDLE_DATA_CHANGE.
  endmethod.


  method HANDLE_HOTSPOT_CLICK.
  endmethod.


  method HANDLE_USER_COMMAND.
  endmethod.


  METHOD hide_button.
    CHECK iv_button IS NOT INITIAL.
    IF line_exists( allowed_buttons[ function = iv_button ] ).
      DELETE allowed_buttons WHERE function = iv_button.
      APPEND iv_button TO excluded_buttons.
    ENDIF.
  ENDMETHOD.


  METHOD show_alv.

    DATA:
      lt_fcat TYPE lvc_t_fcat,
      ls_fcat TYPE lvc_s_fcat.
    DATA:
      ls_layout TYPE lvc_s_layo.
*      lv_ucomm  TYPE sy-ucomm.

    DATA:
      it_exclude TYPE ui_functions,
      is_exclude TYPE ui_func.

    DATA:
      alv_container TYPE REF TO cl_gui_custom_container,
**  event_receiver_bills TYPE REF TO lcl_event_receiver_bills,
      alv_grid      TYPE REF TO cl_gui_alv_grid,
      gs_stbl       TYPE lvc_s_stbl.

* Create Controls
    CREATE OBJECT alv_container
      EXPORTING
        container_name = 'ALV_GRID'.

    CREATE OBJECT alv_grid
      EXPORTING
        i_parent = alv_container.

    CLEAR: lt_fcat, lt_fcat[].
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = i_tabname
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT lt_fcat INTO ls_fcat.
      DATA(lv_tabix) = sy-tabix.
      ls_fcat-no_zero = i_nozero.

      READ TABLE t_hide INTO DATA(s_hide) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-no_out = 'X'.
      ENDIF.

      READ TABLE t_sum INTO DATA(s_sum) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-do_sum = 'X'.
      ENDIF.

      READ TABLE t_nosum INTO DATA(s_nosum) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-no_sum = 'X'.
      ENDIF.

      READ TABLE t_edit INTO DATA(s_edit) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-edit = 'X'.
      ENDIF.

      READ TABLE t_checkbox INTO DATA(s_checkbox) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-checkbox = 'X'.
      ENDIF.

      READ TABLE t_hotspot INTO DATA(s_hotspot) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-hotspot = 'X'.
      ENDIF.

      READ TABLE t_fixed INTO DATA(s_fixed) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-key = 'X'.
      ENDIF.

      READ TABLE t_replace INTO DATA(s_replace) WITH KEY fieldname = ls_fcat-fieldname.
      IF sy-subrc = 0.
        ls_fcat-scrtext_s = ls_fcat-scrtext_m = ls_fcat-scrtext_l = s_replace-seltext_l.
        ls_fcat-reptext    = s_replace-seltext_l.
        IF s_replace-outputlen IS NOT INITIAL.
          ls_fcat-outputlen  = s_replace-outputlen.
        ENDIF.
        IF s_replace-icon IS NOT INITIAL.
          ls_fcat-icon  = s_replace-icon.
        ENDIF.
        IF s_replace-just IS NOT INITIAL.
          ls_fcat-just = s_replace-just.
        ENDIF.
      ENDIF.

    ENDLOOP.


*    LOOP AT t_include INTO DATA(s_include).
*
*      CALL METHOD zme_alv=>add_button
*        EXPORTING
*          iv_button              = s_include-fcode
*          iv_text                = s_include-text
*          iv_icon                = s_include-icon
*          iv_qinfo               = s_include-qinfo
*          iv_allowed             = abap_true
*        EXCEPTIONS
*          button_already_filled  = 1
*          button_does_not_exists = 2
*          icon_and_text_empty    = 3
*          OTHERS                 = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*    ENDLOOP.
*
    ls_layout-stylefname = 'CELLTAB'.
    ls_layout-info_fname = 'ROWCOL'.
    ls_layout-sel_mode   = 'D'.
    ls_layout-ctab_fname = 'CELLCOL'.
    ls_layout-box_fname  = 'SEL'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-no_totline = i_no_totline.
    ls_layout-zebra      = i_zebra.
    ls_layout-numc_total = abap_false.
    ls_layout-cwidth_opt = i_optimize.
    ls_layout-totals_bef = i_totals_before.

    is_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
    APPEND is_exclude TO it_exclude.

**    CLEAR   i_grid_settings .
**    i_grid_settings-coll_top_p = ' '.
**    i_grid_settings-coll_end_l = 'X'.
**    i_grid_settings-edt_cll_cb = 'X'.


    SET HANDLER:
      zme_alv=>handle_button_click  FOR alv_grid,
      zme_alv=>handle_user_command  FOR alv_grid,
      zme_alv=>handle_hotspot_click FOR alv_grid,
      zme_alv=>handle_data_change   FOR alv_grid.

    CALL METHOD alv_grid->set_toolbar_interactive.

    CALL METHOD alv_grid->register_edit_event EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = it_exclude
        is_layout            = ls_layout
      CHANGING
        it_outtab            = t_data
        it_fieldcatalog      = lt_fcat
        it_sort              = t_sort.

  ENDMETHOD.


  METHOD show_button.
    CHECK iv_button IS NOT INITIAL.
    IF NOT line_exists( allowed_buttons[ function = iv_button ] ).
      DATA(allowed) = VALUE t_allowed_but( function = iv_button ).
      APPEND allowed TO allowed_buttons.
      DELETE excluded_buttons WHERE table_line EQ iv_button.
    ENDIF.
  ENDMETHOD.


  METHOD show_gui_status.
    DATA:
      wa_exclude      TYPE slis_extab.

    LOOP AT i_exclude INTO wa_exclude.
      append wa_exclude-fcode to excluded_buttons.
    ENDLOOP.

    SET PF-STATUS i_status EXCLUDING excluded_buttons[] OF PROGRAM program_name.

  ENDMETHOD.


  METHOD show_title.

    SET TITLEBAR 'DYNAMIC_TITLE' OF PROGRAM program_name WITH iv_text1 iv_text2 iv_text3 iv_text4 iv_text5.

  ENDMETHOD.


  method TOOLBAR.
  endmethod.


  METHOD top_of_page.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = ta_comment
      EXCEPTIONS
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD z_alv_button_create.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab  TYPE lvc_t_styl.
    DATA: temp_celltab  TYPE TABLE OF lvc_s_styl.

    CLEAR: celltab, celltab[], cellline, temp_celltab, temp_celltab[].

    temp_celltab[] = i_celltab.

    CLEAR: e_celltab, e_celltab[].

    DELETE temp_celltab WHERE fieldname = i_fieldname and style = cl_gui_alv_grid=>mc_style_button.

    cellline-style = cl_gui_alv_grid=>mc_style_button.
    cellline-fieldname = i_fieldname.

    APPEND cellline TO temp_celltab.

    SORT temp_celltab.

    e_celltab = temp_celltab[].

  ENDMETHOD.


  METHOD z_alv_button_delete.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab TYPE lvc_t_styl.

    CLEAR: celltab, celltab[], cellline.

    celltab[] = i_celltab.

    DELETE celltab
      WHERE fieldname = i_fieldname
      AND   style = cl_gui_alv_grid=>mc_style_button.

    e_celltab = celltab[].

    CLEAR: celltab, celltab[], cellline.

  ENDMETHOD.


  METHOD z_alv_color_create.

    DATA:
      col    TYPE lvc_s_scol,
      coltab TYPE lvc_t_scol.

    CLEAR: coltab, coltab[].

    coltab[] = i_coltab.

    DELETE coltab WHERE fname = i_fieldname.

    col-color-col = i_color.
    col-color-int = i_int.
    col-color-inv = i_inv.
    col-fname = i_fieldname.
    APPEND col TO coltab.

    SORT coltab.

    e_coltab = coltab[].

  ENDMETHOD.


  METHOD Z_ALV_COLOR_DELETE.

    DATA:
      col    TYPE lvc_s_scol,
      coltab TYPE lvc_t_scol.

    CLEAR: coltab, coltab[].

    coltab[] = i_coltab.

    DELETE coltab WHERE fname = i_fieldname.

    SORT coltab.

    e_coltab = coltab[].

  ENDMETHOD.


  METHOD z_alv_edit_create.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab  TYPE lvc_t_styl.
    DATA: temp_celltab TYPE TABLE OF lvc_s_styl.

    CLEAR: celltab, celltab[], cellline, temp_celltab, temp_celltab[].

    temp_celltab[] = i_celltab.

*    CLEAR: e_celltab, e_celltab[].

    cellline-style = cl_gui_alv_grid=>mc_style_enabled.
    cellline-fieldname = i_fieldname.

    DELETE temp_celltab WHERE fieldname = i_fieldname AND ( style = cl_gui_alv_grid=>mc_style_disabled OR
                                                            style = cl_gui_alv_grid=>mc_style_enabled ).

    APPEND cellline TO temp_celltab.

    SORT temp_celltab.

    e_celltab = temp_celltab[].

  ENDMETHOD.


  METHOD z_alv_edit_delete.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab  TYPE lvc_t_styl.
    DATA: temp_celltab  TYPE TABLE OF lvc_s_styl.

    CLEAR: celltab, celltab[], cellline, temp_celltab, temp_celltab[].

    temp_celltab[] = i_celltab.

    CLEAR: e_celltab, e_celltab[].

    cellline-style = cl_gui_alv_grid=>mc_style_disabled.
    cellline-fieldname = i_fieldname.

    DELETE temp_celltab WHERE fieldname = i_fieldname AND ( style = cl_gui_alv_grid=>mc_style_disabled OR
                                                            style = cl_gui_alv_grid=>mc_style_enabled ).

    APPEND cellline TO temp_celltab.

    SORT temp_celltab.

    e_celltab = temp_celltab[].

  ENDMETHOD.


  METHOD z_alv_hotspot_create.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab  TYPE lvc_t_styl.
    DATA: temp_celltab  TYPE TABLE OF lvc_s_styl.

    CLEAR: celltab, celltab[], cellline, temp_celltab, temp_celltab[].

    temp_celltab[] = i_celltab.

    CLEAR: e_celltab, e_celltab[].

    cellline-style = cl_gui_alv_grid=>mc_style_hotspot.
    cellline-fieldname = i_fieldname.

    DELETE temp_celltab WHERE fieldname = i_fieldname AND ( style = cl_gui_alv_grid=>mc_style_hotspot_no OR
                                                            style = cl_gui_alv_grid=>mc_style_hotspot ).

    APPEND cellline TO temp_celltab.

    SORT temp_celltab.

    e_celltab = temp_celltab[].

  ENDMETHOD.


  METHOD z_alv_hotspot_delete.

    DATA: cellline TYPE lvc_s_styl.
    DATA: celltab TYPE lvc_t_styl.
    DATA: temp_celltab  TYPE TABLE OF lvc_s_styl.

    CLEAR: celltab, celltab[], cellline, temp_celltab, temp_celltab[].

    temp_celltab[] = i_celltab.

    CLEAR: e_celltab, e_celltab[].

    cellline-style = cl_gui_alv_grid=>mc_style_hotspot_no.
    cellline-fieldname = i_fieldname.

    DELETE temp_celltab WHERE fieldname = i_fieldname AND ( style = cl_gui_alv_grid=>mc_style_hotspot_no OR
                                                            style = cl_gui_alv_grid=>mc_style_hotspot ).

    APPEND cellline TO temp_celltab.

    SORT temp_celltab.

    e_celltab = temp_celltab[].

  ENDMETHOD.


  METHOD z_upload_file.


    DATA: lt_text_data   TYPE truxs_t_text_data,
          lv_file_string TYPE string,
          t_files        TYPE filetable,
          s_files        TYPE file_table,
          v_rcode        TYPE int4,
          v_action       TYPE int4,
          v_dir          TYPE string,
          lv_filetype    TYPE char10.

    IF i_filetype = 'XLS'.
      lv_filetype = 'ASC'.
    ELSEIF i_filetype = 'CSV'.
      lv_filetype = 'DAT'.
    ENDIF.

    IF i_file = ' '.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          default_extension       = '.xls'
          file_filter             = '*.xls'
          initial_directory       = v_dir
        CHANGING
          file_table              = t_files
          rc                      = v_rcode
          user_action             = v_action
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE e208(00) WITH 'FILE_OPEN_DIALOG'.
      ENDIF.
    ELSE.
      s_files-filename = i_file.
      APPEND s_files TO t_files.
      v_action = '0'.
    ENDIF.


    CHECK: v_action = 0.

    READ TABLE t_files INDEX 1 INTO s_files.
    IF sy-subrc = 0.
      lv_file_string = s_files-filename.
    ELSE.
      MESSAGE e208(00).
    ENDIF.  "sy-subrc = 0

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_file_string
        filetype                = lv_filetype
        has_field_separator     = 'X'
        dat_mode                = ' '
      TABLES
        data_tab                = lt_text_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.

      MESSAGE ID 'ZFI_BP_MIGR' TYPE 'E' NUMBER '034'
              WITH lv_file_string.
    ENDIF.

    DATA:
      i_filename TYPE rlgrap-filename.

    i_filename = lv_file_string.

    IF i_filetype = 'XLS'.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = i_header
          i_tab_raw_data       = lt_text_data
          i_filename           = i_filename
        TABLES
          i_tab_converted_data = c_input
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE ID 'ZFI_BP_MIGR' TYPE 'E' NUMBER '035'
                WITH lv_file_string.
      ENDIF.

    ELSEIF i_filetype = 'CSV'.

      CALL FUNCTION 'TEXT_CONVERT_CSV_TO_SAP'
        EXPORTING
          i_field_seperator    = ';'
          i_line_header        = 'X'
          i_tab_raw_data       = lt_text_data
          i_filename           = i_filename
        TABLES
          i_tab_converted_data = c_input
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.

        MESSAGE ID 'ZFI_BP_MIGR' TYPE 'E' NUMBER '035'
                WITH lv_file_string.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD z_upload_file_v2.


    DATA: lt_text_data     TYPE solix_tab,
          lv_file_string   TYPE string,
          t_files          TYPE filetable,
          s_files          TYPE file_table,
          v_rcode          TYPE int4,
          v_action         TYPE int4,
          v_dir            TYPE string,
          lv_filetype      TYPE char10,
          lv_headerxstring TYPE xstring,
          lv_filelength    TYPE i.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

    DATA:
      lv_filename TYPE string.

    FIELD-SYMBOLS : <gt_data> TYPE STANDARD TABLE.

    IF i_filetype = 'XLS'.
      lv_filetype = 'ASC'.
    ELSEIF i_filetype = 'CSV'.
      lv_filetype = 'DAT'.
    ENDIF.

    IF i_file = ' '.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          default_extension       = '.xlsx'
          file_filter             = 'Excel Workbook (*.xlsx)|*.xlsx'
          initial_directory       = v_dir
        CHANGING
          file_table              = t_files
          rc                      = v_rcode
          user_action             = v_action
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE e208(00) WITH 'FILE_OPEN_DIALOG'.
      ENDIF.
    ELSE.
      s_files-filename = i_file.
      APPEND s_files TO t_files.
      v_action = '0'.
    ENDIF.

    CHECK: v_action = 0.

    READ TABLE t_files INDEX 1 INTO s_files.
    IF sy-subrc = 0.
      lv_file_string = s_files-filename.
    ELSE.
      MESSAGE e208(00).
    ENDIF.  "sy-subrc = 0

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_file_string
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
        header                  = lv_headerxstring
      TABLES
        data_tab                = lt_text_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.

      MESSAGE ID 'ZFI_BP_MIGR' TYPE 'E' NUMBER '034'
              WITH lv_file_string.
    ENDIF.


    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_text_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    lv_filename = lv_file_string.
    e_filename = lv_filename.

    TRY .
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
                                document_name = lv_filename
                                xdocument     = lv_headerxstring ) .
      CATCH cx_fdt_excel_core.
        "Implement suitable error handling here
    ENDTRY .

    "Get List of Worksheets
    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.
      DESCRIBE TABLE lt_worksheets LINES DATA(lv_sheets).

      IF lv_sheets > 1.
        DATA:
          lt_fields         TYPE slis_t_fieldcat_alv,
          ls_fields         TYPE slis_fieldcat_alv,
          lt_worksheetnames TYPE STANDARD TABLE OF zme_worksheet_names,
          ls_worksheetnames TYPE zme_worksheet_names.

        DATA:
          lv_selfield TYPE  slis_selfield,
          lv_exit.

        LOOP AT lt_worksheets INTO DATA(ls_worksheets).
          ls_worksheetnames-worksheetname = ls_worksheets.
          APPEND ls_worksheetnames TO lt_worksheetnames.
        ENDLOOP.

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
*           I_TITLE              =
            i_selection          = 'X'
*           I_ALLOW_NO_SELECTION =
*           I_ZEBRA              = ' '
*           I_SCREEN_START_COLUMN         = 0
*           I_SCREEN_START_LINE  = 0
*           I_SCREEN_END_COLUMN  = 0
*           I_SCREEN_END_LINE    = 0
*           I_CHECKBOX_FIELDNAME =
*           I_LINEMARK_FIELDNAME =
            i_scroll_to_sel_line = 'X'
            i_tabname            = 'LT_WORKSHEETNAMES'
            i_structure_name     = 'ZME_WORKSHEET_NAMES'
*           IT_FIELDCAT          = lt_fields
*           IT_EXCLUDING         =
*           I_CALLBACK_PROGRAM   =
*           I_CALLBACK_USER_COMMAND       =
*           IS_PRIVATE           =
          IMPORTING
            es_selfield          = lv_selfield
            e_exit               = lv_exit
          TABLES
            t_outtab             = lt_worksheetnames
          EXCEPTIONS
            program_error        = 1
            OTHERS               = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          BREAK lmourelatos.
        ENDIF.
*        BREAK lmourelatos.


        READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX lv_selfield-tabindex.
      ELSE.

        READ TABLE lt_worksheets INTO lv_woksheetname INDEX 1.

      ENDIF.

      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                               lv_woksheetname ).
      "now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <gt_data>.

      CHECK <gt_data> IS ASSIGNED.

      DATA:
        linefiled.

      DATA ref TYPE REF TO data.
      CREATE DATA ref LIKE LINE OF c_input.
      ASSIGN ref->* TO FIELD-SYMBOL(<record>).

      DATA(lv_numberofcolumns) = i_columns.
      LOOP AT <gt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
        CHECK <ls_data> IS ASSIGNED.
        CLEAR linefiled.
        DO lv_numberofcolumns TIMES.

          ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data>  TO FIELD-SYMBOL(<lv_output>).
          IF <lv_output> IS NOT INITIAL.
            linefiled = 'X'.
          ENDIF.
          ASSIGN COMPONENT sy-index OF STRUCTURE <record> TO FIELD-SYMBOL(<lv_input>).
          IF sy-subrc = 0.
            MOVE <lv_output> TO <lv_input>.
          ENDIF.
        ENDDO.
        IF linefiled = 'X'.
          APPEND <record> TO c_input.
        ENDIF.
        UNASSIGN: <lv_output>, <lv_input>.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
