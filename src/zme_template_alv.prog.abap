*&---------------------------------------------------------------------*
*& Report  Z_TEMPLATE_FOR_ALV
*&
*&---------------------------------------------------------------------*
*& Η Function δημιουργεί ALV με τη δυνατότητα "ανοιχτών" πεδίων για
*& διόρθωση με επιλογή ελέγχου του εισαγώμενου πεδίου με την υπόλοιπη
*& γραμμή.
*&
*& Στο πρόγραμμα που φτιάχνω πρέπει να υπάρχουν υποχρεωτικά οι φόρμες
*&
*&  - r0_set_pf_status
*&
*&  - top_of_page
*&
*&  - user_command.
*&
*& Η φόρμα "r0_set_pf_status" χρησιμοποιείται ανάλογα με τα buttons που
*& θέλω να προσθέσω στην οθόνη μου. Η "post" είναι με standard button
*& ενώ η "post1" είναι με variable ονοματα και οικονίδια. Με την "post"
*& γεμίζω τον i_include και για την "post1" τον i_includev με τις
*& αντίστοιχες φόρμες.
*&
*& Οι υπόλοιπες φόρμες χρησιμοποιούνται ανάλογα με τη λειτουργία που
*& θέλω να ενεργοποίησω.
*&
*& Υπάρχει η δυνατότητα δημιουργίας πεδίου με λειτουργία "edit" όπου
*& μετα την εισαγωγή της τιμής από τον χρήστη να πραγματοποιείται
*& έλεγχος και τροποποίηση όλοκληρης της γραμμής με τη νέα τιμή.
*&
*& Επίσης υπάρχει η δυνατότητα να προσθέσω με προυποθέσεις button στην
*& γραμμή και να εκτέλω κώδικα. Για τη λειτουργία του button πρέπει να
*& υπάρχει στην Structure το πεδίο CELLTAB με τύπο LVC_T_STYL.
*&
*& Για τον χρωματισμό ολόκληρης γραμής στην Structure πρέπει να
*& υπάρχει το πεδίο ROWCOL με τύπο CHAR4 που συμπληρώνεται με το κωδικό
*& του χρώματος. Το πεδίο πρέπει να συμπληρώνεται με τιμές αποτελούμενες
*& από 4 χαρακτήρες με την εξής ανάλυση:
*&    Χαρακτήρας 1 = C
*&    Χαρακτήρας 2 = 3 (Color codes: 1 - 7)
*&    Χαρακτήρας 3 = Intensified on/off ( 1 or 0 )
*&    Χαρακτήρας 4 = Inverse display on/off ( 1 or 0 )
*&
*&    Syntax of color value in col color
*&    0 GUI-specific
*&    { 1 | COL_HEADING } 1 Gray-blue
*&    { 2 | COL_NORMAL } 2 Light gray
*&    { 3 | COL_TOTAL } 3 Yellow
*&    { 4 | COL_KEY } 4 Blue-green
*&    { 5 | COL_POSITIVE } 5 Green
*&    { 6 | COL_NEGATIVE } 6 Red
*&    { 7 | COL_GROUP } 7 Violet
*& Για τον χρωματισμό ολόκληρης στήλης στην Structure πρέπει να υπάρχει
*& το πεδίο CELLCOL με τύπο LVC_T_SCOL
*&---------------------------------------------------------------------*
*& Template Structure         ZET_TEMPLATE_ALV2
*&-------------Δείγμα structure-----------------------------------------
*&  SEL     CHAR4           CHAR  4   0 Μη Περισσότ.Καθορισμ.Περιοχή,
*&                                      Πιθανώς Χρησιμ.για Επίπε.Διόρθ
*&  ZCHECK  ZSTATUS_CHECK   CHAR  4   0 Κατάσταση
*&  ZERRORS ZERROR_MESSAGES CHAR  80  0 Μηνύματα
*&  CELLTAB LVC_T_STYL            0   0 Ελεγχος ALV: Πίνακας Στύλ
*&                                      για Κελιά
*&---------------------------------------------------------------------*

REPORT  z_template_for_alv.

TYPE-POOLS: slis.

DATA:
  tabname TYPE ddobjname VALUE 'ZT001_STR',
  alvname TYPE ddobjname VALUE 'ZT001_ALV',
  tblname TYPE ddobjname VALUE 'ZT001_TBL'.

TYPES: BEGIN OF itype.
         INCLUDE TYPE zt001_str.
TYPES: END   OF itype.

TYPES: BEGIN OF itype_alv.
         INCLUDE TYPE zt001_alv.
TYPES: END   OF itype_alv.

* Begin of Selection Screen
PARAMETERS: file_in TYPE string.
PARAMETERS: p_var LIKE disvariant.

* End   of Selection Screen

DATA:
  inttab     TYPE STANDARD TABLE OF itype     WITH HEADER LINE,
  inttab_alv TYPE STANDARD TABLE OF itype_alv WITH HEADER LINE.

DATA: answer TYPE c.

* Upload declarations.

*data:
*  flag_error(1) type                   c,
*  l_count       type                   i.

FIELD-SYMBOLS : <fs_source> TYPE any.
* <<<<

*"Variants
DATA: gs_variant LIKE disvariant.

DATA:
* Header Data
  i_head          TYPE slis_t_listheader,
* Sum Fields
  i_sum           TYPE slis_t_fieldcat_alv,
* Sort and Subtotal fields
  i_sort          TYPE lvc_t_sort,
* Texts for subtotal fields
  i_sfields       TYPE slis_t_fieldcat_alv,
* Hiden Fields
  i_hide          TYPE slis_t_fieldcat_alv,
* Header text info
  i_head_t        TYPE slis_t_fieldcat_alv,
* Editable Fields
  i_edit          TYPE slis_t_fieldcat_alv,
* Text for concat in field head
  i_concat        TYPE slis_t_fieldcat_alv,
* Excluding buttons
  i_exclude       TYPE slis_t_extab,
* Including buttons
  i_includev      TYPE zme_extab OCCURS 0 WITH HEADER LINE,
  i_include       TYPE slis_t_extab,
* Hotspot fields
  i_hotspot       TYPE slis_t_fieldcat_alv,
* Checkbox fields
  i_checkbox      TYPE slis_t_fieldcat_alv,
* Text for replace in field header
  i_replace       TYPE slis_t_fieldcat_alv,
* Fixed fields
  i_fixed         TYPE slis_t_fieldcat_alv,
* Text for replace in field header
  i_noedit        LIKE zme_alv_no_edit OCCURS 0 WITH HEADER LINE,
* Fields not to be sumed
  i_nosum         TYPE slis_t_fieldcat_alv,
* Fields to be changed in order
  i_order         TYPE slis_t_fieldcat_alv,
  ref1            TYPE REF TO cl_gui_alv_grid,
  cellline        TYPE lvc_s_styl,
  celltab_no_edit TYPE lvc_t_styl,

  lt_event_exit   TYPE slis_t_event_exit,
  ls_event_exit   TYPE slis_event_exit.

*asc for File open location
DATA: lv_filename      TYPE string,
      lt_filetable     TYPE filetable,
      lv_rc            TYPE i,
      gv_rc            TYPE subrc,
      gv_filename(128) TYPE c, "string.
      gs_filename      TYPE string.

DATA : BEGIN OF int_t001 OCCURS 0.
         INCLUDE STRUCTURE t001.
DATA : END OF int_t001.

* Ορισμός τυπου για την εισαγωγή αρχείου. Πρέπει να έχει τόσα πεδία
* όσα και οι στήλες που πρεπει αν εισάγω από το αρχείο
TYPES:
  BEGIN OF filein,
    field01(40),
    field02(40),
*    .
*    .
*    .
*    fieldxx(40),
  END   OF filein.
DATA:
  it_input     TYPE STANDARD TABLE OF filein,
  wa_input     TYPE filein,
  t_excel_data TYPE soi_generic_table.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_in.
  PERFORM f4_path_and_file CHANGING file_in.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  gs_variant-report = sy-repid.
  PERFORM variant_f4 USING p_var.
  gs_variant-variant = p_var.
*---------------------------------------------------------------------*
*at selection-screen on p_bukrs.
*  perform authcheck_p_bukrs in program zellak_scaam_auth
*                            using  p_bukrs
*                                   '03'
*                            if found.

*at selection-screen on s_bukrs.

*  IF s_bukrs IS NOT INITIAL.
*
*    SELECT * FROM t001
*      INTO wa_t001
*      WHERE bukrs IN s_bukrs.
*
*      AUTHORITY-CHECK OBJECT 'S_BUKRS_BUK'
*        ID 'ACTVT' DUMMY
*        ID 'BUKRS' FIELD wa_t001-bukrs.
*      IF sy-subrc <> 0.
*        MESSAGE ID 'F5' TYPE 'E' NUMBER 083
*          WITH wa_t001-bukrs.
*      ENDIF.
*
*    ENDSELECT.
*
*  ENDIF.
*----------------------------------------------------------------------*
*       CLASS lcl_event_handlers DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handlers DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_button_click
      FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id es_row_no.

ENDCLASS.                    "lcl_event_handlers DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handlers IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handlers IMPLEMENTATION.
  METHOD handle_button_click.
*    break mourelatosjr.
    DATA: l_row(10),
          l_colname(100),
          l_str          TYPE string,
          gs_stbl        TYPE lvc_s_stbl,
          success.
    FIELD-SYMBOLS:
      <inttab> TYPE itype_alv.
    l_row = es_row_no-row_id.
    l_colname = es_col_id-fieldname.

    CLEAR inttab_alv.
    LOOP AT inttab_alv FROM l_row TO l_row ASSIGNING <inttab>.

*      move-corresponding <inttab> to wa_inttab.
*      perform create_creditor using wa_inttab.
*      <inttab>-button = ' '.
*      <inttab>-lifnr = wa_inttab-lifnr.
*      perform check_validity using <inttab>.
*      if <inttab>-button = ' '.
*        clear <inttab>-celltab.
*      endif.
      <inttab>-sel = 'X'.
    ENDLOOP.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref1.

    gs_stbl-row = 'X'.
    gs_stbl-col = 'X'.
    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = gs_stbl.

  ENDMETHOD.                    "handle_button_click
ENDCLASS.                    "lcl_event_handlers IMPLEMENTATION

INITIALIZATION.

START-OF-SELECTION.

  DATA:
    g_filetable TYPE filetable,
    g_rc        TYPE i.


  CALL METHOD cl_gui_frontend_services=>get_gui_version
    CHANGING
      version_table            = g_filetable
      rc                       = g_rc
    EXCEPTIONS
      get_gui_version_failed   = 1
      cant_write_version_table = 2
      gui_no_version           = 3
      cntl_error               = 4
      error_no_gui             = 5
      not_supported_by_gui     = 6
      OTHERS                   = 7.
  IF sy-subrc <> 0.
    DATA(gv_fiori) = 'X'.
  ENDIF.

  SET TITLEBAR '100' WITH TEXT-h01.

  sy-title = TEXT-h01.

*  if s_bukrs is initial.
*
*    clear: s_bukrs, s_bukrs[],
*           int_t001, int_t001[].
*    select * from t001 into table int_t001.
*
*    loop at int_t001.
*
*      perform authcheck_p_bukrs_guar in program zellak_scaam_auth
*                                using int_t001-bukrs
*                                      '03'
*                                if found.
*
*      if sy-subrc = 0.
*        s_bukrs-sign   = 'I'.
*        s_bukrs-option = 'EQ'.
*        s_bukrs-low    = int_t001-bukrs.
*        append s_bukrs.
*      endif.
*    endloop.
*  endif.

  IF gs_variant-variant IS INITIAL AND p_var IS NOT INITIAL.
    gs_variant-report = sy-repid.
    gs_variant-variant = p_var.
  ENDIF.

* asc for file location
*  PERFORM file_open.

* open excel file
*  PERFORM file_upload.

* open ascii file
*  PERFORM read_ascii.

  PERFORM select_data.

  CHECK inttab[] IS NOT INITIAL.

  PERFORM create_alv_table.

  PERFORM make_header TABLES i_head.

  PERFORM sum_fields TABLES i_sum.

  PERFORM subtotals_fields TABLES i_sort.

  PERFORM subtotals_texts TABLES i_sfields.

  PERFORM hiden_fields TABLES i_hide.

  PERFORM ddic_text TABLES i_head_t.

  PERFORM define_edit_fields TABLES i_edit.

  PERFORM concatenate_texts TABLES i_concat.

  PERFORM exclude_buttons TABLES i_exclude.

  PERFORM include_buttons TABLES i_include.
  PERFORM include_buttonsv TABLES i_includev.

  PERFORM make_hotspot TABLES i_hotspot.

  PERFORM make_checkbox TABLES i_checkbox.

  PERFORM make_replace TABLES i_replace.

  PERFORM make_fixed TABLES i_fixed.

  PERFORM when_fields_no_edit TABLES i_noedit.

  PERFORM make_nosum TABLES i_nosum.

  PERFORM make_order TABLES i_order.

*
*  ls_event_exit-ucomm = '&F12'.
*  ls_event_exit-before = ' '.
*  ls_event_exit-after  = 'X'.
*  APPEND ls_event_exit TO lt_event_exit.

  CALL FUNCTION 'Z_CREATE_SIMPLE_ALV_LVC'
    EXPORTING
      i_prog            = sy-repid
      i_tabname         = alvname
      i_inttab          = 'INTTAB_ALV'
*     I_SEL             = 'X' "επιλογή γραμμής
*     I_CELLCOL         = 'X' "χρωματισμός γραμμής
*     I_NOT_OPTIMIZE    = 'X' "Βελτισοποίηση Στηλών
*     I_BUTTON          = 'X' "Αυτόματη εμφάνιση Button
*     I_CALLER_EXIT     = 'X' "Έλεγχος Button
*     I_NO_TOTLINE      = 'X' "Χωρίς "Συνολικό Αποτέλεσμα"
*     I_TOTALS_BEFORE   = 'X' "Γραμμή Συνόλων στη κορυφή
*     I_NO_ZERO         = 'X' "Απόκρυψη μηδενικών
*     I_ZEBRA           = ' '
*     I_BACKGROUND_ID   = ' '
      i_variant         = gs_variant
*     I_GRID_TITLE      = ' '
*     I_DATA_CHANGE     = 'X' "έλεγχος αλλαγμένης γραμμής
*     I_CHANGE_FORM     = 'DATA_CHANGED'
      i_user_command    = 'USER_COMMAND'
      i_pf_status       = 'SET_PF_STATUS'
      i_top_of_page     = 'TOP_OF_PAGE'
*     I_screen_start_column = 0
*     I_screen_start_line   = 0
*     I_screen_end_column   = 0
*     I_screen_end_line = 0
      i_event_exit      = lt_event_exit
*    IMPORTING
*     E_EXIT_CAUSED_BY_USER  =
    TABLES
      t_inttab_d        = inttab_alv
      t_inttab_head     = i_head
      t_inttab_sum      = i_sum
      t_inttab_sort     = i_sort
      t_inttab_sfields  = i_sfields
      t_inttab_hide     = i_hide
      t_inttab_head_t   = i_head_t
      t_inttab_edit     = i_edit
      t_inttab_concat   = i_concat
      t_inttab_exclude  = i_exclude
      t_inttab_include  = i_include
      t_inttab_includev = i_includev
      t_inttab_hotspot  = i_hotspot
      t_inttab_checkbox = i_checkbox
      t_inttab_replace  = i_replace
      t_inttab_fixed    = i_fixed
      t_inttab_noedit   = i_noedit
      t_inttab_nosum    = i_nosum
      t_inttab_order    = i_order.
*&---------------------------------------------------------------------*
*&      Form  test
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->INTTAB_ALV_NEW  text
*----------------------------------------------------------------------*
FORM data_changed USING lrc_i_dc TYPE REF TO
                        cl_alv_changed_data_protocol.

  DATA:
    ialv_new        TYPE STANDARD TABLE OF itype_alv,
    ialv_line       TYPE itype_alv,
    mod_cells       TYPE STANDARD TABLE OF lvc_s_modi,
    mod_cells_line  TYPE lvc_s_modi,
    mod_layout      TYPE STANDARD TABLE OF lvc_s_layo,
    mod_layout_line TYPE lvc_s_layo,
    tabix           LIKE sy-tabix,
    value(60),
    stable          TYPE lvc_s_stbl.

  FIELD-SYMBOLS: <lfs_alv> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <mod_cells> TYPE ANY TABLE.
  FIELD-SYMBOLS: <mod_layout>.

  ASSIGN lrc_i_dc->mp_mod_rows->* TO <lfs_alv>.
  ASSIGN lrc_i_dc->mt_mod_cells TO <mod_cells>.
  ASSIGN lrc_i_dc->ms_layout TO <mod_layout>.

  ialv_new[]  = <lfs_alv>[].
  mod_cells[] = <mod_cells>[].

  READ TABLE ialv_new INTO ialv_line INDEX 1.
*  read table mod_cells into mod_cells_line index 1.
  mod_layout_line = <mod_layout>.

  LOOP AT mod_cells INTO mod_cells_line.
    READ TABLE ialv_new
      INTO ialv_line
      INDEX mod_cells_line-tabix.

*-- make changes in inttab_alv_line

    MODIFY inttab_alv
      FROM ialv_line
      INDEX mod_cells_line-row_id.

    MODIFY <lfs_alv>
      FROM ialv_line
      INDEX mod_cells_line-tabix.

  ENDLOOP.

  stable-row = 'X'.
  stable-col = 'X'.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.
  CALL METHOD ref1->refresh_table_display
    EXPORTING
      is_stable = stable.

ENDFORM.                    "data_changed
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->pr_ucomm   ok_code
*      -->pls_selfield cursor field
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA sth_slctd.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.
  CALL METHOD ref1->check_changed_data.
  rs_selfield-refresh = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

  CLEAR sth_slctd.

*  CASE r_ucomm.
*    WHEN 'COMPLETE'.

*      loop at inttab_alv
*        where sel    = 'X'
*        and   zcheck = icon_green_light.
*        sth_slctd = 'X'.
*        exit.
*      endloop.
*      if  sth_slctd = 'X' .
*        call function 'POPUP_TO_CONFIRM'
*          exporting
*            titlebar              = text-c01
*            DIAGNOSE_OBJECT       = ' '
*            text_question         = text-c02
*            text_button_1         = 'Ναι'(t03)
*            ICON_BUTTON_1         = ' '
*            text_button_2         = 'Όχι'(t04)
*            ICON_BUTTON_2         = ' '
*            DEFAULT_BUTTON        = '1'
*            DISPLAY_CANCEL_BUTTON = 'X'
*          importing
*            answer                =  answer.
*        if sy-subrc eq 0.
*          if  answer   eq '1' .
*            loop at inttab_alv
*              where sel    = 'X'
*              and   zcheck = icon_green_light.
*
*              perform process_slctd_entries.
*
*            endloop.
*          endif.
*        endif.
*      else .
*        call function 'POPUP_TO_DISPLAY_TEXT'
*          exporting
*            titel              = text-c01
*            textline1          = text-m01.
*
*      endif.


****** hotspot *******************************************************
*    when '&IC1'.
*      read table inttab_alv index rs_selfield-tabindex.
*      if rs_selfield-tabindex ne 0.
*      case rs_selfield-fieldname.
*        when 'BELNR'.
*          set parameter id 'BUK' field inttab_alv-bukrs.
*          set parameter id 'BLN' field inttab_alv-belnr.
*          set parameter id 'GJR' field inttab_alv-gjahr.
*          call transaction 'FB03' and skip first screen.
*        when 'BELNR2_GR'.
*          set parameter id 'BUK' field inttab_alv-bukrs.
*          set parameter id 'BLN' field inttab_alv-belnr_gr.
*          set parameter id 'GJR' field inttab_alv-gjahr.
*          call transaction 'FB03' and skip first screen.
*      endcase.
*     endif.
*  ENDCASE.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.
  CALL METHOD ref1->check_changed_data.
  rs_selfield-refresh = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

ENDFORM.                     "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_VAR  text
*----------------------------------------------------------------------*
FORM variant_f4  USING pv_var.
  DATA: g_variant LIKE disvariant,
        h_exit.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
*     IT_DEFAULT_FIELDCAT =
      i_save        = 'A'
    IMPORTING
      e_exit        = h_exit
      es_variant    = g_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF h_exit IS INITIAL.
    pv_var             = g_variant-variant.
  ENDIF.

ENDFORM.                    " VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  r0_set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UTA_EXTAB  text
*----------------------------------------------------------------------*
FORM set_pf_status USING uta_extab TYPE slis_t_extab.

*  SET PF-STATUS 'DEFAULT' OF PROGRAM 'SAPLZME_ALV' EXCLUDING uta_extab.

  zme_alv=>show_gui_status( i_exclude = i_exclude ).

ENDFORM.                        "SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  r2_set_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

  CALL METHOD zme_alv=>top_of_page
    EXPORTING
      ta_comment = i_head.

ENDFORM.                     "R2_SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEAD  text
*----------------------------------------------------------------------*
FORM make_header TABLES p_head TYPE slis_t_listheader.

  DATA:
    wa_head  LIKE LINE OF p_head.
  " H = Header, S = Selection, A = Action

  FREE p_head.

  CLEAR wa_head.
  wa_head-typ  = 'H'.
  wa_head-key = ' '.
  wa_head-info = TEXT-h01.
  APPEND wa_head TO p_head.

*  CLEAR wa_head.
*  wa_head-typ  = 'S'.
*  wa_head-key = 'Κωδικός Εταιρείάς:'.
*  concatenate '(' wa_t001-bukrs ')' into wa_head-info.
*  concatenate wa_t001-butxt wa_head-info into wa_head-info SEPARATED BY space.
*  APPEND wa_head TO p_head.

ENDFORM.                     " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  SUM_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUM  text
*----------------------------------------------------------------------*
FORM sum_fields TABLES p_sum TYPE slis_t_fieldcat_alv.

  DATA:
    wa_sum LIKE LINE OF p_sum.

  FREE p_sum.

*  wa_sum-fieldname = 'DMBTR'.
*  APPEND wa_sum TO p_sum.
*  wa_sum-fieldname = 'PERC'.
*  APPEND wa_sum TO p_sum.

ENDFORM.                     " SUM_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SUBTOTALS_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SORT  text
*----------------------------------------------------------------------*
FORM subtotals_fields TABLES p_sort TYPE lvc_t_sort.

  DATA:
    wa_sort LIKE LINE OF p_sort.

  FREE p_sort.

  wa_sort-subtot = 'X'.
  wa_sort-up = 'X'.

*  wa_sort-fieldname = 'AR_SIMV'.
*  APPEND wa_sort TO p_sort.

ENDFORM.                     " SUBTOTALS_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SUBTOTALS_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HIDE  text
*----------------------------------------------------------------------*
FORM subtotals_texts TABLES p_texts TYPE slis_t_fieldcat_alv.

  DATA:
    wa_texts LIKE LINE OF p_texts.

  FREE p_texts.

*  wa_texts-fieldname = 'PRCTR'.
*  wa_texts-text_fieldname = 'LTEXT_PRCTR'.
*  APPEND wa_texts TO p_texts.
*
*  wa_texts-fieldname = 'FKSTL'.
*  wa_texts-text_fieldname = 'LTEXT_FKSTL'.
*  APPEND wa_texts TO p_texts.

ENDFORM.                     " SUBTOTALS_TEXTS
*&---------------------------------------------------------------------*
*&      Form  HIDEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HIDE  text
*----------------------------------------------------------------------*
FORM hiden_fields TABLES p_hide TYPE slis_t_fieldcat_alv.

  DATA:
    wa_hide LIKE LINE OF p_hide.

  FREE p_hide.

*  wa_hide-fieldname = 'ZPERC'.
*  APPEND wa_hide TO p_hide.

ENDFORM.                     " HIDEN_FIELDS
*&---------------------------------------------------------------------*
*&      Form  DDIC_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEAD_T  text
*----------------------------------------------------------------------*
FORM ddic_text TABLES p_head_t TYPE slis_t_fieldcat_alv.

  DATA:
    wa_head_t LIKE LINE OF p_head_t.

  FREE p_head_t.

*  wa_head_t-ddictxt = 'L'.
*  wa_head_t-fieldname = 'VULOCKED_DES'.
*  APPEND wa_head_t TO p_head_t.

ENDFORM.                     " DDIC_TEXT
*&---------------------------------------------------------------------*
*&      Form  DEFINE_EDIT_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EDIT  text
*----------------------------------------------------------------------*
FORM define_edit_fields TABLES p_edit TYPE slis_t_fieldcat_alv.

  DATA:
    wa_edit LIKE LINE OF p_edit.

  FREE p_edit.

*  wa_edit-fieldname = 'XSLTA'.
*  append wa_edit to p_edit.

  SORT p_edit.

  FREE celltab_no_edit.

  IF p_edit[] IS NOT INITIAL.

    LOOP AT p_edit INTO wa_edit.
      cellline-style = cl_gui_alv_grid=>mc_style_disabled.
      cellline-fieldname = wa_edit-fieldname.
      APPEND cellline TO celltab_no_edit.
    ENDLOOP.

  ENDIF.

ENDFORM.                     " DEFINE_EDIT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  CONCATENATE_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CONCAT  text
*----------------------------------------------------------------------*
FORM concatenate_texts TABLES p_concat TYPE slis_t_fieldcat_alv.

  DATA:
    wa_concat LIKE LINE OF p_concat.

  FREE p_concat.

*  clear wa_replace.
*  wa_concat-fieldname = 'MIKTA'.
*  wa_concat-seltext_s =
*  wa_concat-seltext_m =
*  wa_concat-seltext_l = p_zyeap.
*  wa_concat-ddictxt = 'L'.
*  APPEND wa_concat TO p_concat.

ENDFORM.                     " CONCATENATE_TEXTS
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_BUTTONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXCLUDE  text
*----------------------------------------------------------------------*
FORM exclude_buttons TABLES p_exclude TYPE slis_t_extab.

  DATA:
    wa_exclude LIKE p_exclude.

  FREE p_exclude.

  wa_exclude-fcode = '&VEXCEL'.
  APPEND wa_exclude TO p_exclude.

  IF gv_fiori = 'X'.

    wa_exclude-fcode = '&XXL'.
    APPEND wa_exclude TO p_exclude.

  ENDIF.

*  wa_exclude-fcode = 'COMPLETE'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&ETA'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&ALL'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&SAL'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&OUP'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&ODN'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&ILT'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&UMC'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&SUM'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&VEXCEL'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&XXL'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '%PC'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '%SL'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&OL0'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&OAD'.
*  APPEND wa_exclude TO p_exclude.
*
*  wa_exclude-fcode = '&AVE'.
*  APPEND wa_exclude TO p_exclude.

ENDFORM.                     " EXCLUDE_BUTTONS
*&---------------------------------------------------------------------*
*&      Form  INCLUDE_BUTTONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INCLUDE  text
*----------------------------------------------------------------------*
FORM include_buttons TABLES p_include TYPE slis_t_extab.

  DATA:
    wa_include LIKE p_include.

  FREE p_include.
*
*CALL METHOD zme_alv=>add_separator
*  EXPORTING
*    iv_button = zme_alv=>b_26
*    .

*  CALL METHOD zme_alv=>add_button
*    EXPORTING
*      iv_button              = zme_alv=>b_01
*      iv_text                = 'Test'
*      iv_icon                = icon_delete
*      iv_qinfo               = 'Delete'
*      iv_allowed             = abap_true
*    EXCEPTIONS
*      button_already_filled  = 1
*      button_does_not_exists = 2
*      icon_and_text_empty    = 3
*      OTHERS                 = 4.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.                     " INCLUDE_BUTTONS
*&---------------------------------------------------------------------*
*&      Form  INCLUDE_BUTTONSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INCLUDE  text
*----------------------------------------------------------------------*
FORM include_buttonsv TABLES p_include STRUCTURE zme_extab.

  DATA:
    wa_include LIKE p_include.

  FREE p_include.

*  wa_include-fcode = 'BUTTON1'.
*  wa_include-fcode_name = 'Δημιουργία'.
*  wa_include-icon_id    = icon_create.
*  wa_include-icon_text  = 'Δημιουργία'.
*  APPEND wa_include TO p_include.

ENDFORM.                     " INCLUDE_BUTTONS
*&---------------------------------------------------------------------*
*&      Form  MAKE_HOTSPOT
*&---------------------------------------------------------------------*
*        text
*----------------------------------------------------------------------*
*      -->P_HOTSPOT  text
*----------------------------------------------------------------------*
FORM make_hotspot TABLES p_hotspot TYPE slis_t_fieldcat_alv.

  DATA:
    wa_hotspot LIKE LINE OF p_hotspot.

  FREE p_hotspot.

*  wa_hotspot-fieldname = 'MIKTA'.
*  APPEND wa_hotspot TO p_hotspot.

ENDFORM.                     " MAKE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  MAKE_CHECKBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECKBOX  text
*----------------------------------------------------------------------*
FORM make_checkbox TABLES p_checkbox TYPE slis_t_fieldcat_alv.

  DATA:
    wa_checkbox LIKE LINE OF p_checkbox.

  FREE p_checkbox.

*  wa_checkbox-fieldname = 'MIKTA'.
*  APPEND wa_checkbox TO p_checkbox.

ENDFORM.                     " MAKE_CHECKBOX
*&---------------------------------------------------------------------*
*&      Form  MAKE_REPLACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_REPLACE  text
*----------------------------------------------------------------------*
FORM make_replace TABLES p_replace TYPE slis_t_fieldcat_alv.

  DATA:
    wa_replace LIKE LINE OF p_replace.

  FREE p_replace.

*  wa_replace-fieldname = 'MIKTA'.
*  wa_replace-seltext_s =
*  wa_replace-seltext_m =
*  wa_replace-seltext_l = p_zyeap.
*  wa_replace-ddictxt = 'L'.
**  wa_replace-outputlen = 'XX'.
*  APPEND wa_replace TO p_replace.

ENDFORM.                     " MAKE_REPLACE
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIXED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FIXED  text
*----------------------------------------------------------------------*
FORM make_fixed TABLES p_fixed TYPE slis_t_fieldcat_alv.

  DATA:
    wa_fixed LIKE p_fixed.

  FREE p_fixed.

*  wa_fixed-fieldname = 'ZCHECK'.
*  append wa_fixed to p_fixed.

ENDFORM.                    " MAKE_FIXED
*&---------------------------------------------------------------------*
*&      Form  WHEN_FIELDS_NO_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NOEDIT  text
*----------------------------------------------------------------------*
FORM when_fields_no_edit TABLES p_noedit STRUCTURE zme_alv_no_edit.

  DATA:
    wa_noedit LIKE p_noedit.

  FREE p_noedit.

*  wa_noedit = icon_checked.
*  APPEND wa_noedit TO p_noedit.
*  wa_noedit = icon_incomplete.
*  APPEND wa_noedit TO p_noedit.
*  wa_noedit = icon_okay.
*  APPEND wa_noedit TO p_noedit.

ENDFORM.                     " WHEN_FIELDS_NO_EDIT
*&---------------------------------------------------------------------*
*&      Form  MAKE_NOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NOSUM  text
*----------------------------------------------------------------------*
FORM make_nosum TABLES p_nosum TYPE slis_t_fieldcat_alv.

  DATA:
    wa_nosum LIKE LINE OF p_nosum.

  FREE p_nosum.

*  wa_nosum-fieldname = 'MIKTA'.
*  APPEND wa_nosum TO p_nosum.

ENDFORM.                     " MAKE_CHECKBOX
*&---------------------------------------------------------------------*
*&      Form  MAKE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ORDER  text
*----------------------------------------------------------------------*
FORM make_order TABLES p_order TYPE slis_t_fieldcat_alv.

  DATA:
    wa_order LIKE LINE OF p_order.

  FREE p_order.

*  wa_order-fieldname = 'NACHN'.
*  wa_order-col_pos = '12'.
*  append wa_order to p_order.
*  wa_order-fieldname = 'VORNA'.
*  wa_order-col_pos = '13'.
*  append wa_order to p_order.

ENDFORM.                     " MAKE_ORDER

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  CLEAR: inttab,     inttab[],
         inttab_alv, inttab_alv[]
*         ,wa_input
         .

** if upload file need it
*  LOOP AT it_input INTO wa_input.
*    MOVE-CORRESPONDING wa_input to inttab.
*    append inttab.
*  endloop.

  SELECT * FROM t001
    INTO CORRESPONDING FIELDS OF TABLE inttab.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_table .

  LOOP AT inttab.
    CLEAR inttab_alv.
    MOVE-CORRESPONDING inttab TO inttab_alv.

*-- Προσθήκη κώδικα για τον έλεγχο της γραμμής

*    CALL METHOD zme_alv=>z_alv_button_create
*      EXPORTING
*        i_celltab   = inttab_alv-celltab
*        i_fieldname = 'BUKRS'
*      IMPORTING
*        e_celltab   = inttab_alv-celltab.
*
*    CALL METHOD zme_alv=>z_alv_button_delete
*      EXPORTING
*        i_celltab   = inttab_alv-celltab
*        i_fieldname = 'BUKRS'
*      IMPORTING
*        e_celltab   = inttab_alv-celltab.
*
*    CALL METHOD zme_alv=>z_alv_hotspot_create
*      EXPORTING
*        i_celltab   = inttab_alv-celltab
*        i_fieldname = 'BUKRS'
*      IMPORTING
*        e_celltab   = inttab_alv-celltab.
*
*    CALL METHOD zme_alv=>z_alv_hotspot_delete
*      EXPORTING
*        i_celltab   = inttab_alv-celltab
*        i_fieldname = 'BUKRS'
*      IMPORTING
*        e_celltab   = inttab_alv-celltab.
*
*    CALL METHOD zme_alv=>z_alv_color_create
*      EXPORTING
*        i_color     = '3'
*        i_coltab    = inttab_alv-cellcol
*        i_fieldname = 'BUKRS'
*      IMPORTING
*        e_coltab    = inttab_alv-cellcol.

    APPEND inttab_alv.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALLER_EXIT
*&---------------------------------------------------------------------*
FORM caller_exit USING ls_data TYPE slis_data_caller_exit.

  DATA: l_ref_alv     TYPE REF TO cl_gui_alv_grid,
        l_ref_handler TYPE REF TO lcl_event_handlers.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
*  call method l_ref_alv->set_drop_down_table
*    exporting
*      it_drop_down = g_t_ddval.
  CREATE OBJECT l_ref_handler.
  SET HANDLER l_ref_handler->handle_button_click FOR l_ref_alv.

ENDFORM.                   "CALLER_EXIT
*&---------------------------------------------------------------------*
*&      Form  f4_path_and_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LY_FILE    text
*----------------------------------------------------------------------*
FORM f4_path_and_file CHANGING ly_file.

  DATA path LIKE ibipparms-path.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = path.

  ly_file = path.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_upload .

  CALL METHOD zme_alv=>z_upload_file
    EXPORTING
      i_file  = file_in
    CHANGING
      c_input = it_input.

*  CLEAR : it_input[], t_excel_data[].
*
*  CALL FUNCTION 'Z_UPLOAD_EXCEL'
*    EXPORTING
*      i_file    = file_in
*      i_top     = 1
*      i_left    = 1
*      i_rows    = 49
*      i_columns = 44
*    IMPORTING
*      e_data    = t_excel_data
*    TABLES
*      t_output  = it_input
*    EXCEPTIONS
*      no_data   = 1
*      OTHERS    = 99.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILE_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_open .

* --- Front-End File open
  CLEAR lv_filename.
  CLEAR gv_filename.

  CLEAR lt_filetable[].

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_filename = lv_filename
    CHANGING
      file_table       = lt_filetable
      rc               = lv_rc.
  IF lv_rc = 0.
    gv_rc = 1.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Uploading file ...'(011).

* --- Front-End File upload
  READ TABLE lt_filetable INDEX 1 INTO lv_filename.
  gv_filename = lv_filename.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_ASCII
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ascii .

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = lv_filename
      filetype = 'ASC'
*     HAS_FIELD_SEPARATOR           = 'X'
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE = ' '
*     CODEPAGE = '1614'
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*     VIRUS_SCAN_PROFILE            =
*     NO_AUTH_CHECK                 = ' '
* IMPORTING
*     FILELENGTH                    =
*     HEADER   =
    TABLES
      data_tab = inttab
* CHANGING
*     ISSCANPERFORMED               = ' '
* EXCEPTIONS
*     FILE_OPEN_ERROR               = 1
*     FILE_READ_ERROR               = 2
*     NO_BATCH = 3
*     GUI_REFUSE_FILETRANSFER       = 4
*     INVALID_TYPE                  = 5
*     NO_AUTHORITY                  = 6
*     UNKNOWN_ERROR                 = 7
*     BAD_DATA_FORMAT               = 8
*     HEADER_NOT_ALLOWED            = 9
*     SEPARATOR_NOT_ALLOWED         = 10
*     HEADER_TOO_LONG               = 11
*     UNKNOWN_DP_ERROR              = 12
*     ACCESS_DENIED                 = 13
*     DP_OUT_OF_MEMORY              = 14
*     DISK_FULL                     = 15
*     DP_TIMEOUT                    = 16
*     OTHERS   = 17
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_log.

*  DATA g_s_log TYPE  bal_s_log.
*
*  g_s_log-extnumber = 'Application Log'.                    "#EC NOTEXT
*  g_s_log-aluser    = sy-uname.
*  g_s_log-alprog    = sy-repid.
** ... see structure BAL_S_LOG for further data ...
** ... which can be added to a log header       ...
** create a log
*
*  CALL FUNCTION 'BAL_LOG_DELETE'
*    EXPORTING
*      i_log_handle  = w_log_handle
*    EXCEPTIONS
*      log_not_found = 1
*      OTHERS        = 2.
*
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*
*  CALL FUNCTION 'BAL_LOG_CREATE'
*    EXPORTING
*      i_s_log      = g_s_log
*    IMPORTING
*      e_log_handle = w_log_handle
*    EXCEPTIONS
*      OTHERS       = 1.
*
**  IF sy-subrc <> 0.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**  ENDIF.

endform.
*
*      PERFORM msg_add USING '' 'E' '00' '398'
*                             text-e18 r_kostl-low '' ''.
*---------------------------------------------------------------------*
*       FORM msg_add                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(I_PROBCLASS)                                            *
*  -->  VALUE(I_MSGTY)                                                *
*  -->  VALUE(I_MSGID)                                                *
*  -->  VALUE(I_MSGNO)                                                *
*  -->  VALUE(I_MSGV1)                                                *
*  -->  VALUE(I_MSGV2)                                                *
*  -->  VALUE(I_MSGV3)                                                *
*  -->  VALUE(I_MSGV4)                                                *
*---------------------------------------------------------------------*
FORM msg_add  USING value(i_probclass) TYPE bal_s_msg-probclass
                   value(i_msgty)
                   value(i_msgid)
                   value(i_msgno)
                   value(i_msgv1)
                   value(i_msgv2)
                   value(i_msgv3)
                   value(i_msgv4).

*  DATA: l_s_msg TYPE bal_s_msg,
*        l_probclass TYPE bal_s_msg-probclass.
**         l_context TYPE zlog_backfluser.
*
*  l_s_msg-msgty     = i_msgty.
*  l_s_msg-msgid     = i_msgid.
*  l_s_msg-msgno     = i_msgno.
*  l_s_msg-msgv1     = i_msgv1.
*  l_s_msg-msgv2     = i_msgv2.
*  l_s_msg-msgv3     = i_msgv3.
*  l_s_msg-msgv4     = i_msgv4.
*  IF i_msgty = 'E' OR i_msgty = 'X'.
*    l_s_msg-probclass = '1'.
*  ELSEIF i_msgty = 'W' .
*    l_s_msg-probclass = '2'.
*  ELSEIF i_msgty = 'S'.
*    l_s_msg-probclass = '3'.
*  ELSE.
*    l_s_msg-probclass = '3'.
*  ENDIF.
**  l_s_msg-probclass = l_probclass.
*
** add this message to log file
** we do not specify I_LOG_HANDLE since we want to add this message
** to the default log.
*
*  CALL FUNCTION 'BAL_LOG_MSG_ADD'
*    EXPORTING
*      i_s_msg       = l_s_msg
*      i_log_handle  = w_log_handle
*    EXCEPTIONS
*      log_not_found = 0
*      OTHERS        = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " msg_add
*---------------------------------------------------------------------*
*       FORM display_bal_log_file                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM display_bal_log_file.

*  DATA: w_handle TYPE bal_t_logh.
*
*
*  APPEND l_log_handle TO w_handle.
*
*  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*       EXCEPTIONS
*            OTHERS                 = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " display_bal_log_file
