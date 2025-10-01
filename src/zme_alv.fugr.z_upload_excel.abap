FUNCTION z_upload_excel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_FILE) TYPE  STRING OPTIONAL
*"     REFERENCE(I_TOP) TYPE  I DEFAULT '1'
*"     REFERENCE(I_LEFT) TYPE  I DEFAULT '1'
*"     REFERENCE(I_ROWS) TYPE  I DEFAULT '1000'
*"     REFERENCE(I_COLUMNS) TYPE  I DEFAULT '10'
*"     REFERENCE(I_DIR) TYPE  STRING DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(E_DATA) TYPE  SOI_GENERIC_TABLE
*"     REFERENCE(E_ERROR) TYPE  CHAR1
*"     REFERENCE(E_FILE) TYPE  STRING
*"  TABLES
*"      T_OUTPUT TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
* Define Screen Container
  DATA: obj_container TYPE REF TO cl_gui_custom_container.
  DATA: o_error       TYPE REF TO i_oi_error,
        o_control     TYPE REF TO i_oi_container_control,
        o_document    TYPE REF TO i_oi_document_proxy,
        o_spreadsheet TYPE REF TO i_oi_spreadsheet.

* Data declarations.
  DATA: t_files    TYPE filetable,
        s_files    TYPE file_table,
        v_doc_name TYPE char256,
        v_changed  TYPE int4,
        v_rcode    TYPE int4,
        t_ranges   TYPE soi_range_list,
        s_ranges   TYPE soi_range_item,
*        t_data     type soi_generic_table,
        s_data     TYPE soi_generic_item,
        v_action   TYPE int4.

  CLASS c_oi_errors DEFINITION LOAD.

* Create Instance control for container
  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = o_control
      error   = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

* Create generic container linked to container in screen 100
  CREATE OBJECT obj_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE e208(00) WITH 'Error creating container'.
  ENDIF.

* Establish connection to GUI Control
  CALL METHOD o_control->init_control
    EXPORTING
      r3_application_name = 'Excel Document Container'
      inplace_enabled     = 'X'
      parent              = obj_container
    IMPORTING
      error               = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

* Create Document Proxy
  CALL METHOD o_control->get_document_proxy
    EXPORTING
      document_type  = soi_doctype_excel_sheet
    IMPORTING
      document_proxy = o_document
      error          = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  FREE t_files.
  IF i_file IS INITIAL.
* Call dialog to navigate to file
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        default_extension       = '.xls'
        file_filter             = '*.xls'
        initial_directory       = i_dir
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
* Only continue if User hasn't cancelled
  CHECK: v_action = 0.

* Determine filename to open Excel document
  READ TABLE t_files INDEX 1 INTO s_files.
  IF sy-subrc = 0.
    CONCATENATE 'FILE://' s_files-filename INTO v_doc_name.
  ELSE.
    MESSAGE e208(00).
  ENDIF.  "sy-subrc = 0

  e_file = s_files-filename.

  DATA:
    retcode TYPE soi_ret_string.

* Open Spreadsheet in SAPWORKDIR
  CALL METHOD o_document->open_document
    EXPORTING
      open_inplace   = 'X'
      document_title = 'Excel'
      document_url   = v_doc_name
      no_flush       = ''
    IMPORTING
      error          = o_error
      retcode        = retcode.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

* Open Spreadsheet interface
  CALL METHOD o_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ''
    IMPORTING
      sheet_interface = o_spreadsheet
      error           = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

*  DATA:
*    lv_lines(4) TYPE n,
*    times_count TYPE i,
*    lv_top      TYPE i VALUE 1,
*    lv_rows     TYPE i VALUE 9999,
*    lt_data     TYPE  soi_generic_table.
*
*  times_count = i_rows DIV 9999.
*
*  ADD 1 TO times_count.
*
*  DO times_count TIMES.
*
*    ADD 9999 TO lv_lines.

* Set selection for 1000 rows
  CALL METHOD o_spreadsheet->set_selection
    EXPORTING
      top     = i_top
*     top     = lv_top
      left    = i_left
      rows    = i_rows
*     rows    = lv_rows
      columns = i_columns.

* Define Range in spreadsheet
  CALL METHOD o_spreadsheet->insert_range
    EXPORTING
      name     = 'Test'
      rows     = i_rows
      columns  = i_columns
      no_flush = ''
    IMPORTING
      error    = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  s_ranges-name    = 'Test'.
  s_ranges-rows    = i_rows.
  s_ranges-columns = i_columns.
  APPEND s_ranges TO t_ranges.

* Get data
  CALL METHOD o_spreadsheet->get_ranges_data
    EXPORTING
      all      = ''
      no_flush = ''
    IMPORTING
      contents = e_data
      error    = o_error
    CHANGING
      ranges   = t_ranges.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
* close document

*    APPEND LINES OF lt_data TO e_data.
*    REFRESH lt_data.
*    ADD 9999 TO lv_top.
*  ENDDO.


* Close the document
  CALL METHOD o_document->close_document
    EXPORTING
      do_save     = ''
      no_flush    = ''
    IMPORTING
      has_changed = v_changed
      error       = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

* Clear Document Resources
  CALL METHOD o_document->release_document
    EXPORTING
      no_flush = ''
    IMPORTING
      error    = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

* Clear table of file names
  FREE: t_files,
        o_control.

* Display the data
*  loop at t_data into s_data.
*    at new row.
*      skip.
*    endat.
*    write:(40) s_data-value.
*  endloop.
  DATA:
    l_count TYPE i.

  FIELD-SYMBOLS:
    <fs_source> TYPE any.

  IF e_data[] IS INITIAL.
    e_error = 'X'.
    MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH TEXT-e02
    RAISING no_data.
    EXIT.
  ELSE.
    LOOP AT e_data INTO s_data.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = s_data-row
        IMPORTING
          output = s_data-row.
      MODIFY e_data FROM s_data.
    ENDLOOP.
    SORT e_data BY row column.
    LOOP AT e_data INTO s_data.
      AT NEW row.
        CLEAR t_output.
      ENDAT.
      MOVE s_data-column TO l_count .
      ASSIGN COMPONENT l_count OF STRUCTURE t_output TO <fs_source>.
      MOVE s_data-value TO <fs_source>.
      AT END OF row.
        APPEND t_output.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
