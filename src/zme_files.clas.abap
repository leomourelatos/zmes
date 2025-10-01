class ZME_FILES definition
  public
  final
  create public .

public section.

  methods ZME_UPLOAD_EXCEL
    importing
      !I_FILE type IBIPPARMS-PATH optional
      !I_COL type STRING default '10'
    exporting
      !E_DATA type STANDARD TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZME_FILES IMPLEMENTATION.


  METHOD zme_upload_excel.

    DATA : lv_filename      TYPE string,
           lt_records       TYPE solix_tab,
           lv_headerxstring TYPE xstring,
           lv_filelength    TYPE i.

    FIELD-SYMBOLS : <lt_data>       TYPE STANDARD TABLE .

    lv_filename = i_file.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
        header                  = lv_headerxstring
      TABLES
        data_tab                = lt_records
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

    "convert binary data to xstring
    "if you are using cl_fdt_xl_spreadsheet in odata then skips this step
    "as excel file will already be in xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

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
      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                               lv_woksheetname ).
      "now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <lt_data>.
    ENDIF.

    BREAK lmourelatos.

  ENDMETHOD.
ENDCLASS.
