report  zupdwd.
***********************************************************************

* Description : download/upload a report from a flat file along with
* its Source code,attributes, Text elements, PF-status and
* Documentation in different languages
*_____________________________________________________________________*
* Inputs:                                                             *
*   Tables:                                                           *
*     SSCRFIELDS - Fields on selection screens                        *
*   Select options:                                                   *
*     N/A                                                             *
*   Parameters:                                                       *
*     P_DWN   -  Radio Button for Download                            *
*     P_UPL   -  Radio Button for Upload                              *
*     P_PROG  -  Program Name                                         *
*     P_FILE  -  File Name                                            *
* Outputs:                                                            *
*  When Uploaded:                                                     *
*    A report is generated along with its Source code, Attributes,    *
*  Text elements, PF-status and Documentation and the report would be *
*  in Active state.                                                   *
*                                                                     *
*  When Downloaded:                                                   *
*    A file is generated on the local system in which Source code,    *
*  Attributes, Text elements, PF-status and Documentation of the      *
*  report are downloaded.                                             *

***********************************************************************

* Table declarations...................................................
tables: sscrfields.                    " Fields on selection screens

* Selection screen elements............................................
selection-screen begin of block b1
                           with frame
                          title tit1.
selection-screen begin of line.
selection-screen comment 1(20) comm1 for field p_dwn.
parameters: p_dwn radiobutton group rad1 default 'X' user-command ucom.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(20) comm2 for field p_upl.
parameters: p_upl radiobutton group rad1 .
selection-screen end of line.

selection-screen skip.

selection-screen begin of block b2
                           with frame
                          title tit2 .
selection-screen begin of line.
selection-screen comment 1(20) comm3 for field p_prog.
parameters: p_prog type trdir-name modif id bl1.
*                                      " Program Name
selection-screen end of line.

selection-screen skip.

selection-screen comment /1(50) comm5.
selection-screen comment /1(50) comm6.

selection-screen begin of line.
selection-screen comment 1(20) comm4 for field p_file.
parameters: p_file   type rlgrap-filename default 'C:\'
                                         modif id bl1.
*                                      " Download File Name
selection-screen end of line.

selection-screen end of block b2.
selection-screen end of block b1.

* Type declarations for internal tables................................
types: begin of type_s_dd03l,
         fieldname type fieldname,     " Field Name
       end of type_s_dd03l,

       begin of type_s_trdir,
         name    type progname,         " Program Name
         edtx    type edtx,             " Editor lock flag
         subc    type subc,             " Program type
         secu    type secu,             " Authorization Group
         fixpt   type fixpt,            " Fixed point arithmetic
         sset    type sset,             " Start only via variant
         uccheck type uccheck,          " Unicode check flag
         rstat   type rdir_rstat,       " Status
         appl    type rdir_appl,        " Application
         ldbname type ldbnam,           " LDB name
         type    type rdir_type,        " Selection screen version
       end   of type_s_trdir.


* Work variables........................................................
data:
  w_file       type string,           " File Name
  w_type(10)   type c,                " File Type
  w_exist(1)   type c,                " Flag
  w_prog(60)   type c,                " Program Name
  w_index      type sytabix,          " Index
  w_text       type repti,            " Title of the program
  w_appl       type  rdir_appl,       " Application
  w_prog2(120) type c,                " Program name
  w_prog3(70)  type c,                " Program name
  w_name       type progname,         " Program name
  w_obj        type trobj_name,       " Object Name in Object List
  w_str        type string,           " String
  w_ans(1)     type c,                " Answer
  w_pgmid      type pgmid,            " Program ID
  w_object     type trobjtype,        " Object Type
  w_char(1)    type c,                " Language Key
  w_len(10)    type c,                " Reserved length for text
  w_state      type dokstate,         " Documentation status
  w_typ        type doku_typ,         " Documentation type
  w_version    type dokvers,          " Documentation version
  w_lang(1)    type c,                " Language Key
  w_mess       type string,           " Message
  w_lin        type i,                " Line Number
  w_wrd        type string,           " Word
  w_strlen     type i,                " String Length
  w_cnt2       type i,                " Counter Variable
  w_cnt3       type i,                " Counter Variable
  w_field(20)  type c,                " Holds Text
  w_val        type string.           " Holds Field Symbol value

* Constants.............................................................
constants:
  c_asc(10)  value 'ASC',              " File type
  c_x(1)     value 'X',                " Flag
  c_lang(1)  value 'E',                " Language
  c_prog(4)  value 'PROG',             " Object type
  c_stat(10) value 'RSMPE_STAT',       " Constant 'RSMPE_STAT'
  c_funt(10) value 'RSMPE_FUNT',       " Constant 'RSMPE_FUNT'
  c_men(9)   value 'RSMPE_MEN',        " Constant 'RSMPE_MEN'
  c_mnlt(10) value 'RSMPE_MNLT',       " Constant 'RSMPE_MNLT'
  c_act(9)   value 'RSMPE_ACT',        " Constant 'RSMPE_ACT'
  c_but(9)   value 'RSMPE_BUT',        " Constant 'RSMPE_BUT'
  c_pfk(9)   value 'RSMPE_PFK',        " Constant 'RSMPE_PFK'
  c_staf(10) value 'RSMPE_STAF',       " Constant 'RSMPE_STAF'
  c_atrt(10) value 'RSMPE_ATRT',       " Constant 'RSMPE_ATRT'
  c_titt(10) value 'RSMPE_TITT',       " Constant 'RSMPE_TITT'
  c_buts(10) value 'RSMPE_BUTS',       " Constant 'RSMPE_BUTS'
  c_sep(1)   value ';',                " Separator ';'
  c_sep2(1)  value '*'.                " Separator '*'

* Field Strings.........................................................
data: fs_trdir      type type_s_trdir, " (Structure) TRDIR
      fs_tadir      type tadir,        " (Structure) TADIR
      fs_tdevc      type tdevc,        " (Structure) TDEVC
      fs_thead      type thead,        " (Structure) THEAD
      fs_adm        type rsmpe_adm,    " (Structure) RSMPE_ADM
      fs_doc(50000) type c,            " (Structure) String
      fs_str(50000) type c,            " (Structure) String
      fs_dir        type trdir,        "  System Table TRDIR
      fs_trkey      type trkey,        " (Structure) TRKEY
      fs_code       type string,       " (Structure) Source Code
      fs_attr       type string,       " (Structure) Attributes
      fs_docu       type string,       " (Structure) Documentation
      fs_text1      type string,       " (Structure) Texts
      fs_pfs        type string,       " (Structure) PF-Status
      fs_data       type string,       " (Structure) Complete Data
      fs_data2      type string,       " (Structure) Complete Data
      fs_dokil      type dokil,        " (Structure) Index for
*                                      " Documentation
      fs_tline      type tline,        " (Structure) Docu Tables
      fs_sta        type rsmpe_stat,   " (Structure) Text-dependentStat
      fs_fun        type rsmpe_funt,   " (Structure) Language-specific
*                                      " function texts
      fs_men        type rsmpe_men,    " (Structure) Menu structure
      fs_mtx        type rsmpe_mnlt,   " (Structure) Language-specific
*                                      " menu texts
      fs_act        type rsmpe_act,    " (Structure) Menu bars
      fs_but        type rsmpe_but,    " (Structure) Pushbuttons
      fs_pfk        type rsmpe_pfk,    " (Structure) Function key
*                                      " assignments
      fs_set        type rsmpe_staf,   " (Structure) Status functions
      fs_atrt       type rsmpe_atrt,   " (Structure) Attributes with
*                                      " texts
      fs_tit        type rsmpe_titt,   " (Structure) Title Codes with
*                                      " texts
      fs_biv        type rsmpe_buts,   " (Structure) Fixed Functions on
*                                      " Application Toolbars
      fs_txt        type textpool,     " (Structure) ABAP Text Pool
*                                      " Definition
      fs_dd03l      type type_s_dd03l. " Table Fields

* Internal tables.......................................................
data:
*----------------------------------------------------------------------*
* Internal table to hold Source code                                   *
*----------------------------------------------------------------------*
  t_code  type table of string,

*----------------------------------------------------------------------*
* Internal table to hold Attributes                                    *
*----------------------------------------------------------------------*
  t_attr  type standard table of string,

*----------------------------------------------------------------------*
* Internal table to hold Documentation                                 *
*----------------------------------------------------------------------*
  t_docu  type table of string,

*----------------------------------------------------------------------*
* Internal table to hold Texts                                         *
*----------------------------------------------------------------------*
  t_text  type table of string,

*----------------------------------------------------------------------*
* Internal table to hold PF-Status                                     *
*----------------------------------------------------------------------*
  t_pfs   type table of string,

*----------------------------------------------------------------------*
* Internal table to hold Complete data                                 *
*----------------------------------------------------------------------*
  t_data  type table of string,
  t_data2 type table of string,

*----------------------------------------------------------------------*
* Internal table to hold Index for Documentation                       *
*----------------------------------------------------------------------*
  t_dokil type table of dokil,

*----------------------------------------------------------------------*
* Internal table to hold Docu tables                                   *
*----------------------------------------------------------------------*
  t_tline type table of tline,

*----------------------------------------------------------------------*
* PF-STATUS related tables                                             *
*----------------------------------------------------------------------*
  t_sta   type table of rsmpe_stat,
  t_fun   type table of rsmpe_funt,
  t_men   type table of rsmpe_men,
  t_mtx   type table of rsmpe_mnlt,
  t_act   type table of rsmpe_act,
  t_but   type table of rsmpe_but,
  t_pfk   type table of rsmpe_pfk,
  t_set   type table of rsmpe_staf,
  t_atrt  type table of rsmpe_atrt,
  t_tit   type table of rsmpe_titt,
  t_biv   type table of rsmpe_buts,
  t_txt   type table of textpool,
  t_dd03l type table of type_s_dd03l.

* Field Symbols........................................................
field-symbols: <fs1> type any.

*---------------------------------------------------------------------*
*                       INITIALIZATION EVENT                          *
*---------------------------------------------------------------------*
initialization.
  move : 'Selection Criteria'                             to tit1,
         'Specify the required parameters'                to tit2,
         'Download'                                       to comm1,
         'Upload'                                         to comm2,
         'Program Name'                                   to comm3,
         'File Path'                                      to comm4,
         'Specify only File Path in case of Download,'    to comm5,
         'filename is taken from Program name by default' to comm6.

*---------------------------------------------------------------------*
*                  AT SELECTION-SCREEN OUTPUT EVENT                   *
*---------------------------------------------------------------------*
at selection-screen output.
* For upload option
  if p_upl = 'X'.
    move ' ' to p_file.
    move ' ' to p_prog.
  endif.                               " IF P_UPL = 'X'

* For download option
  if p_dwn = 'X'.
    move 'C:\' to p_file.
  endif.                               " IF P_DWN = 'X'


*----------------------------------------------------------------*
*      AT SELECTION-SCREEN ON VALUE-REQUEST FOR FIELD EVENT      *
*----------------------------------------------------------------*
at selection-screen on value-request for p_file.
* F4 help for file
  perform file_help changing p_file.

*--------------------------------------------------------------------*
*                   AT SELECTION-SCREEN EVENT                        *
*--------------------------------------------------------------------*
at selection-screen.
* If program name is not entered on the screen
  if sscrfields-ucomm = 'ONLI'.
    if p_prog is initial.
      message 'Specify Program Name' type 'E'.
    endif.                             " IF P_PROG IS INITIAL
  endif.                               " IF SSCRFIELDS-UCOMM = 'ONLI'

* If file path is not entered on the screen
  if sscrfields-ucomm = 'ONLI'.
    if p_file is initial.
      message 'Specify File Path' type 'E'.
    endif.                             " IF P_FILE IS INITIAL
  endif.                               " IF SSCRFIELDS-UCOMM = 'ONLI'

* check if program name entered is greater than 30 chars
  w_strlen = strlen( p_prog ).
  if w_strlen gt 30.
    concatenate 'Program name too long. '
                'Names longer than 30 chars for internal use only'
           into w_str.
    message w_str type 'E'.
    clear w_str.
  endif.                               " IF W_STRLEN GT 30...

* Check if the file already exists
  perform check_file.

*---------------------------------------------------------------------*
*                   START-OF-SELECTION EVENT                          *
*---------------------------------------------------------------------*
start-of-selection.

* When download option is selected
  if p_dwn = 'X'.

* Get Program Name
    perform get_prog_name.

* Check if the program is active or not
    perform check_prog_status.

* Get Source code
    perform get_source using fs_trdir-name.

* Get Attributes
    perform get_attr using fs_trdir.

* Get Documentaion maintained in all the languages
* i.e; includes translations
    perform get_docu.

* Get all the texts maintained in all the languages
* i.e; includes translations
    perform get_text using fs_trdir-name.

* Get PF-STATUS
    perform get_pfstat using fs_trdir-name.

* File type
    move c_asc to w_type.

* Append all the data to final internal table
    append lines of t_code to t_data.
    append lines of t_attr to t_data.
    append lines of t_docu to t_data.
    append lines of t_text to t_data.
    append lines of t_pfs  to t_data.

* Download file
    perform download tables t_data
                     using  w_file
                            w_type.
  endif.                               " IF P_DWN = 'X'

* When upload option is selected
  if p_upl = 'X'.

* Check if the program already exists
    perform check_prog.

* File type
    move c_asc to w_type.

* Upload File
    perform upload tables t_data
                   using  w_file
                          w_type.

* Split the data into different tables
    perform process_data.

* Create New Program
    perform create_prog.
  endif.                               " IF P_UPL = 'X'

*&---------------------------------------------------------------------*
*&      Form  FILE_HELP                                                *
*&---------------------------------------------------------------------*
* Subroutine for f4 help for file                                      *
*----------------------------------------------------------------------*
* PV_FILE ==> File Name                                                *
*----------------------------------------------------------------------*
form file_help  changing pv_file type rlgrap-filename.
  call function 'F4_FILENAME'
    importing
      file_name = pv_file.
endform.                               " FILE_HELP

*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE                                               *
*&---------------------------------------------------------------------*
* Subroutine to check if file exists or not                            *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form check_file .

* Concatenate Filepath and Program name to get filename in case
* of download
  if p_dwn = 'X'.
    if p_file ns '.txt'.
      concatenate p_file
                  p_prog
                  '.txt'
             into p_file.
    endif.                             " IF p_file NS...
  endif.                               " IF P_DWN = 'X'

* Populate file and program variables
  move p_file to w_file.
  move p_prog to w_prog2.
  move p_prog to w_prog3.

  call function 'TMP_GUI_GET_FILE_EXIST'
    exporting
      fname          = p_file
    importing
      exist          = w_exist
    exceptions
      fileinfo_error = 1
      others         = 2.

  if sy-subrc eq 0.
* If file already exists in case of download
    if w_exist = c_x and p_dwn = 'X'.
      clear: w_str,w_ans.
      concatenate 'File '
                   p_file
                  ' already exists,'
                  'do you want to overwrite it?'
             into w_str
     separated by space.

      call function 'POPUP_TO_CONFIRM'
        exporting
          text_question         = w_str
          display_cancel_button = ' '
        importing
          answer                = w_ans
        exceptions
          text_not_found        = 1.

      if sy-subrc = 0.
* If user doesn't want to overwrite the existing file,
* allow him to specify different file name, otherwise continue
        if w_ans = '2'.
          message 'Specify valid Filename along with Path and Extension'
          type 'S'.
          stop.
        endif.                         " IF w_ans = '2'
      endif.                           " IF sy-subrc = 0
* If file does not exist in case of upload
    elseif w_exist ne c_x and p_upl = 'X'.
      message 'File does not exist' type 'S'.
      stop.
    endif.                             " IF W_EXIST = C_X...
  endif.                               " IF SY-SUBRC EQ 0

  clear: w_str,w_ans.

endform.                               " CHECK_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_PROG_NAME                                            *
*&---------------------------------------------------------------------*
* Subroutine to get program name                                       *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form get_prog_name.

  move p_prog to w_prog.

  select single name                   " ABAP Program Name
                edtx                   " Editor lock flag
                subc                   " Program type
                secu                   " Authorization Group
                fixpt                  " Fixed point arithmetic
                sset                   " Start only via variant
                uccheck                " Unicode check was performed
                rstat                  " Status
                appl                   " Application
                ldbname                " LDB Name
                type                   " Selection screen version
           from trdir
           into fs_trdir
          where name = w_prog.

  if sy-subrc ne 0.
    message 'Invalid Program name' type 'S'.
    stop.
  endif.                               " IF SY-SUBRC NE 0
endform.                               " GET_PROG_NAME

*&---------------------------------------------------------------------*
*&      Form  GET_SOURCE                                               *
*&---------------------------------------------------------------------*
* Subroutine to get source code                                        *
*----------------------------------------------------------------------*
* PV_NAME ==> Program Name                                             *
*----------------------------------------------------------------------*
form get_source using pv_name type trdir-name.

  read report pv_name into t_code.

  if sy-subrc eq 0.

    concatenate '**This code is automatically generated by YASH program'
                ', please do not make any changes**'
           into fs_code
   separated by space.
    insert fs_code into t_code index 1.

    loop at t_code into fs_code.
      if sy-tabix ne 1.
        move sy-tabix to w_index.
        concatenate 'C'
                    fs_code
               into fs_code.
        modify t_code from fs_code index w_index.
      else.
        move sy-tabix to w_index.
        concatenate 'H'
                    fs_code
               into fs_code.
        modify t_code from fs_code index w_index.

      endif.                           " IF SY-TABIX NE 1
    endloop.                           " LOOP AT T_CODE INTO FS_CODE...
  endif.                               " IF SY-SUBRC EQ 0
endform.                               " GET_SOURCE

*&---------------------------------------------------------------------*
*&      Form  GET_ATTR                                                 *
*&---------------------------------------------------------------------*
* Subroutine to get attributes                                         *
*----------------------------------------------------------------------*
* PV_TRDIR ==> TRDIR structure                                         *
*----------------------------------------------------------------------*
form get_attr using pv_trdir type type_s_trdir.

* Report Title
  select single text                   " Report Title
           from trdirt
           into w_text
          where name  = p_prog
            and sprsl = c_lang.

  if sy-subrc eq 0.
    concatenate 'A'
                'TEXT'
                w_text
           into fs_attr.
    append fs_attr to t_attr.
    clear  fs_attr.
  endif.                               " IF SY-SUBRC EQ 0


* Type
  concatenate 'A'
              'SUBC'
              pv_trdir-subc
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Status
  concatenate 'A'
              'RSTAT'
              pv_trdir-rstat
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Application
  select single appl                   " Applications programs,function
*                                      " modules, logical databases
           from taplp
           into w_appl
          where appl = pv_trdir-appl.

  if sy-subrc eq 0.
    concatenate 'A'
                'APPL'
                w_appl
           into fs_attr.
    append fs_attr to t_attr.
    clear  fs_attr.
  endif.                               " IF SY-SUBRC EQ 0

* Authorization Group
  concatenate 'A'
              'SECU'
              pv_trdir-secu
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Package
  call function 'AKB_GET_TADIR'
    exporting
      obj_type         = c_prog
      obj_name         = pv_trdir-name
    importing
      tadir            = fs_tadir
      tdevc            = fs_tdevc
    exceptions
      object_not_found = 1
      others           = 2.

  if sy-subrc eq 0.
    concatenate 'A'
                'DEVCLASS'
                fs_tdevc-devclass
           into fs_attr.
    append fs_attr to t_attr.
    clear  fs_attr.
  else.
    message 'Object not found' type 'S'.
  endif.                               " IF SY-SUBRC EQ 0

* Logical database
  concatenate 'A'
              'LDBNAME'
              pv_trdir-ldbname
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Selection screen version
  concatenate 'A'
              'TYPE'
              pv_trdir-type
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Editor Lock
  concatenate 'A'
              'EDTX'
              pv_trdir-edtx
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Fixed point arithmetic
  concatenate 'A'
              'FIXPT'
              pv_trdir-fixpt
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Unicode checks active
  concatenate 'A'
              'UCCHECK'
              pv_trdir-uccheck
         into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Start using variant
  concatenate 'A'
              'SSET'
              pv_trdir-sset
              into fs_attr.
  append fs_attr to t_attr.
  clear  fs_attr.

* Variables for documentation
* Program ID
  concatenate 'D'
              'PGMID'
              fs_tadir-pgmid
         into fs_docu.
  append fs_docu to t_docu.
  clear  fs_docu.

* Object Type
  concatenate 'D'
              'OBJECT'
              fs_tadir-object
         into fs_docu.
  append fs_docu to t_docu.
  clear  fs_docu.
endform.                               " GET_ATTR

*&---------------------------------------------------------------------*
*&      Form  GET_DOCU                                                 *
*&---------------------------------------------------------------------*
* Subroutine to get documentation                                      *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form get_docu.

* Get Index for Documentation
  select id                            " Document class
         object                        " Documentation Object
         langu                         " Documentation Language
         typ                           " Documentation type
         version                       " Version of DocumentationModule
         dokstate                      " Status of Documentation Module
    from dokil
    into table t_dokil
   where object = w_prog.

  if sy-subrc eq 0.
    loop at t_dokil into fs_dokil.
      clear: fs_thead,
             fs_tline,
             t_tline[].

      call function 'DOCU_READ'
        exporting
          id      = fs_dokil-id
          langu   = fs_dokil-langu
          object  = fs_dokil-object
          typ     = fs_dokil-typ
          version = fs_dokil-version
        importing
          head    = fs_thead
        tables
          line    = t_tline.

* Text lines
      loop at t_tline into fs_tline.
        concatenate 'DLINE'
                    fs_tline-tdformat
                    fs_tline-tdline
               into fs_docu
       separated by ';'.
        append fs_docu to t_docu.
        clear  fs_docu.
      endloop.                         " LOOP AT T_TLINE INTO FS_TLINE

* Text header
      concatenate 'DHEAD'
                  fs_thead-tdobject fs_thead-tdname     fs_thead-tdid
                  fs_thead-tdspras  fs_thead-tdtitle    fs_thead-tdform
                  fs_thead-tdstyle  fs_thead-tdversion
                  fs_thead-tdfuser  fs_thead-tdfreles
                  fs_thead-tdfdate  fs_thead-tdftime
                  fs_thead-tdluser  fs_thead-tdlreles
                  fs_thead-tdldate  fs_thead-tdltime
                  fs_thead-tdlinesize
                  fs_thead-tdtxtlines fs_thead-tdhyphenat
                  fs_thead-tdospras   fs_thead-tdtranstat
                  fs_thead-tdmacode1  fs_thead-tdmacode2
                  fs_thead-tdrefobj   fs_thead-tdrefname
                  fs_thead-tdrefid    fs_thead-tdtexttype
                  fs_thead-tdcompress fs_thead-mandt fs_thead-tdoclass
                  fs_thead-logsys
             into fs_docu
     separated by ';'.

      append fs_docu to t_docu.
      clear  fs_docu.

* Other parameters
* Documentation Status
      concatenate 'D'
                  'DOKSTATE'
                  fs_dokil-dokstate
             into fs_docu.
      append fs_docu to t_docu.
      clear  fs_docu.

* Documentation Type
      concatenate 'D'
                  'TYP'
                  fs_dokil-typ
             into fs_docu.
      append fs_docu to t_docu.
      clear  fs_docu.

* Documentation Version
      concatenate 'D'
                  'DOKVERSION'
                  fs_dokil-version
             into fs_docu.
      append fs_docu to t_docu.
      clear  fs_docu.
    endloop.                           " LOOP AT T_DOKIL INTO FS_DOKIL
  endif.                               " IF SY-SUBRC EQ 0
endform.                               " GET_DOCU

*&---------------------------------------------------------------------*
*&      Form  GET_TEXT                                                 *
*&---------------------------------------------------------------------*
* Subroutine to get text elements                                      *
*----------------------------------------------------------------------*
* PV_NAME ==> Program Name                                             *
*----------------------------------------------------------------------*
form get_text using pv_name type trdir-name.

  data: lv_len(10) type c.

  types: begin of type_s_txtlang,
           language type spras,
         end   of type_s_txtlang.

  data: fs_txtlang type type_s_txtlang,
        lt_txtlang type table of type_s_txtlang.

  select language
    from repotext
    into table lt_txtlang
   where progname = pv_name.

  if sy-subrc eq 0.

    loop at lt_txtlang into fs_txtlang.
      read textpool pv_name into t_txt language fs_txtlang-language.
      if sy-subrc eq 0.
        loop at t_txt into fs_txt.
          move fs_txt-length to lv_len.
          concatenate 'T'          fs_txtlang-language
                      fs_txt-id    fs_txt-key
                      fs_txt-entry lv_len
                     into fs_text1 separated by '*%'.
          append fs_text1 to t_text.
          clear: fs_text1,
                 lv_len.
        endloop.                       " LOOP AT T_TXT INTO FS_TXT
* IF report title is not populated, exceptional cases
        clear: w_lang.
        move sy-langu to w_lang.
        if fs_txtlang-language = w_lang.
          clear: fs_txt-key,
                 lv_len,
                 fs_text1,
                 fs_txt.

          read table t_txt into fs_txt with key id = 'R'.
          if sy-subrc ne 0.
            lv_len = strlen( w_text ).
            concatenate 'T'          fs_txtlang-language
                        'R'          fs_txt-key
                        w_text       lv_len
                       into fs_text1 separated by '*%'.
            append fs_text1 to t_text.
            clear: fs_text1,
                   lv_len.
          endif.                       " IF SY-SUBRC NE 0
        endif.                         " IF FS_TXTLANG-LANGUAGE...
      endif.                           " IF SY-SUBRC EQ 0
    endloop.                           " LOOP AT lt_txtlang
  endif.                               " IF SY-SUBRC EQ 0
endform.                               " GET_TEXT

*&---------------------------------------------------------------------*
*&      Form  GET_PFSTAT                                               *
*&---------------------------------------------------------------------*
* Subroutine to get pf-status                                          *
*----------------------------------------------------------------------*
* PV_NAME ==> Program Name                                             *
*----------------------------------------------------------------------*
form get_pfstat using pv_name type trdir-name.

  data:
    lt_langu type table of sprsl,
    fs_langu type sprsl.

  select sprsl
    from rsmptexts
    into table lt_langu
   where progname = pv_name.

  if sy-subrc eq 0.
    sort lt_langu.

    delete adjacent duplicates from lt_langu.

    loop at lt_langu into fs_langu.
      clear: fs_adm,
             fs_sta, t_sta[],
             fs_fun, t_fun[],
             fs_men, t_men[],
             fs_mtx, t_mtx[],
             fs_act, t_act[],
             fs_but, t_but[],
             fs_pfk, t_pfk[],
             fs_set, t_set[],
             fs_atrt,t_atrt[],
             fs_tit, t_tit[],
             fs_biv, t_biv[].

      call function 'RS_CUA_INTERNAL_FETCH'
        exporting
          program         = pv_name
          language        = fs_langu
        importing
          adm             = fs_adm
        tables
          sta             = t_sta
          fun             = t_fun
          men             = t_men
          mtx             = t_mtx
          act             = t_act
          but             = t_but
          pfk             = t_pfk
          set             = t_set
          doc             = t_atrt
          tit             = t_tit
          biv             = t_biv
        exceptions
          not_found       = 1
          unknown_version = 2
          others          = 3.

      if sy-subrc eq 0.

        concatenate 'PLAN'
                    fs_langu
               into fs_pfs.
        append fs_pfs to t_pfs.
        clear  fs_pfs.

        clear: w_cnt3.
        perform download_pf_tabs tables t_sta
                                 using  c_stat
                                        fs_sta
                                        'FS_STA-'
                                        'PSTA'.

        perform download_pf_tabs tables t_fun
                                 using  c_funt
                                        fs_fun
                                        'FS_FUN-'
                                        'PFUN'.

        perform download_pf_tabs tables t_men
                                 using  c_men
                                        fs_men
                                        'FS_MEN-'
                                        'PMEN'.

        perform download_pf_tabs tables t_mtx
                                 using  c_mnlt
                                        fs_mtx
                                        'FS_MTX-'
                                        'PMTX'.

        perform download_pf_tabs tables t_act
                                 using  c_act
                                        fs_act
                                        'FS_ACT-'
                                        'PACT'.

        perform download_pf_tabs tables t_but
                                 using  c_but
                                        fs_but
                                        'FS_BUT-'
                                        'PBUT'.

        perform download_pf_tabs tables t_pfk
                                 using  c_pfk
                                        fs_pfk
                                        'FS_PFK-'
                                        'PPFK'.

        perform download_pf_tabs tables t_set
                                 using  c_staf
                                        fs_set
                                        'FS_SET-'
                                        'PSET'.

        perform download_pf_tabs tables t_atrt
                                 using  c_atrt
                                        fs_atrt
                                        'FS_ATRT-'
                                        'PATR'.

        perform download_pf_tabs tables t_tit
                                 using  c_titt
                                        fs_tit
                                        'FS_TIT-'
                                        'PTIT'.

        perform download_pf_tabs tables t_biv
                                 using  c_buts
                                        fs_biv
                                        'FS_BIV-'
                                        'PBIV'.
        clear: w_cnt3.

        concatenate 'PADM'
                    fs_adm-actcode    fs_adm-mencode    fs_adm-pfkcode
                    fs_adm-defaultact fs_adm-defaultpfk
                    fs_adm-mod_langu
               into fs_pfs
       separated by ';'.
        append fs_pfs to t_pfs.
        clear  fs_pfs.

      else.
        message 'Error during PF-STATUS download' type 'E' display like
        'S'.
      endif.                           " IF SY-SUBRC EQ 0
    endloop.                           " LOOP AT LT_LANGU INTO FS_LANGU
  endif.                               " IF SY-SUBRC EQ 0

  concatenate 'PTRK'
              fs_tadir-devclass
              fs_tadir-object
              p_prog
         into fs_pfs
 separated by ';'.
  append fs_pfs to t_pfs.
  clear  fs_pfs.
endform.                               " GET_PFSTAT

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD                                                 *
*&---------------------------------------------------------------------*
* Subroutine to downlaod File to PC                                    *
*----------------------------------------------------------------------*
* PT_ITAB                                                              *
* PC_FILE ==> Filename                                                 *
* PC_TYPE ==> Filetype                                                 *
*----------------------------------------------------------------------*
form download tables pt_itab
              using  pc_file type string
                     pc_type type char10.

  call function 'GUI_DOWNLOAD'
    exporting
      filename                = pc_file
      filetype                = pc_type
    tables
      data_tab                = pt_itab
    exceptions
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
      others                  = 22.

  if sy-subrc ne 0.
    message 'Error during file download' type 'S'.
  endif.                               " IF SY-SUBRC NE 0
endform.                               " DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  CHECK_PROG_STATUS                                        *
*&---------------------------------------------------------------------*
* Subroutine to check program status                                   *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form check_prog_status .

  select obj_name
    from dwinactiv
    into w_obj
      up to 1 rows
   where obj_name = p_prog.

  endselect.                           " SELECT OBJ_NAME...

  if sy-subrc eq 0.
    message 'Given program is inactive, activate it before downloading'
       type 'S'.
    stop.
  endif.                               " IF SY-SUBRC EQ 0
endform.                               " CHECK_PROG_STATUS

*&---------------------------------------------------------------------*
*&      Form  CHECK_PROG                                               *
*&---------------------------------------------------------------------*
* Subroutine to check if the program exists                            *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form check_prog .

*  if p_prog+0(1) = 'Y'
*  or p_prog+0(1) = 'Z'.
    select single name                 " ABAP Program Name
             from trdir
             into w_name
            where name = p_prog.

    if sy-subrc eq 0.

      concatenate 'Program '
                   p_prog
                  ' already exists,'
                  'do you want to overwrite it?'
             into w_str
     separated by space.

      call function 'POPUP_TO_CONFIRM'
        exporting
          text_question         = w_str
          display_cancel_button = ' '
        importing
          answer                = w_ans
        exceptions
          text_not_found        = 1
          others                = 2.
      if sy-subrc eq 0.
* If user doesn't want to overwrite the existing program,
* Stop and come out of the program
        if w_ans = '2'.
          stop.
* If the user wants to overwrite the existing program,
* delete it and continue
        else.
          call function 'RS_DELETE_PROGRAM'
            exporting
              program            = p_prog
              with_cua           = 'X'
            exceptions
              enqueue_lock       = 1
              object_not_found   = 2
              permission_failure = 3
              reject_deletion    = 4.

          if sy-subrc eq 1.
            message
            'Another User is currently editing the given program'
               type 'S'.
            stop.
          endif.                       " IF SY-SUBRC EQ 1
        endif.                         " IF W_ANS = '2'
      endif.                           " IF SY-SUBRC EQ 0
      clear w_str.
    endif.                             " IF SY-SUBRC EQ 0
*  else.
*    message 'Test objects cannot be created in foreign namespaces'
*       type 'S'.
*    stop.
*  endif.                               " IF P_PROG+0(1) = 'Y'...
endform.                               " CHECK_PROG

*&---------------------------------------------------------------------*
*&      Form  UPLOAD                                                   *
*&---------------------------------------------------------------------*
* Subroutine to Upload file data to internal table                     *
*----------------------------------------------------------------------*
* PT_ITAB                                                              *
* PC_FILE ==> Filename                                                 *
* PC_TYPE ==> Filetype                                                 *
*----------------------------------------------------------------------*
form upload  tables   pt_itab
             using    pc_file type string
                      pc_type type char10.

  call function 'GUI_UPLOAD'
    exporting
      filename                = pc_file
      filetype                = pc_type
    tables
      data_tab                = pt_itab
    exceptions
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
      others                  = 17.

  if sy-subrc ne 0.
    message 'Error during file upload' type 'S'.
  endif.                               " IF SY-SUBRC NE 0
endform.                               " UPLOAD

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA                                             *
*&---------------------------------------------------------------------*
* Subroutine to process data                                           *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form process_data .

  loop at t_data into fs_data.
    clear: fs_doc,
           fs_str.
    move sy-tabix to w_index.

    case fs_data+0(1).
* Header Text
      when 'H'.
        delete t_data index w_index.

* Code
      when 'C'.
        move fs_data+1 to fs_code.
        append fs_code to t_code.
        clear  fs_code.
        delete t_data index w_index.

* Documentation
      when 'D'.
        move fs_data+1 to fs_doc.
        if fs_doc+0(5) = 'PGMID'.
          shift fs_doc by 5 places.
          move fs_doc to w_pgmid.

        elseif fs_doc+0(6) = 'OBJECT'.
          shift fs_doc by 6 places.
          move fs_doc to w_object.
        endif.                         " IF FS_DOC+0(5) = 'PGMID'

* Attributes
      when 'A'.
        move fs_data+1 to fs_doc.
        if fs_doc+0(4) = 'SUBC'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-subc.

        elseif fs_doc+0(5) = 'FIXPT'.
          shift fs_doc by 5 places.
          move fs_doc to fs_dir-fixpt.

        elseif fs_doc+0(7) = 'UCCHECK'.
          shift fs_doc by 7 places.
          move fs_doc to fs_dir-uccheck.

        elseif fs_doc+0(4) = 'SECU'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-secu.

        elseif fs_doc+0(4) = 'EDTX'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-edtx.

        elseif fs_doc+0(4) = 'SSET'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-sset.

        elseif fs_doc+0(7) = 'LDBNAME'.
          shift fs_doc by 7 places.
          move fs_doc to fs_dir-ldbname.

        elseif fs_doc+0(4) = 'APPL'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-appl.

        elseif fs_doc+0(5) = 'RSTAT'.
          shift fs_doc by 5 places.
          move fs_doc to fs_dir-rstat.

        elseif fs_doc+0(4) = 'TYPE'.
          shift fs_doc by 4 places.
          move fs_doc to fs_dir-type.
        endif.                         " IF FS_DOC+0(4)..

        delete t_data index w_index.

* PF-STATUS
      when 'P'.
        move fs_data+1 to fs_doc.
        if fs_doc+0(3) = 'TRK'.
          fs_str = fs_doc+4.
          split fs_str at ';'
                     into fs_trkey-devclass
                          fs_trkey-obj_type
                          fs_trkey-obj_name.
        endif.                         " IF FS_DOC+0(3)

* Text elements
      when 'T'.
        move fs_data to fs_data2.
        append fs_data2 to t_data2.
        clear  fs_data2.
        delete t_data index w_index.
    endcase.                           " CASE T_DATA+0(1)
  endloop.                             " LOOP AT T_DATA...
endform.                               " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  CREATE_PROG                                              *
*&---------------------------------------------------------------------*
* Subroutine to create new program                                     *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine    *
*----------------------------------------------------------------------*
form create_prog .

* Creates a new program uploading source code and attributes
  insert report p_prog
           from t_code
directory entry fs_dir.

* Create TADIR entry for the new program
  call function 'TR_TADIR_POPUP_ENTRY_E071'
    exporting
      wi_e071_pgmid             = w_pgmid
      wi_e071_object            = w_object
      wi_e071_obj_name          = w_prog2
    importing
      we_tadir                  = fs_tadir
      es_tdevc                  = fs_tdevc
    exceptions
      display_mode              = 1
      exit                      = 2
      global_tadir_insert_error = 3
      no_repair_selected        = 4
      no_systemname             = 5
      no_systemtype             = 6
      no_tadir_type             = 7
      reserved_name             = 8
      tadir_enqueue_failed      = 9
      devclass_not_found        = 10
      tadir_not_exist           = 11
      object_exists             = 12
      internal_error            = 13
      object_append_error       = 14
      tadir_modify_error        = 15
      object_locked             = 16
      no_object_authority       = 17
      others                    = 18.

  if sy-subrc ne 0.
    message 'Error while creating TADIR entry' type 'S'.
  endif.                               " IF SY-SUBRC NE 0

* Upload text elements to the new program,
* Using translation they can be maintained in different languages
  move 1 to w_index.

  describe table t_data2 lines w_cnt2.

  loop at t_data2 into fs_data2.
    w_cnt3 = w_cnt3 + 1.
    clear: fs_doc,fs_str.

    if w_index = 1.
      move fs_data2+3(1) to w_char.
    endif.                             " IF W_INDEX = 1
* Check if language is same
    if w_char = fs_data2+3(1).
      move fs_data2+6 to fs_doc.
      split fs_doc at '*%'
               into fs_txt-id
                    fs_txt-key
                    fs_txt-entry
                    w_len.
      move w_len to fs_txt-length.
      append fs_txt to t_txt.
      clear  fs_txt.
      w_index = w_index + 1.
* If it comes to last line of the internal table
      if w_cnt3 = w_cnt2.
* Upload text elements to the new program
        insert textpool p_prog from t_txt
                               language w_char.
        clear: w_char,
               fs_doc,
               fs_txt,
               t_txt[].
      endif.                           " IF W_CNT3 = W_CNT2
* If language changes, insert text elements up to here
* into the given language
    else.
* Upload text elements to the new program
      insert textpool p_prog from t_txt
                             language w_char.
      clear: w_char,
             fs_doc,
             t_txt,
             t_txt[].
* Append 1st line of new language here
      move fs_data2+6 to fs_doc.
      split fs_doc at '*%'
               into fs_txt-id
                    fs_txt-key
                    fs_txt-entry
                    w_len.
      move w_len to fs_txt-length.
      append fs_txt to t_txt.
      clear  fs_txt.
      move 1 to w_index.
    endif.                             " IF W_CHAR =...
  endloop.                             " LOOP AT T_DATA2

  loop at t_data into fs_data.
    clear: fs_doc,
           fs_str.

    case fs_data+0(1).
* Documentation
      when 'D'.
        move fs_data+1 to fs_doc.

        if fs_doc+0(4) = 'LINE'.
          move fs_doc+5 to fs_str.
          split fs_str at ';'
                     into fs_tline-tdformat
                          fs_tline-tdline.
          append fs_tline to t_tline.
          clear: fs_tline,
                 fs_str.

        elseif fs_doc+0(4)    = 'HEAD'.
          move fs_doc+5 to fs_str.
          split fs_str at ';'
                     into  fs_thead-tdobject   fs_thead-tdname
                           fs_thead-tdid       fs_thead-tdspras
                           fs_thead-tdtitle    fs_thead-tdform
                           fs_thead-tdstyle    fs_thead-tdversion
                           fs_thead-tdfuser    fs_thead-tdfreles
                           fs_thead-tdfdate    fs_thead-tdftime
                           fs_thead-tdluser    fs_thead-tdlreles
                           fs_thead-tdldate    fs_thead-tdltime
                           fs_thead-tdlinesize fs_thead-tdtxtlines
                           fs_thead-tdhyphenat fs_thead-tdospras
                           fs_thead-tdtranstat fs_thead-tdmacode1
                           fs_thead-tdmacode2  fs_thead-tdrefobj
                           fs_thead-tdrefname  fs_thead-tdrefid
                           fs_thead-tdtexttype fs_thead-tdcompress
                           fs_thead-mandt      fs_thead-tdoclass
                           fs_thead-logsys.

          clear fs_thead-tdname.
          move w_prog3 to fs_thead-tdname.
          clear fs_str.

        elseif fs_doc+0(8) = 'DOKSTATE'.
          shift fs_doc by 8 places.
          move fs_doc to w_state.

        elseif fs_doc+0(3) = 'TYP'.
          shift fs_doc by 3 places.
          move fs_doc to w_typ.

        elseif fs_doc+0(10) = 'DOKVERSION'.
          shift fs_doc by 10 places.
          move fs_doc to w_version.

* Update
          call function 'DOCU_UPDATE'
            exporting
              head    = fs_thead
              state   = w_state
              typ     = w_typ
              version = w_version
            tables
              line    = t_tline.

          clear: fs_tline,
                 t_tline[],
                 fs_thead,
                 w_state,
                 w_typ,
                 w_version.
        endif.                         " IF FS_DOC+0(4) = 'LINE'

* PF-Status
      when 'P'.
        move fs_data+1 to fs_doc.

        if fs_doc+0(3) = 'LAN'.
          move fs_doc+3 to w_lang.

        elseif fs_doc+0(3) = 'STA'.
          perform populate_pf_tabs tables t_sta
                                    using 'FS_STA'
                                          fs_sta
                                          c_stat.

        elseif fs_doc+0(3) = 'FUN'.
          perform populate_pf_tabs tables t_fun
                                    using 'FS_FUN'
                                          fs_fun
                                          c_funt.

        elseif fs_doc+0(3) = 'MEN'.
          perform populate_pf_tabs tables t_men
                                    using 'FS_MEN'
                                          fs_men
                                          c_men.

        elseif fs_doc+0(3) = 'MTX'.
          perform populate_pf_tabs tables t_mtx
                                    using 'FS_MTX'
                                          fs_mtx
                                          c_mnlt.

        elseif fs_doc+0(3) = 'ACT'.
          perform populate_pf_tabs tables t_act
                                    using 'FS_ACT'
                                          fs_act
                                          c_act.

        elseif fs_doc+0(3) = 'BUT'.
          perform populate_pf_tabs tables t_but
                                    using 'FS_BUT'
                                          fs_but
                                          c_but.

        elseif fs_doc+0(3) = 'PFK'.
          perform populate_pf_tabs tables t_pfk
                                    using 'FS_PFK'
                                          fs_pfk
                                          c_pfk.

        elseif fs_doc+0(3) = 'SET'.
          perform populate_pf_tabs tables t_set
                                    using 'FS_SET'
                                          fs_set
                                          c_staf.

        elseif fs_doc+0(3) = 'ATR'.
          perform populate_pf_tabs tables t_atrt
                                    using 'FS_ATRT'
                                          fs_atrt
                                          c_atrt.

        elseif fs_doc+0(3) = 'TIT'.
          perform populate_pf_tabs tables t_tit
                                    using 'FS_TIT'
                                          fs_tit
                                          c_titt.

        elseif fs_doc+0(3) = 'BIV'.
          perform populate_pf_tabs tables t_biv
                                    using 'FS_BIV'
                                          fs_biv
                                          c_buts.

        elseif fs_doc+0(3) = 'ADM'.
          move fs_doc+4 to fs_str.
          split fs_str at ';'
                     into fs_adm-actcode
                          fs_adm-mencode
                          fs_adm-pfkcode
                          fs_adm-defaultact
                          fs_adm-defaultpfk
                          fs_adm-mod_langu.

* Upload PF-STATUS to the new program
          call function 'RS_CUA_INTERNAL_WRITE'
            exporting
              program   = p_prog
              language  = w_lang
              tr_key    = fs_trkey
              adm       = fs_adm
            tables
              sta       = t_sta
              fun       = t_fun
              men       = t_men
              mtx       = t_mtx
              act       = t_act
              but       = t_but
              pfk       = t_pfk
              set       = t_set
              doc       = t_atrt
              tit       = t_tit
              biv       = t_biv
            exceptions
              not_found = 1
              others    = 2.

          if sy-subrc ne 0.
            message 'Error during PF-STATUS upload' type 'S'.
          endif.                       " IF SY-SUBRC NE 0
          clear: w_lang, fs_adm,
                 fs_sta, t_sta[],
                 fs_fun, t_fun[],
                 fs_men, t_men[],
                 fs_mtx, t_mtx[],
                 fs_act, t_act[],
                 fs_but, t_but[],
                 fs_pfk, t_pfk[],
                 fs_set, t_set[],
                 fs_atrt,t_atrt[],
                 fs_tit, t_tit[],
                 fs_biv, t_biv[].
        endif.                         " IF FS_DOC+0(3) = 'LAN'
    endcase.                           " CASE FS_DATA+0(1)
  endloop.                             " LOOP AT T_DATA...

  syntax-check for t_code message w_mess
                             line w_lin
                             word w_wrd
                          program p_prog.
  if sy-subrc ne 0.
    concatenate 'Program '
                 p_prog
                ' is syntactically incorrect,'
                'correct it before executing'
           into w_str
   separated by space.

    message w_str type 'S'.
    clear w_str.
    stop.
  else.
    concatenate p_prog
                ' created successfully'
           into w_str
   separated by space.

    message w_str type 'S'.
    clear w_str.
  endif.                               " IF SY-SUBRC NE 0
endform.                               " CREATE_PROG

*&---------------------------------------------------------------------*
*&      Form  download_pf_tabs                                         *
*&---------------------------------------------------------------------*
* This subroutine downloads PF Tabs                                    *
*----------------------------------------------------------------------*
*  PT_TAB                                                              *
*  PC_TABNAME ==> Text                                                 *
*  PC_WA      ==> Text                                                 *
*  PC_TXT     ==> Text                                                 *
*  PC_CONS    ==> Text                                                 *
*----------------------------------------------------------------------*
form download_pf_tabs tables pt_tab
                       using pc_tabname
                             pc_wa
                             pc_txt
                             pc_cons.
  clear: fs_dd03l,t_dd03l[].

  select fieldname
    from dd03l
    into table t_dd03l
   where tabname = pc_tabname.

  if sy-subrc eq 0.
    clear: w_cnt3.
    loop at t_dd03l into fs_dd03l where fieldname = '.INCLUDE'.
      delete table t_dd03l from fs_dd03l.
    endloop.                           " LOOP AT T_DD03L INTO...
    describe table t_dd03l lines w_cnt3.
  endif.                               " IF SY-SUBRC EQ 0

  loop at pt_tab into pc_wa.
    clear: w_index,
           w_field,
           fs_pfs.

    loop at t_dd03l into fs_dd03l.
      move sy-tabix to w_index.
      concatenate pc_txt fs_dd03l-fieldname into w_field.
      condense w_field no-gaps.
      assign (w_field) to <fs1>.
      if <fs1> is assigned.
        if w_index = 1.
          concatenate pc_cons
                      fs_dd03l-fieldname '*' <fs1>
                      into fs_pfs.
        else.
          concatenate fs_pfs
                      ';'
                      fs_dd03l-fieldname '*' <fs1>
                      into fs_pfs.
        endif.                         " IF W_INDEX = 1
      endif.                           " IF <FS1> IS ASSIGNED
    endloop.                           " LOOP AT T_DD03L INTO...
    append fs_pfs to t_pfs.
  endloop.                             " LOOP AT P_TAB INTO P_WA
endform.                               " DOWNLOAD_PF_TABS

*&---------------------------------------------------------------------*
*&      Form  POPULATE_PF_TABS                                         *
*&---------------------------------------------------------------------*
* This subroutine populates PF Tabs                                    *
*----------------------------------------------------------------------*
* PT_TAB                                                               *
* PC_WANAME  ==>  Text                                                 *
* PC_WA      ==>  Text                                                 *
* PC_STRUCT  ==>  Text                                                 *
*----------------------------------------------------------------------*
form populate_pf_tabs tables pt_tab
                       using pc_waname
                             pc_wa
                             pc_struct.

  unassign: <fs1>.

  field-symbols: <fs_wa>.

  clear: w_str,
         w_cnt2,
         fs_str.

  select fieldname
    from dd03l
    into table t_dd03l
   where tabname = pc_struct.

  if sy-subrc eq 0.
    sort t_dd03l.

    move fs_doc+3 to fs_str.
    assign (pc_waname) to <fs_wa>.

    while not fs_str is initial.
      if fs_str cs c_sep.
        move sy-fdpos to w_cnt2.
        move fs_str+0(w_cnt2) to w_str.
        w_cnt2 = w_cnt2 + 1.
        shift fs_str by w_cnt2 places left.

        if w_str cs c_sep2.
          clear: w_cnt2.
          move sy-fdpos to w_cnt2.
          move w_str+0(w_cnt2) to w_wrd.
          w_cnt2 = w_cnt2 + 1.
          move w_str+w_cnt2 to w_val.

          read table t_dd03l into fs_dd03l with key
                        fieldname = w_wrd binary search.
          if sy-subrc eq 0.
            if <fs_wa> is assigned.
              assign component fs_dd03l-fieldname of
                       structure <fs_wa> to <fs1>.
              if <fs1> is assigned.
                move w_val to <fs1>.
                unassign <fs1>.
              endif.                   " IF <FS1> IS ASSIGNED
            endif.                     " IF <FS_WA> IS ASSIGNED
            clear: w_cnt2,
                   w_str,
                   w_wrd,
                   w_val,
                   fs_dd03l.
          endif.                       " IF SY-SUBRC EQ 0
        endif.                         " IF W_STR CS C_SEP2
      else.
        if fs_str cs c_sep2.
          clear: w_cnt2.
          move sy-fdpos to w_cnt2.
          move fs_str+0(w_cnt2) to w_wrd.
          w_cnt2 = w_cnt2 + 1.
          move fs_str+w_cnt2 to w_val.

          read table t_dd03l into fs_dd03l with key
                        fieldname = w_wrd binary search.
          if sy-subrc eq 0.
            if <fs_wa> is assigned.
              assign component fs_dd03l-fieldname of
                       structure <fs_wa> to <fs1>.
              if <fs1> is assigned.
                move w_val to <fs1>.
                unassign <fs1>.
              endif.                   " IF <FS1> IS ASSIGNED
            endif.                     " IF <FS_WA> IS ASSIGNED
            clear: w_cnt2,
                 w_str,
                 w_wrd,
                 w_val,
                 fs_dd03l,
                 fs_str.
          endif.                       " IF SY-SUBRC EQ 0
        endif.                         " IF FS_STR CS C_SEP2
      endif.                           " IF FS_STR CS C_SEP
    endwhile.                          " WHILE NOT FS_STR IS INITIAL

    append pc_wa to pt_tab.
    clear  pc_wa.
  endif.                               " IF SY-SUBRC EQ 0

  unassign: <fs1>,
            <fs_wa>.
endform.                               " POPULATE_PF_TABS
