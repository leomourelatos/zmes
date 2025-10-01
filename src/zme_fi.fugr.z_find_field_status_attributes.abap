FUNCTION Z_FIND_FIELD_STATUS_ATTRIBUTES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(S_FSTVA) TYPE  BUKRS
*"     REFERENCE(S_GROUP) TYPE  FSTAG
*"     REFERENCE(S_KOART) TYPE  KOART
*"     REFERENCE(S_UMSKS) TYPE  KOART
*"  TABLES
*"      FIELD_STATUS STRUCTURE  ZME_FIELD_STATUS_ST
*"----------------------------------------------------------------------
  data:
    type.
  select * from t004f into table i_t004f   "
           where bukrs = s_fstva
             and fstag = s_group.
  loop at i_t004f into w_t004f.          " field status groups (FSG)
*    skip 1.
*    write: / w_t004f-fstag.
    clear gruptab.  refresh gruptab.

    incoming_string        = w_t004f-faus1.
    incoming_string+90(50) = w_t004f-faus2.
*
    select * from tmodf where fauna = 'SKB1-FAUS1'.
      gruptab-ggrup = tmodf-ggrup.
      select single * from tmodg where spras = sy-langu
                                 and   fauna = tmodf-fauna
                                 and   ggrup = tmodf-ggrup.
      if sy-subrc = 0.
        gruptab-ftext = tmodg-ftext.
      else.
        gruptab-ftext = text-ngg.
      endif.
      append gruptab.
*      skip 1.
*      write: /5 gruptab-ftext,
*             50 'Suppress',
*             60 'Required',
*             70 'Optional'.
      select * from tmodo where fauna = 'SKB1-FAUS1'
                            and ggrup = tmodf-ggrup.

        select single * from tmodp where spras = sy-langu
                                   and   fauna = tmodo-fauna
                                   and   modif = tmodo-modif.

        select * from tmodu
          where fauna = tmodo-fauna
          and   modif = tmodo-modif
          and   koart = s_koart
          and   umsks = s_umsks.
.
*          write: /10 tmodp-ftext.
* get individual field options
          write tmodp-modif to w_string_position.
          w_string_position = w_string_position - 1.
          case incoming_string+w_string_position(1).
            when '+'.
*              write: 60 'X'.             " obligatory
              write: 'O' to field_status-field_type.
            when '.'.
*              write: 70 'X'.             " optional.
              write: 'P' to field_status-field_type.
            when '-'.
*              write: 50 'X'.             " supress
              write: 'S' to field_status-field_type.
          endcase.
**      write: 80 tmodu-tabnm, tmodu-feldn.
*          write: tmodu-tabnm to field_status-tabnm,
*                 tmodu-feldn to field_status-feldn.
          field_status-tabnm = tmodu-tabnm.
          field_status-feldn = tmodu-feldn.
          CONCATENATE tmodu-tabnm '-' tmodu-feldn
            into field_status-tabnm_feldn.
          append field_status.

        endselect.
      endselect.  " FROM tmodp
    endselect.  "  FROM tmodf

  endloop.                                                  " i_t004f

sort field_status.
endfunction.
