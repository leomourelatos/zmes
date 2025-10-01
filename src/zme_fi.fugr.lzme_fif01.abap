*----------------------------------------------------------------------*
***INCLUDE LZME_FIF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIND_FIELD_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD_STATUS  text
*      -->P_ZBKPF  text
*      -->P_ZBSEG  text
*----------------------------------------------------------------------*
form find_field_status tables p_status structure zet_field_status_str
                       using  p_zbkpf STRUCTURE bkpf
                              p_zbseg STRUCTURE bseg.

  if p_zbseg-umskz <> ' '.
    select single * from t074u
      into wa_t074u
      where umskz = p_zbseg-umskz
      and   koart = wa_tbsl-koart.
    if sy-subrc = 0.
      if wa_t074u-umsks = 'A'.
        if p_zbseg-zfbdt = ' ' or p_zbseg-zfbdt = '00000000'.
          raise special_gl_nodate.
        endif.
      endif.
    endif.
  endif.

  select single * from skb1
    into wa_skb1
    where saknr = p_zbseg-hkont
    and   bukrs = p_zbkpf-bukrs.

  call function 'Z_FIND_FIELD_STATUS_ATTRIBUTES'
    exporting
      s_fstva      = wa_t001-fstva
      s_group      = wa_skb1-fstag
      s_koart      = wa_tbsl-koart
      s_umsks      = wa_t074u-umsks
    tables
      field_status = p_status.

endform.                    " FIND_FIELD_STATUS
*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  L_BDCDATA                                                     *
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
form bdc_dynpro tables l_bdcdata structure bdcdata
                using program dynpro.

  clear l_bdcdata.
  l_bdcdata-program = program.
  l_bdcdata-dynpro  = dynpro.
  l_bdcdata-dynbegin = 'X'.
  append l_bdcdata.

endform.                    "bdc_dynpro
*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  L_BDCDATA                                                     *
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
form bdc_field tables l_bdcdata structure bdcdata
               using fnam fval.

  clear l_bdcdata.
  l_bdcdata-fnam = fnam.
  l_bdcdata-fval = fval.
  append l_bdcdata.

endform.                    "bdc_field
