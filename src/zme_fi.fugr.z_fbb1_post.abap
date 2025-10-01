function z_fbb1_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SIMULATE) TYPE  CHAR1
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'N'
*"     REFERENCE(POST) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(TRANS) TYPE  TCODE DEFAULT 'FBB1'
*"     REFERENCE(NO_BALANCE_CHECK) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(VAT_ANALYSIS) TYPE  CHAR1 DEFAULT ' '
*"  EXPORTING
*"     VALUE(BELNR) TYPE  BKPF-BELNR
*"     VALUE(GJAHR) TYPE  BKPF-GJAHR
*"  TABLES
*"      ZBKPF STRUCTURE  BKPF
*"      ZBSEG STRUCTURE  BSEG
*"      MESSTAB STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      CREDIT_VS_DEBIT
*"      WRONG_POSTING_KEY
*"      NO_ACCOUNT
*"      WRONG_ACCOUNT
*"      ACCOUNT_NOT_IN_BUKRS
*"      ACCOUNT_NOT_IN_SAP
*"      SPECIAL_GL_NODATE
*"      SPECIAL_GL_ERROR
*"      FIELDS_MISSING
*"      SUPRESSED_FIELD
*"----------------------------------------------------------------------
  data:
*    budat(10),
*    bldat(10),
    zfbdt(10),
    dmbtr(13),
    wrbtr(13),
    newbs like rf05a-newbs,
    newko like rf05a-newko,
    newum like rf05a-newum,
    pnl,
    xnegp,
    tabix like sy-tabix,
    gsber like zbseg-gsber,
    projk like zbseg-projk,
    posid like prps-posid,
    kostl like zbseg-kostl,
    aufnr like zbseg-aufnr,
    pernr like zbseg-pernr,
*    zzkodikos like zbseg-zzkodikos,
    rmvct like cobl-rmvct,
    scrnum(4),
    begin of taxtab occurs 0,
      saknr  like bseg-saknr,
      fwste  like bset-fwste,
      bschl  like bseg-bschl,
      mwskz  like bseg-mwskz,
      mwskzz like bseg-mwskz,
    end   of taxtab,
    begin of taxtab_all occurs 0,
      saknr  like bseg-saknr,
      fwste  like bset-fwste,
      bschl  like bseg-bschl,
      mwskz  like bseg-mwskz,
      mwskzz like bseg-mwskz,
    end   of taxtab_all,
    begin of kosttab occurs 0,
*      bschl like bseg-bschl,
      gsber like bseg-gsber,
*      projk like bseg-projk,
*      kostl like bseg-kostl,
*      aufnr like bseg-aufnr,
*      pernr like bseg-pernr,
*      bewar like bseg-bewar,
      mwsts like bseg-wmwst,
      mwskz like bseg-mwskz,
      sgtxt like bseg-sgtxt,
    end   of kosttab,
    mwsts like bseg-mwsts,
    buzei like bseg-buzei,
    wa_bseg type bseg,
    p_bseg type bseg occurs 0 with header line,
    sgtxt type bseg-sgtxt,
    kost_num(3) type n,
    bseg_num(3) type n,
    dmbtr_check like zbseg-dmbtr,
    wrbtr_check like zbseg-wrbtr,
    lv_index like sy-tabix,
    field_status  like  zet_field_status_str occurs 0 with header line,
    taxflag.

  field-symbols:
    <test_field>.
  data:
    test_field(100).

  set parameter id 'BLN' field ' '.
  set parameter id 'GJR' field ' '.

  clear: belnr,
         gjahr.

  clear: messtab, messtab[].

  read table zbkpf index 1.

  select single * from t001
    into wa_t001
    where bukrs = zbkpf-bukrs.

  select single * from tka02
    into wa_tka02
    where bukrs = zbkpf-bukrs.

*  data:
*    lta_fcat type slis_t_fieldcat_alv,
*    ltb_fcat type slis_t_fieldcat_alv,
*    wa_fcat  type slis_fieldcat_alv.
*
*  free lta_fcat.
*  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
*    exporting
*      i_program_name         = sy-repid
*      i_structure_name       = 'BSEG'
*      i_bypassing_buffer     = abap_true
*    changing
*      ct_fieldcat            = lta_fcat
*    exceptions
*      inconsistent_interface = 1
*      program_error          = 2
*      others                 = 3.

  clear: dmbtr_check, wrbtr_check, kosttab, kosttab[], sgtxt.
  loop at zbseg.
    lv_index = sy-tabix.
    if zbseg-lifnr is initial and
       zbseg-kunnr is initial and
       zbseg-saknr is initial.
      raise no_account.
    endif.
    select single * from tbsl
      into wa_tbsl
      where bschl = zbseg-bschl.
    if sy-subrc = 0.
      if wa_tbsl-koart = 'K'.
        perform check_lifnr using zbseg zbkpf wa_t001.
        perform check_saknr using zbseg-hkont
                                  zbseg-hkont
                                  ' '
                                  ' '
                                  zbkpf-bukrs
                                  wa_t001-ktopl.
        if lv_index = 1.
          sgtxt = zbseg-sgtxt.
        endif.
      elseif wa_tbsl-koart = 'D'.
        perform check_kunnr using zbseg zbkpf wa_t001.
        perform check_saknr using zbseg-hkont
                                  zbseg-hkont
                                  ' '
                                  ' '
                                  zbkpf-bukrs
                                  wa_t001-ktopl.
        if lv_index = 1.
          sgtxt = zbseg-sgtxt.
        endif.
      elseif wa_tbsl-koart = 'S'.
        perform check_saknr using zbseg-saknr
                                  zbseg-hkont
                                  zbseg-lifnr
                                  zbseg-kunnr
                                  zbkpf-bukrs
                                  wa_t001-ktopl.
      endif.
    else.
      message id '00' type 'E' number 398
      with 'Λαθος Κλειδί Καταχώρησης' zbseg-bschl
      raising wrong_posting_key.
    endif.


    free field_status.
    clear field_status.
    perform find_field_status tables field_status
                              using zbkpf
                                    zbseg.

*    free ltb_fcat.
*    break mourelatosjr.
*    loop at lta_fcat into wa_fcat
*      where key = ' '.
*      if wa_fcat-fieldname = 'KOSTL' or
*         wa_fcat-fieldname = 'AUFNR' or
*         wa_fcat-fieldname = 'PROJK' or
*         wa_fcat-fieldname = 'BEWAR' or
*         wa_fcat-fieldname = 'PERNR' or
*         wa_fcat-fieldname = 'MENGE' or
*         wa_fcat-fieldname = 'MEINS' or
*         wa_fcat-fieldname = 'ZZKODIKOS' or
*         wa_fcat-fieldname = 'ZZMENGE' or
*         wa_fcat-fieldname = 'ZZMEINS'.
*        clear test_field.
*        concatenate 'ZBSEG-' wa_fcat-fieldname into test_field.
*        assign (test_field) to <test_field>.
*        if <test_field> is not initial.
*          append wa_fcat to ltb_fcat.
*        endif.
*      endif.
*    endloop.

    loop at field_status where field_type = 'O'.
      clear test_field.
      concatenate 'Z' field_status-tabnm_feldn into test_field.
      assign test_field to <test_field>.
      if <test_field> is initial.
        message id '00' type 'E' number 398
        with 'Field' <test_field> 'is missing' raising fields_missing.
*        raise fields_missing.
      endif.
    endloop.

    loop at field_status where field_type = 'S'.
      clear test_field.
      concatenate 'Z' field_status-tabnm_feldn into test_field.
      assign (test_field) to <test_field>.
      if <test_field> is not initial.
        message id '00' type 'E' number 398
        with zbseg-buzei test_field raising supressed_field.
*        RAISE supressed_field.
      endif.
    endloop.

*    loop at ltb_fcat into wa_fcat.
*      read table field_status
*        with key feldn = wa_fcat-fieldname.
*      if sy-subrc <> 0.
*        message id '00' type 'E' number 398
*        with 'Το πεδίο' wa_fcat-seltext_s 'δεν υπάρχει στην καταχώρηση'
*          raising fields_missing.
**        raise fields_missing.
*      endif.
*    endloop.

    if wa_tbsl-shkzg = 'S'.
      if zbseg-bschl = '31' or zbseg-bschl = '21' or
         zbseg-bschl = '01' or zbseg-bschl = '11' or
         zbseg-bschl = '02' or zbseg-bschl = '12'.
        dmbtr_check = dmbtr_check + zbseg-dmbtr - zbseg-wmwst.
        wrbtr_check = wrbtr_check + zbseg-wrbtr - zbseg-wmwst.
      else.
        dmbtr_check = dmbtr_check + zbseg-dmbtr + zbseg-wmwst.
        wrbtr_check = wrbtr_check + zbseg-wrbtr + zbseg-wmwst.
      endif.
    elseif wa_tbsl-shkzg = 'H'.
      if zbseg-bschl = '31' or zbseg-bschl = '21' or
         zbseg-bschl = '01' or zbseg-bschl = '11' or
         zbseg-bschl = '02' or zbseg-bschl = '12'.
        dmbtr_check = dmbtr_check - zbseg-dmbtr + zbseg-wmwst.
        wrbtr_check = wrbtr_check - zbseg-wrbtr + zbseg-wmwst.
      else.
        dmbtr_check = dmbtr_check - zbseg-dmbtr - zbseg-wmwst.
        wrbtr_check = wrbtr_check - zbseg-wrbtr - zbseg-wmwst.
      endif.
    endif.
*    modify zbseg index lv_index.

    if vat_analysis = 'X'.
      if zbseg-mwskz = '35' or "23%
         zbseg-mwskz = '42' or
         zbseg-mwskz = '43' or
         zbseg-mwskz = '44' or
         zbseg-mwskz = '64' or
         zbseg-mwskz = '6M' or
         zbseg-mwskz = '6T' or
         zbseg-mwskz = '1N' or
         zbseg-mwskz = '2A' or
         zbseg-mwskz = '8D' or

         zbseg-mwskz = '33' or "24%
         zbseg-mwskz = '34' or
         zbseg-mwskz = '45' or
         zbseg-mwskz = '46' or
         zbseg-mwskz = '4A' or
         zbseg-mwskz = '5L' or
         zbseg-mwskz = '8I' or
         zbseg-mwskz = '5S' or
         zbseg-mwskz = '1Y' or
         zbseg-mwskz = '8U'.

        clear kosttab.
        move-corresponding zbseg to kosttab.
        kosttab-sgtxt = sgtxt.
*        clear kosttab-bschl.
        if zbseg-bschl = 50.
          kosttab-mwsts = - kosttab-mwsts.
        endif.
        collect kosttab.

        if zbseg-bschl = 50.
          mwsts = mwsts - zbseg-mwsts.
        else.
          mwsts = mwsts + zbseg-mwsts.
        endif.
      endif.
    endif.
  endloop.

  if vat_analysis = 'X'.
    clear: p_bseg, p_bseg[].

    buzei = '001'.
    loop at zbseg into wa_bseg.
*    lv_index = sy-tabix.
      wa_bseg-buzei = buzei.

      append wa_bseg to p_bseg.

      if wa_bseg-mwskz = '35' or "23%
         wa_bseg-mwskz = '42' or
         wa_bseg-mwskz = '43' or
         wa_bseg-mwskz = '44' or
         wa_bseg-mwskz = '64' or
         wa_bseg-mwskz = '6M' or
         wa_bseg-mwskz = '6T' or
         wa_bseg-mwskz = '1N' or
         wa_bseg-mwskz = '2A' or
         wa_bseg-mwskz = '8D' or

         wa_bseg-mwskz = '33' or "24%
         wa_bseg-mwskz = '34' or
         wa_bseg-mwskz = '45' or
         wa_bseg-mwskz = '46' or
         wa_bseg-mwskz = '4A' or
         wa_bseg-mwskz = '5L' or
         wa_bseg-mwskz = '8I' or
         wa_bseg-mwskz = '5S' or
         wa_bseg-mwskz = '1Y' or
         wa_bseg-mwskz = '8U'.

        select single * from t030k
          into wa_t030k
          where ktopl = wa_t001-ktopl
          and   ktosl = 'VST'
          and   mwskz = wa_bseg-mwskz.
        buzei = buzei + 1.
        wa_bseg-buzei  = buzei.
        wa_bseg-saknr  = wa_t030k-konth.
        wa_bseg-dmbtr  = wa_bseg-mwsts.
        append wa_bseg to p_bseg.
      endif.
*    modify zbseg index lv_index.
      buzei = buzei + 1.
    endloop.

    clear: zbseg, zbseg[].
    zbseg[] = p_bseg[].
    sort zbseg.
    clear: p_bseg, p_bseg[].


*  describe table kosttab lines kost_num.
*  describe table zbseg   lines bseg_num.
*
*  if vat_analysis = 'X' and kost_num > 1.
*    clear mwsts.
*    loop at kosttab.
*      mwsts = mwsts + kosttab-mwsts.
*
*      bseg_num = bseg_num + 1.
*
*      clear zbseg.
*      if mwsts > 0.
*        zbseg-bschl  = '40'.
*      else.
*        zbseg-bschl  = '50'.
*      endif.
*      zbseg-saknr  = '6398100000'.
*      zbseg-dmbtr  = kosttab-mwsts.
*      zbseg-mwskz  = kosttab-mwskz.
*      zbseg-sgtxt  = 'test analysi'.
*      zbseg-gsber  = kosttab-gsber.
*      zbseg-projk  = kosttab-projk.
**       zbseg-zuonr  = p_zuonr.
*      zbseg-kostl  = kosttab-kostl.
*      zbseg-aufnr  = kosttab-aufnr.
*      zbseg-xnegp  = ' '.
*      zbseg-buzei  = bseg_num.
*      zbseg-pernr  = kosttab-pernr.
*      zbseg-bewar  = kosttab-bewar.
**       zbseg-menge  = items-menge.
**       zbseg-meins  = items-meins.
**       if items-kilom <> 0.
**         zbseg-xref2  = items-kilom.
**       endif.
**       zbseg-zzmenge  = items-zzmenge.
**       zbseg-zzmeins  = items-zzmeins.
**       zbseg-zzkodikos  = items-zzkodikos.
*      append zbseg.
*      bseg_num = bseg_num + 1.
*    endloop.

    loop at kosttab.
      clear zbseg.
      if kosttab-mwsts < 0.
        zbseg-bschl  = '40'.
      else.
        zbseg-bschl  = '50'.
      endif.
      zbseg-saknr  = wa_t030k-konth.
      zbseg-dmbtr  = abs( kosttab-mwsts ).
      zbseg-mwskz  = kosttab-mwskz.
      zbseg-sgtxt  = kosttab-sgtxt.
      zbseg-gsber  = kosttab-gsber.
*      zbseg-kostl  = zetcc6398-kostl.
      zbseg-xnegp  = ' '.
      zbseg-buzei  = buzei.
      append zbseg.
      buzei = buzei + 1.
    endloop.
  endif.

*  break mourelatosjr.
  loop at zbseg
    where mwskz <> '**'
    and   mwskz <> '  '.

    select single * from a003
      into wa_a003
      where mwskz = zbseg-mwskz
      and   ( kschl = 'MWVS' or kschl = 'MWVZ' or
              kschl = 'MWVX' or kschl = 'MWAS' )
      and   aland = wa_t001-land1.

    select single * from konp
      into wa_konp
      where knumh =  wa_a003-knumh.

    if sy-subrc = 0." and konp-kbetr <> 0.

      select single * from t030k
        into wa_t030k
        where ktopl = wa_t001-ktopl
        and   mwskz = zbseg-mwskz
        and   konts <> ' '.
      clear taxtab_all.
      taxtab_all-mwskz = zbseg-mwskz.
      taxtab_all-saknr = wa_t030k-konts.
      if zbseg-bschl = '50'.
        taxtab_all-fwste = - zbseg-wmwst.
      elseif zbseg-bschl = '40'.
        taxtab_all-fwste = zbseg-wmwst.
      endif.
*      taxtab-bschl = zbseg-bschl.

      select single * from skb1
        into wa_skb1
        where bukrs = wa_t001-bukrs
        and   saknr = wa_t030k-konts.
      if sy-subrc = 0.
        taxtab_all-mwskzz = wa_skb1-mwskz.
      endif.
      collect taxtab_all.
      if wa_konp-kbetr <> 0.
        clear taxtab.
        taxtab = taxtab_all.
        collect taxtab.
      endif.
    endif.
  endloop.

  data:
    db_index like sy-tabix.

  sort taxtab by mwskz.
  loop at taxtab.
    db_index = sy-tabix.
    if taxtab-fwste < 0.
      taxtab-bschl = '50'.
      taxtab-fwste = - taxtab-fwste.
    elseif taxtab-fwste > 0.
      taxtab-bschl = '40'.
    endif.
    modify taxtab index db_index.
  endloop.

  if no_balance_check = ' '.
    if dmbtr_check <> 0 or wrbtr_check <> 0.
      message id '00' type 'E' number 398
      with 'Λαθος χρέωση - πίστωση'
      raising credit_vs_debit.
    endif.
  endif.

  clear: _bdcdata, _bdcdata[].

  write zbkpf-budat to budat dd/mm/yyyy.
  write zbkpf-bldat to bldat dd/mm/yyyy.

  perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '0100'.
  perform bdc_field   tables _bdcdata using 'BKPF-BLDAT'  bldat.
  perform bdc_field   tables _bdcdata using 'BKPF-BUDAT'  budat.
  perform bdc_field   tables _bdcdata using 'BKPF-BLART'  zbkpf-blart.
  perform bdc_field   tables _bdcdata using 'BKPF-BUKRS'  zbkpf-bukrs.
  perform bdc_field   tables _bdcdata using 'BKPF-MONAT'  zbkpf-monat.
  perform bdc_field   tables _bdcdata using 'BKPF-WAERS'  zbkpf-waers.
  perform bdc_field   tables _bdcdata using 'BKPF-XBLNR'  zbkpf-xblnr.
  perform bdc_field   tables _bdcdata using 'BKPF-BKTXT'  zbkpf-bktxt.
*  perform bdc_field   tables _bdcdata using 'FS006-DOCID' '*'.
  clear: pnl, gsber, projk, kostl, aufnr, posid, xnegp, taxflag.
  loop at zbseg.
    tabix = sy-tabix.

    free field_status.
    clear field_status.
    perform find_field_status tables field_status
                              using zbkpf
                                    zbseg.

    clear: newbs, newko, newum, scrnum.
    if zbseg-lifnr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-lifnr.
      newum = zbseg-umskz.
      scrnum = '0302'.
      if zbseg-umskz <> ' '.
        case zbseg-umskz.
          when 'O' or 'I' or 'A' or 'S' or 'C'. "per+
            scrnum = '0304'.
          when others.
            scrnum = '0303'.
        endcase.
      endif.
    elseif zbseg-kunnr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-kunnr.
      newum = zbseg-umskz.
      scrnum = '0301'.
      if zbseg-umskz <> ' '.
        case zbseg-umskz.
          when 'N' or 'S' or 'C' or 'G' or 'I' or 'O' or 'A'. "per+
            scrnum = '0304'.
          when 'D' or 'W' . "per+
            scrnum = '2320'.
          when others.
            scrnum = '0303'.
        endcase.
      endif.
    elseif zbseg-saknr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-saknr.
      scrnum = '0300'.
    endif.
    perform bdc_field   tables _bdcdata using 'RF05A-NEWBS'
                                              newbs.
    perform bdc_field   tables _bdcdata using 'RF05A-NEWKO'
                                              newko.
*    perform bdc_field   tables _bdcdata using 'RF05A-NEWUM'
*                                              newum.
    if tabix = 1.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '/00'.
    else.
      if xnegp = ' '.
        perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '/00'.
      else.
        perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=ZK'.
      endif.
    endif.

    if pnl <> ' '.
      perform bdc_dynpro  tables _bdcdata using 'SAPLKACB'    '0002'.
      perform bdc_field   tables _bdcdata using 'COBL-GSBER'
                                                gsber.
      if pnl = 'P'.
        perform bdc_field   tables _bdcdata using 'BDC_CURSOR'
                                                  'COBL-AUFNR'.
        perform bdc_field   tables _bdcdata using 'COBL-PS_POSID'
                                                  posid.
        perform bdc_field   tables _bdcdata using 'COBL-KOSTL'
                                                  kostl.
        perform bdc_field   tables _bdcdata using 'COBL-AUFNR'
                                                  aufnr.
        if pernr is not initial.
          perform bdc_field   tables _bdcdata using 'COBL-PERNR'
                                                    pernr.
        endif.

        if rmvct is not initial.
          perform bdc_field   tables _bdcdata using 'COBL-RMVCT'
                                                    rmvct.
        endif.
      elseif pnl = 'B'.
        if posid is not initial.
          read table field_status with key tabnm_feldn = 'BSEG-PROJK'.
          if sy-subrc = 0.
            perform bdc_field   tables _bdcdata using 'COBL-PS_POSID'
                                                      posid.
          endif.
        endif.
        if kostl is not initial.
          read table field_status with key tabnm_feldn = 'BSEG-KOSTL'.
          if sy-subrc = 0.
            perform bdc_field   tables _bdcdata using 'COBL-KOSTL'
                                                      kostl.
          endif.
        endif.
        if aufnr is not initial.
          read table field_status with key tabnm_feldn = 'BSEG-AUFNR'.
          if sy-subrc = 0.
            perform bdc_field   tables _bdcdata using 'COBL-AUFNR'
                                                      aufnr.
          endif.
        endif.
      endif.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=ENTE'.
    endif.

    if xnegp = 'X'.
      if pnl = ' '.
        perform bdc_dynpro  tables _bdcdata using 'SAPMF05A' '0332'.
      else.
        perform bdc_dynpro  tables _bdcdata using 'SAPMF05A' '0330'.
      endif.
      perform bdc_field   tables _bdcdata using 'BSEG-XNEGP'
                                                xnegp.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE' '=AB'.
    endif.


*-- 1η αναλυτική γραμμή
    perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'   scrnum.
    clear dmbtr.
    write zbseg-dmbtr to dmbtr.
    clear wrbtr.
    write zbseg-wrbtr to wrbtr.
    perform bdc_field   tables _bdcdata using 'BSEG-DMBTR' dmbtr.
    perform bdc_field   tables _bdcdata using 'BSEG-WRBTR' wrbtr.
    if zbseg-zterm ne ' '.
     perform bdc_field   tables _bdcdata using 'BSEG-ZTERM' zbseg-zterm.
    endif.
    if zbseg-mwskz <> ' '.
      perform bdc_field   tables _bdcdata using 'BSEG-MWSKZ'
                                                zbseg-mwskz.
    endif.
    if zbkpf-xmwst = 'X'" and ( newbs = '31' or newbs = '21' )
      and tabix = 1.
      perform bdc_field   tables _bdcdata using 'BKPF-XMWST'
                                                zbkpf-xmwst.
    endif.

*    if zbkpf-xmwst <> ' ' and tabix = 1.
*      perform bdc_field   tables _bdcdata using 'BKPF-XMWST'
*                                                zbkpf-xmwst.
*    endif.
    if scrnum = '0301' or
       scrnum = '0302' or
       scrnum = '0303' or
       scrnum = '0304'.

      if zbseg-wmwst <> 0.
        taxflag = 'X'.
        clear dmbtr.
        write zbseg-wmwst to dmbtr.
        perform bdc_field   tables _bdcdata using 'BSEG-WMWST'
                                                  dmbtr.
      endif.
      perform bdc_field   tables _bdcdata using 'BSEG-GSBER'
                                                zbseg-gsber.
    endif.
    if scrnum = '0302' or
       scrnum = '0303' or
       scrnum = '0304' or
       scrnum = '2320'.
      write zbseg-zfbdt to zfbdt dd/mm/yyyy.
      perform bdc_field   tables _bdcdata using 'BSEG-ZFBDT'
                                                zfbdt.
    endif.

    perform bdc_field   tables _bdcdata using 'BSEG-ZUONR'
                                              zbseg-zuonr.
    perform bdc_field   tables _bdcdata using 'BSEG-SGTXT'
                                              zbseg-sgtxt.
    clear: pnl, gsber, projk, kostl, aufnr, posid.
    if zbseg-saknr <> ' '.
      gsber = zbseg-gsber.
      projk = zbseg-projk.
      if zbseg-projk <> '00000000'.

        select single * from prps
          into wa_prps
          where pspnr = zbseg-projk.
        if sy-subrc = 0.
          posid = wa_prps-posid.
        endif.
      endif.
      kostl = zbseg-kostl.
      aufnr = zbseg-aufnr.
      pernr = zbseg-pernr.
      rmvct = zbseg-bewar.

      select single * from ska1
        into wa_ska1
        where ktopl = wa_t001-ktopl
        and   saknr = newko.
      if sy-subrc = 0.
        if wa_ska1-gvtyp <> ' '.
          pnl = 'P'.
        else.
          pnl = 'B'.
        endif.
      endif.
    else.
      clear pnl.
    endif.
    if zbseg-xnegp = 'X' and pnl <> ' '.
      perform bdc_field   tables _bdcdata using 'DKACB-FMORE'
                                                'X'.
    endif.
    xnegp = zbseg-xnegp.
  endloop.

  if xnegp = ' '.
    perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=BS'.
  else.
    perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=ZK'.
  endif.



  perform bdc_dynpro  tables _bdcdata using 'SAPLKACB'        '0002'.
  perform bdc_field   tables _bdcdata using 'COBL-GSBER'      gsber.
  if pnl = 'P'.
    perform bdc_field   tables _bdcdata using 'COBL-PS_POSID'   posid.
    perform bdc_field   tables _bdcdata using 'COBL-KOSTL'      kostl.
    perform bdc_field   tables _bdcdata using 'COBL-AUFNR'      aufnr.
    if pernr is not initial.
      perform bdc_field   tables _bdcdata using 'COBL-PERNR'      pernr.
    endif.
    if rmvct is not initial.
      perform bdc_field   tables _bdcdata using 'COBL-RMVCT'      rmvct.
    endif.
  elseif pnl = 'B'.
    if posid is not initial.
      read table field_status with key tabnm_feldn = 'BSEG-PROJK'.
      if sy-subrc = 0.
        perform bdc_field   tables _bdcdata using 'COBL-PS_POSID'
                                                  posid.
      endif.
    endif.
    if kostl is not initial.
      read table field_status with key tabnm_feldn = 'BSEG-KOSTL'.
      if sy-subrc = 0.
        perform bdc_field   tables _bdcdata using 'COBL-KOSTL'
                                                  kostl.
      endif.
    endif.
    if aufnr is not initial.
      read table field_status with key tabnm_feldn = 'BSEG-AUFNR'.
      if sy-subrc = 0.
        perform bdc_field   tables _bdcdata using 'COBL-AUFNR'
                                                  aufnr.
      endif.
    endif.
  endif.
  perform bdc_field   tables _bdcdata using 'BDC_OKCODE'      '=ENTE'.


  if xnegp = 'X'.
    perform bdc_dynpro  tables _bdcdata using 'SAPMF05A' '0330'.
    perform bdc_field   tables _bdcdata using 'BSEG-XNEGP'
                                              xnegp.
    perform bdc_field   tables _bdcdata using 'BDC_OKCODE' '=BS'.
  endif.

  data:
    tax(2) type n,
    tax_all(2) type n,
    pos(2) type n,
    w_fwste(016),
    fieldname like dd03l-fieldname.

  if no_balance_check = ' ' .
    if zbkpf-xmwst = ' '.
      if taxflag = ' '.
        describe table taxtab lines tax.
        describe table taxtab_all lines tax_all.
        if tax > 0.

          perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0700'.
          perform bdc_field   tables _bdcdata using 'BDC_OKCODE'
                                                    '=STIB'.
          do tax_all times.
            perform bdc_dynpro tables _bdcdata using 'SAPLTAX1' '0300'.
            perform bdc_field  tables _bdcdata using 'BDC_CURSOR'
                                                     'BSET-FWSTE(01)'.
            perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                                     '=DELZ'.
          enddo.

          perform bdc_dynpro tables _bdcdata using 'SAPLTAX1' '0300'.
          perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                                   '=DETL'.

          perform bdc_dynpro tables _bdcdata using 'SAPLTAX1' '0450'.
          clear pos.
          loop at taxtab.
            add 1 to pos.

            write: taxtab-fwste to w_fwste.

            concatenate 'BSET-FWSTE(' pos ')' into fieldname.
            perform bdc_field  tables _bdcdata using fieldname
                                                     w_fwste.
            concatenate 'BSET-MWSKZ(' pos ')' into fieldname.
            perform bdc_field  tables _bdcdata using fieldname
                                                     taxtab-mwskz.
            concatenate 'BSEG-BSCHL(' pos ')' into fieldname.
            perform bdc_field  tables _bdcdata using fieldname
                                                     taxtab-bschl.
          endloop.
          perform bdc_field tables _bdcdata using 'BDC_OKCODE'
                                                  '=GO'.


          perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0700'.
          perform bdc_field  tables _bdcdata using 'BDC_OKCODE' '=BS'.
        endif.
      endif.
    endif.
*    break mourelatosjr.

    loop at taxtab.
      perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '0700'.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=NK'.
**-- γραμμή φόρου

      select single * from ska1
        into wa_ska1
        where ktopl = wa_t001-ktopl
        and   saknr = taxtab-saknr.
      if sy-subrc = 0.
        if wa_ska1-gvtyp <> ' '.
          pnl = 'P'.
        else.
          pnl = 'B'.
        endif.
      endif.

      if taxtab-mwskzz = '<' or taxtab-mwskzz = '>'.
        perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '0312'.
      else.
        perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '0300'.
      endif.

      perform bdc_field   tables _bdcdata using 'BSEG-ZUONR'
                                                zbkpf-bktxt.
      perform bdc_field   tables _bdcdata using 'BSEG-SGTXT'
                                                zbseg-sgtxt.
*    perform bdc_field   tables _bdcdata using 'DKACB-FMORE'
*                                              'X'.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'
                                                '=AB'.


      if taxtab-mwskzz <> '<' and taxtab-mwskzz <> '>'.
        perform bdc_dynpro  tables _bdcdata using 'SAPLKACB'    '0002'.
        perform bdc_field   tables _bdcdata using 'COBL-GSBER'
                                                  gsber.
        if pnl = 'P'.

          if kost_num > 1.
            kostl = ' '.
            aufnr = ' '.
            pernr = ' '.
            posid = ' '.
          endif.

          perform bdc_field   tables _bdcdata using 'COBL-PS_POSID'
                                                    posid.
          perform bdc_field   tables _bdcdata using 'COBL-KOSTL'
                                                    kostl.
          perform bdc_field   tables _bdcdata using 'COBL-AUFNR'
                                                    aufnr.
          if pernr is not initial.
            perform bdc_field   tables _bdcdata using 'COBL-PERNR'
                                                      pernr.
          endif.
          if rmvct is not initial.
            perform bdc_field   tables _bdcdata using 'COBL-RMVCT'
                                                      rmvct.
          endif.
        endif.
        perform bdc_field   tables _bdcdata using 'BDC_OKCODE'
                                                  '=ENTE'.
      endif.
    endloop.

    perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '0700'.
    if simulate = 'X'.
      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=LA'.

      if post = 'X'.
        perform bdc_dynpro  tables _bdcdata using 'SAPMF05A'    '700'.
        perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '/n'.
      endif.
    else.
      if post = 'X'.
        perform bdc_field   tables _bdcdata using 'BDC_OKCODE'  '=BU'.
      endif.
    endif.
  endif.

*  break mourelatosjr.

  call transaction trans
       using _bdcdata
       messages into messtab
       mode mode
       update 'S'.


  if belnr = ' '.
    read table messtab with key msgid = 'F5'
                                msgnr = 312.
    if sy-subrc = 0.
      belnr = messtab-msgv1.
      gjahr = zbkpf-budat(4).
    else.
*      get parameter id 'BLN' field belnr.
*      get parameter id 'GJR' field gjahr.
    endif.
  endif.

endfunction.
*&---------------------------------------------------------------------*
*&      Form  CHECK_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*form check_lifnr using p_zbseg structure bseg
*                       p_zbkpf structure bkpf
*                       p_t001  structure t001.
*
*  if p_zbseg-lifnr is initial.
*    message id '00' type 'E' number 398
*    with 'Για το κλειδί καταχώρησης'
*          p_zbseg-bschl
*         'πρέπει να είναι συμπληρωμένος'
*         'και ο Κωδικός Προμηθευτή.'
*    raising wrong_account.
*  endif.
*  if p_zbseg-lifnr is not initial and
*     ( p_zbseg-kunnr is not initial or
*       p_zbseg-saknr is not initial ).
*    message id '00' type 'E' number 398
*    with  'Πρέπει να είναι συμπληρωμένος μόνο'
*          '"Κωδικός Πιστωτή"'
*    raising wrong_account.
*  endif.
*  call function 'CONVERSION_EXIT_ALPHA_INPUT'
*    exporting
*      input  = p_zbseg-lifnr
*    importing
*      output = p_zbseg-lifnr.
*
*
*  clear lfa1.
*  select single * from lfa1
*    where lifnr = p_zbseg-lifnr.
*  if sy-subrc <> 0.
*    message id '00' type 'E' number 398
*    with 'Ο Πιστωτής' p_zbseg-lifnr
*         'δεν υπάρχει'
*    raising account_not_in_sap.
*  else.
*    select single * from lfb1
*      where lifnr = p_zbseg-lifnr
*      and   bukrs = p_zbkpf-bukrs.
*    if sy-subrc <> 0.
*      message id '00' type 'E' number 398
*      with 'Ο Πιστωτής' p_zbseg-lifnr
*           'δεν υπάρχει στην εταιρεία' p_zbkpf-bukrs
*      raising account_not_in_bukrs.
*    else.
*      if p_zbseg-umskz <> ' '.
*        clear t074.
*        select single * from t074
*          where ktopl = p_t001-ktopl
*          and   koart = 'K'
*          and   umskz = p_zbseg-umskz
*          and   hkont = lfb1-akont.
*        if sy-subrc <> 0.
*          message id '00' type 'E' number 398
*          with 'Το είδικό κλείδι' p_zbseg-umskz
*               'είναι λάθος'
*          raising special_gl_error.
*        else.
*          p_zbseg-hkont = t074-skont.
*        endif.
*
*      else.
*        p_zbseg-hkont = lfb1-akont.
*      endif.
*    endif.
*  endif.
*
*endform.                    "CHECK_LIFNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*form check_kunnr using p_zbseg structure bseg
*                       p_zbkpf structure bkpf
*                       p_t001  structure t001.
*
*  if p_zbseg-kunnr is initial.
*    message id '00' type 'E' number 398
*    with 'Για το κλειδί καταχώρησης'
*          p_zbseg-bschl
*         'πρέπει να είναι συμπηρωμένος'
*         'και ο Κωδικός Πελάτη.'
*    raising wrong_account.
*  endif.
*  if p_zbseg-kunnr is not initial and
*     ( p_zbseg-lifnr is not initial or
*       p_zbseg-saknr is not initial ).
*    message id '00' type 'E' number 398
*    with  'Πρέπει να είναι συμπληρωμένος μόνο'
*          '"Κωδικός Χρεώστη"'
*    raising wrong_account.
*  endif.
*  call function 'CONVERSION_EXIT_ALPHA_INPUT'
*    exporting
*      input  = p_zbseg-kunnr
*    importing
*      output = p_zbseg-kunnr.
*
*  clear kna1.
*  select single * from kna1
*    where kunnr = p_zbseg-kunnr.
*  if sy-subrc <> 0.
*    message id '00' type 'E' number 398
*    with 'Ο Πελάτης' p_zbseg-kunnr
*         'δεν υπάρχει'
*    raising account_not_in_sap.
*  else.
*    select single * from knb1
*      where kunnr = p_zbseg-kunnr
*      and   bukrs = p_zbkpf-bukrs.
*    if sy-subrc <> 0.
*      message id '00' type 'E' number 398
*      with 'Ο Πελάτης' p_zbseg-kunnr
*           'δεν υπάρχει στην εταιρεία' p_zbkpf-bukrs
*      raising account_not_in_bukrs.
*    else.
*      if p_zbseg-umskz <> ' '.
*        clear t074.
*        select single * from t074
*          where ktopl = p_t001-ktopl
*          and   koart = 'D'
*          and   umskz = p_zbseg-umskz
*          and   hkont = knb1-akont.
*        if sy-subrc <> 0.
*          message id '00' type 'E' number 398
*          with 'Το είδικό κλείδι' p_zbseg-umskz
*               'είναι λάθος'
*          raising special_gl_error.
*        else.
*          p_zbseg-hkont = t074-skont.
*        endif.
*      else.
*        p_zbseg-hkont = knb1-akont.
*      endif.
*    endif.
*  endif.
*
*endform.                    "CHECK_KUNNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_SAKNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*form check_saknr using p_saknr like bseg-saknr
*                       p_hkont like bseg-hkont
*                       p_lifnr like bseg-lifnr
*                       p_kunnr like bseg-kunnr
*                       p_bukrs like bkpf-bukrs
*                       p_ktopl like t001-ktopl.
*
*  if p_saknr is initial.
*    message id '00' type 'E' number 398
*    with 'Ο λογαριαμός δεν μπορει να είναι κενός'
*    raising wrong_account.
*  endif.
*  if p_saknr is not initial and
*     ( p_lifnr is not initial or
*       p_kunnr is not initial ).
*    message id '00' type 'E' number 398
*    with 'Λαθος στους λογαριασμούς'
*    raising wrong_account.
*  endif.
*  call function 'CONVERSION_EXIT_ALPHA_INPUT'
*    exporting
*      input  = p_saknr
*    importing
*      output = p_saknr.
*
*  clear ska1.
*  select single * from ska1
*    where saknr = p_saknr
*    and   ktopl = p_ktopl.
*  if sy-subrc <> 0.
*    message id '00' type 'E' number 398
*    with 'Ο λογαριασμός' p_saknr
*         'δεν υπάρχει στο Λογ.Σχέδιο' p_ktopl
*    raising account_not_in_sap.
*  else.
*    clear skb1.
*    select single * from skb1
*      where saknr = p_saknr
*      and   bukrs = p_bukrs.
*    if sy-subrc <> 0.
*      message id '00' type 'E' number 398
*      with 'Ο λογαριασμός' p_saknr
*           'δεν υπάρχει στο Λογ.Σχέδιο' p_bukrs
*      raising account_not_in_bukrs.
*    endif.
*  endif.
*
*  p_hkont = p_saknr.
*
*endform.                    "CHECK_SAKNR
