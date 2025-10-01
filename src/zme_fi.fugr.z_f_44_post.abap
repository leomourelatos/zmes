function z_f_44_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LIFNR DEFAULT '4009620'
*"     REFERENCE(I_BUDAT) TYPE  DATUM DEFAULT SY-DATUM
*"     REFERENCE(I_MONAT) TYPE  MONAT DEFAULT SY-DATUM+4(2)
*"     REFERENCE(I_BUKRS) TYPE  BUKRS DEFAULT '0021'
*"     REFERENCE(I_WAERS) TYPE  WAERS DEFAULT 'EUR'
*"     REFERENCE(I_AGUMS) TYPE  AGUMS DEFAULT ' '
*"     REFERENCE(I_OPEN_ITEMS) TYPE  XNOPS DEFAULT 'X'
*"     REFERENCE(I_XPOS1_01) LIKE  BDCDATA-FVAL DEFAULT ' '
*"     REFERENCE(I_XPOS1_03) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     REFERENCE(I_MODE) TYPE  CHAR1 DEFAULT 'A'
*"     REFERENCE(I_TRANS) TYPE  TCODE DEFAULT 'F-44'
*"     REFERENCE(I_SIMULATE) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_POST) TYPE  CHAR1 DEFAULT ' '
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      BELNR_TAB
*"----------------------------------------------------------------------
*subrc = 0.
*
*perform bdc_nodata      using NODATA.
*
*perform open_group      using GROUP USER KEEP HOLDDATE CTU.
*
  data:
    begin of belnrtab occurs 0,
      belnr type belnr_d,
    end of belnrtab.

  free: _bdcdata.
  clear: _bdcdata.
  clear: messtab, messtab[].

  loop at belnr_tab.
    move-corresponding belnr_tab to belnrtab.
    collect belnrtab.
  endloop.

  write i_budat to budat.
  perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0131'.
*  perform bdc_field  tables _bdcdata using 'BDC_CURSOR'
*                                           'RF05A-XPOS1(03)'.
  perform bdc_field  tables _bdcdata using 'RF05A-AGKON'
                                           i_lifnr.
  perform bdc_field  tables _bdcdata using 'BKPF-BUDAT'
                                           budat.
  perform bdc_field  tables _bdcdata using 'BKPF-MONAT'
                                           i_monat.
  perform bdc_field  tables _bdcdata using 'BKPF-BUKRS'
                                           i_bukrs.
  perform bdc_field  tables _bdcdata using 'BKPF-WAERS'
                                           i_waers.
  perform bdc_field  tables _bdcdata using 'RF05A-AGUMS'
                                           i_agums.
  perform bdc_field  tables _bdcdata using 'RF05A-XNOPS'
                                            i_open_items.
  perform bdc_field  tables _bdcdata using 'RF05A-XPOS1(01)'
                                           i_xpos1_01.
  perform bdc_field  tables _bdcdata using 'RF05A-XPOS1(03)'
                                           i_xpos1_03.
  perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                           '/00'.

  loop at belnrtab.
    perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0731'.
*    perform bdc_field  tables _bdcdata using 'BDC_CURSOR'
*                                             'RF05A-SEL01(01)'.
    perform bdc_field  tables _bdcdata using 'RF05A-SEL01(01)'
                                             belnrtab-belnr.
    perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                             '/00'.
  endloop.

  perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0731'.
  perform bdc_field  tables _bdcdata using 'BDC_CURSOR'
                                           'RF05A-SEL01(01)'.
  perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                           '=PA'.

  if i_simulate = 'X'.
    perform bdc_dynpro tables _bdcdata using 'SAPDF05X' '3100'.
    perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                             '=BS'.
  endif.

  perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0700'.
  perform bdc_field  tables _bdcdata using 'BKPF-BKTXT'
                                           '000'.




*perform bdc_dynpro      using 'SAPMF05A' '0731'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RF05A-SEL01(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
*perform bdc_field       using 'RF05A-SEL01(01)'
*                              SEL01_01_010.
*perform bdc_dynpro      using 'SAPMF05A' '0731'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RF05A-SEL01(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
*perform bdc_field       using 'RF05A-SEL01(01)'
*                              SEL01_01_011.
*perform bdc_dynpro      using 'SAPDF05X' '3100'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=BS'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'DF05B-PSSKT(01)'.
*perform bdc_field       using 'RF05A-ABPOS'
*                              ABPOS_012.
  if i_post = 'X'.
*    perform bdc_dynpro tables _bdcdata using 'SAPMF05A' '0700'.
*    perform bdc_field  tables _bdcdata using 'BDC_CURSOR'
*                                             'RF05A-NEWBS'.
    perform bdc_field  tables _bdcdata using 'BDC_OKCODE'
                                             '=BU'.
  endif.
  call transaction i_trans
       using _bdcdata
       messages into messtab
       mode i_mode
       update 'S'.
*if sy-subrc <> 0.
*  subrc = sy-subrc.
*  exit.
*endif.
*
*perform close_group using     CTU.

endfunction.
