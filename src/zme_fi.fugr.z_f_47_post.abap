FUNCTION z_f_47_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SIMULATE) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'A'
*"     REFERENCE(POST) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(TRANS) TYPE  TCODE DEFAULT 'F-47'
*"     REFERENCE(I_BKPF) TYPE  BKPF
*"     REFERENCE(I_BSEG) TYPE  BSEG OPTIONAL
*"  EXPORTING
*"     REFERENCE(BELNR) TYPE  BKPF-BELNR
*"     REFERENCE(GJAHR) TYPE  BKPF-GJAHR
*"  TABLES
*"      ZBSEG STRUCTURE  BSEG OPTIONAL
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
  DATA:
    zfbdt(10),
    dmbtr(13),
    newko     LIKE rf05a-newko,
    scrnum(4).

  SET PARAMETER ID 'BLN' FIELD ' '.
  SET PARAMETER ID 'GJR' FIELD ' '.

  CLEAR: belnr,
         gjahr.

  FREE: _bdcdata.
  CLEAR: _bdcdata.
  CLEAR: messtab, messtab[].

  SELECT SINGLE * FROM t001
    INTO wa_t001
    WHERE bukrs = i_bkpf-bukrs.

  WRITE i_bkpf-budat TO budat DD/MM/YYYY.
  WRITE i_bkpf-bldat TO bldat DD/MM/YYYY.

  newko = i_bseg-lifnr.
  scrnum = '0304'.

  PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0112'.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BLDAT'  bldat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BUDAT'  budat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BLART'  i_bkpf-blart.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BUKRS'  i_bkpf-bukrs.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-MONAT'  i_bkpf-monat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-WAERS'  i_bkpf-waers.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-XBLNR'  i_bkpf-xblnr.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BKTXT'  i_bkpf-bktxt.
  PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-NEWKO' newko.
  PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-ZUMSK' 'A'.
  PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.

  DESCRIBE TABLE zbseg LINES DATA(bseglines).

  LOOP AT zbseg INTO DATA(ls_bseg).
    DATA(lv_index) = sy-tabix.
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    scrnum.
    CLEAR dmbtr.
    WRITE ls_bseg-dmbtr TO dmbtr.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-WRBTR'  dmbtr.
    IF ls_bseg-zterm IS NOT INITIAL.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZTERM'  ls_bseg-zterm.
    ENDIF.
    WRITE ls_bseg-zfbdt TO zfbdt DD/MM/YYYY.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZFBDT'  zfbdt.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-EBELN'  ls_bseg-ebeln.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-EBELP'  ls_bseg-ebelp.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZEKKN'  ls_bseg-zekkn.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZUONR'  ls_bseg-zuonr.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-SGTXT'  ls_bseg-sgtxt.

    IF lv_index < bseglines.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=NP'.
    ENDIF.

  ENDLOOP.

*  PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    scrnum.
  PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=BS'.
*  SET PARAMETER ID 'BATCH' FIELD 'X'.

  CALL TRANSACTION trans
       USING _bdcdata
       MESSAGES INTO messtab
       MODE mode
       UPDATE 'S'.

*  SET PARAMETER ID 'BATCH' FIELD ' '.

  IF belnr = ' '.
    READ TABLE messtab WITH KEY msgid = 'F5'
                                msgnr = 312.
    IF sy-subrc = 0.
      belnr = messtab-msgv1.
      gjahr = i_bkpf-budat(4).
    ELSE.
      GET PARAMETER ID 'BLN' FIELD belnr.
      GET PARAMETER ID 'GJR' FIELD gjahr.
    ENDIF.
  ENDIF.

ENDFUNCTION.
