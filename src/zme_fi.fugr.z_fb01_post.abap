FUNCTION z_fb01_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SIMULATE) TYPE  CHAR1
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'A'
*"     REFERENCE(POST) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(TRANS) TYPE  TCODE DEFAULT 'FB01'
*"     REFERENCE(NO_BALANCE_CHECK) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(VAT_ANALYSIS) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_BKPF) TYPE  BKPF
*"     REFERENCE(I_CLEAR) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_CHECK) TYPE  CHAR1 DEFAULT ' '
*"     REFERENCE(I_XNOPS) TYPE  XNOPS DEFAULT ' '
*"     REFERENCE(I_AGUMS) TYPE  AGUMS DEFAULT ' '
*"     REFERENCE(I_AGKOA) TYPE  KOART DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(BELNR) TYPE  BKPF-BELNR
*"     REFERENCE(GJAHR) TYPE  BKPF-GJAHR
*"  TABLES
*"      ZBSEG STRUCTURE  BSEG
*"      ZBSED STRUCTURE  BSED OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL
*"      CBELNR TYPE  ZME_CLEAR_BELNR_TBL OPTIONAL
*"      CPYORD TYPE  ZME_PAYMENT_ORDER_TBL OPTIONAL
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

*    budat(10),
*    bldat(10),
    zfbdt(10),
    dmbtr(13),
    newbs     LIKE rf05a-newbs,
    newko     LIKE rf05a-newko,
    newum     LIKE rf05a-newum,
    pnl,
    xnegp,
    tabix     LIKE sy-tabix,
    gsber     LIKE zbseg-gsber,
    projk     LIKE zbseg-projk,
    posid     LIKE prps-posid,
    kostl     LIKE zbseg-kostl,
    aufnr     LIKE zbseg-aufnr,
    pernr     LIKE zbseg-pernr,
*    zzkodikos like zbseg-zzkodikos,
    rmvct     LIKE cobl-rmvct,
    zlsch     LIKE zbseg-zlsch,
    kidno     LIKE zbseg-kidno,
    bvtyp     LIKE zbseg-bvtyp,
    scrnum(4),
    BEGIN OF taxtab OCCURS 0,
      saknr  LIKE bseg-saknr,
      fwste  LIKE bset-fwste,
      bschl  LIKE bseg-bschl,
      mwskz  LIKE bseg-mwskz,
      mwskzz LIKE bseg-mwskz,
    END   OF taxtab,
    BEGIN OF taxtab_all OCCURS 0,
      saknr  LIKE bseg-saknr,
      fwste  LIKE bset-fwste,
      bschl  LIKE bseg-bschl,
      mwskz  LIKE bseg-mwskz,
      mwskzz LIKE bseg-mwskz,
    END   OF taxtab_all,
    BEGIN OF kosttab OCCURS 0,
*      bschl like bseg-bschl,
      gsber LIKE bseg-gsber,
*      projk like bseg-projk,
*      kostl like bseg-kostl,
*      aufnr like bseg-aufnr,
*      pernr like bseg-pernr,
*      bewar like bseg-bewar,
      mwsts LIKE bseg-wmwst,
      mwskz LIKE bseg-mwskz,
      sgtxt LIKE bseg-sgtxt,
    END   OF kosttab,
    mwsts        LIKE bseg-mwsts,
    buzei        LIKE bseg-buzei,
    wa_bseg      TYPE bseg,
    p_bseg       TYPE bseg OCCURS 0 WITH HEADER LINE,
    sgtxt        TYPE bseg-sgtxt,
    kost_num(3)  TYPE n,
    bseg_num(3)  TYPE n,
    dmbtr_check  LIKE zbseg-dmbtr,
    lv_index     LIKE sy-tabix,
    field_status LIKE  zet_field_status_str OCCURS 0 WITH HEADER LINE,
    taxflag,
    xbsed        TYPE bsed.



  FIELD-SYMBOLS:
    <test_field>.
  DATA:
    test_field(100).

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

  SELECT SINGLE * FROM tka02
    INTO wa_tka02
    WHERE bukrs = i_bkpf-bukrs.

  CLEAR: dmbtr_check, kosttab, kosttab[], sgtxt.
  LOOP AT zbseg.
    lv_index = sy-tabix.
    IF zbseg-lifnr IS INITIAL AND
       zbseg-kunnr IS INITIAL AND
       zbseg-saknr IS INITIAL.
      RAISE no_account.
    ENDIF.

    SELECT SINGLE * FROM tbsl
      INTO wa_tbsl
      WHERE bschl = zbseg-bschl.
    IF sy-subrc = 0.
      IF wa_tbsl-koart = 'K'.
        PERFORM check_lifnr USING zbseg i_bkpf wa_t001.
        PERFORM check_saknr USING zbseg-hkont
                                  zbseg-hkont
                                  ' '
                                  ' '
                                  i_bkpf-bukrs
                                  wa_t001-ktopl.
        IF lv_index = 1.
          sgtxt = zbseg-sgtxt.
        ENDIF.
      ELSEIF wa_tbsl-koart = 'D'.
        PERFORM check_kunnr USING zbseg i_bkpf wa_t001.
        PERFORM check_saknr USING zbseg-hkont
                                  zbseg-hkont
                                  ' '
                                  ' '
                                  i_bkpf-bukrs
                                  wa_t001-ktopl.
        IF lv_index = 1.
          sgtxt = zbseg-sgtxt.
        ENDIF.
      ELSEIF wa_tbsl-koart = 'S'.
        PERFORM check_saknr USING zbseg-saknr
                                  zbseg-hkont
                                  zbseg-lifnr
                                  zbseg-kunnr
                                  i_bkpf-bukrs
                                  wa_t001-ktopl.
      ENDIF.
    ELSE.
      MESSAGE ID '00' TYPE 'E' NUMBER 398
      WITH 'Λαθος Κλειδί Καταχώρησης' zbseg-bschl
      RAISING wrong_posting_key.
    ENDIF.


    FREE field_status.
    CLEAR field_status.
    PERFORM find_field_status TABLES field_status
                              USING i_bkpf
                                    zbseg.

    LOOP AT field_status WHERE field_type = 'O'.
      CLEAR test_field.
      CONCATENATE 'Z' field_status-tabnm_feldn INTO test_field.
      ASSIGN test_field TO <test_field>.
      IF <test_field> IS INITIAL.
        MESSAGE ID '00' TYPE 'E' NUMBER 398
        WITH 'Field' <test_field> 'is missing' RAISING fields_missing.
      ENDIF.
    ENDLOOP.

    LOOP AT field_status WHERE field_type = 'S'.
      IF field_status-tabnm(1) = '*'.
        CONTINUE.
      ENDIF.
      CLEAR test_field.
      CONCATENATE 'Z' field_status-tabnm_feldn INTO test_field.
      ASSIGN (test_field) TO <test_field>.
      IF <test_field> IS NOT INITIAL.
        MESSAGE ID '00' TYPE 'E' NUMBER 398
        WITH zbseg-buzei test_field RAISING supressed_field.
      ENDIF.
    ENDLOOP.

    IF wa_tbsl-shkzg = 'S'.
      IF zbseg-bschl = '31' OR zbseg-bschl = '21' OR
         zbseg-bschl = '01' OR zbseg-bschl = '11' OR
         zbseg-bschl = '02' OR zbseg-bschl = '12'.
        dmbtr_check = dmbtr_check + zbseg-dmbtr - zbseg-wmwst.
      ELSE.
        dmbtr_check = dmbtr_check + zbseg-dmbtr + zbseg-wmwst.
      ENDIF.
    ELSEIF wa_tbsl-shkzg = 'H'.
      IF zbseg-bschl = '31' OR zbseg-bschl = '21' OR
         zbseg-bschl = '01' OR zbseg-bschl = '11' OR
         zbseg-bschl = '02' OR zbseg-bschl = '12'.
        dmbtr_check = dmbtr_check - zbseg-dmbtr + zbseg-wmwst.
      ELSE.
        dmbtr_check = dmbtr_check - zbseg-dmbtr - zbseg-wmwst.
      ENDIF.
    ENDIF.

    IF vat_analysis = 'X'.
*      IF zbseg-mwskz = '35' OR "23%
*         zbseg-mwskz = '42' OR
*         zbseg-mwskz = '43' OR
*         zbseg-mwskz = '44' OR
*         zbseg-mwskz = '64' OR
*         zbseg-mwskz = '6M' OR
*         zbseg-mwskz = '6T' OR
*         zbseg-mwskz = '1N' OR
*         zbseg-mwskz = '2A' OR
*         zbseg-mwskz = '8D' OR
*
*         zbseg-mwskz = '33' OR "24%
*         zbseg-mwskz = '34' OR
*         zbseg-mwskz = '45' OR
*         zbseg-mwskz = '46' OR
*         zbseg-mwskz = '4A' OR
*         zbseg-mwskz = '5L' OR
*         zbseg-mwskz = '8I' OR
*         zbseg-mwskz = '5S' OR
*         zbseg-mwskz = '1Y' OR
*         zbseg-mwskz = '8U'.
*
*        CLEAR kosttab.
*        MOVE-CORRESPONDING zbseg TO kosttab.
*        kosttab-sgtxt = sgtxt.
**        clear kosttab-bschl.
*        IF zbseg-bschl = 50.
*          kosttab-mwsts = - kosttab-mwsts.
*        ENDIF.
*        COLLECT kosttab.
*
*        IF zbseg-bschl = 50.
*          mwsts = mwsts - zbseg-mwsts.
*        ELSE.
*          mwsts = mwsts + zbseg-mwsts.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDLOOP.

  IF vat_analysis = 'X'.
    CLEAR: p_bseg, p_bseg[].

    buzei = '001'.
    LOOP AT zbseg INTO wa_bseg.
*    lv_index = sy-tabix.
      wa_bseg-buzei = buzei.

      APPEND wa_bseg TO p_bseg.

*      IF wa_bseg-mwskz = '35' OR "23%
*         wa_bseg-mwskz = '42' OR
*         wa_bseg-mwskz = '43' OR
*         wa_bseg-mwskz = '44' OR
*         wa_bseg-mwskz = '64' OR
*         wa_bseg-mwskz = '6M' OR
*         wa_bseg-mwskz = '6T' OR
*         wa_bseg-mwskz = '1N' OR
*         wa_bseg-mwskz = '2A' OR
*         wa_bseg-mwskz = '8D' OR
*
*         wa_bseg-mwskz = '33' OR "24%
*         wa_bseg-mwskz = '34' OR
*         wa_bseg-mwskz = '45' OR
*         wa_bseg-mwskz = '46' OR
*         wa_bseg-mwskz = '4A' OR
*         wa_bseg-mwskz = '5L' OR
*         wa_bseg-mwskz = '8I' OR
*         wa_bseg-mwskz = '5S' OR
*         wa_bseg-mwskz = '1Y' OR
*         wa_bseg-mwskz = '8U'.
*
*        SELECT SINGLE * FROM t030k
*          INTO wa_t030k
*          WHERE ktopl = wa_t001-ktopl
*          AND   ktosl = 'VST'
*          AND   mwskz = wa_bseg-mwskz.
*        buzei = buzei + 1.
*        wa_bseg-buzei  = buzei.
*        wa_bseg-saknr  = wa_t030k-konth.
*        wa_bseg-dmbtr  = wa_bseg-mwsts.
*        APPEND wa_bseg TO p_bseg.
*      ENDIF.
*    modify zbseg index lv_index.
      buzei = buzei + 1.
    ENDLOOP.

    CLEAR: zbseg, zbseg[].
    zbseg[] = p_bseg[].
    SORT zbseg.
    CLEAR: p_bseg, p_bseg[].


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

    LOOP AT kosttab.
      CLEAR zbseg.
      IF kosttab-mwsts < 0.
        zbseg-bschl  = '40'.
      ELSE.
        zbseg-bschl  = '50'.
      ENDIF.
      zbseg-saknr  = wa_t030k-konth.
      zbseg-dmbtr  = abs( kosttab-mwsts ).
      zbseg-mwskz  = kosttab-mwskz.
      zbseg-sgtxt  = kosttab-sgtxt.
      zbseg-gsber  = kosttab-gsber.
*      zbseg-kostl  = zetcc6398-kostl.
      zbseg-xnegp  = ' '.
      zbseg-buzei  = buzei.
      APPEND zbseg.
      buzei = buzei + 1.
    ENDLOOP.
  ENDIF.

*  break mourelatosjr.
  LOOP AT zbseg
    WHERE mwskz <> '**'
    AND   mwskz <> '  '.

    SELECT SINGLE * FROM a003
      INTO wa_a003
      WHERE mwskz = zbseg-mwskz
*      AND   ( kschl = 'MWVN' )
      AND   aland = wa_t001-land1.

    SELECT SINGLE * FROM konp
      INTO wa_konp
      WHERE knumh =  wa_a003-knumh.

    IF sy-subrc = 0." and konp-kbetr <> 0.
      CLEAR wa_t030k.
      SELECT SINGLE * FROM t030k
        INTO wa_t030k
        WHERE ktopl = wa_t001-ktopl
        AND   mwskz = zbseg-mwskz
        AND   konts <> ' '.
      CLEAR taxtab_all.
      taxtab_all-mwskz = zbseg-mwskz.
      taxtab_all-saknr = wa_t030k-konts.
      IF zbseg-bschl = '50'.
        taxtab_all-fwste = - zbseg-wmwst.
      ELSEIF zbseg-bschl = '40'.
        taxtab_all-fwste = zbseg-wmwst.
      ENDIF.
*      taxtab-bschl = zbseg-bschl.

      SELECT SINGLE * FROM skb1
        INTO wa_skb1
        WHERE bukrs = wa_t001-bukrs
        AND   saknr = wa_t030k-konts.
      IF sy-subrc = 0.
        taxtab_all-mwskzz = wa_skb1-mwskz.
      ENDIF.
      COLLECT taxtab_all.
      IF wa_konp-kbetr <> 0.
        CLEAR taxtab.
        taxtab = taxtab_all.
        COLLECT taxtab.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  delete taxtab where fwste = 0.

  DATA:
    db_index LIKE sy-tabix.

  SORT taxtab BY mwskz.
  LOOP AT taxtab.
    db_index = sy-tabix.
    IF taxtab-fwste < 0.
      taxtab-bschl = '50'.
      taxtab-fwste = - taxtab-fwste.
    ELSEIF taxtab-fwste > 0.
      taxtab-bschl = '40'.
    ENDIF.
    MODIFY taxtab INDEX db_index.
  ENDLOOP.

  IF no_balance_check = ' '.
    IF dmbtr_check <> 0.
      MESSAGE ID '00' TYPE 'E' NUMBER 398
      WITH 'Λαθος χρέωση - πίστωση'
      RAISING credit_vs_debit.
    ENDIF.
  ENDIF.

  CLEAR: _bdcdata, _bdcdata[].

  WRITE i_bkpf-budat TO budat DD/MM/YYYY.
  WRITE i_bkpf-bldat TO bldat DD/MM/YYYY.

  IF i_clear = 'X'.
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0122'.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XPOS1(01)'
                                              i_clear.
  ELSE.
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'       '0100'.
    PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-XREF1_HD'  i_bkpf-xref1_hd.
    PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-XREF2_HD'  i_bkpf-xref2_hd.
  ENDIF.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BLDAT'  bldat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BUDAT'  budat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BLART'  i_bkpf-blart.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BUKRS'  i_bkpf-bukrs.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-MONAT'  i_bkpf-monat.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-WAERS'  i_bkpf-waers.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-XBLNR'  i_bkpf-xblnr.
  PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-BKTXT'  i_bkpf-bktxt.
*  perform bdc_field   tables _bdcdata using 'FS006-DOCID' '*'.
  CLEAR: pnl, gsber, projk, kostl, aufnr, posid, xnegp, taxflag.
  LOOP AT zbseg.
    tabix = sy-tabix.

    FREE field_status.
    CLEAR field_status.
    PERFORM find_field_status TABLES field_status
                              USING i_bkpf
                                    zbseg.

    CLEAR: newbs, newko, newum, scrnum.
    IF zbseg-lifnr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-lifnr.
      newum = zbseg-umskz.
      scrnum = '0302'.
      IF zbseg-umskz <> ' '.
        SELECT SINGLE * FROM t074u
          INTO wa_t074u
          WHERE koart = 'K'
          AND   umskz = zbseg-umskz.
        IF wa_t074u-umsks = 'A'.
          scrnum = '0304'.
        ELSEIF wa_t074u-umsks = 'W'.
          scrnum = '2320'.
        ELSE.
          scrnum = '0303'.
        ENDIF.
      ENDIF.
    ELSEIF zbseg-kunnr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-kunnr.
      newum = zbseg-umskz.
      scrnum = '0301'.
      IF zbseg-umskz <> ' '.
        SELECT SINGLE * FROM t074u
          INTO wa_t074u
          WHERE koart = 'K'
          AND   umskz = zbseg-umskz.
        IF wa_t074u-umsks = 'A'.
          scrnum = '0304'.
        ELSEIF wa_t074u-umsks = 'W'.
          scrnum = '2320'.
        ELSE.
          scrnum = '0303'.
        ENDIF.
      ENDIF.
    ELSEIF zbseg-saknr <> ' '.
      newbs = zbseg-bschl.
      newko = zbseg-saknr.
      scrnum = '0300'.
    ENDIF.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-NEWBS'
                                              newbs.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-NEWKO'
                                              newko.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-NEWUM'
                                              newum.
    IF tabix = 1.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
    ELSE.
      IF xnegp = 'X' OR
*         zlsch <> ' ' or
         kidno <> ' ' OR
         bvtyp <> ' '.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ZK'.
      ELSE.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
      ENDIF.
    ENDIF.

    IF pnl <> ' ' AND pnl <> 'W'.
      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLKACB'    '0002'.
*      perform bdc_field   tables _bdcdata using 'COBL-GSBER'
*                                                gsber.
      IF pnl = 'P'.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_CURSOR'
                                                  'COBL-AUFNR'.
        PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PS_POSID'
                                                  posid.
        PERFORM bdc_field   TABLES _bdcdata USING 'COBL-KOSTL'
                                                  kostl.
        PERFORM bdc_field   TABLES _bdcdata USING 'COBL-AUFNR'
                                                  aufnr.
        IF pernr IS NOT INITIAL.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PERNR'
                                                    pernr.
        ENDIF.
        IF rmvct IS NOT INITIAL.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-RMVCT'
                                                    rmvct.
        ENDIF.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ENTE'.
      ELSEIF pnl = 'B'.
        IF posid IS NOT INITIAL.
          READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-PROJK'.
          IF sy-subrc = 0.
            PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PS_POSID'
                                                      posid.
          ENDIF.
        ENDIF.
        IF kostl IS NOT INITIAL.
          READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-KOSTL'.
          IF sy-subrc = 0.
            PERFORM bdc_field   TABLES _bdcdata USING 'COBL-KOSTL'
                                                      kostl.
          ENDIF.
        ENDIF.
        IF aufnr IS NOT INITIAL.
          READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-AUFNR'.
          IF sy-subrc = 0.
            PERFORM bdc_field   TABLES _bdcdata USING 'COBL-AUFNR'
                                                      aufnr.
          ENDIF.
        ENDIF.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ENTE'.
*      ELSEIF pnl = 'W'.
*        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLFWTD'    '0100'.
*        CLEAR dmbtr.
*        WRITE zbseg-qsshb TO dmbtr.
*        PERFORM bdc_field   TABLES _bdcdata USING 'WITH_DIALG-WT_BASE(01)'  dmbtr.
*        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=GO'.
      ENDIF.
    ELSE.
*      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ENTE'.
*      IF zbseg-bschl = '39' AND zbseg-umskz = 'G'.
*        DATA:
*          wa_lfbw TYPE lfbw.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = zbseg-lifnr
*          IMPORTING
*            output = zbseg-lifnr.
*
*        SELECT SINGLE * FROM lfbw
*          INTO wa_lfbw
*          WHERE lifnr = zbseg-lifnr
*          AND   wt_subjct = 'X'.
*        IF sy-subrc = 0.
*          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLFWTD'    '0100'.
*          PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=GO'.
*          CLEAR dmbtr.
*          WRITE zbseg-qsshb TO dmbtr.
*          PERFORM bdc_field   TABLES _bdcdata USING 'WITH_DIALG-WT_BASE(01)'  dmbtr.
*        ENDIF.
*        endif.
    ENDIF.

    IF xnegp = 'X' OR
*       zlsch <> ' ' or
       kidno <> ' ' OR
       bvtyp <> ' '.
      IF pnl = ' '.
        IF scrnum = '0301'.
          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0331'.
        ELSEIF scrnum = '0302'.
          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0332'.
        ENDIF.
      ELSE.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0330'.
      ENDIF.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-XNEGP'
                                                xnegp.
*      if zlsch <> ' '.
*        perform bdc_field   tables _bdcdata using 'BSEG-ZLSCH'
*                                                  zlsch.
*      endif.
      IF kidno <> ' '.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-KIDNO'
                                                  kidno.
      ENDIF.
      IF bvtyp <> ' '.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-BVTYP'
                                                  bvtyp.
      ENDIF.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE' '=AB'.
    ENDIF.


*-- 1η αναλυτική γραμμή
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'   scrnum.
    CLEAR dmbtr.
    WRITE zbseg-dmbtr TO dmbtr.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-WRBTR' dmbtr.
    IF zbseg-zterm NE ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZTERM' zbseg-zterm.
    ENDIF.
    IF zbseg-mwskz <> ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-MWSKZ'
                                                zbseg-mwskz.
    ENDIF.
    IF i_bkpf-xmwst = 'X'" and ( newbs = '31' or newbs = '21' )
      AND tabix = 1.
      PERFORM bdc_field   TABLES _bdcdata USING 'BKPF-XMWST'
                                                i_bkpf-xmwst.
    ENDIF.

    IF zbseg-zlsch <> ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZLSCH'
                                                zbseg-zlsch.
    ENDIF.

    IF zbseg-kidno <> ' ' AND zbseg-saknr = ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-KIDNO'
                                                zbseg-kidno.
    ENDIF.

*    if zbkpf-xmwst <> ' ' and tabix = 1.
*      perform bdc_field   tables _bdcdata using 'BKPF-XMWST'
*                                                zbkpf-xmwst.
*    endif.
    IF scrnum = '0301' OR
       scrnum = '0302' OR
       scrnum = '0303' OR
       scrnum = '0304'.

      IF zbseg-wmwst <> 0.
        taxflag = 'X'.
        CLEAR dmbtr.
        WRITE zbseg-wmwst TO dmbtr.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-WMWST'
                                                  dmbtr.
      ENDIF.
*      perform bdc_field   tables _bdcdata using 'BSEG-GSBER'
*                                                zbseg-gsber.
    ENDIF.
    IF scrnum = '0302' or scrnum = '0303' OR scrnum = '0304' and zbseg-zfbdt is not initial.
      WRITE zbseg-zfbdt TO zfbdt DD/MM/YYYY.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZFBDT'
                                                zfbdt.
    ENDIF.
    IF scrnum = '2320'.
      READ TABLE zbsed INTO xbsed WITH KEY buzei = zbseg-buzei.
      IF sy-subrc = 0.
        WRITE zbseg-zfbdt TO zfbdt DD/MM/YYYY.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZFBDT'
                                                  zfbdt.
        WRITE xbsed-wdate TO zfbdt DD/MM/YYYY.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSED-WDATE'
                                                  zfbdt.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSED-BOENO'
                                                  xbsed-boeno.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSED-BANK'
                                                  xbsed-bank.
        PERFORM bdc_field   TABLES _bdcdata USING 'BSED-ACCOU'
                                                  xbsed-accou.
      ENDIF.
    ENDIF.


    IF zbseg-umskz = 'A' AND zbseg-ebeln <> ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-EBELN'
                                                zbseg-ebeln.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-EBELP'
                                                zbseg-ebelp.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZEKKN'
                                                zbseg-zekkn.
    ENDIF.

    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZUONR'
                                              zbseg-zuonr.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-SGTXT'
                                              zbseg-sgtxt.
    CLEAR: pnl, gsber, projk, kostl, aufnr, posid, zlsch,
           kidno, bvtyp.
    IF zbseg-saknr <> ' '.
      gsber = zbseg-gsber.
      projk = zbseg-projk.
      IF zbseg-projk <> '00000000'.
        SELECT SINGLE * FROM prps
          INTO wa_prps
          WHERE pspnr = zbseg-projk.
        IF sy-subrc = 0.
          posid = wa_prps-posid.
        ENDIF.
      ENDIF.
      kostl = zbseg-kostl.
      aufnr = zbseg-aufnr.
      pernr = zbseg-pernr.
      rmvct = zbseg-bewar.

      SELECT SINGLE * FROM ska1
        INTO wa_ska1
        WHERE ktopl = wa_t001-ktopl
        AND   saknr = newko.
      IF sy-subrc = 0.
        IF wa_ska1-gvtyp <> ' '.
          pnl = 'P'.
        ELSE.
          pnl = 'B'.
        ENDIF.
      ENDIF.
    ELSE.
*---- Παρακρατούμενος Φόρος
      DATA:
        wa_lfbw TYPE lfbw.
      SELECT SINGLE * FROM tbsl
        INTO wa_tbsl
        WHERE bschl = zbseg-bschl.
      IF wa_tbsl-xzahl = ' ' AND wa_tbsl-xsonu = ' ' AND wa_tbsl-xumsw = 'X'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = zbseg-lifnr
          IMPORTING
            output = zbseg-lifnr.

        SELECT SINGLE * FROM lfbw
          INTO wa_lfbw
          WHERE lifnr = zbseg-lifnr
          AND   wt_subjct = 'X'.
        IF sy-subrc = 0.
          pnl = 'W'.
          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLFWTD'    '0100'.
          PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=GO'.
          CLEAR dmbtr.
          WRITE zbseg-qsshb TO dmbtr.
          PERFORM bdc_field   TABLES _bdcdata USING 'WITH_DIALG-WT_BASE(01)'  dmbtr.
          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'   scrnum.
        ELSE.
          CLEAR pnl.
        ENDIF.

      ENDIF.
    ENDIF.
    IF zbseg-xnegp = 'X' AND pnl <> ' ' AND ( "zbseg-zlsch <> ' ' or
                                              zbseg-kidno <> ' ' "or
*                                              zbseg-bvtyp <> ' '
                                                                  ).
      PERFORM bdc_field   TABLES _bdcdata USING 'DKACB-FMORE'
                                                'X'.
    ENDIF.
    xnegp = zbseg-xnegp.
*    if zbseg-zlsch <> ' '.
*      zlsch = zbseg-zlsch.
*    endif.
    IF zbseg-kidno <> ' ' AND pnl <> ' '.
      kidno = zbseg-kidno.
    ENDIF.
    IF zbseg-bvtyp <> ' '.
      bvtyp = zbseg-bvtyp.
    ENDIF.
  ENDLOOP.

  IF xnegp = 'X' OR
*     zlsch <> ' ' or
     kidno <> ' ' OR
     bvtyp <> ' '.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ZK'.
  ELSE.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=BS'.
*    IF zbseg-zfbdt IS NOT INITIAL.
*    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' scrnum.
*    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
*    ENDIF.
  ENDIF.


  IF pnl <> ' ' AND pnl <> 'W'.
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLKACB'        '0002'.
*    perform bdc_field   tables _bdcdata using 'COBL-GSBER'      gsber.
    IF pnl = 'P'.
      PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PS_POSID'   posid.
      PERFORM bdc_field   TABLES _bdcdata USING 'COBL-KOSTL'      kostl.
      PERFORM bdc_field   TABLES _bdcdata USING 'COBL-AUFNR'      aufnr.
      IF pernr IS NOT INITIAL.
        PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PERNR'      pernr.
      ENDIF.
      IF rmvct IS NOT INITIAL.
        PERFORM bdc_field   TABLES _bdcdata USING 'COBL-RMVCT'      rmvct.
      ENDIF.
    ELSEIF pnl = 'B'.
      IF posid IS NOT INITIAL.
        READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-PROJK'.
        IF sy-subrc = 0.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PS_POSID'
                                                    posid.
        ENDIF.
      ENDIF.
      IF kostl IS NOT INITIAL.
        READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-KOSTL'.
        IF sy-subrc = 0.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-KOSTL'
                                                    kostl.
        ENDIF.
      ENDIF.
      IF aufnr IS NOT INITIAL.
        READ TABLE field_status WITH KEY tabnm_feldn = 'BSEG-AUFNR'.
        IF sy-subrc = 0.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-AUFNR'
                                                    aufnr.
        ENDIF.
      ENDIF.
*    ELSEIF pnl = 'W'.
*      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLFWTD'    '0100'.
*      CLEAR dmbtr.
*      WRITE zbseg-qsshb TO dmbtr.
*      PERFORM bdc_field   TABLES _bdcdata USING 'WITH_DIALG-WT_BASE(01)'  dmbtr.
*      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=GO'.
    ENDIF.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'      '=ENTE'.
  ENDIF.

  IF xnegp = 'X' OR
*     zlsch <> ' ' or
     kidno <> ' ' OR
     bvtyp <> ' '.
    IF pnl = ' '.
      IF scrnum = '0301'.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0331'.
      ELSEIF scrnum = '0302'.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0332'.
      ENDIF.
    ELSE.
      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0330'.
    ENDIF.
    PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-XNEGP'
                                              xnegp.
*    if zlsch <> ' '.
*      perform bdc_field   tables _bdcdata using 'BSEG-ZLSCH'
*                                                zlsch.
*    endif.
    IF kidno <> ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-KIDNO'
                                                kidno.
    ENDIF.
    IF bvtyp <> ' '.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-BVTYP'
                                                bvtyp.
    ENDIF.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE' '=BS'.
  ENDIF.

  DATA:
    tax(2)       TYPE n,
    tax_all(2)   TYPE n,
    pos(2)       TYPE n,
    w_fwste(016),
    fieldname    LIKE dd03l-fieldname.

  IF no_balance_check = ' ' .
    IF i_bkpf-xmwst = ' '.
      IF taxflag = ' '.
        DESCRIBE TABLE taxtab LINES tax.
        DESCRIBE TABLE taxtab_all LINES tax_all.
        IF tax > 0.

          PERFORM bdc_dynpro TABLES _bdcdata USING 'SAPMF05A' '0700'.
          PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'
                                                    '=STIB'.
          tax_all = tax_all + 1.
          DO tax_all TIMES.
            PERFORM bdc_dynpro TABLES _bdcdata USING 'SAPLTAX1' '0300'.
            PERFORM bdc_field  TABLES _bdcdata USING 'BDC_CURSOR'
                                                     'BSET-FWSTE(01)'.
            PERFORM bdc_field  TABLES _bdcdata USING 'BDC_OKCODE'
                                                     '=DELZ'.
          ENDDO.

          PERFORM bdc_dynpro TABLES _bdcdata USING 'SAPLTAX1' '0300'.
          PERFORM bdc_field  TABLES _bdcdata USING 'BDC_OKCODE'
                                                   '=DETL'.

          PERFORM bdc_dynpro TABLES _bdcdata USING 'SAPLTAX1' '0450'.
          CLEAR pos.
          LOOP AT taxtab.
            ADD 1 TO pos.

            WRITE: taxtab-fwste TO w_fwste.

            CONCATENATE 'BSET-FWSTE(' pos ')' INTO fieldname.
            PERFORM bdc_field  TABLES _bdcdata USING fieldname
                                                     w_fwste.
            CONCATENATE 'BSET-MWSKZ(' pos ')' INTO fieldname.
            PERFORM bdc_field  TABLES _bdcdata USING fieldname
                                                     taxtab-mwskz.
            CONCATENATE 'BSEG-BSCHL(' pos ')' INTO fieldname.
            PERFORM bdc_field  TABLES _bdcdata USING fieldname
                                                     taxtab-bschl.
          ENDLOOP.
          PERFORM bdc_field TABLES _bdcdata USING 'BDC_OKCODE'
                                                  '=GO'.


          PERFORM bdc_dynpro TABLES _bdcdata USING 'SAPMF05A' '0700'.
          PERFORM bdc_field  TABLES _bdcdata USING 'BDC_OKCODE' '=BS'.
        ENDIF.
      ENDIF.
    ENDIF.
*    BREAK lmourelatos.

    LOOP AT taxtab WHERE saknr(1) = '6' AND fwste > 0.
      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0700'.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=NK'.
**-- γραμμή φόρου

      CLEAR wa_ska1.
      SELECT SINGLE * FROM ska1
        INTO wa_ska1
        WHERE ktopl = wa_t001-ktopl
        AND   saknr = taxtab-saknr.
      IF sy-subrc = 0.
        IF wa_ska1-gvtyp <> ' '.
          pnl = 'P'.
        ELSE.
          pnl = 'B'.
        ENDIF.
      ENDIF.

      IF taxtab-mwskzz = '<' OR taxtab-mwskzz = '>'.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0312'.
      ELSE.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0300'.
      ENDIF.

      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-ZUONR'
                                                i_bkpf-bktxt.
      PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-SGTXT'
                                                zbseg-sgtxt.
*    perform bdc_field   tables _bdcdata using 'DKACB-FMORE'
*                                              'X'.
*      perform bdc_field   tables _bdcdata using 'BDC_OKCODE'
*                                                '=AB'.
      IF xnegp = ' '.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=AB'.
      ELSE.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ZK'.
      ENDIF.

      IF taxtab-mwskzz <> '<' AND taxtab-mwskzz <> '>'.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPLKACB'    '0002'.
*        perform bdc_field   tables _bdcdata using 'COBL-GSBER'
*                                                  gsber.
        IF pnl = 'P'.

          IF kost_num > 1.
            kostl = ' '.
            aufnr = ' '.
            pernr = ' '.
            posid = ' '.
          ENDIF.

          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PS_POSID'
                                                    posid.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-KOSTL'
                                                    kostl.
          PERFORM bdc_field   TABLES _bdcdata USING 'COBL-AUFNR'
                                                    aufnr.
          IF pernr IS NOT INITIAL.
            PERFORM bdc_field   TABLES _bdcdata USING 'COBL-PERNR'
                                                      pernr.
          ENDIF.
          IF rmvct IS NOT INITIAL.
            PERFORM bdc_field   TABLES _bdcdata USING 'COBL-RMVCT'
                                                      rmvct.
          ENDIF.
        ENDIF.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'
                                                  '=ENTE'.


        IF xnegp = 'X'.

          PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' '0330'.
          PERFORM bdc_field   TABLES _bdcdata USING 'BSEG-XNEGP'
                                                    xnegp.
          PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE' '=AB'.

        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDIF.


  IF i_clear IS NOT INITIAL.

*    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A' scrnum.
*    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=SL'.
*
    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0710'.
*                                                 BDC_CURSOR  RF05A-XPOS1(03)
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-AGBUK' i_bkpf-bukrs.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-AGKOA' i_agkoa.
*    IF i_check = 'X'.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XNOPS' i_xnops.
    PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-AGUMS' i_agums.
*    ELSEIF i_check = ' '.
*      PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XNOPS' 'X'.
*      PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-AGUMS' ' '.
*    ENDIF.
    IF cbelnr[] IS NOT INITIAL.
      PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XPOS1(03)'  'X'.
    ELSEIF cpyord[] IS NOT INITIAL.
      PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XPOS1(07)'  'X'.
    ENDIF.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.

    IF cbelnr[] IS NOT INITIAL.
      LOOP AT cbelnr.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0731'.
        PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-SEL01(01)'  cbelnr-belnr.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
      ENDLOOP.
    ELSEIF cpyord[] IS NOT INITIAL.
      LOOP AT cpyord.
        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0731'.
        PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-SEL01(01)'  cpyord-pyord.
        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
      ENDLOOP.
    ENDIF.

*    IF cbelnr[] IS NOT INITIAL.
*
*      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0731'.
*      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=SLK'.
*
*      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0710'.
*      PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-XPOS1(04)'  'X'.
*      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=ENTR'.
*
*      LOOP AT cbelnr.
*        PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0731'.
*        WRITE cbelnr-budat TO budat.
*        PERFORM bdc_field   TABLES _bdcdata USING 'RF05A-SEL01(01)'  budat.
*        PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '/00'.
*      ENDLOOP.
*
*    ENDIF.

    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'  '0731'.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=PA'.

    IF cpyord[] IS INITIAL.
      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPDF05X'  '3100'.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=OMX'.
    ENDIF.

    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPDF05X'  '3100'.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=Z+'.


    PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPDF05X'  '3100'.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=BS'.

  ENDIF.

  PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0700'.
  IF simulate = 'X'.
    PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=LA'.

    IF post = 'X'.
      PERFORM bdc_dynpro  TABLES _bdcdata USING 'SAPMF05A'    '0700'.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=BS'.
    ENDIF.
  ELSE.
    IF post = 'X'.
      PERFORM bdc_field   TABLES _bdcdata USING 'BDC_OKCODE'  '=BU'.
    ENDIF.
  ENDIF.

*  break mourelatosjr.

  SET PARAMETER ID 'BATCH' FIELD 'X'.

  CALL TRANSACTION trans
       USING _bdcdata
       MESSAGES INTO messtab
       MODE mode
       UPDATE 'S'.

  SET PARAMETER ID 'BATCH' FIELD ' '.

  IF belnr = ' '.
    READ TABLE messtab WITH KEY msgid = 'F5'
                                msgnr = 312.
    IF sy-subrc = 0.
      belnr = messtab-msgv1.
      gjahr = i_bkpf-budat(4).
    ELSE.
*      get parameter id 'BLN' field belnr.
*      get parameter id 'GJR' field gjahr.
    ENDIF.
  ENDIF.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  CHECK_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_lifnr USING p_zbseg STRUCTURE bseg
                       p_zbkpf STRUCTURE bkpf
                       p_t001  STRUCTURE t001.

  IF p_zbseg-lifnr IS INITIAL.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Για το κλειδί καταχώρησης'
          p_zbseg-bschl
         'πρέπει να είναι συμπληρωμένος'
         'και ο Κωδικός Προμηθευτή.'
    RAISING wrong_account.
  ENDIF.
  IF p_zbseg-lifnr IS NOT INITIAL AND
     ( p_zbseg-kunnr IS NOT INITIAL OR
       p_zbseg-saknr IS NOT INITIAL ).
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH  'Πρέπει να είναι συμπληρωμένος μόνο'
          '"Κωδικός Πιστωτή"'
    RAISING wrong_account.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_zbseg-lifnr
    IMPORTING
      output = p_zbseg-lifnr.


  SELECT SINGLE * FROM lfa1
    INTO wa_lfa1
    WHERE lifnr = p_zbseg-lifnr.
  IF sy-subrc <> 0.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Ο Πιστωτής' p_zbseg-lifnr
         'δεν υπάρχει'
    RAISING account_not_in_sap.
  ELSE.
    SELECT SINGLE * FROM lfb1
      INTO wa_lfb1
      WHERE lifnr = p_zbseg-lifnr
      AND   bukrs = p_zbkpf-bukrs.
    IF sy-subrc <> 0.
      MESSAGE ID '00' TYPE 'E' NUMBER 398
      WITH 'Ο Πιστωτής' p_zbseg-lifnr
           'δεν υπάρχει στην εταιρεία' p_zbkpf-bukrs
      RAISING account_not_in_bukrs.
    ELSE.
      IF p_zbseg-umskz <> ' '.

        SELECT SINGLE * FROM t074
          INTO wa_t074
          WHERE ktopl = p_t001-ktopl
          AND   koart = 'K'
          AND   umskz = p_zbseg-umskz
          AND   hkont = wa_lfb1-akont.
        IF sy-subrc <> 0.
          MESSAGE ID '00' TYPE 'E' NUMBER 398
          WITH 'Το είδικό κλείδι' p_zbseg-umskz
               'είναι λάθος'
          RAISING special_gl_error.
        ELSE.
          p_zbseg-hkont = wa_t074-skont.
        ENDIF.

      ELSE.
        p_zbseg-hkont = wa_lfb1-akont.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "CHECK_LIFNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_kunnr USING p_zbseg STRUCTURE bseg
                       p_zbkpf STRUCTURE bkpf
                       p_t001  STRUCTURE t001.

  IF p_zbseg-kunnr IS INITIAL.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Για το κλειδί καταχώρησης'
          p_zbseg-bschl
         'πρέπει να είναι συμπηρωμένος'
         'και ο Κωδικός Πελάτη.'
    RAISING wrong_account.
  ENDIF.
  IF p_zbseg-kunnr IS NOT INITIAL AND
     ( p_zbseg-lifnr IS NOT INITIAL OR
       p_zbseg-saknr IS NOT INITIAL ).
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH  'Πρέπει να είναι συμπληρωμένος μόνο'
          '"Κωδικός Χρεώστη"'
    RAISING wrong_account.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_zbseg-kunnr
    IMPORTING
      output = p_zbseg-kunnr.

  SELECT SINGLE * FROM kna1
    INTO wa_kna1
    WHERE kunnr = p_zbseg-kunnr.
  IF sy-subrc <> 0.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Ο Πελάτης' p_zbseg-kunnr
         'δεν υπάρχει'
    RAISING account_not_in_sap.
  ELSE.
    SELECT SINGLE * FROM knb1
      INTO wa_knb1
      WHERE kunnr = p_zbseg-kunnr
      AND   bukrs = p_zbkpf-bukrs.
    IF sy-subrc <> 0.
      MESSAGE ID '00' TYPE 'E' NUMBER 398
      WITH 'Ο Πελάτης' p_zbseg-kunnr
           'δεν υπάρχει στην εταιρεία' p_zbkpf-bukrs
      RAISING account_not_in_bukrs.
    ELSE.
      IF p_zbseg-umskz <> ' '.
        SELECT SINGLE * FROM t074
          INTO wa_t074
          WHERE ktopl = p_t001-ktopl
          AND   koart = 'D'
          AND   umskz = p_zbseg-umskz
          AND   hkont = wa_knb1-akont.
        IF sy-subrc <> 0.
          MESSAGE ID '00' TYPE 'E' NUMBER 398
          WITH 'Το είδικό κλείδι' p_zbseg-umskz
               'είναι λάθος'
          RAISING special_gl_error.
        ELSE.
          p_zbseg-hkont = wa_t074-skont.
        ENDIF.
      ELSE.
        p_zbseg-hkont = wa_knb1-akont.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "CHECK_KUNNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_SAKNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_saknr USING p_saknr LIKE bseg-saknr
                       p_hkont LIKE bseg-hkont
                       p_lifnr LIKE bseg-lifnr
                       p_kunnr LIKE bseg-kunnr
                       p_bukrs LIKE bkpf-bukrs
                       p_ktopl LIKE t001-ktopl.

  IF p_saknr IS INITIAL.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Ο λογαριαμός δεν μπορει να είναι κενός'
    RAISING wrong_account.
  ENDIF.
  IF p_saknr IS NOT INITIAL AND
     ( p_lifnr IS NOT INITIAL OR
       p_kunnr IS NOT INITIAL ).
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Λαθος στους λογαριασμούς'
    RAISING wrong_account.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_saknr
    IMPORTING
      output = p_saknr.

  SELECT SINGLE * FROM ska1
    INTO wa_ska1
    WHERE saknr = p_saknr
    AND   ktopl = p_ktopl.
  IF sy-subrc <> 0.
    MESSAGE ID '00' TYPE 'E' NUMBER 398
    WITH 'Ο λογαριασμός' p_saknr
         'δεν υπάρχει στο Λογ.Σχέδιο' p_ktopl
    RAISING account_not_in_sap.
  ELSE.

    SELECT SINGLE * FROM skb1
      INTO wa_skb1
      WHERE saknr = p_saknr
      AND   bukrs = p_bukrs.
    IF sy-subrc <> 0.
      MESSAGE ID '00' TYPE 'E' NUMBER 398
      WITH 'Ο λογαριασμός' p_saknr
           'δεν υπάρχει στο Λογ.Σχέδιο' p_bukrs
      RAISING account_not_in_bukrs.
    ENDIF.
  ENDIF.

  p_hkont = p_saknr.

ENDFORM.                    "CHECK_SAKNR
