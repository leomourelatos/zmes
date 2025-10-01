class ZME_CHECKS definition
  public
  final
  create public .

public section.

  methods Z_AMKA_CHECK
    importing
      !AMKACODE type CHAR11
    exceptions
      WRONG_INPUT_TYPE
      NOT_VALID_LENGTH
      NOT_NUMERIC
      NOT_VALID
      NO_INPUT .
protected section.
private section.
ENDCLASS.



CLASS ZME_CHECKS IMPLEMENTATION.


  METHOD z_amka_check.

    CONSTANTS:
  numbers(11) VALUE '0123456789'.
    DATA:
      w_amkacode(11) TYPE c,
      w_flength      TYPE i,                         "Field' s length
      w_ftype        TYPE c,                           "Field' s type
      w_p01          TYPE i,
      w_p02(2)       TYPE c,
      w_p021         TYPE i,
      w_p022         TYPE i,
      w_p03          TYPE i,
      w_p04(2)       TYPE c,
      w_p041         TYPE i,
      w_p042         TYPE i,
      w_p05          TYPE i,
      w_p06(2)       TYPE c,
      w_p061         TYPE i,
      w_p062         TYPE i,
      w_p07          TYPE i,
      w_p08(2)       TYPE c,
      w_p081         TYPE i,
      w_p082         TYPE i,
      w_p09          TYPE i,
      w_p10(2)       TYPE c,
      w_p101         TYPE i,
      w_p102         TYPE i,
      w_p11          TYPE i,
      w_sum          TYPE i,
      w_m11          TYPE i.
* }
* Check type
    DESCRIBE FIELD amkacode TYPE w_ftype.
    IF NOT ( w_ftype = 'C' OR w_ftype = 'N' ).
      MESSAGE e030(8n1) WITH 'Z_AMKA_CODE_CHECK'
              RAISING wrong_input_type.
    ENDIF.

* Check length
    w_flength = strlen( amkacode ).
    IF ( w_flength = 0 ).
      MESSAGE e001(zmess) WITH amkacode RAISING no_input.
    ENDIF.

    IF NOT ( w_flength = 11 ).
      MESSAGE e001(zmess) WITH amkacode RAISING not_valid_length.
    ENDIF.


* Check contents
    w_amkacode = amkacode.
    IF w_amkacode CN numbers.
      MESSAGE e002(zmess) WITH amkacode RAISING not_numeric.
    ENDIF.

* Check validity
    w_p01 = w_amkacode+0(1).
    w_p02 = w_amkacode+1(1) * 2.
    w_p021 = w_p02+0(1).
    w_p022 = w_p02+1(1).
    w_p03 = w_amkacode+2(1).
    w_p04 = w_amkacode+3(1) * 2.
    w_p041 = w_p04+0(1).
    w_p042 = w_p04+1(1).
    w_p05 = w_amkacode+4(1).
    w_p06 = w_amkacode+5(1) * 2.
    w_p061 = w_p06+0(1).
    w_p062 = w_p06+1(1).
    w_p07 = w_amkacode+6(1).
    w_p08 = w_amkacode+7(1) * 2.
    w_p081 = w_p08+0(1).
    w_p082 = w_p08+1(1).
    w_p09 = w_amkacode+8(1).
    w_p10 = w_amkacode+9(1) * 2.
    w_p101 = w_p10+0(1).
    w_p102 = w_p10+1(1).
    w_p11 = w_amkacode+10(1).

    w_sum = w_p01 +
            w_p021 +
            w_p022 +
            w_p03 +
            w_p041 +
            w_p042 +
            w_p05 +
            w_p061 +
            w_p062 +
            w_p07 +
            w_p081 +
            w_p082 +
            w_p09 +
            w_p101 +
            w_p102 +
            w_p11.
    w_m11 = w_sum MOD 10.
    IF w_m11 = 10.
      w_m11 = 0.
    ENDIF.
    IF w_m11 <> 0.
      MESSAGE e003(zmess) WITH amkacode RAISING not_valid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
