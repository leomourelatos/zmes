*&---------------------------------------------------------------------*
*& Report ZME_READ_AD_V1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zme_read_ad_v1
  NO STANDARD PAGE HEADING
  LINE-COUNT 65
  LINE-SIZE 255.

DATA:
  lt_ldap TYPE ldapetab.

PARAMETERS:
  p_read RADIOBUTTON GROUP a DEFAULT 'X'.

PARAMETERS:
  p_check RADIOBUTTON GROUP a.

PARAMETERS:
  p_create RADIOBUTTON GROUP a.

PARAMETERS:
  p_enable RADIOBUTTON GROUP a.

PARAMETERS:
  p_disabl RADIOBUTTON GROUP a.

PARAMETERS:
  p_psw RADIOBUTTON GROUP a.

PARAMETERS:
  givennam TYPE string LOWER CASE DEFAULT 'Test',
  sn       TYPE string LOWER CASE DEFAULT 'RFQ'.

START-OF-SELECTION.

  IF p_check = ' '.

    CALL FUNCTION 'LDAP_SIMPLEBIND'
      EXPORTING
        serverid     = 'ITHAKIDC3'
        usr          = 'leoadmin@ithakicentral.gr'
        pwd          = 'Leo1234!!!@'
*        usr          = 'leo.mourelatos@ithakicentral.gr'
*        pwd          = 'L0gitech+('
*       USR_STRING   =
*       PWD_STRING   =
*       WAIT_TIME    = 0
* IMPORTING
*       LDAPRC       =
* CHANGING
*       HOLDSESS     = 0
      EXCEPTIONS
        no_authoriz  = 1
        config_error = 2
        nomore_conns = 3
        ldap_failure = 4
        not_alive    = 5
        other_error  = 6
        OTHERS       = 7.
    IF sy-subrc <> 0.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ELSE.

      IF p_read = 'X'.

        zme_user=>ad_read( ).

      ELSEIF p_create = 'X'.

        zme_user=>ad_create( i_givenname = givennam
                             i_sn        = sn ).

      ELSEIF p_enable = 'X'.

        zme_user=>ad_enable( i_givenname = givennam
                             i_sn        = sn ).

      ELSEIF p_disabl = 'X'.

        zme_user=>ad_disable( i_givenname = givennam
                              i_sn        = sn ).

      ELSEIF p_psw = 'X'.

        zme_user=>ad_changepsw( i_givenname = givennam
                                i_sn        = sn ).

      ENDIF.

    ENDIF.
  ELSE.

    zme_user=>ad_check( i_givenname = givennam
                        i_sn        = sn ).

  ENDIF.

  CALL FUNCTION 'LDAP_UNBIND'
* IMPORTING
*   LDAPRC             =
    EXCEPTIONS
      conn_outdate = 1
      ldap_failure = 2
      not_alive    = 3
      other_error  = 4
      OTHERS       = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
