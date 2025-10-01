FUNCTION-POOL ZME_ALV.                      "MESSAGE-ID ..

TYPE-POOLS: slis,SDYDO.

INCLUDE: <list>.

DATA: lv_inttab_head TYPE slis_t_listheader.
DATA: ta_comment     TYPE slis_t_listheader.

FIELD-SYMBOLS: <prog_name> TYPE sy-cprog.

data:
  lv_scr1  TYPE smp_dyntxt,
  lv_scr2  TYPE smp_dyntxt,
  lv_scr3  TYPE smp_dyntxt,
  lv_scr4  TYPE smp_dyntxt,
  lv_scr5  TYPE smp_dyntxt,
  lv_scr6  TYPE smp_dyntxt,
  lv_scr7  TYPE smp_dyntxt,
  lv_scr8  TYPE smp_dyntxt,
  lv_scr9  TYPE smp_dyntxt,
  lv_scr10 TYPE smp_dyntxt.
