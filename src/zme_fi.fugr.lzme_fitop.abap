FUNCTION-POOL zme_fi.                       "MESSAGE-ID ..

DATA BEGIN OF _bdcdata OCCURS 1000.
INCLUDE STRUCTURE bdcdata.
DATA END OF _bdcdata.

DATA:
  budat(10),
  bldat(10).

DATA:
  wa_t001  TYPE t001,
  wa_tka02 TYPE tka02,
  wa_tbsl  TYPE tbsl,
  wa_t030k TYPE t030k,
  wa_a003  TYPE a003,
  wa_konp  TYPE konp,
  wa_skb1  TYPE skb1,
  wa_prps  TYPE prps,
  wa_ska1  TYPE ska1,
  wa_lfa1  TYPE lfa1,
  wa_lfb1  TYPE lfb1,
  wa_kna1  TYPE kna1,
  wa_knb1  TYPE knb1,
  wa_t074  TYPE t074,
  wa_t074u type t074u.

tables: t004f,       " Field status definition groups
        tmodf,       " Groups in Field Selection Bar
        tmodg,       " Name of Groups in Field Selection Bar
        tmodo,       " Items in Field Selection Definitions
        tmodp.       " Name of Items in Field Selection Definitions

tables: tmodu.
*
data: w_t004f type t004f.
data: i_t004f type table of t004f.
data: begin of gruptab occurs 0,
        ggrup   like tmodo-ggrup,
        ftext   like tmodp-ftext,
        xhell   type c,
      end of gruptab.
data: c500(500)    type c.
data: incoming_string(200).
data: fleng(4)     type n.
data: w_string_position(3) type n.
