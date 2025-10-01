function z_test.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_CELLTAB) TYPE  LVC_T_STYL
*"     REFERENCE(I_FIELDNAME) TYPE  CHAR100
*"  EXPORTING
*"     REFERENCE(E_CELLTAB) TYPE  LVC_T_STYL
*"----------------------------------------------------------------------
  data:
    cellline type lvc_s_styl.

  data: begin of celltab occurs 0.
      include structure lvc_s_styl.
  data: end   of celltab.

  clear: celltab, celltab[].

  celltab[] = i_celltab.

  delete celltab where fieldname = i_fieldname.

  cellline-style = cl_gui_alv_grid=>mc_style_button.
  cellline-fieldname = i_fieldname.

  append cellline to celltab.
  sort celltab.
  e_celltab = celltab[].

endfunction.
