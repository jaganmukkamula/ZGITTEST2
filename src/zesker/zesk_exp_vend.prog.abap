*&---------------------------------------------------------------------*
*& Report  ZESK_EXP_VEND
*&
*&---------------------------------------------------------------------*
*&
*&
*----------------------------------------------------------------------*
*  SAP Change Request  : S4DK906802
*  Change Driver       : IN2294
*  Author              : Alvaro Alvarez de las Horas
*  Modification Date   : 08/18/2023
*  Description         : Enhancements for additional information extraction
*----------------------------------------------------------------------*
*  SAP Change Request  : S4DK907231
*  Change Driver       : IN2294
*  Author              : Maria Saavedra
*  Modification Date   : 08/29/2023
*  Description         : 3 Fields change (Header Name and mapping)
*                         - Street 1, Street 2 and MobilePhoneNumber
*----------------------------------------------------------------------*
report:  zesk_exp_vend.

type-pools: truxs.

tables : t001 , lfa1, lfb1.

data: ti_vendors type table of zesk_exp_vendors.
data: wa_vendors type zesk_exp_vendors.

data: v_error  type char1.

data: lv_titulo type string,
      lv_linea1 type string.

types: begin of ty_lineas,
         linea1 type string,
       end of ty_lineas.

data: wa_lineas type ty_lineas,
      tb_lineas type standard table of ty_lineas.

data: filename(255) type c.
data: filename1(255) type c.

start-of-selection.

  selection-screen begin of block 1.
    select-options: s_bukrs  for t001-bukrs.
    select-options: s_ktokk  for lfa1-ktokk.
    select-options: s_loevma  for lfa1-loevm no intervals no-extension.
    select-options: s_sperra  for lfa1-sperr no intervals no-extension.
    select-options: s_loevmb  for lfb1-loevm no intervals no-extension.
    select-options: s_sperrb  for lfb1-sperr no intervals no-extension.
  selection-screen end of block 1.

initialization.
  s_loevma-sign = 'I'.
  s_loevma-option = 'NE'.
  s_loevma-low = 'X'.
  append s_loevma.

  s_sperra-sign = 'I'.
  s_sperra-option = 'NE'.
  s_sperra-low = 'X'.
  append s_sperra.

  s_loevmb-sign = 'I'.
  s_loevmb-option = 'NE'.
  s_loevmb-low = 'X'.
  append s_loevmb.

  s_sperrb-sign = 'I'.
  s_sperrb-option = 'NE'.
  s_sperrb-low = 'X'.
  append s_sperrb.

  selection-screen skip.


  selection-screen begin of block 2.
    parameters: p_local  radiobutton group a1 default  'X',
                p_f_loc  type zesker_file  default  'C:\TEMP\P2P__SAP__Vendors__.CSV',
                p_server radiobutton group a1,
                p_f_ser  type zesker_file  default '\\SAPDES\ESKER\P2P__SAP__Vendors__.CSV'.

  selection-screen end of block 2.

  selection-screen begin of block 3.
    parameters: p_chkbox  as checkbox default ''.
  selection-screen end of block 3.

  selection-screen begin of block 4.
    parameters: p_stcd1  as checkbox default ''.
  selection-screen end of block 4.


end-of-selection.

  call function 'ZESK_EXPORT_VENDORS'
    exporting
      p_chkbox          = p_chkbox
      p_stcd1           = p_stcd1
    importing
      v_error           = v_error
    tables
      tabla_vendors     = ti_vendors
      s_bukrs           = s_bukrs
      s_ktokk           = s_ktokk
      s_loevma          = s_loevma
      s_sperra          = s_sperra
      s_loevmb          = s_loevmb
      s_sperrb          = s_sperrb
    exceptions
      conversion_failed = 1
      others            = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  if not ti_vendors[] is initial.
    perform armo_cabecera.
    perform tabla_salida.
    perform descargar_csv.
  endif.
*&---------------------------------------------------------------------*
*&      Form  armo_cabecera
*&---------------------------------------------------------------------*
form armo_cabecera.

  concatenate '"CompanyCode__"'
              ',"Number__"'
              ',"Name__"'
*              ',"Street__"'            " DEL S4DK907231
              ',"Street1__"'            " INS S4DK907231
              ',"City__"'
              ',"PostalCode__"'
              ',"Region__"'
              ',"Country__"'
*              ',"Sub__"'               " DEL S4DK907231
              ',"Street2__"'            " INS S4DK907231
              ',"PhoneNumber__"'
*              ',"FaxNumber__"'         " DEL S4DK907231
              ',"MobilePhoneNumber__"'  " INS S4DK907231
              ',"VATNumber__"'
              ',"Email__"'
              ',"PaymentTermCode__"'
              ',"Currency__"'
*              ',"P.O. box"'
              ',"PostOfficeBox__"'
              ',"TaxSystem__"'
              ',"SupplierDue__"'
              ',"ParafiscalTax__"'
              ',"GeneralAccount__"'
              ',"PreferredInvoiceType__"'
              ',"DUNSNumber__"'
              ',"Z_CompanyName2__"'                         "S4DK906802
              ',"Z_POCity__"'                               "S4DK906802
              ',"Z_POState__"'                              "S4DK906802
              ',"Z_POZip__"'                                "S4DK906802
               into lv_titulo.

  move lv_titulo to wa_lineas-linea1.
  append  wa_lineas to tb_lineas.

endform.      "armo_cabecera.

*&---------------------------------------------------------------------*
*&      Form  TABLA_SALIDA
*&---------------------------------------------------------------------*
form tabla_salida.

* ARMAR LINEA X LINEA

  data: str_bukrs                type string,
        str_lifnr                type string,
        str_name1                type string,
        str_stras                type string,
        str_ort01                type string,
        str_pstlz                type string,
        str_regio                type string,
        str_land1                type string,
        str_sub                  type string,
        str_telf1                type string,
        str_telfx                type string,
        str_stcd1                type string,
*          str_stceg TYPE string,
        str_smtp_addr            type string,
        str_zterm                type string,
        str_waers                type string,
        str_pfach                type string,
        str_fityp                type string,    "TaxSystem__
        str_supplierdue          type string, "SupplierDue__
        str_qsbgr                type string,    "ParafiscalTax__
        str_vbund                type string,    "GeneralAccount__
        str_preferredinvoicetype type string, "PreferredInvoiceType__
        str_dunsnumber           type string, "DUNSNumber__
        str_name2                type string,               "S4DK906802
        str_poboxloc             type string,               "S4DK906802
        str_poboxreg             type string,               "S4DK906802
        str_postcode2            type string.               "S4DK906802

  loop at ti_vendors into wa_vendors.
    clear: str_bukrs,
           str_lifnr,
           str_name1,
           str_stras,
           str_ort01,
           str_pstlz,
           str_regio,
           str_land1,
           str_sub,
           str_telf1,
           str_telfx,
           str_stcd1,
*               str_stceg,
           str_smtp_addr,
           str_zterm,
           str_waers,
           str_pfach,
           str_fityp,
           str_supplierdue,
           str_qsbgr,
           str_vbund,
           str_preferredinvoicetype,
           str_dunsnumber.


    move wa_vendors-bukrs to str_bukrs.
    perform escape_quotation_marks using str_bukrs
                                changing str_bukrs.

    move wa_vendors-lifnr to str_lifnr.
    perform escape_quotation_marks using str_lifnr
                                changing str_lifnr.

    move wa_vendors-name1 to str_name1.
    perform escape_quotation_marks using str_name1
                                changing str_name1.

    move wa_vendors-stras to str_stras.
    perform escape_quotation_marks using str_stras
                                changing str_stras.

    move wa_vendors-ort01 to str_ort01.
    perform escape_quotation_marks using str_ort01
                                changing str_ort01.

    move wa_vendors-pstlz to str_pstlz.
    perform escape_quotation_marks using str_pstlz
                                changing str_pstlz.

    move wa_vendors-regio to str_regio.
    perform escape_quotation_marks using str_regio
                                changing str_regio.

    move wa_vendors-land1 to str_land1.
    perform escape_quotation_marks using str_land1
                                changing str_land1.

*    move '' to str_sub.                                " DEL S4DK907231
    move wa_vendors-str_suppl1 to str_sub.              " INS S4DK907231 - Sub is Street2 now
    perform escape_quotation_marks using str_sub
                                changing str_sub.

    move wa_vendors-telf1 to str_telf1.
    perform escape_quotation_marks using str_telf1
                                changing str_telf1.

*    move wa_vendors-telfx to str_telfx.                " DEL S4DK907231
    move wa_vendors-telf2 to str_telfx.                 " INS S4DK907231 - Telfx is now Telf2
    perform escape_quotation_marks using str_telfx
                                changing str_telfx .

    move wa_vendors-stcd1 to str_stcd1.
    perform escape_quotation_marks using str_stcd1
                                changing str_stcd1 .

*         MOVE wa_vendors-stceg TO str_stceg.
*         PERFORM escape_quotation_marks USING str_stceg
*                                     CHANGING str_stceg.


    move wa_vendors-smtp_addr to str_smtp_addr.
    perform escape_quotation_marks using str_smtp_addr
                                changing str_smtp_addr.

    move wa_vendors-zterm to str_zterm.
    perform escape_quotation_marks using str_zterm
                                changing str_zterm.

    move wa_vendors-waers to str_waers.
    perform escape_quotation_marks using str_waers
                                changing str_waers.

    move wa_vendors-pfach to str_pfach.
    perform escape_quotation_marks using str_pfach
                                changing str_pfach.

    move wa_vendors-kraus to str_dunsnumber.
    perform escape_quotation_marks using str_dunsnumber
                                changing str_dunsnumber.

* Begin Addition for S4DK906802
    move wa_vendors-name2 to str_name2.
    perform escape_quotation_marks using str_name2
                                changing str_name2.

    move wa_vendors-po_box_loc to str_poboxloc.
    perform escape_quotation_marks using str_poboxloc
                                changing str_poboxloc.

    move wa_vendors-po_box_reg to str_poboxreg.
    perform escape_quotation_marks using str_poboxreg
                                changing str_poboxreg.

    move wa_vendors-post_code2 to str_postcode2.
    perform escape_quotation_marks using str_postcode2
                                changing str_postcode2.
* End   Addition for S4DK906802

    concatenate '"' str_bukrs '"'
               ',"' str_lifnr '"'
               ',"' str_name1 '"'
               ',"' str_stras '"'
               ',"' str_ort01 '"'
               ',"' str_pstlz '"'
               ',"' str_regio '"'
               ',"' str_land1 '"'
               ',"' str_sub   '"'
               ',"' str_telf1 '"'
               ',"' str_telfx '"'
********************************************************************
* esta modificacion se hizo para Jeff para un cliente de Europa
*se graba en el campo Vatnumber el campo STCEG en lugar de STCD1

               ',"' str_stcd1 '"'
*                    ',"' str_stceg '"'
********************************************************************
               ',"' str_smtp_addr '"'
               ',"' str_zterm '"'
               ',"' str_waers '"'
               ',"' str_pfach '"'
               ',"' str_fityp '"'
               ',"' str_supplierdue '"'
               ',"' str_qsbgr '"'
               ',"' str_vbund '"'
               ',"' str_preferredinvoicetype '"'
               ',"' str_dunsnumber '"'
               ',"' str_name2    '"'                        "S4DK906802
               ',"' str_poboxloc '"'                        "S4DK906802
               ',"' str_poboxreg '"'                        "S4DK906802
               ',"' str_postcode2 '"'                       "S4DK906802
               into lv_linea1.

    move   lv_linea1 to wa_lineas-linea1.
    append wa_lineas to tb_lineas.

  endloop.

endform.                    " TABLA_SALIDA
*&---------------------------------------------------------------------*
*&      Form  escape_quotation_marks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form escape_quotation_marks   using  lv_original type string
                              changing lv_escaped type string.

  move lv_original to lv_escaped.
  replace all occurrences of '"' in lv_escaped with '""'.

endform.                    "escape_quotation_marks

*&---------------------------------------------------------------------*
*&      Form  DESCARGAR_CSV
*&---------------------------------------------------------------------*
form descargar_csv.

*Hacemos la bajada del archivo utilizando la tabla
*que obtuvimos en el paso anterior.

  data: linea         type table_line.
  data: v_fecha       type date.
  data: v_hora        type sy-uzeit.
  data: filename3(255) type c.

  v_fecha = sy-datum.
  v_hora  = sy-uzeit.

  concatenate filename3 v_fecha v_hora '.CSV' into filename3.

  if p_server = 'X'.

    replace all occurrences of 'VENDORS' in p_f_ser with 'Vendors'.
    replace all occurrences of '.CSV' in p_f_ser with filename3.

    open dataset p_f_ser for output in text mode encoding default.

    if sy-subrc eq 0.


      loop at tb_lineas into wa_lineas.
        transfer wa_lineas-linea1 to p_f_ser.
      endloop.

      close dataset p_f_ser.

    endif.

  else.
    replace all occurrences of 'VENDORS' in p_f_loc with 'Vendors'.
    replace all occurrences of '.CSV' in p_f_loc with filename3.
    call function 'GUI_DOWNLOAD'
      exporting
        filename                = p_f_loc
      tables
        data_tab                = tb_lineas
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6.

  endif.


endform.                    " DESCARGAR_CSV


end-of-selection.
