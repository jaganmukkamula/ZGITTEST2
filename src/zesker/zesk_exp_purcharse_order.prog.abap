*&------------------------------------------------------------------*
*& Report  ZESK_EXP_PURCHARSE_ORDER
*&
*&------------------------------------------------------------------*
REPORT  zesk_exp_purcharse_order.

TYPE-POOLS: truxs.

TABLES : t001 , ekko ,ekpo , ekbe , lfa1.

DATA: ti_purcharse_order_h TYPE TABLE OF zesk_exp_purcharse_order_h.
DATA: wa_purcharse_order_h TYPE zesk_exp_purcharse_order_h.

DATA: ti_purcharse_order_i TYPE TABLE OF zesk_exp_purcharse_order_i.
DATA: wa_purcharse_order_i TYPE zesk_exp_purcharse_order_i.

DATA: ti_goodsreceipt_i TYPE TABLE OF zesk_exp_goodsreceipt_i.
DATA: wa_goodsreceipt_i  TYPE zesk_exp_goodsreceipt_i.


DATA: v_error  TYPE char1,
      v_fecha  TYPE zesk_exp_exchange_rate-fecha.

*DATA: lv_titulo_h(255) TYPE c,
DATA: lv_titulo_h TYPE string,
      lv_linea1_h TYPE string.


TYPES: BEGIN OF ty_lineas_h,
       linea1_h TYPE string,
       END OF ty_lineas_h.

DATA: wa_lineas_h TYPE ty_lineas_h,
      tb_lineas_h TYPE STANDARD TABLE OF ty_lineas_h.

*DATA: lv_titulo_i(255) TYPE c,
DATA: lv_titulo_i TYPE string,
      lv_linea1_i TYPE string.


TYPES: BEGIN OF ty_lineas_i,
       linea1_i TYPE string,
       END OF ty_lineas_i.

DATA: wa_lineas_i TYPE ty_lineas_i,
      tb_lineas_i TYPE STANDARD TABLE OF ty_lineas_i.


*DATA: lv_titulo_r(255) TYPE c,
DATA: lv_titulo_r TYPE string,
      lv_linea1_r TYPE string.

TYPES: BEGIN OF ty_lineas_r,
       linea1_r TYPE string,
       END OF ty_lineas_r.

DATA: wa_lineas_r TYPE ty_lineas_r,
      tb_lineas_r TYPE STANDARD TABLE OF ty_lineas_r.


*DATA: filename(255) TYPE c.
DATA: filename TYPE string.
*DATA: filename1(255) TYPE c.
DATA: filename1 TYPE string.



*START-OF-SELECTION.

*v_fecha = sy-datum.

  SELECTION-SCREEN BEGIN OF BLOCK 1.
  SELECT-OPTIONS: s_bukrs  FOR t001-bukrs OBLIGATORY.
  SELECT-OPTIONS: s_ebeln  FOR ekko-ebeln.
  SELECT-OPTIONS: s_vendor FOR lfa1-lifnr.
  SELECT-OPTIONS: s_fecha  FOR ekko-aedat OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK 1.

  SELECTION-SCREEN SKIP.


  SELECTION-SCREEN BEGIN OF BLOCK 2.
  PARAMETERS: p_localh  RADIOBUTTON GROUP a1 DEFAULT  'X',
              p_f_loch TYPE zesker_file
       DEFAULT  'C:\TEMP\ERP__PurchaseorderHeaders__.CSV',
              p_serveh RADIOBUTTON GROUP a1,
              p_f_serh TYPE zesker_file
       DEFAULT '\\SAPDES\ESKER\ERP__PurchaseorderHeaders__.CSV'.

  SELECTION-SCREEN END OF BLOCK 2.

  SELECTION-SCREEN BEGIN OF BLOCK 3.
  PARAMETERS: p_locali  RADIOBUTTON GROUP a2 DEFAULT  'X',
              p_f_loci TYPE zesker_file
       DEFAULT  'C:\TEMP\ERP__PurchaseorderItems__.CSV',
              p_servei RADIOBUTTON GROUP a2,
              p_f_seri TYPE zesker_file
       DEFAULT '\\SAPDES\ESKER\ERP__PurchaseorderItems__.CSV'.

  SELECTION-SCREEN END OF BLOCK 3.

  SELECTION-SCREEN BEGIN OF BLOCK 4.
  PARAMETERS: p_localr  RADIOBUTTON GROUP a3 DEFAULT  'X',
              p_f_locr TYPE zesker_file
              DEFAULT  'C:\TEMP\ERP__GoodsReceiptItems__.CSV',
              p_server RADIOBUTTON GROUP a3,
              p_f_serr TYPE zesker_file
              DEFAULT '\\SAPDES\ESKER\ERP__GoodsReceiptItems__.CSV'.

  SELECTION-SCREEN END OF BLOCK 4.


  SELECTION-SCREEN BEGIN OF BLOCK 5.
     PARAMETERS: p_chkbox  AS CHECKBOX DEFAULT ''.
   SELECTION-SCREEN END OF BLOCK 5.

END-OF-SELECTION.

INITIALIZATION.

  s_ebeln-sign = 'I'.
  s_ebeln-option = 'BT'.
  s_ebeln-low  = '4500000000'.
  s_ebeln-high = '4500999999'.
  APPEND s_ebeln.

  s_fecha-sign = 'I'.
  s_fecha-option = 'BT'.
  s_fecha-low = sy-datum - 365.
  s_fecha-high = sy-datum.
  APPEND s_fecha.

START-OF-SELECTION.

  CALL FUNCTION 'ZESK_EXPORT_PURCHARSE_ORDER'
    EXPORTING
      p_chkbox          = p_chkbox
    IMPORTING
      v_error           = v_error
    TABLES
      tabla_purcharse_order_h = ti_purcharse_order_h
      tabla_purcharse_order_i = ti_purcharse_order_i
      tabla_goodsreceipt_i       = ti_goodsreceipt_i
      s_bukrs             = s_bukrs
      s_ebeln             = s_ebeln
      s_fecha             = s_fecha
      s_vendor            = s_vendor
    EXCEPTIONS
      conversion_failed = 1
    OTHERS
                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF NOT ti_purcharse_order_h[] IS INITIAL AND
     NOT ti_purcharse_order_i[] IS INITIAL. " AND
*     NOT ti_goodsreceipt_i[] IS INITIAL.


    PERFORM armo_cabecera_header.
    PERFORM armo_cabecera_item.
    PERFORM armo_cabecera_recep.
    PERFORM tabla_salida_header.
    PERFORM tabla_salida_item.
    PERFORM tabla_salida_recep.
    PERFORM descargar_csv_header.
    PERFORM descargar_csv_item.
    PERFORM descargar_csv_recep.
  ENDIF.
*&-----------------------------------------------------------------*
*&      Form  armo_cabecera_header
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM armo_cabecera_header.

  CONCATENATE '"CompanyCode__"'
              ',"VendorNumber__"'
              ',"OrderNumber__"'
              ',"OrderDate__"'
              ',"OrderedAmount__"'
              ',"DeliveredAmount__"'
              ',"InvoicedAmount__"'
              ',"PInumber__"'
              ',"InternalOrder__"'
              ',"Buyer__"'       INTO lv_titulo_h.


  MOVE lv_titulo_h TO wa_lineas_h-linea1_h.
  APPEND  wa_lineas_h TO tb_lineas_h.

ENDFORM.      "armo_cabecera_header.

*&-----------------------------------------------------------------*
*&      Form  armo_cabecera_item
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM armo_cabecera_item.

  CONCATENATE '"CompanyCode__"'
              ',"VendorNumber__"'
              ',"OrderNumber__"'
              ',"ItemNumber__"'
              ',"OrderDate__"'
              ',"PartNumber__"'
              ',"Description__"'
              ',"G/Laccount__"'
              ',"CostCenter__"'
              ',"UnitPrice__"'
              ',"OrderedAmount__"'
              ',"OrderedQuantity__"'
              ',"InvoicedAmount__"'
              ',"InvoicedQuantity__"'
              ',"DeliveredAmount__"'
              ',"DeliveredQuantity__"'
              ',"TaxCode__"'
              ',"Gr_Iv__"'   INTO lv_titulo_i.

  MOVE lv_titulo_i TO wa_lineas_i-linea1_i.
  APPEND  wa_lineas_i TO tb_lineas_i.

ENDFORM.      "armo_cabecera_item

*&-----------------------------------------------------------------*
*&      Form  armo_cabecera_recep
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM armo_cabecera_recep.

  CONCATENATE '"CompanyCode__"'
              ',"GoodsReceipt__"'
              ',"DeliveryNote__"'
              ',"OrderNumber__"'
              ',"ItemNumber__"'
              ',"DeliveryDate__"'
              ',"DeliveryCompleted__"'
              ',"Quantity__"'
              ',"Amount__"'
              ',"InvoicedAmount__"'
              ',"InvoicedQuantity__"' INTO lv_titulo_r.

  MOVE lv_titulo_r TO wa_lineas_r-linea1_r.
  APPEND  wa_lineas_r TO tb_lineas_r.

ENDFORM.      "armo_cabecera_recep


*&-----------------------------------------------------------------*
*&      Form  TABLA_SALIDA_HEADER
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM tabla_salida_header.

* ARMAR LINEA X LINEA

  DATA: str_bukrs          TYPE string,
        str_lifnr          TYPE string,
        str_ebeln          TYPE string,
        str_aedat          TYPE string,
        str_po_amount      TYPE string, "OrderedAmount__"'
        str_del_amount     TYPE string, "DeliveredAmount__"'
        str_inv_amount     TYPE string, "InvoicedAmount__"'
        str_func_inter     TYPE string,
        str_internal_order TYPE string,
        str_buyer          TYPE string.

  LOOP AT ti_purcharse_order_h  INTO wa_purcharse_order_h.
    CLEAR: str_bukrs,
           str_lifnr,
           str_ebeln,
           str_aedat,
           str_po_amount,
           str_del_amount,
           str_inv_amount,
           str_func_inter,
           str_internal_order,
           str_buyer.

    MOVE wa_purcharse_order_h-bukrs TO str_bukrs.
    PERFORM escape_quotation_marks USING str_bukrs
                                CHANGING str_bukrs.

    MOVE wa_purcharse_order_h-lifnr TO str_lifnr.
    PERFORM escape_quotation_marks USING str_lifnr
                                CHANGING str_lifnr.

    MOVE wa_purcharse_order_h-ebeln TO str_ebeln.
    PERFORM escape_quotation_marks USING str_ebeln
                                CHANGING str_ebeln.

    MOVE wa_purcharse_order_h-aedat TO str_aedat.
    PERFORM escape_quotation_marks USING str_aedat
                                CHANGING str_aedat.

    MOVE wa_purcharse_order_h-po_amount TO str_po_amount.
    PERFORM escape_quotation_marks USING str_po_amount
                                CHANGING str_po_amount.

    MOVE wa_purcharse_order_h-del_amount TO str_del_amount.
    PERFORM escape_quotation_marks USING str_del_amount
                                CHANGING str_del_amount.

    MOVE wa_purcharse_order_h-inv_amount TO str_inv_amount.
    PERFORM escape_quotation_marks USING str_inv_amount
                                CHANGING str_inv_amount.

    MOVE wa_purcharse_order_h-func_inter TO str_func_inter.
    PERFORM escape_quotation_marks USING str_func_inter
                                CHANGING str_func_inter.

    MOVE wa_purcharse_order_h-internal_order TO str_internal_order.
    PERFORM escape_quotation_marks USING str_internal_order
                                CHANGING str_internal_order.

    MOVE wa_purcharse_order_h-buyer TO str_buyer.
    PERFORM escape_quotation_marks USING str_buyer
                                CHANGING str_buyer.

    CONCATENATE '"' str_bukrs '"'
               ',"' str_lifnr '"'
               ',"' str_ebeln '"'
               ',"' str_aedat '"'
               ',"' str_po_amount'"'
               ',"' str_del_amount '"'
               ',"' str_inv_amount '"'
               ',"' str_func_inter '"'
               ',"' str_internal_order '"'
               ',"' str_buyer '"'    INTO lv_linea1_h.

    MOVE   lv_linea1_h TO wa_lineas_h-linea1_h.
    APPEND wa_lineas_h TO tb_lineas_h.

  ENDLOOP.

ENDFORM.                    " TABLA_SALIDA_HEADER

*&-----------------------------------------------------------------*
*&      Form  TABLA_SALIDA_ITEM
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM tabla_salida_item.

* ARMAR LINEA X LINEA

  DATA: str_bukrs        TYPE string,
        str_lifnr        TYPE string,
        str_ebeln        TYPE string,
        str_ebelp        TYPE string,
        str_aedat        TYPE string,
        str_matnr        TYPE string,
        str_txz01        TYPE string,
        str_sakto        TYPE string, "Cuenta Mayor
        str_kostl        TYPE string, "Centro Costo
        str_unit_price   TYPE string, "UnitPrice__"'
        str_netwr        TYPE string, "OrderedAmount__"'
        str_menge        TYPE string, "OrderedQuantity__"'
        str_inv_amount   TYPE string, "InvoicedAmount__"'
        str_inv_quantity TYPE string, "InvoicedQuantity__"'
        str_del_amount   TYPE string, "DeliveredAmount__"'
        str_del_quantity TYPE string, "DeliveredQuantity__"'
        str_mwskz        TYPE string,
        str_gr_iv        TYPE string.


  LOOP AT ti_purcharse_order_i  INTO wa_purcharse_order_i.
    CLEAR: str_bukrs,
           str_lifnr,
           str_ebeln,
           str_ebelp,
           str_aedat,
           str_matnr,
           str_txz01,
           str_sakto,
           str_kostl,
           str_unit_price,
           str_netwr,
           str_menge,
           str_inv_amount,
           str_inv_quantity,
           str_del_amount,
           str_del_quantity,
           str_mwskz,
           str_gr_iv.

    MOVE wa_purcharse_order_i-bukrs TO str_bukrs.
    PERFORM escape_quotation_marks USING str_bukrs
                                CHANGING str_bukrs.

    MOVE wa_purcharse_order_i-lifnr TO str_lifnr.
    PERFORM escape_quotation_marks USING str_lifnr
                                CHANGING str_lifnr.

    MOVE wa_purcharse_order_i-ebeln TO str_ebeln.
    PERFORM escape_quotation_marks USING str_ebeln
                                CHANGING str_ebeln.

    MOVE wa_purcharse_order_i-ebelp TO str_ebelp.
    PERFORM escape_quotation_marks USING str_ebelp
                                CHANGING str_ebelp.

    MOVE wa_purcharse_order_i-aedat TO str_aedat.
    PERFORM escape_quotation_marks USING str_aedat
                                CHANGING str_aedat.

    MOVE wa_purcharse_order_i-matnr TO str_matnr.
    PERFORM escape_quotation_marks USING str_matnr
                                CHANGING str_matnr.

    MOVE wa_purcharse_order_i-txz01 TO str_txz01.
    PERFORM escape_quotation_marks USING str_txz01
                                CHANGING str_txz01.

    MOVE wa_purcharse_order_i-sakto TO str_sakto.
    PERFORM escape_quotation_marks USING str_sakto
                                CHANGING str_sakto.


    MOVE wa_purcharse_order_i-kostl TO str_kostl.
    PERFORM escape_quotation_marks USING str_kostl
                                CHANGING str_kostl.


    MOVE wa_purcharse_order_i-unit_price TO str_unit_price.
    PERFORM escape_quotation_marks USING str_unit_price
                                CHANGING str_unit_price.

    MOVE wa_purcharse_order_i-netwr TO str_netwr.
    PERFORM escape_quotation_marks USING str_netwr
                                CHANGING str_netwr.


    MOVE wa_purcharse_order_i-menge TO str_menge.
    PERFORM escape_quotation_marks USING str_menge
                                CHANGING str_menge.


    MOVE wa_purcharse_order_i-inv_amount TO str_inv_amount.
    PERFORM escape_quotation_marks USING str_inv_amount
                                CHANGING str_inv_amount.


    MOVE wa_purcharse_order_i-inv_quantity TO str_inv_quantity.
    PERFORM escape_quotation_marks USING str_inv_quantity
                                CHANGING str_inv_quantity.

    MOVE wa_purcharse_order_i-del_amount TO str_del_amount.
    PERFORM escape_quotation_marks USING str_del_amount
                                CHANGING str_del_amount.

    MOVE wa_purcharse_order_i-del_quantity TO str_del_quantity.
    PERFORM escape_quotation_marks USING str_del_quantity
                                CHANGING str_del_quantity.

    MOVE wa_purcharse_order_i-mwskz TO str_mwskz.
    PERFORM escape_quotation_marks USING str_mwskz
                                CHANGING str_mwskz.

    MOVE wa_purcharse_order_i-gr_iv TO str_gr_iv.
    PERFORM escape_quotation_marks USING str_gr_iv
                                CHANGING str_gr_iv.


    CONCATENATE '"' str_bukrs '"'
               ',"' str_lifnr '"'
               ',"' str_ebeln '"'
               ',"' str_ebelp '"'
               ',"' str_aedat '"'
               ',"' str_matnr '"'
               ',"' str_txz01 '"'
               ',"' str_sakto '"'
               ',"' str_kostl '"'
               ',"' str_unit_price '"'
               ',"' str_netwr'"'
               ',"' str_menge'"'
               ',"' str_inv_amount '"'
               ',"' str_inv_quantity'"'
               ',"' str_del_amount '"'
               ',"' str_del_quantity'"'
               ',"' str_mwskz '"'
               ',"' str_gr_iv '"'  INTO lv_linea1_i.

    MOVE   lv_linea1_i TO wa_lineas_i-linea1_i.
    APPEND wa_lineas_i TO tb_lineas_i.

  ENDLOOP.

ENDFORM.                    " TABLA_SALIDA_ITEM

*&-----------------------------------------------------------------*
*&      Form  TABLA_SALIDA_RECEP
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM tabla_salida_recep.

* ARMAR LINEA X LINEA

*  CONCATENATE     '"CompanyCode__"'
*                 ',"GoodsReceipt_"'
*                 ',"DeliveryNote__"'
*                 ',"OrderNumber__"'
*                 ',"ItemNumber__"'
*                 ',"DeliveryDate__"'
*                 ',"DeliveryCompleted__"'
*                 ',"Quantity__"'
*                 ',"Amount__"'
*                 ',"InvoicedAmount__"'
*                 ',"InvoicedQuantity__"'      INTO lv_titulo_r.

  DATA: str_bukrs         TYPE string,
        str_goods_receipt TYPE string,
        str_del_note      TYPE string,
        str_ebeln         TYPE string,
        str_ebelp         TYPE string,
        str_del_date      TYPE string,
        str_del_completed TYPE string,
        str_quantity      TYPE string,
        str_amount        TYPE string,
        str_inv_amount    TYPE string,
        str_inv_quantity  TYPE string.


  LOOP AT ti_goodsreceipt_i  INTO wa_goodsreceipt_i.
    CLEAR: str_bukrs,
           str_goods_receipt,
           str_del_note,
           str_ebeln,
           str_ebelp,
           str_del_date,
           str_del_completed,
           str_quantity,
           str_amount,
           str_inv_amount,
           str_inv_quantity.

    MOVE wa_goodsreceipt_i-bukrs TO str_bukrs.
    PERFORM escape_quotation_marks USING str_bukrs
                                CHANGING str_bukrs.

    MOVE wa_goodsreceipt_i-goods_receipt TO str_goods_receipt.
    PERFORM escape_quotation_marks USING str_goods_receipt
                                CHANGING str_goods_receipt.

    MOVE wa_goodsreceipt_i-del_note TO str_del_note.
    PERFORM escape_quotation_marks USING str_del_note
                                CHANGING str_del_note.

    MOVE wa_goodsreceipt_i-ebeln TO str_ebeln.
    PERFORM escape_quotation_marks USING str_ebeln
                                CHANGING str_ebeln.

    MOVE wa_goodsreceipt_i-ebelp TO str_ebelp.
    PERFORM escape_quotation_marks USING str_ebelp
                                CHANGING str_ebelp.

    MOVE wa_goodsreceipt_i-del_date TO str_del_date.
    PERFORM escape_quotation_marks USING str_del_date
                                CHANGING str_del_date.

    MOVE wa_goodsreceipt_i-del_completed TO str_del_completed.
    PERFORM escape_quotation_marks USING str_del_completed
                                CHANGING str_del_completed.

    MOVE wa_goodsreceipt_i-quantity TO str_quantity.
    PERFORM escape_quotation_marks USING str_quantity
                                CHANGING str_quantity.

    MOVE wa_goodsreceipt_i-amount TO str_amount.
    PERFORM escape_quotation_marks USING str_amount
                                CHANGING str_amount.

    MOVE wa_goodsreceipt_i-inv_amount TO str_inv_amount.
    PERFORM escape_quotation_marks USING str_inv_amount
                                CHANGING str_inv_amount.

    MOVE wa_goodsreceipt_i-inv_quantity TO str_inv_quantity.
    PERFORM escape_quotation_marks USING str_inv_quantity
                                CHANGING str_inv_quantity.


    CONCATENATE '"' str_bukrs '"'
               ',"' str_goods_receipt '"'
               ',"' str_del_note '"'
               ',"' str_ebeln '"'
               ',"' str_ebelp '"'
               ',"' str_del_date '"'
               ',"' str_del_completed '"'
               ',"' str_quantity '"'
               ',"' str_amount '"'
               ',"' str_inv_amount '"'
               ',"' str_inv_quantity '"'       INTO lv_linea1_r.


    MOVE   lv_linea1_r TO wa_lineas_r-linea1_r.
    APPEND wa_lineas_r TO tb_lineas_r.

ENDLOOP.

ENDFORM.                    " TABLA_SALIDA_RECEP

*&-----------------------------------------------------------------*
*&      Form  escape_quotation_marks
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM escape_quotation_marks   USING  lv_original TYPE string
                              CHANGING lv_escaped TYPE string.

  REPLACE ALL OCCURENCES OF '"' IN lv_original WITH '""'.

  MOVE lv_original TO lv_escaped.

ENDFORM.                    "escape_quotation_marks

*&-----------------------------------------------------------------*
*&      Form  DESCARGAR_CSV_HEADER
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM descargar_csv_header.

*Hacemos la bajada del archivo utilizando la tabla
*que obtuvimos en el paso anterior.

  DATA: linea         TYPE table_line.
  DATA: v_fecha       TYPE date.
  DATA: v_hora        TYPE sy-uzeit.
*  DATA: filename3_h(255) TYPE c.
  DATA: filename3_h TYPE string.

  v_fecha = sy-datum.
  v_hora  = sy-uzeit.

  CONCATENATE filename3_h v_fecha v_hora '.CSV' INTO filename3_h.

  IF p_serveh = 'X'.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_serh WITH filename3_h.

    OPEN DATASET p_f_serh FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc EQ 0.

      LOOP AT tb_lineas_h INTO wa_lineas_h.
        TRANSFER wa_lineas_h-linea1_h TO p_f_serh.
      ENDLOOP.

      CLOSE DATASET p_f_serh.

    ENDIF.

  ELSE.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_loch WITH filename3_h.
    CALL FUNCTION  'GUI_DOWNLOAD'
     EXPORTING
           filename = p_f_loch
     TABLES
*                data_tab = tb_outtab
           data_tab = tb_lineas_h
    EXCEPTIONS
         file_write_error = 1
         no_batch = 2
         gui_refuse_filetransfer = 3
         invalid_type = 4
         no_authority = 5
         unknown_error = 6.

  ENDIF.


ENDFORM.                    " DESCARGAR_CSV_HEADER

*&-----------------------------------------------------------------*
*&      Form  DESCARGAR_CSV_ITEM
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM descargar_csv_item.

  DATA: linea         TYPE table_line.
  DATA: v_fecha       TYPE date.
  DATA: v_hora        TYPE sy-uzeit.
*  DATA: filename3_i(255) TYPE c.
  DATA: filename3_i TYPE string.

  v_fecha = sy-datum.
  v_hora  = sy-uzeit.


  CONCATENATE filename3_i v_fecha v_hora '.CSV' INTO filename3_i.

  IF p_servei = 'X'.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_seri WITH filename3_i.

    OPEN DATASET p_f_seri FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc EQ 0.

      LOOP AT tb_lineas_i INTO wa_lineas_i.
        TRANSFER wa_lineas_i-linea1_i TO p_f_seri.
      ENDLOOP.

      CLOSE DATASET p_f_seri.

    ENDIF.

  ELSE.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_loci WITH filename3_i.
    CALL FUNCTION  'GUI_DOWNLOAD'
     EXPORTING
           filename = p_f_loci
     TABLES
*                data_tab = tb_outtab
           data_tab = tb_lineas_i
    EXCEPTIONS
         file_write_error = 1
         no_batch = 2
         gui_refuse_filetransfer = 3
         invalid_type = 4
         no_authority = 5
         unknown_error = 6.

  ENDIF.


ENDFORM.                    " DESCARGAR_CSV_ITEM
*&-----------------------------------------------------------------*
*&      Form  DESCARGAR_CSV_RECEP
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------------------------------------------------------*
FORM descargar_csv_recep.

  DATA: linea         TYPE table_line.
  DATA: v_fecha       TYPE date.
  DATA: v_hora        TYPE sy-uzeit.
*  DATA: filename3_r(255) TYPE c.
  DATA: filename3_r TYPE string.

  v_fecha = sy-datum.
  v_hora  = sy-uzeit.

  CONCATENATE filename3_r v_fecha v_hora '.CSV' INTO filename3_r.

  IF p_server = 'X'.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_serr WITH filename3_r.

    OPEN DATASET p_f_serr FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc EQ 0.

      LOOP AT tb_lineas_r INTO wa_lineas_r.
        TRANSFER wa_lineas_r-linea1_r TO p_f_serr.
      ENDLOOP.

      CLOSE DATASET p_f_serr.

    ENDIF.

  ELSE.

    REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_locr WITH filename3_r.
    CALL FUNCTION  'GUI_DOWNLOAD'
     EXPORTING
           filename = p_f_locr
     TABLES
           data_tab = tb_lineas_r
    EXCEPTIONS
         file_write_error = 1
         no_batch = 2
         gui_refuse_filetransfer = 3
         invalid_type = 4
         no_authority = 5
         unknown_error = 6.

  ENDIF.


ENDFORM.                    " DESCARGAR_CSV_RECEP
