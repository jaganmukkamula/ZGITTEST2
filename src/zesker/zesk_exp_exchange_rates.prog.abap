*&---------------------------------------------------------------------*
*& Report  ZESK_EXP_EXCHANGE_RATES
*&
*&---------------------------------------------------------------------*

REPORT  ZESK_EXP_EXCHANGE_RATES.

type-pools: truxs.

TABLES : t001 , tcurr.

DATA: ti_exchange_rate type table of zesk_exp_exchange_rate.
DATA: wa_exchange_rate TYPE zesk_exp_exchange_rate.

DATA: v_error  type char1,
*      V_GDATU  TYPE tcurr-GDATU,
      v_fecha  TYPE zesk_exp_exchange_rate-fecha.

DATA: lv_titulo(255) TYPE c,
      lv_linea1 TYPE string.


TYPES: BEGIN OF ty_lineas,
       linea1 TYPE string,
       END OF ty_lineas.

DATA: wa_lineas TYPE ty_lineas,
      tb_lineas TYPE STANDARD TABLE OF ty_lineas.

DATA: filename(255) TYPE c.
DATA: filename1(255) TYPE c.



START-OF-SELECTION.

*v_fecha = sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK 1.
 SELECT-OPTIONS: s_bukrs  FOR t001-bukrs OBLIGATORY.
 SELECT-OPTIONS: s_tcurr  FOR tcurr-tcurr.
* SELECT-OPTIONS: s_gdatu  FOR tcurr-GDATU NO INTERVALS NO-EXTENSION.
 SELECT-OPTIONS: s_gdatu  FOR v_fecha  default sy-datum
                                         NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN SKIP.


 SELECTION-SCREEN BEGIN OF BLOCK 2.
    PARAMETERS: p_local  RADIOBUTTON GROUP a1 DEFAULT  'X',
                p_f_loc TYPE ZESKER_FILE
                        DEFAULT  'C:\TEMP\ERP__Currencies__.CSV',
                p_server RADIOBUTTON GROUP a1,
                p_f_ser TYPE ZESKER_FILE
                        DEFAULT '\\SAPDES\ESKER\ERP__Currencies__.CSV'.

  SELECTION-SCREEN END OF BLOCK 2.

END-OF-SELECTION.


  CALL FUNCTION 'ZESK_EXPORT_EXCHANGE_RATE'
    IMPORTING
      v_error           = v_error
    TABLES
      tabla_exchange_rate = ti_exchange_rate
      s_bukrs             = s_bukrs
      s_tcurr             = s_tcurr
      s_gdatu             = s_gdatu
    EXCEPTIONS
      conversion_failed = 1
    OTHERS
                = 2.
  IF sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF NOT ti_exchange_rate[] IS INITIAL.

      PERFORM armo_cabecera.
      PERFORM tabla_salida.
      PERFORM descargar_csv.
  ENDIF.
*&-------------------------------------------------------------------*
*&      Form  armo_cabecera
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
  FORM armo_cabecera.

    CONCATENATE '"CompanyCode__"'
                ',"CurrencyFrom__"'   "moneda destino
                ',"RatioFrom__"'      "factor desde  si es 0 poner 1
                ',"RatioTo__"'        "factor hasta  si es 0 poner 1
                ',"Rate__"'      INTO lv_titulo.   " tipo de cambio

    MOVE lv_titulo to wa_lineas-linea1.
    APPEND  wa_lineas to tb_lineas.

  ENDFORM.      "armo_cabecera.

*&-------------------------------------------------------------------*
*&      Form  TABLA_SALIDA
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
  FORM tabla_salida.

* ARMAR LINEA X LINEA

    DATA: str_bukrs TYPE string,
          str_tcurr TYPE string,
          str_ffact TYPE string,
          str_tfact TYPE string,
          str_ukurs TYPE string.


    LOOP AT ti_exchange_rate INTO wa_exchange_rate.
        CLEAR: str_bukrs,
               str_tcurr,
               str_ffact,
               str_tfact,
               str_ukurs.

         MOVE wa_exchange_rate-bukrs TO str_bukrs.
         PERFORM escape_quotation_marks USING str_bukrs
                                     CHANGING str_bukrs.

         MOVE wa_exchange_rate-fcurr TO str_tcurr.
         PERFORM escape_quotation_marks USING str_tcurr
                                     CHANGING str_tcurr.

         MOVE wa_exchange_rate-ffact TO str_ffact.
         PERFORM escape_quotation_marks USING str_ffact
                                     CHANGING str_ffact.

         MOVE wa_exchange_rate-tfact TO str_tfact.
         PERFORM escape_quotation_marks USING str_tfact
                                     CHANGING str_tfact.

         MOVE wa_exchange_rate-ukurs TO str_ukurs.
         PERFORM escape_quotation_marks USING str_ukurs
                                     CHANGING str_ukurs.



         CONCATENATE '"' str_bukrs '"'
                    ',"' str_tcurr '"'
                    ',"' str_ffact '"'
                    ',"' str_tfact '"'
                    ',"' str_ukurs '"'    INTO lv_linea1.

          MOVE   lv_linea1 to wa_lineas-linea1.
          APPEND wa_lineas to tb_lineas.

  ENDLOOP.

ENDFORM.                    " TABLA_SALIDA
*&-------------------------------------------------------------------*
*&      Form  escape_quotation_marks
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM escape_quotation_marks   USING  lv_original TYPE string
                              CHANGING lv_escaped TYPE string.

    REPLACE ALL OCCURENCES OF '"' IN lv_original WITH '""'.

    MOVE lv_original TO lv_escaped.

ENDFORM.

*&-------------------------------------------------------------------*
*&      Form  DESCARGAR_CSV
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM descargar_csv.

*Hacemos la bajada del archivo utilizando la tabla
*que obtuvimos en el paso anterior.

  DATA: linea         TYPE table_line.
  DATA: v_fecha       TYPE date.
  DATA: v_hora        TYPE sy-uzeit.
  DATA: filename3(255) TYPE c.

  v_fecha = sy-datum.
  v_hora  = sy-uzeit.

   CONCATENATE filename3 v_fecha v_hora '.CSV' INTO filename3.

  IF p_server = 'X'.

       REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_ser WITH filename3.

       OPEN DATASET p_f_ser FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

       IF sy-subrc EQ 0.

         LOOP AT tb_lineas INTO wa_lineas.
           TRANSFER wa_lineas-linea1 TO p_f_ser.
         ENDLOOP.

         CLOSE DATASET p_f_ser.

       ENDIF.

  ELSE.

         REPLACE ALL OCCURRENCES OF '.CSV' IN p_f_loc WITH filename3.
         CALL FUNCTION  'GUI_DOWNLOAD'
          EXPORTING
                filename = p_f_loc
          TABLES
*                data_tab = tb_outtab
                data_tab = tb_lineas
         EXCEPTIONS
              file_write_error = 1
              no_batch = 2
              gui_refuse_filetransfer = 3
              invalid_type = 4
              no_authority = 5
              unknown_error = 6.

  ENDIF.


ENDFORM.                    " DESCARGAR_CSV


END-OF-SELECTION.
