FUNCTION zesk_export_exchange_rate.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     REFERENCE(V_ERROR) TYPE  CHAR1
*"  TABLES
*"      TABLA_EXCHANGE_RATE STRUCTURE  ZESK_EXP_EXCHANGE_RATE
*"      S_BUKRS STRUCTURE  VKKKIBUKRS
*"      S_TCURR STRUCTURE  ZESK_TCURR
*"      S_GDATU STRUCTURE  ZESK_GDATU
*"----------------------------------------------------------------------

  DATA: tb_t001 TYPE STANDARD TABLE OF t001,
        wa_t001 TYPE t001.

  DATA: r_fcurr TYPE STANDARD TABLE OF zesk_tcurr,
        v_fcurr TYPE zesk_tcurr.

  DATA: tb_rates TYPE STANDARD TABLE OF bapi1093_0,
        wa_rates TYPE bapi1093_0.

  DATA: tb_return TYPE STANDARD TABLE OF bapiret1.

  DATA: wa_tabla_exchange_rate LIKE LINE OF tabla_exchange_rate.

  DATA: v_gdatu TYPE bapi1093_2-trans_date.

  IF s_gdatu[] IS NOT INITIAL.
    READ TABLE s_gdatu INDEX 1.
    v_gdatu = s_gdatu-gdatu_low.
  ELSE.
    v_gdatu = sy-datum.
  ENDIF.

  SELECT *
    from t001
    into TABLE tb_t001
  WHERE bukrs in s_bukrs.

  LOOP at tb_t001 into wa_t001.
    CLEAR v_fcurr.

    v_fcurr-sign = 'I'.
    v_fcurr-option = 'EQ'.
    v_fcurr-tcurr_low = wa_t001-waers.

    APPEND v_fcurr TO r_fcurr.

  ENDLOOP.

  CALL FUNCTION 'BAPI_EXCHRATE_GETCURRENTRATES'
    EXPORTING
      date            =  v_gdatu
      date_type       =  'V'
      rate_type       =  'M'
    TABLES
      from_curr_range   =  s_tcurr
      to_currncy_range  =  r_fcurr
      exch_rate_list    =  tb_rates
      return            =  tb_return.

  LOOP AT tb_t001 INTO wa_t001.

    IF wa_t001-waers IN s_tcurr.

      CLEAR wa_tabla_exchange_rate.
      wa_tabla_exchange_rate-bukrs = wa_t001-bukrs.
      wa_tabla_exchange_rate-fcurr = wa_t001-waers.
      wa_tabla_exchange_rate-tcurr = wa_t001-waers.
      wa_tabla_exchange_rate-gdatu = v_gdatu.
      wa_tabla_exchange_rate-ffact = 1.
      wa_tabla_exchange_rate-tfact = 1.
      wa_tabla_exchange_rate-ukurs = 1.

      APPEND wa_tabla_exchange_rate TO tabla_exchange_rate.

    ENDIF.

    LOOP AT tb_rates INTO wa_rates
      WHERE to_currncy EQ wa_t001-waers.

        CLEAR wa_tabla_exchange_rate.
        wa_tabla_exchange_rate-bukrs = wa_t001-bukrs.
        wa_tabla_exchange_rate-fcurr = wa_rates-from_curr.
        wa_tabla_exchange_rate-tcurr = wa_rates-to_currncy.
        wa_tabla_exchange_rate-gdatu = wa_rates-valid_from.
        wa_tabla_exchange_rate-ffact = wa_rates-from_factor.
        wa_tabla_exchange_rate-tfact = wa_rates-to_factor.
        wa_tabla_exchange_rate-ukurs = wa_rates-exch_rate.

        APPEND wa_tabla_exchange_rate TO tabla_exchange_rate.

    ENDLOOP.
  ENDLOOP.

  SORT tabla_exchange_rate BY bukrs fcurr gdatu.
  DELETE ADJACENT DUPLICATES FROM tabla_exchange_rate
         COMPARING bukrs fcurr.

  IF tabla_exchange_rate[] IS INITIAL.
    v_error = 'X'.
  ENDIF.

ENDFUNCTION.
