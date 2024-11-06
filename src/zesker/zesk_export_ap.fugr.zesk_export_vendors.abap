FUNCTION zesk_export_vendors .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_CHKBOX) TYPE  CHAR1
*"     VALUE(P_STCD1) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(V_ERROR) TYPE  CHAR1
*"  TABLES
*"      TABLA_VENDORS STRUCTURE  ZESK_EXP_VENDORS OPTIONAL
*"      S_BUKRS STRUCTURE  VKKKIBUKRS OPTIONAL
*"      S_KTOKK STRUCTURE  KTOK_RANGE OPTIONAL
*"      S_LOEVMA STRUCTURE  ZESK_LOEVM_RANGE OPTIONAL
*"      S_LOEVMB STRUCTURE  ZESK_LOEVM_RANGE OPTIONAL
*"      S_SPERRA STRUCTURE  ZESK_SPERR_RANGE OPTIONAL
*"      S_SPERRB STRUCTURE  ZESK_SPERR_RANGE OPTIONAL
*"----------------------------------------------------------------------
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
*  Description         : Mapping Changes
*----------------------------------------------------------------------*
  DATA: tabla_vendors1 TYPE STANDARD TABLE OF zesk_exp_vendors.
  DATA: wa_tabla_vendors1 LIKE LINE OF  tabla_vendors1.
  DATA: wa_tabla_vendors LIKE LINE OF  tabla_vendors.

  TYPES: BEGIN OF ty_adr6,
           addrnumber TYPE adr6-addrnumber,
           smtp_addr  TYPE adr6-smtp_addr,
         END OF ty_adr6.

  TYPES: BEGIN OF ty_lfa1,
           lifnr TYPE lfa1-lifnr,
           pfach TYPE lfa1-pfach,
         END OF ty_lfa1.


  TYPES: BEGIN OF ty_lfb1,
           lifnr TYPE lfb1-lifnr,
           bukrs TYPE lfb1-bukrs,
           zterm TYPE lfb1-zterm,
         END OF ty_lfb1.

  TYPES: BEGIN OF ty_lfm1,
           lifnr TYPE lfm1-lifnr,
           waers TYPE lfm1-waers,
         END OF ty_lfm1.

  TYPES: BEGIN OF ty_wyt3,
           lifnr TYPE wyt3-lifnr,
           lifn2 TYPE wyt3-lifn2,
           parvw TYPE wyt3-parvw,
         END OF ty_wyt3.

  TYPES: BEGIN OF ty_lifnr,
           lifnr TYPE lfa1-lifnr,
         END OF ty_lifnr.


  DATA: tb_adr6  TYPE STANDARD TABLE OF ty_adr6,
        tb_lfa1  TYPE STANDARD TABLE OF ty_lfa1,
        tb_lfb1  TYPE STANDARD TABLE OF ty_lfb1,
        tb_lfm1  TYPE STANDARD TABLE OF ty_lfm1,
        tb_wyt3  TYPE STANDARD TABLE OF ty_wyt3,
        tb_lifnr TYPE STANDARD TABLE OF ty_lifnr,
        wa_adr6  TYPE ty_adr6,
        wa_lfa1  TYPE ty_lfa1,
        wa_lfb1  TYPE ty_lfb1,
        wa_lfm1  TYPE ty_lfm1,
        wa_wyt3  TYPE ty_wyt3,
        wa_lifnr TYPE ty_lifnr.

  CLEAR : v_error.

  SELECT lifnr bukrs zterm
     FROM lfb1
   INTO TABLE tb_lfb1
   WHERE  bukrs IN  s_bukrs AND
          loevm IN s_loevmb AND
          sperr IN s_sperrb.


  SELECT lifnr lifn2 parvw
     FROM wyt3
   INTO TABLE tb_wyt3
   FOR ALL ENTRIES IN tb_lfb1
*   WHERE lifnr EQ tb_lfb1-lifnr
    WHERE lifn2 EQ tb_lfb1-lifnr
*    AND parvw = 'PI'.
    AND parvw = 'RS'.

  IF NOT tb_lfb1[] IS INITIAL.
    LOOP AT tb_lfb1 INTO wa_lfb1.
      wa_lifnr-lifnr = wa_lfb1-lifnr.
      APPEND wa_lifnr TO tb_lifnr.
    ENDLOOP.

    IF NOT tb_wyt3[] IS INITIAL.

      LOOP AT tb_wyt3 INTO wa_wyt3.
*        wa_lifnr-lifnr = wa_wyt3-lifn2.

        wa_lifnr-lifnr = wa_wyt3-lifnr.
        APPEND wa_lifnr TO tb_lifnr.
      ENDLOOP.

    ENDIF.
  ENDIF.


*************************************************************************
* esta modificacion se hizo para Jeff para un cliente de Europa
*se graba en el campo Vatnumber el campo STCEG en lugar de STCD1
* se agrega en el select y luego comento el campo "stceg"

*  SELECT bukrs lifnr name1 stras ort01 pstlz regio land1

  " Begin Removal for S4DK907231
*  select lifnr name1 stras ort01 pstlz regio land1
*      telf1 telfx stcd1 adrnr pfach ktokk stceg kraus "stceg
*     name2                                                  "S4DK906802
*     telf2                                                  " INS S4DK907231
*    into corresponding fields of table  tabla_vendors1
*    from lfa1
*    for all entries in tb_lifnr
*    where lifnr eq tb_lifnr-lifnr and
*          ktokk in s_ktokk and
*          loevm in s_loevma and
*          sperr in s_sperra.
*  "loevm NE 'X'.
  " End Removal for S4DK907231

  " Begin Additional for S4DK907231 - Get LFA1-TLF2 and VAT Number from DFKKBPTAXNUM -- TAXNUM
  SELECT DISTINCT
         lifnr, name1, stras, ort01, pstlz, regio, land1, telf1, telfx,
         dfkkbptaxnum~taxnum AS stcd1,                   " CHNG S4DK907231
         adrnr, pfach, ktokk, stceg, kraus,
         name2,                                          " INS S4DK906802
         telf2                                           " INS S4DK907231
    INTO CORRESPONDING FIELDS OF TABLE  @tabla_vendors1
    FROM
      lfa1
    LEFT OUTER JOIN
      cvi_vend_link  ON lfa1~lifnr = cvi_vend_link~vendor
    LEFT OUTER JOIN
      but000         ON cvi_vend_link~partner_guid = but000~partner_guid
    LEFT OUTER JOIN
      dfkkbptaxnum   ON dfkkbptaxnum~partner = but000~partner
    FOR ALL ENTRIES IN @tb_lifnr
    WHERE lifnr EQ @tb_lifnr-lifnr AND
          ktokk IN @s_ktokk AND
          loevm IN @s_loevma AND
          sperr IN @s_sperra.
  "loevm NE 'X'.
  " End Additional for S4DK907231

*  SELECT bukrs lifnr name1 stras ort01 pstlz regio land1
*      telf1 telfx stcd1 adrnr
*    INTO CORRESPONDING FIELDS OF TABLE  tabla_vendors1
*    FROM zesk_vendors
*    WHERE bukrs IN s_bukrs.


*  SELECT lifnr bukrs zterm
*     FROM lfb1
*     INTO TABLE tb_lfb1
*     FOR ALL ENTRIES IN tabla_vendors1
*   WHERE lifnr EQ tabla_vendors1-lifnr
*    AND  bukrs EQ tabla_vendors1-bukrs.

*  SELECT lifnr pfach
*     FROM lfa1
*     INTO TABLE tb_lfa1
*     FOR ALL ENTRIES IN tabla_vendors1
*   WHERE lifnr EQ tabla_vendors1-lifnr.


  SELECT lifnr waers
     FROM lfm1
     INTO TABLE tb_lfm1
   FOR ALL ENTRIES IN tabla_vendors1
   WHERE lifnr EQ tabla_vendors1-lifnr.

  SELECT addrnumber smtp_addr
      FROM adr6
      INTO TABLE tb_adr6
    FOR ALL ENTRIES IN tabla_vendors1
    WHERE addrnumber EQ tabla_vendors1-adrnr
      AND persnumber EQ space                               "S4DK906802
      AND flgdefault EQ abap_true.                          "S4DK906802

* Begin Addition for S4DK906802
  SELECT addrnumber, po_box_loc, po_box_reg, post_code2,
         str_suppl1                                         " INS S4DK907231
    FROM adrc
    INTO TABLE @DATA(lt_adrc)
     FOR ALL ENTRIES IN @tabla_vendors1
   WHERE addrnumber  EQ @tabla_vendors1-adrnr.

  SORT lt_adrc BY addrnumber.
* End   Addition for S4DK906802

  IF NOT  tabla_vendors1[] IS INITIAL.
    LOOP AT tabla_vendors1 INTO wa_tabla_vendors1.

*      wa_tabla_vendors-bukrs = wa_tabla_vendors1-bukrs.

      wa_tabla_vendors-lifnr = wa_tabla_vendors1-lifnr.

      IF p_chkbox = ''.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_tabla_vendors-lifnr
          IMPORTING
            output = wa_tabla_vendors-lifnr.
      ENDIF.


      wa_tabla_vendors-name1 = wa_tabla_vendors1-name1.
      wa_tabla_vendors-stras = wa_tabla_vendors1-stras.
      wa_tabla_vendors-ort01 = wa_tabla_vendors1-ort01.
      wa_tabla_vendors-pstlz = wa_tabla_vendors1-pstlz.
      wa_tabla_vendors-regio = wa_tabla_vendors1-regio.
      wa_tabla_vendors-land1 = wa_tabla_vendors1-land1.
      wa_tabla_vendors-telf1 = wa_tabla_vendors1-telf1.
      wa_tabla_vendors-telfx = wa_tabla_vendors1-telfx.
      wa_tabla_vendors-telf2 = wa_tabla_vendors1-telf2.  " INS S4DK907231
      wa_tabla_vendors-stcd1 = wa_tabla_vendors1-stcd1.
      wa_tabla_vendors-kraus = wa_tabla_vendors1-kraus.
* esta modificacion se hizo para Jeff para un cliente de Europa
*se graba en el campo Vatnumber el campo STCEG en lugar de STCD1
*      wa_tabla_vendors-stceg = wa_tabla_vendors1-stceg.

      IF p_stcd1 = 'X'.
        CLEAR: wa_tabla_vendors-stcd1.
      ENDIF.

      wa_tabla_vendors-adrnr = wa_tabla_vendors1-adrnr.

      READ TABLE tb_adr6 INTO wa_adr6
      WITH KEY addrnumber = wa_tabla_vendors1-adrnr.

*      READ TABLE tb_lfa1 INTO wa_lfa1
*      WITH KEY lifnr = wa_tabla_vendors1-lifnr.

      READ TABLE tb_lfm1 INTO wa_lfm1
      WITH KEY lifnr = wa_tabla_vendors1-lifnr.

      wa_tabla_vendors-smtp_addr = wa_adr6-smtp_addr.
      wa_tabla_vendors-waers     = wa_lfm1-waers.
*      wa_tabla_vendors-pfach     = wa_lfa1-pfach.

      wa_tabla_vendors-pfach     = wa_tabla_vendors1-pfach.

* Begin Addition for S4DK906802
      wa_tabla_vendors-name2 = wa_tabla_vendors1-name2.

      READ TABLE lt_adrc ASSIGNING FIELD-SYMBOL(<ls_adrc>) WITH KEY addrnumber = wa_tabla_vendors1-adrnr
                                                           BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        wa_tabla_vendors-po_box_loc = <ls_adrc>-po_box_loc.
        wa_tabla_vendors-po_box_reg = <ls_adrc>-po_box_reg.
        wa_tabla_vendors-post_code2 = <ls_adrc>-post_code2.
        wa_tabla_vendors-str_suppl1 = <ls_adrc>-str_suppl1. " INS S4DK907231

      ENDIF.
* End   Addition for S4DK906802


      LOOP AT tb_lfb1 INTO wa_lfb1
      WHERE lifnr EQ wa_tabla_vendors1-lifnr.
*      READ TABLE tb_lfb1 INTO wa_lfb1
*      WITH KEY lifnr = wa_tabla_vendors1-lifnr
*               bukrs = wa_tabla_vendors1-bukrs.
*                bukrs = s_bukrs.
        wa_tabla_vendors-bukrs     = wa_lfb1-bukrs.
        wa_tabla_vendors-zterm     = wa_lfb1-zterm.
        APPEND wa_tabla_vendors TO tabla_vendors.
      ENDLOOP.

      READ TABLE tb_lfb1 INTO wa_lfb1
      WITH KEY lifnr = wa_tabla_vendors1-lifnr.

      IF sy-subrc NE 0.
        APPEND wa_tabla_vendors TO tabla_vendors.
      ENDIF.

      CLEAR : wa_tabla_vendors , wa_adr6  , wa_lfa1, wa_lfb1  , wa_lfm1.
    ENDLOOP.

    DELETE tabla_vendors WHERE bukrs = ''.

  ELSE.
    v_error = 'X'.
  ENDIF.






ENDFUNCTION.
