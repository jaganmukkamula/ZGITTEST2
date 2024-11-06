FUNCTION zesk_export_purcharse_order.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(P_CHKBOX) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(V_ERROR) TYPE  CHAR1
*"  TABLES
*"      TABLA_PURCHARSE_ORDER_H STRUCTURE  ZESK_EXP_PURCHARSE_ORDER_H
*"      TABLA_PURCHARSE_ORDER_I STRUCTURE  ZESK_EXP_PURCHARSE_ORDER_I
*"      TABLA_GOODSRECEIPT_I STRUCTURE  ZESK_EXP_GOODSRECEIPT_I
*"      S_BUKRS STRUCTURE  VKKKIBUKRS
*"      S_EBELN STRUCTURE  ZESK_EBELN
*"      S_FECHA STRUCTURE  ZESK_AEDAT
*"      S_VENDOR STRUCTURE  LIF_RANGE
*"----------------------------------------------------------------------
 DATA: wa_tabla_purcharse_order_h LIKE LINE OF tabla_purcharse_order_h.

 DATA: wa_tabla_purcharse_order_i LIKE LINE OF tabla_purcharse_order_i.

  DATA: wa_tabla_goodsreceipt_i LIKE LINE OF  tabla_goodsreceipt_i.

  DATA: tb_ekko TYPE STANDARD TABLE OF ekko,
        tb_ekpo TYPE STANDARD TABLE OF ekpo,
        tb_ekbe TYPE STANDARD TABLE OF ekbe,
        tb_ekkn TYPE STANDARD TABLE OF ekkn,
        tb_t024 TYPE STANDARD TABLE OF t024,
        wa_t024 TYPE t024,
        wa_ekko TYPE ekko,
        wa_ekpo TYPE ekpo,
        wa_ekbe TYPE ekbe,
        wa_ekkn TYPE ekkn.


  TABLES : ekpo , ekkn.

  DATA: v_ord_amount TYPE ekpo-netwr,
        v_del_amount TYPE ekbe-wrbtr,
        v_del_amount_h TYPE ekbe-wrbtr,
        v_inv_amount TYPE ekbe-wrbtr,
        v_inv_amount_h TYPE ekbe-wrbtr,
        v_del_quantity TYPE ekbe-menge,
        v_inv_quantity TYPE ekbe-menge,
        v_index      TYPE sy-tabix,
        v_index1     TYPE sy-tabix,
        v_index2     TYPE sy-tabix,
        v_ekbe-budat TYPE char10,
        v_ekko-aedat TYPE char10.

  CLEAR : v_error , v_ord_amount , v_del_amount ,v_inv_amount.

  SELECT *
    INTO TABLE tb_ekko
    FROM ekko
    WHERE bukrs IN s_bukrs  AND
          ebeln IN s_ebeln  AND
          aedat IN s_fecha  AND
          lifnr IN s_vendor.

  SELECT *
    FROM ekpo
    INTO TABLE tb_ekpo
    FOR ALL ENTRIES IN tb_ekko
    WHERE ebeln EQ tb_ekko-ebeln.

  SELECT *
    FROM ekbe
    INTO TABLE tb_ekbe
    FOR ALL ENTRIES IN tb_ekko
    WHERE ebeln EQ  tb_ekko-ebeln.

  SELECT *
    FROM ekkn
    INTO TABLE tb_ekkn
    FOR ALL ENTRIES IN tb_ekko
    WHERE ebeln EQ  tb_ekko-ebeln.

    SORT tb_ekkn BY ebeln ebelp ASCENDING ZEKKN DESCENDING.

  SELECT *
    FROM t024
    INTO TABLE tb_t024.


  IF NOT tb_ekko[] IS INITIAL.
    LOOP AT tb_ekko INTO wa_ekko.
      v_index = sy-tabix.
      wa_tabla_purcharse_order_h-bukrs = wa_ekko-bukrs.

      READ TABLE tb_t024 into wa_t024
      WITH KEY ekgrp = wa_ekko-ekgrp.

      wa_tabla_purcharse_order_h-buyer = wa_t024-eknam.

      wa_tabla_purcharse_order_h-lifnr = wa_ekko-lifnr.

      IF p_chkbox = ''.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
           input =  wa_tabla_purcharse_order_h-lifnr
          IMPORTING
           output = wa_tabla_purcharse_order_h-lifnr.
      ENDIF.

      wa_tabla_purcharse_order_h-ebeln = wa_ekko-ebeln.
      CONCATENATE wa_ekko-aedat(4) '-' wa_ekko-aedat+4(2) '-'
                  wa_ekko-aedat+6(2)  INTO v_ekko-aedat.
      wa_tabla_purcharse_order_h-aedat = v_ekko-aedat.

      wa_tabla_purcharse_order_h-func_inter = wa_ekko-lifre.

      IF p_chkbox = ''.
         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
            input =  wa_tabla_purcharse_order_h-func_inter
           IMPORTING
            output = wa_tabla_purcharse_order_h-func_inter.
      ENDIF.


      READ TABLE tb_ekkn INTO wa_ekkn
      WITH KEY ebeln = wa_ekko-ebeln.

      IF sy-subrc = 0.
            wa_tabla_purcharse_order_h-internal_order = wa_ekkn-aufnr.

            IF p_chkbox = ''.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                 EXPORTING
                   input =  wa_tabla_purcharse_order_h-internal_order
                 IMPORTING
                   output = wa_tabla_purcharse_order_h-internal_order.
            ENDIF.
      ENDIF.



      APPEND wa_tabla_purcharse_order_h TO tabla_purcharse_order_h.

      LOOP AT tb_ekpo INTO wa_ekpo
         WHERE ebeln EQ wa_ekko-ebeln.
        v_index1 = sy-tabix.

        READ TABLE tb_ekkn INTO wa_ekkn
          WITH KEY ebeln = wa_ekpo-ebeln
                   ebelp = wa_ekpo-ebelp.

        IF sy-subrc = 0.
             wa_tabla_purcharse_order_i-sakto = wa_ekkn-sakto.

            IF p_chkbox = ''.
               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                 EXPORTING
                  input =  wa_tabla_purcharse_order_i-sakto
                 IMPORTING
                  output = wa_tabla_purcharse_order_i-sakto.
            ENDIF.

            IF wa_ekkn-zekkn = '1'.
                wa_tabla_purcharse_order_i-kostl = wa_ekkn-kostl.

                IF p_chkbox = ''.
                   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                     EXPORTING
                      input =  wa_tabla_purcharse_order_i-kostl
                     IMPORTING
                      output = wa_tabla_purcharse_order_i-kostl.
                ENDIF.
            ELSEIF wa_ekkn-zekkn > '1'.
                wa_tabla_purcharse_order_i-kostl = 'MULTIPLE'.
            ENDIF.

        ENDIF.

        wa_tabla_purcharse_order_i-bukrs = wa_ekpo-bukrs.

        wa_tabla_purcharse_order_i-lifnr = wa_ekko-lifnr.

        IF p_chkbox = ''.
           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
              input =  wa_tabla_purcharse_order_i-lifnr
             IMPORTING
              output = wa_tabla_purcharse_order_i-lifnr.
        ENDIF.


        wa_tabla_purcharse_order_i-ebeln = wa_ekpo-ebeln.

        wa_tabla_purcharse_order_i-ebelp = wa_ekpo-ebelp.

        IF p_chkbox = ''.
           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
              input =  wa_tabla_purcharse_order_i-ebelp
             IMPORTING
              output = wa_tabla_purcharse_order_i-ebelp.
        ENDIF.

        wa_tabla_purcharse_order_i-aedat = v_ekko-aedat.

        wa_tabla_purcharse_order_i-matnr = wa_ekpo-matnr.

        IF p_chkbox = ''.
           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
              input =  wa_tabla_purcharse_order_i-matnr
             IMPORTING
              output = wa_tabla_purcharse_order_i-matnr.
        ENDIF.

        wa_tabla_purcharse_order_i-txz01 = wa_ekpo-txz01.



        IF wa_ekpo-menge = 0.
           wa_ekpo-menge = 1.
        ENDIF.
        wa_tabla_purcharse_order_i-unit_price = wa_ekpo-netwr /
        wa_ekpo-menge.
        wa_tabla_purcharse_order_i-netwr = wa_ekpo-netwr.
        wa_tabla_purcharse_order_i-menge = wa_ekpo-menge.
        wa_tabla_purcharse_order_i-mwskz = wa_ekpo-mwskz.
*        wa_tabla_purcharse_order_i-gr_iv = wa_ekpo-webre.
*        IF wa_ekpo-webre = 'X' OR wa_ekpo-wepos = 'X'.
        IF wa_ekpo-webre = 'X'.
             wa_tabla_purcharse_order_i-gr_iv = 'X'.
        ENDIF.
        v_ord_amount = v_ord_amount + wa_ekpo-netwr.

        APPEND wa_tabla_purcharse_order_i TO tabla_purcharse_order_i.


        LOOP AT tb_ekbe INTO wa_ekbe
            WHERE ebeln EQ wa_ekpo-ebeln AND
                  ebelp EQ wa_ekpo-ebelp.

          IF wa_ekbe-vgabe = '1'.
            wa_tabla_goodsreceipt_i-bukrs         = wa_ekpo-bukrs.

            wa_tabla_goodsreceipt_i-goods_receipt = wa_ekbe-belnr.

            IF p_chkbox = ''.
               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                 EXPORTING
                  input =  wa_tabla_goodsreceipt_i-goods_receipt
                 IMPORTING
                  output = wa_tabla_goodsreceipt_i-goods_receipt.
            ENDIF.

*            wa_tabla_goodsreceipt_i-del_note      = wa_ekbe-belnr. "??
            wa_tabla_goodsreceipt_i-ebeln         = wa_ekbe-ebeln.


            wa_tabla_goodsreceipt_i-ebelp         = wa_ekbe-ebelp.

            IF p_chkbox = ''.
               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                 EXPORTING
                  input =  wa_tabla_goodsreceipt_i-ebelp
                 IMPORTING
                  output = wa_tabla_goodsreceipt_i-ebelp.
            ENDIF.

            CONCATENATE wa_ekbe-budat(4) '-' wa_ekbe-budat+4(2) '-'
                        wa_ekbe-budat+6(2)  INTO v_ekbe-budat.
            wa_tabla_goodsreceipt_i-del_date      = v_ekbe-budat.
            IF wa_ekbe-elikz = 'X'.
              wa_tabla_goodsreceipt_i-del_completed = 'TRUE'.
            ELSEIF wa_ekbe-elikz = ''.
              wa_tabla_goodsreceipt_i-del_completed = 'FALSE'.
            ENDIF.

            IF wa_ekbe-shkzg = 'S'.
              wa_tabla_goodsreceipt_i-quantity      = wa_ekbe-menge.
              wa_tabla_goodsreceipt_i-amount        = wa_ekbe-wrbtr.
*              wa_tabla_goodsreceipt_i-inv_amount    = wa_ekbe-wrbtr.
*              wa_tabla_goodsreceipt_i-inv_quantity  = wa_ekbe-menge.
            ELSEIF wa_ekbe-shkzg = 'H'.
              wa_tabla_goodsreceipt_i-quantity    = wa_ekbe-menge * -1.
              wa_tabla_goodsreceipt_i-amount      = wa_ekbe-wrbtr * -1.
*              wa_tabla_goodsreceipt_i-inv_amount  = wa_ekbe-wrbtr * -1.
*              wa_tabla_goodsreceipt_i-inv_quantity = wa_ekbe-menge * -1.
            ENDIF.

            APPEND wa_tabla_goodsreceipt_i TO tabla_goodsreceipt_i.

          ENDIF.

*        IF wa_ekbe-bewtp = 'E'.
          IF wa_ekbe-vgabe = '1'.
            IF  wa_ekbe-shkzg = 'S'.
              v_del_amount = v_del_amount + wa_ekbe-wrbtr.
              v_del_quantity = v_del_quantity + wa_ekbe-menge.
            ELSEIF  wa_ekbe-shkzg = 'H'.
              v_del_amount = v_del_amount - wa_ekbe-wrbtr.
              v_del_quantity = v_del_quantity - wa_ekbe-menge.
            ENDIF.
*        ELSEIF wa_ekbe-bewtp = 'Q'.
          ELSEIF wa_ekbe-vgabe = '2' OR wa_ekbe-vgabe = '3' OR
                 wa_ekbe-vgabe = '4'.
            IF  wa_ekbe-shkzg = 'S'.
              v_inv_amount = v_inv_amount + wa_ekbe-wrbtr.
              v_inv_quantity = v_inv_quantity + wa_ekbe-menge.
            ELSEIF wa_ekbe-shkzg = 'H'.
              v_inv_amount = v_inv_amount - wa_ekbe-wrbtr.
              v_inv_quantity = v_inv_quantity - wa_ekbe-menge.
            ENDIF.
          ENDIF.


        ENDLOOP.

*Modifico Items
        wa_tabla_purcharse_order_i-del_amount    = v_del_amount.
        wa_tabla_purcharse_order_i-del_quantity  = v_del_quantity.
        wa_tabla_purcharse_order_i-inv_amount    = v_inv_amount.
        wa_tabla_purcharse_order_i-inv_quantity  = v_inv_quantity.

        MODIFY tabla_purcharse_order_i INDEX v_index1
                FROM wa_tabla_purcharse_order_i
                TRANSPORTING del_amount del_quantity
                inv_amount inv_quantity.

        v_del_amount_h = v_del_amount_h + v_del_amount.
        v_inv_amount_h =  v_inv_amount_h  + v_inv_amount.

        IF p_chkbox = ''.
               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                 EXPORTING
                  input =  wa_ekpo-ebelp
                 IMPORTING
                  output = wa_ekpo-ebelp.
        ENDIF.


        LOOP AT tabla_goodsreceipt_i INTO wa_tabla_goodsreceipt_i
            WHERE ebeln EQ wa_ekpo-ebeln AND
                  ebelp EQ wa_ekpo-ebelp.
              v_index2 = sy-tabix.
              wa_tabla_goodsreceipt_i-inv_amount   = v_inv_amount.
              wa_tabla_goodsreceipt_i-inv_quantity = v_inv_quantity.
              MODIFY tabla_goodsreceipt_i INDEX v_index2
              FROM wa_tabla_goodsreceipt_i.

        ENDLOOP.

        CLEAR : v_del_quantity ,  v_inv_quantity ,
                v_del_amount , v_inv_amount.
        CLEAR : wa_tabla_purcharse_order_i.


      ENDLOOP.

*Modifico header
      wa_tabla_purcharse_order_h-po_amount = v_ord_amount.
      wa_tabla_purcharse_order_h-del_amount = v_del_amount_h.
      wa_tabla_purcharse_order_h-inv_amount = v_inv_amount_h.

      IF v_ord_amount GE v_inv_amount_h.
            wa_tabla_purcharse_order_h-orden_cerrada = ' '.
      ELSE.
            wa_tabla_purcharse_order_h-orden_cerrada = 'X'.
      ENDIF.

      MODIFY tabla_purcharse_order_h INDEX v_index
             FROM wa_tabla_purcharse_order_h
           TRANSPORTING po_amount del_amount inv_amount orden_cerrada.

      CLEAR :  v_ord_amount , v_del_amount , v_inv_amount ,
               v_del_quantity , v_inv_quantity , v_del_amount_h ,
               v_inv_amount_h.
      CLEAR : wa_tabla_purcharse_order_h.

    ENDLOOP.


    SORT tabla_purcharse_order_h BY ebeln.
    LOOP AT tabla_purcharse_order_h  INTO wa_tabla_purcharse_order_h
                WHERE orden_cerrada = 'X'.

      DELETE tabla_purcharse_order_i
                      WHERE ebeln = wa_tabla_purcharse_order_h-ebeln.
      DELETE tabla_goodsreceipt_i
                      WHERE ebeln = wa_tabla_purcharse_order_h-ebeln.

    ENDLOOP.

    SORT tabla_purcharse_order_h BY ebeln.
    DELETE tabla_purcharse_order_h WHERE orden_cerrada = 'X'.


    IF NOT tabla_purcharse_order_h[] IS INITIAL
      AND NOT tabla_purcharse_order_i[] IS INITIAL.
    ELSE.
      v_error = 'X'.
    ENDIF.

  ELSE.
    v_error = 'X'.
  ENDIF.

ENDFUNCTION.
