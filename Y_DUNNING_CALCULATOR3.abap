*&---------------------------------------------------------------------*
*& Report  Y_DUNNING_CALCULATOR
*&
*&---------------------------------------------------------------------*
*&  Created by GDIMALIWAT 03/12/2013
*&
*&---------------------------------------------------------------------*

REPORT  y_dunning_calculator.


TYPES:  BEGIN OF ty_oc,
          oc       TYPE eabl-ablhinw,
          adatsoll TYPE eabl-adatsoll,
          mrid     TYPE eabl-ablbelnr,
        END OF ty_oc,

        BEGIN OF ty_pua_overdue,
          opbel TYPE dfkkop-opbel,
          faedn TYPE dfkkop-faedn,
          budat TYPE dfkkop-budat,
          betrh TYPE dfkkop-betrh,
        END OF ty_pua_overdue,

        BEGIN OF ty_opbel_only,
          opbel TYPE dfkkop-opbel,
        END OF ty_opbel_only,

        BEGIN OF ty_dunn,
        docno      TYPE char20,
        ficadoc    TYPE dfkkop-opbel,
        budat      TYPE erdk-budat,
        bldat      TYPE erdk-bldat,
        faedn      TYPE dfkkop-faedn,
        "amount     TYPE char15,
        amount     TYPE dfkkop-betrh,
      END OF ty_dunn,

      BEGIN OF ty_dunn2,
        docno      TYPE char20,
        ficadoc    TYPE dfkkop-opbel,
        budat      TYPE erdk-budat,
        bldat      TYPE erdk-bldat,
        faedn      TYPE dfkkop-faedn,
        amount     TYPE dfkkop-betrh,
        sperz      TYPE dfkkop-sperz,
      END OF ty_dunn2,

       BEGIN OF ty_probill,
        loobj1     TYPE dfkklocks-loobj1,
        lockr      TYPE dfkklocks-lockr,
        vkont      TYPE dfkkop-vkont,
      END OF ty_probill.


DATA:  gt_oc          TYPE TABLE OF ty_oc,
      gt_dunn        TYPE TABLE OF ty_dunn,
      gt_dunn2       TYPE TABLE OF ty_dunn2,
      gt_probill     TYPE TABLE OF ty_probill.

DATA : gv_bp   TYPE bu_partner,
       gv_can  TYPE vkont_kk,
       gv_ba   TYPE fkkvkp-gsber.

DATA: BEGIN OF address,
        roomnumber TYPE adrc-roomnumber,
        floor      TYPE adrc-floor,
        house_num1 TYPE adrc-house_num1,
        house_num2 TYPE adrc-house_num2,
        str_suppl1 TYPE adrc-str_suppl1,
        street     TYPE adrc-street,
        str_suppl2 TYPE adrc-str_suppl1,
        str_suppl3 TYPE adrc-str_suppl3,
        location   TYPE adrc-location,
        city2      TYPE adrc-city2,
        city1      TYPE adrc-city1,
        post_code1 TYPE adrc-post_code1,
      END OF address.

PARAMETERS : pa_can   TYPE vkont_kk,
             pa_bp    TYPE bu_partner,
             pa_ddate TYPE dfkkop-faedn,
             pa_rept  AS CHECKBOX,
             pa_file  TYPE rlgrap-filename DEFAULT '\\saprep01.mayniladsap.com\repuserdata\Glenn\ZDUNNLIST\Output.txt' OBLIGATORY.

FIELD-SYMBOLS:  <fs_oc>           TYPE ty_oc,
                <fs_pua_overdue>  TYPE ty_pua_overdue,
                <fs_current>      TYPE ty_pua_overdue,
                <fs_installation> TYPE ty_opbel_only,
                <fs_misc>  TYPE ty_pua_overdue,
                <fs_dunn>         TYPE ty_dunn,
                <fs_dunn2>        TYPE ty_dunn2,
                <fs_probill>      TYPE ty_probill.

CONSTANTS : co_nextline TYPE c VALUE cl_abap_char_utilities=>newline,
            co_green  TYPE icon_d     VALUE '@08@',
            co_yellow TYPE icon_d     VALUE '@09@',
            co_red    TYPE icon_d     VALUE '@0A@',
            co_misc   TYPE icon_d     VALUE '@0J@',
            co_separator   TYPE c     VALUE '|'.

START-OF-SELECTION.

  DATA : lv_balance TYPE betrh_kk,
         lv_balance_str TYPE string,
         lv_pua     TYPE char15,
         lv_current TYPE char15,
         lv_overdue TYPE char15,
         lv_misc    TYPE char15,
         lv_test_total     TYPE char15,
         lv_ableinh        TYPE eablg-ableinh,
         lv_eqpt           TYPE egerh-equnr,
         lv_anlage         TYPE ever-anlage,
         lv_vstelle        TYPE evbs-vstelle,
         lv_premise_param  TYPE sy-datum,
         lv_haus           TYPE evbs-haus,
         lv_rsg            TYPE char8,
         lv_name           TYPE fkkvk-vkbez,
        lv_addr           TYPE char80,
        lv_sortfld        TYPE char6,
        lv_device         TYPE eabl-gernr,
        lv_printdoc       TYPE c LENGTH 20,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw,
        lv_adatsoll       TYPE eabl-adatsoll,
        lv_mrid           TYPE eabl-ablbelnr,
        lv_posdt          TYPE erdk-budat,
        lv_diff           TYPE p,
        lv_diff_str       TYPE string,
        lv_current_date   TYPE sy-datum,
        lv_mindt          TYPE sy-datum,
        lv_maxdt          TYPE sy-datum,
        lv_string         TYPE string,
        lv_flag           TYPE char1.


  IF pa_bp IS INITIAL AND pa_can IS INITIAL.
    WRITE : / 'Please enter BP or CAN.'.
    sy-subrc = 4.
  ELSEIF pa_ddate IS INITIAL.
    WRITE : / 'Please enter Due Date.'.
    sy-subrc = 4.
  ELSEIF pa_bp IS INITIAL.
    gv_can = pa_can.
    SELECT SINGLE gpart gsber
    INTO (gv_bp, gv_ba)
    FROM fkkvkp
    WHERE vkont EQ pa_can.
  ELSEIF pa_can IS INITIAL.
    gv_bp = pa_bp.
    SELECT SINGLE vkont gsber
    INTO (gv_can, gv_ba)
    FROM fkkvkp
    WHERE gpart EQ pa_bp.
  ENDIF.

  IF sy-subrc EQ 0.
    WRITE:/ 'BP: '           , gv_bp.
    WRITE:/ 'CAN: '          , gv_can.
    WRITE:/ 'DUE DATE: '     , pa_ddate.
    WRITE:/ 'Business Area: ', gv_ba.
    WRITE:/ '--------------------------------------------------'.

    PERFORM get_due_balance_by_duedate USING gv_can
                                             gv_bp
                                       CHANGING lv_balance
                                                lv_pua
                                                lv_current
                                                lv_overdue
                                                lv_misc.
    WRITE:/ '--------------------------------------------------'.
    WRITE:/ 'Overdue: ', lv_overdue.
    WRITE:/ 'PUA: '    , lv_pua.
    WRITE:/ 'Current: ', lv_current.
    WRITE:/ 'Misc:'    , lv_misc.
    WRITE:/ 'Balance: ', lv_balance.

    lv_test_total = lv_overdue + lv_current + lv_pua + lv_misc.
    WRITE:/ 'Overdue + PUA + Current + Misc= ', lv_test_total.

    IF pa_rept EQ 'X'.
      lv_current_date = sy-datum.
*      lv_mindt = pa_ddate.
*      lv_diff = lv_current_date - lv_mindt.
*      lv_premise_param = pa_ddate - 7.


      "-- DUNN List
      SELECT xblnr opbel budat bldat faedn SUM( betrh ) AS amount
        INTO TABLE gt_dunn
        FROM dfkkop
       WHERE vkont EQ gv_can
         AND hvorg IN ('0100', '0200')
         AND augrd NE '05'
         AND augst EQ space
         AND faedn LE pa_ddate
         AND budat NE '00000000'
         AND abwbl EQ space
       GROUP BY xblnr budat bldat opbel faedn.

      lv_diff = 0.
      lv_mindt = '00000000'.
      lv_maxdt = '00000000'.

      SORT gt_dunn BY faedn ASCENDING.
      LOOP AT gt_dunn ASSIGNING <fs_dunn>.
        lv_flag = '0'.
        LOOP AT gt_probill ASSIGNING <fs_probill> WHERE loobj1+0(12) EQ <fs_dunn>-ficadoc.
          lv_flag = '1'.
        ENDLOOP.
        UNASSIGN <fs_probill>.

        IF lv_flag NE '1'.
          APPEND INITIAL LINE TO gt_dunn2 ASSIGNING <fs_dunn2>.
          <fs_dunn2>-docno    =  <fs_dunn>-docno.
          <fs_dunn2>-budat    =  <fs_dunn>-budat.
          <fs_dunn2>-bldat    =  <fs_dunn>-bldat.
          <fs_dunn2>-ficadoc  =  <fs_dunn>-ficadoc.
          <fs_dunn2>-faedn    =  <fs_dunn>-faedn.
          <fs_dunn2>-amount   =  <fs_dunn>-amount.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_dunn>.

      SORT gt_dunn2 BY faedn ASCENDING.

      LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
        lv_mindt = <fs_dunn2>-faedn.
        EXIT.
      ENDLOOP.
      UNASSIGN <fs_dunn2>.

      LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
        IF <fs_dunn2>-amount NE 0.
          lv_printdoc = <fs_dunn2>-docno.
          lv_posdt    = <fs_dunn2>-budat.
          lv_maxdt    = <fs_dunn2>-faedn.
        ELSE.
          lv_current = 0.
          lv_printdoc = space.
          lv_posdt = space.
        ENDIF.
        EXIT.
      ENDLOOP.
      UNASSIGN <fs_dunn2>.

      IF lv_mindt EQ '00000000'.
        IF lv_maxdt EQ '00000000'.
          lv_diff = 0.
        ELSE.
          lv_diff = lv_current_date - lv_maxdt.
        ENDIF.
      ELSE.
        lv_diff = lv_current_date - lv_mindt.
      ENDIF.


      lv_premise_param = lv_maxdt - 7.

****( Get MRU )**************************************************************
      SELECT SINGLE anlage
        INTO (lv_anlage)
        FROM ever
       WHERE vkonto EQ gv_can.

****( Get Installation Number )**********************************************
      SELECT SINGLE ableinh
        INTO (lv_ableinh)
        FROM eanlh
       WHERE anlage EQ lv_anlage
         AND bis = '99991231'.

      SELECT ablhinw eablg~adatsoll eablg~ablbelnr
      INTO TABLE gt_oc
      FROM eablg
      INNER JOIN eabl ON eablg~ablbelnr = eabl~ablbelnr
      UP TO 1 ROWS
      WHERE anlage = lv_anlage
      AND ablesgr = '01'
      AND ablstat NE '0'
      ORDER BY eablg~adatsoll DESCENDING.

      READ TABLE gt_oc INDEX 1 ASSIGNING <fs_oc>.
      IF sy-subrc EQ 0.
        lv_oc = <fs_oc>-oc.
        lv_adatsoll = <fs_oc>-adatsoll.
        lv_mrid = <fs_oc>-mrid.
      ENDIF.

      SELECT SINGLE meterreadingnot2
      INTO lv_oc2
      FROM zmwosb_s01p
      WHERE mridnumber = lv_mrid.
****( Get Equipment Number )**************************************************
      SELECT SINGLE equnr
        INTO (lv_eqpt)
        FROM egerh INNER JOIN eastl ON egerh~logiknr = eastl~logiknr
       WHERE eastl~anlage = lv_anlage
         AND egerh~bis = '99991231'.

****( Get Premise Number )****************************************************
      SELECT SINGLE vstelle
        INTO (lv_vstelle)
        FROM eanlh INNER JOIN eanl ON eanlh~anlage = eanl~anlage
       WHERE eanlh~anlage = lv_anlage
         AND eanlh~ab LE lv_premise_param
         AND eanlh~bis GE lv_premise_param.

****( Get Connection Object )******************************************************
      SELECT SINGLE haus
        INTO (lv_haus)
        FROM evbs
       WHERE vstelle = lv_vstelle.

****( Get RSG )********************************************************************
      SELECT SINGLE regiogroup
        INTO lv_rsg
        FROM ehauisu
       WHERE haus = lv_haus.

      PERFORM name_address  CHANGING lv_name lv_addr.
      PERFORM get_sortfield CHANGING lv_sortfld lv_device lv_eqpt.
      SHIFT lv_printdoc LEFT DELETING LEADING '0'.

*      WRITE:/ gv_ba.
*      WRITE:/ lv_rsg.
*      WRITE:/ lv_ableinh.
*      WRITE:/ gv_bp.
*      WRITE:/ gv_can.
*      WRITE:/ lv_printdoc.
*      WRITE:/ lv_name.
*      WRITE:/ lv_addr.
*      WRITE:/ lv_posdt.
*      WRITE:/ lv_current.
*      WRITE:/ lv_pua.
*      WRITE:/ lv_overdue.
*      WRITE:/ lv_balance.
*      WRITE:/ lv_sortfld.
*      WRITE:/ lv_device.
*      WRITE:/ lv_diff.
*      WRITE:/ lv_oc.
*      WRITE:/ lv_oc2.

      OPEN DATASET pa_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.

        CONCATENATE 'BA' 'RSG' 'MRU' 'BP' 'CAN' 'PRINTDOC' 'NAME' 'ADDRESS' 'POST DT' 'CURRENT' 'PUA' '61+_OVERDUE' 'TOTAL' 'SEQ NO' 'DEVICE' 'DAYS' 'OC' 'OC2' INTO lv_string SEPARATED BY co_separator.
        TRANSFER lv_string TO pa_file.

        lv_balance_str = lv_balance.
        lv_diff_str    = lv_diff.
        CONCATENATE gv_ba lv_rsg lv_ableinh gv_bp gv_can lv_printdoc lv_name lv_addr lv_posdt lv_current lv_pua lv_overdue lv_balance_str lv_sortfld lv_device lv_diff_str lv_oc lv_oc2 INTO lv_string SEPARATED BY co_separator.
        TRANSFER lv_string TO pa_file.
        CLOSE DATASET pa_file.
      ELSE.
        WRITE:/ 'Report file could not be opened.'.
      ENDIF.
    ENDIF.

  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DUE_BALANCE_BY_DUEDATE
*&---------------------------------------------------------------------*
*       Subroutine for Fetching Due Balance of an Account
*----------------------------------------------------------------------*
FORM get_due_balance_by_duedate  USING    pv_can     TYPE vkont_kk
                                          pv_bp      TYPE bu_partner
                                 CHANGING pv_balance TYPE betrh_kk
                                          pv_pua     TYPE char15
                                          pv_current TYPE char15
                                          pv_overdue TYPE char15
                                          pv_misc    TYPE char15.

  DATA : lt_selection        TYPE TABLE OF bapifkkopselhead,
         lt_balances         TYPE TABLE OF bapifkkepos,
         lv_payment          TYPE char15,
         lv_days60           TYPE dfkkop-faedn,
         lv_days25           TYPE dfkkop-faedn,
         lv_overdue          TYPE dfkkop-betrh,
         lv_overdue_cleared  TYPE dfkkop-betrh,
         lv_overdue_pia      TYPE dfkkop-betrh,
         lv_pua              TYPE dfkkop-betrh,
         lv_pua_cleared      TYPE dfkkop-betrh,
         lv_pua_pia          TYPE dfkkop-betrh,
         lv_current          TYPE dfkkop-betrh,
         lv_current_cleared  TYPE dfkkop-betrh,
         lv_current_pia      TYPE dfkkop-betrh,
         lv_installation_overdue    TYPE dfkkop-betrh,
         lv_installation_pua        TYPE dfkkop-betrh,
         lv_installation_current    TYPE dfkkop-betrh,
         lv_misc            TYPE dfkkop-betrh,
         lv_misc1           TYPE dfkkop-betrh,
         lv_misc1_not_yet_due TYPE dfkkop-betrh,
         lv_misc_cleared    TYPE dfkkop-betrh,
         ls_rfkn1            TYPE rfkn1,
         lv_check_cleared_e0  TYPE dfkkop-opbel.

  DATA : lt_pua_overdue TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_current     TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_installation TYPE TABLE OF ty_opbel_only,
         lt_misc         TYPE TABLE OF ty_pua_overdue,
         lt_sfkkop      TYPE TABLE OF sfkkop.

  FIELD-SYMBOLS :
        <fs_selection> TYPE bapifkkopselhead,
        <fs_balances>  TYPE bapifkkepos,
        <fs_sfkkop>    TYPE sfkkop.

  "--Reinitialize Variables
  CLEAR : pv_balance,
          pv_pua,
          pv_current,
          pv_misc.

  "--Reinitialize Tables
  REFRESH : lt_selection,
            lt_balances.

  "--Append Selections
  APPEND INITIAL LINE TO lt_selection ASSIGNING <fs_selection>.
  <fs_selection>-buspartner = pv_bp.
  <fs_selection>-cont_acct  = pv_can.

  "--Call FM for Fetching Total Due Amount
  CALL FUNCTION 'BAPI_CTRACCONTRACTACCOUNT_GBAL'
    TABLES
      mainselections = lt_selection
      balanceitems   = lt_balances.

  "--Check if there are Due Amount
  IF lt_balances[] IS NOT INITIAL.
    SORT lt_balances BY net_date DESCENDING.

    "--Iterate per Due Amount
    LOOP AT lt_balances ASSIGNING <fs_balances>.
      IF <fs_balances>-net_date LE pa_ddate.
        IF <fs_balances>-doc_type NE 'AB'.
          "--Summarize Due Amounts
          pv_balance = pv_balance + <fs_balances>-amount.
        ELSE.
          "--If Installment Plan and Already Due
          IF <fs_balances>-disc_due LE pa_ddate.
            "--Summarize Due Amounts
            pv_balance = pv_balance + <fs_balances>-amount.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    pv_balance = 0.
  ENDIF.

  "--Get Current Debit
*  PERFORM get_current_debit_by_duedate  USING    pv_can
*                                                 pv_bp
*                                        CHANGING pv_current.
  "--Get Overdue
  lv_days60 = pa_ddate - 60.
  lv_days25 = pa_ddate - 25.

  WRITE:/ 'Computed 60 days before:' ,lv_days60.
  WRITE:/ 'Computed 25 days before:' ,lv_days25.
  WRITE:/ '--------------------------------------------------'.
  WRITE:/ 'Legend:'.
  WRITE:/ co_green, 'Partially Cleared'.
  WRITE:/ co_yellow,'CURRENT - 0 to 30 days'.
  WRITE:/ co_red,'PUA - 31 to 60 days'.
  WRITE:/ co_red,'Overdue - 61+ days'.
  WRITE:/ '--------------------------------------------------'.

  SELECT opbel faedn budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space.
  "AND xragl NE 'X'.

  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_pua_overdue COMPARING opbel.

    LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.

      CLEAR lv_overdue.
      CLEAR lv_overdue_cleared.
      CLEAR lv_overdue_pia.
      CLEAR lv_check_cleared_e0.

      SELECT SUM( betrh )
      INTO lv_overdue
      FROM dfkkopk
      WHERE opbel EQ <fs_pua_overdue>-opbel.

      IF sy-subrc EQ 0.
        lv_overdue = lv_overdue * -1.
        ADD lv_overdue TO pv_overdue.
        WRITE : / co_red, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_overdue, pv_overdue, '--- OVERDUE'.

        "Check partial clearing
        SELECT SUM( betrh )
        INTO lv_overdue_cleared
        FROM dfkkop
        WHERE opbel EQ <fs_pua_overdue>-opbel
        AND augst EQ '9'
        AND mwskz NE 'E0'.

        IF sy-subrc EQ 0.
          IF lv_overdue_cleared NE 0.
            SUBTRACT lv_overdue_cleared FROM pv_overdue.
          ENDIF.
        ENDIF.

        "Check E0 and Cleared
        SELECT SINGLE opbel
        INTO lv_check_cleared_e0
        FROM dfkkop
        WHERE opbel EQ <fs_pua_overdue>-opbel
        AND augst EQ '9'
        AND mwskz EQ 'E0'.

        IF sy-subrc EQ 0.
          "Check PIA
          SELECT SUM( betrh )
          INTO lv_overdue_pia
          FROM dfkkopk
          WHERE opbel EQ <fs_pua_overdue>-opbel
            AND ( hkont EQ '2160800000' OR hkont EQ '2160810000' )
            AND mwskz EQ 'E0'.

          IF sy-subrc EQ 0.
            IF lv_overdue_pia NE 0.
              ADD lv_overdue_pia TO pv_overdue.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF lv_overdue_cleared NE 0.
        WRITE:/ co_green, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_overdue_cleared, pv_overdue, '--- PARTIALLY CLEARED FROM OVERDUE'.
      ENDIF.

      IF lv_overdue_pia NE 0.
        WRITE:/ co_green, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_overdue_pia, pv_overdue, '--- PIA'.
      ENDIF.

    ENDLOOP.
    UNASSIGN <fs_pua_overdue>.
  ENDIF.

  "--PUA Computation
  SELECT opbel faedn budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days25
  AND faedn GE lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space.
  "AND xragl NE 'X'.

  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_pua_overdue COMPARING opbel.

    LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.
      CLEAR lv_pua.
      CLEAR lv_pua_cleared.
      CLEAR lv_pua_pia.
      CLEAR lv_check_cleared_e0.

      SELECT SUM( betrh )
      INTO lv_pua
      FROM dfkkopk
      WHERE opbel EQ <fs_pua_overdue>-opbel.

      IF sy-subrc EQ 0.
        lv_pua = lv_pua * -1.
        ADD lv_pua TO pv_pua.
        WRITE : / co_red, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_pua, pv_pua, '--- PUA'.

        "Check partial clearing
        SELECT SUM( betrh )
        INTO lv_pua_cleared
        FROM dfkkop
        WHERE opbel EQ <fs_pua_overdue>-opbel
        AND augst EQ '9'
        AND mwskz NE 'E0'.

        IF sy-subrc EQ 0.
          IF lv_pua_cleared NE 0.
            SUBTRACT lv_pua_cleared FROM pv_pua.
          ENDIF.
        ENDIF.

        "Check E0 and Cleared
        SELECT SINGLE opbel
        INTO lv_check_cleared_E0
        FROM dfkkop
        WHERE opbel EQ <fs_pua_overdue>-opbel
        AND augst EQ '9'
        AND mwskz EQ 'E0'.

        IF sy-subrc EQ 0.
          "Check PIA
          SELECT SUM( betrh )
          INTO lv_pua_pia
          FROM dfkkopk
          WHERE opbel EQ <fs_pua_overdue>-opbel
            AND ( hkont EQ '2160800000' OR hkont EQ '2160810000' )
            AND mwskz EQ 'E0'.

          IF sy-subrc EQ 0.
            IF lv_pua_pia NE 0.
              ADD lv_pua_pia TO pv_pua.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF lv_pua_cleared NE 0.
        WRITE:/ co_green, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_pua_cleared, pv_pua, '--- PARTIALLY CLEARED FROM PUA: '.
      ENDIF.

      IF lv_pua_pia NE 0.
        WRITE:/ co_green, <fs_pua_overdue>-opbel, <fs_pua_overdue>-faedn, <fs_pua_overdue>-budat, lv_pua_pia, pv_pua, '--- PIA'.
      ENDIF.

    ENDLOOP.
    UNASSIGN <fs_pua_overdue>.
  ENDIF.

  "--Current Computation
  SELECT opbel faedn budat betrh
  INTO TABLE lt_current
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LE pa_ddate
  AND faedn GE lv_days25
  AND budat NE '00000000'
  AND abwbl EQ space.
  "AND xragl NE 'X'.

  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_current COMPARING opbel.

    LOOP AT lt_current ASSIGNING <fs_current>.
      CLEAR lv_current.
      CLEAR lv_current_cleared.
      CLEAR lv_current_pia.
      CLEAR lv_check_cleared_e0.

      SELECT SUM( betrh )
      INTO lv_current
      FROM dfkkopk
      WHERE opbel EQ <fs_current>-opbel.

      IF sy-subrc EQ 0.
        lv_current = lv_current * -1.
        ADD lv_current TO pv_current.
        WRITE : / co_yellow, <fs_current>-opbel, <fs_current>-faedn, <fs_current>-budat, lv_current, pv_current, '--- CURRENT'.

        "Check partial clearing
        SELECT SUM( betrh )
        INTO lv_current_cleared
        FROM dfkkop
        WHERE opbel EQ <fs_current>-opbel
        AND augst EQ '9'
        AND mwskz NE 'E0'.

        IF sy-subrc EQ 0.
          IF lv_current_cleared NE 0.
            SUBTRACT lv_current_cleared FROM pv_current.
          ENDIF.
        ENDIF.

        "Check E0 and Cleared
        SELECT SINGLE opbel
        INTO lv_check_cleared_E0
        FROM dfkkop
        WHERE opbel EQ <fs_current>-opbel
        AND augst EQ '9'
        AND mwskz EQ 'E0'.

        IF sy-subrc EQ 0.
          "Check PIA
          SELECT SUM( betrh )
          INTO lv_current_pia
          FROM dfkkopk
          WHERE opbel EQ <fs_current>-opbel
            AND ( hkont EQ '2160800000' OR hkont EQ '2160810000' )
            AND mwskz EQ 'E0'.

          IF sy-subrc EQ 0.
            IF lv_current_pia NE 0.
              ADD lv_current_pia TO pv_current.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF lv_current_cleared NE 0.
        WRITE:/ co_green, <fs_current>-opbel, <fs_current>-faedn, <fs_current>-budat, lv_current_cleared, pv_current, '--- PARTIALLY CLEARED FROM CURRENT: '.
      ENDIF.

      IF lv_current_pia NE 0.
        WRITE:/ co_green, <fs_current>-opbel, <fs_current>-faedn, <fs_current>-budat, lv_current_pia, pv_current, '--- PIA'.
      ENDIF.

    ENDLOOP.
    UNASSIGN <fs_current>.
  ENDIF.

  "--Misc Computation - Installment
  SELECT DISTINCT opbel "To retrieve distinct installment plan
  INTO TABLE lt_installation
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0080')
  AND augrd NE '05'
  AND augst EQ space
  "AND faedn LE pa_ddate
  "AND budat EQ '00000000'
  AND abwbl EQ space.

  LOOP AT lt_installation ASSIGNING <fs_installation>.
    CLEAR lv_installation_overdue.
    CLEAR lv_installation_pua.
    CLEAR lv_installation_current.

    "WRITE:/ <fs_misc>-opbel, <fs_misc>-faedn, <fs_misc>-budat, <fs_misc>-betrh.

*    SELECT SUM( betrh )
*    INTO lv_misc
*    FROM dfkkopk
*    WHERE opbel EQ <fs_misc>-opbel.
*    SELECT SUM( betrh )
*    INTO lv_misc1
*    FROM dfkkop
*    WHERE opbel EQ <fs_installation>-opbel.

    "-- Get Account Balances
    CALL FUNCTION 'FKK_S_INSTPLAN_PROVIDE'
      EXPORTING
        i_opbel        = <fs_installation>-opbel
        i_for_update   = space
      IMPORTING
        e_rfkn1        = ls_rfkn1
      TABLES
        raten_fkkop    = lt_sfkkop
      EXCEPTIONS
        already_locked = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
*      lv_start_date  = ls_rfkn1-sttdt.
*      lv_amt         = ls_rfkn1-plana.
*      lv_balance     = ls_rfkn1-gesof.
*      lv_misc = lv_misc * -1.

      lv_misc1 = ls_rfkn1-gesof.
      ADD lv_misc1 TO pv_misc.

      WRITE : / co_misc, <fs_installation>-opbel, 'INSTALLMENT PLAN', lv_misc1, pv_misc, '--- MISC INSTALLMENT'.

      LOOP AT lt_sfkkop ASSIGNING <fs_sfkkop> WHERE faedn GT pa_ddate.
        "WRITE:/ <fs_sfkkop>-faedn, <fs_sfkkop>-betrh, <fs_sfkkop>-betrw.
        lv_misc1_not_yet_due = <fs_sfkkop>-betrh.
        SUBTRACT lv_misc1_not_yet_due FROM pv_misc.
      ENDLOOP.


    ENDIF.

    IF lv_misc1_not_yet_due NE 0.
      lv_misc1_not_yet_due = lv_misc1_not_yet_due * -1. "Turn to negative just for presentation
      WRITE:/ co_misc, <fs_installation>-opbel, 'INSTALLMENT PLAN', lv_misc1_not_yet_due, pv_misc, '--- NOT YET DUE FROM MISC INSTALLMENT: '.
    ENDIF.

*    SELECT SUM( betrh )
*    INTO lv_installation_overdue
*    FROM dfkkop
*    WHERE hvorg IN ('0100', '0200')
*      AND abwbl EQ <fs_installation>-opbel
*      AND augst EQ space
*      AND faedn LT lv_days60.
*
*    SELECT SUM( betrh )
*    INTO lv_installation_pua
*    FROM dfkkop
*    WHERE hvorg IN ('0100', '0200')
*      AND abwbl EQ <fs_installation>-opbel
*      AND augst EQ space
*      AND faedn LT lv_days25
*      AND faedn GE lv_days60.
*
*    SELECT SUM( betrh )
*    INTO lv_installation_current
*    FROM dfkkop
*    WHERE hvorg IN ('0100', '0200')
*      AND abwbl EQ <fs_installation>-opbel
*      AND augst EQ space
*      AND faedn LE pa_ddate
*      AND faedn GE lv_days25.
*
*    IF lv_installation_overdue NE 0.
*      WRITE:/ co_misc, <fs_installation>-opbel, 'INSTALLMENT PLAN OVERDUE', lv_installation_overdue.
*      ADD lv_installation_overdue TO pv_overdue.
*    ENDIF.
*    IF lv_installation_pua NE 0.
*      WRITE:/ co_misc, <fs_installation>-opbel, 'INSTALLMENT PLAN PUA', lv_installation_pua.
*      ADD lv_installation_pua TO pv_pua.
*    ENDIF.
*    IF lv_installation_current NE 0.
*      WRITE:/ co_misc, <fs_installation>-opbel, 'INSTALLMENT PLAN CURRENT', lv_installation_current.
*      ADD lv_installation_current TO pv_current.
*    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_installation>.

  "--Misc Computation - Others
  SELECT opbel faedn budat betrh
  INTO TABLE lt_misc
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg NOT IN ('0100', '0200', '0080')
  AND augrd NE '05'
  AND augst EQ space
  "AND faedn LE pa_ddate
  "AND budat EQ '00000000'
  AND abwbl EQ space.

  IF sy-subrc EQ 0.
    SORT lt_misc BY opbel.
    DELETE ADJACENT DUPLICATES FROM lt_misc COMPARING opbel.
    SORT lt_misc BY budat ASCENDING.
    LOOP AT lt_misc ASSIGNING <fs_misc>.
      CLEAR lv_misc.
      CLEAR lv_misc_cleared.

      "WRITE:/ <fs_misc>-opbel, <fs_misc>-faedn, <fs_misc>-budat, <fs_misc>-betrh.

*      SELECT SUM( betrh )
*      INTO lv_misc
*      FROM dfkkopk
*      WHERE opbel EQ <fs_misc>-opbel.
      SELECT SUM( betrh )
      INTO lv_misc
      FROM dfkkop
      WHERE opbel EQ <fs_misc>-opbel.

      IF sy-subrc EQ 0.
*        lv_misc = lv_misc * -1.
        ADD lv_misc TO pv_misc.

        WRITE : / co_misc, <fs_misc>-opbel, <fs_misc>-faedn, <fs_misc>-budat, lv_misc, pv_misc, '--- MISC OTHERS'.

        SELECT SUM( betrh )
        INTO lv_misc_cleared
        FROM dfkkop
        WHERE opbel EQ <fs_misc>-opbel
        AND augst EQ '9'
        AND mwskz NE 'E0'.

        IF sy-subrc EQ 0.
          IF lv_misc_cleared NE 0.
            SUBTRACT lv_misc_cleared FROM pv_misc.
            "ADD lv_misc_cleared TO pv_misc.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_misc_cleared NE 0.
        WRITE:/ co_misc, <fs_misc>-opbel, <fs_misc>-faedn, <fs_misc>-budat, lv_misc_cleared, pv_misc, '--- PARTIALLY CLEARED FROM MISC OTHERS: '.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_misc>.
  ENDIF.

  IF pv_overdue IS INITIAL.
    pv_overdue = '0.00'.
  ENDIF.
  IF pv_pua IS INITIAL.
    pv_pua = '0.00'.
  ENDIF.
  IF pv_current IS INITIAL.
    pv_current = '0.00'.
  ENDIF.
  IF pv_misc IS INITIAL.
    pv_misc = '0.00'.
  ENDIF.

ENDFORM.                    " GET_DUE_BALANCE_BY_DUEDATE

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_DEBIT_BY_DUEDATE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_current_debit_by_duedate  USING    pv_can     TYPE vkont_kk
                                 pv_bp      TYPE bu_partner
                        CHANGING pv_current TYPE char15.

  TYPES : BEGIN OF ty_current,
            doc_no     TYPE opbel_kk,
            current    TYPE betrw_kk,
          END OF ty_current.

  DATA :  lt_current   TYPE TABLE OF ty_current,
          lv_due_date  TYPE faedn_kk,
          lv_doc_no    TYPE opbel_kk,
          lv_current   TYPE betrw_kk.

  FIELD-SYMBOLS :
         <fs_current> TYPE ty_current.

  "--Reinitialize Variables
  CLEAR : lv_due_date, lv_doc_no, pv_current, lv_current.

  "--Get Document Number of Current Billing
  SELECT faedn opbel
    INTO (lv_due_date, lv_doc_no)
  FROM   dfkkop
  WHERE  gpart EQ pv_bp
  AND    vkont EQ pv_can
  AND    hvorg EQ '0100' "blart NE 'BA'
  AND    faedn EQ pa_ddate
  ORDER BY faedn DESCENDING.
    EXIT.
  ENDSELECT.

  "--Reinitialize Local Table for Current
  REFRESH : lt_current.

  "--Get Current Debit Amount
  SELECT opbel betrw
    INTO TABLE lt_current
  FROM   dfkkopk
  WHERE  opbel EQ lv_doc_no.

  "--Get Summation of Current
  LOOP AT lt_current ASSIGNING <fs_current>.
    lv_current = lv_current + <fs_current>-current.
  ENDLOOP.

  "--Always positive values
  pv_current = abs( lv_current ).
ENDFORM.                    " GET_CURRENT_DEBIT_BY_DUEDATE

*&---------------------------------------------------------------------*
*&      Form  name_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM name_address CHANGING
                    pv_name TYPE fkkvk-vkbez
                    pv_addr TYPE char80.

  DATA:  bpkind TYPE but000-bpkind.
  DATA: lastname(20), firstname(20).
  DATA: addrnum TYPE but020-addrnumber.

  CLEAR address.
  SELECT SINGLE name_last name_first bpkind
    INTO (lastname, firstname, bpkind)
    FROM but000
    WHERE partner = gv_bp.

  IF bpkind = '0002'.
    SELECT SINGLE name_org1
    INTO (firstname)
    FROM but000 WHERE partner = gv_bp.
  ENDIF.

  SELECT SINGLE addrnumber INTO addrnum FROM but020
  WHERE partner = gv_bp.

  CONDENSE firstname.
  CONDENSE lastname.

  REPLACE ',' WITH space INTO firstname.
  REPLACE ',' WITH space INTO lastname.

  IF lastname = firstname.  " OR lastname = 'NO NAME'.
    lastname = space.
  ENDIF.

  CONCATENATE firstname lastname INTO pv_name SEPARATED BY space.
  "TRANSLATE dunnlist-name TO UPPER CASE.

  SELECT SINGLE roomnumber floor house_num1 house_num2 str_suppl1 street
         str_suppl2 str_suppl3 location city2 city1 post_code1
     INTO CORRESPONDING FIELDS OF address FROM adrc
     WHERE addrnumber = addrnum.

  REPLACE ',' WITH space INTO address-roomnumber.
  REPLACE ',' WITH space INTO address-floor.
  REPLACE ',' WITH space INTO address-house_num1.
  REPLACE ',' WITH space INTO address-house_num2.
  REPLACE ',' WITH space INTO address-str_suppl1.
  REPLACE ',' WITH space INTO address-street.
  REPLACE ',' WITH space INTO address-str_suppl2.
  REPLACE ',' WITH space INTO address-str_suppl3.
  REPLACE ',' WITH space INTO address-location.
  REPLACE ',' WITH space INTO address-city2.
  REPLACE ',' WITH space INTO address-city1.
  REPLACE ',' WITH space INTO address-post_code1.

  IF address-house_num1 = '0000'. address-house_num1 = space. ENDIF.
  IF address-street = 'NO NAME'. address-street = space. ENDIF.
  IF address-city2 = 'NO NAME'. address-city2 = space. ENDIF.
  IF address-city1 = 'NO NAME'. address-city1 = space. ENDIF.
  CONDENSE address.
  TRANSLATE address TO UPPER CASE.
  MOVE address TO pv_addr.
ENDFORM.                    "name_address

*&---------------------------------------------------------------------*
*&      Form  get_sortfield
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sortfield CHANGING
                    pv_sortfld     TYPE char6
                    pv_device      TYPE eabl-gernr
                    pv_eqpt        TYPE egerh-equnr.

  DATA: devloc TYPE egerh-devloc.
  DATA: s_iloan TYPE iloa-iloan.

  SELECT SINGLE eqfnr sernr
    INTO (pv_sortfld, pv_device)
  FROM v_equi
  WHERE equnr EQ pv_eqpt
  AND datbi EQ '99991231'
  AND eqfnr NE '0'.
ENDFORM.                    "get_sortfield