*&---------------------------------------------------------------------*
*&      Form  GET_DUE_BALANCE
*&---------------------------------------------------------------------*
*       Subroutine for Fetching Due Balance of an Account
*----------------------------------------------------------------------*
FORM get_due_balance  USING    pv_can     TYPE vkont_kk
                               pv_bp      TYPE bu_partner
                      CHANGING pv_balance TYPE betrh_kk
                               pv_pua     TYPE char15
                               pv_current TYPE char15
                               pv_overdue TYPE char15
                               pv_misc TYPE char15.

  DATA: lt_selection   TYPE TABLE OF bapifkkopselhead,
        lt_balances    TYPE TABLE OF bapifkkepos,
        "lv_payment     TYPE char15,
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
        lv_misc1            TYPE dfkkop-betrh,
        lv_misc1_not_yet_due TYPE dfkkop-betrh,
        lv_misc2            TYPE dfkkop-betrh,
        lv_misc2_cleared    TYPE dfkkop-betrh,
        lv_misc1_total      TYPE dfkkop-betrh,
        lv_misc2_total      TYPE dfkkop-betrh,
        ls_rfkn1            TYPE rfkn1,
        lv_check_cleared_e0 TYPE dfkkop-opbel.

  DATA : lt_pua_overdue TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_current     TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_misc_inst   TYPE TABLE OF ty_opbel_only,
         lt_misc_others TYPE TABLE OF ty_pua_overdue,
         lt_sfkkop      TYPE TABLE OF sfkkop.

  FIELD-SYMBOLS:
        <fs_selection>    TYPE bapifkkopselhead,
        <fs_balances>     TYPE bapifkkepos,
        <fs_pua_overdue>  TYPE ty_pua_overdue,
        <fs_current>      TYPE ty_pua_overdue,
        <fs_sfkkop>       TYPE sfkkop,
        <fs_misc_inst>    TYPE ty_opbel_only,
        <fs_misc_others>  TYPE ty_pua_overdue.

  "-- Reinitialize Variables
  CLEAR: pv_balance,
         pv_pua,
         pv_current.

  "-- Reinitialize Tables
  REFRESH: lt_selection,
           lt_balances.

  "-- Append Selections
  APPEND INITIAL LINE TO lt_selection ASSIGNING <fs_selection>.
  <fs_selection>-buspartner = pv_bp.
  <fs_selection>-cont_acct  = pv_can.

  "-- Call FM for Fetching Total Due Amount
  CALL FUNCTION 'BAPI_CTRACCONTRACTACCOUNT_GBAL'
    TABLES
      mainselections = lt_selection
      balanceitems   = lt_balances.

  "-- Check if there are Due Amount
  IF lt_balances[] IS NOT INITIAL.
    SORT lt_balances BY net_date DESCENDING.

    "-- Iterate per Due Amount
    LOOP AT lt_balances ASSIGNING <fs_balances>.
      IF <fs_balances>-net_date LE gv_highest_duedate.
        IF <fs_balances>-doc_type NE 'AB'.
          "-- Summarize Due Amounts
          pv_balance = pv_balance + <fs_balances>-amount.
        ELSE.
          "-- If Installment Plan and Already Due
          IF <fs_balances>-disc_due LE gv_highest_duedate.
            "-- Summarize Due Amounts
            pv_balance = pv_balance + <fs_balances>-amount.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    pv_balance = 0.
  ENDIF.

*  "-- Get Current Debit
*  PERFORM get_current_debit_by_duedate  USING    pv_can
*                                                 pv_bp
*                                        CHANGING pv_current.

  "-- Get Overdue
  lv_days60 = gv_highest_duedate - co_60.
  lv_days25 = gv_highest_duedate - co_25.

  SELECT opbel budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space.
  "AND xragl NE 'X' .

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
        AND mwskz EQ 'E0'.                                  "#EC WARNOK

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
    ENDLOOP .
    UNASSIGN <fs_pua_overdue>.
  ENDIF.

  "--PUA Computation
  SELECT opbel budat betrh
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
  "AND xragl NE 'X' .

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
        INTO lv_check_cleared_e0
        FROM dfkkop
        WHERE opbel EQ <fs_pua_overdue>-opbel
        AND augst EQ '9'
        AND mwskz EQ 'E0'.                                  "#EC WARNOK

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
    ENDLOOP .
    UNASSIGN <fs_pua_overdue>.
  ENDIF.

  "--Current Computation
  SELECT opbel budat betrh
  INTO TABLE lt_current
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LE gv_highest_duedate
  AND faedn GE lv_days25
  AND budat NE '00000000'
  AND abwbl EQ space.
  "AND xragl NE 'X' .

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
        INTO lv_check_cleared_e0
        FROM dfkkop
        WHERE opbel EQ <fs_current>-opbel
        AND augst EQ '9'
        AND mwskz EQ 'E0'.                                  "#EC WARNOK

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
    ENDLOOP.
    UNASSIGN <fs_current>.
  ENDIF.

  "--Misc Computation - Installment
  SELECT DISTINCT opbel "To retrieve distinct installment plan
  INTO TABLE lt_misc_inst
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0080')
  AND augrd NE '05'
  AND augst EQ space
  AND abwbl EQ space.

  IF sy-subrc EQ 0.
    LOOP AT lt_misc_inst ASSIGNING <fs_misc_inst>.
      CLEAR lv_misc1.
      CLEAR lv_misc1_not_yet_due.

      "-- Get Account Balances
      CALL FUNCTION 'FKK_S_INSTPLAN_PROVIDE'
        EXPORTING
          i_opbel        = <fs_misc_inst>-opbel
          i_for_update   = space
        IMPORTING
          e_rfkn1        = ls_rfkn1
        TABLES
          raten_fkkop    = lt_sfkkop
        EXCEPTIONS
          already_locked = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        lv_misc1 = ls_rfkn1-gesof.
        ADD lv_misc1 TO lv_misc1_total.

        LOOP AT lt_sfkkop ASSIGNING <fs_sfkkop> WHERE faedn GT sy-datum.
          lv_misc1_not_yet_due = <fs_sfkkop>-betrh.
          SUBTRACT lv_misc1_not_yet_due FROM lv_misc1_total.
        ENDLOOP.


      ENDIF.

      IF lv_misc1_not_yet_due NE 0.
        lv_misc1_not_yet_due = lv_misc1_not_yet_due * -1. "Turn to negative just for presentation
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_misc_inst>.
  ENDIF.

  "--Misc Computation - Others
  SELECT opbel budat betrh
  INTO TABLE lt_misc_others
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg NOT IN ('0100', '0200', '0080')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LE gv_highest_duedate
  AND abwbl EQ space.

  IF sy-subrc EQ 0.
    SORT lt_misc_others BY opbel.
    DELETE ADJACENT DUPLICATES FROM lt_misc_others COMPARING opbel.

    LOOP AT lt_misc_others ASSIGNING <fs_misc_others>.
      CLEAR lv_misc2.
      CLEAR lv_misc2_cleared.

      SELECT SUM( betrh )
      INTO lv_misc2
      FROM dfkkop
      WHERE opbel EQ <fs_misc_others>-opbel.

      IF sy-subrc EQ 0.
        ADD lv_misc2 TO lv_misc2_total.

        SELECT SUM( betrh )
        INTO lv_misc2_cleared
        FROM dfkkop
        WHERE opbel EQ <fs_misc_others>-opbel
        AND augst EQ '9'.
        "AND mwskz NE 'E0'.

        IF sy-subrc EQ 0.
          IF lv_misc2_cleared NE 0.
            SUBTRACT lv_misc2_cleared FROM lv_misc2_total.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_misc_others>.
  ENDIF.

  pv_misc = lv_misc1_total + lv_misc2_total.

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
ENDFORM.                    " GET_DUE_BALANCE