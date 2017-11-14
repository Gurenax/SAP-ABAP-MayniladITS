FUNCTION zmwosb_get_pua.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VKONT) LIKE  FKKVK-VKONT
*"     VALUE(I_BASED_LINE_DATE) LIKE  SY-DATUM
*"     REFERENCE(I_END_TIME) TYPE  COITM_KK
*"     REFERENCE(I_START_BILL_PRD) TYPE  SY-DATUM
*"     REFERENCE(I_END_BILL_PRD) TYPE  SY-DATUM
*"  EXPORTING
*"     REFERENCE(E_PUA) LIKE  FKKOP-BETRH
*"     REFERENCE(E_DUE_DATE) LIKE  FKKOP-FAEDN
*"----------------------------------------------------------------------
  DATA: lt_insttab      TYPE TABLE OF fkkepos,
        lt_postab       TYPE TABLE OF fkkepos,
        lv_balance      TYPE fkkop-betrh,
        lv_undue_instal TYPE fkkop-betrh,
        lv_curr_bill    TYPE fkkop-betrh,
        lv_misc         TYPE fkkop-betrh,
        lr_opbel        TYPE RANGE OF fkkop-opbel,
        ls_opbel        LIKE LINE  OF lr_opbel.

  FIELD-SYMBOLS:
        <fs_insttab>    TYPE fkkepos,
        <fs_postab>     TYPE fkkepos.

  CONSTANTS:
        co_time         TYPE coitm_kk VALUE `000000`,
        co_start_date   TYPE sy-datum VALUE `00000000`.

  "-- Macro Definition for Appending Range
  DEFINE mc_append_range.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  "-- Refresh All
  CLEAR:   lv_balance, e_pua, lv_undue_instal, lv_curr_bill, lv_misc.
  REFRESH: lt_insttab, lt_postab.

  "-- Get Account Balances
  CALL FUNCTION 'FKK_ACCOUNT_BALANCE_COMPUTE'
    EXPORTING
      i_vkont      = i_vkont
      i_start_date = co_start_date
      i_start_time = co_time
      i_end_date   = i_based_line_date
      i_end_time   = i_end_time
    IMPORTING
      e_balance    = lv_balance
    TABLES
      t_postab     = lt_postab
      t_insttab    = lt_insttab.

  IF lt_insttab[] IS NOT INITIAL.
    "-- Sort Itab for Installment Plan by Due Date
    SORT lt_insttab BY faedn.

    "-- Get Installment Plans which are not Due
    LOOP AT lt_insttab ASSIGNING <fs_insttab> WHERE faedn GE i_start_bill_prd.
      lv_undue_instal = lv_undue_instal + <fs_insttab>-betrh.

      "-- Append to list of installment plan
      mc_append_range: ls_opbel lr_opbel <fs_insttab>-opbel.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_opbel.
  ENDIF.

  "-- Get Current Balance
  IF lt_postab[] IS NOT INITIAL.
    "-- Sort Table by Posting Date and Time
    "--SORT lt_postab BY budat DESCENDING cputm DESCENDING. "--Commented by GDIMALIWAT 09/22/2014
    SORT lt_postab BY budat DESCENDING cputm DESCENDING." orino ASCENDING. "-- Added by GDIMALIWAT 09/22/2014

*    "-- Get Latest Posting
*    READ TABLE lt_postab ASSIGNING <fs_insttab> INDEX 1.
*    IF sy-subrc EQ 0.
*
*      e_due_date = <fs_insttab>-faedn.
*
*      "-- If Due Date is Greater Than Given Date
*      IF <fs_insttab>-faedn GT i_based_line_date.
*        "-- If not Reversal Document
*        IF <fs_insttab>-blart NE 'RD'.
*          "-- Get Current Billing
*          lv_curr_bill = <fs_insttab>-betrw.
*        ENDIF.
*      ENDIF.
*    ENDIF.

    "-- Get Current Billing
    LOOP AT lt_postab ASSIGNING <fs_insttab> WHERE faedn GT i_based_line_date.  "-- Guarantee Deposit.
                                             "AND   blart NE 'RD'.
      e_due_date = <fs_insttab>-faedn.
      lv_curr_bill = lv_curr_bill + <fs_insttab>-betrw.
    ENDLOOP.

    "-- Get Miscellaneous Charges
    LOOP AT lt_postab ASSIGNING <fs_insttab> WHERE faedn GE i_start_bill_prd
                                               AND ( hvorg EQ '9000' OR  "-- Installation Charges
                                                     hvorg EQ '7003' OR  "-- Re-opening Charges
                                                     hvorg EQ '7002' OR  "-- Meter Charges
                                                     hvorg EQ '7007' OR  "-- Other Charges
                                                     hvorg EQ '6045' ).  "-- Guarantee Deposit.

      IF <fs_insttab>-augst ne '9' AND <fs_insttab>-augdt le i_end_bill_prd.
        IF <fs_insttab>-abwbl NOT IN lr_opbel.
          lv_misc = lv_misc + <fs_insttab>-betrw.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0.
      e_due_date = i_based_line_date.
    ENDIF.

  ENDIF.

  "-- Computation for Current Balance
  IF lv_undue_instal NE 0.
    e_pua = lv_balance - ( lv_undue_instal + lv_curr_bill + lv_misc ).
  ELSE.
    "--Commented by GDIMALIWAT
*    IF lv_curr_bill LT 0.
*      e_pua = lv_curr_bill.
*    ELSEIF lv_curr_bill GT 0.
*      e_pua = lv_curr_bill * -1.
*    ENDIF.
    "--End of Commented by GDIMALIWAT
    e_pua = lv_balance - lv_misc.
  ENDIF.
ENDFUNCTION.