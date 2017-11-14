*&---------------------------------------------------------------------*
*& Report  ZRFI_SUPP_CONT_DISBURSEMENT
*& DESCRIPTION: Suppliers/Contractors Disbursement Report
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 2013/07/09
*& Reference  : TN#38701
*&---------------------------------------------------------------------*
*& Date       | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/07/09 | GDIMALIWAT | 38701      | Created
*&---------------------------------------------------------------------*
REPORT zrfi_supp_cont_disbursement MESSAGE-ID zisu.

*INCLUDE
TABLES: bsak.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_blart_param,
        blart TYPE bsak-blart,
        description TYPE char25,
       END OF ty_blart_param,

*       BEGIN OF ty_bsak,
*         shkzg TYPE bsak-shkzg,
*         blart TYPE bsak-blart,
*         lifnr TYPE bsak-lifnr,
*         empfb TYPE bsak-empfb,
*         zlsch TYPE bsak-zlsch,
*         budat TYPE bsak-budat,
*         waers TYPE bsak-waers,
*         gjahr TYPE bsak-gjahr,
*         belnr TYPE bsak-belnr,
*         wrbtr TYPE bsak-wrbtr,
*         bukrs TYPE bsak-bukrs,
*       END OF ty_bsak,

       BEGIN OF ty_bsak,
         bukrs TYPE bsak-bukrs,
         belnr TYPE bsak-belnr,
         gjahr TYPE bsak-gjahr,
         rebzg TYPE bsak-rebzg,
         rebzj TYPE bsak-rebzj,
         lifnr TYPE bsak-lifnr,
         empfb TYPE bsak-empfb,
         wrbtr TYPE bsak-wrbtr,
         zlsch TYPE bsak-zlsch,
         waers TYPE bsak-waers,
         budat TYPE bsak-budat,
       END OF ty_bsak,

       BEGIN OF ty_bsak_amount,
         dmbtr TYPE bsak-dmbtr,
         wrbtr TYPE bsak-wrbtr,
         shkzg TYPE bsak-shkzg,
       END OF ty_bsak_amount,

*       BEGIN OF ty_bsas,
*         belnr TYPE bsas-belnr,
*         augbl TYPE bsas-augbl,
*         augdt TYPE bsas-augdt,
*         gjahr TYPE bsas-gjahr,
*         bukrs TYPE bsas-bukrs,
*       END OF ty_bsas,

        BEGIN OF ty_bkpf,
          bukrs TYPE bkpf-bukrs,
          belnr TYPE bkpf-belnr,
          gjahr TYPE bkpf-gjahr,
          "blart TYPE bkpf-blart,
        END OF ty_bkpf,

        BEGIN OF ty_bsas,
          hkont TYPE bsas-hkont,
          dmbtr TYPE bsas-dmbtr,
          wrbtr TYPE bsas-wrbtr,
          shkzg TYPE bsas-shkzg,
          sgtxt TYPE bsas-sgtxt,
          buzid TYPE bsas-buzid,
          belnr TYPE bsas-belnr,
          gjahr TYPE bsas-gjahr,
        END OF ty_bsas,

        BEGIN OF ty_bsas3,
          hkont TYPE bsas-hkont,
          dmbtr TYPE bsas-dmbtr,
          wrbtr TYPE bsas-wrbtr,
          shkzg TYPE bsas-shkzg,
        END OF ty_bsas3,

        BEGIN OF ty_bsas4,
          hkont TYPE bsas-hkont,
          dmbtr TYPE bsas-dmbtr,
          wrbtr TYPE bsas-wrbtr,
          sgtxt TYPE bsas-sgtxt,
        END OF ty_bsas4,

        BEGIN OF ty_hkont,
          hkont TYPE bsas-hkont,
        END OF ty_hkont,

        BEGIN OF ty_bsis,
          hkont TYPE bsis-hkont,
          dmbtr TYPE bsis-dmbtr,
          wrbtr TYPE bsis-wrbtr,
          shkzg TYPE bsis-shkzg,
          sgtxt TYPE bsis-sgtxt,
          buzid TYPE bsis-buzid,
          belnr TYPE bsis-belnr,
          gjahr TYPE bsis-gjahr,
        END OF ty_bsis,

        BEGIN OF ty_bsis2,
          hkont TYPE bsis-hkont,
          dmbtr TYPE bsis-dmbtr,
          wrbtr TYPE bsis-wrbtr,
          sgtxt TYPE bsis-sgtxt,
          shkzg TYPE bsas-shkzg,
        END OF ty_bsis2,

        BEGIN OF ty_bsis3,
          hkont TYPE bsis-hkont,
          dmbtr TYPE bsis-dmbtr,
          wrbtr TYPE bsis-wrbtr,
          shkzg TYPE bsis-shkzg,
        END OF ty_bsis3,

*       BEGIN OF ty_payr,
*         zbukr TYPE payr-zbukr,
*         lifnr TYPE payr-lifnr,
*         gjahr TYPE payr-gjahr,
*         vblnr TYPE payr-vblnr,
*         rwbtr TYPE payr-rwbtr,
*         waers TYPE payr-waers,
*       END OF ty_payr,

      BEGIN OF ty_output,
        belnr TYPE c LENGTH 10, "for testing
        vendor_code      TYPE c LENGTH 11,
        payee            TYPE c LENGTH 71,
        payment_method   TYPE c LENGTH 14,
        check_no         TYPE c LENGTH 13,
        payment_date     TYPE c LENGTH 12,
        currency         TYPE c LENGTH 8,
        amount           TYPE c LENGTH 16,
        amount_in_php    TYPE c LENGTH 16,
        work_done        TYPE c LENGTH 50,
      END OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_blart_param_value     TYPE TABLE OF ty_blart_param,
      gt_blart_param_return    LIKE ddshretval OCCURS 0 WITH HEADER LINE.

DATA: gt_bsak          TYPE TABLE OF ty_bsak,        "All Payment Docs
      gt_bsak_amount   TYPE TABLE OF ty_bsak_amount, "Net payment amount

      gt_bsas          TYPE TABLE OF ty_bsas,        "Scenario 1
      gt_bsas2         TYPE TABLE OF ty_bsas,        "Scenario 1 - Filtered HKONT
      gt_bsas_hkont    TYPE TABLE OF ty_hkont,       "Scenario 1 - Unique HKONT List
      gt_bsas3         TYPE TABLE OF ty_bsas3,       "Scenario 1 - Grouped by HKONT, Sum of Amounts for Proration

      gt_bsis          TYPE TABLE OF ty_bsis,        "Scenario 2
      gt_bsis2         TYPE TABLE OF ty_bsis2,       "Scenario 2 - Filtered HKONT
      gt_bsis_hkont    TYPE TABLE OF ty_hkont,       "Scenario 2 - Unique HKONT List
      gt_bsis3         TYPE TABLE OF ty_bsis3,       "Scenario 2 - Grouped by HKONT, Sum of Amounts for Proration

      "gt_payr   TYPE TABLE OF ty_payr,
      gt_output TYPE TABLE OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_blart_param  TYPE ty_blart_param.
DATA: gv_netpay_dmbtr_amt TYPE bsak-dmbtr,
      gv_netpay_wrbtr_amt TYPE bsak-wrbtr,
      gv_dmbtr_sum_all    TYPE bsas-dmbtr,
      gv_wrbtr_sum_all    TYPE bsas-wrbtr.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_pipe TYPE c LENGTH 1 VALUE '|'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_bsak>          TYPE ty_bsak,
                <fs_bsak_amount>   TYPE ty_bsak_amount,
                "<fs_bsas>   TYPE ty_bsas,
                "<fs_bkpf>   TYPE ty_bkpf,
                <fs_bsas>          TYPE ty_bsas,
                <fs_bsas2>         TYPE ty_bsas,
                <fs_bsas3>         TYPE ty_bsas3,
                <fs_bsas_hkont>    TYPE ty_hkont,
                <fs_bsis>          TYPE ty_bsis,
                <fs_bsis2>         TYPE ty_bsis2,
                <fs_bsis3>         TYPE ty_bsis3,
                <fs_bsis_hkont>    TYPE ty_hkont,
                "<fs_gl>     TYPE ty_bsis,
"<fs_payr>   TYPE ty_payr,
<fs_output> TYPE ty_output.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS:     pa_bukrs     TYPE bsak-bukrs DEFAULT 'MWSI' OBLIGATORY.  "Co. Code
SELECT-OPTIONS: so_budat     FOR  bsak-budat OBLIGATORY,                 "Date / Date range
                "so_shkzg     FOR  bsak-shkzg NO INTERVALS NO-EXTENSION,  "All payments and/or reversal of payments
                so_blart     FOR  bsak-blart NO INTERVALS NO-EXTENSION,  "All petty cash-related payments and/or reversal of payments
                so_zlsch     FOR  bsak-zlsch NO INTERVALS.               "One or more payment methods

PARAMETERS:     pa_file      TYPE rlgrap-filename DEFAULT '\\172.18.1.240\repuserdata\Glenn\Testing\Output.txt'.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*AT SELECTION-SCREEN ON <para|selcrit>
*
*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_blart-low.

  PERFORM value_request_for_so_blart.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  "Default is Date range = 1st day of current month up to current system date
  CONCATENATE sy-datum(6) '01' INTO so_budat-low.
  so_budat-high = sy-datum.
  APPEND so_budat.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  PERFORM begin_extraction.
  PERFORM extract_output.

*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_FOR_SO_BLART
*&---------------------------------------------------------------------*
*       Create values for SO_BLART
*----------------------------------------------------------------------*
FORM value_request_for_so_blart .
  REFRESH gt_blart_param_value[].
  gv_blart_param-blart = 'IF'.
  gv_blart_param-description = 'Petty cash-related'.
  APPEND gv_blart_param TO gt_blart_param_value.
  gv_blart_param-blart = 'ZP'.
  gv_blart_param-description = 'Non petty-cash related'.
  APPEND gv_blart_param TO gt_blart_param_value.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'BLART'   "field of internal table
      value_org  = 'S'
    TABLES
      value_tab  = gt_blart_param_value
      return_tab = gt_blart_param_return.

  WRITE gt_blart_param_return-fieldval TO so_blart-low.
  REFRESH gt_blart_param_value.
ENDFORM.                    " VALUE_REQUEST_FOR_SO_BLART

*&---------------------------------------------------------------------*
*&      Form  BEGIN_EXTRACTION
*&---------------------------------------------------------------------*
*       Start report extraction
*----------------------------------------------------------------------*
FORM begin_extraction .



  DATA: lv_name1     TYPE lfa1-name1,
        lv_name2     TYPE lfa1-name2,
        lv_neg_wrbtr TYPE bsak-wrbtr,
        "lv_wrbtr     TYPE bsas-wrbtr,
        "lv_dmbtr     TYPE bsas-dmbtr,
        lv_stblg     TYPE bkpf-stblg.

  IF so_blart IS NOT INITIAL.
    SELECT bukrs belnr gjahr rebzg rebzj lifnr empfb wrbtr zlsch waers budat  "All Payment Docs
    INTO TABLE gt_bsak
    FROM bsak
    WHERE bukrs EQ pa_bukrs
    AND budat IN so_budat
    "AND shkzg IN so_shkzg
    AND blart IN so_blart
    AND zlsch IN so_zlsch.
  ELSE.
    SELECT bukrs belnr gjahr rebzg rebzj lifnr empfb wrbtr zlsch waers budat  "All Payment Docs
      INTO TABLE gt_bsak
      FROM bsak
      WHERE bukrs EQ pa_bukrs
      AND budat IN so_budat
      "AND shkzg IN so_shkzg
      AND ( blart EQ 'ZP' OR blart EQ 'IF' OR blart EQ 'KZ' )
      AND zlsch IN so_zlsch.
  ENDIF.

  IF sy-subrc EQ 0.
    LOOP AT gt_bsak ASSIGNING <fs_bsak>.
      REFRESH: gt_bsak_amount, gt_bsas, gt_bsas2, gt_bsas_hkont, gt_bsas3.
      CLEAR: gv_netpay_dmbtr_amt, gv_netpay_wrbtr_amt, gv_dmbtr_sum_all, gv_wrbtr_sum_all.

      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.

      "for testing
      <fs_output>-belnr = <fs_bsak>-belnr.

      "Vendor
      <fs_output>-vendor_code = <fs_bsak>-lifnr.

      "Payee
      IF <fs_bsak>-empfb IS NOT INITIAL.
        <fs_output>-payee = <fs_bsak>-empfb.
      ELSE.
        SELECT SINGLE name1 name2
        INTO (lv_name1, lv_name2)
        FROM lfa1
        WHERE lifnr EQ <fs_bsak>-lifnr.

        IF sy-subrc EQ 0.
          CONCATENATE lv_name1 lv_name2 INTO <fs_output>-payee SEPARATED BY space.
        ENDIF.
      ENDIF.

      "Negate WRBTR
      lv_neg_wrbtr = <fs_bsak>-wrbtr * -1.

      "Payment method / Check No.
      IF <fs_bsak>-zlsch IS NOT INITIAL.
        <fs_output>-payment_method = <fs_bsak>-zlsch.

        SELECT SINGLE chect
        INTO <fs_output>-check_no
        FROM payr
        WHERE zbukr EQ pa_bukrs
          AND lifnr EQ <fs_bsak>-lifnr
          AND gjahr EQ <fs_bsak>-gjahr
          AND vblnr EQ <fs_bsak>-belnr
          AND rwbtr EQ lv_neg_wrbtr
          AND waers EQ <fs_bsak>-waers.
      ELSE.
        SELECT SINGLE rzawe chect
        INTO (<fs_output>-payment_method, <fs_output>-check_no)
        FROM payr
        WHERE zbukr EQ pa_bukrs
          AND lifnr EQ <fs_bsak>-lifnr
          AND gjahr EQ <fs_bsak>-gjahr
          AND vblnr EQ <fs_bsak>-belnr
          AND rwbtr EQ lv_neg_wrbtr
          AND waers EQ <fs_bsak>-waers.
      ENDIF.

      "Payment Date
      <fs_output>-payment_date = <fs_bsak>-budat.

      "Currency
      <fs_output>-currency = <fs_bsak>-waers.


      SELECT dmbtr wrbtr shkzg    "Net Payment Amount
      INTO TABLE gt_bsak_amount
      FROM bsak
      WHERE bukrs EQ <fs_bsak>-bukrs
        AND belnr EQ <fs_bsak>-belnr
        AND gjahr EQ <fs_bsak>-gjahr.

      IF sy-subrc EQ 0.
        LOOP AT gt_bsak_amount ASSIGNING <fs_bsak_amount>.
          IF <fs_bsak_amount>-shkzg EQ 'H'.
            <fs_bsak_amount>-dmbtr = <fs_bsak_amount>-dmbtr * -1.
            <fs_bsak_amount>-wrbtr = <fs_bsak_amount>-wrbtr * -1.
          ENDIF.
          ADD <fs_bsak_amount>-dmbtr TO gv_netpay_dmbtr_amt.       "Net payment amount in php
          ADD <fs_bsak_amount>-dmbtr TO gv_netpay_wrbtr_amt.       "Net payment amount
        ENDLOOP.
        UNASSIGN <fs_bsak_amount>.
      ENDIF.

      SELECT hkont dmbtr wrbtr shkzg sgtxt buzid belnr gjahr "Scenario 1 Non Payment Doc
      INTO TABLE gt_bsas
      FROM bsas
      WHERE bukrs EQ pa_bukrs
        AND augbl EQ <fs_bsak>-belnr
        AND auggj EQ <fs_bsak>-gjahr
        AND ( blart NE 'ZP' AND blart NE 'KZ').

      IF sy-subrc EQ 0.
*        Start of Scenario 1
        PERFORM SCENARIO1.
      ELSE.
*        Start of Scenario 2
        SELECT hkont dmbtr wrbtr shkzg sgtxt buzid belnr gjahr  "Scenario 2 Non Payment Doc
        INTO TABLE gt_bsis
        FROM bsak
        WHERE belnr EQ <fs_bsak>-rebzg
          AND gjahr EQ <fs_bsak>-rebzj.

        IF sy-subrc EQ 0.
          CLEAR lv_stblg.

          SELECT SINGLE stblg
          INTO lv_stblg
          FROM bkpf
          WHERE bukrs EQ pa_bukrs
            AND belnr EQ <fs_bsak>-belnr
            AND gjahr EQ <fs_bsak>-gjahr.

          IF lv_stblg IS INITIAL.
            PERFORM SCENARIO2.
          ELSE.
            PERFORM SCENARIO3.
          ENDIF.
        ELSE.
          PERFORM SCENARIO3.
        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_bsak>.
  ENDIF.
ENDFORM.                    " BEGIN_EXTRACTION

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_OUTPUT
*&---------------------------------------------------------------------*
*       Extract output file
*----------------------------------------------------------------------*
FORM extract_output .

  DATA: lt_output TYPE string,
        lt_header TYPE ty_output.

  OPEN DATASET pa_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    lt_header-belnr    = 'BELNR'. " For testing
    lt_header-vendor_code    = 'VENDOR CODE'.
    lt_header-payee          = 'PAYEE'.
    lt_header-payment_method = 'PAYMENT METHOD'.
    lt_header-check_no       = 'CHECK NO'.
    lt_header-payment_date   = 'PAYMENT DATE'.
    lt_header-currency       = 'CURRENCY'.
    lt_header-amount         = 'AMOUNT'.
    lt_header-amount_in_php  = 'AMOUNT IN PHP'.
    lt_header-work_done      = 'WORK DONE'.

    INSERT lt_header INTO gt_output INDEX 1.
    LOOP AT gt_output ASSIGNING <fs_output>.
      WRITE:/ <fs_output>-belnr, <fs_output>-vendor_code, <fs_output>-payee, <fs_output>-payment_method, <fs_output>-check_no, <fs_output>-payment_date, <fs_output>-currency, <fs_output>-amount, <fs_output>-amount_in_php, <fs_output>-work_done.
      CONCATENATE <fs_output>-belnr <fs_output>-vendor_code <fs_output>-payee <fs_output>-payment_method <fs_output>-check_no <fs_output>-payment_date <fs_output>-currency <fs_output>-amount <fs_output>-amount_in_php <fs_output>-work_done INTO
lt_output SEPARATED BY
co_pipe RESPECTING BLANKS.
      TRANSFER lt_output TO pa_file.
    ENDLOOP.
  ENDIF.
  CLOSE DATASET pa_file.
ENDFORM.                    " EXTRACT_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SCENARIO1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form SCENARIO1 .

  DATA: lv_dmbtr_sum        TYPE bsas-dmbtr,
        lv_wrbtr_sum        TYPE bsas-wrbtr.

  DATA:
        lv_temp_ergsl       TYPE ergsl,
        lv_work_done        TYPE bsas-sgtxt.

  LOOP AT gt_bsas ASSIGNING <fs_bsas>.
    "Replace the BSAS hkont with a BSIS hkont
    SELECT SINGLE hkont
    INTO <fs_bsas>-hkont
    FROM bsis
    WHERE bukrs EQ pa_bukrs
      AND belnr EQ <fs_bsas>-belnr
      AND gjahr EQ <fs_bsas>-gjahr.

    IF <fs_bsas>-hkont EQ '2161000000' OR <fs_bsas>-hkont EQ '1160103000'.
      IF <fs_bsas>-sgtxt CS 'MWSS MOE'.
        APPEND INITIAL LINE TO gt_bsas2 ASSIGNING <fs_bsas2>.
        <fs_bsas2> = <fs_bsas>.
      ENDIF.
    ELSEIF <fs_bsas>-buzid NE 'T'.
      APPEND INITIAL LINE TO gt_bsas2 ASSIGNING <fs_bsas2>.
      <fs_bsas2> = <fs_bsas>.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_bsas>.

  SORT gt_bsas2 BY hkont ASCENDING.
  LOOP AT gt_bsas2 ASSIGNING <fs_bsas2>.
    AT NEW hkont.
      APPEND INITIAL LINE TO gt_bsas_hkont ASSIGNING <fs_bsas_hkont>.
      <fs_bsas_hkont>-hkont = <fs_bsas2>-hkont.
    ENDAT.
  ENDLOOP.
  UNASSIGN <fs_bsas2>.

  LOOP AT gt_bsas_hkont ASSIGNING <fs_bsas_hkont>.
    CLEAR: lv_dmbtr_sum, lv_wrbtr_sum.

    AT NEW hkont.
      LOOP AT gt_bsas2 ASSIGNING <fs_bsas> WHERE hkont EQ <fs_bsas_hkont>-hkont.
        IF <fs_bsas>-shkzg EQ 'H'.
          <fs_bsas>-dmbtr = <fs_bsas>-dmbtr * -1.
          <fs_bsas>-wrbtr = <fs_bsas>-wrbtr * -1.
        ENDIF.

        ADD <fs_bsas>-dmbtr TO lv_dmbtr_sum.
        ADD <fs_bsas>-wrbtr TO lv_wrbtr_sum.
        APPEND INITIAL LINE TO gt_bsas3 ASSIGNING <fs_bsas3>.
        <fs_bsas3>-hkont = <fs_bsas>-hkont.
        <fs_bsas3>-shkzg = <fs_bsas>-shkzg.
        <fs_bsas3>-dmbtr = lv_dmbtr_sum.
        <fs_bsas3>-wrbtr = lv_wrbtr_sum.

        ADD lv_dmbtr_sum TO gv_dmbtr_sum_all.
        ADD lv_wrbtr_sum TO gv_wrbtr_sum_all.
      ENDLOOP.
      UNASSIGN <fs_bsas>.
    ENDAT.
  ENDLOOP.
  UNASSIGN <fs_bsas_hkont>.

  LOOP AT gt_bsas3 ASSIGNING <fs_bsas3>.
    CLEAR: lv_temp_ergsl, lv_work_done.
    "Proration
    <fs_output>-amount_in_php = ( <fs_bsas3>-dmbtr / gv_dmbtr_sum_all ) * gv_netpay_dmbtr_amt.
    <fs_output>-amount        = ( <fs_bsas3>-wrbtr / gv_wrbtr_sum_all ) * gv_netpay_wrbtr_amt.

    SELECT SINGLE rowdesc
    INTO lv_work_done
    FROM zfagl_011zc
    WHERE vonkt LE <fs_bsas3>-hkont
      AND biskt GE <fs_bsas3>-hkont.

    IF sy-subrc EQ 0.
      <fs_output>-work_done     = lv_work_done.
    ELSE.
      SELECT SINGLE ergsl
      INTO lv_temp_ergsl
      FROM fagl_011zc
      WHERE vonkt LE <fs_bsas3>-hkont
        AND biskt GE <fs_bsas3>-hkont.

      IF sy-subrc EQ 0.
        SELECT SINGLE rowdesc
        INTO lv_work_done
        FROM zfagl_011zc
        WHERE ergsl EQ lv_temp_ergsl.

        IF sy-subrc EQ 0.
          <fs_output>-work_done     = lv_work_done.
        ELSE.
          CONCATENATE 'UNCLASSIFIED' <fs_bsas3>-hkont 'Scenario 1' lv_temp_ergsl INTO <fs_output>-work_done SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
endform.                    " SCENARIO1
*&---------------------------------------------------------------------*
*&      Form  SCENARIO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form SCENARIO2 .
  DATA: lv_dmbtr_sum        TYPE bsis-dmbtr,
        lv_wrbtr_sum        TYPE bsis-wrbtr,
        lv_temp_ergsl       TYPE ergsl,
        lv_work_done        TYPE bsis-sgtxt.

  LOOP AT gt_bsis ASSIGNING <fs_bsis>.
    IF <fs_bsis>-hkont EQ '2161000000' OR <fs_bsis>-hkont EQ '1160103000'.
      IF <fs_bsis>-sgtxt CS 'MWSS MOE'.
        APPEND INITIAL LINE TO gt_bsis2 ASSIGNING <fs_bsis2>.
        <fs_bsis2> = <fs_bsis>.
      ENDIF.
    ELSEIF <fs_bsis>-buzid NE 'T'.
      APPEND INITIAL LINE TO gt_bsis2 ASSIGNING <fs_bsis2>.
      <fs_bsis2> = <fs_bsis>.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_bsis>.

  SORT gt_bsis2 BY hkont ASCENDING.
  LOOP AT gt_bsis2 ASSIGNING <fs_bsis2>.
    AT NEW hkont.
      APPEND INITIAL LINE TO gt_bsis_hkont ASSIGNING <fs_bsis_hkont>.
      <fs_bsis_hkont>-hkont = <fs_bsis2>-hkont.
    ENDAT.
  ENDLOOP.
  UNASSIGN <fs_bsis2>.

  LOOP AT gt_bsis_hkont ASSIGNING <fs_bsis_hkont>.
    CLEAR: lv_dmbtr_sum, lv_wrbtr_sum.

    AT NEW hkont.
      LOOP AT gt_bsis2 ASSIGNING <fs_bsis2> WHERE hkont EQ <fs_bsis_hkont>-hkont.
        IF <fs_bsis2>-shkzg EQ 'H'.
          <fs_bsis2>-dmbtr = <fs_bsis2>-dmbtr * -1.
          <fs_bsis2>-wrbtr = <fs_bsis2>-wrbtr * -1.
        ENDIF.

        ADD <fs_bsis2>-dmbtr TO lv_dmbtr_sum.
        ADD <fs_bsis2>-wrbtr TO lv_wrbtr_sum.
        APPEND INITIAL LINE TO gt_bsis3 ASSIGNING <fs_bsis3>.
        <fs_bsis3>-hkont = <fs_bsis2>-hkont.
        <fs_bsis3>-shkzg = <fs_bsis2>-shkzg.
        <fs_bsis3>-dmbtr = lv_dmbtr_sum.
        <fs_bsis3>-wrbtr = lv_wrbtr_sum.

        ADD lv_dmbtr_sum TO gv_dmbtr_sum_all.
        ADD lv_wrbtr_sum TO gv_wrbtr_sum_all.
      ENDLOOP.
      UNASSIGN <fs_bsis>.
    ENDAT.
  ENDLOOP.
  UNASSIGN <fs_bsis_hkont>.

  LOOP AT gt_bsis3 ASSIGNING <fs_bsis3>.
    CLEAR: lv_temp_ergsl, lv_work_done.
    "Proration
    <fs_output>-amount_in_php = ( <fs_bsis3>-dmbtr / gv_dmbtr_sum_all ) * gv_netpay_dmbtr_amt.
    <fs_output>-amount        = ( <fs_bsis3>-wrbtr / gv_wrbtr_sum_all ) * gv_netpay_wrbtr_amt.

    SELECT SINGLE rowdesc
    INTO lv_work_done
    FROM zfagl_011zc
    WHERE vonkt LE <fs_bsis3>-hkont
      AND biskt GE <fs_bsis3>-hkont.

    IF sy-subrc EQ 0.
      <fs_output>-work_done     = lv_work_done.
    ELSE.
      SELECT SINGLE ergsl
      INTO lv_temp_ergsl
      FROM fagl_011zc
      WHERE vonkt LE <fs_bsis3>-hkont
        AND biskt GE <fs_bsis3>-hkont.

      IF sy-subrc EQ 0.
        SELECT SINGLE rowdesc
        INTO lv_work_done
        FROM zfagl_011zc
        WHERE ergsl EQ lv_temp_ergsl.

        IF sy-subrc EQ 0.
          <fs_output>-work_done     = lv_work_done.
        ELSE.
          CONCATENATE 'UNCLASSIFIED' <fs_bsis3>-hkont 'Scenario 2' lv_temp_ergsl INTO <fs_output>-work_done SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
endform.                    " SCENARIO2
*&---------------------------------------------------------------------*
*&      Form  SCENARIO3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form SCENARIO3 .
  <fs_output>-amount_in_php = gv_netpay_dmbtr_amt.
  <fs_output>-amount        = gv_netpay_wrbtr_amt.
  <fs_output>-work_done     = 'Payment document was reversed'.
endform.                    " SCENARIO3