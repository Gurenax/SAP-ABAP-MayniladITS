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

       BEGIN OF ty_bsak,
         shkzg TYPE bsak-shkzg,
         blart TYPE bsak-blart,
         lifnr TYPE bsak-lifnr,
         empfb TYPE bsak-empfb,
         zlsch TYPE bsak-zlsch,
         budat TYPE bsak-budat,
         waers TYPE bsak-waers,
         gjahr TYPE bsak-gjahr,
         belnr TYPE bsak-belnr,
         wrbtr TYPE bsak-wrbtr,
         bukrs TYPE bsak-bukrs,
       END OF ty_bsak,

       BEGIN OF ty_bsas,
         belnr TYPE bsas-belnr,
         augbl TYPE bsas-augbl,
         augdt TYPE bsas-augdt,
         gjahr TYPE bsas-gjahr,
         bukrs TYPE bsas-bukrs,
       END OF ty_bsas,

        BEGIN OF ty_bkpf,
          belnr TYPE bkpf-belnr,
          gjahr TYPE bkpf-gjahr,
          bukrs TYPE bkpf-bukrs,
          blart TYPE bkpf-blart,
        END OF ty_bkpf,

        BEGIN OF ty_bsis,
          hkont TYPE bsis-hkont,
          dmbtr TYPE bsis-dmbtr,
        END OF ty_bsis,
*       BEGIN OF ty_payr,
*         zbukr TYPE payr-zbukr,
*         lifnr TYPE payr-lifnr,
*         gjahr TYPE payr-gjahr,
*         vblnr TYPE payr-vblnr,
*         rwbtr TYPE payr-rwbtr,
*         waers TYPE payr-waers,
*       END OF ty_payr,

      BEGIN OF ty_output,
        vendor_code      TYPE c LENGTH 11,
        payee            TYPE c LENGTH 71,
        payment_method   TYPE c LENGTH 14,
        check_no         TYPE c LENGTH 13,
        payment_date     TYPE c LENGTH 12,
        currency         TYPE c LENGTH 8,
        amount           TYPE c LENGTH 16,
        amount_in_php    TYPE c LENGTH 16,
      END OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_blart_param_value     TYPE TABLE OF ty_blart_param,
      gt_blart_param_return    LIKE ddshretval OCCURS 0 WITH HEADER LINE.

DATA: gt_bsak   TYPE TABLE OF ty_bsak,
      "gt_payr   TYPE TABLE OF ty_payr,
      gt_output TYPE TABLE OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_blart_param  TYPE ty_blart_param.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_pipe TYPE c LENGTH 1 VALUE '|'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_bsak>   TYPE ty_bsak,
                <fs_bsas>   TYPE ty_bsas,
                <fs_bkpf>   TYPE ty_bkpf,
                <fs_bsis>   TYPE ty_bsis,
"<fs_payr>   TYPE ty_payr,
<fs_output> type ty_output.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS:     pa_bukrs     TYPE bsak-bukrs DEFAULT 'MWSI' OBLIGATORY.  "Co. Code
SELECT-OPTIONS: so_budat     FOR  bsak-budat OBLIGATORY,                 "Date / Date range
                so_shkzg     FOR  bsak-shkzg NO INTERVALS NO-EXTENSION,  "All payments and/or reversal of payments
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

  DATA: lt_name1     TYPE lfa1-name1,
        lt_name2     TYPE lfa1-name2,
        lt_neg_wrbtr TYPE bsak-wrbtr,
        lt_wrbtr     TYPE bsas-wrbtr,
        lt_dmbtr     TYPE bsas-dmbtr.

  DATA: lt_bsas      TYPE TABLE OF ty_bsas,
        lt_bkpf      TYPE TABLE OF ty_bkpf,
        lt_bsis      TYPE TABLE OF ty_bsis.

  IF so_blart IS NOT INITIAL.
    SELECT shkzg blart lifnr empfb zlsch budat waers gjahr belnr wrbtr bukrs
      INTO TABLE gt_bsak
      FROM bsak
      WHERE lifnr EQ '0021000326'
      AND bukrs EQ pa_bukrs
      AND budat IN so_budat
      AND shkzg IN so_shkzg
      AND blart IN so_blart
      AND zlsch IN so_zlsch.
  ELSE.
    SELECT shkzg blart lifnr empfb zlsch budat waers gjahr belnr wrbtr bukrs
      INTO TABLE gt_bsak
      FROM bsak
      WHERE lifnr EQ '0021000326'
      AND bukrs EQ pa_bukrs
      AND budat IN so_budat
      AND shkzg IN so_shkzg
      AND ( blart EQ 'ZP' OR blart EQ 'IF' )
      AND zlsch IN so_zlsch.
  ENDIF.

  IF sy-subrc EQ 0.

    LOOP AT gt_bsak ASSIGNING <fs_bsak>.
      REFRESH lt_bkpf.
      CLEAR lt_bkpf.
      REFRESH lt_bsas.
      CLEAR lt_bsas.
      REFRESH lt_bsis.
      CLEAR lt_bsis.
*      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
**      WRITE:/ 'SHKZG', <fs_bsak>-shkzg.
**      WRITE:/ 'BLART', <fs_bsak>-blart.
**      WRITE:/ 'LIFNR', <fs_bsak>-lifnr.
**      WRITE:/ 'EMPFB', <fs_bsak>-empfb.
**      WRITE:/ 'ZLSCH', <fs_bsak>-zlsch.
**      WRITE:/ 'BUDAT', <fs_bsak>-budat.
**      WRITE:/ 'WAERS', <fs_bsak>-waers.
*
*      <fs_output>-vendor_code = <fs_bsak>-lifnr.
*
*      "Payee
*      IF <fs_bsak>-empfb IS NOT INITIAL.
*        <fs_output>-payee = <fs_bsak>-empfb.
*      ELSE.
*        SELECT SINGLE name1 name2
*        INTO (lt_name1, lt_name2)
*        FROM lfa1
*        WHERE lifnr EQ <fs_bsak>-lifnr.
*
*        IF sy-subrc EQ 0.
*          CONCATENATE lt_name1 lt_name2 INTO <fs_output>-payee SEPARATED BY space.
*        ENDIF.
*      ENDIF.
*
*      "Negate WRBTR
*      lt_neg_wrbtr = <fs_bsak>-wrbtr * -1.
*
*      "Payment method / Check No.
*      IF <fs_bsak>-zlsch IS NOT INITIAL.
*        <fs_output>-payment_method = <fs_bsak>-zlsch.
*
*        SELECT SINGLE chect
*        INTO <fs_output>-check_no
*        FROM payr
*        WHERE zbukr EQ pa_bukrs
*          AND lifnr EQ <fs_bsak>-lifnr
*          AND gjahr EQ <fs_bsak>-gjahr
*          AND vblnr EQ <fs_bsak>-belnr
*          AND rwbtr EQ lt_neg_wrbtr
*          AND waers EQ <fs_bsak>-waers.
*      ELSE.
*        SELECT SINGLE rzawe chect
*        INTO (<fs_output>-payment_method, <fs_output>-check_no)
*        FROM payr
*        WHERE zbukr EQ pa_bukrs
*          AND lifnr EQ <fs_bsak>-lifnr
*          AND gjahr EQ <fs_bsak>-gjahr
*          AND vblnr EQ <fs_bsak>-belnr
*          AND rwbtr EQ lt_neg_wrbtr
*          AND waers EQ <fs_bsak>-waers.
*      ENDIF.
*
*      "Payment Date
*      <fs_output>-payment_date = <fs_bsak>-budat.
*
*      "Currency
*      <fs_output>-currency = <fs_bsak>-waers.
*
*      "Amount / Amount in PHP
*      SELECT SINGLE wrbtr dmbtr
*        INTO (lt_wrbtr, lt_dmbtr)
*        FROM bsas
*        WHERE bukrs EQ pa_bukrs
*          AND gjahr EQ <fs_bsak>-gjahr
*          AND belnr EQ <fs_bsak>-belnr
*          AND budat IN so_budat
*          AND blart IN so_blart
*          AND waers EQ <fs_bsak>-waers.
*      IF sy-subrc eq 0.
*        <fs_output>-amount = lt_wrbtr.
*        <fs_output>-amount_in_php = lt_dmbtr.
*      ENDIF.


      "Work Done
      APPEND INITIAL LINE TO lt_bsas ASSIGNING <fs_bsas>.
      SELECT SINGLE belnr augbl augdt gjahr bukrs
        INTO (<fs_bsas>-belnr, <fs_bsas>-augbl, <fs_bsas>-augdt, <fs_bsas>-gjahr, <fs_bsas>-bukrs)
        FROM bsas
        WHERE augbl EQ <fs_bsak>-belnr
          AND belnr NE <fs_bsak>-belnr
          AND augdt EQ <fs_bsak>-budat
          AND bukrs EQ <fs_bsak>-bukrs.

      IF sy-subrc EQ 0.
        WRITE:/ <fs_bsas>-belnr, <fs_bsas>-augbl, <fs_bsas>-augdt, <fs_bsas>-gjahr, <fs_bsas>-bukrs.

        APPEND INITIAL LINE TO lt_bkpf ASSIGNING <fs_bkpf>.
        SELECT SINGLE belnr gjahr bukrs blart
          INTO (<fs_bkpf>-belnr, <fs_bkpf>-gjahr, <fs_bkpf>-bukrs, <fs_bkpf>-blart)
          FROM bkpf
          WHERE belnr EQ <fs_bsas>-belnr
            AND gjahr EQ <fs_bsas>-gjahr
            AND bukrs EQ <fs_bsas>-bukrs.

        IF sy-subrc EQ 0.
          WRITE:/ <fs_bkpf>-belnr, <fs_bkpf>-gjahr, <fs_bkpf>-bukrs, <fs_bkpf>-blart.

          IF <fs_bkpf>-blart EQ 'ZP'.
            WRITE:/ 'Payment document reversal'.
          ELSE.
            SELECT hkont sum( dmbtr )
              INTO TABLE lt_bsis
              FROM bsis
              WHERE bukrs EQ <fs_bsas>-bukrs
                AND belnr EQ <fs_bsas>-belnr
                AND gjahr EQ <fs_bsas>-gjahr
                AND shkzg EQ 'S'
                AND ( buzid NE 'T' OR buzid NE 'S')
              GROUP BY hkont
              ORDER BY hkont ASCENDING.

            IF sy-subrc EQ 0.
              LOOP AT lt_bsis ASSIGNING <fs_bsis>.
                WRITE:/ <fs_bsis>-hkont, <fs_bsis>-dmbtr.
              ENDLOOP.
              UNASSIGN <fs_bsis>.
            ENDIF.
          ENDIF.

          UNASSIGN <fs_bkpf>.
        ENDIF.

        UNASSIGN <fs_bsas>.
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
    lt_header-vendor_code    = 'VENDOR CODE'.
    lt_header-payee          = 'PAYEE'.
    lt_header-payment_method = 'PAYMENT METHOD'.
    lt_header-check_no       = 'CHECK NO'.
    lt_header-payment_date   = 'PAYMENT DATE'.
    lt_header-currency       = 'CURRENCY'.
    lt_header-amount         = 'AMOUNT'.
    lt_header-amount_in_php  = 'AMOUNT IN PHP'.

    INSERT lt_header INTO gt_output INDEX 1.
    LOOP AT gt_output ASSIGNING <fs_output>.
      WRITE:/ <fs_output>-vendor_code, <fs_output>-payee, <fs_output>-payment_method, <fs_output>-check_no, <fs_output>-payment_date, <fs_output>-currency, <fs_output>-amount, <fs_output>-amount_in_php.
      CONCATENATE <fs_output>-vendor_code <fs_output>-payee <fs_output>-payment_method <fs_output>-check_no <fs_output>-payment_date <fs_output>-currency <fs_output>-amount <fs_output>-amount_in_php INTO lt_output SEPARATED BY co_pipe RESPECTING BLANKS.
      TRANSFER lt_output TO pa_file.
    ENDLOOP.
  ENDIF.
  CLOSE DATASET pa_file.
ENDFORM.                    " EXTRACT_OUTPUT