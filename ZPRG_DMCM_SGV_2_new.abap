*&---------------------------------------------------------------------*
*& Report  ZPRG_DMCM_SGV_2
*& DESCRIPTION: Extraction of Bill Adjustment Details
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 09/30/2011
*& Reference  : 46861
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/11/25| GDIMALIWAT |46861      | Revised program for Maynilad ABAP Standards
*&---------------------------------------------------------------------*
*&
*&  Report format is:
*&  gpart(10), '|', "Business Partner
*&  vkont(12), '|', "Contract Account Number
*&  gsber(4), '|',  "Business Area
*&  budat(8), '|', "Posting Date
*&  blart(2), '|', "Document Type
*&  opbel(12), '|', "Document Number
*&  bldat(8), '|', "Document Date
*&  cokey(10), '|', "Cost Center
*&  amount(13), '|',"Amount
*&---------------------------------------------------------------------*

REPORT zprg_dmcm_sgv_2 MESSAGE-ID zisu.
*INCLUDE
TABLES: dfkkko, dfkkop.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_dmcm_load_add,
        budat           TYPE dfkkko-budat,
        opbel           TYPE dfkkop-opbel,
        augbl           TYPE dfkkop-augbl,
        hvorg           TYPE dfkkop-hvorg,
        tvorg           TYPE dfkkop-tvorg,
        betrw           TYPE dfkkop-betrw,
        vkont           TYPE dfkkop-vkont,
        gsber           TYPE dfkkop-gsber,
        sctax           TYPE dfkkop-sctax,
        xblnr           TYPE dfkkop-xblnr,
        augrd           TYPE dfkkop-augrd,
        blart           TYPE dfkkko-blart,
      END OF ty_dmcm_load_add,

      BEGIN OF ty_dmcm_load_neg,
        budat           TYPE dfkkko-budat,
        opbel           TYPE dfkkop-opbel,
        augbl           TYPE dfkkop-augbl,
        hvorg           TYPE dfkkop-hvorg,
        tvorg           TYPE dfkkop-tvorg,
        betrw           TYPE dfkkop-betrw,
        vkont           TYPE dfkkop-vkont,
        gsber           TYPE dfkkop-gsber,
        sctax           TYPE dfkkop-sctax,
        xblnr           TYPE dfkkop-xblnr,
        augrd           TYPE dfkkop-augrd,
        blart           TYPE dfkkko-blart,
      END OF ty_dmcm_load_neg,

      BEGIN OF ty_dmcm_all,
        opbel           TYPE dfkkop-opbel,
        vkont           TYPE dfkkop-vkont,
        budat           TYPE dfkkko-budat,
        betrw           TYPE dfkkop-betrw,
        gsber           TYPE dfkkop-gsber,
        sctax           TYPE dfkkop-sctax,
        revbetrw        TYPE dfkkop-betrw,
        revsctax        TYPE dfkkop-sctax,
        blart           TYPE dfkkko-blart,
        bldat           TYPE dfkkko-bldat,
        cokey           TYPE ever-cokey,
        gpart           TYPE fkkvkp-gpart,
        dmcmbill        TYPE dfkkop-betrw,
        revdmcmbill     TYPE dfkkop-betrw,
        dmcmmisc        TYPE dfkkop-betrw,
        revdmcmmisc     TYPE dfkkop-betrw,
        amount          TYPE dfkkop-betrw,
      END OF ty_dmcm_all.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_dmcm_load_add     TYPE TABLE OF ty_dmcm_load_add,
      gt_dmcm_load_neg     TYPE TABLE OF ty_dmcm_load_neg,
      gt_dmcm_all          TYPE TABLE OF ty_dmcm_all.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_path  TYPE rlgrap-filename.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_prefix type string value 'BILLADJUSTMENT_',
            co_ext    type string value '.txt'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_dmcm_load_add> TYPE ty_dmcm_load_add,
                <fs_dmcm_load_neg> TYPE ty_dmcm_load_neg,
                <fs_dmcm_all>      TYPE ty_dmcm_all.
*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
SELECT-OPTIONS: so_budat FOR  dfkkko-budat OBLIGATORY NO INTERVALS NO-EXTENSION.
PARAMETERS:     pa_path  TYPE rlgrap-filename DEFAULT '\\localhost\dev\SGV\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*AT SELECTION-SCREEN ON <para|selcrit>
*
*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& Initialization
*&
*INITIALIZATION.

*&--------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  "-- Get last day of s-budat month if the s_budat-high is not set
  IF so_budat-high IS INITIAL.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = so_budat-low
      IMPORTING
        last_day_of_month = so_budat-high.
  ENDIF.

  "-- Create path to file
  CONCATENATE pa_path co_prefix so_budat-low+4(2) so_budat-low(4) co_ext INTO gv_path.

  PERFORM begin_extract.
  WRITE:/ text-001 , so_budat-low, text-002, so_budat-high. "Extracted DMCM Report from date-low to date-high
  PERFORM write_to_file.
*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  WRITE_TO_FILE
*&---------------------------------------------------------------------*
FORM write_to_file .

  DATA: lv_string     TYPE string,
        lv_amount_str TYPE string,
        lv_count      TYPE i VALUE 0.

  DO 100 TIMES.
    OPEN DATASET gv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      ADD 1 TO lv_count.
    ENDIF.
  ENDDO.
  IF sy-subrc EQ 0.
    LOOP AT gt_dmcm_all ASSIGNING <fs_dmcm_all>.
      CLEAR lv_string.
      CLEAR lv_amount_str.

      lv_amount_str = <fs_dmcm_all>-amount.

      CONCATENATE <fs_dmcm_all>-gpart <fs_dmcm_all>-vkont <fs_dmcm_all>-gsber <fs_dmcm_all>-budat
                  <fs_dmcm_all>-blart <fs_dmcm_all>-opbel <fs_dmcm_all>-bldat <fs_dmcm_all>-cokey
                  lv_amount_str INTO lv_string SEPARATED BY '|' RESPECTING BLANKS.
      TRANSFER lv_string TO gv_path.
    ENDLOOP.
    UNASSIGN <fs_dmcm_all>.
    WRITE:/ text-003, gv_path.  "The program successfully created a file
    CLOSE DATASET gv_path.
  ELSE.
    WRITE:/ text-004, gv_path, text-005, lv_count, text-006.  "The file could not be opened. Failed x times.
  ENDIF.

ENDFORM.                    " WRITE_TO_FILE

*&---------------------------------------------------------------------*
*&      Form  BEGIN_EXTRACT
*&---------------------------------------------------------------------*
*       Begin the extraction
*----------------------------------------------------------------------*
FORM begin_extract .

  DATA: lv_gpart        TYPE fkkvkp-gpart,
        lv_cokey        TYPE ever-cokey.

  SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
  INTO TABLE gt_dmcm_load_add
  FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~opbel
  WHERE ( dfkkop~opbel >= '000100000000' AND dfkkop~opbel < '000200000000' )
  AND ( dfkkko~budat >= so_budat-low AND dfkkko~budat <= so_budat-high ).

  IF sy-subrc EQ 0.

    LOOP AT gt_dmcm_load_add ASSIGNING <fs_dmcm_load_add>.
      CLEAR: lv_gpart,
             lv_cokey.

      SELECT SINGLE gpart
      INTO lv_gpart
      FROM fkkvkp
      WHERE vkont EQ <fs_dmcm_load_add>-vkont.

      SELECT SINGLE cokey
      INTO lv_cokey
      FROM ever
      WHERE vkonto = <fs_dmcm_load_add>-vkont.

*      SELECT SINGLE body_nb prprty_ty ty_connect
*      INTO (lv_gpart, lv_billingclass, lv_ratecode)
*      FROM zmcf
*      WHERE acct_nb EQ <fs_dmcm_load_add>-vkont.

      IF ( <fs_dmcm_load_add>-opbel >= '000100000000' AND <fs_dmcm_load_add>-opbel < '000200000000' )
           AND ( <fs_dmcm_load_add>-hvorg = '0040' OR <fs_dmcm_load_add>-hvorg = '0100' OR <fs_dmcm_load_add>-hvorg = '0300' OR <fs_dmcm_load_add>-hvorg = '0605' OR <fs_dmcm_load_add>-hvorg = '6005' OR <fs_dmcm_load_add>-hvorg = '0625' )
           AND <fs_dmcm_load_add>-augrd <> '05'.

        APPEND INITIAL LINE TO gt_dmcm_all ASSIGNING <fs_dmcm_all>.

        <fs_dmcm_all>-vkont = <fs_dmcm_load_add>-vkont.
        <fs_dmcm_all>-budat = <fs_dmcm_load_add>-budat.
        <fs_dmcm_all>-betrw = <fs_dmcm_load_add>-betrw.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_add>-sctax.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_add>-sctax.
        <fs_dmcm_all>-blart = <fs_dmcm_load_add>-blart.
        <fs_dmcm_all>-gsber = <fs_dmcm_load_add>-gsber.
        <fs_dmcm_all>-gpart = lv_gpart.
        <fs_dmcm_all>-opbel = <fs_dmcm_load_add>-opbel.
        <fs_dmcm_all>-cokey = lv_cokey.

        "---  Get taxed amount only.
        <fs_dmcm_all>-dmcmbill = <fs_dmcm_all>-betrw.
        <fs_dmcm_all>-revdmcmbill = <fs_dmcm_all>-revbetrw.
        <fs_dmcm_all>-amount = <fs_dmcm_all>-dmcmbill + <fs_dmcm_all>-revdmcmbill.
      ENDIF.
      IF ( <fs_dmcm_load_add>-opbel >= '000100000000' AND <fs_dmcm_load_add>-opbel < '000200000000' )
           AND ( <fs_dmcm_load_add>-hvorg <> '0100' AND <fs_dmcm_load_add>-hvorg <> '0300' AND <fs_dmcm_load_add>-hvorg <> '0605' AND <fs_dmcm_load_add>-hvorg <> '6005' AND <fs_dmcm_load_add>-hvorg <> '0625' AND <fs_dmcm_load_add>-hvorg <> '0060'
                 AND <fs_dmcm_load_add>-hvorg <> '0600' AND <fs_dmcm_load_add>-hvorg <> '0080' ) .

        APPEND INITIAL LINE TO gt_dmcm_all ASSIGNING <fs_dmcm_all>.
        <fs_dmcm_all>-vkont = <fs_dmcm_load_add>-vkont.
        <fs_dmcm_all>-budat = <fs_dmcm_load_add>-budat.
        <fs_dmcm_all>-betrw = <fs_dmcm_load_add>-betrw.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_add>-sctax.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_add>-sctax.
        <fs_dmcm_all>-blart = <fs_dmcm_load_add>-blart.
        <fs_dmcm_all>-gsber = <fs_dmcm_load_add>-gsber.
        <fs_dmcm_all>-gpart = lv_gpart.
        <fs_dmcm_all>-opbel = <fs_dmcm_load_add>-opbel.
        <fs_dmcm_all>-cokey = lv_cokey.

*     Get taxed amount only.
        <fs_dmcm_all>-dmcmbill = <fs_dmcm_all>-betrw.
        <fs_dmcm_all>-revdmcmbill = <fs_dmcm_all>-revbetrw.
        <fs_dmcm_all>-amount = <fs_dmcm_all>-dmcmbill + <fs_dmcm_all>-revdmcmbill.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_dmcm_load_add>.
    REFRESH gt_dmcm_load_add.
    CLEAR gt_dmcm_load_add.
  ENDIF.


  SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
  INTO TABLE gt_dmcm_load_neg
  FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~augbl
  WHERE ( dfkkop~augbl >= '000100000000' AND dfkkop~augbl < '000200000000' )
  AND ( dfkkko~budat >= so_budat-low AND dfkkko~budat <= so_budat-high ).

  IF sy-subrc EQ 0.
    LOOP AT gt_dmcm_load_neg ASSIGNING <fs_dmcm_load_neg>.

      CLEAR: lv_gpart,
             lv_cokey.

      SELECT SINGLE gpart
      INTO lv_gpart
      FROM fkkvkp
      WHERE vkont EQ <fs_dmcm_load_neg>-vkont.

      SELECT SINGLE cokey
      INTO lv_cokey
      FROM ever
      WHERE vkonto = <fs_dmcm_load_neg>-vkont.

*      SELECT SINGLE body_nb prprty_ty ty_connect
*      INTO (lv_gpart, lv_billingclass, lv_ratecode)
*      FROM zmcf
*      WHERE acct_nb EQ <fs_dmcm_load_neg>-vkont.

      IF ( ( <fs_dmcm_load_neg>-augbl >= '000100000000' AND <fs_dmcm_load_neg>-augbl < '000200000000' ) OR ( <fs_dmcm_load_neg>-augbl >= '000500000000' AND <fs_dmcm_load_neg>-augbl < '000600000000' ) )
           AND ( <fs_dmcm_load_neg>-hvorg = '0040' OR <fs_dmcm_load_neg>-hvorg = '0100' OR <fs_dmcm_load_neg>-hvorg = '0300' OR <fs_dmcm_load_neg>-hvorg = '0605' OR <fs_dmcm_load_neg>-hvorg = '6005' OR <fs_dmcm_load_neg>-hvorg = '0625' )
           AND <fs_dmcm_load_neg>-augrd <> '05'.

        APPEND INITIAL LINE TO gt_dmcm_all ASSIGNING <fs_dmcm_all>.
        <fs_dmcm_all>-vkont =   <fs_dmcm_load_neg>-vkont.
        <fs_dmcm_all>-budat =   <fs_dmcm_load_neg>-budat.
        <fs_dmcm_all>-revbetrw = <fs_dmcm_load_neg>-betrw * -1.
        <fs_dmcm_all>-revsctax = <fs_dmcm_load_neg>-sctax * -1.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_neg>-sctax.
        <fs_dmcm_all>-blart = <fs_dmcm_load_neg>-blart.
        <fs_dmcm_all>-gsber = <fs_dmcm_load_neg>-gsber.
        <fs_dmcm_all>-gpart = lv_gpart.
        <fs_dmcm_all>-opbel = <fs_dmcm_load_neg>-augbl.
        <fs_dmcm_all>-cokey = lv_cokey.

*     Get taxed amount only.
        <fs_dmcm_all>-dmcmbill = <fs_dmcm_all>-betrw.
        <fs_dmcm_all>-revdmcmbill = <fs_dmcm_all>-revbetrw.
        <fs_dmcm_all>-amount = <fs_dmcm_all>-dmcmbill + <fs_dmcm_all>-revdmcmbill.
      ENDIF.
      IF ( ( <fs_dmcm_load_neg>-augbl >= '000100000000' AND <fs_dmcm_load_neg>-augbl < '000200000000' ) OR ( ( <fs_dmcm_load_neg>-augbl >= '000500000000' AND <fs_dmcm_load_neg>-augbl < '000600000000' )
            AND ( ( <fs_dmcm_load_neg>-opbel >= '000700000000' AND <fs_dmcm_load_neg>-opbel < '000800000000' ) ) ) )
          AND ( <fs_dmcm_load_neg>-hvorg <> '0100' AND <fs_dmcm_load_neg>-hvorg <> '0300' AND <fs_dmcm_load_neg>-hvorg <> '9950' AND <fs_dmcm_load_neg>-hvorg <> '0605' AND <fs_dmcm_load_neg>-hvorg <> '6005'
            AND <fs_dmcm_load_neg>-hvorg <> '0625' AND <fs_dmcm_load_neg>-hvorg <> '0060' AND <fs_dmcm_load_neg>-hvorg <> '0600' AND <fs_dmcm_load_neg>-hvorg <> '0080' ) .

        APPEND INITIAL LINE TO gt_dmcm_all ASSIGNING <fs_dmcm_all>.
        <fs_dmcm_all>-vkont =   <fs_dmcm_load_neg>-vkont.
        <fs_dmcm_all>-budat =   <fs_dmcm_load_neg>-budat.
        <fs_dmcm_all>-revbetrw = <fs_dmcm_load_neg>-betrw * -1.
        <fs_dmcm_all>-revsctax = <fs_dmcm_load_neg>-sctax * -1.
        <fs_dmcm_all>-sctax = <fs_dmcm_load_neg>-sctax.
        <fs_dmcm_all>-blart = <fs_dmcm_load_neg>-blart.
        <fs_dmcm_all>-gsber = <fs_dmcm_load_neg>-gsber.
        <fs_dmcm_all>-gpart = lv_gpart.
        <fs_dmcm_all>-opbel = <fs_dmcm_load_neg>-augbl.
        <fs_dmcm_all>-cokey = lv_cokey.

*     Get taxed amount only.
        <fs_dmcm_all>-dmcmbill = <fs_dmcm_all>-betrw.
        <fs_dmcm_all>-revdmcmbill = <fs_dmcm_all>-revbetrw.
        <fs_dmcm_all>-amount = <fs_dmcm_all>-dmcmbill + <fs_dmcm_all>-revdmcmbill.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_dmcm_load_neg>.
    REFRESH gt_dmcm_load_neg.
    CLEAR gt_dmcm_load_neg.
  ENDIF.

  SORT gt_dmcm_all BY budat vkont opbel.
ENDFORM.                    " BEGIN_EXTRACT