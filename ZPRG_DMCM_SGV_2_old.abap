*&---------------------------------------------------------------------*
*& Report  ZPRG_DMCM_SGV_2
*&
*&---------------------------------------------------------------------*
*&  Extraction of Bill Adjustment for Audit
*&
*&  09/30/2011    GDIMALIWAT    Created
*&  11/11/2011    GDIMALIWAT    Updated file write
*&
*&  Report Format is:
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

REPORT  zprg_dmcm_sgv_2.

TABLES: dfkkko, dfkkop.

DATA: BEGIN OF it_dmcm_load_add OCCURS 0,
    budat LIKE dfkkko-budat,
    opbel LIKE dfkkop-opbel,
    augbl LIKE dfkkop-augbl,
    hvorg LIKE dfkkop-hvorg,
    tvorg LIKE dfkkop-tvorg,
    betrw LIKE dfkkop-betrw,
    vkont LIKE dfkkop-vkont,
    gsber LIKE dfkkop-gsber,
    sctax LIKE dfkkop-sctax,
    xblnr LIKE dfkkop-xblnr,
    augrd LIKE dfkkop-augrd,
    blart LIKE dfkkko-blart,
    bldat LIKE dfkkko-bldat,
    cokey LIKE ever-cokey,
    gpart LIKE zmcf-body_nb,
  END OF it_dmcm_load_add.

DATA: BEGIN OF it_dmcm_load_neg OCCURS 0,
    budat LIKE dfkkko-budat,
    opbel LIKE dfkkop-opbel,
    augbl LIKE dfkkop-augbl,
    hvorg LIKE dfkkop-hvorg,
    tvorg LIKE dfkkop-tvorg,
    betrw LIKE dfkkop-betrw,
    vkont LIKE dfkkop-vkont,
    gsber LIKE dfkkop-gsber,
    sctax LIKE dfkkop-sctax,
    xblnr LIKE dfkkop-xblnr,
    augrd LIKE dfkkop-augrd,
    blart LIKE dfkkko-blart,
    bldat LIKE dfkkko-bldat,
    cokey LIKE ever-cokey,
    gpart LIKE zmcf-body_nb,
  END OF it_dmcm_load_neg.

DATA: BEGIN OF it_dmcm_all OCCURS 0,
    opbel  LIKE dfkkop-opbel,
    augbl LIKE dfkkop-augbl,
    vkont LIKE dfkkop-vkont,
    budat LIKE dfkkko-budat,
    betrw LIKE dfkkop-betrw,
    gsber LIKE dfkkop-gsber,
    sctax LIKE dfkkop-sctax,
    revbetrw LIKE dfkkop-betrw,
    revsctax LIKE dfkkop-sctax,
    xblnr LIKE dfkkop-xblnr,
    augrd LIKE dfkkop-augrd,
    blart LIKE dfkkko-blart,
    hvorg LIKE dfkkop-hvorg,
    tvorg LIKE dfkkop-tvorg,
    bldat LIKE dfkkko-bldat,
    cokey LIKE ever-cokey,
    gpart LIKE zmcf-body_nb,
    dmcmbill LIKE dfkkop-betrw,
    revdmcmbill LIKE dfkkop-betrw,
    dmcmmisc LIKE dfkkop-betrw,
    revdmcmmisc LIKE dfkkop-betrw,
    amount LIKE dfkkop-betrw,
    source(5),
  END OF it_dmcm_all.


SELECT-OPTIONS: s_budat FOR dfkkko-budat OBLIGATORY NO-EXTENSION.
PARAMETERS: datapath LIKE rlgrap-filename DEFAULT '\\localhost\dev\SGV\' OBLIGATORY.

* Get last day of s-budat month if the s_budat-high is not set
IF s_budat-high IS INITIAL.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = s_budat-low
    IMPORTING
      last_day_of_month = s_budat-high.
ENDIF.

WRITE:/ 'Extracted Bill Adjustment Report for ', s_budat-low, ' to ', s_budat-high.

CONCATENATE datapath 'BILLADJUSTMENT_' s_budat-low+4(2) s_budat-low(4) 'to_' s_budat-high+4(2) s_budat-high(4) '.txt' INTO datapath.

OPEN DATASET datapath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

IF sy-subrc = 0.

  SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
  INTO CORRESPONDING FIELDS OF it_dmcm_load_add
  FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~opbel
  WHERE ( dfkkop~opbel >= '000100000000' AND dfkkop~opbel < '000200000000' )
  AND ( dfkkko~budat >= s_budat-low AND dfkkko~budat <= s_budat-high ).
    APPEND it_dmcm_load_add.
  ENDSELECT.

  SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
  INTO CORRESPONDING FIELDS OF it_dmcm_load_neg
  FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~augbl
  WHERE ( dfkkop~augbl >= '000100000000' AND dfkkop~augbl < '000200000000' )
  AND ( dfkkko~budat >= s_budat-low AND dfkkko~budat <= s_budat-high ).
    APPEND it_dmcm_load_neg.
  ENDSELECT.

  LOOP AT it_dmcm_load_add.
    IF ( it_dmcm_load_add-opbel >= '000100000000' AND it_dmcm_load_add-opbel < '000200000000' )
         AND ( it_dmcm_load_add-hvorg = '0040' OR it_dmcm_load_add-hvorg = '0100' OR it_dmcm_load_add-hvorg = '0300' OR it_dmcm_load_add-hvorg = '0605' OR it_dmcm_load_add-hvorg = '6005' OR it_dmcm_load_add-hvorg = '0625' )
         AND it_dmcm_load_add-augrd <> '05'.
      CLEAR it_dmcm_all.

      SELECT SINGLE body_nb
      FROM zmcf
      INTO it_dmcm_all-gpart
      WHERE acct_nb = it_dmcm_load_add-vkont.

      SELECT SINGLE cokey
      FROM ever
      INTO it_dmcm_all-cokey
      WHERE vkonto = it_dmcm_load_add-vkont.

      it_dmcm_all-vkont = it_dmcm_load_add-vkont.
      it_dmcm_all-budat = it_dmcm_load_add-budat.
      it_dmcm_all-betrw = it_dmcm_load_add-betrw.
      it_dmcm_all-sctax = it_dmcm_load_add-sctax.
      it_dmcm_all-sctax = it_dmcm_load_add-sctax.
      it_dmcm_all-xblnr = it_dmcm_load_add-xblnr.
      it_dmcm_all-augrd = it_dmcm_load_add-augrd.
      it_dmcm_all-blart = it_dmcm_load_add-blart.
      it_dmcm_all-gsber = it_dmcm_load_add-gsber.
      it_dmcm_all-hvorg = it_dmcm_load_add-hvorg.
      it_dmcm_all-tvorg = it_dmcm_load_add-tvorg.
      it_dmcm_all-bldat = it_dmcm_load_add-bldat.

      it_dmcm_all-opbel = it_dmcm_load_add-opbel.
      it_dmcm_all-augbl = ''.
      it_dmcm_all-source = 'OPBEL'.

*   Get taxed amount only.
      it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
      it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
      it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

      APPEND it_dmcm_all.
    ENDIF.
    IF ( it_dmcm_load_add-opbel >= '000100000000' AND it_dmcm_load_add-opbel < '000200000000' )
         AND ( it_dmcm_load_add-hvorg <> '0100' AND it_dmcm_load_add-hvorg <> '0300' AND it_dmcm_load_add-hvorg <> '0605' AND it_dmcm_load_add-hvorg <> '6005' AND it_dmcm_load_add-hvorg <> '0625' AND it_dmcm_load_add-hvorg <> '0060'
               AND it_dmcm_load_add-hvorg <> '0600' AND it_dmcm_load_add-hvorg <> '0080' ) .
      CLEAR it_dmcm_all.

      SELECT SINGLE body_nb
      FROM zmcf
      INTO it_dmcm_all-gpart
      WHERE acct_nb = it_dmcm_load_add-vkont.

      SELECT SINGLE cokey
      FROM ever
      INTO it_dmcm_all-cokey
      WHERE vkonto = it_dmcm_load_add-vkont.

      it_dmcm_all-vkont = it_dmcm_load_add-vkont.
      it_dmcm_all-budat = it_dmcm_load_add-budat.
      it_dmcm_all-betrw = it_dmcm_load_add-betrw.
      it_dmcm_all-sctax = it_dmcm_load_add-sctax.
      it_dmcm_all-sctax = it_dmcm_load_add-sctax.
      it_dmcm_all-xblnr = it_dmcm_load_add-xblnr.
      it_dmcm_all-augrd = it_dmcm_load_add-augrd.
      it_dmcm_all-blart = it_dmcm_load_add-blart.
      it_dmcm_all-gsber = it_dmcm_load_add-gsber.
      it_dmcm_all-hvorg = it_dmcm_load_add-hvorg.
      it_dmcm_all-tvorg = it_dmcm_load_add-tvorg.
      it_dmcm_all-bldat = it_dmcm_load_add-bldat.

      it_dmcm_all-opbel = it_dmcm_load_add-opbel.
      it_dmcm_all-augbl = ''.
      it_dmcm_all-source = 'OPBEL'.

*   Get taxed amount only.
      it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
      it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
      it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

      APPEND it_dmcm_all.
    ENDIF.
  ENDLOOP.
  REFRESH it_dmcm_load_add.
  CLEAR it_dmcm_load_add.

  LOOP AT it_dmcm_load_neg.

    IF ( ( it_dmcm_load_neg-augbl >= '000100000000' AND it_dmcm_load_neg-augbl < '000200000000' ) OR ( it_dmcm_load_neg-augbl >= '000500000000' AND it_dmcm_load_neg-augbl < '000600000000' ) )
         AND ( it_dmcm_load_neg-hvorg = '0040' OR it_dmcm_load_neg-hvorg = '0100' OR it_dmcm_load_neg-hvorg = '0300' OR it_dmcm_load_neg-hvorg = '0605' OR it_dmcm_load_neg-hvorg = '6005' OR it_dmcm_load_neg-hvorg = '0625' )
         AND it_dmcm_load_neg-augrd <> '05'.
      CLEAR it_dmcm_all.

      SELECT SINGLE body_nb
      FROM zmcf
      INTO it_dmcm_all-gpart
      WHERE acct_nb = it_dmcm_load_neg-vkont.

      SELECT SINGLE cokey
      FROM ever
      INTO it_dmcm_all-cokey
      WHERE vkonto = it_dmcm_load_neg-vkont.

      it_dmcm_all-vkont =   it_dmcm_load_neg-vkont.
      it_dmcm_all-budat =   it_dmcm_load_neg-budat.
      it_dmcm_all-revbetrw = it_dmcm_load_neg-betrw * -1.
      it_dmcm_all-revsctax = it_dmcm_load_neg-sctax * -1.
      it_dmcm_all-sctax = it_dmcm_load_neg-sctax.
      it_dmcm_all-xblnr = it_dmcm_load_neg-xblnr.
      it_dmcm_all-augrd = it_dmcm_load_neg-augrd.
      it_dmcm_all-blart = it_dmcm_load_neg-blart.
      it_dmcm_all-gsber = it_dmcm_load_neg-gsber.
      it_dmcm_all-hvorg = it_dmcm_load_neg-hvorg.
      it_dmcm_all-tvorg = it_dmcm_load_neg-tvorg.
      it_dmcm_all-bldat = it_dmcm_load_neg-bldat.

      it_dmcm_all-opbel = it_dmcm_load_neg-augbl.
      it_dmcm_all-augbl = it_dmcm_load_neg-opbel.
      it_dmcm_all-source = 'AUGBL'.

*   Get taxed amount only.
      it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
      it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
      it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

      APPEND it_dmcm_all.
    ENDIF.
    IF ( ( it_dmcm_load_neg-augbl >= '000100000000' AND it_dmcm_load_neg-augbl < '000200000000' ) OR ( ( it_dmcm_load_neg-augbl >= '000500000000' AND it_dmcm_load_neg-augbl < '000600000000' )
          AND ( ( it_dmcm_load_neg-opbel >= '000700000000' AND it_dmcm_load_neg-opbel < '000800000000' ) ) ) )
        AND ( it_dmcm_load_neg-hvorg <> '0100' AND it_dmcm_load_neg-hvorg <> '0300' AND it_dmcm_load_neg-hvorg <> '9950' AND it_dmcm_load_neg-hvorg <> '0605' AND it_dmcm_load_neg-hvorg <> '6005'
          AND it_dmcm_load_neg-hvorg <> '0625' AND it_dmcm_load_neg-hvorg <> '0060' AND it_dmcm_load_neg-hvorg <> '0600' AND it_dmcm_load_neg-hvorg <> '0080' ) .
      CLEAR it_dmcm_all.

      SELECT SINGLE body_nb
      FROM zmcf
      INTO it_dmcm_all-gpart
      WHERE acct_nb = it_dmcm_load_neg-vkont.

      SELECT SINGLE cokey
      FROM ever
      INTO it_dmcm_all-cokey
      WHERE vkonto = it_dmcm_load_neg-vkont.

      it_dmcm_all-vkont =   it_dmcm_load_neg-vkont.
      it_dmcm_all-budat =   it_dmcm_load_neg-budat.
      it_dmcm_all-revbetrw = it_dmcm_load_neg-betrw * -1.
      it_dmcm_all-revsctax = it_dmcm_load_neg-sctax * -1.
      it_dmcm_all-sctax = it_dmcm_load_neg-sctax.
      it_dmcm_all-xblnr = it_dmcm_load_neg-xblnr.
      it_dmcm_all-augrd = it_dmcm_load_neg-augrd.
      it_dmcm_all-blart = it_dmcm_load_neg-blart.
      it_dmcm_all-gsber = it_dmcm_load_neg-gsber.
      it_dmcm_all-hvorg = it_dmcm_load_neg-hvorg.
      it_dmcm_all-tvorg = it_dmcm_load_neg-tvorg.
      it_dmcm_all-bldat = it_dmcm_load_neg-bldat.

      it_dmcm_all-opbel = it_dmcm_load_neg-augbl.
      it_dmcm_all-augbl = it_dmcm_load_neg-opbel.
      it_dmcm_all-source = 'AUGBL'.

*   Get taxed amount only.
      it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
      it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
      it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

      APPEND it_dmcm_all.
    ENDIF.
  ENDLOOP.
  REFRESH it_dmcm_load_neg.
  CLEAR it_dmcm_load_neg.

  SORT it_dmcm_all BY budat vkont opbel.

*  1. Business Partner
*  2. CAN
*  3. Branch Area
*  4. Posting Date
*  5. Document Type
*  6. Document Number
*  7. Document Date
*  8. Cost Center
*  9. Amount

*   CREATE OUTPUT TABLE *
  DATA: BEGIN OF v_string OCCURS 0,
    gpart(10), '|', "Business Partner
    vkont(12), '|', "Contract Account Number
    gsber(4), '|',  "Business Area
    budat(8), '|', "Posting Date
    blart(2), '|', "Document Type
    opbel(12), '|', "Document Number
    bldat(8), '|', "Document Date
    cokey(10), '|', "Cost Center
    amount(13), '|',"Amount
*    hkont(10), '|',"GL Account Number
*    hvorg(4), '|',"Main Trans
*    tvorg(4), '|',"Sub Trans
  END OF v_string.
*   END OF OUTPUT TABLE *

  COMMIT WORK.
  PERFORM write_to_file.
ENDIF.
**  WRITE TO FILE *
*DATA: write_count TYPE p VALUE 0.
*
*DO 100 TIMES.
*  OPEN DATASET datapath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*  IF sy-subrc = 0.
*    PERFORM write_to_file.
*    EXIT.
*  ELSE.
*    write_count = write_count + 1.
** SLEEP FOR 2 SECS *
*    CALL FUNCTION 'ENQUE_SLEEP'
*      EXPORTING
*        seconds = 2.
*  ENDIF.
*ENDDO.
*
*IF write_count > 0.
*  WRITE:/ 'The file',datapath,'could not be opened. The program failed to write',write_count,'times'.
*ELSE.
*  WRITE:/ 'The program successfully created a file',datapath,'without errors.'.
*ENDIF.
**   END OF WRITE TO FILE *

*&---------------------------------------------------------------------*
*&      Form  WRITE_TO_FILE
*&---------------------------------------------------------------------*
FORM write_to_file .
  LOOP AT it_dmcm_all.
    MOVE-CORRESPONDING it_dmcm_all TO v_string.
    TRANSFER v_string TO datapath.
  ENDLOOP.

  CLOSE DATASET datapath.
ENDFORM.                    " WRITE_TO_FILE
