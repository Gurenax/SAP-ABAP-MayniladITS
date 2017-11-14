*&---------------------------------------------------------------------*
*& Report  ZPRG_DMCM_SGV
*&
*&---------------------------------------------------------------------*
*&  Extraction of DMCM Details
*&
*&  09/08/2011    GDIMALIWAT    Created
*&  09/30/2011    GDIMALIWAT    Added new fields based on TA
*&  10/03/2011    GDIMALIWAT    Added new fields based on Task Assignment
*&  10/05/2011    GDIMALIWAT    Updated select statements and select sing statements
*&  10/07/2011    GDIMALIWAT    Updated billing class
*&  10/11/2011    GDIMALIWAT    Removed DC from conditions
*&  10/12/2011    GDIMALIWAT    Removed DFKKOP~OPBEL Series 0005 from conditions
*&  10/13/2011    GDIMALIWAT    Added source column for OPBEL table or AUGBL table
*&
*&  Report format is:
*&  gpart(10), '|',        "Business Partner
*&  vkont(12), '|',        "Contract Account Number
*&  gsber(4), '|',         "Business Area
*&  billingclass(4), '|',  "Billing Class
*&  ratecode(10), '|',     "Rate Code
*&  augbl(12), '|',        "Clearing Document Number
*&  amount(13), '|',       "Amount
*&  budat(8), '|',         "Posting Date
*&  opbel(12), '|',        "Payment Document Number
*&  source(5), '|',        "Source
*&---------------------------------------------------------------------*

REPORT  zprg_dmcm_sgv.

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
    billingclass LIKE zmcf-prprty_ty,
    gpart LIKE zmcf-body_nb,
*    belnr LIKE erch-belnr,
    billdoc LIKE dfkkop-opbel,
    ratecode LIKE zmcf-ty_connect,
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
    billingclass LIKE zmcf-prprty_ty,
    gpart LIKE zmcf-body_nb,
*    belnr LIKE erch-belnr,
    billdoc LIKE dfkkop-opbel,
    ratecode LIKE zmcf-ty_connect,
  END OF it_dmcm_load_neg.

DATA: BEGIN OF it_dmcm_all OCCURS 0,
    opbel  LIKE dfkkop-opbel,
    augbl LIKE dfkkop-augbl,
*    name(8) TYPE c,
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
    billingclass LIKE zmcf-prprty_ty,
    gpart LIKE zmcf-body_nb,
*    belnr LIKE erch-belnr,
    billdoc LIKE dfkkop-opbel,
    ratecode LIKE zmcf-ty_connect,
    dmcmbill LIKE dfkkop-betrw,
    revdmcmbill LIKE dfkkop-betrw,
    dmcmmisc LIKE dfkkop-betrw,
    revdmcmmisc LIKE dfkkop-betrw,
    amount LIKE dfkkop-betrw,
    source(5),
  END OF it_dmcm_all.


*DATA: v_xblnr_temp type dfkkop-xblnr,
*v_xblnr(12).

SELECT-OPTIONS: s_budat FOR dfkkko-budat OBLIGATORY NO INTERVALS NO-EXTENSION.
PARAMETERS: datapath LIKE rlgrap-filename DEFAULT '\\localhost\dev\SGV\' OBLIGATORY.

* Get last day of s-budat month if the s_budat-high is not set
IF s_budat-high IS INITIAL.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = s_budat-low
    IMPORTING
      last_day_of_month = s_budat-high.
ENDIF.

WRITE:/ 'Extracted DMCM Report for ', s_budat-low, ' to ', s_budat-high.

CONCATENATE datapath 'DMCM_' s_budat-low+4(2) s_budat-low(4) '.txt' INTO datapath.


SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
INTO CORRESPONDING FIELDS OF it_dmcm_load_add
FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~opbel
WHERE ( dfkkop~opbel >= '000100000000' AND dfkkop~opbel < '000200000000' )
* OR ( dfkkop~opbel >= '000500000000' AND dfkkop~opbel < '000600000000' ) )
AND ( dfkkko~budat >= s_budat-low AND dfkkko~budat <= s_budat-high ).
*AND dfkkop~blart = 'DC'.
  APPEND it_dmcm_load_add.
ENDSELECT.

SELECT dfkkko~budat dfkkop~opbel dfkkop~augbl dfkkop~hvorg dfkkop~tvorg dfkkop~betrw dfkkop~vkont dfkkop~gsber dfkkop~sctax dfkkop~xblnr dfkkop~augrd dfkkop~blart
INTO CORRESPONDING FIELDS OF it_dmcm_load_neg
FROM dfkkko INNER JOIN dfkkop ON dfkkko~opbel = dfkkop~augbl
WHERE ( dfkkop~augbl >= '000100000000' AND dfkkop~augbl < '000200000000' )
AND ( dfkkko~budat >= s_budat-low AND dfkkko~budat <= s_budat-high ).
*AND dfkkop~blart = 'DC'.
  APPEND it_dmcm_load_neg.
ENDSELECT.

LOOP AT it_dmcm_load_add.

  SELECT SINGLE body_nb prprty_ty ty_connect
  INTO  (it_dmcm_load_add-gpart, it_dmcm_load_add-billingclass, it_dmcm_load_add-ratecode)
  FROM zmcf
  WHERE acct_nb = it_dmcm_load_add-vkont.

*  IF it_dmcm_load_add-augbl <> ''.
*    CLEAR v_xblnr_temp.
*    CLEAR v_xblnr.

*    SELECT SINGLE opbel hvorg tvorg
*    FROM DFKKOP
*    INTO (it_dmcm_load_add-billdoc, it_dmcm_load_add-hvorg, it_dmcm_load_add-tvorg)
*    WHERE augbl = it_dmcm_load_add-augbl
*    AND blart = 'FA'
*    AND xblnr <> ''.

*    SELECT SINGLE xblnr hvorg tvorg
*    FROM DFKKOP
*    INTO (v_xblnr_temp, it_dmcm_load_add-hvorg, it_dmcm_load_add-tvorg)
*    WHERE augbl = it_dmcm_load_add-augbl
*    AND blart = 'FA'
*    AND xblnr <> ''.

*    v_xblnr = v_xblnr_temp+4(12).
*
*    IF v_xblnr <> '' AND v_xblnr <> '999999999999'.
*
*      SELECT SINGLE belnr
*      INTO it_dmcm_load_add-belnr
*      FROM erchc WHERE opbel = v_xblnr.
*
*      IF it_dmcm_load_add-belnr = '' AND it_dmcm_load_add-augbl <> ''.
*        SELECT SINGLE belnr
*        INTO  it_dmcm_load_add-belnr
*        FROM zmwosb_psr WHERE opbel = it_dmcm_load_add-augbl.
*      ENDIF.
*
*    ELSEIF v_xblnr = '' OR v_xblnr = '999999999999'.
*      IF it_dmcm_load_add-augbl <> ''.
*        SELECT SINGLE belnr
*        INTO  it_dmcm_load_add-belnr
*        FROM zmwosb_psr WHERE opbel = it_dmcm_load_add-augbl.
*      ENDIF.
*    ENDIF.

*  ENDIF.

*  IF  it_dmcm_load_add-belnr <> ''.
*    SELECT SINGLE tarifnr
*    INTO  it_dmcm_load_add-ratecode
*    FROM dberchz1
*    WHERE belnr =  it_dmcm_load_add-belnr
*    AND ( tvorg = '0021' OR tvorg = '0011' )
*    AND buchrel = 'X'.
*
*    IF  it_dmcm_load_add-ratecode <> ''.
*      SELECT SINGLE AKLASSE
*      INTO  it_dmcm_load_add-billingclass
*      FROM ETRF
*      WHERE tarifnr =  it_dmcm_load_add-ratecode.
*    ENDIF.
*  ENDIF.

*  IF ( ( it_dmcm_load_add-opbel >= '000100000000' AND it_dmcm_load_add-opbel < '000200000000' ) OR ( it_dmcm_load_add-opbel >= '000500000000' AND it_dmcm_load_add-opbel < '000600000000' ) )\
  IF ( it_dmcm_load_add-opbel >= '000100000000' AND it_dmcm_load_add-opbel < '000200000000' )
       AND ( it_dmcm_load_add-hvorg = '0040' OR it_dmcm_load_add-hvorg = '0100' OR it_dmcm_load_add-hvorg = '0300' OR it_dmcm_load_add-hvorg = '0605' OR it_dmcm_load_add-hvorg = '6005' OR it_dmcm_load_add-hvorg = '0625' )
       AND it_dmcm_load_add-augrd <> '05'.
    CLEAR it_dmcm_all.
*    it_dmcm_all-name = 'DMCMBill'.
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
    it_dmcm_all-billingclass = it_dmcm_load_add-billingclass.
    it_dmcm_all-gpart = it_dmcm_load_add-gpart.
*    it_dmcm_all-belnr = it_dmcm_load_add-belnr.
    it_dmcm_all-ratecode = it_dmcm_load_add-ratecode.

    it_dmcm_all-opbel = it_dmcm_load_add-opbel.
    it_dmcm_all-augbl = ''.
    it_dmcm_all-source = 'OPBEL'.

*    it_dmcm_all-dmcmbill = it_dmcm_all-betrw - it_dmcm_all-sctax.
*    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw - it_dmcm_all-revsctax.
* Get taxed amount only.
    it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
    it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

    APPEND it_dmcm_all.
  ENDIF.
  IF ( it_dmcm_load_add-opbel >= '000100000000' AND it_dmcm_load_add-opbel < '000200000000' )
       AND ( it_dmcm_load_add-hvorg <> '0100' AND it_dmcm_load_add-hvorg <> '0300' AND it_dmcm_load_add-hvorg <> '0605' AND it_dmcm_load_add-hvorg <> '6005' AND it_dmcm_load_add-hvorg <> '0625' AND it_dmcm_load_add-hvorg <> '0060'
             AND it_dmcm_load_add-hvorg <> '0600' AND it_dmcm_load_add-hvorg <> '0080' ) .
    CLEAR it_dmcm_all.
*    it_dmcm_all-name = 'DMCMMisc'.
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
    it_dmcm_all-billingclass = it_dmcm_load_add-billingclass.
    it_dmcm_all-gpart = it_dmcm_load_add-gpart.
*    it_dmcm_all-belnr = it_dmcm_load_add-belnr.
    it_dmcm_all-ratecode = it_dmcm_load_add-ratecode.

    it_dmcm_all-opbel = it_dmcm_load_add-opbel.
    it_dmcm_all-augbl = ''.
*    it_dmcm_all-augbl = it_dmcm_load_add-augbl.
    it_dmcm_all-source = 'OPBEL'.

*    it_dmcm_all-dmcmbill = it_dmcm_all-betrw - it_dmcm_all-sctax.
*    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw - it_dmcm_all-revsctax.
* Get taxed amount only.
    it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
    it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

    APPEND it_dmcm_all.
  ENDIF.
ENDLOOP.
REFRESH it_dmcm_load_add.
CLEAR it_dmcm_load_add.

LOOP AT it_dmcm_load_neg.

  SELECT SINGLE body_nb prprty_ty ty_connect
  INTO  (it_dmcm_load_neg-gpart, it_dmcm_load_neg-billingclass, it_dmcm_load_neg-ratecode)
  FROM zmcf
  WHERE acct_nb = it_dmcm_load_neg-vkont.

*  IF it_dmcm_load_neg-augbl <> ''.
*    CLEAR v_xblnr_temp.
*    CLEAR v_xblnr.
*
*    SELECT SINGLE xblnr hvorg tvorg
*    FROM DFKKOP
*    INTO (v_xblnr_temp, it_dmcm_load_neg-hvorg, it_dmcm_load_neg-tvorg)
*    WHERE augbl = it_dmcm_load_neg-augbl
*    AND blart = 'FA'
*    AND xblnr <> ''.
*
*    v_xblnr = v_xblnr_temp+4(12).
*
*    IF v_xblnr <> '' AND v_xblnr <> '999999999999'.
*
*      SELECT SINGLE belnr
*      INTO  it_dmcm_load_neg-belnr
*      FROM erchc WHERE opbel = v_xblnr.
*
*      IF  it_dmcm_load_neg-belnr = '' AND  it_dmcm_load_neg-augbl <> ''.
*        SELECT SINGLE belnr
*        INTO  it_dmcm_load_neg-belnr
*        FROM zmwosb_psr WHERE opbel = it_dmcm_load_neg-augbl.
*      ENDIF.
*
*    ELSEIF v_xblnr = '' OR v_xblnr = '999999999999'.
*      IF it_dmcm_load_neg-augbl <> ''.
*        SELECT SINGLE belnr
*        INTO  it_dmcm_load_neg-belnr
*        FROM zmwosb_psr WHERE opbel = it_dmcm_load_neg-augbl.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*  IF  it_dmcm_load_neg-belnr <> ''.
*    SELECT SINGLE tarifnr
*    INTO  it_dmcm_load_neg-ratecode
*    FROM dberchz1
*    WHERE belnr =  it_dmcm_load_neg-belnr
*    AND ( tvorg = '0021' OR tvorg = '0011' )
*    AND buchrel = 'X'.
*
*    IF  it_dmcm_load_neg-ratecode <> ''.
*      SELECT SINGLE AKLASSE
*      INTO  it_dmcm_load_neg-billingclass
*      FROM ETRF
*      WHERE tarifnr =  it_dmcm_load_neg-ratecode.
*    ENDIF.
*  ENDIF.

  IF ( ( it_dmcm_load_neg-augbl >= '000100000000' AND it_dmcm_load_neg-augbl < '000200000000' ) OR ( it_dmcm_load_neg-augbl >= '000500000000' AND it_dmcm_load_neg-augbl < '000600000000' ) )
       AND ( it_dmcm_load_neg-hvorg = '0040' OR it_dmcm_load_neg-hvorg = '0100' OR it_dmcm_load_neg-hvorg = '0300' OR it_dmcm_load_neg-hvorg = '0605' OR it_dmcm_load_neg-hvorg = '6005' OR it_dmcm_load_neg-hvorg = '0625' )
       AND it_dmcm_load_neg-augrd <> '05'.
    CLEAR it_dmcm_all.
*    it_dmcm_all-name = 'DMCMBill'.
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
    it_dmcm_all-billingclass = it_dmcm_load_neg-billingclass.
    it_dmcm_all-gpart = it_dmcm_load_neg-gpart.
*    it_dmcm_all-belnr = it_dmcm_load_neg-belnr.
    it_dmcm_all-ratecode = it_dmcm_load_neg-ratecode.

    it_dmcm_all-opbel = it_dmcm_load_neg-augbl.
    it_dmcm_all-augbl = it_dmcm_load_neg-opbel.
    it_dmcm_all-source = 'AUGBL'.

*    it_dmcm_all-dmcmbill = it_dmcm_all-betrw - it_dmcm_all-sctax.
*    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw - it_dmcm_all-revsctax.
* Get taxed amount only.
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
*    it_dmcm_all-name = 'DMCMMisc'.
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
    it_dmcm_all-billingclass = it_dmcm_load_neg-billingclass.
    it_dmcm_all-gpart = it_dmcm_load_neg-gpart.
*    it_dmcm_all-belnr = it_dmcm_load_neg-belnr.
    it_dmcm_all-ratecode = it_dmcm_load_neg-ratecode.

    it_dmcm_all-opbel = it_dmcm_load_neg-augbl.
    it_dmcm_all-augbl = it_dmcm_load_neg-opbel.
    it_dmcm_all-source = 'AUGBL'.

*    it_dmcm_all-dmcmbill = it_dmcm_all-betrw - it_dmcm_all-sctax.
*    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw - it_dmcm_all-revsctax.
* Get taxed amount only.
    it_dmcm_all-dmcmbill = it_dmcm_all-betrw.
    it_dmcm_all-revdmcmbill = it_dmcm_all-revbetrw.
    it_dmcm_all-amount = it_dmcm_all-dmcmbill + it_dmcm_all-revdmcmbill.

    APPEND it_dmcm_all.
  ENDIF.
ENDLOOP.
REFRESH it_dmcm_load_neg.
CLEAR it_dmcm_load_neg.

SORT it_dmcm_all BY budat vkont opbel.


* CREATE OUTPUT TABLE *
DATA: BEGIN OF v_string OCCURS 0,
  gpart(10), '|',
  vkont(12), '|',
  gsber(4), '|',
  billingclass(4), '|',
  ratecode(10), '|',
  augbl(12), '|',
*  belnr(12), '|',
  amount(13), '|',
  budat(8), '|',
  opbel(12), '|',
  source(5), '|',
*  hvorg(4), '|',
*  tvorg(4), '|',
 END OF v_string.
* END OF OUTPUT TABLE *

COMMIT WORK.

*  WRITE TO FILE *
DATA: write_count TYPE p VALUE 0.

DO 100 TIMES.
  OPEN DATASET datapath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    PERFORM write_to_file.
    EXIT.
  ELSE.
    write_count = write_count + 1.
* SLEEP FOR 2 SECS *
    CALL FUNCTION 'ENQUE_SLEEP'
      EXPORTING
        seconds = 2.
  ENDIF.
ENDDO.

IF write_count > 0.
  WRITE:/ 'The file',datapath,'could not be opened. The program failed to write',write_count,'times'.
ELSE.
  WRITE:/ 'The program successfully created a file',datapath,'without errors.'.
ENDIF.
*   END OF WRITE TO FILE *

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
