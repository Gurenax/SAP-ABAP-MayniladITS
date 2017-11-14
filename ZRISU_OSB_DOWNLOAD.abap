*****************************************************************************************************
* PROGRAM ID      : ZRISU_OSB_DOWNLOAD
* TITLE           : Meter Reading Download for OSB
* CREATE DATE     : Mar 09, 2011
* AUTHOR          : MPBACANI
*----------------------------------------------------------------------------------------------------
* DESCRIPTION     :
*----------------------------------------------------------------------------------------------------
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------------
*  DATE        |   NAME     |  DESCRIPTION                                 | Reference
*----------------------------------------------------------------------------------------------------
* Apr 25, 2011 | MPBACANI   | Format of negative sign of PUA,Meter Info    | Konch, Ma'am Celia
* Apr 25, 2011 | MPBACANI   | PUA back to orig format                      | Konch, Vince
* Oct 20, 2011 | GDIMALIWAT | Additonal column ACCT_DET                    | Salve Sibal
* Dec 08, 2011 | ACUCUECO   | Additonal column OLD_BA                      | Salve Sibal
* Dec 08, 2011 | ACUCUECO   | Additonal column OLD_BA                      | Salve Sibal
* Feb 10, 2012 | GDIMALIWAT | Added MOVEITAB within the VKONT validation   | Salve Sibal
* Feb 15, 2012 | ACUCUECO   | Change in table sources, Set default value   | Katherine Flores
*              |            | of No. of house hold to 1 if null            | TN#11062 & TN#11157
* Mar 15, 2012 | BMABINI    | Inserting of PUA to ZOSB_PUA table           | TN#10513
* Mar 19, 2012 | RANDYSY    | Deletion of More than a year data in         |
*              |            | ZOSB_PUA table                               |
* Apr 18, 2012 | GDIMALIWAT | Added tagging of dihonored checks            | TN#12903
* May 05, 2012 | GDIMALIWAT | Uncommented dishonored Checks                | TN#12903
* Jul 23, 2012 | RANDYSY    | Change Source of PUA (Use Custom FM)         | TN#13816
* Aug 08, 2012 | RANDYSY    | Revert computation of PUA                    | TN#19142
* Aug 10, 2012 | ANNACLO    | Additioanal columns for HOA (AMAYI)          | TN#18283
* Aug 31, 2012 | RANDYSY    | Exception of HOA PUA for OSB PUA Computation | TN#18283
* Sep 24, 2012 | RANDYSY    | Added Indicators for BIR Requirements        | TN#20140
*              |            | and Zero Rated Indicator                     |
* Feb 28, 2013 | RANDYSY    | Correction of PUA and Installment Computation| TN#19142 & TN#26308
* Jan 15, 2013 | ANNACLO    | -Create exception list for incomplete data   | TN#23986
*              | ANNACLO    | -Removal of deadcodes within the program     |
* Apr 03, 2013 | RANDYSY    | Payment History Change                       | TN#29733
* Jun 15, 2013 | GDIMALIWAT | Fixed overpayment with Disc Tag 2            | TN#36905
* Jul 01, 2013 | RANDYSY    | Additional Exception List                    | TN#31606
* Jan 15, 2014 | RANDYSY    | Modification for Address Cleansing Project   | TN#51529
***************************************************************************************************
* Oct 07, 2014 | GDIMALIWAT | Modification for ZTISU_OSBEXCEP logs         | TN#67143
* Oct 17, 2014 | GDIMALIWAT | Added bill group parameter in ztisu_osbexcep | TN#226
* Nov 03, 2014 | GDIMALIWAT | Added portion parameter in ztisu_osbexcep    | TN#226
* Nov 07, 2014 | RANDYSY    | Fix for Payment History                      | TN#234
* Dec 03, 2014 | RANDYSY    | IRR Implementation                           | RFC#48
* Dec 03, 2014 | GDIMALIWAT | Continuation of IRR Implementation           | RFC#48
***************************************************************************************************
REPORT zrisu_osb_download  MESSAGE-ID zosb  "TN#23986 Inserted by Ann 01/17/13
                          LINE-SIZE 300 NO STANDARD PAGE HEADING.

INCLUDE ziisu_osb_download_randy.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_mrdate TYPE sydatum DEFAULT sy-datum OBLIGATORY.

SELECT-OPTIONS:
            p_mru FOR adrstrtmru-ableinh MATCHCODE OBJECT ableinh_mdg.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
"-- Destination File Name
PARAMETERS: pa_path TYPE rlgrap-filename DEFAULT gv_path OBLIGATORY,
            pa_exc  TYPE rlgrap-filename DEFAULT gv_exc  OBLIGATORY. "-- TN#31606 Inserted by RANDYSY 07/01/2013
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  "-- Macro Definition for Conversion Alpha
  DEFINE mc_conversion_alpha.
    "-- Add Zero Paddings
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &1.
  END-OF-DEFINITION.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path.
  PERFORM select_file_path USING pa_path.

START-OF-SELECTION.

  SELECT ableinh adatsoll ablbelnr anlage abrdats
    INTO CORRESPONDING FIELDS OF TABLE it_eablg
  FROM   eablg
  WHERE  "ablbelnr EQ '00000000000118696572' "--GDIMALIWAT 12/04/2014 For Testing RFC#48 
  ableinh  IN p_mru
  AND    adatsoll EQ p_mrdate.
*  AND    anlage   EQ '8000196302'.

  PERFORM output_header.

  REFRESH: gt_pua.

  LOOP AT it_eablg.
    CLEAR:   gv_error, gv_msg.  "TN#23986 Inserted by Ann 01/17/13
    REFRESH: itab.

    itab-mru     = it_eablg-ableinh.
    itab-idnum   = it_eablg-ablbelnr.
    itab-install = it_eablg-anlage.
    itab-abrdats = it_eablg-abrdats.

    SELECT SINGLE equnr gernr stanzvor adat
      INTO (itab-eqpt, itab-device, itab-predecimalplaces, itab-adat)
    FROM   eabl
    WHERE  ablbelnr EQ itab-idnum.

    it_mru-mru      = it_eablg-ableinh.
    it_mru-adatsoll = it_eablg-adatsoll.

*    itab-adat = '00000000'.

    IF itab-adat EQ '00000000'.
      APPEND itab.
    ELSE. "--TN#23986 Inserted by Ann 01/15/2013
      MESSAGE e020 INTO gv_msg WITH itab-adat.
      PERFORM build_exception_list TABLES gt_exception
                                   USING itab-mru itab-idnum it_mru-adatsoll ' '  ' '  gv_msg.
    ENDIF. "/-TN#23986 Inserted by Ann 01/15/2013

    rec = 0.

    SORT itab BY mru ASCENDING.

    LOOP AT itab.
      "COMMIT WORK.       "TN#23986 comment out by Ann 01/17/2013 (No need to commit anything, since no update found)
      REFRESH itab_sap.

      PERFORM get_mrnote.
      PERFORM transfer.            "transfer retrieved data to itab_sap
      PERFORM material.            "get material code
      PERFORM relationship.        "get opcode            " CIM 10/22/2001
      PERFORM buspart.             "get bus part number first to get names get checkmeter.
      "--TN#23986 Inserted by Ann 01/17/2013
      IF gv_error EQ 'X'.
        MESSAGE e030 INTO gv_msg WITH gv_mom_acct gv_kid_acct. "TN#23986 Inserted by Ann 02/06/2013
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.

      "-- TN#31606 Inserted by RANDYSY 07/01/2013
      "-- Check Device
      PERFORM check_device USING    itab-install
                           CHANGING gv_serial_count.

      "-- Device Exception
      IF gv_serial_count GT 1.
        MESSAGE e032 INTO gv_msg.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.

      IF itab_sap-vkont IS INITIAL.
        gv_error = 'X'.
        MESSAGE e026 INTO gv_msg.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.
      IF itab_sap-name IS INITIAL.
        gv_error = 'X'.
        MESSAGE e021 INTO gv_msg.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.
      IF itab_sap-matnr EQ 'XX0009' AND itab_sap-block_cd EQ space.
        gv_error = 'X'.
        MESSAGE e027 INTO gv_msg.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.
      "/-TN#23986 Inserted by Ann 01/17/2013

      PERFORM address.             "get address data (using bus part no.)
      PERFORM rate.                "get rate categ (using instaln no.)
      "--TN#23986 Inserted by Ann 01/17/2013
      IF gv_error EQ 'X'.
        IF gv_rate_cd IS INITIAL. "Not found in Read and Bill Table of Rates
          CONCATENATE text-m00 itab-mru itab_sap-rate INTO gv_msg SEPARATED BY space.
          PERFORM build_exception_list TABLES gt_exception
                                       USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
        ENDIF.
      ELSEIF gv_error EQ 'Y'.
        "-- Invalid combination of Rate Type and Rate Category.
        CONCATENATE text-m02 itab-mru itab_sap-rate INTO gv_msg SEPARATED BY space.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ENDIF.
      "/-TN#23986 Inserted by Ann 01/17/2013

      PERFORM prevread_aveconsump."get prev reading and ave consump
      "--TN#23986 Inserted by Ann 01/17/2013
      IF itab_sap-prev_mrdate IS INITIAL OR itab_sap-prev_mrdate EQ '00000000'.
        gv_error = 'X'.
        MESSAGE e022 INTO gv_msg.
        PERFORM build_exception_list TABLES gt_exception
                                     USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
      ELSE.
        "--Check if Previous Meter Reading Date is greater than the current MR Schedule
        IF itab_sap-prev_mrdate GE it_mru-adatsoll.
          gv_error = 'X'.
          MESSAGE e023 INTO gv_msg WITH itab_sap-prev_mrdate.
          PERFORM build_exception_list TABLES gt_exception
                                       USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
        ELSE.
          CLEAR gv_diff.
          gv_diff = it_mru-adatsoll - itab_sap-prev_mrdate.
          "--Check if the date difference from the current MR Date is more than one year
          IF gv_diff GT 365.
            gv_error = 'X'.
            MESSAGE e024 INTO gv_msg WITH itab_sap-prev_mrdate.
            PERFORM build_exception_list TABLES gt_exception
                                         USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
          ENDIF.
        ENDIF.
      ENDIF.
      "/-TN#23986 Inserted by Ann 01/17/2013

      PERFORM seq_locationdesc.    "get seq num & devloc# to get description, RSG
      PERFORM staticnote.          "get static note
      PERFORM billing_period.      "get billing period and status
      PERFORM get_hoa_details.     "Inserted by Ann 2012/08/14

      IF itab_sap-vkont NE space.
        PERFORM balance_charges.      "get balance charges

        PERFORM previous_payment2 USING    itab_sap-vkont
                                           buspart
                                           itab_sap-prev_mr_erdat
                                           itab_sap-end_billdt
                                  CHANGING itab_sap-water_or
                                           itab_sap-water_pydt
                                           itab_sap-water_net
                                           itab_sap-water_vat
                                           itab_sap-water_total
                                           itab_sap-water_or2
                                           itab_sap-water_pydt2
                                           itab_sap-water_net2
                                           itab_sap-water_vat2
                                           itab_sap-water_total2
                                           itab_sap-misc_or
                                           itab_sap-misc_pydt
                                           itab_sap-misc_net
                                           itab_sap-misc_vat
                                           itab_sap-misc_total
                                           itab_sap-gd_or
                                           itab_sap-gd_pydt
                                           itab_sap-gd_net.


        PERFORM get_acct_det.         "get account determination id
        PERFORM get_dishonored_check. "check if the account is a dishonored check
        PERFORM set_bir_indicators.   "-- Sets the BIR Indicators - RANDYSY 09/24/2012

        "-- Get Previous Business Area
        PERFORM get_prev_ba USING    itab_sap-vkont
                            CHANGING itab_sap2-gsber.

        "-- Previous Business Area Exception
        IF itab_sap2-gsber IS INITIAL AND itab_sap-mru+0(4) EQ '1800'.
          gv_error = 'X'.
          MESSAGE e031 INTO gv_msg WITH itab_sap-prev_mrdate.
          PERFORM build_exception_list TABLES gt_exception
                                       USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
        ENDIF.

        "-- SANZPER Exception
        IF  ( itab_sap-sanzper IS INITIAL OR itab_sap-sanzper = '0.00' )
        AND ( itab_sap-ratecateg EQ 'AVERES' OR itab_sap-ratecateg EQ 'AVESEM').
          gv_error = 'X'.
          gv_msg   = text-m03.
          PERFORM build_exception_list TABLES gt_exception
                                       USING itab-mru itab-idnum it_mru-adatsoll itab_sap-vkont itab-install gv_msg.
        ENDIF.
      ENDIF.

      PERFORM moveitab. "transfer data for writing
    ENDLOOP.

    PERFORM trans_text.
    CLEAR: itab, itab_sap2, itab_sap.
  ENDLOOP.

  "-- Download Exceptions Data
  PERFORM download_exceptions.

  "-- Download Data (Valid/Invalid)
  PERFORM download_data.

  DATA: lo_sy_open_sql TYPE REF TO cx_sy_open_sql_db,
        lv_text        TYPE string.

  "-- Insert to table the gathered PUA's
  IF gt_pua[] IS NOT INITIAL.
    LOOP AT gt_pua ASSIGNING <fs_pua>.
      TRY.
          INSERT INTO zosb_pua VALUES <fs_pua>.

          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
        CATCH cx_sy_open_sql_db INTO lo_sy_open_sql.
          lv_text = lo_sy_open_sql->get_text( ).

          CONCATENATE 'ZOSB_PUA Error Upon Insert :' <fs_pua>-mrid ' : ' lv_text INTO gv_msg.
          WRITE:/ gv_msg.
      ENDTRY.
    ENDLOOP.
  ENDIF.

  "-- Delete more than a year data of PUA
  reference_date = sy-datum - 365.

  TRY.
      DELETE FROM zosb_pua WHERE reading_date LT reference_date.

      IF sy-subrc EQ 0.
        COMMIT WORK.
      ENDIF.
    CATCH cx_sy_open_sql_db INTO lo_sy_open_sql.
      lv_text = lo_sy_open_sql->get_text( ).

      CONCATENATE 'ZOSB_PUA Error Upon Deletion :' lv_text INTO gv_msg.
      WRITE:/ gv_msg.
  ENDTRY.

*---------------------------------------------------------------------*
*       FORM OUTPUT_HEADER                                            *
*---------------------------------------------------------------------*
FORM output_header.
  FORMAT COLOR COL_HEADING.
  WRITE: / 'NO', 5 'MRU', 14 'SEQ', 19 'MRDOCNUM', 30 'DEVICE',
         46 'MANUFSERIAL#', 61 'METER', 68 'MR',
         74 'MR', 80 'NAME',
        106 'ADDRESS', 146 'LOCN DESC', 167 'STAT', 191 'INSTALN#',
        202 'OPERN',
        209 'PREV',
        221 'PREV ACT',
        231 'PREV BLD',
        241 'PREV',
        251 'AVG',
        260 'RATE   ',
        270 'EXPECTED  ',
        280 'MR   ',
        290 'OLD_BA'. " added by acucueco

  WRITE: /61 'TYPE', 68 'NUM',
          74 'NOTE', 167 'NOTE',
        202 'CODE',
        209 'MRDATE',
        221 'RDG',
        231 'RDG',
        241 'CONSUM',
        251 'CONSUM',
        260 'CATEG  ',
        270 'MR        ',
        280 'TYPE ',
        295 ''. "added by acucueco
  FORMAT RESET. SKIP.

ENDFORM.                    "OUTPUT_HEADER

*&---------------------------------------------------------------------*
*&      FORM GET_INACTIVE_LIST
*&---------------------------------------------------------------------*
FORM get_inactive_list.
  SELECT DISTINCT logiknr INTO itab-logicdev FROM ediscobj INNER JOIN
    ediscdoc ON ediscdoc~discno = ediscobj~discno WHERE
    ediscdoc~status = '21'.

    SELECT SINGLE equnr INTO itab-eqpt FROM egerh WHERE
      logiknr = itab-logicdev.
    APPEND itab. CLEAR itab.
  ENDSELECT.

  SORT itab BY eqpt idnum DESCENDING.
  DELETE ADJACENT DUPLICATES FROM itab COMPARING eqpt.

  SORT itab BY idnum DESCENDING.
  LOOP AT itab WHERE idnum = space.
    MOVE itab-eqpt TO meter-eqpt.
    MOVE itab-devloc TO meter-devloc.
    MOVE itab-logicdev TO meter-logicdev.
    APPEND meter. CLEAR meter.
    DELETE itab.
  ENDLOOP.
ENDFORM.                    "GET_INACTIVE_LIST

**&---------------------------------------------------------------------*
**&      FORM GET_INACTIVE_DATA
**&---------------------------------------------------------------------*
*FORM get_inactive_data.
**  data: consump type i.
*  REFRESH inactive.
*  LOOP AT meter.
**** GET INSTALLATION (TO GET RATE CATEG AND MRU) ***
*    SELECT SINGLE anlage INTO inactive-instal FROM eastl
*    WHERE logiknr = meter-logicdev.
*
*    SELECT SINGLE tariftyp ableinh INTO (inactive-ratecateg,
*    inactive-mru) FROM eanlh WHERE anlage = inactive-instal.
*
**** GET SIZE, MANUF SERIAL#, DEVICE#, MATREIAL  ***
*
*    SELECT SINGLE stanzvor ableser ablhinw INTO (inactive-predec,
*    inactive-mrnumber, inactive-mrnote) FROM eabl
*    WHERE equnr = meter-eqpt.
*
**** FORM STATICNOTE - GET STATIC NOTE FOR METER READERS ***
*    SELECT SINGLE note_text INTO statnote FROM te582t INNER JOIN enote
*      ON te582t~note = enote~note WHERE enote~objkey = itab-devloc
*      AND te582t~spras = 'E'.
*    CONDENSE statnote.
**** ronald's addition URF:BG040405 04/29/2004
*    OVERLAY itab_sap-statnote WITH space
*            ONLY '~`@#$%^&*()_-+={}[]|\:;"<>,.?/'.
*    MOVE statnote TO itab_sap-statnote.
*
**** GET DEVICE LOCATION DESCRIPTION ***
*    SELECT SINGLE stortzus INTO inactive-location FROM egpltx
*    WHERE devloc = meter-devloc.
*
**** GET PREMISE (TO GET SEQUENCE#) ***
*    SELECT SINGLE prems INTO premise FROM iflot
*    WHERE tplnr = meter-devloc.
*
*    SELECT SINGLE eqfnr INTO inactive-seq FROM itob
*    WHERE sernr = inactive AND prems = premise AND datbi > sy-datum.
*
**** GET BUSINESS PART# (TO GET NAME AND ADDRESS) ***
*    SELECT SINGLE kunde INTO buspart FROM eein INNER JOIN ever
*    ON eein~vkont = ever~vkonto WHERE ever~anlage = inactive-instal.
*
*    SELECT SINGLE name_org1 name_first name_last INTO (orgname,
*    firstname, lastname) FROM but000 WHERE partner = buspart.
*    IF orgname = space.
*      CONCATENATE firstname lastname INTO inactive-name SEPARATED
*      BY space.
*    ELSE.
*      CONDENSE orgname. MOVE orgname TO inactive-name.
*    ENDIF.
*
*    SELECT SINGLE house_num1 house_num2 street str_suppl1 city1 floor
*      roomnumber INTO CORRESPONDING FIELDS OF address FROM
*      ebp_addr_service WHERE partner = buspart.
*    IF address-floor = space. address-flrtxt = space. ENDIF.
*    IF address-roomnumber = space. address-rmtxt = space. ENDIF.
*    CONDENSE address. MOVE address TO inactive-address.
*
**** GET OPERATION CODE (MAIN OR SECONDARY REGISTER) ***
*    SELECT SINGLE logikzw INTO regnum FROM etdz
*      WHERE equnr = meter-eqpt.
*    SELECT SINGLE opcode INTO inactive-operncode FROM easti
*      WHERE logikzw = regnum.
*
**** GET PREV READING, PREV CONSUMPTION & AVE CONSUMPTION ***
*    REFRESH mreading.
*    period = mrdate - 366.
*    SELECT v_zwstand adatsoll adat INTO (mreading-reading,
*    mreading-mrsched, mreading-mrdate) FROM eabl WHERE
*    equnr = meter-eqpt AND adat NE '00000000' AND
*    ( adatsoll < mrdate AND adatsoll GE period ).
*      IF mreading-mrdate = '00000000'.
*        SELECT SINGLE bis INTO mreading-mrdate FROM eanlh WHERE
*        anlage = inactive-instal.
*      ENDIF.
*      APPEND mreading. CLEAR mreading.
*    ENDSELECT.
*
*    SORT mreading BY mrsched DESCENDING.
*    CLEAR: num, prev1, prev2, perconsump.
*    LOOP AT mreading.
*      num = num + 1.
*      prev2 = mreading-reading.
*      perconsump = perconsump + ( prev1 - prev2 ).
*      IF num =  1.
*        perconsump = 0.
*        inactive-prev_mrdate = mreading-mrdate.
**        INACTIVE-prevread = MREADING-reading.
*      ENDIF.
*      IF num = 2.
*        inactive-prevconsump = prev1 - prev2.
*      ENDIF.
*      prev1 = prev2.
*      CLEAR: prev2, mreading.
*    ENDLOOP.
*    IF num > 0.
*      inactive-aveconsump = trunc( perconsump / num ).
**      write consump to INACTIVE-AVECONSUMP.
*    ENDIF.
*    APPEND inactive.
*    CLEAR: premise, buspart, orgname, firstname, lastname,
*           address, regnum, num.
*  ENDLOOP.
*ENDFORM.                    "GET_INACTIVE_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANSFER
*&---------------------------------------------------------------------*
FORM transfer.
  itab_sap-mru     = itab-mru.
  itab_sap-mrid    = itab-idnum.
  itab_sap-device  = itab-device.
*** ronald's addition URF:BG040405 04/29/2004
  OVERLAY itab_sap-device WITH space
            ONLY '~`@#$%^&*()_-+={}[]|\:;"<>,.?/'.
  itab_sap-predec  = itab-predecimalplaces.
  itab_sap-mrnote  = itab-ablhinw.
  itab_sap-bill_dt = itab-abrdats.
  itab_sap-instal  = itab-install.
ENDFORM.                    "TRANSFER

*&---------------------------------------------------------------------*
*&      GET MATERIAL CODE BASED ON EQUIPMENT NO.
*&---------------------------------------------------------------------*
FORM material.
  DATA: ilenmatnr TYPE i.

  SELECT SINGLE matnr INTO itab_sap-matnr FROM equi
  WHERE equnr = itab-eqpt.
  CONDENSE itab_sap-matnr.
  ilenmatnr = strlen( itab_sap-matnr ).
  IF ilenmatnr > 6.
    itab_sap-matnr = itab_sap-matnr+0(6).
  ENDIF.
ENDFORM.                    "MATERIAL

*---------------------------------------------------------------------*
*       FORM BUSPART - GET BUS.PARTNER NUM (TO GET NAMES) AND CAN
*---------------------------------------------------------------------*
FORM buspart.
*will only be initial if opcode = '00' and therefore business
*partner name

  TYPES: BEGIN OF ty_can,
          can TYPE vkont_kk,
          mru TYPE ableinheit,
         END OF ty_can.

  DATA: frstname  TYPE c LENGTH 40,
        lstname   TYPE c LENGTH 40,
        orgname   TYPE c LENGTH 40,
        name_grp1 TYPE c LENGTH 40,
        name_lst2 TYPE c LENGTH 40,
*--TN#23986 Inserted by Ann 02/01/2013
        lv_contract  TYPE vkont_kk,
        lt_contract  TYPE STANDARD TABLE OF ever,
        lt_can       TYPE STANDARD TABLE OF ty_can, "vkont_kk,
        lv_mom_mru   TYPE ableinheit,
        lv_kid_mru   TYPE ableinheit.

  FIELD-SYMBOLS: <fs_cont> TYPE ever,
                 <fs_can>  TYPE ty_can. "vkont_kk.
*/-TN#23986 Inserted by Ann 02/01/2013

  CLEAR itab_sap-vkont.
  CLEAR itab_sap-name.
  CLEAR itab_sap-block_cd.
  CLEAR buspart.
  CLEAR itab_sap-tin.
  CLEAR itab_sap-check_meter.

  SELECT SINGLE vkonto abrsperr INTO (itab_sap-vkont, itab_sap-block_cd) FROM ever
    WHERE anlage = itab_sap-instal
      AND auszdat = '99991231'.

  SELECT SINGLE kunde INTO buspart FROM eein
    WHERE vkont = itab_sap-vkont.

  SELECT SINGLE soc_secure
    INTO itab_sap-tin
    FROM ekun
    WHERE partner = buspart.

  IF itab_sap-tin NE space.
    CONCATENATE itab_sap-tin+0(3) '-' itab_sap-tin+3(3) '-' itab_sap-tin+6(3) '-' itab_sap-tin+9(3) INTO itab_sap-tin.
  ENDIF.

  SELECT SINGLE name_last name_first name_org1 name_grp1 name_lst2
    INTO (lstname, frstname, orgname, name_grp1, name_lst2)
    FROM but000
    WHERE partner =  buspart.

  IF frstname IS INITIAL.
    CONDENSE orgname. MOVE orgname TO itab_sap-name.
    IF itab_sap-name IS INITIAL.
      CONDENSE name_grp1. MOVE name_grp1 TO itab_sap-name.
    ENDIF.
    IF itab_sap-name IS INITIAL.
      CONDENSE name_lst2. MOVE name_lst2 TO itab_sap-name.
    ENDIF.
  ELSE.
    CONCATENATE frstname lstname INTO itab_sap-name SEPARATED BY space.
  ENDIF.

  CLEAR: lv_contract, gv_mom_acct, gv_kid_acct. "TN#23986 Inserted by Ann 02/01/2013
  REFRESH lt_can.                               "TN#23986 Inserted by Ann 02/01/2013

  SELECT SINGLE meter_tag
    INTO itab_sap-check_meter
  FROM   zcheckmtr
  WHERE  vkont   EQ itab_sap-vkont
  AND    blk_tag NE 'X'.

  IF sy-subrc EQ 0.
    "-- Check if CAN is Mother
    IF itab_sap-check_meter EQ 'C' OR itab_sap-check_meter EQ 'M'.
      "-- Set Mother MRU
      lv_mom_mru  = itab-mru.

      "-- Set Mother CAN
      gv_mom_acct = itab_sap-vkont.

      "-- Get all the Child CANs
      SELECT vkont
        INTO TABLE lt_can
      FROM   zcheckmtr
      WHERE  meter_tag EQ itab_sap-vkont
      AND    blk_tag   NE 'X'.

      "-- Get MRU of Mother CAN
      SELECT SINGLE a~ableinh
        INTO lv_mom_mru
      FROM   eanlh AS a INNER JOIN ever AS b ON ( a~anlage EQ b~anlage )
      WHERE  b~vkonto  EQ gv_mom_acct
      AND    a~bis     EQ '99991231'.

      "-- Get MRUs of Child CAN
      LOOP AT lt_can ASSIGNING <fs_can>.
        SELECT SINGLE a~ableinh
          INTO <fs_can>-mru
        FROM   eanlh AS a INNER JOIN ever AS b ON ( a~anlage EQ b~anlage )
        WHERE  b~vkonto  EQ <fs_can>-can
        AND    a~bis     EQ '99991231'.

        IF <fs_can>-mru NE lv_mom_mru.
          gv_kid_acct = <fs_can>-can.
          gv_error    = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

*      "-- Check Mother Account's MRU
*      CALL FUNCTION 'ISU_ABLEINH_CONTRACTS_SELECT'
*        EXPORTING
*          x_ableinh    = lv_mom_mru
*          x_abdatum    = sy-datum
*        TABLES
*          yt_ever      = lt_contract
*        EXCEPTIONS
*          not_found    = 1
*          system_error = 2
*          OTHERS       = 3.
*
*      IF sy-subrc EQ 0.
*        "-- Check Child CANs
*        LOOP AT lt_can ASSIGNING <fs_can>.
*          READ TABLE lt_contract ASSIGNING <fs_cont> WITH KEY vkonto = <fs_can>.
*          IF sy-subrc NE 0.
*            gv_kid_acct = <fs_can>.
*            gv_error    = 'X'.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
*      ELSE.
*        RETURN.
*      ENDIF.
    ELSE.
      "-- Set Mother CAN
      gv_mom_acct = itab_sap-check_meter.

      "-- Set Child CAN
      gv_kid_acct = itab_sap-vkont.

      "-- Get MRU of Mother CAN
      SELECT SINGLE a~ableinh
        INTO lv_mom_mru
      FROM   eanlh AS a INNER JOIN ever AS b ON ( a~anlage EQ b~anlage )
      WHERE  b~vkonto  EQ gv_mom_acct
      AND    a~bis     EQ '99991231'.

      "-- Get MRU of Child CAN
      SELECT SINGLE a~ableinh
        INTO lv_kid_mru
      FROM   eanlh AS a INNER JOIN ever AS b ON ( a~anlage EQ b~anlage )
      WHERE  b~vkonto  EQ gv_kid_acct
      AND    a~bis     EQ '99991231'.

      IF lv_kid_mru NE lv_mom_mru.
        gv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

*** start - commented by RANDYSY TN#31606 07/15/2013
*  "--TN#23986 Inserted by Ann 02/01/2013
*  IF sy-subrc EQ 0.
*    gv_mom_acct = lv_contract = itab_sap-check_meter.
*    IF lv_contract EQ 'C'. "Indicator for Mother Account
*      "--Get all the Child accounts
*      SELECT vkont INTO TABLE lt_can
*        FROM zcheckmtr
*        WHERE meter_tag EQ itab_sap-vkont
*          AND blk_tag   NE 'X'.
*      gv_mom_acct = itab_sap-vkont.
*    ENDIF.
*    CLEAR lv_contract.
*  ELSE.
*    SELECT SINGLE vkont INTO lv_contract
*    FROM zcheckmtr
*    WHERE meter_tag EQ itab_sap-vkont
*      AND blk_tag   NE 'X'.
*    gv_kid_acct = lv_contract.
*  ENDIF.
*  "/-TN#23986 Inserted by Ann 02/01/2013
*  "--TN#23986 Inserted by Ann 01/18/2013
*  IF lv_contract IS NOT INITIAL OR lt_can[] IS NOT INITIAL.
*    "--Check Mother Account's MRU
*    CALL FUNCTION 'ISU_ABLEINH_CONTRACTS_SELECT'
*      EXPORTING
*        x_ableinh    = itab-mru
*      TABLES
*        yt_ever      = lt_contract
*      EXCEPTIONS
*        not_found    = 1
*        system_error = 2
*        OTHERS       = 3.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    "--Check if the mother account is found
*    IF lv_contract IS NOT INITIAL.
*      READ TABLE lt_contract ASSIGNING <fs_cont> WITH KEY vkonto = lv_contract.
*      IF sy-subrc NE 0.
*        gv_error = 'X'.
*      ENDIF.
*    ELSEIF lt_can IS NOT INITIAL.
*      LOOP AT lt_can ASSIGNING <fs_can>.
*        READ TABLE lt_contract ASSIGNING <fs_cont> WITH KEY vkonto = <fs_can>.
*        IF sy-subrc NE 0.
*          gv_kid_acct = <fs_can>.
*          gv_error    = 'X'.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*    REFRESH lt_contract.
*  ENDIF.
*  "/-TN#23986 Inserted by Ann 01/18/2013
*** end - commented by RANDYSY TN#31606 07/15/2013
ENDFORM.                    "BUSPART

*---------------------------------------------------------------------*
*        FORM ADDRESS
*---------------------------------------------------------------------*
FORM address.
  DATA: addrnum    TYPE but020-addrnumber,
        tmpvalidfr TYPE but020-addr_valid_from,
        lv_address TYPE char70.

  CLEAR lv_address.

  SELECT MAX( addrnumber ) MAX( addr_valid_from )
    INTO (addrnum, tmpvalidfr)
  FROM   but020
  WHERE  partner EQ buspart.

  IF sy-subrc = 0.
*-- start - RANDYSY TN#51529 12/17/2013
*    SELECT SINGLE house_num1 house_num2 street str_suppl1
*                  city1 city2 floor roomnumber
*           INTO CORRESPONDING FIELDS OF address FROM adrc
*           WHERE addrnumber = addrnum.

    "-- Get Address Data
    SELECT SINGLE house_num1
                  str_suppl1
                  house_num2
                  street
                  str_suppl3
                  location
                  city2
                  city1
      INTO CORRESPONDING FIELDS OF address
    FROM   adrc
    WHERE  addrnumber EQ addrnum.

    IF sy-subrc EQ 0.
      "-- Remove Spaces for each fields
      CONDENSE: address-house_num1,
                address-str_suppl1,
                address-house_num2,
                address-street,
                address-str_suppl3,
                address-location,
                address-city2,
                address-city1.

      "-- Get first 10 Characters
      address-str_suppl1 = address-str_suppl1(10).

      TRANSLATE address-location TO UPPER CASE.

      "-- Location (Barangay) should not contain "BARANGAY"
*      SEARCH address-location  FOR 'BARANGAY'.
*      IF sy-subrc EQ 0.
*        CLEAR: address-location.
*      ENDIF.

      SEARCH address-location  FOR 'BRGY 0'.
      IF sy-subrc EQ 0.
        CLEAR: address-location.
      ENDIF.

*      SEARCH address-location  FOR 'BRGY.'.
*      IF sy-subrc EQ 0.
*        CLEAR: address-location.
*      ENDIF.

      "-- Remove "* City"
      TRANSLATE address-city1 TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF 'CITY' IN address-city1 WITH ''.

      "-- House Number & Street 2 Rule
      IF address-str_suppl1 IS NOT INITIAL AND address-house_num1 IS NOT INITIAL.
        lv_address = address-house_num1.
      ELSEIF address-str_suppl1 IS NOT INITIAL AND address-house_num1 IS INITIAL.
        lv_address = address-str_suppl1.
      ELSEIF address-str_suppl1 IS INITIAL AND address-house_num1 IS NOT INITIAL.
        lv_address = address-house_num1.
      ENDIF.

      "-- Supplement and Street Field
      CONCATENATE lv_address address-house_num2 address-street INTO lv_address SEPARATED BY space.

      "-- Barangay & Location Rule
      IF address-str_suppl3 IS NOT INITIAL AND address-location IS NOT INITIAL.
*        CONCATENATE lv_address address-location INTO lv_address SEPARATED BY space.
        CONCATENATE lv_address address-str_suppl3 INTO lv_address SEPARATED BY space.
      ELSEIF address-location IS NOT INITIAL AND address-str_suppl3 IS INITIAL.
        CONCATENATE lv_address address-location INTO lv_address SEPARATED BY space.
      ELSEIF address-location IS INITIAL AND address-str_suppl3 IS NOT INITIAL.
        CONCATENATE lv_address address-str_suppl3 INTO lv_address SEPARATED BY space.
      ENDIF.

      TRANSLATE address-city2 TO UPPER CASE.

      "-- City2 (District) should not contain "DISTRICT"
      SEARCH address-city2  FOR 'DISTRICT'.
      IF sy-subrc EQ 0.
        CLEAR: address-city2.
      ENDIF.

*      SEARCH address-city2  FOR 'DIST.'.
*      IF sy-subrc EQ 0.
*        CLEAR: address-city2.
*      ENDIF.

      IF address-city2 IS NOT INITIAL.
        CONCATENATE lv_address address-city2 address-city1 INTO lv_address SEPARATED BY space.
      ELSE.
        CONCATENATE lv_address address-city1 INTO lv_address SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

*  IF address-floor = space.
*    address-flrtxt = space.
*  ENDIF.
*  IF address-roomnumber = space.
*    address-rmtxt = space.
*  ENDIF.

*  CONDENSE address.
*  MOVE address TO itab_sap-address.

  CONDENSE lv_address.
  MOVE lv_address TO itab_sap-address.

*-- end - RANDYSY TN#51529 12/17/2013
ENDFORM.                    "ADDRESS

*---------------------------------------------------------------------*
*       FORM BILLRATE - GET RATE CATEG, BILLING CLASS, RATE
*---------------------------------------------------------------------*
FORM rate.

  DATA : tmptariftyp  TYPE c LENGTH 6,
         tmpbulk      TYPE c LENGTH 3,
         tmplenght    TYPE dfkkop-betrw,
         tmpsanzper   TYPE dfkkop-betrw.

  SELECT SINGLE tariftyp aklasse INTO (itab_sap-ratecateg, itab_sap-bill_class) FROM eanlh
    WHERE anlage = itab_sap-instal AND bis >= p_mrdate.
*  UPDATED BY RONALD 04/11/2002

  SELECT SINGLE tarifart INTO ratetyp FROM easts
    WHERE anlage = itab_sap-instal AND bis >= p_mrdate.

  SELECT SINGLE tarifnr INTO itab_sap-rate FROM ertfnd
    WHERE tarifart = ratetyp
      AND tariftyp = itab_sap-ratecateg
      AND bis      >= p_mrdate.

  IF sy-subrc EQ 0. "-- TN#31606 Inserted by RANDYSY 07/01/2013

    "--TN#23986 Inserted by Ann 01/18/2013
    IF itab_sap-rate IS NOT INITIAL.
      "--Check if rate is defined in READ and BILL Table of Rates
      CLEAR gv_rate_cd.
      SELECT SINGLE rate_cd INTO gv_rate_cd
        FROM zrb_rates
        WHERE rate_cd EQ itab_sap-rate.
      IF gv_rate_cd IS INITIAL.
        gv_error = 'X'.
        RETURN.
      ENDIF.
    ENDIF.
    "/-TN#23986 Inserted by Ann 01/18/2013

  ELSE.
    gv_error = 'Y'.   "-- TN#31606 Inserted by RANDYSY 07/01/2013
    RETURN.
  ENDIF.

  tmpbulk = itab_sap-ratecateg+0(3).
  tmpsanzper = space.
  IF tmpbulk = 'AVE'.
    SELECT SINGLE wert1 INTO tmpsanzper FROM ettifn
    WHERE anlage = itab_sap-instal
    AND operand = 'WFACTOR__3'
    AND bis = '99991231'.

    itab_sap-sanzper = tmpsanzper.
    tmplenght = strlen( itab_sap-ratecateg ).

    IF tmplenght > 5.
      IF ( itab_sap-sanzper EQ space OR itab_sap-sanzper LE 0 ).
        tmptariftyp = itab_sap-ratecateg+0(5).
        IF tmptariftyp = 'AVESM'.
          tmplenght = tmplenght - 5.
          tmpsanzper = itab_sap-ratecateg+5(tmplenght).
        ELSE.
          IF tmplenght > 6.
            tmplenght = tmplenght - 6.
            tmpsanzper = itab_sap-ratecateg+6(tmplenght).
          ENDIF.
        ENDIF.
        itab_sap-sanzper = tmpsanzper.
      ENDIF.
    ENDIF.
  ELSE.
    itab_sap-sanzper = space.
  ENDIF.
  SHIFT itab_sap-sanzper  LEFT DELETING LEADING space.

ENDFORM.                    "RATE

*---------------------------------------------------------------------*
*       FORM SEQ_LOCATIONDESC, RSG
*---------------------------------------------------------------------*
FORM seq_locationdesc.
  DATA: lv_iloan TYPE equz-iloan,
        datob    TYPE v_equi-datbi,
        mtrlen   TYPE i.

** GET PREMISE TO GET SEQ NUM **
  SELECT SINGLE vstelle anlart INTO (premise, tmpgov)
  FROM eanl WHERE
  anlage = itab_sap-instal.

  "-- Inserted by RANDYSY 09/26/2102
  IF ( tmpgov EQ '5001' OR tmpgov EQ '4052' OR tmpgov EQ '5006' OR tmpgov EQ '5007' OR tmpgov EQ '5008').
    "-- Inserted by RANDYSY 09/26/2102
    itab_sap-grp_flag = tmpgov.
  ELSE.
    itab_sap-grp_flag = space.
  ENDIF.

  SELECT SINGLE anzpers haus INTO (itab_sap-no_users, connobj)
  FROM evbs
  WHERE vstelle = premise.

  SELECT SINGLE groes serge INTO (itab_sap-size, itab_sap-mfgsernum)
     FROM equi WHERE equnr = itab-eqpt.

  mtrlen = strlen( itab_sap-mfgsernum ).
  IF mtrlen = 18.
    SHIFT itab_sap-mfgsernum LEFT DELETING LEADING '0'.
  ENDIF.

  SELECT SINGLE iloan INTO lv_iloan FROM equz WHERE equnr = itab-eqpt
                                     AND datbi > p_mrdate.
  IF sy-subrc = 0.
    SELECT SINGLE eqfnr INTO itab_sap-seq FROM iloa
                             WHERE iloan = lv_iloan.

  ENDIF.

  SELECT SINGLE regiogroup
    INTO itab_sap-rsg
  FROM ehauisu
  WHERE haus = connobj.

ENDFORM.                    "SEQ_LOCATIONDESC

*---------------------------------------------------------------------*
*       FORM STATICNOTE - GET STATIC NOTE FOR METER READERS
*---------------------------------------------------------------------*
FORM staticnote.
  DATA: inlines  TYPE tline OCCURS 0 WITH HEADER LINE.
  DATA: lines    TYPE tline OCCURS 0 WITH HEADER LINE.
  DATA: ilen     TYPE i.
  DATA: statnote TYPE te582t-note_text.

  SELECT SINGLE * FROM stxh WHERE tdobject = 'EQUI' AND
                                  tdname = itab-eqpt AND
                                  tdid = 'LTXT' AND
                                  tdspras = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'READ_TEXT_INLINE'
      EXPORTING
        id              = stxh-tdid
        inline_count    = stxh-tdtxtlines
        language        = stxh-tdspras
        name            = stxh-tdname
        object          = stxh-tdobject
      TABLES
        inlines         = inlines
        lines           = lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        not_found       = 4
        object          = 5
        reference_check = 6
        OTHERS          = 7.
    IF sy-subrc = 0.
      LOOP AT inlines.
        CONCATENATE statnote inlines-tdline INTO
                    statnote SEPARATED BY space.
        CONDENSE statnote.
        DO.
          SEARCH statnote FOR '|' STARTING AT 1.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            REPLACE '|' WITH ' '
                    INTO statnote.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.
  ENDIF.
  itab_sap-statnote = statnote.
ENDFORM.                    "STATICNOTE
*&---------------------------------------------------------------------*
*&      FORM TRANS_TEXT - TRANSFER DATA TO TEXT
*&---------------------------------------------------------------------*
FORM trans_text.
  SORT inactive BY mru ASCENDING. CLEAR c2.
  LOOP AT inactive WHERE mru = mru.
    c2 = c2 + 1.
    MOVE-CORRESPONDING inactive TO itab_sap2.
    APPEND itab_sap2.
    CLEAR itab_sap2.
  ENDLOOP.

  SORT itab_sap2 BY seq ASCENDING mrid DESCENDING. CLEAR c.
  LOOP AT itab_sap2.
    c = c + 1.
    rec = c.
    CONDENSE rec.
    IF itab_sap2-prevconsump = space.
      itab_sap2-prevconsump = '0'.
    ENDIF.

    SHIFT itab_sap2-billed_prevread LEFT DELETING LEADING space.
    SHIFT itab_sap2-actual_prevread LEFT DELETING LEADING space.
    SHIFT itab_sap2-prevconsump LEFT DELETING LEADING space.
    SHIFT itab_sap2-aveconsump LEFT DELETING LEADING space.
    SHIFT itab_sap2-expected LEFT DELETING LEADING space.
    SHIFT itab_sap2-seq LEFT DELETING LEADING '0'.
    SHIFT itab_sap2-initial_rdg LEFT DELETING LEADING space.
    SHIFT itab_sap2-final_rdg LEFT DELETING LEADING space.
    SHIFT itab_sap2-pua LEFT DELETING LEADING space.

    "-- start - Modified by RANDYSY 12/02/2014 RFC#48
*    SHIFT itab_sap2-other_charges LEFT DELETING LEADING space.
*    SHIFT itab_sap2-meter_charges LEFT DELETING LEADING space.
*    SHIFT itab_sap2-re_opening_fee LEFT DELETING LEADING space.
*    SHIFT itab_sap2-gd LEFT DELETING LEADING space.
*    SHIFT itab_sap2-installment LEFT DELETING LEADING space.
*    SHIFT itab_sap2-install_charge LEFT DELETING LEADING space.

    SHIFT itab_sap2-septic_charge         LEFT DELETING LEADING space.
    SHIFT itab_sap2-changesize_charge     LEFT DELETING LEADING space.
    SHIFT itab_sap2-restoration_charge    LEFT DELETING LEADING space.
    SHIFT itab_sap2-misc_charge           LEFT DELETING LEADING space.
    SHIFT itab_sap2-meter_charges         LEFT DELETING LEADING space.
    SHIFT itab_sap2-re_opening_fee        LEFT DELETING LEADING space.
    SHIFT itab_sap2-gd                    LEFT DELETING LEADING space.
    SHIFT itab_sap2-install_water_charge  LEFT DELETING LEADING space.
    SHIFT itab_sap2-install_sewer_charge  LEFT DELETING LEADING space.
    SHIFT itab_sap2-advances              LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_install_water      LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_install_water_ind  LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_install_sewer      LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_install_sewer_ind  LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_ar                 LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_ar_ind             LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_adv                LEFT DELETING LEADING space.
    SHIFT itab_sap2-pn_adv_ind            LEFT DELETING LEADING space.
    "-- end - Modified by RANDYSY 12/02/2014 RFC#48

    SHIFT itab_sap2-water_net LEFT DELETING LEADING space.
    SHIFT itab_sap2-water_vat LEFT DELETING LEADING space.
    SHIFT itab_sap2-water_total LEFT DELETING LEADING space.
    SHIFT itab_sap2-water_net2 LEFT DELETING LEADING space.
    SHIFT itab_sap2-water_vat2 LEFT DELETING LEADING space.
    SHIFT itab_sap2-water_total2 LEFT DELETING LEADING space.
    SHIFT itab_sap2-misc_net LEFT DELETING LEADING space.
    SHIFT itab_sap2-misc_vat LEFT DELETING LEADING space.
    SHIFT itab_sap2-misc_total LEFT DELETING LEADING space.
    SHIFT itab_sap2-gd_net LEFT DELETING LEADING space.
    SHIFT itab_sap2-final_rdg LEFT DELETING LEADING space.
    SHIFT itab_sap2-recon_rdg LEFT DELETING LEADING space.
    SHIFT itab_sap2-sanzpers LEFT DELETING LEADING space.


    IF itab_sap2-recon_dt EQ '00000000'.
      itab_sap2-recon_dt = space.
    ENDIF.


    IF itab_sap2-seq IS INITIAL.
      itab_sap2-seq = '0'.
    ENDIF.
    SHIFT itab_sap2-matnr LEFT DELETING LEADING '0'.
*** ronald's addition URF:BG040405 04/29/2004
    OVERLAY itab_sap2-device WITH space
            ONLY '~`@#$%^&*()_-+={}[]|\:;"<>,.?/'.
*** JBBAUTISTA ADDITION : 04/26/2005

    OVERLAY itab_sap2-device WITH '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@' ONLY ''''.
    OVERLAY itab_sap2-mfgsernum WITH '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@' ONLY
    ''''.

    OVERLAY itab_sap2-aveconsump WITH space
                ONLY '~`@#$%^&*()_-+={}[]|\:;"<>,.?/'.

    CONDENSE itab_sap2-aveconsump NO-GAPS.


*** END OF JBBAUTISTA
    CONCATENATE itab_sap2-mru   itab_sap2-seq         itab_sap2-mrid
            itab_sap2-device    itab_sap2-mfgsernum   itab_sap2-matnr
            itab_sap2-mrnumber  itab_sap2-mrnote      itab_sap2-name
            itab_sap2-address   itab_sap2-location    itab_sap2-statnote
            itab_sap2-instal    itab_sap2-operncode   itab_sap2-prev_mrdate
            itab_sap2-billed_prevread
            itab_sap2-prevconsump
            itab_sap2-aveconsump
*ADDED BY FGONZALES
*            AVECONSUM
            itab_sap2-ratecateg itab_sap2-opcode itab_sap2-expected
            itab_sap2-istablart itab_sap2-actual_prevread
            itab_sap2-sanzpers p_mrdate itab_sap2-vkont
            itab_sap2-bill_class itab_sap2-rate
            itab_sap2-newmeter_info itab_sap2-initial_rdg
            itab_sap2-removal_dt itab_sap2-final_rdg
            itab_sap2-grp_flag itab_sap2-stat_flag
            itab_sap2-block_cd itab_sap2-disc_tag
            itab_sap2-compl_tag itab_sap2-pua

"-- start - Modified by RANDYSY 12/02/2014 RFC#48
*            itab_sap2-other_charges
*            itab_sap2-meter_charges
*            itab_sap2-re_opening_fee
*            itab_sap2-gd
*            itab_sap2-installment
*            itab_sap2-install_charge

            itab_sap2-septic_charge
            itab_sap2-changesize_charge
            itab_sap2-restoration_charge
            itab_sap2-misc_charge
            itab_sap2-meter_charges
            itab_sap2-re_opening_fee
            itab_sap2-gd
            itab_sap2-install_water_charge
            itab_sap2-install_sewer_charge
            itab_sap2-advances
            itab_sap2-pn_install_water
            itab_sap2-pn_install_water_ind
            itab_sap2-pn_install_sewer
            itab_sap2-pn_install_sewer_ind
            itab_sap2-pn_ar
            itab_sap2-pn_ar_ind
            itab_sap2-pn_adv
            itab_sap2-pn_adv_ind
"-- end - Modified by RANDYSY 12/02/2014 RFC#48

            itab_sap2-invoice
            itab_sap2-water_or
            itab_sap2-water_pydt itab_sap2-water_net
            itab_sap2-water_vat itab_sap2-water_total
            itab_sap2-water_or2 itab_sap2-water_pydt2
            itab_sap2-water_net2 itab_sap2-water_vat2
            itab_sap2-water_total2 itab_sap2-misc_or
            itab_sap2-misc_pydt itab_sap2-misc_net
            itab_sap2-misc_vat itab_sap2-misc_total
            itab_sap2-gd_or itab_sap2-gd_pydt
            itab_sap2-gd_net itab_sap2-sanzper
            itab_sap2-tin itab_sap2-bill_dt
            itab_sap2-bill_tr itab_sap2-start_billdt
            itab_sap2-bill_doctype itab_sap2-main_trans
            itab_sap2-in_doctype itab_sap2-new_connection_tag
            itab_sap2-check_meter itab_sap2-rsg
            itab_sap2-recon_dt itab_sap2-recon_rdg
**    Added by GDIMALIWAT
            itab_sap2-acct_det
***  Added by Acucueco
            itab_sap2-gsber
*    Added by GDIMALIWAT
            itab_sap2-dis_chk
            "--Inserted by Ann 2012/08/14
            itab_sap2-hoa_pua    itab_sap2-hoa_or
            itab_sap2-hoa_paydt  itab_sap2-hoa_netvat
            itab_sap2-hoa_vat    itab_sap2-hoa_gross
            "/-Inserted by Ann 2012/08/14
            "-- Inserted by RANDYSY 09/25/2012
            itab_sap2-hoa_ind
            itab_sap2-water_ind
            itab_sap2-water_ind2
            itab_sap2-misc_ind
            itab_sap2-gd_ind
            "-- Inserted by RANDYSY 09/25/2012

            INTO itab_mr-mrdata_sap SEPARATED BY '|'.
*    CONDENSE ITAB_MR-MRDATA_SAP NO-GAPS.
    APPEND itab_mr.

*    itab_out-mru = itab_sap2-mru.
*    itab_out-details = itab_mr-mrdata_sap.
*    APPEND itab_out.

    APPEND INITIAL LINE TO itab_out ASSIGNING <fs_out>.
    <fs_out>-mru     = itab_sap2-mru.
    <fs_out>-mrid    = itab_sap2-mrid.
    <fs_out>-details = itab_mr-mrdata_sap.

*** beg BMABINI 03/15/2012 -output mrid, can, reading date, pua to ZOSB_PUA for OSB SOA
*    REFRESH gt_pua.
    APPEND INITIAL LINE TO gt_pua ASSIGNING <fs_pua>.

    <fs_pua>-mrid         = itab_sap2-mrid.
    <fs_pua>-can          = itab_sap2-vkont.
    <fs_pua>-reading_date = p_mrdate.
    <fs_pua>-pua          = itab_sap2-pua.

*    INSERT INTO zosb_pua VALUES <fs_pua>.
*** end BMABINI 03/15/2012 -output mrid, can, reading date, pua to ZOSB_PUA for OSB SOA

    itab_sap2-operncode = itab_sap2-opcode.
    WRITE: / rec COLOR COL_TOTAL INTENSIFIED.
    WRITE: 5 itab_sap2-mru,        14 itab_sap2-seq,
          19 itab_sap2-mrid,       30 itab_sap2-device,
          46 itab_sap2-mfgsernum,  61 itab_sap2-matnr,
          68 itab_sap2-mrnumber,   74 itab_sap2-mrnote,
          80 itab_sap2-name,      106 itab_sap2-address,
         146 itab_sap2-location,  167 itab_sap2-statnote+0(23),
         191 itab_sap2-instal,    203 itab_sap2-operncode,
         209 itab_sap2-prev_mrdate,
         222 itab_sap2-actual_prevread RIGHT-JUSTIFIED,
         232 itab_sap2-billed_prevread RIGHT-JUSTIFIED,
         240 itab_sap2-prevconsump RIGHT-JUSTIFIED,
         249 itab_sap2-aveconsump RIGHT-JUSTIFIED,
         260 itab_sap2-ratecateg,
         270 itab_sap2-expected RIGHT-JUSTIFIED,
         280 itab_sap2-istablart,
         283 itab_sap2-sanzpers,
***  Added by Acucueco
         290 itab_sap2-gsber.
  ENDLOOP.

  itab_cnt-mru = itab_sap2-mru.
  itab_cnt-count = c.
  APPEND itab_cnt.

  CLEAR itab_sap2.
  REFRESH itab_sap2.
*  SKIP.
  c_inactive = c2.
  CONDENSE c_inactive.

  c_active = c - c2.
  CONDENSE c_active.

ENDFORM.                    "TRANS_TEXT
*&---------------------------------------------------------------------*
*&      Form  relationship
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM relationship.
  DATA: lv_vstelle TYPE evbs-vstelle,
        lv_strerg2 TYPE evbs-str_erg2,
        lv_strerg4 TYPE evbs-str_erg4.

  SELECT SINGLE * FROM etdz WHERE equnr = itab-eqpt.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM easti WHERE logikzw = etdz-logikzw.
    IF sy-subrc = 0.
      itab_sap-opcode = easti-opcode.
    ENDIF.
  ENDIF.

*get location
  SELECT SINGLE devloc INTO itab_sap-devloc FROM egerh WHERE
         equnr = itab-eqpt AND bis >= p_mrdate AND ab <= p_mrdate.

  IF NOT itab_sap-devloc IS INITIAL.
    SELECT SINGLE stortzus INTO itab_sap-location FROM egpltx
           WHERE devloc = itab_sap-devloc AND spras = 'E'.

*if opcode <> 00; change name (from device location) and
*address (from premise)
    IF NOT itab_sap-opcode IS INITIAL.

*---->get name from IFLOTX via DEVLOC
      SELECT SINGLE pltxt INTO itab_sap-name FROM iflotx
                    WHERE tplnr = itab_sap-devloc AND
                          spras = 'E'.

*---->get address from IFLOT via DEVLOC
      SELECT SINGLE prems INTO lv_vstelle FROM iflot WHERE
                            tplnr = itab_sap-devloc.

      IF NOT lv_vstelle IS INITIAL.
        SELECT SINGLE str_erg2 str_erg4 anzpers
        INTO (lv_strerg2, lv_strerg4, anzpers)
               FROM evbs WHERE vstelle = lv_vstelle.
        itab_sap-sanzpers = anzpers.
        CONCATENATE lv_strerg2 lv_strerg4 INTO itab_sap-address
                               SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " relationship
*&---------------------------------------------------------------------*
*&      Form  get_mrnote
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mrnote.
  DATA: lv_ablbelnr TYPE eabl-ablbelnr.
  DATA: lv_adat     TYPE sy-datum.

  SELECT DISTINCT ablbelnr adat ablhinw ableser istablart INTO
        (lv_ablbelnr, lv_adat, itab-ablhinw, itab_sap-mrnumber,
         itab_sap-istablart)
                 FROM eabl WHERE equnr = itab-eqpt AND
                                 adat < p_mrdate AND
                                 adat <> '00000000'
                           ORDER BY adat DESCENDING.

    itab_sap-istablart = '01'.
    MODIFY itab.
    EXIT.
  ENDSELECT.
ENDFORM.                    " get_mrnote
*&---------------------------------------------------------------------*
*&      Form  prevread_aveconsump
*&---------------------------------------------------------------------*
FORM prevread_aveconsump.
  DATA: lv_y_verberw TYPE eablh-i_verberw.
  DATA: lv_y_zwsterw TYPE eablh-i_zwsterw.

  DATA: lv_x_verberw TYPE i.
  DATA: lv_x_zwsterw TYPE i.

  DATA: lv_equnr    TYPE eabl-equnr,
        lv_zwnummer TYPE eabl-zwnummer,
        x_adattats  TYPE eabl-adat,
        x_adat      TYPE eabl-adat,
        x_atim      TYPE eabl-atim,
        x_v_zwstndab TYPE eabl-v_zwstndab,
        x_n_zwstndab TYPE eabl-n_zwstndab,

        x_adatvor   TYPE eabl-adat,
        x_atimvor   TYPE eabl-atim,
        x_v_zwstvor TYPE eabl-v_zwstndab,
        x_n_zwstvor TYPE eabl-n_zwstndab.

  DATA: y_v_abrmenge TYPE dberchz-v_abrmenge.
  DATA: tx_rdtyp.
  DATA: consump TYPE i,
        tmpablesgr TYPE c LENGTH 2,
        tmpmrid TYPE c LENGTH 20,
        tmpequip TYPE c LENGTH 18,
        tmpadatsoll TYPE eablg-adatsoll.
  .

* GET PREVIOUS METER READING DATE, READING, AND CONSUMPTION
  REFRESH mreading.
  period = p_mrdate - 366.

  "-- Get Meter Reading ID of Previous Reading
  SELECT MAX( ablbelnr ) ablesgr adatsoll
    INTO (tmpmrid, tmpablesgr, tmpadatsoll)
  FROM   eablg
  WHERE  anlage   EQ itab_sap-instal
  AND    ablesgr  EQ '01'
  AND    adatsoll LT p_mrdate
  GROUP BY ablesgr adatsoll.
  ENDSELECT.

  IF tmpmrid EQ space.
    SELECT MAX( ablbelnr ) ablesgr adatsoll
      INTO (tmpmrid, tmpablesgr, tmpadatsoll)
    FROM   eablg
    WHERE  anlage   EQ itab_sap-instal
    AND    ablesgr  EQ '06'
    AND    adatsoll LT p_mrdate
    GROUP BY ablesgr adatsoll.
    ENDSELECT.
  ENDIF.

  SELECT SINGLE equnr erdat
    INTO (tmpequip, itab_sap-prev_mr_erdat)
  FROM   eabl
  WHERE  ablbelnr EQ tmpmrid.

  "-- Get Meter Reading Details of Previous Reading
  SELECT equnr zwnummer v_zwstand v_zwstndab adatsoll adat atim
         stanzvor stanznac zuorddat ablbelnr adattats adattats
    INTO (lv_equnr, lv_zwnummer, mreading-mrding, mreading-reading,
         mreading-mrsched, mreading-mrdate, mreading-mrtime,
         mreading-x_stanzvor, mreading-x_stanznac, mreading-x_zuorddat,
         mreading-x_ablbelnr, mreading-mrdate2, mreading-x_adattats )
  FROM   eabl
  WHERE  equnr    EQ tmpequip
  AND    adat     NE '00000000'
  AND    adatsoll LE tmpadatsoll.

    lv_equnr = tmpequip.

    IF mreading-mrdate EQ '00000000'.
      SELECT SINGLE bis
        INTO mreading-mrdate
      FROM   eanlh
      WHERE  anlage EQ itab_sap-instal.
    ENDIF.

    IF mreading-mrdate2 EQ '00000000'.
      mreading-mrdate2 = mreading-mrsched.
    ENDIF.

    IF tmpablesgr EQ '01'.
      mreading-newtag = space.
    ELSE.
      mreading-newtag = 'N'.
    ENDIF.

    APPEND mreading. CLEAR mreading.
  ENDSELECT.

  SORT mreading BY mrsched DESCENDING.
  CLEAR: num, prev1, prev2, perconsump.

  LOOP AT mreading.
    num = num + 1.

    IF num =  1.
      IF mreading-newtag = 'N'.
        itab_sap-new_connection_tag = 'N'.
      ELSE.
        itab_sap-new_connection_tag = space.
      ENDIF.

      itab_sap-prev_mrdate     = mreading-mrdate2.
      tmpablbelnr              = mreading-x_ablbelnr.
      itab_sap-actual_prevread = mreading-reading.
      itab_sap-billed_prevread = mreading-mrding.

      IF mreading-reading EQ mreading-mrding.
        tx_rdtyp     = 'S'.    "same
        x_v_zwstndab = mreading-mrding.
      ELSE.
        tx_rdtyp     = 'D'.    "diff
        x_v_zwstndab = mreading-reading.
      ENDIF.

      x_adat       = mreading-mrdate.
      x_atim       = mreading-mrtime.
      x_v_zwstndab = mreading-reading.

      CLEAR: x_n_zwstndab.

      PERFORM register_extrapolate USING lv_equnr
                                         lv_zwnummer
                                         x_adat
                                         x_atim
                                         mreading-x_stanzvor
                                         mreading-x_stanznac
                                         mreading-x_zuorddat
                                         mreading-x_ablbelnr
                                         lv_y_verberw
                                         lv_y_zwsterw.

      lv_x_verberw = trunc( lv_y_verberw ).

      IF itab_sap-new_connection_tag = 'N'.
        itab_sap-aveconsump = 0.
      ELSE.
        itab_sap-aveconsump = trunc( lv_x_verberw ).
      ENDIF.

      IF itab_sap-aveconsump = 0.
        SELECT SINGLE * FROM easte WHERE logikzw = etdz-logikzw AND
                                         bis >= p_mrdate.
        IF sy-subrc = 0.
          itab_sap-aveconsump = easte-perverbr.
          itab_sap-aveconsump = trunc( itab_sap-aveconsump / 12 ).
        ENDIF.
      ENDIF.

    ENDIF.

    IF num EQ 2.
      x_adatvor = mreading-mrdate.
      x_atimvor = mreading-mrtime.

      IF tx_rdtyp = 'D'.
        x_v_zwstvor = mreading-mrding.
      ELSE.
        x_v_zwstvor = mreading-mrding.
      ENDIF.

      CLEAR: x_n_zwstvor.

      CALL FUNCTION 'ISU_CONSUMPTION_DETERMINE'
        EXPORTING
          x_equnr           = lv_equnr
          x_zwnummer        = lv_zwnummer
          x_adat            = x_adat
          x_atim            = x_atim
          x_v_zwstndab      = x_v_zwstndab
          x_n_zwstndab      = x_n_zwstndab
          x_adatvor         = x_adatvor
          x_atimvor         = x_atimvor
          x_v_zwstvor       = x_v_zwstvor
          x_n_zwstvor       = x_n_zwstvor
        IMPORTING
          y_v_abrmenge      = y_v_abrmenge
        EXCEPTIONS
          general_fault     = 1
          zwstandab_missing = 2
          zwstand_missing   = 3
          parameter_missing = 4
          no_inst_structure = 5
          no_ratetyp        = 6
          OTHERS            = 7.

      IF sy-subrc = 0.
        IF itab_sap-new_connection_tag = 'N'.
          itab_sap-prevconsump = 0.
        ELSE.
          itab_sap-prevconsump = y_v_abrmenge.
        ENDIF.
      ELSE.
        CLEAR itab_sap-prevconsump.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: mreading, x_atim, lv_y_verberw.

  SELECT SINGLE equnr zwnummer stanzvor stanznac adatsoll zuorddat
  INTO   (lv_equnr, lv_zwnummer, mreading-x_stanzvor, mreading-x_stanznac,
         x_adat, mreading-x_zuorddat)
  FROM   eabl
  WHERE  equnr    EQ itab-eqpt
  AND    adat     EQ '00000000'
  AND    adatsoll EQ p_mrdate.

  IF sy-subrc EQ 0.
    PERFORM register_extrapolate USING lv_equnr
                                       lv_zwnummer
                                       x_adat
                                       x_atim
                                       mreading-x_stanzvor
                                       mreading-x_stanznac
                                       mreading-x_zuorddat
                                       mreading-x_ablbelnr
                                       lv_y_verberw
                                       lv_y_zwsterw.

    lv_x_zwsterw      = trunc( lv_y_zwsterw ).
    itab_sap-expected = lv_x_zwsterw.
  ELSE.
    CLEAR itab_sap-expected.
  ENDIF.
ENDFORM.                    " prevread_aveconsump1
*&---------------------------------------------------------------------*
*&      Form  register_extrapolate
*&---------------------------------------------------------------------*
FORM register_extrapolate USING    p_lv_equnr
                                   p_lv_zwnummer
                                   p_x_adat
                                   p_x_atim
                                   p_mreading_x_stanzvor
                                   p_mreading_x_stanznac
                                   p_mreading_x_zuorddat
                                   p_mreading_x_ablbelnr
                                   p_lv_y_verberw
                                   p_lv_y_zwsterw.

  CALL FUNCTION 'ISU_REGISTER_EXTRAPOLATION'
    EXPORTING
      x_equnr              = p_lv_equnr
      x_zwnummer           = p_lv_zwnummer
      x_adatsoll           = p_x_adat
      x_atimsoll           = p_x_atim
      x_stanzvor           = p_mreading_x_stanzvor
      x_stanznac           = p_mreading_x_stanznac
      x_zuorddat           = p_mreading_x_zuorddat
      x_ablbelnr           = p_mreading_x_ablbelnr
      x_actual             = space
      x_read_all_eabl      = 'X'
    IMPORTING
      y_verberw            = p_lv_y_verberw
      y_zwsterw            = p_lv_y_zwsterw
    EXCEPTIONS
      not_found            = 1
      system_error         = 2
      rate_not_found       = 3
      rate_without_operand = 4
      parameter_missing    = 5
      not_valid            = 6
      not_qualified        = 7
      invalid_interval     = 8
      general_fault        = 9
      no_extrapolation     = 10
      no_period_found      = 11
      internal_error       = 12
      OTHERS               = 13.

  IF sy-subrc <> 0.
    CLEAR: p_lv_y_verberw, p_lv_y_zwsterw.
  ENDIF.

ENDFORM.                    " register_extrapolate


*&---------------------------------------------------------------------*
*&      FORM BILLING_PERIOD AND ACCOUNT STATUS
*&---------------------------------------------------------------------*
FORM billing_period.
  DATA : discon_dt TYPE eablg-adatsoll,
         recon_dt  TYPE eablg-adatsoll,
         tag       TYPE c LENGTH 1,
         n_zwstand TYPE eabl-v_zwstand,
         cnt       TYPE i,
         newmeter_info TYPE bapieabl-device,
         initial_rdg TYPE c LENGTH 9,
         removal_dt TYPE bapieabl-actualmrdate,
         final_rdg TYPE c LENGTH 9,
         recon_rdg TYPE c LENGTH 9.

  DATA: BEGIN  OF mr OCCURS 0,
       adat TYPE eabl-adatsoll,
       n_zwstand TYPE eabl-v_zwstand,
       ablesgr TYPE eablg-ablesgr,
       gernr TYPE eabl-gernr,
       adatsoll TYPE eabl-adatsoll,
       END OF mr.

  "-- Get Ceiling of Billing Period
  SELECT SINGLE erdat
    INTO itab_sap-end_billdt
  FROM   eabl
  WHERE  ablbelnr = itab_sap-mrid.

*  itab_sap-end_billdt = mrdate.
  itab_sap-start_billdt = itab_sap-prev_mrdate + 1.

  itab_sap-bill_tr = '01'.
  itab_sap-bill_doctype = 'VA'.
  itab_sap-main_trans = '0100'.
  itab_sap-in_doctype = 'FA'.

  REFRESH mr.
  SELECT eabl~adat eabl~v_zwstand eablg~ablesgr eabl~gernr eabl~adatsoll
   INTO (mr-adat, mr-n_zwstand, mr-ablesgr, mr-gernr, mr-adatsoll)
   FROM eabl INNER JOIN eablg
   ON eabl~ablbelnr = eablg~ablbelnr
   WHERE eablg~anlage = itab_sap-instal
   AND eablg~adatsoll LT p_mrdate
   AND ( eablg~ablesgr EQ '13' OR eablg~ablesgr EQ '18').

    APPEND mr.
  ENDSELECT.

  IF sy-subrc NE 0.
    itab_sap-stat_flag = '1'.
  ENDIF.

  SORT mr BY adatsoll DESCENDING.

  cnt = 1.
  LOOP AT mr.
    IF cnt = 1.
      IF mr-ablesgr EQ '13'.
        itab_sap-stat_flag = '2'.
      ELSE.
        itab_sap-stat_flag = '1'.
      ENDIF.
    ENDIF.
    IF mr-ablesgr EQ '18'.
      IF mr-adat GT itab_sap-prev_mrdate.
        recon_dt = mr-adat.
        recon_rdg = mr-n_zwstand.
      ELSE.
        recon_rdg = space.
      ENDIF.
      EXIT.
    ENDIF.
    cnt = cnt + 1.
  ENDLOOP.

  REFRESH mr.
  SELECT eabl~adat eabl~v_zwstand eablg~ablesgr eabl~gernr eabl~adatsoll
   INTO (mr-adat, mr-n_zwstand, mr-ablesgr, mr-gernr, mr-adatsoll)
   FROM eabl INNER JOIN eablg
   ON eabl~ablbelnr = eablg~ablbelnr
   WHERE eablg~anlage = itab_sap-instal
   AND eablg~adatsoll LT p_mrdate.
*   AND ( EABLG~ABLESGR EQ '21' OR EABLG~ABLESGR EQ '22').
    APPEND mr.
  ENDSELECT.

  SORT mr BY adatsoll DESCENDING.
  tag = space.
  cnt = 1.

  CLEAR tag.

  LOOP AT mr.
    IF cnt = 1.
      IF mr-ablesgr EQ '21'.
        newmeter_info = mr-gernr.
        initial_rdg = mr-n_zwstand.
*            ITAB_SAP-REMOVAL_DT = MR-ADAT.
        tag = 'X'.
      ELSE.
        initial_rdg = space.
      ENDIF.
    ELSE.
      IF mr-ablesgr EQ '22'.
        final_rdg = mr-n_zwstand.
        removal_dt = mr-adat.
      ELSE.
        final_rdg = space.
      ENDIF.
      EXIT.
    ENDIF.

    cnt = cnt + 1.
  ENDLOOP.

  SELECT SINGLE abrdats INTO tmpbill_sched FROM eablg
     WHERE ablbelnr = tmpablbelnr.

  SELECT erchc~opbel
      INTO itab_sap-invoice
      FROM erchc INNER JOIN erch
      ON erchc~belnr = erch~belnr
      WHERE
            erchc~invoiced = 'X' AND
            erchc~spcanc = 'X' AND
            erch~abrdats =  tmpbill_sched
            AND erch~vkont = itab_sap-vkont.
  ENDSELECT.

  IF itab_sap-invoice EQ '999999999999'.
    itab_sap-invoice = space.
  ENDIF.

  SHIFT recon_rdg LEFT DELETING LEADING space.
  SHIFT initial_rdg LEFT DELETING LEADING space.
  SHIFT final_rdg LEFT DELETING LEADING space.

  itab_sap-newmeter_info = newmeter_info.
  itab_sap-recon_dt      = recon_dt.
  itab_sap-recon_rdg     = recon_rdg.
  itab_sap-initial_rdg   = initial_rdg.
  itab_sap-removal_dt    = removal_dt.
  itab_sap-final_rdg     =  final_rdg.
ENDFORM.                    "BILLING_PERIOD

*&---------------------------------------------------------------------*
*&      Form  BALANCE CHARGES
*&---------------------------------------------------------------------*
FORM balance_charges.
  TYPES: BEGIN OF ty_dfkkop,
         amount     TYPE betrw_kk,
         date       TYPE faedn_kk,
         hvorg      TYPE hvorg_kk,
         tvorg      TYPE tvorg_kk,
         opbel      TYPE opbel_kk,
         faedn      TYPE faedn_kk,
         opupk      TYPE opupk_kk,
       END OF ty_dfkkop.

  DATA:  sbetw        TYPE sbetw_kk,
         hvorg        TYPE hvorg_kk,
         tvorg        TYPE tvorg_kk,
         opbel        TYPE opbel_kk,
         faedn        TYPE faedn_kk,
         belzeile     TYPE belzeile,
         billdoc      TYPE e_belnr,

         othertax     TYPE betrw_kk,
         other        TYPE betrw_kk,
         store_bal    TYPE betrw_kk,
         tmpother     TYPE betrw_kk,
         meter        TYPE betrw_kk,
         "-- Added by GDIMALIWAT 12/04/2014 RFC#48
         instln_wat   TYPE betrw_kk,
         instln_sew   TYPE betrw_kk,
         instln_adv   TYPE betrw_kk,
         "-- End -- Added by GDIMALIWAT 12/04/2014 RFC#48
         pua          TYPE betrw_kk,
         reopen       TYPE betrw_kk,
         tmpamount    TYPE betrw_kk,
         tmpamount2   TYPE betrw_kk,
         tmpamount3   TYPE betrw_kk,
         otax         TYPE betrw_kk,
         gdep         TYPE betrw_kk,
         subtot       TYPE betrw_kk,
         drum         TYPE betrw_kk,
         puainstall   TYPE betrw_kk,
         pia          TYPE betrw_kk,
         pa           TYPE betrw_kk,
         powercost    TYPE betrw_kk,
         gd           TYPE betrw_kk,
         ar_adv       TYPE betrw_kk,
         installment  TYPE betrw_kk,
         date         TYPE faedn_kk,
         created      TYPE faedn_kk,
         amount       TYPE betrw_kk,
         "-- Added by GDIMALIWAT 12/03/2014 RFC#48
         septic_charge        TYPE betrw_kk,
         changesize_charge    TYPE betrw_kk,
         restoration_charge   TYPE betrw_kk,
         misc_charge          TYPE betrw_kk,
         install_water_charge TYPE betrw_kk,
         install_sewer_charge TYPE betrw_kk,
         advances             TYPE betrw_kk,
         pn_install_water     TYPE betrw_kk,
         pn_install_water_ind TYPE c LENGTH 5,
         pn_install_sewer     TYPE betrw_kk,
         pn_install_sewer_ind TYPE c LENGTH 5,
         pn_ar                TYPE betrw_kk,
         pn_ar_ind            TYPE c LENGTH 5,
         pn_adv               TYPE betrw_kk,
         pn_adv_ind           TYPE c LENGTH 5,
         "-- End -- Added by GDIMALIWAT 12/03/2014 RFC#48
         instmain     TYPE hvorg_kk,
         instsub      TYPE tvorg_kk,
         opupk        TYPE opupk_kk,
         opupw        TYPE opupw_kk,
         augbl        TYPE augbl_kk,

         lowest_date  TYPE sydatum,
         lowest_date2 TYPE sydatum,
         tmpdt60      TYPE sydatum,
         tmpdt45      TYPE sydatum,

         lv_pia       TYPE betrw_kk.

  DATA:  lt_fkkop     TYPE TABLE OF fkkop,
         lt_dfkkop    TYPE TABLE OF ty_dfkkop,
         lv_clr_amt   TYPE betrh_kk.

  DATA:  lv_orig_end  TYPE begabrpe.

  FIELD-SYMBOLS:
         <fs_fkkop>   TYPE fkkop,
         <fs_dfkkop>  TYPE ty_dfkkop.

  CLEAR: meter, reopen, instln_wat, instln_sew, instln_adv, other, otax, othertax, tmpother, store_bal, amount, sbetw, drum,
         puainstall, pia, pa, gd, ar_adv, installment.

  puainstall = 0.

  lowest_date  = '00000000'.
  lowest_date2 = '00000000'.
  created      = itab_sap-end_billdt.

  lv_orig_end  = itab_sap-end_billdt.
  itab_sap-end_billdt = p_mrdate.

  REFRESH: lt_dfkkop.

  SELECT betrw faedn hvorg tvorg opbel faedn opupk
    INTO TABLE lt_dfkkop
  FROM dfkkop
  WHERE vkont EQ itab_sap-vkont
  AND ( augst EQ space OR augdt GT itab_sap-end_billdt )
  AND abwbl   EQ space
  AND ( budat NE '00000000' )
  AND faedn   LT itab_sap-end_billdt
  AND abrzu   LT itab_sap-start_billdt
  AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
  AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) ).

  IF sy-subrc EQ 0.
    SORT lt_dfkkop BY date DESCENDING.

    LOOP AT lt_dfkkop ASSIGNING <fs_dfkkop>.
      IF <fs_dfkkop>-hvorg NE '0060' AND <fs_dfkkop>-hvorg NE '0625'.
        IF ( lowest_date = '00000000' AND date < itab_sap-end_billdt ).
          lowest_date = <fs_dfkkop>-date.
        ELSE.
          lowest_date = <fs_dfkkop>-date.
        ENDIF.
      ENDIF.

      IF   ( <fs_dfkkop>-hvorg = '0040' AND <fs_dfkkop>-tvorg = '0020' ).
        SELECT SUM( ibtrg )
        INTO (tmpother)
        FROM dfkkia
        WHERE iopbel = <fs_dfkkop>-opbel
        AND round = space.

        SELECT SUM( ibtrg )
        INTO (otax)
        FROM dfkkia
        WHERE iopbel = <fs_dfkkop>-opbel
        AND round = 'X'.

        other = other + tmpother.
        othertax = othertax + otax.

      ELSEIF ( <fs_dfkkop>-hvorg = '0300' AND   <fs_dfkkop>-tvorg = '0920' ) OR  "MIGDATA
             ( <fs_dfkkop>-hvorg = '0200' AND   <fs_dfkkop>-tvorg = '0020' ) OR  "FINAL BILLING
             ( <fs_dfkkop>-hvorg = '0300' AND   <fs_dfkkop>-tvorg = '0020' ) OR  "MANUAL BACKBILLING
             ( <fs_dfkkop>-hvorg = '0100' AND   <fs_dfkkop>-tvorg = '0020' ) OR  "BILLS
             ( <fs_dfkkop>-hvorg = '0100' AND   <fs_dfkkop>-tvorg = '0010' ) OR  "BILLS ADDITIONAL
             ( <fs_dfkkop>-hvorg = '0060' AND   <fs_dfkkop>-tvorg = '0010' ) OR  "Overpayment
*             ( <fs_dfkkop>-hvorg = '0625' AND   <fs_dfkkop>-tvorg = '0010' ) OR  "Reset cleared items
             ( <fs_dfkkop>-hvorg = '0600' AND   <fs_dfkkop>-tvorg = '0010' ) OR  "Open Payments
             ( <fs_dfkkop>-hvorg = '9950' AND   <fs_dfkkop>-tvorg = '0020' ) OR
             ( <fs_dfkkop>-hvorg = '0300' AND   <fs_dfkkop>-tvorg = '0940' ) OR
             ( <fs_dfkkop>-hvorg = '6005' AND ( <fs_dfkkop>-tvorg BETWEEN '0020' AND '0027' ) ) OR
             ( <fs_dfkkop>-hvorg = '0300' AND ( <fs_dfkkop>-tvorg BETWEEN '0510' AND '0528' ) ).       "CREDIT

        IF <fs_dfkkop>-faedn LT itab_sap-end_billdt.
          CLEAR: lv_pia. "lv_overdue.

*          "-- Get PIA
*          SELECT SUM( betrh )
*            INTO lv_overdue
*          FROM   dfkkopk
*          WHERE  opbel EQ <fs_dfkkop>-opbel
*          AND    hkont EQ '2160800000'
*          AND    mwskz EQ 'E0'.

          " GDIMALIWAT Commented 06/14/2013
*          "-- Get PIA Amount
*          SELECT SUM( betrh )
*            INTO lv_pia
*          FROM   dfkkop
*          WHERE  opbel  EQ <fs_dfkkop>-opbel
*          AND    mwszkz EQ 'E0'
*          AND    hvorg  EQ '0100'
*          AND    tvorg  EQ '0010'.
          "          store_bal = store_bal + <fs_dfkkop>-amount + lv_pia. "- lv_overdue.

          store_bal = store_bal + <fs_dfkkop>-amount.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT betrw    faedn hvorg  tvorg opbel  faedn   opupk  opupw INTO
         (amount, date, hvorg, tvorg, opbel, faedn, opupk, opupw) FROM dfkkop
         WHERE vkont = itab_sap-vkont
         AND ( budat NE '00000000' )
         AND abwbl = space
         AND ( faedn LE itab_sap-end_billdt AND faedn NE '00000000' )
         AND augst EQ space
         AND ( hvorg NE '0080' AND hvorg NE '0040' )
         AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
         AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) ).

    IF ( hvorg = '9000').
      IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
        IF tvorg EQ '0250' OR tvorg EQ '0020'.
          instln_wat = instln_wat + amount.
        ELSEIF tvorg EQ '0450'.
          instln_sew = instln_sew + amount.
        ENDIF.
      ENDIF.
      IF faedn LT itab_sap-start_billdt.
        puainstall = puainstall + amount.
      ENDIF.

    ELSE.
      IF ( hvorg = '7002').
        IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
          meter = meter + amount.
        ENDIF.
        IF faedn LT itab_sap-start_billdt.
          puainstall = puainstall + amount.
        ENDIF.
      ELSE.
        IF ( hvorg = '7003').
          IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
            reopen = reopen + amount.
          ENDIF.
          IF faedn LT itab_sap-start_billdt.
            puainstall = puainstall + amount.
          ENDIF.
        ELSE.
** GD MPBACANI
          IF ( hvorg = '6045').
            IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
              gd = gd + amount.
            ENDIF.
            IF faedn LT itab_sap-start_billdt.
              puainstall = puainstall + amount.
*              gd = gd + amount.
            ENDIF.
          ELSE.
* AR WRITE_OFF
            IF ( hvorg = '7004').
              IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                drum = drum + amount.
              ENDIF.
              IF faedn LT itab_sap-start_billdt.
                puainstall = puainstall + amount.
*                drum = drum + amount.
              ENDIF.
            ELSE.
* AR ADVANCES
              IF ( hvorg = '7008').
                IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                  instln_adv = instln_adv + amount.
                ENDIF.
                IF faedn LT itab_sap-start_billdt.
                  puainstall = puainstall + amount.
                ENDIF.
              ELSE.
                "-- Added by GDIMALIWAT 12/03/2014 RFC#48
                IF ( hvorg = '7007' ).
                  IF tvorg EQ '0020'.
                    septic_charge = septic_charge + amount.
                  ELSEIF tvorg EQ '0022'.
                    changesize_charge = changesize_charge + amount.
                  ELSEIF tvorg EQ '0027'.
                    restoration_charge = restoration_charge + amount.
                  ELSEIF tvorg EQ '0021' OR tvorg EQ '0023' OR tvorg EQ '0024' OR tvorg EQ '0025' OR tvorg EQ '0026' OR tvorg EQ '0028'.
                    misc_charge = misc_charge + amount.
                  ENDIF.
                ELSE.
                  "-- End -- Added by GDIMALIWAT 12/03/2014 RFC#48

                  IF ( hvorg = '0300' AND tvorg = '0920' ) OR   "MIGDATA
                          ( hvorg = '0200' AND tvorg = '0020' ) OR  "FINAL BILLING
                          ( hvorg = '0300' AND tvorg = '0020' ) OR  "MANUAL BACKBILLING
                          ( hvorg = '0100' AND tvorg = '0020' ) OR  "BILLS
                          ( hvorg = '0100' AND tvorg = '0010' ) OR  "BILLS ADDITIONAL
                          ( hvorg = '0060' AND tvorg = '0010' ) OR  "Overpayment
                          ( hvorg = '0600' AND tvorg = '0010' ) OR  "Open Payments
                          ( hvorg = '9950' AND tvorg = '0020' ) OR
                          ( hvorg = '0300' AND tvorg = '0940' ) OR
                    "-- Inserted by RANDYSY 08/31/2012 TN#18283
                          ( hvorg = '7009' AND tvorg = '0020' ) OR   "HOA Dues Debit
                    "-- Inserted by RANDYSY 08/31/2012 TN#18283
                          ( hvorg = '6005' AND (
                            tvorg BETWEEN '0020' AND '0027' ) ) OR
                          ( hvorg = '0300' AND (
                            tvorg BETWEEN '0510' AND '0528' ) ).       "CREDIT

                  ELSE.
                    IF faedn LE itab_sap-end_billdt.
                      puainstall = puainstall + amount.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR amount.
  ENDSELECT.

  SELECT betrw faedn hvorg tvorg opbel faedn opupk opupw INTO
         (amount, date, hvorg, tvorg, opbel, faedn, opupk, opupw) FROM dfkkop
         WHERE vkont = itab_sap-vkont
         AND ( budat NE '00000000' )
         AND abwbl = space
         AND ( faedn LE itab_sap-end_billdt AND faedn NE '00000000' )
         AND augst EQ space
         AND opupw = '000'
         AND hvorg = '0080' AND tvorg = '0010'
         AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
         AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) ).

    SELECT SINGLE hvorg tvorg
    INTO
    (instmain, instsub)
    FROM dfkkop
    WHERE abwbl = opbel.

    IF  ( instmain = '9000').
      IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
        IF tvorg EQ '0250' OR tvorg EQ '0020'.
          instln_wat = instln_wat + amount.
        ELSEIF tvorg EQ '0450'.
          instln_sew = instln_sew + amount.
        ENDIF.
      ENDIF.
      IF faedn LT itab_sap-start_billdt.
        puainstall = puainstall + amount.
      ENDIF.
    ELSE.
      IF ( instmain = '7002').
        IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
          meter = meter + amount.
        ENDIF.
        IF faedn LT itab_sap-start_billdt.
          puainstall = puainstall + amount.
        ENDIF.
      ELSE.
        IF ( instmain = '7003').
          IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
            reopen = reopen + amount.
          ENDIF.
          IF faedn LT itab_sap-start_billdt.
            puainstall = puainstall + amount.
          ENDIF.
        ELSE.

** GD MPBACANI
          IF ( instmain = '6045').
            IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
              gd = gd + amount.
            ENDIF.
            IF faedn LT itab_sap-start_billdt.
              puainstall = puainstall + amount.
*              gd = gd + amount.
            ENDIF.
          ELSE.
* AR WRITE_OFF
            IF ( instmain = '7004').
              IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                drum = drum + amount.
              ENDIF.
              IF faedn LT itab_sap-start_billdt.
                puainstall = puainstall + amount.
*                drum = drum + amount.
              ENDIF.
            ELSE.
* AR ADVANCES
              IF ( instmain = '7008').
                IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                  instln_adv = instln_adv + amount.
                ENDIF.
                IF faedn LT itab_sap-start_billdt.
                  puainstall = puainstall + amount.
                ENDIF.
              ELSE.
                "-- Added by GDIMALIWAT 12/03/2014 RFC#48
                IF ( hvorg = '7007' ).
                  IF tvorg EQ '0020'.
                    septic_charge = septic_charge + amount.
                  ELSEIF tvorg EQ '0022'.
                    changesize_charge = changesize_charge + amount.
                  ELSEIF tvorg EQ '0027'.
                    restoration_charge = restoration_charge + amount.
                  ELSEIF tvorg EQ '0021' OR tvorg EQ '0023' OR tvorg EQ '0024' OR tvorg EQ '0025' OR tvorg EQ '0026' OR tvorg EQ '0028'.
                    misc_charge = misc_charge + amount.
                  ENDIF.
                ELSE.
                  "-- End -- Added by GDIMALIWAT 12/03/2014 RFC#48

                  IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                    installment = installment + amount.
                  ENDIF.
                  IF faedn LT itab_sap-start_billdt.
                    puainstall = puainstall + amount.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR amount.
  ENDSELECT.

  SELECT opbel faedn opupw whanz augbl
    INTO (opbel, faedn, opupw, opupk, augbl)
  FROM   dfkkopw
  WHERE  vkont EQ itab_sap-vkont
  AND    faedn LE itab_sap-end_billdt
  AND augrd EQ space.

    IF augbl IS NOT INITIAL.
      "-- Refresh Internal Table for Cleared Items
      REFRESH: lt_fkkop.

      CLEAR: lv_clr_amt.

      "-- Get Cleared Item Amount for the selected month
      CALL FUNCTION 'FKK_GET_CLEARED_ITEMS'
        EXPORTING
          i_augbl = augbl
        TABLES
          t_fkkop = lt_fkkop.

      IF lt_fkkop[] IS NOT INITIAL.
        READ TABLE lt_fkkop ASSIGNING <fs_fkkop> WITH KEY faedn = faedn.
        IF sy-subrc EQ 0.
          IF <fs_fkkop>-augdt GE p_mrdate.
            lv_clr_amt = 0.
          ELSE.
            lv_clr_amt = <fs_fkkop>-augbt.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE hvorg tvorg
    INTO
    (instmain, instsub)
    FROM dfkkop
    WHERE abwbl = opbel.

    SELECT SINGLE betrw
    INTO (amount)
    FROM dfkkop
    WHERE  opupk = opupk
    AND opupw = '000'
    AND opbel = opbel.

    CLEAR tmpamount.

    SELECT SUM( betrw )
    INTO (tmpamount) FROM dfkkop
    WHERE opbel = opbel
    AND ( augst = space OR augdt GT itab_sap-end_billdt )
    AND opupw = opupw
    AND opupk = opupk
    AND faedn LE itab_sap-end_billdt.

    CLEAR tmpamount2.

    SELECT SUM( betrw )
      INTO (tmpamount2) FROM dfkkop
    WHERE opbel = opbel
    AND ( augst NE space AND augdt LT itab_sap-end_billdt )
    AND opupw = opupw
    AND opupk = opupk
    AND faedn LE itab_sap-end_billdt.

    IF tmpamount >= 0 AND tmpamount2 > 0.
      amount = tmpamount.
    ENDIF.

    amount = amount - lv_clr_amt.

    IF amount GT 0.

      IF  ( instmain = '9000').
        IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
          IF tvorg EQ '0250' OR tvorg EQ '0020'.
            instln_wat = instln_wat + amount.
          ELSEIF tvorg EQ '0450'.
            instln_sew = instln_sew + amount.
          ENDIF.
        ENDIF.
        IF faedn LT itab_sap-start_billdt.
          puainstall = puainstall + amount.
        ENDIF.
      ELSE.
        IF ( instmain = '7002').
          IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
            meter = meter + amount.
          ENDIF.
          IF faedn LT itab_sap-start_billdt.
            puainstall = puainstall + amount.
          ENDIF.
        ELSE.
          IF ( instmain = '7003').
            IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
              reopen = reopen + amount.
            ENDIF.
            IF faedn LT itab_sap-start_billdt.
              puainstall = puainstall + amount.
            ENDIF.
          ELSE.
** GD MPBACANI
            IF ( instmain = '6045').
              IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                gd = gd + amount.
              ENDIF.
              IF faedn LT itab_sap-start_billdt.
                puainstall = puainstall + amount.
*                gd = gd + amount.
              ENDIF.
            ELSE.
* AR WRITE_OFF
              IF ( instmain = '7004').
                IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                  drum = drum + amount.
                ENDIF.
                IF faedn LT itab_sap-start_billdt.
                  puainstall = puainstall + amount.
*                drum = drum + amount.
                ENDIF.
              ELSE.
* AR ADVANCES
                IF ( instmain = '7008').
                  IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                    instln_adv = instln_adv + amount.
                  ENDIF.
                  IF faedn LT itab_sap-start_billdt.
                    puainstall = puainstall + amount.
                  ENDIF.
                ELSE.
                  "-- Added by GDIMALIWAT 12/03/2014 RFC#48
                  IF ( hvorg = '7007' ).
                    IF tvorg EQ '0020'.
                      septic_charge = septic_charge + amount.
                    ELSEIF tvorg EQ '0022'.
                      changesize_charge = changesize_charge + amount.
                    ELSEIF tvorg EQ '0027'.
                      restoration_charge = restoration_charge + amount.
                    ELSEIF tvorg EQ '0021' OR tvorg EQ '0023' OR tvorg EQ '0024' OR tvorg EQ '0025' OR tvorg EQ '0026' OR tvorg EQ '0028'.
                      misc_charge = misc_charge + amount.
                    ENDIF.
                  ELSE.
                    "-- End -- Added by GDIMALIWAT 12/03/2014 RFC#48

                    IF faedn LE itab_sap-end_billdt AND faedn GE itab_sap-start_billdt.
                      installment = installment + amount.
                    ENDIF.
                    IF faedn LT itab_sap-start_billdt.
                      puainstall = puainstall + amount.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR amount.
  ENDSELECT.

  itab_sap-pua            = store_bal + other + othertax + puainstall.
*  itab_sap-other_charges  = drum.
  itab_sap-meter_charges  = meter.
  itab_sap-re_opening_fee = reopen.
  itab_sap-gd             = gd.
*  itab_sap-installment    = installment.
*  itab_sap-install_charge = instln.

  "-- Added by GDIMALIWAT 12/03/2014 RFC#48
  itab_sap-septic_charge        = septic_charge.
  itab_sap-changesize_charge    = changesize_charge.
  itab_sap-restoration_charge   = restoration_charge.
  itab_sap-misc_charge          = misc_charge.
  itab_sap-install_water_charge = instln_wat.
  itab_sap-install_sewer_charge = instln_sew.
  itab_sap-advances             = advances.

  PERFORM get_installment_due USING    itab_sap-vkont
                                       itab_sap-start_billdt
                                       itab_sap-end_billdt
                              CHANGING itab_sap-pn_install_water
                                       itab_sap-pn_install_sewer
                                       itab_sap-pn_adv
                                       itab_sap-pn_ar
                                       itab_sap-pn_install_water_ind
                                       itab_sap-pn_install_sewer_ind
                                       itab_sap-pn_adv_ind
                                       itab_sap-pn_ar_ind
                                       sy-subrc.

  "-- End -- Added by GDIMALIWAT 12/03/2014 RFC#48

  DATA: lv_subrc TYPE sysubrc.


  IF ( lowest_date2 < lowest_date AND lowest_date2 NE '00000000' ).
    lowest_date = lowest_date2.
  ENDIF.

  IF itab_sap-pua LT 1.
    itab_sap-disc_tag = '0'.
  ELSEIF itab_sap-pua GT 0.

    IF ( store_bal > 0  ).
      IF ( store_bal <  80 ).
        itab_sap-disc_tag = '0'.
      ELSE.
        tmpdt60 = lowest_date + 40. "60.

        IF ( tmpdt60 < created ).
          itab_sap-disc_tag = '2'.
        ELSE.
          itab_sap-disc_tag = '1'.
        ENDIF.
      ENDIF.
    ELSE.
      itab_sap-disc_tag = '0'.
    ENDIF.
  ENDIF.

  itab_sap-end_billdt = lv_orig_end.
ENDFORM.                    "balance_charges

*---------------------------------------------------------------------*
*       FORM PREVIOUS_PAYMENT                                      *
*---------------------------------------------------------------------*
FORM previous_payment.
  DATA: BEGIN OF paydoc OCCURS 0,
        budat    TYPE c LENGTH 10,
        paydoc   TYPE c LENGTH 12,
        tag      TYPE c LENGTH 4,
        amount   TYPE dfkkop-betrw,
        vat      TYPE dfkkop-betrw,
        netofvat TYPE dfkkop-betrw,
        END OF paydoc.

  DATA: txtbudat    TYPE c LENGTH 10,
        txtpaydoc   TYPE c LENGTH 12,
        txtamount   TYPE c LENGTH 15,
        txtvat      TYPE c LENGTH 15,
        txtnetofvat TYPE c LENGTH 15.

  DATA: amt  TYPE dfkkop-betrw,
        amt2 TYPE dfkkop-betrw,
        totwb TYPE dfkkop-betrw,
        totgd TYPE dfkkop-betrw,
        totmisc TYPE dfkkop-betrw,
        totvatwb TYPE dfkkop-betrw,
        totvatmisc TYPE dfkkop-betrw,
        instwb TYPE dfkkop-betrw,
        instgd TYPE dfkkop-betrw,
        instmisc TYPE dfkkop-betrw,
        instvatwb TYPE dfkkop-betrw,
        instvatmisc TYPE dfkkop-betrw,
        tot TYPE dfkkop-betrw,
        total TYPE dfkkop-betrw,
        augbt TYPE dfkkop-betrw,
        augbd TYPE dfkkop-augdt,
        augbl TYPE dfkkop-augbl,
        hvorg TYPE dfkkop-hvorg,
        tvorg TYPE dfkkop-tvorg,
        xblnr TYPE dfkkop-xblnr,
        tmpopbel TYPE c LENGTH 12,
        opbel    TYPE c LENGTH 12,
        tmpdoc   TYPE c LENGTH 12,
        augbl2 TYPE dfkkop-augbl,
        sctax  TYPE dfkkop-sctax,
        instmain TYPE dfkkop-hvorg,
        instsub  TYPE dfkkop-tvorg,
        augbl3   TYPE dfkkop-augbl.

  DATA: vat TYPE dberdl-sbetw,
        totvat TYPE dberdl-sbetw,
        vatamt TYPE dberdl-sbetw,
        totcurrent TYPE dberdl-sbetw,
        netofvat TYPE erdk-total_amnt,
        current TYPE dberdl-sbasw,
        multiplicand TYPE p.

  DATA: txtdoc TYPE c LENGTH 400.
  DATA: tmpbp  TYPE fkkvkp-gpart,
        tmpbp2 TYPE fkkvkp-gpart,
        tmpcan TYPE fkkvkp-vkont,
        txttag TYPE c LENGTH 4,
        conacct TYPE fkkvkp-vkont,
        paydt TYPE dfkkop-augdt.

  DATA: cnt TYPE i.
  DATA: cnt2 TYPE i.
  DATA: cnt3 TYPE i.

  DATA: water_or TYPE c LENGTH 12,
        water_pydt TYPE dfkkop-augdt,
        water_net TYPE c LENGTH 11,
        water_vat TYPE c LENGTH 11,
        water_total TYPE c LENGTH 11,
        water_or2 TYPE c LENGTH 12,
        water_pydt2 TYPE dfkkop-augdt,
        water_net2 TYPE c LENGTH 11,
        water_vat2 TYPE c LENGTH 11,
        water_total2 TYPE c LENGTH 11,
        misc_or TYPE c LENGTH 12,
        misc_pydt TYPE dfkkop-augdt,
        misc_net TYPE c LENGTH 11,
        misc_vat TYPE c LENGTH 11,
        misc_total TYPE c LENGTH 11,
        gd_or TYPE c LENGTH 12,
        gd_pydt TYPE dfkkop-augdt,
        gd_net TYPE c LENGTH 11.

  DATA: pvat TYPE p.
  pvat = '0.0'.
  tot = 0.

  tmpbp2 = buspart.
  tmpbp = buspart.
  conacct = itab_sap-vkont.
  tmpcan = itab_sap-vkont.

  SHIFT tmpbp LEFT DELETING LEADING '0'.
  SHIFT conacct LEFT DELETING LEADING '0'.

***** MAIN PAYMENT LOOP *****
  SELECT opbel budat betrz
  INTO   (augbl2, augbd, amt)
  FROM   dfkkzp
  WHERE budat LE itab_sap-end_billdt
  AND   budat GE itab_sap-start_billdt
*  AND   budat GE itab_sap-prev_mr_erdat
  AND   opbel NE '*'
  AND   opbel NE space
  AND   ruear EQ space
  AND   ( ( selw1 EQ conacct OR selw1 EQ tmpbp OR selw1 EQ tmpcan OR selw1 EQ tmpbp2 ) OR
          ( selw2 EQ conacct OR selw2 EQ tmpbp OR selw2 EQ tmpcan OR selw2 EQ tmpbp2 ) OR
          ( selw3 EQ conacct OR selw3 EQ tmpbp OR selw3 EQ tmpcan OR selw3 EQ tmpbp2 )
        ).

* unAllocated or Hang Payments
    tot = 0.
    amt2 = 0.
    vatamt = 0.
    SELECT hvorg tvorg opbel SUM( betrw )
    INTO (hvorg, tvorg, opbel, amt2)
    FROM dfkkop
    WHERE opbel EQ augbl2 AND
          augrd NE '05'
    GROUP BY hvorg tvorg opbel.

      IF ( hvorg = '0060' OR  hvorg = '0600' ).
        tot = tot + ( amt2 * -1 ).
      ENDIF.

    ENDSELECT.

    paydoc-paydoc = augbl2.
    paydoc-amount = tot.
    total = tot.
    totvat = 0.
    totcurrent = 0.

    SHIFT paydoc-paydoc LEFT DELETING LEADING '0'.
    paydoc-vat = totvat.
    paydoc-netofvat = paydoc-amount.
    paydoc-budat = augbd.
    amt2 = 0.
    vatamt = 0.
    totwb = 0.
    totgd = 0.
    totmisc = 0.
    totvatwb = 0.
    totvatmisc = 0.
    instwb = 0.
    instgd = 0.
    instmisc = 0.
    instvatwb = 0.
    instvatmisc = 0.

* For Regular Payment **
    SELECT hvorg tvorg augbl SUM( betrw ) SUM( sctax )
    INTO (hvorg, tvorg, augbl, amt2, sctax)
    FROM dfkkop
    WHERE augbl EQ augbl2
          AND augrd NE '05'
*          AND abwbl EQ space  "-- Removed by RANDYSY to capture 7008, 7004, 7003, 7002 & 9000
          AND hvorg NE '0080'
    GROUP BY hvorg tvorg augbl.

      IF ( hvorg = '0100' OR hvorg = '0300' OR hvorg = '0040' OR hvorg = '0060'  ).
        totwb = totwb + amt2.
        totvatwb = totvatwb + sctax.
      ELSE.
        IF ( hvorg EQ '6040' OR hvorg EQ '6045' OR hvorg EQ '6080' ).
          totgd = totgd + amt2.
        ELSE.
          totmisc = totmisc + amt2.
          totvatmisc = totvatmisc + sctax.
        ENDIF.
      ENDIF.
      CLEAR amt2.
      CLEAR sctax.

    ENDSELECT.

* For Installment **
    SELECT hvorg tvorg opbel betrw sctax
    INTO (hvorg, tvorg, opbel, amt2, sctax)
    FROM dfkkop
    WHERE augbl EQ augbl2
          AND augrd NE '05'
          AND abwbl EQ space
          AND ( hvorg EQ '0080' AND tvorg EQ '0010' ).

      SELECT SINGLE hvorg tvorg
      INTO
      (instmain, instsub)
      FROM dfkkop
      WHERE abwbl = opbel.

      IF ( instmain = '0100' OR instmain = '0300' OR instmain = '0040' ).
        instwb = instwb + amt2.
      ELSE.
        IF ( hvorg EQ '6040' OR hvorg EQ '6045' OR hvorg EQ '6080' ).
          instgd = instgd + amt2.
        ELSE.
          instmisc = instmisc + amt2.
        ENDIF.
      ENDIF.
      CLEAR amt2.
    ENDSELECT.

    instvatwb     = ( ( instwb / '1.12' ) * '0.12' ).
    instvatmisc   = ( ( instmisc / '1.12' ) * '0.12' ).
    paydoc-paydoc = augbl2.
    SHIFT paydoc-paydoc LEFT DELETING LEADING '0'.
    paydoc-budat  = augbd.

* Append to paydoc all details for WB **
    paydoc-amount   = totwb + tot + instwb.
    paydoc-vat      = totvatwb + instvatwb.
    paydoc-netofvat = paydoc-amount - paydoc-vat.
    paydoc-tag      = 'WB'.

    IF ( paydoc-amount > 0 ).
      APPEND paydoc.
    ENDIF.

* Append to paydoc all details for GD **
    paydoc-amount   = totgd + instgd.
    paydoc-vat      = 0.
    paydoc-netofvat = paydoc-amount.
    paydoc-tag      = 'GD'.

    IF ( paydoc-amount > 0 ).
      APPEND paydoc.
    ENDIF.

* Append to paydoc all details for MISC **
    paydoc-amount   = totmisc + instmisc.
    paydoc-vat      = totvatmisc + instvatmisc.
    paydoc-netofvat = paydoc-amount - paydoc-vat.
    paydoc-tag      = 'MISC'.

    IF ( paydoc-amount > 0 ).
      APPEND paydoc.
    ENDIF.

  ENDSELECT.

  txtdoc = ''.
  txttag = ''.

  SORT paydoc BY tag ASCENDING paydoc ASCENDING.

  cnt = 1.
  cnt2 = 1.
  cnt3 = 1.
  LOOP AT paydoc WHERE tag EQ 'WB'.
    IF cnt = 1.
      water_pydt = paydoc-budat.
      water_or = paydoc-paydoc.
      water_total = paydoc-amount.
      water_net = paydoc-netofvat.
      water_vat = paydoc-vat.
      SHIFT water_or LEFT DELETING LEADING '0'.
    ELSE.
      IF cnt = 2.
        water_pydt2 = paydoc-budat.
        water_or2 = paydoc-paydoc.
        water_total2 = paydoc-amount.
        water_net2 = paydoc-netofvat.
        water_vat2 = paydoc-vat.
        SHIFT water_or2 LEFT DELETING LEADING '0'.
      ENDIF.
    ENDIF.
    cnt = cnt + 1.
  ENDLOOP.

  LOOP AT paydoc WHERE tag NE 'WB'.
    IF paydoc-tag = 'MISC'.
      misc_pydt = paydoc-budat.
      misc_or = paydoc-paydoc.
      misc_total = paydoc-amount.
      misc_net = paydoc-netofvat.
      misc_vat = paydoc-vat.
      SHIFT misc_or LEFT DELETING LEADING '0'.
    ELSE.
      gd_pydt = paydoc-budat.
      gd_or = paydoc-paydoc.
      gd_net = paydoc-netofvat.
      SHIFT gd_or LEFT DELETING LEADING '0'.
    ENDIF.
  ENDLOOP.

  itab_sap-water_or       =  water_or.
  itab_sap-water_pydt     =  water_pydt.
  itab_sap-water_net      =  water_net.
  itab_sap-water_vat      =  water_vat.
  itab_sap-water_total    =  water_total.

  itab_sap-water_or2      =  water_or2.
  itab_sap-water_pydt2    =  water_pydt2.
  itab_sap-water_net2     =  water_net2.
  itab_sap-water_vat2     =  water_vat2.
  itab_sap-water_total2   =  water_total2.

  itab_sap-misc_or        =  misc_or.
  itab_sap-misc_pydt      =  misc_pydt.
  itab_sap-misc_net       =  misc_net.
  itab_sap-misc_vat       =  misc_vat.
  itab_sap-misc_total     =  misc_total.

  itab_sap-gd_or          =  gd_or.
  itab_sap-gd_pydt        =  gd_pydt.
  itab_sap-gd_net         =  gd_net.
ENDFORM.                    "previous_payment

*&---------------------------------------------------------------------*
*& Get Previous Payment
*&---------------------------------------------------------------------*
FORM previous_payment2 USING    pv_can           TYPE vkont_kk
                                pv_buspart       TYPE gpart_kk
                                pv_from_date     TYPE begabrpe
                                pv_to_date      TYPE endabrpe
                       CHANGING pv_water_or      TYPE char12
                                pv_water_pydt    TYPE augdt_kk
                                pv_water_net     TYPE char11
                                pv_water_vat     TYPE char11
                                pv_water_total   TYPE char11
                                pv_water_or2     TYPE char12
                                pv_water_pydt2   TYPE augdt_kk
                                pv_water_net2    TYPE char11
                                pv_water_vat2    TYPE char11
                                pv_water_total2  TYPE char11
                                pv_misc_or       TYPE char12
                                pv_misc_pydt     TYPE augdt_kk
                                pv_misc_net      TYPE char11
                                pv_misc_vat      TYPE char11
                                pv_misc_total    TYPE char11
                                pv_gd_or         TYPE char12
                                pv_gd_pydt       TYPE augdt_kk
                                pv_gd_net        TYPE char11.

  TYPES: BEGIN OF ty_dfkkzp,
            doc_no          TYPE augbl_kk,
            post_date       TYPE budat_kk,
            amount          TYPE betrz_kk,
         END OF ty_dfkkzp,

         BEGIN OF ty_paydoc,
            budat           TYPE char10,
            paydoc          TYPE char12,
            tag             TYPE char04,
            amount          TYPE betrw_kk,
            vat             TYPE betrw_kk,
            netofvat        TYPE betrw_kk,
         END OF ty_paydoc.

  DATA:  lv_can             TYPE vkont_kk,
         lv_buspart         TYPE gpart_kk,

         lv_tag             TYPE string,

         lv_vat             TYPE betrw_kk,

         lv_water           TYPE betrw_kk,
         lv_gd              TYPE betrw_kk,
         lv_misc            TYPE betrw_kk,

         lv_water_vat       TYPE betrw_kk,
         lv_misc_vat        TYPE betrw_kk,

         lv_counter         TYPE i,

         lv_augbl           TYPE augbl_kk, "-- Added by RANDYSY 11/07/2014 ( KBOX#67280 / SD#234 )

         lt_dfkkzp          TYPE TABLE OF ty_dfkkzp,
         lt_fkkop           TYPE TABLE OF fkkop,
         lt_paydoc          TYPE TABLE OF ty_paydoc.

  FIELD-SYMBOLS:
         <fs_dfkkzp>        TYPE ty_dfkkzp,
         <fs_fkkop>         TYPE fkkop,
         <fs_paydoc_check>  TYPE ty_paydoc,
         <fs_paydoc>        TYPE ty_paydoc.

  "-- Macro Definition for Appending Payment Document Table
  DEFINE mc_append_paydoc.
    append initial line to lt_paydoc assigning <fs_paydoc>.
    <fs_paydoc>-budat    = &1.
    <fs_paydoc>-paydoc   = &2.
    <fs_paydoc>-amount   = &3.
    <fs_paydoc>-vat      = &4.
    <fs_paydoc>-tag      = &5.
    <fs_paydoc>-netofvat = <fs_paydoc>-amount - <fs_paydoc>-vat.
  END-OF-DEFINITION.

  "-- Macro Definition for Updating or Appending Payment Document Table
  DEFINE mc_update_append_paydoc.
    read table lt_paydoc assigning <fs_paydoc_check> with key tag = &5.
    if sy-subrc eq 0.
      <fs_paydoc_check>-budat    = &1.
      <fs_paydoc_check>-paydoc   = &2.
      <fs_paydoc_check>-amount   = &3.
      <fs_paydoc_check>-vat      = &4.
      <fs_paydoc_check>-tag      = &5.
      <fs_paydoc_check>-netofvat = <fs_paydoc_check>-amount - <fs_paydoc_check>-vat.
    else.
      append initial line to lt_paydoc assigning <fs_paydoc>.
      <fs_paydoc>-budat    = &1.
      <fs_paydoc>-paydoc   = &2.
      <fs_paydoc>-amount   = &3.
      <fs_paydoc>-vat      = &4.
      <fs_paydoc>-tag      = &5.
      <fs_paydoc>-netofvat = <fs_paydoc>-amount - <fs_paydoc>-vat.
    endif.
  END-OF-DEFINITION.

  "-- Prepare Local Variables
  lv_can     = pv_can.
  lv_buspart = pv_buspart.

  SHIFT: lv_can     LEFT DELETING LEADING '0',
         lv_buspart LEFT DELETING LEADING '0'.

  "-- Reinitialize Payment tables
  REFRESH: lt_dfkkzp, lt_paydoc.

  "-- Fix Zero Paddings
  mc_conversion_alpha: pv_can, pv_buspart.

  "-- Get Payments made
  SELECT opbel budat betrz
    INTO TABLE lt_dfkkzp
  FROM   dfkkzp
  WHERE  budat LE pv_to_date
  AND    budat GE pv_from_date
  AND    opbel NE '*'
  AND    opbel NE space
  AND    ruear EQ space
  AND    ( ( selw1 EQ lv_can OR selw1 EQ lv_buspart OR selw1 EQ pv_can OR selw1 EQ pv_buspart ) OR
           ( selw2 EQ lv_can OR selw2 EQ lv_buspart OR selw2 EQ pv_can OR selw2 EQ pv_buspart ) OR
           ( selw3 EQ lv_can OR selw3 EQ lv_buspart OR selw3 EQ pv_can OR selw3 EQ pv_buspart )
         ).

  IF sy-subrc EQ 0.
    LOOP AT lt_dfkkzp ASSIGNING <fs_dfkkzp>.
      "-- Reinitialize Clearing Table
      REFRESH: lt_fkkop.

      "-- Get Cleared Items Amount
      CALL FUNCTION 'FKK_GET_CLEARED_ITEMS'
        EXPORTING
          i_augbl = <fs_dfkkzp>-doc_no
        TABLES
          t_fkkop = lt_fkkop.

      "-- start - Added by RANDYSY 11/07/2014 ( KBOX#67280 / SD#234 )
      IF lt_fkkop[] IS INITIAL.
        "-- Get Clearing document number
        SELECT SINGLE augbl
          INTO lv_augbl
        FROM   dfkkop
        WHERE  opbel EQ <fs_dfkkzp>-doc_no.

        "-- Get Cleared Items Amount
        CALL FUNCTION 'FKK_GET_CLEARED_ITEMS'
          EXPORTING
            i_augbl = lv_augbl
          TABLES
            t_fkkop = lt_fkkop.
      ENDIF.

      "-- end - Added by RANDYSY 11/07/2014 ( KBOX#67280 / SD#234 )

      IF lt_fkkop[] IS NOT INITIAL.
        LOOP AT lt_fkkop ASSIGNING <fs_fkkop>.
          IF <fs_fkkop>-betrw GT 0.

            "-- Reinitialize Tax Variable
            CLEAR: lv_vat.

            "-- Get VAT Amount
            SELECT SINGLE sctax
              INTO lv_vat
            FROM   dfkkop
            WHERE  augbl EQ <fs_dfkkzp>-doc_no
            AND    abwbl EQ <fs_fkkop>-opbel
            AND    opupz EQ <fs_fkkop>-opupz.

            IF     <fs_fkkop>-hvorg EQ '0100' OR
                   <fs_fkkop>-hvorg EQ '0300' OR
                   <fs_fkkop>-hvorg EQ '0040' OR
                   <fs_fkkop>-hvorg EQ '0060' OR
                   <fs_fkkop>-hvorg EQ '0600'.
              "-- Water Bill Payment
              lv_water     = lv_water     + <fs_fkkop>-betrw.
              lv_water_vat = lv_water_vat + lv_vat.

            ELSEIF <fs_fkkop>-hvorg EQ '6040' OR
                   <fs_fkkop>-hvorg EQ '6045' OR
                   <fs_fkkop>-hvorg EQ '6080'.
              "-- GD Payment
              lv_gd = lv_gd + <fs_fkkop>-betrw.

            ELSE.
              "-- Miscellaneous Payment
              lv_misc     = lv_misc     + <fs_fkkop>-betrw.
              lv_misc_vat = lv_misc_vat + lv_vat.

            ENDIF.
          ENDIF.
        ENDLOOP.

        "-- Append to Payment Document Table - Water Bill
        IF lv_water GT 0.
          mc_append_paydoc: <fs_dfkkzp>-post_date <fs_dfkkzp>-doc_no lv_water lv_water_vat 'WB'.
        ENDIF.

        "-- Append to Payment Document Table - GD
        IF lv_gd GT 0.
          "-- Check if 'GD' Already Exists, Update or Append
          mc_update_append_paydoc: <fs_dfkkzp>-post_date <fs_dfkkzp>-doc_no lv_gd 0 'GD'.

        ENDIF.

        "-- Append to Payment Document Table - Miscellaneous Fees
        IF lv_misc GT 0.
          "-- Check if 'MISC' Already Exists, Update or Append
          mc_update_append_paydoc: <fs_dfkkzp>-post_date <fs_dfkkzp>-doc_no lv_misc lv_misc_vat 'MISC'.
        ENDIF.

        "-- Reinitialize Variables for Total Payments (WB, GD, Misc.)
        CLEAR: lv_water, lv_gd, lv_misc.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lt_paydoc[] IS NOT INITIAL.
    "-- Sort Payment Document Table by Tag then by Doc.No.
    SORT lt_paydoc BY tag ASCENDING paydoc ASCENDING.

    "-- Set Counter
    lv_counter = 1.

    LOOP AT lt_paydoc ASSIGNING <fs_paydoc> WHERE tag EQ 'WB'.
      "-- Remove Zeros
      SHIFT <fs_paydoc>-paydoc LEFT DELETING LEADING '0'.

      IF lv_counter EQ 1.
        pv_water_or    = <fs_paydoc>-paydoc.
        pv_water_pydt  = <fs_paydoc>-budat.
        pv_water_net   = <fs_paydoc>-netofvat.
        pv_water_vat   = <fs_paydoc>-vat.
        pv_water_total = <fs_paydoc>-amount.
      ELSEIF lv_counter EQ 2.
        pv_water_or2    = <fs_paydoc>-paydoc.
        pv_water_pydt2  = <fs_paydoc>-budat.
        pv_water_net2   = <fs_paydoc>-netofvat.
        pv_water_vat2   = <fs_paydoc>-vat.
        pv_water_total2 = <fs_paydoc>-amount.
      ENDIF.

      "-- Set Counter
      lv_counter = lv_counter + 1.
    ENDLOOP.

    LOOP AT lt_paydoc ASSIGNING <fs_paydoc> WHERE tag NE 'WB'.
      "-- Remove Zeros
      SHIFT <fs_paydoc>-paydoc LEFT DELETING LEADING '0'.

      IF <fs_paydoc>-tag EQ 'MISC'.
        pv_misc_or    = <fs_paydoc>-paydoc.
        pv_misc_pydt  = <fs_paydoc>-budat.
        pv_misc_net   = <fs_paydoc>-netofvat.
        pv_misc_vat   = <fs_paydoc>-vat.
        pv_misc_total = <fs_paydoc>-amount.
      ELSE.
        pv_gd_or      = <fs_paydoc>-paydoc.
        pv_gd_pydt    = <fs_paydoc>-budat.
        pv_gd_net     = <fs_paydoc>-amount.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "previous_payment2

*&---------------------------------------------------------------------*
*&      FORM MOVE ITAB
*&---------------------------------------------------------------------*
FORM moveitab.
  itab_sap2-mru                = itab_sap-mru.
  itab_sap2-seq                = itab_sap-seq.
  itab_sap2-mrid               = itab_sap-mrid.
  itab_sap2-device             = itab_sap-device.
  itab_sap2-matnr              = itab_sap-matnr.
  itab_sap2-mfgsernum          = itab_sap-mfgsernum.
  itab_sap2-size               = itab_sap-size.
  itab_sap2-predec             = itab_sap-predec.
  itab_sap2-mrnumber           = itab_sap-mrnumber.
  itab_sap2-mrnote             = itab_sap-mrnote.
  itab_sap2-name               = itab_sap-name.
  itab_sap2-address            = itab_sap-address.
  itab_sap2-location           = itab_sap-location.
  itab_sap2-statnote           = itab_sap-statnote.
  itab_sap2-instal             = itab_sap-instal.
  itab_sap2-operncode          = itab_sap-operncode.
  itab_sap2-prev_mrdate        = itab_sap-prev_mrdate.
  itab_sap2-billed_prevread    = itab_sap-billed_prevread.
  itab_sap2-actual_prevread    = itab_sap-actual_prevread.
  IF itab_sap-new_connection_tag = 'N'.
    itab_sap2-prevconsump        = '0'.
  ELSE.
    itab_sap2-prevconsump        = itab_sap-prevconsump.
  ENDIF.
  itab_sap2-aveconsump         = itab_sap-aveconsump.
  itab_sap2-ratecateg          = itab_sap-ratecateg.
  itab_sap2-opcode             = itab_sap-opcode.
  itab_sap2-devloc             = itab_sap-devloc.
  itab_sap2-expected           = itab_sap-expected.
  itab_sap2-istablart          = itab_sap-istablart.
  itab_sap2-vkont              = itab_sap-vkont.
  itab_sap2-sanzpers           = itab_sap-sanzpers.
  itab_sap2-bill_class         = itab_sap-bill_class.
  itab_sap2-rate               = itab_sap-rate.
  itab_sap2-newmeter_info      = itab_sap-newmeter_info.
  itab_sap2-initial_rdg        = itab_sap-initial_rdg.
  itab_sap2-removal_dt         = itab_sap-removal_dt.
  itab_sap2-final_rdg          = itab_sap-final_rdg.
  itab_sap2-grp_flag           = itab_sap-grp_flag.
  itab_sap2-stat_flag          = itab_sap-stat_flag.
  itab_sap2-block_cd           = itab_sap-block_cd.
  itab_sap2-disc_tag           = itab_sap-disc_tag.
  itab_sap2-compl_tag          = itab_sap-compl_tag.
  itab_sap2-pua                = itab_sap-pua.

  "-- start - Modified by RANDYSY 12/02/2014 RFC#48
*  itab_sap2-other_charges      = itab_sap-other_charges.
*  itab_sap2-meter_charges      = itab_sap-meter_charges.
*  itab_sap2-re_opening_fee     = itab_sap-re_opening_fee.
*  itab_sap2-gd                 = itab_sap-gd.
*  itab_sap2-installment        = itab_sap-installment.
*  itab_sap2-install_charge     = itab_sap-install_charge.

  itab_sap2-septic_charge        = itab_sap-septic_charge.
  itab_sap2-changesize_charge    = itab_sap-changesize_charge.
  itab_sap2-restoration_charge   = itab_sap-restoration_charge.
  itab_sap2-misc_charge          = itab_sap-misc_charge.
  itab_sap2-meter_charges        = itab_sap-meter_charges.
  itab_sap2-re_opening_fee       = itab_sap-re_opening_fee.
  itab_sap2-gd                   = itab_sap-gd.
  itab_sap2-install_water_charge = itab_sap-install_water_charge.
  itab_sap2-install_sewer_charge = itab_sap-install_sewer_charge.
  itab_sap2-advances             = itab_sap-advances.
  itab_sap2-pn_install_water     = itab_sap-pn_install_water.
  itab_sap2-pn_install_water_ind = itab_sap-pn_install_water_ind.
  itab_sap2-pn_install_sewer     = itab_sap-pn_install_sewer.
  itab_sap2-pn_install_sewer_ind = itab_sap-pn_install_sewer_ind.
  itab_sap2-pn_ar                = itab_sap-pn_ar.
  itab_sap2-pn_ar_ind            = itab_sap-pn_ar_ind.
  itab_sap2-pn_adv               = itab_sap-pn_adv.
  itab_sap2-pn_adv_ind           = itab_sap-pn_adv_ind.
  "-- start - Modified by RANDYSY 12/02/2014 RFC#48

  itab_sap2-invoice            = itab_sap-invoice.
  itab_sap2-water_or           = itab_sap-water_or.
  itab_sap2-water_pydt         = itab_sap-water_pydt.
  itab_sap2-water_net          = itab_sap-water_net.
  itab_sap2-water_vat          = itab_sap-water_vat.
  itab_sap2-water_total        = itab_sap-water_total.
  itab_sap2-water_or2          = itab_sap-water_or2.
  itab_sap2-water_pydt2        = itab_sap-water_pydt2.
  itab_sap2-water_net2         = itab_sap-water_net2.
  itab_sap2-water_vat2         = itab_sap-water_vat2.
  itab_sap2-water_total2       = itab_sap-water_total2.
  itab_sap2-misc_or            = itab_sap-misc_or.
  itab_sap2-misc_pydt          = itab_sap-misc_pydt.
  itab_sap2-misc_net           = itab_sap-misc_net.
  itab_sap2-misc_vat           = itab_sap-misc_vat.
  itab_sap2-misc_total         = itab_sap-misc_total.
  itab_sap2-gd_or              = itab_sap-gd_or.
  itab_sap2-gd_pydt            = itab_sap-gd_pydt.
  itab_sap2-gd_net             = itab_sap-gd_net.
*** BEG ACUCUECO Ticket No. 1157 default value of 1 if null ***
  IF ( itab_sap-sanzper IS INITIAL OR itab_sap-sanzper = '0.00' ) AND ( itab_sap-ratecateg = 'AVERES' OR itab_sap-ratecateg = 'AVESEM').
    itab_sap2-sanzper = '1'.
  ELSE.
    itab_sap2-sanzper            = itab_sap-sanzper.
  ENDIF.
*** END ACUCUECO Ticket No. 1157
  itab_sap2-tin                = itab_sap-tin.
  itab_sap2-bill_dt            = itab_sap-bill_dt.
  itab_sap2-bill_tr            = itab_sap-bill_tr.
  itab_sap2-start_billdt       = itab_sap-start_billdt.
  itab_sap2-end_billdt         = itab_sap-end_billdt.
  itab_sap2-bill_doctype       = itab_sap-bill_doctype.
  itab_sap2-main_trans         = itab_sap-main_trans.
  itab_sap2-in_doctype         = itab_sap-in_doctype.
  itab_sap2-no_users           = itab_sap-no_users.
  itab_sap2-new_connection_tag = itab_sap-new_connection_tag.
  itab_sap2-check_meter        = itab_sap-check_meter.
  itab_sap2-rsg                = itab_sap-rsg.
  itab_sap2-recon_dt           = itab_sap-recon_dt.
  itab_sap2-recon_rdg          = itab_sap-recon_rdg.
  itab_sap2-acct_det           = itab_sap-acct_det.
  itab_sap2-dis_chk            = itab_sap-dis_chk.
  "--Inserted by Ann 2012/08/14
  WRITE itab_sap-hoa_pua TO itab_sap2-hoa_pua DECIMALS 2.
  REPLACE ALL OCCURRENCES OF ',' IN itab_sap2-hoa_pua WITH space.
  CONDENSE itab_sap2-hoa_pua.
  itab_sap2-hoa_or    = itab_sap-hoa_or.
  itab_sap2-hoa_paydt = itab_sap-hoa_paydt.
  WRITE: itab_sap-hoa_netvat TO itab_sap2-hoa_netvat,
         itab_sap-hoa_vat    TO itab_sap2-hoa_vat,
         itab_sap-hoa_gross  TO itab_sap2-hoa_gross.
  REPLACE ALL OCCURRENCES OF ',' IN: itab_sap2-hoa_netvat WITH space,
                                     itab_sap2-hoa_vat WITH space,
                                     itab_sap2-hoa_gross WITH space.
  CONDENSE: itab_sap2-hoa_netvat,
            itab_sap2-hoa_vat,
            itab_sap2-hoa_gross.
  "/-Inserted by Ann 2012/08/14

  "-- Inserted by RANDYSY 09/25/2012
  WRITE: itab_sap-hoa_ind    TO itab_sap2-hoa_ind,
         itab_sap-water_ind  TO itab_sap2-water_ind,
         itab_sap-water_ind2 TO itab_sap2-water_ind2,
         itab_sap-misc_ind   TO itab_sap2-misc_ind,
         itab_sap-gd_ind     TO itab_sap2-gd_ind.

  "-- Inserted by RANDYSY 09/25/2012

*  PERFORM get_prev_ba. "-- TN#31606 Commented by RANDYSY 07/01/2013

  APPEND itab_sap2.
  REFRESH itab_sap.

ENDFORM.                    "MOVE ITAB.

*---------------------------------------------------------------------*
*       FORM getcons                                               *
*---------------------------------------------------------------------*

FORM getcons.
  DATA: x_belnr TYPE erch-belnr,
        opbel   TYPE erchc-opbel,
        osb_tag TYPE erch-origdoc,
        cera    TYPE i,
        amount  TYPE dberchz3-nettobtr,
        belzeile TYPE dberchz3-belzeile,
        billdoc  TYPE dberchz3-belnr.

***** Get the BILLDOC AND ORIGDOC(IF OSB or Normal Billing)
  SELECT SINGLE belnr origdoc INTO (x_belnr, osb_tag)
    FROM erch
    WHERE vkont   = itab_sap-vkont
    AND adatsoll  = mreading-mrsched
    AND stornodat = '00000000'.

  IF sy-subrc = 0.
    IF osb_tag = '03'.
      SELECT SUM( dberchz3~nettobtr )
       INTO (amount )
       FROM dberchz3 INNER JOIN dberchz1
       ON dberchz3~belnr = dberchz1~belnr
       AND dberchz3~belzeile = dberchz1~belzeile
       WHERE dberchz3~belnr = x_belnr
       AND dberchz3~mwskz <> space
       AND dberchz3~preistuf = space
       AND preis LIKE 'CERA%'.

      cera = amount.
      itab_sap-prevconsump = cera.

    ELSE.
      SELECT SINGLE erchc~opbel
          INTO opbel
          FROM erdk INNER JOIN erchc
          ON erdk~opbel = erchc~opbel
          WHERE
                erdk~invoiced = 'X' AND
                erchc~invoiced = 'X' AND
                erdk~intopbel = space AND
                erchc~intopbel = space AND
                erchc~belnr = x_belnr.

*             CERA
      SELECT DISTINCT dberchz3~nettobtr dberchz3~belzeile dberchz3~belnr
      INTO (amount, belzeile, billdoc) FROM dberchz3
      INNER JOIN dberdlb ON dberchz3~belnr = dberdlb~billdoc
      WHERE
      dberdlb~printdoc = opbel
      AND dberchz3~mwskz <> space
      AND preis LIKE 'CERA%'.
        cera = cera + amount.
        CLEAR amount.
      ENDSELECT.

      itab_sap-prevconsump = cera.
    ENDIF.
  ENDIF.
ENDFORM.                    "getcons

*&---------------------------------------------------------------------*
*&      Form  GET_ACCT_DET
*&---------------------------------------------------------------------*
*       Retrieves the Account Determination ID of an Account

*----------------------------------------------------------------------*
FORM get_acct_det .
  SELECT SINGLE kofiz
  FROM ever
  INTO itab_sap-acct_det
  WHERE vkonto = itab_sap-vkont.

  IF sy-subrc <> 0.
    SELECT SINGLE kofiz_sd
    FROM fkkvkp
    INTO itab_sap-acct_det
    WHERE vkont = itab_sap-vkont.
  ENDIF.
ENDFORM.                    " GET_ACCT_DET

*&---------------------------------------------------------------------*
*&      Form  GET_PREV_BA
*&---------------------------------------------------------------------*
*       Gets previous BA from referential table ZPREVBA based from VKONT
*----------------------------------------------------------------------*
*  Gets maximum date where Meter Reading Date > DATAB
*
*----------------------------------------------------------------------*
FORM get_prev_ba USING    pv_can    TYPE vkont_kk
                 CHANGING pv_old_ba TYPE gsber.

  DATA: var_max TYPE sydatum.
  CLEAR var_max.

  SELECT MAX( datab )
    INTO var_max
  FROM   zprevba
  WHERE  vkont EQ pv_can
  AND    datab LT p_mrdate.

  SELECT SINGLE old_gsber
    INTO pv_old_ba
  FROM   zprevba
  WHERE  vkont EQ pv_can
  AND    datab EQ var_max.
ENDFORM.                    " GET_PREV_BA

*&---------------------------------------------------------------------*
*&      Form  GET_DISHONORED_CHECK
*&---------------------------------------------------------------------*
*       Check if the account is a dishonored check
*----------------------------------------------------------------------*
FORM get_dishonored_check .
  DATA: v_vkont TYPE zdishonoredcheck.

  SELECT SINGLE vkont
  INTO v_vkont
  FROM zdishonoredcheck
  WHERE vkont EQ itab_sap-vkont.

  IF sy-subrc EQ 0.
    itab_sap-dis_chk = 'X'.
  ELSE.
    itab_sap-dis_chk = ''.
  ENDIF.
ENDFORM.                    " GET_DISHONORED_CHECK

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPL_FILE  text
*----------------------------------------------------------------------*
FORM select_file_path  USING  pa_path.
  DATA :
    lv_subrc  TYPE sy-subrc,
    lv_path   TYPE string,
    lt_it_tab TYPE string.

  lv_path = gv_path.

  "-- Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Select Destination File'
      initial_folder  = lv_path
    CHANGING
      selected_folder = lt_it_tab.

  "-- Write path on input area
  IF lt_it_tab IS NOT INITIAL.
    CONCATENATE lt_it_tab `\` INTO pa_path.
  ENDIF.
ENDFORM.                    " SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  GET_HOA_DETAILS
*&---------------------------------------------------------------------*
*       Get Home Owners Assoc. (HOA) dues for AMAYI Water Systems
*----------------------------------------------------------------------*
FORM get_hoa_details .
  DATA: lt_payment        TYPE fkk_t_fkkop,
        lt_acctbalance    TYPE TABLE OF bapiaccbalances,
        lv_credit         TYPE bapicurr_d,
        lv_ind            TYPE char1.
  FIELD-SYMBOLS: <fs_pay> TYPE fkkop,
                 <fs_bal> TYPE bapiaccbalances.

  CALL FUNCTION 'ISU_DB_FKKOP_SELECT_GP_VK'
    EXPORTING
      x_sort        = 'X'
      x_vkont       = itab_sap-vkont
      x_gpart       = buspart
    TABLES
      t_fkkop       = lt_payment
    EXCEPTIONS
      not_found     = 1
      system_error  = 2
      not_qualified = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.
  DELETE lt_payment WHERE ( hvorg NE '7009' AND ( tvorg NE '0010' OR tvorg NE '0020' ) ).
  IF lt_payment[] IS NOT INITIAL.
    SORT lt_payment BY opbel DESCENDING.
    "--Get HOA Previous Unpaid Bills
    LOOP AT lt_payment ASSIGNING <fs_pay>.
      itab_sap-hoa_pua = itab_sap-hoa_pua + ( <fs_pay>-betrw - <fs_pay>-augbt ).
      IF lv_ind IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF itab_sap-hoa_or IS INITIAL.
        lv_ind = 'X'.
        itab_sap-hoa_or     = <fs_pay>-opbel.     "HOA Payment document number
        itab_sap-hoa_paydt  = <fs_pay>-augdt.     "HOA Payment date
        itab_sap-hoa_netvat = <fs_pay>-augbt.     "HOA Amount with NET of VAT
        itab_sap-hoa_vat    = 0.                  "HOA VAT amount (Default to zero(0))
        itab_sap-hoa_gross  = <fs_pay>-augbt.     "HOA total amount with VAT
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_HOA_DETAILS
*&---------------------------------------------------------------------*
*&      Form  SET_BIR_INDICATORS
*&---------------------------------------------------------------------*
*       Sets the BIR Requirement Indicators
*----------------------------------------------------------------------*
FORM set_bir_indicators.

  DATA: r_grpflag  TYPE RANGE OF char04,
        ls_grpflat LIKE LINE  OF r_grpflag.

  "-- Macro Definition for Appending Range
  DEFINE mc_append_range.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  "-- Macro Definition for Setting BIR Indicators
  DEFINE mc_sets_indicators.
    if &1 is not initial.
      if itab_sap-acct_det ne `22`.
        if itab_sap-grp_flag not in r_grpflag.
          &2 = `1`.
        elseif itab_sap-grp_flag in r_grpflag.
          &2 = `3`.
        endif.
      elseif itab_sap-acct_det eq `22`.
        if itab_sap-grp_flag not in r_grpflag.
          &2 = `2`.
        elseif itab_sap-grp_flag in r_grpflag.
          &2 = `3`.
        endif.
      endif.
    endif.
  END-OF-DEFINITION.

  "-- Appending Range
  mc_append_range: ls_grpflat r_grpflag `5006`,
                   ls_grpflat r_grpflag `5007`.

  "-- Reinitialize Indicators
  CLEAR: itab_sap-hoa_ind,
         itab_sap-water_ind,
         itab_sap-water_ind2,
         itab_sap-misc_ind,
         itab_sap-gd_ind.

  "-- Sets HOA Indicator
  IF itab_sap-hoa_or IS NOT INITIAL.
    itab_sap-hoa_ind = `4`.
  ENDIF.

  "-- Sets Indicators
  mc_sets_indicators: itab_sap-water_or  itab_sap-water_ind,  "-- Sets Water Indicator
                      itab_sap-water_or2 itab_sap-water_ind2, "-- Sets Water Indicator2
                      itab_sap-misc_or   itab_sap-misc_ind.   "-- Sets Miscellaneous Indicator

  "-- Sets Guarantee Deposit Indicator
  IF itab_sap-gd_or IS NOT INITIAL.
    itab_sap-gd_ind = `4`.
  ENDIF.

  "-- Zero Rated Indicator
  IF itab_sap-water_ind  EQ `3`. itab_sap-water_vat  = `0`. itab_sap-water_total  = itab_sap-water_net.  ENDIF.
  IF itab_sap-water_ind2 EQ `3`. itab_sap-water_vat2 = `0`. itab_sap-water_total2 = itab_sap-water_net2. ENDIF.
  IF itab_sap-misc_ind   EQ `3`. itab_sap-misc_vat   = `0`. itab_sap-misc_total   = itab_sap-misc_net.   ENDIF.
ENDFORM.                    " SET_BIR_INDICATORS


*&---------------------------------------------------------------------*
*&      Form  GET_INSTALLMENT_DUE
*&---------------------------------------------------------------------*
*       Get installment charges
*----------------------------------------------------------------------*
FORM get_installment_due  USING    pv_can           TYPE vkont_kk
                                   pv_start_billdt  TYPE sydatum
                                   pv_end_billdt    TYPE sydatum
                          CHANGING pv_install_water TYPE char11
                                   pv_install_sewer TYPE char11
                                   pv_install_adv   TYPE char11
                                   pv_install_ar    TYPE char11
                                   pv_ind_water     TYPE char5
                                   pv_ind_sewer     TYPE char5
                                   pv_ind_adv       TYPE char5
                                   pv_ind_ar        TYPE char5
                                   pv_subrc         TYPE sysubrc.

  TYPES: BEGIN OF ty_docno,
           docno      TYPE opbel_kk,
           due_date   TYPE faedn_kk,
         END OF ty_docno.

  DATA:  lt_docno       TYPE TABLE OF ty_docno,
         lt_sfkkop      TYPE TABLE OF sfkkop,
         lt_fkkop       TYPE TABLE OF fkkop,
         lt_fkkko       TYPE TABLE OF fkkko,
         ls_rfkn1       TYPE rfkn1,
         lv_main        TYPE hvorg_kk,
         lv_sub         TYPE tvorg_kk,
         lv_install_doc TYPE fkkop-opbel,
         lv_water       TYPE c LENGTH 1,
         lv_sewer       TYPE c LENGTH 1,
         lv_adv         TYPE c LENGTH 1,
         lv_ar          TYPE c LENGTH 1,
         lv_ind_item    TYPE c LENGTH 3,
         lv_ind_max     TYPE c LENGTH 3.

  FIELD-SYMBOLS:
         <fs_docno>   TYPE ty_docno,
         <fs_sfkkop>  TYPE sfkkop,
         <fs_fkkko>   TYPE fkkko.

  CONSTANTS:
         co_dash TYPE c LENGTH 1 VALUE '-'.

  pv_install_water = 0.
  pv_install_sewer = 0.
  pv_install_adv   = 0.
  pv_install_ar    = 0.
  pv_subrc = 0.

  "-- Get Document Numbers and Due Dates of all Installment Plans
  SELECT opbel faedn
    INTO TABLE lt_docno
  FROM   dfkkopw
  WHERE  vkont EQ pv_can
  AND    faedn LE pv_end_billdt
  AND    augrd EQ space.

  IF sy-subrc EQ 0.
    "-- Get Latest Document Number
    SORT lt_docno BY due_date DESCENDING.

    READ TABLE lt_docno ASSIGNING <fs_docno> INDEX 1.
    IF sy-subrc EQ 0.

      REFRESH: lt_sfkkop.

      "-- Get Account Balances
      CALL FUNCTION 'FKK_S_INSTPLAN_PROVIDE'
        EXPORTING
          i_opbel        = <fs_docno>-docno
          i_for_update   = space
        IMPORTING
          e_rfkn1        = ls_rfkn1
        TABLES
          raten_fkkop    = lt_sfkkop
        EXCEPTIONS
          already_locked = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        "-- Added by GDIMALIWAT 12/04/2014 RFC#48
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_rfkn1-rpnum
          IMPORTING
            output = lv_install_doc.

        lv_ind_max = ls_rfkn1-ninst.
        SHIFT lv_ind_max LEFT DELETING LEADING '0'.

        CALL FUNCTION 'FKK_DB_INSTPLAN_ORIG_SELECT'
          EXPORTING
            i_opbel       = lv_install_doc
            i_select_head = 'X'
          TABLES
            t_fkkop       = lt_fkkop
            t_fkkko       = lt_fkkko.

        IF lt_fkkko IS NOT INITIAL.
          "-- Classify the original document if Water, Sewer, Advance or AR
          LOOP AT lt_fkkko ASSIGNING <fs_fkkko>.
            SELECT SINGLE hvorg tvorg
              INTO (lv_main, lv_sub)
              FROM dfkkop
             WHERE opbel EQ <fs_fkkko>-opbel.

            IF lv_main EQ '9000' AND ( lv_sub EQ '0250' OR lv_sub EQ '0020' ).
              lv_water = 'X'.
            ELSEIF lv_main EQ '9000' AND lv_sub EQ '0450'.
              lv_sewer = 'X'.
            ELSEIF lv_main EQ '7008'.
              lv_adv = 'X'.
            ELSEIF lv_main NE '9000' AND lv_main NE '7008'.
              lv_ar  = 'X'.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_fkkko>.

          "-- If results are mixed, consider as AR
          IF lv_water EQ 'X' AND lv_sewer EQ 'X'.
            lv_ar = 'X'.
          ELSEIF lv_water EQ 'X' AND lv_adv EQ 'X'.
            lv_ar = 'X'.
          ELSEIF lv_sewer EQ 'X' AND lv_adv EQ 'X'.
            lv_ar = 'X'.
          ENDIF.
        ENDIF.

        SORT lt_sfkkop BY faedn ASCENDING.
        "-- End -- Added by GDIMALIWAT 12/04/2014 RFC#48

        LOOP AT lt_sfkkop ASSIGNING <fs_sfkkop> WHERE faedn GE pv_start_billdt
                                                AND   faedn LE pv_end_billdt.
          "-- Added by GDIMALIWAT 12/04/2014 RFC#48
          IF lv_water EQ 'X' AND lv_sewer EQ '' AND lv_adv EQ '' AND lv_ar EQ ''.
            pv_install_water = pv_install_water + <fs_sfkkop>-betrw.
          ELSEIF lv_water EQ '' AND lv_sewer EQ 'X' AND lv_adv EQ '' AND lv_ar EQ ''.
            pv_install_sewer = pv_install_sewer + <fs_sfkkop>-betrw.
          ELSEIF lv_water EQ '' AND lv_sewer EQ '' AND lv_adv EQ 'X' AND lv_ar EQ ''.
            pv_install_adv = pv_install_adv + <fs_sfkkop>-betrw.
          ELSEIF lv_water EQ '' AND lv_sewer EQ '' AND lv_adv EQ '' AND lv_ar EQ 'X'.
            pv_install_ar = pv_install_ar + <fs_sfkkop>-betrw.
          ELSE.
            pv_install_ar = pv_install_ar + <fs_sfkkop>-betrw.
          ENDIF.
          lv_ind_item = sy-tabix.
          "-- End -- Added by GDIMALIWAT 12/04/2014 RFC#48
        ENDLOOP.
        "-- Set indicator
        IF lv_ind_item IS NOT INITIAL.
          IF lv_water EQ 'X' AND lv_sewer EQ '' AND lv_adv EQ '' AND lv_ar EQ ''.
            CONCATENATE lv_ind_item lv_ind_max INTO pv_ind_water SEPARATED BY co_dash.
          ELSEIF lv_water EQ '' AND lv_sewer EQ 'X' AND lv_adv EQ '' AND lv_ar EQ ''.
            CONCATENATE lv_ind_item lv_ind_max INTO pv_ind_sewer SEPARATED BY co_dash.
          ELSEIF lv_water EQ '' AND lv_sewer EQ '' AND lv_adv EQ 'X' AND lv_ar EQ ''.
            CONCATENATE lv_ind_item lv_ind_max INTO pv_ind_adv SEPARATED BY co_dash.
          ELSEIF lv_water EQ '' AND lv_sewer EQ '' AND lv_adv EQ '' AND lv_ar EQ 'X'.
            CONCATENATE lv_ind_item lv_ind_max INTO pv_ind_ar SEPARATED BY co_dash.
          ELSE.
            CONCATENATE lv_ind_item lv_ind_max INTO pv_ind_ar SEPARATED BY co_dash.
          ENDIF.
        ENDIF.

        IF sy-subrc NE 0.
          pv_subrc = 4.
        ENDIF.
      ELSE.
        pv_subrc = 4.
      ENDIF.
    ELSE.
      pv_subrc = 4.
    ENDIF.
  ELSE.
    pv_subrc = 4.
  ENDIF.
ENDFORM.                    " GET_INSTALLMENT_DUE
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEPTION_LIST
*&---------------------------------------------------------------------*
*       Move error in exception list file and build list
*----------------------------------------------------------------------*
*      <-->PT_EXCEPTION     Exception list table
*      --->PV_MRU           Meter Reading Unit
*      --->PV_MRID          Meter Reading No.
*      --->PV_MRSCHED       Meter Reading Schedule
*      --->PV_CAN           Contract Account No.
*      --->PV_INSTAL        Installation No.
*      --->PV_MSG           Error Message
*----------------------------------------------------------------------*
FORM build_exception_list TABLES pt_exception TYPE ty_exception_t
                           USING pv_mru       TYPE ableinh
                                 pv_mrid      TYPE ablbelnr
                                 pv_mrsched   TYPE adatsoll
                                 pv_can       TYPE vkont_kk
                                 pv_instal    TYPE anlage
                                 pv_msg       TYPE char100.

  DATA: ls_osbexcep TYPE ztisu_osbexcep,
        lv_portion  TYPE te422-portion.

  "--Retrieve CAN if blank is passed
  IF pv_can IS INITIAL.
    SELECT SINGLE vkonto
    INTO pv_can
    FROM ever
    WHERE anlage EQ pv_instal.
  ENDIF.

  "--Append error log to exception table
  APPEND INITIAL LINE TO pt_exception ASSIGNING <fs_exc>.
  <fs_exc>-mru     = pv_mru.
  <fs_exc>-mrid    = pv_mrid.
  <fs_exc>-mrsched = pv_mrsched.
  <fs_exc>-can     = pv_can.
  <fs_exc>-instal  = pv_instal.
  <fs_exc>-msg     = pv_msg.

  "--Prepare table data for ZTISU_OSBEXCEP
  CLEAR: ls_osbexcep, lv_portion.

  "-- Get Business Area
  IF pv_can IS NOT INITIAL.
    SELECT SINGLE gsber
    INTO ls_osbexcep-business_area
    FROM fkkvkp
    WHERE vkont EQ pv_can.
  ENDIF.

  "-- Get Portion
  IF pv_mru IS NOT INITIAL.
    SELECT SINGLE portion
    INTO lv_portion
    FROM te422
    WHERE termschl EQ pv_mru.
  ENDIF.

  ls_osbexcep-portion           = lv_portion.
  ls_osbexcep-bill_group        = p_mrdate+6(2).
  ls_osbexcep-billing_cycle     = pv_mrsched+4(2).
  ls_osbexcep-mru               = pv_mru.
  ls_osbexcep-mrid              = pv_mrid.
  ls_osbexcep-mr_schedule       = pv_mrsched.

  "-- Get Exception Count
  SELECT SINGLE MAX( exception_count )
  INTO ls_osbexcep-exception_count
  FROM ztisu_osbexcep
  WHERE billing_cycle EQ pv_mrsched+4(2)
    AND mru           EQ pv_mru
    AND mrid          EQ pv_mrid
    AND mr_schedule   EQ pv_mrsched.
  IF sy-subrc EQ 0.
    ADD 1 TO ls_osbexcep-exception_count.
  ELSE.
    ls_osbexcep-exception_count = 1.
  ENDIF.

  ls_osbexcep-contract_account  = pv_can.
  ls_osbexcep-installation      = pv_instal.
  ls_osbexcep-exception_message = pv_msg.
  ls_osbexcep-tagging           = ''.

  "--Insert to ZTISU_OSBEXCEP table
  INSERT ztisu_osbexcep
  FROM ls_osbexcep.

ENDFORM.                    " BUILD_EXCEPTION_LIST

*&---------------------------------------------------------------------*
*&      Form  CHECK_DEVICE
*&---------------------------------------------------------------------*
*       Check Device
*----------------------------------------------------------------------*
FORM check_device USING    pv_instal       TYPE anlage
                  CHANGING pv_serial_count TYPE i.

  TYPES: BEGIN OF ty_equip,
          equip  TYPE equnr,
          serial TYPE gernr,
          devloc TYPE devloc,
          bis    TYPE biszeitsch,
         END OF ty_equip.

  DATA:  lt_equip TYPE TABLE OF ty_equip,
         lv_dev   TYPE logiknr.

  FIELD-SYMBOLS:
         <fs_equip> TYPE ty_equip.

  "-- Reiniatialize Changing Parameter
  CLEAR: pv_serial_count.

  "-- Get Logical Device Number
  SELECT SINGLE logiknr
    INTO lv_dev
  FROM   eastl
  WHERE  anlage EQ pv_instal.

  IF sy-subrc EQ 0.
    "-- Reinitialize Equipment Table
    REFRESH: lt_equip.

    "-- Get Equipment Numbers
    SELECT a~equnr b~sernr a~devloc a~bis
      INTO TABLE lt_equip
    FROM   egerh AS a INNER JOIN equi AS b ON a~equnr EQ b~equnr
    WHERE  a~logiknr EQ lv_dev.

    IF sy-subrc EQ 0.
      "-- Get Latest Device Location
      SORT lt_equip BY bis DESCENDING.

      READ TABLE lt_equip ASSIGNING <fs_equip> INDEX 1.
      IF sy-subrc EQ 0.

        "-- Get Number of Fetched Device/Serial Numbers
        CALL FUNCTION 'ISU_DB_EGER_SELECT_DEVLOC'
          EXPORTING
            x_devloc      = <fs_equip>-devloc
          IMPORTING
            y_count       = pv_serial_count
          EXCEPTIONS
            not_found     = 1
            not_qualified = 2
            OTHERS        = 3.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_DEVICE

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEPTIONS
*&---------------------------------------------------------------------*
*       Download Exceptions
*----------------------------------------------------------------------*
FORM download_exceptions.
  DATA: lv_path        TYPE string,
        lv_except_path TYPE string.

  CONSTANTS:
        co_excep       TYPE string VALUE 'Exception'.

  lv_path = pa_exc. "pa_path.

  IF gt_exception[] IS NOT INITIAL.
    SORT gt_exception BY mru.

*    "-- Check if Exception folder exists, if no create folder
*    PERFORM check_folder USING    lv_path
*                                  co_excep
*                         CHANGING lv_except_path.

    lv_except_path = lv_path.

    gv_diff = strlen( lv_except_path ).
    SUBTRACT 1 FROM gv_diff.

    LOOP AT gt_exception ASSIGNING <fs_exc>.
      AT NEW mru.
        IF lv_except_path+gv_diff(1) = '\'.
          CONCATENATE lv_except_path <fs_exc>-mru text-000 sy-datum sy-uzeit text-001 INTO gv_fpath.
        ELSE.
          SUBTRACT 3 FROM gv_diff.
          IF pa_path+gv_diff(4) = '.txt' OR pa_path+gv_diff(4) = '.TXT'.
            gv_fpath = lv_except_path.
          ELSE.
            CONCATENATE lv_except_path '\' <fs_exc>-mru  text-000 sy-datum sy-uzeit text-001 INTO gv_fpath.
          ENDIF.
        ENDIF.

        WRITE gv_fpath TO gv_excep.
        OPEN DATASET gv_excep FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE gv_msg.

        IF sy-subrc EQ 0.
          gv_fpath = text-003.
          TRANSFER gv_fpath TO gv_excep.
        ENDIF.
      ENDAT.

      IF sy-subrc EQ 0.
        WRITE:/ gv_excep, gv_msg.

        CLEAR gv_fpath.
        CONCATENATE <fs_exc>-mru <fs_exc>-mrid <fs_exc>-mrsched <fs_exc>-can <fs_exc>-instal <fs_exc>-msg
               INTO gv_fpath SEPARATED BY '|' RESPECTING BLANKS.
        TRANSFER gv_fpath TO gv_excep.
      ENDIF.

      AT END OF mru.
        CLOSE DATASET gv_excep.
      ENDAT.
    ENDLOOP.
  ELSE.
    WRITE:/ text-m01.
  ENDIF.
ENDFORM.                    " DOWNLOAD_EXCEPTIONS

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download Data
*----------------------------------------------------------------------*
FORM download_data .
  TYPES:  BEGIN OF ty_mru_exceptions,
            mru TYPE ableinh_ts,
          END OF ty_mru_exceptions.

  DATA: flag            TYPE n VALUE 1,
        new_mru         TYPE ableinh_ts,
        new_mrid        TYPE ablbelnr,
        lv_ext          TYPE char255,
        lv_namex        TYPE char255,
        lv_name         TYPE char255,
        lv_path         TYPE string,
        lv_invalid_path TYPE string.

  DATA: lt_mru_exceptions TYPE TABLE OF ty_mru_exceptions.

  FIELD-SYMBOLS:  <fs_mru_exceptions> TYPE ty_mru_exceptions.
  CONSTANTS:
        co_invalid TYPE string VALUE 'Invalid'.

  CLEAR gv_fpath.

  gv_fpath = pa_path.

  CALL FUNCTION 'CH_SPLIT_FILENAME'
    EXPORTING
      complete_filename = gv_fpath
    IMPORTING
      extension         = lv_ext
      name              = lv_name
      name_with_ext     = lv_namex
      path              = lv_path
    EXCEPTIONS
      invalid_drive     = 1
      invalid_path      = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE e052(zisu).
  ENDIF.
  "--Process based on retrieved file name
  IF lv_ext IS INITIAL. "Only path is entered, no filename extension
    IF lv_namex IS INITIAL.
      gv_fpath = lv_path.
    ELSE.
      CONCATENATE gv_fpath '\' INTO gv_fpath.
    ENDIF.
  ELSE.
    IF lv_ext = '.txt' OR lv_ext = '.TXT'.
      "--If filename with extension is indicated, get only the path
      gv_fpath = lv_path.
    ENDIF.
  ENDIF.
  "/-TN#23986 Inserted by Ann 02/05/2013

  "Added by GDIMALIWAT
  LOOP AT itab_out ASSIGNING <fs_out>.
    READ TABLE gt_exception ASSIGNING <fs_exc> WITH KEY mrid = <fs_out>-mrid.
    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO lt_mru_exceptions ASSIGNING <fs_mru_exceptions>.
      <fs_mru_exceptions> = <fs_out>-mru.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_out>.
  UNASSIGN <fs_mru_exceptions>.
  "End of modifs by GDIMALIWAT

  SORT itab_out BY mru.

  LOOP AT itab_out ASSIGNING <fs_out>.

    new_mru  = <fs_out>-mru.
    new_mrid = <fs_out>-mrid.

    AT NEW mru.
      "Added by GDIMALIWAT
*      READ TABLE gt_exception ASSIGNING <fs_exc> WITH KEY mrid = new_mrid.
      READ TABLE lt_mru_exceptions ASSIGNING <fs_mru_exceptions> WITH KEY mru = new_mru.
      "End of modifs by GDIMALIWAT
      IF sy-subrc EQ 0.
*        "-- Check if Invalid folder exists, if no create folder
*        PERFORM check_folder USING    gv_fpath
*                                      co_invalid
*                             CHANGING lv_invalid_path.
*
*        CONCATENATE lv_invalid_path new_mru '_Invalid.txt' INTO spl_file2.
        CONCATENATE pa_exc new_mru '_Invalid.txt' INTO spl_file2.
      ELSE.
        CONCATENATE gv_fpath new_mru '.txt' INTO spl_file2.
      ENDIF.

      WRITE spl_file2 TO rpt.
      OPEN DATASET rpt FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    ENDAT.

    IF sy-subrc EQ 0.
      TRANSFER <fs_out>-details TO rpt.
    ENDIF.

    AT END OF mru.
      CLOSE DATASET rpt.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " DOWNLOAD_DATA