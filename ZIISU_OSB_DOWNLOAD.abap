*****************************************************************************************************
* PROGRAM ID      : ZPRG_DOWNLOAD_OSB
* TITLE           : Meter Reading Download for OSB
* CREATE DATE     : Mar 09, 2011
* AUTHOR          : MPBACANI
*----------------------------------------------------------------------------------------------------
* DESCRIPTION     :
*----------------------------------------------------------------------------------------------------
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------------
*  DATE            | NAME      |  DESCRIPTION                               | Reference
*----------------------------------------------------------------------------------------------------
* Apr 25, 2011 | MPBACANI   | Format of negative sign of PUA,Meter Info    | Konch, Ma'am Celia
* Apr 25, 2011 | MPBACANI   | PUA back to orig format                      | Konch, Vince
* Oct 20, 2011 | GDIMALIWAT | Additonal column ACCT_DET                    | Salve Sibal
* Dec 08, 2011 | ACUCUECO   | Additonal column OLD_BA                      | Salve Sibal
* Dec 08, 2011 | ACUCUECO   | Additonal column OLD_BA                      | Salve Sibal
* Feb 10, 2012 | GDIMALIWAT | Added MOVEITAB within the VKONT validation   | Salve Sibal
* Feb 15, 2012 | ACUCUECO   | Change in table sources, Set default value   | Katherine Flores
*              |            | of No. of house hold to 1 if null            | Ticket No. 11062, 11157
* Mar 15, 2012 | BMABINI    | Inserting of PUA to ZOSB_PUA table           | TN# 10513
* Mar 19, 2012 | RANDYSY    | Deletion of More than a year data in         |
*              |            | ZOSB_PUA table                               |
* Apr 18, 2012 | GDIMALIWAT | Added tagging of dihonored checks            | TN# 12903
* May 05, 2012 | GDIMALIWAT | Uncommented dishonored Checks                | TN# 12903
* Jul 23, 2012 | RANDYSY    | Change Source of PUA (Use Custom FM)         | TN# 13816
* Aug 08, 2012 | RANDYSY    | Revert computation of PUA                    | TN# 19142
* Aug 10, 2012 | ANNACLO    | Additioanal columns for HOA (AMAYI)          | TN# 18283
* Aug 31, 2012 | RANDYSY    | Exception of HOA PUA for OSB PUA Computation | TN# 18283
* Sep 24, 2012 | RANDYSY    | Added Indicators for BIR Requirements        | TN# 20140
* Nov 27, 2012 | ANNACLO    | Replaced hardcoded IP w/ domain/host name    | TN# 23184
* Jan 15, 2013 | ANNACLO    | -Create exception list for incomplete data   | TN# 23986
*              | ANNACLO    | -Removal of deadcodes within the program     |
* Dec 02, 2014 | RANDYSY    | IRR Implementation                           | RFC# 48
* Dec 03, 2014 | GDIMALIWAT | Continuation of IRR Implementation           | RFC#48
*****************************************************************************************************

***ZDLREADING_VAR_LOCAL_OSB.
TABLES: eabl,
        but020,
        adrc,
        eablg,
        easte,
        ever,
        eein,
        adrstrtmru,
        stxh,
        etdz,
        easti.

"--Inserted by Ann 01/15/2013
TYPES: BEGIN OF ty_exception_s,
         mru          TYPE ableinh,
         mrid         TYPE ablbelnr,
         mrsched      TYPE adatsoll,
         can          TYPE vkont_kk,
         instal       TYPE anlage,
         msg          TYPE char100,
       END OF ty_exception_s,
       ty_exception_t TYPE STANDARD TABLE OF ty_exception_s.

DATA: gt_exception    TYPE ty_exception_t,
      gv_diff         TYPE i,
      gv_date         TYPE char10,
      gv_msg          TYPE char100,
      gv_error        TYPE char1,
      gv_fpath        TYPE string,
      gv_excep        TYPE c LENGTH 2000,
      gv_rate_cd      TYPE txw_pbasatz, "Inserted by Ann 01/18/2013
      gv_mom_acct     TYPE vkont_kk,    "Inserted by Ann 02/06/2013
      gv_kid_acct     TYPE vkont_kk.    "Inserted by Ann 02/06/2013

FIELD-SYMBOLS <fs_exc> TYPE ty_exception_s.
"/-Inserted by Ann 01/15/2013

DATA:
      "path LIKE rlgrap-filename VALUE '\\172.18.1.240\mrs\download\',             "Comment out by Ann 2012/11/27
       gv_path TYPE rlgrap-filename VALUE '\\saprep01.mayniladsap.com\mrs\download\', "Inserted by Ann 2012/11/27
       gv_exc  TYPE rlgrap-filename VALUE '\\saprep01.mayniladsap.com\mrs\download\Exception\', "Inserted by Ann 2012/11/27
*      PATH LIKE RLGRAP-FILENAME VALUE '\\172.18.1.56\dev\Glenn\osb\Y',
*      PATH LIKE RLGRAP-FILENAME VALUE 'G:\Download\',
       gv_ftype TYPE c LENGTH 4 VALUE '.txt',
       gv_ptype TYPE c LENGTH 4 VALUE '.IPQ'.

DATA: BEGIN OF itab OCCURS 0,
        mru      TYPE eablg-ableinh,
        install  TYPE eablg-anlage,
        idnum    TYPE eabl-ablbelnr,
        eqpt     TYPE eabl-equnr,
        device   TYPE eabl-gernr,
        devloc   TYPE egerh-devloc,
        logicdev TYPE egerh-logiknr,
        ablhinw  TYPE eabl-ablhinw,
        abrdats  TYPE eablg-abrdats,
        predecimalplaces TYPE eabl-stanzvor,
        adat     TYPE eabl-adat,
      END OF itab.

DATA: BEGIN OF meter OCCURS 0,
        eqpt     TYPE egerh-equnr,
        devloc   TYPE egerh-devloc,
        logicdev TYPE egerh-logiknr,
      END OF meter.

DATA: BEGIN OF itab2 OCCURS 0,
        mru TYPE eablg-ableinh,
      END OF itab2.

DATA: BEGIN OF it_mru OCCURS 0,
        mru      TYPE te418-termschl,
        adatsoll TYPE te418-adatsoll,
      END OF it_mru.


*** FOR TRANSFERRING TO TEXT FILE ****
DATA: BEGIN OF itab_sap OCCURS 0,
        mru TYPE eablg-ableinh,
        seq TYPE itob-eqfnr,
        mrid TYPE bapieabl-mridnumber,
        device TYPE bapieabl-device,
        matnr TYPE equi-matnr,
        mfgsernum TYPE itob-serge,
        size TYPE itob-groes,
        predec TYPE bapieabl-predecimalplaces,
        mrnumber TYPE bapieabl-meterreader,
        mrnote TYPE bapieabl-meterreadingnote,
        name TYPE but000-name_first,
        address TYPE c LENGTH 70,
        location TYPE egpltx-stortzus,
        statnote TYPE te582t-note_text,
        instal TYPE bapieablg-installation,
        operncode TYPE easti-opcode,
        prev_mrdate TYPE bapieabl-actualmrdate,
        billed_prevread TYPE c LENGTH 17,
        actual_prevread TYPE c LENGTH 17,
        prevconsump     TYPE c LENGTH 17,
        aveconsump      TYPE c LENGTH 17,
        ratecateg TYPE eanlh-tariftyp,
        opcode    TYPE easti-opcode,
        devloc    TYPE egerh-devloc,
        expected  TYPE c LENGTH 17,
        istablart TYPE bapieabl-actualcustomermrtype,
        vkont     TYPE ever-vkonto,
        sanzpers  TYPE c LENGTH 6,
        bill_class TYPE eanlh-tariftyp,
        rate       TYPE ertfnd-tarifnr,
        newmeter_info TYPE bapieabl-device,
        initial_rdg   TYPE c LENGTH 9,
        removal_dt TYPE bapieabl-actualmrdate,
        final_rdg  TYPE c LENGTH 9,
        grp_flag   TYPE c LENGTH 4,
        stat_flag  TYPE c LENGTH 2,
        block_cd   TYPE c LENGTH 2,
        disc_tag   TYPE c LENGTH 1,
        compl_tag  TYPE c LENGTH 4,
        pua        TYPE c LENGTH 11,

"-- start - Modified by RANDYSY 12/02/2014 RFC#48
*        other_charges(11),
*        meter_charges(11),
*        re_opening_fee(11),
*        gd(11),
*        installment(11),
*        install_charge(11),

        septic_charge         TYPE c LENGTH 11,
        changesize_charge     TYPE c LENGTH 11,
        restoration_charge    TYPE c LENGTH 11,
        misc_charge           TYPE c LENGTH 11,
        meter_charges         TYPE c LENGTH 11,
        re_opening_fee        TYPE c LENGTH 11,
        gd                    TYPE c LENGTH 11,
        install_water_charge  TYPE c LENGTH 11,
        install_sewer_charge  TYPE c LENGTH 11,
        advances              TYPE c LENGTH 11,
        pn_install_water      TYPE c LENGTH 11,
        pn_install_water_ind  TYPE c LENGTH  5,
        pn_install_sewer      TYPE c LENGTH 11,
        pn_install_sewer_ind  TYPE c LENGTH  5,
        pn_ar                 TYPE c LENGTH 11,
        pn_ar_ind             TYPE c LENGTH  5,
        pn_adv                TYPE c LENGTH 11,
        pn_adv_ind            TYPE c LENGTH  5,

"-- end - Modified by RANDYSY 12/02/2014 RFC#48

        invoice TYPE c LENGTH 12,
        water_or TYPE c LENGTH 12,
        water_pydt TYPE dfkkop-augdt,
        water_net TYPE c LENGTH 11,
        water_vat TYPE c LENGTH 11,
        water_total TYPE c LENGTH 11,
        water_or2   TYPE c LENGTH 12,
        water_pydt2 TYPE dfkkop-augdt,
        water_net2  TYPE c LENGTH 11,
        water_vat2  TYPE c LENGTH 11,
        water_total2 TYPE c LENGTH 11,
        misc_or TYPE c LENGTH 12,
        misc_pydt TYPE dfkkop-augdt,
        misc_net TYPE c LENGTH 11,
        misc_vat TYPE c LENGTH 11,
        misc_total TYPE c LENGTH 11,
        gd_or TYPE c LENGTH 12,
        gd_pydt TYPE dfkkop-augdt,
        gd_net  TYPE c LENGTH 11,
        sanzper TYPE c LENGTH 16,
        tin     TYPE c LENGTH 15,
        bill_dt TYPE eablg-abrdats,
        bill_tr TYPE c LENGTH 2,
        start_billdt TYPE erch-begabrpe,
        end_billdt TYPE erch-begabrpe,
        bill_doctype TYPE c LENGTH 2,
        main_trans   TYPE c LENGTH 4,
        in_doctype   TYPE c LENGTH 2,
        no_users     TYPE c LENGTH 10,
        new_connection_tag  TYPE c LENGTH 1,
        check_meter  TYPE c LENGTH 12,
        rsg          TYPE c LENGTH 8,
        recon_dt     TYPE c LENGTH 8,
        recon_rdg    TYPE c LENGTH 9,
        acct_det     TYPE c LENGTH 2,
        dis_chk      TYPE c LENGTH 1,
        "--Inserted by Ann 2012/08/14
        hoa_pua    TYPE betrw_kk,
        hoa_or     TYPE opbel_kk,
        hoa_paydt  TYPE augdt_kk,
        hoa_netvat TYPE augbt_kk,
        hoa_vat    TYPE augbt_kk,
        hoa_gross  TYPE betrw_kk,
        "/-Inserted by Ann 2012/08/14
        "/-Inserted by RANDYSY 2012/09/24
        hoa_ind    TYPE zty_birindicator,
        water_ind  TYPE zty_birindicator,
        water_ind2 TYPE zty_birindicator,
        misc_ind   TYPE zty_birindicator,
        gd_ind     TYPE zty_birindicator,
        "/-Inserted by RANDYSY 2012/09/24
        "/-Inserted by RANDYSY 2013/03/14
        prev_mr_erdat TYPE erdat,
        "/-Inserted by RANDYSY 2013/03/14
  END OF itab_sap.

DATA: BEGIN OF itab_sap2 OCCURS 0,
        mru TYPE eablg-ableinh,
        seq TYPE itob-eqfnr,
        mrid TYPE bapieabl-mridnumber,
        device TYPE bapieabl-device,
        matnr TYPE equi-matnr,
        mfgsernum TYPE itob-serge,
        size TYPE itob-groes,
        predec TYPE bapieabl-predecimalplaces,
        mrnumber TYPE bapieabl-meterreader,
        mrnote TYPE bapieabl-meterreadingnote,
        name TYPE but000-name_first,
        address TYPE c LENGTH 70,
        location TYPE egpltx-stortzus,
        statnote TYPE te582t-note_text,
        instal TYPE bapieablg-installation,
        operncode TYPE easti-opcode,
        prev_mrdate TYPE bapieabl-actualmrdate,
        billed_prevread TYPE c LENGTH 17,
        actual_prevread TYPE c LENGTH 17,
        prevconsump     TYPE c LENGTH 17,
        aveconsump      TYPE c LENGTH 17,
        ratecateg TYPE eanlh-tariftyp,
        opcode TYPE easti-opcode,
        devloc TYPE egerh-devloc,
        expected  TYPE c LENGTH 17,
        istablart TYPE bapieabl-actualcustomermrtype,
        vkont TYPE ever-vkonto,
        sanzpers  TYPE c LENGTH 6,
        bill_class TYPE eanlh-tariftyp,
        rate TYPE ertfnd-tarifnr,
        newmeter_info TYPE bapieabl-device,
        initial_rdg TYPE c LENGTH 9,
        removal_dt TYPE bapieabl-actualmrdate,
        final_rdg TYPE c LENGTH 9,
        grp_flag  TYPE c LENGTH 4,
        stat_flag TYPE c LENGTH 2,
        block_cd  TYPE c LENGTH 2,
        disc_tag  TYPE c LENGTH 1,
        compl_tag TYPE c LENGTH 4,
        pua       TYPE c LENGTH 11,

"-- start - Modified by RANDYSY 12/02/2014 RFC#48
*        other_charges(11),
*        meter_charges(11),
*        re_opening_fee(11),
*        gd(11),
*        installment(11),
*        install_charge(11),

        septic_charge         TYPE c LENGTH 11,
        changesize_charge     TYPE c LENGTH 11,
        restoration_charge    TYPE c LENGTH 11,
        misc_charge           TYPE c LENGTH 11,
        meter_charges         TYPE c LENGTH 11,
        re_opening_fee        TYPE c LENGTH 11,
        gd                    TYPE c LENGTH 11,
        install_water_charge  TYPE c LENGTH 11,
        install_sewer_charge  TYPE c LENGTH 11,
        advances              TYPE c LENGTH 11,
        pn_install_water      TYPE c LENGTH 11,
        pn_install_water_ind  TYPE c LENGTH  5,
        pn_install_sewer      TYPE c LENGTH 11,
        pn_install_sewer_ind  TYPE c LENGTH  5,
        pn_ar                 TYPE c LENGTH 11,
        pn_ar_ind             TYPE c LENGTH  5,
        pn_adv                TYPE c LENGTH 11,
        pn_adv_ind            TYPE c LENGTH  5,

"-- end - Modified by RANDYSY 12/02/2014 RFC#48

        invoice TYPE c LENGTH 12,
        water_or TYPE c LENGTH 12,
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
        gd_net TYPE c LENGTH 11,
        sanzper TYPE c LENGTH 16,
        tin TYPE c LENGTH 15,
        bill_dt TYPE eablg-abrdats,
        bill_tr TYPE c LENGTH 2,
        start_billdt TYPE erch-begabrpe,
        end_billdt TYPE erch-begabrpe,
        bill_doctype TYPE c LENGTH 2,
        main_trans TYPE c LENGTH 4,
        in_doctype TYPE c LENGTH 2,
        no_users TYPE c LENGTH 10,
        new_connection_tag TYPE c LENGTH 1,
        check_meter TYPE c LENGTH 12,
        rsg TYPE c LENGTH 8,
        recon_dt TYPE c LENGTH 8,
        recon_rdg TYPE c LENGTH 9,
        acct_det TYPE c LENGTH 2,
        gsber TYPE c LENGTH 4, " To capture previous BA added by ACUCUECO
        dis_chk TYPE c LENGTH 1,
        "--Inserted by Ann 2012/08/14
        hoa_pua    TYPE char18,       "HOA Previously unpaid bills
        hoa_or     TYPE opbel_kk,     "HOA OR
        hoa_paydt  TYPE char8,        "HOA Date Paid
        hoa_netvat TYPE char18,       "HOA Net of VAT
        hoa_vat    TYPE char18,       "HOA VAT
        hoa_gross  TYPE char18,       "HOA Gross amount
        "/-Inserted by Ann 2012/08/14
        "/-Inserted by RANDYSY 2012/09/24
        hoa_ind    TYPE zty_birindicator,
        water_ind  TYPE zty_birindicator,
        water_ind2 TYPE zty_birindicator,
        misc_ind   TYPE zty_birindicator,
        gd_ind     TYPE zty_birindicator,
        "/-Inserted by RANDYSY 2012/09/24
  END OF itab_sap2.


DATA: BEGIN OF itab_cnt OCCURS 0,
         mru  TYPE eablg-ableinh,
         count TYPE i,
      END OF itab_cnt.

DATA: inactive LIKE itab_sap OCCURS 0 WITH HEADER LINE.
*      ITAB_SCHED LIKE BAPIEABL OCCURS 0 WITH HEADER LINE,
*      ITAB_INSTAL LIKE BAPIEABLG OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab_mr OCCURS 0,
* Change field length from 2000 to 2005 to accomodate '|' and GSBER
        "mrdata_sap(2005),      "Comment out by Ann 08/16/2012
        mrdata_sap TYPE string, "Inserted by Ann 08/16/2012
      END OF itab_mr.

*** FOR RETRIEVING ALL READINGS OF EQPT TO GET PREV READING & CONSUMP **
DATA : BEGIN OF mreading OCCURS 0,
         mrding TYPE eabl-v_zwstand,     "meter reading
         reading TYPE eabl-v_zwstndab,   "billed meter reading
         mrsched TYPE eabl-adatsoll,
         mrdate TYPE eabl-adattats,
         mrdate2 TYPE eabl-adat,
         mrnote TYPE eabl-ablhinw,
         mrtime TYPE eabl-atim,
         newtag TYPE c LENGTH 1,
         x_stanzvor TYPE eabl-stanzvor,
         x_stanznac TYPE eabl-stanznac,
         x_zuorddat TYPE eabl-zuorddat,
         x_ablbelnr TYPE eabl-ablbelnr,
         x_adattats TYPE eabl-adattats,
       END OF mreading.

DATA: BEGIN OF address,
        roomnumber TYPE ad_roomnum,
        floor TYPE ad_floor,
        building TYPE ad_bldng,
        house_num1 TYPE ad_hsnm1,
        str_suppl1 TYPE ad_strspp1,
        house_num2 TYPE ad_hsnm2,
        street TYPE ad_street,
        str_suppl3 TYPE ad_strspp3,
        location TYPE ad_lctn,
        city2 TYPE ad_city2,
        city1 TYPE ad_city1,
      END OF address.

DATA: instal TYPE eastl-anlage,
      buspart TYPE but000-partner,
      devloc TYPE itob-tplnr,
      premise TYPE eanl-vstelle,
      orgname TYPE but000-name_org1,
      firstname TYPE but000-name_first,
      lastname TYPE but000-name_first,
      statnote TYPE te582t-note_text,
      prev1 TYPE eabl-v_zwstand,
      prev2 TYPE eabl-v_zwstand,
      prevread    TYPE c LENGTH 6,
      prevconsump TYPE c LENGTH 6,
      aveconsump  TYPE c LENGTH 8,
      expected    TYPE c LENGTH 8,
      can         TYPE ever-vkonto,
      perconsump TYPE i,
      predec TYPE n,
      regnum TYPE etdz-logikzw,
      ratetyp TYPE easts-tarifart,
      connobj TYPE evbs-haus,
      tmpgov TYPE eanl-anlart.


DATA: mrsched TYPE rlgrap-filename,
      mru     TYPE c LENGTH 8,
      tmpablbelnr TYPE eabl-ablbelnr,
      tmpbill_sched TYPE eablg-abrdats,
      spl_file2 TYPE rlgrap-filename,
      rpt TYPE c LENGTH 2000.

DATA: rc TYPE bapireturn1.

*** FOR COUNTING THE RETRIEVED RECORDS ***
DATA: c TYPE i,
      c2 TYPE i,
      rec TYPE c LENGTH 6,
      c_active TYPE c LENGTH 6,
      c_inactive TYPE c LENGTH 6,
      num TYPE p DECIMALS 0.

*** FOR GETTING THE 12 MONTH PERIOD FOR CONSUMPTION ***
DATA: period TYPE d.
DATA:  num_rec TYPE n, num_rec2 TYPE n, num_rec3 TYPE n.

DATA: anzpers TYPE evbs-anzpers.

DATA: BEGIN OF itab_cs OCCURS 0,
         vkont TYPE fkkvkp-vkont,
         meter_tag TYPE zcheckmtr-meter_tag,
      END OF itab_cs.

*DATA : BEGIN OF itab_out OCCURS 0,
*        mru(8),
*        details(2005),
*       END OF itab_out.

TYPES: BEGIN OF ty_out,
        mru TYPE c LENGTH 8,
        mrid TYPE ablbelnr,
        details TYPE c LENGTH 2005,
       END OF ty_out.

DATA:  itab_out TYPE TABLE OF ty_out.

FIELD-SYMBOLS:
       <fs_out> TYPE ty_out.

DATA:  BEGIN OF it_eablg OCCURS 0,
         ableinh     TYPE ableinh,
         adatsoll    TYPE adatsoll,
         ablbelnr    TYPE ablbelnr,
         anlage      TYPE anlage,
         abrdats     TYPE abrdats,
       END OF it_eablg.

DATA: gt_pua         TYPE TABLE OF zosb_pua,
      reference_date TYPE sydatum.

FIELD-SYMBOLS:
      <fs_pua>       TYPE zosb_pua.

DATA: gv_serial_count TYPE i.