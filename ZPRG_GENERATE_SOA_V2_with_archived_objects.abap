***************************************************************************************************
* PROGRAM ID      : ZPRG_GENERATE_SOA
* TITLE           : Extraction of SOA
* CREATE DATE     : 01/24/2012
* AUTHOR          : BMABINI
*--------------------------------------------------------------------------------------------------
* DESCRIPTION     :
*--------------------------------------------------------------------------------------------------
* CHANGE HISTORY
*--------------------------------------------------------------------------------------------------
*    DATE    |  NAME   |                  DESCRIPTION                        |     Reference
*--------------------------------------------------------------------------------------------------
* 01/24/2012 | BMABINI | Initial Development                                 |
* 03/15/2012 | RANDYSY | Fix PUA Field / Optimize Executions                 |
* 05/31/2012 | RANDYSY | Re-implementation of Single Billing/Invoicing       | TN#15645
* 07/04/2012 | RANDYSY | Error on Printing of SOA & Change Source of TIN No. | TN#17230 & TN#17164
* 07/31/2012 | RANDYSY | Add STM and PIA computations                        | TN#17891
* 08/16/2012 | RANDYSY | Use Actual Meter Reading Date as basis              | TN#18798
* 09/10/2012 | RANDYSY | Change Arrangement of Items & Include Installation  | TN#19587
* 09/26/2012 | RANDYSY | Add Tagging for Dishonored Checks                   | TN#20454
* 10/11/2012 | RANDYSY | Changed the query for Installment Dues              | TN#21248
* 10/12/2012 | RANDYSY | Added Tax Code on Payment History                   | TN#
* 11/05/2012 | RANDYSY | Special Transaction code for KAM Accts              | c/o Geriec
* 10/21/2013 | RANDYSY | Copied latest SOA program                           | TN#45188
* 10/29/2013 | RANDYSY | Adjusted Layout for Bill.History and Payment        | TN#47542
* 11/26/2013 | GDIMALIWAT | Added retrieval of Archived Bill object          | TN#47542
*--------------------------------------------------------------------------------------------------

REPORT  zprg_generate_soa LINE-SIZE 1000 NO STANDARD PAGE HEADING.

*&---------------------------------------------------------------------*
*& Includes - (<include name/s>)
*&
INCLUDE: zinclude_esoa.

DATA:    s_months TYPE s012-spmon,
         gv_archived TYPE c LENGTH 1.

*&---------------------------------------------------------------------*
*& Type Pools - (<pool name/s>)
*&
TYPE-POOLS: abap, vrm, isu07.

*&---------------------------------------------------------------------*
*& Tables - (<table name/s>)
*&
TABLES:  s012, but000, zmcf, fkkvkp, t247, erch, ever, etrft, dberchz1, equi, zmwosb_s01p, equz,
         iloa, dfkkop, but020, adrc.

*&----------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
 s_can    FOR fkkvkp-vkont OBLIGATORY NO-EXTENSION NO INTERVALS DEFAULT '52718898'. "-- CAN

PARAMETER:
* s_months LIKE s012-spmon AS LISTBOX OBLIGATORY VISIBLE LENGTH 9 DEFAULT sy-datum.
  s_month  TYPE char02 AS LISTBOX OBLIGATORY VISIBLE LENGTH 6 DEFAULT sy-datum+4(2),
  s_year   TYPE char04 AS LISTBOX OBLIGATORY VISIBLE LENGTH 8 DEFAULT sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.

PARAMETERS:
 rb_prev RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
 rb_save RADIOBUTTON GROUP grp1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
"-- Destination File Name
PARAMETER: spl_file LIKE rlgrap-filename DEFAULT 'C:\' MODIF ID f1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(75) text-004 MODIF ID f1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR spl_file.
  PERFORM select_file_path USING spl_file.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'F1'.
      CASE 'X'.
        WHEN rb_prev.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN rb_save.
          screen-active = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Initialization - Initial process at start of program
*&
INITIALIZATION.
*  "-- Set Months of List Parameter
*  PERFORM populate_s_months.

  "-- Populate Month & Year Parameter
  PERFORM populate_monthyear.

  "-- Macro Definition for Adding Values
  DEFINE m_get_sum.
    &1 = &1 + &2.
  END-OF-DEFINITION.

  "-- Macro Definition for Appending Notice Table
  DEFINE mc_append_notice.
    append initial line to gt_notice assigning <fs_notice>.
    <fs_notice>-text = &1.
  END-OF-DEFINITION.

  "-- Macro Definition for Conversion Alpha
  DEFINE mc_conversion_alpha.
    "-- Add Zero Paddings
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = &1
      IMPORTING
        output = &1.
  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  "-- Set Month-Year Parameter
  CONCATENATE s_year s_month INTO s_months.

  "-- Check Parameter Inputs
  PERFORM check_parameters CHANGING sy_subrc.

  IF sy_subrc EQ 0.

    PERFORM: get_monthtxt      USING s_months,
             get_billdaterange USING s_months.

    "-- Get CAN, Client Name, Business Partner, Business Area
    SELECT vkont vkbez gpart gsber
      INTO CORRESPONDING FIELDS OF TABLE it_soa
    FROM   fkkvkp
    WHERE  vkont IN s_can.

    IF sy-subrc NE 0.
      MESSAGE i398(00) WITH 'Contract Account not found.'.
      LEAVE PROGRAM.
    ENDIF.

    LOOP AT it_soa ASSIGNING <fs_soa>.
      <fs_soa>-billmonth = monthtext.

      PERFORM get_address USING    <fs_soa>-gpart
                          CHANGING <fs_soa>-address.

      PERFORM get_ba_text USING    <fs_soa>-gsber
                          CHANGING <fs_soa>-gsbertxt
                                   <fs_soa>-tin.

      "-- Get Meter Reading Details
      PERFORM get_reading_details USING    <fs_soa>-vkont
                                           tmp_firstdayofmonth
                                           tmp_lastdayofmonth
                                  CHANGING sy_subrc
                                           <fs_soa>-anlage
                                           <fs_soa>-ablbelnr
                                           <fs_soa>-ableinh
                                           <fs_soa>-adatsoll
                                           <fs_soa>-adattats
                                           <fs_soa>-equnr
                                           <fs_soa>-erdat
                                           <fs_soa>-istablart
                                           <fs_soa>-stanzvor
                                           <fs_soa>-stanznac
                                           <fs_soa>-zuorddat
                                           <fs_soa>-atim
                                           <fs_soa>-zwnummer
                                           <fs_soa>-mrreason.

      IF sy_subrc EQ 0.
        "-- Actual Meter Reading Date (ADATTATS) will be used if greater than Scheduled Meter Reading Date (ADATSOLL)
        IF <fs_soa>-adattats GE <fs_soa>-adatsoll.
          <fs_soa>-reading_date = <fs_soa>-adattats.
        ELSE.
          <fs_soa>-reading_date = <fs_soa>-adatsoll.
        ENDIF.

        "-- Retrieve Meter Number (Serial Number)
        SELECT SINGLE sernr
          INTO <fs_soa>-sernr
        FROM   equi
        WHERE  equnr EQ <fs_soa>-equnr.

        "-- Retrieve Location and account assignment (ILOAN) required by Sequence number in Table ILOA.
        SELECT SINGLE iloan
          INTO <fs_soa>-iloan
        FROM   equz
        WHERE  equnr EQ <fs_soa>-equnr
        AND    datbi EQ '99991231'.

        "-- Retrieve Sequence Number
        SELECT SINGLE eqfnr
          INTO <fs_soa>-eqfnr
        FROM   iloa
        WHERE  iloan EQ <fs_soa>-iloan.

        "-- Concatenate Prev and Pres Reading Dates for Billing Period (Text)
        CONCATENATE s_months(4) '/' s_months+4(2) INTO <fs_soa>-billing_period.

        "-- Reset archive variable
        CLEAR gv_archived.

        "-- Retrieve Billing Document Number
        SELECT SINGLE belnr
          INTO (<fs_soa>-belnr)
        FROM   erch
        WHERE  vkont     EQ <fs_soa>-vkont
        AND    stornodat EQ ``
        AND    adatsoll  EQ <fs_soa>-adatsoll.

        IF sy-subrc EQ 0.
          "-- Extract details normally
          SELECT SINGLE tarifnr
          INTO <fs_soa>-tarifnr
          FROM   dberchz1
          WHERE  belnr EQ <fs_soa>-belnr
          AND    belzart EQ 'ZBASIC'.

          SELECT SINGLE belzeile
            INTO <fs_soa>-belzeile
          FROM   dberchz1
          WHERE  belnr EQ <fs_soa>-belnr
          AND    belzart EQ 'ZOTR'.

          IF sy-subrc EQ 0.
            SELECT SINGLE nettobtr
              INTO <fs_soa>-otr
            FROM   dberchz3
            WHERE  belnr    EQ <fs_soa>-belnr
            AND    belzeile EQ <fs_soa>-belzeile.
          ENDIF.
        ELSE.
          "-- Tag archive variable
          gv_archived = 'X'.

          "-- Extract details from archive
          PERFORM get_archived_bill USING <fs_soa>-vkont <fs_soa>-adatsoll
                                    CHANGING <fs_soa>-belnr.

          PERFORM get_archived_tarifnr USING <fs_soa>-belnr
                                       CHANGING <fs_soa>-tarifnr.

          PERFORM get_archived_belzeile USING <fs_soa>-belnr
                                        CHANGING <fs_soa>-belzeile.

          IF <fs_soa>-belzeile IS NOT INITIAL.
            PERFORM get_archived_nettobtr USING <fs_soa>-belnr <fs_soa>-belzeile
                                          CHANGING <fs_soa>-otr.
          ENDIF.
        ENDIF.

        "-- Retrieve CAN Address
        SELECT SINGLE lb_place
          INTO <fs_soa>-lb_place
        FROM zmcf
        WHERE acct_nb IN s_can.


        CASE <fs_soa>-tarifnr.
          WHEN 'COM01' OR 'COM02' OR 'COM03'.
            <fs_soa>-tarifnrtxt = 'Commercial'.

          WHEN 'RES01' OR 'RES02' OR 'RES03'.
            <fs_soa>-tarifnrtxt = 'Residential'.

          WHEN 'IND01' OR 'IND02' OR 'IND03'.
            <fs_soa>-tarifnrtxt = 'Industrial'.

          WHEN 'SEM01' OR 'SEM02' OR 'SEM03'.
            <fs_soa>-tarifnrtxt = 'Semi-Business'.

          WHEN OTHERS.
            SELECT SINGLE tarifbez
              INTO <fs_soa>-tarifnrtxt
            FROM   etrft
            WHERE  spras   EQ 'E'
            AND    tarifnr EQ <fs_soa>-tarifnr.
        ENDCASE.

        "-- Get Reference Number of Bill Month
        PERFORM get_bill_reference_number USING    <fs_soa>-vkont
                                                   <fs_soa>-erdat
                                          CHANGING gv_refno
                                                   <fs_soa>-faedn.
        "-- Get Bill Details
        PERFORM zbilldet_routine USING    gv_refno
                                          co_refno
                                 CHANGING <fs_soa>-belnr
                                          sy_subrc.

        IF sy_subrc NE 0.
          MESSAGE s398(00) WITH 'No Billing Summary Records'. EXIT.
        ENDIF.

        "-- Check if ISU Invoicing - Get Correct Document Number
        IF gv_refno NE co_refno.
          DATA: lv_xblnr TYPE dfkkop-xblnr.

          lv_xblnr = wa_zbilldet-opbel.

          "-- Format Document Number - Add Zero Paddings
          mc_conversion_alpha lv_xblnr.

          "-- Get Document Number
          SELECT SINGLE opbel
            INTO wa_zbilldet-opbel
          FROM   dfkkop
          WHERE  xblnr EQ lv_xblnr
          AND    vkont EQ <fs_soa>-vkont.
        ENDIF.

        IF sy-subrc NE 0.
          CONCATENATE tmp_errors 'NO ZBILLDET DATA' INTO tmp_errors SEPARATED BY space.
        ENDIF.

        <fs_soa>-consump = floor( wa_zbilldet-consump ).

        SHIFT <fs_soa>-consump LEFT DELETING LEADING space.

        <fs_soa>-prevrdgdt = wa_zbilldet-prevrdgdt.
        <fs_soa>-presrdgdt = wa_zbilldet-presrdgdt.
        <fs_soa>-prevrdg   = wa_zbilldet-prevrdg. SHIFT <fs_soa>-prevrdg LEFT DELETING LEADING space.
        <fs_soa>-presrdg   = wa_zbilldet-presrdg. SHIFT <fs_soa>-presrdg LEFT DELETING LEADING space.
        <fs_soa>-basic     = wa_zbilldet-basic.
        <fs_soa>-fcda      = wa_zbilldet-fcda.
        <fs_soa>-env       = wa_zbilldet-env.
        <fs_soa>-sew       = wa_zbilldet-sew.
        <fs_soa>-msc       = wa_zbilldet-msc.
        <fs_soa>-scd       = wa_zbilldet-dissc.
        <fs_soa>-opbel     = wa_zbilldet-opbel.
        <fs_soa>-stm       = wa_zbilldet-stm.
        <fs_soa>-pia       = wa_zbilldet-pia.

        PERFORM get_paydoc     USING <fs_soa>-gpart
                                     <fs_soa>-vkont
                                     <fs_soa>-anlage.
        PERFORM disctag.

        IF wa_zbilldet-cera > 0.
          <fs_soa>-basic = <fs_soa>-basic + wa_zbilldet-cera.
        ENDIF.

        IF wa_zbilldet-pa <> 0.
          <fs_soa>-basic = <fs_soa>-basic + wa_zbilldet-pa.
        ENDIF.

        "-- Get Sum of Total Current Charges before Taxes
        m_get_sum <fs_soa>-tccbt: <fs_soa>-basic,
                                  <fs_soa>-fcda,
                                  <fs_soa>-env,
                                  <fs_soa>-msc,
                                  <fs_soa>-scd,
                                  <fs_soa>-sew,
                                  <fs_soa>-otr.

        IF <fs_soa>-tot_cur_chrg IS INITIAL.
          "-- VAT Computation
          PERFORM compute_vat USING    <fs_soa>-vkont
                                       <fs_soa>-anlage
                                       <fs_soa>-tccbt
                              CHANGING <fs_soa>-vat.

          "-- Total Current Charges
          <fs_soa>-tot_cur_chrg = <fs_soa>-tccbt        + <fs_soa>-vat.

          "-- Total Current Charges Less PIA
          <fs_soa>-tot_cur_chrg = <fs_soa>-tot_cur_chrg + <fs_soa>-pia.
        ENDIF.

        "-- Get Miscellaneous Charges
        PERFORM get_charges USING    <fs_soa>-vkont
                            CHANGING <fs_soa>-instal_due
                                     <fs_soa>-inst_charge
                                     <fs_soa>-reopen_charge
                                     <fs_soa>-mtr_charge
                                     <fs_soa>-other_charge
                                     <fs_soa>-guar_deposit.

        "-- Get Sum of Subtotal Amount
        m_get_sum <fs_soa>-subtot_amt: <fs_soa>-tot_cur_chrg,
                                       <fs_soa>-pua,
                                       <fs_soa>-instal_due,
                                       <fs_soa>-reopen_charge,
                                       <fs_soa>-inst_charge,
                                       <fs_soa>-mtr_charge,
                                       <fs_soa>-other_charge,
                                       <fs_soa>-stm,
                                       <fs_soa>-guar_deposit.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0.
      MESSAGE i398(00) WITH tmp_errors.
      LEAVE PROGRAM.
    ENDIF.

    "-- Tagging for Dishonored Check
    PERFORM tag_dishonored_check      USING    <fs_soa>-vkont
                                               <fs_soa>-adattats
                                      CHANGING <fs_soa>-cash_payment.

    "-- Get Previous 3 Months Consumptions
    PERFORM get_previous_consumptions USING    <fs_soa>-vkont
                                               tmp_firstdayofmonth
                                      CHANGING <fs_soa>-prev_months
                                               <fs_soa>-prev_consumptions.

    "-- Populate Advisories
    PERFORM populate_advisory USING <fs_soa>-consump
                                    <fs_soa>-ave_consump    "TN#30334 Inserted by Ann 20130325
                                    <fs_soa>-anlage
                                    tmp_firstdayofmonth
                                    <fs_soa>-reading_date.
    "*** end - 02/27/2013 - Change SOA Format - TN#28322

    CASE <fs_soa>-istablart.
      WHEN '91'.
        CONCATENATE <fs_soa>-consump 'AVE' INTO <fs_soa>-consump SEPARATED BY space.
      WHEN '93'.
        CONCATENATE <fs_soa>-consump 'ADJ' INTO <fs_soa>-consump SEPARATED BY space.
    ENDCASE.

    SHIFT <fs_soa>-consump LEFT DELETING LEADING space.

    "-- Transfer To ITAB and Display
    PERFORM display_details.

    IF it_pmthist[] IS INITIAL.
      "it_pmthist-desc1 = 'None'. APPEND it_pmthist.  "TN#30334 Comment out by Ann 20130327
      MESSAGE s398(00) WITH 'No Payment History Records'.
    ENDIF.

    IF it_billsum[] IS INITIAL.
      MESSAGE s398(00) WITH 'No Billing Summary Records'. EXIT.
    ENDIF.

    PERFORM smartform_pdf.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_MONTHTXT
*&---------------------------------------------------------------------*
*       Retrieve Month Name in text based on Month Number
*----------------------------------------------------------------------*
FORM get_monthtxt USING mon_year.
  DATA: tmp_monthnumber(2) TYPE n.
  DATA: tmp_t247 TYPE TABLE OF t247 WITH HEADER LINE.

  tmp_monthnumber = mon_year+4(2).

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = 'E'
      month = tmp_monthnumber
    IMPORTING
      t247  = tmp_t247.

  monthtext = tmp_t247-ltx.
  CONCATENATE 'For the month of' monthtext mon_year(4) INTO monthtext SEPARATED BY space.
ENDFORM.                    "GET_MONTHTXT

*&---------------------------------------------------------------------*
*&      Form  GET_BILLDATERANGE
*&---------------------------------------------------------------------*
*       Retrieve date of Last Day of Billing Month
*----------------------------------------------------------------------*
FORM get_billdaterange USING mon_year.

  CONCATENATE mon_year '01' INTO tmp_firstdayofmonth.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = tmp_firstdayofmonth
    IMPORTING
      last_day_of_month = tmp_lastdayofmonth.

ENDFORM.                    "GET_BILLDATERANGE

*&---------------------------------------------------------------------*
*&      Form  GET_BUSINESS_AREA_TEXT
*&---------------------------------------------------------------------*
*       Get Business Area Text
*----------------------------------------------------------------------*
FORM get_ba_text USING    lv_gsber    LIKE fkkvkp-gsber
                 CHANGING lv_gsbertxt TYPE char30
                          lv_tin      TYPE char15.
  "-- Clear Variables
  CLEAR: lv_tin, lv_gsbertxt.

  "-- Get TIN No.
  SELECT SINGLE soc_secure
    INTO lv_tin
  FROM   ekun
  WHERE  partner EQ <fs_soa>-gpart.

  "--Get BA Text directly from Table
  SELECT SINGLE gtext INTO lv_gsbertxt
    FROM tgsbt
    WHERE spras EQ sy-langu
      AND gsber EQ lv_gsber.
ENDFORM.                    "GET_BUSINESS_AREA_TEXT

*&---------------------------------------------------------------------*
*&      Form  get_paydoc
*&---------------------------------------------------------------------*
*       --FROM ZPRG_BILLPRINTV2
*----------------------------------------------------------------------*
FORM get_paydoc USING f_bp        TYPE gpart_kk
                      f_can       TYPE vkont_kk
                      f_instal_no TYPE anlage.

  TYPES: BEGIN OF ty_paydoc,
          budat       TYPE char10,
          zdel1       TYPE char01, "'-',
          paydoc      TYPE char12,
          zdel2       TYPE char01, "'-',
          tag         TYPE char04,
          zdel3       TYPE char01, "'-',
          amount      LIKE dfkkop-betrw,
          zdel4       TYPE char01, "'-',
          vat         LIKE dfkkop-betrw,
          zdel5       TYPE char01, "'-',
          netofvat    LIKE dfkkop-betrw,
         END OF ty_paydoc.

  DATA:  it_paydoc    TYPE TABLE OF ty_paydoc,
         wa_paydoc    TYPE ty_paydoc.

  FIELD-SYMBOLS:
        <fs_paydoc>   TYPE ty_paydoc.

  TYPES: BEGIN OF ty_dfkkzp,
          augbl2      LIKE dfkkop-augbl,
          augbd       LIKE dfkkop-augdt,
          amt         LIKE dfkkop-betrw,
         END OF ty_dfkkzp.

  DATA:  it_dfkkzp    TYPE TABLE OF ty_dfkkzp.

  FIELD-SYMBOLS:
         <fs_dfkkzp>  TYPE ty_dfkkzp.

  DATA:  txtbudat(10), txtpaydoc(12), txtamount(15), txtvat(15),
         txtnetofvat(15).

  DATA:  totwb        LIKE dfkkop-betrw,
         totgd        LIKE dfkkop-betrw,
         totmisc      LIKE dfkkop-betrw,
         totvatwb     LIKE dfkkop-betrw,
         totvatmisc   LIKE dfkkop-betrw,
         instwb       LIKE dfkkop-betrw,
         instgd       LIKE dfkkop-betrw,
         instmisc     LIKE dfkkop-betrw,
         instvatwb    LIKE dfkkop-betrw,
         instvatmisc  LIKE dfkkop-betrw,
         tot          LIKE dfkkop-betrw,
         total        LIKE dfkkop-betrw.

  DATA:  totvat       LIKE dberdl-sbetw.

  DATA:  txtdoc(400),
         tmpbp        LIKE fkkvkp-gpart,
         tmpbp2       LIKE fkkvkp-gpart,
         tmpcan       LIKE fkkvkp-vkont,
         txttag       TYPE char04,
         conacct      LIKE fkkvkp-vkont.

  DATA:  pvat         TYPE p.

  TYPES: BEGIN OF ty_dfkkop,
          hvorg       LIKE dfkkop-hvorg,
          tvorg       LIKE dfkkop-tvorg,
          opbel       LIKE dfkkop-opbel,
          amt2        LIKE dfkkop-betrw,
          sctax       LIKE dfkkop-sctax,
         END OF ty_dfkkop.

  DATA:  it_dfkkop    TYPE TABLE OF ty_dfkkop.

  FIELD-SYMBOLS:
         <fs_dfkkop>  TYPE ty_dfkkop.

  pvat    = '0.0'.
  tot     = 0.
  tmpbp2  = f_bp.
  tmpbp   = f_bp.
  conacct = f_can.
  tmpcan  = f_can.

  SHIFT tmpbp   LEFT DELETING LEADING '0'.
  SHIFT conacct LEFT DELETING LEADING '0'.

***** MAIN PAYMENT LOOP *****
  REFRESH it_dfkkzp.

  SELECT opbel AS augbl2
         budat AS augbd
         betrz AS amt
  INTO  TABLE it_dfkkzp
  FROM dfkkzp
  WHERE budat LE <fs_soa>-presrdgdt AND
        budat GE <fs_soa>-prevrdgdt AND
        opbel NE '*' AND
        opbel NE space AND
        ruear EQ space AND
     ( ( selw1 EQ conacct OR selw1 EQ tmpbp OR selw1 EQ tmpcan OR selw1
   EQ tmpbp2 ) OR
       ( selw2 EQ conacct OR selw2 EQ tmpbp OR selw2 EQ tmpcan OR selw2
   EQ tmpbp2 ) OR
       ( selw3 EQ conacct OR selw3 EQ tmpbp OR selw3 EQ tmpcan OR selw3
   EQ tmpbp2 ) ).

  LOOP AT it_dfkkzp ASSIGNING <fs_dfkkzp>.
* unAllocated or Hang Payments
    tot    = 0.

    REFRESH it_dfkkop.
    SELECT hvorg tvorg opbel SUM( betrw ) AS amt2
    INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
    FROM dfkkop
    WHERE opbel EQ <fs_dfkkzp>-augbl2 AND
          augrd NE '05'
    GROUP BY hvorg tvorg opbel.

    LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
      IF ( <fs_dfkkop>-hvorg = '0060' OR  <fs_dfkkop>-hvorg = '0600' ).
        tot = tot + ( <fs_dfkkop>-amt2 * -1 ).
      ENDIF.
    ENDLOOP.

    wa_paydoc-paydoc = <fs_dfkkzp>-augbl2.
    wa_paydoc-amount = tot.
    total            = tot.
    totvat           = 0.

    SHIFT wa_paydoc-paydoc LEFT DELETING LEADING '0'.
    wa_paydoc-vat      = totvat.
    wa_paydoc-netofvat = wa_paydoc-amount.

    date = <fs_dfkkzp>-augbd.
    PERFORM convert_date.
    wa_paydoc-budat = date.
    IF wa_paydoc-tag EQ space.
      wa_paydoc-tag = 'WB'.
    ENDIF.

*   Allocated Payments  *****
    totwb       = 0.
    totgd       = 0.
    totmisc     = 0.
    totvatwb    = 0.
    totvatmisc  = 0.
    instwb      = 0.
    instgd      = 0.
    instmisc    = 0.
    instvatwb   = 0.
    instvatmisc = 0.

    "-- For Regular Payment **
    REFRESH it_dfkkop.

    SELECT hvorg
           tvorg
           augbl
           betrw AS amt2
           sctax
    INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
    FROM dfkkop
    WHERE augbl EQ <fs_dfkkzp>-augbl2
          AND augrd NE '05'
          AND hvorg NE '0080'.

    LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
      IF (    <fs_dfkkop>-hvorg = '0100'
           OR <fs_dfkkop>-hvorg = '0300'
           OR <fs_dfkkop>-hvorg = '0040'
           OR <fs_dfkkop>-hvorg = '0060'  ).
        totwb    = totwb    + <fs_dfkkop>-amt2.
        totvatwb = totvatwb + <fs_dfkkop>-sctax.
      ELSE.
        IF (   <fs_dfkkop>-hvorg EQ '6040'
            OR <fs_dfkkop>-hvorg EQ '6045'
            OR <fs_dfkkop>-hvorg EQ '6080' ).
          totgd = totgd + <fs_dfkkop>-amt2.
        ELSE.
          totmisc    = totmisc    + <fs_dfkkop>-amt2.
          totvatmisc = totvatmisc + <fs_dfkkop>-sctax.
        ENDIF.
      ENDIF.
    ENDLOOP.

    instvatwb        = ( ( instwb / '1.12' ) * '0.12' ).
    instvatmisc      = ( ( instmisc / '1.12' ) * '0.12' ).
    wa_paydoc-paydoc = <fs_dfkkzp>-augbl2.
    SHIFT wa_paydoc-paydoc LEFT DELETING LEADING '0'.

* Append to paydoc all details for WB **
    wa_paydoc-amount   = totwb + tot + instwb.
    wa_paydoc-vat      = totvatwb + instvatwb.
    wa_paydoc-netofvat = wa_paydoc-amount - wa_paydoc-vat.
    wa_paydoc-tag      = 'WB'.

    IF ( wa_paydoc-amount > 0 ).
      APPEND wa_paydoc TO it_paydoc.
    ENDIF.

* Append to paydoc all details for GD **
    wa_paydoc-amount   = totgd + instgd.
    wa_paydoc-vat      = 0.
    wa_paydoc-netofvat = wa_paydoc-amount.
    wa_paydoc-tag      = 'GD'.

    IF ( wa_paydoc-amount > 0 ).
      APPEND wa_paydoc TO it_paydoc.
    ENDIF.

* Append to paydoc all details for MISC **
    wa_paydoc-amount   = totmisc + instmisc.
    wa_paydoc-vat      = totvatmisc + instvatmisc.
    wa_paydoc-netofvat = wa_paydoc-amount - wa_paydoc-vat.
    wa_paydoc-tag      = 'MISC'.

    IF ( wa_paydoc-amount > 0 ).
      APPEND wa_paydoc TO it_paydoc.
    ENDIF.

  ENDLOOP.

  txtdoc = ''.
  txttag = ''.

  SORT it_paydoc BY tag ASCENDING paydoc ASCENDING.

  LOOP AT it_paydoc ASSIGNING <fs_paydoc>.
    txtbudat    = <fs_paydoc>-budat.
    txtpaydoc   = <fs_paydoc>-paydoc.
    txtamount   = <fs_paydoc>-amount.
    pvat        = <fs_paydoc>-vat.

    IF ( pvat > '0' ).
      txtvat    = <fs_paydoc>-vat.
    ELSE.
      txtvat    = '0.00'.
    ENDIF.

    txtnetofvat = <fs_paydoc>-netofvat.
    txttag      = <fs_paydoc>-tag.

    SHIFT txtpaydoc LEFT DELETING LEADING '0'.
    CONDENSE: txtbudat, txtpaydoc, txtamount, txtvat, txtnetofvat, txttag.

    IF txtdoc = ''.
      CONCATENATE txtbudat  '-'
                  txtpaydoc '-'
                  txttag    '-'
                  txtamount '-'
                  txtvat    '-'
                  txtnetofvat
             INTO txtdoc.
    ELSE.
      CONCATENATE txtdoc    ','
                  txtbudat  '-'
                  txtpaydoc '-'
                  txttag    '-'
                  txtamount '-'
                  txtvat    '-'
                  txtnetofvat
             INTO txtdoc.
    ENDIF.

    it_pmthist-desc1     = txttag.
    it_pmthist-netamt    = txtnetofvat.
    it_pmthist-pmtvat    = txtvat.
    it_pmthist-pmttotamt = txtamount.
    it_pmthist-or_no     = txtpaydoc.
    it_pmthist-or_dt     = txtbudat.

    "-- Indentify Tax Code for Payment - Inserted by RANDYSY 10/12/2012
    PERFORM determine_tax_code USING    f_can
                                        f_instal_no
                                        txttag
                               CHANGING it_pmthist-tax_code.

    IF it_pmthist-tax_code EQ `Zero-Rated Sales`.
      it_pmthist-pmttotamt = it_pmthist-netamt.
      it_pmthist-pmtvat    = '0.00'.
    ENDIF.

    APPEND it_pmthist.
  ENDLOOP.

  SORT it_pmthist BY or_dt DESCENDING or_no DESCENDING.
ENDFORM.                    "get_paydoc

*----------------------------------------------------------------------*
*                     FORM CONVERT DATE  _FROM ZPRG_BILLPRINTV2
*----------------------------------------------------------------------*
FORM convert_date.
  day = date+6(2).
  mo  = date+4(2).
  yr  = date+2(2).

  CASE mo.
    WHEN '01'. mo = 'Jan'. WHEN '02'. mo = 'Feb'.
    WHEN '03'. mo = 'Mar'. WHEN '04'. mo = 'Apr'.
    WHEN '05'. mo = 'May'. WHEN '06'. mo = 'Jun'.
    WHEN '07'. mo = 'Jul'. WHEN '08'. mo = 'Aug'.
    WHEN '09'. mo = 'Sep'. WHEN '10'. mo = 'Oct'.
    WHEN '11'. mo = 'Nov'. WHEN '12'. mo = 'Dec'.
  ENDCASE.
ENDFORM.                    "convert_date

*&---------------------------------------------------------------------*
*&      Form  smartform_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM smartform_pdf.
  CONSTANTS co_formname TYPE tdsfname VALUE 'ZFISU_OSBSOA_FORM'.

  "-- Get Function Module name of Smartforn
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING      "formname           = 'ZOSBSOA_FORM4' "'ZOSBSOA_FORM3'   "TN#30334 Comment out by Ann 20130325
      formname           = co_formname                         "TN#30334 Inserted by Ann 20130325
    IMPORTING
      fm_name            = form_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CASE 'X'.
    WHEN rb_prev.
      "-- Preview PDF File
      PERFORM preview_pdf.
    WHEN rb_save.
      "-- Download to PDF File
      PERFORM download_pdf.
  ENDCASE.
ENDFORM.                    "smartform_pdf

*&---------------------------------------------------------------------*
*&      Form  DISCTAG
*&---------------------------------------------------------------------*
*       Determine Disconnection Status (Reused from ZDOWNLOADRB by mpbacani)
*----------------------------------------------------------------------*
FORM disctag.
  DATA:  othertax     LIKE dfkkop-betrw,
         other        LIKE dfkkop-betrw,
         store_bal    LIKE dfkkop-betrw,
         store_inst   LIKE dfkkop-betrw,
         tmpother     LIKE dfkkop-betrw,
         instln       LIKE dfkkop-betrw,
         meter        LIKE dfkkop-betrw,
         reopen       LIKE dfkkop-betrw,
         tmpamount    LIKE dfkkop-betrw,
         tmpamount2   LIKE dfkkop-betrw,
         otax         LIKE dfkkop-betrw,
         puainstall   LIKE dfkkop-betrw,
         drum         LIKE dfkkop-betrw,
         installment  LIKE dfkkop-betrw,
         instmain     LIKE dfkkop-hvorg,
         instsub      LIKE dfkkop-tvorg.

  DATA:  lowest_date  LIKE sy-datum,
         tmpdt60      LIKE sy-datum.

  DATA:  end_billdt   LIKE eabl-erdat,
         start_billdt LIKE eabl-erdat,

         disc_tag     TYPE i.

  DATA:  it_dfkkop    TYPE TABLE OF ty_dfkkop_soa,
         lv_belnr     TYPE e_belnr.

  FIELD-SYMBOLS:
         <fs_dfkkop>  TYPE ty_dfkkop_soa.

  start_billdt = <fs_soa>-prevrdgdt.
  end_billdt   = <fs_soa>-erdat.
  puainstall   = 0.

  "-- Get Paid Instalment Plan
  PERFORM get_instal_amt "TABLES   it_dfkkop
                         USING    <fs_soa>-vkont
                                  <fs_soa>-faedn
                                  <fs_soa>-prevrdgdt
                                  <fs_soa>-erdat
                         CHANGING store_inst.

  "-- Get Store Balance
  PERFORM get_store_bal  TABLES   it_dfkkop
                         USING    <fs_soa>-vkont
                                  "<fs_soa>-faedn
                                  start_billdt
                                  end_billdt
                                  lowest_date
                                  <fs_soa>-tot_cur_chrg
                                  store_inst
                         CHANGING store_bal
                                  tmpother
                                  otax
                                  other
                                  othertax.
  REFRESH it_dfkkop.
  SELECT betrw AS amount
         faedn AS date
         hvorg
         tvorg
         opbel
         faedn
         opupk
         opupw
    INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
  FROM dfkkop
  WHERE vkont = <fs_soa>-vkont
  AND ( budat NE '00000000' )
  AND abwbl = space
  AND ( faedn LE end_billdt AND faedn NE '00000000' )
  AND ( augst EQ space )
  AND ( hvorg NE '0080' AND hvorg NE '0040' )
  AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
  AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) ).

  LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
    IF ( <fs_dfkkop>-hvorg = '9000').
      IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
        instln = instln + <fs_dfkkop>-amount.
      ENDIF.
      IF <fs_dfkkop>-faedn LT start_billdt.
        puainstall = puainstall + <fs_dfkkop>-amount.
      ENDIF.

    ELSE.
      IF ( <fs_dfkkop>-hvorg = '7002').
        IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
          meter = meter + <fs_dfkkop>-amount.
        ENDIF.
        IF <fs_dfkkop>-faedn LT start_billdt.
          puainstall = puainstall + <fs_dfkkop>-amount.
        ENDIF.
      ELSE.
        IF ( <fs_dfkkop>-hvorg = '7003').
          IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
            reopen = reopen + <fs_dfkkop>-amount.
          ENDIF.
          IF <fs_dfkkop>-faedn LT start_billdt.
            puainstall = puainstall + <fs_dfkkop>-amount.
          ENDIF.
        ELSE.
          IF ( <fs_dfkkop>-hvorg = '7004').
            IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
              drum = drum + <fs_dfkkop>-amount.
            ENDIF.
            IF <fs_dfkkop>-faedn LT start_billdt.
              puainstall = puainstall + <fs_dfkkop>-amount.
            ENDIF.
          ELSE.
            IF ( <fs_dfkkop>-hvorg = '7008').
              IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
                instln = instln + <fs_dfkkop>-amount.
              ENDIF.
              IF <fs_dfkkop>-faedn LT start_billdt.
                puainstall = puainstall + <fs_dfkkop>-amount.
              ENDIF.
            ELSE.

              IF ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0920' ) OR  "MIGDATA
                 ( <fs_dfkkop>-hvorg = '0200' AND <fs_dfkkop>-tvorg = '0020' ) OR  "FINAL BILLING
                 ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0020' ) OR  "MANUAL BACKBILLING
                 ( <fs_dfkkop>-hvorg = '0100' AND <fs_dfkkop>-tvorg = '0020' ) OR  "BILLS
                 ( <fs_dfkkop>-hvorg = '0100' AND <fs_dfkkop>-tvorg = '0010' ) OR  "BILLS ADDITIONAL
                 ( <fs_dfkkop>-hvorg = '0060' AND <fs_dfkkop>-tvorg = '0010' ) OR  "Overpayment
                 ( <fs_dfkkop>-hvorg = '0600' AND <fs_dfkkop>-tvorg = '0010' ) OR  "Open Payments
                 ( <fs_dfkkop>-hvorg = '9950' AND <fs_dfkkop>-tvorg = '0020' ) OR
                 ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0940' ) OR
                 ( <fs_dfkkop>-hvorg = '6005' AND (
                   <fs_dfkkop>-tvorg BETWEEN '0020' AND '0027' ) ) OR
                 ( <fs_dfkkop>-hvorg = '0300' AND (
                   <fs_dfkkop>-tvorg BETWEEN '0510' AND '0528' ) ).       "CREDIT

              ELSE.
                IF <fs_dfkkop>-faedn LE end_billdt.
                  puainstall = puainstall + <fs_dfkkop>-amount.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR <fs_dfkkop>-amount.

  ENDLOOP.

  REFRESH it_dfkkop.
  SELECT betrw AS amount
         faedn AS date
         hvorg
         tvorg
         opbel
         faedn
         opupk
         opupw
   INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
  FROM dfkkop
  WHERE vkont = <fs_soa>-vkont
  AND ( budat NE '00000000' )
  AND abwbl = space
  AND ( faedn LE end_billdt AND faedn NE '00000000' )
  AND ( augst EQ space )
  AND opupw = '000'
  AND hvorg = '0080' AND tvorg = '0010'
  AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
  AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) ).

  LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
    SELECT SINGLE hvorg tvorg
    INTO
    (instmain, instsub)
    FROM dfkkop
    WHERE abwbl = <fs_dfkkop>-opbel.

    IF  ( instmain = '9000').
      IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
        instln = instln + <fs_dfkkop>-amount.
      ENDIF.
      IF <fs_dfkkop>-faedn LT start_billdt.
        puainstall = puainstall + <fs_dfkkop>-amount.
      ENDIF.
    ELSE.
      IF ( instmain = '7002').
        IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
          meter = meter + <fs_dfkkop>-amount.
        ENDIF.
        IF <fs_dfkkop>-faedn LT start_billdt.
          puainstall = puainstall + <fs_dfkkop>-amount.
        ENDIF.
      ELSE.
        IF ( instmain = '7003').
          IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
            reopen = reopen + <fs_dfkkop>-amount.
          ENDIF.
          IF <fs_dfkkop>-faedn LT start_billdt.
            puainstall = puainstall + <fs_dfkkop>-amount.
          ENDIF.
        ELSE.
          IF ( instmain = '7004').
            IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
              drum = drum + <fs_dfkkop>-amount.
            ENDIF.
            IF <fs_dfkkop>-faedn LT start_billdt.
              puainstall = puainstall + <fs_dfkkop>-amount.
            ENDIF.
          ELSE.

            IF ( instmain = '7008').
              IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
                instln = instln + <fs_dfkkop>-amount.
              ENDIF.
              IF <fs_dfkkop>-faedn LT start_billdt.
                puainstall = puainstall + <fs_dfkkop>-amount.
              ENDIF.
            ELSE.

              IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
                installment = installment + <fs_dfkkop>-amount.
              ENDIF.
              IF <fs_dfkkop>-faedn LT start_billdt.
                puainstall = puainstall + <fs_dfkkop>-amount.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR <fs_dfkkop>-amount.
  ENDLOOP.

  REFRESH it_dfkkop.
  SELECT opbel
         faedn
         opupw
         whanz AS opupk
         augvd
    INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
  FROM dfkkopw
  WHERE vkont = <fs_soa>-vkont
  AND faedn LE end_billdt
  AND augvd NE space
  .

  LOOP AT it_dfkkop ASSIGNING <fs_dfkkop> WHERE augvd IS NOT INITIAL.
    SELECT SINGLE hvorg tvorg
    INTO   (instmain, instsub)
    FROM   dfkkop
    WHERE  abwbl = <fs_dfkkop>-opbel.

    SELECT SINGLE betrw
    INTO   (<fs_dfkkop>-amount)
    FROM   dfkkop
    WHERE  opupk = <fs_dfkkop>-opupk
    AND    opupw = '000'
    AND    opbel = <fs_dfkkop>-opbel
    AND    augbl = space "***
    .
    CLEAR  tmpamount.

    SELECT SUM( betrw )
    INTO   (tmpamount)
    FROM   dfkkop
    WHERE  opbel   = <fs_dfkkop>-opbel
    AND    ( augst = space OR augdt GT end_billdt )
    AND    opupw   = <fs_dfkkop>-opupw
    AND    opupk   = <fs_dfkkop>-opupk
    AND    faedn   LE end_billdt.

    CLEAR  tmpamount2.
    SELECT SUM( betrw )
    INTO   (tmpamount2)
    FROM   dfkkop
    WHERE  opbel   = <fs_dfkkop>-opbel
    AND    ( augst NE space AND augdt LT end_billdt )
    AND    opupw   = <fs_dfkkop>-opupw
    AND    opupk   = <fs_dfkkop>-opupk
    AND    faedn   LE end_billdt.

    IF tmpamount >= 0 AND tmpamount2 > 0.
      <fs_dfkkop>-amount = tmpamount.
    ENDIF.

    IF  ( instmain = '9000').
      IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
        instln = instln + <fs_dfkkop>-amount.
      ENDIF.
      IF <fs_dfkkop>-faedn LT start_billdt.
        puainstall = puainstall + <fs_dfkkop>-amount.
      ENDIF.
    ELSE.
      IF ( instmain = '7002').
        IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
          meter = meter + <fs_dfkkop>-amount.
        ENDIF.
        IF <fs_dfkkop>-faedn LT start_billdt.
          puainstall = puainstall + <fs_dfkkop>-amount.
        ENDIF.
      ELSE.
        IF ( instmain = '7003').
          IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
            reopen = reopen + <fs_dfkkop>-amount.
          ENDIF.
          IF <fs_dfkkop>-faedn LT start_billdt.
            puainstall = puainstall + <fs_dfkkop>-amount.
          ENDIF.
        ELSE.
          IF ( instmain = '7004').
            IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
              drum = drum + <fs_dfkkop>-amount.
            ENDIF.
            IF <fs_dfkkop>-faedn LT start_billdt.
              puainstall = puainstall + <fs_dfkkop>-amount.
            ENDIF.
          ELSE.
            IF ( instmain = '7008').
              IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
                instln = instln + <fs_dfkkop>-amount.
              ENDIF.
              IF <fs_dfkkop>-faedn LT start_billdt.
                puainstall = puainstall + <fs_dfkkop>-amount.
              ENDIF.
            ELSE.
              IF <fs_dfkkop>-faedn LE end_billdt AND <fs_dfkkop>-faedn GE start_billdt.
                installment = installment + <fs_dfkkop>-amount.
              ENDIF.
              IF <fs_dfkkop>-faedn LT start_billdt.
                puainstall = puainstall + <fs_dfkkop>-amount.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR <fs_dfkkop>-amount.
  ENDLOOP.

  "-- Checks if in PSR Table
  SELECT SINGLE belnr
    INTO lv_belnr
  FROM   zmwosb_psr
  WHERE  belnr EQ <fs_soa>-belnr.

  IF sy-subrc EQ 0.
    "-- Get PUA from ZOSB_PUA (Data based from OSB Download)
    PERFORM get_pua_from_download USING    <fs_soa>-vkont
                                           <fs_soa>-adatsoll
                                  CHANGING <fs_soa>-pua.

    IF sy-subrc NE 0.
      "-- If PUA is not in PUA Table
      IF <fs_soa>-pua IS INITIAL.
        <fs_soa>-pua = store_bal + other + othertax + puainstall.
      ENDIF.
    ENDIF.
  ELSE.
    DATA: lv_budat    TYPE char08,
          lv_due_date TYPE faedn_kk,
          lv_length   TYPE i,
          lv_sched_mr TYPE sydatum.

    lv_budat = wa_zbilldet-budat. "<fs_soa>-erdat. "<fs_soa>-adatsoll.

    "-- Get PUA from Function Module
    PERFORM get_pua_from_fm USING    <fs_soa>-vkont
                                     lv_budat
                                     wa_zbilldet-opbel
                            CHANGING <fs_soa>-pua
                                     lv_due_date.

    "-- Check if the Bill. Date Range is greater that 34 days.
    lv_length = <fs_soa>-presrdgdt - <fs_soa>-prevrdgdt.

    IF lv_length GT 34.
      "-- Get Sched MR
      PERFORM get_sched_mr USING    <fs_soa>-ableinh
                                    <fs_soa>-vkont
                                    tmp_lastdayofmonth
                           CHANGING lv_sched_mr.

      "-- Check if Due Date of PUA is within Billing Period then Removes PUA
      IF lv_due_date GE lv_sched_mr. "AND pv_due_date LE <fs_soa>-presrdgdt.
        CLEAR: <fs_soa>-pua.
      ENDIF.
    ELSE.
      "-- Check if Due Date of PUA is within Billing Period then Removes PUA
      IF lv_due_date GE <fs_soa>-prevrdgdt. "AND pv_due_date LE <fs_soa>-presrdgdt.
        CLEAR: <fs_soa>-pua.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( store_bal > 0  ).
    IF ( store_bal <  80 ).
      disc_tag = '0'.
      <fs_soa>-disc_txt = space.
    ELSE.
      tmpdt60 = lowest_date + 40.

      IF ( tmpdt60 < end_billdt ).
        <fs_soa>-disc_txt = '2'.
      ELSE.
        <fs_soa>-disc_txt = '1'.
      ENDIF.
    ENDIF.
  ELSE.
    disc_tag = '0'.
    <fs_soa>-disc_txt = space.
  ENDIF.
ENDFORM.                    "DISCTAG

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_details .
  DATA: lv_pua_desc TYPE char100,
        lv_amt      TYPE p DECIMALS 2,
        lv_subtot   TYPE betrw_kk.   "TN#30334 Inserted by Ann 20130326

  DEFINE m_append_bill_sum.
    clear lv_amt.
    if &1 ne &2.
      it_billsum-billdesc = &4.
      lv_amt              = &1.
      write lv_amt to it_billsum-amt. "TN#30334 Inserted by Ann 20130326
      it_billsum-tabind   = &3.       "TN#30334 Inserted by Ann 20130325
      call function 'CLOI_PUT_SIGN_IN_FRONT'
        changing
          value = it_billsum-amt.
      append it_billsum.
    endif.
  END-OF-DEFINITION.

  LOOP AT it_soa ASSIGNING <fs_soa>.
    it_pdf-billmonth         = <fs_soa>-billmonth.
    it_pdf-soanumber         = <fs_soa>-ablbelnr.
    it_pdf-vkont             = <fs_soa>-vkont.
    it_pdf-vkbez             = <fs_soa>-vkbez.
    it_pdf-address           = <fs_soa>-address.
    it_pdf-tarifnrtxt        = <fs_soa>-tarifnrtxt.
    it_pdf-gsbertxt          = <fs_soa>-gsbertxt.
    it_pdf-tin               = <fs_soa>-tin.
    it_pdf-sernr             = <fs_soa>-sernr.
    it_pdf-ableinh           = <fs_soa>-ableinh.
    it_pdf-eqfnr             = <fs_soa>-eqfnr.
    it_pdf-reading_date      = <fs_soa>-reading_date.
    it_pdf-presrdg           = <fs_soa>-presrdg.
    it_pdf-prevrdg           = <fs_soa>-prevrdg.
    it_pdf-presrdgdt         = <fs_soa>-presrdgdt.
    it_pdf-prevrdgdt         = <fs_soa>-prevrdgdt.
    it_pdf-consump           = <fs_soa>-consump.
    lv_subtot                = <fs_soa>-subtot_amt.
    WRITE lv_subtot TO it_pdf-subtot_amt.
    it_pdf-pmtduedt          = <fs_soa>-faedn.
    it_pdf-disctxt           = <fs_soa>-disc_txt.
    it_pdf-cash_payment      = <fs_soa>-cash_payment.
    it_pdf-prev_months       = <fs_soa>-prev_months.
    it_pdf-prev_consumptions = <fs_soa>-prev_consumptions.


    CLEAR lv_pua_desc.

    IF <fs_soa>-pua NE 0.
      IF <fs_soa>-pua < 0.
        lv_pua_desc  = 'Overpayments'.
        <fs_soa>-pua = abs( <fs_soa>-pua ) ."-- Always positive values
      ELSE.
        lv_pua_desc  = 'Overdue Amount (Please pay immediately)'.     "--TN#30334 Inserted by Ann 20130326
      ENDIF.
    ENDIF.

    m_append_bill_sum:
         <fs_soa>-basic         0       abap_true   'Basic Charge',
         <fs_soa>-fcda          space   abap_true   'FCDA',                                             "TN#30334 Inserted by Ann 20130326
         <fs_soa>-env           0       abap_true   'Environmental Charge',
         <fs_soa>-sew           0       abap_true   'Sewer Charge',
         <fs_soa>-msc           0       abap_true   'Maintenance Service Charge (MSC)',
         <fs_soa>-scd           0       abap_true   'Senior Citizen Discount',    "-- Added RANDYSY 09/12/2012
         <fs_soa>-otr           0       abap_true   '2012 One Time Tariff Reduction',
         <fs_soa>-tccbt         0       abap_true   'Total Current Charges before Taxes',
         <fs_soa>-vat           0       abap_true   'VAT',                    "TN#30334 Inserted by Ann 20130326
         <fs_soa>-tot_cur_chrg  0       ' '         'Total Current Charges',
         <fs_soa>-pua           0       ' '         lv_pua_desc,
         <fs_soa>-instal_due    0       ' '         'Installments Due',           "-- Added RANDYSY 09/11/2012
         <fs_soa>-inst_charge   0       ' '         'Installation Charges',
         <fs_soa>-reopen_charge 0       ' '         'Re-Opening Fee',
         <fs_soa>-mtr_charge    0       ' '         'Meter Charge',
         <fs_soa>-other_charge  0       ' '         'Other Charges',

         <fs_soa>-stm           0       ' '         'Special Transitory Mechanism',
         <fs_soa>-pia           0       ' '         'Payment Incentive Adjustment',

         <fs_soa>-subtot_amt    0       ' '         'Sub-Total Amount',
         <fs_soa>-guar_deposit  0       ' '         'Guarantee Deposit'.
  ENDLOOP.
ENDFORM.                    " DISPLAY_DETAILS

*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS
*&---------------------------------------------------------------------*
*       Get Business Partner Address
*----------------------------------------------------------------------*
FORM get_address  USING    pv_gpart
                  CHANGING pv_address.

  DATA: lv_addrnumber LIKE but020-addrnumber.

  CLEAR: tmp_house_num1, tmp_street, tmp_street2, tmp_city2, tmp_city1.

  SELECT SINGLE addrnumber
    INTO lv_addrnumber
    FROM but020
   WHERE partner EQ pv_gpart.

  SELECT SINGLE house_num1 street str_suppl1 city2 city1
    INTO (tmp_house_num1, tmp_street, tmp_street2, tmp_city2, tmp_city1)
    FROM adrc
   WHERE addrnumber EQ lv_addrnumber.

  CONCATENATE tmp_house_num1 tmp_street tmp_street2 tmp_city2 tmp_city1
         INTO pv_address
    SEPARATED BY space.
ENDFORM.                    " GET_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_INSTAL_CHARGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_charges  USING    pv_vkont         TYPE vkont_kk
                  CHANGING pv_instal_due    TYPE betrw_kk
                           pv_inst_charge   TYPE betrw_kk
                           pv_reopen_charge TYPE betrw_kk
                           pv_mtr_charge    TYPE betrw_kk
                           pv_other_charge  TYPE betrw_kk
                           pv_guar_deposit  TYPE betrw_kk.

  DATA: lv_bldat LIKE dfkkop-bldat.

  TYPES: BEGIN OF ty_docno,
          docno     TYPE opbel_kk,
         END OF ty_docno.

  DATA: lt_docno    TYPE TABLE OF ty_docno,
        lt_sfkkop   TYPE TABLE OF sfkkop,
        lt_fkkop    TYPE TABLE OF fkkop,
        lv_clr_amt  TYPE betrh_kk,

        lv_start_bill TYPE sydatum,
        lv_end_bill   TYPE sydatum.

  FIELD-SYMBOLS:
        <fs_docno>  TYPE ty_docno,
        <fs_sfkkop> TYPE sfkkop,
        <fs_fkkop>  TYPE fkkop.

  "-- Reinitialize Local Tables
  REFRESH: it_dfkkop,
           lt_sfkkop.

  "-- Reinitialize Variables
  CLEAR:   pv_instal_due, pv_inst_charge, pv_reopen_charge, pv_mtr_charge, pv_other_charge, pv_guar_deposit.

  lv_start_bill = <fs_soa>-prevrdgdt + 1. "-- Added 1 day - RANDYSY 08/27/2013 TN#41237
  lv_end_bill   = <fs_soa>-presrdgdt.

  "-- Get Miscellaneous Charges
  SELECT betrw augbl hvorg bldat
  INTO   TABLE it_dfkkop
  FROM   dfkkop
  WHERE  vkont EQ pv_vkont
*  AND    budat BETWEEN <fs_soa>-prevrdgdt AND <fs_soa>-presrdgdt
  AND    budat BETWEEN lv_start_bill AND lv_end_bill
  AND    ( ( hvorg EQ '9000' ) OR  "-- Installation Charges
           ( hvorg EQ '7003' ) OR  "-- Re-opening Charges
           ( hvorg EQ '7002' ) OR  "-- Meter Charges
           ( hvorg EQ '7007' ) OR  "-- Other Charges
           ( hvorg EQ '6045' ) )   "-- Guarantee Deposit
  AND    augrd NE '05'
  AND    abwtp NE 'R'. "-- Added by RANDYSY 07/13/2013 - TN#39659

  LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
    IF <fs_dfkkop>-hvorg EQ '7003'.
      lv_bldat = <fs_dfkkop>-bldat.
    ELSE.
      IF <fs_dfkkop>-augbl NE space.
        SELECT SINGLE bldat
        INTO   lv_bldat
        FROM   dfkkop
        WHERE  vkont EQ pv_vkont
        AND    opbel EQ <fs_dfkkop>-augbl.
      ENDIF.
    ENDIF.

    IF lv_bldat EQ '00000000'.
      lv_bldat = '99999999'.
    ENDIF.

    IF <fs_dfkkop>-augbl IS NOT INITIAL.
      "-- Refresh Internal Table for Cleared Items
      REFRESH: lt_fkkop.

      CLEAR: lv_clr_amt.

      "-- Get Cleared Item Amount for the selected month
      CALL FUNCTION 'FKK_GET_CLEARED_ITEMS'
        EXPORTING
          i_augbl = <fs_dfkkop>-augbl
        TABLES
          t_fkkop = lt_fkkop.

      LOOP AT lt_fkkop ASSIGNING <fs_fkkop> WHERE faedn GE <fs_soa>-prevrdgdt  "tmp_firstdayofmonth
                                              AND faedn LE <fs_soa>-presrdgdt  "tmp_lastdayofmonth.
                                              AND hvorg EQ <fs_dfkkop>-hvorg.

        IF <fs_dfkkop>-bldat GE <fs_fkkop>-bldat. "<fs_fkkop>-augdt.
          lv_clr_amt = <fs_fkkop>-augbt.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CASE <fs_dfkkop>-hvorg.
      WHEN '9000'. "-- Get Installation Charges
        pv_inst_charge   = pv_inst_charge   + <fs_dfkkop>-betrw - lv_clr_amt.
      WHEN '7003'. "-- Get Re-opening Charges
        pv_reopen_charge = pv_reopen_charge + <fs_dfkkop>-betrw - lv_clr_amt.
      WHEN '7002'. "-- Get Meter Charges
        pv_mtr_charge    = pv_mtr_charge    + <fs_dfkkop>-betrw - lv_clr_amt.
      WHEN '7007'. "-- Get Other Charges
        pv_other_charge  = pv_other_charge  + <fs_dfkkop>-betrw - lv_clr_amt.
      WHEN '6045'. "-- Get Guarantee Deposit
        pv_guar_deposit  = pv_guar_deposit  + <fs_dfkkop>-betrw - lv_clr_amt.
    ENDCASE.

    CLEAR: lv_clr_amt.
  ENDLOOP.

  "-- Get Billing Period for Installment Plan
  PERFORM get_instaldue_bill_period USING    <fs_soa>-ableinh
                                             pv_vkont
*                                             <fs_soa>-prevrdgdt
*                                             <fs_soa>-presrdgdt
                                             tmp_lastdayofmonth
                                    CHANGING lv_start_bill
                                             lv_end_bill.

  "-- Reintialize Local Table for Installment Dues Doc. No.
  REFRESH: lt_docno.

  "-- Get All Selected Installment Plan
*  SELECT a~opbel
*    INTO TABLE lt_docno
*  FROM   dfkkop AS a INNER JOIN dfkkopw AS b ON  ( a~opbel EQ b~opbel )
*                                             AND ( a~vkont EQ b~vkont )
*  WHERE  a~vkont EQ pv_vkont
*  AND    a~hvorg EQ '0080'
*  AND    b~faedn BETWEEN lv_start_bill AND lv_end_bill
*  AND    b~augob EQ space.

  SELECT opbel
    INTO TABLE lt_docno
  FROM   dfkkop
  WHERE  vkont EQ pv_vkont
  AND    hvorg EQ '0080'
  AND    augrd NE '05'
  AND    augst EQ space
  AND    abwbl EQ space.
*  AND    faedn BETWEEN lv_start_bill AND lv_end_bill.

  IF sy-subrc EQ 0.
    "-- Sort then Remove Duplicate Doc. No.
    SORT lt_docno BY docno.
    DELETE ADJACENT DUPLICATES FROM lt_docno.

    LOOP AT lt_docno ASSIGNING <fs_docno>.
      "-- Get Installation Plan Details
      CALL FUNCTION 'FKK_S_INSTPLAN_PROVIDE'
        EXPORTING
          i_opbel        = <fs_docno>-docno
          i_for_update   = space
        TABLES
          raten_fkkop    = lt_sfkkop
        EXCEPTIONS
          already_locked = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        "-- Get Installation Plan Amount for Bill Month
        LOOP AT lt_sfkkop ASSIGNING <fs_sfkkop> WHERE faedn GE lv_start_bill "<fs_soa>-prevrdgdt  "tmp_firstdayofmonth
                                                AND   faedn LE lv_end_bill   "<fs_soa>-presrdgdt. "tmp_lastdayofmonth.
                                                AND   augob EQ space.

          IF <fs_sfkkop>-faedn LE sy-datum.
            IF <fs_sfkkop>-augbl IS NOT INITIAL AND <fs_sfkkop>-augbl NE '*'.
              "-- Refresh Internal Table for Cleared Items
              REFRESH: lt_fkkop.

              "-- Get Cleared Item Amount for the selected month
              CALL FUNCTION 'FKK_GET_CLEARED_ITEMS'
                EXPORTING
                  i_augbl = <fs_sfkkop>-augbl
                TABLES
                  t_fkkop = lt_fkkop.

              pv_instal_due = <fs_sfkkop>-betrh.

              LOOP AT lt_fkkop ASSIGNING <fs_fkkop> WHERE faedn GE lv_start_bill  "<fs_soa>-prevrdgdt  "tmp_firstdayofmonth
                                                     AND  faedn LE lv_end_bill    "<fs_soa>-presrdgdt  "tmp_lastdayofmonth
                                                     AND  hvorg EQ '0080'.

                IF <fs_soa>-reading_date GE <fs_fkkop>-augdt.
                  pv_instal_due = pv_instal_due - <fs_fkkop>-augbt.

*                  pv_instal_due = pv_instal_due + <fs_sfkkop>-betrh - <fs_fkkop>-augbt.
*                ELSE.
*                  pv_instal_due = pv_instal_due + <fs_sfkkop>-betrh.
                ENDIF.
              ENDLOOP.
            ELSE.
              CLEAR: pv_instal_due.

              "-- Deduct Cleared Amount
              <fs_sfkkop>-betrh = <fs_sfkkop>-betrh - <fs_sfkkop>-augbt.
              pv_instal_due     = pv_instal_due + <fs_sfkkop>-betrh.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_INSTAL_CHARGES

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM select_file_path  USING    pa_path LIKE rlgrap-filename.
  DATA :
    lv_subrc  LIKE sy-subrc,
    lt_it_tab TYPE string.

  "-- Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Select Destination File'
      initial_folder  = 'C:\'
    CHANGING
      selected_folder = lt_it_tab.

  "-- Write path on input area
  IF lt_it_tab IS NOT INITIAL.
    CONCATENATE lt_it_tab `\` INTO pa_path.
  ENDIF.
ENDFORM.                    " SELECT_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETERS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_parameters CHANGING sy_subrc.
  DEFINE m_show_error.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline = sy-title
        text1    = text-005
        text2    = text-004
        ticon    = 'E'
        button_1 = 'Continue'.
  END-OF-DEFINITION.

  sy_subrc = 0.

  IF rb_save EQ 'X'.
    IF spl_file IS INITIAL.
      sy_subrc = 4.
      m_show_error. EXIT.
    ELSE.
      lv_len   = strlen( spl_file ).
      lv_start = lv_len - 4.
      lv_end   = 4.

      TRY.
          IF spl_file+lv_start(lv_end) NE '.pdf'.
            sy_subrc = 4.
            m_show_error. EXIT.
          ENDIF.
        CATCH cx_sy_range_out_of_bounds.
          sy_subrc = 4.
          m_show_error. EXIT.
      ENDTRY.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_PARAMETERS

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM download_pdf .
  wa_ctrlop-getotf    = 'X'.
  wa_ctrlop-no_dialog = 'X'.
  wa_outopt-tdnoprev  = 'X'.

  CALL FUNCTION form_name
    EXPORTING
      tabdetails         = it_pdf
      control_parameters = wa_ctrlop
      output_options     = wa_outopt
      user_settings      = 'X'
      im_disctag         = <fs_soa>-disc_txt      "TN#30334 Inserted by Ann 20130325
      im_duedate         = <fs_soa>-faedn         "TN#30334 Inserted by Ann 20130325
      im_aveconsump      = <fs_soa>-ave_consump   "TN#30334 Inserted by Ann 20130325
      im_consump         = <fs_soa>-consump       "TN#30334 Inserted by Ann 20130325
    IMPORTING
      job_output_info    = t_otfdata
    TABLES
      it_pmthist         = it_pmthist
      it_billsum         = it_billsum
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  t_otf[] = t_otfdata-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = w_bin_filesize
    TABLES
      otf                   = t_otf
      lines                 = t_pdf_tab
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  full_path = spl_file.

  "-- Download the file to the selected path
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = w_bin_filesize
      filename                = full_path "spl_file
      filetype                = 'BIN'
    IMPORTING
      filelength              = w_filesize
    TABLES
      data_tab                = t_pdf_tab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc NE 0.
    MESSAGE 'File not downloaded' TYPE 'I'.
  ELSE.
    MESSAGE 'File downloaded' TYPE 'I'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_PDF

*&---------------------------------------------------------------------*
*&      Form  PREVIEW_PDF
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM preview_pdf .
  DATA: i_ctrl  TYPE ssfctrlop.

  "-- Parameters for Direct Print Preview
  i_ctrl-preview   = 'X'.
  i_ctrl-no_dialog = 'X'.

  CALL FUNCTION form_name
    EXPORTING
      tabdetails         = it_pdf
      control_parameters = i_ctrl
      user_settings      = ' '
      im_disctag         = <fs_soa>-disc_txt      "TN#30334 Inserted by Ann 20130325
      im_duedate         = <fs_soa>-faedn         "TN#30334 Inserted by Ann 20130325
      im_aveconsump      = <fs_soa>-ave_consump   "TN#30334 Inserted by Ann 20130325
      im_consump         = <fs_soa>-consump       "TN#30334 Inserted by Ann 20130325
    IMPORTING
      job_output_info    = t_otfdata
    TABLES
      it_pmthist         = it_pmthist
      it_billsum         = it_billsum
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " PREVIEW_PDF

*&---------------------------------------------------------------------*
*&      Form  get_pua_from_download
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_pua_from_download  USING    pv_vkont        LIKE fkkvkp-vkont
                                     pv_reading_date LIKE eabl-adattats
                            CHANGING pv_pua          LIKE zmwosb_s01p-total.
  SELECT SINGLE pua
    INTO pv_pua
  FROM   zosb_pua
  WHERE  can EQ pv_vkont
  AND    reading_date BETWEEN tmp_firstdayofmonth AND tmp_lastdayofmonth.

  IF sy-subrc NE 0.
    CLEAR pv_pua.
  ENDIF.
ENDFORM.                    " get_pua_from_download

*&---------------------------------------------------------------------*
*&      Form  get_instal_amt
*&---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM get_instal_amt USING    lv_vkont     LIKE dfkkop-vkont
                             lv_faedn     LIKE dfkkop-faedn
                             start_billdt LIKE eabl-erdat
                             end_billdt   LIKE eabl-erdat
                    CHANGING store_inst   LIKE dfkkop-betrw.

  TYPES: BEGIN OF ty_instal,
          amount    TYPE dfkkop-betrw,
          date      TYPE dfkkop-faedn,
          hvorg     TYPE dfkkop-hvorg,
          tvorg     TYPE dfkkop-tvorg,
          opbel     TYPE dfkkop-opbel,
          faedn     TYPE dfkkop-faedn,
          opupk     TYPE dfkkop-opupk,
          opupw     TYPE dfkkop-opupw,
          augob     TYPE dfkkop-augob,
          augbl     TYPE dfkkop-augbl,
          blart     TYPE dfkkop-blart,
          augdt     TYPE dfkkop-augdt,
         END OF ty_instal.

  DATA: lt_instal   TYPE TABLE OF ty_instal.

  FIELD-SYMBOLS:
        <fs_dfkkop> TYPE ty_instal.

  SELECT betrw AS amount
         faedn AS date
         hvorg
         tvorg
         opbel
         faedn
         opupk
         opupw
         augob
         augbl
         blart
         augdt
    INTO TABLE lt_instal "CORRESPONDING FIELDS OF TABLE it_dfkkop
  FROM dfkkop
  WHERE vkont = lv_vkont
  AND ( augst = space OR augdt GE end_billdt )
  AND abwbl = space
  AND ( budat NE '00000000' )
  AND faedn   LT end_billdt
  AND abrzu   LT start_billdt
  AND NOT ( dfkkop~hvorg = '0600' AND dfkkop~tvorg = '0010' AND dfkkop~stakz = 'Q' )
  AND NOT ( ( hvorg = '6040' OR hvorg = '6050' ) AND ( tvorg = '0020' OR tvorg = '0040' ) )
  AND augob EQ space.

  LOOP AT lt_instal ASSIGNING <fs_dfkkop> WHERE augdt LT lv_faedn.
    IF ( <fs_dfkkop>-hvorg = '7008' AND <fs_dfkkop>-tvorg = '0020' ).  "Othr A/R Advncs for Cust De
      IF <fs_dfkkop>-faedn LT end_billdt.
        store_inst = store_inst + <fs_dfkkop>-amount.
      ENDIF.
    ENDIF.
    CLEAR <fs_dfkkop>-amount.
  ENDLOOP.
ENDFORM.                    " get_instal_amt

*&---------------------------------------------------------------------*
*&      Form  GET_STORE_BAL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_store_bal TABLES   it_dfkkop    TYPE STANDARD TABLE
                   USING    lv_vkont     LIKE dfkkop-vkont
                            start_billdt LIKE eabl-erdat
                            end_billdt   LIKE eabl-erdat
                            lowest_date  LIKE sy-datum
                            tot_cur_chrg LIKE zmwosb_s01p-total
                            store_inst   LIKE dfkkop-betrw
                   CHANGING store_bal    LIKE dfkkop-betrw
                            tmpother     LIKE dfkkop-betrw
                            otax         LIKE dfkkop-betrw
                            other        LIKE dfkkop-betrw
                            othertax     LIKE dfkkop-betrw.
  FIELD-SYMBOLS:
         <fs_dfkkop> TYPE ty_dfkkop_soa.

  REFRESH it_dfkkop.

  SELECT betrw AS amount
         faedn AS date
         hvorg
         tvorg
         opbel
         faedn
         opupk
         opupw
         augob
         augbl
         blart
         augdt
    INTO CORRESPONDING FIELDS OF TABLE it_dfkkop
  FROM   dfkkop
  WHERE  vkont   EQ lv_vkont
  AND    ( augst EQ space OR augdt GE end_billdt )
  AND    abwbl   EQ space
  AND    ( budat NE '00000000' )
  AND    faedn   LT end_billdt
  AND    abrzu   LT start_billdt
  AND    NOT ( dfkkop~hvorg EQ '0600' AND dfkkop~tvorg EQ '0010' AND dfkkop~stakz EQ 'Q' )
  AND    NOT ( ( hvorg EQ '6040' OR hvorg EQ '6050' ) AND ( tvorg EQ '0020' OR tvorg EQ '0040' ) )
  AND    augob EQ space
  ORDER BY opbel.

  LOOP AT it_dfkkop ASSIGNING <fs_dfkkop>.
    IF ( lowest_date = '00000000' OR lowest_date > <fs_dfkkop>-date ).
      lowest_date = <fs_dfkkop>-date.
    ENDIF.

    IF   ( <fs_dfkkop>-hvorg = '0040' AND <fs_dfkkop>-tvorg = '0020' ).
      SELECT SUM( ibtrg )
      INTO   (tmpother)
      FROM   dfkkia
      WHERE  iopbel EQ <fs_dfkkop>-opbel
      AND    round  EQ space.

      SELECT SUM( ibtrg )
      INTO   (otax)
      FROM   dfkkia
      WHERE  iopbel = <fs_dfkkop>-opbel
      AND    round  = 'X'.

      other     = other    + tmpother.
      othertax  = othertax + otax.

    ELSEIF ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0920' ) OR  "MIGDATA
           ( <fs_dfkkop>-hvorg = '0200' AND <fs_dfkkop>-tvorg = '0020' ) OR  "FINAL BILLING
           ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0020' ) OR  "MANUAL BACKBILLING
           ( <fs_dfkkop>-hvorg = '0100' AND <fs_dfkkop>-tvorg = '0020' ) OR  "BILLS
           ( <fs_dfkkop>-hvorg = '0100' AND <fs_dfkkop>-tvorg = '0010' ) OR  "BILLS ADDITIONAL
           ( <fs_dfkkop>-hvorg = '0060' AND <fs_dfkkop>-tvorg = '0010' ) OR  "Overpayment
           ( <fs_dfkkop>-hvorg = '0600' AND <fs_dfkkop>-tvorg = '0010' ) OR  "Open Payments
           ( <fs_dfkkop>-hvorg = '9950' AND <fs_dfkkop>-tvorg = '0020' ) OR
           ( <fs_dfkkop>-hvorg = '0300' AND <fs_dfkkop>-tvorg = '0940' ) OR
           ( <fs_dfkkop>-hvorg = '0080' AND <fs_dfkkop>-tvorg = '0010' ) OR  "Installment receivable Dr
           ( <fs_dfkkop>-hvorg = '7003' AND <fs_dfkkop>-tvorg = '0020' ) OR  "A/R Reopening Fee Dr - Water
           ( <fs_dfkkop>-hvorg = '6005' AND (
             <fs_dfkkop>-tvorg BETWEEN '0020' AND '0027' ) ) OR
           ( <fs_dfkkop>-hvorg = '0300' AND (
             <fs_dfkkop>-tvorg BETWEEN '0510' AND '0528' ) ).       "CREDIT

      IF <fs_dfkkop>-faedn LT end_billdt.
        store_bal = store_bal + <fs_dfkkop>-amount.
      ENDIF.
    ENDIF.
    CLEAR <fs_dfkkop>-amount.
  ENDLOOP.

  IF store_inst IS NOT INITIAL.
    store_bal = ( store_bal + store_inst ) - tot_cur_chrg.
  ENDIF.
ENDFORM.                    " GET_STORE_BAL

*&---------------------------------------------------------------------*
*&      Form  POPULATE_S_MONTHS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM populate_s_months .
  DATA: it_list   TYPE vrm_values,
        lv_datum  LIKE sy-datum,
        lv_datum2 TYPE c LENGTH 7.

  FIELD-SYMBOLS:
        <fs_list> TYPE vrm_value.

  DEFINE m_append_list.
    concatenate &1+4(2) `/` &1+0(4) into lv_datum2.

    append initial line to it_list assigning <fs_list>.
    <fs_list>-key = lv_datum2.
  END-OF-DEFINITION.

  lv_datum = sy-datum.

  m_append_list: lv_datum.

  DO 11 TIMES.
    CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = -1
        olddate = lv_datum
      IMPORTING
        newdate = lv_datum.

    m_append_list: lv_datum.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'S_MONTHS'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " POPULATE_S_MONTHS

*&---------------------------------------------------------------------*
*&      Form  GET_BILL_REFERENCE_NUMBER
*&---------------------------------------------------------------------*
*       Subroutine for Getting Reference Number of Bill Month
*----------------------------------------------------------------------*
FORM get_bill_reference_number  USING    pv_vkont       TYPE vkont_kk
                                         pv_lowest_date TYPE sydatum
                                CHANGING pv_refno       TYPE xblnr_kk
                                         pv_due_date    TYPE faedn_kk.

  "-- Local Variable Declarations
  DATA:    lt_refno        TYPE TABLE OF ty_refno,
           lv_next_mr_date TYPE sydatum.

  "-- Clear Variables
  CLEAR:   pv_refno, lv_next_mr_date.

  "-- Refresh Itab for Reference Nos.
  REFRESH: lt_refno.

  "-- Get Next Scheduled Meter Reading
  SELECT SINGLE adatsoll
    INTO lv_next_mr_date
  FROM   eablg
  WHERE  anlage   EQ <fs_soa>-anlage
  AND    ablesgr  NE '13'
  AND    adatsoll GT <fs_soa>-reading_date.

  IF sy-subrc NE 0.
    lv_next_mr_date = sy-datum.
  ENDIF.

  "-- Select Reference No. and Due Date
  IF pv_lowest_date GT lv_next_mr_date.
    SELECT xblnr faedn INTO TABLE lt_refno
    FROM   dfkkop
    WHERE  vkont EQ pv_vkont
    AND    budat BETWEEN  lv_next_mr_date AND pv_lowest_date.
  ELSE.
    SELECT xblnr faedn INTO TABLE lt_refno
    FROM   dfkkop
    WHERE  vkont EQ pv_vkont
    AND    budat BETWEEN pv_lowest_date AND lv_next_mr_date.
  ENDIF.

  IF lt_refno[] IS NOT INITIAL.
    DELETE lt_refno WHERE: refno    IS INITIAL,
                           due_date IS INITIAL.

    "-- Sort by Due Date to Get the Latest Billing Ref. No.
    SORT lt_refno BY due_date DESCENDING.

    "-- Get Latest Reference No. from Itab
    READ TABLE lt_refno ASSIGNING <fs_refno> INDEX 1.
    IF sy-subrc EQ 0.
      pv_refno    = <fs_refno>-refno.
      pv_due_date = <fs_refno>-due_date.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_BILL_REFERENCE_NUMBER

*&---------------------------------------------------------------------*
*&      Form  GET_PUA_FROM_FM
*&---------------------------------------------------------------------*
*       Subroutine for Getting PUA Value for ISU-Invoicing
*----------------------------------------------------------------------*
FORM get_pua_from_fm  USING    pv_vkont    TYPE vkont_kk
                               pv_budat    TYPE char08
                               pv_opbel    TYPE opbel_kk
                      CHANGING pv_pua      TYPE betrh_kk
                               pv_due_date TYPE faedn_kk.

  DATA:  lv_based_date TYPE sydatum,
         lv_end_time   TYPE coitm_kk.

  CONSTANTS:
         lv_less_sec   TYPE coitm_kk VALUE `000001`.

  "-- Clear Variables
  CLEAR: pv_pua, lv_based_date, lv_end_time, pv_due_date.

  "-- Set Based Line Date
  lv_based_date = pv_budat.

  "-- Get Time End
  SELECT SINGLE cputm
    INTO lv_end_time
  FROM   dfkkko
  WHERE  opbel EQ pv_opbel.

  IF sy-subrc EQ 0.
    "-- Deduct A Second to Get Previous Value of PUA
    lv_end_time = lv_end_time - lv_less_sec.

    "-- Call FM for Getting PUA (Single Billing/Invoicing Reimplementation)
    CALL FUNCTION 'ZMWOSB_GET_PUA'
      EXPORTING
        i_vkont           = pv_vkont
        i_based_line_date = lv_based_date "end_billdt "lowest_date
        i_end_time        = lv_end_time
      IMPORTING
        e_pua             = pv_pua
        e_due_date        = pv_due_date.
  ENDIF.
ENDFORM.                    " GET_PUA_FROM_FM

*&---------------------------------------------------------------------*
*&      Form  GET_READING_DETAILS
*&---------------------------------------------------------------------*
*       Subroutine for getting Meter Reading Details
*----------------------------------------------------------------------*
FORM get_reading_details  USING    pv_vkont      TYPE vkont_kk
                                   pv_firstday   TYPE sydatum
                                   pv_lastday    TYPE sydatum
                          CHANGING pv_subrc      TYPE sysubrc
                                   pv_anlage     TYPE anlage
                                   pv_ablbelnr   TYPE ablbelnr
                                   pv_ableinh    TYPE ableinheit
                                   pv_adatsoll   TYPE adatsoll
                                   pv_adattats   TYPE adattats
                                   pv_equnr      TYPE equnr
                                   pv_erdat      TYPE erdat
                                   pv_istablart  TYPE istablart
                                   pv_stanzvor   TYPE	stanzvore
                                   pv_stanznac   TYPE stanznace
                                   pv_zuorddat   TYPE e_zuorddat
                                   pv_atim       TYPE atim
                                   pv_zwnummer   TYPE e_zwnummer
                                   pv_mrreason   TYPE ablesgr.

  DATA: lt_eabl        TYPE TABLE OF bapieabl,
        lt_eablg       TYPE TABLE OF bapieablg.

  DATA: lv_holiday     TYPE sysubrc,
        lv_weekend     TYPE sysubrc,
        lv_calendar_id TYPE hident,
        lv_mru         TYPE ableinheit.

  FIELD-SYMBOLS:
        <fs_eabl>      TYPE bapieabl,
        <fs_eablg>     TYPE bapieablg.

  CONSTANTS:
        co_mrtype      TYPE mrdocumenttype VALUE '2',
        co_mrreason    TYPE ablesgr        VALUE '01'.

  "-- Reinitialize Variables & Local Tables
  CLEAR: pv_subrc,
         pv_anlage,
         pv_ablbelnr,
         pv_ableinh,
         pv_adatsoll,
         pv_adattats,
         pv_equnr,
         pv_erdat,
         pv_istablart.

  REFRESH: lt_eabl, lt_eablg.

  pv_subrc = 4.

  "-- Get Installation Number
  SELECT SINGLE anlage
    INTO pv_anlage
  FROM   ever
  WHERE  vkonto EQ pv_vkont
  AND    anlage NE space
  AND    loevm  EQ space.

  IF sy-subrc EQ 0 AND pv_anlage IS NOT INITIAL.
    CLEAR: lv_calendar_id, lv_holiday, lv_weekend, lv_mru.

    SELECT ableinh
    INTO lv_mru
    FROM eablg
    UP TO 1 ROWS
    WHERE anlage EQ pv_anlage
    ORDER BY adatsoll DESCENDING.
    ENDSELECT.

    IF sy-subrc EQ 0.
      "-- Check if MRU is valid & Get Calendar ID
      PERFORM get_calendar_id USING    lv_mru
                              CHANGING lv_calendar_id.
      "-- Check if Holiday
      PERFORM check_holiday   USING    pv_firstday
                                       lv_calendar_id
                              CHANGING lv_holiday.
      "-- Check if Weekend
      PERFORM check_weekend   USING    pv_firstday
                              CHANGING lv_weekend.

      WHILE lv_holiday EQ 0 OR lv_weekend EQ 0.
        SUBTRACT 1 FROM pv_firstday.
        "-- Re-check if Holiday
        PERFORM check_holiday   USING    pv_firstday
                                         lv_calendar_id
                                CHANGING lv_holiday.
        "-- Re-check if Weekend
        PERFORM check_weekend   USING    pv_firstday
                                CHANGING lv_weekend.
      ENDWHILE.
    ENDIF.

    "-- Get Meter Reading Schedules Data
    CALL FUNCTION 'BAPI_MTRREADDOC_GETLIST'
      EXPORTING
        installation      = pv_anlage
        mrdatefrom        = pv_firstday
        mrdateto          = pv_lastday
        mrdocumenttype    = co_mrtype
      TABLES
        mrdocumentdata    = lt_eabl
        mrdocumentreasons = lt_eablg.

    IF lt_eabl[] IS NOT INITIAL.
      "-- Get Periodic Meter Reading Only '01'
      READ TABLE lt_eablg ASSIGNING <fs_eablg> WITH KEY mrreason = co_mrreason.
      IF sy-subrc EQ 0.
        "-- Get Meter Reading Schedule Data
        READ TABLE lt_eabl ASSIGNING <fs_eabl> WITH KEY mridnumber = <fs_eablg>-mridnumber.
        IF sy-subrc EQ 0.
          pv_subrc     = sy-subrc.
          pv_ablbelnr  = <fs_eabl>-mridnumber.
          pv_ableinh   = <fs_eablg>-meterreadingunit.
          pv_adatsoll  = <fs_eabl>-targetmrdate.
          pv_adattats  = <fs_eabl>-actualmrdate.
          pv_equnr     = <fs_eabl>-equipment.
          pv_erdat     = <fs_eabl>-created_on.
          pv_istablart = <fs_eabl>-actualcustomermrtype.
          pv_stanzvor  = <fs_eabl>-predecimalplaces.
          pv_stanznac  = <fs_eabl>-decimalplaces.
          pv_zuorddat  = <fs_eabl>-allocationdate.
          pv_atim      = <fs_eabl>-mrtimeforbilling.
          pv_zwnummer  = <fs_eabl>-register.
          pv_mrreason  = <fs_eablg>-mrreason.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_READING_DETAILS

*&---------------------------------------------------------------------*
*&      Form  TAG_DISHONORED_CHECK
*&---------------------------------------------------------------------*
*       Tagging for Dishonored Check
*----------------------------------------------------------------------*
FORM tag_dishonored_check  USING    pv_can          TYPE vkont_kk
                                    pv_actual_mr    TYPE adattats
                           CHANGING pv_cash_payment TYPE char17.

  DATA: ls_dishonoredcheck TYPE zdishonoredcheck.

  "-- Reinitialize Tag
  CLEAR: pv_cash_payment.

  "-- Check CAN if included in Dishonored Checks
  SELECT SINGLE *
    INTO ls_dishonoredcheck
  FROM   zdishonoredcheck
  WHERE  vkont EQ pv_can
  AND    budat LT pv_actual_mr.

  IF sy-subrc EQ 0.
    "-- Tag - CASH PAYMENT ONLY
    pv_cash_payment = text-006.
  ENDIF.
ENDFORM.                    " TAG_DISHONORED_CHECK

*&---------------------------------------------------------------------*
*&      Form  ZBILLDET_ROUTINE
*&---------------------------------------------------------------------*
*       Get Billing Details
*----------------------------------------------------------------------*
FORM zbilldet_routine  USING    pv_refno     TYPE xblnr
                                pv_osb_refno TYPE xblnr
                       CHANGING pv_belnr     TYPE belnr_kk
                                pv_subrc     TYPE sysubrc.

  DATA: gt_bdcdata    TYPE tab_bdcdata,
        lv_msc        TYPE nettobtr,
        lv_scd        TYPE nettobtr.

  FIELD-SYMBOLS:
        <fs_bdc>       TYPE bdcdata,
        <fs_messages>  TYPE bdcmsgcoll.

  CONSTANTS:
        co_tcode       TYPE sytcode VALUE 'ZBILLDET',
        co_mode        TYPE char01  VALUE 'N'.

  IF gv_archived eq 'X'.
    "-- Macro definition for fetching SCD and MSC
    DEFINE mc_get_dberch_data.
      select sum( dberchz3~nettobtr )
      into   (&3)
      from   dberchz3 inner join dberchz1 on  dberchz3~belnr    eq dberchz1~belnr
                                          and dberchz3~belzeile eq dberchz1~belzeile
      where  dberchz3~belnr    eq &1
      and    dberchz3~mwskz    ne space
      and    dberchz1~belzart  eq &2.

      if sy-subrc ne 0.
        clear: &3.
      endif.
    END-OF-DEFINITION.
  ELSE.
*    "-- Macro definition for fetching SCD and MSC (FROM ARCHIVE)
*    DEFINE mc_get_dberch_data.
*      select sum( dberchz3~nettobtr )
*      into   (&3)
*      from   dberchz3 inner join dberchz1 on  dberchz3~belnr    eq dberchz1~belnr
*                                          and dberchz3~belzeile eq dberchz1~belzeile
*      where  dberchz3~belnr    eq &1
*      and    dberchz3~mwskz    ne space
*      and    dberchz1~belzart  eq &2.
*
*      if sy-subrc ne 0.
*        clear: &3.
*      endif.
*    END-OF-DEFINITION.
  ENDIF.

  "--Macro definition for BDC creation
  DEFINE mc_bdc_dynpro.
    append initial line to gt_bdcdata assigning <fs_bdc>.
    if &1 ne space.
      <fs_bdc>-program  = &2.
      <fs_bdc>-dynpro   = &3.
      <fs_bdc>-dynbegin = &1.
    else.
      <fs_bdc>-fnam     = &2.
      <fs_bdc>-fval     = &3.
    endif.
  END-OF-DEFINITION.

  pv_subrc = 4.

  CHECK pv_belnr IS NOT INITIAL.

  "-- Reinitialize Variables
  CLEAR: pv_subrc, wa_zbilldet.

  IF pv_refno NE pv_osb_refno.
    "-- Format Document Number - Add Zero Paddings
    mc_conversion_alpha pv_belnr.

    "-- Reinitialize BDC Table
    REFRESH: gt_bdcdata.

    "-- Pass BDC screens, fields and value
    mc_bdc_dynpro:  'X'   'ZPRG_BILLDET_V2' '1000',
                    space 'BDC_CURSOR'      'BILLDT-LOW',
                    space 'BDC_OKCODE'      '=ONLI',
                    space 'BELNR-LOW'       pv_belnr,
                    space 'BILLDT-LOW'      '',
                    'X'   'SAPMSSY0'        '0120',
                    space 'BDC_OKCODE'      '=BACK',
                    'X'   'ZPRG_BILLDET_V2' '1000',
                    space 'BDC_OKCODE'      '/EE'.

    "-- Perform Transaction ZBILLDET
    CALL TRANSACTION co_tcode USING gt_bdcdata
                              MODE  co_mode.
  ENDIF.

  "-- Get Billing Details from Table ZBILLDET
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_zbilldet
  FROM   zbilldet
  WHERE  belnr EQ pv_belnr.

  mc_get_dberch_data: pv_belnr 'ZMSC'   lv_msc, "-- Get MSC
                      pv_belnr 'ZDISSC' lv_scd. "-- Get DISSC

  wa_zbilldet-msc   = lv_msc.
  wa_zbilldet-dissc = lv_scd.

  "-- Return Result
  pv_subrc = sy-subrc.
ENDFORM.                    " ZBILLDET_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_TAX_CODE
*&---------------------------------------------------------------------*
*       Subroutine for Indentifying Tax Code for Payment
*----------------------------------------------------------------------*
FORM determine_tax_code  USING    pv_can       TYPE vkont_kk
                                  pv_instal_no TYPE anlage
                                  pv_desc      TYPE char04
                         CHANGING pv_tax_code  TYPE char20.

  DATA: r_grpflag      TYPE RANGE OF char04,
        ls_grpflat     LIKE LINE  OF r_grpflag,
        lv_acct_det    TYPE e_kofiz,
        lv_grp_flag    TYPE anlart.

  "-- Macro Definition for Appending Range
  DEFINE mc_append_range.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  IF pv_desc EQ `GD`.
    pv_tax_code = `Non-VAT`.
    EXIT.
  ENDIF.

  "-- Appending Range
  mc_append_range: ls_grpflat r_grpflag `5006`,
                   ls_grpflat r_grpflag `5007`.

  "-- Reinitialize Variables
  CLEAR: pv_tax_code,
         lv_acct_det,
         lv_grp_flag.

  "-- Get Account Determination and Installation Type (Group Flag)
  SELECT SINGLE a~kofiz b~anlart
    INTO (lv_acct_det, lv_grp_flag)
  FROM   ever AS a INNER JOIN eanl AS b ON ( a~anlage EQ b~anlage )
  WHERE  a~anlage EQ pv_instal_no
  AND    a~vkonto EQ pv_can.

  IF sy-subrc EQ 0.
    "-- Determine Tax Code
    IF lv_acct_det NE `22`.
      IF lv_grp_flag NOT IN r_grpflag.
        pv_tax_code = `VATable Sales`.
      ELSEIF lv_grp_flag IN r_grpflag.
        pv_tax_code = `Zero-Rated Sales`.
      ENDIF.
    ELSEIF lv_acct_det EQ `22`.
      IF lv_grp_flag NOT IN r_grpflag.
        pv_tax_code = `VATable-Exempt Sales`.
      ELSEIF lv_grp_flag IN r_grpflag.
        pv_tax_code = `Zero-Rated Sales`.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DETERMINE_TAX_CODE

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_VAT
*&---------------------------------------------------------------------*
*       Compute VAT
*----------------------------------------------------------------------*
FORM compute_vat  USING    pv_can       TYPE vkont_kk
                           pv_instal_no TYPE anlage
                           pv_tccbt     TYPE p
                  CHANGING pv_vat       TYPE nettobtr.

  DATA: lv_acct_det TYPE e_kofiz.

  "-- Reinitialize Variables
  CLEAR: lv_acct_det, pv_vat.

  "-- Get Account Determination
  SELECT SINGLE kofiz
    INTO lv_acct_det
  FROM   ever
  WHERE  anlage EQ pv_instal_no
  AND    vkonto EQ pv_can.

  IF lv_acct_det NE `22`.
    pv_vat = pv_tccbt * `0.12`.
  ENDIF.
ENDFORM.                    " COMPUTE_VAT

*&---------------------------------------------------------------------*
*&      Form  GET_PREVIOUS_CONSUMPTIONS
*&---------------------------------------------------------------------*
*       Get Previous 3 Months Consumptions
*----------------------------------------------------------------------*
FORM get_previous_consumptions  USING    pv_can               TYPE vkont_kk
                                         pv_first_day         TYPE sydatum
                                CHANGING pv_prev_months       TYPE char80  "char45 TN#30334 Changed by Ann 20130325
                                         pv_prev_consumptions TYPE char80.

  DATA: lv_last_day     TYPE sydatum,
        lv_consumption2 TYPE consmpt,
        lv_consumption  TYPE consmpt,
        lv_prev_month   TYPE char03,
        lv_month_no     TYPE n LENGTH 2,
        lv_pos          TYPE i,     "TN#30334 Inserted by Ann 20130326
        lv_pos2         TYPE i.     "TN#30334 Inserted by Ann 20130326

  DATA: lt_month_name  TYPE TABLE OF t247 WITH HEADER LINE.

  CONSTANTS:
        co_spaces TYPE char12 VALUE `            `.

  "-- Reinitialize Changing Variables
  CLEAR: pv_prev_months,
         pv_prev_consumptions.

  "-- Get Previous Consumptions for Three(3) Months
  DO 3 TIMES.
    CLEAR:   lv_consumption, lv_prev_month, lv_month_no.
    REFRESH: lt_month_name.

    "-- Get Last Day of Previous Month
    lv_last_day = pv_first_day - 1.

    "-- Get First Day of Previous Month
    CONCATENATE lv_last_day+0(6) '01' INTO pv_first_day.

    "-- Get Consumption of Previous Month
    SELECT SINGLE consump
      INTO lv_consumption2
    FROM   zbilldet
    WHERE  vkont    EQ pv_can
    AND    adatsoll BETWEEN pv_first_day AND lv_last_day.

    IF sy-subrc EQ 0.
      WRITE: lv_consumption2 TO lv_consumption ROUND 0 DECIMALS 0.
      "--Transfer only the whole no. excluding the decimals
      lv_consumption = abs( lv_consumption2 ).  "TN#30334 Inserted by Ann 20130326
    ELSE.
      lv_consumption = 0.
    ENDIF.
    CONDENSE lv_consumption. "TN#30334 Inserted by Ann 20130326

    "-- Get Month Name
    lv_month_no = pv_first_day+4(2).

    CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
      EXPORTING
        langu = 'E'
        month = lv_month_no
      IMPORTING
        t247  = lt_month_name.

    lv_prev_month = lt_month_name-ltx.
    TRANSLATE lv_prev_month TO UPPER CASE.

    "-- Pass to Changing Parameters
    WRITE: lv_prev_month  TO pv_prev_months+lv_pos(12) RIGHT-JUSTIFIED,
           lv_consumption TO pv_prev_consumptions+lv_pos2(15) RIGHT-JUSTIFIED.
    lv_pos  = ( 24 * sy-index ).
    lv_pos2 = lv_pos.
  ENDDO.
ENDFORM.                    " GET_PREVIOUS_CONSUMPTIONS

*&---------------------------------------------------------------------*
*&      Form  POPULATE_ADVISORY
*&---------------------------------------------------------------------*
*       Populate Advisory Table
*----------------------------------------------------------------------*
FORM populate_advisory USING pv_curr_consump TYPE char15
                             pv_ave_consump  TYPE consmpt   "TN#30334 Inserted by Ann 20130325
                             pv_installation TYPE anlage
                             pv_first_day    TYPE sydatum
                             pv_reading_date TYPE adattats.

  DATA: lv_message     TYPE c LENGTH 150,
        lv_ave_consump TYPE consmpt,
        lv_tolerance   TYPE p LENGTH 10 DECIMALS 2,
        lv_abrupt_tag  TYPE i.

  "-- Reinitialize Variables
  CLEAR: lv_message, lv_ave_consump, lv_abrupt_tag.

  "-- Get Average Consumption
  PERFORM get_ave_consumption USING    pv_installation
                                       pv_reading_date
                              CHANGING lv_ave_consump.

  IF lv_ave_consump GT 0.
    "-- Compute Tolerance
    lv_tolerance = pv_curr_consump - lv_ave_consump.
    lv_tolerance = lv_tolerance / lv_ave_consump.
    lv_tolerance = lv_tolerance * 100.

    "-- Check Consumption Ceiling Table
    IF   lv_ave_consump   GE 1  AND lv_ave_consump LE 20.
      IF lv_tolerance GT 300. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 21 AND lv_ave_consump LE 40.
      IF lv_tolerance GT 250. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 41 AND  lv_ave_consump LE 60.
      IF lv_tolerance GT 200. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 61 AND lv_ave_consump LE 80.
      IF lv_tolerance GT 150. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 81 AND lv_ave_consump LE 100.
      IF lv_tolerance GT 100. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 101 AND lv_ave_consump LE 150.
      IF lv_tolerance GT 50. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 151 AND lv_ave_consump LE 200.
      IF lv_tolerance GT 40. lv_abrupt_tag = 1. ENDIF.
    ELSEIF lv_ave_consump GE 201 AND lv_ave_consump LE 999999.
      IF lv_tolerance GT 40. lv_abrupt_tag = 1. ENDIF.
    ENDIF.

    "-- Check if Adrupt Increase
    IF lv_abrupt_tag EQ 1.
      "-- Set Message Line
      pv_ave_consump = lv_ave_consump.  "TN#30334 Inserted by Ann 20130325
    ENDIF.
  ENDIF.
ENDFORM.                    " POPULATE_ADVISORY

*&---------------------------------------------------------------------*
*&      Form  GET_AVE_CONSUMPTION
*&---------------------------------------------------------------------*
*       Get Average Consumption for the past Months
*----------------------------------------------------------------------*
FORM get_ave_consumption  USING    pv_installation TYPE anlage
                                   pv_reading_date TYPE sydatum
                          CHANGING pv_ave_consump  TYPE consmpt.
  TYPES: BEGIN OF ty_mrid,
          mrid           TYPE ablbelnr,
          reading_date   TYPE adatsoll,
         END OF ty_mrid.

  DATA:  lt_mrid         TYPE TABLE OF ty_mrid,
         lt_eabl         TYPE TABLE OF bapieabl,
         lt_eablg        TYPE TABLE OF bapieablg,

         lv_first_day    TYPE sydatum,
         lv_last_day     TYPE sydatum,
         lv_eqpt         TYPE equnr,
         lv_zwnummer     TYPE e_zwnummer,
         lv_adat         TYPE adattats,
         lv_atim         TYPE atim,
         lv_v_zwstndab   TYPE v_zwstndab,
         lv_cnsmpt       TYPE i_abrmenge,
         lv_ave_cnsmpt   TYPE i_abrmenge,
         lv_counter      TYPE i.

  FIELD-SYMBOLS:
         <fs_mrid>       TYPE ty_mrid,
         <fs_eabl>       TYPE bapieabl,
         <fs_eablg>      TYPE bapieablg.

  CONSTANTS:
         co_mrtype       TYPE mrdocumenttype VALUE '2',
         co_mrreason     TYPE ablesgr        VALUE '01'.

  "-- Get First day of the Month
  CONCATENATE pv_reading_date+0(6) '01' INTO lv_first_day.

  "-- Reinitialize Counter Variable.
  CLEAR: lv_counter.

  DO 12 TIMES.
    "-- Reinitialize Local Variables
    CLEAR: lv_eqpt, lv_zwnummer, lv_adat, lv_atim, lv_v_zwstndab.

    "-- Get Last Day of Previous Month
    lv_last_day = lv_first_day - 1.

    "-- Get First Day of Previous Month
    CONCATENATE lv_last_day+0(6) `01` INTO lv_first_day.

    "-- Reinitialize Local Tables
    REFRESH: lt_eabl, lt_eablg.

    "-- Get Meter Reading Schedules Data
    CALL FUNCTION 'BAPI_MTRREADDOC_GETLIST'
      EXPORTING
        installation      = pv_installation
        mrdatefrom        = lv_first_day
        mrdateto          = lv_last_day
        mrdocumenttype    = co_mrtype
      TABLES
        mrdocumentdata    = lt_eabl
        mrdocumentreasons = lt_eablg.

    IF lt_eabl[] IS NOT INITIAL.
      "-- Get Periodic Meter Reading Only '01'
      READ TABLE lt_eablg ASSIGNING <fs_eablg> WITH KEY mrreason = co_mrreason.
      IF sy-subrc EQ 0.
        "-- Get Meter Reading Schedule Data
        READ TABLE lt_eabl ASSIGNING <fs_eabl> WITH KEY mridnumber = <fs_eablg>-mridnumber.
        IF sy-subrc EQ 0.
          lv_counter = lv_counter + 1.

          lv_eqpt       = <fs_eabl>-equipment.
          lv_zwnummer   = <fs_eabl>-register.
          lv_adat       = <fs_eabl>-mrdateforbilling.
          lv_atim       = <fs_eabl>-mrtimeforbilling.
          lv_v_zwstndab = <fs_eabl>-readingresult.

          "-- Get Current Consumption
          PERFORM get_curr_consumption USING    lv_eqpt
                                                lv_zwnummer
                                                lv_adat
                                                lv_atim
                                                lv_v_zwstndab
                                       CHANGING lv_cnsmpt.

          lv_ave_cnsmpt = lv_ave_cnsmpt + lv_cnsmpt.
        ENDIF.
      ENDIF.
    ENDIF.

    "-- Check for Six Months of Reading
    IF lv_counter EQ 6.
      EXIT.
    ENDIF.
  ENDDO.

  "-- Checks if No Reading for the past 1 year
  IF lv_counter EQ 0.
    "-- Get Latest Valid Reading
    SELECT ablbelnr adatsoll
      INTO TABLE lt_mrid
    FROM   eablg
    WHERE  anlage   EQ pv_installation
    AND    ablesgr  EQ '01'
    AND    adatsoll LT lv_first_day.

    IF sy-subrc EQ 0.
      "-- Sort by Latest Reading
      SORT lt_mrid BY reading_date DESCENDING.

      "-- Get Latest Reading Date
      READ TABLE lt_mrid ASSIGNING <fs_mrid> INDEX 1.
      IF sy-subrc EQ 0.
        "-- Get Reading Details
        SELECT SINGLE equnr zwnummer adattats atimtats v_zwstndab
          INTO (lv_eqpt, lv_zwnummer, lv_adat, lv_atim, lv_v_zwstndab)
        FROM   eabl
        WHERE  ablbelnr EQ <fs_mrid>-mrid.

        IF sy-subrc EQ 0.
          "-- Get Current Consumption
          PERFORM get_curr_consumption USING    lv_eqpt
                                                lv_zwnummer
                                                lv_adat
                                                lv_atim
                                                lv_v_zwstndab
                                       CHANGING lv_cnsmpt.

          "-- Ave.Consump. should be equal to the Current Consumption of the latest valid
          lv_ave_cnsmpt = lv_cnsmpt.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    "-- Compute Average Consumption
    lv_ave_cnsmpt = lv_ave_cnsmpt / lv_counter.
  ENDIF.

  IF lv_ave_cnsmpt GT 0.
    WRITE lv_ave_cnsmpt TO pv_ave_consump ROUND 0 DECIMALS 0.

    REPLACE ALL OCCURRENCES OF ',' IN pv_ave_consump WITH ''.

    SHIFT pv_ave_consump LEFT DELETING LEADING space.
  ENDIF.
ENDFORM.                    " GET_AVE_CONSUMPTION

*&---------------------------------------------------------------------*
*&      Form  GET_CURR_CONSUMPTION
*&---------------------------------------------------------------------*
*       Get Current Consumption for the given Month
*&---------------------------------------------------------------------*
FORM get_curr_consumption USING    pv_eqpt       TYPE equnr
                                   pv_zwnummer   TYPE e_zwnummer
                                   pv_adat       TYPE adattats
                                   pv_atim       TYPE atim
                                   pv_v_zwstndab TYPE v_zwstndab
                          CHANGING pv_consump    TYPE i_abrmenge.

  DATA: lv_n_zwstndab  TYPE n_zwstndab,
        lv_atim        TYPE atim.

  DATA: lt_eabl_pre    TYPE TABLE OF eabl,
        lt_eablg_pre   TYPE TABLE OF eablg.

  FIELD-SYMBOLS:
        <fs_eabl_pre>  TYPE eabl,
        <fs_eablg_pre> TYPE eablg.

  "-- Reinitialize Current Consumption Variable
  CLEAR: pv_consump.

  "-- Get Reading Details as of Previous Months
  CALL FUNCTION 'ISU_DB_EABL_SELECT_EQUNR_ENT'
    EXPORTING
      x_equnr          = pv_eqpt
      x_zwnummer       = pv_zwnummer
      x_adatbis        = pv_adat
    TABLES
      ty_eabl          = lt_eabl_pre[]
      ty_eablg         = lt_eablg_pre[]
    EXCEPTIONS
      not_found        = 1
      system_error     = 2
      not_qualified    = 3
      invalid_interval = 4
      OTHERS           = 5.

  IF lt_eabl_pre[] IS NOT INITIAL.
    "-- Sort Meter Readings by Reading Date
    SORT lt_eabl_pre BY adat DESCENDING atim DESCENDING.

    "-- Remove Inactive Readings
    DELETE lt_eabl_pre WHERE: adat = pv_adat AND atim > pv_atim,
                              aktiv NE '1'.

    READ TABLE lt_eabl_pre ASSIGNING <fs_eabl_pre> INDEX 1.
    IF sy-subrc EQ 0.
      IF <fs_eabl_pre>-adat EQ pv_adat AND <fs_eabl_pre>-atim EQ pv_atim..
        READ TABLE lt_eabl_pre ASSIGNING <fs_eabl_pre> INDEX 2.
        IF sy-subrc EQ 0.
          READ TABLE lt_eablg_pre ASSIGNING <fs_eablg_pre> WITH KEY ablbelnr = <fs_eabl_pre>-ablbelnr.
          IF sy-subrc EQ 0.
            IF <fs_eablg_pre>-ablesgr NE '01'.
              READ TABLE lt_eabl_pre ASSIGNING <fs_eabl_pre> INDEX 3.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE lt_eabl_pre ASSIGNING <fs_eabl_pre> INDEX 1.
      ENDIF.
    ENDIF.

    "-- Get Current Consumptions
    CALL FUNCTION 'ISU_CONSUMPTION_DETERMINE'
      EXPORTING
        x_equnr           = pv_eqpt
        x_zwnummer        = pv_zwnummer
        x_adat            = pv_adat
        x_atim            = lv_atim
        x_v_zwstndab      = pv_v_zwstndab
        x_n_zwstndab      = lv_n_zwstndab
        x_adatvor         = <fs_eabl_pre>-adat
        x_atimvor         = <fs_eabl_pre>-atim
        x_v_zwstvor       = <fs_eabl_pre>-v_zwstand
        x_n_zwstvor       = <fs_eabl_pre>-n_zwstand
      IMPORTING
        y_i_abrmenge      = pv_consump
      EXCEPTIONS
        general_fault     = 1
        zwstandab_missing = 2
        zwstand_missing   = 3
        parameter_missing = 4
        no_inst_structure = 5
        OTHERS            = 6.

    IF sy-subrc NE 0.
      pv_consump = 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_CURR_CONSUMPTION

*&---------------------------------------------------------------------*
*&      Form  CHECK_HOLIDAY
*&---------------------------------------------------------------------*
*         Check if Holiday
*----------------------------------------------------------------------*
FORM check_holiday  USING    pv_actual_sched TYPE sydatum
                             pv_calendar_id  TYPE hident
                    CHANGING pv_holiday      TYPE sysubrc.

  DATA: lv_holiday_found TYPE char01.

  "-- Check Holiday
  CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
    EXPORTING
      date                         = pv_actual_sched
      holiday_calendar_id          = pv_calendar_id
    IMPORTING
      holiday_found                = lv_holiday_found
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      holiday_calendar_id_missing  = 5
      holiday_calendar_not_found   = 6
      OTHERS                       = 7.

  IF sy-subrc EQ 0 AND lv_holiday_found EQ abap_true.
    pv_holiday = 0.
  ELSE.
    pv_holiday = 4.
  ENDIF.
ENDFORM.                    " CHECK_HOLIDAY

*&---------------------------------------------------------------------*
*&      Form  CHECK_WEEKEND
*&---------------------------------------------------------------------*
*         Check if Weekend
*----------------------------------------------------------------------*
FORM check_weekend  USING    pv_actual_sched TYPE sydatum
                    CHANGING pv_weekend      TYPE sysubrc.

  DATA: lv_day_indicator TYPE cind,
        lv_actual_day    TYPE scdatum.

  CLEAR: pv_weekend.

  "-- Get Number of Day (1-Mon, 2-Tues, etc..)
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = pv_actual_sched
    IMPORTING
      day  = lv_day_indicator.

  IF lv_day_indicator EQ 6 OR lv_day_indicator EQ 7.
    pv_weekend = 0.
  ELSE.
    pv_weekend = 4.
  ENDIF.
ENDFORM.                    " CHECK_WEEKEND

*&---------------------------------------------------------------------*
*&      Form  GET_CALENDAR_ID
*&---------------------------------------------------------------------*
*         Get Calendar ID of Business Area
*----------------------------------------------------------------------*
FORM get_calendar_id  USING    pv_mru         TYPE ableinh
                      CHANGING pv_calendar_id TYPE hident.

  "-- Reinitialize Changing Variable
  CLEAR: pv_calendar_id.

  "-- Add Zero Padding
  mc_conversion_alpha pv_mru.

  "-- Get Calendar ID from Personnel Area/Subarea
  SELECT SINGLE ident
    INTO pv_calendar_id
  FROM   te422
  WHERE  termschl EQ pv_mru.

  IF sy-subrc NE 0.
    CLEAR: pv_calendar_id.
  ENDIF.
ENDFORM.                    " GET_CALENDAR_ID

*&---------------------------------------------------------------------*
*&      Form  GET_INSTALDUE_BILL_PERIOD
*&---------------------------------------------------------------------*
*       Get Billing Period for Installment Plan
*----------------------------------------------------------------------*
FORM get_instaldue_bill_period  USING    pv_mru        TYPE ableinheit
                                         pv_can        TYPE vkont_kk
*                                         pv_prevrdgdt  TYPE sydatum
*                                         pv_presrdgdt  TYPE sydatum
                                         pv_lastday    TYPE sydatum
                                CHANGING pv_start_bill TYPE sydatum
                                         pv_end_bill   TYPE sydatum.

  TYPES: BEGIN OF ty_billings,
           doc_no     TYPE opbel_kk,
         END OF ty_billings.

  DATA: lt_mr_dates   TYPE TABLE OF te418,
        lt_billings   TYPE TABLE OF ty_billings.

  FIELD-SYMBOLS:
        <fs_mr_dates> TYPE te418.

  "-- Check if First Billing
  SELECT opbel
    INTO TABLE lt_billings
  FROM   dfkkop
  WHERE  vkont EQ pv_can
  AND    opupk EQ '1'
  AND    hvorg EQ '0100'
  AND    faedn LE pv_lastday.

  IF sy-dbcnt LE 1. "EQ 0.
    "-- Get MR Dates for MRU
    CALL FUNCTION 'ISU_DB_TE418_SELECT_TERMTDAT'
      EXPORTING
        x_termschl    = pv_mru
        x_termtdat    = pv_lastday
      TABLES
        t_te418       = lt_mr_dates
      EXCEPTIONS
        not_found     = 1
        system_error  = 2
        not_qualified = 3
        OTHERS        = 4.

    IF sy-subrc EQ 0.
      "-- Sort by MR. Sched. Dates
      SORT lt_mr_dates BY adatsoll DESCENDING.

      "-- Get End Bill Date
      READ TABLE lt_mr_dates ASSIGNING <fs_mr_dates> INDEX 1.
      IF sy-subrc EQ 0.
        pv_end_bill = <fs_mr_dates>-adatsoll.
      ENDIF.

      "-- Get Start Bill Date
      READ TABLE lt_mr_dates ASSIGNING <fs_mr_dates> INDEX 2.
      IF sy-subrc EQ 0.
        pv_start_bill = <fs_mr_dates>-adatsoll.
      ENDIF.
    ENDIF.
*  ELSE.
*    pv_start_bill = pv_prevrdgdt + 1. "-- Added 1 day - RANDYSY 08/27/2013 TN#41237
*    pv_end_bill   = pv_presrdgdt.
  ENDIF.
ENDFORM.                    " GET_INSTALDUE_BILL_PERIOD

*&---------------------------------------------------------------------*
*&      Form  GET_SCHED_MR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_sched_mr USING    pv_mru        TYPE ableinheit
                           pv_can        TYPE vkont_kk
                           pv_lastday    TYPE sydatum
                  CHANGING pv_start_bill TYPE sydatum.

  DATA: lt_mr_dates   TYPE TABLE OF te418.

  FIELD-SYMBOLS:
        <fs_mr_dates> TYPE te418.

  "-- Get MR Dates for MRU
  CALL FUNCTION 'ISU_DB_TE418_SELECT_TERMTDAT'
    EXPORTING
      x_termschl    = pv_mru
      x_termtdat    = pv_lastday
    TABLES
      t_te418       = lt_mr_dates
    EXCEPTIONS
      not_found     = 1
      system_error  = 2
      not_qualified = 3
      OTHERS        = 4.

  IF sy-subrc EQ 0.
    "-- Sort by MR. Sched. Dates
    SORT lt_mr_dates BY adatsoll DESCENDING.

    "-- Get Start Bill Date
    READ TABLE lt_mr_dates ASSIGNING <fs_mr_dates> INDEX 2.
    IF sy-subrc EQ 0.
      pv_start_bill = <fs_mr_dates>-adatsoll.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_SCHED_MR

*&---------------------------------------------------------------------*
*&      Form  POPULATE_MONTHYEAR
*&---------------------------------------------------------------------*
*       Populate Month & Year Parameter
*----------------------------------------------------------------------*
FORM populate_monthyear .
  DATA: it_list   TYPE vrm_values,
        lv_month  TYPE n LENGTH 2,
        lv_year   TYPE n LENGTH 4.

  FIELD-SYMBOLS:
      <fs_list> TYPE vrm_value.

  "-- Macro Definition for Appending List
  DEFINE mc_append_list.
    append initial line to it_list assigning <fs_list>.
    <fs_list>-key = &1.
  END-OF-DEFINITION.

  "-- Reinitialize Local List Table
  REFRESH it_list.

  "-- Set First Month Value
  lv_month = 12.
  mc_append_list lv_month.

  "-- Populate Months
  DO 11 TIMES.
    lv_month = lv_month - 1.
    mc_append_list lv_month.
  ENDDO.

  "-- Populate Parameter Months with values from Local Table
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'S_MONTH'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  "-- Reinitialize Local List Table
  REFRESH it_list.

  "-- Set First Year Value
  lv_year = sy-datum+0(4).
  mc_append_list lv_year.

  "-- Populate Year
  DO 50 TIMES.
    lv_year = lv_year - 1.
    mc_append_list lv_year.
  ENDDO.

  "-- Populate Parameter Year with values from Local Table
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'S_YEAR'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " POPULATE_MONTHYEAR
*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVED_BILL
*&---------------------------------------------------------------------*
*       Extract bill document from archive
*----------------------------------------------------------------------*
*      -->PV_VKONT     Contract Account Number
*      -->PV_ADATSOLL  Scheduled Meter Reading Date
*      <--PV_BELNR     Billing Document Number
*----------------------------------------------------------------------*
FORM get_archived_bill  USING    pv_vkont    TYPE erch-vkont
                                 pv_adatsoll TYPE erch-adatsoll
                        CHANGING pv_belnr    TYPE erch-belnr.

  TYPES : BEGIN OF ty_selection,
            vkont     TYPE erch-vkont,
            adatsoll  TYPE erch-adatsoll,
            stornodat TYPE erch-stornodat,
          END OF ty_selection.

  DATA:   lv_selection     TYPE ty_selection,
          lv_handle        TYPE sy-tabix,
          lv_fieldcat      TYPE aind_str1-archindex,
          lv_prueflos      TYPE aind_arkey,
          lv_archivekey    TYPE arkey,
          lv_archiveofs    TYPE admi_offst,
          lv_doc_data      TYPE as_t_tablebuffer,
          lv_doc_data2     TYPE astablebuffer,
          lv_erch          TYPE erch.

  DATA:   lt_obligatory_fields  TYPE STANDARD TABLE OF fieldname,
          lt_fieldcat           TYPE STANDARD TABLE OF aind_str1-archindex,
          lt_result             TYPE STANDARD TABLE OF aind_arkey,
          lt_prueflos           TYPE STANDARD TABLE OF aind_arkey,
          lt_erch               TYPE STANDARD TABLE OF erch.

  FIELD-SYMBOLS: <fs_doc_data> TYPE STANDARD TABLE.

  lv_selection-vkont     = pv_vkont.
  lv_selection-adatsoll  = pv_adatsoll.
  lv_selection-stornodat = '00000000'.

  APPEND 'BELNR'    TO lt_obligatory_fields.

  SELECT archindex
  INTO TABLE lt_fieldcat
  FROM   aind_str1
  WHERE  object EQ 'ISU_BILL'
  AND    itype  EQ 'S'.

  LOOP AT lt_fieldcat INTO lv_fieldcat.
    CALL FUNCTION 'AS_API_READ'
      EXPORTING
        i_fieldcat                = lv_fieldcat
        i_selections              = lv_selection
        i_obligatory_fields       = lt_obligatory_fields[]
*       i_selections              = lt_range
*       i_maxrows                 = 1
      IMPORTING
        e_result                  = lt_result
      EXCEPTIONS
        no_infostruc_found        = 2
        field_missing_in_fieldcat = 3.

    IF sy-subrc = 0 AND NOT lt_result IS INITIAL.
      APPEND LINES OF lt_result TO lt_prueflos.
      SORT lt_prueflos DESCENDING.        "return the newest doc in archive
      DELETE ADJACENT DUPLICATES FROM lt_prueflos.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_prueflos INDEX 1 INTO lv_prueflos.

  IF sy-subrc EQ 0.

    lv_archivekey = lv_prueflos-archivekey.
    lv_archiveofs = lv_prueflos-archiveofs.

    CALL FUNCTION 'ARCHIVE_GET_OBJECT_BY_OFFSET'
      EXPORTING
        archivkey        = lv_archivekey
        offset           = lv_archiveofs
      IMPORTING
        archive_handle   = lv_handle
      EXCEPTIONS
        no_record_found  = 1
        file_io_error    = 2
        internal_error   = 3
        open_error       = 4
        object_not_found = 5
        not_authorized   = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      " some error handling .
    ENDIF.

    CALL FUNCTION 'ARCHIVE_READ_OBJECT_BY_HANDLE'
      EXPORTING
        iv_handle   = lv_handle
      CHANGING
        ct_obj_data = lv_doc_data.

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle = lv_handle.

    "-- ERCH Table
    READ TABLE lv_doc_data WITH KEY tabname = 'ERCH' INTO lv_doc_data2.
    IF sy-subrc EQ 0.
      ASSIGN lv_doc_data2-tabref->* TO <fs_doc_data>.
      lt_erch = <fs_doc_data>.
      READ TABLE lt_erch INDEX 1 INTO lv_erch.
      pv_belnr = lv_erch-belnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ARCHIVED_BILL
*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVED_TARIFNR
*&---------------------------------------------------------------------*
*       Extrract TARIFNR from archive
*----------------------------------------------------------------------*
*      -->PV_BELNR    Billing document number
*      <--PV_TARIFNR  Rate key
*----------------------------------------------------------------------*
FORM GET_ARCHIVED_TARIFNR  USING    PV_BELNR type dberchz1-belnr
                           CHANGING PV_TARIFNR type dberchz1-tarifnr.

  TYPES : BEGIN OF ty_selection,
            belnr   TYPE dberchz1-belnr,
          END OF ty_selection.

  DATA:   lv_selection     TYPE ty_selection,
          lv_handle        TYPE sy-tabix,
          lv_fieldcat      TYPE aind_str1-archindex,
          lv_prueflos      TYPE aind_arkey,
          lv_archivekey    TYPE arkey,
          lv_archiveofs    TYPE admi_offst,
          lv_doc_data      TYPE as_t_tablebuffer,
          lv_doc_data2     TYPE astablebuffer,
          lv_dberchz1      TYPE dberchz1.

  DATA:   lt_obligatory_fields  TYPE STANDARD TABLE OF fieldname,
          lt_fieldcat           TYPE STANDARD TABLE OF aind_str1-archindex,
          lt_result             TYPE STANDARD TABLE OF aind_arkey,
          lt_prueflos           TYPE STANDARD TABLE OF aind_arkey,
          lt_dberchz1           TYPE STANDARD TABLE OF dberchz1.

  FIELD-SYMBOLS: <fs_doc_data> TYPE STANDARD TABLE.

  lv_selection-belnr = pv_belnr.

  APPEND 'BELNR'    TO lt_obligatory_fields.

  SELECT archindex
  INTO TABLE lt_fieldcat
  FROM   aind_str1
  WHERE  object EQ 'ISU_BILLZ'
  AND    itype  EQ 'S'.

  LOOP AT lt_fieldcat INTO lv_fieldcat.
    CALL FUNCTION 'AS_API_READ'
      EXPORTING
        i_fieldcat                = lv_fieldcat
        i_selections              = lv_selection
        i_obligatory_fields       = lt_obligatory_fields[]
*       i_selections              = lt_range
*       i_maxrows                 = 1
      IMPORTING
        e_result                  = lt_result
      EXCEPTIONS
        no_infostruc_found        = 2
        field_missing_in_fieldcat = 3.

    IF sy-subrc = 0 AND NOT lt_result IS INITIAL.
      APPEND LINES OF lt_result TO lt_prueflos.
      SORT lt_prueflos DESCENDING.        "return the newest doc in archive
      DELETE ADJACENT DUPLICATES FROM lt_prueflos.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_prueflos INDEX 1 INTO lv_prueflos.

  IF sy-subrc EQ 0.

    lv_archivekey = lv_prueflos-archivekey.
    lv_archiveofs = lv_prueflos-archiveofs.

    CALL FUNCTION 'ARCHIVE_GET_OBJECT_BY_OFFSET'
      EXPORTING
        archivkey        = lv_archivekey
        offset           = lv_archiveofs
      IMPORTING
        archive_handle   = lv_handle
      EXCEPTIONS
        no_record_found  = 1
        file_io_error    = 2
        internal_error   = 3
        open_error       = 4
        object_not_found = 5
        not_authorized   = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      " some error handling .
    ENDIF.

    CALL FUNCTION 'ARCHIVE_READ_OBJECT_BY_HANDLE'
      EXPORTING
        iv_handle   = lv_handle
      CHANGING
        ct_obj_data = lv_doc_data.

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle = lv_handle.

    "-- ERCH Table
    READ TABLE lv_doc_data WITH KEY tabname = 'DBERCHZ1' INTO lv_doc_data2.
    IF sy-subrc EQ 0.
      ASSIGN lv_doc_data2-tabref->* TO <fs_doc_data>.
      lt_dberchz1 = <fs_doc_data>.
      READ TABLE lt_dberchz1 WITH KEY BELZART = 'ZBASIC' INTO lv_dberchz1.
      pv_tarifnr = lv_dberchz1-tarifnr.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_ARCHIVED_TARIFNR
*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVED_BELZEILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_BELNR  Billing document number
*      <--PV_BELZEILE  Billing line item
*----------------------------------------------------------------------*
FORM GET_ARCHIVED_BELZEILE  USING    PV_BELNR     type dberchz1-belnr
                            CHANGING PV_BELZEILE  type dberchz1-belzeile.
  TYPES : BEGIN OF ty_selection,
            belnr   TYPE dberchz1-belnr,
          END OF ty_selection.

  DATA:   lv_selection     TYPE ty_selection,
          lv_handle        TYPE sy-tabix,
          lv_fieldcat      TYPE aind_str1-archindex,
          lv_prueflos      TYPE aind_arkey,
          lv_archivekey    TYPE arkey,
          lv_archiveofs    TYPE admi_offst,
          lv_doc_data      TYPE as_t_tablebuffer,
          lv_doc_data2     TYPE astablebuffer,
          lv_dberchz1      TYPE dberchz1.

  DATA:   lt_obligatory_fields  TYPE STANDARD TABLE OF fieldname,
          lt_fieldcat           TYPE STANDARD TABLE OF aind_str1-archindex,
          lt_result             TYPE STANDARD TABLE OF aind_arkey,
          lt_prueflos           TYPE STANDARD TABLE OF aind_arkey,
          lt_dberchz1           TYPE STANDARD TABLE OF dberchz1.

  FIELD-SYMBOLS: <fs_doc_data> TYPE STANDARD TABLE.

  lv_selection-belnr = pv_belnr.

  APPEND 'BELNR'    TO lt_obligatory_fields.

  SELECT archindex
  INTO TABLE lt_fieldcat
  FROM   aind_str1
  WHERE  object EQ 'ISU_BILLZ'
  AND    itype  EQ 'S'.

  LOOP AT lt_fieldcat INTO lv_fieldcat.
    CALL FUNCTION 'AS_API_READ'
      EXPORTING
        i_fieldcat                = lv_fieldcat
        i_selections              = lv_selection
        i_obligatory_fields       = lt_obligatory_fields[]
*       i_selections              = lt_range
*       i_maxrows                 = 1
      IMPORTING
        e_result                  = lt_result
      EXCEPTIONS
        no_infostruc_found        = 2
        field_missing_in_fieldcat = 3.

    IF sy-subrc = 0 AND NOT lt_result IS INITIAL.
      APPEND LINES OF lt_result TO lt_prueflos.
      SORT lt_prueflos DESCENDING.        "return the newest doc in archive
      DELETE ADJACENT DUPLICATES FROM lt_prueflos.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_prueflos INDEX 1 INTO lv_prueflos.

  IF sy-subrc EQ 0.

    lv_archivekey = lv_prueflos-archivekey.
    lv_archiveofs = lv_prueflos-archiveofs.

    CALL FUNCTION 'ARCHIVE_GET_OBJECT_BY_OFFSET'
      EXPORTING
        archivkey        = lv_archivekey
        offset           = lv_archiveofs
      IMPORTING
        archive_handle   = lv_handle
      EXCEPTIONS
        no_record_found  = 1
        file_io_error    = 2
        internal_error   = 3
        open_error       = 4
        object_not_found = 5
        not_authorized   = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      " some error handling .
    ENDIF.

    CALL FUNCTION 'ARCHIVE_READ_OBJECT_BY_HANDLE'
      EXPORTING
        iv_handle   = lv_handle
      CHANGING
        ct_obj_data = lv_doc_data.

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle = lv_handle.

    "-- ERCH Table
    READ TABLE lv_doc_data WITH KEY tabname = 'DBERCHZ1' INTO lv_doc_data2.
    IF sy-subrc EQ 0.
      ASSIGN lv_doc_data2-tabref->* TO <fs_doc_data>.
      lt_dberchz1 = <fs_doc_data>.
      READ TABLE lt_dberchz1 WITH KEY BELZART = 'ZOTR' INTO lv_dberchz1.
      pv_belzeile = lv_dberchz1-belzeile.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_ARCHIVED_BELZEILE
*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVED_NETTOBTR
*&---------------------------------------------------------------------*
*       Extract nettobtr from archive
*----------------------------------------------------------------------*
*      -->PV_BELNR      Billing document number
*      -->PV_BELZEILE   Billing line item
*      <--PV_OTR        OTR line item amount
*----------------------------------------------------------------------*
FORM GET_ARCHIVED_NETTOBTR  USING    PV_BELNR     type dberchz3-belnr
                                     PV_BELZEILE  type dberchz3-belzeile
                            CHANGING PV_OTR       type dberchz3-nettobtr.
  TYPES : BEGIN OF ty_selection,
            belnr    TYPE dberchz3-belnr,
            belzeile TYPE dberchz3-belzeile,
          END OF ty_selection.

  DATA:   lv_selection     TYPE ty_selection,
          lv_handle        TYPE sy-tabix,
          lv_fieldcat      TYPE aind_str1-archindex,
          lv_prueflos      TYPE aind_arkey,
          lv_archivekey    TYPE arkey,
          lv_archiveofs    TYPE admi_offst,
          lv_doc_data      TYPE as_t_tablebuffer,
          lv_doc_data2     TYPE astablebuffer,
          lv_dberchz3      TYPE dberchz3.

  DATA:   lt_obligatory_fields  TYPE STANDARD TABLE OF fieldname,
          lt_fieldcat           TYPE STANDARD TABLE OF aind_str1-archindex,
          lt_result             TYPE STANDARD TABLE OF aind_arkey,
          lt_prueflos           TYPE STANDARD TABLE OF aind_arkey,
          lt_dberchz3           TYPE STANDARD TABLE OF dberchz3.

  FIELD-SYMBOLS: <fs_doc_data> TYPE STANDARD TABLE.

  lv_selection-belnr    = pv_belnr.
  lv_selection-belzeile = pv_belzeile.

  APPEND 'BELNR'    TO lt_obligatory_fields.

  SELECT archindex
  INTO TABLE lt_fieldcat
  FROM   aind_str1
  WHERE  object EQ 'ISU_BILLZ'
  AND    itype  EQ 'S'.

  LOOP AT lt_fieldcat INTO lv_fieldcat.
    CALL FUNCTION 'AS_API_READ'
      EXPORTING
        i_fieldcat                = lv_fieldcat
        i_selections              = lv_selection
        i_obligatory_fields       = lt_obligatory_fields[]
*       i_selections              = lt_range
*       i_maxrows                 = 1
      IMPORTING
        e_result                  = lt_result
      EXCEPTIONS
        no_infostruc_found        = 2
        field_missing_in_fieldcat = 3.

    IF sy-subrc = 0 AND NOT lt_result IS INITIAL.
      APPEND LINES OF lt_result TO lt_prueflos.
      SORT lt_prueflos DESCENDING.        "return the newest doc in archive
      DELETE ADJACENT DUPLICATES FROM lt_prueflos.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_prueflos INDEX 1 INTO lv_prueflos.

  IF sy-subrc EQ 0.

    lv_archivekey = lv_prueflos-archivekey.
    lv_archiveofs = lv_prueflos-archiveofs.

    CALL FUNCTION 'ARCHIVE_GET_OBJECT_BY_OFFSET'
      EXPORTING
        archivkey        = lv_archivekey
        offset           = lv_archiveofs
      IMPORTING
        archive_handle   = lv_handle
      EXCEPTIONS
        no_record_found  = 1
        file_io_error    = 2
        internal_error   = 3
        open_error       = 4
        object_not_found = 5
        not_authorized   = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      " some error handling .
    ENDIF.

    CALL FUNCTION 'ARCHIVE_READ_OBJECT_BY_HANDLE'
      EXPORTING
        iv_handle   = lv_handle
      CHANGING
        ct_obj_data = lv_doc_data.

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle = lv_handle.

    "-- ERCH Table
    READ TABLE lv_doc_data WITH KEY tabname = 'DBERCHZ3' INTO lv_doc_data2.
    IF sy-subrc EQ 0.
      ASSIGN lv_doc_data2-tabref->* TO <fs_doc_data>.
      lt_dberchz3 = <fs_doc_data>.
      READ TABLE lt_dberchz3 INDEX 1 INTO lv_dberchz3.
      pv_belzeile = lv_dberchz3-belzeile.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_ARCHIVED_NETTOBTR