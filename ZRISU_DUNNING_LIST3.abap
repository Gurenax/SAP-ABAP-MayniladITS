*&---------------------------------------------------------------------*
*& Report     : ZRISU_DOWNLOAD_OSB
*& DESCRIPTION: Dunning List
*&---------------------------------------------------------------------*
*& Created by: Glenn Dimaliwat
*& Created On: January 22, 2013
*& Reference : Based on original program ZRISU_DUNNING_LIST by FCGONZALES
*&             dated March 28, 2008
*&---------------------------------------------------------------------*
*& Date      | Author ID   |Ticket No.        | Description
*&---------------------------------------------------------------------*
* 01/22/2013 | GDIMALIWAT  | 23217            | Initial Development. Program renamed to ZRISU_DUNNING_LIST2. Added 61+_OVERDUE Field/Conditions
* 02/01/2013 | GDIMALIWAT  | 23217            | Program modified to run a 2nd extraction (less 22 days) when results are less than 100
* 02/05/2013 | GDIMALIWAT  | 23217            | Program modified to add condition on 61+_OVERDUE. Write to file only when overdue > 0 (DUEDATE extraction only)
* 02/18/2013 | GDIMALIWAT  | 28331            | Program modified to Change Output File of Per BP/Contract Account Extraction
* 02/20/2013 | GDIMALIWAT  | 28331            | Program modified to modify E-mail Notification message
* 03/05/2013 | GDIMALIWAT  | 29641            | Program modified to add READ TABLE validations
* 03/07/2013 | GDIMALIWAT  | 29797            | Program modified to update Due Date parameter validations and to fix missing Headers
* 03/11/2013 | GDIMALIWAT  | 29848            | Program modified to optimize ZDUNNLIST and fix Current/Overdue
* 03/18/2013 | GDIMALIWAT  | 30567            | Optimization of ZDUNNLIST Program
* 03/20/2013 | GDIMALIWAT  | 30567            | Renamed program name to ZRISU_DUNNING_LIST3 / Fix current computation / Removed PUA > 0 condition
* 03/26/2013 | GDIMALIWAT  | 30567            | Program modified to update Due Date parameters for e-mail notification
* 04/02/2013 | GDIMALIWAT  | 30567            | Program modified to remove backtracking and exclude accounts with negative value in total column
* 04/15/2013 | GDIMALIWAT  | 30567            | Program modified to fix protested bill condition
* 04/24/2013 | GDIMALIWAT  | 30567            | Program modified to add miscellaneous column
* 05/30/2013 | GDIMALIWAT  | 30567            | Program modified to change current condition to 0-25 days and PUA condition to 26-60 days
* 05/30/2013 | GDIMALIWAT  | 30567            | Program modified to change e-mail recipents (Air 21)
* 06/06/2013 | GDIMALIWAT  | 30567            | Program modified to fix missing data - Name, MRU, RSG, Device
* 06/10/2013 | GDIMALIWAT  | 30567            | Program modified to fix other unpaid data
*& Changes made in Arangkada Dev ------------------------------------------------------------*
* 07/10/2013 | GDIMALIWAT  | 30567            | Program modified to remove checking of condoned accounts
* 07/10/2013 | GDIMALIWAT  | 30567            | Program modified to add the validation - oldest open bill > 60 days
* 07/10/2013 | GDIMALIWAT  | 30567            | Program modified to remove HVORG 0100 and 0200 in main extraction condition
* 07/15/2013 | GDIMALIWAT  | 30567            | Added filter for Reversed Posting - for Due Balance
* 07/15/2013 | GDIMALIWAT  | 30567            | Removed -7 days computation from lv_premise_param
* 09/06/2013 | GDIMALIWAT  | 31700            | Added condition lv_misc >= co_0 in Dunning sub-routine
* 09/12/2013 | GDIMALIWAT  | 31700            | Added PIA computation using GL and removed XRAGL conditions
* 09/19/2013 | GDIMALIWAT  | 31700            | Removed E0 condition for Misc computation
* 09/30/2013 | GDIMALIWAT  | 31700            | Added due date condition to Misc - Others
*&-------------------------------------------------------------------------------------------*

REPORT zrisu_dunning_list3 NO STANDARD PAGE HEADING LINE-SIZE 280.

*&---------------------------------------------------------------------*
*& Table List - Table Names
*&
TABLES: fkkvkp, dfkkop.

*&---------------------------------------------------------------------*
*& Type Pools - Pool Names
*&
TYPE-POOLS: vrm.

*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_gsber,
         gsber      TYPE fkkvkp-gsber,
       END OF ty_gsber,

       BEGIN OF ty_main,
         gsber      TYPE fkkvkp-gsber,
         rsg        TYPE c LENGTH 8,
         ableinh    TYPE eablg-ableinh,
         gpart      TYPE fkkvkp-gpart,
         vkont      TYPE fkkvkp-vkont,
         printdoc   TYPE c LENGTH 12,
         name       TYPE fkkvk-vkbez,
         addr       TYPE c LENGTH 80,
         budat      TYPE c LENGTH 8,
         current    TYPE c LENGTH 16,
         pua        TYPE c LENGTH 15,
         overdue    TYPE c LENGTH 15,
         misc       TYPE c LENGTH 15,
         total      TYPE c LENGTH 15,
         sortfld    TYPE c LENGTH 6,
         device     TYPE eabl-gernr,
         nodays     TYPE c LENGTH 4,
         oc         TYPE c LENGTH 4,
         oc2        TYPE c LENGTH 4,
       END OF ty_main,

       BEGIN OF ty_dunn,
        docno      TYPE char20,
        ficadoc    TYPE dfkkop-opbel,
        budat      TYPE erdk-budat,
        bldat      TYPE erdk-bldat,
        faedn      TYPE dfkkop-faedn,
        amount     TYPE dfkkop-betrh,
      END OF ty_dunn,

      BEGIN OF ty_dunn2,
        docno      TYPE char20,
        ficadoc    TYPE dfkkop-opbel,
        budat      TYPE erdk-budat,
        bldat      TYPE erdk-bldat,
        faedn      TYPE dfkkop-faedn,
        amount     TYPE dfkkop-betrh,
        sperz      TYPE dfkkop-sperz,
      END OF ty_dunn2,

      BEGIN OF ty_probill,
        loobj1     TYPE dfkklocks-loobj1,
        lockr      TYPE dfkklocks-lockr,
        vkont      TYPE dfkkop-vkont,
      END OF ty_probill,

      BEGIN OF ty_dunnhead,
        gsber      TYPE fkkvkp-gsber,
        gpart      TYPE fkkvkp-gpart,
        contacct   TYPE fkkvkp-vkont,
        name       TYPE fkkvk-vkbez,
        anlage     TYPE ever-anlage,
        abrsperr   TYPE ever-abrsperr,
        ableinh    TYPE eanlh-ableinh,
      END OF ty_dunnhead,

*      BEGIN OF ty_portlist,
*        portion    TYPE te420-termschl,
*      END OF ty_portlist,

*      BEGIN OF ty_condo,
*        vkont      TYPE fkkvkp-vkont,
*        cond_tag   TYPE zcondonation-cond_tag,
*      END OF ty_condo,

      BEGIN  OF ty_mr,
        adatsoll   TYPE eabl-adatsoll,
        ablesgr    TYPE eablg-ablesgr,
      END OF ty_mr,

      BEGIN OF ty_billdoc,
        belnr      TYPE erch-belnr,
        origdoc    TYPE erch-origdoc,
        adatsoll   TYPE erch-adatsoll,
      END OF ty_billdoc,

      BEGIN OF ty_oc,
        oc       TYPE eabl-ablhinw,
        adatsoll TYPE eabl-adatsoll,
        mrid     TYPE eabl-ablbelnr,
      END OF ty_oc,

      BEGIN OF ty_pua_overdue,
        opbel TYPE dfkkop-opbel,
        budat TYPE dfkkop-budat,
        betrh TYPE dfkkop-betrh,
       END OF ty_pua_overdue,

      BEGIN OF ty_opbel_only,
        opbel TYPE dfkkop-opbel,
       END OF ty_opbel_only,

       BEGIN OF ty_address,
        roomnumber TYPE adrc-roomnumber,
        floor      TYPE adrc-floor,
        house_num1 TYPE adrc-house_num1,
        house_num2 TYPE adrc-house_num2,
        str_suppl1 TYPE adrc-str_suppl1,
        street     TYPE adrc-street,
        str_suppl2 TYPE adrc-str_suppl1,
        str_suppl3 TYPE adrc-str_suppl3,
        location   TYPE adrc-location,
        city2      TYPE adrc-city2,
        city1      TYPE adrc-city1,
        post_code1 TYPE adrc-post_code1,
       END OF ty_address,

       BEGIN OF ty_dunnlist,
        gsber      TYPE fkkvkp-gsber,
        rsg        TYPE char8,
        ableinh    TYPE eablg-ableinh,
        gpart      TYPE fkkvkp-gpart,
        vkont      TYPE fkkvkp-vkont,
        printdoc   TYPE char12,
        name       TYPE fkkvk-vkbez,
        addr       TYPE char80,
        budat      TYPE char8,
        current    TYPE char16,
        pua        TYPE char15,
        overdue    TYPE char15,
        misc       TYPE char15,
        total      TYPE char15,
        sortfld    TYPE char6,
        device     TYPE eabl-gernr,
        nodays     TYPE char4,
        oc         TYPE char4,
        oc2        TYPE char4,
      END OF ty_dunnlist.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_main      TYPE TABLE OF ty_main     ##needed,
      "gt_condo     TYPE TABLE OF ty_condo    ##needed,
      gt_mr        TYPE TABLE OF ty_mr       ##needed,
      gt_billdoc   TYPE TABLE OF ty_billdoc  ##needed,
      gt_dunnlist  TYPE TABLE OF ty_dunnlist ##needed,
      gt_gsber     TYPE TABLE OF ty_gsber    ##needed.

*&---------------------------------------------------------------------*
*& Data Definitions - Global parameters (gs_<structure name>)
*&                                      (gv_<var name>),
*                                       (gr_<range>),
*                                       (go_<class>)
*DATA: new_portion    TYPE te420-termschl.
DATA:  gv_cnt              TYPE i                 ##needed,
       gv_valid_send       TYPE sysubrc           ##needed,
       gv_count            TYPE i                 ##needed,
       gv_highest_duedate  TYPE dfkkop-faedn      ##needed,
       gv_lowest_duedate   TYPE dfkkop-faedn      ##needed.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (fs_<name>)
*&
FIELD-SYMBOLS:  <fs_dunnhead>     TYPE ty_dunnhead ##needed.

*&---------------------------------------------------------------------*
*& Constants
*&
CONSTANTS:  co_1800 TYPE p VALUE 1800,
            co_80   TYPE p VALUE 80,
            co_60   TYPE p VALUE 60,
            co_25   TYPE p VALUE 25,
            "co_22   TYPE p VALUE 22,
            co_0    TYPE p VALUE 0.

*&----------------------------------------------------------------------
*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK entry WITH FRAME TITLE ent.
PARAMETERS:
      "old   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr, "GDIMALIWAT - Disable Regular Facility 03/07/2013
      pa_new   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
      pa_ddate RADIOBUTTON GROUP grp1 .
SELECTION-SCREEN END OF BLOCK entry.

*SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE blk1ttl. "GDIMALIWAT - Disable Regular Facility 03/07/2013
*PARAMETER:
*  ba       TYPE fkkvkp-gsber   AS LISTBOX VISIBLE LENGTH 10 DEFAULT '0400' MODIF ID b1 USER-COMMAND uba OBLIGATORY,
*  port_low TYPE te420-termschl AS LISTBOX VISIBLE LENGTH 10                MODIF ID b1 USER-COMMAND prt,
*  port_hi  TYPE te420-termschl AS LISTBOX VISIBLE LENGTH 10                MODIF ID b1 USER-COMMAND prt.
*
*PARAMETER: scheddt TYPE sy-datum DEFAULT sy-datum MODIF ID b1.
*SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK  blk2 WITH FRAME TITLE blk2ttl.
SELECT-OPTIONS:
      so_par FOR fkkvkp-gpart MODIF ID b2,
      so_can FOR fkkvkp-vkont MODIF ID b2.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK  blk5 WITH FRAME TITLE blk5ttl.
PARAMETER:        pa_ba2  TYPE fkkvkp-gsber   AS LISTBOX VISIBLE LENGTH 10 DEFAULT '0400' MODIF ID b3 OBLIGATORY.
SELECT-OPTIONS:   so_ddate FOR dfkkop-faedn MODIF ID b3.
SELECTION-SCREEN END OF BLOCK blk5.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE blk3ttl.
PARAMETER: pa_file TYPE rlgrap-filename DEFAULT
           '\\saprep01.mayniladsap.com\Mtr_RdngINDRA\ZDunning\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE blk4ttl.
PARAMETERS: pa_email AS CHECKBOX DEFAULT 'X' MODIF ID e1.
SELECTION-SCREEN END OF BLOCK blk4.

*&---------------------------------------------------------------------*
*& Initialization - Initial process at start of program
*&
INITIALIZATION.
  "-- Check if User is valid to send Email
  PERFORM is_valid_to_send_email CHANGING gv_valid_send.

  "-- Don't Display Send Email Checkbox
  IF gv_valid_send NE 0.
    LOOP AT SCREEN.
      IF screen-group1 = 'E1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "-- Populate BA Parameter
  PERFORM populate_ba.

  "-- Dynamic Changing of Portion Parameter Per BA
*  PERFORM get_portions USING ba: 'PORT_LOW',    "GDIMALIWAT - Disable Regular Facility 03/07/2013
*                                 'PORT_HI'.

  ent     = text-006.
*  blk1ttl = 'Regular Facility'. "GDIMALIWAT - Disable Regular Facility 03/07/2013
  blk2ttl = text-007.
  blk3ttl = text-008.
  blk5ttl = text-009.

  "-- For developer purpose
  PERFORM developer_params.

*&---------------------------------------------------------------------*
*& At Selection Screen
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  "-- Popup Window Dialog Box
  PERFORM select_file_path USING pa_file.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
*    IF old = 'X' AND ( screen-group1 = 'B2' OR screen-group1 = 'B3' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
*      screen-active = 0.
*      MODIFY SCREEN.
*    ENDIF.
    IF pa_new = 'X' AND screen-group1 = 'B3'."( screen-group1 = 'B1' OR screen-group1 = 'B3' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF pa_ddate = 'X' AND screen-group1 = 'B2'."( screen-group1 = 'B1' OR screen-group1 = 'B2' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    "-- Don't Display Send Email Checkbox
    IF gv_valid_send NE 0 OR pa_new EQ 'X'. "If user is invalid to send or if Facility is Special
      IF screen-group1 = 'E1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*AT SELECTION-SCREEN. "GDIMALIWAT - Disable Regular Facility 03/07/2013
*  CASE sy-ucomm.
*    WHEN 'UBA'.
*      "-- Dynamic Changing of Portion Parameter Per BA
*      PERFORM get_portions USING ba: 'PORT_LOW',
*                                     'PORT_HI'.
*    WHEN 'PRT'.
*      "-- Checking for Portion parameter
*      PERFORM portion_param_check.
*  ENDCASE.

*  IF 1800 = ba AND old = 'X'.  "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    MOVE '0400' TO ba.
*    MODIFY SCREEN.
*    MESSAGE e001(zerr).
*  ENDIF.
AT SELECTION-SCREEN.
  IF pa_ba2 = co_1800 AND pa_ddate = 'X'.
    MOVE '0400' TO pa_ba2.
    MODIFY SCREEN.
    MESSAGE e001(zerr).
  ENDIF.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  "-- Checking for Portion parameter
  "PERFORM portion_param_check. "GDIMALIWAT - Disable Regular Facility 03/07/2013
  "-- Check Due Date
  PERFORM duedate_param_check.
  "-- Retrieve condonation accounts
  "PERFORM get_condonation_accounts.

  PERFORM extraction.
* Commented backtracking 04/02/2013
*  "-- If record count was less than 100, perform 2nd iteration of extraction with 22 less days from input due date
*  IF pa_ddate EQ 'X' AND gv_count LT 100.
*    PERFORM backtrack.
*    PERFORM extraction.
*  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  EXTRACTION
*&---------------------------------------------------------------------*
*       Perform Dunning List Extraction
*----------------------------------------------------------------------*
FORM extraction .

  DATA: ls_dunnlist_header TYPE ty_dunnlist.
  FIELD-SYMBOLS: <fs_gsber>    TYPE ty_gsber.

  REFRESH gt_dunnlist.

  "-- For Old Selection
*  IF old = 'X'.  "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    PERFORM old_extract.
*  ELSE
  IF pa_new = 'X'.
    PERFORM special_extract.
  ELSEIF pa_ddate = 'X'.
    PERFORM extract_with_ddate.
  ENDIF.

*  IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    "-- Use dunnlist2
*    "-- Get Row Count of Local Table
*    DESCRIBE TABLE dunnlist2 LINES gv_cnt.
*  ELSE
  IF pa_new EQ 'X' OR pa_ddate EQ 'X'.
    "-- Use dunnlist3
    "-- Get Row Count of Local Table
    DESCRIBE TABLE gt_dunnlist LINES gv_cnt.
    "DESCRIBE TABLE dunnlist3 LINES gv_cnt.
  ENDIF.

  IF gv_cnt > 0.
    CLEAR ls_dunnlist_header.
    ls_dunnlist_header-gsber    =  'BA'.
    ls_dunnlist_header-rsg      =  'RSG'.
    ls_dunnlist_header-ableinh  =  'MRU'.
    ls_dunnlist_header-gpart    =  'BP'.
    ls_dunnlist_header-vkont    =  'CAN'.
    ls_dunnlist_header-printdoc =  'PRINTDOC'.
    ls_dunnlist_header-name     =  'NAME'.
    ls_dunnlist_header-addr     =  'ADDRESS'.
    ls_dunnlist_header-budat    =  'POST DT'.
    ls_dunnlist_header-current  =  'CURRENT'.
    ls_dunnlist_header-pua      =  'PUA'.
    ls_dunnlist_header-misc      = 'OTHER UNPAID'.
    ls_dunnlist_header-overdue  =  '61+_OVERDUE'.
    ls_dunnlist_header-total    =  'TOTAL'.
    ls_dunnlist_header-sortfld  =  'SEQ NO'.
    ls_dunnlist_header-device   =  'DEVICE'.
    ls_dunnlist_header-nodays   =  'DAYS'.
    ls_dunnlist_header-oc       =  'OC'.
    ls_dunnlist_header-oc2      =  'OC2'.
    INSERT ls_dunnlist_header INTO gt_dunnlist INDEX 1.

    "-- Download Data
    PERFORM download_data.

    "-- Make Distinct List
    SORT gt_gsber BY gsber.
    DELETE: ADJACENT DUPLICATES FROM gt_gsber,
            gt_gsber WHERE gsber EQ 'BA'.

    "-- Send Email Notifications per BA.
    "IF old EQ 'X' OR ddate EQ 'X'. "Only if Regular or Due Date "GDIMALIWAT - Disable Regular Facility 03/07/2013
    IF pa_ddate EQ 'X'.
      LOOP AT gt_gsber ASSIGNING <fs_gsber> WHERE gsber NE 'BA'.
        IF <fs_gsber>-gsber IS NOT INITIAL.
          PERFORM send_notification USING <fs_gsber>-gsber.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "-- Display table
    PERFORM display_report.
  ENDIF.
ENDFORM.                    " EXTRACTION

*&---------------------------------------------------------------------*
*&      Form  DUNNING
*&---------------------------------------------------------------------*
*       Process dunning
*----------------------------------------------------------------------*
FORM dunning.

  DATA: lv_flag           TYPE char1,
        lv_date_param     TYPE sy-datum,
        lv_current_date   TYPE sy-datum,
        lv_premise_param  TYPE sy-datum.

  DATA: "lv_amt            TYPE dfkkop-betrh,
        lv_total          TYPE dfkkop-betrh,
        "lv_msg            TYPE c LENGTH 25,
        lv_eqpt           TYPE egerh-equnr.
  "lv_pia            TYPE dfkkop-betrh.

  DATA: lv_printdoc       TYPE c LENGTH 20,
        lv_posdt          TYPE erdk-budat,
        lv_current        TYPE char15,
        lv_pua            TYPE char15,
        lv_rsg            TYPE char8,
        lv_overdue        TYPE char15,
        lv_misc           TYPE char15.

  DATA: lv_mindt          TYPE sy-datum,
        lv_maxdt          TYPE sy-datum,
        lv_diff           TYPE p.

  DATA: "lv_contacct       TYPE fkkvkp-vkont,
        lv_ableinh        TYPE eablg-ableinh,
        lv_vstelle        TYPE evbs-vstelle,
        lv_haus           TYPE evbs-haus.

  DATA: "lv_currdt         TYPE eabl-adatsoll,
        "lv_opbel          TYPE erdk-opbel,
        lv_mrid           TYPE eabl-ablbelnr,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw.
  "lv_adatsoll       TYPE eabl-adatsoll.

  DATA: lv_addr           TYPE char80.
  DATA: lv_sortfld        TYPE char6,
        lv_device         TYPE eabl-gernr.

  DATA: lt_dunn        TYPE TABLE OF ty_dunn,
        lt_dunn2       TYPE TABLE OF ty_dunn2,
        lt_probill     TYPE TABLE OF ty_probill,
        lt_oc          TYPE TABLE OF ty_oc.

  DATA: lv_begda          TYPE sy-datum,
        lv_endda          TYPE sy-datum.

  DATA: lv_blk_code       TYPE ever-abrsperr,
        lv_mro_subrc      TYPE sy-subrc,
        lv_doc_type_order TYPE bapieablh-mrdocumenttype VALUE '1'.

  DATA: lv_ok             TYPE c LENGTH 1,
        lv_count_dunn2    TYPE i.

  FIELD-SYMBOLS: <fs_dunn>         TYPE ty_dunn,
                 <fs_dunn2>        TYPE ty_dunn2,
                 <fs_probill>      TYPE ty_probill,
                 <fs_oc>           TYPE ty_oc,
                 <fs_dunnlist>     TYPE ty_dunnlist.

  "--- Determine Date used for Comparison
  lv_current_date = sy-datum.

*  IF old EQ 'X'. "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    lv_date_param = scheddt.
*  ELSE
  IF pa_ddate EQ 'X'.
    lv_date_param = gv_highest_duedate.
  ENDIF.

  "--- Reset values
  CLEAR:  lv_rsg,
          lv_printdoc,
          lv_addr,
          lv_posdt,
          lv_current,
          lv_pua,
          lv_misc,
          lv_overdue,
          lv_total,
          lv_sortfld,
          lv_device,
          lv_diff,
          lv_oc,
          lv_oc2,
          "lv_pia,
          lv_eqpt,
          lv_vstelle,
          lv_haus,
          lv_rsg,
          lv_maxdt,
          lv_mindt.

  REFRESH lt_dunn.
  REFRESH lt_dunn2.

  "--- Probill
  SELECT loobj1 lockr vkont
  INTO TABLE lt_probill
  FROM dfkklocks
  WHERE vkont = <fs_dunnhead>-contacct
  AND lockr = '9'.

  "-- DUNN List
  SELECT xblnr opbel budat bldat faedn SUM( betrh ) AS amount
    INTO TABLE lt_dunn
    FROM dfkkop
   WHERE vkont EQ <fs_dunnhead>-contacct
     "AND hvorg IN ('0100', '0200')
     AND augrd NE '05'
     AND augst EQ space
     AND faedn LE lv_date_param
     AND budat NE '00000000'
     AND abwbl EQ space
   GROUP BY xblnr budat bldat opbel faedn.

  gv_cnt = 0.
  lv_diff = 0.
  lv_mindt = '00000000'.
  lv_maxdt = '00000000'.

  SORT lt_dunn BY faedn ASCENDING.
  LOOP AT lt_dunn ASSIGNING <fs_dunn>.
    lv_flag = '0'.
    LOOP AT lt_probill ASSIGNING <fs_probill> WHERE loobj1+0(12) EQ <fs_dunn>-ficadoc.
      lv_flag = '1'.
    ENDLOOP.
    UNASSIGN <fs_probill>.

    IF lv_flag NE '1'.
      APPEND INITIAL LINE TO lt_dunn2 ASSIGNING <fs_dunn2>.
      <fs_dunn2>-docno    =  <fs_dunn>-docno.
      <fs_dunn2>-budat    =  <fs_dunn>-budat.
      <fs_dunn2>-bldat    =  <fs_dunn>-bldat.
      <fs_dunn2>-ficadoc  =  <fs_dunn>-ficadoc.
      <fs_dunn2>-faedn    =  <fs_dunn>-faedn.
      <fs_dunn2>-amount   =  <fs_dunn>-amount.
      gv_cnt = gv_cnt + 1.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_dunn>.

  SORT lt_dunn2 BY faedn ASCENDING.

  LOOP AT lt_dunn2 ASSIGNING <fs_dunn2>.
    lv_mindt = <fs_dunn2>-faedn.
    EXIT.
  ENDLOOP.
  UNASSIGN <fs_dunn2>.

  LOOP AT lt_dunn2 ASSIGNING <fs_dunn2>.
    IF <fs_dunn2>-amount NE 0.
      lv_printdoc = <fs_dunn2>-docno.
      lv_posdt    = <fs_dunn2>-budat.
      lv_maxdt    = <fs_dunn2>-faedn.
*      SELECT SUM( betrh )
*        INTO lv_pia
*        FROM dfkkop
*      WHERE opbel = <fs_dunn2>-ficadoc
*      AND   augdt LE lv_current_date
*      AND   augst = '9'
*      AND   ( hvorg = '0100' AND tvorg = '0010' ).
*      lv_current = lv_amt + lv_pia.
    ELSE.
      lv_current = 0.
      lv_printdoc = space.
      lv_posdt = space.
    ENDIF.
    EXIT.
  ENDLOOP.
  UNASSIGN <fs_dunn2>.

  IF lv_mindt EQ '00000000'.
    IF lv_maxdt EQ '00000000'.
      lv_diff = 0.
    ELSE.
      lv_diff = lv_current_date - lv_maxdt.
    ENDIF.
  ELSE.
    lv_diff = lv_current_date - lv_mindt.
  ENDIF.

*  IF old EQ 'X'. "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    lv_premise_param = scheddt.
*  ELSE
  IF pa_ddate EQ 'X'.
    "lv_premise_param = lv_maxdt - 7.
    lv_premise_param = lv_maxdt.
  ENDIF.

  DESCRIBE TABLE lt_dunn2 LINES lv_count_dunn2.

  IF lv_count_dunn2 NE 0.
    IF lv_diff > co_60." OR pa_ddate EQ'X'. "Don't check for 60 days if Due Date extraction
*      "--- Get Installation Number
*      SELECT SINGLE anlage
*        INTO (lv_anlage)
*        FROM ever
*       WHERE vkonto EQ <fs_dunnhead>-contacct.                "#EC WARNOK

*  ***( Get MRU )**************************************************************
      SELECT SINGLE ableinh
        INTO (lv_ableinh)
        FROM eanlh
       WHERE anlage EQ <fs_dunnhead>-anlage
         AND bis = '99991231'.

      SELECT ablhinw eablg~adatsoll eablg~ablbelnr
      INTO TABLE lt_oc
      FROM eablg
      INNER JOIN eabl ON eablg~ablbelnr = eabl~ablbelnr
      UP TO 1 ROWS
      WHERE anlage = <fs_dunnhead>-anlage
      AND ablesgr = '01'
      AND ablstat NE '0'
      ORDER BY eablg~adatsoll DESCENDING.

      READ TABLE lt_oc INDEX 1 ASSIGNING <fs_oc>.
      IF sy-subrc EQ 0.
        lv_oc = <fs_oc>-oc.
        "lv_adatsoll = <fs_oc>-adatsoll.
        lv_mrid = <fs_oc>-mrid.
      ENDIF.

      SELECT SINGLE meterreadingnot2
      INTO lv_oc2
      FROM zmwosb_s01p
      WHERE mridnumber = lv_mrid.

      "-- Call Function Module for fetching PUA
*      IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*        PERFORM get_due_balance USING    <fs_dunnhead>-contacct
*                                         <fs_dunnhead>-gpart
*                                CHANGING lv_total
*                                         lv_pua
*                                         lv_current.
*      ELSE.
      PERFORM get_due_balance_by_duedate USING    <fs_dunnhead>-contacct
                                                  <fs_dunnhead>-gpart
                                         CHANGING lv_total
                                                  lv_pua
                                                  lv_current
                                                  lv_overdue
                                                  lv_misc.
*      ENDIF.

*  ***( Get Equipment Number )*******************************************************
      SELECT SINGLE equnr
        INTO (lv_eqpt)
        FROM egerh INNER JOIN eastl ON egerh~logiknr = eastl~logiknr
       WHERE eastl~anlage = <fs_dunnhead>-anlage
         AND egerh~bis = '99991231'.                        "#EC WARNOK

      gv_cnt = 1.

*  ***( Get Premise Number )****************************************************
      SELECT vstelle
        INTO (lv_vstelle)
        FROM eanlh INNER JOIN eanl ON eanlh~anlage = eanl~anlage
        UP TO 1 ROWS
       WHERE eanlh~anlage = <fs_dunnhead>-anlage
         AND eanlh~ab LE lv_premise_param
         AND eanlh~bis GE lv_premise_param.
      ENDSELECT.

*  ***( Get Connection Object )******************************************************
      SELECT SINGLE haus
        INTO (lv_haus)
        FROM evbs
       WHERE vstelle = lv_vstelle.

*  ***( Get RSG )********************************************************************
      SELECT SINGLE regiogroup
        INTO lv_rsg
        FROM ehauisu
       WHERE haus = lv_haus.

      PERFORM get_address  CHANGING lv_addr.
      PERFORM get_sortfield CHANGING lv_sortfld lv_device lv_eqpt.
      SHIFT lv_printdoc LEFT DELETING LEADING '0'.

      "-- If overdue is greater than 80 and total is greater than or equal 0, include record to report
      IF lv_overdue > co_80 AND lv_total > co_0 AND lv_misc >= co_0.
        lv_ok = 'Y'.
        lv_endda = lv_begda = gv_highest_duedate.
        lv_begda+6(2) = '01'.
        CALL FUNCTION 'DETERMINE_END_OF_MONTH'
          EXPORTING
            i_datum = lv_begda
          IMPORTING
            e_tt    = lv_endda+6(2).

        IF lv_oc EQ '21' OR lv_oc EQ '22' OR lv_oc2 EQ '03' OR <fs_dunnhead>-gsber EQ '1800'.
          lv_ok = 'N'.
        ELSE.
          PERFORM check_mro_exists USING <fs_dunnhead>-gpart lv_begda lv_endda lv_mro_subrc lv_doc_type_order.
          IF lv_mro_subrc = 0.
            WRITE:/ text-036, <fs_dunnhead>-gpart.
            lv_ok = 'N'.
          ENDIF.
          PERFORM check_if_blocked USING <fs_dunnhead>-contacct lv_blk_code.
          IF NOT lv_blk_code IS INITIAL.
            WRITE:/ text-037, <fs_dunnhead>-gpart.
            lv_ok = 'N'.
          ENDIF.
        ENDIF.

        IF lv_ok EQ 'Y'.
          APPEND INITIAL LINE TO gt_dunnlist ASSIGNING <fs_dunnlist>.
          <fs_dunnlist>-gsber    = <fs_dunnhead>-gsber.
          <fs_dunnlist>-rsg      = lv_rsg.
          <fs_dunnlist>-ableinh  = lv_ableinh.
          <fs_dunnlist>-gpart    = <fs_dunnhead>-gpart.
          <fs_dunnlist>-vkont    = <fs_dunnhead>-contacct.
          <fs_dunnlist>-printdoc = lv_printdoc.
          <fs_dunnlist>-name     = <fs_dunnhead>-name.
          <fs_dunnlist>-addr     = lv_addr.
          <fs_dunnlist>-budat    = lv_posdt.
          <fs_dunnlist>-current  = lv_current.
          <fs_dunnlist>-pua      = lv_pua.
          <fs_dunnlist>-overdue  = lv_overdue.
          <fs_dunnlist>-misc     = lv_misc.
          <fs_dunnlist>-total    = lv_total.
          <fs_dunnlist>-sortfld  = lv_sortfld.
          <fs_dunnlist>-device   = lv_device.
          <fs_dunnlist>-nodays   = lv_diff.
          <fs_dunnlist>-oc       = lv_oc.
          <fs_dunnlist>-oc2      = lv_oc2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "DUNNING

*&---------------------------------------------------------------------*
*&      Form  CHECK_CLOSED
*&---------------------------------------------------------------------*
FORM check_closed CHANGING pv_note TYPE eablg-ablesgr.

  DATA: lv_diff       TYPE p,
        "lv_anlage     TYPE ever-anlage,
        lv_currdt     TYPE eabl-adatsoll,
        lv_opbel      TYPE erdk-opbel ##needed.
  "lv_dunn_tag   TYPE char1.
  "lv_x_flag     TYPE char1.

*  DATA: BEGIN  OF mr OCCURS 0,
*         adatsoll TYPE eabl-adatsoll,
*         ablesgr TYPE eablg-ablesgr,
*        END OF mr.
  DATA: lt_mr TYPE TABLE OF ty_mr.

  FIELD-SYMBOLS:  <fs_mr>       TYPE ty_mr,
                  <fs_mr2>      TYPE ty_mr,
                  <fs_billdoc>  TYPE ty_billdoc.
*  DATA: lv_mrnote         TYPE eablg-ablesgr.
*        lv_sched          TYPE eabl-adatsoll.

  "lv_mrnote = space.
  lv_diff = 0.

  SELECT adatsoll ablesgr
  INTO TABLE lt_mr
  FROM eablg
  WHERE anlage = <fs_dunnhead>-anlage
  AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
  AND ( ablesgr = '18' OR ablesgr = '13' ).

  SORT lt_mr BY adatsoll DESCENDING.

  IF sy-subrc = 0.
    LOOP AT lt_mr ASSIGNING <fs_mr>.
      IF <fs_mr>-ablesgr = '13'.
*** CHECK IF HAS CURRENT BILL
        REFRESH gt_mr.
        REFRESH gt_billdoc.
        lv_currdt = space.

        SELECT adatsoll ablesgr
        INTO TABLE gt_mr
        FROM eablg
        WHERE anlage = <fs_dunnhead>-anlage
        AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
        AND ( ablesgr = '01' OR ablesgr = '06' ).

        SORT gt_mr BY adatsoll DESCENDING.

        LOOP AT gt_mr ASSIGNING <fs_mr2>.
          lv_currdt = <fs_mr2>-adatsoll.
          EXIT.
        ENDLOOP.
        UNASSIGN <fs_mr2>.
        IF pa_ddate NE 'X'.
          lv_diff = sy-datum - lv_currdt.
        ELSE.
          lv_diff = gv_highest_duedate - lv_currdt.
        ENDIF.

        IF lv_diff LE '34'.
          REFRESH gt_billdoc.
          lv_opbel = space.
          SELECT belnr origdoc adatsoll
          INTO TABLE gt_billdoc
          FROM erch
          WHERE vkont = <fs_dunnhead>-contacct
          AND adatsoll EQ lv_currdt
          AND ( stornodat = '00000000' OR stornodat = space ).

          IF sy-subrc = 0.
            SORT gt_billdoc BY adatsoll DESCENDING.

            LOOP AT gt_billdoc ASSIGNING <fs_billdoc>.
              IF <fs_billdoc>-origdoc = '03'.
                PERFORM dunning.
                "lv_dunn_tag = 'X'.
              ELSE.
                CLEAR lv_opbel.
                SELECT erdk~opbel
                  INTO lv_opbel
                FROM erdk INNER JOIN erchc
                ON erdk~opbel = erchc~opbel
                INNER JOIN erch
                ON erchc~belnr = erch~belnr
                UP TO 1 ROWS
                WHERE erdk~invoiced = 'X' AND
                      erchc~invoiced = 'X' AND
                      erdk~intopbel = space AND
                      erchc~intopbel = space AND
                      erchc~belnr = <fs_billdoc>-belnr.
                ENDSELECT.
                IF sy-subrc = 0.
                  PERFORM dunning.
                  "lv_dunn_tag = 'X'.
                ENDIF.
              ENDIF.
              EXIT.
            ENDLOOP.
            UNASSIGN <fs_billdoc>.
          ENDIF.
        ENDIF.
      ENDIF.
      pv_note = <fs_mr>-ablesgr.
      EXIT.
    ENDLOOP.

  ENDIF.
ENDFORM.                    "CHECK_CLOSED

*&---------------------------------------------------------------------*
*&      Form  name_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_address CHANGING "pv_name TYPE fkkvk-vkbez
                           pv_addr TYPE char80.

*  DATA: lv_bpkind TYPE but000-bpkind.
*  DATA: lv_lastname TYPE c LENGTH 20,
*        lv_firstname TYPE c LENGTH 20.
  DATA: lv_addrnum TYPE but020-addrnumber.
  DATA: ls_address TYPE ty_address.

  CLEAR ls_address.
*  SELECT SINGLE name_last name_first bpkind
*    INTO (lv_lastname, lv_firstname, lv_bpkind)
*    FROM but000
*    WHERE partner = <fs_dunnhead>-gpart.
*
*  IF lv_bpkind = '0002'.
*    SELECT SINGLE name_org1
*    INTO (lv_firstname)
*    FROM but000 WHERE partner = <fs_dunnhead>-gpart.
*  ENDIF.

  SELECT SINGLE addrnumber INTO lv_addrnum FROM but020
  WHERE partner = <fs_dunnhead>-gpart.                      "#EC WARNOK

*  CONDENSE lv_firstname.
*  CONDENSE lv_lastname.
*
*  REPLACE ',' WITH space INTO lv_firstname.
*  REPLACE ',' WITH space INTO lv_lastname.
*
*  IF lv_lastname = lv_firstname.  " OR lastname = 'NO NAME'.
*    lv_lastname = space.
*  ENDIF.

*  CONCATENATE lv_firstname lv_lastname INTO pv_name SEPARATED BY space.
*  "TRANSLATE dunnlist-name TO UPPER CASE.

  SELECT SINGLE roomnumber floor house_num1 house_num2 str_suppl1 street
                str_suppl2 str_suppl3 location city2 city1 post_code1
  INTO (ls_address-roomnumber, ls_address-floor, ls_address-house_num1, ls_address-house_num2, ls_address-str_suppl1, ls_address-street,
        ls_address-str_suppl2, ls_address-str_suppl3, ls_address-location, ls_address-city2, ls_address-city1, ls_address-post_code1)
  FROM adrc
  WHERE addrnumber = lv_addrnum.                            "#EC WARNOK

  REPLACE ',' WITH space INTO ls_address-roomnumber.
  REPLACE ',' WITH space INTO ls_address-floor.
  REPLACE ',' WITH space INTO ls_address-house_num1.
  REPLACE ',' WITH space INTO ls_address-house_num2.
  REPLACE ',' WITH space INTO ls_address-str_suppl1.
  REPLACE ',' WITH space INTO ls_address-street.
  REPLACE ',' WITH space INTO ls_address-str_suppl2.
  REPLACE ',' WITH space INTO ls_address-str_suppl3.
  REPLACE ',' WITH space INTO ls_address-location.
  REPLACE ',' WITH space INTO ls_address-city2.
  REPLACE ',' WITH space INTO ls_address-city1.
  REPLACE ',' WITH space INTO ls_address-post_code1.

  IF ls_address-house_num1 = '0000'. ls_address-house_num1 = space. ENDIF.
  IF ls_address-street = 'NO NAME'.  ls_address-street = space. ENDIF.
  IF ls_address-city2 = 'NO NAME'.   ls_address-city2 = space. ENDIF.
  IF ls_address-city1 = 'NO NAME'.   ls_address-city1 = space. ENDIF.
  CONDENSE ls_address.
  TRANSLATE ls_address TO UPPER CASE.
  MOVE ls_address TO pv_addr.
ENDFORM.                    "name_address

*&---------------------------------------------------------------------*
*&      Form  get_sortfield
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sortfield CHANGING
                    pv_sortfld     TYPE char6
                    pv_device      TYPE eabl-gernr
                    pv_eqpt        TYPE egerh-equnr.

  SELECT SINGLE eqfnr sernr equnr
    INTO (pv_sortfld, pv_device, pv_eqpt)
  FROM v_equi
  WHERE equnr EQ pv_eqpt
  AND datbi EQ '99991231'
  AND eqfnr NE '0'.                                         "#EC WARNOK

ENDFORM.                    "get_sortfield

*&---------------------------------------------------------------------*
*&      Form  CHECK_MRO_EXISTS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_mro_exists  USING    pv_buspartner TYPE fkkvkp-gpart
                                pv_begda      TYPE sy-datum
                                pv_endda      TYPE sy-datum
                                pv_subrc      TYPE sy-subrc
                                pv_doc_type   TYPE bapieablh-mrdocumenttype.

  CLEAR pv_subrc.
  DATA: lt_mrdoc TYPE TABLE OF bapieabl.
*** alternatively, we can select directly from EABL with ABLSTAT = 0.

  CALL FUNCTION 'BAPI_MTRREADDOC_GETLIST'
    EXPORTING
      customer         = pv_buspartner
      targetmrdatefrom = pv_begda
      targetmrdateto   = pv_endda
      mrdocumenttype   = pv_doc_type
    TABLES
      mrdocumentdata   = lt_mrdoc.

  IF lt_mrdoc[] IS INITIAL.
    pv_subrc = 4.
  ENDIF.
ENDFORM.                    " CHECK_MRO_EXISTS

**&---------------------------------------------------------------------* GDIMALIWAT - TN#30567 Optimization of ZDUNNLIST Program 03/18/2013
**&      Form  CONVERT_PORTION
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM convert_portion USING p_port.
*  IF p_port+5 GE 1 AND p_port+5 LE 9.
*    CONCATENATE p_port(5) '0' p_port+5 INTO new_portion.
*  ELSE.
*    new_portion = 'BLANK'.
*  ENDIF.
*ENDFORM.                    "CONVERT_PORTION

*&---------------------------------------------------------------------*
*&      Form  check_if_blocked
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_if_blocked  USING    pv_cac TYPE fkkvkp-vkont
                                pv_blk TYPE ever-abrsperr.
  CLEAR pv_blk.
  SELECT SINGLE abrsperr
  INTO pv_blk
  FROM ever
  WHERE vkonto = pv_cac
    AND abrsperr IS NOT NULL.                               "#EC WARNOK

ENDFORM.                    " CHECK_IF_BLOCKED

*&---------------------------------------------------------------------*
*&      Form  SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_notification USING pv_gsber TYPE fkkvkp-gsber.
  "-- Checks if Checkbox is Ticked
  CHECK pa_email EQ 'X'.

  DATA: lv_sender     TYPE so_rec_ext VALUE 'IT.OPERATION@MAYNILADWATER.COM.PH',  "Email Sender
        lv_receiver   TYPE so_recname,     "Email Receiver
        lv_subject    TYPE so_obj_des,                                            "Email Subject
        lt_messages   TYPE STANDARD TABLE OF solisti1,
        lt_receivers  TYPE STANDARD TABLE OF somlreci1,
        "lv_copy       TYPE c LENGTH 1,
        lv_ba_name    TYPE c LENGTH 50,
        lv_message    TYPE c LENGTH 250.

  DATA: lt_email      TYPE TABLE OF zdunn_email_list.

  FIELD-SYMBOLS:
        <fs_email>    TYPE zdunn_email_list.

  DATA: ls_messages   TYPE solisti1,
        ls_receivers  TYPE somlreci1.


  DATA: lr_gsber      TYPE RANGE OF fkkvkp-gsber,
        ls_gsber      LIKE LINE  OF lr_gsber.

  DATA: "lv_port_range    TYPE c LENGTH 15,
        lv_duedate_range TYPE c LENGTH 23,
        lv_ddate_low     TYPE string,
        lv_ddate_hi      TYPE string.

  DATA: lv_msg1            TYPE string,
        lv_msg2            TYPE string,
        lv_msg3            TYPE string,
        lv_msg4            TYPE string,
        lv_msg5            TYPE string,
        lv_msg6            TYPE string,
        lv_msg7            TYPE string,
        lv_msg8            TYPE string,
        lv_msg9            TYPE string,
        lv_msg10           TYPE string.

  "-- Assign values to variables
  lv_subject  = text-040.
  lv_msg1     = text-041.
  lv_msg2     = text-042.
  lv_msg3     = text-043.
  lv_msg4     = text-044.
  lv_msg5     = text-045.
  lv_msg6     = text-046.
  lv_msg7     = text-047.
  lv_msg8     = text-048.
  lv_msg9     = text-049.
  lv_msg10    = text-050.
  lv_receiver = text-051.

  "-- Macro Definition for Adding Receivers
  DEFINE mc_append_receivers.
    ls_receivers-receiver = &1.
    if &2 eq 'X'.
      ls_receivers-copy   = 'X'.
    endif.
    ls_receivers-rec_type = 'U'.
    append ls_receivers to lt_receivers.
    clear  lt_receivers.
  END-OF-DEFINITION.

  "-- Macro Definition for Adding Messages
  DEFINE mc_append_messages.
    ls_messages = &1.
    append  ls_messages to lt_messages.
  END-OF-DEFINITION.

  "-- Macro Definition for Ranges
  DEFINE mc_ranges.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  "-- Check if User is valid to send Email
  IF gv_valid_send EQ 0.
    "-- Initialize BA Range
    IF pv_gsber EQ '1100'.
      mc_ranges: ls_gsber lr_gsber pv_gsber,
                 ls_gsber lr_gsber `PS`,
                 ls_gsber lr_gsber `A21`.
    ELSE.
      mc_ranges: ls_gsber lr_gsber pv_gsber,
                 ls_gsber lr_gsber `PS`,
                 ls_gsber lr_gsber `OCF`.
    ENDIF.
*    mc_ranges: ls_gsber lr_gsber pv_gsber,
*               ls_gsber lr_gsber `PS`.

    "-- Populate Email Receivers
    CLEAR   lt_receivers.
    REFRESH lt_receivers.

    "-- Fetch from database table (Per BA)
    SELECT *
    FROM   zdunn_email_list
      INTO CORRESPONDING FIELDS OF TABLE lt_email
    WHERE  ba_code IN lr_gsber.

    IF sy-subrc EQ 0.

      LOOP AT lt_email ASSIGNING <fs_email>.
        IF <fs_email>-ba_name IS NOT INITIAL.
          CONCATENATE <fs_email>-ba_name `,` INTO lv_ba_name.
        ENDIF.

        mc_append_receivers:
          <fs_email>-com_head_email <fs_email>-com_head_copy.

        IF <fs_email>-bo_head_email IS NOT INITIAL.
          mc_append_receivers:
            <fs_email>-bo_head_email  <fs_email>-bo_head_copy.
        ENDIF.
      ENDLOOP.

*    m_append_receivers: 'bernadette.its@mayniladwater.com.ph' '',
*                        'michael.its@mayniladwater.com.ph'    '',
*                        'randy.its@mayniladwater.com.ph'      '',
*                        'reena.its@mayniladwater.com.ph'      ''.
      mc_append_receivers: lv_receiver  ''.
      "-- Populate Email Body
      CLEAR   lt_messages.
      REFRESH lt_messages.

      "-- Email Body
*      IF old EQ 'X'. "--Regular    "GDIMALIWAT - Disable Regular Facility 03/07/2013
*        "-- Prepare Portion Range, to be included in email message
*        CONCATENATE port_low
*                    port_hi
*               INTO lv_port_range SEPARATED BY `-`.
*
*        CONCATENATE  `Please be informed that the dunning list for`
*                     lv_ba_name     "-- BA Name
*                     `Portion`
*                     lv_port_range  "-- Portion Range
*                     `is now available in \\saprep01.mayniladsap.com\Mtr_RdngINDRA\ZDunning.`
*               INTO  lv_message
*               SEPARATED BY space.
*      ELSE
      IF pa_ddate EQ 'X'. "--Due Date
        CLEAR: lv_ddate_low, lv_ddate_hi.
        CONCATENATE gv_lowest_duedate+4(2) gv_lowest_duedate+6(2) gv_lowest_duedate(4) INTO lv_ddate_low SEPARATED BY '/'.
        CONCATENATE gv_highest_duedate+4(2) gv_highest_duedate+6(2) gv_highest_duedate(4) INTO lv_ddate_hi SEPARATED BY '/'.
        IF gv_lowest_duedate EQ gv_highest_duedate.
          MOVE lv_ddate_hi TO lv_duedate_range.
        ELSE.
          CONCATENATE lv_ddate_low
                      lv_ddate_hi
                 INTO lv_duedate_range SEPARATED BY `-`.
        ENDIF.

        CONCATENATE  lv_msg1
                     lv_ba_name     "-- BA Name
                     lv_msg2
                     lv_duedate_range  "-- Due Date Range
                     lv_msg3
                     pa_file
               INTO  lv_message
               SEPARATED BY space.
      ENDIF.

      mc_append_messages:
          lv_msg4,
          '',
          lv_message,
          lv_msg5,
          '',
          lv_msg6,
          '',
          lv_msg7,
          lv_msg8,
          lv_msg9,
          lv_msg10.

      "-- Call Custom Function Module
      CALL FUNCTION 'ZFM_SEND_SIMPLE_EMAIL'
        EXPORTING
          sender       = lv_sender
          subject      = lv_subject
        TABLES
          it_messages  = lt_messages
          it_receivers = lt_receivers.
    ENDIF.
  ENDIF.
ENDFORM.                    " SEND_NOTIFICATION

**&---------------------------------------------------------------------*   "GDIMALIWAT - Disable Regular Facility 03/07/2013
**&      Form  PORTION_PARAM_CHECK
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM portion_param_check .
*  IF port_low IS NOT INITIAL AND port_hi IS NOT INITIAL.
*    IF port_low GT port_hi.
*      MESSAGE e000(zosb) WITH 'Portion Low is Greater than Portion High!'.
*    ENDIF.
*  ENDIF.
*
*  IF sy-ucomm NE 'PRT'.
*    IF old EQ 'X'.
*      IF port_low IS INITIAL.
*        MESSAGE e000(zosb)   WITH 'Please input Portion Low!'.
*      ELSEIF port_hi IS INITIAL.
*        MESSAGE e000(zosb)   WITH 'Please input Portion High!'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " PORTION_PARAM_CHECK

**&---------------------------------------------------------------------*   "GDIMALIWAT - Disable Regular Facility 03/07/2013
**&      Form  GET_PORTIONS
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM get_portions  USING p_ba
*                         lv_port.
*  port_low = ``.
*  port_hi  = ``.
*
*  DATA: it_list  TYPE vrm_values,
*        it_list2 TYPE vrm_values,
*        wa_list  TYPE vrm_value,
*        wa_list2 TYPE vrm_value.
*
*  DATA: lv_ba    TYPE te420-termschl,
*        lv_start TYPE i,
*        lv_end   TYPE i.
*
*  REFRESH: it_list, it_list2.
*
*  CONCATENATE ba+0(2) `%` INTO lv_ba.
*
*  SELECT termschl INTO wa_list-key
*  FROM   te420
*  WHERE  termschl LIKE lv_ba.
*    APPEND wa_list TO it_list.
*  ENDSELECT.
*
*  IF sy-subrc EQ 0.
*    DELETE it_list WHERE key CP '*A'.
*
*    LOOP AT it_list INTO wa_list.
*      IF wa_list-key CP '*BG*' AND strlen( wa_list-key ) LE 7.
*        PERFORM convert_portion USING wa_list-key.
*
*        IF new_portion NE 'BLANK'.
*          wa_list-key = new_portion.
*          CLEAR new_portion.
*        ENDIF.
*
*        wa_list-key = wa_list-key.
*
*        lv_end      = strlen( wa_list-key ).
*        lv_start    = lv_end - 2.
*
*        wa_list2-key = wa_list-key+lv_start(lv_end).
*        APPEND wa_list2 TO it_list2.
*      ENDIF.
*    ENDLOOP.
*
*    SORT it_list2.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id     = lv_port
*        values = it_list2.
**      EXCEPTIONS
**        id_illegal_name = 1
**        OTHERS          = 2.
*  ENDIF.
*ENDFORM.                    " GET_PORTIONS
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM populate_ba .
  DATA: lt_list TYPE vrm_values,
        ls_list TYPE vrm_value.

  DEFINE mc_append_list.
    ls_list-key = &1.
    append ls_list to lt_list.
    clear  ls_list.
  END-OF-DEFINITION.

  mc_append_list: '0200', '0300', '0400', '0500', '0600', '0700', '0800', '0900', '1000', '1100', '1200', '1700'.

*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'PA_BA'
*      values = lt_list.
**    EXCEPTIONS
**      id_illegal_name = 1
**      OTHERS          = 2.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'PA_BA2'
      values = lt_list.
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.
ENDFORM.                    " POPULATE_BA

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM select_file_path  USING    pv_path TYPE rlgrap-filename.
  DATA :
    "lv_subrc  TYPE sy-subrc,
    lv_tab TYPE string,
    lv_window_title TYPE string,
    lv_initial_folder TYPE string.

  "-- Assign values to variables
  lv_window_title   = text-022.
  lv_initial_folder = text-023.

  "-- Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = lv_window_title
      initial_folder  = lv_initial_folder
    CHANGING
      selected_folder = lv_tab.

  "-- Write path on input area
  IF lv_tab IS NOT INITIAL.
    CONCATENATE lv_tab `\` INTO pv_path.
  ENDIF.
ENDFORM.                    " SELECT_FILE_PATH

**&---------------------------------------------------------------------*   "GDIMALIWAT - Disable Regular Facility 03/07/2013
**&      Form  POPULATE_IT_PORTLIST
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM populate_it_portlist.
*  DATA: lv_start    TYPE i VALUE  1,
*        lv_end      TYPE i,
*        lv_diff     TYPE i,
*        lv_len      TYPE i.
*
*  DATA: lv_ba       TYPE c LENGTH 2,
*        lv_portion  TYPE c LENGTH 2,
*        lv_termschl TYPE te420-termschl.
*
*  DATA: lv_tmp_portion  TYPE te420-termschl.
*
*  IF port_low IS NOT INITIAL.
*    lv_start = port_low - 1.
*  ENDIF.
*
*  lv_end   = port_hi.
*  lv_diff  = lv_end - lv_start.
*
*  lv_ba    = ba+0(2).
*
*  IF port_low IS NOT INITIAL.
*    lv_start = port_low.
*  ENDIF.
*
*  DO lv_diff TIMES.
*    lv_portion = lv_start.
*    CONCATENATE lv_ba `_BG` lv_portion INTO lv_tmp_portion.
*
*    SELECT SINGLE termschl INTO lv_termschl
*    FROM   te420
*    WHERE  termschl = lv_tmp_portion.
*
*    IF sy-subrc EQ 0.
*      APPEND INITIAL LINE TO gt_portlist ASSIGNING <fs_portlist>.
*
*      lv_len     = strlen( lv_portion ).
*      lv_portion = lv_start.
*      CONCATENATE lv_ba `_BG` lv_portion INTO <fs_portlist>-portion.
*
*    ENDIF.
*    lv_start = lv_start + 1.
*  ENDDO.
*ENDFORM.                    " POPULATE_IT_PORTLIST

**&---------------------------------------------------------------------*   GDIMALIWAT - TN#30567 Optimization of ZDUNNLIST Program 03/18/2013
**&      Form  GET_DUE_BALANCE
**&---------------------------------------------------------------------*
**       Subroutine for Fetching Due Balance of an Account
**----------------------------------------------------------------------*
*FORM get_due_balance  USING    pv_can     TYPE vkont_kk
*                               pv_bp      TYPE bu_partner
*                      CHANGING pv_balance TYPE betrh_kk
*                               pv_pua     TYPE char15
*                               pv_current TYPE char15.
*
*  DATA: lt_selection   TYPE TABLE OF bapifkkopselhead,
*        lt_balances    TYPE TABLE OF bapifkkepos,
*
*        lv_payment     TYPE char15.
*
*  FIELD-SYMBOLS:
*        <fs_selection> TYPE bapifkkopselhead,
*        <fs_balances>  TYPE bapifkkepos.
*
*  "-- Reinitialize Variables
*  CLEAR: pv_balance,
*         pv_pua,
*         pv_current.
*
*  "-- Reinitialize Tables
*  REFRESH: lt_selection,
*           lt_balances.
*
*  "-- Append Selections
*  APPEND INITIAL LINE TO lt_selection ASSIGNING <fs_selection>.
*  <fs_selection>-buspartner = pv_bp.
*  <fs_selection>-cont_acct  = pv_can.
*
*  "-- Call FM for Fetching Total Due Amount
*  CALL FUNCTION 'BAPI_CTRACCONTRACTACCOUNT_GBAL'
*    TABLES
*      mainselections = lt_selection
*      balanceitems   = lt_balances.
*
*  "-- Check if there are Due Amount
*  IF lt_balances[] IS NOT INITIAL.
*    SORT lt_balances BY net_date DESCENDING.
*
*    "-- Iterate per Due Amount
*    LOOP AT lt_balances ASSIGNING <fs_balances>.
*      IF <fs_balances>-doc_type NE 'AB'.
*        "-- Summarize Due Amounts
*        pv_balance = pv_balance + <fs_balances>-amount.
*      ELSE.
*        "-- If Installment Plan and Already Due
*        IF <fs_balances>-disc_due LE sy-datum.
*          "-- Summarize Due Amounts
*          pv_balance = pv_balance + <fs_balances>-amount.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    pv_balance = 0.
*  ENDIF.
*
*  "-- Get Current Debit
*  PERFORM get_current_debit  USING    pv_can
*                                      pv_bp
*                             CHANGING pv_current.
*
*  "-- PUA Computation
*  pv_pua = pv_balance - pv_current.
*ENDFORM.                    " GET_DUE_BALANCE

*&---------------------------------------------------------------------*
*&      Form  GET_DUE_BALANCE_BY_DUEDATE
*&---------------------------------------------------------------------*
*       Subroutine for Fetching Due Balance of an Account
*----------------------------------------------------------------------*
FORM get_due_balance_by_duedate  USING    pv_can     TYPE vkont_kk
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
        AND mwskz EQ 'E0'.

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
        AND mwskz EQ 'E0'.

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
        AND mwskz EQ 'E0'.

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
ENDFORM.                    " GET_DUE_BALANCE_BY_DUEDATE

**&---------------------------------------------------------------------*
**&      Form  GET_CURRENT_DEBIT
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM get_current_debit  USING    pv_can     TYPE vkont_kk
*                                 pv_bp      TYPE bu_partner
*                        CHANGING pv_current TYPE char15.
*
*  TYPES: BEGIN OF ty_current,
*           doc_no     TYPE opbel_kk,
*           current    TYPE betrw_kk,
*         END OF ty_current.
*
*  DATA:  lt_current   TYPE TABLE OF ty_current,
*         lv_due_date  TYPE faedn_kk,
*         lv_doc_no    TYPE opbel_kk,
*         lv_current   TYPE betrw_kk.
*
*  FIELD-SYMBOLS:
*         <fs_current> TYPE ty_current.
*
*  "-- Reinitialize Variables
*  CLEAR: lv_due_date, lv_doc_no, pv_current, lv_current.
*
*  "-- Get Document Number of Current Billing
*  SELECT faedn opbel
*    INTO (lv_due_date, lv_doc_no)
*  FROM   dfkkop
*  WHERE  gpart EQ pv_bp
*  AND    vkont EQ pv_can
*  AND    hvorg EQ '0100' "blart NE 'BA'
*  ORDER BY faedn DESCENDING.
*    EXIT.
*  ENDSELECT.
*
*  "-- Reinitialize Local Table for Current
*  REFRESH: lt_current.
*
*  "-- Get Current Debit Amount
*  SELECT opbel betrw
*    INTO TABLE lt_current
*  FROM   dfkkopk
*  WHERE  opbel EQ lv_doc_no.
*
*  "-- Get Summation of Current
*  LOOP AT lt_current ASSIGNING <fs_current>.
*    lv_current = lv_current + <fs_current>-current.
*  ENDLOOP.
*
*  "-- Always positive values
*  pv_current = abs( lv_current ).
*ENDFORM.                    " GET_CURRENT_DEBIT

**&---------------------------------------------------------------------*
**&      Form  GET_CURRENT_DEBIT_BY_DUEDATE
**&---------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*FORM get_current_debit_by_duedate  USING    pv_can     TYPE vkont_kk
*                                            pv_bp      TYPE bu_partner
*                                CHANGING    pv_current TYPE char15.
*
*  TYPES: BEGIN OF ty_current,
*           doc_no     TYPE opbel_kk,
*           current    TYPE betrw_kk,
*         END OF ty_current.
*
*  DATA:  lt_current   TYPE TABLE OF ty_current,
*         lv_due_date  TYPE faedn_kk,
*         lv_doc_no    TYPE opbel_kk,
*         lv_current   TYPE betrw_kk.
*
*  FIELD-SYMBOLS:
*         <fs_current> TYPE ty_current.
*
*  "-- Reinitialize Variables
*  CLEAR: lv_due_date, lv_doc_no, pv_current, lv_current.
*
*  "-- Get Document Number of Current Billing
*  SELECT faedn opbel
*    INTO (lv_due_date, lv_doc_no)
*  FROM   dfkkop
*  WHERE  gpart EQ pv_bp
*  AND    vkont EQ pv_can
*  AND    hvorg EQ '0100' "blart NE 'BA'
*  AND    faedn IN so_ddate
*  ORDER BY faedn DESCENDING.
*    EXIT.
*  ENDSELECT.
*
*  "-- Reinitialize Local Table for Current
*  REFRESH: lt_current.
*
*  "-- Get Current Debit Amount
*  SELECT opbel betrw
*    INTO TABLE lt_current
*  FROM   dfkkopk
*  WHERE  opbel EQ lv_doc_no.
*
*  "-- Get Summation of Current
*  LOOP AT lt_current ASSIGNING <fs_current>.
*    lv_current = lv_current + <fs_current>-current.
*  ENDLOOP.
*
*  "-- Always positive values
*  pv_current = abs( lv_current ).
*ENDFORM.                    " GET_CURRENT_DEBIT_BY_DUEDATE

*&---------------------------------------------------------------------*
*&      Form  IS_VALID_TO_SEND_EMAIL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM is_valid_to_send_email CHANGING pv_subrc TYPE sysubrc.

  DATA: lv_profile  TYPE agprofile.

  CONSTANTS:  co_role  TYPE agr_name VALUE 'Z_IT_ANALYST'.

  "-- Reinitialize Variable
  CLEAR: pv_subrc.

  "-- Check if PS User
  SELECT SINGLE profile
  INTO lv_profile
  FROM ust04
  WHERE bname EQ sy-uname.                                  "#EC WARNOK

  IF sy-subrc EQ 0.
    SELECT SINGLE profile
    INTO lv_profile
    FROM agr_1016
    WHERE profile   EQ lv_profile
      AND agr_name  EQ co_role.                             "#EC WARNOK
  ENDIF.

  "-- Pass Result
  pv_subrc = sy-subrc.

  "-- FOR DEBUGGING PURPOSES - GDIMALIWAT - Disallow e-mail notifications
*  IF sy-uname eq 'GDIMALIWAT'.
*    pv_subrc = '0'.
*  ENDIF.
ENDFORM.                    " IS_VALID_TO_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download Extracted Data
*----------------------------------------------------------------------*
FORM download_data .
  DATA: lv_ba_port TYPE c LENGTH 11,
        lv_file    TYPE rlgrap-filename.

  DATA: lv_string  TYPE string,
        ls_gsber   TYPE ty_gsber.

  CONSTANTS:  co_separator TYPE c LENGTH 1 VALUE '|'.

  FIELD-SYMBOLS:  <fs_dunnlist> TYPE ty_dunnlist.

  CASE 'X'.
*    WHEN old.              "GDIMALIWAT - Disable Regular Facility 03/07/2013
*      "-- e.g. 0200(01-17)
*      CONCATENATE ba `(`
*                  port_low `-`
*                  port_hi  `)`
*             INTO lv_ba_port.
*      "-- e.g. \\172.18.1.240\Mtr_RdngINDRA\ZDunning\<folder>\0200(01-17)20120229.txt
*      CONCATENATE spl_file
*                lv_ba_port
*                sy-datum
*                'dunn.txt'
*           INTO lv_file.

    WHEN pa_new.
      "-- e.g. 0000
      lv_ba_port = '0000-'.
      "-- e.g. \\172.18.1.240\Mtr_RdngINDRA\ZDunning\<folder>\0200(01-17)20120229.txt
      CONCATENATE pa_file
                lv_ba_port
                sy-datum
                text-039
           INTO lv_file.
    WHEN pa_ddate.
      "-- e.g. 0000
      CONCATENATE pa_ba2 `-`
             INTO lv_ba_port.
      "-- e.g. \\172.18.1.240\Mtr_RdngINDRA\ZDunning\<folder>\0200(01-17)20120229.txt
      CONCATENATE pa_file
                lv_ba_port
                gv_highest_duedate
                text-039
           INTO lv_file.
  ENDCASE.

*  WRITE lv_file TO rpt.
  "  OPEN DATASET rpt FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

*  IF old EQ 'X'.         "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    LOOP AT dunnlist2.
*      IF dunnlist2-gsber NE 'BA'.
*        "-- If not Header (not Column Names)
*        REPLACE ALL OCCURRENCES OF ',' IN dunnlist2-pua WITH ''.
*        CONDENSE dunnlist2-pua.
*        IF dunnlist2-pua GT 0.
*          "TRANSFER dunnlist2 TO rpt.
*          TRANSFER dunnlist2 TO lv_file.
*        ENDIF.
*      ELSE.
*        "-- If Header (Column Names)
*        "TRANSFER dunnlist2 TO rpt.
*        TRANSFER dunnlist2 TO lv_file.
*      ENDIF.
*
*      "-- Append to Main Itab
*      IF dunnlist2-gsber NE 'BA'.
*        PERFORM append_main_itab USING dunnlist2-gsber
*                                       dunnlist2-rsg
*                                       dunnlist2-ableinh
*                                       dunnlist2-gpart
*                                       dunnlist2-vkont
*                                       dunnlist2-printdoc
*                                       dunnlist2-name
*                                       dunnlist2-addr
*                                       dunnlist2-budat
*                                       dunnlist2-current
*                                       dunnlist2-pua
*                                       dunnlist2-total
*                                       dunnlist2-sortfld
*                                       dunnlist2-device
*                                       dunnlist2-nodays
*                                       dunnlist2-oc
*                                       dunnlist2-oc2.
*
*        "-- Append to Itab of BA's
*        wa_gsber-gsber = dunnlist2-gsber.
*        APPEND wa_gsber TO it_gsber.
*        CLEAR  wa_gsber.
*      ENDIF.
*    ENDLOOP.
*  ELSE
  IF pa_new EQ 'X' OR pa_ddate EQ 'X'.
    LOOP AT gt_dunnlist ASSIGNING <fs_dunnlist>.
      IF <fs_dunnlist>-gsber NE 'BA'.
        "-- If not Header (not Column Names)
        REPLACE ALL OCCURRENCES OF ',' IN <fs_dunnlist>-pua WITH ''.
        CONDENSE <fs_dunnlist>-pua.
        "IF <fs_dunnlist>-pua GT 0.
        "TRANSFER dunnlist3 TO rpt.

        CONCATENATE <fs_dunnlist>-gsber
                    <fs_dunnlist>-rsg
                    <fs_dunnlist>-ableinh
                    <fs_dunnlist>-gpart
                    <fs_dunnlist>-vkont
                    <fs_dunnlist>-printdoc
                    <fs_dunnlist>-name
                    <fs_dunnlist>-addr
                    <fs_dunnlist>-budat
                    <fs_dunnlist>-current
                    <fs_dunnlist>-pua
                    <fs_dunnlist>-overdue
                    <fs_dunnlist>-misc
                    <fs_dunnlist>-total
                    <fs_dunnlist>-sortfld
                    <fs_dunnlist>-device
                    <fs_dunnlist>-nodays
                    <fs_dunnlist>-oc
                    <fs_dunnlist>-oc2 INTO lv_string SEPARATED BY co_separator RESPECTING BLANKS.
        TRANSFER lv_string TO lv_file.
        "ENDIF.
      ELSE.
        "-- If Header (Column Names)
        "TRANSFER dunnlist3 TO rpt.
        "TRANSFER dunnlist3 TO lv_file.
        CONCATENATE <fs_dunnlist>-gsber
                      <fs_dunnlist>-rsg
                      <fs_dunnlist>-ableinh
                      <fs_dunnlist>-gpart
                      <fs_dunnlist>-vkont
                      <fs_dunnlist>-printdoc
                      <fs_dunnlist>-name
                      <fs_dunnlist>-addr
                      <fs_dunnlist>-budat
                      <fs_dunnlist>-current
                      <fs_dunnlist>-pua
                      <fs_dunnlist>-overdue
                      <fs_dunnlist>-misc
                      <fs_dunnlist>-total
                      <fs_dunnlist>-sortfld
                      <fs_dunnlist>-device
                      <fs_dunnlist>-nodays
                      <fs_dunnlist>-oc
                      <fs_dunnlist>-oc2 INTO lv_string SEPARATED BY co_separator RESPECTING BLANKS.
        TRANSFER lv_string TO lv_file.
      ENDIF.

      "-- Append to Main Itab
      IF <fs_dunnlist>-gsber NE 'BA'.
        PERFORM append_main_itab2 USING <fs_dunnlist>-gsber
                                        <fs_dunnlist>-rsg
                                        <fs_dunnlist>-ableinh
                                        <fs_dunnlist>-gpart
                                        <fs_dunnlist>-vkont
                                        <fs_dunnlist>-printdoc
                                        <fs_dunnlist>-name
                                        <fs_dunnlist>-addr
                                        <fs_dunnlist>-budat
                                        <fs_dunnlist>-current
                                        <fs_dunnlist>-pua
                                        <fs_dunnlist>-overdue
                                        <fs_dunnlist>-misc
                                        <fs_dunnlist>-total
                                        <fs_dunnlist>-sortfld
                                        <fs_dunnlist>-device
                                        <fs_dunnlist>-nodays
                                        <fs_dunnlist>-oc
                                        <fs_dunnlist>-oc2.

        "-- Append to Itab of BA's
        ls_gsber-gsber = <fs_dunnlist>-gsber.
        APPEND ls_gsber TO gt_gsber.
        CLEAR  ls_gsber.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_dunnlist>.
  ENDIF.

  "CLOSE DATASET rpt.
  CLOSE DATASET lv_file.
ENDFORM.                    " DOWNLOAD_DATA

**&---------------------------------------------------------------------*   GDIMALIWAT - TN#30567 Optimization of ZDUNNLIST Program 03/18/2013
**&      Form  APPEND_MAIN_ITAB
**&---------------------------------------------------------------------*
**   Append to Main Itab
**----------------------------------------------------------------------*
*FORM append_main_itab  USING    pv_gsber    pv_rsg    pv_ableinh    pv_gpart    pv_vkont    pv_printdoc
*                                pv_name     pv_addr   pv_budat      pv_current  pv_pua      pv_total
*                                pv_sortfld  pv_device pv_nodays     pv_oc       pv_oc2.
*
*  "-- Append to Main Itab
*  APPEND INITIAL LINE TO gt_main ASSIGNING <fs_main>.
*
*  <fs_main>-gsber    = pv_gsber.
*  <fs_main>-rsg      = pv_rsg.
*  <fs_main>-ableinh  = pv_ableinh.
*  <fs_main>-gpart    = pv_gpart.
*  <fs_main>-vkont    = pv_vkont.
*  <fs_main>-printdoc = pv_printdoc.
*  <fs_main>-name     = pv_name.
*  <fs_main>-addr     = pv_addr.
*  <fs_main>-budat    = pv_budat.
*  <fs_main>-current  = pv_current.
*  <fs_main>-pua      = pv_pua.
*  <fs_main>-total    = pv_total.
*  <fs_main>-sortfld  = pv_sortfld.
*  <fs_main>-device   = pv_device.
*  <fs_main>-nodays   = pv_nodays.
*  <fs_main>-oc       = pv_oc.
*  <fs_main>-oc2      = pv_oc2.
*ENDFORM.                    " APPEND_MAIN_ITAB
*&---------------------------------------------------------------------*
*&      Form  APPEND_MAIN_ITAB2
*&---------------------------------------------------------------------*
*   Append to Main Itab
*----------------------------------------------------------------------*
FORM append_main_itab2  USING   pv_gsber      TYPE fkkvkp-gsber
                                pv_rsg        TYPE char8
                                pv_ableinh    TYPE eablg-ableinh
                                pv_gpart      TYPE fkkvkp-gpart
                                pv_vkont      TYPE fkkvkp-vkont
                                pv_printdoc   TYPE char12
                                pv_name       TYPE fkkvk-vkbez
                                pv_addr       TYPE char80
                                pv_budat      TYPE char8
                                pv_current    TYPE char16
                                pv_pua        TYPE char15
                                pv_overdue    TYPE char15
                                pv_misc       TYPE char15
                                pv_total      TYPE char15
                                pv_sortfld    TYPE char6
                                pv_device     TYPE eabl-gernr
                                pv_nodays     TYPE char4
                                pv_oc         TYPE char4
                                pv_oc2        TYPE char4.

  FIELD-SYMBOLS:  <fs_main2>  TYPE ty_main.

  "-- Append to Main Itab
  APPEND INITIAL LINE TO gt_main ASSIGNING <fs_main2>.

  <fs_main2>-gsber    = pv_gsber.
  <fs_main2>-rsg      = pv_rsg.
  <fs_main2>-ableinh  = pv_ableinh.
  <fs_main2>-gpart    = pv_gpart.
  <fs_main2>-vkont    = pv_vkont.
  <fs_main2>-printdoc = pv_printdoc.
  <fs_main2>-name     = pv_name.
  <fs_main2>-addr     = pv_addr.
  <fs_main2>-budat    = pv_budat.
  <fs_main2>-current  = pv_current.
  <fs_main2>-pua      = pv_pua.
  <fs_main2>-overdue  = pv_overdue.
  <fs_main2>-misc     = pv_misc.
  <fs_main2>-total    = pv_total.
  <fs_main2>-sortfld  = pv_sortfld.
  <fs_main2>-device   = pv_device.
  <fs_main2>-nodays   = pv_nodays.
  <fs_main2>-oc       = pv_oc.
  <fs_main2>-oc2      = pv_oc2.
ENDFORM.                    " APPEND_MAIN_ITAB
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       Subroutine for Displaying Results
*----------------------------------------------------------------------*
FORM display_report.

  DATA: lv_string   TYPE string.

  DATA: lo_table          TYPE REF TO cl_salv_table,
        lo_functions      TYPE REF TO cl_salv_functions_list,
        lo_columns        TYPE REF TO cl_salv_columns_table,
        lo_column         TYPE REF TO cl_salv_column_table,
        lo_salv_msg       TYPE REF TO cx_salv_msg,
        lo_salv_not_found TYPE REF TO cx_salv_not_found.

  "-- Macro Definition - For Changing ALV Column Names
  DEFINE mc_change_column_names.
    try.
        lo_column ?= lo_columns->get_column( &1 ). "Pass Column Name
        lo_column->set_long_text(   &1 ).          "Pass Long   Text
        lo_column->set_medium_text( &2 ).          "Pass Medium Text
        lo_column->set_short_text(  &2 ).          "Pass Short  Text
      catch cx_salv_not_found into lo_salv_not_found.
        "-- Error Handler
        lv_string = lo_salv_not_found->get_text( ).         "#EC NEEDED
        "-- Show Error Message
*        message gv_string type 'S'.
    endtry.
  END-OF-DEFINITION.

*  IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    "-- Create ALV object
*    TRY.
*        CALL METHOD cl_salv_table=>factory
*          IMPORTING
*            r_salv_table = go_table
*          CHANGING
*            t_table      = gt_main.
*      CATCH cx_salv_msg INTO go_salv_msg.
*        "-- Error Handler
*        gv_string = go_salv_msg->get_text( ).               "#EC NEEDED
*        "-- Show Error Message
*        MESSAGE gv_string TYPE 'I'.
*    ENDTRY.
*  ELSE
  IF pa_new EQ 'X' OR pa_ddate EQ 'X'.
    "-- Create ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_main.
      CATCH cx_salv_msg INTO lo_salv_msg.
        "-- Error Handler
        lv_string = lo_salv_msg->get_text( ).               "#EC NEEDED
        "-- Show Error Message
        MESSAGE lv_string TYPE 'I'.
    ENDTRY.
  ENDIF.

  "-- Enable all ALV Functions
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( 'X' ).

  "-- Get field attributes
  lo_columns   = lo_table->get_columns( ).

  "-- Change ALV Column Name
*  IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    mc_change_column_names: 'RSG'       'RSG',
*                            'PRINTDOC'  'PrintDoc',
*                            'ADDR'      'Address',
*                            'BUDAT'     'Post Dt',
*                            'CURRENT'   'Current',
*                            'PUA'       'PUA',
*                            'TOTAL'     'Total',
*                            'SORTFLD'   'Seq.No',
*                            'NODAYS'    'Days',
*                            'OC'        'OC',
*                            'OC2'       'OC2'.
*  ELSE.
  mc_change_column_names: 'RSG'           text-024,
                          'PRINTDOC'      text-025,
                          'ADDR'          text-026,
                          'BUDAT'         text-027,
                          'CURRENT'       text-028,
                          'PUA'           text-029,
                          'OVERDUE'       text-030,
                          'MISC'          text-052,
                          'TOTAL'         text-031,
                          'SORTFLD'       text-032,
                          'NODAYS'        text-033,
                          'OC'            text-034,
                          'OC2'           text-035.
*  ENDIF.

  "-- Optimize Field Length
  lo_columns->set_optimize( 'X' ).

  "-- Display ALV table
  lo_table->display( ).
ENDFORM.                    " DISPLAY_REPORT
**&---------------------------------------------------------------------*   "GDIMALIWAT - Disable Regular Facility 03/07/2013
**&      Form  OLD_EXTRACT
**&---------------------------------------------------------------------*
**       Old Dunning List Extraction ( Per BC / Portion )
**----------------------------------------------------------------------*
*FORM old_extract .
*
*  DATA: lv_diff       TYPE p,
*        lv_x_flag     TYPE char1,
*        lv_anlart     TYPE eanl-anlart,
*        lv_dunn_tag   TYPE char1.
*
**  DATA: lv_mrid       TYPE eabl-ablbelnr,
**        lv_oc         TYPE eabl-ablhinw,
**        lv_oc2        TYPE eabl-ablhinw,
**        lv_adatsoll   TYPE eabl-adatsoll.
*
*  DATA: lv_currdt     TYPE eabl-adatsoll,
*        lv_opbel      TYPE erdk-opbel.
*
*  DATA: lv_note       TYPE eablg-ablesgr.
**  DATA: lv_mrnote     TYPE eablg-ablesgr,
**        lv_note       TYPE eablg-ablesgr,
**        lv_sched      TYPE eabl-adatsoll.
*
*  "-- Populate Portion Parameter
*  PERFORM populate_it_portlist.
*
*  LOOP AT gt_portlist ASSIGNING <fs_portlist>.
*    SELECT fkkvkp~gsber gpart vkont vkbez ever~anlage abrsperr eanlh~ableinh
*    INTO TABLE gt_dunnhead
*    FROM   fkkvkp INNER JOIN ever  ON fkkvkp~vkont  = ever~vkonto
*                  INNER JOIN eanlh ON ever~anlage   = eanlh~anlage
*                  INNER JOIN te422 ON eanlh~ableinh = te422~termschl
*    WHERE fkkvkp~gpart    IN par
*    AND   fkkvkp~gsber    =  ba
*    AND   auszdat         =  '99991231'
*    AND   eanlh~bis       =  '99991231'
*    AND   te422~portion   =  <fs_portlist>-portion
*    AND   ever~loevm      NE 'X'
*    AND   fkkvkp~kofiz_sd NE '02'
*    AND   fkkvkp~kofiz_sd NE '22'.
*  ENDLOOP.
*
*  LOOP AT gt_dunnhead ASSIGNING <fs_dunnhead>.
*    lv_x_flag = space.
*    lv_note   = space.
*    REFRESH gt_billdoc.
*    lv_dunn_tag = space.
*    lv_diff = 0.
*
*    LOOP AT gt_condo ASSIGNING <fs_condo> WHERE vkont = <fs_dunnhead>-contacct.
*******************CHECK IF ACCOUNT UNDER CONDO*********************************
*      IF <fs_condo>-cond_tag EQ 'X'.
*        lv_x_flag = 'X'.
*      ENDIF.
*    ENDLOOP.
*
*******************CHECK IF ACCOUNT UNDER EXCLUDE ANLART************************
*    SELECT SINGLE anlart INTO lv_anlart
*    FROM eanl
*    WHERE anlage = <fs_dunnhead>-anlage.
*
*    IF ( lv_anlart EQ '5003' OR lv_anlart EQ '5002' OR lv_anlart EQ '5001' OR lv_anlart EQ '4051' OR lv_anlart EQ '4052' ).
*      lv_x_flag = 'X'.
*    ENDIF.
*
*******************CHECK IF ACCOUNT IS VALID BUT FOR DUNNING********************
*    IF  lv_x_flag NE 'X'.
*      IF <fs_dunnhead>-abrsperr NE space.
*********** ***** Get the BILLDOC AND ORIGDOC(IF OSB or Normal Billing)*********
*        REFRESH gt_mr2.
*        REFRESH gt_billdoc.
*        lv_currdt = space.
*
*        SELECT adatsoll ablesgr
*        INTO TABLE gt_mr2
*        FROM eablg
*        WHERE anlage = <fs_dunnhead>-anlage
*        AND adatsoll LE scheddt
*        AND ( ablesgr = '01' OR ablesgr = '06' ).
*
*        SORT gt_mr2 BY adatsoll DESCENDING.
*
*        LOOP AT gt_mr2 ASSIGNING <fs_mr2>.
*          lv_currdt = <fs_mr2>-adatsoll.
*          EXIT.
*        ENDLOOP.
*        lv_diff = sy-datum - lv_currdt.
*
*        IF lv_diff LE '34'.
*          SELECT belnr origdoc adatsoll
*          INTO TABLE gt_billdoc
*          FROM erch
*          WHERE vkont = <fs_dunnhead>-contacct
*          AND adatsoll EQ lv_currdt
*          AND ( stornodat = '00000000' OR stornodat = space ).
*
*          IF sy-subrc = 0.
*            LOOP AT gt_billdoc ASSIGNING <fs_billdoc>.
*              IF <fs_billdoc>-origdoc = '03'.
*                PERFORM dunning.
*                lv_dunn_tag = 'X'.
*              ELSE.
*                SELECT SINGLE erdk~opbel
*                INTO lv_opbel
*                FROM erdk
*                INNER JOIN erchc ON erdk~opbel = erchc~opbel
*                INNER JOIN erch  ON erchc~belnr = erch~belnr
*                WHERE
*                  erdk~invoiced = 'X' AND
*                  erchc~invoiced = 'X' AND
*                  erdk~intopbel = space AND
*                  erchc~intopbel = space AND
*                  erchc~belnr = <fs_billdoc>-belnr.
*                IF sy-subrc = 0.
*                  PERFORM dunning.
*                  lv_dunn_tag = 'X'.
*                ENDIF.
*              ENDIF.
*              EXIT.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*      IF lv_dunn_tag NE 'X'.
*        PERFORM check_closed CHANGING lv_note.
*      ENDIF.
*      IF lv_dunn_tag NE 'X'.
*        IF ( <fs_dunnhead>-abrsperr EQ space AND lv_note NE '13' ).
*          PERFORM dunning.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*  ENDLOOP.
*
*  DATA: begda TYPE sy-datum.
*  DATA: endda TYPE sy-datum.
*  endda = begda = sy-datum.
*  begda+6(2) = '01'.
*  CALL FUNCTION 'DETERMINE_END_OF_MONTH'
*    EXPORTING
*      i_datum = begda
*    IMPORTING
*      e_tt    = endda+6(2).
*  DATA: blk_code TYPE ever-abrsperr.
*  DELETE dunnlist2 WHERE oc = '21' OR oc2 = '03' OR oc = '22' OR gsber = '1800'.
*  LOOP AT dunnlist2.
*    PERFORM check_mro_exists USING dunnlist2-gpart begda endda mro_subrc doc_type_order.
*    IF mro_subrc = 0.
*      WRITE:/ 'With MRO -', dunnlist2-gpart.
*      DELETE dunnlist2.
*    ENDIF.
*    PERFORM check_if_blocked USING dunnlist2-vkont blk_code.
*    IF NOT blk_code IS INITIAL.
*      WRITE:/ 'Account blocked -', dunnlist2-gpart.
*      DELETE dunnlist2.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " OLD_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_WITH_DDATE
*&---------------------------------------------------------------------*
*       Extract Dunning List using Due Date
*----------------------------------------------------------------------*
FORM extract_with_ddate .

  TYPES:  BEGIN OF ty_dfkkop,
            vkont TYPE dfkkop-vkont,
          END OF ty_dfkkop.

  DATA: lt_dfkkop      TYPE SORTED TABLE OF ty_dfkkop WITH NON-UNIQUE KEY vkont,
        lt_dunnhead    TYPE TABLE OF ty_dunnhead.

  DATA: lv_diff           TYPE p,
        lv_x_flag         TYPE char1,
        lv_anlart         TYPE eanl-anlart,
        lv_dunn_tag       TYPE char1.

*  DATA: lv_mrid           TYPE eabl-ablbelnr,
*        lv_oc             TYPE eabl-ablhinw,
*        lv_oc2            TYPE eabl-ablhinw,
*        lv_adatsoll       TYPE eabl-adatsoll.

  DATA: lv_currdt         TYPE eabl-adatsoll,
        lv_opbel          TYPE erdk-opbel ##needed.

  DATA: "lv_mrnote         TYPE eablg-ablesgr,
        lv_note           TYPE eablg-ablesgr.
  "lv_sched          TYPE eabl-adatsoll.

  DATA: lv_vkont          TYPE fkkvkp-vkont,
        lv_gpart          TYPE fkkvkp-gpart,
        lv_gsber          TYPE fkkvkp-gsber,
        lv_anlage         TYPE ever-anlage,
        lv_abrsperr       TYPE ever-abrsperr,
        lv_ableinh        TYPE eanlh-ableinh,
        lv_vkbez          TYPE fkkvk-vkbez.


  FIELD-SYMBOLS: <fs_dfkkop>   TYPE ty_dfkkop,
                 "<fs_condo>    TYPE ty_condo,
                 <fs_mr2>      TYPE ty_mr,
                 <fs_billdoc>  TYPE ty_billdoc.

  SELECT vkont
  INTO TABLE lt_dfkkop
  FROM dfkkop
  "WHERE hvorg IN ('0100', '0200')
  WHERE augrd NE '05'
    AND augst EQ space
    AND faedn IN so_ddate
    AND budat NE '00000000'
    AND abwbl EQ space.

  DELETE ADJACENT DUPLICATES FROM lt_dfkkop COMPARING vkont.

  LOOP AT lt_dfkkop ASSIGNING <fs_dfkkop>.
    CLEAR:  lv_vkont,
            lv_gpart,
            lv_gsber,
            lv_anlage,
            lv_abrsperr,
            lv_ableinh,
            lv_vkbez.

* Check the business area if it is not 1800 because KAM accounts are excluded.
* Check if the account determination id is not equal to 02 or 22
    SELECT SINGLE vkont gpart gsber
    INTO (lv_vkont, lv_gpart, lv_gsber)
    FROM fkkvkp
    WHERE vkont EQ <fs_dfkkop>-vkont
      AND gsber EQ pa_ba2
      AND gsber NE '1800'
      AND kofiz_sd NOT IN ('02','22').                      "#EC WARNOK

    IF sy-subrc EQ 0.
* Check if the account is not moved-out
* Check if the account is not tagged for deletion
      SELECT SINGLE anlage abrsperr
      INTO (lv_anlage, lv_abrsperr)
      FROM ever
      WHERE vkonto EQ <fs_dfkkop>-vkont
        AND auszdat  EQ '99991231'
        AND loevm    NE 'X'.                                "#EC WARNOK

      IF sy-subrc EQ 0.

        SELECT SINGLE ableinh
        INTO lv_ableinh
        FROM eanlh
        WHERE anlage EQ lv_anlage
          AND bis    EQ  '99991231'.

        IF sy-subrc EQ 0.
          SELECT SINGLE vkbez
          INTO lv_vkbez
          FROM fkkvk
          WHERE vkont EQ <fs_dfkkop>-vkont.

          APPEND INITIAL LINE TO lt_dunnhead ASSIGNING <fs_dunnhead>.
          <fs_dunnhead>-contacct = lv_vkont.
          <fs_dunnhead>-gpart    = lv_gpart.
          <fs_dunnhead>-gsber    = lv_gsber.
          <fs_dunnhead>-anlage   = lv_anlage.
          <fs_dunnhead>-abrsperr = lv_abrsperr.
          <fs_dunnhead>-ableinh  = lv_ableinh.
          <fs_dunnhead>-name     = lv_vkbez.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_dfkkop>.

  LOOP AT lt_dunnhead ASSIGNING <fs_dunnhead>.
    lv_x_flag = space.
    lv_note   = space.
    REFRESH gt_billdoc.
    lv_dunn_tag = space.
    lv_diff = 0.

*    LOOP AT gt_condo ASSIGNING <fs_condo> WHERE vkont = <fs_dunnhead>-contacct.
*******************CHECK IF ACCOUNT UNDER CONDO*********************************
*      IF <fs_condo>-cond_tag EQ 'X'.
*        lv_x_flag = 'X'.
*      ENDIF.
*    ENDLOOP.
*    UNASSIGN <fs_condo>.

******************CHECK IF ACCOUNT UNDER EXCLUDE ANLART************************
    SELECT SINGLE anlart INTO lv_anlart
    FROM eanl
    WHERE anlage = <fs_dunnhead>-anlage.

    IF ( lv_anlart EQ '5003' OR lv_anlart EQ '5002' OR lv_anlart EQ '5001' OR lv_anlart EQ '4051' OR lv_anlart EQ '4052' ).
      lv_x_flag = 'X'.
    ENDIF.

******************CHECK IF ACCOUNT IS VALID BUT FOR DUNNING********************
    IF  lv_x_flag NE 'X'.
      IF <fs_dunnhead>-abrsperr NE space.
********** ***** Get the BILLDOC AND ORIGDOC(IF OSB or Normal Billing)*********
        REFRESH gt_mr.
        REFRESH gt_billdoc.
        lv_currdt = space.

        SELECT adatsoll ablesgr
        INTO TABLE gt_mr
        FROM eablg
        WHERE anlage = <fs_dunnhead>-anlage
        AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
        AND ( ablesgr = '01' OR ablesgr = '06' ).

        SORT gt_mr BY adatsoll DESCENDING.

        LOOP AT gt_mr ASSIGNING <fs_mr2>.
          lv_currdt = <fs_mr2>-adatsoll.
          EXIT.
        ENDLOOP.
        UNASSIGN <fs_mr2>.
        lv_diff = gv_highest_duedate - lv_currdt.

        IF lv_diff LE '34'.
          SELECT belnr origdoc adatsoll
          INTO TABLE gt_billdoc
          FROM erch
          WHERE vkont = <fs_dunnhead>-contacct
          AND adatsoll EQ lv_currdt
          AND ( stornodat = '00000000' OR stornodat = space ).

          IF sy-subrc = 0.
            LOOP AT gt_billdoc ASSIGNING <fs_billdoc>.
              IF <fs_billdoc>-origdoc = '03'.
                PERFORM dunning.
                lv_dunn_tag = 'X'.
              ELSE.
                CLEAR lv_opbel.
                SELECT erdk~opbel
                  INTO lv_opbel
                FROM erdk
                INNER JOIN erchc ON erdk~opbel = erchc~opbel
                INNER JOIN erch  ON erchc~belnr = erch~belnr
                UP TO 1 ROWS
                WHERE
                  erdk~invoiced = 'X' AND
                  erchc~invoiced = 'X' AND
                  erdk~intopbel = space AND
                  erchc~intopbel = space AND
                  erchc~belnr = <fs_billdoc>-belnr.
                ENDSELECT.
                IF sy-subrc = 0.
                  PERFORM dunning.
                  lv_dunn_tag = 'X'.
                ENDIF.
              ENDIF.
              EXIT.
            ENDLOOP.
            UNASSIGN <fs_billdoc>.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_dunn_tag NE 'X'.
        PERFORM check_closed CHANGING lv_note.
      ENDIF.
      IF lv_dunn_tag NE 'X'.
        IF ( <fs_dunnhead>-abrsperr EQ space AND lv_note NE '13' ).
          PERFORM dunning.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_dunnhead>.

  "-- Get number of records in dunnlist3
  DESCRIBE TABLE gt_dunnlist LINES gv_count.
ENDFORM.                    " EXTRACT_WITH_DDATE
*&---------------------------------------------------------------------*
*&      Form  DUEDATE_PARAM_CHECK
*&---------------------------------------------------------------------*
*       Check Due Date
*----------------------------------------------------------------------*
FORM duedate_param_check.
  DATA: ls_ddate TYPE selopt ##needed.

  CLEAR ls_ddate.
  LOOP AT so_ddate INTO ls_ddate.
    IF gv_highest_duedate IS INITIAL.
      gv_highest_duedate = so_ddate-low.
    ELSE.
      "If the highest due date is less than so_ddate-low
      IF gv_highest_duedate LT so_ddate-low.
        gv_highest_duedate = so_ddate-low.
      ENDIF.
    ENDIF.
    "If the highest due date is less than so_ddate-high
    IF gv_highest_duedate LT so_ddate-high.
      gv_highest_duedate = so_ddate-high.
    ENDIF.
  ENDLOOP.

  CLEAR ls_ddate.
  LOOP AT so_ddate INTO ls_ddate.
    IF gv_lowest_duedate IS INITIAL.
      gv_lowest_duedate = so_ddate-low.
    ELSE.
      "If the lowest due date is greater than so_ddate-low
      IF gv_lowest_duedate GT so_ddate-low.
        gv_lowest_duedate = so_ddate-low.
      ENDIF.
    ENDIF.
    "If the lowest due date is greater than so_ddate-high
    IF gv_lowest_duedate GT so_ddate-high.
      gv_lowest_duedate = so_ddate-high.
    ENDIF.
  ENDLOOP.

  IF so_ddate-high IS INITIAL.
    CLEAR ls_ddate.
    LOOP AT so_ddate INTO ls_ddate.
      IF gv_lowest_duedate IS INITIAL.
        gv_lowest_duedate = so_ddate-low.
      ELSE.
        IF gv_lowest_duedate GT so_ddate-low.
          gv_lowest_duedate = so_ddate-low.
        ENDIF.
      ENDIF.
      IF gv_lowest_duedate GT so_ddate-high AND so_ddate-high IS NOT INITIAL.
        gv_lowest_duedate = so_ddate-high.
      ENDIF.
    ENDLOOP.

    CLEAR ls_ddate.
    LOOP AT so_ddate INTO ls_ddate.
      IF gv_highest_duedate IS INITIAL.
        gv_highest_duedate = so_ddate-low.
      ELSE.
        IF gv_highest_duedate LT so_ddate-low.
          gv_highest_duedate = so_ddate-low.
        ENDIF.
      ENDIF.
      IF gv_highest_duedate LT so_ddate-high AND so_ddate-high IS NOT INITIAL.
        gv_highest_duedate = so_ddate-high.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF so_ddate-low IS INITIAL.
    CLEAR ls_ddate.
    LOOP AT so_ddate INTO ls_ddate.
      IF gv_lowest_duedate IS INITIAL.
        gv_lowest_duedate = so_ddate-high.
      ELSE.
        IF gv_lowest_duedate GT so_ddate-high.
          gv_lowest_duedate = so_ddate-high.
        ENDIF.
      ENDIF.
      IF gv_lowest_duedate GT so_ddate-low AND so_ddate-low IS NOT INITIAL.
        gv_lowest_duedate = so_ddate-low.
      ENDIF.
    ENDLOOP.

    CLEAR ls_ddate.
    LOOP AT so_ddate INTO ls_ddate.
      IF gv_highest_duedate IS INITIAL.
        gv_highest_duedate = so_ddate-high.
      ELSE.
        IF gv_highest_duedate LT so_ddate-high.
          gv_highest_duedate = so_ddate-high.
        ENDIF.
      ENDIF.
      IF gv_highest_duedate LT so_ddate-low AND so_ddate-low IS NOT INITIAL.
        gv_highest_duedate = so_ddate-low.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF gv_lowest_duedate IS INITIAL.
    gv_lowest_duedate = gv_highest_duedate.
  ELSEIF gv_highest_duedate IS INITIAL.
    gv_highest_duedate = gv_lowest_duedate.
  ENDIF.
*  IF so_ddate-high IS INITIAL.
*    so_ddate-high = so_ddate-low.
*  ENDIF.
ENDFORM.                    " DUEDATE_PARAM_CHECK
*&---------------------------------------------------------------------*
*&      Form  SPECIAL_EXTRACT
*&---------------------------------------------------------------------*
*       Extract Dunning List using BP / Contract Account
*----------------------------------------------------------------------*
FORM special_extract .

  DATA: lv_flag           TYPE char1,
        lv_current_date   TYPE sy-datum.

  DATA: lv_addr        TYPE char80.

  DATA: "lv_amt            TYPE dfkkop-betrh,
        lv_total          TYPE dfkkop-betrh,
        "lv_msg            TYPE c LENGTH 25,
        lv_eqpt           TYPE egerh-equnr.
  "lv_pia            TYPE dfkkop-betrh.

  DATA: lv_printdoc    TYPE c LENGTH 20,
        lv_posdt       TYPE erdk-budat,
        lv_current     TYPE char15,
        lv_pua         TYPE char15,
        lv_rsg         TYPE char8,
        lv_overdue     TYPE char15,
        lv_misc        TYPE char15.

  DATA: lv_mindt          TYPE sy-datum,
        lv_maxdt          TYPE sy-datum,
        lv_diff           TYPE p.

  DATA: lv_sortfld        TYPE char6,
        lv_device         TYPE eabl-gernr,
        lv_vstelle        TYPE evbs-vstelle,
        lv_haus           TYPE evbs-haus,
        lv_mrid           TYPE eabl-ablbelnr,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw.
  "lv_adatsoll       TYPE eabl-adatsoll.

  DATA: lt_dunn        TYPE TABLE OF ty_dunn,
        lt_dunn2       TYPE TABLE OF ty_dunn2,
        lt_probill     TYPE TABLE OF ty_probill,
        lt_dunnhead    TYPE TABLE OF ty_dunnhead,
        lt_oc          TYPE TABLE OF ty_oc.

  FIELD-SYMBOLS: <fs_dunn>         TYPE ty_dunn,
                 <fs_dunn2>        TYPE ty_dunn2,
                 <fs_probill>      TYPE ty_probill,
                 <fs_oc>           TYPE ty_oc,
                 <fs_dunnlist>     TYPE ty_dunnlist.

  lv_current_date = sy-datum.

  SELECT fkkvkp~gsber gpart vkont vkbez ever~anlage abrsperr eanlh~ableinh
    INTO TABLE lt_dunnhead
    FROM fkkvkp INNER JOIN ever ON fkkvkp~vkont = ever~vkonto
                INNER JOIN eanlh ON ever~anlage = eanlh~anlage
    WHERE fkkvkp~vkont IN so_can
    AND fkkvkp~gsber NE '1800' "exclude KAM accounts
    AND fkkvkp~gpart IN so_par
    AND auszdat = '99991231'
    AND eanlh~bis = '99991231'
    AND ever~loevm NE 'X'
    AND fkkvkp~kofiz_sd NE '02'
    AND fkkvkp~kofiz_sd NE '22'.

  LOOP AT lt_dunnhead ASSIGNING <fs_dunnhead>.

    "--- Reset values
    CLEAR:  lv_rsg,
            lv_printdoc,
            lv_addr,
            lv_posdt,
            lv_current,
            lv_pua,
            lv_overdue,
            lv_misc,
            lv_total,
            lv_sortfld,
            lv_device,
            lv_diff,
            lv_oc,
            lv_oc2,
            "lv_pia,
            lv_eqpt,
            lv_vstelle,
            lv_haus,
            lv_rsg,
            lv_maxdt,
            lv_mindt.
    REFRESH lt_dunn.
    REFRESH lt_dunn2.

    PERFORM get_address CHANGING lv_addr.

    SELECT xblnr opbel budat bldat faedn SUM( betrh ) AS amount
    INTO TABLE lt_dunn
    FROM dfkkop
    WHERE vkont EQ <fs_dunnhead>-contacct
      "AND hvorg IN ('0100', '0200')
      AND augrd NE '05'
      AND augst EQ space
      AND faedn LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
      AND budat NE '00000000'
      AND abwbl EQ space
    GROUP BY xblnr budat bldat opbel faedn.

***( end of data for DUNN itab )***************************************************

    gv_cnt = 0.
    lv_diff = 0.
    lv_mindt = '00000000'.
    lv_maxdt = '00000000'.

    SORT lt_dunn BY faedn ASCENDING.
    LOOP AT lt_dunn ASSIGNING <fs_dunn>.
      lv_flag = '0'.
      LOOP AT lt_probill ASSIGNING <fs_probill> WHERE loobj1+0(12) EQ <fs_dunn>-ficadoc.
        lv_flag = '1'.
      ENDLOOP.
      UNASSIGN <fs_probill>.

      IF lv_flag NE '1'.
        APPEND INITIAL LINE TO lt_dunn2 ASSIGNING <fs_dunn2>.
        <fs_dunn2>-docno    =  <fs_dunn>-docno.
        <fs_dunn2>-budat    =  <fs_dunn>-budat.
        <fs_dunn2>-bldat    =  <fs_dunn>-bldat.
        <fs_dunn2>-ficadoc  =  <fs_dunn>-ficadoc.
        <fs_dunn2>-faedn    =  <fs_dunn>-faedn.
        <fs_dunn2>-amount   =  <fs_dunn>-amount.
        gv_cnt = gv_cnt + 1.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_dunn>.

    SORT lt_dunn2 BY faedn ASCENDING.

    LOOP AT lt_dunn2 ASSIGNING <fs_dunn2>.
      lv_mindt = <fs_dunn2>-faedn.
      EXIT.
    ENDLOOP.
    UNASSIGN <fs_dunn2>.

    LOOP AT lt_dunn2 ASSIGNING <fs_dunn2>.
      IF <fs_dunn2>-amount NE 0.
        lv_printdoc = <fs_dunn2>-docno.
        lv_posdt    = <fs_dunn2>-budat.
        lv_maxdt    = <fs_dunn2>-faedn.
*        SELECT SUM( betrh )
*          INTO lv_pia
*          FROM dfkkop
*        WHERE opbel = <fs_dunn2>-ficadoc
*        AND   augdt LE lv_current_date
*        AND   augst = '9'
*        AND   ( hvorg = '0100' AND tvorg = '0010' ).
*        lv_current = lv_amt + lv_pia.
      ELSE.
        lv_current = 0.
        lv_printdoc = space.
        lv_posdt = space.
      ENDIF.
      EXIT.
    ENDLOOP.
    UNASSIGN <fs_dunn2>.

    IF lv_mindt EQ '00000000'.
      IF lv_maxdt EQ '00000000'.
        lv_diff = 0.
      ELSE.
        lv_diff = lv_current_date - lv_maxdt.
      ENDIF.
    ELSE.
      lv_diff = lv_current_date - lv_mindt.
    ENDIF.

***  DEVICE SEQ***
    CLEAR lv_eqpt.
    SELECT SINGLE equnr
      INTO (lv_eqpt)
      FROM egerh INNER JOIN eastl ON egerh~logiknr = eastl~logiknr
     WHERE eastl~anlage = <fs_dunnhead>-anlage
       AND egerh~bis = '99991231'.                          "#EC WARNOK

    CLEAR lv_device.
    CLEAR lv_sortfld.
    SELECT SINGLE eqfnr sernr
      INTO (lv_sortfld, lv_device)
      FROM v_equi
      WHERE equnr EQ lv_eqpt
      AND datbi EQ '99991231'
      AND eqfnr NE '0'.                                     "#EC WARNOK
***  DEVICE SEQ***

*** RSG ***
    CLEAR lv_vstelle.
    SELECT vstelle
    INTO (lv_vstelle)
    FROM eanlh INNER JOIN eanl ON eanlh~anlage = eanl~anlage
    UP TO 1 ROWS
    WHERE eanlh~anlage = <fs_dunnhead>-anlage
    AND eanlh~ab LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
    AND eanlh~bis GE sy-datum. "scheddt. "GDIMALIWAT - Disable Regular Facility 03/07/2013
    ENDSELECT.

    CLEAR lv_haus.
    SELECT SINGLE haus
       INTO (lv_haus)
       FROM evbs
      WHERE vstelle = lv_vstelle.

    CLEAR lv_rsg.
    SELECT SINGLE regiogroup
       INTO lv_rsg
       FROM ehauisu
      WHERE haus = lv_haus.
*** RSG ***

*** OC ***
    SELECT ablhinw eablg~adatsoll eablg~ablbelnr
    INTO TABLE lt_oc
    FROM eablg
    INNER JOIN eabl ON eablg~ablbelnr = eabl~ablbelnr
    UP TO 1 ROWS
    WHERE anlage = <fs_dunnhead>-anlage
    AND ablesgr = '01'
    AND ablstat NE '0'
    ORDER BY eablg~adatsoll DESCENDING.

    READ TABLE lt_oc INDEX 1 ASSIGNING <fs_oc>.
    IF sy-subrc EQ 0.
      lv_oc = <fs_oc>-oc.
      "lv_adatsoll = <fs_oc>-adatsoll.
      lv_mrid = <fs_oc>-mrid.
    ENDIF.
    UNASSIGN <fs_oc>.

    SELECT SINGLE meterreadingnot2
    INTO   lv_oc2
    FROM   zmwosb_s01p
    WHERE mridnumber = lv_mrid.
*** OC ***

    "-- Call Function Module for fetching PUA
    PERFORM get_due_balance_by_special USING    <fs_dunnhead>-contacct
                                                <fs_dunnhead>-gpart
                                       CHANGING lv_total
                                                lv_pua
                                                lv_current
                                                lv_overdue
                                                lv_misc.

    "-- Delete leading zeroes from Printdoc
    SHIFT lv_printdoc LEFT DELETING LEADING '0'.
    APPEND INITIAL LINE TO gt_dunnlist ASSIGNING <fs_dunnlist>.
    <fs_dunnlist>-gsber    = <fs_dunnhead>-gsber.
    <fs_dunnlist>-rsg      = lv_rsg.
    <fs_dunnlist>-ableinh  = <fs_dunnhead>-ableinh.
    <fs_dunnlist>-gpart    = <fs_dunnhead>-gpart.
    <fs_dunnlist>-vkont    = <fs_dunnhead>-contacct.
    <fs_dunnlist>-printdoc = lv_printdoc.
    <fs_dunnlist>-name     = <fs_dunnhead>-name.
    <fs_dunnlist>-addr     = lv_addr.
    <fs_dunnlist>-budat    = lv_posdt.
    <fs_dunnlist>-current  = lv_current.
    <fs_dunnlist>-pua      = lv_pua.
    <fs_dunnlist>-overdue  = lv_overdue.
    <fs_dunnlist>-misc     = lv_misc.
    <fs_dunnlist>-total    = lv_total.
    <fs_dunnlist>-sortfld  = lv_sortfld.
    <fs_dunnlist>-device   = lv_device.
    <fs_dunnlist>-nodays   = lv_diff.
    <fs_dunnlist>-oc       = lv_oc.
    <fs_dunnlist>-oc2      = lv_oc2.
    "APPEND dunnlist3.
  ENDLOOP.
  UNASSIGN <fs_dunnhead>.
ENDFORM.                    " SPECIAL_EXTRACT

*&---------------------------------------------------------------------*
*&      Form  GET_DUE_BALANCE_BY_SPECIAL
*&---------------------------------------------------------------------*
*       Subroutine for Fetching Due Balance of an Account
*----------------------------------------------------------------------*
FORM get_due_balance_by_special  USING    pv_can     TYPE vkont_kk
                                          pv_bp      TYPE bu_partner
                                 CHANGING pv_balance TYPE betrh_kk
                                          pv_pua     TYPE char15
                                          pv_current TYPE char15
                                          pv_overdue TYPE char15
                                          pv_misc    TYPE char15.

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
      IF <fs_balances>-net_date LE sy-datum.
        IF <fs_balances>-doc_type NE 'AB'.
          "-- Summarize Due Amounts
          pv_balance = pv_balance + <fs_balances>-amount.
        ELSE.
          "-- If Installment Plan and Already Due
          IF <fs_balances>-disc_due LE sy-datum.
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
*  PERFORM get_current_debit  USING    pv_can
*                                      pv_bp
*                             CHANGING pv_current.

  "-- Get Overdue
  lv_days60 = sy-datum - co_60.
  lv_days25 = sy-datum - co_25.

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
        AND mwskz EQ 'E0'.

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
        AND mwskz EQ 'E0'.

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
  AND faedn LE sy-datum
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
        AND mwskz EQ 'E0'.

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
  AND faedn LE sy-datum
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
    pv_pua = '0.00'.
  ENDIF.
  IF pv_misc IS INITIAL.
    pv_misc = '0.00'.
  ENDIF.
ENDFORM.                    " GET_DUE_BALANCE_BY_SPECIAL
**&---------------------------------------------------------------------*
**&      Form  GET_CONDONATION_ACCOUNTS
**&---------------------------------------------------------------------*
**       Retrieve condonation accounts
**----------------------------------------------------------------------*
*FORM get_condonation_accounts .
*  REFRESH gt_condo.
*
*  SELECT vkont cond_tag
*  INTO TABLE gt_condo
*  FROM zcondonation
*  WHERE cond_tag = 'X'.
*ENDFORM.                    " GET_CONDONATION_ACCOUNTS
*&---------------------------------------------------------------------*
*&      Form  DEVELOPER_PARAMS
*&---------------------------------------------------------------------*
*       For developer purposes
*----------------------------------------------------------------------*
FORM developer_params .
  DATA: lv_uname TYPE sy-uname.

  lv_uname = sy-uname.

  IF lv_uname EQ 'GDIMALIWAT'.
    pa_file  = '\\172.18.1.240\repuserdata\Glenn\ZDUNNLIST\'.
  ENDIF.
ENDFORM.                    " DEVELOPER_PARAMS
**&---------------------------------------------------------------------*
**&      Form  BACKTRACK
**&---------------------------------------------------------------------*
**       Subtract 22 days from all input due dates
**----------------------------------------------------------------------*
*FORM backtrack .
*
*  DATA: ls_ddate TYPE selopt ##needed.
*
*  SUBTRACT co_22 FROM gv_lowest_duedate.
*  SUBTRACT co_22 FROM gv_highest_duedate.
*
*  LOOP AT so_ddate INTO ls_ddate.
*    IF so_ddate-low IS NOT INITIAL.
*      SUBTRACT co_22 FROM so_ddate-low.
*    ENDIF.
*    IF so_ddate-high IS NOT INITIAL.
*      SUBTRACT co_22 FROM so_ddate-high.
*    ENDIF.
*    MODIFY so_ddate FROM so_ddate INDEX sy-tabix.
*  ENDLOOP.
*ENDFORM.                    " BACKTRACK