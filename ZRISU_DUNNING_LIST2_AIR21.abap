*&-------------------------------------------------------------------------------------------*
* PROGRAM ID      : ZRISU_DUNNING_LIST2_AIR21
* DESCRIPTION     : Dunning List Program
*---------------------------------------------------------------------------------------------
*& Module       : ISU
*& Author       : FCGONZALES
*& Designed by  :
*& Date Created : 03/28/2008
*---------------------------------------------------------------------------------------------
*  DATE      | Author ID   |  Ticket No.      |  DESCRIPTION
*---------------------------------------------------------------------------------------------
* 10/22/2010 |             |                  | Additional field ( No. of days,
*            |             |                  | OBSERVATION CODE ) and INSTALLATION TYPE
*            |             |                  | 4051 and 4052 exclude c/o sir jerry
* 11/03/2010 |             |                  | COrrection on Protested Bill (no of days)
* 11/10/2010 |             |                  | Correct Curr.Bill incase of Disconnected
* 08/01/2011 | EJOVELLANOS |                  | exclude records with MRO or MRNote type 1
*            |             |                  | OC 21 or MRNote type 2 OC 03
* 08/24/2011 | EJOVELLANOS |                  | include portion in selection
* 09/05/2011 | BMABINI     |                  | Allow range selection to Portion Parameter.
* 09/14/2011 | EJOVELLANOS |                  | Exclude blocked entries
* 11/24/2011 | ACUCUECO    |                  | Included Form "Special"
* 11/28/2011 | ACUCUECO    |                  | Additional validation as requested for
*            |             |                  | KAM Setup (exclude BA '1800')
* 01/19/2012 | RANDYSY     | TN#9848          | Send Email Notification
*            |             |                  | (Only for BC/Portion & if BA is 1700)
* 02/14/2012 | RANDYSY     | TN#11532         | Send Email Notification (Sending Per BA)
* 02/22/2012 | RANDYSY     |                  | Parameter BA & Portion Changed to ListBox
* 02/29/2012 | RANDYSY     |                  | File Name Changed  (e.g. 0200_01_17_120229.txt)
* 03/16/2012 | RANDYSY     | c/o Mike Rinoza  | Fix Portion Parameters
* 04/03/2012 | RANDYSY     | c/o Reena Flores | Added Checkbox of Option of Sending Email
* 07/17/2012 | RANDYSY     | TN#17698         | Filter for Non-PS Users, for sending email
* 08/02/2012 | RANDYSY     | TN#13816         | Change Source of Current, PUA and Total
* 10/23/2012 | RANDYSY     | c/o Reena Flores | Removal of Overpayments
* 11/26/2012 | ANNACLO     | 23184            | Replaced hard-coded IP w/ Domains/host names
* 01/08/2013 | GDIMALIWAT  | 23217            | Updated for Dunning List Extraction Parameter Based on Due Date
* 01/17/2013 | GDIMALIWAT  | 23217            | Updated PUA/Balance Computation for Due Date / Fixed PrintDoc for special
* 01/22/2013 | GDIMALIWAT  | 23217            | Program renamed to ZRISU_DUNNING_LIST2. Added 61+_OVERDUE Field/Conditions
* 02/01/2013 | GDIMALIWAT  | 23217            | Program modified to run a 2nd extraction (less 22 days) when results are less than 100
* 02/05/2013 | GDIMALIWAT  | 23217            | Program modified to add condition on 61+_OVERDUE. Write to file only when overdue > 0 (DUEDATE extraction only)
* 02/18/2013 | GDIMALIWAT  | 28331            | Program modified to Change Output File of Per BP/Contract Account Extraction
* 02/20/2013 | GDIMALIWAT  | 28331            | Program modified to modify E-mail Notification message
* 03/05/2013 | GDIMALIWAT  | 29641            | Program modified to add READ TABLE validations
* 03/07/2013 | GDIMALIWAT  | 29797            | Program modified to update Due Date parameter validations and to fix missing Headers
* 03/11/2013 | GDIMALIWAT  | 29848            | Program modified to optimize ZDUNNLIST and fix Current/Overdue
* 03/20/2013 | GDIMALIWAT  | 29848            | Program modified to remove PUA gt 0 condition and modified the Current Computation
* 03/26/2013 | GDIMALIWAT  | 31403            | Program modified to update Due Date parameters for e-mail notification
* 04/02/2013 | GDIMALIWAT  | 31700            | Program modified to remove backtracking and exclude accounts with negative value in total column
*&-------------------------------------------------------------------------------------------*

REPORT ZRISU_DUNNING_LIST2_AIR21 NO STANDARD PAGE HEADING LINE-SIZE 280.

*&---------------------------------------------------------------------*
*& Table List - Table Names
*&
TABLES: fkkvkp, te420, dfkkop.

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
         total      TYPE c LENGTH 15,
         sortfld    TYPE c LENGTH 6,
         device     TYPE eabl-gernr,
         nodays     TYPE c LENGTH 4,
         oc         TYPE c LENGTH 4,
         oc2        TYPE c LENGTH 4,
       END OF ty_main,

       BEGIN OF ty_main2,
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
         total      TYPE c LENGTH 15,
         sortfld    TYPE c LENGTH 6,
         device     TYPE eabl-gernr,
         nodays     TYPE c LENGTH 4,
         oc         TYPE c LENGTH 4,
         oc2        TYPE c LENGTH 4,
       END OF ty_main2,

       "--- GDIMALIWAT
       BEGIN OF ty_dunn,
        docno      TYPE char20,
        ficadoc    TYPE dfkkop-opbel,
        budat      TYPE erdk-budat,
        bldat      TYPE erdk-bldat,
        faedn      TYPE dfkkop-faedn,
        "amount     TYPE char15,
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

      BEGIN OF ty_portlist,
        portion    TYPE te420-termschl,
      END OF ty_portlist,

      BEGIN OF ty_condo,
        vkont      TYPE fkkvkp-vkont,
        cond_tag   TYPE zcondonation-cond_tag,
      END OF ty_condo,

      BEGIN  OF ty_mr2,
        adatsoll   TYPE eabl-adatsoll,
        ablesgr    TYPE eablg-ablesgr,
      END OF ty_mr2,

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
       END OF ty_pua_overdue.
"--- end

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_main      TYPE TABLE OF ty_main,
      gt_main2     TYPE TABLE OF ty_main2.

DATA: gt_dunn        TYPE TABLE OF ty_dunn,
      gt_dunn2       TYPE TABLE OF ty_dunn2,
      gt_probill     TYPE TABLE OF ty_probill,
      gt_dunnhead    TYPE TABLE OF ty_dunnhead,
      gt_portlist    TYPE TABLE OF ty_portlist,
      gt_condo       TYPE TABLE OF ty_condo,
      gt_mr2         TYPE TABLE OF ty_mr2,
      gt_billdoc     TYPE TABLE OF ty_billdoc,
      gt_oc          TYPE TABLE OF ty_oc.

*DATA: BEGIN OF dunnlist2 OCCURS 0, "GDIMALIWAT - Disable Regular Facility 03/07/2013
*        gsber      TYPE fkkvkp-gsber,  '|',
*        rsg        TYPE char8,         '|',
*        ableinh    TYPE eablg-ableinh, '|',
*        gpart      TYPE fkkvkp-gpart,  '|',
*        vkont      TYPE fkkvkp-vkont,  '|',
*        printdoc   TYPE char12,        '|',
*        name       TYPE fkkvk-vkbez,   '|',
*        addr       TYPE char80,        '|',
*        budat      TYPE char8,         '|',
*        current    TYPE char16,        '|',
*        pua        TYPE char15,        '|',
*        total      TYPE char15,        '|',
*        sortfld    TYPE char6,         '|',
*        device     TYPE eabl-gernr,    '|',
*        nodays     TYPE char4,         '|',
*        oc         TYPE char4,         '|',
*        oc2        TYPE char4,         '|',
*      END OF dunnlist2.

DATA: BEGIN OF dunnlist3 OCCURS 0,
        gsber      TYPE fkkvkp-gsber,  '|',
        rsg        TYPE char8,         '|',
        ableinh    TYPE eablg-ableinh, '|',
        gpart      TYPE fkkvkp-gpart,  '|',
        vkont      TYPE fkkvkp-vkont,  '|',
        printdoc   TYPE char12,        '|',
        name       TYPE fkkvk-vkbez,   '|',
        addr       TYPE char80,        '|',
        budat      TYPE char8,         '|',
        current    TYPE char16,        '|',
        pua        TYPE char15,        '|',
        overdue    TYPE char15,        '|',
        total      TYPE char15,        '|',
        sortfld    TYPE char6,         '|',
        device     TYPE eabl-gernr,    '|',
        nodays     TYPE char4,         '|',
        oc         TYPE char4,         '|',
        oc2        TYPE char4,         '|',
      END OF dunnlist3.

DATA: BEGIN OF address,
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
      END OF address.

*&---------------------------------------------------------------------*
*& Data Definitions - Global parameters (gs_<structure name>)
*&                                      (gv_<var name>),
*                                       (gr_<range>),
*                                       (go_<class>)
DATA: new_portion    TYPE te420-termschl.

DATA:  gv_cnt        TYPE i.

DATA: mro_subrc      TYPE sy-subrc.

DATA: doc_type_order TYPE bapieablh-mrdocumenttype VALUE '1'.

DATA: it_gsber       TYPE TABLE OF ty_gsber,
      wa_gsber       TYPE ty_gsber.

DATA: gv_valid_send  TYPE sysubrc.

DATA: gv_string         TYPE string,
      gv_count          TYPE i,
      go_table          TYPE REF TO cl_salv_table,
      go_functions      TYPE REF TO cl_salv_functions_list,
      go_columns        TYPE REF TO cl_salv_columns_table,
      go_column         TYPE REF TO cl_salv_column_table,
      go_salv_msg       TYPE REF TO cx_salv_msg,
      go_salv_not_found TYPE REF TO cx_salv_not_found.

DATA: gv_highest_duedate  TYPE dfkkop-faedn,
      gv_lowest_duedate   TYPE dfkkop-faedn.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (fs_<name>)
*&
FIELD-SYMBOLS:
      <fs_gsber>        TYPE ty_gsber,
      <fs_main>         TYPE ty_main,
      <fs_main2>        TYPE ty_main2,
      <fs_dunn>         TYPE ty_dunn,
      <fs_dunn2>        TYPE ty_dunn2,
      <fs_probill>      TYPE ty_probill,
      <fs_dunnhead>     TYPE ty_dunnhead,
      <fs_portlist>     TYPE ty_portlist,
      <fs_condo>        TYPE ty_condo,
      <fs_mr2>          TYPE ty_mr2,
      <fs_billdoc>      TYPE ty_billdoc,
      <fs_eablg>        TYPE eablg,
      <fs_oc>           TYPE ty_oc.
"-- end
*&----------------------------------------------------------------------
*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK entry WITH FRAME TITLE ent.
PARAMETERS:
      "old   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr, "GDIMALIWAT - Disable Regular Facility 03/07/2013
      new   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
      ddate RADIOBUTTON GROUP grp1 .
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
      par FOR fkkvkp-gpart MODIF ID b2,
      can FOR fkkvkp-vkont MODIF ID b2.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK  blk5 WITH FRAME TITLE blk5ttl.
PARAMETER:        ba2  TYPE fkkvkp-gsber   AS LISTBOX VISIBLE LENGTH 10 DEFAULT '0400' MODIF ID b3 OBLIGATORY.
SELECT-OPTIONS:   so_ddate FOR dfkkop-faedn MODIF ID b3.
SELECTION-SCREEN END OF BLOCK blk5.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE blk3ttl.
PARAMETER: spl_file TYPE rlgrap-filename DEFAULT
           '\\saprep01.mayniladsap.com\Mtr_RdngINDRA\ZDunning\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE blk4ttl.
PARAMETERS: c_email AS CHECKBOX DEFAULT 'X' MODIF ID e1.
SELECTION-SCREEN END OF BLOCK blk4.

*&---------------------------------------------------------------------*
*& Initialization - Initial process at start of program
*&
INITIALIZATION.
  "-- Check if User is valid to send Email
  PERFORM is_valid_to_send_email USING    sy-uname
                                 CHANGING gv_valid_send.

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

  ent     = 'Dunning List Extraction Selection'.
*  blk1ttl = 'Regular Facility'. "GDIMALIWAT - Disable Regular Facility 03/07/2013
  blk2ttl = 'Special Facility'.
  blk3ttl = 'Path File'.
  blk5ttl = 'Due Date Facility'.

  "-- For developer purpose
  PERFORM developer_params.

*&---------------------------------------------------------------------*
*& At Selection Screen
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR spl_file.
  "-- Popup Window Dialog Box
  PERFORM select_file_path USING spl_file.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
*    IF old = 'X' AND ( screen-group1 = 'B2' OR screen-group1 = 'B3' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
*      screen-active = 0.
*      MODIFY SCREEN.
*    ENDIF.
    IF new = 'X' AND screen-group1 = 'B3'."( screen-group1 = 'B1' OR screen-group1 = 'B3' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF ddate = 'X' AND screen-group1 = 'B2'."( screen-group1 = 'B1' OR screen-group1 = 'B2' ). "GDIMALIWAT - Disable Regular Facility 03/07/2013
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    "-- Don't Display Send Email Checkbox
    IF gv_valid_send NE 0 OR new EQ 'X'. "If user is invalid to send or if Facility is Special
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

  IF 1800 = ba2 AND ddate = 'X'.
    MOVE '0400' TO ba2.
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
  PERFORM get_condonation_accounts.
  PERFORM extraction.
* Commented backtracking 04/02/2013
*  "-- If record count was less than 100, perform 2nd iteration of extraction with 22 less days from input due date
*  IF ddate EQ 'X' AND gv_count LT 100.
*    PERFORM backtrack.
*    PERFORM extraction.
*  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  EXTRACTION
*&---------------------------------------------------------------------*
*       Perform Dunning List Extraction
*----------------------------------------------------------------------*
FORM extraction .
*  IF old EQ 'X'.     "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    "-- Use dunnlist2
*    REFRESH dunnlist2.
*    dunnlist2-gsber    =  'BA'.
*    dunnlist2-rsg      =  'RSG'.
*    dunnlist2-ableinh  =  'MRU'.
*    dunnlist2-gpart    =  'BP'.
*    dunnlist2-vkont    =  'CAN'.
*    dunnlist2-printdoc =  'PRINTDOC'.
*    dunnlist2-name     =  'NAME'.
*    dunnlist2-addr     =  'ADDRESS'.
*    dunnlist2-budat    =  'POST DT'.
*    dunnlist2-current  =  'CURRENT'.
*    dunnlist2-pua      =  'PUA'.
*    dunnlist2-total    =  'TOTAL'.
*    dunnlist2-sortfld  =  'SEQ NO'.
*    dunnlist2-device   =  'DEVICE'.
*    dunnlist2-nodays   =  'DAYS'.
*    dunnlist2-oc       =  'OC'.
*    dunnlist2-oc2      =  'OC2'.
*    APPEND dunnlist2.
*  ELSEIF new EQ 'X' OR ddate EQ 'X'.
  "-- Use dunnlist3
  REFRESH dunnlist3.
*    dunnlist3-gsber    =  'BA'.
*    dunnlist3-rsg      =  'RSG'.
*    dunnlist3-ableinh  =  'MRU'.
*    dunnlist3-gpart    =  'BP'.
*    dunnlist3-vkont    =  'CAN'.
*    dunnlist3-printdoc =  'PRINTDOC'.
*    dunnlist3-name     =  'NAME'.
*    dunnlist3-addr     =  'ADDRESS'.
*    dunnlist3-budat    =  'POST DT'.
*    dunnlist3-current  =  'CURRENT'.
*    dunnlist3-pua      =  'PUA'.
*    dunnlist3-overdue  =  '61+_OVERDUE'.
*    dunnlist3-total    =  'TOTAL'.
*    dunnlist3-sortfld  =  'SEQ NO'.
*    dunnlist3-device   =  'DEVICE'.
*    dunnlist3-nodays   =  'DAYS'.
*    dunnlist3-oc       =  'OC'.
*    dunnlist3-oc2      =  'OC2'.
*    APPEND dunnlist3.
*  ENDIF.

  "-- For Old Selection
*  IF old = 'X'.  "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    PERFORM old_extract.
*  ELSE
  IF new = 'X'.
    PERFORM special_extract.
  ELSEIF ddate = 'X'.
    PERFORM extract_with_ddate.
  ENDIF.

*  IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    "-- Use dunnlist2
*    "-- Get Row Count of Local Table
*    DESCRIBE TABLE dunnlist2 LINES gv_cnt.
*  ELSE
  IF new EQ 'X' OR ddate EQ 'X'.
    "-- Use dunnlist3
    "-- Get Row Count of Local Table
    DESCRIBE TABLE dunnlist3 LINES gv_cnt.
  ENDIF.

  IF gv_cnt > 0.
    dunnlist3-gsber    =  'BA'.
    dunnlist3-rsg      =  'RSG'.
    dunnlist3-ableinh  =  'MRU'.
    dunnlist3-gpart    =  'BP'.
    dunnlist3-vkont    =  'CAN'.
    dunnlist3-printdoc =  'PRINTDOC'.
    dunnlist3-name     =  'NAME'.
    dunnlist3-addr     =  'ADDRESS'.
    dunnlist3-budat    =  'POST DT'.
    dunnlist3-current  =  'CURRENT'.
    dunnlist3-pua      =  'PUA'.
    dunnlist3-overdue  =  '61+_OVERDUE'.
    dunnlist3-total    =  'TOTAL'.
    dunnlist3-sortfld  =  'SEQ NO'.
    dunnlist3-device   =  'DEVICE'.
    dunnlist3-nodays   =  'DAYS'.
    dunnlist3-oc       =  'OC'.
    dunnlist3-oc2      =  'OC2'.
    INSERT dunnlist3 INDEX 1.

    "-- Download Data
    PERFORM download_data.

    "-- Make Distinct List
    SORT it_gsber BY gsber.
    DELETE: ADJACENT DUPLICATES FROM it_gsber,
            it_gsber WHERE gsber EQ 'BA'.

    "-- Send Email Notifications per BA.
    "IF old EQ 'X' OR ddate EQ 'X'. "Only if Regular or Due Date "GDIMALIWAT - Disable Regular Facility 03/07/2013
    IF ddate EQ 'X'.
      LOOP AT it_gsber ASSIGNING <fs_gsber> WHERE gsber NE 'BA'.
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

  DATA: lv_amt            TYPE dfkkop-betrh,
        lv_total          TYPE dfkkop-betrh,
        lv_msg            TYPE c LENGTH 25,
        lv_anlage         TYPE ever-anlage,
        lv_eqpt           TYPE egerh-equnr,
        lv_pia            TYPE dfkkop-betrh.

  DATA: lv_printdoc       TYPE c LENGTH 20,
        lv_posdt          TYPE erdk-budat,
        lv_current        TYPE char15,
        lv_pua            TYPE char15,
        lv_rsg            TYPE char8,
        lv_overdue        TYPE char15.

  DATA: lv_mindt          TYPE sy-datum,
        lv_maxdt          TYPE sy-datum,
        lv_diff           TYPE p.

  DATA: lv_contacct       TYPE fkkvkp-vkont,
        lv_ableinh        TYPE eablg-ableinh,
        lv_vstelle        TYPE evbs-vstelle,
        lv_haus           TYPE evbs-haus.

  DATA: lv_currdt         TYPE eabl-adatsoll,
        lv_opbel          TYPE erdk-opbel,
        lv_mrid           TYPE eabl-ablbelnr,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw,
        lv_adatsoll       TYPE eabl-adatsoll.

  DATA: lv_name           TYPE fkkvk-vkbez,
        lv_addr           TYPE char80.
  DATA: lv_sortfld        TYPE char6,
        lv_device         TYPE eabl-gernr.
  "--- Determine Date used for Comparison
  lv_current_date = sy-datum.

*  IF old EQ 'X'. "GDIMALIWAT - Disable Regular Facility 03/07/2013
*    lv_date_param = scheddt.
*  ELSE
  IF ddate EQ 'X'.
    lv_date_param = gv_highest_duedate.
  ENDIF.

  "--- Reset values
  CLEAR:  lv_rsg,
          lv_printdoc,
          lv_addr,
          lv_posdt,
          lv_current,
          lv_pua,
          lv_overdue,
          lv_total,
          lv_sortfld,
          lv_device,
          lv_diff,
          lv_oc,
          lv_oc2,
          lv_pia,
          lv_eqpt,
          lv_vstelle,
          lv_haus,
          lv_rsg,
          lv_maxdt,
          lv_mindt.

  REFRESH gt_dunn.
  REFRESH gt_dunn2.

  "--- Probill
  SELECT loobj1 lockr vkont
  INTO TABLE gt_probill
  FROM dfkklocks
  WHERE vkont = <fs_dunnhead>-contacct
  AND lockr = '9'.

  "-- DUNN List
  SELECT xblnr opbel budat bldat faedn SUM( betrh ) AS amount
    INTO TABLE gt_dunn
    FROM dfkkop
   WHERE vkont EQ <fs_dunnhead>-contacct
     AND hvorg IN ('0100', '0200')
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

  SORT gt_dunn BY faedn ASCENDING.
  LOOP AT gt_dunn ASSIGNING <fs_dunn>.
    lv_flag = '0'.
    LOOP AT gt_probill ASSIGNING <fs_probill> WHERE loobj1+0(12) EQ <fs_dunn>-ficadoc.
      lv_flag = '1'.
    ENDLOOP.
    UNASSIGN <fs_probill>.

    IF lv_flag NE '1'.
      APPEND INITIAL LINE TO gt_dunn2 ASSIGNING <fs_dunn2>.
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

  SORT gt_dunn2 BY faedn ASCENDING.

  LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
    lv_mindt = <fs_dunn2>-faedn.
    EXIT.
  ENDLOOP.
  UNASSIGN <fs_dunn2>.

  LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
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
  IF ddate EQ 'X'.
    lv_premise_param = lv_maxdt - 7.
  ENDIF.

  IF lv_diff > 60 OR ddate EQ'X'. "Don't check for 60 days if Due Date extraction
    "--- Get Installation Number
    SELECT SINGLE anlage
      INTO (lv_anlage)
      FROM ever
     WHERE vkonto EQ <fs_dunnhead>-contacct.

****( Get MRU )**************************************************************
    SELECT SINGLE ableinh
      INTO (lv_ableinh)
      FROM eanlh
     WHERE anlage EQ lv_anlage
       AND bis = '99991231'.

    SELECT ablhinw eablg~adatsoll eablg~ablbelnr
    INTO TABLE gt_oc
    FROM eablg
    INNER JOIN eabl ON eablg~ablbelnr = eabl~ablbelnr
    UP TO 1 ROWS
    WHERE anlage = <fs_dunnhead>-anlage
    AND ablesgr = '01'
    AND ablstat NE '0'
    ORDER BY eablg~adatsoll DESCENDING.

    READ TABLE gt_oc INDEX 1 ASSIGNING <fs_oc>.
    IF sy-subrc EQ 0.
      lv_oc = <fs_oc>-oc.
      lv_adatsoll = <fs_oc>-adatsoll.
      lv_mrid = <fs_oc>-mrid.
    ENDIF.

    SELECT SINGLE meterreadingnot2
    INTO lv_oc2
    FROM zmwosb_s01p
    WHERE mridnumber = lv_mrid.

    "-- Call Function Module for fetching PUA
*    IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*      PERFORM get_due_balance USING    <fs_dunnhead>-contacct
*                                       <fs_dunnhead>-gpart
*                              CHANGING lv_total
*                                       lv_pua
*                                       lv_current.
*    ELSE.
    PERFORM get_due_balance_by_duedate USING    <fs_dunnhead>-contacct
                                                <fs_dunnhead>-gpart
                                       CHANGING lv_total
                                                lv_pua
                                                lv_current
                                                lv_overdue.
*    ENDIF.

****( Get Equipment Number )*******************************************************
    SELECT SINGLE equnr
      INTO (lv_eqpt)
      FROM egerh INNER JOIN eastl ON egerh~logiknr = eastl~logiknr
     WHERE eastl~anlage = lv_anlage
       AND egerh~bis = '99991231'.

    gv_cnt = 1.

****( Get Premise Number )****************************************************
    SELECT SINGLE vstelle
      INTO (lv_vstelle)
      FROM eanlh INNER JOIN eanl ON eanlh~anlage = eanl~anlage
     WHERE eanlh~anlage = lv_anlage
       AND eanlh~ab LE lv_premise_param
       AND eanlh~bis GE lv_premise_param.

****( Get Connection Object )******************************************************
    SELECT SINGLE haus
      INTO (lv_haus)
      FROM evbs
     WHERE vstelle = lv_vstelle.

****( Get RSG )********************************************************************
    SELECT SINGLE regiogroup
      INTO lv_rsg
      FROM ehauisu
     WHERE haus = lv_haus.

    PERFORM name_address  CHANGING lv_name lv_addr.
    PERFORM get_sortfield CHANGING lv_sortfld lv_device lv_eqpt.
    SHIFT lv_printdoc LEFT DELETING LEADING '0'.

*    IF old EQ 'X'.   "GDIMALIWAT - Disable Regular Facility 03/07/2013
*      dunnlist2-gsber    = <fs_dunnhead>-gsber.
*      dunnlist2-rsg      = lv_rsg.
*      dunnlist2-ableinh  = lv_ableinh.
*      dunnlist2-gpart    = <fs_dunnhead>-gpart.
*      dunnlist2-vkont    = <fs_dunnhead>-contacct.
*      dunnlist2-printdoc = lv_printdoc.
*      dunnlist2-name     = lv_name.
*      dunnlist2-addr     = lv_addr.
*      dunnlist2-budat    = lv_posdt.
*      dunnlist2-current  = lv_current.
*      dunnlist2-pua      = lv_pua.
*      dunnlist2-total    = lv_total.
*      dunnlist2-sortfld  = lv_sortfld.
*      dunnlist2-device   = lv_device.
*      dunnlist2-nodays   = lv_diff.
*      dunnlist2-oc       = lv_oc.
*      dunnlist2-oc2      = lv_oc2.
*      "-- If total is greater than 80, include record to report
*      IF dunnlist2-total > 80.
*        APPEND dunnlist2.
*      ENDIF.
*    ELSE.
    dunnlist3-gsber    = <fs_dunnhead>-gsber.
    dunnlist3-rsg      = lv_rsg.
    dunnlist3-ableinh  = lv_ableinh.
    dunnlist3-gpart    = <fs_dunnhead>-gpart.
    dunnlist3-vkont    = <fs_dunnhead>-contacct.
    dunnlist3-printdoc = lv_printdoc.
    dunnlist3-name     = lv_name.
    dunnlist3-addr     = lv_addr.
    dunnlist3-budat    = lv_posdt.
    dunnlist3-current  = lv_current.
    dunnlist3-pua      = lv_pua.
    dunnlist3-overdue  = lv_overdue.
    dunnlist3-total    = lv_total.
    dunnlist3-sortfld  = lv_sortfld.
    dunnlist3-device   = lv_device.
    dunnlist3-nodays   = lv_diff.
    dunnlist3-oc       = lv_oc.
    dunnlist3-oc2      = lv_oc2.
    "-- If overdue is greater than 80 and if total is greater than or equal to 0, include record to report
    IF dunnlist3-overdue > 80 AND dunnlist3-total >= 0.
      APPEND dunnlist3.
    ENDIF.
    "    ENDIF.
  ENDIF.
ENDFORM.                    "DUNNING

*&---------------------------------------------------------------------*
*&      Form  CHECK_CLOSED
*&---------------------------------------------------------------------*
FORM check_closed CHANGING pv_note TYPE eablg-ablesgr.

  DATA: lv_diff       TYPE p,
        "lv_anlage     TYPE ever-anlage,
        lv_currdt     TYPE eabl-adatsoll,
        lv_opbel      TYPE erdk-opbel,
        lv_dunn_tag   TYPE char1,
        lv_x_flag     TYPE char1.

  DATA: BEGIN  OF mr OCCURS 0,
         adatsoll TYPE eabl-adatsoll,
         ablesgr TYPE eablg-ablesgr,
        END OF mr.

  DATA: lv_mrnote         TYPE eablg-ablesgr,
        lv_sched          TYPE eabl-adatsoll.

  lv_mrnote = space.
  lv_diff = 0.

  SELECT adatsoll ablesgr
  INTO TABLE mr
  FROM eablg
  WHERE anlage = <fs_dunnhead>-anlage
  AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
  AND ( ablesgr = '18' OR ablesgr = '13' ).

  SORT mr BY adatsoll DESCENDING.

  IF sy-subrc = 0.
    LOOP AT mr.
      IF mr-ablesgr = '13'.
*** CHECK IF HAS CURRENT BILL
        REFRESH gt_mr2.
        REFRESH gt_billdoc.
        lv_currdt = space.

        SELECT adatsoll ablesgr
        INTO TABLE gt_mr2
        FROM eablg
        WHERE anlage = <fs_dunnhead>-anlage
        AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
        AND ( ablesgr = '01' OR ablesgr = '06' ).

        SORT gt_mr2 BY adatsoll DESCENDING.

        LOOP AT gt_mr2 ASSIGNING <fs_mr2>.
          lv_currdt = <fs_mr2>-adatsoll.
          EXIT.
        ENDLOOP.
        UNASSIGN <fs_mr2>.
        IF ddate NE 'X'.
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
                lv_dunn_tag = 'X'.
              ELSE.
                SELECT SINGLE erdk~opbel INTO lv_opbel
                  FROM erdk INNER JOIN erchc
                  ON erdk~opbel = erchc~opbel
                  INNER JOIN erch
                  ON erchc~belnr = erch~belnr
                  WHERE
                    erdk~invoiced = 'X' AND
                    erchc~invoiced = 'X' AND
                    erdk~intopbel = space AND
                    erchc~intopbel = space AND
                    erchc~belnr = <fs_billdoc>-belnr.
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
      pv_note = mr-ablesgr.
      EXIT.
    ENDLOOP.

  ENDIF.
ENDFORM.                    "CHECK_CLOSED

*&---------------------------------------------------------------------*
*&      Form  name_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM name_address CHANGING
                    pv_name TYPE fkkvk-vkbez
                    pv_addr TYPE char80.

  DATA:  bpkind TYPE but000-bpkind.
  DATA: lastname(20), firstname(20).
  DATA: addrnum TYPE but020-addrnumber.

  CLEAR address.
  SELECT SINGLE name_last name_first bpkind
    INTO (lastname, firstname, bpkind)
    FROM but000
    WHERE partner = <fs_dunnhead>-gpart.

  IF bpkind = '0002'.
    SELECT SINGLE name_org1
    INTO (firstname)
    FROM but000 WHERE partner = <fs_dunnhead>-gpart.
  ENDIF.

  SELECT SINGLE addrnumber INTO addrnum FROM but020
  WHERE partner = <fs_dunnhead>-gpart.

  CONDENSE firstname.
  CONDENSE lastname.

  REPLACE ',' WITH space INTO firstname.
  REPLACE ',' WITH space INTO lastname.

  IF lastname = firstname.  " OR lastname = 'NO NAME'.
    lastname = space.
  ENDIF.

  CONCATENATE firstname lastname INTO pv_name SEPARATED BY space.
  "TRANSLATE dunnlist-name TO UPPER CASE.

  SELECT SINGLE roomnumber floor house_num1 house_num2 str_suppl1 street
         str_suppl2 str_suppl3 location city2 city1 post_code1
     INTO CORRESPONDING FIELDS OF address FROM adrc
     WHERE addrnumber = addrnum.

  REPLACE ',' WITH space INTO address-roomnumber.
  REPLACE ',' WITH space INTO address-floor.
  REPLACE ',' WITH space INTO address-house_num1.
  REPLACE ',' WITH space INTO address-house_num2.
  REPLACE ',' WITH space INTO address-str_suppl1.
  REPLACE ',' WITH space INTO address-street.
  REPLACE ',' WITH space INTO address-str_suppl2.
  REPLACE ',' WITH space INTO address-str_suppl3.
  REPLACE ',' WITH space INTO address-location.
  REPLACE ',' WITH space INTO address-city2.
  REPLACE ',' WITH space INTO address-city1.
  REPLACE ',' WITH space INTO address-post_code1.

  IF address-house_num1 = '0000'. address-house_num1 = space. ENDIF.
  IF address-street = 'NO NAME'. address-street = space. ENDIF.
  IF address-city2 = 'NO NAME'. address-city2 = space. ENDIF.
  IF address-city1 = 'NO NAME'. address-city1 = space. ENDIF.
  CONDENSE address.
  TRANSLATE address TO UPPER CASE.
  MOVE address TO pv_addr.
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

  DATA: devloc TYPE egerh-devloc.
  DATA: s_iloan TYPE iloa-iloan.

  SELECT SINGLE eqfnr sernr
    INTO (pv_sortfld, pv_device)
  FROM v_equi
  WHERE equnr EQ pv_eqpt
  AND datbi EQ '99991231'
  AND eqfnr NE '0'.
ENDFORM.                    "get_sortfield

*&---------------------------------------------------------------------*
*&      Form  CHECK_MRO_EXISTS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_mro_exists  USING    p_buspartner
                                p_begda
                                p_endda
                                p_subrc
                                p_doc_type.

  CLEAR p_subrc.
  DATA: tab_mrdoc TYPE TABLE OF bapieabl.
*** alternatively, we can select directly from EABL with ABLSTAT = 0.

  CALL FUNCTION 'BAPI_MTRREADDOC_GETLIST'
    EXPORTING
      customer         = p_buspartner
      targetmrdatefrom = p_begda
      targetmrdateto   = p_endda
      mrdocumenttype   = p_doc_type
    TABLES
      mrdocumentdata   = tab_mrdoc.

  IF tab_mrdoc[] IS INITIAL.
    p_subrc = 4.
  ENDIF.
ENDFORM.                    " CHECK_MRO_EXISTS

*&---------------------------------------------------------------------*
*&      Form  CONVERT_PORTION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM convert_portion USING p_port.
  IF p_port+5 GE 1 AND p_port+5 LE 9.
    CONCATENATE p_port(5) '0' p_port+5 INTO new_portion.
  ELSE.
    new_portion = 'BLANK'.
  ENDIF.
ENDFORM.                    "CONVERT_PORTION

*&---------------------------------------------------------------------*
*&      Form  check_if_blocked
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_if_blocked  USING    p_cac TYPE fkkvkp-vkont
                                p_blk.
  CLEAR p_blk.
  SELECT SINGLE abrsperr
  INTO p_blk
  FROM ever
  WHERE vkonto = p_cac
    AND abrsperr IS NOT NULL.
ENDFORM.                    " CHECK_IF_BLOCKED

*&---------------------------------------------------------------------*
*&      Form  SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_notification USING lv_gsber TYPE fkkvkp-gsber.
  "-- Checks if Checkbox is Ticked
  CHECK c_email EQ 'X'.

  DATA: lv_sender     TYPE so_rec_ext VALUE 'IT.OPERATION@MAYNILADWATER.COM.PH',  "Email Sender
        lv_receiver   TYPE so_recname ,                                           "Email Receiver
        lv_subject    TYPE so_obj_des VALUE 'Dunning List',                       "Email Subject
        it_messages   TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE,
        it_receivers  TYPE somlreci1         OCCURS 0 WITH HEADER LINE,
        lv_copy       TYPE c LENGTH 1,
        lv_ba_name    TYPE c LENGTH 50,
        lv_message    TYPE c LENGTH 250.

  DATA: it_email      TYPE TABLE OF zdunn_email_list.

  FIELD-SYMBOLS:
        <fs_email>    TYPE zdunn_email_list.

  DATA: r_gsber       TYPE RANGE OF fkkvkp-gsber,
        wa_gsber      LIKE LINE  OF r_gsber.

  DATA: lv_port_range    TYPE c LENGTH 15,
        lv_duedate_range TYPE c LENGTH 23,
        ddate_low        TYPE string,
        ddate_hi         TYPE string.

  DATA: lv_subrc      TYPE sysubrc.

  "-- Macro Definition for Adding Receivers
  DEFINE m_append_receivers.
    it_receivers-receiver = &1.
    if &2 eq 'X'.
      it_receivers-copy   = 'X'.
    endif.
    it_receivers-rec_type = 'U'.
    append it_receivers.
    clear  it_receivers.
  END-OF-DEFINITION.

  "-- Macro Definition for Adding Messages
  DEFINE m_append_messages.
    it_messages = &1.
    append  it_messages.
  END-OF-DEFINITION.

  "-- Macro Definition for Ranges
  DEFINE m_ranges.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  "-- Check if User is valid to send Email
  IF gv_valid_send EQ 0.
    "-- Initialize BA Range
    IF lv_gsber EQ '1100'.
      m_ranges: wa_gsber r_gsber lv_gsber,
                wa_gsber r_gsber `PS`,
                wa_gsber r_gsber `A21`.
    ELSE.
      m_ranges: wa_gsber r_gsber lv_gsber,
                wa_gsber r_gsber `PS`,
                wa_gsber r_gsber `OCF`.
    ENDIF.

    "-- Populate Email Receivers
    CLEAR   it_receivers.
    REFRESH it_receivers.

    "-- Fetch from database table (Per BA)
    SELECT *
    FROM   zdunn_email_list
      INTO CORRESPONDING FIELDS OF TABLE it_email
    WHERE  ba_code IN r_gsber.

    IF sy-subrc EQ 0.

      LOOP AT it_email ASSIGNING <fs_email>.
        IF <fs_email>-ba_name IS NOT INITIAL.
          CONCATENATE <fs_email>-ba_name `,` INTO lv_ba_name.
        ENDIF.

        m_append_receivers:
          <fs_email>-com_head_email <fs_email>-com_head_copy.

        IF <fs_email>-bo_head_email IS NOT INITIAL.
          m_append_receivers:
            <fs_email>-bo_head_email  <fs_email>-bo_head_copy.
        ENDIF.
      ENDLOOP.

*    m_append_receivers: 'bernadette.its@mayniladwater.com.ph' '',
*                        'michael.its@mayniladwater.com.ph'    '',
*                        'randy.its@mayniladwater.com.ph'      '',
*                        'reena.its@mayniladwater.com.ph'      ''.
      m_append_receivers: 'reena.its@mayniladwater.com.ph'      ''.
      "-- Populate Email Body
      CLEAR   it_messages.
      REFRESH it_messages.

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
      IF ddate EQ 'X'. "--Due Date
        CLEAR: ddate_low, ddate_hi.
        CONCATENATE gv_lowest_duedate+4(2) gv_lowest_duedate+6(2) gv_lowest_duedate(4) INTO ddate_low SEPARATED BY '/'.
        CONCATENATE gv_highest_duedate+4(2) gv_highest_duedate+6(2) gv_highest_duedate(4) INTO ddate_hi SEPARATED BY '/'.
        IF gv_lowest_duedate EQ gv_highest_duedate.
          MOVE ddate_hi TO lv_duedate_range.
        ELSE.
          CONCATENATE ddate_low
                      ddate_hi
                 INTO lv_duedate_range SEPARATED BY `-`.
        ENDIF.

        CONCATENATE  `Please be informed that the dunning list for`
                     lv_ba_name     "-- BA Name
                     `with due date`
                     lv_duedate_range  "-- Due Date Range
                     `is now available in`
                     spl_file   " \\saprep01.mayniladsap.com\Mtr_RdngINDRA\ZDunning.`
               INTO  lv_message
               SEPARATED BY space.
      ENDIF.

      m_append_messages:
          'Good Day!',
          '',
          lv_message,
          'Please confirm once received.',
          '',
          'Thank you!',
          '',
          'IT Production Support',
          'Information Technology',
          'Outsourcing Services',
          'Indra Philippines, Inc.'.

      "-- Call Custom Function Module
      CALL FUNCTION 'ZFM_SEND_SIMPLE_EMAIL'
        EXPORTING
          sender       = lv_sender
          subject      = lv_subject
        TABLES
          it_messages  = it_messages
          it_receivers = it_receivers.
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
  DATA: it_list TYPE vrm_values,
        wa_list TYPE vrm_value.

  DEFINE m_append_list.
    wa_list-key = &1.
    append wa_list to it_list.
    clear  wa_list.
  END-OF-DEFINITION.

  m_append_list: '0200', '0300', '0400', '0500', '0600', '0700', '0800', '0900', '1000', '1100', '1200', '1700'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'BA'
      values = it_list.
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'BA2'
      values = it_list.
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.
ENDFORM.                    " POPULATE_BA

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM select_file_path  USING    pa_path TYPE rlgrap-filename.
  DATA :
    lv_subrc  TYPE sy-subrc,
    lt_it_tab TYPE string.

  "-- Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Select Source File'
      initial_folder  = '\\saprep01.mayniladsap.com\Mtr_RdngINDRA\ZDunning\' "Inserted by Ann 2012/11/26
    CHANGING
      selected_folder = lt_it_tab.

  "-- Write path on input area
  IF lt_it_tab IS NOT INITIAL.
    CONCATENATE lt_it_tab `\` INTO pa_path.
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

*&---------------------------------------------------------------------*
*&      Form  GET_DUE_BALANCE
*&---------------------------------------------------------------------*
*       Subroutine for Fetching Due Balance of an Account
*----------------------------------------------------------------------*
FORM get_due_balance  USING    pv_can     TYPE vkont_kk
                               pv_bp      TYPE bu_partner
                      CHANGING pv_balance TYPE betrh_kk
                               pv_pua     TYPE char15
                               pv_current TYPE char15.

  DATA: lt_selection   TYPE TABLE OF bapifkkopselhead,
        lt_balances    TYPE TABLE OF bapifkkepos,

        lv_payment     TYPE char15.

  FIELD-SYMBOLS:
        <fs_selection> TYPE bapifkkopselhead,
        <fs_balances>  TYPE bapifkkepos.

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
      IF <fs_balances>-doc_type NE 'AB'.
        "-- Summarize Due Amounts
        pv_balance = pv_balance + <fs_balances>-amount.
      ELSE.
        "-- If Installment Plan and Already Due
        IF <fs_balances>-disc_due LT sy-datum.
          "-- Summarize Due Amounts
          pv_balance = pv_balance + <fs_balances>-amount.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    pv_balance = 0.
  ENDIF.

  "-- Get Current Debit
  PERFORM get_current_debit  USING    pv_can
                                      pv_bp
                             CHANGING pv_current.

  "-- PUA Computation
  pv_pua = pv_balance - pv_current.
ENDFORM.                    " GET_DUE_BALANCE

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
                                          pv_overdue TYPE char15.

  DATA: lt_selection   TYPE TABLE OF bapifkkopselhead,
        lt_balances    TYPE TABLE OF bapifkkepos,
        lv_payment     TYPE char15,
        lv_days60           TYPE dfkkop-faedn,
        lv_days30           TYPE dfkkop-faedn,
        lv_overdue          TYPE dfkkop-betrh,
        lv_overdue_cleared  TYPE dfkkop-betrh,
        lv_pua              TYPE dfkkop-betrh,
        lv_pua_cleared      TYPE dfkkop-betrh,
        lv_current          TYPE dfkkop-betrh,
        lv_current_cleared  TYPE dfkkop-betrh.

  DATA : lt_pua_overdue TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_current     TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat.

  FIELD-SYMBOLS:
        <fs_selection>    TYPE bapifkkopselhead,
        <fs_balances>     TYPE bapifkkepos,
        <fs_pua_overdue>  TYPE ty_pua_overdue,
        <fs_current>      TYPE ty_pua_overdue.

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
          IF <fs_balances>-disc_due LT gv_highest_duedate.
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
  lv_days60 = gv_highest_duedate - 60.
  lv_days30 = gv_highest_duedate - 30.

  SELECT opbel budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space .

  LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.
    CLEAR lv_overdue.
    CLEAR lv_overdue_cleared.

    SELECT SUM( betrh )
    INTO lv_overdue
    FROM dfkkopk
    WHERE opbel EQ <fs_pua_overdue>-opbel.

    IF sy-subrc EQ 0.
      lv_overdue = lv_overdue * -1.
      ADD lv_overdue TO pv_overdue.

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

    ENDIF.
  ENDLOOP .
  UNASSIGN <fs_pua_overdue>.

  "--PUA Computation
  SELECT opbel budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days30
  AND faedn GE lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space .

  LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.
    CLEAR lv_pua.
    CLEAR lv_pua_cleared.

    SELECT SUM( betrh )
    INTO lv_pua
    FROM dfkkopk
    WHERE opbel EQ <fs_pua_overdue>-opbel.

    IF sy-subrc EQ 0.
      lv_pua = lv_pua * -1.
      ADD lv_pua TO pv_pua.

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
    ENDIF.
  ENDLOOP .
  UNASSIGN <fs_pua_overdue>.

"--Current Computation
  SELECT opbel budat betrh
  INTO TABLE lt_current
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LE gv_highest_duedate
  AND faedn GE lv_days30
  AND budat NE '00000000'
  AND abwbl EQ space.

  LOOP AT lt_current ASSIGNING <fs_current>.
    CLEAR lv_current.
    CLEAR lv_current_cleared.

    SELECT SUM( betrh )
    INTO lv_current
    FROM dfkkopk
    WHERE opbel EQ <fs_current>-opbel.

    IF sy-subrc EQ 0.
      lv_current = lv_current * -1.
      ADD lv_current TO pv_current.

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
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_current>.

  IF pv_overdue IS INITIAL.
    pv_overdue = '0.00'.
  ENDIF.
  IF pv_pua IS INITIAL.
    pv_pua = '0.00'.
  ENDIF.
  IF pv_current IS INITIAL.
    pv_current = '0.00'.
  ENDIF.
ENDFORM.                    " GET_DUE_BALANCE_BY_DUEDATE

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_DEBIT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_current_debit  USING    pv_can     TYPE vkont_kk
                                 pv_bp      TYPE bu_partner
                        CHANGING pv_current TYPE char15.

  TYPES: BEGIN OF ty_current,
           doc_no     TYPE opbel_kk,
           current    TYPE betrw_kk,
         END OF ty_current.

  DATA:  lt_current   TYPE TABLE OF ty_current,
         lv_due_date  TYPE faedn_kk,
         lv_doc_no    TYPE opbel_kk,
         lv_current   TYPE betrw_kk.

  FIELD-SYMBOLS:
         <fs_current> TYPE ty_current.

  "-- Reinitialize Variables
  CLEAR: lv_due_date, lv_doc_no, pv_current, lv_current.

  "-- Get Document Number of Current Billing
  SELECT faedn opbel
    INTO (lv_due_date, lv_doc_no)
  FROM   dfkkop
  WHERE  gpart EQ pv_bp
  AND    vkont EQ pv_can
  AND    hvorg EQ '0100' "blart NE 'BA'
  ORDER BY faedn DESCENDING.
    EXIT.
  ENDSELECT.

  "-- Reinitialize Local Table for Current
  REFRESH: lt_current.

  "-- Get Current Debit Amount
  SELECT opbel betrw
    INTO TABLE lt_current
  FROM   dfkkopk
  WHERE  opbel EQ lv_doc_no.

  "-- Get Summation of Current
  LOOP AT lt_current ASSIGNING <fs_current>.
    lv_current = lv_current + <fs_current>-current.
  ENDLOOP.

  "-- Always positive values
  pv_current = abs( lv_current ).
ENDFORM.                    " GET_CURRENT_DEBIT

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_DEBIT_BY_DUEDATE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_current_debit_by_duedate  USING    pv_can     TYPE vkont_kk
                                 pv_bp      TYPE bu_partner
                        CHANGING pv_current TYPE char15.

  TYPES: BEGIN OF ty_current,
           doc_no     TYPE opbel_kk,
           current    TYPE betrw_kk,
         END OF ty_current.

  DATA:  lt_current   TYPE TABLE OF ty_current,
         lv_due_date  TYPE faedn_kk,
         lv_doc_no    TYPE opbel_kk,
         lv_current   TYPE betrw_kk.

  FIELD-SYMBOLS:
         <fs_current> TYPE ty_current.

  "-- Reinitialize Variables
  CLEAR: lv_due_date, lv_doc_no, pv_current, lv_current.

  "-- Get Document Number of Current Billing
  SELECT faedn opbel
    INTO (lv_due_date, lv_doc_no)
  FROM   dfkkop
  WHERE  gpart EQ pv_bp
  AND    vkont EQ pv_can
  AND    hvorg EQ '0100' "blart NE 'BA'
  AND    faedn IN so_ddate
  ORDER BY faedn DESCENDING.
    EXIT.
  ENDSELECT.

  "-- Reinitialize Local Table for Current
  REFRESH: lt_current.

  "-- Get Current Debit Amount
  SELECT opbel betrw
    INTO TABLE lt_current
  FROM   dfkkopk
  WHERE  opbel EQ lv_doc_no.

  "-- Get Summation of Current
  LOOP AT lt_current ASSIGNING <fs_current>.
    lv_current = lv_current + <fs_current>-current.
  ENDLOOP.

  "-- Always positive values
  pv_current = abs( lv_current ).
ENDFORM.                    " GET_CURRENT_DEBIT_BY_DUEDATE

*&---------------------------------------------------------------------*
*&      Form  IS_VALID_TO_SEND_EMAIL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM is_valid_to_send_email  USING    pv_uname TYPE syuname
                             CHANGING pv_subrc TYPE sysubrc.

  DATA: lv_uname TYPE syuname.

  CONSTANTS:
        co_role  TYPE agr_name VALUE 'Z_IT_ANALYST'.

  "-- Reinitialize Variable
  CLEAR: pv_subrc.

  "-- Check if PS User
  SELECT SINGLE ust04~bname
    INTO lv_uname
  FROM   ust04
  INNER  JOIN agr_1016
  ON     ust04~profile     EQ agr_1016~profile
  WHERE  agr_1016~agr_name EQ co_role
  AND    ust04~bname       EQ pv_uname.

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

    WHEN new.
      "-- e.g. 0000
      lv_ba_port = '0000-'.
      "-- e.g. \\172.18.1.240\Mtr_RdngINDRA\ZDunning\<folder>\0200(01-17)20120229.txt
      CONCATENATE spl_file
                lv_ba_port
                sy-datum
                'dunn.txt'
           INTO lv_file.
    WHEN ddate.
      "-- e.g. 0000
      CONCATENATE ba2 `-`
             INTO lv_ba_port.
      "-- e.g. \\172.18.1.240\Mtr_RdngINDRA\ZDunning\<folder>\0200(01-17)20120229.txt
      CONCATENATE spl_file
                lv_ba_port
                gv_highest_duedate
                'dunn.txt'
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
  IF new EQ 'X' OR ddate EQ 'X'.
    LOOP AT dunnlist3.
      IF dunnlist3-gsber NE 'BA'.
        "-- If not Header (not Column Names)
        REPLACE ALL OCCURRENCES OF ',' IN dunnlist3-pua WITH ''.
        CONDENSE dunnlist3-pua.
        "IF dunnlist3-pua GT 0.
          "TRANSFER dunnlist3 TO rpt.
          TRANSFER dunnlist3 TO lv_file.
        "ENDIF.
      ELSE.
        "-- If Header (Column Names)
        "TRANSFER dunnlist3 TO rpt.
        TRANSFER dunnlist3 TO lv_file.
      ENDIF.

      "-- Append to Main Itab
      IF dunnlist3-gsber NE 'BA'.
        PERFORM append_main_itab2 USING dunnlist3-gsber
                                        dunnlist3-rsg
                                        dunnlist3-ableinh
                                        dunnlist3-gpart
                                        dunnlist3-vkont
                                        dunnlist3-printdoc
                                        dunnlist3-name
                                        dunnlist3-addr
                                        dunnlist3-budat
                                        dunnlist3-current
                                        dunnlist3-pua
                                        dunnlist3-overdue
                                        dunnlist3-total
                                        dunnlist3-sortfld
                                        dunnlist3-device
                                        dunnlist3-nodays
                                        dunnlist3-oc
                                        dunnlist3-oc2.

        "-- Append to Itab of BA's
        wa_gsber-gsber = dunnlist3-gsber.
        APPEND wa_gsber TO it_gsber.
        CLEAR  wa_gsber.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "CLOSE DATASET rpt.
  CLOSE DATASET lv_file.
ENDFORM.                    " DOWNLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  APPEND_MAIN_ITAB
*&---------------------------------------------------------------------*
*   Append to Main Itab
*----------------------------------------------------------------------*
FORM append_main_itab  USING    pv_gsber    pv_rsg    pv_ableinh    pv_gpart    pv_vkont    pv_printdoc
                                pv_name     pv_addr   pv_budat      pv_current  pv_pua      pv_total
                                pv_sortfld  pv_device pv_nodays     pv_oc       pv_oc2.

  "-- Append to Main Itab
  APPEND INITIAL LINE TO gt_main ASSIGNING <fs_main>.

  <fs_main>-gsber    = pv_gsber.
  <fs_main>-rsg      = pv_rsg.
  <fs_main>-ableinh  = pv_ableinh.
  <fs_main>-gpart    = pv_gpart.
  <fs_main>-vkont    = pv_vkont.
  <fs_main>-printdoc = pv_printdoc.
  <fs_main>-name     = pv_name.
  <fs_main>-addr     = pv_addr.
  <fs_main>-budat    = pv_budat.
  <fs_main>-current  = pv_current.
  <fs_main>-pua      = pv_pua.
  <fs_main>-total    = pv_total.
  <fs_main>-sortfld  = pv_sortfld.
  <fs_main>-device   = pv_device.
  <fs_main>-nodays   = pv_nodays.
  <fs_main>-oc       = pv_oc.
  <fs_main>-oc2      = pv_oc2.
ENDFORM.                    " APPEND_MAIN_ITAB
*&---------------------------------------------------------------------*
*&      Form  APPEND_MAIN_ITAB2
*&---------------------------------------------------------------------*
*   Append to Main Itab
*----------------------------------------------------------------------*
FORM append_main_itab2  USING   pv_gsber    pv_rsg      pv_ableinh    pv_gpart    pv_vkont    pv_printdoc
                                pv_name     pv_addr     pv_budat      pv_current  pv_pua      pv_overdue
                                pv_total    pv_sortfld  pv_device     pv_nodays   pv_oc       pv_oc2.

  "-- Append to Main Itab
  APPEND INITIAL LINE TO gt_main2 ASSIGNING <fs_main2>.

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
  "-- Macro Definition - For Changing ALV Column Names
  DEFINE mc_change_column_names.
    try.
        go_column ?= go_columns->get_column( &1 ). "Pass Column Name
        go_column->set_long_text(   &1 ).          "Pass Long   Text
        go_column->set_medium_text( &2 ).          "Pass Medium Text
        go_column->set_short_text(  &2 ).          "Pass Short  Text
      catch cx_salv_not_found into go_salv_not_found.
        "-- Error Handler
        gv_string = go_salv_not_found->get_text( ).         "#EC NEEDED
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
  IF new EQ 'X' OR ddate EQ 'X'.
    "-- Create ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_table
          CHANGING
            t_table      = gt_main2.
      CATCH cx_salv_msg INTO go_salv_msg.
        "-- Error Handler
        gv_string = go_salv_msg->get_text( ).               "#EC NEEDED
        "-- Show Error Message
        MESSAGE gv_string TYPE 'I'.
    ENDTRY.
  ENDIF.

  "-- Enable all ALV Functions
  go_functions = go_table->get_functions( ).
  go_functions->set_all( 'X' ).

  "-- Get field attributes
  go_columns   = go_table->get_columns( ).

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
  mc_change_column_names: 'RSG'       'RSG',
                          'PRINTDOC'  'PrintDoc',
                          'ADDR'      'Address',
                          'BUDAT'     'Post Dt',
                          'CURRENT'   'Current',
                          'PUA'       'PUA',
                          'OVERDUE'   'Overdue',
                          'TOTAL'     'Total',
                          'SORTFLD'   'Seq.No',
                          'NODAYS'    'Days',
                          'OC'        'OC',
                          'OC2'       'OC2'.
*  ENDIF.

  "-- Optimize Field Length
  go_columns->set_optimize( 'X' ).

  "-- Display ALV table
  go_table->display( ).
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

  DATA: gt_dfkkop TYPE SORTED TABLE OF ty_dfkkop WITH NON-UNIQUE KEY vkont.

  DATA: lv_diff           TYPE p,
        lv_x_flag         TYPE char1,
        lv_anlart         TYPE eanl-anlart,
        lv_dunn_tag       TYPE char1.

  DATA: lv_mrid           TYPE eabl-ablbelnr,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw,
        lv_adatsoll       TYPE eabl-adatsoll.

  DATA: lv_currdt         TYPE eabl-adatsoll,
        lv_opbel          TYPE erdk-opbel.

  DATA: lv_mrnote         TYPE eablg-ablesgr,
        lv_note           TYPE eablg-ablesgr,
        lv_sched          TYPE eabl-adatsoll.

  DATA: lv_vkont          TYPE fkkvkp-vkont,
        lv_gpart          TYPE fkkvkp-gpart,
        lv_gsber          TYPE fkkvkp-gsber,
        lv_anlage         TYPE ever-anlage,
        lv_abrsperr       TYPE ever-abrsperr,
        lv_ableinh        TYPE eanlh-ableinh,
        lv_vkbez          TYPE fkkvk-vkbez.

  FIELD-SYMBOLS: <fs_dfkkop> TYPE ty_dfkkop.

  SELECT vkont
  INTO TABLE gt_dfkkop
  FROM dfkkop
  WHERE hvorg IN ('0100', '0200')
    AND augrd NE '05'
    AND augst EQ space
    AND faedn IN so_ddate
    AND budat NE '00000000'
    AND abwbl EQ space.

  DELETE ADJACENT DUPLICATES FROM gt_dfkkop COMPARING vkont.

  LOOP AT gt_dfkkop ASSIGNING <fs_dfkkop>.
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
      AND gsber EQ ba2
      AND gsber NE '1800'
      AND kofiz_sd NOT IN ('02','22').

    IF sy-subrc EQ 0.
* Check if the account is not moved-out
* Check if the account is not tagged for deletion
      SELECT SINGLE anlage abrsperr
      INTO (lv_anlage, lv_abrsperr)
      FROM ever
      WHERE vkonto EQ <fs_dfkkop>-vkont
        AND auszdat  EQ '99991231'
        AND loevm    NE 'X'.

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

          APPEND INITIAL LINE TO gt_dunnhead ASSIGNING <fs_dunnhead>.
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

  LOOP AT gt_dunnhead ASSIGNING <fs_dunnhead>.
    lv_x_flag = space.
    lv_note   = space.
    REFRESH gt_billdoc.
    lv_dunn_tag = space.
    lv_diff = 0.

    LOOP AT gt_condo ASSIGNING <fs_condo> WHERE vkont = <fs_dunnhead>-contacct.
******************CHECK IF ACCOUNT UNDER CONDO*********************************
      IF <fs_condo>-cond_tag EQ 'X'.
        lv_x_flag = 'X'.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_condo>.

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
        REFRESH gt_mr2.
        REFRESH gt_billdoc.
        lv_currdt = space.

        SELECT adatsoll ablesgr
        INTO TABLE gt_mr2
        FROM eablg
        WHERE anlage = <fs_dunnhead>-anlage
        AND adatsoll LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
        AND ( ablesgr = '01' OR ablesgr = '06' ).

        SORT gt_mr2 BY adatsoll DESCENDING.

        LOOP AT gt_mr2 ASSIGNING <fs_mr2>.
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
                SELECT SINGLE erdk~opbel
                INTO lv_opbel
                FROM erdk
                INNER JOIN erchc ON erdk~opbel = erchc~opbel
                INNER JOIN erch  ON erchc~belnr = erch~belnr
                WHERE
                  erdk~invoiced = 'X' AND
                  erchc~invoiced = 'X' AND
                  erdk~intopbel = space AND
                  erchc~intopbel = space AND
                  erchc~belnr = <fs_billdoc>-belnr.
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

  DATA: begda TYPE sy-datum.
  DATA: endda TYPE sy-datum.
  endda = begda = gv_highest_duedate.
  begda+6(2) = '01'.
  CALL FUNCTION 'DETERMINE_END_OF_MONTH'
    EXPORTING
      i_datum = begda
    IMPORTING
      e_tt    = endda+6(2).
  DATA: blk_code TYPE ever-abrsperr.
  DELETE dunnlist3 WHERE oc = '21' OR oc2 = '03' OR oc = '22' OR gsber = '1800'.
  LOOP AT dunnlist3.
    PERFORM check_mro_exists USING dunnlist3-gpart begda endda mro_subrc doc_type_order.
    IF mro_subrc = 0.
      WRITE:/ 'With MRO -', dunnlist3-gpart.
      DELETE dunnlist3.
    ENDIF.
    PERFORM check_if_blocked USING dunnlist3-vkont blk_code.
    IF NOT blk_code IS INITIAL.
      WRITE:/ 'Account blocked -', dunnlist3-gpart.
      DELETE dunnlist3.
    ENDIF.
  ENDLOOP.

  "-- Get number of records in dunnlist3
  DESCRIBE TABLE dunnlist3 LINES gv_count.
ENDFORM.                    " EXTRACT_WITH_DDATE
*&---------------------------------------------------------------------*
*&      Form  DUEDATE_PARAM_CHECK
*&---------------------------------------------------------------------*
*       Check Due Date
*----------------------------------------------------------------------*
FORM duedate_param_check .
  LOOP AT so_ddate.
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

  LOOP AT so_ddate.
    IF gv_lowest_duedate IS INITIAL.
      gv_lowest_duedate = so_ddate-low.
    ELSE.
      "If the highest due date is greater than so_ddate-low
      IF gv_lowest_duedate GT so_ddate-low.
        gv_lowest_duedate = so_ddate-low.
      ENDIF.
    ENDIF.
    "If the highest due date is greater than so_ddate-high
    IF gv_lowest_duedate GT so_ddate-high.
      gv_lowest_duedate = so_ddate-high.
    ENDIF.
  ENDLOOP.

  IF so_ddate-high IS INITIAL.
    LOOP AT so_ddate.
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

    LOOP AT so_ddate.
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
    LOOP AT so_ddate.
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

    LOOP AT so_ddate.
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

  DATA: lv_name        TYPE fkkvk-vkbez,
        lv_addr        TYPE char80.

  DATA: lv_amt            TYPE dfkkop-betrh,
        lv_total          TYPE dfkkop-betrh,
        lv_msg            TYPE c LENGTH 25,
        lv_eqpt           TYPE egerh-equnr,
        lv_pia            TYPE dfkkop-betrh.

  DATA: lv_printdoc    TYPE c LENGTH 20,
        lv_posdt       TYPE erdk-budat,
        lv_current     TYPE char15,
        lv_pua         TYPE char15,
        lv_rsg         TYPE char8,
        lv_overdue     TYPE char15.

  DATA: lv_mindt          TYPE sy-datum,
        lv_maxdt          TYPE sy-datum,
        lv_diff           TYPE p.

  DATA: lv_sortfld        TYPE char6,
        lv_device         TYPE eabl-gernr,
        lv_vstelle        TYPE evbs-vstelle,
        lv_haus           TYPE evbs-haus,
        lv_mrid           TYPE eabl-ablbelnr,
        lv_oc             TYPE eabl-ablhinw,
        lv_oc2            TYPE eabl-ablhinw,
        lv_adatsoll       TYPE eabl-adatsoll.

  lv_current_date = sy-datum.

  SELECT fkkvkp~gsber gpart vkont vkbez ever~anlage abrsperr eanlh~ableinh
    INTO TABLE gt_dunnhead
    FROM fkkvkp INNER JOIN ever ON fkkvkp~vkont = ever~vkonto
                INNER JOIN eanlh ON ever~anlage = eanlh~anlage
    WHERE fkkvkp~vkont IN can
    AND fkkvkp~gsber NE '1800' "exclude KAM accounts
    AND fkkvkp~gpart IN par
    AND auszdat = '99991231'
    AND eanlh~bis = '99991231'
    AND ever~loevm NE 'X'
    AND fkkvkp~kofiz_sd NE '02'
    AND fkkvkp~kofiz_sd NE '22'.

  LOOP AT gt_dunnhead ASSIGNING <fs_dunnhead>.

    "--- Reset values
    CLEAR:  lv_rsg,
            lv_printdoc,
            lv_addr,
            lv_posdt,
            lv_current,
            lv_pua,
            lv_overdue,
            lv_total,
            lv_sortfld,
            lv_device,
            lv_diff,
            lv_oc,
            lv_oc2,
            lv_pia,
            lv_eqpt,
            lv_vstelle,
            lv_haus,
            lv_rsg,
            lv_maxdt,
            lv_mindt.
    REFRESH gt_dunn.
    REFRESH gt_dunn2.

    PERFORM name_address CHANGING lv_name lv_addr.

    SELECT xblnr opbel budat bldat faedn SUM( betrh ) AS amount
    INTO TABLE gt_dunn
    FROM dfkkop
    WHERE vkont EQ <fs_dunnhead>-contacct
      AND hvorg IN ('0100', '0200')
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

    SORT gt_dunn BY faedn ASCENDING.
    LOOP AT gt_dunn ASSIGNING <fs_dunn>.
      lv_flag = '0'.
      LOOP AT gt_probill ASSIGNING <fs_probill> WHERE loobj1+0(12) EQ <fs_dunn>-ficadoc.
        lv_flag = '1'.
      ENDLOOP.
      UNASSIGN <fs_probill>.

      IF lv_flag NE '1'.
        APPEND INITIAL LINE TO gt_dunn2 ASSIGNING <fs_dunn2>.
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

    SORT gt_dunn2 BY faedn ASCENDING.

    LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
      lv_mindt = <fs_dunn2>-faedn.
      EXIT.
    ENDLOOP.
    UNASSIGN <fs_dunn2>.

    LOOP AT gt_dunn2 ASSIGNING <fs_dunn2>.
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
       AND egerh~bis = '99991231'.

    CLEAR lv_device.
    CLEAR lv_sortfld.
    SELECT SINGLE eqfnr sernr
      INTO (lv_sortfld, lv_device)
      FROM v_equi
      WHERE equnr EQ lv_eqpt
      AND datbi EQ '99991231'
      AND eqfnr NE '0'.
***  DEVICE SEQ***

*** RSG ***
    CLEAR lv_vstelle.
    SELECT SINGLE vstelle
    INTO (lv_vstelle)
    FROM eanlh INNER JOIN eanl ON eanlh~anlage = eanl~anlage
    WHERE eanlh~anlage = <fs_dunnhead>-anlage
    AND eanlh~ab LE sy-datum "scheddt "GDIMALIWAT - Disable Regular Facility 03/07/2013
    AND eanlh~bis GE sy-datum. "scheddt. "GDIMALIWAT - Disable Regular Facility 03/07/2013

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
    INTO TABLE gt_oc
    FROM eablg
    INNER JOIN eabl ON eablg~ablbelnr = eabl~ablbelnr
    UP TO 1 ROWS
    WHERE anlage = <fs_dunnhead>-anlage
    AND ablesgr = '01'
    AND ablstat NE '0'
    ORDER BY eablg~adatsoll DESCENDING.

    READ TABLE gt_oc INDEX 1 ASSIGNING <fs_oc>.
    IF sy-subrc EQ 0.
      lv_oc = <fs_oc>-oc.
      lv_adatsoll = <fs_oc>-adatsoll.
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
                                                lv_overdue.

    "-- Delete leading zeroes from Printdoc
    SHIFT lv_printdoc LEFT DELETING LEADING '0'.

    dunnlist3-gsber    = <fs_dunnhead>-gsber.
    dunnlist3-rsg      = lv_rsg.
    dunnlist3-ableinh  = <fs_dunnhead>-ableinh.
    dunnlist3-gpart    = <fs_dunnhead>-gpart.
    dunnlist3-vkont    = <fs_dunnhead>-contacct.
    dunnlist3-printdoc = lv_printdoc.
    dunnlist3-name     = <fs_dunnhead>-name.
    dunnlist3-addr     = lv_addr.
    dunnlist3-budat    = lv_posdt.
    dunnlist3-current  = lv_current.
    dunnlist3-pua      = lv_pua.
    dunnlist3-overdue  = lv_overdue.
    dunnlist3-total    = lv_total.
    dunnlist3-sortfld  = lv_sortfld.
    dunnlist3-device   = lv_device.
    dunnlist3-nodays   = lv_diff.
    dunnlist3-oc       = lv_oc.
    dunnlist3-oc2      = lv_oc2.
    APPEND dunnlist3.
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
                                          pv_overdue TYPE char15.

  DATA: lt_selection   TYPE TABLE OF bapifkkopselhead,
        lt_balances    TYPE TABLE OF bapifkkepos,
        lv_payment     TYPE char15,
        lv_days60           TYPE dfkkop-faedn,
        lv_days30           TYPE dfkkop-faedn,
        lv_overdue          TYPE dfkkop-betrh,
        lv_overdue_cleared  TYPE dfkkop-betrh,
        lv_pua              TYPE dfkkop-betrh,
        lv_pua_cleared      TYPE dfkkop-betrh,
        lv_current          TYPE dfkkop-betrh,
        lv_current_cleared  TYPE dfkkop-betrh.

  DATA : lt_pua_overdue TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat,
         lt_current     TYPE SORTED TABLE OF ty_pua_overdue WITH NON-UNIQUE KEY budat.

  FIELD-SYMBOLS:
        <fs_selection>    TYPE bapifkkopselhead,
        <fs_balances>     TYPE bapifkkepos,
        <fs_pua_overdue>  TYPE ty_pua_overdue,
        <fs_current>      TYPE ty_pua_overdue.

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
          IF <fs_balances>-disc_due LT sy-datum.
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
  lv_days60 = sy-datum - 60.
  lv_days30 = sy-datum - 30.

  SELECT opbel budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space .

  LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.
    CLEAR lv_overdue.
    CLEAR lv_overdue_cleared.

    SELECT SUM( betrh )
    INTO lv_overdue
    FROM dfkkopk
    WHERE opbel EQ <fs_pua_overdue>-opbel.

    IF sy-subrc EQ 0.
      lv_overdue = lv_overdue * -1.
      ADD lv_overdue TO pv_overdue.

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

    ENDIF.
  ENDLOOP .
  UNASSIGN <fs_pua_overdue>.

  "--PUA Computation
  SELECT opbel budat betrh
  INTO TABLE lt_pua_overdue
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LT lv_days30
  AND faedn GE lv_days60
  AND budat NE '00000000'
  AND abwbl EQ space .

  LOOP AT lt_pua_overdue ASSIGNING <fs_pua_overdue>.
    CLEAR lv_pua.
    CLEAR lv_pua_cleared.

    SELECT SUM( betrh )
    INTO lv_pua
    FROM dfkkopk
    WHERE opbel EQ <fs_pua_overdue>-opbel.

    IF sy-subrc EQ 0.
      lv_pua = lv_pua * -1.
      ADD lv_pua TO pv_pua.

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
    ENDIF.
  ENDLOOP .
  UNASSIGN <fs_pua_overdue>.

  "--Current Computation
  SELECT opbel budat betrh
  INTO TABLE lt_current
  FROM dfkkop
  WHERE vkont EQ pv_can
  AND hvorg IN ('0100', '0200')
  AND augrd NE '05'
  AND augst EQ space
  AND faedn LE sy-datum
  AND faedn GE lv_days30
  AND budat NE '00000000'
  AND abwbl EQ space.

  LOOP AT lt_current ASSIGNING <fs_current>.
    CLEAR lv_current.
    CLEAR lv_current_cleared.

    SELECT SUM( betrh )
    INTO lv_current
    FROM dfkkopk
    WHERE opbel EQ <fs_current>-opbel.

    IF sy-subrc EQ 0.
      lv_current = lv_current * -1.
      ADD lv_current TO pv_current.

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
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_current>.

  IF pv_overdue IS INITIAL.
    pv_overdue = '0.00'.
  ENDIF.
  IF pv_pua IS INITIAL.
    pv_pua = '0.00'.
  ENDIF.
  IF pv_current IS INITIAL.
    pv_pua = '0.00'.
  ENDIF.
ENDFORM.                    " GET_DUE_BALANCE_BY_SPECIAL
*&---------------------------------------------------------------------*
*&      Form  GET_CONDONATION_ACCOUNTS
*&---------------------------------------------------------------------*
*       Retrieve condonation accounts
*----------------------------------------------------------------------*
FORM get_condonation_accounts .
  REFRESH gt_condo.

  SELECT vkont cond_tag
  INTO TABLE gt_condo
  FROM zcondonation
  WHERE cond_tag = 'X'.
ENDFORM.                    " GET_CONDONATION_ACCOUNTS
*&---------------------------------------------------------------------*
*&      Form  DEVELOPER_PARAMS
*&---------------------------------------------------------------------*
*       For developer purposes
*----------------------------------------------------------------------*
FORM developer_params .
  DATA: lv_uname TYPE sy-uname.

  lv_uname = sy-uname.

  IF lv_uname EQ 'GDIMALIWAT'.
    spl_file  = '\\172.18.1.240\repuserdata\Glenn\ZDUNNLIST\'.
  ENDIF.
ENDFORM.                    " DEVELOPER_PARAMS
**&---------------------------------------------------------------------*
**&      Form  BACKTRACK
**&---------------------------------------------------------------------*
**       Subtract 22 days from all input due dates
**----------------------------------------------------------------------*
*FORM backtrack .
*  SUBTRACT 22 FROM gv_lowest_duedate.
*  SUBTRACT 22 FROM gv_highest_duedate.
*
*  LOOP AT so_ddate.
*    IF so_ddate-low IS NOT INITIAL.
*      SUBTRACT 22 FROM so_ddate-low.
*    ENDIF.
*    IF so_ddate-high IS NOT INITIAL.
*      SUBTRACT 22 FROM so_ddate-high.
*    ENDIF.
*    MODIFY so_ddate FROM so_ddate INDEX sy-tabix.
*  ENDLOOP.
*ENDFORM.                    " BACKTRACK
