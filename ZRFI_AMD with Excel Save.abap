*&---------------------------------------------------------------------*
*& Report  ZRFI_AMD
*& DESCRIPTION: Asset Master Data List
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 10/29/2013
*& Reference  : 47360
*&---------------------------------------------------------------------*
*& Date      | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/10/29| GDIMALIWAT | 47360      | Created based on old ZAAMD4 SAP Query
*& 2013/12/11| GDIMALIWAT | 51252      | Changed Date format to MM/DD/YYYY
*& 2013/12/12| GDIMALIWAT | 51375      | Added SERNR column to the output / Removed leading zeroes from Serial Number
*& 2014/02/10| PMEDIOS    |            |
*& 2014/03/25| JLIWAG     | 55003      | Removed Dowload to excel button and changed ZESS_ASSETDATA program to ZRFI_AMD_V3
*&---------------------------------------------------------------------*
REPORT zrfi_amd_v3 MESSAGE-ID zisu.
*INCLUDE
TABLES: anla, anlb, anlz, anlu.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_amd,
        "--- ANLA
        bukrs               TYPE anla-bukrs,            "Company Code
        anln1               TYPE anla-anln1,            "Main Asset Number
        anlkl               TYPE anla-anlkl,            "Asset Class
        ernam               TYPE anla-ernam,            "Name of Person who Created the Object
        erdat               TYPE anla-erdat,            "Date on which Record was Created
        xloev               TYPE anla-xloev,            "Indicator: Account Marked for Deletion?
        xspeb               TYPE anla-xspeb,            "Indicator: Asset Locked to Acquisition Postings
        aktiv               TYPE anla-aktiv,            "Asset Capitalization Date
        deakt               TYPE anla-deakt,            "Deactivation Date
        ord41               TYPE anla-ord41,            "Evaluation Group 1 (Status)
        ord42               TYPE anla-ord42,            "Evaluation Group 2 (Asset Item Description)
        lifnr               TYPE anla-lifnr,            "Account number of vendor (other keyword)
        aibn1               TYPE anla-aibn1,            "Original asset that was transferred
        eaufn               TYPE anla-eaufn,            "Investment Order
        txt50               TYPE anla-txt50,            "Asset description
        txa50               TYPE anla-txa50,            "Asset description (2)
        gdlgrp              TYPE anla-gdlgrp,           "Evaluation group 5
        sernr               TYPE anla-sernr,            "Serial number
        "--- ANLB
        afabe               TYPE anlb-afabe,            "Real depreciation area
        afabg               TYPE anlb-afabg,            "Depreciation calculation start date
        afasl               TYPE anlb-afasl,            "Depreciation key
        ndjar               TYPE anlb-ndjar,            "Planned useful life in years
        ndper               TYPE anlb-ndper,            "Planned useful life in periods
        "--- ANLZ
        kostl               TYPE anlz-kostl,            "Cost Center
        "--- ANLU
        zzold_prop_num      TYPE anlu-zzold_prop_num,   "Old Property Number
        zzbrand             TYPE anlu-zzbrand,          "Brand
        zztype              TYPE anlu-zztype,           "Type
        zzacqui_cost        TYPE anlu-zzacqui_cost,     "Acquisition Cost
        zzengas_code        TYPE anlu-zzengas_code,     "Engas Code
        zzartcle_code       TYPE anlu-zzartcle_code,    "Article Code
        zzcontract_num      TYPE anlu-zzcontract_num,   "Contract Number
        zzcontract_desc     TYPE anlu-zzcontract_desc,  "Contract Description
        zzlocation          TYPE anlu-zzlocation,       "Location
        zzasset_category    TYPE anlu-zzasset_category, "Asset Category
        zzloan_package      TYPE anlu-zzloan_package,   "Loan Package
        zz_loan_currency    TYPE anlu-zz_loan_currency, "Loan Package Currency
        zz_proof_owner      TYPE anlu-zz_proof_owner,   "Proof of Ownership
        zz_lot_no           TYPE anlu-zz_lot_no,        "Lot No.
        zz_survey_no        TYPE anlu-zz_survey_no,     "Survey No.
        zz_acqui_date       TYPE anlu-zz_acqui_date,    "Acquisition/Delivery Date
        zz_plate_no         TYPE anlu-zz_plate_no,      "Plate No.
        zz_engine_no        TYPE anlu-zz_engine_no,     "Engine No.
        zz_chassis_no       TYPE anlu-zz_chassis_no,    "Chassis No.
        zz_aadf_no          TYPE anlu-zz_aadf_no,       "AADF No.
        zz_aadf_date        TYPE anlu-zz_aadf_date,     "AADF Date
        zz_or_no            TYPE anlu-zz_or_no,         "OR No.
        zz_or_date          TYPE anlu-zz_or_date,       "OR Date
        zz_zone             TYPE anlu-zz_zone,          "Zone/Grid
        zz_rr_no            TYPE anlu-zz_rr_no,         "RR No.
        zz_rr_date          TYPE anlu-zz_rr_date,       "RR Date
        zz_invtag_no        TYPE anlu-zz_invtag_no,     "Inventory Tag No.
        zz_former_acct      TYPE anlu-zz_former_acct,   "Former Accountable
        zz_remarks          TYPE anlu-zz_remarks,       "Remarks
        zzdate_modified     TYPE anlu-zzdate_modified,  "Date Modified
        "--- CSKT
        datbi               TYPE cskt-datbi,            "Valid To Date
        ktext               TYPE cskt-ktext,            "General Name
*        "--- T087S
*        gdlgrp_txt          TYPE t087s-gdlgrp_txt,      "Text for evaluation group 8 places
*        "--- T087T
*        ord4x               TYPE t087t-ord4x,           "Evaluation groups 1 - 4
*        ordtx               TYPE t087t-ordtx,           "Evaluation group:Short description
        "--- ANEP
        "anbtr               TYPE anep-anbtr,            "Acquisition Value
        ivdat               TYPE anla-ivdat,             "Last inventory on
        inken               TYPE anla-inken,             "Inventory List
        invzu               TYPE anla-invzu,             "Inventory Note
        ltext               TYPE cskt-ltext,             "Cost Center Name
       END OF ty_amd,

       BEGIN OF ty_output,
         anln1               TYPE anla-anln1,            "Main Asset Number
         bukrs               TYPE anla-bukrs,            "Company Code
         anlkl               TYPE anla-anlkl,            "Asset Class
         txt50               TYPE anla-txt50,            "Asset description
         txa50               TYPE anla-txa50,            "Asset Description 2
         ordtx2              TYPE t087t-ordtx,           "Generic Description
         sernr               TYPE anla-sernr,            "Serial number
         aktiv               TYPE c LENGTH 10,"anla-aktiv,            "Asset Capitalization Date
         deakt               TYPE c LENGTH 10,"anla-deakt,            "Deactivation Date
         kostl               TYPE anlz-kostl,            "Cost Center
         ltext               TYPE cskt-ltext,            "Cost Center Name
         gdlgrp              TYPE anla-gdlgrp,           "Evaluation group 5
         gdlgrp_txt          TYPE t087s-gdlgrp_txt,      "Evaluation group 5
         ivdat               TYPE anla-ivdat,            "Last inventory on
         inken               TYPE anla-inken,            "Inventory List
         invzu               TYPE anla-invzu,            "Inventory Note
         ernam               TYPE anla-ernam,            "Name of Person who Created the Object
         erdat               TYPE c LENGTH 10,"anla-erdat,            "Date on which Record was Created
         datbi               TYPE c LENGTH 10,"cskt-datbi,            "Valid To Date
         zzold_prop_num      TYPE anlu-zzold_prop_num,   "Old Property Number
         zzbrand             TYPE anlu-zzbrand,          "Brand
         zztype              TYPE anlu-zztype,           "Type
         zzacqui_cost        TYPE anlu-zzacqui_cost,     "Acquisition Cost
         anbtr               TYPE anep-anbtr,            "Acquisition Value
         zzengas_code        TYPE anlu-zzengas_code,     "Engas Code
         zzartcle_code       TYPE anlu-zzartcle_code,    "Article Code
         zzcontract_desc     TYPE anlu-zzcontract_desc,  "Contract Description
         zzcontract_num      TYPE anlu-zzcontract_num,   "Contract Number
         zzlocation          TYPE anlu-zzlocation,       "Location
         zzasset_category    TYPE anlu-zzasset_category, "Asset Category
         zzloan_package      TYPE anlu-zzloan_package,   "Loan Package
         zz_loan_currency    TYPE anlu-zz_loan_currency, "Loan Package Currency
         zz_proof_owner      TYPE anlu-zz_proof_owner,   "Proof of Ownership
         zz_lot_no           TYPE anlu-zz_lot_no,        "Lot No.
         zz_survey_no        TYPE anlu-zz_survey_no,     "Survey No.
         zz_acqui_date       TYPE c LENGTH 30,"anlu-zz_acqui_date,    "Acquisition/Delivery Date
         zz_plate_no         TYPE anlu-zz_plate_no,      "Plate No.
         zz_engine_no        TYPE anlu-zz_engine_no,     "Engine No.
         zz_chassis_no       TYPE anlu-zz_chassis_no,    "Chassis No.
         zz_aadf_no          TYPE anlu-zz_aadf_no,       "AADF No.
         zz_aadf_date        TYPE c LENGTH 30,"anlu-zz_aadf_date,     "AADF Date
         zz_or_no            TYPE anlu-zz_or_no,         "OR No.
         zz_or_date          TYPE c LENGTH 30,"anlu-zz_or_date,       "OR Date
         zz_zone             TYPE anlu-zz_zone,          "Zone/Grid
         zz_rr_no            TYPE anlu-zz_rr_no,         "RR No.
         zz_rr_date          TYPE c LENGTH 30,"anlu-zz_rr_date,       "RR Date
         zz_invtag_no        TYPE anlu-zz_invtag_no,     "Inventory Tag No.
         zz_former_acct      TYPE anlu-zz_former_acct,   "Former Accountable
         ordtx1              TYPE t087t-ordtx,           "Status Description
         zz_remarks          TYPE anlu-zz_remarks,       "Remarks
         afabe               TYPE anlb-afabe,            "Real depreciation area
       END OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_amd      TYPE TABLE OF ty_amd,       "#EC NEEDED
      gt_output   TYPE TABLE OF ty_output,    "#EC NEEDED
      gt_fieldcat TYPE lvc_t_fcat.            "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: go_tab             TYPE REF TO data,                     "#EC NEEDED
      go_structure       TYPE REF TO data,                     "#EC NEEDED
      go_table           TYPE REF TO cl_salv_table,            "#EC NEEDED
      go_functions       TYPE REF TO cl_salv_functions_list,   "#EC NEEDED
      go_columns         TYPE REF TO cl_salv_columns_table,    "#EC NEEDED
      go_column          TYPE REF TO cl_salv_column_table,     "#EC NEEDED
      go_settings        TYPE REF TO cl_salv_display_settings, "#EC NEEDED
      go_aggregations    TYPE REF TO cl_salv_aggregations,     "#EC NEEDED
      go_toolbar         TYPE REF TO cl_salv_functions_list,   "#EC NEEDED
      go_content         TYPE REF TO cl_salv_form_element,     "#EC NEEDED
      go_layout          TYPE REF TO cl_salv_layout,           "#EC NEEDED
      go_exc             TYPE REF TO cx_salv_error,            "#EC NEEDED
      go_salv_msg        TYPE REF TO cx_salv_msg,              "#EC NEEDED
      go_salv_not_found  TYPE REF TO cx_salv_not_found,        "#EC NEEDED
      go_salv_data_error TYPE REF TO cx_salv_data_error,       "#EC NEEDED
      go_salv_existing   TYPE REF TO cx_salv_existing.         "#EC NEEDED

* Start - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
                            IMPORTING e_salv_function.

ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM save_data_to_excel USING e_salv_function.
  ENDMETHOD.

ENDCLASS.

DATA: go_event_handler TYPE REF TO lcl_handle_events,
      go_events        TYPE REF TO cl_salv_events_table.
* End - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_date_separator TYPE c LENGTH 1 VALUE '/'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_amd>         TYPE ty_amd,                "#EC NEEDED
               <fs_output>      TYPE ty_output,             "#EC NEEDED
               <fs_fieldcat>    TYPE lvc_s_fcat,            "#EC NEEDED
               <ft_tab>         TYPE STANDARD TABLE,        "#EC NEEDED
               <fv_wa>          TYPE any,                   "#EC NEEDED
               <fv_field>       TYPE any.                   "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
SELECT-OPTIONS: so_anln1  FOR anla-anln1,                       "Main Asset Number
                so_bukrs  FOR anla-bukrs DEFAULT 'MWSI',        "Company Code
                so_anlkl  FOR anla-anlkl,                       "Asset Class
                so_txt50  FOR anla-txt50,                       "Asset description
                so_ord41  FOR anla-ord41,                       "Status Description
                so_ord42  FOR anla-ord42,                       "Generic Description
                so_aktiv  FOR anla-aktiv,                       "Asset Capitalization Date
                so_deakt  FOR anla-deakt,                       "Deactivation Date
                so_kostl  FOR anlz-kostl,                       "Cost Center
                so_evgp5  FOR anla-gdlgrp,                      "Evaluation group 5
                so_ernam  FOR anla-ernam,                       "Name of Person who Created the Object
                so_erdat  FOR anla-erdat,                       "Date on which Record was Created
                "so_datbi  FOR cskt-datbi,                      "Valid To Date
                so_oldpn  FOR anlu-zzold_prop_num,              "Old Property Number
                so_brand  FOR anlu-zzbrand,                     "Brand
                so_type   FOR anlu-zztype,                      "Type
                so_acost  FOR anlu-zzacqui_cost,                "Acquisition Cost
                so_ecode  FOR anlu-zzengas_code,                "Engas Code
                so_acode  FOR anlu-zzartcle_code,               "Article Code
                so_cdesc  FOR anlu-zzcontract_desc,             "Contract Description
                so_cnum   FOR anlu-zzcontract_num,              "Contract Number
                so_loc    FOR anlu-zzlocation,                  "Location
                so_ascat  FOR anlu-zzasset_category,            "Asset Category
                so_loanp  FOR anlu-zzloan_package,              "Loan Package
                so_loanc  FOR anlu-zz_loan_currency,            "Loan Package Currency
                so_pownr  FOR anlu-zz_proof_owner,              "Proof of Ownership
                so_lotno  FOR anlu-zz_lot_no,                   "Lot No.
                so_svyno  FOR anlu-zz_survey_no,                "Survey No.
                so_adate  FOR anlu-zz_acqui_date,               "Acquisition/Delivery Date
                so_platn  FOR anlu-zz_plate_no,                 "Plate No.
                so_engin  FOR anlu-zz_engine_no,                "Engine No.
                so_chasn  FOR anlu-zz_chassis_no,               "Chassis No.
                so_aadfn  FOR anlu-zz_aadf_no,                  "AADF No.
                so_aadfd  FOR anlu-zz_aadf_date,                "AADF Date
                so_orno   FOR anlu-zz_or_no,                    "OR No.
                so_ordat  FOR anlu-zz_or_date,                  "OR Date
                so_zone   FOR anlu-zz_zone,                     "Zone/Grid
                so_rrno   FOR anlu-zz_rr_no,                    "RR No.
                so_rrdat  FOR anlu-zz_rr_date,                  "RR Date
                so_invtn  FOR anlu-zz_invtag_no,                "Inventory Tag No.
                so_facct  FOR anlu-zz_former_acct,              "Former Accountable
                so_rmrks  FOR anlu-zz_remarks,                  "Remarks
                so_afabe  FOR anlb-afabe.                       "Real depreciation area
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
INITIALIZATION.
  "-- Macro Definition for Appending to Global Cluster Table
  DEFINE mc_append_cluster.
    append initial line to gt_cluster assigning <fs_cluster>.
    <fs_cluster>-pernr       = &1.
    <fs_cluster>-seq_no      = &2.
    <fs_cluster>-end_payroll = &3.
  END-OF-DEFINITION.

  "-- Macro Definition for Appending Field Catalogue Table
  DEFINE mc_append_fieldcat.
    "-- Append to Field Catalogue Table
    append initial line to gt_fieldcat assigning <fs_fieldcat>.
    <fs_fieldcat>-fieldname  = &1.
    <fs_fieldcat>-tabname    = &2.
    <fs_fieldcat>-datatype   = &3.
    <fs_fieldcat>-outputlen  = &4.
    <fs_fieldcat>-decimals_o = &5.
    <fs_fieldcat>-col_pos    = &6.

    if &7 is not initial.
      "-- Set Field Names for End Periods
      <fs_fieldcat>-scrtext_l = &7.
      <fs_fieldcat>-scrtext_m = &7.
      <fs_fieldcat>-scrtext_s = &7.
    else.
      "-- Set Field Names for End Periods
      <fs_fieldcat>-domname   = &1.
    endif.
  END-OF-DEFINITION.

  "-- Macro Definition for Setting ALV Summarized Field
  DEFINE mc_set_summarized.
    try .
        go_aggregations->add_aggregation( columnname = &1 ).
      catch cx_salv_not_found into go_salv_not_found.
        message go_salv_not_found type 'I' display like 'E'.
      catch cx_salv_data_error into go_salv_data_error.
        message go_salv_data_error type 'I' display like 'E'.
      catch cx_salv_existing into go_salv_existing.
        message go_salv_existing type 'I' display like 'E'.
    endtry.
  END-OF-DEFINITION.

  "-- Macro Definition - For Changing ALV Column Names
  DEFINE mc_change_column_names.
    try.
        go_column ?= go_columns->get_column( &1 ). "-- Pass Column Name
        go_column->set_long_text(   &2 ).          "-- Pass Long   Text
        go_column->set_medium_text( &3 ).          "-- Pass Medium Text
        go_column->set_short_text(  &4 ).          "-- Pass Short  Text
      catch cx_salv_error into go_exc.                  "#EC NO_HANDLER
        message go_exc type 'I' display like 'E'.
    endtry.
  END-OF-DEFINITION.

  "-- Macro Definition - Set a Field to Invisible
  DEFINE mc_set_invisible.
    try.
        go_column ?= go_columns->get_column( &1 ).
        go_column->set_visible( ' ' ).
      catch cx_salv_not_found into go_salv_not_found.
        message go_salv_not_found type 'I' display like 'E'. "#EC NEEDED
    endtry.
  END-OF-DEFINITION.

  "-- Macro Definition for Conversion Alpha
  DEFINE mc_conversion_alpha.
    "-- Add Zero Paddings
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &1.
  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  SELECT a~bukrs a~anln1 a~anlkl a~ernam a~erdat a~xloev a~xspeb a~aktiv a~deakt a~ord41 a~ord42 a~lifnr a~aibn1 a~eaufn
         a~txt50 a~txa50 a~gdlgrp a~sernr b~afabe b~afabg b~afasl b~ndjar b~ndper z~kostl u~zzold_prop_num u~zzbrand
         u~zztype u~zzacqui_cost u~zzengas_code u~zzartcle_code u~zzcontract_num u~zzcontract_desc u~zzlocation
         u~zzasset_category u~zzloan_package u~zz_loan_currency u~zz_proof_owner u~zz_lot_no u~zz_survey_no u~zz_acqui_date
         u~zz_plate_no u~zz_engine_no u~zz_chassis_no u~zz_aadf_no u~zz_aadf_date u~zz_or_no u~zz_or_date u~zz_zone u~zz_rr_no
         u~zz_rr_date u~zz_invtag_no u~zz_former_acct u~zz_remarks u~zzdate_modified c~datbi c~ktext a~ivdat a~inken
         a~invzu c~ltext
    INTO TABLE gt_amd
    FROM ( ( ( ( anla AS a
      INNER JOIN anlb AS b  ON a~anln1 = b~anln1  )
      INNER JOIN anlz AS z  ON a~anln1 = z~anln1  )
      INNER JOIN anlu AS u  ON a~anln1 = u~anln1
                          AND a~bukrs = u~bukrs  )
      INNER JOIN cskt AS c ON z~kostl  = c~kostl )
    WHERE a~anln1            IN so_anln1  "Main Asset Number
      AND a~bukrs            IN so_bukrs  "Company Code
      AND b~bukrs            IN so_bukrs  "Company Code
      AND z~bukrs            IN so_bukrs  "Company Code
      AND u~bukrs            IN so_bukrs  "Company Code
      AND a~anlkl            IN so_anlkl  "Asset Class
      AND a~txt50            IN so_txt50  "Asset description
      AND a~ord41            IN so_ord41  "Status Description
      AND a~ord42            IN so_ord42  "Generic Description
      AND a~aktiv            IN so_aktiv  "Asset Capitalization Date
      AND a~deakt            IN so_deakt  "Deactivation Date
      AND z~kostl            IN so_kostl  "Cost Center
      AND a~gdlgrp           IN so_evgp5  "Evaluation group 5
      AND a~ernam            IN so_ernam  "Name of Person who Created the Object
      AND a~erdat            IN so_erdat  "Date on which Record was Created
      "AND c~datbi            IN so_datbi  "Valid To Date
      AND c~datbi            EQ '99991231' "Valid To Date
      AND u~zzold_prop_num   IN so_oldpn  "Old Property Number
      AND u~zzbrand          IN so_brand  "Brand
      AND u~zztype           IN so_type   "Type
      AND u~zzacqui_cost     IN so_acost  "Acquisition Cost
      AND u~zzengas_code     IN so_ecode  "Engas Code
      AND u~zzartcle_code    IN so_acode  "Article Code
      AND u~zzcontract_desc  IN so_cdesc  "Contract Description
      AND u~zzcontract_num   IN so_cnum   "Contract Number
      AND u~zzlocation       IN so_loc    "Location
      AND u~zzasset_category IN so_ascat  "Asset Category
      AND u~zzloan_package   IN so_loanp  "Loan Package
      AND u~zz_loan_currency IN so_loanc  "Loan Package Currency
      AND u~zz_proof_owner   IN so_pownr  "Proof of Ownership
      AND u~zz_lot_no        IN so_lotno  "Lot No.
      AND u~zz_survey_no     IN so_svyno  "Survey No.
      AND u~zz_acqui_date    IN so_adate  "Acquisition/Delivery Date
      AND u~zz_plate_no      IN so_platn  "Plate No.
      AND u~zz_engine_no     IN so_engin  "Engine No.
      AND u~zz_chassis_no    IN so_chasn  "Chassis No.
      AND u~zz_aadf_no       IN so_aadfn  "AADF No.
      AND u~zz_aadf_date     IN so_aadfd  "AADF Date
      AND u~zz_or_no         IN so_orno   "OR No.
      AND u~zz_or_date       IN so_ordat  "OR Date
      AND u~zz_zone          IN so_zone   "Zone/Grid
      AND u~zz_rr_no         IN so_rrno   "RR No.
      AND u~zz_rr_date       IN so_rrdat  "RR Date
      AND u~zz_invtag_no     IN so_invtn  "Inventory Tag No.
      AND u~zz_former_acct   IN so_facct  "Former Accountable
      AND u~zz_remarks       IN so_rmrks  "Remarks
      AND b~afabe            IN so_afabe  "Real depreciation area
      AND z~adatu            LE sy-datum  "System Date Check FROM
      AND z~bdatu            GE sy-datum. "System Date Check TO

  IF sy-subrc EQ 0.
*    LEFT JOIN t087s AS s ON a~gdlgrp = s~gdlgrp )
*      LEFT JOIN t087t AS t ON a~ord41  = t~ord4x
*                          AND a~ord42  = t~ord4x  )
*      INNER JOIN cskt AS c ON z~kostl  = c~kostl  )
    "c~datbi c~ktext s~gdlgrp_txt t~ord4x t~ordtx
    LOOP AT gt_amd ASSIGNING <fs_amd>.
      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
      <fs_output>-anln1            = <fs_amd>-anln1.
      <fs_output>-bukrs            = <fs_amd>-bukrs.
      <fs_output>-anlkl            = <fs_amd>-anlkl.
      <fs_output>-txt50            = <fs_amd>-txt50.
*      <fs_output>-ordtx1           = <fs_amd>-ordtx1.
*      <fs_output>-ordtx2           = <fs_amd>-ordtx2.
      "<fs_output>-aktiv            = <fs_amd>-aktiv.
      <fs_output>-sernr            = <fs_amd>-sernr.
      SHIFT <fs_output>-sernr LEFT DELETING LEADING '0'.
      CONCATENATE <fs_amd>-aktiv+4(2) <fs_amd>-aktiv+6(2) <fs_amd>-aktiv(4) INTO <fs_output>-aktiv SEPARATED BY co_date_separator.
      "<fs_output>-deakt            = <fs_amd>-deakt.
      CONCATENATE <fs_amd>-deakt+4(2) <fs_amd>-deakt+6(2) <fs_amd>-deakt(4) INTO <fs_output>-deakt SEPARATED BY co_date_separator.
      <fs_output>-kostl            = <fs_amd>-kostl.
      <fs_output>-gdlgrp           = <fs_amd>-gdlgrp.
      <fs_output>-ernam            = <fs_amd>-ernam.
      "<fs_output>-erdat            = <fs_amd>-erdat.
      CONCATENATE <fs_amd>-erdat+4(2) <fs_amd>-erdat+6(2) <fs_amd>-erdat(4) INTO <fs_output>-erdat SEPARATED BY co_date_separator.
      "<fs_output>-datbi            = <fs_amd>-datbi.
      CONCATENATE <fs_amd>-datbi+4(2) <fs_amd>-datbi+6(2) <fs_amd>-datbi(4) INTO <fs_output>-datbi SEPARATED BY co_date_separator.
      <fs_output>-zzold_prop_num   = <fs_amd>-zzold_prop_num.
      <fs_output>-zzbrand          = <fs_amd>-zzbrand.
      <fs_output>-zztype           = <fs_amd>-zztype.
      <fs_output>-zzacqui_cost     = <fs_amd>-zzacqui_cost.
      <fs_output>-zzengas_code     = <fs_amd>-zzengas_code.
      <fs_output>-zzartcle_code    = <fs_amd>-zzartcle_code.
      <fs_output>-zzcontract_desc  = <fs_amd>-zzcontract_desc.
      <fs_output>-zzcontract_num   = <fs_amd>-zzcontract_num.
      <fs_output>-zzlocation       = <fs_amd>-zzlocation.
      <fs_output>-zzasset_category = <fs_amd>-zzasset_category.
      <fs_output>-zzloan_package   = <fs_amd>-zzloan_package.
      <fs_output>-zz_loan_currency = <fs_amd>-zz_loan_currency.
      <fs_output>-zz_proof_owner   = <fs_amd>-zz_proof_owner.
      <fs_output>-zz_lot_no        = <fs_amd>-zz_lot_no.
      <fs_output>-zz_survey_no     = <fs_amd>-zz_survey_no.
      <fs_output>-zz_acqui_date    = <fs_amd>-zz_acqui_date.
      "CONCATENATE <fs_amd>-zz_acqui_date+4(2) <fs_amd>-zz_acqui_date+6(2) <fs_amd>-zz_acqui_date(4) INTO <fs_output>-zz_acqui_date SEPARATED BY co_date_separator.
      <fs_output>-zz_plate_no      = <fs_amd>-zz_plate_no.
      <fs_output>-zz_engine_no     = <fs_amd>-zz_engine_no.
      <fs_output>-zz_chassis_no    = <fs_amd>-zz_chassis_no.
      <fs_output>-zz_aadf_no       = <fs_amd>-zz_aadf_no.
      <fs_output>-zz_aadf_date     = <fs_amd>-zz_aadf_date.
      "CONCATENATE <fs_amd>-zz_aadf_date+4(2) <fs_amd>-zz_aadf_date+6(2) <fs_amd>-zz_aadf_date(4) INTO <fs_output>-zz_aadf_date SEPARATED BY co_date_separator.
      <fs_output>-zz_or_no         = <fs_amd>-zz_or_no.
      <fs_output>-zz_or_date       = <fs_amd>-zz_or_date.
      "CONCATENATE <fs_amd>-zz_or_date+4(2) <fs_amd>-zz_or_date+6(2) <fs_amd>-zz_or_date(4) INTO <fs_output>-zz_or_date SEPARATED BY co_date_separator.
      <fs_output>-zz_zone          = <fs_amd>-zz_zone.
      <fs_output>-zz_rr_no         = <fs_amd>-zz_rr_no.
      <fs_output>-zz_rr_date       = <fs_amd>-zz_rr_date.
      "CONCATENATE <fs_amd>-zz_rr_date+4(2) <fs_amd>-zz_rr_date+6(2) <fs_amd>-zz_rr_date(4) INTO <fs_output>-zz_rr_date SEPARATED BY co_date_separator.
      <fs_output>-zz_invtag_no     = <fs_amd>-zz_invtag_no.
      <fs_output>-zz_former_acct   = <fs_amd>-zz_former_acct.
      <fs_output>-zz_remarks       = <fs_amd>-zz_remarks.
      <fs_output>-afabe            = <fs_amd>-afabe.
      <fs_output>-txa50            = <fs_amd>-txa50.
      <fs_output>-ivdat            = <fs_amd>-ivdat.
      <fs_output>-inken            = <fs_amd>-inken.
      <fs_output>-invzu            = <fs_amd>-invzu.
      <fs_output>-ltext            = <fs_amd>-ltext.

      "Text for Evaluation Group 1
      SELECT SINGLE ordtx
      INTO <fs_output>-ordtx1
      FROM t087t
      WHERE ordnr EQ '1'
      AND ord4x EQ <fs_amd>-ord41. "#EC WARNOK

      "Text for Evaluation Group 2
      SELECT SINGLE ordtx
      INTO <fs_output>-ordtx2
      FROM t087t
      WHERE ordnr EQ '2'
      AND ord4x EQ <fs_amd>-ord42. "#EC WARNOK

      "Text for Evaluation Group 5
      SELECT SINGLE gdlgrp_txt
      INTO <fs_output>-gdlgrp_txt
      FROM t087s
      WHERE gdlgrp EQ <fs_amd>-gdlgrp. "#EC WARNOK

*      "Valid To Date and General Name for Cost Center
*      SELECT SINGLE datbi" ktext
*      INTO <fs_output>-datbi", lv_ktext)
*      FROM cskt
*      WHERE kostl EQ <fs_amd>-kostl.

      "Acquisition value
      SELECT SUM( anbtr )
      INTO <fs_output>-anbtr
      FROM anep
      WHERE bukrs = <fs_amd>-bukrs
        AND anln1 = <fs_amd>-anln1
        AND afabe = <fs_amd>-afabe.
      IF sy-subrc NE 0.
        <fs_output>-anbtr = 0.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_amd>.
  ENDIF.


*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM sort_data.
  PERFORM build_catalogue.
  PERFORM assign_data_to_alv.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOGUE
*&---------------------------------------------------------------------*
*       Subroutine for Building Field Catalogue
*----------------------------------------------------------------------*
FORM build_catalogue.

  "-- Reinitialize Field Catalogue Table
  REFRESH: gt_fieldcat.

  "-- Append to Field Catalogue
  mc_append_fieldcat: 'ANLN1'               'ANLA'    'CHAR'  12  0 1   space,
                      'BUKRS'               'ANLA'    'CHAR'   4  0 2   space,
                      'ANLKL'               'ANLA'    'CHAR'   8  0 3   space,
                      'TXT50'               'ANLA'    'CHAR'  50  0 4   space,
                      'TXA50'               'ANLA'    'CHAR'  50  0 5   space,
                      'ORDTX2'              'T087T'   'CHAR'  30  0 6   space,
                      'SERNR'               'ANLA'    'CHAR'  18  0 7   space,
                      'AKTIV'               'ANLA'    'CHAR'  10  0 8   space,
                      'DEAKT'               'ANLA'    'CHAR'  10  0 9   space,
                      'KOSTL'               'ANLZ'    'CHAR'  10  0 10   space,
                      'LTEXT'               'CSKT'    'CHAR'  40  0 11   space,
                      'GDLGRP'              'ANLA'    'CHAR'   8  0 12  space,
                      'GDLGRP_TXT'          'T087S'   'CHAR'  50  0 13  space,
                      'IVDAT'               'ANLA'    'CHAR'   8  0 14  space,
                      'INKEN'               'ANLA'    'CHAR'   1  0 15  space,
                      'INVZU'               'ANLA'    'CHAR'  15  0 16  space,
                      'ERNAM'               'ANLA'    'CHAR'  12  0 17  space,
                      'ERDAT'               'ANLA'    'CHAR'  10  0 18  space,
                      'DATBI'               'CSKT'    'CHAR'  10  0 19  space,
                      'ZZOLD_PROP_NUM'      'ANLU'    'CHAR'  20  0 20  space,
                      'ZZBRAND'             'ANLU'    'CHAR'  40  0 21  space,
                      'ZZTYPE'              'ANLU'    'CHAR'  40  0 22  space,
                      'ZZACQUI_COST'        'ANLU'    'CURR'  13  2 23  space,
                      'ANBTR'               'ANEP'    'CURR'  13  2 24  space,
                      'ZZENGAS_CODE'        'ANLU'    'CHAR'  30  0 25  space,
                      'ZZARTCLE_CODE'       'ANLU'    'CHAR'  30  0 26  space,
                      'ZZCONTRACT_DESC'     'ANLU'    'CHAR'  40  0 27  space,
                      'ZZCONTRACT_NUM'      'ANLU'    'CHAR'  30  0 28  space,
                      'ZZLOCATION'          'ANLU'    'CHAR'  40  0 29  space,
                      'ZZASSET_CATEGORY'    'ANLU'    'CHAR'  16  0 30  space,
                      'ZZLOAN_PACKAGE'      'ANLU'    'CHAR'  10  0 31  space,
                      'ZZ_LOAN_CURRENCY'    'ANLU'    'CHAR'   3  0 32  space,
                      'ZZ_PROOF_OWNER'      'ANLU'    'CHAR'  20  0 33  space,
                      'ZZ_LOT_NO'           'ANLU'    'CHAR'  30  0 34  space,
                      'ZZ_SURVEY_NO'        'ANLU'    'CHAR'  30  0 35  space,
                      'ZZ_ACQUI_DATE'       'ANLU'    'CHAR'  30  0 36  space,
                      'ZZ_PLATE_NO'         'ANLU'    'CHAR'  16  0 37  space,
                      'ZZ_ENGINE_NO'        'ANLU'    'CHAR'  30  0 38  space,
                      'ZZ_CHASSIS_NO'       'ANLU'    'CHAR'  30  0 39  space,
                      'ZZ_AADF_NO'          'ANLU'    'CHAR'  30  0 40  space,
                      'ZZ_AADF_DATE'        'ANLU'    'CHAR'  30  0 41  space,
                      'ZZ_OR_NO'            'ANLU'    'CHAR'  30  0 42  space,
                      'ZZ_OR_DATE'          'ANLU'    'CHAR'  30  0 43  space,
                      'ZZ_ZONE'             'ANLU'    'CHAR'  30  0 44  space,
                      'ZZ_RR_NO'            'ANLU'    'CHAR'  30  0 45  space,
                      'ZZ_RR_DATE'          'ANLU'    'CHAR'  30  0 46  space,
                      'ZZ_INVTAG_NO'        'ANLU'    'CHAR'  30  0 47  space,
                      'ZZ_FORMER_ACCT'      'ANLU'    'CHAR'  30  0 48  space,
                      'ORDTX1'              'T087T'   'CHAR'  30  0 49  space,
                      'ZZ_REMARKS'          'ANLU'    'CHAR'  30  0 50  space,
                      'AFABE'               'ANLB'    'CHAR'   2  0 51  space.
  "-- Reintialize Itab
  IF <ft_tab> IS ASSIGNED.
    REFRESH: <ft_tab>.
  ENDIF.

  "-- Generate Dynamic Itab based on ALV field catalog
  cl_alv_table_create=>create_dynamic_table(
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
      ep_table        = go_tab ).

  ASSIGN go_tab->* TO <ft_tab>.
ENDFORM.                    " BUILD_CATALOGUE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*         Subroutine for Displaying Upload Report
*----------------------------------------------------------------------*
FORM display_report.

  DATA: lv_key  TYPE salv_s_layout_key.

  "-- Create ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_table
        CHANGING
          t_table      = <ft_tab>.
    CATCH cx_salv_msg INTO go_salv_msg.                 "#EC NO_HANDLER
      MESSAGE go_salv_msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

  "-- Enable all ALV Functions
  go_functions = go_table->get_functions( ).
  go_functions->set_all( abap_true ).

  "-- Enable Layout Save
  go_layout = go_table->get_layout( ).
  lv_key-report = sy-repid.
  go_layout->set_key( lv_key ).
  go_functions->set_layout_save( abap_true ).
  go_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  go_layout->set_default( abap_true ).

  "-- Get field attributes
  go_columns   = go_table->get_columns( ).

  "-- Change ALV Column Name
*  mc_change_column_names: 'ANLN1'             text-001    text-001    'Asset No.',"'ANLN1',
*                          'BUKRS'             text-002    text-002    'Co. Code',"'BUKRS',
*                          'ANLKL'             text-003    text-003    'AssetClass',"'ANLKL',
*                          'TXT50'             text-004    text-004    'AssetDesc',"'TXT50',
*                          'TXA50'             text-056    text-061    'AssetDesc2',
*                          'ORDTX2'            text-006    text-006    'A.ItemDesc',"'ORDTX2',
*                          'SERNR'             text-063    text-064    'Serial No.',"'AKTIV',
*                          'AKTIV'             text-007    text-050    'Cap. Date',"'AKTIV',
*                          'DEAKT'             text-008    text-008    'Deact.Date',"'DEAKT',
*                          'KOSTL'             text-009    text-009    'CostCenter',"'KOSTL',
*                          'LTEXT'             text-060    text-060    'CC Name',
*                          'GDLGRP'            text-010    text-010    'Eval Grp 5',"'GDLGRP',
*                          'GDLGRP_TXT'        text-054    text-055    'Ev.Grp5Txt',"'GDLGRP_TXT',
*                          'IVDAT'             text-057    text-057    'Invty Date',
*                          'INKEN'             text-058    text-058    'Invty List',
*                          'INVZU'             text-059    text-059    'Invty Note',
*                          'ERNAM'             text-011    text-011    'Created By',"'ERNAM',
*                          'ERDAT'             text-012    text-012    'Created On',"'ERDAT',
*                          'DATBI'             text-013    text-013    'CC Date To',"'DATBI',
*                          'ZZOLD_PROP_NUM'    text-014    text-014    'OldProp.No',"'OLP.PROPNO',
*                          'ZZBRAND'           text-015    text-015    'Brand',"'BRAND',
*                          'ZZTYPE'            text-016    text-016    'Type',"'TYPE',
*                          'ZZACQUI_COST'      text-017    text-017    'Acq. Cost',"'ACQ.COST',
*                          'ANBTR'             text-044    text-044    'Acq. Value',"'ANBTR'.
*                          'ZZENGAS_CODE'      text-018    text-018    'Engas Code',"'ENGASCD',
*                          'ZZARTCLE_CODE'     text-019    text-019    'ArticlCode',"'ARTCLECD',
*                          'ZZCONTRACT_DESC'   text-020    text-020    'Contr Desc',"'CONT_DESC',
*                          'ZZCONTRACT_NUM'    text-021    text-021    'Contr No',"'CONT_NUM',
*                          'ZZLOCATION'        text-022    text-022    'Location',"'LOCATION',
*                          'ZZASSET_CATEGORY'  text-023    text-023    'AssetCateg',"'ASSET_CAT',
*                          'ZZLOAN_PACKAGE'    text-024    text-024    'Loan Pack',"'LOAN_PACK',
*                          'ZZ_LOAN_CURRENCY'  text-025    text-051    'Loan Ccy',"'LOAN_CURR',
*                          'ZZ_PROOF_OWNER'    text-026    text-026    'ProofOfOwn',"'PROOF_OWN',
*                          'ZZ_LOT_NO'         text-027    text-027    'Lot No.',"'LOT_NO',
*                          'ZZ_SURVEY_NO'      text-028    text-028    'Survey No.',"'SURVEY_NO',
*                          'ZZ_ACQUI_DATE'     text-029    text-053    'Acq. Date',"'ACQUI_DATE',
*                          'ZZ_PLATE_NO'       text-030    text-030    'Plate No.',"'PLATE_NO',
*                          'ZZ_ENGINE_NO'      text-031    text-031    'Engine No.',"'ENGINE_NO',
*                          'ZZ_CHASSIS_NO'     text-032    text-032    'Chassis No',"'CHASSIS_NO',
*                          'ZZ_AADF_NO'        text-033    text-033    'AADF No.',"'AADF_NO',
*                          'ZZ_AADF_DATE'      text-034    text-034    'AADF Date',"'AADF_DATE',
*                          'ZZ_OR_NO'          text-035    text-035    'OR No.',"'OR_NO',
*                          'ZZ_OR_DATE'        text-036    text-036    'OR Date',"'OR_DATE',
*                          'ZZ_ZONE'           text-037    text-037    'Zone',"'ZONE',
*                          'ZZ_RR_NO'          text-038    text-038    'RR No.',"'RR_NO',
*                          'ZZ_RR_DATE'        text-039    text-039    'RR Date',"'RR_DATE',
*                          'ZZ_INVTAG_NO'      text-040    text-040    'InvtyTagNo',"'INVTAG_NO',
*                          'ZZ_FORMER_ACCT'    text-041    text-041    'F.Acctable',"'FORMERACCT',
*                          'ORDTX1'            text-005    text-005    'Status',"'ORDTX1',
*                          'ZZ_REMARKS'        text-042    text-042    'Remarks',"'REMARKS',
*                          'AFABE'             text-043    text-052    'Deprn Area'."'AFABE',
    mc_change_column_names: 'ANLN1'             text-001    text-001    text-065,
                            'BUKRS'             text-002    text-002    text-066,
                            'ANLKL'             text-003    text-003    text-067,
                            'TXT50'             text-004    text-004    text-068,
                            'TXA50'             text-056    text-062    text-069,
                            'ORDTX2'            text-006    text-006    text-070,
                            'SERNR'             text-063    text-064    text-071,
                            'AKTIV'             text-007    text-050    text-072,
                            'DEAKT'             text-008    text-008    text-073,
                            'KOSTL'             text-009    text-009    text-074,
                            'LTEXT'             text-060    text-060    text-075,
                            'GDLGRP'            text-010    text-010    text-076,
                            'GDLGRP_TXT'        text-054    text-055    text-077,
                            'IVDAT'             text-057    text-057    text-078,
                            'INKEN'             text-058    text-058    text-079,
                            'INVZU'             text-059    text-059    text-080,
                            'ERNAM'             text-011    text-011    text-081,
                            'ERDAT'             text-012    text-012    text-082,
                            'DATBI'             text-013    text-013    text-083,
                            'ZZOLD_PROP_NUM'    text-014    text-014    text-084,
                            'ZZBRAND'           text-015    text-015    text-085,
                            'ZZTYPE'            text-016    text-016    text-086,
                            'ZZACQUI_COST'      text-017    text-017    text-087,
                            'ANBTR'             text-044    text-044    text-088,
                            'ZZENGAS_CODE'      text-018    text-018    text-089,
                            'ZZARTCLE_CODE'     text-019    text-019    text-090,
                            'ZZCONTRACT_DESC'   text-020    text-020    text-091,
                            'ZZCONTRACT_NUM'    text-021    text-021    text-092,
                            'ZZLOCATION'        text-022    text-022    text-093,
                            'ZZASSET_CATEGORY'  text-023    text-023    text-094,
                            'ZZLOAN_PACKAGE'    text-024    text-024    text-095,
                            'ZZ_LOAN_CURRENCY'  text-025    text-051    text-096,
                            'ZZ_PROOF_OWNER'    text-026    text-026    text-097,
                            'ZZ_LOT_NO'         text-027    text-027    text-098,
                            'ZZ_SURVEY_NO'      text-028    text-028    text-099,
                            'ZZ_ACQUI_DATE'     text-029    text-053    text-100,
                            'ZZ_PLATE_NO'       text-030    text-030    text-101,
                            'ZZ_ENGINE_NO'      text-031    text-031    text-102,
                            'ZZ_CHASSIS_NO'     text-032    text-032    text-103,
                            'ZZ_AADF_NO'        text-033    text-033    text-104,
                            'ZZ_AADF_DATE'      text-034    text-034    text-105,
                            'ZZ_OR_NO'          text-035    text-035    text-106,
                            'ZZ_OR_DATE'        text-036    text-036    text-107,
                            'ZZ_ZONE'           text-037    text-037    text-108,
                            'ZZ_RR_NO'          text-038    text-038    text-109,
                            'ZZ_RR_DATE'        text-039    text-039    text-110,
                            'ZZ_INVTAG_NO'      text-040    text-040    text-111,
                            'ZZ_FORMER_ACCT'    text-041    text-041    text-112,
                            'ORDTX1'            text-005    text-005    text-113,
                            'ZZ_REMARKS'        text-042    text-042    text-114,
                            'AFABE'             text-043    text-052    text-115.


  "-- Optimize Field Length
  go_columns->set_optimize( abap_true ).

  "-- Set zebra pattern
  go_settings = go_table->get_display_settings( ).
  go_settings->set_striped_pattern( abap_true ).

  "-- Prepare Aggregate Functions
  go_aggregations = go_table->get_aggregations( ).
  go_toolbar      = go_table->get_functions( ) .
  go_toolbar->set_all( value  = if_salv_c_bool_sap=>true ).

  go_aggregations->clear( ).

  "-- Set ALV HTML Header
  PERFORM build_header CHANGING go_content.

  go_table->set_top_of_list( go_content ).

* Start - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2
  TRY.
    CALL METHOD go_table->set_screen_status
      EXPORTING
        report   = sy-repid
        pfstatus = 'ZSTATUS'.

    CLEAR go_events.
    CALL METHOD go_table->get_event
      RECEIVING
        value  = go_events.

    CREATE OBJECT go_event_handler.
    SET HANDLER go_event_handler->on_user_command FOR go_events.
  ENDTRY.
* End - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2

  "-- Display ALV table
  go_table->display( ).

ENDFORM.                    " DISPLAY_REPORT

*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       Sort the data by Main Asset Number
*----------------------------------------------------------------------*
FORM sort_data .
  SORT gt_output BY anln1.
ENDFORM.                    " SORT_DATA

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA_TO_ALV
*&---------------------------------------------------------------------*
*       Transfer the data to ALV
*----------------------------------------------------------------------*
FORM assign_data_to_alv.
  DATA: lv_field            TYPE char20.

  LOOP AT gt_output ASSIGNING <fs_output>.
    "-- Generate Dynamic Work Area then assign to Field Symbol
    CREATE DATA go_structure LIKE LINE OF <ft_tab>.
    ASSIGN go_structure->* TO <fv_wa>.

    lv_field = 'ANLN1'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-anln1.

    lv_field = 'BUKRS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-bukrs.

    lv_field = 'ANLKL'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-anlkl.

    lv_field = 'TXT50'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-txt50.

    lv_field = 'ORDTX1'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ordtx1.

    lv_field = 'ORDTX2'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ordtx2.

    lv_field = 'SERNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-sernr.

    lv_field = 'AKTIV'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-aktiv.

    lv_field = 'DEAKT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-deakt.

    lv_field = 'KOSTL'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-kostl.

    lv_field = 'GDLGRP'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-gdlgrp.

    lv_field = 'GDLGRP_TXT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-gdlgrp_txt.

    lv_field = 'ERNAM'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ernam.

    lv_field = 'ERDAT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-erdat.

    lv_field = 'DATBI'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-datbi.

    lv_field = 'ZZOLD_PROP_NUM'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzold_prop_num.

    lv_field = 'ZZBRAND'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzbrand.

    lv_field = 'ZZTYPE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zztype.

    lv_field = 'ZZACQUI_COST'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzacqui_cost.

    lv_field = 'ZZENGAS_CODE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzengas_code.

    lv_field = 'ZZARTCLE_CODE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzartcle_code.

    lv_field = 'ZZCONTRACT_DESC'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzcontract_desc.

    lv_field = 'ZZCONTRACT_NUM'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzcontract_num.

    lv_field = 'ZZLOCATION'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzlocation.

    lv_field = 'ZZASSET_CATEGORY'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzasset_category.

    lv_field = 'ZZLOAN_PACKAGE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zzloan_package.

    lv_field = 'ZZ_LOAN_CURRENCY'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_loan_currency.

    lv_field = 'ZZ_PROOF_OWNER'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_proof_owner.

    lv_field = 'ZZ_LOT_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_lot_no.

    lv_field = 'ZZ_SURVEY_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_survey_no.

    lv_field = 'ZZ_ACQUI_DATE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_acqui_date.

    lv_field = 'ZZ_PLATE_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_plate_no.

    lv_field = 'ZZ_ENGINE_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_engine_no.

    lv_field = 'ZZ_CHASSIS_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_chassis_no.

    lv_field = 'ZZ_AADF_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_aadf_no.

    lv_field = 'ZZ_AADF_DATE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_aadf_date.

    lv_field = 'ZZ_OR_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_or_no.

    lv_field = 'ZZ_OR_DATE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_or_date.

    lv_field = 'ZZ_ZONE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_zone.

    lv_field = 'ZZ_RR_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_rr_no.

    lv_field = 'ZZ_RR_DATE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_rr_date.

    lv_field = 'ZZ_INVTAG_NO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_invtag_no.

    lv_field = 'ZZ_FORMER_ACCT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_former_acct.

    lv_field = 'ZZ_REMARKS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-zz_remarks.

    lv_field = 'AFABE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-afabe.

    lv_field = 'ANBTR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-anbtr.

    lv_field = 'TXA50'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-txa50.

    lv_field = 'IVDAT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ivdat.

    lv_field = 'INKEN'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-inken.

    lv_field = 'INVZU'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-invzu.

    lv_field = 'LTEXT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ltext.

    " Append to dynamic table
    APPEND <fv_wa> TO <ft_tab>.
  ENDLOOP.
  UNASSIGN <fs_output>.
ENDFORM.                    " ASSIGN_DATA_TO_ALV

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       Build Header Data Here
*----------------------------------------------------------------------*
FORM build_header CHANGING po_content     TYPE REF TO cl_salv_form_element.

  DATA: lr_grid        TYPE REF TO cl_salv_form_layout_grid,
        lv_text        TYPE string.

  "-- Create Header object
  CREATE OBJECT lr_grid.

  lv_text = text-116.

  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lv_text
                                      tooltip = lv_text ).
  po_content = lr_grid.
ENDFORM.                    " BUILD_HEADER

* Start - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_TO_EXCEL
*&---------------------------------------------------------------------*
*  Save data to excel
*----------------------------------------------------------------------*
FORM save_data_to_excel USING iv_salv_function TYPE salv_de_function.

  TYPES: BEGIN OF ty_excel_s,
            anln1            TYPE c LENGTH 12 ,  "Asset No.
            bukrs            TYPE c LENGTH 10 ,  "Co. Code
            anlkl            TYPE c LENGTH 10 ,  "AssetClass
            txt50            TYPE c LENGTH 50 ,  "AssetDesc
            txa50            TYPE c LENGTH 50 ,  "AssetDesc2
            ordtx2           TYPE c LENGTH 30 ,  "A.ItemDesc
            sernr            TYPE c LENGTH 18 ,  "Serial No.
            aktiv            TYPE c LENGTH 10 ,  "Cap. Date
            deakt            TYPE c LENGTH 10 ,  "Deact.Date
            kostl            TYPE c LENGTH 10 ,  "CostCenter
            ltext            TYPE c LENGTH 40 ,  "CC Name
            gdlgrp           TYPE c LENGTH 10 ,  "Eval Grp 5
            gdlgrp_txt       TYPE c LENGTH 50 ,  "Ev.Grp5Txt
            ivdat            TYPE c LENGTH 10 ,  "Invty Date
            inken            TYPE c LENGTH 10 ,  "Invty List
            invzu            TYPE c LENGTH 15 ,  "Invty Note
            ernam            TYPE c LENGTH 12 ,  "Created By
            erdat            TYPE c LENGTH 10 ,  "Created On
            datbi            TYPE c LENGTH 10 ,  "CC Date To
            zzold_prop_num   TYPE c LENGTH 20 ,  "OldProp.No
            zzbrand          TYPE c LENGTH 40 ,  "Brand
            zztype           TYPE c LENGTH 40 ,  "Type
            zzacqui_cost     TYPE c LENGTH 20 ,  "Acq. Cost
            anbtr            TYPE c LENGTH 20 ,  "Acq. Value
            zzengas_code     TYPE c LENGTH 30 ,  "Engas Code
            zzartcle_code    TYPE c LENGTH 30 ,  "ArticlCode
            zzcontract_desc  TYPE c LENGTH 40 ,  "Contr Desc
            zzcontract_num   TYPE c LENGTH 30 ,  "Contr No
            zzlocation       TYPE c LENGTH 40 ,  "Location
            zzasset_category TYPE c LENGTH 16 ,  "AssetCateg
            zzloan_package   TYPE c LENGTH 10 ,  "Loan Pack
            zz_loan_currency TYPE c LENGTH 10 ,  "Loan Ccy
            zz_proof_owner   TYPE c LENGTH 20 ,  "ProofOfOwn
            zz_lot_no        TYPE c LENGTH 30 ,  "Lot No.
            zz_survey_no     TYPE c LENGTH 30 ,  "Survey No.
            zz_acqui_date    TYPE c LENGTH 30 ,  "Acq. Date
            zz_plate_no      TYPE c LENGTH 16 ,  "Plate No.
            zz_engine_no     TYPE c LENGTH 30 ,  "Engine No.
            zz_chassis_no    TYPE c LENGTH 30 ,  "Chassis No
            zz_aadf_no       TYPE c LENGTH 30 ,  "AADF No.
            zz_aadf_date     TYPE c LENGTH 30 ,  "AADF Date
            zz_or_no         TYPE c LENGTH 30 ,  "OR No.
            zz_or_date       TYPE c LENGTH 30 ,  "OR Date
            zz_zone          TYPE c LENGTH 30 ,  "Zone
            zz_rr_no         TYPE c LENGTH 30 ,  "RR No.
            zz_rr_date       TYPE c LENGTH 30 ,  "RR Date
            zz_invtag_no     TYPE c LENGTH 30 ,  "InvtyTagNo
            zz_former_acct   TYPE c LENGTH 30 ,  "F.Acctable
            ordtx1           TYPE c LENGTH 30 ,  "Status
            zz_remarks       TYPE c LENGTH 30 ,  "Remarks
            afabe            TYPE c LENGTH 10 ,  "Deprn Area
         END OF ty_excel_s.

  DATA: lt_excel       TYPE STANDARD TABLE OF ty_excel_s,
        ls_excel       TYPE ty_excel_s,
        lv_filename    TYPE string,
        lv_path        TYPE string,
        lv_fullpath    TYPE string,
        lv_fullpath2   TYPE c LENGTH 1000,
        lv_user_action TYPE i,
        lv_length1     TYPE i,
        lv_length2     TYPE i.

  FIELD-SYMBOLS: <fs_tab>    TYPE any,
                 <acquicost> TYPE any,
                 <anbtr>     TYPE any.

  CASE iv_salv_function.
    WHEN 'EXCEL'.
      CLEAR: lv_filename,
             lv_path,
             lv_fullpath,
             lv_user_action.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        CHANGING
          filename             = lv_filename
          path                 = lv_path
          fullpath             = lv_fullpath
          user_action          = lv_user_action
*          file_encoding        =
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          others               = 4.
      IF sy-subrc EQ 0.
        IF lv_user_action EQ cl_gui_frontend_services=>action_ok.
          CLEAR: lv_length1,
                 lv_length2.
          lv_length1 = strlen( lv_fullpath ).
          lv_length2 = lv_length1 - 5.
          lv_length1 = lv_length1 - 4.

          CLEAR lv_fullpath2.
          lv_fullpath2 = lv_fullpath.
          IF  lv_fullpath+lv_length1(4) NE '.xls'
          AND lv_fullpath+lv_length1(4) NE '.XLS'
          AND lv_fullpath+lv_length2(5) NE '.xlsx'
          AND lv_fullpath+lv_length2(5) NE '.XLSX'.
            CONCATENATE lv_fullpath2
                        '.xls'
                   INTO lv_fullpath2.
          ENDIF.
          lv_fullpath = lv_fullpath2.

          REFRESH lt_excel.
          CLEAR ls_excel.
          ls_excel-anln1            = text-065.
          ls_excel-bukrs            = text-066.
          ls_excel-anlkl            = text-067.
          ls_excel-txt50            = text-068.
          ls_excel-txa50            = text-069.
          ls_excel-ordtx2           = text-070.
          ls_excel-sernr            = text-071.
          ls_excel-aktiv            = text-072.
          ls_excel-deakt            = text-073.
          ls_excel-kostl            = text-074.
          ls_excel-ltext            = text-075.
          ls_excel-gdlgrp           = text-076.
          ls_excel-gdlgrp_txt       = text-077.
          ls_excel-ivdat            = text-078.
          ls_excel-inken            = text-079.
          ls_excel-invzu            = text-080.
          ls_excel-ernam            = text-081.
          ls_excel-erdat            = text-082.
          ls_excel-datbi            = text-083.
          ls_excel-zzold_prop_num   = text-084.
          ls_excel-zzbrand          = text-085.
          ls_excel-zztype           = text-086.
          ls_excel-zzacqui_cost     = text-087.
          ls_excel-anbtr            = text-088.
          ls_excel-zzengas_code     = text-089.
          ls_excel-zzartcle_code    = text-090.
          ls_excel-zzcontract_desc  = text-091.
          ls_excel-zzcontract_num   = text-092.
          ls_excel-zzlocation       = text-093.
          ls_excel-zzasset_category = text-094.
          ls_excel-zzloan_package   = text-095.
          ls_excel-zz_loan_currency = text-096.
          ls_excel-zz_proof_owner   = text-097.
          ls_excel-zz_lot_no        = text-098.
          ls_excel-zz_survey_no     = text-099.
          ls_excel-zz_acqui_date    = text-100.
          ls_excel-zz_plate_no      = text-101.
          ls_excel-zz_engine_no     = text-102.
          ls_excel-zz_chassis_no    = text-103.
          ls_excel-zz_aadf_no       = text-104.
          ls_excel-zz_aadf_date     = text-105.
          ls_excel-zz_or_no         = text-106.
          ls_excel-zz_or_date       = text-107.
          ls_excel-zz_zone          = text-108.
          ls_excel-zz_rr_no         = text-109.
          ls_excel-zz_rr_date       = text-110.
          ls_excel-zz_invtag_no     = text-111.
          ls_excel-zz_former_acct   = text-112.
          ls_excel-ordtx1           = text-113.
          ls_excel-zz_remarks       = text-114.
          ls_excel-afabe            = text-115.
          APPEND ls_excel TO lt_excel.

          IF <ft_tab> IS ASSIGNED.
            UNASSIGN <fs_tab>.
            LOOP AT <ft_tab> ASSIGNING <fs_tab>.
              CLEAR ls_excel.
              MOVE-CORRESPONDING <fs_tab> TO ls_excel.

              UNASSIGN <acquicost>.
              ASSIGN COMPONENT 'ZZACQUI_COST' OF STRUCTURE <fs_tab> TO <acquicost>.
              IF <acquicost> IS ASSIGNED.
                WRITE <acquicost> TO ls_excel-zzacqui_cost CURRENCY 'PHP'.
              ENDIF.

              UNASSIGN <anbtr>.
              ASSIGN COMPONENT 'ANBTR' OF STRUCTURE <fs_tab> TO <anbtr>.
              IF <anbtr> IS ASSIGNED.
                WRITE <anbtr> TO ls_excel-anbtr CURRENCY 'PHP'.
              ENDIF.

              APPEND ls_excel TO lt_excel.
            ENDLOOP.
          ENDIF.

          IF lt_excel[] IS NOT INITIAL.
            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                filename                = lv_fullpath
                filetype                = 'DAT'
              TABLES
                data_tab                = lt_excel
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
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " SAVE_DATA_TO_EXCEL
* End - DV2K914376 - PMEDIOS - FI.AM.Asset Master Data List - copy.v2