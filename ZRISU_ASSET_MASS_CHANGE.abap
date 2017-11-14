*&---------------------------------------------------------------------*
*& PROGRAM     : ZRISU_ASSET_MASS_CHANGE
*& DESCRIPTION : This program Massively Changes/Updates Assets'
*&               Master Data via Transaction AS02 (Change Asset)
*&---------------------------------------------------------------------*
*& Module       : ISU
*& Author       : RANDYSY
*& Designed by  :
*& Date Created : 07/10/2012
*& Reference    : TN#17468
*&---------------------------------------------------------------------*
*&   Date     | Author ID | Ticket No.|         Description
*&---------------------------------------------------------------------*
*& 07/10/2012 | RANDYSY   |  17468    | Initial Development            *
*& 11/26/2012 | ANNACLO   |  23184    | Replaced hardcoded IP w/ names *
*&---------------------------------------------------------------------*

REPORT  zrisu_asset_mass_change2 NO STANDARD PAGE HEADING.

*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES:  BEGIN OF ty_upload,
          upload_status   TYPE natxt,                 "-- Upload Status
          asset_category  TYPE anlu-zzasset_category, "-- Asset Category
          property_no     TYPE anlu-zzold_prop_num,   "-- Property No.
          asset_no        TYPE anlu-anln1,            "-- Asset No.
          asset_desc      TYPE char100,               "-- Asset Description
          serial_no       TYPE anlu-zz_serial_no,     "-- Serial No.
          plate_no        TYPE anlu-zz_plate_no,      "-- Plate No.
          engine_no       TYPE anlu-zz_engine_no,     "-- Engine No.
          chassis_no      TYPE anlu-zz_chassis_no,    "-- Chassis No.
          date_acquired   TYPE anlu-zz_acqui_date,    "-- Date Acquisition
          acq_cost        TYPE zacqcost,              "-- Acquisition Cost
          contract_no     TYPE anlu-zzcontract_num,   "-- Contract Number
          rr_no           TYPE anlu-zz_rr_no,         "-- RR No.
          rr_date         TYPE anlu-zz_rr_date,       "-- RR Date
          location        TYPE anlu-zzlocation,       "-- Location
          manno           TYPE anla-gdlgrp,           "-- MANNO / Evaluation Group 5
          bus_area        TYPE anlz-gsber,            "-- Business Area
          cost_center     TYPE anlz-kostl,            "-- Cost Center
          remarks         TYPE anlu-zz_remarks,       "-- Remarks
        END OF ty_upload,

        BEGIN OF ty_string,
          string         TYPE c LENGTH 1000,
        END OF ty_string.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA:   gt_bdcdata        TYPE tab_bdcdata,
        gt_upload         TYPE TABLE OF ty_upload.

*&---------------------------------------------------------------------*
*& Data Definitions - Global parameters (gs_<structure name>)
*&                                      (gv_<var name>),
*                                       (gr_<range>),
*                                       (go_<class>)
DATA:   gs_string         TYPE ty_string,
        gv_string         TYPE string,                      "#EC NEEDED
        go_table          TYPE REF TO cl_salv_table,
        go_functions      TYPE REF TO cl_salv_functions_list,
        go_columns        TYPE REF TO cl_salv_columns_table,
        go_column         TYPE REF TO cl_salv_column_table,
        go_salv_msg       TYPE REF TO cx_salv_msg.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (fs_<name>)
*&
FIELD-SYMBOLS:
        <fs_bdc>          TYPE bdcdata,
        <fs_upload>       TYPE ty_upload,
        <fs_messages>     TYPE bdcmsgcoll.

*&---------------------------------------------------------------------*
*& Data Definitions - Constant Variables (gc_<var name>)
*&
CONSTANTS:
        co_company_code   TYPE bukrs    VALUE `MWSI`,
        co_flag           TYPE c        VALUE `X`,
        co_tab            TYPE c        VALUE cl_abap_char_utilities=>horizontal_tab,
        co_tcode          TYPE sy-tcode VALUE `AS02`,
        co_init_directory TYPE string   VALUE '\\saprep01.mayniladsap.com\'.   "\\172.18.1.240\.- Replaced by Ann 2012/11/26

*&----------------------------------------------------------------------
*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
PARAMETERS :
  pa_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& At Selection Screen
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM select_file CHANGING pa_file.

*&---------------------------------------------------------------------*
*& Initialization - Initial process at start of program
*&
INITIALIZATION.
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

  "-- Macro Definition - Format Asset Number - Add Zero Paddings
  DEFINE mc_format_number.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &1.
  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  "-- Upload File
  PERFORM upload_file USING pa_file.

  "-- If there are uploaded data
  IF gt_upload[] IS NOT INITIAL.
    "-- Change Asset General Data
    PERFORM change_general_data.

    "-- Change Time Dependent and Allocations Data
    PERFORM change_timedep_alloc.

    "-- Display Result of Mass Change
    PERFORM display_report.
  ENDIF.

*&---------------------------------------------------------------------*
*& End of Selection - End of Main Program
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Subroutine - FORM subr_<name/s> USING    pv_<variable name/s>
*&                                 CHANGING pv_<variable name/s>
*&                                 TABLES   lt_<table    name/s>

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE
*&---------------------------------------------------------------------*
*       Subroutine to Browse the File Path & Name
*----------------------------------------------------------------------*
FORM select_file CHANGING pv_file TYPE localfile.
  DATA: lv_title TYPE string,
        lv_subrc TYPE sysubrc,
        lt_tab   TYPE filetable.

  CONSTANTS:
        co_xls   TYPE string VALUE `*.txt`.

  "-- Prepare Window Title
  lv_title = text-001.

  "-- Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      default_filename  = co_xls
      multiselection    = ' '
      initial_directory = co_init_directory
    CHANGING
      file_table        = lt_tab
      rc                = lv_subrc.

  "-- Write path on input area
  READ TABLE lt_tab INTO pv_file INDEX 1.
ENDFORM.                    " SELECT_FILE

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
FORM call_transaction  USING    pv_tcode   TYPE sy-tcode
                       CHANGING pv_message TYPE natxt.

  DATA: lt_messages   TYPE STANDARD TABLE OF bdcmsgcoll,
        lt_options    TYPE ctu_params.

  CONSTANTS:
        co_mode TYPE char01 VALUE 'N'.

  "-- Clear Error Message.
  CLEAR: pv_message.

  "-- Perform call transaction
  CALL TRANSACTION pv_tcode USING         gt_bdcdata
                            MESSAGES INTO lt_messages
                            MODE          co_mode.       "#EC CI_CALLTA

  IF sy-subrc EQ 0 OR sy-subrc EQ 1001.
    pv_message = 0.
  ELSE.
    "-- Check if there are BDC Messages
    IF lt_messages[] IS NOT INITIAL.
      "-- Get Call Transaction Results
      PERFORM get_bdc_messages TABLES   lt_messages
                               CHANGING pv_message.
    ENDIF.
  ENDIF.

  "-- Reinitialize Itab for BDC
  REFRESH: gt_bdcdata,
           lt_messages.
ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Uploading of File to Internal table
*----------------------------------------------------------------------*
FORM upload_file USING pv_file TYPE localfile.
  DATA: lv_acq_cost TYPE char16.

  "-- Locate and open the space-delimited input file
  OPEN DATASET pv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  "-- Check if the file exists
  IF sy-subrc EQ 0.
    "-- Iterate to get data from file
    DO.
      "-- Transfer Data to Structure
      READ DATASET pv_file INTO gs_string.

      IF sy-subrc EQ 0.
        "-- Append to Internal Table from Structure
        APPEND INITIAL LINE TO gt_upload ASSIGNING <fs_upload>.

        "-- Split String At Tab
        SPLIT gs_string-string AT co_tab INTO <fs_upload>-asset_category  "-- Asset Category
                                              <fs_upload>-property_no     "-- Property No.
                                              <fs_upload>-asset_no        "-- Asset No.
                                              <fs_upload>-asset_desc      "-- Asset Description
                                              <fs_upload>-serial_no       "-- Serial No.
                                              <fs_upload>-plate_no        "-- Plate No.
                                              <fs_upload>-engine_no       "-- Engine No.
                                              <fs_upload>-chassis_no      "-- Chassis No.
                                              <fs_upload>-date_acquired   "-- Date Acquisition
                                              lv_acq_cost                 "-- Acquisition Cost
                                              <fs_upload>-contract_no     "-- Contract Number
                                              <fs_upload>-rr_no           "-- RR No.
                                              <fs_upload>-rr_date         "-- RR Date
                                              <fs_upload>-location        "-- Location
                                              <fs_upload>-manno           "-- MANNO / Evaluation Grp 5
                                              <fs_upload>-bus_area        "-- Business Area
                                              <fs_upload>-cost_center     "-- Cost Center
                                              <fs_upload>-remarks.        "-- Remarks

        "-- Correct Format and Type of Acquisition Cost
        IF lv_acq_cost NE ``.
          PERFORM check_acquisition_cost CHANGING lv_acq_cost
                                                  <fs_upload>-acq_cost.
        ELSE.
          <fs_upload>-acq_cost = 0.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ELSE.
    "-- Display Message Text
    MESSAGE s208(00) WITH text-001 DISPLAY LIKE `E`.
  ENDIF.

  "-- Close File
  CLOSE DATASET pv_file.
ENDFORM.                    " UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_BDC_MESSAGES
*&---------------------------------------------------------------------*
*       Get BDC messages
*----------------------------------------------------------------------*
FORM get_bdc_messages  TABLES   pt_bdcmsgs TYPE tab_bdcmsgcoll
                       CHANGING pv_text    TYPE natxt.

  "-- Get First Row of BDC Message
  READ TABLE pt_bdcmsgs ASSIGNING <fs_messages> INDEX 1.
  IF sy-subrc EQ 0.
    "-- Call FM to Format BDC Message
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = <fs_messages>-msgid
        lang      = sy-langu
        no        = <fs_messages>-msgnr
        v1        = <fs_messages>-msgv1
        v2        = <fs_messages>-msgv2
        v3        = <fs_messages>-msgv3
        v4        = <fs_messages>-msgv4
      IMPORTING
        msg       = pv_text
      EXCEPTIONS
        not_found = 0
        OTHERS    = 0.
  ENDIF.
ENDFORM.                    " get_bdc_messages

*&---------------------------------------------------------------------*
*&      Form  CHANGE_GENERAL_DATA
*&---------------------------------------------------------------------*
*       Perform BDC Transaction
*----------------------------------------------------------------------*
FORM change_general_data.
  DATA: lv_desc_1         TYPE char50,
        lv_desc_2         TYPE char50,
        lv_desc_length    TYPE i,
        lv_acq_cost       TYPE char18.

  "-- Iterate Rows of Internal Table
  LOOP AT gt_upload ASSIGNING <fs_upload>.
    "-- Reinitialize Variables Per Row
    CLEAR: lv_desc_1, lv_desc_2, lv_desc_length.

    "-- Get Length of Asset Description
    lv_desc_length = STRLEN( <fs_upload>-asset_desc ).

    "-- Prepare Asset Description
    IF lv_desc_length GT 50.
      lv_desc_1 = <fs_upload>-asset_desc+0(50).
      lv_desc_2 = <fs_upload>-asset_desc+50(50).
    ELSE.
      lv_desc_1 = <fs_upload>-asset_desc.
    ENDIF.

    "-- Prepare Acquisition Cost
    lv_acq_cost = <fs_upload>-acq_cost.
    SHIFT lv_acq_cost LEFT DELETING LEADING space.

    "-- Pass BDC screens, fields and value
    mc_bdc_dynpro:  co_flag 'SAPLAIST'              '0100',
                    space   'BDC_CURSOR'            'ANLA-ANLN1',
                    space   'BDC_OKCODE'            '/00',
                    space   'ANLA-ANLN1'            <fs_upload>-asset_no,        "-- Asset No.
                    space   'ANLA-ANLN2'            '0',                         "-- Asset Subnumber
                    space   'ANLA-BUKRS'            co_company_code,             "-- Company Code - MWSI
                    co_flag 'SAPLAIST'              '1000',
                    space   'BDC_OKCODE'            '=BUCH',
                    space   'ANLA-TXT50'            lv_desc_1,                   "-- Asset Description 1
                    space   'ANLA-TXA50'            lv_desc_2,                   "-- Asset Description 2
                    space   'ANLU-ZZOLD_PROP_NUM'   <fs_upload>-property_no,     "-- Property No.
                    space   'ANLU-ZZACQUI_COST'     lv_acq_cost,                 "-- Acquisition Cost
                    space   'ANLU-ZZCONTRACT_NUM'   <fs_upload>-contract_no,     "-- Contract Number
                    space   'ANLU-ZZLOCATION'       <fs_upload>-location,        "-- Location
                    space   'ANLU-ZZASSET_CATEGORY' <fs_upload>-asset_category,  "-- Asset Category
                    space   'ANLU-ZZ_SERIAL_NO'     <fs_upload>-serial_no,       "-- Serial No.
                    space   'ANLU-ZZ_PLATE_NO'      <fs_upload>-plate_no,        "-- Plate No.
                    space   'ANLU-ZZ_ENGINE_NO'     <fs_upload>-engine_no,       "-- Engine No.
                    space   'ANLU-ZZ_CHASSIS_NO'    <fs_upload>-chassis_no,      "-- Chassis No.
                    space   'ANLU-ZZ_ACQUI_DATE'    <fs_upload>-date_acquired,   "-- Acquisition Date
                    space   'ANLU-ZZ_RR_NO'         <fs_upload>-rr_no,           "-- RR No.
                    space   'ANLU-ZZ_RR_DATE'       <fs_upload>-rr_date,         "-- RR Date
                    space   'ANLU-ZZ_REMARKS'       <fs_upload>-remarks.         "-- Remarks

    "-- Perform Transaction AS02
    PERFORM call_transaction USING    co_tcode
                             CHANGING <fs_upload>-upload_status.                  "-- Update Upload Status
  ENDLOOP.
ENDFORM.                    " CHANGE_GENERAL_DATA

*&---------------------------------------------------------------------*
*&      Form  change_timedep_alloc
*&---------------------------------------------------------------------*
*       Perform BDC Transaction
*----------------------------------------------------------------------*
FORM change_timedep_alloc.
  DATA: ls_timedependent  TYPE bapi1022_feglg003,
        ls_timedependentx TYPE bapi1022_feglg003x,
        ls_allocation     TYPE bapi1022_feglg004,
        ls_allocationx    TYPE bapi1022_feglg004x,
        ls_return         TYPE bapiret2.

  "-- Iterate Rows of Internal Table
  LOOP AT gt_upload ASSIGNING <fs_upload>. "WHERE upload_status EQ '0'.
    CONDENSE <fs_upload>-upload_status.

    IF <fs_upload>-upload_status EQ '0'.
      "-- Reinitialize Local Structures
      CLEAR: ls_timedependent,
             ls_timedependentx,
             ls_allocation,
             ls_allocationx,
             ls_return.

      "-- Format Asset Nos. and Business Area
      mc_format_number: <fs_upload>-asset_no,
                        <fs_upload>-cost_center,
                        <fs_upload>-bus_area.

      "-- Populate Time Dependent Data
      IF <fs_upload>-bus_area IS NOT INITIAL.
        ls_timedependent-bus_area  = <fs_upload>-bus_area.    "-- Business Area
        ls_timedependentx-bus_area = co_flag.
      ENDIF.

      ls_timedependent-costcenter  = <fs_upload>-cost_center. "-- Cost Center
      ls_timedependentx-costcenter = co_flag.

      "-- Populate Allocations Data
      ls_allocation-evalgroup5     = <fs_upload>-manno.       "-- MANNO / Evaluation Grp 5
      ls_allocationx-evalgroup5    = co_flag.

      "-- Call FM to Change Time Dependent and Allocations Data
      CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
        EXPORTING
          companycode        = co_company_code                "-- Company Code - MWSI
          asset              = <fs_upload>-asset_no           "-- Asset No.
          subnumber          = '0000'                         "-- Asset Subnumber
          timedependentdata  = ls_timedependent
          timedependentdatax = ls_timedependentx
          allocations        = ls_allocation
          allocationsx       = ls_allocationx
        IMPORTING
          return             = ls_return.

      "-- Update Upload Status
      <fs_upload>-upload_status = ls_return-message.

      "-- Call FM to Commit Changes
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " change_timedep_alloc

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       Subroutine for Displaying Results
*----------------------------------------------------------------------*
FORM display_report.
  DATA: lo_salv_not_found TYPE REF TO cx_salv_not_found.

  "-- Macro Definition - For Changing ALV Column Names
  DEFINE mc_change_column_names.
    try.
        go_column ?= go_columns->get_column( &1 ). "Pass Column Name
        go_column->set_long_text(   &1 ).          "Pass Long   Text
        go_column->set_medium_text( &2 ).          "Pass Medium Text
        go_column->set_short_text(  &2 ).          "Pass Short  Text
      catch cx_salv_not_found into lo_salv_not_found.
        "-- Error Handler
        gv_string = lo_salv_not_found->get_text( ).         "#EC NEEDED
        "-- Show Error Message
*        message gv_string type 'S'.
    endtry.
  END-OF-DEFINITION.

  "-- Create ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_table
        CHANGING
          t_table      = gt_upload.
    CATCH cx_salv_msg INTO go_salv_msg.
      "-- Error Handler
      gv_string = go_salv_msg->get_text( ).                 "#EC NEEDED
      "-- Show Error Message
      MESSAGE gv_string TYPE 'I'.
  ENDTRY.

  "-- Enable all ALV Functions
  go_functions = go_table->get_functions( ).
  go_functions->set_all( 'X' ).

  "-- Get field attributes
  go_columns   = go_table->get_columns( ).

  "-- Change ALV Column Name
  mc_change_column_names: text-002 text-003, "-- Asset Description
                          text-004 text-005. "-- Upload Status

  "-- Optimize Field Length
  go_columns->set_optimize( 'X' ).

  "-- Display ALV table
  go_table->display( ).

  "-- Display End Message
  MESSAGE s355(00).
ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ACQUISITION_COST
*&---------------------------------------------------------------------*
*       Correct Format and Type of Acquisition Cost
*----------------------------------------------------------------------*
FORM check_acquisition_cost CHANGING pv_acq_cost_char TYPE char16
                                     pv_acq_cost_num  TYPE zacqcost.
  "-- Reinitialize to Zero
  CLEAR: pv_acq_cost_num.

  "-- Remove Leading and Trailing Spaces
  CONDENSE: pv_acq_cost_char.

  "-- Remove Commas in Currency Field
  REPLACE ALL OCCURRENCES OF: `,`   IN pv_acq_cost_char WITH ``,
                              `.00` IN pv_acq_cost_char WITH ``,
                              `-`   IN pv_acq_cost_char WITH ``.

  IF pv_acq_cost_char NE ``.
    pv_acq_cost_num = pv_acq_cost_char.
  ENDIF.
ENDFORM.                    " CHECK_ACQUISITION_COST