*&---------------------------------------------------------------------*
*& Report  ZMM_PURCHASE_JOURNAL
*& DESCRIPTION: Purchase Journal
*&---------------------------------------------------------------------*
*& Created by : GCDIMALIWAT
*& Created On : 2013/12/10
*& Reference  : Functional Specification Indra BIR Purchase Journal v1 20131204.docx
*&---------------------------------------------------------------------*
*& Date      | Author ID   | Description
*&---------------------------------------------------------------------*
*& 2013/12/10| GCDIMALIWAT | Created BIR Purchase Journal
*&---------------------------------------------------------------------*
REPORT zmm_purchase_journal MESSAGE-ID zmmpjph.

*INCLUDE
TABLES: bkpf.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_bkpf,
        belnr TYPE bkpf-belnr,
        bukrs TYPE bkpf-bukrs,
        monat TYPE bkpf-monat,
        gjahr TYPE bkpf-gjahr,
        budat TYPE bkpf-budat,
        awkey TYPE bkpf-awkey,
        xblnr TYPE bkpf-xblnr,
       END OF ty_bkpf,

       BEGIN OF ty_bseg,
         dmbtr  TYPE bseg-dmbtr,
         knto   TYPE bseg-sknto,
         mwskz  TYPE bseg-mwskz,
       END OF ty_bseg,

       BEGIN OF ty_output,
         butxt   TYPE c LENGTH 25,  "Company Name
         tin     TYPE c LENGTH 11,  "VAT REG. TIN
         bpn     TYPE c LENGTH 25,  "BIR PERMIT NO
         di      TYPE c LENGTH 10,  "DATE ISSUED
         period  TYPE c LENGTH 8,   "PERIOD
         budat   TYPE c LENGTH 10,  "Date
         lifnr   TYPE c LENGTH 10,  "Vendor Code
         name    TYPE c LENGTH 71,  "Vendor Name
         stcd1   TYPE c LENGTH 16,  "Vendor TIN Number
         address TYPE c LENGTH 71,  "Vendor Address
         sgtxt   TYPE c LENGTH 50,  "Description
         ebeln   TYPE c LENGTH 10,  "PO Number
         awkey   TYPE c LENGTH 20,  "SAP Reference No.
         xblnr   TYPE c LENGTH 16,  "Supplier's Invoice No.
         dmbtr   TYPE p LENGTH 16 DECIMALS 2,  "Amount (Php)
         knto    TYPE p LENGTH 16 DECIMALS 2,  "Discount (Php)
         vat_amt TYPE p LENGTH 16 DECIMALS 2,  "VAT Amount (Php)
         net_pur TYPE p LENGTH 16 DECIMALS 2,  "Net Purchases (Php)
       END OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_output     TYPE TABLE OF ty_output,
      gt_fieldcat   TYPE lvc_t_fcat.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: go_tab             TYPE REF TO data,                  "#EC NEEDED
      go_structure       TYPE REF TO data,                  "#EC NEEDED
      go_table           TYPE REF TO cl_salv_table,         "#EC NEEDED
      go_functions       TYPE REF TO cl_salv_functions_list, "#EC NEEDED
      go_columns         TYPE REF TO cl_salv_columns_table, "#EC NEEDED
      go_column          TYPE REF TO cl_salv_column_table,  "#EC NEEDED
      go_settings        TYPE REF TO cl_salv_display_settings, "#EC NEEDED
      go_aggregations    TYPE REF TO cl_salv_aggregations,  "#EC NEEDED
      go_toolbar         TYPE REF TO cl_salv_functions_list, "#EC NEEDED
      go_content         TYPE REF TO cl_salv_form_element,  "#EC NEEDED
      go_layout          TYPE REF TO cl_salv_layout,        "#EC NEEDED
      go_exc             TYPE REF TO cx_salv_error,         "#EC NEEDED
      go_salv_msg        TYPE REF TO cx_salv_msg,           "#EC NEEDED
      go_salv_not_found  TYPE REF TO cx_salv_not_found,     "#EC NEEDED
      go_salv_data_error TYPE REF TO cx_salv_data_error,    "#EC NEEDED
      go_salv_existing   TYPE REF TO cx_salv_existing.      "#EC NEEDED

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_date_separator TYPE c LENGTH 1 VALUE '/'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_bkpf>        TYPE ty_bkpf,
               <fs_bseg>        TYPE ty_bseg,
               <fs_output>      TYPE ty_output,
               <fs_fieldcat>    TYPE lvc_s_fcat,            "#EC NEEDED
               <ft_tab>         TYPE STANDARD TABLE,        "#EC NEEDED
               <fv_wa>          TYPE ANY,
               <fv_field>       TYPE ANY.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01." WITH FRAME TITLE text-000.
PARAMETERS:     pa_bukrs     TYPE bkpf-bukrs OBLIGATORY.
SELECT-OPTIONS: so_monat     FOR  bkpf-monat NO-EXTENSION OBLIGATORY.
PARAMETERS:     pa_gjahr     TYPE bkpf-gjahr OBLIGATORY.
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
*"-- Macro Definition for Appending to Global Cluster Table
*  DEFINE mc_append_cluster.
*    append initial line to gt_cluster assigning <fs_cluster>.
*    <fs_cluster>-pernr       = &1.
*    <fs_cluster>-seq_no      = &2.
*    <fs_cluster>-end_payroll = &3.
*  END-OF-DEFINITION.

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

*  "-- Macro Definition for Setting ALV Summarized Field
*  DEFINE mc_set_summarized.
*    try .
*        go_aggregations->add_aggregation( columnname = &1 ).
*      catch cx_salv_not_found into go_salv_not_found.
*        message go_salv_not_found type 'I' display like 'E'.
*      catch cx_salv_data_error into go_salv_data_error.
*        message go_salv_data_error type 'I' display like 'E'.
*      catch cx_salv_existing into go_salv_existing.
*        message go_salv_existing type 'I' display like 'E'.
*    endtry.
*  END-OF-DEFINITION.

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

*  "-- Macro Definition - Set a Field to Invisible
*  DEFINE mc_set_invisible.
*    try.
*        go_column ?= go_columns->get_column( &1 ).
*        go_column->set_visible( ' ' ).
*      catch cx_salv_not_found into go_salv_not_found.
*        message go_salv_not_found type 'I' display like 'E'. "#EC NEEDED
*    endtry.
*  END-OF-DEFINITION.

*  "-- Macro Definition for Conversion Alpha
*  DEFINE mc_conversion_alpha.
*    "-- Add Zero Paddings
*    call function 'CONVERSION_EXIT_ALPHA_INPUT'
*      exporting
*        input  = &1
*      importing
*        output = &1.
*  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM extract_data.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  TRY.
   "PERFORM sort_data.
    PERFORM build_catalogue.
    PERFORM assign_data_to_alv.
    PERFORM display_report.

  CATCH CX_ROOT.
    MESSAGE e001(zmmpjph).
  ENDTRY.


*&---------------------------------------------------------------------*
*&      Form  EXTRACT_DATA
*&---------------------------------------------------------------------*
*       Retrieve data from BKPF, BSEG, LFA1, T001, and ZFI_BIRPH
*----------------------------------------------------------------------*
FORM extract_data .
  DATA: lt_bkpf   TYPE TABLE OF ty_bkpf,
        lt_bseg   TYPE TABLE OF ty_bseg,
        lv_name1  TYPE lfa1-name1,
        lv_name2  TYPE lfa1-name2,
        lv_strlen TYPE i,
        lv_dmbtr  TYPE bseg-dmbtr,
        lv_knto   TYPE bseg-sknto,
        lv_mwskz  TYPE bseg-mwskz.

  "-- Extract table BKPF
  SELECT a~belnr a~bukrs a~monat a~gjahr a~budat a~awkey a~xblnr
  INTO TABLE lt_bkpf
  FROM bkpf AS a
  INNER JOIN faglflexa AS b ON a~belnr EQ b~belnr AND a~bukrs EQ b~rbukrs AND a~gjahr EQ b~gjahr
  WHERE a~bukrs EQ pa_bukrs
    AND a~monat IN so_monat
    AND a~gjahr EQ pa_gjahr
    AND a~blart IN ('KR','RE','RN','KA','KG','KS','KN')
    AND b~rldnr EQ '0L'.

  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_bkpf COMPARING bukrs belnr gjahr.

    LOOP AT lt_bkpf ASSIGNING <fs_bkpf>.
      REFRESH: lt_bseg.
      CLEAR: lt_bseg,
             lv_name1,
             lv_name2,
             lv_strlen,
             lv_dmbtr,
             lv_knto,
             lv_mwskz.

      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.

      "-- Company Name
      SELECT SINGLE butxt
      INTO <fs_output>-butxt
      FROM t001
      WHERE bukrs EQ <fs_bkpf>-bukrs.

      "-- VAT REG. TIN.
      IF <fs_bkpf>-bukrs EQ '7618'.
        <fs_output>-tin = 'XXX-XXX-XXX'.
      ENDIF.

      "-- BIR PERMIT NO. / DATE ISSUED
      SELECT SINGLE zfi_birph_bpn zfi_birph_di              "#EC *
      INTO (<fs_output>-bpn, <fs_output>-di)
      FROM zfi_birph
      WHERE zbukrs EQ <fs_bkpf>-bukrs.

      "-- PERIOD
      IF so_monat-high IS INITIAL.
        MOVE so_monat-low TO <fs_output>-period.
      ELSE.
        CONCATENATE so_monat-low 'to' so_monat-high INTO <fs_output>-period SEPARATED BY space.
      ENDIF.

      "-- DATE
      CONCATENATE <fs_bkpf>-budat+4(2) <fs_bkpf>-budat+6(2) <fs_bkpf>-budat(4) INTO <fs_output>-budat SEPARATED BY co_date_separator.

      "-- Vendor Code / Description / PO Number
      SELECT SINGLE lifnr sgtxt ebeln                       "#EC *
        INTO (<fs_output>-lifnr, <fs_output>-sgtxt, <fs_output>-ebeln)
        FROM bseg
       WHERE bukrs EQ <fs_bkpf>-bukrs
         AND gjahr EQ <fs_bkpf>-gjahr
         AND belnr EQ <fs_bkpf>-belnr.

      "-- Vendor Name / Vendor TIN Number / Vendor Address
      SELECT SINGLE name1 name2 stcd1 stras
        INTO (lv_name1, lv_name2, <fs_output>-stcd1, <fs_output>-address )
        FROM lfa1
       WHERE lifnr EQ <fs_output>-lifnr.

      IF sy-subrc EQ 0.
        CONCATENATE lv_name1 lv_name2 INTO <fs_output>-name SEPARATED BY space.
      ENDIF.

      "-- SAP Reference Number
      lv_strlen = STRLEN( <fs_bkpf>-awkey ) - 4. "--Exclude last 4 digits
      <fs_output>-awkey = <fs_bkpf>-awkey(lv_strlen).

      "-- Supplier's invoice No.
      <fs_output>-xblnr = <fs_bkpf>-xblnr.

      "-- Amount (Php) / Discount (Php) / VAT Amount (Php) / Net Purchase (Php)
      SELECT dmbtr sknto mwskz
      INTO TABLE lt_bseg
      FROM bseg
      WHERE bukrs EQ <fs_bkpf>-bukrs
        AND gjahr EQ <fs_bkpf>-gjahr
        AND belnr EQ <fs_bkpf>-belnr
        AND koart EQ 'K'.

      IF sy-subrc EQ 0.
        LOOP AT lt_bseg ASSIGNING <fs_bseg>.
          ADD <fs_bseg>-dmbtr TO lv_dmbtr.
          ADD <fs_bseg>-knto  TO lv_knto.

          IF <fs_bseg>-mwskz EQ 'S1' OR <fs_bseg>-mwskz EQ 'E1' OR <fs_bseg>-mwskz EQ 'E2'.
            lv_mwskz = <fs_bseg>-mwskz.
          ENDIF.
        ENDLOOP.
        UNASSIGN <fs_bseg>.

        <fs_output>-dmbtr   = lv_dmbtr.
        <fs_output>-knto    = lv_knto.
        <fs_output>-vat_amt = lv_dmbtr - lv_knto.

        IF lv_mwskz EQ 'S1' OR lv_mwskz EQ 'E1' OR lv_mwskz EQ 'E2'. "-- S1, E1, or E2 found
          <fs_output>-net_pur = ( lv_dmbtr - lv_knto ) / '1.12'.
        ELSE.
          <fs_output>-net_pur = lv_dmbtr - lv_knto.
        ENDIF.
      ENDIF.

    ENDLOOP.
    UNASSIGN <fs_bkpf>.
  ELSE.
    MESSAGE e000(zmmpjph).
  ENDIF.
ENDFORM.                    " EXTRACT_DATA

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOGUE
*&---------------------------------------------------------------------*
*       Subroutine for Building Field Catalogue
*----------------------------------------------------------------------*
FORM build_catalogue.

  "-- Reinitialize Field Catalogue Table
  REFRESH: gt_fieldcat.

  "-- Append to Field Catalogue
  mc_append_fieldcat: 'BUDAT'               'BKPF'    'CHAR'  10  0 1   space,
                      'LIFNR'               'BSEG'    'CHAR'  10  0 2   space,
                      'NAME'                'LFA1'    'CHAR'  71  0 3   space,
                      'STCD1'               'LFA1'    'CHAR'  16  0 4   space,
                      'ADDRESS'             'BSEG'    'CHAR'  71  0 5   space,
                      'SGTXT'               'BSEG'    'CHAR'  50  0 6   space,
                      'EBELN'               'BKPF'    'CHAR'  10  0 7   space,
                      'AWKEY'               'BKPF'    'CHAR'  20  0 8   space,
                      'XBLNR'               'BKPF'    'CHAR'  16  0 9   space,
                      'DMBTR'               'BSEG'    'CURR'  16  2 10  space,
                      'KNTO'                'BSEG'    'CURR'  16  2 11  space,
                      'VAT_AMT'             'BSEG'    'CURR'  16  2 12  space,
                      'NET_PUR'             'BSEG'    'CURR'  16  2 13  space.

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
  mc_change_column_names: 'BUDAT'             text-006    text-006    text-020,
                          'LIFNR'             text-007    text-007    text-021,
                          'NAME'              text-008    text-008    text-022,
                          'STCD1'             text-009    text-009    text-023,
                          'ADDRESS'           text-010    text-010    text-024,
                          'SGTXT'             text-011    text-011    text-025,
                          'EBELN'             text-012    text-012    text-026,
                          'AWKEY'             text-013    text-013    text-027,
                          'XBLNR'             text-014    text-019    text-028,
                          'DMBTR'             text-015    text-015    text-029,
                          'KNTO'              text-016    text-016    text-030,
                          'VAT_AMT'           text-017    text-017    text-031,
                          'NET_PUR'           text-018    text-018    text-032.

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

  "-- Display ALV table
  go_table->display( ).
ENDFORM.                    " DISPLAY_REPORT

**&---------------------------------------------------------------------*
**&      Form  SORT_DATA
**&---------------------------------------------------------------------*
**       Sort the data by
**----------------------------------------------------------------------*
*FORM sort_data .
*  "SORT gt_output BY anln1.
*ENDFORM.                    " SORT_DATA

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA_TO_ALV
*&---------------------------------------------------------------------*
*       Transfer the data to ALV
*----------------------------------------------------------------------*
FORM assign_data_to_alv.
  DATA: lv_field            TYPE char15.

  LOOP AT gt_output ASSIGNING <fs_output>.
    "-- Generate Dynamic Work Area then assign to Field Symbol
    CREATE DATA go_structure LIKE LINE OF <ft_tab>.
    ASSIGN go_structure->* TO <fv_wa>.

    lv_field = 'BUDAT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-budat.

    lv_field = 'LIFNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-lifnr.

    lv_field = 'NAME'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-name.

    lv_field = 'STCD1'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-stcd1.

    lv_field = 'ADDRESS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-address.

    lv_field = 'SGTXT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-sgtxt.

    lv_field = 'EBELN'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ebeln.

    lv_field = 'AWKEY'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-awkey.

    lv_field = 'XBLNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-xblnr.

    lv_field = 'DMBTR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-dmbtr.

    lv_field = 'KNTO'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-knto.

    lv_field = 'VAT_AMT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-vat_amt.

    lv_field = 'NET_PUR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-net_pur.

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

  READ TABLE gt_output INDEX 1 ASSIGNING <fs_output>.

  IF sy-subrc EQ 0.

    CLEAR lv_text.
    MOVE text-001 TO lv_text.
    lr_grid->create_header_information( row     = 1
                                        column  = 1
                                        text    = lv_text
                                        tooltip = lv_text ).

    lr_grid->add_row( ).

    MOVE <fs_output>-butxt TO lv_text.
    lr_grid->create_text( row     = 3
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).

    CLEAR lv_text.
    MOVE <fs_output>-butxt TO lv_text.
    lr_grid->create_text( row     = 3
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).

    CLEAR lv_text.
    CONCATENATE text-002 <fs_output>-tin INTO lv_text SEPARATED BY space.
    lr_grid->create_text( row     = 4
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).

    CLEAR lv_text.
    CONCATENATE text-003 <fs_output>-bpn INTO lv_text SEPARATED BY space.
    lr_grid->create_text( row     = 5
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).

    IF <fs_output>-bpn IS INITIAL.
      MESSAGE s002(zmmpjph).
    ENDIF.

    CLEAR lv_text.
    CONCATENATE text-004 <fs_output>-di INTO lv_text SEPARATED BY space.
    lr_grid->create_text( row     = 6
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).

    CLEAR lv_text.
    CONCATENATE text-005 <fs_output>-period INTO lv_text SEPARATED BY space.
    lr_grid->create_text( row     = 7
                          column  = 1
                          text    = lv_text
                          tooltip = lv_text ).
  ENDIF.
  po_content = lr_grid.
ENDFORM.                    " BUILD_HEADER