*&---------------------------------------------------------------------*
*& Report  ZRPM_RECLASS_CLEANUP_REPORT
*& DESCRIPTION: Report for Reclass Clean-up Updates
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 09/19/2013
*& Reference  : TN#43896
*&---------------------------------------------------------------------*
*& Date      | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/09/09| GDIMALIWAT | 43896      | Created
*&---------------------------------------------------------------------*
REPORT zrpm_reclass_cleanup_report.

*INCLUDE
TABLES: mseg.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_mseg,
        mblnr     TYPE mseg-mblnr,
        line_id   TYPE mseg-line_id,
        matnr     TYPE mseg-matnr,
        menge     TYPE mseg-menge,
        aufnr     TYPE mseg-aufnr,
        mat_pspnr TYPE mseg-mat_pspnr,
        werks     TYPE mseg-werks,
        sakto     TYPE mseg-sakto,
        nplnr     TYPE mseg-nplnr,
       END OF ty_mseg,

       BEGIN OF ty_bsis,
        belnr    TYPE bsis-belnr,
        dmbtr    TYPE bsis-dmbtr,
        shkzg    TYPE bsis-shkzg,
        blart    TYPE bsis-blart,
        bldat    TYPE bsis-bldat,
        budat    TYPE bsis-budat,
       END OF ty_bsis,

       BEGIN OF ty_output,
        blart       TYPE bsis-blart,
        bldat       TYPE bsis-bldat,
        budat       TYPE bsis-budat,
        belnr       TYPE bsis-belnr,
        mat_pspnr   TYPE c LENGTH 40,
        aufnr       TYPE mseg-aufnr,
        dmbtr       TYPE bsis-dmbtr,
        werks       TYPE mseg-werks,
        ingpr       TYPE afih-ingpr,
        ilart       TYPE afih-ilart,
        vaplz       TYPE caufv-vaplz,
        mblnr       TYPE mseg-mblnr,
        line_id     TYPE mseg-line_id,
        matnr       TYPE mseg-matnr,
        menge       TYPE mseg-menge,
       END OF ty_output.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_mseg     TYPE TABLE OF ty_mseg,
      gt_mseg2    TYPE TABLE OF ty_mseg,
      gt_bsis     TYPE TABLE OF ty_bsis,
      gt_bsis2    TYPE TABLE OF ty_bsis,
      gt_bsis3    TYPE TABLE OF ty_bsis,
      gt_output   TYPE TABLE OF ty_output,
      gt_fieldcat TYPE lvc_t_fcat.                          "#EC NEEDED

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
      go_exc             TYPE REF TO cx_salv_error,         "#EC NEEDED
      go_salv_msg        TYPE REF TO cx_salv_msg,           "#EC NEEDED
      go_salv_not_found  TYPE REF TO cx_salv_not_found,     "#EC NEEDED
      go_salv_data_error TYPE REF TO cx_salv_data_error,    "#EC NEEDED
      go_salv_existing   TYPE REF TO cx_salv_existing.      "#EC NEEDED

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
*CONSTANTS:

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_mseg>   TYPE ty_mseg,
                <fs_bsis>   TYPE ty_bsis,
                <fs_output> TYPE ty_output,
                <fs_fieldcat>    TYPE lvc_s_fcat,           "#EC NEEDED
                <ft_tab>         TYPE STANDARD TABLE,       "#EC NEEDED
                <fv_wa>,
                <fv_field>        TYPE any.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000. "Order Number
SELECT-OPTIONS: so_order FOR  mseg-aufnr.
SELECTION-SCREEN END OF BLOCK blk01.

SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE text-001. "WBS Element
SELECT-OPTIONS: so_wbs   FOR  mseg-mat_pspnr.
SELECTION-SCREEN END OF BLOCK blk02.

SELECTION-SCREEN BEGIN OF BLOCK blk03 WITH FRAME TITLE text-002. "Plant and Year
SELECT-OPTIONS: so_plant FOR  mseg-werks,
                so_year  FOR  mseg-mjahr.
SELECTION-SCREEN END OF BLOCK blk03.

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

  IF so_order IS NOT INITIAL.
    PERFORM query_by_order_no.
  ENDIF.

  IF so_wbs IS NOT INITIAL.
    PERFORM query_by_wbs.
  ENDIF.

  IF so_order IS INITIAL AND so_wbs IS INITIAL.
    WRITE:/ text-003.
  ENDIF.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
 PERFORM SORT_DATA.
 PERFORM BUILD_CATALOGUE.
 PERFORM ASSIGN_DATA_TO_ALV.
 PERFORM DISPLAY_REPORT.

*&---------------------------------------------------------------------*
*&      Form  QUERY_BY_ORDER_NO
*&---------------------------------------------------------------------*
*       Retrieve records using the order number
*----------------------------------------------------------------------*
FORM query_by_order_no .

  DATA: lv_ingpr TYPE afih-ingpr,
        lv_ilart TYPE afih-ilart,
        lv_vaplz TYPE caufv-vaplz,
        lv_wbs   TYPE c LENGTH 40.

  REFRESH gt_mseg.

  "MSEG
  SELECT mblnr line_id matnr menge aufnr mat_pspnr werks sakto nplnr
  INTO TABLE gt_mseg
  FROM mseg
  WHERE aufnr     IN so_order
    AND werks     IN so_plant
    AND mjahr     IN so_year.

  IF sy-subrc EQ 0.
    LOOP AT gt_mseg ASSIGNING <fs_mseg>.
      REFRESH gt_bsis.
      CLEAR: lv_ingpr,
             lv_ilart,
             lv_vaplz,
             lv_wbs.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_mseg>-mat_pspnr
        IMPORTING
          output = lv_wbs.

      "AFIH
      SELECT SINGLE ingpr ilart
      INTO (lv_ingpr,lv_ilart)
      FROM afih
      WHERE aufnr EQ <fs_mseg>-aufnr.

      "CAUFV
      SELECT SINGLE vaplz
      INTO lv_vaplz
      FROM caufv
      WHERE aufnr EQ <fs_mseg>-aufnr.

      "BSIS
      SELECT belnr dmbtr shkzg blart bldat budat
      INTO TABLE gt_bsis
      FROM bsis
      WHERE hkont EQ <fs_mseg>-sakto
        AND aufnr EQ <fs_mseg>-aufnr.

      IF sy-subrc EQ 0.
        LOOP AT gt_bsis ASSIGNING <fs_bsis>.
          APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
          <fs_output>-blart     = <fs_bsis>-blart.
          <fs_output>-bldat     = <fs_bsis>-bldat.
          <fs_output>-budat     = <fs_bsis>-budat.
          <fs_output>-belnr     = <fs_bsis>-belnr.
          <fs_output>-mat_pspnr	= lv_wbs.
          <fs_output>-aufnr     = <fs_mseg>-aufnr.
          <fs_output>-dmbtr     = <fs_bsis>-dmbtr.
          <fs_output>-werks     = <fs_mseg>-werks.
          <fs_output>-ingpr     = lv_ingpr.
          <fs_output>-ilart     = lv_ilart.
          <fs_output>-vaplz     = lv_vaplz.
          <fs_output>-mblnr     = <fs_mseg>-mblnr.
          <fs_output>-line_id   = <fs_mseg>-line_id.
          <fs_output>-matnr     = <fs_mseg>-matnr.
          <fs_output>-menge     = <fs_mseg>-menge.
        ENDLOOP.
        UNASSIGN <fs_bsis>.
      ENDIF.

    ENDLOOP.
    UNASSIGN <fs_mseg>.
  ENDIF.
ENDFORM.                    " QUERY_BY_ORDER_NO
*&---------------------------------------------------------------------*
*&      Form  QUERY_BY_WBS
*&---------------------------------------------------------------------*
*       Retrieve records using the WBS Element
*----------------------------------------------------------------------*
FORM query_by_wbs .

  DATA: lv_measure TYPE bsis-measure,
        lv_wbs     TYPE c LENGTH 40,
        lv_wbs_len TYPE i.

  REFRESH gt_mseg2.
  "MSEG
  SELECT mblnr line_id matnr menge aufnr mat_pspnr werks sakto nplnr
  INTO TABLE gt_mseg2
  FROM mseg
  WHERE mat_pspnr IN so_wbs
    AND werks     IN so_plant
    AND mjahr     IN so_year
    AND bwart     EQ 'Z52'.

  IF sy-subrc EQ 0.
    LOOP AT gt_mseg2 ASSIGNING <fs_mseg>.
      REFRESH gt_bsis2.
      REFRESH gt_bsis3.
      CLEAR: lv_measure,
             lv_wbs,
             lv_wbs_len.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_mseg>-mat_pspnr
        IMPORTING
          output = lv_wbs.

      lv_wbs_len = strlen( lv_wbs ) - 1.
      lv_measure = lv_wbs(lv_wbs_len).

      "BSIS MEASURE
      SELECT belnr dmbtr shkzg blart bldat budat
      INTO TABLE gt_bsis2
      FROM bsis
      WHERE hkont EQ <fs_mseg>-sakto
      AND measure EQ lv_measure.

      IF sy-subrc EQ 0.
        LOOP AT gt_bsis2 ASSIGNING <fs_bsis>.
          APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
          <fs_output>-blart     = <fs_bsis>-blart.
          <fs_output>-bldat     = <fs_bsis>-bldat.
          <fs_output>-budat     = <fs_bsis>-budat.
          <fs_output>-belnr     = <fs_bsis>-belnr.
          <fs_output>-mat_pspnr	= lv_wbs.
          <fs_output>-aufnr     = <fs_mseg>-aufnr.
          <fs_output>-dmbtr     = <fs_bsis>-dmbtr.
          <fs_output>-werks     = <fs_mseg>-werks.
          <fs_output>-ingpr     = ''.
          <fs_output>-ilart     = ''.
          <fs_output>-vaplz     = ''.
          <fs_output>-mblnr     = <fs_mseg>-mblnr.
          <fs_output>-line_id   = <fs_mseg>-line_id.
          <fs_output>-matnr     = <fs_mseg>-matnr.
          <fs_output>-menge     = <fs_mseg>-menge.
        ENDLOOP.
        UNASSIGN <fs_bsis>.
      ENDIF.

      "BSIS PROJK
      SELECT belnr dmbtr shkzg blart bldat budat
      INTO TABLE gt_bsis3
      FROM bsis
      WHERE hkont EQ <fs_mseg>-sakto
        AND projk EQ <fs_mseg>-mat_pspnr.

      IF sy-subrc EQ 0.
        LOOP AT gt_bsis3 ASSIGNING <fs_bsis>.
          APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
          <fs_output>-blart     = <fs_bsis>-blart.
          <fs_output>-bldat     = <fs_bsis>-bldat.
          <fs_output>-budat     = <fs_bsis>-budat.
          <fs_output>-belnr     = <fs_bsis>-belnr.
          <fs_output>-mat_pspnr	= lv_wbs.
          <fs_output>-aufnr     = <fs_mseg>-aufnr.
          <fs_output>-dmbtr     = <fs_bsis>-dmbtr.
          <fs_output>-werks     = <fs_mseg>-werks.
          <fs_output>-ingpr     = ''.
          <fs_output>-ilart     = ''.
          <fs_output>-vaplz     = ''.
          <fs_output>-mblnr     = <fs_mseg>-mblnr.
          <fs_output>-line_id   = <fs_mseg>-LINE_ID.
          <fs_output>-matnr     = <fs_mseg>-matnr.
          <fs_output>-menge     = <fs_mseg>-menge.
        ENDLOOP.
        UNASSIGN <fs_bsis>.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_mseg>.
  ENDIF.
ENDFORM.                    " QUERY_BY_WBS

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOGUE
*&---------------------------------------------------------------------*
*       Subroutine for Building Field Catalogue
*----------------------------------------------------------------------*
FORM build_catalogue.
  DATA: lv_field_name TYPE char10.

  "-- Reinitialize Field Catalogue Table
  REFRESH: gt_fieldcat.

  "-- Append to Field Catalogue
  mc_append_fieldcat: 'BLART'     'BSIS'   'CHAR'  2 0 1  space,
                      'BLDAT'     'BSIS'   'CHAR'  8 0 2  space,
                      'BUDAT'     'BSIS'   'CHAR'  8 0 3  space,
                      'BELNR'     'BSIS'   'CHAR' 10 9 4  space,
                      'MAT_PSPNR' 'MSEG'   'CHAR' 40 0 5  space,
                      'AUFNR'     'MSEG'   'CHAR' 40 0 6  space,
                      'DMBTR'     'BSIS'   'CURR' 13 2 7  space,
                      'WERKS'     'MSEG'   'CHAR'  4 0 8  space,
                      'INGPR'     'AFIH'   'CHAR' 40 0 9  space,
                      'ILART'     'AFIH'   'CHAR' 40 0 10 space,
                      'VAPLZ'     'CAUFV'  'CHAR' 40 0 11 space,
                      'MBLNR'     'MSEG'   'CHAR' 10 0 12 space,
                      'LINE_ID'   'MSEG'   'NUMC'  6 0 13 space,
                      'MATNR'     'MSEG'   'CHAR' 18 0 14 space,
                      'MENGE'     'MSEG'   'QUAN' 13 3 15 space.

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

  DATA: lv_field_name TYPE lvc_fname,
        lv_date       TYPE char10,
        lv_long_txt   TYPE scrtext_l,
        lv_medm_txt   TYPE scrtext_m,
        lv_shrt_txt   TYPE scrtext_s.

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

  "-- Get field attributes
  go_columns   = go_table->get_columns( ).

  "-- Change ALV Column Name
  mc_change_column_names: 'BLART'     text-004 text-004 'BLART',
                          'BLDAT'     text-005 text-005 'BLDAT',
                          'BUDAT'     text-006 text-006 'BUDAT',
                          'BELNR'     text-007 text-007 'BELNR',
                          'MAT_PSPNR' text-008 text-008 'MAT_PSPNR',
                          'AUFNR'     text-009 text-009 'AUFNR',
                          'DMBTR'     text-010 text-010 'DMBTR',
                          'WERKS'     text-011 text-011 'WERKS',
                          'INGPR'     text-012 text-012 'INGPR',
                          'ILART'     text-013 text-013 'ILART',
                          'VAPLZ'     text-014 text-014 'VAPLZ',
                          'MBLNR'     text-015 text-015 'MBLNR',
                          'LINE_ID'   text-016 text-016 'LINE_ID',
                          'MATNR'     text-017 text-017 'MATNR',
                          'MENGE'     text-018 text-018 'MENGE'.

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

  go_table->set_top_of_list( go_content ).

  "-- Display ALV table
  go_table->display( ).
ENDFORM.                    " DISPLAY_REPORT

*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       Sort the data and delete duplicate BELNR and LINE_ID
*----------------------------------------------------------------------*
form SORT_DATA .
  SORT gt_output BY belnr line_id ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_output COMPARING belnr line_id.
endform.                    " SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA_TO_ALV
*&---------------------------------------------------------------------*
*       Transfer the data to ALV
*----------------------------------------------------------------------*
form ASSIGN_DATA_TO_ALV .
  DATA: lv_field            TYPE char15,
        lv_amount           TYPE meng15,
        lv_total            TYPE meng15.

  LOOP AT gt_output ASSIGNING <fs_output>.
    "-- Generate Dynamic Work Area then assign to Field Symbol
    CREATE DATA go_structure LIKE LINE OF <ft_tab>.
    ASSIGN go_structure->* TO <fv_wa>.

    lv_field  = 'BLART'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-blart.

    lv_field  = 'BLDAT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-bldat.

    lv_field  = 'BUDAT'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-budat.

    lv_field  = 'BELNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-belnr.

    lv_field  = 'MAT_PSPNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-mat_pspnr.

    lv_field  = 'AUFNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-aufnr.

    lv_field  = 'DMBTR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-dmbtr.

    lv_field  = 'WERKS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-werks.

    lv_field  = 'INGPR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ingpr.

    lv_field  = 'ILART'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ilart.

    lv_field  = 'VAPLZ'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-vaplz.

    lv_field  = 'MBLNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-mblnr.
    CLEAR lv_total.

    lv_field  = 'LINE_ID'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-line_id.

    lv_field  = 'MATNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-matnr.

    lv_field  = 'MENGE'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-menge.

    " Append to dynamic table
    APPEND <fv_wa> TO <ft_tab>.
  ENDLOOP.
  UNASSIGN <fs_output>.
endform.                    " ASSIGN_DATA_TO_ALV