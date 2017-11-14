*&---------------------------------------------------------------------*
*& Report  ZR_PM_PIR_UPLOAD_V5
*& DESCRIPTION: <Description>
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 07/25/2014
*& Reference  : TN#63434
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/07/25| GDIMALIWAT |63434      | Created
*&---------------------------------------------------------------------*
REPORT zr_pm_pir_upload_v5.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_upload,
         mat_code  TYPE string,
         mat_desc  TYPE string,
         mrp_area  TYPE string,
         req_year  TYPE string,
         req_month TYPE string,
         req_qty   TYPE string,
         uom       TYPE string,
         version   TYPE string,
       END OF ty_upload,

       BEGIN OF ty_alv,
         material  TYPE matnr,
         mrp       TYPE berid,
         version   TYPE c LENGTH 2,
         year      TYPE c LENGTH 4,
         note      TYPE c LENGTH 200,
         m01       TYPE pbed-plnmg,
         m02       TYPE pbed-plnmg,
         m03       TYPE pbed-plnmg,
         m04       TYPE pbed-plnmg,
         m05       TYPE pbed-plnmg,
         m06       TYPE pbed-plnmg,
         m07       TYPE pbed-plnmg,
         m08       TYPE pbed-plnmg,
         m09       TYPE pbed-plnmg,
         m10       TYPE pbed-plnmg,
         m11       TYPE pbed-plnmg,
         m12       TYPE pbed-plnmg,
         scol      TYPE lvc_t_scol,
         error     TYPE c LENGTH 1,
       END OF ty_alv,

       BEGIN OF ty_logs,
         matnr     TYPE matnr,
         berid     TYPE c LENGTH 4,
         version   TYPE c LENGTH 2,
         type      TYPE c LENGTH 1,
         message   TYPE c LENGTH 255,
       END OF ty_logs.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_upload     TYPE TABLE OF ty_upload,
      gt_alv        TYPE TABLE OF ty_alv,
      gt_fieldcat   TYPE lvc_t_fcat,                        "#EC NEEDED
      gt_logs       TYPE TABLE OF ty_logs.

*&---------------------------------------------------------------------*
*& Class Definitions
*&
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
                            IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM process_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

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
      go_salv_existing   TYPE REF TO cx_salv_existing,      "#EC NEEDED
      go_event_handler   TYPE REF TO lcl_handle_events,     "#EC NEEDED
      go_events          TYPE REF TO cl_salv_events_table.  "#EC NEEDED

DATA: gv_current_year     TYPE c LENGTH 4,
      gv_current_month    TYPE c LENGTH 2,
      gv_next_year        TYPE c LENGTH 4.
*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
  CONSTANTS: co_blank_values TYPE c LENGTH 1 VALUE 'X',
             co_version_00   TYPE c LENGTH 2 VALUE '00',
             co_version_01   TYPE c LENGTH 2 VALUE '01'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_upload>      TYPE ty_upload,
                <fs_alv>         TYPE ty_alv,
                <fs_logs>        TYPE ty_logs.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file     TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM get_file.

*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
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
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = &1
      IMPORTING
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
  "-- Set current year and current month
  gv_current_year  = sy-datum(4).
  gv_current_month = sy-datum+4(2).
  gv_next_year     = gv_current_year + 1.

  PERFORM read_data.
*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM sort_data.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       Retrieve upload file
*----------------------------------------------------------------------*
FORM get_file .
  DATA: lv_file_table TYPE filetable,
        lv_rc         TYPE i,
        lv_title      TYPE string.

  lv_title = text-001.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      default_extension = 'txt'
      file_filter       = '*.xls*|*.XLS|*.xlsx|*.XLSX'
    CHANGING
      file_table        = lv_file_table
      rc                = lv_rc.

  IF sy-subrc EQ 0.
    READ TABLE lv_file_table INTO pa_file INDEX 1.
  ENDIF.
ENDFORM.                    " GET_FILE


*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read data from input file
*----------------------------------------------------------------------*
FORM read_data .

  DATA: lt_xls   TYPE TABLE OF alsmex_tabline.

  FIELD-SYMBOLS: <fs_xls> TYPE alsmex_tabline.

  CONSTANTS: co_xcel_max_row TYPE i VALUE '65000'.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pa_file
      i_begin_col             = 1
      i_begin_row             = 5
      i_end_col               = 7
      i_end_row               = co_xcel_max_row
    TABLES
      intern                  = lt_xls
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc EQ 0.
    LOOP AT lt_xls ASSIGNING <fs_xls>.
      IF <fs_xls>-col EQ '0001'.
        APPEND INITIAL LINE TO gt_upload ASSIGNING <fs_upload>.
        <fs_upload>-mat_code = <fs_xls>-value.
      ELSEIF <fs_xls>-col EQ '0002'.
        <fs_upload>-mat_desc = <fs_xls>-value.
      ELSEIF <fs_xls>-col EQ '0003'.
        <fs_upload>-mrp_area = <fs_xls>-value.
      ELSEIF <fs_xls>-col EQ '0004'.
        <fs_upload>-req_year = <fs_xls>-value(4).
        <fs_upload>-req_month = <fs_xls>-value+4(2).
      ELSEIF <fs_xls>-col EQ '0005'.
        <fs_upload>-req_qty = <fs_xls>-value.
      ELSEIF <fs_xls>-col EQ '0006'.
        <fs_upload>-uom = <fs_xls>-value.
      ELSEIF <fs_xls>-col EQ '0007'.
        <fs_upload>-version = <fs_xls>-value.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_xls>.
  ELSE.
    MESSAGE 'Excel file upload error' TYPE 'E'.
  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       Sort the upload data
*----------------------------------------------------------------------*
FORM sort_data .

  DATA: lv_material         TYPE matnr,
        lv_mrp              TYPE berid,
        lv_year             TYPE c LENGTH 4,
        lv_month            TYPE c LENGTH 2,
        lv_version          TYPE c LENGTH 2.

  DATA: ls_color   TYPE lvc_s_scol,
        lt_color   TYPE lvc_t_scol.

  CONSTANTS: BEGIN OF lc_color_struct_yellow,
               col TYPE lvc_col VALUE '3', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_yellow,

             BEGIN OF lc_color_struct_blue,
               col TYPE lvc_col VALUE '1', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_blue,

             BEGIN OF lc_color_struct_green,
               col TYPE lvc_col VALUE '5', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_green,

             BEGIN OF lc_color_struct_red,
               col TYPE lvc_col VALUE '6', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_red.

  SORT gt_upload BY mat_code mrp_area version req_year req_month ASCENDING.

  LOOP AT gt_upload ASSIGNING <fs_upload>.
    IF lv_material IS INITIAL AND
       lv_mrp      IS INITIAL AND
       lv_version  IS INITIAL AND
       lv_year     IS INITIAL AND
       lv_month    IS INITIAL.

      lv_material = <fs_upload>-mat_code.
      lv_mrp      = <fs_upload>-mrp_area.
      lv_version  = <fs_upload>-version.
      lv_year     = <fs_upload>-req_year.
      lv_month    = <fs_upload>-req_month.

      APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
      <fs_alv>-material = lv_material.
      mc_conversion_alpha <fs_alv>-material.
      <fs_alv>-mrp      = lv_mrp.
      <fs_alv>-version  = lv_version.
      <fs_alv>-year     = lv_year.

      IF lv_version EQ '00' AND lv_year LT gv_next_year.
        CONCATENATE 'Planned Requirements for year' lv_year 'is already final, cannot upload.' INTO <fs_alv>-note SEPARATED BY space.
        PERFORM color_field USING 'NOTE' 'RED' CHANGING <fs_alv>-scol.
        <fs_alv>-error = 'X'.
      ELSE.
        <fs_alv>-note  = ''.
      ENDIF.

      IF <fs_alv>-error NE 'X'.
        PERFORM color_months USING lv_month
                                   lv_year
                          CHANGING lt_color.
      ENDIF.

    ELSEIF lv_material EQ <fs_upload>-mat_code AND
       lv_mrp      EQ <fs_upload>-mrp_area AND
       lv_version  EQ <fs_upload>-version  AND
       lv_year     EQ <fs_upload>-req_year AND
       lv_month    NE <fs_upload>-req_month.

      lv_month = <fs_upload>-req_month.
      IF <fs_alv>-error NE 'X'.
        PERFORM color_months USING lv_month
                                   lv_year
                          CHANGING lt_color.
      ENDIF.

    ELSE.
      lv_material = <fs_upload>-mat_code.
      lv_mrp      = <fs_upload>-mrp_area.
      lv_version  = <fs_upload>-version.
      lv_year     = <fs_upload>-req_year.
      lv_month    = <fs_upload>-req_month.

      APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
      <fs_alv>-material = lv_material.
      mc_conversion_alpha <fs_alv>-material.
      <fs_alv>-mrp      = lv_mrp.
      <fs_alv>-version  = lv_version.
      <fs_alv>-year     = lv_year.
      <fs_alv>-note     = ''.

      IF <fs_alv>-error NE 'X'.
        PERFORM color_months USING lv_month
                                   lv_year
                          CHANGING lt_color.
      ENDIF.
    ENDIF.

  ENDLOOP.
  UNASSIGN <fs_upload>.
ENDFORM.                    " SORT_DATA

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
          t_table      = gt_alv.

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
      mc_change_column_names: 'MATERIAL'          'Material'    'Material'    'Material',
                              'MRP'               'MRP'         'MRP'         'MRP',
                              'VERSION'           'Ver'         'Ver'         'Ver',
                              'YEAR'              'Year'        'Year'        'Year',
                              'NOTE'              'Note'        'Note'        'Note',
                              'M01'               'M 01'        'M 01'        'M 01',
                              'M02'               'M 02'        'M 02'        'M 02',
                              'M03'               'M 03'        'M 03'        'M 03',
                              'M04'               'M 04'        'M 04'        'M 04',
                              'M05'               'M 05'        'M 05'        'M 05',
                              'M06'               'M 06'        'M 06'        'M 06',
                              'M07'               'M 07'        'M 07'        'M 07',
                              'M08'               'M 08'        'M 08'        'M 08',
                              'M09'               'M 09'        'M 09'        'M 09',
                              'M10'               'M 10'        'M 10'        'M 10',
                              'M11'               'M 11'        'M 11'        'M 11',
                              'M12'               'M 12'        'M 12'        'M 12'.

      "-- Hide fields
      mc_set_invisible: 'ERROR'.

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

*      "-- Set ALV HTML Header
*      PERFORM build_header CHANGING go_content.
*
*      go_table->set_top_of_list( go_content ).

      TRY.
        CALL METHOD go_table->set_screen_status
          EXPORTING
            report   = sy-repid
            pfstatus = 'ZSTATUS'.

        CLEAR go_events.
        CALL METHOD go_table->get_event
          RECEIVING
            value = go_events.

        CREATE OBJECT go_event_handler.
        SET HANDLER go_event_handler->on_user_command FOR go_events.
      ENDTRY.

      "-- Set cell colors
      go_columns->set_color_column( 'SCOL' ).

      "-- Display ALV table
      go_table->display( ).

    CATCH cx_salv_data_error INTO go_salv_data_error.
      MESSAGE go_salv_data_error TYPE 'I' DISPLAY LIKE 'E'.
    CATCH cx_salv_msg INTO go_salv_msg.                 "#EC NO_HANDLER
      MESSAGE go_salv_msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.                    " DISPLAY_REPORT

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
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMMAND
*&---------------------------------------------------------------------*
*       Process the user command
*----------------------------------------------------------------------*
*      -->PV_SALV_FUNCTION  User command
*----------------------------------------------------------------------*
FORM process_command  USING    pv_salv_function TYPE salv_de_function.

  CONSTANTS: co_test_mode TYPE c LENGTH 1 VALUE 'X'.

  CASE pv_salv_function.
    WHEN 'UPLOAD'.
      PERFORM post_main USING space.
    WHEN 'UP_TEST'.
      PERFORM post_main USING co_test_mode.
  ENDCASE.

  PERFORM display_logs.

ENDFORM.                    " PROCESS_COMMAND

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*         Subroutine for Displaying Upload Report
*----------------------------------------------------------------------*
FORM display_logs.

  DATA: lv_key  TYPE salv_s_layout_key.

  "-- Create ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_table
        CHANGING
          t_table      = gt_logs.

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
      mc_change_column_names: 'MATNR'           'Material'        'Material'        'Material',
                              'BERID'           'MRP Area'        'MRP Area'        'MRP Area',
                              'VERSION'         'Version'         'Version'         'Version',
                              'TYPE'            'Type'            'Type'            'Type',
                              'MESSAGE'         'Message'         'Message'         'Message'.

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

      "-- Display ALV table
      go_table->display( ).

    CATCH cx_salv_data_error INTO go_salv_data_error.
      MESSAGE go_salv_data_error TYPE 'I' DISPLAY LIKE 'E'.
    CATCH cx_salv_msg INTO go_salv_msg.                 "#EC NO_HANDLER
      MESSAGE go_salv_msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.                    " DISPLAY_LOGS

*&---------------------------------------------------------------------*
*&      Form  CREATE_LOGGING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_ALV     Data ALV
*      -->P_LT_RETURN  Return Message
*      -->PV_MESSAGE   Message
*----------------------------------------------------------------------*
FORM create_logging  USING    pv_alv      TYPE ty_alv
                              pt_return   TYPE bapireturn1
                              pv_message  TYPE char255.

  APPEND INITIAL LINE TO gt_logs ASSIGNING <fs_logs>.
  <fs_logs>-matnr   = pv_alv-material.
  <fs_logs>-berid   = pv_alv-mrp.
  <fs_logs>-version = pv_alv-version.
  IF pt_return IS INITIAL.
    <fs_logs>-type  = 'S'.
    <fs_logs>-message = pv_message.
  ELSE.
    <fs_logs>-type  = pt_return-type.
    <fs_logs>-message = pt_return-message.
  ENDIF.

ENDFORM.                    " CREATE_LOGGING
*&---------------------------------------------------------------------*
*&      Form  COLOR_MONTHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_MONTH  Month
*      -->PV_YEAR   Year
*      <--PT_COLOR  LVC_T_SCOL Structure
*----------------------------------------------------------------------*
FORM color_months  USING    pv_month TYPE char02
                            pv_year  TYPE char04
                   CHANGING pt_color TYPE lvc_t_scol.

  DATA: ls_color   TYPE lvc_s_scol.

  CONSTANTS: BEGIN OF lc_color_struct_yellow,
               col TYPE lvc_col VALUE '3', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_yellow,

             BEGIN OF lc_color_struct_blue,
               col TYPE lvc_col VALUE '1', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_blue,

             BEGIN OF lc_color_struct_green,
               col TYPE lvc_col VALUE '5', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_green,

             BEGIN OF lc_color_struct_red,
               col TYPE lvc_col VALUE '6', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_red.

  IF pv_month EQ '01' OR pv_month EQ '02' OR pv_month EQ '03' OR pv_month EQ '04' OR pv_month EQ '05' OR pv_month EQ '06' OR
     pv_month EQ '07' OR pv_month EQ '08' OR pv_month EQ '09' OR pv_month EQ '10' OR pv_month EQ '11' OR pv_month EQ '12'.

    CASE pv_month.
      WHEN '01'.
        <fs_alv>-m01 = <fs_upload>-req_qty.
      WHEN '02'.
        <fs_alv>-m02 = <fs_upload>-req_qty.
      WHEN '03'.
        <fs_alv>-m03 = <fs_upload>-req_qty.
      WHEN '04'.
        <fs_alv>-m04 = <fs_upload>-req_qty.
      WHEN '05'.
        <fs_alv>-m05 = <fs_upload>-req_qty.
      WHEN '06'.
        <fs_alv>-m06 = <fs_upload>-req_qty.
      WHEN '07'.
        <fs_alv>-m07 = <fs_upload>-req_qty.
      WHEN '08'.
        <fs_alv>-m08 = <fs_upload>-req_qty.
      WHEN '09'.
        <fs_alv>-m09 = <fs_upload>-req_qty.
      WHEN '10'.
        <fs_alv>-m10 = <fs_upload>-req_qty.
      WHEN '11'.
        <fs_alv>-m11 = <fs_upload>-req_qty.
      WHEN '12'.
        <fs_alv>-m12 = <fs_upload>-req_qty.
    ENDCASE.

    IF gv_current_year EQ pv_year.
      IF sy-datum+4(2) LE pv_month.
        CONCATENATE 'M' pv_month INTO ls_color-fname.
        ls_color-color = lc_color_struct_green.
        APPEND ls_color TO pt_color.
        <fs_alv>-scol = pt_color.
      ENDIF.
    ELSEIF gv_current_year LT pv_year.
      CONCATENATE 'M' pv_month INTO ls_color-fname.
      ls_color-color = lc_color_struct_green.
      APPEND ls_color TO pt_color.
      <fs_alv>-scol = pt_color.
    ENDIF.
  ENDIF.
ENDFORM.                    " COLOR_MONTHS
*&---------------------------------------------------------------------*
*&      Form  COLOR_FIELD
*&---------------------------------------------------------------------*
*       Change the color of a specific field in ALV
*----------------------------------------------------------------------*
*      -->PV_FIELD   Field Name
*      -->PV_COLOR   Color
*      <--PT_COLOR   LVC_T_SCOL Structure
*----------------------------------------------------------------------*
FORM color_field  USING    pv_field TYPE char20
                           pv_color TYPE char10
                  CHANGING pt_color TYPE lvc_t_scol.

  DATA: ls_color   TYPE lvc_s_scol.

  CONSTANTS: BEGIN OF lc_color_struct_yellow,
               col TYPE lvc_col VALUE '3', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_yellow,

             BEGIN OF lc_color_struct_blue,
               col TYPE lvc_col VALUE '1', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_blue,

             BEGIN OF lc_color_struct_green,
               col TYPE lvc_col VALUE '5', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_green,

             BEGIN OF lc_color_struct_red,
               col TYPE lvc_col VALUE '6', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_red.

  ls_color-fname = pv_field.
  IF pv_color EQ 'RED'.
    ls_color-color = lc_color_struct_red.
  ELSEIF pv_color EQ 'GREEN'.
    ls_color-color = lc_color_struct_green.
  ELSEIF pv_color EQ 'YELLOW'.
    ls_color-color = lc_color_struct_yellow.
  ELSEIF pv_color EQ 'BLUE'.
    ls_color-color = lc_color_struct_blue.
  ENDIF.

  APPEND ls_color TO pt_color.
ENDFORM.                    " COLOR_FIELD

*&---------------------------------------------------------------------*
*&      Form  POST_MAIN
*&---------------------------------------------------------------------*
*       Main Posting Subroutine
*----------------------------------------------------------------------*
*      -->pv_test_mode   Test Mode Flag
*----------------------------------------------------------------------*
FORM post_main  USING  pv_test_mode TYPE char01.

  DATA: lv_bdzei          TYPE pbim-bdzei,
        lv_active_version TYPE pbim-vervs,
        lv_plnmg          TYPE pbed-plnmg,
        lv_return         TYPE bapireturn1,
        lv_pdatu_low      TYPE pbed-pdatu,
        lv_pdatu_high     TYPE pbed-pdatu.

  REFRESH: gt_logs.
  CLEAR: gt_logs.

  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE error NE 'X'.

    "-- Uploading V00
    IF <fs_alv>-version EQ co_version_00.
      "-- Check if there's an existing Version 01
      SELECT SINGLE bdzei
      INTO lv_bdzei
      FROM pbim
      WHERE matnr EQ <fs_alv>-material
        AND werks EQ <fs_alv>-mrp
        AND versb EQ co_version_01.

      "--Check All Months
      CONCATENATE <fs_alv>-year '0101' INTO lv_pdatu_low.
      CONCATENATE <fs_alv>-year '1231' INTO lv_pdatu_high.

      SELECT SINGLE plnmg
      INTO lv_plnmg
      FROM pbed
      WHERE bdzei EQ lv_bdzei
        AND pdatu BETWEEN lv_pdatu_low AND lv_pdatu_high.

      IF sy-subrc EQ 0.
        "-- Yes, there is no existing Version 01
        "-- Check if there's an existing Version 00
        SELECT SINGLE bdzei
        INTO lv_bdzei
        FROM pbim
        WHERE matnr EQ <fs_alv>-material
          AND werks EQ <fs_alv>-mrp
          AND versb EQ co_version_00.

        IF sy-subrc eq 0.
          "--Check All Months
          CONCATENATE <fs_alv>-year '0101' INTO lv_pdatu_low.
          CONCATENATE <fs_alv>-year '1231' INTO lv_pdatu_high.

          SELECT SINGLE plnmg
          INTO lv_plnmg
          FROM pbed
          WHERE bdzei EQ lv_bdzei
            AND pdatu BETWEEN lv_pdatu_low AND lv_pdatu_high.

          IF sy-subrc eq 0.
            "-- Yes, there is an existing Version 00
            PERFORM change_version_00 USING <fs_alv>
                                             pv_test_mode
                                             space.
          ELSE.
            "-- No, there is no existing Version 00
            PERFORM create_version_00 USING <fs_alv>
                                            pv_test_mode
                                            co_blank_values.
          ENDIF.
        ELSE.
          "-- No, there is no existing Version 00
          PERFORM create_version_00 USING <fs_alv>
                                          pv_test_mode
                                          co_blank_values.
        ENDIF.
      ELSE.
        "-- No, there is no existing Version 01
        "-- Check if there's an existing Version 00
        SELECT SINGLE bdzei
        INTO lv_bdzei
        FROM pbim
        WHERE matnr EQ <fs_alv>-material
          AND werks EQ <fs_alv>-mrp
          AND versb EQ co_version_00.

        IF sy-subrc eq 0.
          "--Check All Months
          CONCATENATE <fs_alv>-year '0101' INTO lv_pdatu_low.
          CONCATENATE <fs_alv>-year '1231' INTO lv_pdatu_high.

          SELECT SINGLE plnmg
          INTO lv_plnmg
          FROM pbed
          WHERE bdzei EQ lv_bdzei
            AND pdatu BETWEEN lv_pdatu_low AND lv_pdatu_high.

          IF sy-subrc eq 0.
            "-- Yes, there is an existing Version 00
            PERFORM change_version_00 USING <fs_alv>
                                             pv_test_mode
                                             space.
          ELSE.
            "-- No, there is no existing Version 00
            PERFORM create_version_00 USING <fs_alv>
                                            pv_test_mode
                                            space.
          ENDIF.
        ELSE.
          "-- No, there is no existing Version 00
          PERFORM create_version_00 USING <fs_alv>
                                          pv_test_mode
                                          space.
        ENDIF.
      ENDIF.

    "-- Uploading V01
    ELSEIF <fs_alv>-version EQ co_version_01.
      "-- Check if there's an existing Version 00
      SELECT SINGLE bdzei
      INTO lv_bdzei
      FROM pbim
      WHERE matnr EQ <fs_alv>-material
        AND werks EQ <fs_alv>-mrp
        AND versb EQ co_version_00.

      "-- Check if there's an existing Version 00
      SELECT SINGLE bdzei
      INTO lv_bdzei
      FROM pbim
      WHERE matnr EQ <fs_alv>-material
        AND werks EQ <fs_alv>-mrp
        AND versb EQ co_version_00.

      IF sy-subrc eq 0.
        "--Check All Months
        CONCATENATE <fs_alv>-year '0101' INTO lv_pdatu_low.
        CONCATENATE <fs_alv>-year '1231' INTO lv_pdatu_high.

        SELECT SINGLE plnmg
        INTO lv_plnmg
        FROM pbed
        WHERE bdzei EQ lv_bdzei
          AND pdatu BETWEEN lv_pdatu_low AND lv_pdatu_high.

        IF sy-subrc EQ 0.
          "-- Yes, there is an existing Version 00
          "-- Check if there's an existing Version 01
          SELECT SINGLE bdzei
          INTO lv_bdzei
          FROM pbim
          WHERE matnr EQ <fs_alv>-material
            AND werks EQ <fs_alv>-mrp
            AND versb EQ co_version_01.

          IF sy-subrc EQ 0.
            "--Check All Months
            CONCATENATE <fs_alv>-year '0101' INTO lv_pdatu_low.
            CONCATENATE <fs_alv>-year '1231' INTO lv_pdatu_high.

            SELECT SINGLE plnmg
            INTO lv_plnmg
            FROM pbed
            WHERE bdzei EQ lv_bdzei
              AND pdatu BETWEEN lv_pdatu_low AND lv_pdatu_high.

            IF sy-subrc eq 0.
              "-- Yes, there is an existing Version 01
              PERFORM change_version_01 USING <fs_alv>
                                               pv_test_mode.
            ELSE.
              "-- No, there is no existing Version 01
               PERFORM deactivate_version_00 USING <fs_alv>
                                                   pv_test_mode.
               PERFORM create_version_01 USING <fs_alv>
                                               pv_test_mode.
            ENDIF.
          ELSE.
            "-- No, there is no existing Version 01
            PERFORM deactivate_version_00 USING <fs_alv>
                                                pv_test_mode.
            PERFORM create_version_01 USING <fs_alv>
                                            pv_test_mode.
          ENDIF.
        ELSE.
          "-- No, there is no existing Version 00
          PERFORM create_logging USING <fs_alv>
                                       lv_return
                                       'Cannot upload version 01, version 00 is missing.'.
        ENDIF.
      ELSE.
        "-- No, there is no existing Version 00
        PERFORM create_logging USING <fs_alv>
                                     lv_return
                                     'Cannot upload version 01, version 00 is missing.'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_alv>.

ENDFORM.                    " POST_MAIN

*&---------------------------------------------------------------------*
*&      Form  CREATE_VERSION_00
*&---------------------------------------------------------------------*
*       Create V00
*----------------------------------------------------------------------*
*      -->PV_ALV                  Data
*      -->PV_TEST_MODE            Flag for Test Mode
*      -->PV_INSERT_BLANK_VALUES  Flag to check if only blank values will be inserted
*----------------------------------------------------------------------*
FORM create_version_00  USING    pv_alv TYPE ty_alv
                                 pv_test_mode
                                 pv_blank_values.

  DATA: ls_requirements_item TYPE bapisitemr,
        ls_req_sched         TYPE bapisshdin,
        ls_requirements_out  TYPE bapisitmeo.

  DATA: lv_do_commit TYPE c LENGTH 1,
        lv_fieldname TYPE char20,
        lv_num       TYPE numc2,
        lv_return    TYPE bapireturn1.

  DATA: lt_sched_in          TYPE TABLE OF bapisshdin,
        lt_requirements_out  TYPE TABLE OF bapisitmeo,
        lt_return            TYPE TABLE OF bapireturn1.

  FIELD-SYMBOLS: <fs_month>             TYPE pbed-plnmg,
                 <fs_requirements_out>  TYPE bapisitmeo.

  IF pv_test_mode IS INITIAL.
    MOVE 'X' TO lv_do_commit.
  ELSE.
    CLEAR:      lv_do_commit.
  ENDIF.

  CLEAR: lt_return, lt_sched_in,ls_requirements_item.

  ls_requirements_item-material           =     pv_alv-material.
  ls_requirements_item-plant              =     pv_alv-mrp.
  ls_requirements_item-version            =     pv_alv-version.

  IF pv_blank_values EQ 'X'.
    ls_requirements_item-vers_activ         =     ''.
  ELSE.
    ls_requirements_item-vers_activ         =     'X'.
  ENDIF.
  ls_requirements_item-mrp_area           =     pv_alv-mrp.

  "-- Prepare the 12 months
  DO 12 TIMES.
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_num.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_num
      IMPORTING
        output = lv_num.

    CONCATENATE 'M' lv_num INTO lv_fieldname.
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE pv_alv TO <fs_month>.

    CONCATENATE pv_alv-year lv_num '01' INTO ls_req_sched-req_date.
    ls_req_sched-date_type   =  3.  "3 for month...
    IF pv_blank_values EQ 'X'.
      ls_req_sched-req_qty     = 0.
    ELSE.
      ls_req_sched-req_qty     =  <fs_month>.
    ENDIF.
    APPEND ls_req_sched TO lt_sched_in.
  ENDDO.

  CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
    EXPORTING
      requirements_item        = ls_requirements_item
      do_commit                = lv_do_commit
      update_mode              = lv_do_commit
    TABLES
      requirements_schedule_in = lt_sched_in
      return                   = lt_return.

  READ TABLE lt_return INTO lv_return INDEX 1.
  PERFORM create_logging USING pv_alv
                               lv_return
                               'Requirements have been created successfully.'.

*    ENDIF.

ENDFORM.                    " CREATE_VERSION_00

*&---------------------------------------------------------------------*
*&      Form  CHANGE_VERSION_00
*&---------------------------------------------------------------------*
*       Change V00
*----------------------------------------------------------------------*
*      -->PV_ALV                  Data
*      -->PV_TEST_MODE            Flag for Test Mode
*      -->PV_INSERT_BLANK_VALUES  Flag to check if only blank values will be inserted
*----------------------------------------------------------------------*
FORM change_version_00  USING    pv_alv TYPE ty_alv
                                 pv_test_mode
                                 pv_blank_values.

  DATA: ls_requirements_item TYPE bapisitemr,
        ls_req_sched         TYPE bapisshdin,
        ls_requirements_out  TYPE bapisitmeo.

  DATA: lv_do_commit TYPE c LENGTH 1,
        lv_fieldname TYPE char20,
        lv_num       TYPE numc2,
        lv_return    TYPE bapireturn1.

  DATA: lt_sched_in          TYPE TABLE OF bapisshdin,
        lt_requirements_out  TYPE TABLE OF bapisitmeo,
        lt_return            TYPE TABLE OF bapireturn1.

  FIELD-SYMBOLS: <fs_month>             TYPE pbed-plnmg,
                 <fs_requirements_out>  TYPE bapisitmeo.

  IF pv_test_mode IS INITIAL.
    MOVE 'X' TO lv_do_commit.
  ELSE.
    CLEAR:      lv_do_commit.
  ENDIF.

  CLEAR: lt_return, lt_sched_in,ls_requirements_item.

  ls_requirements_item-material           =     pv_alv-material.
  ls_requirements_item-plant              =     pv_alv-mrp.
  ls_requirements_item-version            =     pv_alv-version.

  IF pv_blank_values EQ 'X'.
    ls_requirements_item-vers_activ         =     ''.
  ELSE.
    ls_requirements_item-vers_activ         =     'X'.
  ENDIF.
  ls_requirements_item-mrp_area           =     pv_alv-mrp.

  "-- Prepare the 12 months
  DO 12 TIMES.
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_num.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_num
      IMPORTING
        output = lv_num.

    CONCATENATE 'M' lv_num INTO lv_fieldname.
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE pv_alv TO <fs_month>.

    CONCATENATE pv_alv-year lv_num '01' INTO ls_req_sched-req_date.
    ls_req_sched-date_type   =  3.  "3 for month...
    IF pv_blank_values EQ 'X'.
      ls_req_sched-req_qty     = 0.
    ELSE.
      ls_req_sched-req_qty     =  <fs_month>.
    ENDIF.
    APPEND ls_req_sched TO lt_sched_in.
  ENDDO.

  "-- Get Requirement Details
  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = ls_requirements_item-material
      plant            = ls_requirements_item-plant
      requirementstype = space
      version          = ls_requirements_item-version
      reqmtsplannumber = space
    TABLES
      requirements_out = lt_requirements_out
      return           = lt_return.

    IF lt_return IS NOT INITIAL.
      READ TABLE lt_return INTO lv_return INDEX 1.
      PERFORM create_logging USING pv_alv
                                   lv_return
                                   space.
    ELSE.
      "-- Get existing schedules which are not in the Upload file
      IF lt_requirements_out IS NOT INITIAL.
        LOOP AT lt_requirements_out ASSIGNING <fs_requirements_out>.
          CHECK <fs_requirements_out>-req_date+0(4) = pv_alv-year.
          CLEAR: ls_req_sched.

          ls_req_sched-date_type   =  3.  "3 for month...
          ls_req_sched-req_date    =  <fs_requirements_out>-req_date.
          ls_req_sched-req_qty     =  <fs_requirements_out>-req_qty.

          READ TABLE lt_sched_in TRANSPORTING NO FIELDS WITH KEY req_date+0(6) = ls_req_sched-req_date+0(6).
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_req_sched TO lt_sched_in.
          ENDIF.
        ENDLOOP.
        UNASSIGN <fs_requirements_out>.
      ENDIF.

      CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
        EXPORTING
          material                 = ls_requirements_item-material
          plant                    = ls_requirements_item-plant
          requirementstype         = space
          version                  = ls_requirements_item-version
          reqmtsplannumber         = space
          vers_activ               = ls_requirements_item-vers_activ
          do_commit                = lv_do_commit
          update_mode              = lv_do_commit
          delete_old               = 'X'
        IMPORTING
          requirement_item_out     = ls_requirements_item
        TABLES
          requirements_schedule_in = lt_sched_in
          return                   = lt_return.

      READ TABLE lt_return INTO lv_return INDEX 1.
      PERFORM create_logging USING pv_alv
                                   lv_return
                                   'Requirements have been changed successfully.'.
    ENDIF.

ENDFORM.                    " CHANGE_VERSION_00

*&---------------------------------------------------------------------*
*&      Form  DEACTIVATE_VERSION_00
*&---------------------------------------------------------------------*
*       Deactivate V00
*----------------------------------------------------------------------*
*      -->PV_ALV                  Data
*      -->PV_TEST_MODE            Flag for Test Mode
*----------------------------------------------------------------------*
FORM deactivate_version_00  USING    pv_alv TYPE ty_alv
                                 pv_test_mode.

  DATA: ls_requirements_item TYPE bapisitemr,
        ls_req_sched         TYPE bapisshdin,
        ls_requirements_out  TYPE bapisitmeo.

  DATA: lv_do_commit TYPE c LENGTH 1,
        lv_fieldname TYPE char20,
        lv_num       TYPE numc2,
        lv_return    TYPE bapireturn1.

  DATA: lt_sched_in          TYPE TABLE OF bapisshdin,
        lt_requirements_out  TYPE TABLE OF bapisitmeo,
        lt_return            TYPE TABLE OF bapireturn1.

  FIELD-SYMBOLS: <fs_month>             TYPE pbed-plnmg,
                 <fs_requirements_out>  TYPE bapisitmeo.

  IF pv_test_mode IS INITIAL.
    MOVE 'X' TO lv_do_commit.
  ELSE.
    CLEAR:      lv_do_commit.
  ENDIF.

  CLEAR: lt_return, lt_sched_in,ls_requirements_item.

  ls_requirements_item-material           =     pv_alv-material.
  ls_requirements_item-plant              =     pv_alv-mrp.
  ls_requirements_item-version            =     '00'. "-- Deactiving Version 00
  ls_requirements_item-vers_activ         =     ''.
  ls_requirements_item-mrp_area           =     pv_alv-mrp.

*  "-- Prepare the 12 months
*  DO 12 TIMES.
*    CLEAR: lv_fieldname.
*    MOVE sy-index TO lv_num.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_num
*      IMPORTING
*        output = lv_num.
*
*    CONCATENATE 'M' lv_num INTO lv_fieldname.
*    ASSIGN COMPONENT lv_fieldname OF STRUCTURE pv_alv TO <fs_month>.
*
*    CONCATENATE pv_alv-year lv_num '01' INTO ls_req_sched-req_date.
*    ls_req_sched-date_type   =  3.  "3 for month...
*    ls_req_sched-req_qty     =  <fs_month>.
*    APPEND ls_req_sched TO lt_sched_in.
*  ENDDO.

  "-- Get Requirement Details
  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = ls_requirements_item-material
      plant            = ls_requirements_item-plant
      requirementstype = space
      version          = '00'   "--Get existing details of Version 00
      reqmtsplannumber = space
    TABLES
      requirements_out = lt_requirements_out
      return           = lt_return.

    IF lt_return IS NOT INITIAL.
      READ TABLE lt_return INTO lv_return INDEX 1.
      PERFORM create_logging USING pv_alv
                                   lv_return
                                   space.
    ELSE.
      "-- Get existing schedules which are not in the Upload file
      IF lt_requirements_out IS NOT INITIAL.
        LOOP AT lt_requirements_out ASSIGNING <fs_requirements_out>.
          CHECK <fs_requirements_out>-req_date+0(4) = pv_alv-year.
          CLEAR: ls_req_sched.

          ls_req_sched-date_type   =  3.  "3 for month...
          ls_req_sched-req_date    =  <fs_requirements_out>-req_date.
          ls_req_sched-req_qty     =  <fs_requirements_out>-req_qty.

          READ TABLE lt_sched_in TRANSPORTING NO FIELDS WITH KEY req_date+0(6) = ls_req_sched-req_date+0(6).
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_req_sched TO lt_sched_in.
          ENDIF.
        ENDLOOP.
        UNASSIGN <fs_requirements_out>.
      ENDIF.

      CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
        EXPORTING
          material                 = ls_requirements_item-material
          plant                    = ls_requirements_item-plant
          requirementstype         = space
          version                  = ls_requirements_item-version
          reqmtsplannumber         = space
          vers_activ               = ls_requirements_item-vers_activ
          do_commit                = lv_do_commit
          update_mode              = lv_do_commit
          delete_old               = 'X'
        IMPORTING
          requirement_item_out     = ls_requirements_item
        TABLES
          requirements_schedule_in = lt_sched_in
          return                   = lt_return.

      READ TABLE lt_return INTO lv_return INDEX 1.
      PERFORM create_logging USING pv_alv
                                   lv_return
                                   'Version 00 successfully deactivated.'.
    ENDIF.

ENDFORM.                    " DEACTIVATE_VERSION_00

*&---------------------------------------------------------------------*
*&      Form  CREATE_VERSION_01
*&---------------------------------------------------------------------*
*       Create V01
*----------------------------------------------------------------------*
*      -->PV_ALV                  Data
*      -->PV_TEST_MODE            Flag for Test Mode
*----------------------------------------------------------------------*
FORM create_version_01  USING    pv_alv TYPE ty_alv
                                 pv_test_mode.

  DATA: ls_requirements_item TYPE bapisitemr,
        ls_req_sched         TYPE bapisshdin,
        ls_requirements_out  TYPE bapisitmeo.

  DATA: lv_do_commit TYPE c LENGTH 1,
        lv_fieldname TYPE char20,
        lv_num       TYPE numc2,
        lv_return    TYPE bapireturn1.

  DATA: lt_sched_in          TYPE TABLE OF bapisshdin,
        lt_requirements_out  TYPE TABLE OF bapisitmeo,
        lt_return            TYPE TABLE OF bapireturn1.

  FIELD-SYMBOLS: <fs_month>             TYPE pbed-plnmg,
                 <fs_requirements_out>  TYPE bapisitmeo.

  IF pv_test_mode IS INITIAL.
    MOVE 'X' TO lv_do_commit.
  ELSE.
    CLEAR:      lv_do_commit.
  ENDIF.

  CLEAR: lt_return, lt_sched_in, ls_requirements_item.

  ls_requirements_item-material           =     pv_alv-material.
  ls_requirements_item-plant              =     pv_alv-mrp.
  ls_requirements_item-version            =     pv_alv-version.
  ls_requirements_item-vers_activ         =     'X'.
  ls_requirements_item-mrp_area           =     pv_alv-mrp.

  "-- Prepare the 12 months
  DO 12 TIMES.
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_num.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_num
      IMPORTING
        output = lv_num.

    CONCATENATE 'M' lv_num INTO lv_fieldname.
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE pv_alv TO <fs_month>.

    CONCATENATE pv_alv-year lv_num '01' INTO ls_req_sched-req_date.
    ls_req_sched-date_type   =  3.  "3 for month...
    ls_req_sched-req_qty     =  <fs_month>.
    APPEND ls_req_sched TO lt_sched_in.
  ENDDO.

  "-- Get Requirement Details
  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = ls_requirements_item-material
      plant            = ls_requirements_item-plant
      requirementstype = space
      version          = '00' "--Get previous requirements from Version 00 since there is no existing Version 01 yet
      reqmtsplannumber = space
    TABLES
      requirements_out = lt_requirements_out
      return           = lt_return.

  IF lt_return IS NOT INITIAL.
    READ TABLE lt_return INTO lv_return INDEX 1.
    PERFORM create_logging USING pv_alv
                                 lv_return
                                 space.
  ELSE.
    "-- Get existing schedules which are not in the Upload file
    IF lt_requirements_out IS NOT INITIAL.
      LOOP AT lt_requirements_out ASSIGNING <fs_requirements_out>.
        CHECK <fs_requirements_out>-req_date+0(4) = pv_alv-year.
        CLEAR: ls_req_sched.

        ls_req_sched-date_type   =  3.  "3 for month...
        ls_req_sched-req_date    =  <fs_requirements_out>-req_date.
        ls_req_sched-req_qty     =  <fs_requirements_out>-req_qty.

        READ TABLE lt_sched_in TRANSPORTING NO FIELDS WITH KEY req_date+0(6) = ls_req_sched-req_date+0(6).
        IF sy-subrc IS NOT INITIAL.
          APPEND ls_req_sched TO lt_sched_in.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_requirements_out>.
    ENDIF.

    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
      EXPORTING
        requirements_item        = ls_requirements_item
        do_commit                = lv_do_commit
        update_mode              = lv_do_commit
      TABLES
        requirements_schedule_in = lt_sched_in
        return                   = lt_return.

    READ TABLE lt_return INTO lv_return INDEX 1.
    PERFORM create_logging USING pv_alv
                                 lv_return
                                 'Requirements have been created successfully.'.

  ENDIF.

ENDFORM.                    " CREATE_VERSION_01

*&---------------------------------------------------------------------*
*&      Form  CHANGE_VERSION_01
*&---------------------------------------------------------------------*
*       Change V01
*----------------------------------------------------------------------*
*      -->PV_ALV                  Data
*      -->PV_TEST_MODE            Flag for Test Mode
*----------------------------------------------------------------------*
FORM change_version_01  USING    pv_alv TYPE ty_alv
                                 pv_test_mode.

  DATA: ls_requirements_item TYPE bapisitemr,
        ls_req_sched         TYPE bapisshdin,
        ls_requirements_out  TYPE bapisitmeo.

  DATA: lv_do_commit TYPE c LENGTH 1,
        lv_fieldname TYPE char20,
        lv_num       TYPE numc2,
        lv_return    TYPE bapireturn1.

  DATA: lt_sched_in          TYPE TABLE OF bapisshdin,
        lt_requirements_out  TYPE TABLE OF bapisitmeo,
        lt_return            TYPE TABLE OF bapireturn1.

  FIELD-SYMBOLS: <fs_month>             TYPE pbed-plnmg,
                 <fs_requirements_out>  TYPE bapisitmeo.

  IF pv_test_mode IS INITIAL.
    MOVE 'X' TO lv_do_commit.
  ELSE.
    CLEAR:      lv_do_commit.
  ENDIF.

  CLEAR: lt_return, lt_sched_in, ls_requirements_item.

  ls_requirements_item-material           =     pv_alv-material.
  ls_requirements_item-plant              =     pv_alv-mrp.
  ls_requirements_item-version            =     pv_alv-version.
  ls_requirements_item-vers_activ         =     'X'.
  ls_requirements_item-mrp_area           =     pv_alv-mrp.

  "-- Prepare the 12 months
  DO 12 TIMES.
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_num.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_num
      IMPORTING
        output = lv_num.

    CONCATENATE 'M' lv_num INTO lv_fieldname.
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE pv_alv TO <fs_month>.

    CONCATENATE pv_alv-year lv_num '01' INTO ls_req_sched-req_date.
    ls_req_sched-date_type   =  3.  "3 for month...
    ls_req_sched-req_qty     =  <fs_month>.
    APPEND ls_req_sched TO lt_sched_in.
  ENDDO.

  "-- Get Requirement Details
  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = ls_requirements_item-material
      plant            = ls_requirements_item-plant
      requirementstype = space
      version          = ls_requirements_item-version
      reqmtsplannumber = space
    TABLES
      requirements_out = lt_requirements_out
      return           = lt_return.

  IF lt_return IS NOT INITIAL.
    READ TABLE lt_return INTO lv_return INDEX 1.
    PERFORM create_logging USING pv_alv
                                 lv_return
                                 space.
  ELSE.
    "-- Get existing schedules which are not in the Upload file
    IF lt_requirements_out IS NOT INITIAL.
      LOOP AT lt_requirements_out ASSIGNING <fs_requirements_out>.
        CHECK <fs_requirements_out>-req_date+0(4) = pv_alv-year.
        CLEAR: ls_req_sched.

        ls_req_sched-date_type   =  3.  "3 for month...
        ls_req_sched-req_date    =  <fs_requirements_out>-req_date.
        ls_req_sched-req_qty     =  <fs_requirements_out>-req_qty.

        READ TABLE lt_sched_in TRANSPORTING NO FIELDS WITH KEY req_date+0(6) = ls_req_sched-req_date+0(6).
        IF sy-subrc IS NOT INITIAL.
          APPEND ls_req_sched TO lt_sched_in.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_requirements_out>.
    ENDIF.

    CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
      EXPORTING
        material                 = ls_requirements_item-material
        plant                    = ls_requirements_item-plant
        requirementstype         = space
        version                  = ls_requirements_item-version
        reqmtsplannumber         = space
        vers_activ               = ls_requirements_item-vers_activ
        do_commit                = lv_do_commit
        update_mode              = lv_do_commit
        delete_old               = 'X'
      IMPORTING
        requirement_item_out     = ls_requirements_item
      TABLES
        requirements_schedule_in = lt_sched_in
        return                   = lt_return.

    READ TABLE lt_return INTO lv_return INDEX 1.
    PERFORM create_logging USING pv_alv
                                 lv_return
                                 'Requirements have been changed successfully.'.

  ENDIF.

ENDFORM.                    " CHANGE_VERSION_01