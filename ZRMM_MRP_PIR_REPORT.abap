*&---------------------------------------------------------------------*
*& Report  ZRMM_MRP_PIR_REPORT
*& DESCRIPTION: PIR Report Extraction Program
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : GDIMALIWAT
*& Reference  : TN#43122
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/09/04|GDIMALIWAT| 43122     | Created
*& 2013/11/18|GDIMALIWAT| 49183     | Added version number as paraneter
*& 2013/12/18|GDIMALIWAT| 51890     | Change: For 2014 onwards, use the parameter in a monthly scheme.
*& 2014/01/02|JLIWAG    | 51890     | Change date parameter
*& 2014/01/02|GDIMALIWAT| 51890     | Added options for Materials for Projects/Maintenance
*&---------------------------------------------------------------------*
REPORT zrmm_mrp_pir_report MESSAGE-ID zisu.

*INCLUDE
TABLES: pbim, pbed, scal.

*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_pbim,
        matnr TYPE c LENGTH 18,
        werks TYPE c LENGTH 4,
        bdzei TYPE pbim-bdzei,
        pspel TYPE pbim-pspel,
        pdatu TYPE pbed-pdatu,
        plnmg TYPE meng15,
       END OF ty_pbim,

       BEGIN OF ty_pbim_2014,
        matnr TYPE c LENGTH 18,
        werks TYPE c LENGTH 4,
        bdzei TYPE pbim-bdzei,
        pspel TYPE pbim-pspel,
        "sobkz TYPE pbim-sobkz, "-- Added by RANDYSY TN#51890 01/02/2014
        pdatu TYPE pbed-pdatu, "-- Added by RANDYSY TN#51890 01/02/2014
        perxx TYPE pbed-perxx,
        plnmg TYPE meng15,
       END OF ty_pbim_2014,

       BEGIN OF ty_pir,
        matnr  TYPE c LENGTH 18,
        werks  TYPE c LENGTH 4,
        "bdzei  TYPE pbim-bdzei,
        pspel TYPE pbim-pspel,
        period TYPE c LENGTH 6,
        plnmg  TYPE meng15,
       END OF ty_pir,

       BEGIN OF ty_final,
        matnr  TYPE c LENGTH 18,
        desc   TYPE c LENGTH 40,
        werks  TYPE c LENGTH 4,
        ucost  TYPE mbew-verpr,
        uom    TYPE mean-meinh,
        wbs    TYPE c LENGTH 40,
        period TYPE c LENGTH 6,
        plnmg  TYPE meng15,
       END OF ty_final,

       BEGIN OF ty_output,
        matnr  TYPE c LENGTH 18,
        desc   TYPE c LENGTH 40,
        werks  TYPE c LENGTH 4,
        ucost  TYPE mbew-verpr,
        uom    TYPE mean-meinh,
        wbs    TYPE c LENGTH 40,
       END OF ty_output,

       BEGIN OF ty_period,
        period TYPE c LENGTH 6,
       END OF ty_period.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_pbim       TYPE TABLE OF ty_pbim,
      gt_pbim_2014  TYPE TABLE OF ty_pbim_2014,
      gt_pir        TYPE TABLE OF ty_pir,
      gt_pre_final  TYPE TABLE OF ty_final,
      gt_final      TYPE TABLE OF ty_final,
      gt_output     TYPE TABLE OF ty_output,
      gt_period     TYPE TABLE OF ty_period,
      gt_fieldcat   TYPE lvc_t_fcat.                        "#EC NEEDED

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
FIELD-SYMBOLS:  <fs_pbim>        TYPE ty_pbim,
                <fs_pbim_2014>   TYPE ty_pbim_2014,
                <fs_pir>         TYPE ty_pir,
                <fs_final>       TYPE ty_final,
                <fs_output>      TYPE ty_output,
                <fs_period>      TYPE ty_period,
                <fs_fieldcat>    TYPE lvc_s_fcat,           "#EC NEEDED
                <ft_tab>         TYPE STANDARD TABLE,       "#EC NEEDED
                <fv_wa>,
                <fv_field>        TYPE any.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
SELECT-OPTIONS: so_matnr FOR  pbim-matnr OBLIGATORY.
SELECT-OPTIONS: so_werks FOR  pbim-werks OBLIGATORY.
SELECT-OPTIONS: so_weeks FOR  scal-week  OBLIGATORY NO-EXTENSION.
PARAMETERS:     pa_versb TYPE pbim-versb OBLIGATORY DEFAULT '00'.
PARAMETERS:     pa_rad1  RADIOBUTTON GROUP g1,
                pa_rad2  RADIOBUTTON GROUP g1.
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

  IF so_weeks-low(4) GE '2014'.
    PERFORM begin_extraction_2014.
  ELSE.
    PERFORM begin_extraction.
  ENDIF.


*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM get_data.
  PERFORM build_catalogue.
  PERFORM assign_data_to_alv.
  PERFORM display_report.


*&---------------------------------------------------------------------*
*&      Form  BEGIN_EXTRACTION
*&---------------------------------------------------------------------*
*       Begin the extraction process
*----------------------------------------------------------------------*
FORM begin_extraction .

  DATA: lv_pir              TYPE ty_pir,
        lv_description      TYPE makt-maktx,
        lv_unit_cost        TYPE mbew-verpr,
        lv_unit_of_measure  TYPE mean-meinh,
        lv_wbs              TYPE c LENGTH 40,
        lv_week             TYPE scal-week.

  DATA: lv_first_day TYPE scal-date,
        lv_last_day TYPE scal-date.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week = so_weeks-low
    IMPORTING
      date = lv_first_day.

  IF so_weeks-high IS NOT INITIAL.
    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week = so_weeks-high
      IMPORTING
        date = lv_last_day.
    ADD 6 TO lv_last_day.
  ELSE.
    lv_last_day = lv_first_day + 6.
  ENDIF.

  SELECT a~matnr a~werks a~bdzei a~pspel b~pdatu SUM( plnmg )
    INTO TABLE gt_pbim
    FROM pbim AS a INNER JOIN pbed AS b ON ( a~bdzei EQ b~bdzei )
    WHERE a~matnr IN so_matnr
      AND a~werks IN so_werks
      AND a~versb EQ pa_versb
      AND b~pdatu BETWEEN lv_first_day AND lv_last_day  "-- Use the week
    GROUP BY a~matnr a~werks a~bdzei a~pspel b~pdatu.

  IF sy-subrc EQ 0.
    SORT gt_pbim BY matnr werks pspel pdatu ASCENDING.

    CLEAR: lv_pir.

    LOOP AT gt_pbim ASSIGNING <fs_pbim>.
      CLEAR lv_week.

      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date = <fs_pbim>-pdatu
        IMPORTING
          week = lv_week.

      IF lv_pir IS INITIAL.
        lv_pir-period = lv_week.
        lv_pir-matnr  = <fs_pbim>-matnr.
        lv_pir-werks  = <fs_pbim>-werks.
        lv_pir-plnmg  = <fs_pbim>-plnmg.
        lv_pir-pspel  = <fs_pbim>-pspel.
      ELSE.
        IF lv_pir-period EQ lv_week
          AND lv_pir-matnr EQ <fs_pbim>-matnr
          AND lv_pir-werks EQ <fs_pbim>-werks
          AND lv_pir-pspel EQ <fs_pbim>-pspel.
          lv_pir-plnmg = lv_pir-plnmg + <fs_pbim>-plnmg.
        ELSE.
          APPEND INITIAL LINE TO gt_pir ASSIGNING <fs_pir>.
          <fs_pir> = lv_pir.
          CLEAR lv_pir.
          lv_pir-period = lv_week.
          lv_pir-matnr  = <fs_pbim>-matnr.
          lv_pir-werks  = <fs_pbim>-werks.
          lv_pir-plnmg  = <fs_pbim>-plnmg.
          lv_pir-pspel  = <fs_pbim>-pspel.
        ENDIF.
      ENDIF.
      "WRITE:/ <fs_pbim>-matnr, <fs_pbim>-werks, <fs_pbim>-bdzei, <fs_pbim>-pdatu, <fs_pbim>-plnmg.
    ENDLOOP.
    APPEND INITIAL LINE TO gt_pir ASSIGNING <fs_pir>.
    <fs_pir> = lv_pir.
    CLEAR lv_pir.
    UNASSIGN <fs_pbim>.

    SORT gt_pir BY matnr werks pspel period ASCENDING.

    LOOP AT gt_pir ASSIGNING <fs_pir>.
      CLEAR: lv_description,
             lv_unit_cost,
             lv_unit_of_measure.

      SELECT SINGLE maktx
        INTO lv_description
        FROM makt
        WHERE matnr EQ <fs_pir>-matnr.

      SELECT SINGLE verpr
        INTO lv_unit_cost
        FROM mbew
        WHERE matnr EQ <fs_pir>-matnr
          AND bwkey EQ <fs_pir>-werks.

      SELECT SINGLE meins
        INTO lv_unit_of_measure
        FROM mara
        WHERE matnr EQ <fs_pir>-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input  = lv_unit_of_measure
        IMPORTING
          output = lv_unit_of_measure.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_pir>-pspel
        IMPORTING
          output = lv_wbs.

      APPEND INITIAL LINE TO gt_final ASSIGNING <fs_final>.
      <fs_final>-matnr  = <fs_pir>-matnr.
      <fs_final>-desc   = lv_description.
      <fs_final>-werks  = <fs_pir>-werks.
      <fs_final>-ucost  = lv_unit_cost.
      <fs_final>-uom    = lv_unit_of_measure.
      <fs_final>-wbs    = lv_wbs.
      <fs_final>-period = <fs_pir>-period.
      <fs_final>-plnmg  = <fs_pir>-plnmg.

      "WRITE:/ <fs_pir>-matnr, lv_description, <fs_pir>-werks, lv_unit_cost, lv_unit_of_measure, <fs_pir>-period, <fs_pir>-plnmg.
    ENDLOOP.
    UNASSIGN <fs_pir>.

    " Clear the contents of unused internal tables
    REFRESH: gt_pbim, gt_pir.
  ENDIF.
ENDFORM.                    " BEGIN_EXTRACTION

*&---------------------------------------------------------------------*
*&      Form  BEGIN_EXTRACTION_2014
*&---------------------------------------------------------------------*
*       Begin the extraction process for 2014 onwards
*----------------------------------------------------------------------*
FORM begin_extraction_2014.
  TYPES: BEGIN OF ty_plnmg,
          pdatu TYPE dat01,
          perxx TYPE perxx,
          plnmg TYPE plnmg,
         END OF ty_plnmg.

  DATA: lv_pir              TYPE ty_pir,
        lv_description      TYPE makt-maktx,
        lv_unit_cost        TYPE mbew-verpr,
        lv_unit_of_measure  TYPE mean-meinh,
        lv_wbs              TYPE c LENGTH 40,
        lv_week             TYPE scal-week,

        lv_plnmg            TYPE plnmg,
        lv_from             TYPE sydatum,
        lv_to               TYPE sydatum,
        lt_plnmg            TYPE TABLE OF ty_plnmg.

  DATA: lv_first_day        TYPE scal-date,
        lv_last_day         TYPE scal-date.

  FIELD-SYMBOLS:
        <fs_plnmg>          TYPE ty_plnmg.



  IF pa_rad1 eq 'X'.
    SELECT a~matnr a~werks a~bdzei a~pspel b~pdatu b~perxx SUM( plnmg )
    INTO TABLE gt_pbim_2014
    FROM pbim AS a INNER JOIN pbed AS b ON ( a~bdzei EQ b~bdzei )
    WHERE a~matnr IN so_matnr
      AND a~werks IN so_werks
      AND a~versb EQ pa_versb
      AND b~perxx BETWEEN so_weeks-low(6) AND so_weeks-high(6)    "-- Use the Month
    GROUP BY a~matnr a~werks a~bdzei a~pspel b~pdatu b~perxx.
  ELSE.
    "-- start - Changed by RANDYSY TN#51890 01/02/2014
    "-- Get From and To Date
    IF so_weeks-low IS NOT INITIAL.
      CONCATENATE so_weeks-low(6)  '01' INTO lv_from.
    ENDIF.

    IF so_weeks-high IS NOT INITIAL.
      CONCATENATE so_weeks-high(6) '01' INTO lv_to.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_to
        IMPORTING
          last_day_of_month = lv_to.
    ENDIF.

    "-- Get Header Data
    SELECT matnr werks bdzei pspel
    INTO TABLE gt_pbim_2014
    FROM   pbim
    WHERE  matnr IN so_matnr
    AND    werks IN so_werks
    AND    versb EQ pa_versb
    AND    sobkz EQ 'Q'.

    IF sy-subrc EQ 0.
      LOOP AT gt_pbim_2014 ASSIGNING <fs_pbim_2014>.
        REFRESH: lt_plnmg.

        CLEAR: lv_plnmg.

        "IF <fs_pbim_2014>-sobkz EQ 'Q'.
        SELECT pdatu perxx plnmg
          INTO TABLE lt_plnmg
        FROM   pbed
        WHERE  bdzei EQ <fs_pbim_2014>-bdzei
        AND    pdatu BETWEEN lv_from AND lv_to.
*        ELSE.
*          SELECT pdatu perxx plnmg
*            INTO TABLE lt_plnmg
*          FROM   pbed
*          WHERE  bdzei EQ <fs_pbim_2014>-bdzei
*          AND    perxx BETWEEN so_weeks-low(6) AND so_weeks-high(6).
        "ENDIF.

        LOOP AT lt_plnmg ASSIGNING <fs_plnmg>.
          <fs_pbim_2014>-pdatu = <fs_plnmg>-pdatu.
          <fs_pbim_2014>-perxx = <fs_plnmg>-perxx.
          lv_plnmg             = lv_plnmg + <fs_plnmg>-plnmg.
        ENDLOOP.

        <fs_pbim_2014>-plnmg = <fs_pbim_2014>-plnmg + lv_plnmg.
      ENDLOOP.

      DELETE gt_pbim_2014 WHERE plnmg EQ 0.
    ENDIF.
    "-- end - Changed by RANDYSY TN#51890 01/02/2014
  ENDIF.

  IF gt_pbim_2014[] IS NOT INITIAL.
    SORT gt_pbim_2014 BY matnr werks pspel perxx ASCENDING.

    CLEAR: lv_pir.

    LOOP AT gt_pbim_2014 ASSIGNING <fs_pbim_2014>.
*      IF <fs_pbim_2014>-perxx EQ ' '.
*        WRITE:/ ''.
*      ENDIF.

      CLEAR lv_week.

      IF <fs_pbim_2014>-perxx IS NOT INITIAL.
        lv_week = <fs_pbim_2014>-perxx.
      ELSE.
        lv_week = <fs_pbim_2014>-pdatu(6).
      ENDIF.

      IF lv_pir IS INITIAL.
        lv_pir-period = lv_week.
        lv_pir-matnr  = <fs_pbim_2014>-matnr.
        lv_pir-werks  = <fs_pbim_2014>-werks.
        lv_pir-plnmg  = <fs_pbim_2014>-plnmg.
        lv_pir-pspel  = <fs_pbim_2014>-pspel.
      ELSE.
        IF lv_pir-period EQ lv_week
          AND lv_pir-matnr EQ <fs_pbim_2014>-matnr
          AND lv_pir-werks EQ <fs_pbim_2014>-werks
          AND lv_pir-pspel EQ <fs_pbim_2014>-pspel.
          lv_pir-plnmg = lv_pir-plnmg + <fs_pbim_2014>-plnmg.
        ELSE.
          APPEND INITIAL LINE TO gt_pir ASSIGNING <fs_pir>.
          <fs_pir> = lv_pir.
          CLEAR lv_pir.
          lv_pir-period = lv_week.
          lv_pir-matnr  = <fs_pbim_2014>-matnr.
          lv_pir-werks  = <fs_pbim_2014>-werks.
          lv_pir-plnmg  = <fs_pbim_2014>-plnmg.
          lv_pir-pspel  = <fs_pbim_2014>-pspel.
        ENDIF.
      ENDIF.
      "WRITE:/ <fs_pbim>-matnr, <fs_pbim>-werks, <fs_pbim>-bdzei, <fs_pbim>-pdatu, <fs_pbim>-plnmg.
    ENDLOOP.
    APPEND INITIAL LINE TO gt_pir ASSIGNING <fs_pir>.
    <fs_pir> = lv_pir.
    CLEAR lv_pir.
    UNASSIGN <fs_pbim_2014>.

    SORT gt_pir BY matnr werks pspel period ASCENDING.

    LOOP AT gt_pir ASSIGNING <fs_pir>.
      CLEAR: lv_description,
             lv_unit_cost,
             lv_unit_of_measure.

      SELECT SINGLE maktx
        INTO lv_description
        FROM makt
        WHERE matnr EQ <fs_pir>-matnr.

      SELECT SINGLE verpr
        INTO lv_unit_cost
        FROM mbew
        WHERE matnr EQ <fs_pir>-matnr
          AND bwkey EQ <fs_pir>-werks.

      SELECT SINGLE meins
        INTO lv_unit_of_measure
        FROM mara
        WHERE matnr EQ <fs_pir>-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input  = lv_unit_of_measure
        IMPORTING
          output = lv_unit_of_measure.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_pir>-pspel
        IMPORTING
          output = lv_wbs.

      APPEND INITIAL LINE TO gt_final ASSIGNING <fs_final>.
      <fs_final>-matnr  = <fs_pir>-matnr.
      <fs_final>-desc   = lv_description.
      <fs_final>-werks  = <fs_pir>-werks.
      <fs_final>-ucost  = lv_unit_cost.
      <fs_final>-uom    = lv_unit_of_measure.
      <fs_final>-wbs    = lv_wbs.
      <fs_final>-period = <fs_pir>-period.
      <fs_final>-plnmg  = <fs_pir>-plnmg.

      "WRITE:/ <fs_pir>-matnr, lv_description, <fs_pir>-werks, lv_unit_cost, lv_unit_of_measure, <fs_pir>-period, <fs_pir>-plnmg.
    ENDLOOP.
    UNASSIGN <fs_pir>.

    " Clear the contents of unused internal tables
    REFRESH: gt_pbim_2014, gt_pir.
  ENDIF.
ENDFORM.                    " BEGIN_EXTRACTION_2014

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOGUE
*&---------------------------------------------------------------------*
*       Subroutine for Building Field Catalogue
*----------------------------------------------------------------------*
FORM build_catalogue.
  DATA: lv_counter    TYPE i,
        lv_field_name TYPE char10.

  "-- Reinitialize Field Catalogue Table
  REFRESH: gt_fieldcat.

  "-- Reinitialize Counter
  CLEAR: lv_counter.

  "-- Append to Field Catalogue
  mc_append_fieldcat: 'MATNR'   'PBIM'   'CHAR' 18 0 1 space,
                      'DESC'    'MAKT'   'CHAR' 40 0 2 space,
                      'WERKS'   'PBIM'   'CHAR'  4 0 3 space,
                      'UCOST'   'MBEW'   'CURR' 11 2 4 space,
                      'UOM'     'MEAN'   'UNIT'  3 0 5 space,
                      'WBS'     'PBIM'   'CHAR' 40 0 6 space.

  "-- Set Counter Variable
  lv_counter = 7.

  "-- Append dynamic field catalogue
  LOOP AT gt_period ASSIGNING <fs_period>.
    lv_field_name = <fs_period>-period.
    mc_append_fieldcat: <fs_period>-period  space 'CURR' 15 3 lv_counter lv_field_name.
    lv_counter = lv_counter + 1.
  ENDLOOP.
  UNASSIGN <fs_period>.

  "-- Append to Total Field to Field Catalogue
  mc_append_fieldcat: 'TOTAL' space 'CURR' 15  2 lv_counter space.

  "-- Reintialize Dynamic Itab
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
  mc_change_column_names: 'MATNR'   text-001 text-001 'MATNR',
                          'DESC'    text-002 text-002 'DESC',
                          'WERKS'   text-003 text-003 'WERKS',
                          'UCOST'   text-004 text-004 'UCOST',
                          'UOM'     text-005 text-005 'UOM',
                          'WBS'     text-006 text-006 'WBS'.

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

  "-- Iterate on Periods
  LOOP AT gt_period ASSIGNING <fs_period>.
    lv_field_name = <fs_period>-period.

    "-- Set Fields to be Summarized
    mc_set_summarized: lv_field_name.

    lv_long_txt = lv_field_name.
    lv_medm_txt = lv_field_name.
    lv_shrt_txt = lv_field_name.
    mc_change_column_names: lv_field_name lv_long_txt lv_medm_txt lv_shrt_txt.
  ENDLOOP.
  UNASSIGN <fs_period>.

  "-- Set Total Fields to be Summarized
  mc_set_summarized: 'TOTAL'.

  "-- Don't Show Other Fields
*  mc_set_invisible: 'BEGDA',
*                    'ORGEH',
*                    'RELID_PCL',
*                    'HRPCLX_TYPE',
*                    'MOLGA'.

  "-- Set ALV HTML Header
*  PERFORM build_header USING    pv_wage
*                                pv_start_date
*                                pv_end_date
*                       CHANGING go_content.

  go_table->set_top_of_list( go_content ).

  "-- Display ALV table
  go_table->display( ).
ENDFORM.                    " DISPLAY_REPORT

**&---------------------------------------------------------------------*
**&      Form  BUILD_HEADER
**&---------------------------------------------------------------------*
**       Build Header Data Here
**----------------------------------------------------------------------*
*FORM build_header USING    pv_wage        TYPE lgart
*                           pv_from_period TYPE begda
*                           pv_to_period   TYPE endda
*                  CHANGING po_content     TYPE REF TO cl_salv_form_element.
*
*  DATA: lr_grid        TYPE REF TO cl_salv_form_layout_grid,
*        lr_grid_1      TYPE REF TO cl_salv_form_layout_grid,
*        lr_label       TYPE REF TO cl_salv_form_label,
*        lr_text        TYPE REF TO cl_salv_form_text,
*
*        lv_text        TYPE string,
*        lv_from_period TYPE char10,
*        lv_to_period   TYPE char10,
*
*        lv_wage_text   TYPE char27. "lgtxt.
*
*  CONSTANTS:
*        co_phil        TYPE molga VALUE '48'.
*
*  "-- Create Header object
*  CREATE OBJECT lr_grid.
*
*  lv_text = text-011. "-- Remittance List
*
*  lr_grid->create_header_information( row     = 1
*                                      column  = 1
*                                      text    = lv_text
*                                      tooltip = lv_text ).
*
*  lr_grid->add_row( ).
*
*  lr_grid_1 = lr_grid->create_grid(   row     = 3
*                                      column  = 1 ).
*
*  "-- 1st Text
*  lr_label = lr_grid_1->create_label( row     = 1
*                                      column  = 1
*                                      text    = text-012 "-- For the Period of:
*                                      tooltip = text-012 ).
*
*  "-- Prepare Date Period
*  CONCATENATE pv_from_period+4(2)
*              pv_from_period+6(2)
*              pv_from_period+0(4)
*         INTO lv_from_period SEPARATED BY '/'.
*
*  CONCATENATE pv_to_period+4(2)
*              pv_to_period+6(2)
*              pv_to_period+0(4)
*         INTO lv_to_period SEPARATED BY '/'.
*
*  CONCATENATE lv_from_period 'to' lv_to_period
*         INTO lv_text SEPARATED BY space.
*
*  lr_text = lr_grid_1->create_text(   row     = 1
*                                      column  = 2
*                                      text    = lv_text
*                                      tooltip = lv_text ).
*
*  lr_label->set_label_for( lr_text ).
*
*  "-- 2nd Text
*  lr_label = lr_grid_1->create_label( row     = 2
*                                      column  = 1
*                                      text    = text-013 "-- For Wage Type:
*                                      tooltip = text-013 ).
*
*  "-- Reinitialize Wage Type Text
*  CLEAR: lv_wage_text.
*
*  "-- Get Wage Type Text
*  SELECT SINGLE lgtxt
*    INTO lv_wage_text
*  FROM   t512t
*  WHERE  sprsl EQ sy-langu
*  AND    molga EQ co_phil
*  AND    lgart EQ pv_wage.
*
*  IF sy-subrc EQ 0.
*    CONCATENATE '(' lv_wage_text ')' INTO lv_wage_text.
*    CONCATENATE pv_wage lv_wage_text INTO lv_text SEPARATED BY space.
*  ELSE.
*    lv_text = pv_wage.
*  ENDIF.
*
*  lr_text = lr_grid_1->create_text(   row     = 2
*                                      column  = 2
*                                      text    = lv_text
*                                      tooltip = lv_text ).
*
*  lr_label->set_label_for( lr_text ).
*
*  "-- 3rd Text
*  lr_label = lr_grid_1->create_label( row     = 3
*                                      column  = 1
*                                      text    = text-014 "-- For Payroll Area:
*                                      tooltip = text-014 ).
*
*  CLEAR: lv_text.
*
**  LOOP AT so_pay.
**    IF sy-tabix EQ 1.
**      CONCATENATE lv_text so_pay-low INTO lv_text.
**    ELSE.
**      CONCATENATE lv_text so_pay-low INTO lv_text SEPARATED BY ', '.
**    ENDIF.
**  ENDLOOP.
*
*  lr_text = lr_grid_1->create_text(   row     = 3
*                                      column  = 2
*                                      text    = lv_text
*                                      tooltip = lv_text ).
*
*  lr_label->set_label_for( lr_text ).
*
*  po_content = lr_grid.
*ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get row data and period data
*----------------------------------------------------------------------*
FORM get_data .
  " Loop at the final reference table
  LOOP AT gt_final ASSIGNING <fs_final>.
    " Retrieve data for each row
    APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
    <fs_output>-matnr = <fs_final>-matnr.
    <fs_output>-desc  = <fs_final>-desc.
    <fs_output>-werks = <fs_final>-werks.
    <fs_output>-ucost = <fs_final>-ucost.
    <fs_output>-uom   = <fs_final>-uom.
    <fs_output>-wbs   = <fs_final>-wbs.

    " Retrieve data for each period
    APPEND INITIAL LINE TO gt_period ASSIGNING <fs_period>.
    <fs_period>-period = <fs_final>-period.
  ENDLOOP.

  " Make sure rows are unique by MATNR and WERKS
  SORT gt_output BY matnr werks wbs ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_output COMPARING matnr werks wbs.

  " Make sure each period is unique
  SORT gt_period BY period ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_period COMPARING period.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA_TO_ALV
*&---------------------------------------------------------------------*
*       Transfer data to ALV
*----------------------------------------------------------------------*
FORM assign_data_to_alv .

  DATA: lv_field            TYPE char15,
        lv_amount           TYPE meng15,
        lv_total            TYPE meng15.

  LOOP AT gt_output ASSIGNING <fs_output>.
    "-- Generate Dynamic Work Area then assign to Field Symbol
    CREATE DATA go_structure LIKE LINE OF <ft_tab>.
    ASSIGN go_structure->* TO <fv_wa>.

    lv_field  = 'MATNR'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-matnr.

    lv_field  = 'DESC'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-desc.

    lv_field  = 'WERKS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-werks.

    lv_field  = 'UCOST'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-ucost.

    lv_field  = 'UOM'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-uom.

    lv_field  = 'WBS'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = <fs_output>-wbs.

    CLEAR lv_total.

    LOOP AT gt_period ASSIGNING <fs_period>.
      READ TABLE gt_final ASSIGNING <fs_final> WITH KEY matnr  = <fs_output>-matnr
                                                        werks  = <fs_output>-werks
                                                        wbs    = <fs_output>-wbs
                                                        period = <fs_period>-period.
      IF sy-subrc EQ 0.
        CLEAR lv_amount.

        lv_field  = <fs_period>-period.
        lv_amount = <fs_final>-plnmg.
        lv_total  = lv_total + lv_amount.
        ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
        <fv_field> = lv_amount.
      ELSE.
        lv_field = <fs_period>-period.
        ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
        <fv_field> = 0.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_period>.

    "-- Put Amount for the Field
    lv_field  = 'TOTAL'.
    ASSIGN COMPONENT lv_field OF STRUCTURE <fv_wa> TO <fv_field>.
    <fv_field> = lv_total.

    " Append to dynamic table
    APPEND <fv_wa> TO <ft_tab>.
  ENDLOOP.
  UNASSIGN <fs_output>.
ENDFORM.                    " GET_DATA