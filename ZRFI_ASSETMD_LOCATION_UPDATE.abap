*&---------------------------------------------------------------------*
*& Report  ZRFI_ASSETMD_LOCATION_UPDATE
*& DESCRIPTION: Massive Update of Location in Asset Master Data
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 09/24/2013
*& Reference  : TN#44264
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/09/24| GDIMALIWAT | 44264     | Created
*&---------------------------------------------------------------------*
REPORT  zrfi_assetmd_location_update.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_upload_file,
        data      TYPE string,
       END OF ty_upload_file.

TYPES: BEGIN OF ty_input,
        anln1     TYPE anlu-anln1,
        anln2     TYPE anlu-anln2,
        location  TYPE anlu-zzlocation,
       END OF ty_input.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_input  TYPE TABLE OF ty_input,
      gt_anlu   TYPE STANDARD TABLE OF anlu.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_records_updated      TYPE i,
      gv_records_not_updated  TYPE i.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_green  TYPE icon_d     VALUE '@08@',
            co_red    TYPE icon_d     VALUE '@0A@'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_input> TYPE ty_input,
                <fs_anlu>  TYPE anlu.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file     TYPE rlgrap-filename.
*select-options: so_<name> for  <data obj>.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM get_file.
*AT SELECTION-SCREEN ON <para|selcrit>
*
*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& Initialization
*&
*INITIALIZATION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM build_data.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM update_table_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data from input file
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_upload_file  TYPE TABLE OF ty_upload_file.
  DATA: lv_file         TYPE string.
  FIELD-SYMBOLS: <fs_upload_file> TYPE ty_upload_file.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pa_file
    IMPORTING
      output = lv_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
    TABLES
      data_tab                = lt_upload_file
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc EQ 0.
    LOOP AT lt_upload_file ASSIGNING <fs_upload_file>.
      APPEND INITIAL LINE TO gt_input ASSIGNING <fs_input>.
      SPLIT <fs_upload_file>-data AT '|' INTO <fs_input>-anln1
                                              <fs_input>-anln2
                                              <fs_input>-location.
    ENDLOOP.
    UNASSIGN <fs_upload_file>.

    SORT gt_input BY anln1 anln2.
  ELSE.
    WRITE:/ text-001.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_DATA
*&---------------------------------------------------------------------*
*       Build table data for modification
*----------------------------------------------------------------------*
FORM build_data .

  DATA: lv_anlu TYPE anlu.

  LOOP AT gt_input ASSIGNING <fs_input>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_input>-anln1
      IMPORTING
        output = <fs_input>-anln1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_input>-anln2
      IMPORTING
        output = <fs_input>-anln2.

    SELECT SINGLE *
    INTO lv_anlu
    FROM anlu
    WHERE bukrs EQ 'MWSI'
      AND anln1 EQ <fs_input>-anln1
      AND anln2 EQ <fs_input>-anln2.

    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO gt_anlu ASSIGNING <fs_anlu>.
      <fs_anlu> = lv_anlu.
      <fs_anlu>-zzlocation = <fs_input>-location.
      ADD 1 TO gv_records_updated.
    ELSE.
      ADD 1 TO gv_records_not_updated.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_input>.

ENDFORM.                    " GET_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_DATA
*&---------------------------------------------------------------------*
*       Update the ANLU table
*----------------------------------------------------------------------*
FORM update_table_data .

  DATA: lo_sy_open_sql TYPE REF TO cx_sy_open_sql_db,
        lv_text        TYPE string.

  TRY.
      MODIFY anlu FROM TABLE gt_anlu.
      WRITE:/ text-003.
      WRITE:/ co_green, gv_records_updated, text-004.
      WRITE:/ co_red, gv_records_not_updated, text-005.
    CATCH cx_sy_open_sql_db INTO lo_sy_open_sql.
      lv_text = lo_sy_open_sql->get_text( ).
      WRITE:/ text-002, lv_text.
  ENDTRY.
ENDFORM.                    " UPDATE_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       Open file dialog box
*----------------------------------------------------------------------*
FORM get_file .
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = pa_file.
ENDFORM.                    " GET_FILE