*&---------------------------------------------------------------------*
*& Report  ZRFI_ASSETMD_EVGRP5_UPDATE
*& DESCRIPTION: Massive Update of Evaluation Group 5 in Asset Master Data
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 01/08/2014
*& Reference  : TN#53399
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/01/08| GDIMALIWAT | 53399     | Created
*&---------------------------------------------------------------------*
REPORT  zrfi_assetmd_evgrp5_update.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_upload_file,
        data      TYPE string,
       END OF ty_upload_file,

       BEGIN OF ty_input,
        anln1     TYPE anla-anln1,
        evgrp5    TYPE anla-gdlgrp,
       END OF ty_input,

       BEGIN OF ty_msgs,
         msg      TYPE string,
       END OF ty_msgs.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_input    TYPE TABLE OF ty_input,
      gt_anla     TYPE STANDARD TABLE OF anla,
      gt_anlb     TYPE STANDARD TABLE OF anlb,
      gt_msgs     TYPE TABLE OF ty_msgs,
      gt_bdcdata  TYPE tab_bdcdata.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_records_updated      TYPE i,
      gv_records_not_updated  TYPE i,
      gv_records_bdc_errors   TYPE i.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_green  TYPE icon_d     VALUE '@08@',
            co_red    TYPE icon_d     VALUE '@0A@',
            co_tcode  TYPE sy-tcode   VALUE `AS02`,
            co_flag   TYPE c LENGTH 1 VALUE `X`.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_input>    TYPE ty_input,
                <fs_anla>     TYPE anla,
                <fs_anlb>     TYPE anlb,
                <fs_msgs>     TYPE ty_msgs,
                <fs_bdc>      TYPE bdcdata,
                <fs_messages> TYPE bdcmsgcoll.

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

  IF sy-subrc EQ 0.
    pv_message = 0.
  ELSEIF sy-subrc EQ 1001.
    pv_message = 1001.
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
                                              <fs_input>-evgrp5.
    ENDLOOP.
    UNASSIGN <fs_upload_file>.

    SORT gt_input BY anln1 evgrp5.
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

  DATA: lv_anla     TYPE anla,
        lv_anlb     TYPE anlb,
        lv_evgrp5   TYPE anla-gdlgrp.

  LOOP AT gt_input ASSIGNING <fs_input>.
    CLEAR: lv_anla,
           lv_anlb,
           lv_evgrp5.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_input>-anln1
      IMPORTING
        output = <fs_input>-anln1.

    "-- Initialize output message table
    APPEND INITIAL LINE TO gt_msgs ASSIGNING <fs_msgs>.

    "-- Check if Asset number is valid (ANLA) - Retrieve ANLA
    SELECT SINGLE *
    INTO lv_anla
    FROM anla
    WHERE bukrs EQ 'MWSI'
      AND anln1 EQ <fs_input>-anln1.

    IF sy-subrc EQ 0.

      "-- Check if Asset number is valid (ANLB) - Retrieve ANLB
      SELECT SINGLE *
      INTO lv_anlb
      FROM anlb
      WHERE bukrs EQ 'MWSI'
        AND anln1 EQ <fs_input>-anln1.

      IF sy-subrc EQ 0.
        "-- Check if Evaluation Group 5 is valid
        SELECT SINGLE gdlgrp
        INTO lv_evgrp5
        FROM t087g
        WHERE gdlgrp EQ <fs_input>-evgrp5.

        IF sy-subrc EQ 0.
          "-- Evaluation Group 5 Value
          APPEND INITIAL LINE TO gt_anla ASSIGNING <fs_anla>.
          <fs_anla> = lv_anla.
          <fs_anla>-gdlgrp = <fs_input>-evgrp5.

          "-- Tick Indicator for Negative Values Allowed
          APPEND INITIAL LINE TO gt_anlb ASSIGNING <fs_anlb>.
          <fs_anlb> = lv_anlb.
          <fs_anlb>-xnega = 'X'.

          CONCATENATE co_green <fs_input>-anln1 <fs_input>-evgrp5 text-006 INTO <fs_msgs>-msg SEPARATED BY space.
          ADD 1 TO gv_records_updated.
        ELSE.
          CONCATENATE co_red <fs_input>-anln1 <fs_input>-evgrp5 text-009 INTO <fs_msgs>-msg SEPARATED BY space.
          ADD 1 TO gv_records_not_updated.
        ENDIF.
      ELSE.
        CONCATENATE co_red <fs_input>-anln1 <fs_input>-evgrp5 text-008 INTO <fs_msgs>-msg SEPARATED BY space.
        ADD 1 TO gv_records_not_updated.
      ENDIF.
    ELSE.
      CONCATENATE co_red <fs_input>-anln1 <fs_input>-evgrp5 text-007 INTO <fs_msgs>-msg SEPARATED BY space.
      ADD 1 TO gv_records_not_updated.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_input>.

ENDFORM.                    " GET_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_DATA
*&---------------------------------------------------------------------*
*       Update the ANLA table
*----------------------------------------------------------------------*
FORM update_table_data .

  DATA: lo_sy_open_sql TYPE REF TO cx_sy_open_sql_db,
        lv_text        TYPE string,
        lv_status      TYPE natxt.

  TRY.
      MODIFY anla FROM TABLE gt_anla.
      MODIFY anlb FROM TABLE gt_anlb.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        LOOP AT gt_msgs ASSIGNING <fs_msgs>.
          WRITE:/ <fs_msgs>-msg.
        ENDLOOP.
        UNASSIGN <fs_msgs>.
        SKIP 1.
        ULINE (100).
        LOOP AT gt_input ASSIGNING <fs_input>.
          mc_bdc_dynpro:  co_flag 'SAPLAIST'              '0100',
                          space   'BDC_CURSOR'            'ANLA-ANLN1',
                          space   'BDC_OKCODE'            '/00',
                          space   'ANLA-ANLN1'            <fs_input>-anln1,           "-- Asset No.
                          space   'ANLA-ANLN2'            '0',                        "-- Asset Subnumber
                          space   'ANLA-BUKRS'            'MWSI',                     "-- Company Code - MWSI
                          co_flag 'SAPLAIST'              '1000',
                          space   'BDC_OKCODE'            '=BUCH'.

          "-- Perform Transaction AS02
          PERFORM call_transaction USING    co_tcode
                                   CHANGING lv_status.                  "-- Update Upload Status

          IF lv_status eq 0.
            WRITE:/ co_green, text-011, <fs_input>-anln1.
          ELSEIF lv_status eq 1001.
            WRITE:/ co_red, text-012, <fs_input>-anln1.
            ADD 1 to gv_records_bdc_errors.
          ELSE.
            WRITE:/ co_red, text-013, <fs_input>-anln1, text-014, lv_status.
            ADD 1 to gv_records_bdc_errors.
          ENDIF.
        ENDLOOP.
        UNASSIGN <fs_input>.
        SKIP 1.
        ULINE (100).
        WRITE:/ text-003.
        WRITE:/ gv_records_updated, text-004.
        WRITE:/ gv_records_not_updated, text-005.
        WRITE:/ gv_records_bdc_errors, text-015.
      ELSE.
        WRITE:/ co_red, text-010.
      ENDIF.
    CATCH cx_sy_open_sql_db INTO lo_sy_open_sql.
      lv_text = lo_sy_open_sql->get_text( ).
      WRITE:/ co_red, text-002, lv_text.
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