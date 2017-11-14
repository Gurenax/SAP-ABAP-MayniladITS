*&---------------------------------------------------------------------*
*& Report  ZFPP2_RSG_UPDATE
*& DESCRIPTION: Massive Updating of RSG per Business Partner
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 01/07/2014
*& Reference  : TN#53264
*&---------------------------------------------------------------------*
*& Date      | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/01/07| GDIMALIWAT | 53264      | Created
*&---------------------------------------------------------------------*
REPORT zfpp2_rsg_update MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_input,
        gpart TYPE c LENGTH 10,
        rsg   TYPE c LENGTH 8,
       END OF ty_input.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_input     TYPE TABLE OF ty_input.                  "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
"DATA: gv_<name>  TYPE <datatype>.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS: co_green  TYPE icon_d     VALUE '@08@',
           co_red    TYPE icon_d     VALUE '@0A@'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_input> TYPE ty_input.                    "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file     TYPE rlgrap-filename.
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
*INITIALIZATION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM read_input_file.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM update_rsg.

*&---------------------------------------------------------------------*
*&      Form  READ_INPUT_FILE
*&---------------------------------------------------------------------*
*       Read input file from server path
*----------------------------------------------------------------------*
FORM read_input_file .
  DATA: lv_string TYPE string.

  OPEN DATASET pa_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET pa_file INTO lv_string.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO gt_input ASSIGNING <fs_input>.
        <fs_input>-gpart = lv_string(10).
        <fs_input>-rsg   = lv_string+10(8).
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLOSE DATASET pa_file.
  ELSE.
    WRITE:/ text-001. "--Unable to open input file
  ENDIF.
ENDFORM.                    " READ_INPUT_FILE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_RSG
*&---------------------------------------------------------------------*
*       Update the RSG per Business Partner based on input data
*----------------------------------------------------------------------*
FORM update_rsg .

  DATA: lv_gpart                TYPE bapibus1006_head-bpartner,
        lr_address_data         TYPE bapibus1006_address,
        lr_address_data_x       TYPE bapibus1006_address_x,
        lr_address_data_check   TYPE bapibus1006_address.

  DATA: lt_return               TYPE TABLE OF bapiret2.

  FIELD-SYMBOLS: <fs_return>    TYPE bapiret2.

  LOOP AT gt_input ASSIGNING <fs_input>.
    REFRESH lt_return.
    CLEAR: lv_gpart,
           lr_address_data,
           lr_address_data_x,
           lr_address_data_check,
           lt_return.

    "-- Mark the fields to be changed
    lr_address_data_x-regiogroup = 'X'.

    "-- Assign the Business Partner
    lv_gpart = <fs_input>-gpart.

    CALL FUNCTION 'BAPI_BUPA_EXISTENCE_CHECK'
      EXPORTING
        businesspartner = lv_gpart
      TABLES
        return          = lt_return.

    READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.

    IF sy-subrc EQ 0 AND ( <fs_return>-type EQ 'E' OR <fs_return>-type EQ 'W' OR <fs_return>-type EQ 'A' ).
      WRITE:/ co_red, lv_gpart, <fs_return>-message.
    ELSE.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
        EXPORTING
          businesspartner = lv_gpart
        IMPORTING
          addressdata     = lr_address_data.

      "-- Change to new RSG
      lr_address_data-regiogroup = <fs_input>-rsg.

      CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
        EXPORTING
          businesspartner = lv_gpart
          addressdata     = lr_address_data
          addressdata_x   = lr_address_data_x.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
        EXPORTING
          businesspartner = lv_gpart
        IMPORTING
          addressdata     = lr_address_data_check.

      IF lr_address_data-regiogroup EQ lr_address_data_check-regiogroup.
        WRITE:/ co_green, lv_gpart, text-002, lr_address_data-regiogroup, text-004, lr_address_data_check-regiogroup.
      ELSE.
        WRITE:/ co_red, lv_gpart,   text-003, lr_address_data-regiogroup, text-004, lr_address_data_check-regiogroup.
      ENDIF.
    ENDIF.

    UNASSIGN <fs_return>.
  ENDLOOP.
  UNASSIGN <fs_input>.
ENDFORM.                    " UPDATE_RSG