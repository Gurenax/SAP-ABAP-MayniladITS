*&---------------------------------------------------------------------*
*& Report  Y_ORDERLIST
*& DESCRIPTION: Order List for Food Take Aways
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT /JLIWAG
*& Created On : 03/01/2013
*& Reference  : N/A
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/03/01|***        |            |                                *
*&---------------------------------------------------------------------*
REPORT y_orderlist.

*INCLUDE
*TABLES:
TYPE-POOLS : vrm.
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
*TYPES: BEGIN OF ty_orderlist,
*        uname type sy-uname,
*       END OF ty_orderlist.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_orderlist     TYPE STANDARD TABLE OF zorderlist.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_orderlist TYPE zorderlist,
      gt_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat,"Field catalog
      i_selected_rows TYPE lvc_t_row,"Selected Rows
      w_selected_rows TYPE lvc_s_row,
      w_variant TYPE disvariant,
      w_modified TYPE zorderlist,
      i_modified TYPE STANDARD TABLE OF zorderlist,"For getting modified rows
      o_docking TYPE REF TO cl_gui_docking_container,"Docking Container
      o_grid TYPE REF TO cl_gui_alv_grid."Grid


*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
*CONSTANTS:

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_fieldcat> TYPE lvc_s_fcat.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_uname     TYPE sy-uname.
*select-options: so_<name> for  <data obj>.
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

  SELECT * FROM zorderlist INTO TABLE gt_orderlist.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*

MODULE status_9000 OUTPUT.

  IF o_docking IS INITIAL.
    SET PF-STATUS 'ZSTATUS'. "GUI Status
    SET TITLEBAR 'MAINTENANCE LIST'.   "Title
* Creating Docking Container and grid
    PERFORM create_object.
* Filling the fieldcatalog table
    PERFORM create_fieldcat.
* Modifying the fieldcatalog table
    PERFORM modify_fieldcat.
* Registering edit
    PERFORM register_edit.
* Displaying the output
    PERFORM display_output.
  ENDIF.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*



MODULE user_command_9000 INPUT.

  DATA lv_ucomm TYPE sy-ucomm.
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'CANCEL' OR 'EXIT'.
      PERFORM free_objects.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN 'SAVE'.
      PERFORM save_database.
      CALL METHOD o_grid->refresh_table_display.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT


*&---------------------------------------------------------------------*
*&      Form  create_object
*&---------------------------------------------------------------------*
*       Creating Docking Container and grid
*----------------------------------------------------------------------*

FORM create_object .
* Creating Docking Container
  CREATE OBJECT o_docking
    EXPORTING
      ratio = '95'.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid
      EXPORTING
        i_parent = o_docking.
  ENDIF.

ENDFORM.                    " create_object

*&---------------------------------------------------------------------*
*&      Form  create_fieldcat
*&---------------------------------------------------------------------*
*       Filling the fieldcatalog table
*----------------------------------------------------------------------*

FORM create_fieldcat .
* Filling the fieldcatalog table
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZORDERLIST'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
ENDFORM.                    " create_fieldcat


*&---------------------------------------------------------------------*
*&      Form  modify_fieldcat
*&---------------------------------------------------------------------*
*       Making the column as ediable
*----------------------------------------------------------------------*
FORM modify_fieldcat .
  LOOP AT gt_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
** Making a column as Editable


*      WHEN 'GSBER'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-003.
*        <fs_fieldcat>-col_pos = '1'.
*      WHEN 'BNAME'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-009.
*        <fs_fieldcat>-col_pos = '2'.
*      WHEN 'UNAME'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-004.
*        <fs_fieldcat>-col_pos = '3'.
*      WHEN 'ROLES'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-005.
*        <fs_fieldcat>-col_pos = '4'.
*      WHEN 'REMARKS'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-008.
*        <fs_fieldcat>-col_pos = '5'.
*      WHEN 'VALFROM'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-006.
*        <fs_fieldcat>-col_pos = '6'.
*      WHEN 'VALTO'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-007.
*        <fs_fieldcat>-col_pos = '7'.
*      WHEN 'UPDATE_TAG'.
*        <fs_fieldcat>-edit = 'X'.
*        <fs_fieldcat>-scrtext_l = text-002.
*        <fs_fieldcat>-col_pos = '8'.
*      WHEN 'UPDATE_DATE'.
*        <fs_fieldcat>-scrtext_l = text-010.
*        <fs_fieldcat>-col_pos = '9'.


    ENDCASE.
  ENDLOOP.
ENDFORM.                    " modify_fieldcat


*&---------------------------------------------------------------------*
*&      Form  register_edit
*&---------------------------------------------------------------------*
*       Registering Edit
*----------------------------------------------------------------------*

FORM register_edit .
  CALL METHOD o_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
ENDFORM.                    " register_edit

*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       Displaying the output
*----------------------------------------------------------------------*

FORM display_output .
  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_orderlist
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " display_output

*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       Free Objects
*----------------------------------------------------------------------*

FORM free_objects .
  CALL METHOD o_grid->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD o_docking->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " free_objects

*&---------------------------------------------------------------------*
*&      Form  save_database
*&---------------------------------------------------------------------*
*       Save in database
*----------------------------------------------------------------------*
FORM save_database.
* Getting the selected rows index
  CALL METHOD o_grid->get_selected_rows
    IMPORTING
      et_index_rows = i_selected_rows.

* Through the index capturing the values of selected rows
  LOOP AT i_selected_rows INTO w_selected_rows.
    READ TABLE gt_orderlist INTO gv_orderlist INDEX w_selected_rows-index.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING gv_orderlist TO w_modified.
      APPEND w_modified TO i_modified.
    ENDIF.
  ENDLOOP.
  MODIFY zorderlist FROM TABLE i_modified.
ENDFORM.                    " save_database