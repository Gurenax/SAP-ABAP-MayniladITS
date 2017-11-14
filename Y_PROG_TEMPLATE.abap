*&---------------------------------------------------------------------*
*& Report  <Report_name>
*& DESCRIPTION: <Description>
*&---------------------------------------------------------------------*
*& Created by : <Developer's Name>
*& Created On : <Creation Date>
*& Reference  : <Reference object, SAP or non-SAP related>
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& YYYY/MM/DD|          |           |                                  *
*&---------------------------------------------------------------------*
REPORT z<report_name> message-id <msg_id>.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_<name>,
       END OF ty_<name>.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_<itab>     TYPE STANDARD TABLE OF ty_<name>.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_<name>  TYPE <datatype>.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
*CONSTANTS:

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
*FIELD-SYMBOLS:

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_<name>     TYPE <datatype>.
select-options: so_<name> for  <data obj>.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON <para|selcrit>

AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

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
  PERFORM <form_name> TABLES lt_tab_in
                      USING  lv_fld_in CHANGING pv_fld_out.
*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  <subroutine>
*&---------------------------------------------------------------------*
*       Get Asset Records as inputted from the Selection screen
*----------------------------------------------------------------------*
*      --->PT_TAB_IN      <description>
*      --->PT_FLD_IN      <description>
*      <---PT_FLD_OUT     <description>
*----------------------------------------------------------------------*
FORM <form_name> TABLES pt_tab_in    TYPE <table type>
                 USING pv_fld_in     TYPE <data type>
                 CHANGING pv_fld_out TYPE <data type>.

ENDFORM.               "<form_name>