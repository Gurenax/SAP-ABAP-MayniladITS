*&---------------------------------------------------------------------*
*& Report  ZRISU_RATECHANGE
*& DESCRIPTION: Change Rate
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 11/27/2013
*& Reference  : <Reference object, SAP or non-SAP related>
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/11/27| GDIMALIWAT |           | Re-created from ZBDC_RATECHANGE
*&---------------------------------------------------------------------*
REPORT zrisu_ratechange MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
*TYPES: BEGIN OF ty_<name>,
*       END OF ty_<name>.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_data     TYPE  TABLE OF string.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
*DATA: gv_<name>  TYPE <datatype>.

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
PARAMETERS: pa_file     TYPE rlgrap-filename  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*AT SELECTION-SCREEN ON <para|selcrit>
*
*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'PATH'
    IMPORTING
      file_name     = pa_file.

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

  PERFORM READ_FILE.
  IF sy-subrc eq 0.

  ENDIF.


*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_FILE
*&---------------------------------------------------------------------*
*       Read the file
*----------------------------------------------------------------------*
FORM READ_FILE .
  DATA: lv_filename  TYPE string.

  lv_filename = pa_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_data
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
   IF sy-subrc <> 0.
*  Implement suitable error handling here
   ENDIF.
ENDFORM.                    " READ_FILE