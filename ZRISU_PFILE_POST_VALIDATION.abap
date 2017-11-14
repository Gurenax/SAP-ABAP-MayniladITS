*&---------------------------------------------------------------------*
*& Report  ZRISU_PFILE_POST_VALIDATION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZRISU_PFILE_POST_VALIDATION.

*INCLUDE
*TABLES:
*TYPE-POLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_payment_data,
        data TYPE string,
       END OF ty_payment_data,

       BEGIN OF ty_dfkkzk,
        keyz1       TYPE keyz1_kk,
        summs       TYPE p LENGTH 15 DECIMALS 2,
        anzpo       TYPE num06,
       END OF ty_dfkkzk,

       BEGIN OF ty_summary,
        payment_lot TYPE c LENGTH 12,
        amount      TYPE p LENGTH 15 DECIMALS 2,
        count       TYPE num06,
        amount_sap  TYPE p LENGTH 15 DECIMALS 2,
        count_sap   TYPE num06,
        status      TYPE c LENGTH 6,
       END OF ty_summary.


*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_converted_files    TYPE STANDARD TABLE OF salfldir,                  "#EC NEEDED
      gt_summary            TYPE STANDARD TABLE OF ty_summary.                "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_batch                TYPE c LENGTH 2.              "#EC NEEDED

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS : co_und      TYPE c          VALUE '_'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_converted_files> TYPE salfldir,            "#EC NEEDED
               <fs_summary>         TYPE ty_summary.          "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-100.
PARAMETERS: pa_rb_01 RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
            pa_rb_02 RADIOBUTTON GROUP grp1,
            pa_rb_03 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk01.

SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE text-101.
PARAMETERS: pa_foldr  TYPE salfile-longname DEFAULT '\\SAPREP01.mayniladsap.com\Operation\PAYMENT_FROM_SFTP\Output\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk02.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*at selection-screen on <para|selcrit>
*at selection-screen.
*
*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF pa_cb NE co_selected AND screen-group1 = 'A'.
*      screen-input = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF pa_cb2 NE co_selected AND screen-group1 = 'B'.
*      screen-input = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  CONCATENATE pa_foldr sy-datum+4(2) sy-datum+6(2) sy-datum(4) '\' INTO pa_foldr.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  IF pa_rb_01 EQ 'X'.
    gv_batch = '01'.
  ELSEIF pa_rb_02 EQ 'X'.
    gv_batch = '02'.
  ELSEIF pa_rb_03 EQ 'X'.
    gv_batch = '03'.
  ENDIF.

  PERFORM read_converted_files.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM check_payment_lot.
  PERFORM generate_summary_file.

*&---------------------------------------------------------------------*
*&      Form  READ_CONVERTED_FILES
*&---------------------------------------------------------------------*
*       Read the converted payment files
*----------------------------------------------------------------------*
FORM read_converted_files .

  DATA: lv_folder_len TYPE i,
        lv_file_len   TYPE i.

  lv_folder_len = strlen( pa_foldr ) - 1.

  IF pa_foldr+lv_folder_len(1) NE '\'.
    CONCATENATE pa_foldr '\BATCH' co_und gv_batch '\' INTO pa_foldr.
  ELSE.
    CONCATENATE pa_foldr 'BATCH' co_und gv_batch '\' INTO pa_foldr.
  ENDIF.

  REFRESH gt_converted_files.
  CLEAR gt_converted_files.
  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name               = pa_foldr
    TABLES
      file_tbl           = gt_converted_files
    EXCEPTIONS
      argument_error     = 1
      not_found          = 2
      no_admin_authority = 3.

  IF sy-subrc EQ 0.
    LOOP AT gt_converted_files ASSIGNING <fs_converted_files>.
      IF <fs_converted_files>-name(1) NE '.'.
        CONDENSE <fs_converted_files>-name.
        TRANSLATE <fs_converted_files>-name TO UPPER CASE.
        lv_file_len = strlen( <fs_converted_files>-name ) - 6.

        IF <fs_converted_files>-name+LV_FILE_LEN(4) eq '.TXT'.
          PERFORM read_file_header USING <fs_converted_files>-name.
        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_converted_files>.
  ELSE.
    WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), pa_foldr.
    RETURN.
  ENDIF.
ENDFORM.                    " READ_FILES

*&---------------------------------------------------------------------*
*&      Form  READ_FILE_HEADER
*&---------------------------------------------------------------------*
*       Read file header of the converted files
*----------------------------------------------------------------------*
*      -->PV_FILE  Converted payment file
*----------------------------------------------------------------------*
FORM READ_FILE_HEADER  USING  PV_FILE TYPE sychar200.

  DATA: lv_dataset     TYPE salfile-longname,
        ls_string      TYPE ty_payment_data,
        lt_data        TYPE TABLE OF ty_payment_data,
        lv_ctr         TYPE i,
        lv_file_length TYPE i.

  FIELD-SYMBOLS: <fs_data>  TYPE ty_payment_data.

  CONSTANTS: co_80     TYPE i VALUE 80.

  lv_file_length = strlen( pv_file ) - 2.
  CONCATENATE pa_foldr '\' pv_file INTO lv_dataset.

  OPEN DATASET lv_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc EQ 0.
    lv_ctr = 0.
    DO.
      READ DATASET lv_dataset INTO ls_string-data.
      IF sy-subrc EQ 0.                                     "#EC NEEDED
        "Do nothing
      ENDIF.
      ADD 1 TO lv_ctr.
      IF lv_ctr EQ 2.
        APPEND ls_string TO lt_data.
        lv_ctr = 0.
        EXIT. "-- Capture only the header so exit at the 2nd line
      ENDIF.
    ENDDO.
    CLOSE DATASET pv_file.

    LOOP AT lt_data ASSIGNING <fs_data>.
      APPEND INITIAL LINE TO gt_summary ASSIGNING <fs_summary>.
      <fs_summary>-payment_lot = pv_file(12).
      <fs_summary>-amount      = <fs_data>-data+210(15).
      <fs_summary>-count       = <fs_data>-data+240(6).
    ENDLOOP.
    UNASSIGN <fs_data>.
  ENDIF.

ENDFORM.                    " READ_FILE_HEADER

*&---------------------------------------------------------------------*
*&      Form  CHECK_PAYMENT_LOT
*&---------------------------------------------------------------------*
*       Check the posted payment files against the Payment Lot table
*----------------------------------------------------------------------*
FORM CHECK_PAYMENT_LOT .

  DATA: lt_dfkkzk TYPE STANDARD TABLE OF ty_dfkkzk.
  FIELD-SYMBOLS: <fs_dfkkzk> TYPE ty_dfkkzk.

  SELECT keyz1 summs anzpo
    INTO TABLE lt_dfkkzk
    FROM dfkkzk
    FOR ALL ENTRIES IN gt_summary
   WHERE keyz1 EQ gt_summary-payment_lot.


  IF sy-subrc EQ 0.

    SORT lt_dfkkzk BY keyz1.
    SORT gt_summary BY payment_lot.

    LOOP AT gt_summary ASSIGNING <fs_summary>.
      READ TABLE lt_dfkkzk ASSIGNING <fs_dfkkzk> WITH KEY keyz1 = <fs_summary>-payment_lot BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_summary>-amount_sap = <fs_dfkkzk>-summs.
        <fs_summary>-count_sap  = <fs_dfkkzk>-anzpo.

        IF <fs_summary>-amount EQ <fs_summary>-amount_sap AND <fs_summary>-count EQ <fs_summary>-count_sap.
          <fs_summary>-status = 'PASSED'.
        ELSE.
          <fs_summary>-status = 'FAILED'.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " CHECK_PAYMENT_LOT
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SUMMARY_FILE
*&---------------------------------------------------------------------*
*       Generate summary for Post Validation
*----------------------------------------------------------------------*
FORM GENERATE_SUMMARY_FILE .

  DATA: lv_dataset  TYPE salfile-longname,
        lv_string   TYPE string.

  DATA: lv_amount_str     TYPE string,
        lv_count_str      TYPE string,
        lv_amount_sap_str TYPE string,
        lv_count_sap_str  TYPE string.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  CONCATENATE pa_foldr '\POST_VALIDATION' co_xls INTO lv_dataset.
  OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    CONCATENATE 'PAYMENT LOT NAME'
                'AMOUNT IN FILE'
                'COUNT IN FILE'
                'AMOUNT IN SAP'
                'COUNT IN SAP'
                'STATUS'
           INTO lv_string SEPARATED BY co_separator.
    TRANSFER lv_string TO lv_dataset.

    LOOP AT gt_summary ASSIGNING <fs_summary>.
      lv_amount_str = <fs_summary>-AMOUNT.
      lv_amount_sap_str = <fs_summary>-AMOUNT_SAP.
      lv_count_str  = <fs_summary>-COUNT.
      lv_count_sap_str  = <fs_summary>-COUNT_SAP.

      WRITE:/ <fs_summary>-payment_lot, lv_amount_str, lv_count_str, lv_amount_sap_str, lv_count_sap_str, <fs_summary>-status.
      CONCATENATE <fs_summary>-payment_lot lv_amount_str lv_count_str lv_amount_sap_str lv_count_sap_str <fs_summary>-status
             INTO lv_string SEPARATED by co_separator.
      TRANSFER lv_string TO lv_dataset.
    ENDLOOP.
    UNASSIGN <fs_summary>.

    CLOSE DATASET lv_dataset.
  ELSE.
    WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
    RETURN.
  ENDIF.
ENDFORM.                    " GENERATE_SUMMARY_FILE