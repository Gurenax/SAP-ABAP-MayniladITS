*&---------------------------------------------------------------------*
*& Report  ZRISU_PFILE_PREPARATION
*& DESCRIPTION: Automatic payment file preparation, validation and reports generation
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 07/23/2012
*& Reference  : TN#15864
*&---------------------------------------------------------------------*
*& Date       | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2012/07/23 | GDIMALIWAT | 15864      | Created
*& 2012/10/12 | GDIMALIWAT | 20992      | Changed Default Parameters
*& 2012/10/18 | GDIMALIWAT | 21489      | Added Company Code Checking For Amayi
*& 2013/01/04 | GDIMALIWAT | 24570      | Fixes for Document Date
*& 2013/02/20 | GDIMALIWAT | 28526      | Additional Handling of payments with 0 amount
*& 2015/07/14 | GDIMALIWAT | RFC#254    | Automation of Payment File Preparation
*& 2015/08/17 | JAUSTRIA   | RFC#254    | Automation of Payment File Preparation
*& 2015/09/29 | GDIMALIWAT | RFC#254    | Automation of Payment File Preparation - Revisions
*& 2015/11/03 | GDIMALIWAT | RFC#254    | Automation of Payment File Preparation - Added Validations/Exceptions
*& 2015/11/04 | GDIMALIWAT | RFC#254    | Automation of Payment File Preparation - Change in Folder Name for Batch 03
*& 2015/11/23 | GDIMALIWAT | RFC#254    | Automation of Payment File Preparation - Additional Fixes
*& 2016/03/02 | GDIMALIWAT | RFC#       | Fixes for Exception File
*&---------------------------------------------------------------------*
REPORT zrisu_pfile_preparation MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_payment_data,
        data TYPE string,
       END OF ty_payment_data,

       BEGIN OF ty_pfile_table1,
         gsber     TYPE fkkvkp-gsber,
         can       TYPE fkkvkp-vkont,
         pamount   TYPE char16,
         transdate TYPE char8,
         paycenter TYPE char50,
         opbuk     TYPE fkkvkp-opbuk,
         line_no   TYPE string,
         filename  TYPE string,
       END OF ty_pfile_table1,

       BEGIN OF ty_can,
         vkont     TYPE fkkvkp-vkont,
       END OF ty_can,

       BEGIN OF ty_vkont,
         vkont     TYPE fkkvkp-vkont,
         gsber     TYPE fkkvkp-gsber,
         opbuk     TYPE fkkvkp-opbuk,
       END OF ty_vkont,

       BEGIN OF ty_pfile_table1_paycenter,
         paycenter TYPE char50,
         gsber     TYPE fkkvkp-gsber,
         can       TYPE fkkvkp-vkont,
         pamount   TYPE char16,
         transdate TYPE char8,
         line_no   TYPE string,
         filename  TYPE string,
       END OF ty_pfile_table1_paycenter,

       BEGIN OF ty_pfile_table1_can,
         can       TYPE fkkvkp-vkont,
         gsber     TYPE fkkvkp-gsber,
         paycenter TYPE char50,
         pamount   TYPE char16,
         transdate TYPE char8,
         line_no   TYPE string,
         filename  TYPE string,
       END OF ty_pfile_table1_can,

       BEGIN OF ty_pfile_table2_line2,
        text1  TYPE c LENGTH 31,
        text2  TYPE c LENGTH 2,
        text3  TYPE c LENGTH 6,
        text4  TYPE c LENGTH 44,
        text5  TYPE c LENGTH 2,
        text6  TYPE c LENGTH 6,
        text7  TYPE c LENGTH 4,
        text8  TYPE c LENGTH 14,
        text9  TYPE c LENGTH 4,
        text10 TYPE c LENGTH 17,
        text11 TYPE c LENGTH 8,
        text12 TYPE c LENGTH 8,
        text13 TYPE c LENGTH 9,
        text14 TYPE c LENGTH 55,
        text15 TYPE c LENGTH 15,
        text16 TYPE c LENGTH 15,
        text17 TYPE c LENGTH 6,
       END OF ty_pfile_table2_line2,

       BEGIN OF ty_pfile_table2_line3,
        text1  TYPE c LENGTH 31,
        text2  TYPE c LENGTH 3,
        text3  TYPE c LENGTH 8,
        text4  TYPE c LENGTH 97,
        text5  TYPE c LENGTH 15,
        text6  TYPE c LENGTH 33,
        text7  TYPE c LENGTH 14,
        text8  TYPE c LENGTH 4,
        text9  TYPE c LENGTH 17,
        text10 TYPE c LENGTH 8,
        text11 TYPE c LENGTH 8,
        text12 TYPE c LENGTH 9,
        text13 TYPE c LENGTH 208,
        text14 TYPE c LENGTH 50,
       END OF ty_pfile_table2_line3,

       BEGIN OF ty_error_files,
        error_msg TYPE string,
       END OF ty_error_files.
*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_payment_files            TYPE STANDARD TABLE OF salfldir,                  "#EC NEEDED
      gt_pfile_table1             TYPE STANDARD TABLE OF ty_pfile_table1,           "#EC NEEDED
      gt_pfile_table1_paycenter   TYPE STANDARD TABLE OF ty_pfile_table1_paycenter, "#EC NEEDED
      gt_pfile_table1_can         TYPE STANDARD TABLE OF ty_pfile_table1_can,       "#EC NEEDED
      gt_error_files              TYPE STANDARD TABLE OF ty_error_files.            "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_payment_file_normal  TYPE salfile-longname,        "#EC NEEDED
      gv_payment_file_bpi     TYPE salfile-longname,        "#EC NEEDED
      gs_pfile_table1         TYPE ty_pfile_table1,         "#EC NEEDED
      gs_pfile_table1_can     TYPE ty_pfile_table1_can,     "#EC NEEDED
      gv_batch                TYPE c LENGTH 2,              "#EC NEEDED
      gv_line                 TYPE string,                  "#EC NEEDED
      gv_filename             TYPE string,                  "#EC NEEDED
      gv_exception            TYPE REF TO cx_root,          "#EC NEEDED
      gv_exception_msg        TYPE string.                  "#EC NEEDED

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS : co_und      TYPE c          VALUE '_',  "+RFC#254
            co_bp       TYPE c LENGTH 2 VALUE 'BP',
            co_rp       TYPE c LENGTH 2 VALUE 'RP'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_pfile_table1>  TYPE ty_pfile_table1,     "#EC NEEDED
               <fs_payment_files> TYPE salfldir,            "#EC NEEDED
               <fs_error_files>   TYPE ty_error_files.      "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-100.
PARAMETERS: pa_rb_01 RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
            pa_rb_02 RADIOBUTTON GROUP grp1,
            pa_rb_03 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk01.

SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE text-101.
PARAMETERS: pa_input  TYPE salfile-longname DEFAULT '\\SAPREP01.mayniladsap.com\Operation\PAYMENT_FROM_SFTP\Input\' OBLIGATORY,
            pa_outpt  TYPE salfile-longname DEFAULT '\\SAPREP01.mayniladsap.com\Operation\PAYMENT_FROM_SFTP\Output\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk02.

SELECTION-SCREEN BEGIN OF BLOCK blk03 WITH FRAME TITLE text-102.
PARAMETERS: pa_cb     AS CHECKBOX DEFAULT '' USER-COMMAND rusr.
SELECTION-SCREEN END OF BLOCK blk03.

SELECTION-SCREEN BEGIN OF BLOCK blk04 WITH FRAME TITLE text-103.
PARAMETERS:  pa_date   TYPE sy-datum MODIF ID a.
SELECTION-SCREEN END OF BLOCK blk04.

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
  CONCATENATE pa_input sy-datum+4(2) sy-datum+6(2) sy-datum(4) '\' INTO pa_input.
  CONCATENATE pa_outpt sy-datum+4(2) sy-datum+6(2) sy-datum(4) '\' INTO pa_outpt.

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

  PERFORM read_files.
  PERFORM generate_final_pfile.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM generate_report_payment_center.
  PERFORM generate_report_ba.
  PERFORM generate_report_can_exception.
  PERFORM generate_payment_report.
  PERFORM generate_report_file_exception.
  PERFORM generate_report_all_valid.

*&---------------------------------------------------------------------*
*&      Form  READ_FILES
*&---------------------------------------------------------------------*
*       Read the raw payment files
*----------------------------------------------------------------------*
FORM read_files .

  DATA: lv_folder_len TYPE i.

  lv_folder_len = strlen( pa_input ) - 1.
  "-- Start RFC 254 - GDIMALIWAT - Added BATCH Folder
  IF pa_input+lv_folder_len(1) NE '\'.
*    CONCATENATE pa_input '\BATCH' space pa_num '\' INTO pa_input.  "-RFC#254
    CONCATENATE pa_input '\BATCH' co_und gv_batch '\' INTO pa_input.  "+RFC#254
  ELSE.
*    CONCATENATE pa_input 'BATCH' space pa_num '\' INTO pa_input.  "-RFC#254
    CONCATENATE pa_input 'BATCH' co_und gv_batch '\' INTO pa_input.  "+RFC#254
  ENDIF.

  IF gv_batch EQ '01'.
    CONCATENATE pa_input 'Payment_Files_Bayad_Center' INTO gv_payment_file_normal.
  ELSE.
    CONCATENATE pa_input 'Payment_Files' INTO gv_payment_file_normal.
  ENDIF.
  "-- End RFC 254 - GDIMALIWAT - Added Batch Folder
  CONCATENATE pa_input 'BPI' INTO gv_payment_file_bpi.

  REFRESH gt_payment_files.
  CLEAR gt_payment_files.
  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name               = gv_payment_file_normal
    TABLES
      file_tbl           = gt_payment_files
    EXCEPTIONS
      argument_error     = 1
      not_found          = 2
      no_admin_authority = 3.

  IF sy-subrc EQ 0.
    LOOP AT gt_payment_files ASSIGNING <fs_payment_files>.
      IF <fs_payment_files>-name(1) NE '.'.
        CONDENSE <fs_payment_files>-name.
        TRANSLATE <fs_payment_files>-name TO UPPER CASE.
        "lv_file_len = strlen( <fs_payment_files>-name ) - 6.

*        IF <fs_payment_files>-name+LV_FILE_LEN(4) eq '.TXT'.
        PERFORM process_normal USING <fs_payment_files>-name.
*        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_payment_files>.
  ELSE.
    APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
    CONCATENATE 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104) gv_payment_file_normal INTO <fs_error_files>-error_msg SEPARATED BY space.
    WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), gv_payment_file_normal.
    RETURN.
  ENDIF.

  IF gv_batch EQ '02'.
    REFRESH gt_payment_files.
    CLEAR gt_payment_files.
    CALL FUNCTION 'RZL_READ_DIR_LOCAL'
      EXPORTING
        name               = gv_payment_file_bpi
      TABLES
        file_tbl           = gt_payment_files
      EXCEPTIONS
        argument_error     = 1
        not_found          = 2
        no_admin_authority = 3.

    IF sy-subrc EQ 0.
      LOOP AT gt_payment_files ASSIGNING <fs_payment_files>.
        IF <fs_payment_files>-name(1) NE '.'.
          CONDENSE <fs_payment_files>-name.
          TRANSLATE <fs_payment_files>-name TO UPPER CASE.
          PERFORM process_bpi USING <fs_payment_files>-name.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_payment_files>.
    ELSE.
      APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
      CONCATENATE 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104) gv_payment_file_bpi INTO <fs_error_files>-error_msg SEPARATED BY space.
      WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), gv_payment_file_bpi.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_FILES

*&---------------------------------------------------------------------*
*&      Form  PROCESS_NORMAL
*&---------------------------------------------------------------------*
*       Process the normal payment files
*----------------------------------------------------------------------*
*      --->PV_FILE      Normal Payment File
*----------------------------------------------------------------------*
FORM process_normal USING pv_file TYPE sychar200.
  DATA: lv_dataset     TYPE salfile-longname,
        ls_string      TYPE ty_payment_data,
        lt_data        TYPE TABLE OF ty_payment_data,
        lv_length      TYPE i,
        lv_file_length TYPE i,
        lv_transdate   TYPE char8,
        lv_amount      TYPE p DECIMALS 2.
  DATA: lo_matcher     TYPE REF TO cl_abap_matcher,
        lv_decimal     TYPE p DECIMALS 2.

  DATA: lt_vkont       TYPE TABLE OF ty_vkont,
        lt_can         TYPE TABLE OF ty_can.

  DATA: lv_can         TYPE fkkvkp-vkont,
        lv_deleted     TYPE i VALUE 0.

  FIELD-SYMBOLS: <fs_data>  TYPE ty_payment_data,
                 <fs_vkont> TYPE ty_vkont,
                 <fs_can>   TYPE ty_can.

  CONSTANTS: co_80     TYPE i VALUE 80.

  lv_file_length = strlen( pv_file ) - 2.
  CONCATENATE gv_payment_file_normal '\' pv_file INTO lv_dataset.

  PERFORM ux2dos  USING lv_dataset.
  OPEN DATASET lv_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc EQ 0.
    DO.
      READ DATASET lv_dataset INTO ls_string-data.
      IF sy-subrc EQ 0.                                     "#EC NEEDED
        "Do nothing
      ENDIF.
      APPEND ls_string TO lt_data.
      IF sy-subrc <> 0. EXIT. ENDIF.
    ENDDO.
    CLOSE DATASET pv_file.

    lv_deleted = 0.
    LOOP AT lt_data ASSIGNING <fs_data> WHERE data NE space.
      gv_line = sy-tabix + lv_deleted. "-- Add number of deleted lines to keep up with the file's line number

      TRY.
          CLEAR: lv_can.
          lv_length = strlen( <fs_data>-data ).
          IF sy-tabix > 1.
            IF lv_length EQ co_80.
              lv_can = <fs_data>-data(11).
            ELSE.
              lv_can = <fs_data>-data+34(30).
            ENDIF.
            CONDENSE lv_can.
            SHIFT lv_can RIGHT DELETING TRAILING space.
            OVERLAY lv_can WITH '000000000000'.

            "-- Retrieve all Contract Account Numbers from Data String
            APPEND INITIAL LINE TO lt_can ASSIGNING <fs_can>.
            <fs_can>-vkont = lv_can.
          ENDIF.
        CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
          gv_exception_msg = gv_exception->get_text( ).
          APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
          CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
          WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, ':', gv_exception_msg.
          "-- Delete line with error from list of Data
          DELETE lt_data INDEX sy-tabix.
          ADD 1 TO lv_deleted.
      ENDTRY.
    ENDLOOP.
    UNASSIGN <fs_data>.

    SELECT vkont gsber opbuk
    INTO TABLE lt_vkont
    FROM fkkvkp
    FOR ALL ENTRIES IN lt_can
    WHERE vkont EQ lt_can-vkont.

    IF sy-subrc EQ 0.
      SORT lt_vkont BY vkont ASCENDING.
    ENDIF.

    lv_deleted = 0.
    LOOP AT lt_data ASSIGNING <fs_data> WHERE data NE space.
      CLEAR: gs_pfile_table1, gs_pfile_table1_can.
      gv_line = sy-tabix + lv_deleted. "-- Add number of deleted lines to keep up with the file's line number

      TRY.
          lv_length = strlen( <fs_data>-data ).
          IF sy-tabix > 1.
            lv_length = strlen( <fs_data>-data ).
            IF lv_length EQ co_80.
*  OLD
              gs_pfile_table1-can = <fs_data>-data(11).
              CONDENSE gs_pfile_table1-can.
              SHIFT gs_pfile_table1-can RIGHT DELETING TRAILING space.
              OVERLAY gs_pfile_table1-can WITH '000000000000'.

              READ TABLE lt_vkont ASSIGNING <fs_vkont> WITH KEY vkont = gs_pfile_table1-can BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_pfile_table1-gsber = <fs_vkont>-gsber.
                gs_pfile_table1-opbuk = <fs_vkont>-opbuk.

                CONCATENATE '20' <fs_data>-data+33(2) <fs_data>-data+29(4) INTO lv_transdate.

                lv_amount = <fs_data>-data+15(9) / 100.
                gs_pfile_table1-pamount = lv_amount.
                gs_pfile_table1-transdate = lv_transdate.
                gs_pfile_table1-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1-pamount.
                CONDENSE gs_pfile_table1-transdate.
                CONDENSE gs_pfile_table1-paycenter.
                CONDENSE gs_pfile_table1-gsber.

                MOVE gs_pfile_table1-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1-pamount INTO gs_pfile_table1-pamount.
                  MOVE gs_pfile_table1-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1-line_no  = gv_line.
                gs_pfile_table1-filename = pv_file.

                APPEND gs_pfile_table1 TO gt_pfile_table1.
              ELSE.
                gs_pfile_table1_can-can   = gs_pfile_table1-can.
                gs_pfile_table1_can-gsber = '0000'.

                CONCATENATE '20' <fs_data>-data+33(2) <fs_data>-data+29(4) INTO lv_transdate.

                lv_amount = <fs_data>-data+15(9) / 100.
                gs_pfile_table1_can-pamount = lv_amount.
                gs_pfile_table1_can-transdate = lv_transdate.
                gs_pfile_table1_can-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1_can-pamount.
                CONDENSE gs_pfile_table1_can-transdate.
                CONDENSE gs_pfile_table1_can-paycenter.
                CONDENSE gs_pfile_table1_can-gsber.

                MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1_can-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1_can-pamount INTO gs_pfile_table1_can-pamount.
                  MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1_can-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1_can-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1_can-line_no  = gv_line.
                gs_pfile_table1_can-filename = pv_file.

                APPEND gs_pfile_table1_can TO gt_pfile_table1_can.

                APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
                gv_exception_msg = ': The contract account number is invalid. Please see CAN exception file.'.
                CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
                WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, gv_exception_msg.
                "-- No need to delete line if CAN exception
              ENDIF.

            ELSEIF lv_length GT co_80.
*  NEW
              gs_pfile_table1-can = <fs_data>-data+34(30).
              CONDENSE gs_pfile_table1-can.
              SHIFT gs_pfile_table1-can RIGHT DELETING TRAILING space.
              OVERLAY gs_pfile_table1-can WITH '000000000000'.

              READ TABLE lt_vkont ASSIGNING <fs_vkont> WITH KEY vkont = gs_pfile_table1-can BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_pfile_table1-gsber = <fs_vkont>-gsber.
                gs_pfile_table1-opbuk = <fs_vkont>-opbuk.

                gs_pfile_table1-pamount = <fs_data>-data+123(16).
                gs_pfile_table1-transdate = <fs_data>-data+215(8).
                gs_pfile_table1-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1-pamount.
                CONDENSE gs_pfile_table1-transdate.
                CONDENSE gs_pfile_table1-paycenter.
                CONDENSE gs_pfile_table1-gsber.

                MOVE gs_pfile_table1-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1-pamount INTO gs_pfile_table1-pamount.
                  MOVE gs_pfile_table1-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1-line_no  = gv_line.
                gs_pfile_table1-filename = pv_file.

                APPEND gs_pfile_table1 TO gt_pfile_table1.
              ELSE.
                gs_pfile_table1_can-can   = gs_pfile_table1-can.
                gs_pfile_table1_can-gsber = '0000'.

                gs_pfile_table1_can-pamount = <fs_data>-data+123(16).
                gs_pfile_table1_can-transdate = <fs_data>-data+215(8).
                gs_pfile_table1_can-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1_can-pamount.
                CONDENSE gs_pfile_table1_can-transdate.
                CONDENSE gs_pfile_table1_can-paycenter.
                CONDENSE gs_pfile_table1_can-gsber.

                MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1_can-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1_can-pamount INTO gs_pfile_table1_can-pamount.
                  MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1_can-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1_can-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1_can-line_no  = gv_line.
                gs_pfile_table1_can-filename = pv_file.

                APPEND gs_pfile_table1_can TO gt_pfile_table1_can.

                APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
                gv_exception_msg = ': The contract account number is invalid. Please see CAN exception file.'.
                CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
                WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, gv_exception_msg.
                "-- No need to delete line if CAN exception
              ENDIF.

            ELSE.
              APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
              gv_exception_msg = ': The length of the line is invalid.'.
              CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
              WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, gv_exception_msg.
              "-- Delete line with error from list of Data
              DELETE lt_data INDEX sy-tabix.
              ADD 1 TO lv_deleted.
            ENDIF.
          ENDIF.
        CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
          gv_exception_msg = gv_exception->get_text( ).
          APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
          CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
          WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, ':', gv_exception_msg.
          "-- Delete line with error from list of Data
          DELETE lt_data INDEX sy-tabix.
          ADD 1 TO lv_deleted.
      ENDTRY.

    ENDLOOP.
    UNASSIGN <fs_data>.
  ELSE.
    APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
    CONCATENATE 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104) lv_dataset INTO <fs_error_files>-error_msg SEPARATED BY space.
    WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
    RETURN.
  ENDIF.
ENDFORM.               "PROCESS_NORMAL

*&---------------------------------------------------------------------*
*&      Form  PROCESS_BPI
*&---------------------------------------------------------------------*
*       Process the BPI payment files
*----------------------------------------------------------------------*
*      --->PV_FILE      BPI Payment File
*----------------------------------------------------------------------*
FORM process_bpi USING pv_file  TYPE sychar200.
  DATA: lv_dataset     TYPE salfile-longname,
        ls_string      TYPE ty_payment_data,
        lt_data        TYPE TABLE OF ty_payment_data,
        lv_length      TYPE i,
        lv_file_length TYPE i,
        lv_transdate   TYPE char8,
        lv_amount      TYPE p DECIMALS 2.
  DATA: lo_matcher     TYPE REF TO cl_abap_matcher,
        lv_decimal     TYPE p DECIMALS 2.

  DATA: lt_vkont TYPE TABLE OF ty_vkont,
        lt_can   TYPE TABLE OF ty_can.

  DATA: lv_can      TYPE fkkvkp-vkont,
        lv_deleted  TYPE i VALUE 0.

  FIELD-SYMBOLS: <fs_data>  TYPE ty_payment_data,
                 <fs_vkont> TYPE ty_vkont,
                 <fs_can>   TYPE ty_can.

  CONSTANTS: co_190    TYPE i VALUE 190.

  lv_file_length = strlen( pv_file ) - 2.
  CONCATENATE gv_payment_file_bpi '\' pv_file INTO lv_dataset.
  OPEN DATASET lv_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET lv_dataset INTO ls_string-data.
      IF sy-subrc EQ 0.                                     "#EC NEEDED
        "Do nothing
      ENDIF.
      APPEND ls_string TO lt_data.
      IF sy-subrc <> 0. EXIT. ENDIF.
    ENDDO.
    CLOSE DATASET pv_file.

    lv_deleted = 0.
    LOOP AT lt_data ASSIGNING <fs_data> WHERE data NE space.
      gv_line = sy-tabix + lv_deleted. "-- Add number of deleted lines to keep up with the file's line number
      TRY.
          CLEAR: lv_can.
          lv_can = <fs_data>-data+37(8).
          CONDENSE lv_can.
          SHIFT lv_can RIGHT DELETING TRAILING space.
          OVERLAY lv_can WITH '000000000000'.

          "-- Retrieve all Contract Account Numbers from Data String
          APPEND INITIAL LINE TO lt_can ASSIGNING <fs_can>.
          <fs_can>-vkont = lv_can.
      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
          gv_exception_msg = gv_exception->get_text( ).
          APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
          CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
          WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, ':', gv_exception_msg.
          "-- Delete line with error from list of Data
          DELETE lt_data INDEX sy-tabix.
          ADD 1 TO lv_deleted.
      ENDTRY.
    ENDLOOP.
    UNASSIGN <fs_data>.

    SELECT vkont gsber opbuk
    INTO TABLE lt_vkont
    FROM fkkvkp
    FOR ALL ENTRIES IN lt_can
    WHERE vkont EQ lt_can-vkont.

    IF sy-subrc EQ 0.
      SORT lt_vkont BY vkont ASCENDING.
    ENDIF.

    lv_deleted = 0.
    LOOP AT lt_data ASSIGNING <fs_data> WHERE data NE space.
      CLEAR: gs_pfile_table1, gs_pfile_table1_can.
      gv_line = sy-tabix + lv_deleted. "-- Add number of deleted lines to keep up with the file's line number
      TRY.
          lv_length = strlen( <fs_data>-data ).
          IF lv_length GT co_190.
            IF <fs_data>-data(1) EQ '2'.

              gs_pfile_table1-can = <fs_data>-data+37(8).
              CONDENSE gs_pfile_table1-can.
              SHIFT gs_pfile_table1-can RIGHT DELETING TRAILING space.
              OVERLAY gs_pfile_table1-can WITH '000000000000'.

              READ TABLE lt_vkont ASSIGNING <fs_vkont> WITH KEY vkont = gs_pfile_table1-can BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_pfile_table1-gsber = <fs_vkont>-gsber.

                MOVE <fs_data>-data+163(8) TO lv_transdate.

                lv_amount = <fs_data>-data+128(9) / 100.
                gs_pfile_table1-pamount = lv_amount.
                gs_pfile_table1-transdate = lv_transdate.
                gs_pfile_table1-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1-pamount.
                CONDENSE gs_pfile_table1-transdate.
                CONDENSE gs_pfile_table1-paycenter.
                CONDENSE gs_pfile_table1-gsber.

                MOVE gs_pfile_table1-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1-pamount INTO gs_pfile_table1-pamount.
                  MOVE gs_pfile_table1-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1-line_no  = gv_line.
                gs_pfile_table1-filename = pv_file.

                APPEND gs_pfile_table1 TO gt_pfile_table1.
              ELSE.
                gs_pfile_table1_can-can   = gs_pfile_table1-can.
                gs_pfile_table1_can-gsber = '0000'.

                MOVE <fs_data>-data+163(8) TO lv_transdate.

                lv_amount = <fs_data>-data+128(9) / 100.
                gs_pfile_table1_can-pamount = lv_amount.
                gs_pfile_table1_can-transdate = lv_transdate.
                gs_pfile_table1_can-paycenter = pv_file(lv_file_length).
                CONDENSE gs_pfile_table1_can-pamount.
                CONDENSE gs_pfile_table1_can-transdate.
                CONDENSE gs_pfile_table1_can-paycenter.
                CONDENSE gs_pfile_table1_can-gsber.

                MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
                lo_matcher = cl_abap_matcher=>create(
                pattern = `^(\.\d+)$`
                text    = gs_pfile_table1_can-pamount ).
                IF lo_matcher->match( ) IS NOT INITIAL.
                  CONCATENATE '0' gs_pfile_table1_can-pamount INTO gs_pfile_table1_can-pamount.
                  MOVE gs_pfile_table1_can-pamount TO lv_decimal.
                  IF lv_decimal LE 0.
                    gs_pfile_table1_can-gsber = '0000'.
                  ENDIF.
                ELSEIF lv_decimal <= 0.
                  gs_pfile_table1_can-gsber = '0000'.
                ENDIF.
                "-- End

                "-- Add line number / filename
                gs_pfile_table1_can-line_no  = gv_line.
                gs_pfile_table1_can-filename = pv_file.

                APPEND gs_pfile_table1_can TO gt_pfile_table1_can.

                APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
                gv_exception_msg = ': The contract account number is invalid. Please see CAN exception file.'.
                CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
                WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, gv_exception_msg.
                "-- No need to delete line if CAN exception
              ENDIF.
            ENDIF.
          ELSE.
            APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
            gv_exception_msg = ': The length of the line is invalid.'.
            CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
            WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, gv_exception_msg.
            "-- Delete line with error from list of Data
            DELETE lt_data INDEX sy-tabix.
            ADD 1 TO lv_deleted.
          ENDIF.
        CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
          gv_exception_msg = gv_exception->get_text( ).
          APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
          CONCATENATE 'File Content Error on'(105) pv_file 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
          WRITE:/ 'File Content Error on'(105), pv_file, 'on Line'(107), gv_line, ':', gv_exception_msg.
          "-- Delete line with error from list of Data
          DELETE lt_data INDEX sy-tabix.
          ADD 1 TO lv_deleted.
      ENDTRY.
    ENDLOOP.
    UNASSIGN <fs_data>.
  ELSE.
    APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
    CONCATENATE 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104) lv_dataset INTO <fs_error_files>-error_msg SEPARATED BY space.
    WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
    RETURN.
  ENDIF.
ENDFORM.               "PROCESS_BPI
*&---------------------------------------------------------------------*
*&      Form  GENERATE_FINAL_PFILE
*&---------------------------------------------------------------------*
*       Generate the Final Payment File
*----------------------------------------------------------------------*
FORM generate_final_pfile .
  DATA: lv_gsber            TYPE fkkvkp-gsber,
        lv_folder_len       TYPE i,
        lv_dataset          TYPE salfile-longname,
        ls_line2            TYPE ty_pfile_table2_line2,
        ls_line3            TYPE ty_pfile_table2_line3,
        lv_can_count        TYPE i,
        lv_total_amt        TYPE p DECIMALS 2,
        lv_can              TYPE char12,
        lv_output           TYPE salfile-longname,
        lv_subrc            TYPE sysubrc VALUE 4,
        lv_deleted          TYPE i VALUE 0.

  FIELD-SYMBOLS: <fs_ba> TYPE ty_pfile_table1.

  lv_folder_len = strlen( pa_outpt ) - 1.

  "-- Start RFC 254 - GDIMALIWAT - Added Batch Folder
  IF pa_outpt+lv_folder_len(1) NE '\'.
*  CONCATENATE pa_outpt '\BATCH' space pa_num '\' INTO pa_outpt.  "-RFC#254
    CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\' INTO lv_output.  "+RFC#254
  ELSE.
*  CONCATENATE pa_outpt 'BATCH' space pa_num '\' INTO pa_outpt.  "-RFC#254
    CONCATENATE pa_outpt 'BATCH' co_und gv_batch '\' INTO lv_output.  "+RFC#254
  ENDIF.
  "-- End RFC 254 - GDIMALIWAT - Added Batch Folder

  DESCRIBE TABLE gt_pfile_table1.
  IF sy-subrc EQ 0.
    IF sy-tfill GT 0.
      WRITE:/ text-000.
    ENDIF.
  ENDIF.

  SORT gt_pfile_table1 BY gsber can ASCENDING.

  lv_deleted = 0.
  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    gv_line = <fs_pfile_table1>-line_no.
    gv_filename = <fs_pfile_table1>-filename.
    TRY.
        AT NEW gsber.
          IF <fs_pfile_table1>-gsber NE '0000'.
            lv_can_count = 0.
            lv_total_amt = 0.
            LOOP AT gt_pfile_table1 ASSIGNING <fs_ba> WHERE gsber EQ <fs_pfile_table1>-gsber.
              ADD 1 TO lv_can_count.
              lv_total_amt = lv_total_amt + <fs_ba>-pamount.
            ENDLOOP.

            lv_gsber = <fs_pfile_table1>-gsber.
            IF pa_date IS INITIAL.
              CONCATENATE lv_output '\' lv_gsber(2) sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) INTO lv_dataset.
            ELSE.
              CONCATENATE lv_output '\' lv_gsber(2)  pa_date+4(2)  pa_date+6(2)  pa_date+2(2) INTO lv_dataset.
            ENDIF.

            IF pa_cb IS INITIAL.
              CONCATENATE lv_dataset co_bp gv_batch '.TXT' INTO lv_dataset.
            ELSE.
              CONCATENATE lv_dataset co_rp gv_batch '.TXT' INTO lv_dataset.
            ENDIF.

            CLEAR: lv_subrc.
            OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
            IF sy-subrc EQ 0.
              lv_subrc = sy-subrc.

              TRANSFER '0500R' TO lv_dataset.
              ls_line2-text1 = '1BFKKZK'.
              ls_line2-text2 = lv_gsber(2).

              IF pa_date IS INITIAL.
                CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) INTO ls_line2-text3.
              ELSE.
                CONCATENATE pa_date+4(2) pa_date+6(2) pa_date+2(2) INTO ls_line2-text3.
              ENDIF.

              IF pa_cb IS INITIAL.
                CONCATENATE co_bp gv_batch INTO ls_line2-text4.
              ELSE.
                CONCATENATE co_rp gv_batch INTO ls_line2-text4.
              ENDIF.

              ls_line2-text5 = lv_gsber(2).
              IF pa_date IS INITIAL.
                CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) INTO ls_line2-text6.
              ELSE.
                CONCATENATE pa_date+4(2) pa_date+6(2) pa_date+2(2) INTO ls_line2-text6.
              ENDIF.

              IF pa_cb IS INITIAL.
                CONCATENATE co_bp gv_batch INTO ls_line2-text7.
              ELSE.
                CONCATENATE co_rp gv_batch INTO ls_line2-text7.
              ENDIF.

              CONCATENATE '9100700000' <fs_pfile_table1>-opbuk INTO ls_line2-text8.
              ls_line2-text9 = lv_gsber.
              ls_line2-text10 = 'BAPHP'.
              IF pa_date IS INITIAL.
                ls_line2-text11 = sy-datum(8).
              ELSE.
                ls_line2-text11 = pa_date(8).
              ENDIF.
              ls_line2-text12 = <fs_pfile_table1>-transdate.
              IF pa_date IS INITIAL.
                ls_line2-text13 = sy-datum(8).
              ELSE.
                ls_line2-text13 = pa_date(8).
              ENDIF.
              ls_line2-text14 = gv_batch.
              ls_line2-text15 = lv_total_amt.
              SHIFT ls_line2-text15 RIGHT DELETING TRAILING space.
              OVERLAY ls_line2-text15 WITH '               '.
              ls_line2-text16 = ''.
              ls_line2-text17 = lv_can_count.
              SHIFT ls_line2-text17 RIGHT DELETING TRAILING space.
              OVERLAY ls_line2-text17 WITH '000000'.
              TRANSFER ls_line2 TO lv_dataset.
            ELSE.
              WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
            ENDIF.
          ELSE.
            lv_subrc = 4.
          ENDIF.
        ENDAT.

        IF lv_subrc EQ 0.
          ls_line3-text1  = '2BFKKZP'.
          ls_line3-text2  = 'K'.
          lv_can          = <fs_pfile_table1>-can.
          SHIFT lv_can LEFT DELETING LEADING '0'.
          ls_line3-text3  = lv_can.
          ls_line3-text4  = ''.
          ls_line3-text5  = <fs_pfile_table1>-pamount.
          SHIFT ls_line3-text5 RIGHT DELETING TRAILING space.
          OVERLAY ls_line3-text5 WITH '               '.
          ls_line3-text6  = ''.
          CONCATENATE '9100700000' <fs_pfile_table1>-opbuk INTO ls_line3-text7.
          ls_line3-text8  = <fs_pfile_table1>-gsber.
          ls_line3-text9  = 'BAPHP'.
          IF pa_date IS INITIAL.
            ls_line3-text10 = sy-datum(8).
          ELSE.
            ls_line3-text10 = pa_date(8).
          ENDIF.
          ls_line3-text11 = <fs_pfile_table1>-transdate.
          IF pa_date IS INITIAL.
            ls_line3-text12 = sy-datum(8).
          ELSE.
            ls_line3-text12 = pa_date(8).
          ENDIF.
          ls_line3-text13 = gv_batch.
          ls_line3-text14 = <fs_pfile_table1>-paycenter.
          TRANSFER ls_line3 TO lv_dataset.
        ENDIF.

        AT END OF gsber.
          IF lv_subrc EQ 0.
            CLOSE DATASET lv_dataset.
            WRITE:/ lv_dataset.
          ENDIF.
        ENDAT.

      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
        gv_exception_msg = gv_exception->get_text( ).
        APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
        CONCATENATE 'File Content Error on'(105) gv_filename 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
        WRITE:/ 'File Content Error on'(105), gv_filename, 'on Line'(107), gv_line, ':', gv_exception_msg.
        "-- Delete line with error from list of Data
        DELETE gt_pfile_table1 INDEX sy-tabix.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <fs_pfile_table1>.
ENDFORM.                    " GENERATE_FINAL_PFILE
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_PAYMENT_CENTER
*&---------------------------------------------------------------------*
*       Generate the Payment File Report per Payment Center
*----------------------------------------------------------------------*
FORM generate_report_payment_center .
  DATA: ls_paycenter                  TYPE ty_pfile_table1_paycenter,
        lv_dataset                    TYPE salfile-longname,
        "lv_file_length                TYPE i,
        lv_paycenter_cnt              TYPE i,
        lv_total_amt                  TYPE p DECIMALS 2,
        lv_paycenter_cnt_str          TYPE string,
        lv_total_amt_str              TYPE string,
        lv_paycenter_cnt_valid        TYPE i,
        lv_total_amt_valid            TYPE p DECIMALS 2,
        lv_paycenter_cnt_valid_str    TYPE string,
        lv_total_amt_valid_str        TYPE string,
        lv_paycenter_cnt_invalid      TYPE i,
        lv_total_amt_invalid          TYPE p DECIMALS 2,
        lv_paycenter_cnt_invalid_str  TYPE string,
        lv_total_amt_invalid_str      TYPE string,
        lv_string                     TYPE string,
        lv_subrc                      TYPE sysubrc VALUE 4.

  FIELD-SYMBOLS: <fs_paycenter> TYPE ty_pfile_table1_paycenter,
                 <fs_pc>        TYPE ty_pfile_table1_paycenter,
                 <fs_vkont>     TYPE ty_pfile_table1_can.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    ls_paycenter-paycenter = <fs_pfile_table1>-paycenter.
    ls_paycenter-gsber = <fs_pfile_table1>-gsber.
    ls_paycenter-can = <fs_pfile_table1>-can.
    ls_paycenter-pamount = <fs_pfile_table1>-pamount.
    ls_paycenter-transdate = <fs_pfile_table1>-transdate.
    ls_paycenter-line_no = <fs_pfile_table1>-line_no.
    ls_paycenter-filename = <fs_pfile_table1>-filename.
    APPEND ls_paycenter TO gt_pfile_table1_paycenter.
  ENDLOOP.

  LOOP AT GT_PFILE_TABLE1_can ASSIGNING <fs_vkont>.
    ls_paycenter-paycenter = <fs_vkont>-paycenter.
    ls_paycenter-gsber = <fs_vkont>-gsber.
    ls_paycenter-can = <fs_vkont>-can.
    ls_paycenter-pamount = <fs_vkont>-pamount.
    ls_paycenter-transdate = <fs_vkont>-transdate.
    ls_paycenter-line_no = <fs_vkont>-line_no.
    ls_paycenter-filename = <fs_vkont>-filename.
    APPEND ls_paycenter TO gt_pfile_table1_paycenter.
  ENDLOOP.

  SORT gt_pfile_table1_paycenter BY paycenter ASCENDING.

  LOOP AT gt_pfile_table1_paycenter ASSIGNING <fs_paycenter>.
    gv_line = <fs_paycenter>-line_no.
    gv_filename = <fs_paycenter>-filename.
    TRY.
        AT FIRST.
          CLEAR: lv_subrc.
          "-- Start RFC 254 - GDIMALIWAT - Added Batch Folder
          CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\REPORT_PAYMENT_CENTER' co_xls INTO lv_dataset.
          "-- End RFC 254 - GDIMALIWAT - Added Batch Folder
          OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            lv_subrc = sy-subrc.
            CONCATENATE text-001
                        text-002
                        text-003
                        text-004
                        text-005
                        text-006
                        text-007
                        INTO lv_string
                        SEPARATED BY co_separator.
            TRANSFER lv_string TO lv_dataset.
          ELSE.
            WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
          ENDIF.
        ENDAT.
        AT NEW paycenter.
          IF lv_subrc EQ 0.
            lv_paycenter_cnt = 0.
            lv_paycenter_cnt_valid = 0.
            lv_paycenter_cnt_invalid = 0.
            lv_total_amt = 0.
            lv_total_amt_valid = 0.
            lv_total_amt_invalid = 0.
            LOOP AT gt_pfile_table1_paycenter ASSIGNING <fs_pc> WHERE paycenter EQ <fs_paycenter>-paycenter.
              ADD 1 TO lv_paycenter_cnt.
              lv_total_amt = lv_total_amt + <fs_pc>-pamount.

              IF <fs_pc>-gsber NE '0000'.
                ADD 1 TO lv_paycenter_cnt_valid.
                lv_total_amt_valid = lv_total_amt_valid + <fs_pc>-pamount.
              ELSE.
                ADD 1 TO lv_paycenter_cnt_invalid.
                lv_total_amt_invalid = lv_total_amt_invalid + <fs_pc>-pamount.
              ENDIF.

            ENDLOOP.

            lv_paycenter_cnt_str = lv_paycenter_cnt.
            lv_total_amt_str = lv_total_amt.
            lv_paycenter_cnt_valid_str = lv_paycenter_cnt_valid.
            lv_total_amt_valid_str = lv_total_amt_valid.
            lv_paycenter_cnt_invalid_str = lv_paycenter_cnt_invalid.
            lv_total_amt_invalid_str = lv_total_amt_invalid.

            CONCATENATE <fs_paycenter>-paycenter
                        lv_paycenter_cnt_str
                        lv_total_amt_str
                        lv_paycenter_cnt_valid_str
                        lv_total_amt_valid_str
                        lv_paycenter_cnt_invalid_str
                        lv_total_amt_invalid_str
                        INTO lv_string
                        SEPARATED BY co_separator.
            TRANSFER lv_string TO lv_dataset.
          ENDIF.
        ENDAT.
      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
        gv_exception_msg = gv_exception->get_text( ).
        APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
        CONCATENATE 'File Content Error on'(105) gv_filename 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
        WRITE:/ 'File Content Error on'(105), gv_filename, 'on Line'(107), gv_line, ':', gv_exception_msg.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <fs_paycenter>.
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
  ENDIF.
ENDFORM.                    " GENERATE_REPORT_PAYMENT_CENTER
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_BA
*&---------------------------------------------------------------------*
*       Generate the Payment File Report per Business Area
*----------------------------------------------------------------------*
FORM generate_report_ba .
  DATA: lv_dataset        TYPE salfile-longname,
        lv_can_cnt        TYPE i,
        lv_total_amt      TYPE p DECIMALS 2,
        lv_can_cnt_str    TYPE string,
        lv_total_amt_str  TYPE string,
        lv_string         TYPE string,
        lv_subrc          TYPE sysubrc VALUE 4.

  FIELD-SYMBOLS: <fs_ba> TYPE ty_pfile_table1.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  SORT gt_pfile_table1 BY gsber ASCENDING.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    gv_line = <fs_pfile_table1>-line_no.
    gv_filename = <fs_pfile_table1>-filename.
    TRY.
        AT FIRST.
          CLEAR: lv_subrc.
          "-- Start RFC 254 - GDIMALIWAT - Added Batch Folder
          CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\REPORT_BUSINESS_AREA' co_xls INTO lv_dataset.
          "-- End RFC 254 - GDIMALIWAT - Added Batch Folder
          OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            lv_subrc = sy-subrc.
            CONCATENATE text-008
                        text-004
                        text-005
                        INTO lv_string
                        SEPARATED BY co_separator.
            TRANSFER lv_string TO lv_dataset.
          ELSE.
            WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
          ENDIF.
        ENDAT.

        AT NEW gsber.
          IF lv_subrc EQ 0.
            lv_can_cnt = 0.
            lv_total_amt = 0.
            IF <fs_pfile_table1>-gsber NE '0000'.
              LOOP AT gt_pfile_table1 ASSIGNING <fs_ba> WHERE gsber EQ <fs_pfile_table1>-gsber.
                ADD 1 TO lv_can_cnt.
                lv_total_amt = lv_total_amt + <fs_ba>-pamount.
              ENDLOOP.

              lv_can_cnt_str = lv_can_cnt.
              lv_total_amt_str = lv_total_amt.

              OVERLAY <fs_pfile_table1>-gsber WITH '0000'.
              CONCATENATE <fs_pfile_table1>-gsber
                          lv_can_cnt_str
                          lv_total_amt_str
                          INTO lv_string
                          SEPARATED BY co_separator.
              TRANSFER lv_string TO lv_dataset.
            ENDIF.
          ENDIF.
        ENDAT.
      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
        gv_exception_msg = gv_exception->get_text( ).
        APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
        CONCATENATE 'File Content Error on'(105) gv_filename 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
        WRITE:/ 'File Content Error on'(105), gv_filename, 'on Line'(107), gv_line, ':', gv_exception_msg.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <fs_pfile_table1>.
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
  ENDIF.

ENDFORM.                    " GENERATE_REPORT_BA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_CAN_EXCEPTION
*&---------------------------------------------------------------------*
*       Generate the CAN Exception Report
*----------------------------------------------------------------------*
FORM generate_report_can_exception .
  DATA: lv_dataset            TYPE salfile-longname,
        lv_total_amt          TYPE p DECIMALS 2,
        "lv_can_cnt_str        TYPE string,
        lv_total_amt_str      TYPE string,
        lv_string             TYPE string,
        ls_can                TYPE ty_pfile_table1_can,
        lv_subrc              TYPE sysubrc VALUE 4.

  FIELD-SYMBOLS: <fs_vkont> TYPE ty_pfile_table1_can,
                 <fs_can>   TYPE ty_pfile_table1_can.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

*  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
*    ls_can-can = <fs_pfile_table1>-can.
*    ls_can-gsber = <fs_pfile_table1>-gsber.
*    ls_can-paycenter = <fs_pfile_table1>-paycenter.
*    ls_can-pamount = <fs_pfile_table1>-pamount.
*    ls_can-transdate = <fs_pfile_table1>-transdate.
*    ls_can-line_no = <fs_pfile_table1>-line_no.
*    ls_can-filename = <fs_pfile_table1>-filename.
*    APPEND ls_can TO gt_pfile_table1_can.
*  ENDLOOP.

  SORT gt_pfile_table1_can BY filename line_no can ASCENDING.

  DELETE gt_pfile_table1_can WHERE can EQ '000000000000'.
  IF sy-subrc EQ 0. "#EC NEEDED
    "-- Do nothing
  ENDIF.
  LOOP AT gt_pfile_table1_can ASSIGNING <fs_vkont>.
    gv_line = <fs_vkont>-line_no.
    gv_filename = <fs_vkont>-filename.
    TRY.
        AT FIRST.
          CLEAR: lv_subrc.
          "-- Start RFC 254 - GDIMALIWAT - Added Batch Folder
          CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\CAN_EXCEPTION' co_xls INTO lv_dataset.
          "-- End RFC 254 - GDIMALIWAT - Added Batch Folder
          OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            lv_subrc = sy-subrc.
            CONCATENATE text-009
                        text-007
                        text-010
                        INTO lv_string
                        SEPARATED BY co_separator.
            TRANSFER lv_string TO lv_dataset.
          ELSE.
            WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104).
            RETURN.
          ENDIF.
        ENDAT.

        AT NEW can.
          IF lv_subrc EQ 0.
            lv_total_amt = 0.

            IF <fs_vkont>-gsber EQ '0000'.
              LOOP AT gt_pfile_table1_can ASSIGNING <fs_can> WHERE can EQ <fs_vkont>-can.
                lv_total_amt = lv_total_amt + <fs_can>-pamount.
              ENDLOOP.

              lv_total_amt_str = lv_total_amt.

              CONCATENATE <fs_vkont>-can
                          lv_total_amt_str
                          <fs_vkont>-paycenter
                          INTO lv_string
                          SEPARATED BY co_separator.
              TRANSFER lv_string TO lv_dataset.
            ENDIF.
          ENDIF.
        ENDAT.
      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
        gv_exception_msg = gv_exception->get_text( ).
        APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
        CONCATENATE 'File Content Error on'(105) gv_filename 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
        WRITE:/ 'File Content Error on'(105), gv_filename, 'on Line'(107), gv_line, ':', gv_exception_msg.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <fs_vkont>.
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
  ENDIF.
ENDFORM.                    " GENERATE_REPORT_CAN_EXCEPTION

*&---------------------------------------------------------------------*
*&      Form  GENERATE_PAYMENT_REPORT
*&---------------------------------------------------------------------*
*       Generate payment report file
*----------------------------------------------------------------------*
FORM generate_payment_report .
  DATA: lv_dataset        TYPE salfile-longname,
        lv_string         TYPE string,
        lv_subrc          TYPE sysubrc VALUE 4.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  SORT gt_pfile_table1 BY can ASCENDING.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    gv_line = <fs_pfile_table1>-line_no.
    gv_filename = <fs_pfile_table1>-filename.
    TRY.
        AT FIRST.
          CLEAR: lv_subrc.
          CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\PAYMENT_REPORT' co_xls INTO lv_dataset.
          OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc EQ 0.
            lv_subrc = sy-subrc.
            CONCATENATE text-009
                        text-011
                        text-012
                        INTO lv_string
                        SEPARATED BY co_separator.
            TRANSFER lv_string TO lv_dataset.
          ELSE.
            WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
          ENDIF.
        ENDAT.

        IF lv_subrc EQ 0.
          CONCATENATE <fs_pfile_table1>-can
                      <fs_pfile_table1>-transdate
                      <fs_pfile_table1>-pamount
                      INTO lv_string
                      SEPARATED BY co_separator.
          TRANSFER lv_string TO lv_dataset.
        ENDIF.
      CATCH cx_root INTO gv_exception. "#EC CATCH_ALL
        gv_exception_msg = gv_exception->get_text( ).
        APPEND INITIAL LINE TO gt_error_files ASSIGNING <fs_error_files>.
        CONCATENATE 'File Content Error on'(105) gv_filename 'on Line'(107) gv_line ':' gv_exception_msg INTO <fs_error_files>-error_msg SEPARATED BY space.
        WRITE:/ 'File Content Error on'(105), gv_filename, 'on Line'(107), gv_line, ':', gv_exception_msg.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <fs_pfile_table1>.
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
  ENDIF.
ENDFORM.                    " GENERATE_PAYMENT_REPORT

*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_FILE_EXCEPTION
*&---------------------------------------------------------------------*
*       Generate the File Exception Report
*----------------------------------------------------------------------*
FORM generate_report_file_exception .
  DATA: lv_dataset            TYPE salfile-longname,
        lv_string             TYPE string,
        lv_subrc              TYPE sysubrc VALUE 4.

  CONSTANTS:  co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  LOOP AT gt_error_files ASSIGNING <fs_error_files>.
    AT FIRST.
      CLEAR: lv_subrc.
      CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\FILE_EXCEPTION' co_xls INTO lv_dataset.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.
         lv_subrc = sy-subrc.
*        lv_string = text-106.
*        TRANSFER lv_string TO lv_dataset.
      ELSE.
        WRITE:/ 'Please check your folder structure or folder authorization. Also, please close any previously opened file.'(104), lv_dataset.
      ENDIF.
    ENDAT.

    lv_string = <fs_error_files>-error_msg.
    TRANSFER lv_string TO lv_dataset.
  ENDLOOP.
  UNASSIGN <fs_error_files>.
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
  ENDIF.
ENDFORM.                    " GENERATE_REPORT_FILE_EXCEPTION

*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_ALL_VALID
*&---------------------------------------------------------------------*
*       All valid payments
*----------------------------------------------------------------------*
FORM GENERATE_REPORT_ALL_VALID .

  DATA: lv_dataset        TYPE salfile-longname,
        lv_folder         TYPE BTCH0000-TEXT80,
        lv_string         TYPE string,
        lv_subrc          TYPE sysubrc VALUE 4,
        lv_dataset_closed TYPE c LENGTH 1 VALUE abap_true.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  SORT gt_pfile_table1 BY gsber can ASCENDING.

  DATA: lv_com TYPE rlgrap-filename.

  CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\ALL_VALID' INTO lv_folder.
  CALL FUNCTION 'PFL_CHECK_DIRECTORY'
    EXPORTING
      directory         = lv_folder
    EXCEPTIONS
      pfl_dir_not_exist = 1.
  IF sy-subrc = 1.
     CONCATENATE 'cmd /c mkdir' lv_folder INTO lv_com SEPARATED BY space.
     CALL 'SYSTEM' ID 'COMMAND' FIELD lv_com.
  ENDIF.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    AT NEW gsber.
      IF sy-tabix NE 1.
        CLOSE DATASET lv_dataset.
        lv_dataset_closed = abap_true.
      ENDIF.

      CLEAR: lv_subrc.
      OVERLAY <fs_pfile_table1>-gsber WITH '0000'.
      CONCATENATE pa_outpt '\BATCH' co_und gv_batch '\ALL_VALID\ALL_VALID_PAYMENTS_' <fs_pfile_table1>-gsber  co_xls INTO lv_dataset.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.
        lv_subrc = sy-subrc.
        lv_dataset_closed = abap_false.

        CONCATENATE 'Branch Code'(008)
                    'CAN'(009)
                    'Amount'(012)
                    'Posting Date'(011)
                    'Paycenter/Filename'(001)
                    'Line No'
                    INTO lv_string
                    SEPARATED BY co_separator.
        TRANSFER lv_string TO lv_dataset.
      ENDIF.
    ENDAT.

    IF lv_subrc EQ 0.
      OVERLAY <fs_pfile_table1>-gsber WITH '0000'.
      CONCATENATE <fs_pfile_table1>-gsber
                  <fs_pfile_table1>-can
                  <fs_pfile_table1>-pamount
                  <fs_pfile_table1>-transdate
                  <fs_pfile_table1>-paycenter
                  <fs_pfile_table1>-line_no
                  INTO lv_string
                  SEPARATED BY co_separator.
      TRANSFER lv_string TO lv_dataset.
    ENDIF.

  ENDLOOP.
  UNASSIGN <fs_pfile_table1> .
  IF lv_subrc EQ 0.
    CLOSE DATASET lv_dataset.
    lv_dataset_closed = abap_true.
  ENDIF.
ENDFORM.                    " GENERATE_REPORT_ALL_VALID


*&---------------------------------------------------------------------*
*&      Form  UX2DOS
*&---------------------------------------------------------------------*
*      Converting a UX File to DOS Format (CRLF)
*----------------------------------------------------------------------*
*      -->PV_FILENAME  Input File
*----------------------------------------------------------------------*
FORM ux2dos USING pv_filename           TYPE csequence.
  DATA: lv_xcr                TYPE xstring,
        "iret                  TYPE i,
        lv_str_filename       TYPE string,
        lv_xout               TYPE xstring,
        lv_xline              TYPE xstring,
        lv_xchar              TYPE x,
        lv_actlen             TYPE i,
        lv_pos                TYPE i,
        lv_pos_prev           TYPE i,
        lv_ncount             TYPE n LENGTH 10.
  FIELD-SYMBOLS: <fs_x_in>   TYPE xstring,
                 <fs_x_out>  TYPE xstring,
                 <fs_x_prv>  TYPE x.

  lv_xcr =  '0D0A'.

  lv_str_filename = pv_filename.
  ASSIGN  lv_xout TO <fs_x_out> .
  TRY.
      OPEN DATASET lv_str_filename FOR INPUT IN BINARY MODE .
      IF sy-subrc EQ 0.                                     "#EC NEEDED
        "Do nothing
      ENDIF.
    CATCH cx_sy_file_authority .
*          write: 'UX2DOS NO ACCESS TO File [', lv_str_filename, ']'.
      RETURN.
  ENDTRY.
  IF sy-subrc <> 0.
*    ULINE.
*    write: 'UX2DOS File [', lv_str_filename, '] could not be opened.'.
    RETURN.
  ENDIF.
  "Read full file into buffer and get actlen = filelen
  READ DATASET lv_str_filename INTO lv_xline ACTUAL LENGTH lv_actlen.
  IF sy-subrc EQ 0.
    CLOSE DATASET lv_str_filename.
  ENDIF.

* Reopen same file binary mode
  OPEN DATASET lv_str_filename FOR OUTPUT IN BINARY MODE .
  IF sy-subrc EQ 0.
    lv_pos = -1. "because pos is incremented right after do - and we need a ZERO.
    DO lv_actlen TIMES.
      ADD 1 TO   lv_pos.
      ASSIGN lv_xline TO  <fs_x_in> .
      IF <fs_x_in> IS NOT ASSIGNED.
        EXIT.
      ENDIF.
      lv_pos_prev = lv_pos - 1.
      IF lv_pos_prev >= 0.
        ASSIGN  lv_xchar TO <fs_x_prv>.
        <fs_x_prv> = <fs_x_in>+lv_pos_prev.
      ENDIF.
*   Check if substitution is neccessary at all
*      if <lfs_x_in>+pos(1) = '0A' AND <lfs_x_prv> <> '0D'.
*      if <lfs_x_in>+pos(1) = '0A' AND <lfs_x_prv> = '0D'.
      TRY.
          IF <fs_x_in>+lv_pos(2) = '0D0A'.
          ELSEIF <fs_x_in>+lv_pos(1) = '0D'.
            TRANSFER lv_xcr TO lv_str_filename LENGTH 2.
            ADD 1 TO lv_ncount.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_range_out_of_bounds.
                                                        "#EC NO_HANDLER
      ENDTRY.

      TRANSFER <fs_x_in>+lv_pos TO lv_str_filename LENGTH 1.
    ENDDO.

    CLOSE DATASET lv_str_filename.
    "ULINE.
  ENDIF.
*  write:  'UX2DOS File [', lv_str_filename, '] [', lv_nCount, '] linefeeds replaced with CRLF.'.

ENDFORM.                                                    "UX2DOS