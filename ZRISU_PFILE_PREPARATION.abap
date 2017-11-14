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
       END OF ty_payment_data.

TYPES: BEGIN OF ty_pfile_table1,
         gsber     TYPE fkkvkp-gsber,
         can       TYPE fkkvkp-vkont,
         pamount   TYPE char16,
         transdate TYPE char8,
         paycenter TYPE char50,
         opbuk     TYPE fkkvkp-opbuk,
       END OF ty_pfile_table1.

TYPES: BEGIN OF ty_pfile_table1_paycenter,
         paycenter TYPE char50,
         gsber     TYPE fkkvkp-gsber,
         can       TYPE fkkvkp-vkont,
         pamount   TYPE char16,
         transdate TYPE char8,
       END OF ty_pfile_table1_paycenter.

TYPES: BEGIN OF ty_pfile_table1_can,
         can TYPE fkkvkp-vkont,
         gsber TYPE fkkvkp-gsber,
         paycenter TYPE char50,
         pamount TYPE char16,
         transdate TYPE char8,
       END OF ty_pfile_table1_can.

TYPES: BEGIN OF ty_pfile_table2_line2,
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
       END OF ty_pfile_table2_line2.

TYPES: BEGIN OF ty_pfile_table2_line3,
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
       END OF ty_pfile_table2_line3.
*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_payment_files            TYPE STANDARD TABLE OF salfldir,
      gt_pfile_table1             TYPE STANDARD TABLE OF ty_pfile_table1,
      gt_pfile_table1_paycenter   TYPE STANDARD TABLE OF ty_pfile_table1_paycenter,
      gt_pfile_table1_can         TYPE STANDARD TABLE OF ty_pfile_table1_can.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_payment_file_normal  TYPE salfile-longname,
      gv_payment_file_bpi     TYPE salfile-longname,
      gv_pfile_table1         TYPE ty_pfile_table1.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS : rbselected TYPE c LENGTH 1 VALUE 'X'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_pfile_table1>  TYPE ty_pfile_table1,
               <fs_payment_files> TYPE salfldir.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01.
PARAMETERS: pa_input  TYPE salfile-longname DEFAULT '\\SAPREP01.mayniladsap.com\Operation\Paymentposting\Input\',
            pa_outpt  TYPE salfile-longname DEFAULT '\\SAPREP01.mayniladsap.com\Operation\Paymentposting\Output\'.
SELECTION-SCREEN SKIP.
PARAMETERS: pa_cb     AS CHECKBOX DEFAULT '' USER-COMMAND rusr,
            pa_date   TYPE sy-datum MODIF ID a.
SELECTION-SCREEN SKIP.
PARAMETERS: pa_cb2    AS CHECKBOX DEFAULT '' USER-COMMAND rusr,
            pa_num    TYPE char2 DEFAULT '01' MODIF ID b.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*at selection-screen on <para|selcrit>
*at selection-screen.
*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF pa_cb NE rbselected AND screen-group1 = 'A'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

    IF pa_cb2 NE rbselected AND screen-group1 = 'B'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  CONCATENATE pa_input sy-datum+4(2) sy-datum+6(2) sy-datum(4)  '\' INTO pa_input.
  CONCATENATE pa_outpt sy-datum+4(2) sy-datum+6(2) sy-datum(4)  '\' INTO pa_outpt.

  PERFORM dev_parameters.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  DATA: lv_folder_len TYPE i,
        lv_file_len TYPE i.

  lv_folder_len = strlen( pa_input ) - 1.

  IF pa_input+lv_folder_len(1) NE '\'.
    CONCATENATE pa_input '\' INTO pa_input.
  ENDIF.

  CONCATENATE pa_input 'PaymentFiles' INTO gv_payment_file_normal.
  CONCATENATE pa_input 'BPI' INTO gv_payment_file_bpi.

  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name     = gv_payment_file_normal
    TABLES
      file_tbl = gt_payment_files.

  IF sy-subrc EQ 0.
    LOOP AT gt_payment_files ASSIGNING <fs_payment_files>.
      IF <fs_payment_files>-name(1) NE '.'.
        CONDENSE <fs_payment_files>-name.
        TRANSLATE <fs_payment_files>-name TO UPPER CASE.
        lv_file_len = strlen( <fs_payment_files>-name ) - 6.

*        IF <fs_payment_files>-name+LV_FILE_LEN(4) eq '.TXT'.
        PERFORM process_normal USING <fs_payment_files>-name.
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name     = gv_payment_file_bpi
    TABLES
      file_tbl = gt_payment_files.

  IF sy-subrc EQ 0.
    LOOP AT gt_payment_files ASSIGNING <fs_payment_files>.
      IF <fs_payment_files>-name(1) NE '.'.
        CONDENSE <fs_payment_files>-name.
        TRANSLATE <fs_payment_files>-name TO UPPER CASE.
        PERFORM process_bpi USING <fs_payment_files>-name.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM generate_final_pfile.
  PERFORM generate_report_payment_center.
  PERFORM generate_report_ba.
  PERFORM generate_report_can_exception.
*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_NORMAL
*&---------------------------------------------------------------------*
*       Process the normal payment files
*----------------------------------------------------------------------*
*      --->PV_FILE      Normal Payment File
*----------------------------------------------------------------------*
FORM process_normal USING pv_file.
  DATA: lv_dataset     TYPE salfile-longname,
        lv_string      TYPE ty_payment_data,
        lt_data        TYPE TABLE OF ty_payment_data,
        lv_length      TYPE i,
        lv_file_length TYPE i,
        lv_transdate   TYPE char8,
        lv_amount      TYPE p DECIMALS 2.
  DATA: lv_matcher     TYPE REF TO cl_abap_matcher,
        lv_decimal     TYPE p DECIMALS 2.

  FIELD-SYMBOLS: <ls_data> TYPE ty_payment_data.

  lv_file_length = strlen( pv_file ) - 2.
  CONCATENATE gv_payment_file_normal '\' pv_file INTO lv_dataset.

  PERFORM ux2dos  USING lv_dataset.
  OPEN DATASET lv_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc EQ 0.
    DO.
      READ DATASET lv_dataset INTO lv_string-data.
      APPEND lv_string TO lt_data.
      IF sy-subrc <> 0. EXIT. ENDIF.
    ENDDO.
    CLOSE DATASET pv_file.

    LOOP AT lt_data ASSIGNING <ls_data>.
      lv_length = strlen( <ls_data>-data ).
      IF sy-tabix > 1.
        lv_length = strlen( <ls_data>-data ).
        IF lv_length EQ 80.
* OLD
          gv_pfile_table1-can = <ls_data>-data(11).
          CONDENSE gv_pfile_table1-can.
          SHIFT gv_pfile_table1-can RIGHT DELETING TRAILING space.
          OVERLAY gv_pfile_table1-can WITH '000000000000'.

          SELECT SINGLE gsber opbuk
          INTO (gv_pfile_table1-gsber, gv_pfile_table1-opbuk)
          FROM fkkvkp
          WHERE vkont EQ gv_pfile_table1-can.

          IF sy-subrc EQ 0.

            CONCATENATE '20' <ls_data>-data+33(2) <ls_data>-data+29(4) INTO lv_transdate.

            lv_amount = <ls_data>-data+15(9) / 100.
            gv_pfile_table1-pamount = lv_amount.
            gv_pfile_table1-transdate = lv_transdate.
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            CONDENSE gv_pfile_table1-gsber.

            MOVE gv_pfile_table1-pamount TO lv_decimal.
            "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
            lv_matcher = cl_abap_matcher=>create(
            pattern = `^(\.\d+)$`
            text    = gv_pfile_table1-pamount ).
            IF lv_matcher->match( ) IS NOT INITIAL.
              CONCATENATE '0' gv_pfile_table1-pamount INTO gv_pfile_table1-pamount.
              MOVE gv_pfile_table1-pamount TO lv_decimal.
              IF lv_decimal LE 0.
                gv_pfile_table1-gsber = '0000'.
              ENDIF.
            ELSEIF lv_decimal <= 0.
              gv_pfile_table1-gsber = '0000'.
            ENDIF.
            "-- End

            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ELSE.

            CONCATENATE '20' <ls_data>-data+33(2) <ls_data>-data+29(4) INTO lv_transdate.

            lv_amount = <ls_data>-data+15(9) / 100.
            gv_pfile_table1-pamount = lv_amount.
            gv_pfile_table1-transdate = lv_transdate.
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            gv_pfile_table1-gsber = '0000'.
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ENDIF.

        ELSEIF lv_length GT 80.
* NEW
          gv_pfile_table1-can = <ls_data>-data+34(30).
          CONDENSE gv_pfile_table1-can.
          SHIFT gv_pfile_table1-can RIGHT DELETING TRAILING space.
          OVERLAY gv_pfile_table1-can WITH '000000000000'.

          SELECT SINGLE gsber opbuk
          INTO (gv_pfile_table1-gsber, gv_pfile_table1-opbuk)
          FROM fkkvkp
          WHERE vkont EQ gv_pfile_table1-can.

          IF sy-subrc EQ 0.
            gv_pfile_table1-pamount = <ls_data>-data+123(16).
            gv_pfile_table1-transdate = <ls_data>-data+215(8).
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            CONDENSE gv_pfile_table1-gsber.

            MOVE gv_pfile_table1-pamount TO lv_decimal.
            "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
            lv_matcher = cl_abap_matcher=>create(
            pattern = `^(\.\d+)$`
            text    = gv_pfile_table1-pamount ).
            IF lv_matcher->match( ) IS NOT INITIAL.
              CONCATENATE '0' gv_pfile_table1-pamount INTO gv_pfile_table1-pamount.
              MOVE gv_pfile_table1-pamount TO lv_decimal.
              IF lv_decimal LE 0.
                gv_pfile_table1-gsber = '0000'.
              ENDIF.
            ELSEIF lv_decimal <= 0.
              gv_pfile_table1-gsber = '0000'.
            ENDIF.
            "-- End
            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ELSE.
            gv_pfile_table1-pamount = <ls_data>-data+123(16).
            gv_pfile_table1-transdate = <ls_data>-data+215(8).
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            gv_pfile_table1-gsber = '0000'.
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.               "PROCESS_NORMAL

*&---------------------------------------------------------------------*
*&      Form  PROCESS_BPI
*&---------------------------------------------------------------------*
*       Process the BPI payment files
*----------------------------------------------------------------------*
*      --->PV_FILE      BPI Payment File
*----------------------------------------------------------------------*
FORM process_bpi USING pv_file.
  DATA: lv_dataset     TYPE salfile-longname,
        lv_string      TYPE ty_payment_data,
        lt_data        TYPE TABLE OF ty_payment_data,
        lv_length      TYPE i,
        lv_file_length TYPE i,
        lv_transdate   TYPE char8,
        lv_amount      TYPE p DECIMALS 2.
  DATA: lv_matcher     TYPE REF TO cl_abap_matcher,
        lv_decimal     TYPE p DECIMALS 2.

  FIELD-SYMBOLS: <ls_data> TYPE ty_payment_data.

  lv_file_length = strlen( pv_file ) - 2.
  CONCATENATE gv_payment_file_bpi '\' pv_file INTO lv_dataset.
  OPEN DATASET lv_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET lv_dataset INTO lv_string-data.
      APPEND lv_string TO lt_data.
      IF sy-subrc <> 0. EXIT. ENDIF.
    ENDDO.
    CLOSE DATASET pv_file.

    LOOP AT lt_data ASSIGNING <ls_data>.

      lv_length = strlen( <ls_data>-data ).
      IF lv_length GT 190.
        IF <ls_data>-data(1) EQ '2'.

          gv_pfile_table1-can = <ls_data>-data+37(8).
          CONDENSE gv_pfile_table1-can.
          SHIFT gv_pfile_table1-can RIGHT DELETING TRAILING space.
          OVERLAY gv_pfile_table1-can WITH '000000000000'.

          SELECT SINGLE gsber
          INTO gv_pfile_table1-gsber
          FROM fkkvkp
          WHERE vkont EQ gv_pfile_table1-can.

          IF sy-subrc EQ 0.

            MOVE <ls_data>-data+163(8) TO lv_transdate.

            lv_amount = <ls_data>-data+128(9) / 100.
            gv_pfile_table1-pamount = lv_amount.
            gv_pfile_table1-transdate = lv_transdate.
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            CONDENSE gv_pfile_table1-gsber.

            MOVE gv_pfile_table1-pamount TO lv_decimal.
            "--- If Amount starts with decimal and is less than or equal to 0, consider as Exception
            lv_matcher = cl_abap_matcher=>create(
            pattern = `^(\.\d+)$`
            text    = gv_pfile_table1-pamount ).
            IF lv_matcher->match( ) IS NOT INITIAL.
              CONCATENATE '0' gv_pfile_table1-pamount INTO gv_pfile_table1-pamount.
              MOVE gv_pfile_table1-pamount TO lv_decimal.
              IF lv_decimal LE 0.
                gv_pfile_table1-gsber = '0000'.
              ENDIF.
            ELSEIF lv_decimal <= 0.
              gv_pfile_table1-gsber = '0000'.
            ENDIF.
            "-- End

            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ELSE.

            MOVE <ls_data>-data+163(8) TO lv_transdate.

            lv_amount = <ls_data>-data+128(9) / 100.
            gv_pfile_table1-pamount = lv_amount.
            gv_pfile_table1-transdate = lv_transdate.
            gv_pfile_table1-paycenter = pv_file(lv_file_length).
            gv_pfile_table1-gsber = '0000'.
            CONDENSE gv_pfile_table1-pamount.
            CONDENSE gv_pfile_table1-transdate.
            CONDENSE gv_pfile_table1-paycenter.
            APPEND gv_pfile_table1 TO gt_pfile_table1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
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
        lv_line2            TYPE ty_pfile_table2_line2,
        lv_line3            TYPE ty_pfile_table2_line3,
        lv_can_count        TYPE i,
        lv_total_amt        TYPE p DECIMALS 2,
        lv_can              TYPE char12.

  FIELD-SYMBOLS: <fs_ba> TYPE ty_pfile_table1.

  IF pa_num IS INITIAL.
    pa_num = '01'.
  ENDIF.

  lv_folder_len = strlen( pa_outpt ) - 1.
  IF pa_outpt+lv_folder_len(1) NE '\'.
    CONCATENATE pa_input '\' INTO pa_input.
  ENDIF.

  DESCRIBE TABLE gt_pfile_table1.
  IF sy-subrc EQ 0.
    IF sy-tfill GT 0.
      WRITE:/ text-000.
    ENDIF.
  ENDIF.

  SORT gt_pfile_table1 BY gsber can ASCENDING.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    AT NEW gsber.
      lv_can_count = 0.
      lv_total_amt = 0.
      LOOP AT gt_pfile_table1 ASSIGNING <fs_ba> WHERE gsber EQ <fs_pfile_table1>-gsber.
        ADD 1 TO lv_can_count.
        lv_total_amt = lv_total_amt + <fs_ba>-pamount.
      ENDLOOP.

      lv_gsber = <fs_pfile_table1>-gsber.
      IF pa_date IS INITIAL.
        CONCATENATE pa_outpt '\' lv_gsber(2) sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) 'MP' pa_num '.TXT' INTO lv_dataset.
      ELSE.
        CONCATENATE pa_outpt '\' lv_gsber(2)  pa_date+4(2)  pa_date+6(2)  pa_date+2(2) 'MP' pa_num '.TXT' INTO lv_dataset.
      ENDIF.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

      TRANSFER '0500R' TO lv_dataset.
      lv_line2-text1 = '1BFKKZK'.
      lv_line2-text2 = lv_gsber(2).

      IF pa_date IS INITIAL.
        CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) INTO lv_line2-text3.
      ELSE.
        CONCATENATE pa_date+4(2) pa_date+6(2) pa_date+2(2) INTO lv_line2-text3.
      ENDIF.
      CONCATENATE 'MP' pa_num INTO lv_line2-text4.
      lv_line2-text5 = lv_gsber(2).
      IF pa_date IS INITIAL.
        CONCATENATE sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) INTO lv_line2-text6.
      ELSE.
        CONCATENATE pa_date+4(2) pa_date+6(2) pa_date+2(2) INTO lv_line2-text6.
      ENDIF.
      CONCATENATE 'MP' pa_num INTO lv_line2-text7.
      CONCATENATE '9100700000' <fs_pfile_table1>-opbuk INTO lv_line2-text8.
      lv_line2-text9 = lv_gsber.
      lv_line2-text10 = 'BAPHP'.
      IF pa_date IS INITIAL.
        lv_line2-text11 = sy-datum(8).
      ELSE.
        lv_line2-text11 = pa_date(8).
      ENDIF.
      lv_line2-text12 = <fs_pfile_table1>-transdate.
      IF pa_date IS INITIAL.
        lv_line2-text13 = sy-datum(8).
      ELSE.
        lv_line2-text13 = pa_date(8).
      ENDIF.
      lv_line2-text14 = pa_num.
      lv_line2-text15 = lv_total_amt.
      SHIFT lv_line2-text15 RIGHT DELETING TRAILING space.
      OVERLAY lv_line2-text15 WITH '               '.
      lv_line2-text16 = ''.
      lv_line2-text17 = lv_can_count.
      SHIFT lv_line2-text17 RIGHT DELETING TRAILING space.
      OVERLAY lv_line2-text17 WITH '000000'.
      TRANSFER lv_line2 TO lv_dataset.
    ENDAT.

    lv_line3-text1  = '2BFKKZP'.
    lv_line3-text2  = 'K'.
    lv_can          = <fs_pfile_table1>-can.
    SHIFT lv_can LEFT DELETING LEADING '0'.
    lv_line3-text3  = lv_can.
    lv_line3-text4  = ''.
    lv_line3-text5  = <fs_pfile_table1>-pamount.
    SHIFT lv_line3-text5 RIGHT DELETING TRAILING space.
    OVERLAY lv_line3-text5 WITH '               '.
    lv_line3-text6  = ''.
    CONCATENATE '9100700000' <fs_pfile_table1>-opbuk INTO lv_line3-text7.
    lv_line3-text8  = <fs_pfile_table1>-gsber.
    lv_line3-text9  = 'BAPHP'.
    IF pa_date IS INITIAL.
      lv_line3-text10 = sy-datum(8).
    ELSE.
      lv_line3-text10 = pa_date(8).
    ENDIF.
    lv_line3-text11 = <fs_pfile_table1>-transdate.
    IF pa_date IS INITIAL.
      lv_line3-text12 = sy-datum(8).
    ELSE.
      lv_line3-text12 = pa_date(8).
    ENDIF.
    lv_line3-text13 = pa_num.
    lv_line3-text14 = <fs_pfile_table1>-paycenter.
    TRANSFER lv_line3 TO lv_dataset.

    AT END OF gsber.
      CLOSE DATASET lv_dataset.
      WRITE:/ lv_dataset.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " GENERATE_FINAL_PFILE
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_PAYMENT_CENTER
*&---------------------------------------------------------------------*
*       Generate the Payment File Report per Payment Center
*----------------------------------------------------------------------*
FORM generate_report_payment_center .
  DATA: lv_paycenter                  TYPE ty_pfile_table1_paycenter,
        lv_dataset                    TYPE salfile-longname,
        lv_file_length                TYPE i,
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
        lv_string                     TYPE string.

  FIELD-SYMBOLS: <fs_paycenter> TYPE ty_pfile_table1_paycenter,
                 <fs_pc>        TYPE ty_pfile_table1_paycenter.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    lv_paycenter-paycenter = <fs_pfile_table1>-paycenter.
    lv_paycenter-gsber = <fs_pfile_table1>-gsber.
    lv_paycenter-can = <fs_pfile_table1>-can.
    lv_paycenter-pamount = <fs_pfile_table1>-pamount.
    lv_paycenter-transdate = <fs_pfile_table1>-transdate.
    APPEND lv_paycenter TO gt_pfile_table1_paycenter.
  ENDLOOP.

  SORT gt_pfile_table1_paycenter BY paycenter ASCENDING.

  LOOP AT gt_pfile_table1_paycenter ASSIGNING <fs_paycenter>.
    AT FIRST.
      CONCATENATE pa_outpt '\' 'REPORT_PAYMENT_CENTER' co_xls INTO lv_dataset.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
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
    ENDAT.
    AT NEW paycenter.
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
    ENDAT.
  ENDLOOP.
  CLOSE DATASET lv_dataset.
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
        lv_string         TYPE string.

  FIELD-SYMBOLS: <fs_ba> TYPE ty_pfile_table1.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  SORT gt_pfile_table1 BY gsber ASCENDING.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    AT FIRST.
      CONCATENATE pa_outpt '\' 'REPORT_BUSINESS_AREA' co_xls INTO lv_dataset.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      CONCATENATE text-008
                  text-004
                  text-005
                  INTO lv_string
                  SEPARATED BY co_separator.
      TRANSFER lv_string TO lv_dataset.
    ENDAT.

    AT NEW gsber.
      lv_can_cnt = 0.
      lv_total_amt = 0.
      IF <fs_pfile_table1>-gsber NE '0000'.
        LOOP AT gt_pfile_table1 ASSIGNING <fs_ba> WHERE gsber EQ <fs_pfile_table1>-gsber.
          ADD 1 TO lv_can_cnt.
          lv_total_amt = lv_total_amt + <fs_ba>-pamount.
        ENDLOOP.

        lv_can_cnt_str = lv_can_cnt.
        lv_total_amt_str = lv_total_amt.

        CONCATENATE <fs_pfile_table1>-gsber
                    lv_can_cnt_str
                    lv_total_amt_str
                    INTO lv_string
                    SEPARATED BY co_separator.
        TRANSFER lv_string TO lv_dataset.
      ENDIF.
    ENDAT.
  ENDLOOP.
  CLOSE DATASET lv_dataset.

ENDFORM.                    " GENERATE_REPORT_BA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT_CAN_EXCEPTION
*&---------------------------------------------------------------------*
*       Generate the CAN Exception Report
*----------------------------------------------------------------------*
FORM generate_report_can_exception .
  DATA: lv_dataset            TYPE salfile-longname,
        lv_total_amt          TYPE p DECIMALS 2,
        lv_can_cnt_str        TYPE string,
        lv_total_amt_str      TYPE string,
        lv_string             TYPE string,
        lv_can                TYPE ty_pfile_table1_can.

  FIELD-SYMBOLS: <fs_vkont> TYPE ty_pfile_table1_can,
                 <fs_can> TYPE ty_pfile_table1_can.

  CONSTANTS:  co_separator   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab, "TYPE c LENGTH 1 VALUE `|`,
              co_xls         TYPE c LENGTH 4 VALUE `.XLS`.

  LOOP AT gt_pfile_table1 ASSIGNING <fs_pfile_table1>.
    lv_can-can = <fs_pfile_table1>-can.
    lv_can-gsber = <fs_pfile_table1>-gsber.
    lv_can-paycenter = <fs_pfile_table1>-paycenter.
    lv_can-pamount = <fs_pfile_table1>-pamount.
    lv_can-transdate = <fs_pfile_table1>-transdate.
    APPEND lv_can TO gt_pfile_table1_can.
  ENDLOOP.


  SORT gt_pfile_table1_can BY can ASCENDING.

  LOOP AT gt_pfile_table1_can ASSIGNING <fs_vkont>.
    AT FIRST.
      CONCATENATE pa_outpt '\' 'CAN_EXCEPTION' co_xls INTO lv_dataset.
      OPEN DATASET lv_dataset FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      CONCATENATE text-009
                  text-007
                  text-010
                  INTO lv_string
                  SEPARATED BY co_separator.
      TRANSFER lv_string TO lv_dataset.
    ENDAT.

    AT NEW can.
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
    ENDAT.
  ENDLOOP.
  CLOSE DATASET lv_dataset.
ENDFORM.                    " GENERATE_REPORT_CAN_EXCEPTION

*&---------------------------------------------------------------------*
*&      Form  UX2DOS
*&---------------------------------------------------------------------*
*      Converting a UX File to DOS Format (CRLF)
*----------------------------------------------------------------------*
*      -->IV_FILENAME  Input File
*----------------------------------------------------------------------*
FORM ux2dos
  USING iv_filename           TYPE csequence.
  DATA: lv_xcr                TYPE xstring,
        iret                  TYPE i,
        lv_str_filename       TYPE string,
        xout                  TYPE xstring,
        xline                 TYPE xstring,
        xchar                 TYPE x,
        actlen                TYPE i,
        pos                   TYPE i,
        pos_prev              TYPE i,
        lv_ncount(10)         TYPE n.
  FIELD-SYMBOLS: <lfs_x_in>   TYPE xstring,
                 <lfs_x_out>  TYPE xstring,
                 <lfs_x_prv>  TYPE x.

  lv_xcr =  '0D0A'.

  lv_str_filename = iv_filename.
  ASSIGN  xout TO <lfs_x_out> .
  TRY.
      OPEN DATASET lv_str_filename FOR INPUT IN BINARY MODE .
    CATCH cx_sy_file_authority .
*          write: 'UX2DOS NO ACCESS TO File [', lv_str_filename, ']'.
      RETURN.
  ENDTRY.
  IF sy-subrc <> 0.
    ULINE.
*    write: 'UX2DOS File [', lv_str_filename, '] could not be opened.'.
    RETURN.
  ENDIF.
  "Read full file into buffer and get actlen = filelen
  READ DATASET lv_str_filename INTO xline ACTUAL LENGTH actlen.
  CLOSE DATASET lv_str_filename.
* Reopen same file binary mode
  OPEN DATASET lv_str_filename FOR OUTPUT IN BINARY MODE .

  pos = -1. "because pos is incremented right after do - and we need a ZERO.
  DO actlen TIMES.
    ADD 1 TO   pos.
    ASSIGN xline TO  <lfs_x_in> .
    IF <lfs_x_in> IS NOT ASSIGNED.
      EXIT.
    ENDIF.
    pos_prev = pos - 1.
    IF pos_prev >= 0.
      ASSIGN  xchar TO <lfs_x_prv>.
      <lfs_x_prv> = <lfs_x_in>+pos_prev.
    ENDIF.
* Check if substitution is neccessary at all
*    if <lfs_x_in>+pos(1) = '0A' AND <lfs_x_prv> <> '0D'.
*    if <lfs_x_in>+pos(1) = '0A' AND <lfs_x_prv> = '0D'.
    TRY.
        IF <lfs_x_in>+pos(2) = '0D0A'.
        ELSEIF <lfs_x_in>+pos(1) = '0D'.
          TRANSFER lv_xcr TO lv_str_filename LENGTH 2.
          ADD 1 TO lv_ncount.
          CONTINUE.
        ENDIF.
      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.

    TRANSFER <lfs_x_in>+pos TO lv_str_filename LENGTH 1.
  ENDDO.

  CLOSE DATASET lv_str_filename.
  ULINE.
*  write:  'UX2DOS File [', lv_str_filename, '] [', lv_nCount, '] linefeeds replaced with CRLF.'.

ENDFORM.                                                    "UX2DOS
*&---------------------------------------------------------------------*
*&      Form  DEV_PARAMETERS
*&---------------------------------------------------------------------*
*       For Debugging Purposes, check Developer's username and set Parameters
*----------------------------------------------------------------------*
FORM dev_parameters .
  IF sy-uname EQ 'GDIMALIWAT'.
    pa_input = '\\172.18.1.240\repuserdata\Glenn\PFILE\Input\'.
    pa_outpt = '\\172.18.1.240\repuserdata\Glenn\PFILE\Output\'.
  ENDIF.
ENDFORM.                    " DEV_PARAMETERS