*&---------------------------------------------------------------------*
*&  Include           ZR_PM_PIR_UPLOAD_V4_SUPP
*&---------------------------------------------------------------------*

FORM init.

  DATA: lv_char255 TYPE char255.


  "-- check status message of previuos run.
  GET PARAMETER ID gc_sy_status FIELD lv_char255.
  IF sy-subrc IS INITIAL.
    IF lv_char255 = 'SPACE'.
      ...
    ELSE.
      MESSAGE lv_char255 TYPE 'S'.
      SET PARAMETER ID gc_sy_status FIELD 'SPACE'.
    ENDIF.
  ELSE.
    ...
  ENDIF.

ENDFORM.                    "init

*&---------------------------------------------------------------------*
*&      Form  get_xls_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CV_XLS     text
*----------------------------------------------------------------------*
FORM get_xls_file CHANGING cv_xls.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = cv_xls.

ENDFORM.                    "get_xls_file

*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_FILEPATH  text
*      -->CT_UPLOAD    text
*      -->CV_STATUS    text
*      -->CV_MESSAGE   text
*----------------------------------------------------------------------*
FORM upload_file USING     uv_filepath
                 CHANGING  ct_upload    TYPE tt_upload
                           cv_status
                           cv_message.

* tables
  DATA: lt_xls   TYPE TABLE OF alsmex_tabline.

* structures.
  DATA: ls_upload LIKE LINE OF ct_upload.

* variables
  DATA: lv_isres  TYPE abap_bool.
  DATA: lv_string TYPE string.
  DATA: lv_ctr    TYPE i.

* field-symbols.
  FIELD-SYMBOLS: <ls_xls> LIKE  LINE OF lt_xls.

  CONSTANTS: lc_25 TYPE i VALUE '25',
             lc_xcel_max_row TYPE i VALUE '65000'.


*& pre-processes
  CHECK uv_filepath IS NOT INITIAL.


*& initialization
  CLEAR: cv_status, cv_message.


*& set status (for logging purposes).
  MESSAGE 'Uploading...'(s01) TYPE 'S'.


*& check file existence
  lv_string = uv_filepath.
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_string
    RECEIVING
      result               = lv_isres
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF sy-subrc IS INITIAL. "Check has been executed.
    IF lv_isres EQ abap_false. "check returned False (Not exist)
      MOVE: 'E'                                   TO cv_status,    "Flag as error.
            'File specified doesn''t exist!'(e91) TO cv_message.   "Set message to be displayed.
      RETURN. "End routine process.
    ELSE. "check returned true. (File Found)
      ...
    ENDIF.
  ELSE. "File check failed.
    MOVE: 'E'                                                 TO cv_status,    "Flag as error.
          '[TECH-ERROR]File specified doesn''t exist!'(e92)   TO cv_message.   "Set message to be displayed.
    RETURN. "End routine process.
  ENDIF.


*& set status (for logging purposes).
  MESSAGE 'Converting to SAP compatible...'(s02) TYPE 'S'.


*& get contents of upload file.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = uv_filepath
      i_begin_col             = 1
      i_begin_row             = 5
      i_end_col               = 7
      i_end_row               = lc_xcel_max_row
    TABLES
      intern                  = lt_xls
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0. "Something wrong happened.
    MOVE: 'E'              TO cv_status,    "Flag as error.
          '[TECH-ERROR] Failed to transfer Excel to Internal Table'(e93)
                           TO cv_message.   "Set message to be displayed.
    RETURN.
  ENDIF.


*& check if file is empty.
  IF lt_xls IS INITIAL. "Check contents of upload file.
    MOVE: 'E'              TO cv_status,    "Flag as error.
          'Unable to process, file specified is empty.'(e94)
                         TO cv_message.   "Set message to be displayed.
    RETURN. "End routine process.
  ELSE.
    ...
  ENDIF.


*& move to ABAP table. -> ct_upload
  LOOP AT lt_xls ASSIGNING <ls_xls>.
    CASE <ls_xls>-col.
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <ls_xls>-value
          IMPORTING
            output = ls_upload-matnr.
      WHEN 2.
        ls_upload-mdesc = <ls_xls>-value.
      WHEN 3.
        ls_upload-berid = <ls_xls>-value.
      WHEN 4.
        ls_upload-rdate = <ls_xls>-value.
      WHEN 5.
        TRY.
            ls_upload-rqty  = <ls_xls>-value.
            IF ls_upload-rqty < 0.
              MOVE: 'E'              TO cv_status,    "Flag as error.
                    'Unable to upload, negative quantities found in the file!'
                                   TO cv_message.   "Set message to be displayed.
              RETURN.

*            MESSAGE 'Unable to upload, negative quantities found on file!' TYPE 'E'.
            ENDIF.

            DATA: lv_qty LIKE ls_upload-rqty.
            CLEAR: lv_qty.
            lv_qty = ls_upload-rqty MOD 1.
            IF lv_qty EQ 0.
              "passed
            ELSE.
              MOVE: 'E'              TO cv_status,    "Flag as error.
                    'Unable to upload, decimal entries are found in some of the quantity!'
                                   TO cv_message.   "Set message to be displayed.
              RETURN.
            ENDIF.

          CATCH  cx_sy_conversion_no_number.
            MOVE: 'E'              TO cv_status,    "Flag as error.
                  'Unable to read quantity in upload file, please use numeric values only!'
                                   TO cv_message.   "Set message to be displayed.
            RETURN.

*          MESSAGE 'Unable to read quantity in upload file, please use numeric values only!' TYPE 'E'.
        ENDTRY.
      WHEN 6.
        ls_upload-uom   = <ls_xls>-value.
      WHEN 7.
        ls_upload-vrsn  = <ls_xls>-value.

        COLLECT ls_upload INTO ct_upload.
        CLEAR ls_upload.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


*& check if file is empty.
  IF ct_upload IS INITIAL.
    MOVE: 'E'              TO cv_status,    "Flag as error.
          'Unable to process, unable to translate upload file.'(e94)
                         TO cv_message.   "Set message to be displayed.
    RETURN.
  ELSE.
    ...
  ENDIF.


ENDFORM.                    "upload_file




*&---------------------------------------------------------------------*
*&      Form  support_retrieve
*&---------------------------------------------------------------------*
*       Retrieve existing PIRs from the standard tables pbim and pbed.
*       Add retrieval to custom table ZTMMRECPS for consumed qty later when FS is final.
*----------------------------------------------------------------------*
*      -->(UT_UPLOAD_SUM)  text
*      -->UR_REPTDATE      text
*      -->CT_PBIM          text
*      -->CT_PBED          text
*      -->CT_CONSUMED      text
*----------------------------------------------------------------------*
FORM support_retrieve USING    value(ut_upload_sum) TYPE tt_alv_upsum
                               ur_reptdate          TYPE tt_reptd_range
                      CHANGING ct_pbim              TYPE tt_pbim_ex
                               ct_pbed              TYPE tt_pbed_ex
                               ct_consumed          TYPE tt_consumed.

  DATA: lr_date_minus_one TYPE RANGE OF bdter,
        ls_date_minus_one LIKE LINE OF lr_date_minus_one.

  DATA: lv_date_tmp       LIKE sy-datum,
        lv_date_first     LIKE sy-datum,
        lv_date_last      LIKE sy-datum
        .

  DATA: ls_consumed       LIKE LINE OF ct_consumed,
        lt_cons_collected LIKE STANDARD TABLE OF ls_consumed.

  FIELD-SYMBOLS: <ls_reptdate>   LIKE LINE OF ur_reptdate.

  FIELD-SYMBOLS: <ls_upload_sum> LIKE LINE OF ut_upload_sum.

  FIELD-SYMBOLS: <ls_consumed>   LIKE LINE OF ct_consumed.

  DATA: lt_z50_mseg TYPE STANDARD TABLE OF mseg,
        lt_z51_mseg_rev TYPE STANDARD TABLE OF mseg.

  DATA: lr_datum TYPE RANGE OF sy-datum,
        ls_datum LIKE LINE OF lr_datum.

  FIELD-SYMBOLS: <ls_z50_mseg> LIKE LINE OF lt_z50_mseg.


  CHECK ut_upload_sum IS NOT INITIAL.

*  "-- filter valid data(flagged previously), delete items that are not valid.
*  LOOP AT ut_upload_sum ASSIGNING <ls_upload_sum>.
*    IF NOT <ls_upload_sum>-iserror IS INITIAL.
*      DELETE ut_upload_sum.
*    ENDIF.
*  ENDLOOP.

****QUICK Fix*********************************************************
  CLEAR: lr_datum.
  LOOP AT ur_reptdate ASSIGNING <ls_reptdate>.
    CLEAR: ls_datum.
    MOVE-CORRESPONDING: <ls_reptdate> TO ls_datum.

    ls_datum-low+6(2)    = '01'.
    IF ls_datum-high EQ '000000' OR ls_datum-high IS INITIAL OR ls_datum-high EQ '00000000'.
    ELSE.
      ls_datum-high+6(2) = '31'.
    ENDIF.

    APPEND ls_datum TO lr_datum.
  ENDLOOP.
*^^*QUICK Fix*********************************************************

  "-- retrieve existing versions of the PIR upload.if any.
  SELECT *
    FROM pbim
    INTO TABLE ct_pbim
    FOR ALL ENTRIES IN ut_upload_sum
    WHERE matnr =  ut_upload_sum-matnr
      AND werks =  ut_upload_sum-berid
      AND loevr <> 'D'.  "Ind, for deleted items. do not select deleted items.

  IF sy-subrc IS INITIAL.
    "-- retrieve supporting data for PBIM.
    SELECT *
      FROM pbed
      INTO TABLE ct_pbed
      FOR ALL ENTRIES IN ct_pbim
      WHERE bdzei =  ct_pbim-bdzei
        AND (    entli =  '3'
              OR entli =  '2' )    "12052013 OPacalso/KCAO
*       AND perxx IN ur_reptdate "12052013 OPacalso/KCAO
        AND pdatu  IN lr_datum    "12052013 OPacalso/KCAO
      .
  ELSE.
    "All uploaded items are new. no existing.
  ENDIF.


***>>> DELETE 11252013 --- *************************************
*  FIELD-SYMBOLS: <ls_pbim> LIKE LINE OF ct_pbim,
*                 <ls_pbed> LIKE LINE OF ct_pbed.
*
*  DATA: lv_isfound TYPE char01.
*
*  LOOP AT ct_pbim ASSIGNING <ls_pbim>.
*    CLEAR: lv_isfound.
*    LOOP AT ct_pbed ASSIGNING <ls_pbed> WHERE bdzei = <ls_pbim>-bdzei.
*      IF <ls_pbed>-perxx IN ur_reptdate.
*        lv_isfound = 'X'.
*        EXIT.
*      ELSE.
*        ...
*      ENDIF.
*    ENDLOOP.
*
*    CASE lv_isfound.
*      WHEN 'X'.
*        ...
*      WHEN OTHERS.
*        DELETE ct_pbim.
*    ENDCASE.
*
*  ENDLOOP.



*--------------------------------------------------------------------*
*-- consumed qty: total withdrawn quantity for each material under reservation number ref to order number,
*-- that has a planning indicator equal to planned order.
*--------------------------------------------------------------------*
  "-- build the date to be used in the selection.. MINUS 1 MONTH date.
* >> MOD, OPACALSO, 11222013 - 000000 - change logic.
*  READ TABLE ur_reptdate ASSIGNING <ls_reptdate> INDEX 1.
*  lv_date_tmp+6(2) = '01'.
*  lv_date_tmp+0(6) = <ls_reptdate>-low.


*  IF sy-datum+4(2) = '01'.  "Check if run date is january, no consumed if january.
  IF p_rdate+4(2) = '01'.  "Check if run date is january, no consumed if january.
    RETURN.
  ELSE.
*    lv_date_tmp = sy-datum.
    lv_date_tmp = p_rdate.
  ENDIF.
* << MOD, OPACALSO, 11222013 - 000000.

  CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
    EXPORTING
      i_date_old = lv_date_tmp
    IMPORTING
      e_date_new = lv_date_last.

* >> MOD 12062013 OPacalso - Change logic - from last month to last monthS.
*  MOVE: lv_date_last TO lv_date_first,
*        '01'         TO lv_date_first+6(2).
  MOVE: '0101'             TO lv_date_first+4(4),
        lv_date_last+0(4)  TO lv_date_first+0(4).
* << MOD 12062013 OPacalso

  CLEAR: ls_date_minus_one.
  MOVE : 'I'              TO   ls_date_minus_one-sign,
         'BT'             TO   ls_date_minus_one-option,
         lv_date_first    TO   ls_date_minus_one-low,
         lv_date_last     TO   ls_date_minus_one-high
         .
  APPEND ls_date_minus_one TO lr_date_minus_one.

  SELECT
      resb~matnr
      resb~bdter
      resb~werks
      resb~rsnum
      resb~aufnr
      resb~enmng
    FROM resb
    INNER JOIN afih
      ON resb~aufnr = afih~aufnr
    INTO TABLE ct_consumed                                     ##too_many_itab_fields
    FOR ALL ENTRIES  IN ut_upload_sum
    WHERE resb~matnr EQ ut_upload_sum-matnr
      AND resb~werks EQ ut_upload_sum-berid
      AND resb~bdter IN lr_date_minus_one
      AND afih~plknz EQ '1'
    .

  LOOP AT ct_consumed ASSIGNING <ls_consumed>.

  ENDLOOP.

  IF ct_consumed IS INITIAL.
    RETURN.
  ELSE.
    "-- z50
    SELECT *
      FROM mseg
      INTO TABLE lt_z50_mseg
      FOR ALL ENTRIES IN ct_consumed
      WHERE aufnr = ct_consumed-aufnr
        AND matnr = ct_consumed-matnr
        AND werks = ct_consumed-werks
        AND bwart = 'Z50'.

    "-- z51 reversed.
    SELECT *
      FROM mseg
      INTO TABLE lt_z51_mseg_rev
      FOR ALL ENTRIES IN ct_consumed
      WHERE aufnr = ct_consumed-aufnr
        AND matnr = ct_consumed-matnr
        AND werks = ct_consumed-werks
        AND bwart = 'Z51'.
  ENDIF.

  "-- remove reversed from mseg (z51).
  LOOP AT lt_z50_mseg ASSIGNING <ls_z50_mseg>.
    READ TABLE lt_z51_mseg_rev TRANSPORTING NO FIELDS
      WITH KEY smbln = <ls_z50_mseg>.
    IF sy-subrc IS INITIAL. "reversal found. remove from list.
      DELETE lt_z50_mseg.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

* >> MOD. | OPACALSO | 11262013 | change in logic, add mseg table validation.
*  CLEAR: lt_cons_collected.
*  LOOP AT ct_consumed ASSIGNING <ls_consumed>.
*    CLEAR: ls_consumed.
*    MOVE:  <ls_consumed>-matnr   TO   ls_consumed-matnr,
*           <ls_consumed>-werks   TO   ls_consumed-werks,
*           <ls_consumed>-enmng   TO   ls_consumed-enmng
*           .
*    COLLECT: ls_consumed INTO lt_cons_collected.
*  ENDLOOP.

  CLEAR: lt_cons_collected.
  LOOP AT lt_z50_mseg ASSIGNING <ls_z50_mseg>.
    CLEAR: ls_consumed.
    MOVE:  <ls_z50_mseg>-matnr   TO   ls_consumed-matnr,
           <ls_z50_mseg>-werks   TO   ls_consumed-werks,
           <ls_z50_mseg>-erfmg   TO   ls_consumed-enmng
           .
    COLLECT: ls_consumed INTO lt_cons_collected.
  ENDLOOP.
* << MOD. | OPACALSO | 11262013 | change in logic, add mseg table validation.


  IF lt_cons_collected IS INITIAL.
    CLEAR: ct_consumed.
  ELSE.
    CLEAR: ct_consumed.
    APPEND LINES OF lt_cons_collected TO ct_consumed.
  ENDIF.

ENDFORM.                    "support_retrieve


*&---------------------------------------------------------------------*
*&      Form  prelim_validation_upload
*&---------------------------------------------------------------------*
*       flag the field 'ISERROR' with letter 'D' for date error -> CT_UPLOAD_SUM-ISERROR
*       flag the field 'isupload' of table ct_upload_sum for all upload entries.
*       assign a value to the field 'year' of the result table -> CT_UPLOAD_SUM-YEAR
*----------------------------------------------------------------------*
*      -->UT_UPLOAD      raw upload items
*      -->UR_REPTDATE    report date range of that are valid
*      -->CT_UPLOAD_SUM  the summarized output from raw upload items.
*----------------------------------------------------------------------*
FORM prelim_validation_upload  USING    ut_upload     TYPE tt_upload
                                        ur_reptdate   TYPE tt_reptd_range
                               CHANGING ct_upload_sum TYPE tt_alv_upsum
                                        cv_status     TYPE char01
                                        cv_message    TYPE char255
                                        .

  DATA: ls_upload_sum LIKE LINE OF ct_upload_sum.

  FIELD-SYMBOLS: <ls_upload> LIKE LINE OF ut_upload,
                 <ls_uploadsum> LIKE LINE OF ct_upload_sum.


  "-- init
  CHECK ut_upload IS NOT INITIAL.


  "-- check upload file date validity.
  LOOP AT ut_upload ASSIGNING <ls_upload>.
    CLEAR: ls_upload_sum.
    MOVE <ls_upload> TO ls_upload_sum. "add defaults.
    ls_upload_sum-seq = sy-tabix.      "add numbering.


    IF <ls_upload>-vrsn = '01'.   "check only applicable for version 01
      IF <ls_upload>-rdate IN ur_reptdate.
        ls_upload_sum-iserror = space.
      ELSE.
*****QUICK FIX**IGNORE IF UPLOAD HAS ERRORS IN DATE, DELETE Upload ENtry
*        ls_upload_sum-iserror = 'D'. "Type D error means error in DATE.
        CONTINUE. "ignore and remove from list
*^endQUICK FIX**IGNORE IF UPLOAD HAS ERRORS IN DATE, DELETE Upload ENtry
      ENDIF.
    ENDIF.

    "-- add supporting details.
    ls_upload_sum-isupload = 'X'.
    ls_upload_sum-year     = <ls_upload>-rdate+0(4).

    APPEND ls_upload_sum TO ct_upload_sum.
  ENDLOOP.

  READ TABLE ct_upload_sum ASSIGNING <ls_uploadsum>
  WITH KEY iserror = 'D'.
  IF sy-subrc IS INITIAL.
    cv_status = 'E'.
    cv_message = 'Upload file contains version 01 with invalid date. (&1/&2)'.

    SHIFT <ls_uploadsum>-matnr LEFT DELETING LEADING '0'.

    REPLACE '&1' IN cv_message WITH <ls_uploadsum>-matnr.
    REPLACE '&2' IN cv_message WITH <ls_uploadsum>-rdate.

  ELSE.
    cv_status = 'S'.
  ENDIF.

ENDFORM.                    "prelim_validation_upload

*&---------------------------------------------------------------------*
*&      Form  report_date_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CR_DATE    text
*----------------------------------------------------------------------*
FORM report_date_build CHANGING cr_date         TYPE tt_reptd_range
                                cr_blocked_date TYPE tt_reptd_range
                                cr_previous     TYPE tt_reptd_range
  .

  DATA: ls_date_range LIKE LINE OF cr_date.

  DATA: lv_start TYPE sy-datum,
        lv_end   TYPE sy-datum.

  FIELD-SYMBOLS: <ls_date> LIKE LINE OF cr_date,
                 <lt_date> TYPE ANY TABLE.


  "-- build report date
  CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'   " retrieve start date.
    EXPORTING
      months  = 2 "get next 2 months. because current and next month is to be excluded
      olddate = p_rdate
    IMPORTING
      newdate = lv_start.

  "-- retrieve end date.
  lv_end+0(4) = lv_start+0(4). "use year of start date
  lv_end+4(2) = '12'.          "month set to december.

  "-- build range.
  ls_date_range-sign   = 'I'.  "Include
  ls_date_range-option = 'BT'. "Between
  ls_date_range-low    = lv_start.
  ls_date_range-high   = lv_end.

  CLEAR: cr_date.
  APPEND ls_date_range TO cr_date.



  "-- build blocked
  CLEAR: cr_blocked_date.
  CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
    EXPORTING
      i_date_old = lv_start
    IMPORTING
      e_date_new = lv_start.
  "-- build range.
  ls_date_range-sign   = 'I'.  "Include
  ls_date_range-option = 'BT'. "Between
*  ls_date_range-low    = sy-datum+0(6).
  ls_date_range-low    = p_rdate+0(6).
  ls_date_range-high   = lv_start.
  APPEND ls_date_range TO cr_blocked_date.



  "-- build previous months
  CLEAR: cr_previous.
  CLEAR: ls_date_range, lv_start, lv_end. "include date for the year only.
  MOVE:  p_rdate+0(4)   TO    lv_start+0(4),   "year
         '01'           TO    lv_start+4(2).   "january
  MOVE:  p_rdate+0(4)   TO    lv_end+0(4),     "year
         '12'           TO    lv_end+4(2).     "december

  MOVE:  'I'      TO    ls_date_range-sign,
         'BT'     TO    ls_date_range-option,
         lv_start TO    ls_date_range-low,
         lv_end   TO    ls_date_range-high.

  APPEND ls_date_range TO cr_previous.

  DO.  "exclude dates in rept_date(cr_date) and blocked dates.
    IF <lt_date> IS ASSIGNED.
      UNASSIGN <lt_date>.
    ENDIF.

    CASE sy-index.
      WHEN 1.
        ASSIGN cr_date TO <lt_date>.
      WHEN 2.
        ASSIGN cr_blocked_date TO <lt_date>.
      WHEN OTHERS.
        EXIT. "Exit DO!
    ENDCASE.

    LOOP AT <lt_date> ASSIGNING <ls_date>.
      CLEAR: ls_date_range.
      MOVE-CORRESPONDING <ls_date> TO ls_date_range.
      MOVE:   'E'     TO    ls_date_range-sign.   "Exclude.
      APPEND ls_date_range TO cr_previous.
    ENDLOOP.
  ENDDO.


ENDFORM.                    "report_date_build


*&---------------------------------------------------------------------*
*&      Form  rate_percentage_compute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE          text
*      -->(US_UPLOAD)    text
*      -->VALUE          text
*      -->(US_EXISTING)  text
*      -->CS_RESULT      text
*----------------------------------------------------------------------*
FORM rate_percentage_compute  USING    value(us_upload)    TYPE  ty_alv_display
                                       value(us_existing)  TYPE  ty_alv_display
                              CHANGING cs_result           TYPE  ty_alv_display
                              .

  DATA: lv_fieldname TYPE char20,
        lv_numc2     TYPE numc2.

  FIELD-SYMBOLS: <ls_month_up> LIKE us_upload-m01.
  FIELD-SYMBOLS: <ls_month_ex> LIKE us_existing-m01.
  FIELD-SYMBOLS: <ls_month_re> LIKE cs_result-m01.


  MOVE us_upload TO cs_result.

  DO 12 TIMES. "12 months

    "-- build fieldname
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_numc2.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_numc2
      IMPORTING
        output = lv_numc2.
    CONCATENATE 'M' lv_numc2 INTO lv_fieldname.

    ASSIGN COMPONENT lv_fieldname OF STRUCTURE us_upload   TO <ls_month_up>.  "upload
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE us_existing TO <ls_month_ex>.  "exist
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE cs_result   TO <ls_month_re>.  "result

    IF <ls_month_up>-value = 0.
      <ls_month_re>-value  = <ls_month_up>-value.
      IF <ls_month_ex>-value <> 0.
        <ls_month_re>-rate   = '.3'.  "0up, !0ex.
      ELSE.
        <ls_month_re>-rate   = '.1'.   "0up, 0ex.
      ENDIF.
    ELSE.
      <ls_month_re>-value  = <ls_month_up>-value.
      IF <ls_month_ex>-value = 0.
        <ls_month_re>-rate = '-1'.    "!0up, 0ex
      ELSE.
        <ls_month_re>-rate = ( <ls_month_up>-value / <ls_month_ex>-value ) - '1'.  "!0up, !0ex
      ENDIF.
    ENDIF.

  ENDDO.


ENDFORM.                    " RATE_PERCENTAGE_COMPUTE


*&---------------------------------------------------------------------*
*&      Form  ask_user_confirm_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CV_STATUS  text
*----------------------------------------------------------------------*
FORM ask_user_confirm_posting  CHANGING cv_status
                                        cv_message TYPE char70.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar                    = 'PIR and Reservation Upload'(h01)
*     DIAGNOSE_OBJECT             = ' '
      text_question               = cv_message
      text_button_1               = 'Yes'(a01)
      icon_button_1               = 'ICON_CHECKED'
      text_button_2               = 'No'(a02)
      icon_button_2               = 'ICON_CANCEL'
      default_button              = '1'
      display_cancel_button       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer                      = cv_status
*   TABLES
*     PARAMETER                   =
*   EXCEPTIONS
*     TEXT_NOT_FOUND              = 1
*     OTHERS                      = 2
            .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.


ENDFORM.                    " ASK_USER_CONFIRM_POSTING

*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_SOURCE  text
*      -->UV_EMAIL   text
*----------------------------------------------------------------------*
FORM send_email USING ut_source TYPE tt_alv_real
                      uv_email  TYPE adr6-smtp_addr.

  DATA: ls_source LIKE LINE OF ut_source,
        lt_src_email LIKE STANDARD TABLE OF ls_source.

  DATA: lv_fieldname TYPE char20,
      lv_numc2     TYPE numc2.

  FIELD-SYMBOLS: <ls_month>   LIKE ls_source-m01.

  FIELD-SYMBOLS: <ls_source> LIKE LINE OF ut_source,
                 <ls_any>    TYPE any.


  CLEAR: lt_src_email.
  LOOP AT ut_source ASSIGNING <ls_source>.

    CLEAR: ls_source.
    ls_source-matnr       =    <ls_source>-matnr.
    ls_source-mdesc       =    <ls_source>-mdesc.
    ls_source-berid       =    <ls_source>-berid.

    DO 12 TIMES.

      "-- build fieldname/ (month fieldname)
      CLEAR: lv_fieldname.
      MOVE sy-index TO lv_numc2.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_numc2
        IMPORTING
          output = lv_numc2.
      CONCATENATE 'M' lv_numc2 INTO lv_fieldname.

      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_source> TO <ls_month>.

      IF <ls_month>-rate > '0.2' OR <ls_month>-rate < '-0.2'.
        CONCATENATE <ls_source>-year lv_numc2 INTO ls_source-rdate.
*        COMPUTE ls_source-rqty     =  <ls_month>-value * <ls_month>-rate. zxzx
        COMPUTE ls_source-rqty     =  <ls_month>-value.
        CALL FUNCTION 'ROUND'
          EXPORTING
            decimals = 1
            input    = ls_source-rqty
*           SIGN     = ' '
          IMPORTING
            output   = ls_source-rqty.
        ls_source-uom      =  <ls_source>-uom.

        APPEND ls_source TO lt_src_email.
      ENDIF.

    ENDDO.
  ENDLOOP.


  IF lt_src_email IS NOT INITIAL.
    PERFORM email_proper USING lt_src_email
                               uv_email.
  ENDIF.

ENDFORM.                    "send_email

*&---------------------------------------------------------------------*
*&      Form  email_proper
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_SOURCE  text
*      -->EMAIL_ADD  text
*----------------------------------------------------------------------*
FORM email_proper USING ut_source TYPE tt_alv_real
                        email_add TYPE adr6-smtp_addr.

  DATA: lcl_send_request TYPE REF TO cl_bcs,
        lcl_document     TYPE REF TO cl_document_bcs,
        lcl_recipient    TYPE REF TO if_recipient_bcs,
        lt_contents      TYPE STANDARD TABLE OF solisti1,
        lt_receivers     TYPE STANDARD TABLE OF somlreci1,
        ls_docdata       TYPE sodocchgi1,
        ls_contents      TYPE solisti1,
        ls_receivers     TYPE somlreci1,
        ls_email         TYPE ad_smtpadr,
        ls_tdline        TYPE tdline,
        lv_lines         TYPE i,
        lv_subject       TYPE string,
        lv_agent_name    TYPE string,
        lv_sent_to_all   TYPE os_boolean.

  DATA: lv_char255 TYPE char255.
  DATA: lv_char1   TYPE char15.
  DATA: lv_char2   TYPE char255.
  DATA: lv_char3   TYPE char255.

  DATA: lv_bdmng   TYPE bdmng.

  CONSTANTS: lco_doctype_htm TYPE so_obj_tp VALUE 'HTM',
             lco_slash(1)    TYPE c         VALUE '/',
             lco_period(1)   TYPE c         VALUE '.'.

  FIELD-SYMBOLS: <ls_source> LIKE LINE OF ut_source.

  DEFINE add_html_txt.
    clear: ls_contents.
    ls_contents = &1.
    append ls_contents to lt_contents.
  END-OF-DEFINITION.

  DEFINE add_trtd.
    add_html_txt '<tr><td>'.
    add_html_txt &1.
    add_html_txt '</td></tr>'.
  END-OF-DEFINITION.

  DEFINE add_tr.
    add_html_txt '<tr>'.
    add_html_txt &1.
    add_html_txt '</tr>'.
  END-OF-DEFINITION.

  DEFINE add_td.
    add_html_txt '<td>'.
    add_html_txt &1.
    add_html_txt '</td>'.
  END-OF-DEFINITION.

*& build EMAIL BODY
  "-- build header message
  add_html_txt '<p>Please be advised that the uploaded Planned Independent Requirements have gone beyond 20%. Please see below.</p><br/>'.

  "-- build content header
  add_html_txt '<table border="1">'.
  add_html_txt '<tr style="background-color:#92d050;border:solid 1 black">'.
  add_td       'Material'.
  add_td       'Material Description'.
  add_td       'MRP Area'.
  add_td       'Date(Month)'.
  add_td       'New Value'.
  add_td       'UoM'.
  add_html_txt '</tr>'.

  "-- build ocntents.
  LOOP AT ut_source ASSIGNING <ls_source>.
    CLEAR: lv_char255.

    "-- convert numbers to string.
    PERFORM convert_bdmng_str255:
             USING     <ls_source>-rqty
             CHANGING  lv_char1.

    SHIFT <ls_source>-matnr LEFT DELETING LEADING '0'.

*    CONCATENATE
*      <ls_source>-matnr
*      <ls_source>-mdesc
*      <ls_source>-berid
*      <ls_source>-rdate
*      lv_char1
*      <ls_source>-uom
*    INTO lv_char255 RESPECTING BLANKS
*    SEPARATED BY ' | '.
*    add_trtd lv_char255.

    add_html_txt '<tr>'."open <tr>

    .. add_td <ls_source>-matnr  .
    .. add_td <ls_source>-mdesc  .
    .. add_td <ls_source>-berid  .
    .. add_td <ls_source>-rdate  .
    .. add_td lv_char1           .
    .. add_td <ls_source>-uom    .

    add_html_txt '</tr>'."Close TR.

  ENDLOOP.

  "-- close tags POST PROCESSING.
  add_html_txt '</table>'.

  add_html_txt '<br/>'.
  add_html_txt '<br/>'.
  add_html_txt '<p>*** This is a system-generated message. Please do not reply to this e-mail. ***.</p>'.

*** Fill document data
  CLEAR: ls_docdata,
         ls_contents,
         lv_lines.
  DESCRIBE TABLE lt_contents LINES lv_lines.
  READ TABLE lt_contents INTO ls_contents INDEX lv_lines.
  ls_docdata-doc_size = ( ( lv_lines - 1 ) * 255 ) + strlen( ls_contents ).

* Subject of the e-mail
  lv_subject = 'PIR Upload Beyond 20%'.
  ls_docdata-obj_descr = lv_subject.

* Create persistent send request
  CLEAR lcl_send_request.
  TRY.
      CALL METHOD cl_bcs=>create_persistent
        RECEIVING
          result = lcl_send_request.

    CATCH cx_send_req_bcs.
  ENDTRY.

  IF lcl_send_request IS NOT INITIAL.
*   Create and set document
    CLEAR lcl_document.
    TRY.
        CALL METHOD cl_document_bcs=>create_document
          EXPORTING
            i_type    = lco_doctype_htm
            i_subject = ls_docdata-obj_descr
            i_length  = ls_docdata-doc_size
            i_text    = lt_contents
          RECEIVING
            result    = lcl_document.

      CATCH cx_document_bcs.
    ENDTRY.

*   Set e-mail subject
    TRY.
        CALL METHOD lcl_send_request->set_message_subject
          EXPORTING
            ip_subject = lv_subject.

      CATCH cx_send_req_bcs.
    ENDTRY.

*** Add document to send request
    IF lcl_document IS NOT INITIAL.
      TRY.
          CALL METHOD lcl_send_request->set_document
            EXPORTING
              i_document = lcl_document.

        CATCH cx_send_req_bcs.
      ENDTRY.
    ENDIF.

*** E-mail recipients
    CLEAR ls_email.
    ls_email = email_add.
    CLEAR lcl_recipient.
    TRY.
        CALL METHOD cl_cam_address_bcs=>create_internet_address
          EXPORTING
            i_address_string = ls_email
          RECEIVING
            result           = lcl_recipient.

      CATCH cx_address_bcs.
    ENDTRY.

    IF lcl_recipient IS NOT INITIAL.
      TRY.
          CALL METHOD lcl_send_request->add_recipient
            EXPORTING
              i_recipient = lcl_recipient.

        CATCH cx_send_req_bcs.
      ENDTRY.
    ENDIF.

*   Send immediately
    TRY.
        CALL METHOD lcl_send_request->set_send_immediately
          EXPORTING
            i_send_immediately = 'X'.

      CATCH cx_send_req_bcs.
    ENDTRY.

*** Send document
    TRY.
        CALL METHOD lcl_send_request->send
          RECEIVING
            result = lv_sent_to_all.

      CATCH cx_send_req_bcs.
    ENDTRY.

    COMMIT WORK.
  ENDIF.


ENDFORM.                    "email_proper


*&---------------------------------------------------------------------*
*&      Form  convert_bdmng_str255
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_BDMNG   text
*      -->CV_CHAR255 text
*----------------------------------------------------------------------*
FORM convert_bdmng_str255 USING uv_bdmng      TYPE bdmng
                          CHANGING cv_char255 TYPE char15.

  CLEAR: cv_char255.

  IF uv_bdmng = 0.
    cv_char255 = '0'.
  ELSE.
    UNPACK uv_bdmng TO cv_char255.
    CONCATENATE cv_char255+1(11) '.' cv_char255+12(3)  INTO cv_char255.
    SHIFT cv_char255 LEFT DELETING LEADING '0'.

    IF uv_bdmng > 0.

    ELSE.
      CONCATENATE '-' cv_char255 INTO cv_char255.
      CONDENSE cv_char255 NO-GAPS.
    ENDIF.
  ENDIF.

ENDFORM.                    "convert_bdmng_str255


*&---------------------------------------------------------------------*
*&      Form  review_qty_validity_check
*&---------------------------------------------------------------------*
*       Check for VALIDITY of upload using FORMULA
*      Total Review Qty	 ==  Annual Total Qty # (Total Actual Consumed Qty + Total Blocked Planned Qty)
*----------------------------------------------------------------------*
*      -->UT_CONSUMED  text
*      -->CT_ALV       text
*----------------------------------------------------------------------*
FORM review_qty_validity_check USING      ut_consumed   TYPE    tt_consumed
                                          ur_rept_date  TYPE    tt_reptd_range
                                          ur_rept_date_blocked  TYPE    tt_reptd_range
                               CHANGING   ct_alv        TYPE    tt_alv_main.

  DATA: lv_review_qty_real       TYPE    enmng,
        lv_review_qty_computed   TYPE    enmng,
        lv_annual_qty            TYPE    enmng,
        lv_actual_consumed_qty   TYPE    enmng,
        lv_blocked_qty           TYPE    enmng
        .

  DATA: ls_alv  LIKE LINE OF ct_alv.

  FIELD-SYMBOLS: <ls_alv>      LIKE LINE OF ct_alv,
                 <ls_alv_read> LIKE LINE OF ct_alv.

  FIELD-SYMBOLS: <ls_consumed_read> LIKE LINE OF ut_consumed.

**********
* Main LOOP!
*-- compute FOR EACH "MATNR" of "BERID/WERKS" version '01'.
**********
  LOOP AT ct_alv ASSIGNING <ls_alv>
  WHERE deact   IS INITIAL
    AND iserror IS INITIAL
    AND vrsn    EQ '01'.  "means loop only applicable for review qty


    "-- get total REVIEW QUANTITY    -  version '01' of MATNR/BERID
    PERFORM get_total_12mos
      USING    <ls_alv>
               gr_report_date
      CHANGING lv_review_qty_real.

    "-- get total ANNUAL QUANTITY    -  version '00' of MATNR/BERID
    READ TABLE ct_alv ASSIGNING <ls_alv_read>
    WITH KEY deact   = space
             iserror = space
             vrsn    = '00'.
    IF sy-subrc IS INITIAL. "exist in upload.
      PERFORM get_total_12mos
        USING    <ls_alv_read>
                 gr_report_date
        CHANGING lv_annual_qty.
    ELSE. "does not exist, retrieve from database.
      CLEAR: ls_alv.
      MOVE:  <ls_alv>  TO  ls_alv,
             '00'      TO  ls_alv-vrsn.
      PERFORM get_total_12mos_frombapi
        USING    ls_alv
        CHANGING lv_annual_qty.
    ENDIF.

* >> MOD OPacalso -12072013- Actual and blocked consumed are already included in the new plan change logic for retrieval, as of 12072013
*    "-- get total ACTUAL CONSUMED QUANTITY   -  from routine parameter ut_consumed
*    CLEAR: lv_actual_consumed_qty.
*    READ TABLE ut_consumed ASSIGNING <ls_consumed_read>
*    WITH KEY matnr = <ls_alv>-matnr
*             werks = <ls_alv>-berid.
*    IF sy-subrc IS INITIAL.
*      MOVE:    <ls_consumed_read>-enmng   TO   lv_actual_consumed_qty.
*    ELSE.
*      "not found, month is january! MAYBE.
*      CLEAR: lv_actual_consumed_qty.
*    ENDIF.
*
*    "-- get total BLOCKED QUANTITY
*    PERFORM get_blocked_qty
*      USING    <ls_alv>
*               ur_rept_date
*               ur_rept_date_blocked
*      CHANGING lv_blocked_qty.

     "-- get total ACTUAL CONSUMED QUANTITY
     PERFORM get_total_12mos
        USING    <ls_alv>
                 gr_report_date_prev
        CHANGING lv_actual_consumed_qty.

     "-- get total BLOCKED QUANTITY
     PERFORM get_total_12mos
        USING    <ls_alv>
                 gr_report_date_blocked
        CHANGING lv_blocked_qty.
* << MOD OPacalso -12072013- Actual and blocked consumed are already included in the new plan change logic for retrieval, as of 12072013




********QUICK FIX 12052013 change logic, new logic for consumed*******
    DATA: lv_delta_no_consume LIKE lv_annual_qty,
          lv_sobra_computed   LIKE lv_annual_qty,
          lv_tira_tira        LIKE lv_annual_qty,
          lv_error_flag       TYPE char01.

    "-- Start - GDIMALIWAT 07/21/2014 - TN#63434
    DATA: lv_matnr TYPE matnr.
    "-- End - GDIMALIWAT 07/21/2014 - TN#63434

    CLEAR: lv_error_flag.

    "-- check total v00 vs v01 (exclude blocked/freezed mos)
    CLEAR: lv_delta_no_consume.
*    lv_delta_no_consume = lv_annual_qty - lv_review_qty_real.
    lv_delta_no_consume = lv_annual_qty - lv_review_qty_real.

    IF lv_delta_no_consume GE 0. "positive . v00 is greater than or equal v01
      ... "passed.
    ELSE. "negative. v00 is less than version 01.
      "error
      "-- Start - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing V01
      SELECT SINGLE matnr
      INTO lv_matnr
      FROM zind_req
      WHERE matnr eq <ls_alv>-MATNR
        AND werks eq <ls_alv>-BERID
        AND versb eq '01'.
      IF sy-subrc eq 0.
        lv_error_flag       = 'Z'. "-- Produce error because there is already an existing V01.. Else proceed because V01 is initially created and V01 planned qty is greater than V00 planned qty
      ENDIF.
      "lv_error_flag       = 'Z'. "-- Commented so program can proceed when V01 planned qty is greater than V00 planned qty
      "-- End - GDIMALIWAT 07/21/2014 - TN#63434
    ENDIF.

    "-- cheeck exceeding version 01, adjust starting from farthest month
    IF lv_error_flag IS INITIAL.

*      lv_sobra_computed = lv_blocked_qty - lv_actual_consumed_qty + lv_delta_no_consume.
*      lv_sobra_computed = lv_delta_no_consume - lv_blocked_qty - lv_actual_consumed_qty.
      lv_sobra_computed = ( lv_blocked_qty + lv_actual_consumed_qty ) - lv_delta_no_consume.

      IF lv_sobra_computed > 0.

        PERFORM adjust_qty USING    lv_sobra_computed
                                    ur_rept_date
                           CHANGING <ls_alv>
                                    lv_tira_tira.

        IF lv_tira_tira IS INITIAL.
          "passed.
        ELSE.
          lv_error_flag = 'X'.
        ENDIF.

      ELSE.
        "--passed
      ENDIF.
    ELSE.
      "-- nothing to execute , porceed to error logging.
    ENDIF.



    IF lv_error_flag = 'Z' OR lv_error_flag = 'X'.

      CASE lv_error_flag.
        WHEN 'Z'.
          <ls_alv>-iserror    =    'Z'.
          <ls_alv>-e_mess     =    'Total V01 quantity is greater than the total V00 quantity.'.
        WHEN 'X'.
          <ls_alv>-iserror    =    'Z'.
          <ls_alv>-e_mess     =    'Insuficient QTY credits to make further adjustments.'.
      ENDCASE.

      READ TABLE ct_alv ASSIGNING <ls_alv_read>  "adjust v00 counterpart (might deactivate if error)
      WITH KEY matnr = <ls_alv>-matnr
               berid = <ls_alv>-berid
               vrsn  = '00'
               deact = 'X'.
      IF sy-subrc IS INITIAL.
        <ls_alv_read>-e_mess = space.
      ENDIF.

    ELSE.
      "all success
    ENDIF.

*^end***QUICK FIX 12052013 change logic, new logic for consumed*******



* >> DEL | As Of 12052013 | OPacalso | Change logic of consumed.
*    "-- validation MAIN!   ->   JUDGEMENT DAY!!!!!!!!!!!!!!!!!!!!
*    lv_review_qty_computed   =   lv_annual_qty -
*                                 ( lv_actual_consumed_qty + lv_blocked_qty )
*                                 .
*
*    "-- put status
**    IF lv_review_qty_real = lv_review_qty_computed.
*    IF lv_review_qty_real LE lv_review_qty_computed.  "11292013 Opacalso - 00001 - allow if 01 is less than
*      "-- PASSED!
*    ELSE.
*      "-- FAILED!
*      <ls_alv>-iserror    =    'Z'.
**      <ls_alv>-e_mess     =    'Total V01 quantity is not equal to the total V00 quantity.'.
*      <ls_alv>-e_mess     =    'Total V01 quantity is greater than the total V00 quantity.'.
*
*      READ TABLE ct_alv ASSIGNING <ls_alv_read>
*      WITH KEY matnr = <ls_alv>-matnr
*               berid = <ls_alv>-berid
*               vrsn  = '00'
*               deact = 'X'.
*      IF sy-subrc IS INITIAL.
*        <ls_alv_read>-e_mess = space.
*      ENDIF.
*
*    ENDIF.
* << DEL | As Of 12052013 | OPacalso

* >> DEL | As Of 11292013 | OPacalso | Removed based from discussion with client | UAT Day 3.
*    "-- put excess to last month
*    DATA: ls_daterept LIKE LINE OF ur_rept_date.
*    DATA: lv_fname    TYPE char20.
*    FIELD-SYMBOLS: <ls_daterept> LIKE ls_daterept.
*    FIELD-SYMBOLS: <ls_month> LIKE ls_alv-m01.
*
*    IF <ls_daterept> IS ASSIGNED.
*      UNASSIGN <ls_daterept>.
*    ENDIF.
*    READ TABLE ur_rept_date ASSIGNING <ls_daterept> INDEX 1.
*
**    CONCATENATE 'M' <ls_daterept>-low+4(2) INTO lv_fname.  "first open month
*    CONCATENATE 'M' <ls_daterept>-high+4(2) INTO lv_fname.  "last open month
*    ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_alv> TO <ls_month>.
*    <ls_month>-value = <ls_month>-value + ( lv_review_qty_computed - lv_review_qty_real ).
* << DEL | As Of 11292013 | OPacalso



  ENDLOOP.
*^ end of Main Loop.
**********


ENDFORM.                    "review_qty_validity_check


*&---------------------------------------------------------------------*
*&      Form  get_total_12mos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_ALV     text
*      -->CV_TOTAL   text
*----------------------------------------------------------------------*
FORM get_total_12mos USING     us_alv   TYPE ty_alv_display
                               ur_date  LIKE gr_report_date
                     CHANGING  cv_total TYPE bdmng.

  DATA: lv_fieldname TYPE char20,
        lv_numc2     TYPE numc2.

  FIELD-SYMBOLS: <ls_month> LIKE us_alv-m01.


  CLEAR: cv_total.

  DO 12 TIMES. "12 months

    "-- build fieldname
    CLEAR: lv_fieldname.
    MOVE sy-index TO lv_numc2.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_numc2
      IMPORTING
        output = lv_numc2.
    CONCATENATE 'M' lv_numc2 INTO lv_fieldname.

    DATA: lv_numc6 TYPE char6.
    lv_numc6+0(4)  =  us_alv-year.
    lv_numc6+4(2)  =  lv_numc2.

    CHECK lv_numc6 IN ur_date.

    ASSIGN COMPONENT lv_fieldname OF STRUCTURE us_alv  TO <ls_month>.
    cv_total = cv_total + <ls_month>-value.

  ENDDO.


ENDFORM.                    "get_total_12mos


*&---------------------------------------------------------------------*
*&      Form  get_total_12mos_frombapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_ALV     text
*      -->CV_TOTAL   text
*----------------------------------------------------------------------*
FORM get_total_12mos_frombapi USING     us_alv   TYPE ty_alv_display
                              CHANGING  cv_total TYPE bdmng.

  DATA: ls_gd_item_out  TYPE bapisitmeo,
        lt_gd_item_out  LIKE STANDARD TABLE OF ls_gd_item_out.

  DATA: lt_gd_ret    TYPE STANDARD TABLE OF bapireturn1.

  FIELD-SYMBOLS: <ls_gd_iout> LIKE LINE OF lt_gd_item_out.

  CLEAR: cv_total.


  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = us_alv-matnr
      plant            = us_alv-berid
      requirementstype = space
      version          = us_alv-vrsn
      reqmtsplannumber = space
*     MRP_AREA         =
*     MATERIAL_EVG     =
    TABLES
      requirements_out = lt_gd_item_out
      return           = lt_gd_ret.

  IF lt_gd_ret IS INITIAL. "Success.
    CLEAR: cv_total.
    LOOP AT lt_gd_item_out ASSIGNING <ls_gd_iout>.
      CHECK <ls_gd_iout>-req_date+0(4) = us_alv-year.
      cv_total = cv_total + <ls_gd_iout>-req_qty.
    ENDLOOP.
  ELSE. "not found perhaps.neways, nothing to compute.
    CLEAR: cv_total.
  ENDIF.

ENDFORM.                    "get_total_12mos_frombapi

*&---------------------------------------------------------------------*
*&      Form  get_blocked_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_ALV               text
*      -->UR_REPTDATE          text
*      -->UR_REPTDATE_BLOCKED  text
*      -->CV_TOTAL             text
*----------------------------------------------------------------------*
FORM get_blocked_qty USING    us_alv                 TYPE ty_alv_display
                              ur_reptdate            TYPE tt_reptd_range
                              ur_reptdate_blocked    TYPE tt_reptd_range
                     CHANGING cv_total     TYPE bdmng.

  DATA: ls_gd_item_out  TYPE bapisitmeo,
        lt_gd_item_out  LIKE STANDARD TABLE OF ls_gd_item_out.

  DATA: lt_gd_ret    TYPE STANDARD TABLE OF bapireturn1.

  FIELD-SYMBOLS: <ls_gd_iout> LIKE LINE OF lt_gd_item_out.

  CLEAR: cv_total.


  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = us_alv-matnr
      plant            = us_alv-berid
      requirementstype = space
      version          = us_alv-vrsn
      reqmtsplannumber = space
*     MRP_AREA         =
*     MATERIAL_EVG     =
    TABLES
      requirements_out = lt_gd_item_out
      return           = lt_gd_ret.

  IF lt_gd_ret IS INITIAL. "Success.
    CLEAR: cv_total.
    LOOP AT lt_gd_item_out ASSIGNING <ls_gd_iout>.
      CHECK <ls_gd_iout>-req_date+0(4) EQ us_alv-year.
      CHECK <ls_gd_iout>-req_date+0(6) IN ur_reptdate_blocked.
      cv_total = cv_total + <ls_gd_iout>-req_qty.
    ENDLOOP.
  ELSE. "not found perhaps.neways, nothing to compute.
    CLEAR: cv_total.
  ENDIF.

ENDFORM.                    "get_blocked_qty


*&---------------------------------------------------------------------*
*&      Form  prelim_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_UPLOAD  text
*      -->CV_STATUS  text
*      -->CV_MESSAGE text
*----------------------------------------------------------------------*
FORM prelim_check     USING    ut_upload  LIKE gt_upload
                      CHANGING cv_status  TYPE char01
                               cv_message TYPE char255.

  CLEAR: cv_status, cv_message.

  "-- Start - GDIMALIWAT 07/21/2014 - TN#63434
  DATA: lv_matnr TYPE matnr.
  "-- End - GDIMALIWAT 07/21/2014 - TN#63434
  FIELD-SYMBOLS: <ls_upload> LIKE LINE OF ut_upload.

  "-- check contents of upload if valid.
  READ TABLE ut_upload ASSIGNING <ls_upload>
    WITH KEY rdate+0(4) = p_rdate+0(4) "sy-datum+0(4)
             vrsn       = '00'.
  IF sy-subrc IS INITIAL.
    "-- Start - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing V00
    SELECT SINGLE matnr
    INTO lv_matnr
    FROM zind_req
    WHERE matnr eq <ls_upload>-MATNR
      AND werks eq <ls_upload>-BERID
      AND versb eq '00'.

    IF sy-subrc eq 0.
      MOVE:   'E'                    TO     cv_status,
              'Planned requirements for year &1 is already final, cannot upload.'
                                     TO     cv_message.
      REPLACE ALL OCCURRENCES OF '&1' IN cv_message WITH <ls_upload>-rdate+0(4).
      RETURN.
    ELSE.
      MOVE:   'S'                    TO     cv_status,
              'Preliminary validation of upload file passed.'
                                     TO     cv_message.
    ENDIF.
*    MOVE:   'E'                    TO     cv_status,
*            'Planned requirements for year &1 is already final, cannot upload.'
*                                   TO     cv_message.
*    REPLACE ALL OCCURRENCES OF '&1' IN cv_message WITH <ls_upload>-rdate+0(4).
*    RETURN.
    "-- End - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing V00
  ELSE.
    MOVE:   'S'                    TO     cv_status,
            'Preliminary validation of upload file passed.'
                                   TO     cv_message.
  ENDIF.

  "zxzx add validation for upload files for the next 2 years.

ENDFORM.                    "prelim_check


*&---------------------------------------------------------------------*
*&      Form  really_wanna_deact
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_REQ     text
*      -->CV_YES     text
*----------------------------------------------------------------------*
FORM really_wanna_deact USING    us_req TYPE ty_alv_real
                        CHANGING cv_yes TYPE char01.

  DATA: lt_pbed TYPE STANDARD TABLE OF pbed.

  FIELD-SYMBOLS: <ls_pbed> TYPE pbed.

  DATA: lv_nextyear TYPE sy-datum.

  DATA: ls_reptdate LIKE LINE OF gr_report_date.

  READ TABLE gr_report_date INTO ls_reptdate INDEX 1.

  lv_nextyear+0(4) = ls_reptdate-low+0(4) + 1.

  SELECT *
    FROM pbed
    INNER JOIN pbim
      ON pbed~bdzei = pbim~bdzei
    INTO CORRESPONDING FIELDS OF TABLE lt_pbed
    WHERE pbim~matnr = us_req-matnr
      AND pbim~werks = us_req-berid
      AND pbim~versb  = '00'
      .
  "-- check if item exist for next year.
  cv_yes = 'X'.
  LOOP AT lt_pbed ASSIGNING <ls_pbed>
  WHERE pdatu+0(4) = lv_nextyear+0(4).
    cv_yes = space.
    EXIT.
  ENDLOOP.


ENDFORM.                    "really_wanna_deact

*&---------------------------------------------------------------------*
*&      Form  CHECK_PROCEED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_ALV_DISPLAY  text
*      -->CV_STATUS       text
*      -->CV_MESSAGE      text
*----------------------------------------------------------------------*
FORM check_proceed  USING    ut_alv_display  LIKE gt_alv_display
                    CHANGING cv_status
                             cv_message.

  FIELD-SYMBOLS: <ls_alv> LIKE LINE OF ut_alv_display.

  CLEAR: cv_status, cv_message.

  READ TABLE ut_alv_display ASSIGNING <ls_alv>
  WITH KEY iserror = 'E'.
  IF sy-subrc IS INITIAL.
    cv_status = <ls_alv>-iserror.
    cv_message = <ls_alv>-e_mess.
    RETURN.
  ELSE.

  ENDIF.

  READ TABLE ut_alv_display ASSIGNING <ls_alv>
  WITH KEY iserror = 'Z'.
  IF sy-subrc IS INITIAL.
    cv_status = <ls_alv>-iserror.
    cv_message = <ls_alv>-e_mess.
  ELSE.

  ENDIF.

ENDFORM.                    " CHECK_PROCEED


*&---------------------------------------------------------------------*
*&      Form  complete_v1_wholeyear_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_EXIST_PBIM  text
*      -->UT_EXIST_PBED  text
*      -->UR_REPT_DATE   text
*      -->CT_UPLOAD_SUM  text
*----------------------------------------------------------------------*
FORM complete_v1_wholeyear_upload
    USING     ut_exist_pbim    TYPE tt_pbim_ex
              ut_exist_pbed    TYPE tt_pbed_ex
              ur_rept_date     TYPE tt_reptd_range
    CHANGING  ct_upload_sum    TYPE tt_alv_upsum.

  DATA: ls_upload_sum    LIKE LINE OF ct_upload_sum.

  DATA: ls_material_list LIKE LINE OF ct_upload_sum,
        lt_material_list LIKE STANDARD TABLE OF ls_material_list.

  DATA: lt_bapiout TYPE STANDARD TABLE OF bapisitmeo.
  DATA: lt_ret1    TYPE STANDARD TABLE OF bapireturn1.

  DATA: lv_numc2   TYPE numc2,
        lv_mon3    TYPE numc06.

  FIELD-SYMBOLS: <ls_upload_sum>  LIKE LINE OF ct_upload_sum,
                 <ls_upload_read> LIKE LINE OF ct_upload_sum,
                 <ls_matlist>     LIKE LINE OF lt_material_list.

  FIELD-SYMBOLS: <ls_reptdate>    LIKE LINE OF ur_rept_date.

  FIELD-SYMBOLS: <ls_bapiout>    LIKE LINE OF lt_bapiout.


  CHECK ct_upload_sum IS NOT INITIAL.

  "-- build year.
  READ TABLE ur_rept_date ASSIGNING <ls_reptdate> INDEX 1.

  "-- build list of materials from upload file.
  lt_material_list = ct_upload_sum.
  SORT lt_material_list
    BY matnr
       berid
       vrsn.
  DELETE ADJACENT DUPLICATES FROM lt_material_list COMPARING
    matnr berid vrsn.

  "-- complete entries
  LOOP AT lt_material_list ASSIGNING <ls_matlist>
*    WHERE vrsn = '01'
    .

    CLEAR: lt_bapiout, lt_ret1.
    CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
      EXPORTING
        material         = <ls_matlist>-matnr
        plant            = <ls_matlist>-berid
        requirementstype = space
*       version          = '00'
        version          = <ls_matlist>-vrsn
        reqmtsplannumber = space
*       MRP_AREA         =
*       MATERIAL_EVG     =
      TABLES
        requirements_out = lt_bapiout
        return           = lt_ret1.

    CHECK lt_ret1 IS INITIAL.

    SORT lt_bapiout BY date_type req_date material plant version vers_activ.

    "-- build 12 montsh
    DO 12 TIMES.

      "-- build per month,
      CLEAR: lv_numc2, lv_mon3.
      lv_numc2 = sy-index.
      CONCATENATE <ls_reptdate>-low+0(4) lv_numc2 INTO lv_mon3.

      READ TABLE ct_upload_sum ASSIGNING <ls_upload_read>
      WITH KEY matnr = <ls_matlist>-matnr
               berid = <ls_matlist>-berid
*               vrsn  = '01'
               vrsn  = <ls_matlist>-vrsn
               rdate = lv_mon3.
      IF sy-subrc IS INITIAL.
        "leave as is.
      ELSE.
        "add new line to ct_upload_sum
        CLEAR: ls_upload_sum.
        MOVE <ls_matlist> TO ls_upload_sum.

        ls_upload_sum-isupload    =   'X'.
        ls_upload_sum-year        =   lv_mon3+0(4).

*        ls_upload_sum-matnr       =   <ls_matlist>-matnr.
*        ls_upload_sum-berid       =   <ls_matlist>-berid.
        ls_upload_sum-rdate       =   lv_mon3.
*        ls_upload_sum-vrsn        =   '01'.
        ls_upload_sum-vrsn        =   <ls_matlist>-vrsn.

        READ TABLE lt_bapiout ASSIGNING <ls_bapiout>
        WITH KEY date_type     = '3'
                 req_date+0(6) = lv_mon3.
        IF sy-subrc IS INITIAL.
          ls_upload_sum-rqty = <ls_bapiout>-req_qty.     "no item in upload, item exist in standard - keep standard
        ELSE.
          ls_upload_sum-rqty = 0.                        "no item in upload AND in standard - zero out
* >> ADD OPacalso - Drop down v00 for v01 if no line in upload file.
          "zxzx
* << ADD OPacalso - Drop down if no line in upload file.
        ENDIF.

********quick fix test test - week entries exist in prod*****************
        "-- add weekly.
        DATA: lv_month_tmp TYPE numc06.

        LOOP AT lt_bapiout ASSIGNING <ls_bapiout>
        WHERE req_date+0(6) EQ lv_mon3
          AND date_type     EQ '2'.  "2 means in weeks.

          CLEAR: lv_month_tmp.
          CALL FUNCTION 'VELO03_DATE_TO_WEEK_OR_MONTH'
            EXPORTING
              date_iv        = <ls_bapiout>-req_date
            IMPORTING
*             WEEK_EV        =
              month_ev       = lv_month_tmp+4(2)
              year_ev        = lv_month_tmp+0(4)
*             MONTH_NAMES_ES =
            EXCEPTIONS
              date_invalid   = 1
              others_error   = 2
              OTHERS         = 3.
          IF sy-subrc IS INITIAL.

            "-- check if date is in current month of processing.
            IF lv_month_tmp EQ lv_mon3.
              ls_upload_sum-rqty = ls_upload_sum-rqty  +  <ls_bapiout>-req_qty.
            ELSE.
              ...
            ENDIF.

          ENDIF.

        ENDLOOP.


*^end***quick fix test test - week entries exist in prod**************


        APPEND ls_upload_sum TO ct_upload_sum.

      ENDIF.

    ENDDO.

  ENDLOOP.

ENDFORM.                    "complete_v1_wholeyear_upload


*&---------------------------------------------------------------------*
*&      Form  ADJUST_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LS_ALV>  text
*----------------------------------------------------------------------*
FORM adjust_qty  USING    uv_variance TYPE bdmng
                          ur_reptdate TYPE tt_reptd_range
                 CHANGING cs_alv      TYPE ty_alv_display
                          cv_excess   TYPE bdmng.

  DATA: lv_ctr   TYPE numc2.

  DATA: lv_month TYPE numc06.

  DATA: lv_numc2 TYPE numc2,
        lv_fname TYPE char20.

  FIELD-SYMBOLS: <ls_mon> LIKE cs_alv-m01.

  "-- init
  lv_ctr = 12.
  cv_excess = uv_variance.

  DO 12 TIMES.

    CHECK cv_excess GT 0.

    "-- build month.
    lv_month+0(4) = cs_alv-year.
    lv_month+4(2) = lv_ctr.

    CHECK lv_month IN ur_reptdate.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ctr
      IMPORTING
        output = lv_numc2.
    CONCATENATE 'M' lv_numc2 INTO lv_fname.

    ASSIGN COMPONENT lv_fname OF STRUCTURE cs_alv TO <ls_mon>.
    cv_excess      = cv_excess - <ls_mon>-value.
    IF cv_excess GE 0.
      <ls_mon>-value = 0.
    ELSE.
      <ls_mon>-value = cv_excess * -1.
      cv_excess = 0.
    ENDIF.



    lv_ctr = lv_ctr - 1.

  ENDDO.

ENDFORM.                    " ADJUST_QTY

*&---------------------------------------------------------------------*
*&      Form  COMPLETE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_ALV_REAL  text
*----------------------------------------------------------------------*
FORM complete_display     USING    ur_reptdate TYPE tt_reptd_range
                          CHANGING ct_alv_real LIKE gt_alv_display.

  DATA: ls_matlist LIKE LINE OF ct_alv_real,
        lt_matlist LIKE STANDARD TABLE OF ls_matlist.

  DATA: ls_req_out TYPE bapisitmeo,
        lt_req_out LIKE STANDARD TABLE OF ls_req_out.

  DATA: lt_req_out_collect LIKE STANDARD TABLE OF ls_req_out.

  DATA: lt_bapiret TYPE STANDARD TABLE OF bapireturn1.

  FIELD-SYMBOLS: <ls_alv> LIKE LINE OF ct_alv_real,
                 <ls_matlist> LIKE LINE OF lt_matlist,
                 <ls_req_out> LIKE LINE OF lt_req_out.



*  "-- build matlist
*  lt_matlist = ct_alv_real.
*  SORT lt_matlist BY matnr berid vrsn.
*  DELETE ADJACENT DUPLICATES FROM lt_matlist COMPARING matnr berid vrsn.
*
*  LOOP AT lt_matlist ASSIGNING <ls_matlist>.
*
*
*
*  ENDLOOP.

  DATA: lv_numc6 TYPE numc06,
        lv_numc2 TYPE numc2.
  DATA: lv_fieldname TYPE char20.


  LOOP AT ct_alv_real ASSIGNING <ls_alv>
*  WHERE vrsn = '01'
  .

    FIELD-SYMBOLS: <ls_month> LIKE <ls_alv>-m01.

    CLEAR: lt_req_out.

    CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
      EXPORTING
        material         = <ls_alv>-matnr
        plant            = <ls_alv>-berid
        requirementstype = space
        version          = <ls_alv>-vrsn
        reqmtsplannumber = space
*       MRP_AREA         =
*       MATERIAL_EVG     =
      TABLES
        requirements_out = lt_req_out
        return           = lt_bapiret.

    CLEAR lt_req_out_collect.
    LOOP AT lt_req_out ASSIGNING <ls_req_out>.
      CLEAR: ls_req_out.
      MOVE: <ls_req_out>-req_date+0(6) TO ls_req_out-req_date+0(6),
            <ls_req_out>-req_qty       TO ls_req_out-req_qty,
            '3'                        TO ls_req_out-date_type.

      COLLECT ls_req_out INTO lt_req_out_collect.
    ENDLOOP.


    DO 12 TIMES.

      "--build months
      lv_numc6+0(4)  =  <ls_alv>-year.
      lv_numc2       = sy-index.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_numc2
        IMPORTING
          output = lv_numc6+4(2).

      CHECK NOT lv_numc6 IN ur_reptdate.

      IF <ls_alv>-vrsn = '00'.
        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out>
        WITH KEY req_date+0(6) = lv_numc6.
        IF sy-subrc IS INITIAL.
          CLEAR: lv_fieldname.
          CONCATENATE 'M' lv_numc6+4(2) INTO lv_fieldname.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_alv> TO <ls_month>.

******QUICK FIX*******************************************************
          "-- check if exist in upload
          SORT gt_alv_upsum BY matnr berid rdate.
          READ TABLE gt_alv_upsum TRANSPORTING NO FIELDS
          WITH KEY matnr = <ls_alv>-matnr
                   berid = <ls_alv>-berid
                   rdate = lv_numc6
                   vrsn  = <ls_alv>-vrsn.
          IF sy-subrc IS INITIAL.
            <ls_month>-value = <ls_month>-value.
          ELSE.
            <ls_month>-value = <ls_req_out>-req_qty.
          ENDIF.
*^end*QUICK FIX*******************************************************
*          IF <ls_month>-value IS INITIAL.
*            <ls_month>-value = <ls_req_out>-req_qty.
*          ELSE.
*            <ls_month>-value = <ls_month>-value.
*          ENDIF.
        ENDIF.
      ELSEIF <ls_alv>-vrsn = '01'.
        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out>
        WITH KEY req_date+0(6) = lv_numc6.
        IF sy-subrc IS INITIAL.
          CLEAR: lv_fieldname.
          CONCATENATE 'M' lv_numc6+4(2) INTO lv_fieldname.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_alv> TO <ls_month>.
          <ls_month>-value = <ls_req_out>-req_qty.
        ENDIF.
      ENDIF.

    ENDDO.

  ENDLOOP.


  "-- drop v00 to v01 for month of Jan and Feb if current month (sy-datum) is january.
*  IF sy-datum+4(2) = '01'.
  IF p_rdate+4(2) = '01'.

    LOOP AT ct_alv_real ASSIGNING <ls_alv>
    WHERE vrsn = '01'.

      CLEAR: lt_req_out, lt_bapiret.
      CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'  "get version 00 of v01
        EXPORTING
          material         = <ls_alv>-matnr
          plant            = <ls_alv>-berid
          requirementstype = space
          version          = '00'
          reqmtsplannumber = space
*         MRP_AREA         =
*         MATERIAL_EVG     =
        TABLES
          requirements_out = lt_req_out
          return           = lt_bapiret.

      CHECK lt_bapiret IS INITIAL.

      CLEAR lt_req_out_collect.
      LOOP AT lt_req_out ASSIGNING <ls_req_out>.
        CLEAR: ls_req_out.
        MOVE: <ls_req_out>-req_date+0(6) TO ls_req_out-req_date+0(6),
              <ls_req_out>-req_qty       TO ls_req_out-req_qty,
              '3'                        TO ls_req_out-date_type.

        COLLECT ls_req_out INTO lt_req_out_collect.
      ENDLOOP.

      IF <ls_alv>-m01-value EQ '0' OR <ls_alv>-m01-value IS INITIAL.
        "-- build date.
        CLEAR: lv_numc6.
        lv_numc6+0(4) = <ls_alv>-year.
        lv_numc6+4(2) = '01'.          "january

        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out>
        WITH KEY req_date+0(6) = lv_numc6.
        IF sy-subrc IS INITIAL.
          <ls_alv>-m01-value = <ls_req_out>-req_qty.
        ENDIF.
      ELSE.
        ... "has contents.. bypass
      ENDIF.

      IF <ls_alv>-m02-value EQ '0' OR <ls_alv>-m02-value IS INITIAL.
        "-- build date.
        CLEAR: lv_numc6.
        lv_numc6+0(4) = <ls_alv>-year.
        lv_numc6+4(2) = '02'.          "feb

        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out>
        WITH KEY req_date+0(6) = lv_numc6.
        IF sy-subrc IS INITIAL.
          <ls_alv>-m02-value = <ls_req_out>-req_qty.
        ENDIF.
      ELSE.
        ... "has contents.. bypass
      ENDIF.


    ENDLOOP.

  ENDIF.

  DATA: lv_next_year TYPE numc4.
  CLEAR: lv_next_year .
*  lv_next_year = sy-datum+0(4) + 1.
  lv_next_year = p_rdate+0(4) + 1.

  "-- drop v00 to v01 for month of Jan if current month (sy-datum) is december previous year.
*  IF sy-datum+4(2) = '12'.
  IF p_rdate+4(2) = '12'.

    LOOP AT ct_alv_real ASSIGNING <ls_alv>
    WHERE vrsn = '01'
      AND year = lv_next_year.  "upload  items for next year.

      CLEAR: lt_req_out, lt_bapiret.
      CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'  "get version 00 of v01
        EXPORTING
          material         = <ls_alv>-matnr
          plant            = <ls_alv>-berid
          requirementstype = space
          version          = '00'
          reqmtsplannumber = space
*         MRP_AREA         =
*         MATERIAL_EVG     =
        TABLES
          requirements_out = lt_req_out
          return           = lt_bapiret.

      CHECK lt_bapiret IS INITIAL.

      CLEAR lt_req_out_collect.
      LOOP AT lt_req_out ASSIGNING <ls_req_out>.
        CLEAR: ls_req_out.
        MOVE: <ls_req_out>-req_date+0(6) TO ls_req_out-req_date+0(6),
              <ls_req_out>-req_qty       TO ls_req_out-req_qty,
              '3'                        TO ls_req_out-date_type.

        COLLECT ls_req_out INTO lt_req_out_collect.
      ENDLOOP.

      IF <ls_alv>-m01-value EQ '0' OR <ls_alv>-m01-value IS INITIAL.
        "-- build date.
        CLEAR: lv_numc6.
        lv_numc6+0(4) = <ls_alv>-year.
        lv_numc6+4(2) = '01'.          "january

        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out>
        WITH KEY req_date+0(6) = lv_numc6.
        IF sy-subrc IS INITIAL.
          <ls_alv>-m01-value = <ls_req_out>-req_qty.
        ENDIF.
      ELSE.
        ... "has contents.. bypass
      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.                    " COMPLETE_DISPLAY

* 1. Change values of Previous Months to Consumed.
* 2. Change values of frozen months to V00 no matter what.
FORM consumed_overwrite  USING    ur_blocked  LIKE gr_report_date_blocked
                                  ur_prev     LIKE gr_report_date_prev
                                  VALUE(ut_consumed) LIKE gt_consumed
                         CHANGING ct_alv      LIKE gt_alv_display.

  DATA: ls_alv LIKE LINE OF ct_alv.

  DATA: lv_numc2 TYPE numc2,
        lv_numc6 TYPE numc06,
        lv_fname TYPE char20.

  DATA: ls_req_out TYPE bapisitmeo,
        lt_req_out LIKE STANDARD TABLE OF ls_req_out.

  DATA: lt_req_out_collect LIKE STANDARD TABLE OF ls_req_out.

  DATA: lt_bapiret TYPE STANDARD TABLE OF bapireturn1.

  FIELD-SYMBOLS: <ls_alv> LIKE LINE OF ct_alv,
                 <ls_consumed_read> LIKE LINE OF ut_consumed,
                 <ls_req_out> LIKE LINE OF lt_req_out,
                 <ls_req_out_read> LIKE LINE OF lt_req_out_collect.

  FIELD-SYMBOLS: <ls_month> LIKE ls_alv-m01.



  LOOP AT ct_alv ASSIGNING <ls_alv>
  WHERE vrsn = '01'.

    CLEAR: lt_req_out_collect, lt_bapiret.
    CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
      EXPORTING
        material         = <ls_alv>-matnr
        plant            = <ls_alv>-berid
        requirementstype = space
        version          = '00'
        reqmtsplannumber = space
      TABLES
        requirements_out = lt_req_out
        return           = lt_bapiret.

    CLEAR lt_req_out_collect.
    LOOP AT lt_req_out ASSIGNING <ls_req_out>.
      CLEAR: ls_req_out.
      MOVE: <ls_req_out>-req_date+0(6) TO ls_req_out-req_date+0(6),
            <ls_req_out>-req_qty       TO ls_req_out-req_qty,
            '3'                        TO ls_req_out-date_type.

      COLLECT ls_req_out INTO lt_req_out_collect.
    ENDLOOP.



    DO 12 TIMES.

      "-- build date.
      MOVE: sy-index TO lv_numc2.
      PERFORM build_month_fname
        USING    lv_numc2
                 <ls_alv>-year
        CHANGING lv_numc6
                 lv_fname.

      "-- check if current fieldname is under blocked date.
      IF lv_numc6 IN ur_blocked.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_alv> TO <ls_month>.
        "-- replace content of new-plan(ct_alv)
        READ TABLE lt_req_out_collect ASSIGNING <ls_req_out_read>
        WITH KEY req_date+0(6)  =  lv_numc6.
        IF sy-subrc IS INITIAL.
          <ls_month>-value = <ls_req_out_read>-req_qty.
        ELSE.
          <ls_month>-value = 0. "zero out, no plan V00
        ENDIF.

      ELSE. "IF lv_numc6 IN ur_blocked.
        ... "not in blocked.

        "-- check if in previous.
        IF lv_numc6 IN ur_prev.
          ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_alv> TO <ls_month>.
          SORT ut_consumed BY matnr werks perxx.
          READ TABLE ut_consumed ASSIGNING <ls_consumed_read>
          WITH KEY matnr       =   <ls_alv>-matnr
                   werks       =   <ls_alv>-berid
                   perxx       =   lv_numc6
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <ls_month>-value = <ls_consumed_read>-enmng.
          ELSE.
            <ls_month>-value = 0. "no consumed, zero out
          ENDIF.

        ELSE.
          ... "not in prev. no business here
        ENDIF.
      ENDIF.

    ENDDO.

  ENDLOOP.

ENDFORM.                    "consumed_overwrite

*&---------------------------------------------------------------------*
*&      Form  build_month_fname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_MONTH   text
*      -->UV_YEAR    text
*      -->CV_YYYYMM  text
*      -->CV_FNAME   text
*----------------------------------------------------------------------*
FORM build_month_fname USING    uv_month  TYPE numc2
                                uv_year   TYPE char04
                       CHANGING cv_yyyymm TYPE numc06
                                cv_fname  TYPE char20
                             .

  CLEAR: cv_yyyymm, cv_fname.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = uv_month
    IMPORTING
      output = cv_yyyymm+4(2).

  MOVE: uv_year TO cv_yyyymm+0(4).

  MOVE: 'M'            TO cv_fname+0(1),
        cv_yyyymm+4(2) TO cv_fname+1(2)
*        'VALUE'        TO cv_fname+3(5)
        .

ENDFORM.                    "build_month_fname