*&---------------------------------------------------------------------*
*& Report     : ZRISU_AMORTIZATION_SETUP
*& DESCRIPTION: Program for Massive posting of amortization AR
*&              (ISU-AR) using BAPI
*&---------------------------------------------------------------------*
*& Created by: Glenn Dimaliwat
*& Created On: August 6, 2012
*& Reference : Transaction FPE1 (Post AR Document in ISU)
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2012/08/06|GDIMALIWAT|      17540| Initial Development              *
*& 2012/08/17|GDIMALIWAT|      17540| Added Installment Plan Creation  *
*&---------------------------------------------------------------------*
REPORT  zrisu_amortization_setup MESSAGE-ID zisu.

TYPE-POOLS: abap, slis.
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_data_s,
         doc_date   TYPE char10,      "Document Date
         doc_type   TYPE blart_kk,    "Document Type
         post_date  TYPE char10,      "Posting Date
         currency   TYPE blwae_kk,    "Currency (WAERS)
         ref_key    TYPE xblnr_kk,    "Reference Key
         recon_key  TYPE fikey_kk,    "Recon key for GL
         clearing   TYPE augrd_kk,    "Clearing Reason
         buspart    TYPE gpart_kk,    "Business partner
         "--Line item for creating Business Item List
         cocode     TYPE bukrs,       "Company Code
         duedate    TYPE char10,      "Due Date for Net Pay (FAEDN_KK)
         cont_acct  TYPE vkont_kk,    "Utility Contract Account
*         main_trans TYPE hvorg_kk,    "Main transaction for Line Item
*         sub_trans  TYPE tvorg_kk,    "Subtransaction for Document Item
         amt_trans  TYPE char18,      "Amount in Transaction Currency with +/- Sign (BETRW_KK)
         contract   TYPE vertrag,     "Reference Contract No. (VTREF_KK)
         msgtyp     TYPE icon_d,      "Shows icon for Error, Information, Warning, Successful upload
         remarks    TYPE char100,      "Remarks (after uploading)
* added by GDIMALIWAT
         text1      TYPE char25,      "Text
         text2      TYPE char12,      "Text
         text3      TYPE char11,      "Text
         recon_key2  TYPE fikey_kk,   "Recon key for installment plan
         start_date  TYPE char10, "Installment plan start date
         inst_number TYPE anzrate_kk, "Number of installments
       END OF ty_data_s,
       ty_data_t    TYPE STANDARD TABLE OF ty_data_s,
       BEGIN OF ty_cont_acct_s,
         erdat      TYPE erdat,       "Creation Date
         vkont      TYPE vkont_kk,    "Utility Contract Account
       END OF ty_cont_acct_s,
       BEGIN OF ty_ref_cacct_s,
         vkonto     TYPE vkont_kk,    "Utility Contract Account
         vertrag    TYPE vertrag,     "Contract Number
       END OF ty_ref_cacct_s.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_filetable  TYPE filetable,
      gt_data       TYPE STANDARD TABLE OF ty_data_s,
      gt_exception  TYPE STANDARD TABLE OF ty_data_s.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gv_win_title  TYPE string,
      gv_rc         TYPE i.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_data>    TYPE ty_data_s,
               <fs_filetab> TYPE file_table.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk00 WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-001.
PARAMETERS: rb_servr RADIOBUTTON GROUP gr1 DEFAULT 'X',
            rb_local RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK blk01.
PARAMETERS: pa_fpath   TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk00.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_fpath.
  "--Open file dialog window
  gv_win_title = text-002.
  cl_gui_frontend_services=>file_open_dialog( EXPORTING  window_title            = gv_win_title
                                                         default_extension       = 'TXT'
                                                         default_filename        = ' '
                                                         initial_directory       = ' '
                                              CHANGING   file_table              = gt_filetable
                                                         rc                      = gv_rc
                                              EXCEPTIONS file_open_dialog_failed = 1
                                                         cntl_error              = 2
                                                         error_no_gui            = 3
                                                         not_supported_by_gui    = 4
                                                         OTHERS                  = 5 ).
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE gt_filetable ASSIGNING <fs_filetab> INDEX 1.
    IF sy-subrc EQ 0.
      pa_fpath = <fs_filetab>-filename.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-ucomm EQ 'ONLI'.
    IF rb_local EQ abap_true.
      PERFORM get_file_local.
    ELSE.
      PERFORM get_file_server.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  "--Macro definition to Convert alphanumeric to fill-up space
  DEFINE mc_convert_input.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &2.
  END-OF-DEFINITION.
  "--Macro definition to convert date to internal format
  DEFINE mc_convert_date_internal.
    call function 'CONVERT_DATE_TO_INTERNAL'
      exporting
        date_external = &1
      importing
        date_internal = &2.
  END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM process_data.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM display_output.

*&---------------------------------------------------------------------*
*&      Form  get_file_local
*&---------------------------------------------------------------------*
*       Get file from Local Drive
*----------------------------------------------------------------------*
FORM get_file_local.
  DATA: lt_data      TYPE TABLE OF string,
        lv_filename  TYPE string.
  FIELD-SYMBOLS: <fv_data> TYPE string.

  lv_filename = pa_fpath.
  cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = lv_filename
                                                   has_field_separator     = abap_true
                                        CHANGING   data_tab                = lt_data
                                        EXCEPTIONS file_open_error         = 1
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
                                                   not_supported_by_gui    = 17
                                                   error_no_gui            = 18
                                                   OTHERS                  = 19 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lt_data ASSIGNING <fv_data>.
      PERFORM transfer_record USING <fv_data>.
    ENDLOOP.
    UNASSIGN <fv_data>.
  ENDIF.
ENDFORM.                    " get_file_local
*&---------------------------------------------------------------------*
*&      Form  get_file_server
*&---------------------------------------------------------------------*
*       Get dataset file from the specified filepath
*----------------------------------------------------------------------*
FORM get_file_server .
  DATA: lv_mes  TYPE char50,
        lv_data TYPE string.
  OPEN DATASET pa_fpath FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_mes.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET pa_fpath INTO lv_data.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      PERFORM transfer_record USING lv_data.
    ENDDO.
    CLOSE DATASET pa_fpath.
  ELSE.
    MESSAGE e007 WITH lv_mes.
  ENDIF.
ENDFORM.                    " get_file_server
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_RECORD
*&---------------------------------------------------------------------*
*       Transfer record from file to internal table
*----------------------------------------------------------------------*
*      -->PV_DATA  Line of data
*----------------------------------------------------------------------*
FORM transfer_record  USING pv_data TYPE string.
  APPEND INITIAL LINE TO gt_data ASSIGNING <fs_data>.
  SPLIT pv_data AT zcl_const_isu=>co_pipe
                INTO <fs_data>-doc_date <fs_data>-doc_type  <fs_data>-post_date <fs_data>-currency
                     <fs_data>-ref_key  <fs_data>-recon_key <fs_data>-clearing  <fs_data>-buspart
                     "--Line item for creating Business Item List
                     <fs_data>-cocode    <fs_data>-duedate   <fs_data>-cont_acct  <fs_data>-amt_trans
                     <fs_data>-text1  <fs_data>-text2  <fs_data>-text3
                     "--Line item for creating Installment Plan
                     <fs_data>-recon_key2 <fs_data>-start_date  <fs_data>-inst_number.
  mc_convert_input: <fs_data>-buspart <fs_data>-buspart.
ENDFORM.                    " transfer_record
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Prepare and process data for uploading
*----------------------------------------------------------------------*
FORM process_data.
  DATA: ls_return  TYPE bapiret2,
        lv_opbel   TYPE opbel_kk.

  SORT gt_data   BY table_line.
  "--Process records
  LOOP AT gt_data ASSIGNING <fs_data>.
    CLEAR ls_return.
    "--Check if recon key exists
    IF <fs_data>-recon_key IS NOT INITIAL.
      PERFORM check_recon_key USING <fs_data>-recon_key CHANGING ls_return.
      IF ls_return-type CA 'EA'.
        "--Create exception if with Error or  program Abort
        PERFORM create_exception USING <fs_data> ls_return-message.
      ELSE.
        CLEAR lv_opbel.
        PERFORM post_account_document USING <fs_data> ls_return-message lv_opbel.
        IF lv_opbel IS NOT INITIAL.
          PERFORM create_installment_plan USING <fs_data> ls_return-message lv_opbel.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM create_exception USING <fs_data> ls_return-message.
    ENDIF.
  ENDLOOP.
ENDFORM.              "Process_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_RECON_KEY
*&---------------------------------------------------------------------*
*       Check if Recon Key exists else create the recon key
*----------------------------------------------------------------------*
*      -->PV_RECON_KEY  Reconciliation Key
*      <--PS_BAPIRET    BAPI Return message details
*----------------------------------------------------------------------*
FORM check_recon_key  USING    pv_recon_key TYPE fikey_kk
                      CHANGING ps_bapiret   TYPE bapiret2.
  DATA: lv_recon_key TYPE fikey_kk.
  "--Check if the Reconciliation key exists
  CALL FUNCTION 'BAPI_CTRACRECKEY_EXISTCHECK'
    EXPORTING
      reconciliationkey = pv_recon_key
    IMPORTING
      return            = ps_bapiret.
  IF ps_bapiret IS NOT INITIAL.
    "--If error found, recon key does not exist then create
    CLEAR ps_bapiret.
    CALL FUNCTION 'BAPI_CTRACRECKEY_CREATE'
      EXPORTING
        newreconciliationkey = pv_recon_key
      IMPORTING
        reconciliationkey    = lv_recon_key
        return               = ps_bapiret.
    IF lv_recon_key IS NOT INITIAL AND ( ps_bapiret-type EQ 'S' OR ps_bapiret-type = space ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      MESSAGE s039 INTO ps_bapiret-message WITH lv_recon_key.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_city
*&---------------------------------------------------------------------*
*&      Form  CREATE_EXCEPTION
*&---------------------------------------------------------------------*
*       Proceed to create exception if no recon key in file or error in
*       creation of recon key
*----------------------------------------------------------------------*
*      -->PS_DATA       Line of Data
*      -->PV_RETMSG     BAPI Return message
*----------------------------------------------------------------------*
FORM create_exception  USING ps_data    TYPE ty_data_s
                             pv_retmsg  TYPE bapi_msg.
  "--Create exception per line item
  ps_data-msgtyp  = zcl_const_isu=>co_error.
  IF pv_retmsg IS NOT INITIAL.
    ps_data-remarks = pv_retmsg.
  ELSE.
    ps_data-remarks = text-004.
  ENDIF.
  APPEND ps_data TO gt_exception.
ENDFORM.                    " CREATE_EXCEPTION
*&---------------------------------------------------------------------*
*&      Form  POST_ACCOUNT_DOCUMENT
*&---------------------------------------------------------------------*
*       Proceed posting of acct document
*----------------------------------------------------------------------*
*      -->PS_DATA       Line of Data
*      -->PV_RETMSG     BAPI Return message
*----------------------------------------------------------------------*
FORM post_account_document  USING ps_data    TYPE ty_data_s
                                  pv_retmsg  TYPE bapiret2-message
                                  pv_opbel   TYPE opbel_kk.


  DATA: ls_dochdr    TYPE bapidfkkko,
        ls_return    TYPE bapiret2,
        lv_docno     TYPE opbel_kk,
        lv_lines     TYPE i,
        lt_partitem  TYPE re_t_xa_bapidfkkop,
        lt_cont_acct TYPE SORTED TABLE OF ty_cont_acct_s WITH NON-UNIQUE KEY table_line,
        lt_ref_cacct TYPE SORTED TABLE OF ty_ref_cacct_s WITH NON-UNIQUE KEY table_line.
  FIELD-SYMBOLS: <fs_cacct>  TYPE ty_cont_acct_s,
                 <fs_ref>    TYPE ty_ref_cacct_s,
                 <fs_prtitm> TYPE bapidfkkop.

  "--Populate account document header
  ls_dochdr-fikey          = ps_data-recon_key.
  ls_dochdr-appl_area      = 'R'.               "Utility Company
  ls_dochdr-doc_type       = ps_data-doc_type.
  ls_dochdr-doc_source_key = '01'.              "Manual Posting
  ls_dochdr-created_by     = sy-uname.
  ls_dochdr-entry_date     = sy-datum.
  ls_dochdr-entry_time     = sy-uzeit.
  mc_convert_date_internal: ps_data-doc_date  ls_dochdr-doc_date,
                            ps_data-post_date ls_dochdr-post_date.
  ls_dochdr-currency       = ps_data-currency.
  ls_dochdr-ref_doc_no     = ps_data-ref_key.

  "--Populate line item record
  mc_convert_input: ps_data-cont_acct ps_data-cont_acct,
                    ps_data-contract  ps_data-contract.
  "--Check for Utility Contract Account
  IF ps_data-cont_acct IS INITIAL.
    SELECT erdat vkont INTO TABLE lt_cont_acct
      FROM fkkvkp
      WHERE gpart EQ ps_data-buspart.
    IF sy-subrc NE 0.
      "--Create an exception if no contract account found
      ps_data-msgtyp  = zcl_const_isu=>co_error.
      MESSAGE e033 INTO ps_data-remarks WITH ps_data-buspart.
      APPEND ps_data TO gt_exception.
      EXIT.
    ELSE.
      DESCRIBE TABLE lt_cont_acct LINES lv_lines.
      IF lv_lines GT 1.
        "--Create an exception if there are more than 1 Contract Accounts found for Business Partner
        ps_data-msgtyp  = zcl_const_isu=>co_error.
        MESSAGE e037 INTO ps_data-remarks WITH ps_data-buspart.
        APPEND ps_data TO gt_exception.
        EXIT.
      ELSE.
        READ TABLE lt_cont_acct ASSIGNING <fs_cacct> INDEX 1.
        IF sy-subrc EQ 0.
          ps_data-cont_acct = <fs_cacct>-vkont.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE vkont FROM fkkvkp INTO ps_data-cont_acct
      WHERE gpart EQ ps_data-buspart
        AND vkont EQ ps_data-cont_acct.
    IF sy-subrc NE 0.
      "--Create an exception if contract account does not exist
      ps_data-msgtyp  = zcl_const_isu=>co_error.
      MESSAGE e035 INTO ps_data-remarks WITH ps_data-cont_acct ps_data-buspart.
      APPEND ps_data TO gt_exception.
      EXIT.
    ENDIF.
  ENDIF.
  "--Check for Reference Contract No.
  IF ps_data-contract IS INITIAL.
    SELECT vkonto vertrag INTO TABLE lt_ref_cacct
      FROM ever
      WHERE vkonto EQ ps_data-cont_acct
        AND bukrs  EQ ps_data-cocode.
    IF sy-subrc NE 0.
      "--Create an exception if no contract account found
      ps_data-msgtyp  = zcl_const_isu=>co_error.
      MESSAGE e034 INTO ps_data-remarks WITH ps_data-cont_acct.
      APPEND ps_data TO gt_exception.
      EXIT.
    ELSE.
      DESCRIBE TABLE lt_ref_cacct LINES lv_lines.
      IF lv_lines GT 1.
        "--Create an exception if there are more than 1 Contract No. found for CAN and company code
        ps_data-msgtyp  = zcl_const_isu=>co_error.
        MESSAGE e038 INTO ps_data-remarks WITH ps_data-cont_acct.
        APPEND ps_data TO gt_exception.
        EXIT.
      ELSE.
        READ TABLE lt_ref_cacct ASSIGNING <fs_ref> INDEX 1.
        IF sy-subrc EQ 0.
          ps_data-contract = <fs_ref>-vertrag.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE vertrag FROM ever INTO ps_data-contract
      WHERE vertrag EQ ps_data-contract
        AND vkonto  EQ ps_data-cont_acct
        AND bukrs   EQ ps_data-cocode.
    IF sy-subrc NE 0.
      "--Create an exception if no contract account found
      ps_data-msgtyp  = zcl_const_isu=>co_error.
      MESSAGE e036 INTO ps_data-remarks WITH ps_data-contract ps_data-cont_acct.
      APPEND ps_data TO gt_exception.
      EXIT.
    ENDIF.
  ENDIF.
  "--Populate partner item table
  APPEND INITIAL LINE TO lt_partitem ASSIGNING <fs_prtitm>.
  ADD 1 TO <fs_prtitm>-item.
  <fs_prtitm>-clear_reas = ps_data-clearing.   "Clearing Reason
  <fs_prtitm>-comp_code  = ps_data-cocode.     "Company Code
  <fs_prtitm>-buspartner = ps_data-buspart.    "Business Partner
  mc_convert_date_internal ps_data-duedate <fs_prtitm>-net_date.  "Due Date for Net Payment
  <fs_prtitm>-cont_acct  = ps_data-cont_acct.  "Contract Account Number
  <fs_prtitm>-main_trans = '7008'.             "Main transaction for line item
  <fs_prtitm>-sub_trans  = '0020'.             "Subtransaction for document item
  REPLACE ALL OCCURRENCES OF ',' IN ps_data-amt_trans WITH space.
  CONDENSE ps_data-amt_trans.
  <fs_prtitm>-amount     = ps_data-amt_trans.  "Amount in Transaction Currency
  mc_convert_input ps_data-contract <fs_prtitm>-contract. "Reference Contract
  <fs_prtitm>-appl_area  = 'R'.
  <fs_prtitm>-doc_date   = ls_dochdr-doc_date.
  <fs_prtitm>-post_date  = ls_dochdr-post_date.
**Added by GDIMALIWAT
  CONCATENATE ps_data-text1 '|' ps_data-text2 '|' ps_data-text3 INTO <fs_prtitm>-text.
  "--------------------- POST DOCUMENT ---------------------
  "--Post document if line item(s) were populated
  IF lt_partitem[] IS NOT INITIAL.
    CALL FUNCTION 'BAPI_CTRACDOCUMENT_CREATE'
      EXPORTING
        testrun          = abap_false
        documentheader   = ls_dochdr
        completedocument = abap_true
      IMPORTING
        documentnumber   = lv_docno
        return           = ls_return
      TABLES
        partnerpositions = lt_partitem.
    "--Commit posting of records if no error found
    IF ls_return-type EQ 'S' OR ls_return-type EQ space.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      IF ls_return-message IS INITIAL.
        MESSAGE s041 INTO ls_return-message WITH lv_docno.
      ENDIF.
      "--Copy the Created Document Number to a returning variable for Installment Plan
      pv_opbel = lv_docno.
    ENDIF.
    "--Populate general message
    IF pv_retmsg IS NOT INITIAL.
      CONCATENATE pv_retmsg ls_return-message INTO pv_retmsg SEPARATED BY zcl_const_isu=>co_pipe.
    ELSE.
      pv_retmsg = ls_return-message.
    ENDIF.
    "--Create Exception lines
    ps_data-remarks = pv_retmsg.
    CASE ls_return-type.
      WHEN 'E' OR 'A'. ps_data-msgtyp = zcl_const_isu=>co_error.
      WHEN 'W'.        ps_data-msgtyp = zcl_const_isu=>co_warning.
      WHEN 'S' OR ' '. ps_data-msgtyp = zcl_const_isu=>co_success.
    ENDCASE.

    " If document number for installment plan creation is blank
*    IF pv_opbel is initial.
      APPEND ps_data TO gt_exception.
*    ENDIF.
  ENDIF.
ENDFORM.                    " POST_ACCOUNT_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       Display output in ALV
*----------------------------------------------------------------------*
FORM display_output .
  DATA: lt_fieldcat  TYPE slis_t_fieldcat_alv,
        lt_sort      TYPE slis_t_sortinfo_alv,
        ls_sort      TYPE slis_sortinfo_alv,
        ls_layout    TYPE slis_layout_alv.
  "--- Define Sort for ALV ---
  DEFINE mc_do_sort.
    ls_sort-fieldname = &1.
    ls_sort-up        = &2.
    ls_sort-down      = &3.
    ls_sort-subtot    = &4.
    append ls_sort to lt_sort.
    clear ls_sort.
  END-OF-DEFINITION.
  mc_do_sort: 'DOC_DATE'   abap_true  ' '  ' ',
              'DOC_TYPE'   abap_true  ' '  ' ',
              'POST_DATE'  abap_true  ' '  ' ',
              'CURRENCY'   abap_true  ' '  ' ',
              'REF_KEY'    abap_true  ' '  ' ',
              'RECON_KEY'  abap_true  ' '  ' ',
              'CLEARING'   abap_true  ' '  ' ',
              'BUSPART'    abap_true  ' '  ' '.
  PERFORM build_alv_fieldcat CHANGING lt_fieldcat.
  "--- Define Layout for ALV display ---
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  ls_layout-numc_sum          = abap_true.

  "--Create ALV GRID display
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = lt_fieldcat
      it_sort            = lt_sort
      is_layout          = ls_layout
      i_save             = 'A'
    TABLES
      t_outtab           = gt_exception[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       Create ALV field cat
*----------------------------------------------------------------------*
*      <--pt_fieldcat  Field catalog
*----------------------------------------------------------------------*
FORM build_alv_fieldcat CHANGING pt_fieldcat TYPE slis_t_fieldcat_alv.
  "--- Define Field Catalog layout ---
  "--- Fld name| Col Name| ref fld |    Ref Tab   | Sum | Key | Len | Edit|No Out|Fix Col|FldCat Table --"
  PERFORM write_fieldcat USING:
    "--Header Details
    'DOC_DATE'    ' '     'BLDAT'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'DOC_TYPE'    ' '     'BLART'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'POST_DATE'   ' '     'BUDAT'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'CURRENCY'    ' '     'WAERS'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'REF_KEY'     ' '     'XBLNR'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'RECON_KEY'   ' '     'FIKEY'       'DFKKKO'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'CLEARING'    ' '     'AUGRD'       'RFPE1'       ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    'BUSPART'     ' '     'GPART'       'DFKKOP'      ' '  'X'   ' '    ' '   ' '    ' '    pt_fieldcat,
    "--Line Item Details
    'COCODE'      ' '     'BUKRS'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'DUEDATE'     ' '     'FAEDN'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'CONT_ACCT'   ' '     'VKONT'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'MAIN_TRANS'  ' '     'HVORG'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'SUB_TRANS'   ' '     'TVORG'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'AMT_TRANS'   ' '     'BETRW'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'CONTRACT'    ' '     'VTREF'       'DFKKOP'      ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'MSGTYP'      ' '     'MSGTYP'      'BDCMSGCOLL'  ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat,
    'REMARKS'   text-003  ' '           ' '           ' '  ' '   ' '    ' '   ' '    ' '    pt_fieldcat.
ENDFORM.                    " BUILD_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  WRITE_FIELDCAT
*&---------------------------------------------------------------------*
*       Write the Field Catalog data to the Field Catalog Table
*----------------------------------------------------------------------*
*      -->pv_name      Field Name
*      -->pv_txt       Column Text
*      -->pv_fld       Ref Table Field
*      -->pv_tab       Ref Table name
*      -->pv_sum       Do summation
*      -->pv_key       Mark for key field
*      -->pv_length    Field Length
*      -->pv_edit      Edit
*      -->pv_no_out    No output
*      -->pv_fix_col   Fixed Column
*      -->pt_fieldcat  Field catalogue table
*----------------------------------------------------------------------*
FORM write_fieldcat USING value(pv_name)   value(pv_txt)  value(pv_fld)
                          value(pv_tab)    value(pv_sum)  value(pv_key)
                          value(pv_length) value(pv_edit) value(pv_no_out)
                          pv_fix_col     TYPE c
                          pt_fieldcattbl TYPE slis_t_fieldcat_alv.
  DATA ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
  ls_fieldcat-fieldname     = pv_name.    "Field name
  ls_fieldcat-reptext_ddic  = pv_txt.     "Colunm text
  ls_fieldcat-ref_fieldname = pv_fld.     "Ref Table Field
  ls_fieldcat-ref_tabname   = pv_tab.     "Ref Table name
  ls_fieldcat-do_sum        = pv_sum.     "Do summation
  ls_fieldcat-key           = pv_key.     "Mark for key field
  ls_fieldcat-outputlen     = pv_length.  "Field Length
  ls_fieldcat-edit          = pv_edit.    "Edit
  ls_fieldcat-no_out        = pv_no_out.  "No output
  ls_fieldcat-fix_column    = pv_fix_col. "Fix column
  APPEND ls_fieldcat TO pt_fieldcattbl.
  CLEAR ls_fieldcat.
ENDFORM.                    " write_fieldcat
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTALLMENT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_DATA   Lines of data
*      -->pv_opbel  Document Number
*----------------------------------------------------------------------*
FORM create_installment_plan  USING    ps_data    TYPE ty_data_s
                                       pv_retmsg  TYPE bapiret2-message
                                       pv_opbel   TYPE opbel_kk.
  DATA: lt_bapi_key TYPE STANDARD TABLE OF bapifkkop_key,
        lv_doc_date TYPE BLDAT,
        lv_post_date TYPE BUDAT,
        lv_start_date TYPE stardat_kk,
        ls_return    TYPE BAPIRETURN,
        lv_inst_doc TYPE BAPIINSTPLN_KEY-DOC_NO,
        lv_retmsg  TYPE bapiret2-message.
  FIELD-SYMBOLS: <fs_bapi_key> TYPE bapifkkop_key.

  CONCATENATE ps_data-doc_date+6(4) ps_data-doc_date(2) ps_data-doc_date+3(2) INTO lv_doc_date.
  CONCATENATE ps_data-post_date+6(4) ps_data-post_date(2) ps_data-post_date+3(2) INTO lv_post_date.
  CONCATENATE ps_data-start_date+6(4) ps_data-start_date(2) ps_data-start_date+3(2) INTO lv_start_date.

  APPEND INITIAL LINE TO lt_bapi_key ASSIGNING <fs_bapi_key>.

  SELECT SINGLE opbel opupw opupk opupz "#EC *
    INTO (<fs_bapi_key>-doc_number, <fs_bapi_key>-rep_item, <fs_bapi_key>-item, <fs_bapi_key>-sub_item)
    FROM dfkkop
    WHERE opbel EQ pv_opbel.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_INSTMNTPLN_CREATEFROMDATA'
      EXPORTING
        currency                   = ps_data-currency
        buspartner                 = ps_data-buspart
        cont_acct                  = ps_data-cont_acct
        doc_date                   = lv_doc_date
        pstng_date                 = lv_post_date
        doc_type                   = 'AB'
        startdate                  = lv_start_date
*        INST_AMNT                  =
        distance                   = '01'
        inst_intrval               = '3'
        round_to                   = '0.01'
*        INST_CAT                   =
*        CHARGE_AMNT                =
        remainamnt                 = '2'
        ch_doc_type                = 'GB'
        inst_number                = ps_data-inst_number
        fikey                      = ps_data-recon_key2
*        INT_ST_DTE                 =
*        INT_KEY                    =
*        INT_DOC_TYPE               =
*        SIMULATION                 =
        category                   = '01'
*        INT_DOC_DUE                =
*        INT_CH_DISTR               =
      IMPORTING
        RETURN                     = ls_return
        DOC_NO                     = lv_inst_doc
      TABLES
        t_bapifkkop_key            = lt_bapi_key
*        T_BAPIFKKOP_INSTPLAN       =
*        T_BAPIFKKOP_CHARGE         =
*        T_BAPIFKKOP_INTEREST       =
              .
  ENDIF.

    IF ls_return-type EQ 'S' OR ls_return-type EQ space.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      IF ls_return-message IS INITIAL.
        MESSAGE s042 INTO ls_return-message WITH lv_inst_doc.
      ENDIF.
    ENDIF.
    "--Populate general message
*    IF pv_retmsg IS NOT INITIAL.
*      CONCATENATE pv_retmsg ls_return-message INTO pv_retmsg SEPARATED BY zcl_const_isu=>co_pipe.
*    ELSE.
      pv_retmsg = ls_return-message.
*    ENDIF.
    "--Create Exception lines
    ps_data-remarks = pv_retmsg.
    CASE ls_return-type.
      WHEN 'E' OR 'A'. ps_data-msgtyp = zcl_const_isu=>co_error.
      WHEN 'W'.        ps_data-msgtyp = zcl_const_isu=>co_warning.
      WHEN 'S' OR ' '. ps_data-msgtyp = zcl_const_isu=>co_success.
    ENDCASE.
    APPEND ps_data TO gt_exception.

ENDFORM.                    " CREATE_INSTALLMENT_PLAN