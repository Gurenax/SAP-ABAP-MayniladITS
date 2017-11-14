*&---------------------------------------------------------------------*
*& PROGRAM     : ZCISU_DISCON
*& DESCRIPTION : Uploading of Disconnection/Reconnection records
*&---------------------------------------------------------------------*
*& Module      : FICA/DM
*& Author      : Ann Aclo
*& Date Created: 11 January 2012
*& Reference   : Based on program ZBDC_DISCON by MPBACANI,
*&               dated June 16, 2010
*&---------------------------------------------------------------------*
*& ID    |CR Number| Date     | Description                            *
*&---------------------------------------------------------------------*
*&ANNACLO|    4953 |2012/02/16|Initial Development                     *
*&ANNACLO|   11789 |2012/02/22|Multiple Disconnection Doc.Handling     *
*&ANNACLO|   11857 |2012/02/24|Inclusion of add'l fields in ZDISCON    *
*&ANNACLO|   12232 |2012/03/12|Changed Account Blocking fr 02 to 06    *
*&ANNACLO|   11938 |2012/03/05|Automatic Queuing of MRO for next up-   *
*&       |         |          |loading                                 *
*&ANNACLO|   12977 |2012/03/27|Additional validation to check if w/ MRO*
*&ANNACLO|   13169 |2012/04/02|Process records with w/ future MRO      *
*&ANNACLO|   13262 |2012/04/04|Addtl reqmts for BDC errors             *
*&ANNACLO|   13748 |2012/04/19|BDC Error corrections                   *
*&ANNACLO|   15424 |2012/05/23|Automatic Notification of Exceptions    *
*&ANNACLO|   17841 |2012/09/13|Automatic set-up of reopening fee, trig-*
*&       |         |          |gered by the reconnections sent by OCF  *
*&ANNACLO|   24308 |2012/12/21|Reopening Fee double posting correction *
*&ANNACLO|   27589 |2013/01/11|Change Default Due Date in Automatic ROF*
*&       |         |          |Set-up and use Posting Date value       *
*&JLIWAG |   25336 |2013/06/13|Addition of recon/discon date and initial/final reading in EENO_DYNP-ZEILE(Notes of Disconnection Object)
*&GDIMALIWAT|40444 |2013/07/29|Moved the deletion/updating of records in mro table before creation of exceptions to avoid error
*&GDIMALIWAT|40790 |2013/07/29|Replaced the messaged for Installation not disconnected
*&GDIMALIWAT|40444 |2013/09/09|Included ZEILE DIS tag and Seal number in EC87 Long Text
*&GDIMALIWAT|40444 |2013/10/07|Re-created transport for Standard Dev after SAP Upgrade
*&GDIMALIWAT|40444 |2013/11/14|Added exemptions for reason code 05 and 06 to proceed reconnection
*&GDIMALIWAT|40444 |2013/11/14|Fixed EL29 BDC to complete without the error
*&GDIMALIWAT|40444 |2013/11/16|Added exception messages for Automatically Locked (Status 2) with Zero Consumption
*&---------------------------------------------------------------------*
REPORT zcisu_discon MESSAGE-ID zisu LINE-SIZE 150.

*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPE-POOLS: slis, abap.

TYPES: BEGIN OF ty_mrdata,
         cacct        TYPE char12,     "Contract Account No.
         stat_code    TYPE char3,      "Status Code  (DIS/REC)
         mrdatf       TYPE sy-datum,   "Discon/Recon Date format 8 (system)
         reading      TYPE char13,     "Meter Reading
         seal_no      TYPE char18,     "Seal No.
         act_mtr      TYPE char18,     "Meter Account No.
         disc_reason  TYPE char3,      "Reason for Disconnection
         reason_code  TYPE char2,      "Reason Code
         resp_person  TYPE char50,     "Responsible Person
         remarks      TYPE char72,     "Remarks
         mrdate       TYPE char10,     "Discon/Recon Date
         billblock    TYPE abrsperr,   "Reason for Blocking Billing
         buspart      TYPE gpart_kk,   "Business Partner
         contract     TYPE vertrag,    "Contract
         install      TYPE anlage,     "Installation No.
         busarea      TYPE gsber,      "Business Area
         rdng_fin     TYPE char13,     "Meter Reading final
       END OF ty_mrdata,
       ty_mrdata_t    TYPE STANDARD TABLE OF ty_mrdata,
       BEGIN OF ty_mrdata_dup,
         cacct        TYPE char12,     "Contract Account No.
         stat_code    TYPE char3,      "Status Code
         count        TYPE i,          "Count existence of record
       END OF ty_mrdata_dup,
       BEGIN OF ty_mrdoc,
         ablbelnr     TYPE ablbelnr,   "Meter reading document
         adatsoll     TYPE adatsoll,   "Scheduled meter reading date
         ablesgr      TYPE ablesgr,    "Meter reading reason
         ablstat      TYPE ablstat,    "Meter Reading Status
       END OF ty_mrdoc,
       BEGIN OF ty_exception,
         busarea      TYPE gsber,      "Business Area
         install      TYPE char10,     "Installation No.
         cacct        TYPE char12,     "Contract Account No.
         buspart      TYPE char10,     "Business Partner
         statcode     TYPE char3,      "Activity Status Code(DIS/REC)
         mrdatf       TYPE char8,      "Discon/Recon Date format 8 (system)
         reading      TYPE char13,     "Meter reading entry
         mro          TYPE char3,      "Indicator that record came from MRO Table
         remarks      TYPE char80,     "Remarks
         rof_remarks  TYPE char50,     "ROF remarks - Inserted by Ann 2012/09/13
       END OF ty_exception,
       ty_exception_t TYPE STANDARD TABLE OF ty_exception,
       BEGIN OF ty_output_msgs,
         cacct        TYPE char12,     "Contract Account No.
         buspart      TYPE char10,     "Business Partner
         install      TYPE char10,     "Installation No.
         mro          TYPE char3,      "Indicator if records came from MRO Table
         tcode        TYPE sytcode,
         reading      TYPE char13,     "Meter reading entry
         msgtyp       TYPE icon_d,
         msgs         TYPE char80,
         rof_msgs     TYPE char50,     "ROF Message - Inserted by Ann 2012/09/13
       END OF ty_output_msgs,
       ty_output_msgs_t TYPE STANDARD TABLE OF ty_output_msgs,
       BEGIN OF ty_mrreason,
         adatsoll     TYPE adatsoll,
         ablbelnr     TYPE ablbelnr,
         anlage       TYPE anlage,
         ablesgr      TYPE ablesgr,
         abrdats      TYPE abrdats,
       END OF ty_mrreason,
       BEGIN OF ty_discdoc,
         discno       TYPE discno,
         status       TYPE edcdocstat,
       END OF ty_discdoc.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_mrdata      TYPE STANDARD TABLE OF ty_mrdata,
      gt_mrodata     TYPE STANDARD TABLE OF ty_mrdata,
      gt_mrdata_dup  TYPE STANDARD TABLE OF ty_mrdata_dup,
      gt_mrdoc       TYPE STANDARD TABLE OF ty_mrdoc,
      gt_excep_mro   TYPE STANDARD TABLE OF ztisu_discon_mro,
      gt_exception   TYPE ty_exception_t,
      gt_bdc_error   TYPE ty_exception_t,
      gt_output_msgs TYPE ty_output_msgs_t,
      gt_bdcdata     TYPE tab_bdcdata,
      gt_filetable   TYPE filetable,
      gt_stringtab   TYPE stringtab.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gr_status     TYPE RANGE OF edcdocstat,
      gs_option     TYPE ctu_params,
      gs_mrdup      TYPE ty_mrdata_dup,
      gv_mrdatbill  TYPE adat,
      gv_readresult TYPE readingresult,
      gv_recon      TYPE i,
      gv_discon     TYPE i,
      gv_bdc_error  TYPE i,
      gv_error      TYPE abap_bool,
      gv_title      TYPE string,
      gv_frmro      TYPE char1,
      gv_rc         TYPE i,
      gv_mrrec      TYPE i,
      gv_mrorec     TYPE i,
      gv_mroproc    TYPE i,
      gv_mroerr     TYPE i,
      gv_mroexe     TYPE i,
      gv_mroret     TYPE i,
      gv_mroupd     TYPE i,
      gv_mrorecn    TYPE i,
      gv_tot_dup    TYPE i,
      gv_wmro       TYPE i,
      gv_datefr     TYPE sy-datum,
      gv_dateto     TYPE sy-datum,
      gv_lpermrdate TYPE adatsoll,  "Inserted by Ann 10/19/2012
      gv_filepath   TYPE string,
      gv_file_error TYPE string,
      gv_notes      TYPE string.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_mr>      TYPE ty_mrdata,
               <fs_mrdoc>   TYPE ty_mrdoc,
               <fs_bdc>     TYPE bdcdata,
               <fs_status>  LIKE LINE OF gr_status,
               <fs_msgs>    TYPE bdcmsgcoll,
               <fs_excep>   TYPE ty_exception,
               <fs_outmsg>  TYPE ty_output_msgs,
               <fs_filetab> TYPE file_table,
               <fs_mro>      TYPE ztisu_discon_mro.

*&----------------------------------------------------------------------*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS: co_pipe       TYPE char1          VALUE '|',
           co_discon     TYPE char3          VALUE 'DIS',
           co_recon      TYPE char3          VALUE 'REC',
           co_updmode    TYPE ctu_update     VALUE 'L',
           co_dismode    TYPE ctu_mode       VALUE 'N',
           co_blkcontrct TYPE abrsperr       VALUE '06',         "Blocking Reason - TCD  "12232
           co_disctype   TYPE char4          VALUE '2',          "Device Disconnected
           co_mrtype     TYPE istablart      VALUE '01',         "Meter Reading by Utility - SAP
           co_mrnumber   TYPE ableser        VALUE 'SZP',        "Soluziona Standard MR ID
           co_docstat    TYPE edcdocstat     VALUE 21,           "Disconnection carried out
           co_mrorder    TYPE mrdocumenttype VALUE '1',          "Meter reading orders
           co_mrresult   TYPE mrdocumenttype VALUE '2',          "Meter reading result
           co_refobjacct TYPE oj_name        VALUE 'ISUACCOUNT',   "Reference object for Account
           co_refobjbprt TYPE oj_name        VALUE 'ISUPARTNER',   "Reference object for Business Partner
           co_refobjinst TYPE oj_name        VALUE 'INSTLN',       "Reference object for Installation
           co_excep      TYPE char9          VALUE 'excep.txt',    "Exception file extension
           co_bdcerror   TYPE char12         VALUE 'bdcerror.txt', "BDC error file extension
           co_mro        TYPE char3          VALUE 'mro',          "MRO error file extension
           co_log        TYPE char7          VALUE 'log.txt',      "Output Summary log extension
           co_uscore     TYPE char1          VALUE '_',
           "--Inserted by Ann 09/13/2012
           co_maintrans  TYPE hvorg_kk       VALUE '7003',        "Main transaction for Line Item
           co_subtrans   TYPE tvorg_kk       VALUE '0020',        "Subtransaction for Document Item
           "co_amttrans   TYPE char18         VALUE '495.24'.      "Amount in Transaction Currency with +/- Sign "Comment out by Ann 01/17/2013
           co_amttrans   TYPE char18         VALUE '511.09'.      "Amount in Transaction Currency with +/- Sign  "Inserted by Ann 01/17/2013
"/-Inserted by Ann 09/13/2012
*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk00 WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-006.
PARAMETERS: rb_file  RADIOBUTTON GROUP grp1 USER-COMMAND ucom,
            rb_mro   RADIOBUTTON GROUP grp1,
            rb_all   RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk01.
SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE text-015 NO INTERVALS.
PARAMETERS: rb_servr RADIOBUTTON GROUP gr2 DEFAULT 'X' MODIF ID loc,
            rb_local RADIOBUTTON GROUP gr2 MODIF ID loc.
SELECTION-SCREEN END OF BLOCK blk02.
PARAMETERS: cb_email TYPE boole_d,     "Inserted by Ann 20120607
            pa_fpath TYPE rlgrap-filename DEFAULT 'g:\MRS\Upload\' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk00.

*&---------------------------------------------------------------------*
*& At Selection Screen Event
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_fpath.
  gv_title = text-007.
  cl_gui_frontend_services=>file_open_dialog( EXPORTING  window_title            = gv_title
                                                         default_extension       = 'TXT'
                                                         default_filename        = 'discon'
                                                         initial_directory       = 'g:\MRS\Upload\'
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

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'LOC' AND rb_mro EQ abap_true.
      screen-invisible = '1'.
    ELSE.
      screen-invisible = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sy-ucomm EQ 'ONLI'.
    IF rb_file EQ abap_true OR rb_all EQ abap_true.
      IF rb_local EQ abap_true.
        PERFORM get_file_local.
      ELSE.
        PERFORM get_file_server.
      ENDIF.
    ENDIF.
    IF rb_mro EQ abap_true OR rb_all EQ abap_true.
      "--Retrieve records with MRO previously stored in table if existing
      SELECT zzcacct zzstatcode zzfiledate zzreading zzsealno zzmtr_acct zzdrreason "#EC CI_NOWHERE
             zzreasoncode zzresp_person zzremarks INTO TABLE gt_mrodata
        FROM ztisu_discon_mro.
      IF rb_mro EQ abap_true AND sy-subrc NE 0.
        MESSAGE e012.
      ENDIF.
    ENDIF.
    PERFORM get_period.
  ENDIF.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  "--Create Exception file
  DEFINE mc_exception.
    if &1 eq abap_true.
      append initial line to gt_excep_mro assigning <fs_mro>.
      <fs_mro>-zzcacct       = &2-cacct.        "Contract Account No.
      <fs_mro>-zzstatcode    = &2-stat_code.    "Activity/Status Code
      <fs_mro>-zzfiledate    = &2-mrdatf.       "Entry Date in File
      <fs_mro>-zzreading     = &2-reading.      "Reading Entry in File
      <fs_mro>-zzsealno      = &2-seal_no.      "Meter Seal No.
      <fs_mro>-zzmtr_acct    = &2-act_mtr.      "Meter Account Number
      <fs_mro>-zzdrreason    = &2-disc_reason.  "Reason for Discon
      <fs_mro>-zzreasoncode  = &2-reason_code.  "Reason code
      <fs_mro>-zzresp_person = &2-resp_person.  "Responsible Person
      <fs_mro>-zzremarks     = &2-remarks.      "Remarks
      <fs_mro>-zzcreated_by  = sy-uname.        "Person who upload the record
      <fs_mro>-zzcreate_date = sy-datum.        "Upload/Creation Date
      <fs_mro>-zzcreate_time = sy-uzeit.        "Upload/Creation Time
    else.
      append initial line to gt_exception assigning <fs_excep>.
      <fs_excep>-busarea     = &2-busarea.     "Business Area
      <fs_excep>-cacct       = &2-cacct.       "Contract Account No.
      <fs_excep>-buspart     = &2-buspart.     "Business Partner
      <fs_excep>-install     = &2-install.     "Installation No.
      <fs_excep>-statcode    = &2-stat_code.   "Activity Status Code(DIS/REC)
      <fs_excep>-mrdatf      = &2-mrdatf.      "Discon/Recon Date
      <fs_excep>-reading     = &2-reading.     "Initial Reading Entry from file
      <fs_excep>-mro         = &3.             "Indicator that record came from MRO table
      <fs_excep>-remarks     = &4.             "Reason code not applicable
      <fs_excep>-rof_remarks = &5.             "ROF Remarks
    endif.
  END-OF-DEFINITION.
  "--Macro definition for BDC creation
  DEFINE mc_bdc_dynpro.
    append initial line to gt_bdcdata assigning <fs_bdc>.
    if &1 eq abap_true.
      <fs_bdc>-program  = &2.
      <fs_bdc>-dynpro   = &3.
      <fs_bdc>-dynbegin = &1.
    else.
      <fs_bdc>-fnam     = &2.
      <fs_bdc>-fval     = &3.
    endif.
  END-OF-DEFINITION.
  "--Range for Disconnection Document Status
  DEFINE mc_discon_status.
    append initial line to gr_status assigning <fs_status>.
    <fs_status>-sign   = 'I'.
    <fs_status>-option = 'EQ'.
    <fs_status>-low    = &1.
  END-OF-DEFINITION.
  "--Add leading zeroes to value
  DEFINE mc_add_leading_zeroes.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = &1
      IMPORTING
        output = &2.
  END-OF-DEFINITION.
  DEFINE mc_output_msgs.
    append initial line to gt_output_msgs assigning <fs_outmsg>.
    <fs_outmsg>-cacct    = &1.     "Contract Account No.
    <fs_outmsg>-buspart  = &2.     "Business Partner
    <fs_outmsg>-install  = &3.     "Installation No.
    <fs_outmsg>-mro      = &4.     "Indicator if record came from MRO Table
    <fs_outmsg>-tcode    = &5.     "Transaction Code
    <fs_outmsg>-reading  = &6.     "MR entry
    "--Message Type
    case &7.
      when 'E' or 'A'. <fs_outmsg>-msgtyp  = zcl_const_isu=>co_error.
      when 'W'.        <fs_outmsg>-msgtyp  = zcl_const_isu=>co_warning.
      when 'S'.        <fs_outmsg>-msgtyp  = zcl_const_isu=>co_success.
      when others.     <fs_outmsg>-msgtyp  = zcl_const_isu=>co_warning.
    endcase.
    <fs_outmsg>-msgs     = &8.     "Messages
    <fs_outmsg>-rof_msgs = &9.     "ROF-related Messages
  END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM prepare_records.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM create_exception_file.

*&---------------------------------------------------------------------*
*&      Form  get_file_local
*&---------------------------------------------------------------------*
*       Get file from Local Drive
*----------------------------------------------------------------------*
FORM get_file_local .

  DATA: lt_data      TYPE TABLE OF string,
        lv_filename  TYPE string.
  FIELD-SYMBOLS: <fv_data> TYPE string.

  lv_filename = pa_fpath.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
      has_field_separator     = abap_true
    CHANGING
      data_tab                = lt_data
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
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
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
    CONCATENATE lv_mes text-005 INTO lv_mes SEPARATED BY '.'.
    MESSAGE e007 WITH lv_mes.
  ENDIF.
ENDFORM.                    " get_file_server
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_RECORD
*&---------------------------------------------------------------------*
*       Transfer record to internal table
*----------------------------------------------------------------------*
*      -->PV_DATA  Line of data
*----------------------------------------------------------------------*
FORM transfer_record  USING pv_data TYPE string.

  APPEND INITIAL LINE TO gt_mrdata ASSIGNING <fs_mr>.
  SPLIT pv_data AT co_pipe INTO <fs_mr>-cacct       <fs_mr>-stat_code   <fs_mr>-mrdatf
                                <fs_mr>-reading     <fs_mr>-seal_no     <fs_mr>-act_mtr
                                <fs_mr>-disc_reason <fs_mr>-reason_code <fs_mr>-resp_person
                                <fs_mr>-remarks.
  IF <fs_mr>-stat_code IS NOT INITIAL.
    SET LOCALE LANGUAGE 'E'.
    TRANSLATE <fs_mr>-stat_code TO UPPER CASE.
  ENDIF.
  "--Create internal table to trace duplicate records
  gs_mrdup-cacct     = <fs_mr>-cacct.
  gs_mrdup-stat_code = <fs_mr>-stat_code.
  gs_mrdup-count     = 1.
  COLLECT gs_mrdup INTO gt_mrdata_dup.
  CLEAR gs_mrdup.
ENDFORM.                    " TRANSFER_RECORD
*&---------------------------------------------------------------------*
*&      Form  PREPARE_RECORDS
*&---------------------------------------------------------------------*
*       Prepare records for processing (from MRO and upload file)
*----------------------------------------------------------------------*
FORM prepare_records .

  DATA: lv_totrec  TYPE i.

  "--Set Call Transaction Options
  gs_option-dismode = co_dismode.
  gs_option-updmode = co_updmode.
  gs_option-nobinpt = abap_false. "ann 20120411
  IF rb_mro EQ abap_true OR rb_all EQ abap_true.
    "--Process records from MRO table
    IF gt_mrodata[] IS NOT INITIAL.
      gv_frmro = abap_true. "Indicator that records came from MRO table
      PERFORM show_progress USING 0 text-027.
      DESCRIBE TABLE gt_mrodata LINES gv_mrorec.
      SORT gt_mrodata BY cacct stat_code mrdatf.
      PERFORM validate_data TABLES gt_mrodata.
      gv_mroproc = gv_discon + gv_recon.           "Total MRO records successfully processed/uploaded
      gv_mroerr  = gv_bdc_error.                   "Total MRO records with errors
      gv_mroret  = gv_wmro.                        "No. of MRO records not uploaded due to MRO
      DESCRIBE TABLE gt_exception LINES gv_mroexe. "Total MRO records save to exception file
      CLEAR: gv_discon, gv_recon, gv_bdc_error, gv_wmro.
    ENDIF.
  ENDIF.
  IF rb_file EQ abap_true OR rb_all EQ abap_true.
    gv_frmro = abap_false.
    "--Process records from upload file
    "--Get Original number of records from upload file
    PERFORM show_progress USING 0 text-030.
    DELETE gt_mrdata WHERE cacct EQ space.
    DESCRIBE TABLE gt_mrdata LINES gv_mrrec.
    "--Sort internal table and delete duplicate records
    SORT gt_mrdata BY cacct stat_code mrdatf.
    DELETE ADJACENT DUPLICATES FROM gt_mrdata COMPARING cacct stat_code.
    DESCRIBE TABLE gt_mrdata LINES lv_totrec.       "Total records remaining after removal of duplicates
    gv_tot_dup  = gv_mrrec - lv_totrec.             "Total no. of duplicate records removed
    IF gt_mrdata_dup[] IS NOT INITIAL.
      "--Sort internal table for duplicate accounts tagging and retain duplicate records
      SORT gt_mrdata_dup BY cacct stat_code.
      DELETE: gt_mrdata_dup WHERE cacct EQ space,
              gt_mrdata_dup WHERE count EQ 1.       "Retain only those with duplicate records
    ENDIF.
    PERFORM validate_data TABLES gt_mrdata.
  ENDIF.
ENDFORM.                    " prepare_records
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       Validate file before uploading and create exception
*----------------------------------------------------------------------*
*      --> pt_mrdata     Discon/Recon data
*----------------------------------------------------------------------*
FORM validate_data TABLES pt_mrdata TYPE ty_mrdata_t.

  DATA: lt_mrdata     TYPE STANDARD TABLE OF ty_mrdata,
        lv_mrreason   TYPE ablesgr,
        lv_mrdate     TYPE adatsoll,
        lv_exist      TYPE char1,
        lv_dup        TYPE char1,
        lv_dial       TYPE i,
        lv_count      TYPE i,
        lv_msge(60)   TYPE c.

  lt_mrdata[] = pt_mrdata[].
  "--Data Validation - to create exception
  LOOP AT lt_mrdata ASSIGNING <fs_mr>.
    CLEAR: lv_msge, lv_dup, lv_exist, lv_mrreason.
    "--Get Account Details
    PERFORM get_account_details CHANGING <fs_mr>-busarea <fs_mr>-cacct <fs_mr>-buspart <fs_mr>-contract <fs_mr>-install <fs_mr>-billblock.
    IF <fs_mr>-install IS INITIAL.
      mc_exception ' ' <fs_mr> gv_frmro text-022 ' '. "Contract Account does not exist in SAP.
      CONTINUE.
    ENDIF.
    IF gv_frmro IS INITIAL. "If record not from MRO table
      IF gt_mrdata_dup[] IS NOT INITIAL.
        "--Check if record has duplicates
        PERFORM check_for_duplicates USING <fs_mr>-cacct <fs_mr>-stat_code CHANGING lv_dup.
        IF lv_dup EQ abap_true.
          "--If with duplicate, do not process but include in exception
          CASE <fs_mr>-stat_code.
            WHEN co_discon. mc_exception ' ' <fs_mr> gv_frmro text-016 ' '. "Not uploaded - Multiple Discon Entries
            WHEN co_recon.  mc_exception ' ' <fs_mr> gv_frmro text-017 ' '. "Not uploaded - Multiple Recon Entries
          ENDCASE.
          CONTINUE.
        ENDIF.
      ENDIF.
      "--Check if reason code exists
      "IF <fs_mr>-reason_code IS NOT INITIAL. "--Commented by GDIMALIWAT 11/14/2013
      IF <fs_mr>-reason_code IS NOT INITIAL AND <fs_mr>-reason_code NE '05' AND <fs_mr>-reason_code NE '06'. "--Added by GDIMALIWAT 11/14/2013
        "--Include records with reason code in exception report
        CONCATENATE text-012 <fs_mr>-reason_code INTO lv_msge SEPARATED BY space.
        mc_exception ' ' <fs_mr> gv_frmro lv_msge ' '. "With reason code
        CONTINUE.
      ENDIF.
    ENDIF.
    "--Check if MRO (Meter Reading Order) exists for the current period
    PERFORM get_mro_details USING <fs_mr>-buspart <fs_mr>-install <fs_mr>-stat_code <fs_mr>-mrdatf co_mrorder
                            CHANGING lv_exist gv_mrdatbill gv_readresult lv_mrreason lv_mrdate lv_dial lv_count
                                     gv_lpermrdate.  "Inserted by Ann 10/19/2012
    IF lv_exist IS NOT INITIAL.
      "--Not Uploaded - With MRO, stored to MRO table - ZTISU_DISCON_MRO
      ADD 1 TO gv_wmro.
      mc_exception abap_true <fs_mr> gv_frmro ' '  ' '.
      CONTINUE.
    ENDIF.
    AT END OF cacct.
      IF <fs_mr>-billblock IS NOT INITIAL. "--Billing is blocked
        "--Perform reconnection first, then disconnect if existing
        PERFORM: append_record TABLES pt_mrdata USING <fs_mr> co_recon lv_mrdate,
                 append_record TABLES pt_mrdata USING <fs_mr> co_discon lv_mrdate.
      ELSE.
        "--Perform disconnection first, then reconnect if existing
        PERFORM: append_record TABLES pt_mrdata USING <fs_mr> co_discon lv_mrdate,
                 append_record TABLES pt_mrdata USING <fs_mr> co_recon lv_mrdate.
      ENDIF.
    ENDAT.
  ENDLOOP.
  UNASSIGN <fs_mr>.
ENDFORM.                    " validate_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_RECORD
*&---------------------------------------------------------------------*
*       Append new arrangement of record
*----------------------------------------------------------------------*
*      --> pt_mrdata     Discon/Recon data
*      --> ps_mr         Discon/Recon data
*      --> pv_action     Action if for Disconnection or Reconnection
*      --> pv_mrdate     MRO date
*----------------------------------------------------------------------*
FORM append_record  TABLES pt_mrdata TYPE ty_mrdata_t
                     USING ps_mr     TYPE ty_mrdata
                           pv_action TYPE char3
                           pv_mrdate TYPE sy-datum.

  FIELD-SYMBOLS: <fs_mrdata> TYPE ty_mrdata.
  "--Create final internal table for validated records
  READ TABLE pt_mrdata ASSIGNING <fs_mrdata> WITH KEY cacct     = ps_mr-cacct
                                                      stat_code = pv_action BINARY SEARCH.
  IF sy-subrc EQ 0.
    <fs_mrdata>-billblock = ps_mr-billblock. "Reason for bill blocking
    <fs_mrdata>-buspart   = ps_mr-buspart.   "Business Partner
    <fs_mrdata>-contract  = ps_mr-contract.  "Contract Acct.
    <fs_mrdata>-install   = ps_mr-install.   "Installation No.
    <fs_mrdata>-busarea   = ps_mr-busarea.   "Business Area
    WRITE ps_mr-mrdatf TO <fs_mrdata>-mrdate.
    PERFORM process_data USING <fs_mrdata> pv_mrdate.
  ENDIF.
ENDFORM.                    " append_record
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Process retrieved Meter Reading data
*----------------------------------------------------------------------*
*       --> ps_mr      Validated discon/recon Data
*       --> pv_mrdate  MRO date
*----------------------------------------------------------------------*
FORM process_data USING ps_mr     TYPE ty_mrdata
                        pv_mrdate TYPE sy-datum.
  DATA: ls_discdoc      TYPE ty_discdoc,
        lv_mrdate       TYPE adatsoll,
        lv_mrreason     TYPE ablesgr,
        lv_exist        TYPE char1,
        lv_zeroconsump  TYPE char1,
        lv_mrrdg_f(16)  TYPE p DECIMALS 14,   "MR Reading from File
        lv_mrrdg_s(16)  TYPE p DECIMALS 14,   "MR Reading from SAP
        lv_upldate      TYPE sy-datum,
        lv_dial         TYPE i,
        lv_length       TYPE i,
        lv_rec          TYPE i,
        lv_count        TYPE i,
        lv_dec          TYPE char32,
        lv_msge(70)     TYPE c,
        lv_rof_msgs(50) TYPE c.               "Inserted by Ann 2012/09/13

  CLEAR: gv_error, lv_exist, lv_count.
  "--Temporarily store date from file as date of upload
  lv_upldate = ps_mr-mrdatf.
  "--Get MRO details and get MR results
  lv_mrdate  = pv_mrdate. "ann-20120402
  PERFORM get_mro_details USING ps_mr-buspart ps_mr-install ps_mr-stat_code ps_mr-mrdatf co_mrresult
                          CHANGING lv_exist gv_mrdatbill gv_readresult lv_mrreason lv_mrdate lv_dial lv_count
                                   gv_lpermrdate.  "Inserted by Ann 10/19/2012
  IF lv_exist EQ abap_false. "No Historical data exists
    mc_exception ' ' ps_mr gv_frmro text-031 ' '.  "MR Result History data does not exist in EL31
    RETURN.
  ENDIF.
  IF lv_count GT 1.
    CASE ps_mr-stat_code.
      WHEN co_discon. mc_exception ' ' ps_mr gv_frmro text-019  ' '. "Not Uploaded - Multiple Reconnection Entries exists in EL31
      WHEN co_recon.  mc_exception ' ' ps_mr gv_frmro text-018  ' '. "Not Uploaded - Multiple Disconnection Entries exists in EL31
    ENDCASE.
    RETURN.
  ENDIF.
  "--Check if Discon/Recon date is <= latest MR Entry Date in EL31
  IF lv_upldate LE lv_mrdate.
    IF lv_mrdate LT sy-datum.  "If last reading date in SAP is < current date
      lv_upldate = lv_mrdate + 1.  "Modify Discon/Recon Date as MR Entry date from SAP + 1 day
    ELSE.
      lv_upldate = lv_mrdate.      "Modify Discon/Recon Date as MR Entry date from SAP
    ENDIF.
    WRITE lv_upldate TO ps_mr-mrdate.
  ENDIF.
  "--Check Discon/Recon MR Entry VS latest meter reading entry in SAP (via EL31)
  "--Reading from file
  ps_mr-rdng_fin = ps_mr-reading.
  IF ps_mr-rdng_fin EQ space.
    ps_mr-rdng_fin = '0'.
  ENDIF.
  lv_mrrdg_f = ps_mr-reading.  "Reading from file
  IF gv_readresult EQ space.
    gv_readresult = '0'.
  ELSE.
    SPLIT gv_readresult AT '.' INTO gv_readresult lv_dec.
  ENDIF.
  lv_mrrdg_s = gv_readresult.  "Latest Reading from SAP (EL31)
  "--Compare MR entry from file VS MR entry in SAP
  IF lv_mrrdg_f LT lv_mrrdg_s.
    lv_zeroconsump = abap_true.         "Set Consumption to Zero
  ELSEIF lv_mrrdg_f GT lv_mrrdg_s.      "Validate if MR entry from file > MR entry in SAP
    CONDENSE ps_mr-rdng_fin.
    lv_length = strlen( ps_mr-rdng_fin ).
    IF lv_length GT lv_dial.            "If no. of digits greater than dials
      CONDENSE gv_readresult.
      ps_mr-rdng_fin = gv_readresult.   "Use the latest reading from SAP
    ENDIF.
  ENDIF.
  "--Process if Discon/Recon date >= latest MR Scheduled Entry Date
  IF lv_upldate GE lv_mrdate.
    "--Check file per status code
    CASE ps_mr-stat_code.
      WHEN co_discon. "Physically disconnected as per upload file
        CASE lv_mrreason.
          WHEN '13'.  "Meter Reading on disconnection
            mc_exception ' ' ps_mr gv_frmro text-004  ' '. "Installation fully disconnected
            RETURN.
          WHEN '22'.  "Meter Reading Billing-rel. removal
            mc_exception ' ' ps_mr gv_frmro text-008  ' '. "Meter removed in SAP (EL31 RR=22)
            RETURN.
          WHEN OTHERS.
            "--Check if Disconnection document already created
            PERFORM get_discon_details USING ps_mr-cacct ps_mr-buspart ps_mr-install co_docstat ps_mr-stat_code CHANGING ls_discdoc lv_rec.
            IF ls_discdoc-status NE 1. "Newly created disconnection doc
              IF ls_discdoc-discno IS NOT INITIAL.
                CONCATENATE text-024 ls_discdoc-discno INTO lv_msge SEPARATED BY space.
                mc_exception ' ' ps_mr gv_frmro lv_msge ' '. "Installation fully disconnected, Disc Doc:
                RETURN.
              ELSE.
                "--Perform BDC to create disconnection document
                PERFORM create_discon_document USING ps_mr ' ' CHANGING ls_discdoc-discno.
                IF gv_error EQ abap_true.
                  ADD 1 TO gv_bdc_error.
                  IF gv_frmro EQ abap_true. "/B-ann20120403
                    "--Retain records with MRO in table if an error was encountered
                    ADD 1 TO gv_wmro.
                    mc_exception abap_true ps_mr gv_frmro ' '  ' '.
                  ENDIF.                    "E-ann20120403
                  RETURN.
                ENDIF.
                IF ls_discdoc-discno IS INITIAL.
                  mc_exception ' ' ps_mr gv_frmro text-001 ' '. "Disconnection doc. does not exist in table EDISCDOC
                  RETURN.
                ENDIF.
              ENDIF.
            ELSE.
              IF lv_rec GT 1. "/B-ann20120402 add to exception if with multip disconnection documents in EDISCDOC
                mc_exception ' ' ps_mr gv_frmro text-003 ' '. "Multiple Disconnection Documents
                RETURN.
              ENDIF.          "/E-ann20120412
            ENDIF.
            "--BDC to update disconnection document
            PERFORM change_discon_document USING co_discon ls_discdoc-discno lv_mrreason ' ' ps_mr.
            IF gv_error EQ abap_true.
              ADD 1 TO gv_bdc_error.
              IF gv_frmro EQ abap_true. "/B-ann20120403
                "--Retain records with MRO in table if an error was encountered
                ADD 1 TO gv_wmro.
                mc_exception abap_true ps_mr gv_frmro ' '  ' '.
              ENDIF.                    "E-ann20120403
              RETURN.
            ENDIF.
            "--BDC to block/unblock contract
            PERFORM: block_contract USING co_blkcontrct ' ' ps_mr,
            "--BDC to release block
                     set_mr_release_status USING ' ' ps_mr.
            IF lv_zeroconsump EQ abap_true.
              PERFORM correct_plausible_mrresult USING ' ' ps_mr '13'.  "add reason code by ann 04/16/2012
              IF gv_error EQ abap_true.
                ADD 1 TO gv_bdc_error.
                IF gv_frmro EQ abap_true. "/B-ann20120403
                  "--Retain records with MRO in table if an error was encountered
                  ADD 1 TO gv_wmro.
                  mc_exception abap_true ps_mr gv_frmro ' '  ' '.
                ENDIF.                    "E-ann20120403
                RETURN.
              ENDIF.
            ENDIF.
            ADD 1 TO gv_discon.
        ENDCASE.
      WHEN co_recon.
        IF lv_mrreason NE '13'. "MR Status in SAP not disconnected, no need for reconnection
          PERFORM autopost_reopening_fee USING ps_mr gv_lpermrdate CHANGING lv_rof_msgs.  "Inserted by Ann 2012/10/24
          mc_exception ' ' ps_mr gv_frmro text-009 lv_rof_msgs. "Installation not disconnection. ROF set-up successfully posted.
          RETURN.
        ENDIF.
        PERFORM get_discon_details USING ps_mr-cacct ps_mr-buspart ps_mr-install co_docstat ps_mr-stat_code CHANGING ls_discdoc lv_rec.
        IF lv_rec GT 1.
          mc_exception ' ' ps_mr gv_frmro text-003 ' '. "Multiple Disconnection Documents
          RETURN.
        ENDIF.
        IF ls_discdoc-discno IS INITIAL.
          CONCATENATE text-002 co_docstat INTO lv_msge SEPARATED BY space.
          mc_exception ' ' ps_mr gv_frmro lv_msge ' '. "Disconnection doc. does not exist in table EDISCDOC
          RETURN.
        ENDIF.
        PERFORM autopost_reopening_fee USING ps_mr gv_lpermrdate CHANGING lv_rof_msgs.  "Inserted by Ann 2012/09/13
        "--BDC to Update disconnection document for Reconnection
        IF ps_mr-remarks IS NOT INITIAL.
          PERFORM update_recon_remarks USING ls_discdoc-discno lv_rof_msgs ps_mr.
          IF gv_error EQ abap_true.
            ADD 1 TO gv_bdc_error.
            IF gv_frmro EQ abap_true. "/B-ann20120403
              "--Retain records with MRO in table if an error was encountered
              ADD 1 TO gv_wmro.
              mc_exception abap_true ps_mr gv_frmro ' '  lv_rof_msgs.
            ENDIF.                    "E-ann20120403
            RETURN.
          ENDIF.
        ENDIF.
        PERFORM change_discon_document USING ' ' ls_discdoc-discno lv_mrreason lv_rof_msgs ps_mr.
        IF gv_error EQ abap_true.
          ADD 1 TO gv_bdc_error.
          IF gv_frmro EQ abap_true. "/B-ann20120403
            "--Retain records with MRO in table if an error was encountered
            ADD 1 TO gv_wmro.
            mc_exception abap_true ps_mr gv_frmro ' ' lv_rof_msgs.
          ENDIF.                    "E-ann20120403
          RETURN.
        ENDIF.
        "--BDC to block/unblock contract
        PERFORM: block_contract USING ' ' lv_rof_msgs ps_mr,
                 "--BDC to release block
                 set_mr_release_status USING lv_rof_msgs ps_mr.
        IF lv_zeroconsump EQ abap_true.
          PERFORM correct_plausible_mrresult USING lv_rof_msgs ps_mr '18'.  "add reason code by ann 04/16/2012.
          IF gv_error EQ abap_true.
            ADD 1 TO gv_bdc_error.
            IF gv_frmro EQ abap_true. "/B-ann20120403
              "--Retain records with MRO in table if an error was encountered
              ADD 1 TO gv_wmro.
              mc_exception abap_true ps_mr gv_frmro ' '  lv_rof_msgs.
            ENDIF.                    "E-ann20120403
            RETURN.
          ENDIF.

          "-- Since the consumption is zero, re-check the Release Status.
          "-- Status 4 means it was successfully corrected. Status 2 means the correction failed and an exception has to be produced.
          PERFORM recheck_release_status USING lv_rof_msgs ps_mr. "Added by GDIMALIWAT 12/16/2013
        ENDIF.
        ADD 1 TO gv_recon.
    ENDCASE.
  ELSE.  "/B-ann20120403
    ADD 1 TO gv_bdc_error.
    IF gv_frmro EQ abap_true. "/B-ann20120403
      "--Retain records with MRO in table if an error was encountered
      ADD 1 TO gv_wmro.
      mc_exception abap_true ps_mr gv_frmro ' '  ' '.
    ENDIF.                    "E-ann20120403
    APPEND INITIAL LINE TO gt_bdc_error ASSIGNING <fs_excep>.
    <fs_excep>-busarea  = ps_mr-busarea.
    <fs_excep>-cacct    = ps_mr-cacct.
    <fs_excep>-buspart  = ps_mr-buspart.
    <fs_excep>-install  = ps_mr-install.
    <fs_excep>-statcode = ps_mr-stat_code.
    <fs_excep>-mrdatf   = ps_mr-mrdatf.
    <fs_excep>-reading  = ps_mr-reading.
    <fs_excep>-mro      = gv_frmro.       "Indicator that record came from MRO Table
    CONCATENATE text-039 ps_mr-mrdate INTO <fs_excep>-remarks SEPARATED BY space. "Record not process
  ENDIF. "/E-ann20120404
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  GET_ACCOUNT_DETAILS
*&---------------------------------------------------------------------*
*       Get Account details of Meter Reading
*----------------------------------------------------------------------*
*     <--  pv_busarea    Business Area
*     <--  pv_cacct      Contract Account
*     <--  pv_buspart    Business Partner
*     <--  pv_contract   Contract
*     <--  pv_install    Installation No.
*     <--  pv_billblock  Reason for bill blocking
*----------------------------------------------------------------------*
FORM get_account_details CHANGING pv_busarea   TYPE gsber
                                  pv_cacct     TYPE char12
                                  pv_buspart   TYPE gpart_kk
                                  pv_contract  TYPE vertrag
                                  pv_install   TYPE anlage
                                  pv_billblock TYPE abrsperr.
  DATA: lv_cacct TYPE char12.
  mc_add_leading_zeroes pv_cacct lv_cacct.
  "--Get corresponding business partner, contract and installation no.
  SELECT SINGLE a~gpart b~vertrag b~anlage b~abrsperr b~gsber
    INTO (pv_buspart, pv_contract, pv_install, pv_billblock, pv_busarea)
    FROM fkkvkp AS a INNER JOIN ever AS b
                             ON ( a~vkont EQ b~vkonto )
    WHERE a~vkont   EQ lv_cacct
      AND b~bukrs   EQ zcl_const_isu=>co_company
      AND b~anlage  NE space.
ENDFORM.                    " get_account_details
*&---------------------------------------------------------------------*
*&      Form  CREATE_DISCON_DOCUMENT
*&---------------------------------------------------------------------*
*       BDC to Create Disconnection Document via tcode EC85
*----------------------------------------------------------------------*
*       -->  ps_mr         Discon/Recon line entry
*       -->  pv_rof_msgs   ROF-related Messages
*       <--  pv_discno     Disconnection doc.
*----------------------------------------------------------------------*
FORM create_discon_document USING    ps_mr       TYPE ty_mrdata
                                     pv_rof_msgs TYPE char50    "Inserted by Ann 2012/09/13
                            CHANGING pv_discno   TYPE discno.
  DATA: lv_discno TYPE discno.

  CONCATENATE ps_mr-stat_code ps_mr-mrdatf ps_mr-rdng_fin ps_mr-seal_no ps_mr-remarks INTO gv_notes SEPARATED BY space. "inserted by GDIMALIWAT 09/09/2013

  mc_bdc_dynpro: 'X'  'SAPLEC85'             '0100',
                ' '  'BDC_OKCODE'           '/00',
                ' '  'EDISCD-DISCNO'        ' ',
                ' '  'RAD_DR-CUSTOMER'      abap_true,
                ' '  'BDC_CURSOR'           'EDISCD-RPARTNER',
                ' '  'RAD_OBJ-PARTNER'      abap_true,
                ' '  'EDISCD-RPARTNER'      ps_mr-buspart,     "Business Partner
                'X'  'SAPLES34'             '0200',
                ' '  'BDC_OKCODE'           '=SAVE',
                ' '  'EDISCDOCS-BILLREL'    abap_true,
                ' '  'BDC_CURSOR'           'EENO_DYNP-ZEILE(01)',
                ' '  'EENO_DYNP-ZEILE(01)'  gv_notes.
  PERFORM bdc_transaction USING 'EC85' ps_mr pv_rof_msgs CHANGING pv_discno.  "Inserted ROF_MSGS by Ann 2012/09/13
  "--Check disconnection number if created in EDISCDOC
  mc_add_leading_zeroes pv_discno pv_discno.
  SELECT SINGLE discno INTO lv_discno
    FROM ediscdoc
    WHERE discno EQ pv_discno.
  IF sy-subrc NE 0.
    CLEAR pv_discno.
  ENDIF.
ENDFORM.                    " create_discon_document
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
*      -->  pv_tcode      Transaction Code
*      -->  ps_mr         Discon/Recon line entry
*      -->  pv_rof_msgs   ROF-related Messages
*      <--  pv_discno     Disconnection No.
*----------------------------------------------------------------------*
FORM bdc_transaction  USING    pv_tcode    TYPE sy-tcode
                               ps_mr       TYPE ty_mrdata
                               pv_rof_msgs TYPE char50     "Inserted by Ann 2012/09/13
                      CHANGING pv_discno   TYPE discno.

  DATA: lt_bdcmsgs            TYPE tab_bdcmsgcoll.
  FIELD-SYMBOLS: <fs_bdcmsgs> TYPE bdcmsgcoll.
  "--Call Transaction
  CALL TRANSACTION pv_tcode USING gt_bdcdata OPTIONS FROM gs_option "#EC CI_CALLTA
                                             MESSAGES INTO lt_bdcmsgs.
  IF sy-subrc EQ 0.
    "--Get disconnection document for t-code EC85
    IF pv_tcode EQ 'EC85'. "Create disconnection number
      READ TABLE lt_bdcmsgs ASSIGNING <fs_bdcmsgs> INDEX 1.
      IF sy-subrc EQ 0.
        IF <fs_bdcmsgs>-msgtyp EQ 'S' AND <fs_bdcmsgs>-msgv2 IS NOT INITIAL.
          pv_discno = <fs_bdcmsgs>-msgv2.
        ENDIF.
      ENDIF.
    ENDIF.
    IF lt_bdcmsgs[] IS  NOT INITIAL.
      PERFORM get_bdc_messages TABLES lt_bdcmsgs USING pv_tcode ps_mr pv_rof_msgs.  "Inserted by Ann 2012/09/13
    ENDIF.
  ELSE.
    PERFORM get_bdc_messages TABLES lt_bdcmsgs USING pv_tcode ps_mr pv_rof_msgs.  "Inserted by Ann 2012/09/13
  ENDIF.
  REFRESH gt_bdcdata.
ENDFORM.                    " BDC_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  GET_BDC_MESSAGES
*&---------------------------------------------------------------------*
*       Get bdc messages
*----------------------------------------------------------------------*
*      -->  pt_bdcmsgs    BDC Messages table
*      -->  pv_tcode      Transaction Code
*      -->  ps_mr         Discon/Recon line entry
*      -->  pv_rof_msgs   ROF-related Messages
*----------------------------------------------------------------------*
FORM get_bdc_messages  TABLES pt_bdcmsgs  TYPE tab_bdcmsgcoll
                       USING  pv_tcode    TYPE sy-tcode
                              ps_mr       TYPE ty_mrdata
                              pv_rof_msgs TYPE char50.   "Inserted by Ann 2012/09/13

  DATA: lv_msgs     TYPE char255,
        lv_ctr      TYPE i,
        lv_ctr10    TYPE char10.
  FIELD-SYMBOLS: <fs_err> TYPE ty_exception.

  LOOP AT pt_bdcmsgs ASSIGNING <fs_msgs>.
    CLEAR lv_msgs.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = <fs_msgs>-msgid
        lang      = sy-langu
        no        = <fs_msgs>-msgnr
        v1        = <fs_msgs>-msgv1
        v2        = <fs_msgs>-msgv2
        v3        = <fs_msgs>-msgv3
        v4        = <fs_msgs>-msgv4
      IMPORTING
        msg       = lv_msgs
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      ADD 1 TO lv_ctr.
      lv_ctr10 = lv_ctr.
      IF lv_msgs+0(19) EQ text-021.
        <fs_msgs>-msgtyp = 'E'.
      ELSE.
        FIND FIRST OCCURRENCE OF text-029 IN lv_msgs IGNORING CASE.   "Checked if locked by user
        IF sy-subrc EQ 0.
          <fs_msgs>-msgtyp = 'E'.
        ELSE.
          FIND FIRST OCCURRENCE OF text-038 IN lv_msgs IGNORING CASE. "Checked if w/ "does not exist in the screen"
          IF sy-subrc EQ 0.
            <fs_msgs>-msgtyp = 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
      CONCATENATE lv_ctr10 lv_msgs INTO lv_msgs SEPARATED BY space.
      CONDENSE lv_msgs.
      mc_output_msgs ps_mr-cacct ps_mr-buspart ps_mr-install gv_frmro pv_tcode ps_mr-rdng_fin <fs_msgs>-msgtyp lv_msgs
                     pv_rof_msgs.  "Inserted by Ann 2012/09/13
      IF <fs_msgs>-msgtyp CA 'AE'.
        CONCATENATE pv_tcode <fs_msgs>-msgtyp lv_msgs INTO lv_msgs SEPARATED BY space.
        gv_error = abap_true.
        "--Create file for BDC Errors
        APPEND INITIAL LINE TO gt_bdc_error ASSIGNING <fs_err>.
        <fs_err>-busarea  = ps_mr-busarea.
        <fs_err>-cacct    = ps_mr-cacct.
        <fs_err>-buspart  = ps_mr-buspart.
        <fs_err>-install  = ps_mr-install.
        <fs_err>-statcode = ps_mr-stat_code.
        <fs_err>-mrdatf   = ps_mr-mrdatf.
        <fs_err>-reading  = ps_mr-reading.
        <fs_err>-mro      = gv_frmro.   "Indicator that record came from MRO Table
        <fs_err>-remarks  = lv_msgs.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_bdc_messages
*&---------------------------------------------------------------------*
*&      Form  GET_DISCON_DETAILS
*&---------------------------------------------------------------------*
*       Get disconnection details of the created discon document
*----------------------------------------------------------------------*
*      -->  pv_cacct      Contract Account
*      -->  pv_buspart    Business Partner
*      -->  pv_install    Installation No.
*      -->  pv_docstat    Document Status
*      -->  pv_statcode   Activity type (DIS/REC)
*      <--  ps_discdoc    Disconnection No. & Status
*      <--  pv_rec        Number of records
*----------------------------------------------------------------------*
FORM get_discon_details USING pv_cacct      TYPE char12
                              pv_buspart    TYPE gpart_kk
                              pv_install    TYPE anlage
                              pv_docstat    TYPE edcdocstat
                              pv_statcode   TYPE char3
                        CHANGING ps_discdoc TYPE ty_discdoc
                                 pv_rec     TYPE i.
  DATA: lv_buspart TYPE gpart_kk,
        lv_refobj  TYPE char22,
        lt_discno  TYPE TABLE OF ty_discdoc.
  REFRESH gr_status.
  IF pv_docstat EQ co_docstat.
    mc_discon_status pv_docstat.
  ENDIF.
  IF pv_statcode EQ co_discon.
    mc_discon_status 1.  "Newly created disconnection document
  ENDIF.
  mc_add_leading_zeroes pv_buspart lv_buspart.
  "--Get disconnection number created (business partner)
  SELECT discno status INTO TABLE lt_discno
    FROM ediscdoc
    WHERE refobjtype EQ co_refobjbprt  "Business Partner
      AND refobjkey  EQ lv_buspart
      AND status     IN gr_status.
  "--Get disconnection number created thru contract account and business partner
  CONCATENATE pv_cacct pv_buspart INTO lv_refobj.
  mc_add_leading_zeroes lv_refobj lv_refobj.
  SELECT discno status APPENDING TABLE lt_discno
    FROM ediscdoc
    WHERE refobjtype EQ co_refobjacct  "ISU Account
      AND refobjkey  EQ lv_refobj
      AND status     IN gr_status.
  "--Get disconnection number created thru Installation Number
  SELECT discno status APPENDING TABLE lt_discno
    FROM ediscdoc
    WHERE refobjtype EQ co_refobjinst  "Installation
      AND refobjkey  EQ pv_install
      AND status     IN gr_status.
  IF lt_discno[] IS NOT INITIAL.
    SORT lt_discno BY discno DESCENDING.
    DESCRIBE TABLE lt_discno LINES pv_rec.
    READ TABLE lt_discno INTO ps_discdoc INDEX 1.
  ENDIF.
  REFRESH gr_status.
ENDFORM.                    " get_discon_details
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISCON_DOCUMENT
*&---------------------------------------------------------------------*
*       Change Disconnection Document via tcode EC86
*----------------------------------------------------------------------*
*      --> pv_discon      Disconnection Indicator
*      --> pv_discno      Disconnection No.
*      --> pv_mrreason    Meter Reading Reason
*      --> pv_rof_msgs    ROF-related Messages
*      --> ps_mr          Discon/Recon line entry
*----------------------------------------------------------------------*
FORM change_discon_document USING pv_discon   TYPE char3
                                  pv_discno   TYPE discno
                                  pv_mrreason TYPE ablesgr
                                  pv_rof_msgs TYPE char50   "Inserted by Ann 2012/09/13
                                  ps_mr       TYPE ty_mrdata.

*  "Modified by GDIMALIWAT 12/11/2013
*  DATA: lv_name     TYPE c LENGTH 12,
*        lv_tname    TYPE thead-tdname,
*        lv_lang     TYPE thead-tdspras,
*        lv_lines    TYPE TABLE OF tline,
*        lv_oldtext  TYPE string.
*
*  FIELD-SYMBOLS:  <fs_lines> TYPE tline.
*  "End of modification by GDIMALIWAT 12/11/2013
  DATA: lv_time TYPE char8.
  WRITE sy-uzeit TO lv_time.

  mc_bdc_dynpro: 'X'  'SAPLEC85'                '0100',
               ' '  'BDC_CURSOR'              'EDISCD-DISCNO',
               ' '  'BDC_OKCODE'              '/00',
               ' '  'EDISCD-DISCNO'           pv_discno.
  IF pv_discon EQ co_discon.
    "--Change Disconnection Document
    mc_bdc_dynpro: 'X'  'SAPLES34'                '0200',
                  ' '  'BDC_OKCODE'              '=DCED',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'EENO_DYNP-ZEILE(01)',
                  'X'  'SAPLES34'                '0310',
                  ' '  'BDC_OKCODE'              '=TAB_READ',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'EDISCPOSS-DISCTYPE(01)',
                  ' '  'EDISCACTS-ACTDATE'       ps_mr-mrdate,    "Date in  MM/DD/YYYY format
                  ' '  'EDISCACTS-ACTTIME'       lv_time,
                  ' '  'EDISCPOSS-ACTDATE(01)'   ps_mr-mrdate,    "Date in MM/DD/YYYY format
                  ' '  'EDISCPOSS-DISCTYPE(01)'  co_disctype,
                  ' '  'EDISCPOSS-SELECTED(01)'  abap_true,
                  'X'  'SAPLES34'                '0310',
                  ' '  'BDC_OKCODE'              '=EL01_250F',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'REABLD-GERNR(01)',
                  ' '  'REL28D-ISTABLART'        co_mrtype,
                  ' '  'REL28D-ABLESER'          co_mrnumber,
                  ' '  'REABLD-ZWSTAND(01)'      ps_mr-rdng_fin,  "Final Reading
                  ' '  'REABLD-ABLHINW(01)'      ' ',
                  ' '  'REABLD-LINE_MARK(01)'    abap_true,
                  'X'  'SAPLES34'                '0310',
                  ' '  'BDC_OKCODE'              '=SAVE',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'REABLD-ZWSTAND(01)',
                  ' '  'REL28D-ISTABLART'        '01',
                  ' '  'REL28D-ABLESER'          co_mrnumber.
    IF pv_mrreason EQ '21'.
      mc_bdc_dynpro: 'X'  'SAPLSPO1'              '0600',
                    ' '  'BDC_OKCODE'            '=OPT1'.
    ENDIF.
  ELSE.
*    "-- Modified by GDIMALIWAT 12/11/2013
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = pv_discno
*    IMPORTING
*      output = lv_name.
*
*    lv_tname = lv_name.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = 'E'
*      IMPORTING
*        output = lv_lang.
*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        client                  = sy-mandt
*        id                      = 'ISU'
*        language                = lv_lang
*        name                    = lv_tname
*        object                  = 'EDCN'
*      TABLES
*        lines                   = lv_lines
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*
*    CLEAR lv_oldtext.
*
*    LOOP AT lv_lines ASSIGNING <fs_lines>.
*      IF sy-tabix EQ 1.
*        MOVE <fs_lines>-tdline TO lv_oldtext.
*      ELSE.
*        CONCATENATE lv_oldtext <fs_lines>-tdline INTO lv_oldtext SEPARATED BY space.
*      ENDIF.
*    ENDLOOP.
*    UNASSIGN <fs_lines>.
*
*    CONCATENATE ps_mr-stat_code ps_mr-mrdatf ps_mr-rdng_fin ps_mr-seal_no ps_mr-remarks INTO gv_notes SEPARATED BY space.
*    CONCATENATE gv_notes lv_oldtext INTO gv_notes SEPARATED BY space.
*    "-- End of modification by GDIMALIWAT 12/11/2013


*"--Update Disconnection Document for Notes
*   mc_bdc_dynpro: 'X'  'SAPLES34'                '0200',
*                  ' '  'BDC_OKCODE'              '=SAVE',
*                  ' '  'BDC_CURSOR'              'EENO_DYNP-ZEILE(01)',
*                  ' '  'EENO_DYNP-ZEILE(01)'     gv_notes. "Added by GDIMALIWAT 12/11/2013
*   PERFORM bdc_transaction USING 'EC86' ps_mr pv_rof_msgs CHANGING pv_discno.

    "--Update Disconnection Document for Reconnection
    mc_bdc_dynpro: 'X'  'SAPLES34'                '0200',
                  ' '  'BDC_OKCODE'              '=RCED',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
*                  ' '  'BDC_CURSOR'              'EENO_DYNP-ZEILE(01)',
*                  ' '  'EENO_DYNP-ZEILE(01)'     gv_notes, "Added by GDIMALIWAT 12/11/2013
*                  'X'  'SAPLSPO1'                '0600',   "Added by GDIMALIWAT 12/16/2013
*                  ' '  'BDC_OKCODE'              '=OPT1',  "Added by GDIMALIWAT 12/16/2013
                  'X'  'SAPLES34'                '0330',
                  ' '  'BDC_OKCODE'              '=TAB_READ',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'EDISCPOSS-ACTDATE(01)',
                  ' '  'EDISCACTS-ACTDATE'       ps_mr-mrdate,    "Date in MM/DD/YYYY format
                  ' '  'EDISCACTS-ACTTIME'       lv_time,
                  ' '  'EDISCPOSS-ACTDATE(01)'   ps_mr-mrdate,    "Date in MM/DD/YYYY format
                  ' '  'EDISCPOSS-SELECTED(01)'  abap_true,       "Selection mark inserted by ann-20120416
                  'X'  'SAPLES34'                '0330',
                  ' '  'BDC_OKCODE'              '=EL01_250F',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'REABLD-GERNR(01)',
                  ' '  'REL28D-ISTABLART'        co_mrtype,
                  ' '  'REL28D-ABLESER'          co_mrnumber,
                  ' '  'REABLD-ZWSTAND(01)'      ps_mr-rdng_fin,  "Final reading
                  ' '  'REABLD-ABLHINW(01)'      ' ',
                  ' '  'REABLD-LINE_MARK(01)'    abap_true,
                  'X'  'SAPLES34'                '0330',
                  ' '  'BDC_OKCODE'              '=SAVE',
                  ' '  'EDISCDOCS-BILLREL'       abap_true,
                  ' '  'BDC_CURSOR'              'REABLD-ZWSTAND(01)',
                  ' '  'REL28D-ISTABLART'        co_mrtype,
                  ' '  'REL28D-ABLESER'          co_mrnumber.
  ENDIF.
  PERFORM bdc_transaction USING 'EC86' ps_mr pv_rof_msgs CHANGING pv_discno.
ENDFORM.                    " change_discon_document
*&---------------------------------------------------------------------*
*&      Form  BLOCK_CONTRACT
*&---------------------------------------------------------------------*
*       Block/Unblock Contract via tcode ES21
*----------------------------------------------------------------------*
*      --> pv_blkcontrct  Reason for Blocking Contract
*      --> pv_rof_msgs    ROF-related Messages
*      --> ps_mr          Discon/Recon line entry
*----------------------------------------------------------------------*
FORM block_contract USING pv_blkcontrct TYPE abrsperr
                          pv_rof_msgs   TYPE char50
                          ps_mr         TYPE ty_mrdata.

  DATA: lv_discno TYPE discno.
  mc_bdc_dynpro: 'X'   'SAPLES20'        '0100',
                ' '   'BDC_CURSOR'      'EVERD-VERTRAG',
                ' '   'BDC_OKCODE'      '/00',
                ' '   'EVERD-VERTRAG'   ps_mr-contract,
                'X'   'SAPLES20'        '0200',
                ' '   'BDC_OKCODE'      '=TAB02',
                ' '   'BDC_CURSOR'      'EVERD-VBEZ',
                'X'   'SAPLES20'        '0200',
                ' '   'BDC_OKCODE'      '=SAVE',
                ' '   'BDC_CURSOR'      'EVERD-ABRSPERR',
                ' '   'EVERD-ABRSPERR'  pv_blkcontrct.  "06 - TCD, ' ' - Unblock
  PERFORM bdc_transaction USING 'ES21' ps_mr pv_rof_msgs CHANGING lv_discno .  "Inserted ROF_MSGS by Ann 2012/09/13
ENDFORM.                    " block_contract
*&---------------------------------------------------------------------*
*&      Form  SET_MR_RELEASE_STATUS
*&---------------------------------------------------------------------*
*      Set MR status in EABL to Released by Agent
*----------------------------------------------------------------------*
*      --> pv_rof_msgs   ROF-related Messages
*      --> ps_mr         Discon/Recon line entry
*----------------------------------------------------------------------*
FORM set_mr_release_status  USING pv_rof_msgs TYPE char50   "Inserted by Ann 2012/09/13
                                  ps_mr       TYPE ty_mrdata.

  DATA: lv_mrreason TYPE ablesgr.
  "--Set Meter Reading Reason
  IF ps_mr-stat_code EQ co_discon.
    lv_mrreason = '13'.  "Meter Reading on Disconnection
  ELSE.
    lv_mrreason = '18'.  "Reconnection Meter Reading
  ENDIF.
  SELECT ablbelnr adatsoll ablesgr ablstat
    INTO TABLE gt_mrdoc
    FROM v_eabl
    WHERE anlage  EQ ps_mr-install
      AND ablesgr EQ lv_mrreason
      AND ablstat EQ '2'.        "Automatically Locked
  IF sy-subrc EQ 0.
    SORT gt_mrdoc BY adatsoll DESCENDING.
    READ TABLE gt_mrdoc ASSIGNING <fs_mrdoc> INDEX 1.
    IF sy-subrc EQ 0.
      IF <fs_mrdoc>-adatsoll+0(6) NE ps_mr-mrdatf+0(6).
        "--Directly update MR status of records with MR reading date not within entry period
        UPDATE eabl SET   ablstat  = '4'
                    WHERE ablbelnr = <fs_mrdoc>-ablbelnr.
      ELSE.
        "--BDC to release and save all accounts for MR Status = 2 (Automatically locked)
        PERFORM correct_implausible_mrresult USING pv_rof_msgs ps_mr <fs_mrdoc>-adatsoll. "Inserted ROF_MSGS by Ann 2012/09/13
      ENDIF.
    ENDIF.
  ENDIF.
  REFRESH gt_mrdoc.
  UNASSIGN <fs_mrdoc>.
ENDFORM.                    " set_mr_release_status
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RECON_REMARKS
*&---------------------------------------------------------------------*
*       BDC to Update Reconnection Remarks via tcode EC86
*----------------------------------------------------------------------*
*      --> pv_discno     Disconnection No.
*      --> pv_rof_msgs   ROF-related Messages
*      --> ps_mr         Discon/Recon line entry
*----------------------------------------------------------------------*
FORM update_recon_remarks  USING pv_discno     TYPE discno
                                 pv_rof_msgs   TYPE char50    "Inserted by Ann 2012/09/13
                                 ps_mr         TYPE ty_mrdata.
  "Modified by GDIMALIWAT 09/09/2013
  DATA: lv_name     TYPE c LENGTH 12,
        lv_tname    TYPE thead-tdname,
        lv_lang     TYPE thead-tdspras,
        lv_lines    TYPE TABLE OF tline,
        lv_oldtext  TYPE string.

  FIELD-SYMBOLS:  <fs_lines> TYPE tline.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pv_discno
    IMPORTING
      output = lv_name.

  lv_tname = lv_name.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = 'E'
    IMPORTING
      output = lv_lang.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'ISU'
      language                = lv_lang
      name                    = lv_tname
      object                  = 'EDCN'
    TABLES
      lines                   = lv_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CLEAR lv_oldtext.

  LOOP AT lv_lines ASSIGNING <fs_lines>.
    IF sy-tabix EQ 1.
      MOVE <fs_lines>-tdline TO lv_oldtext.
    ELSE.
      CONCATENATE lv_oldtext <fs_lines>-tdline INTO lv_oldtext SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_lines>.

  CONCATENATE ps_mr-stat_code ps_mr-mrdatf ps_mr-rdng_fin ps_mr-seal_no ps_mr-remarks INTO gv_notes SEPARATED BY space.
  CONCATENATE gv_notes lv_oldtext INTO gv_notes SEPARATED BY space.
  "End of Modifs by GDIMALIWAT 09/09/2013
  mc_bdc_dynpro: 'X'  'SAPLEC85'             '0100',
                ' '  'BDC_CURSOR'           'EDISCD-DISCNO',
                ' '  'BDC_OKCODE'           '/00',
                ' '  'EDISCD-DISCNO'        pv_discno,
                ' '  'BDC_SUBSCR'           'SAPLEC85                                0001RSLSUB',
                'X'  'SAPLES34'             '0200',
                ' '  'BDC_OKCODE'           '=SAVE',
                ' '  'BDC_SUBSCR'           'SAPLES34                                0405HAESUB',
                ' '  'EDISCDOCS-BILLREL'    abap_true,
                ' '  'BDC_SUBSCR'	          'SAPLES34SUBSCREEN                       0410ROBSUB',
                ' '  'BDC_SUBSCR'	          'SAPLES34                                0400ACTSUB',
                ' '  'BDC_SUBSCR'	          'SAPLEWD2                                0203OBJSUB',
                ' '  'BDC_SUBSCR'	          'SAPLEENO                                1001NOTSUB',
                ' '  'BDC_CURSOR'	          'EENO_DYNP-ZEILE(01)',
                ' '  'EENO_DYNP-ZEILE(01)'  gv_notes.
  PERFORM bdc_transaction USING 'EC86' ps_mr pv_rof_msgs CHANGING pv_discno.  "Inserted ROF_MSGS by Ann 2012/09/13
ENDFORM.                    " update_recon_remarks
*&---------------------------------------------------------------------*
*&      Form  CORRECT_PLAUSIBLE_MRRESULT
*&---------------------------------------------------------------------*
*       Correct Plausible MR Result via t-code EL29
*----------------------------------------------------------------------*
*      --> pv_msg_rof    ROF-related Messages
*      --> ps_mr         Discon/Recon line entry
*      --> pv_reason     Discon/Recon reason code (13/18)
*----------------------------------------------------------------------*
FORM correct_plausible_mrresult   USING pv_msg_rof TYPE char50    "Inserted by Ann 2012/09/13
                                        ps_mr      TYPE ty_mrdata
                                        value(pv_reason).
  DATA: lv_discno TYPE discno.
  mc_bdc_dynpro: 'X'  'SAPLEL01'                   '0120',
                ' '  'BDC_CURSOR'                 'REL28D-ADAT',
                ' '  'BDC_OKCODE'                  '/00',
                ' '  'RELX1-PARTNER_DY'            abap_true,
                ' '  'REL28D-KUNDE'                ps_mr-buspart,  "Business Partner
                ' '  'REL28D-ANLAGE'               ps_mr-install,  "Installation no.
                ' '  'REL28D-ADAT'                 ps_mr-mrdate,   "MR date MM/DD/YYYY
                ' '  'REL28D-ABLESGR'              pv_reason,      "inserted by ann 04/16/2012
                'X'  'SAPLEL01'                    '0240',
                ' '  'BDC_CURSOR'                  'RVALDAT-CONSMPT_MRENT(01)',
                ' '  'BDC_OKCODE'                  '=SAVE',
                ' '  'REABLD-ADATTATS(01)'         ps_mr-mrdate,  "MR Date
                ' '  'RVALDAT-CONSMPT_MRENT(01)'   '0',           "Set consumption to zero
                "Added by GDIMALIWAT 11/19/2013 to fix BDC no input error
                'X'  'SAPLEL01'                    '0240',
                ' '  'BDC_CURSOR'                  'RVALDAT-CONSMPT_MRENT(01)',
                ' '  'BDC_OKCODE'                  '=SAVE'.
  PERFORM bdc_transaction USING 'EL29' ps_mr pv_msg_rof CHANGING lv_discno.  "Inserted ROF_MSGS by Ann 2012/09/13
ENDFORM.                    " correct_plausible_mrresult
*&---------------------------------------------------------------------*
*&      Form  CORRECT_IMPLAUSIBLE_MRRESULT
*&---------------------------------------------------------------------*
*       Correct Implausible MR Reading via t-code EL27, this will
*       release the account blocking for accounts with MR Status = 2
*----------------------------------------------------------------------*
*      --> pv_rof_msgs   ROF-related Messages
*      --> ps_mr         Discon/Recon line entry
*      --> pv_mrdate_s   Meter Reading Date in SAP
*----------------------------------------------------------------------*
FORM correct_implausible_mrresult  USING pv_rof_msgs TYPE char50
                                         ps_mr       TYPE ty_mrdata
                                         pv_mrdate_s TYPE sy-datum.
  DATA: lv_date   TYPE char10,
        lv_discno TYPE discno.
  WRITE pv_mrdate_s TO lv_date.
  mc_bdc_dynpro: 'X'  'SAPLEL01'           '0120',
                ' '  'RELX1-PARTNER_DY'   abap_true,
                ' '  'REL28D-KUNDE'       ps_mr-buspart,  "Business Partner
                ' '  'REL28D-ADAT'        lv_date,
                ' '  'BDC_OKCODE'         '/00',
                'X'  'SAPLEL01'           '0510',
                ' '  'BDC_OKCODE'         '=REL',
                'X'  'SAPLEL01'           '0510',
                ' '  'BDC_OKCODE'         '=SAVE'.
  PERFORM bdc_transaction USING 'EL27' ps_mr pv_rof_msgs  CHANGING lv_discno.  "Inserted ROF_MSGS by Ann 2012/09/13
ENDFORM.                    " correct_implausible_mrresult
*&---------------------------------------------------------------------*
*&      Form  CREATE_EXCEPTION_FILE
*&---------------------------------------------------------------------*
*       Create exception file in .txt format if existing
*----------------------------------------------------------------------*
FORM create_exception_file .

  DATA: lt_ba_count     TYPE ztisu_gsber_count,  "Inserted by Ann 05/25/2012
        ls_ba_count     TYPE zstisu_gsber_count, "Inserted by Ann 05/25/2012
        lv_attacheddoc  TYPE string,             "Inserted by Ann 05/25/2012
        lv_log          TYPE string,
        lv_excepline    TYPE char300,
        lv_lineitem     TYPE char300,
        lv_tcode        TYPE char5,
        lv_msgtyp       TYPE char6,
        lv_length       TYPE i,
        lv_col_rem      TYPE char11.  "Inserted by Ann 10/19/2012
  FIELD-SYMBOLS: <fv_line>  TYPE string,
                 <fs_out>   TYPE ty_output_msgs.

  lv_length = strlen( pa_fpath ) - 4.
  "--Check if with '.txt' extension
  IF pa_fpath+lv_length(4) NE '.txt' AND pa_fpath+lv_length(4) NE '.TXT'.
    lv_length = strlen( pa_fpath ).
  ENDIF.

  "--Delete current records in MRO table
  IF gv_mrorec NE 0.
    PERFORM show_progress USING 0 text-035.
    DELETE FROM ztisu_discon_mro WHERE zzfiledate LE sy-datum. "#EC CI_NOFIRST
  ENDIF.
  "--Update MRO table
  IF gt_excep_mro[] IS NOT INITIAL.
    DESCRIBE TABLE gt_excep_mro LINES gv_mrorecn.
    PERFORM show_progress USING 0 text-036.
    MODIFY ztisu_discon_mro FROM TABLE gt_excep_mro.
    IF sy-dbcnt NE 0.
      gv_mroupd = sy-dbcnt.
    ENDIF.
  ENDIF.

  "--Create Exception if exception messages exists
  IF gt_exception[] IS NOT INITIAL.
    PERFORM show_progress USING 0 text-033.
    IF rb_mro EQ abap_true.
      CONCATENATE pa_fpath+0(lv_length) co_mro co_excep INTO gv_filepath SEPARATED BY co_uscore.
    ELSE.
      CONCATENATE pa_fpath+0(lv_length) co_excep INTO gv_filepath SEPARATED BY co_uscore.
    ENDIF.
    IF sy-batch IS NOT INITIAL.
      WRITE:/ lv_length, gv_filepath.
    ENDIF.
    lv_lineitem = text-025.
    lv_col_rem  = text-040.             "Inserted by Ann 10/19/2012
    TRANSLATE lv_col_rem TO UPPER CASE. "Inserted by Ann 10/19/2012
    CONCATENATE lv_lineitem+0(151) lv_col_rem INTO lv_lineitem SEPARATED BY zcl_const_isu=>co_pipe RESPECTING BLANKS.  "Inserted by Ann 09/13/2012
    "--Transfer Data to local file
    IF rb_local EQ abap_true.
      "--Display exceptions in screen
      PERFORM download_exception TABLES gt_exception USING gv_filepath lv_lineitem.
    ELSE.
      SORT gt_exception BY busarea.
      "--Transfer Data to server
      WRITE gv_filepath TO lv_excepline.
      OPEN DATASET lv_excepline FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      TRANSFER lv_lineitem TO lv_excepline.
      IF cb_email EQ abap_true.  "--Inserted by Ann 06/07/2012
        "--Inserted by Ann 05/29/2012
        "---Create Header for attached document
        REPLACE ALL OCCURRENCES OF co_pipe IN lv_lineitem WITH cl_bcs_convert=>gc_tab.
        CONCATENATE lv_lineitem cl_bcs_convert=>gc_crlf INTO lv_attacheddoc.
      ENDIF.                     "/-Inserted by Ann 06/07/2012
      "/-Inserted by Ann 05/29/2012
      LOOP AT gt_exception ASSIGNING <fs_excep>.
        CLEAR: lv_lineitem, ls_ba_count.
        CONCATENATE <fs_excep>-busarea <fs_excep>-install <fs_excep>-cacct <fs_excep>-buspart <fs_excep>-statcode
                    <fs_excep>-mrdatf  <fs_excep>-reading <fs_excep>-mro   <fs_excep>-remarks
                    <fs_excep>-rof_remarks INTO lv_lineitem       "--Inserted by Ann 09/13/2012
          SEPARATED BY co_pipe RESPECTING BLANKS.
        TRANSFER lv_lineitem TO lv_excepline.
        IF cb_email EQ abap_true.  "--Inserted by Ann 06/07/2012
          "--Inserted by Ann 05/29/2012
          "---Create BA Summary Count for exception`
          ls_ba_count-gsber     = <fs_excep>-busarea.
          ls_ba_count-zzbacount = 1.
          COLLECT ls_ba_count INTO lt_ba_count.
          "/-Inserted by Ann 05/29/2012
          "--Inserted by Ann 05/29/2012
          "---Create lines for attached document
          REPLACE ALL OCCURRENCES OF co_pipe IN lv_lineitem WITH cl_bcs_convert=>gc_tab.
          CONCATENATE lv_attacheddoc lv_lineitem cl_bcs_convert=>gc_crlf INTO lv_attacheddoc.
          "/-Inserted by Ann 05/29/2012
        ENDIF.                     "/-Inserted by Ann 06/07/2012
      ENDLOOP.
      CLOSE DATASET lv_excepline.
      "--Inserted by Ann 20120523
      IF sy-subrc EQ 0 AND cb_email EQ abap_true.
        PERFORM send_notification TABLES lt_ba_count USING gv_filepath lv_attacheddoc.
      ENDIF.
      "/-Inserted by Ann 20120523
    ENDIF.
    PERFORM show_progress USING 100 text-010.
  ENDIF.
  "--Create BDC Transaction errors
  IF gt_bdc_error[] IS NOT INITIAL.
    PERFORM show_progress USING 0 text-034.
    CLEAR: lv_excepline.
    IF rb_mro EQ abap_true.
      CONCATENATE pa_fpath+0(lv_length) co_mro co_bdcerror INTO gv_file_error SEPARATED BY co_uscore.
    ELSE.
      CONCATENATE pa_fpath+0(lv_length) co_bdcerror INTO gv_file_error SEPARATED BY co_uscore.
    ENDIF.
    lv_lineitem = text-025.
    "--Transfer Data to local file
    IF rb_local EQ abap_true.
      "--Display exceptions in screen
      PERFORM download_exception TABLES gt_bdc_error USING gv_file_error lv_lineitem.
    ELSE.
      SORT gt_bdc_error BY busarea.
      "--Transfer Data to server
      WRITE gv_file_error TO lv_excepline.
      OPEN DATASET lv_excepline FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      TRANSFER lv_lineitem TO lv_excepline.
      LOOP AT gt_bdc_error ASSIGNING <fs_excep>.
        CLEAR lv_lineitem.
        CONCATENATE <fs_excep>-busarea <fs_excep>-install <fs_excep>-cacct <fs_excep>-buspart <fs_excep>-statcode
                    <fs_excep>-mrdatf  <fs_excep>-reading <fs_excep>-mro   <fs_excep>-remarks INTO lv_lineitem
          SEPARATED BY co_pipe RESPECTING BLANKS.
        TRANSFER lv_lineitem TO lv_excepline.
      ENDLOOP.
      CLOSE DATASET lv_excepline.
    ENDIF.
    PERFORM show_progress USING 100 text-026.
  ENDIF.

  "--Generate Upload Summary
  IF sy-batch IS NOT INITIAL.
    "--Generate Output log file if run in batch/background processing
    PERFORM build_upload_summary TABLES gt_stringtab.
    CONCATENATE pa_fpath+0(lv_length) co_log INTO lv_log SEPARATED BY co_uscore.
    "--Transfer Data to server
    WRITE lv_log TO lv_excepline.
    OPEN DATASET lv_excepline FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    "--Create Header Summary for Output log
    LOOP AT gt_stringtab ASSIGNING <fv_line>.
      WRITE:/ <fv_line>.
      lv_lineitem = <fv_line>.
      TRANSFER lv_lineitem TO lv_excepline.
    ENDLOOP.
    lv_lineitem = sy-uline.
    TRANSFER lv_lineitem TO lv_excepline.
    lv_lineitem = text-028.
    TRANSFER lv_lineitem TO lv_excepline.
    lv_lineitem = sy-uline.
    TRANSFER lv_lineitem TO lv_excepline.
    "--Create Output Log results
    LOOP AT gt_output_msgs ASSIGNING <fs_out>.
      CLEAR lv_lineitem.
      CONDENSE: <fs_out>-tcode, <fs_out>-msgtyp.
      lv_tcode  = <fs_out>-tcode+0(5).
      lv_msgtyp = <fs_out>-msgtyp.
      CONCATENATE <fs_out>-cacct   <fs_out>-buspart <fs_out>-install <fs_out>-mro lv_tcode
                  <fs_out>-reading lv_msgtyp <fs_out>-msgs <fs_out>-rof_msgs  "Inserted rof_msgs by Ann 09/13/2012
                  INTO lv_lineitem SEPARATED BY co_pipe RESPECTING BLANKS.
      TRANSFER lv_lineitem TO lv_excepline.
    ENDLOOP.
    CLOSE DATASET lv_excepline.
  ELSE.
    PERFORM display_upload_summary.
  ENDIF.
ENDFORM.                    " create_exception_file
*&---------------------------------------------------------------------*
*&      Form  GET_PERIOD
*&---------------------------------------------------------------------*
*       Get period range
*----------------------------------------------------------------------*
FORM get_period .

  DATA: lt_result   TYPE match_result_tab,
        lv_offset   TYPE i,
        lv_date     TYPE sy-datum.  "inserted by Ann 05/28/2012
  FIELD-SYMBOLS: <fs_match> TYPE match_result.

  IF pa_fpath IS INITIAL.
    MESSAGE e007 WITH text-023.
  ENDIF.
  FIND ALL OCCURRENCES OF REGEX '(1)|(2)|(3)|(4)|(5)|(6)|(7)|(8)|(9)|(0)'
    IN pa_fpath RESULTS lt_result.
  IF sy-subrc EQ 0.
    SORT lt_result BY offset DESCENDING.
    READ TABLE lt_result ASSIGNING <fs_match> INDEX 1.
    lv_offset      = <fs_match>-offset - 7.
    IF pa_fpath+lv_offset(8) CN '1234567890'.
      MESSAGE e007 WITH text-023.
    ENDIF.
    gv_datefr      = pa_fpath+lv_offset(8).
  ELSE.
    gv_datefr      = sy-datum.
  ENDIF.
  lv_date      = gv_datefr.
  lv_date+6(2) = '01'.   "--First day of the current month in file
  "--Get date of the following month
  CALL FUNCTION 'OIL_GET_NEXT_MONTH'
    EXPORTING
      i_date = lv_date
    IMPORTING
      e_date = gv_dateto.
  "--Get last day of the following month
  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = gv_dateto
    IMPORTING
      e_date = gv_dateto.
ENDFORM.                    " get_period
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_DUPLICATES
*&---------------------------------------------------------------------*
*       Check if record has duplicates
*----------------------------------------------------------------------*
*      --> pv_cacct      Contract Account
*      --> pv_stat_code  Status Code
*      <-- pv_dup        Indicator for duplicate record
*----------------------------------------------------------------------*
FORM check_for_duplicates USING    pv_cacct     TYPE char12
                                   pv_stat_code TYPE char3
                          CHANGING pv_dup       TYPE char1.
  CLEAR gs_mrdup.
  READ TABLE gt_mrdata_dup INTO gs_mrdup WITH KEY cacct     = pv_cacct
                                                  stat_code = pv_stat_code BINARY SEARCH.
  IF sy-subrc EQ 0.
    pv_dup = abap_true.
  ENDIF.
ENDFORM.                    " check_for_duplicates
*&---------------------------------------------------------------------*
*&      Form  GET_MRO_DETAILS
*&---------------------------------------------------------------------*
*       Check if records has Meter Reading Order
*----------------------------------------------------------------------*
*      --> pv_buspart     Business Partner/Customer
*      --> pv_install     Installation No.
*      --> pv_statcode    Status Code  (DIS/REC)
*      --> pv_mrdatf      DISCON/RECON date in file
*      --> pv_mrdoctype   MR Document Type
*      <-- pv_exist       Indicator for existing
*      <-- pv_mrdatbill   MR Date for Billing
*      <-- pv_readresult  Reading Result
*      <-- pv_mrreason    Meter Reading Reason
*      <-- pv_mrdate      Meter Reading Date
*      <-- pv_dial        No. of Dials
*      <-- pv_count       Count no. of DISCON/RECON entries in EL31
*      <-- pv_lpermrdate  Last Periodic Meter Reading Date
*----------------------------------------------------------------------*
FORM get_mro_details  USING  pv_buspart    TYPE gpart_kk
                             pv_install    TYPE anlage
                             pv_statcode   TYPE char3
                             pv_mrdatf     TYPE sy-datum
                             pv_mrdoctype  TYPE mrdocumenttype
                    CHANGING pv_exist      TYPE char1
                             pv_mrdatbill  TYPE adat
                             pv_readresult TYPE readingresult
                             pv_mrreason   TYPE ablesgr
                             pv_mrdate     TYPE adatsoll
                             pv_dial       TYPE i
                             pv_count      TYPE i
                             pv_lpermrdate TYPE adatsoll.  "Inserted by Ann 10/19/2012
  DATA: lt_mrreason      TYPE TABLE OF ty_mrreason,
        lt_mrreason01    TYPE TABLE OF ty_mrreason,
        lt_mrdocdata     TYPE TABLE OF bapieabl,
        ls_mrreason      TYPE ty_mrreason,
        ls_mrdocdata     TYPE bapieabl,
        lv_mridno01      TYPE ablbelnr,
        lv_mridno        TYPE ablbelnr,
        lv_mrdate        TYPE adatsoll,
        lv_dials         TYPE e_zwgruppe,
        lv_date1318      TYPE sy-datum,     "Discon/Recon Date
        lv_date01        TYPE sy-datum.     "Reading Date

  CALL FUNCTION 'BAPI_MTRREADDOC_GETLIST'
    EXPORTING
      customer       = pv_buspart
      targetmrdateto = gv_dateto
      mrdocumenttype = pv_mrdoctype
    TABLES
      mrdocumentdata = lt_mrdocdata.

  CASE pv_mrdoctype.
    WHEN co_mrresult.
      IF lt_mrdocdata[] IS NOT INITIAL.
        pv_exist = abap_true. "--MR Historical Data exists
        "--Check if MRO date GT DISCON/RECON date
        IF pv_mrdate+0(6) GT pv_mrdatf+0(6).
          lv_mrdate = pv_mrdate - 1.
        ELSE.
          lv_mrdate = gv_dateto.
        ENDIF.
        "--Get MR details
        SELECT adatsoll ablbelnr anlage ablesgr abrdats
          INTO TABLE lt_mrreason FROM eablg
          WHERE anlage   EQ pv_install
            AND adatsoll LE lv_mrdate.
        IF sy-subrc EQ 0.
          SORT lt_mrreason BY adatsoll DESCENDING.
          lt_mrreason01[] = lt_mrreason[].
          DELETE: lt_mrreason   WHERE ablesgr NE '13' AND ablesgr NE '18'  "Discon/Recon records only
                                  AND ablesgr NE '21' AND ablesgr NE '22', "Meter reading at Bill-rel. Inst./Removal
                  lt_mrreason01 WHERE ablesgr NE '01'.                     "Retain only Periodic Meter Reading
          "--Get MR Doc. reason for Discon/Recon
          READ TABLE lt_mrreason INTO ls_mrreason INDEX 1.
          IF sy-subrc EQ 0.
            pv_mrreason   = ls_mrreason-ablesgr.   "Get latest discon/recon reason
            lv_date1318   = ls_mrreason-adatsoll.  "Get latest Scheduled Meter Reading details in SAP
            lv_mridno     = ls_mrreason-ablbelnr.
            "--Check if disconnection/reconnection subsequently exists
            IF pv_statcode EQ co_recon.        "Reconnection entry
              IF ls_mrreason-ablesgr = '13'.   "Check if disconnection
                ADD 1 TO pv_count.
              ENDIF.
            ELSEIF pv_statcode EQ co_discon.   "Disconnection entry
              IF ls_mrreason-ablesgr = '18'.   "Check if reconnection
                ADD 1 TO pv_count.
              ENDIF.
            ENDIF.
          ENDIF.
          "--Check if disconnection/reconnection subsequently exists
          READ TABLE lt_mrreason INTO ls_mrreason INDEX 2.
          IF sy-subrc EQ 0.
            IF pv_statcode EQ co_recon.        "Reconnection entry
              IF ls_mrreason-ablesgr = '13'.   "Check if disconnection
                ADD 1 TO pv_count.
              ENDIF.
            ELSEIF pv_statcode EQ co_discon.   "Disconnection entry
              IF ls_mrreason-ablesgr = '18'.   "Check if reconnection
                ADD 1 TO pv_count.
              ENDIF.
            ENDIF.
          ENDIF.
          IF pv_count GT 1.
            RETURN.
          ENDIF.
          "--Get latest Meter Reading Schedule of periodic reading
          READ TABLE lt_mrreason01 INTO ls_mrreason INDEX 1.
          IF sy-subrc EQ 0.
            "--Get latest Meter Reading details in SAP
            pv_mrdatbill  = ls_mrreason-abrdats.                "Scheduled MR Billing
            "pv_lpermrdate = lv_date01 = ls_mrreason-adatsoll.  "Scheduled Meter Reading "Comment out by Ann 01/08/2012
            lv_date01     = ls_mrreason-adatsoll.               "Scheduled Meter Reading "Inserted by Ann 01/08/2012
            lv_mridno01   = ls_mrreason-ablbelnr.
          ENDIF.
          "--Get the latest MR Entry date
          IF lv_date01 GE lv_date1318.
            pv_mrdate = lv_date01.
            lv_mridno = lv_mridno01.
            "pv_lpermrdate = lv_date1318.          "Meter Reading Date for latest disconnection - Inserted by Ann 20121221  "Comment out by Ann 01/08/2012
          ELSE.
            pv_mrdate     = lv_date1318.
          ENDIF.
          "--Inserted by Ann 01/08/2012
          "--Get Meter Reading Date for the latest disconnection = 13
          DELETE lt_mrreason WHERE ablesgr NE '13'.
          READ TABLE lt_mrreason INTO ls_mrreason INDEX 1.
          IF sy-subrc EQ 0.
            "--If last periodic reading is less/equal MR Date at disconnection, get the MR date at latest periodic reading
            IF lv_date01 LE ls_mrreason-adatsoll.
              pv_lpermrdate = lv_date01.
            ELSE.
              pv_lpermrdate = ls_mrreason-adatsoll.
            ENDIF.
          ENDIF.
          "/-Inserted by Ann 01/08/2012
          READ TABLE lt_mrdocdata INTO ls_mrdocdata WITH KEY mridnumber = lv_mridno BINARY SEARCH.
          IF sy-subrc EQ 0.
            pv_readresult = ls_mrdocdata-readingresult.    "Meter Reading entry
            "--Get Reg. Group/Dials
            SELECT SINGLE zwgruppe INTO lv_dials
              FROM egerh
              WHERE equnr EQ ls_mrdocdata-equipment
                AND bis   GE ls_mrdocdata-targetmrdate
                AND ab    LE ls_mrdocdata-targetmrdate.
            IF sy-subrc EQ 0.
              pv_dial = lv_dials.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN OTHERS. "--Check for MRO
      IF lt_mrdocdata[] IS NOT INITIAL.
        "--Check if MRO is within current MR upload month and with MR Reason = '01'
        SORT lt_mrdocdata BY mridnumber DESCENDING.
        READ TABLE lt_mrdocdata INTO ls_mrdocdata INDEX 1.
        IF sy-subrc EQ 0.
          SELECT SINGLE adatsoll ablbelnr anlage ablesgr abrdats
            INTO ls_mrreason FROM eablg
            WHERE ablbelnr EQ ls_mrdocdata-mridnumber.
          IF sy-subrc EQ 0.
            pv_mrdate = ls_mrreason-adatsoll.  "MRO Date
            IF ls_mrreason-ablesgr EQ '01' AND ls_mrreason-adatsoll+0(6) EQ pv_mrdatf+0(6).
              pv_exist = abap_true. "--MRO exists if MR reason = '01' and pv_
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " get_mro_details
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEPTION
*&---------------------------------------------------------------------*
*       Download Exception listings
*----------------------------------------------------------------------*
*      -->  pt_exception  Internal table for exception file
*      -->  pv_filepath   File path and location
*      -->  pv_header     Output file header title
*----------------------------------------------------------------------*
FORM download_exception TABLES pt_exception  TYPE ty_exception_t
                        USING  pv_filepath   TYPE string
                               pv_header     TYPE char300.
  DATA: ls_header TYPE ty_exception.

  ls_header = pv_header.
  REPLACE ALL OCCURRENCES OF co_pipe IN ls_header WITH space.
  INSERT ls_header INTO pt_exception INDEX 1.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = pv_filepath
      write_field_separator   = abap_true
    CHANGING
      data_tab                = pt_exception[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " download_exception
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_UPLOAD_SUMMARY
*&---------------------------------------------------------------------*
*       Display download summary report
*----------------------------------------------------------------------*
FORM display_upload_summary .

  DATA: ls_layout    TYPE slis_layout_alv,
        ls_events    TYPE slis_alv_event,
        ls_sort      TYPE slis_sortinfo_alv,
        lt_sort      TYPE slis_t_sortinfo_alv,
        lt_fieldcat  TYPE slis_t_fieldcat_alv,
        lt_events    TYPE slis_t_event.
  "--Define ALV events
  DEFINE mc_build_events.
    ls_events-form = &1.
    ls_events-name = &2.
    append ls_events to lt_events.
    clear ls_events.
  END-OF-DEFINITION.
  "--- Define Sort for ALV ---
  DEFINE mc_do_sort.
    ls_sort-fieldname = &1.
    ls_sort-up        = &2.
    ls_sort-down      = &3.
    ls_sort-subtot    = &4.
    append ls_sort to lt_sort.
    clear ls_sort.
  END-OF-DEFINITION.
  mc_do_sort: 'CACCT'   abap_true  ' '  ' ',
              'BUSPART' abap_true  ' '  ' ',
              'INSTALL' abap_true  ' '  ' '.
  "--- Define Layout for ALV display ---
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  ls_layout-min_linesize      = 150.
  "--- Define Field Catalog layout ---
  "--- Fld name|Col Name |ref fld|Ref Tab| Sum | Key | Pos | Len |Icon |Htspot|Edit|No Out|Fix Col|FldCat Table --"
  PERFORM write_fieldcat USING:
    'CACCT'    ' '        'VKONTO'   'EVER'        ' '  'X'   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'BUSPART'  ' '        'GPART'    'FKKVKP'      ' '  'X'   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'INSTALL'  ' '        'ANLAGE'   'EABLG'       ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'MRO'      text-014   ' '        ' '           ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'TCODE'    ' '        'TCODE'    'SYST'        ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'READING'  text-032   ' '        ' '           ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'MSGTYP'   ' '        'MSGTYP'   'BDCMSGCOLL'  ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'MSGS'     text-011   ' '        ' '           ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat,
    'ROF_MSGS' text-040   ' '        ' '           ' '  ' '   ' '   ' '   ' '    ' '   ' '   ' '   ' '   lt_fieldcat. "Inserted by Ann 09/13/2012
  PERFORM build_upload_summary TABLES gt_stringtab.
  "--Build events
  mc_build_events: 'TOP_OF_LIST'                            "#EC CALLED
                   'TOP_OF_LIST'.
  "--Create ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-repid
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      it_fieldcat                 = lt_fieldcat
      it_events                   = lt_events
      it_sort                     = lt_sort[]
      is_layout                   = ls_layout
      i_save                      = 'A'
    TABLES
      t_outtab                    = gt_output_msgs[]
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_upload_summary
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
*      -->pv_pos       Position Number
*      -->pv_length    Field Length
*      -->pv_icon      Display as Icon
*      -->pv_hot       Hotspot
*      -->pv_edit      Edit
*      -->pv_no_out    No output
*      -->pv_fix_col   Fixed Column
*      -->pt_fieldcat  Field catalogue table
*----------------------------------------------------------------------*
FORM write_fieldcat USING pv_name pv_txt pv_fld  pv_tab
                          pv_sum  pv_key pv_pos  pv_length
                          pv_icon pv_hot pv_edit pv_no_out
                          pv_fix_col     TYPE c
                          pt_fieldcattbl TYPE slis_t_fieldcat_alv.

  DATA ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
  ls_fieldcat-fieldname     = pv_name.    "Field name
  ls_fieldcat-reptext_ddic  = pv_txt.     "Colunm text
  ls_fieldcat-ref_fieldname = pv_fld.     "Ref Table Field
  ls_fieldcat-ref_tabname   = pv_tab.     "Ref Table name
  ls_fieldcat-do_sum        = pv_sum.     "Do summation
  ls_fieldcat-key           = pv_key.     "Mark for key field
  ls_fieldcat-col_pos       = pv_pos.     "Position Number
  ls_fieldcat-outputlen     = pv_length.  "Field Length
  ls_fieldcat-icon          = pv_icon.    "Display as Icon
  ls_fieldcat-hotspot       = pv_hot.     "Hotspot
  ls_fieldcat-edit          = pv_edit.    "Edit
  ls_fieldcat-no_out        = pv_no_out.  "No output
  ls_fieldcat-fix_column    = pv_fix_col. "Fix column
  APPEND ls_fieldcat TO pt_fieldcattbl.
  CLEAR ls_fieldcat.
ENDFORM.                    " write_fieldcat
*&---------------------------------------------------------------------*
*&      Form  BUILD_UPLOAD_SUMMARY
*&---------------------------------------------------------------------*
*       Create summary for upload in table
*----------------------------------------------------------------------*
*      --> pt_stringtab  Summary table
*----------------------------------------------------------------------*
FORM build_upload_summary TABLES pt_stringtab TYPE stringtab.

  DATA: lv_line    TYPE string,
        lv_tot_rec TYPE i,
        lv_excep   TYPE i.

  DEFINE mc_build_lineitem.
    concatenate &1 &2 into lv_line separated by space.
    condense lv_line.
    append lv_line to pt_stringtab.
    clear lv_line.
  END-OF-DEFINITION.
  "--Total MRO records Queued, Uploaded, Exception, BDCError
  IF gv_mrorec NE 0.
    MESSAGE i013 WITH gv_mrorec gv_mroproc gv_mroexe gv_mroerr INTO lv_line.
    mc_build_lineitem lv_line ' '.
    IF gv_mroret NE 0. "MRO records still with MRO
      MESSAGE i016 WITH gv_mroret INTO lv_line.
      mc_build_lineitem lv_line ' '.
    ENDIF.
  ENDIF.
  "--Total records from Upload file, Uploaded, Exception, BDCError
  IF gv_mrrec NE 0.
    "--Summary of Total records uploaded (both discon and recon)
    lv_tot_rec = gv_recon + gv_discon.
    DESCRIBE TABLE gt_exception LINES lv_excep.
    IF rb_local EQ abap_true.
      IF lv_excep GT 0.
        SUBTRACT 1 FROM lv_excep.  "Exclude Header line titles
      ENDIF.
    ENDIF.
    IF gv_mroexe NE 0.
      lv_excep = lv_excep - gv_mroexe.
    ENDIF.
    MESSAGE i014 WITH gv_mrrec lv_tot_rec lv_excep gv_bdc_error INTO lv_line.
    mc_build_lineitem lv_line ' '.
    "--Total duplicate entries removed
    IF gv_tot_dup NE 0.
      MESSAGE i015 WITH gv_tot_dup INTO lv_line.  "No. of duplicate entries removed/not processed
      mc_build_lineitem lv_line ' '.
    ENDIF.
    "--Total records from file with MRO
    IF gv_wmro NE 0.
      MESSAGE i017 WITH gv_wmro INTO lv_line.
      mc_build_lineitem lv_line ' '.
      MESSAGE i018 WITH gv_mroupd INTO lv_line.   "Total no. of records with MRO uploaded to TABLE:
      mc_build_lineitem lv_line ' '.
    ENDIF.
  ENDIF.
  "--Summary of files created
  IF gv_filepath IS NOT INITIAL.  "If exception records exists
    IF lv_tot_rec NE 0 OR gv_mroproc NE 0.   "Total records uploaded
      mc_build_lineitem text-020 gv_filepath. "Exception file generated:
    ELSE.
      mc_build_lineitem text-013 gv_filepath. "No data uploaded see exceptions at:
    ENDIF.
  ENDIF.
  IF gv_file_error IS NOT INITIAL.  "Path for BDC error file
    mc_build_lineitem text-037 gv_file_error.
  ENDIF.
ENDFORM.                    " build_upload_summary
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST
*&---------------------------------------------------------------------*
*       Display Top of list
*----------------------------------------------------------------------*
FORM top_of_list.                                           "#EC CALLED
  FIELD-SYMBOLS: <fv_line> TYPE string.
  LOOP AT gt_stringtab ASSIGNING <fv_line>.
    WRITE:/ <fv_line>.
  ENDLOOP.
ENDFORM.                   " top_of_list     "#EC CALLED
*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of Page comment
*----------------------------------------------------------------------*
*       --> po_top  Write header
*----------------------------------------------------------------------*
FORM html_top_of_page USING po_top TYPE REF TO cl_dd_document.
  DATA: lv_line    TYPE char255.
  FIELD-SYMBOLS: <fv_line> TYPE string.
  LOOP AT gt_stringtab ASSIGNING <fv_line>.
    IF sy-tabix NE 1.
      po_top->new_line( ).
    ENDIF.
    lv_line = <fv_line>.
    po_top->add_text( text = lv_line ).
  ENDLOOP.
ENDFORM.                    " html_top_of_page
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       Show GUI progress indicator
*----------------------------------------------------------------------*
*      -->pv_per     Percent of progress
*      -->pv_text    Progress Information
*----------------------------------------------------------------------*
FORM show_progress  USING value(pv_per)
                               pv_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pv_per
      text       = pv_text.
ENDFORM.                    " show_progress
*&---------------------------------------------------------------------*
*&      Form  SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*       Send Email Notification of generated exception to recipients
*----------------------------------------------------------------------*
*      -->PT_BA_COUNT     BA Count summary
*      -->PV_FILEPATH     Filepath
*      -->PV_ATTACHEDDOC  Attached Document
*----------------------------------------------------------------------*
FORM send_notification  TABLES pt_ba_count     TYPE ztisu_gsber_count
                        USING  pv_filepath     TYPE string
                               pv_attacheddoc  TYPE string.

  DATA: lt_emailbody    TYPE soli_tab,
        lt_result_tab   TYPE match_result_tab,
        lv_subject      TYPE so_obj_des,
        lv_cdate        TYPE char10,
        lv_date_txt     TYPE char20,
        lv_cnumber      TYPE char10,
        lv_message      TYPE string,
        lv_filelength   TYPE i,
        lv_filepath     TYPE string,
        lv_filename     TYPE string.

  FIELD-SYMBOLS: <fs_ebody>  TYPE soli,
                 <fs_restab> TYPE match_result,
                 <fs_ba_cnt> TYPE zstisu_gsber_count.

  DEFINE mc_compose_msg.
    append initial line to lt_emailbody assigning <fs_ebody>.
    <fs_ebody> = &1.
  END-OF-DEFINITION.

  "--Separate filepath and filename
  FIND ALL OCCURRENCES OF '/' IN pv_filepath RESULTS lt_result_tab.
  IF lt_result_tab[] IS INITIAL.
    FIND ALL OCCURRENCES OF '\' IN pv_filepath RESULTS lt_result_tab.
  ENDIF.
  lv_filelength = strlen( pv_filepath ).
  SORT lt_result_tab BY offset DESCENDING.
  READ TABLE lt_result_tab ASSIGNING <fs_restab> INDEX 1.
  IF sy-subrc EQ 0.
    ADD 1 TO <fs_restab>-offset.
    lv_filepath = pv_filepath+0(<fs_restab>-offset).
    SUBTRACT <fs_restab>-offset FROM lv_filelength.
    lv_filename = pv_filepath+<fs_restab>-offset(lv_filelength).
    CLEAR lv_filelength.
    "--Check if extension name is in text format, then remove extension
    FIND REGEX '.txt|.TXT' IN lv_filename MATCH OFFSET lv_filelength.
    IF lv_filelength GT 0.
      lv_filename = lv_filename+0(lv_filelength).
    ENDIF.
  ENDIF.
  "--Convert Date to Text Date
  CALL FUNCTION 'CONVERSION_EXIT_LDATE_OUTPUT'
    EXPORTING
      input  = gv_datefr
    IMPORTING
      output = lv_date_txt.
  "--Convert Date to it's char. equivalent
  WRITE gv_datefr TO lv_cdate.
  "--Create Email subject
  CONCATENATE text-e00 lv_date_txt INTO lv_subject SEPARATED BY space.
  "--Compose Email Message
  CONCATENATE text-e02 lv_cdate text-e03 INTO lv_message SEPARATED BY space.
  mc_compose_msg: text-e01, ' ', lv_message, lv_filepath, ' ', text-e04.
  CLEAR lv_message.
  "--Loop to transfer BA Summary to email body
  IF pt_ba_count[] IS NOT INITIAL.
    mc_compose_msg: text-e10, sy-uline(24).
    LOOP AT pt_ba_count ASSIGNING <fs_ba_cnt>.
      WRITE <fs_ba_cnt>-zzbacount TO lv_cnumber.
      CONCATENATE <fs_ba_cnt>-gsber lv_cnumber INTO lv_message RESPECTING BLANKS.
      mc_compose_msg lv_message.
      CLEAR lv_message.
    ENDLOOP.
  ENDIF.
  "--Remaining Email Message
  mc_compose_msg: ' ', text-e05, text-e06, ' ', text-e07, text-e08, ' ', text-e09.
  "--Send Email Notification
  CALL FUNCTION 'ZFM_ISU_EMAIL_NOTIF'
    EXPORTING
      iv_sender         = zcl_const_isu=>co_sender
      iv_recipient_dl   = zcl_const_isu=>co_discon_rcvr_dl
      iv_subject        = lv_subject
      iv_file           = lv_filename
      iv_attachment     = pv_attacheddoc
    TABLES
      rt_emailbody      = lt_emailbody
    EXCEPTIONS
      no_attachment_xls = 1
      no_attachment_txt = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*&      Form  AUTOPOST_REOPENING_FEE
*&---------------------------------------------------------------------*
*       Automatic Posting of Reopening Fee for RECON sent by OCF
*----------------------------------------------------------------------*
*       --> ps_mr         Validated discon/recon Data
*       --> pv_lpermrdate   Latest Periodic Meter Reading Date
*       <-- pv_rof_msgs     ROF message
*----------------------------------------------------------------------*
FORM autopost_reopening_fee USING ps_mr           TYPE ty_mrdata
                                  pv_lpermrdate   TYPE adatsoll  "Inserted by Ann 10/19/2012
                            CHANGING pv_rof_msgs  TYPE char50.
  "--Inserted by Ann 10/11/2012
  TYPES: BEGIN OF ty_payment_s,
           opbel TYPE opbel_kk,  "Inserted by Ann 01/08/2013
           gpart TYPE gpart_kk,
           vtref TYPE vtref_kk,
           vkont TYPE vkont_kk,
           hvorg TYPE hvorg_kk,
           budat TYPE budat,     "Inserted by Ann 10/19/2012
         END OF ty_payment_s.
  "/-Inserted by Ann 10/11/2012
  DATA: lt_prtnrpos   TYPE re_t_xa_bapidfkkop,
        ls_payment    TYPE ty_payment_s,  "Inserted by Ann 10/11/2012
        ls_dochdr     TYPE bapidfkkko,
        ls_return     TYPE bapiret2,
        lv_utilcont   TYPE char20,        "Inserted by Ann 10/11/2012
        lv_docno      TYPE opbel_kk,
        lv_reconkey   TYPE fikey_kk,
        lv_revdoc     TYPE storb_kk.      "Inserted by Ann 01/08/2013
  FIELD-SYMBOLS: <fs_prtpos> TYPE bapidfkkop.
  CLEAR pv_rof_msgs.
  "--Inserted by Ann 10/11/2012
  "--Check if payment with ROF
  lv_utilcont = ps_mr-contract.
  SHIFT lv_utilcont RIGHT DELETING TRAILING space.
  OVERLAY lv_utilcont WITH '00000000000000000000'.
  SELECT SINGLE opbel gpart vtref vkont hvorg budat INTO ls_payment
    FROM dfkkop
    WHERE gpart EQ ps_mr-buspart
      AND vtref EQ lv_utilcont
      AND vkont EQ ps_mr-cacct
      AND hvorg EQ co_maintrans     "AR Recon fee - 7003
      AND budat GE gv_lpermrdate.   "Inserted by Ann 10/19/2012
  IF ls_payment-hvorg EQ co_maintrans.
    "--Inserted by Ann 01/08/2013
    CLEAR lv_revdoc.
    "--Check if payment already reversed
    SELECT SINGLE storb INTO lv_revdoc
      FROM dfkkko
      WHERE opbel EQ ls_payment-opbel.
    IF lv_revdoc IS INITIAL.  "Payment Not reversed
      WRITE ls_payment-budat TO pv_rof_msgs.                                "Inserted by Ann 10/19/2012
      CONCATENATE text-041 pv_rof_msgs INTO pv_rof_msgs SEPARATED BY space. "Inserted by Ann 10/19/2012
      RETURN.
    ENDIF.
    "/-Inserted by Ann 01/08/2013
  ENDIF.
  "/-Inserted by Ann 10/11/2012
  "--Populate recon key
  "CONCATENATE ps_mr-busarea+0(2) sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) 'PS11'  INTO lv_reconkey. "Comment out by Ann 20121218
  CONCATENATE ps_mr-busarea+0(2) sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) 'PS17'  INTO lv_reconkey.  "Changed by Ann 20121218
  "--Check if Recon Key exists
  CALL FUNCTION 'BAPI_CTRACRECKEY_EXISTCHECK'
    EXPORTING
      reconciliationkey = lv_reconkey
    IMPORTING
      return            = ls_return.
  IF ls_return IS NOT INITIAL.
    "--If error found, recon key does not exist then create
    CLEAR ls_return.
    CALL FUNCTION 'BAPI_CTRACRECKEY_CREATE'
      EXPORTING
        newreconciliationkey = lv_reconkey
      IMPORTING
        reconciliationkey    = ls_dochdr-fikey
        return               = ls_return.
    IF ls_dochdr-fikey IS NOT INITIAL AND ( ls_return-type EQ 'S' OR ls_return-type = space ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      pv_rof_msgs = ls_return-message.
      EXIT.
    ENDIF.
  ELSE.
    ls_dochdr-fikey = lv_reconkey.   "Reconciliation Key
  ENDIF.
  "--Populate account document header
  ls_dochdr-appl_area      = 'R'.           "Utility Company
  "ls_dochdr-doc_type       = 'AB'.         "Document Postings  "Comment out by Ann 20121218
  ls_dochdr-doc_type       = 'MB'.          "Manual Billing     "Inserted by Ann 20121218
  ls_dochdr-doc_source_key = '01'.          "Manual Posting
  ls_dochdr-created_by     = sy-uname.
  ls_dochdr-entry_date     = sy-datum.
  ls_dochdr-entry_time     = sy-uzeit.
  "ls_dochdr-doc_date       = ls_dochdr-post_date = ps_mr-mrdatf. "Comment out by GDIMALIWAT 20130910
  ls_dochdr-doc_date       = ps_mr-mrdatf.  "Insert by GDIMALIWAT 20130910 - Recon Date
  ls_dochdr-post_date      = sy-datum.      "Insert by GDIMALIWAT 20130910
  ls_dochdr-currency       = 'PHP'.
  ls_dochdr-ref_doc_no     = 'ROF SET-UP'.   "Reference Key
  "--Populate data for Partner Positions
  APPEND INITIAL LINE TO lt_prtnrpos ASSIGNING <fs_prtpos>.
  <fs_prtpos>-item       = 1.
  "<fs_prtpos>-clear_reas = '11'.         "Clearing Reason                         "Comment out by Ann 20121218
  <fs_prtpos>-clear_reas = '12'.          "Clearing Reason - Debit Entry Creation  "Inserted by Ann 20121218
  <fs_prtpos>-buspartner = ps_mr-buspart. "Business Partner
  "--Get Company code of Contract Account
  SELECT SINGLE opbuk INTO <fs_prtpos>-comp_code
    FROM fkkvkp
    WHERE vkont EQ ps_mr-cacct
      AND gpart EQ ps_mr-buspart.
  "<fs_prtpos>-net_date   = ls_dochdr-post_date + 7. "Due Date for Net Payment "Comment out by Ann 20130211
  "<fs_prtpos>-net_date   = ls_dochdr-post_date.      "Due Date for Net Payment "Inserted by Ann 20130211 "Comment out by GDIMALIWAT 20130910
  <fs_prtpos>-net_date   = ps_mr-mrdatf.            "Insert by GDIMALIWAT 20130910  - Recon Date
  <fs_prtpos>-cont_acct  = ps_mr-cacct.              "Contract Account Number
  <fs_prtpos>-main_trans = co_maintrans.             "Main transaction for line item
  <fs_prtpos>-sub_trans  = co_subtrans.              "Subtransaction for document item
  <fs_prtpos>-amount     = co_amttrans.              "Amount in Transaction Currency with +/- Sign (BETRW_KK)
  <fs_prtpos>-contract   = ps_mr-contract.           "Reference Utility Contract
  <fs_prtpos>-appl_area  = 'R'.
  <fs_prtpos>-doc_date   = ls_dochdr-doc_date.
  <fs_prtpos>-post_date  = ls_dochdr-post_date.
  "--Post document if line item was populated
  CALL FUNCTION 'BAPI_CTRACDOCUMENT_CREATE'
    EXPORTING
      testrun          = abap_false
      documentheader   = ls_dochdr
      completedocument = abap_true
    IMPORTING
      documentnumber   = lv_docno
      return           = ls_return
    TABLES
      partnerpositions = lt_prtnrpos.
  "--Commit posting of records if no error found
  IF ls_return-type EQ 'S' OR ls_return-type EQ space.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
    IF ls_return-message IS INITIAL.
      MESSAGE s043 INTO ls_return-message WITH lv_docno. "ROF posted under document number xxxxx
    ENDIF.
  ENDIF.
  pv_rof_msgs = ls_return-message.
  CLEAR: ls_dochdr, ls_return, lv_docno.
  REFRESH lt_prtnrpos.
ENDFORM.                    " AUTOPOST_REOPENING_FEE

*&---------------------------------------------------------------------*
*&      Form  RECHECK_RELEASE_STATUS
*&---------------------------------------------------------------------*
*       Re-check release status if it is equal to 2. If yes, produce an exception
*----------------------------------------------------------------------*
*      --> pv_rof_msgs   ROF-related Messages
*      --> ps_mr         Discon/Recon line entry
*----------------------------------------------------------------------*
FORM RECHECK_RELEASE_STATUS  USING pv_rof_msgs TYPE char50
                                   ps_mr       TYPE ty_mrdata.
DATA: lv_mrreason TYPE ablesgr.
  "--Set Meter Reading Reason
  IF ps_mr-stat_code EQ co_discon.
    lv_mrreason = '13'.  "Meter Reading on Disconnection
  ELSE.
    lv_mrreason = '18'.  "Reconnection Meter Reading
  ENDIF.
  SELECT ablbelnr adatsoll ablesgr ablstat
    INTO TABLE gt_mrdoc
    FROM v_eabl
    WHERE anlage  EQ ps_mr-install
      AND ablesgr EQ lv_mrreason
      AND ablstat EQ '2'.        "Automatically Locked
  IF sy-subrc EQ 0.
    mc_exception ' ' <fs_mr> gv_frmro text-043 ' '. "Account successfully reconnected with status 2 (Automatically Locked)
  ENDIF.
ENDFORM.                    " RECHECK_RELEASE_STATUS