*&---------------------------------------------------------------------*
*& Report  ZRMM_MASSIVE_ME32K
*& DESCRIPTION: Massive Change Contract
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 2014/02/03
*& Reference  : 55604
*&---------------------------------------------------------------------*
*& Date       | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/02/03 | GDIMALIWAT | 55604      | Created
*&---------------------------------------------------------------------*
REPORT zrmm_massive_me32k MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_message,
         recno    TYPE syindex,
         tcode    TYPE sytcode,
         msgtyp   TYPE icon_d,
         msge     TYPE bdc_vtext1,
       END OF ty_message,

       BEGIN OF ty_upload,
         evrtn    TYPE c LENGTH 10,
         ebelp    TYPE c LENGTH 5,
         ktmng    TYPE c LENGTH 16,
       END OF ty_upload.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_bdcdata    TYPE tab_bdcdata,                       "#EC NEEDED
      gt_message    TYPE TABLE OF ty_message,               "#EC NEEDED
      gt_upload     TYPE TABLE OF ty_upload.                "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gs_option TYPE ctu_params,
      gv_msge   TYPE bdc_vtext1,
      gv_index  TYPE sy-tabix,
      gv_icon   TYPE icon_d.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS: co_add         TYPE char1      VALUE 'A',
           co_rem         TYPE char1      VALUE 'R',
           co_updmode     TYPE ctu_update VALUE 'L',
           co_dismode     TYPE ctu_mode   VALUE 'N',
           co_success     TYPE icon_d     VALUE '@08@',
           co_warning     TYPE icon_d     VALUE '@09@',
           co_error       TYPE icon_d     VALUE '@0A@',
           co_sub_err     TYPE balsubobj  VALUE 'ERROR',
           co_separator   TYPE c          VALUE cl_abap_char_utilities=>horizontal_tab.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS: <fs_bdc>    TYPE bdcdata,
               <fs_mess>   TYPE ty_message,
               <fs_msgs>   TYPE bdcmsgcoll,
               <fs_upload> TYPE ty_upload.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file     TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM get_file.

*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  "--Macro definition for message creation
  DEFINE m_message.
    append initial line to gt_message assigning <fs_mess>.
    <fs_mess>-recno  = &1.
    <fs_mess>-tcode  = &2.
    <fs_mess>-msgtyp = &3.
    <fs_mess>-msge   = &4.
    <fs_mess>-vkont  = &5.
  END-OF-DEFINITION.
  "--Macro definition for BDC creation
  DEFINE m_bdc_dynpro.
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

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  PERFORM read_data.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.
  PERFORM process_data.

*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       Open file dialog box
*----------------------------------------------------------------------*
FORM get_file .
  DATA: file_table TYPE filetable,
        rc         TYPE i,
        lv_title   TYPE string.

  lv_title = text-002.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      default_extension = 'txt'
*     DEFAULT_FILENAME  =
      file_filter       = '*.xls*|*.XLS|*.xlsx|*.XLSX'
*     WITH_ENCODING     =
*     INITIAL_DIRECTORY =
*     MULTISELECTION    =
    CHANGING
      file_table        = file_table
      rc                = rc.

  IF sy-subrc EQ 0.
    READ TABLE file_table INTO pa_file INDEX 1.
  ENDIF.
ENDFORM.                    " GET_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read data from input file
*----------------------------------------------------------------------*
FORM read_data .

*  DATA: lv_file         TYPE string.
  DATA: lt_raw          TYPE truxs_t_text_data.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = pa_file
*    IMPORTING
*      output = lv_file.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                = lv_file
*    TABLES
*      data_tab                = gt_file
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      OTHERS                  = 17.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = lt_raw
      i_filename           = pa_file
    TABLES
      i_tab_converted_data = gt_upload
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc NE 0.
    IF sy-subrc EQ 1.
      MESSAGE text-002 TYPE 'E'.
    ELSE.
      MESSAGE text-003 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Execute the BDC for ME32K
*----------------------------------------------------------------------*
FORM process_data .

  DATA: lv_evrtn        TYPE string,
        lv_ebelp        TYPE i,
        lv_ebelp2       TYPE str,
        lv_ktmng        TYPE ekpo-ktmng,
        lv_ktmng_field  TYPE string,
        "lv_ekpo_ktmng   TYPE ekpo-ktmng,
        lv_ekab_menge   TYPE ekab-menge,
        lv_ekab_konnr   TYPE ekab-konnr,
        lv_ekab_ktpnr   TYPE ekab-ktpnr,
        lv_ekpo_ebeln   TYPE ekpo-ebeln,
        lv_ok           TYPE c LENGTH 1 VALUE ''.


  CONSTANTS: co_ktmng_fld1 TYPE c LENGTH 11 VALUE 'EKPO-KTMNG(',
             co_ktmng_fld2 TYPE c LENGTH 1  VALUE ')',
             co_zero       TYPE c LENGTH 1 VALUE '0'.

  "--Set Call Transaction Options
  gs_option-dismode = co_dismode.
  gs_option-updmode = co_updmode.
  gs_option-nobinpt = abap_true.

  LOOP AT gt_upload ASSIGNING <fs_upload>.
    CLEAR: lv_evrtn,
           lv_ebelp,
           lv_ktmng,
           lv_ktmng_field,
           "lv_ekpo_ktmng,
           lv_ekab_menge,
           lv_ekab_konnr,
           lv_ekab_ktpnr,
           lv_ekpo_ebeln,
           lv_ok.

    lv_ebelp = <fs_upload>-ebelp.
    lv_ebelp = ( lv_ebelp / 10 ).
    lv_ebelp2 = lv_ebelp.
    CONDENSE lv_ebelp2.

    IF lv_ebelp2 lt 10.
      CONCATENATE co_zero lv_ebelp2 INTO lv_ebelp2.
    ENDIF.

    CONCATENATE co_ktmng_fld1 lv_ebelp2 co_ktmng_fld2 INTO lv_ktmng_field.

    lv_evrtn = <fs_upload>-evrtn.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = <fs_upload>-evrtn
      IMPORTING
        OUTPUT        = lv_ekab_konnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = <fs_upload>-ebelp
      IMPORTING
        OUTPUT        = lv_ekab_ktpnr.

*    SELECT SINGLE ktmng
*    INTO lv_ekpo_ktmng
*    FROM ekpo
*    WHERE ebeln eq lv_ekpo_ebeln
*      AND ebelp eq lv_ekpo_ebelp.

    SELECT SUM( menge )
        INTO lv_ekab_menge
        FROM ekab
       WHERE konnr eq lv_ekab_konnr
         AND ktpnr eq lv_ekab_ktpnr.

    IF sy-subrc eq 0.
      IF <fs_upload>-ktmng eq 0.
        gv_icon = co_error.
        WRITE:/6 gv_icon, lv_ekab_konnr, text-010.
      ELSE.
        IF <fs_upload>-ktmng lt lv_ekab_menge.
          gv_icon = co_error.
          WRITE:/6 gv_icon, text-007, lv_ekab_konnr, text-008, <fs_upload>-ktmng, text-009, lv_ekab_menge.
        ELSE.
          lv_ok = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_ok eq 'X'.
      m_bdc_dynpro: 'X' 'SAPMM06E'         '0205',
                    ' ' 'BDC_CURSOR'      'RM06E-EVRTN',
                    ' ' 'BDC_OKCODE'      '/00',
                    ' ' 'RM06E-EVRTN'     <fs_upload>-evrtn,
                    'X' 'SAPMM06E'        '0220',
                    ' ' 'BDC_CURSOR'      lv_ktmng_field,
                    ' ' 'BDC_OKCODE'      '=BU',
                    ' ' lv_ktmng_field    <fs_upload>-ktmng.
      PERFORM bdc_transaction USING 'ME32K' lv_evrtn.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
*      -->PV_TCODE  Transaction Code
*      -->PV_EVRTN  Agreement Number
*----------------------------------------------------------------------*
FORM bdc_transaction USING value(pv_tcode)
                                 pv_evrtn  TYPE string.

  DATA: lt_bdcmsgs  TYPE tab_bdcmsgcoll,
        lv_log      TYPE char1.
  "--Call Transaction
  CLEAR: gv_msge, gs_option-racommit.
*  IF pv_tcode EQ 'EA21' OR pv_tcode EQ 'ZMWS_EA24'.
*    gs_option-racommit = abap_true.
*  ENDIF.
  CALL TRANSACTION pv_tcode USING gt_bdcdata OPTIONS FROM gs_option
                                             MESSAGES INTO lt_bdcmsgs.
*  IF sy-subrc EQ 0.
*    gv_icon = co_success.
*    WRITE:/ gv_icon, pv_evrtn, text-004.
*  ELSE.
*    gv_icon = co_error.
*    WRITE:/ gv_icon , pv_evrtn, text-005.
*    LOOP AT lt_bdcmsgs ASSIGNING <fs_msgs>.
*      PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr
*                                   <fs_msgs>-msgtyp <fs_msgs>-msgv1
*                                   <fs_msgs>-msgv2 <fs_msgs>-msgv3
*                                   <fs_msgs>-msgv4
*                             CHANGING gv_msge gv_icon.
*      IF gv_msge IS NOT INITIAL.
*        WRITE:/6 gv_icon, gv_msge.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  LOOP AT lt_bdcmsgs ASSIGNING <fs_msgs>.
      PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr
                                   <fs_msgs>-msgtyp <fs_msgs>-msgv1
                                   <fs_msgs>-msgv2 <fs_msgs>-msgv3
                                   <fs_msgs>-msgv4
                             CHANGING gv_msge gv_icon.
      IF gv_msge IS NOT INITIAL.
        IF gv_msge CS text-006.
          CONTINUE.
        ELSE.
          WRITE:/6 gv_icon, pv_evrtn, gv_msge.
        ENDIF.
      ENDIF.
    ENDLOOP.
  REFRESH gt_bdcdata.
ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  FORMAT_MESSAGE
*&---------------------------------------------------------------------*
*       Get/format message in text form
*----------------------------------------------------------------------*
*      -->pv_msgid   Message ID
*      -->pv_msgnr   Message Number
*      -->pv_msgtyp  Message Type
*      -->pv_msgv1   Message Parameter 1
*      -->pv_msgv2   Message Parameter 2
*      -->pv_msgv3   Message Parameter 3
*      -->pv_msgv4   Message Parameter 4
*      <--pv_msgs    Output Message
*      <--pv_type    Message Type (ICON)
*----------------------------------------------------------------------*
FORM format_message  USING  value(pv_msgid)
                            value(pv_msgnr)
                            value(pv_msgtyp)
                            value(pv_msgv1)
                            value(pv_msgv2)
                            value(pv_msgv3)
                            value(pv_msgv4)
                     CHANGING value(pv_msgs)
                              value(pv_type).

  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = pv_msgid
      lang      = sy-langu
      no        = pv_msgnr
      v1        = pv_msgv1
      v2        = pv_msgv2
      v3        = pv_msgv3
      v4        = pv_msgv4
    IMPORTING
      msg       = pv_msgs
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    CASE pv_msgtyp.
      WHEN 'S'.
        pv_type = co_success.
      WHEN 'A' OR 'E' OR 'I'.
        pv_type = co_error.
      WHEN 'W'. pv_type = co_warning.
    ENDCASE.
  ENDIF.
ENDFORM.                    " FORMAT_MESSAGE