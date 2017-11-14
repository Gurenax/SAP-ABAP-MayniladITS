*&---------------------------------------------------------------------*
*& Report  ZMRUCREATE
*& DESCRIPTION: Massive Creation of MRU
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 09/10/2013
*& Reference  : Based on the original ZMRUCREATE by RANDYSY 12/15/2011
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/09/10|GDIMALIWAT|43620      | Re-created for Panaya Task ID 2
*&---------------------------------------------------------------------*
REPORT zmrucreate MESSAGE-ID zisu.

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

       BEGIN OF ty_data,
         schluess2 TYPE rea41-schluess2,
         v_abl     TYPE rea41-v_abl,
         termtext  TYPE te422-termtext,
         portion   TYPE te422-portion,
         stichtag  TYPE te422-stichtag,
         ident     TYPE te422-ident,
         sapkal    TYPE te422-sapkal,
         ableser   TYPE te422-ableser,
       END OF ty_data.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_bdcdata  TYPE tab_bdcdata,
      gt_message  TYPE STANDARD TABLE OF ty_message.

DATA: gt_upload   TYPE TABLE OF alsmex_tabline,
      gt_data     TYPE TABLE OF ty_data.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gs_option TYPE ctu_params,
      gv_msge   TYPE bdc_vtext1,
      gv_index  TYPE sy-tabix,
      gv_icon   TYPE icon_d.

DATA: gv_file_error TYPE c.

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
FIELD-SYMBOLS: <fs_data>  TYPE ty_data,
               <fs_bdc>   TYPE bdcdata,
               <fs_mess>  TYPE ty_message,
               <fs_msgs>  TYPE bdcmsgcoll,
               <fs_upload> TYPE alsmex_tabline.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file         LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM select_file USING pa_file.

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
  PERFORM upload_excel.
  IF gv_file_error NE '1'.
    PERFORM load_bdc.
  ELSE.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        textline1 = text-001.
  ENDIF.
*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE
*&---------------------------------------------------------------------*
*       Select excel file
*----------------------------------------------------------------------*
*      -->PV_INFILE  text
*----------------------------------------------------------------------*
FORM select_file USING pv_infile TYPE localfile.
  DATA :
    lv_subrc  TYPE sy-subrc,
    lt_it_tab TYPE filetable.

  " Display File Open Dialog control/screen
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select Source Excel File' "#EC NOTEXT
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = lt_it_tab
      rc               = lv_subrc.

  " Write path on input area
  LOOP AT lt_it_tab INTO pv_infile.
  ENDLOOP.

ENDFORM.                    " SELECT_FILE

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*       Upload excel file
*----------------------------------------------------------------------*
FORM upload_excel .
  gv_file_error = ``.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pa_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 14
      i_end_row               = 5000
    TABLES
      intern                  = gt_upload
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc NE 0.
*      WRITE: / 'File Error.'.

    gv_file_error = '1'.

*      MESSAGE i000.
*      EXIT.
  ENDIF.

  IF gt_upload IS INITIAL.
*      WRITE: / 'No data uploaded.'.

    gv_file_error = '1'.
  ELSE.
    SORT gt_upload BY row col.

    LOOP AT gt_upload ASSIGNING <fs_upload>.
      CASE <fs_upload>-col.
        WHEN 1.
          APPEND INITIAL LINE TO gt_data ASSIGNING <fs_data>.
          MOVE <fs_upload>-value TO <fs_data>-schluess2.
        WHEN 2.
          MOVE <fs_upload>-value TO <fs_data>-v_abl.
        WHEN 3.
          MOVE <fs_upload>-value TO <fs_data>-termtext.
        WHEN 4.
          MOVE <fs_upload>-value TO <fs_data>-portion.
        WHEN 5.
          MOVE <fs_upload>-value TO <fs_data>-stichtag.
        WHEN 6.
          MOVE <fs_upload>-value TO <fs_data>-ident.
        WHEN 7.
          MOVE <fs_upload>-value TO <fs_data>-sapkal.
        WHEN 8.
          MOVE <fs_upload>-value TO <fs_data>-ableser.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPLOAD_EXCEL

*&---------------------------------------------------------------------*
*&      Form  LOAD_BDC
*&---------------------------------------------------------------------*
*       Execute BDC
*----------------------------------------------------------------------*
FORM load_bdc .
  LOOP AT gt_data ASSIGNING <fs_data>.

    m_bdc_dynpro: 'X' 'SAPLE15A'        '0104',
                  ' ' 'BDC_CURSOR'      'REA41-V_ABL',
                  ' ' 'BDC_OKCODE'      '/00',
                  ' ' 'REA41-SCHLUESS2' <fs_data>-schluess2,
                  ' ' 'REA41-V_ABL'     <fs_data>-v_abl,
                  'X' 'SAPLE15A'        '0201',
                  ' ' 'BDC_CURSOR'      'TE422-TERMTEXT',
                  ' ' 'BDC_OKCODE'      '=SAVE',
                  ' ' 'TE422-TERMTEXT'  <fs_data>-termtext,
                  ' ' 'TE422-PORTION'   <fs_data>-portion,
                  ' ' 'TE422-STICHTAG'  <fs_data>-stichtag,
                  ' ' 'TE422-IDENT'     <fs_data>-ident,
                  ' ' 'TE422-SAPKAL'    <fs_data>-sapkal,
                  ' ' 'TE422-ABLESER'   <fs_data>-ableser.

    PERFORM bdc_transaction USING 'E41H'.

  ENDLOOP.
ENDFORM.                    " LOAD_BDC


*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
FORM bdc_transaction USING value(pv_tcode).

  DATA: lt_bdcmsgs  TYPE tab_bdcmsgcoll,
        lv_log      TYPE char1.
  "--Call Transaction
  CLEAR: gv_msge, gs_option-racommit, lt_bdcmsgs, lv_log.
*  IF pv_tcode EQ 'EA21' OR pv_tcode EQ 'ZMWS_EA24'.
*    gs_option-racommit = abap_true.
*  ENDIF.
  CALL TRANSACTION pv_tcode USING gt_bdcdata OPTIONS FROM gs_option
                                             MESSAGES INTO lt_bdcmsgs.
  IF sy-subrc EQ 0.
    gv_icon = co_success.
    LOOP AT lt_bdcmsgs ASSIGNING <fs_msgs>.
      PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr
                                   <fs_msgs>-msgtyp <fs_msgs>-msgv1
                                   <fs_msgs>-msgv2 <fs_msgs>-msgv3
                                   <fs_msgs>-msgv4
                             CHANGING gv_msge gv_icon.
      IF gv_msge IS NOT INITIAL.
        WRITE:/6 gv_icon, gv_msge.
      ENDIF.
    ENDLOOP.
  ELSE.
    gv_icon = co_error.
    LOOP AT lt_bdcmsgs ASSIGNING <fs_msgs>.
      PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr
                                   <fs_msgs>-msgtyp <fs_msgs>-msgv1
                                   <fs_msgs>-msgv2 <fs_msgs>-msgv3
                                   <fs_msgs>-msgv4
                             CHANGING gv_msge gv_icon.
      IF gv_msge IS NOT INITIAL.
        WRITE:/6 gv_icon, gv_msge.
      ENDIF.
    ENDLOOP.
  ENDIF.
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