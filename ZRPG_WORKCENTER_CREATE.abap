*&---------------------------------------------------------------------*
*& Report  ZRPM_WORKCENTER_CREATE
*& DESCRIPTION: Massive Creation of Work Center
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 2013/01/21
*& Reference  : TN#
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/01/21|GDIMALIWAT|           | Created                          *
*&---------------------------------------------------------------------*
REPORT zrpm_workcenter_create MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_data,
         werks      TYPE string,
         arbpl      TYPE string,
         stext      TYPE string,
         begda      TYPE string,
         endda      TYPE string,
         kostl      TYPE string,
       END OF ty_data,
       BEGIN OF ty_message,
         recno    TYPE syindex,
         tcode    TYPE sytcode,
         msgtyp   TYPE icon_d,
         msge     TYPE bdc_vtext1,
         werks    TYPE string,
       END OF ty_message.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_bdcdata  TYPE tab_bdcdata,
      gt_message  TYPE STANDARD TABLE OF ty_message.

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA: gs_option TYPE ctu_params,
      gv_data   TYPE TABLE OF ty_data,
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
FIELD-SYMBOLS: <fs_data>  TYPE ty_data,
               <fs_bdc>   TYPE bdcdata,
               <fs_mess>  TYPE ty_message,
               <fs_msgs>  TYPE bdcmsgcoll.

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_data     TYPE rlgrap-filename DEFAULT '\\saprep01.mayniladsap.com\zwcvalext\workcenter.txt'.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*AT SELECTION-SCREEN ON <para|selcrit>
*
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

  PERFORM read_data_file.

  LOOP AT gv_data ASSIGNING <fs_data>.
    PERFORM create_workcenter.
  ENDLOOP.


*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FILE
*&---------------------------------------------------------------------*
*       Read the Input File containing the CAN and New Address
*----------------------------------------------------------------------*
FORM read_data_file .
  DATA: lv_string TYPE string.

  OPEN DATASET pa_data FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET pa_data INTO lv_string.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO gv_data ASSIGNING <fs_data>.

        SPLIT lv_string AT co_separator INTO <fs_data>-werks
                                             <fs_data>-arbpl
                                             <fs_data>-stext
                                             <fs_data>-begda
                                             <fs_data>-endda
                                             <fs_data>-kostl.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLOSE DATASET pa_data.
  ELSE.
    WRITE:/ text-001.
  ENDIF.
ENDFORM.                    " READ_DATA_FILE

*&---------------------------------------------------------------------*
*&      Form  CREATE_WORKCENTER
*&---------------------------------------------------------------------*
*       BDC sub-routine for Workcenter Creation
*----------------------------------------------------------------------*
FORM create_workcenter .

  "--Set Call Transaction Options
  gs_option-dismode = co_dismode.
  gs_option-updmode = co_updmode.
  gs_option-nobinpt = abap_true.

  m_bdc_dynpro: 'X' 'SAPLCRA0' '0101',
                ' ' 'BDC_CURSOR' 'RC68A-VERWE',
                ' ' 'BDC_OKCODE' '=NEXT',
                ' ' 'RC68A-WERKS' <fs_data>-werks,
                ' ' 'RC68A-ARBPL' <fs_data>-arbpl,
                ' ' 'RC68A-VERWE' '0003',
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '=KAUE',
                ' ' 'P1000-STEXT' <fs_data>-stext,
                ' ' 'BDC_CURSOR' 'P3000-VGWTS',
                ' ' 'P3000-VERAN' '000',
                ' ' 'P3000-PLANV' '009',
                ' ' 'P3000-VGWTS' 'SAP0',
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '/00',
                ' ' 'BDC_CURSOR' 'RC68A-KAPART(01)',
                ' ' 'RC68A-KAPART(01)' '002',
                'X' 'SAPLCRK0' '0101',
                ' ' 'BDC_OKCODE' '=BACK',
                ' ' 'KAKO-PLANR' 'A',
                ' ' 'BDC_CURSOR' 'KAKO-AZNOR',
                ' ' 'RC68K-BEGZT' '00:00:00',
                ' ' 'RC68K-ENDZT' '00:00:00',
                ' ' 'KAKO-NGRAD' '100',
                ' ' 'RC68K-PAUSE' '00:00:00',
                ' ' 'KAKO-AZNOR' '1',
                ' ' 'KAKO-MEINS' 'HR',
                ' ' 'RC68K-KAPLPL' 'X',
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '=TERM',
                ' ' 'BDC_CURSOR' 'P3006-FORK1(01)',
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '=VK11',
                ' ' 'BDC_CURSOR' 'RC68A-KAPART',
                ' ' 'RC68A-KAPART' '002',
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '=TECH',
                ' ' 'BDC_CURSOR' 'CRKEYK-KOSTL',
                ' ' 'P1001-BEGDA' <fs_data>-begda,
                ' ' 'P1001-ENDDA' <fs_data>-endda,
                ' ' 'CRKEYK-KOSTL' <fs_data>-kostl,
                'X' 'SAPLCRA0' '4000',
                ' ' 'BDC_OKCODE' '=UPD',
                ' ' 'BDC_CURSOR' 'P3007-CPLGR',
                ' ' 'P3007-MATYP' '0001',
                ' ' 'P3007-SORTB' '0001',
                ' ' 'P3007-CPLGR' '000'.
  PERFORM bdc_transaction USING 'IR01' <fs_data>-arbpl.
ENDFORM.                    " CREATE_WORKCENTER
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
*      -->PV_TCODE  Transaction Code
*      -->PV_VKONT  Contract Account Number
*----------------------------------------------------------------------*
FORM bdc_transaction USING value(pv_tcode)
                            pv_arbpl  TYPE string.

  DATA: lt_bdcmsgs  TYPE tab_bdcmsgcoll,
        lv_log      TYPE char1.
  "--Call Transaction
  CLEAR: gv_msge, gs_option-racommit.
*  IF pv_tcode EQ 'EA21' OR pv_tcode EQ 'ZMWS_EA24'.
*    gs_option-racommit = abap_true.
*  ENDIF.
  CALL TRANSACTION pv_tcode USING gt_bdcdata OPTIONS FROM gs_option
                                             MESSAGES INTO lt_bdcmsgs.
  IF sy-subrc EQ 0.
    gv_icon = co_success.
    WRITE:/ gv_icon, pv_arbpl, text-001.
  ELSE.
    gv_icon = co_error.
    WRITE:/ gv_icon , pv_arbpl, text-002.
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