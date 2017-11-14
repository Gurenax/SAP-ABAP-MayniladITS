*&---------------------------------------------------------------------*
*& Report  ZRMM_VATEX_CLEANUP
*& DESCRIPTION: Massive clean-up for the Service VATEX records
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 07/02/2014
*& Reference  : TN#64596
*&---------------------------------------------------------------------*
*& Date      | Author ID  |Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/07/02| GDIMALIWAT | 64596     | Created
*&---------------------------------------------------------------------*
REPORT zrmm_vatex_cleanup.

*INCLUDE
TABLES: essr, ekkn.
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_entrysheet_no,
         entrysheet_no TYPE essr-lblni,
       END OF ty_entrysheet_no,

       BEGIN OF ty_eslh,
         ebeln TYPE eslh-ebeln,
         ebelp TYPE eslh-ebelp,
       END OF ty_eslh,

       BEGIN OF ty_ekkn,
         ebeln TYPE ekkn-ebeln,
         ebelp TYPE ekkn-ebelp,
         aufnr TYPE ekkn-aufnr,
       END OF ty_ekkn.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA:   gt_bdcdata    TYPE tab_bdcdata,                     "#EC NEEDED
        gt_messages   TYPE STANDARD TABLE OF bdcmsgcoll.    "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
DATA:   go_table          TYPE REF TO cl_salv_table,        "#EC NEEDED
        go_functions      TYPE REF TO cl_salv_functions_list, "#EC NEEDED
        go_columns        TYPE REF TO cl_salv_columns_table, "#EC NEEDED
        go_column         TYPE REF TO cl_salv_column_table, "#EC NEEDED
        go_salv_msg       TYPE REF TO cx_salv_msg.          "#EC NEEDED

DATA:   gv_pdoc           TYPE c LENGTH 15.                 "#EC NEEDED

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_flag   TYPE c          VALUE `X`,
            co_green  TYPE icon_d     VALUE '@08@',
            co_red    TYPE icon_d     VALUE '@0A@'.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:
        <fs_bdc>          TYPE bdcdata,                     "#EC NEEDED
        <fs_messages>     TYPE bdcmsgcoll.                  "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk00 WITH FRAME TITLE text-000.
PARAMETERS: pa_rb1  RADIOBUTTON GROUP grp1 USER-COMMAND uc1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-001  MODIF ID b0.
PARAMETERS: pa_rb2  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-002 MODIF ID b0.
PARAMETERS: pa_rb3  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-003 MODIF ID b0.
PARAMETERS: pa_rb4  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-004 MODIF ID b0.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 3(10) text-011 MODIF ID b0.
*SELECTION-SCREEN COMMENT 36(50) text-005 MODIF ID b0.
*SELECTION-SCREEN END OF LINE.
PARAMETERS: pa_rb5  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-005 MODIF ID b0.
PARAMETERS: pa_rb6  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(51) text-006 MODIF ID b0.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(10) text-012 MODIF ID b0.
SELECTION-SCREEN COMMENT 36(50) text-007 MODIF ID b0.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(10) text-013 MODIF ID b0.
SELECTION-SCREEN COMMENT 36(50) text-008 MODIF ID b0.
SELECTION-SCREEN END OF LINE.
PARAMETERS: pa_rb9  RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-009 MODIF ID b0.
PARAMETERS: pa_rb10 RADIOBUTTON GROUP grp1 MODIF ID b0. SELECTION-SCREEN COMMENT 36(50) text-010 MODIF ID b0.
SELECTION-SCREEN END OF BLOCK blk00.

SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_es1 FOR essr-lblni MODIF ID b1.
SELECTION-SCREEN END OF BLOCK blk01.

SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE text-002.
SELECT-OPTIONS: so_es2 FOR essr-lblni MODIF ID b2.
SELECTION-SCREEN END OF BLOCK blk02.

SELECTION-SCREEN BEGIN OF BLOCK blk03 WITH FRAME TITLE text-003.
SELECT-OPTIONS: so_es3 FOR essr-lblni MODIF ID b3.
SELECTION-SCREEN END OF BLOCK blk03.

SELECTION-SCREEN BEGIN OF BLOCK blk04 WITH FRAME TITLE text-004.
SELECT-OPTIONS: so_pdoc1 FOR (gv_pdoc) NO INTERVALS MODIF ID b4.
SELECTION-SCREEN END OF BLOCK blk04.

SELECTION-SCREEN BEGIN OF BLOCK blk05 WITH FRAME TITLE text-005.
SELECT-OPTIONS: so_pdoc2 FOR (gv_pdoc) NO INTERVALS MODIF ID b5.
SELECTION-SCREEN END OF BLOCK blk05.

SELECTION-SCREEN BEGIN OF BLOCK blk06 WITH FRAME TITLE text-006.
SELECT-OPTIONS: so_mo1 FOR ekkn-aufnr MODIF ID b6.
SELECTION-SCREEN END OF BLOCK blk06.

SELECTION-SCREEN BEGIN OF BLOCK blk09 WITH FRAME TITLE text-009.
SELECT-OPTIONS: so_pdoc3 FOR (gv_pdoc) NO INTERVALS MODIF ID b9.
PARAMETERS: pa_stxt TYPE txz01_essr MODIF ID b9.
SELECTION-SCREEN END OF BLOCK blk09.

SELECTION-SCREEN BEGIN OF BLOCK blk10 WITH FRAME TITLE text-010.
SELECT-OPTIONS: so_es4 FOR essr-lblni MODIF ID b10.
SELECTION-SCREEN END OF BLOCK blk10.
*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
*AT SELECTION-SCREEN ON <para|selcrit>
*
*AT SELECTION-SCREEN.
*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF pa_rb1 EQ 'X'.
      IF screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb2 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb3 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb4 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb5 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb6 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb9 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pa_rb10 EQ 'X'.
      IF screen-group1 EQ 'B1' OR screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'B2' OR screen-group1 EQ 'B3' OR screen-group1 EQ 'B4' OR screen-group1 EQ 'B5' OR screen-group1 EQ 'B6' OR screen-group1 EQ 'B9' OR screen-group1 EQ 'B10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Initialization
*&
INITIALIZATION.
  "--Macro definition for BDC creation
  DEFINE mc_bdc_dynpro.
    append initial line to gt_bdcdata assigning <fs_bdc>.
    if &1 ne space.
      <fs_bdc>-program  = &2.
      <fs_bdc>-dynpro   = &3.
      <fs_bdc>-dynbegin = &1.
    else.
      <fs_bdc>-fnam     = &2.
      <fs_bdc>-fval     = &3.
    endif.
  END-OF-DEFINITION.

  "-- Macro Definition - Format Asset Number - Add Zero Paddings
  DEFINE mc_format_number.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = &1
      importing
        output = &1.
  END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.
  CASE 'X'.
    WHEN pa_rb1.
      PERFORM step01.
    WHEN pa_rb2.
      PERFORM step02.
    WHEN pa_rb3.
      PERFORM step03.
    WHEN pa_rb4.
      PERFORM step04.
    WHEN pa_rb5.
      PERFORM step05.
    WHEN pa_rb6.
      PERFORM step06.
    WHEN pa_rb9.
      PERFORM step09.
    WHEN pa_rb10.
      PERFORM step10.
  ENDCASE.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
FORM call_transaction  USING    pv_tcode   TYPE sy-tcode
                       CHANGING pv_message TYPE natxt
                                pv_msgtyp  TYPE char1.

  CONSTANTS:  co_mode TYPE char01 VALUE 'N'.

  "-- Clear Error Message.
  CLEAR: pv_message.

  "-- Reinitialize Itab for Messages
  REFRESH: gt_messages.

  "-- Perform call transaction
  CALL TRANSACTION pv_tcode USING         gt_bdcdata
                            MESSAGES INTO gt_messages
                            MODE          co_mode.       "#EC CI_CALLTA

*  IF sy-subrc EQ 0 OR sy-subrc EQ 1001.
*    pv_message = 0.
*  ELSE.
  "-- Check if there are BDC Messages
  IF gt_messages[] IS NOT INITIAL.
    "-- Get Call Transaction Results
    PERFORM get_bdc_messages TABLES   gt_messages
                             CHANGING pv_message
                                      pv_msgtyp.
  ENDIF.
  "ENDIF.

  "-- Reinitialize Itab for BDC
  REFRESH: gt_bdcdata.

ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  GET_BDC_MESSAGES
*&---------------------------------------------------------------------*
*       Get BDC messages
*----------------------------------------------------------------------*
FORM get_bdc_messages  TABLES   pt_bdcmsgs TYPE tab_bdcmsgcoll
                       CHANGING pv_text    TYPE natxt
                                pv_msgtyp  TYPE char1.

  "-- Get First Row of BDC Message
  READ TABLE pt_bdcmsgs ASSIGNING <fs_messages> INDEX 1.
  IF sy-subrc EQ 0.
    "-- Call FM to Format BDC Message
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = <fs_messages>-msgid
        lang      = sy-langu
        no        = <fs_messages>-msgnr
        v1        = <fs_messages>-msgv1
        v2        = <fs_messages>-msgv2
        v3        = <fs_messages>-msgv3
        v4        = <fs_messages>-msgv4
      IMPORTING
        msg       = pv_text
      EXCEPTIONS
        not_found = 0
        OTHERS    = 0.

    pv_msgtyp = <fs_messages>-msgtyp.
  ENDIF.
ENDFORM.                    " get_bdc_messages
*&---------------------------------------------------------------------*
*&      Form  STEP01
*&---------------------------------------------------------------------*
*       Revoke Acceptance per Service Entry Sheet
*----------------------------------------------------------------------*
FORM step01 .

  DATA: lv_message TYPE natxt,
        lv_msgtyp  TYPE char1.

  DATA: lt_entrysheet_no TYPE TABLE OF ty_entrysheet_no.

  FIELD-SYMBOLS: <fs_entrysheet_no> TYPE ty_entrysheet_no.

  IF so_es1 IS NOT INITIAL.
    SELECT lblni
      INTO TABLE lt_entrysheet_no
      FROM essr
      WHERE lblni IN so_es1.

    IF sy-subrc EQ 0.
      LOOP AT lt_entrysheet_no ASSIGNING <fs_entrysheet_no>.
        mc_bdc_dynpro:  co_flag   'SAPLMLSR'        '0400',
                        space     'BDC_OKCODE'      '=SELP',
                        space     'BDC_CURSOR'      'RM11P-NEW_ROW',
                        space     'RM11P-NEW_ROW'   '10',
                        co_flag   'SAPLMLSR'        '0340',
                        space     'BDC_CURSOR'      'RM11R-LBLNI',
                        space     'BDC_OKCODE'      '=ENTE',
                        space     'RM11R-LBLNI'     <fs_entrysheet_no>-entrysheet_no,
                        co_flag   'SAPLMLSR'        '0400',
                        space     'BDC_OKCODE'      '=AKCH',
                        space     'BDC_CURSOR'      'RM11P-NEW_ROW',
*                        space     'RM11P-NEW_ROW'   '10',
                        co_flag   'SAPLMLSR'        '0400',
                        space     'BDC_OKCODE'      '=ACCR',
                        space     'BDC_CURSOR'      'RM11P-NEW_ROW',
*                        space     'RM11P-NEW_ROW'   '10',
                        co_flag   'SAPLMLSR'        '0400',
                        space     'BDC_OKCODE'      '=SAVE',
                        space     'BDC_CURSOR'      'RM11P-NEW_ROW',
*                        space     'RM11P-NEW_ROW'   '10',
                        co_flag   'SAPLSPO1'        '0300',
                        space     'BDC_OKCODE'      '=YES',
                        co_flag   'SAPLMLSR'        '0110',
                        space     'BDC_CURSOR'      'IMKPF-BLDAT',
                        space     'BDC_OKCODE'      '=ENTE'.

        PERFORM call_transaction USING 'ML81N' CHANGING lv_message lv_msgtyp.

        IF lv_message CS 'No batch input data'.
          WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ELSEIF lv_msgtyp EQ 'S'.
          WRITE:/ co_green, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ELSE.
          WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_entrysheet_no>.
    ENDIF.
  ELSE.
    MESSAGE 'Please provide an Entry Sheet Number' TYPE 'E'. "#EC NOTEXT
  ENDIF.

ENDFORM.                    " STEP01

*&---------------------------------------------------------------------*
*&      Form  STEP02
*&---------------------------------------------------------------------*
*       Delete line items per Service Entry Sheet
*----------------------------------------------------------------------*
FORM step02 .

  DATA: lv_message TYPE natxt,
        lv_msgtyp  TYPE char1.

  DATA: lt_entrysheet_no TYPE TABLE OF ty_entrysheet_no.

  FIELD-SYMBOLS: <fs_entrysheet_no> TYPE ty_entrysheet_no.

  IF so_es2 IS NOT INITIAL.
    SELECT lblni
      INTO TABLE lt_entrysheet_no
      FROM essr
      WHERE lblni IN so_es2.

    IF sy-subrc EQ 0.
      LOOP AT lt_entrysheet_no ASSIGNING <fs_entrysheet_no>.
        mc_bdc_dynpro:  co_flag 'SAPLMLSR'      '0400',
                        space   'BDC_OKCODE'    '=SELP',
                        space   'BDC_CURSOR'    'RM11P-NEW_ROW',
                        space   'RM11P-NEW_ROW' '10',
                        co_flag 'SAPLMLSR'      '0340',
                        space   'BDC_CURSOR'    'RM11R-LBLNI',
                        space   'BDC_OKCODE'    '=ENTE',
                        space   'RM11R-LBLNI'   <fs_entrysheet_no>-entrysheet_no,
                        co_flag 'SAPLMLSR'      '0400',
                        space   'BDC_OKCODE'    '=AKCH',
                        space   'BDC_CURSOR'    'RM11P-NEW_ROW',
*                        space   'RM11P-NEW_ROW' '10',
                        co_flag 'SAPLMLSR'      '0400',
                        space   'BDC_CURSOR'    'ESSR-TXZ01',
                        space   'BDC_OKCODE'    '=BAM',
*                        space   'ESSR-TXZ01'    'PROGRESS BILLING#1',
*                        space   'ESSR-BLDAT'    '06/23/2014',
*                        space   'ESSR-BUDAT'    '06/23/2014',
*                        space   'RM11P-NEW_ROW' '10',
                        co_flag 'SAPLMLSR'      '0400',
                        space   'BDC_OKCODE'    '/EBDZ',
                        space   'BDC_CURSOR'    'ESSR-TXZ01',
*                        space   'ESSR-TXZ01'    'PROGRESS BILLING#1',
*                        space   'ESSR-BLDAT'    '06/23/2014',
*                        space   'ESSR-BUDAT'    '06/23/2014',
*                        space   'RM11P-NEW_ROW' '10',
                        co_flag 'SAPLSPO1'      '0100',
                        space   'BDC_OKCODE'    '=YES',
                        co_flag 'SAPLMLSR'      '0400',
                        space   'BDC_CURSOR'    'ESSR-TXZ01',
                        space   'BDC_OKCODE'    '=SAVE',
*                        space   'ESSR-TXZ01'    'PROGRESS BILLING#1',
*                        space   'ESSR-LBLDT'    '06/24/2014',
*                        space   'ESSR-BLDAT'    '06/23/2014',
*                        space   'ESSR-BUDAT'    '06/23/2014',
*                        space   'RM11P-NEW_ROW' '10',
                        co_flag 'SAPLSPO1'      '0300',
                        space   'BDC_OKCODE'    '=YES'.

        PERFORM call_transaction USING 'ML81N' CHANGING lv_message lv_msgtyp.

        IF lv_message CS 'No batch input data'.
          WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ELSEIF lv_msgtyp EQ 'S'.
          WRITE:/ co_green, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ELSE.
          WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_entrysheet_no>.
    ENDIF.
  ELSE.
    MESSAGE 'Please provide an Entry Sheet Number' TYPE 'E'. "#EC NOTEXT
  ENDIF.
ENDFORM.                    " STEP02
*&---------------------------------------------------------------------*
*&      Form  STEP03
*&---------------------------------------------------------------------*
*       Delete Service Entry Sheet
*----------------------------------------------------------------------*
FORM step03 .

  DATA: lt_return TYPE TABLE OF bapiret2,
        lt_entrysheet_no TYPE TABLE OF ty_entrysheet_no.

  FIELD-SYMBOLS: <fs_entrysheet_no> TYPE ty_entrysheet_no,
                 <fs_return>        TYPE bapiret2.

  IF so_es3 IS NOT INITIAL.
    SELECT lblni
      INTO TABLE lt_entrysheet_no
      FROM essr
      WHERE lblni IN so_es3.

    IF sy-subrc EQ 0.
      LOOP AT lt_entrysheet_no ASSIGNING <fs_entrysheet_no>.
        CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
          EXPORTING
            entrysheet = <fs_entrysheet_no>-entrysheet_no
          TABLES
            return     = lt_return.

        LOOP AT lt_return ASSIGNING <fs_return>.
          IF <fs_return>-type EQ 'S' OR <fs_return>-type EQ 'I'.
            WRITE:/ co_green, <fs_entrysheet_no>-entrysheet_no, <fs_return>-message.
          ELSE.
            WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, <fs_return>-message.
          ENDIF.
        ENDLOOP.
        UNASSIGN <fs_return>.
      ENDLOOP.
      UNASSIGN <fs_entrysheet_no>.
    ENDIF.
  ELSE.
    MESSAGE 'Please provide an Entry Sheet Number' TYPE 'E'. "#EC NOTEXT
  ENDIF.

ENDFORM.                    " STEP03
*&---------------------------------------------------------------------*
*&      Form  STEP04
*&---------------------------------------------------------------------*
*       Set Condition New Pricing per PO line item
*----------------------------------------------------------------------*
FORM step04 .

  DATA: lv_message TYPE natxt,
        lv_msgtyp  TYPE char1.

  DATA: lv_pdoc          TYPE selopt,
        lv_item_no_num   TYPE i,
        lv_item_no       TYPE c LENGTH 4,
        lv_pack_no       TYPE packno,
        lv_pack_no_count TYPE i.

  DATA: lt_eslh          TYPE TABLE OF ty_eslh.

  FIELD-SYMBOLS: <fs_eslh> TYPE ty_eslh.

  IF so_pdoc1 IS NOT INITIAL.
    LOOP AT so_pdoc1 INTO lv_pdoc.
      IF strlen( lv_pdoc-low ) EQ 15.
        APPEND INITIAL LINE TO lt_eslh ASSIGNING <fs_eslh>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_pdoc-low(10)
          IMPORTING
            output = <fs_eslh>-ebeln.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_pdoc-low+10(5)
          IMPORTING
            output = <fs_eslh>-ebelp.
      ELSE.
        WRITE:/ co_red, lv_pdoc-low, 'Invalid length for Purchase Document No. and Item No.'. "#EC NOTEXT
      ENDIF.
    ENDLOOP.

    SORT lt_eslh BY ebeln ebelp ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_eslh.

    LOOP AT lt_eslh ASSIGNING <fs_eslh>.
      lv_item_no_num = <fs_eslh>-ebelp / 10.
      lv_item_no     = lv_item_no_num.
      IF strlen( lv_item_no ) LT 4.
        SHIFT lv_item_no RIGHT.
      ENDIF.

      IF lv_item_no GT 0.
        CLEAR: lv_pack_no, lv_pack_no_count.

        SELECT MAX( packno )
        INTO lv_pack_no
        FROM eslh
        WHERE ebeln EQ <fs_eslh>-ebeln
          AND ebelp EQ <fs_eslh>-ebelp.

        IF sy-subrc EQ 0.
          "WRITE:/ <fs_eslh>-ebeln, lv_item_no, lv_pack_no.

          SELECT COUNT( DISTINCT introw )
          INTO lv_pack_no_count
          FROM esll
          WHERE packno EQ lv_pack_no.

          IF sy-subrc EQ 0.
            "WRITE:/ lv_pack_no_count.
            mc_bdc_dynpro:  co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=MECHOB',
                            co_flag 'SAPLMEGUI'           '0002',
                            space   'BDC_OKCODE'          '=MEOK',
                            space   'BDC_CURSOR'          'MEPO_SELECT-EBELN',
                            space   'MEPO_SELECT-EBELN'   <fs_eslh>-ebeln,
                            "space   'MEPO_SELECT-BSTYP_F' 'X',
                            co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=MEV4001BUTTON',
                            co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=MEV4002BUTTON',
                            co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=DDOWN3200',
                            space   'BDC_CURSOR'          'DYN_6000-LIST',
                            space   'DYN_6000-LIST'       lv_pack_no,
                            "space   'RM11P-NEW_ROW'       '10',
                            co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=BAM',
                            "space   'DYN_6000-LIST'       '   3',
                            space   'BDC_CURSOR'          'ESLL-SRVPOS(01)',
                            "space   'RM11P-NEW_ROW'       '10',
                            co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=COND',
                            "space   'DYN_6000-LIST'       '   3',
                            space   'BDC_CURSOR'          'ESLL-SRVPOS(01)'.
            "space   'RM11P-NEW_ROW'       '10'.

            DO lv_pack_no_count TIMES.
              mc_bdc_dynpro:  co_flag 'SAPLV69A'            '9000',
                              space   'BDC_OKCODE'          '=V69A_KONB',
                              space   'BDC_CURSOR'          'KOMV-KSCHL(10)',
                              co_flag 'SAPLV69A'            '9000',
                              space   'BDC_OKCODE'          '=BACK',
                              space   'BDC_CURSOR'          'KOMV-KSCHL(10)',
                              co_flag 'SAPLMLSK'            '0200',
                              space   'BDC_CURSOR'          'RM11K-MKNTM(01)',
                              space   'BDC_OKCODE'          '=BACK'.
            ENDDO.

            mc_bdc_dynpro:  co_flag 'SAPLMEGUI'           '0014',
                            space   'BDC_OKCODE'          '=MESAVE',
                            "space   'DYN_6000-LIST'       '   3',
                            space   'BDC_CURSOR'          'ESLL-SRVPOS(01)',
                            space   'RM11P-NEW_ROW'       '10',
                            co_flag 'SAPLSPO2'            '0101',
                            space   'BDC_OKCODE'          '=OPT1'.

            PERFORM call_transaction USING 'ME22N' CHANGING lv_message lv_msgtyp.

            IF lv_message CS 'No batch input data'.
              WRITE:/ co_red, <fs_eslh>-ebeln, <fs_eslh>-ebelp, lv_message.
            ELSEIF lv_msgtyp EQ 'S'.
              WRITE:/ co_green, <fs_eslh>-ebeln, <fs_eslh>-ebelp, lv_message.
            ELSE.
              WRITE:/ co_red, <fs_eslh>-ebeln, <fs_eslh>-ebelp, lv_message.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        WRITE:/ co_red, <fs_eslh>-ebeln, <fs_eslh>-ebelp, 'Invalid Item No.'. "#EC NOTEXT
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_eslh>.
  ELSE.
    MESSAGE 'Please provide a Purchase Document No. and Item No.' TYPE 'E'. "#EC NOTEXT
  ENDIF.
ENDFORM.                    " STEP04
*&---------------------------------------------------------------------*
*&      Form  STEP05
*&---------------------------------------------------------------------*
*       Delete PO line items
*----------------------------------------------------------------------*
FORM step05 .
  DATA: lt_return   TYPE TABLE OF bapiret2,
        lt_poitem   TYPE TABLE OF bapimepoitem,
        lt_poitemx  TYPE TABLE OF bapimepoitemx,
        lt_eslh     TYPE TABLE OF ty_eslh.

  DATA: lv_pdoc     TYPE selopt,
        lv_po_num   TYPE bapimepoheader-po_number.

  FIELD-SYMBOLS: <fs_eslh>    TYPE ty_eslh,
                 <fs_poitem>  TYPE bapimepoitem,
                 <fs_poitemx> TYPE bapimepoitemx,
                 <fs_return>  TYPE bapiret2.


  IF so_pdoc2 IS NOT INITIAL.
    LOOP AT so_pdoc2 INTO lv_pdoc.
      IF strlen( lv_pdoc-low ) EQ 15.
        APPEND INITIAL LINE TO lt_eslh ASSIGNING <fs_eslh>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_pdoc-low(10)
          IMPORTING
            output = <fs_eslh>-ebeln.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_pdoc-low+10(5)
          IMPORTING
            output = <fs_eslh>-ebelp.
      ELSE.
        WRITE:/ co_red, lv_pdoc-low, 'Invalid length for Purchase Document No. and Item No.'. "#EC NOTEXT
      ENDIF.
    ENDLOOP.

    SORT lt_eslh BY ebeln ebelp ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_eslh.

    LOOP AT lt_eslh ASSIGNING <fs_eslh>.
      REFRESH: lt_poitem, lt_poitemx.
      CLEAR:   lt_poitem, lt_poitemx.

      lv_po_num = <fs_eslh>-ebeln.

      APPEND INITIAL LINE TO lt_poitem ASSIGNING <fs_poitem>.
      <fs_poitem>-po_item     = <fs_eslh>-ebelp.
      <fs_poitem>-delete_ind  = 'X'.

      APPEND INITIAL LINE TO lt_poitemx ASSIGNING <fs_poitemx>.
      <fs_poitemx>-po_item     = <fs_eslh>-ebelp.
      <fs_poitemx>-po_itemx    = 'X'.
      <fs_poitemx>-delete_ind  = 'X'.

      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder = lv_po_num
        TABLES
          return        = lt_return
          poitem        = lt_poitem
          poitemx       = lt_poitemx.

      READ TABLE lt_return ASSIGNING <fs_return> WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      LOOP AT lt_return ASSIGNING <fs_return>.
        IF <fs_return>-type EQ 'S' OR <fs_return>-type EQ 'I'.
          WRITE:/ co_green, <fs_eslh>-ebeln, <fs_eslh>-ebelp, <fs_return>-message.
        ELSE.
          WRITE:/ co_red, <fs_eslh>-ebeln, <fs_eslh>-ebelp, <fs_return>-message.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_return>.
    ENDLOOP.
    UNASSIGN <fs_eslh>.
  ELSE.
    MESSAGE 'Please provide a Purchase Document No. and Item No.' TYPE 'E'. "#EC NOTEXT
  ENDIF.
ENDFORM.                    " STEP05
*&---------------------------------------------------------------------*
*&      Form  STEP06
*&---------------------------------------------------------------------*
*       Set Condition New Pricing per MO External line item
*----------------------------------------------------------------------*
FORM step06 .
  DATA: lv_message       TYPE natxt,
        lv_msgtyp        TYPE char1,
        lv_pack_no       TYPE packno,
        lv_pack_no_count TYPE i.

  DATA: lt_ekkn    TYPE TABLE OF ty_ekkn.

  FIELD-SYMBOLS: <fs_ekkn> TYPE ty_ekkn.

  IF so_mo1 IS NOT INITIAL.
    SELECT ebeln ebelp aufnr
    INTO TABLE lt_ekkn
    FROM ekkn
    WHERE aufnr IN so_mo1.

    IF sy-subrc EQ 0.
      LOOP AT lt_ekkn ASSIGNING <fs_ekkn>.
        CLEAR: lv_pack_no, lv_pack_no_count.

        SELECT MAX( packno )
        INTO lv_pack_no
        FROM eslh
        WHERE ebeln EQ <fs_ekkn>-ebeln
          AND ebelp EQ <fs_ekkn>-ebelp.

        IF sy-subrc EQ 0.
          SELECT COUNT( DISTINCT introw )
          INTO lv_pack_no_count
          FROM esll
          WHERE packno EQ lv_pack_no.

          IF sy-subrc EQ 0.
            mc_bdc_dynpro:  co_flag 'SAPLCOIH'      '0101',
                            space   'BDC_CURSOR'    'CAUFVD-AUFNR',
                            space   'BDC_OKCODE'    '=VGUE',
                            space   'CAUFVD-AUFNR'  <fs_ekkn>-aufnr,
                            co_flag 'SAPLCOIH'      '3000',
                            space   'BDC_OKCODE'    '/00',
                            space   'BDC_CURSOR'    'AFVGD-LTXA1(02)',
                            co_flag 'SAPLCOIH'      '3000',
                            space   'BDC_OKCODE'    '=VGD2',
                            space   'BDC_CURSOR'    'AFVGD-LTXA1(02)',
                            co_flag 'SAPLCOIH'      '3000',
                            space   'BDC_OKCODE'    '=BAM',
                            space   'BDC_CURSOR'    'AFVGD-LTXA1',
                            co_flag 'SAPLCOIH'      '3000',
                            space   'BDC_OKCODE'    '=COND',
                            space   'BDC_CURSOR'    'AFVGD-LTXA1'.

            DO lv_pack_no_count TIMES.
              mc_bdc_dynpro:  co_flag 'SAPLV69A'      '9000',
                              space   'BDC_OKCODE'    '=V69A_KONB',
                              space   'BDC_CURSOR'    'KOMV-KSCHL(10)',
                              co_flag 'SAPLV69A'      '9000',
                              space   'BDC_OKCODE'    '=BACK',
                              space   'BDC_CURSOR'    'KOMV-KSCHL(10)'.
            ENDDO.

            mc_bdc_dynpro:  co_flag 'SAPLCOIH'      '3000',
                            space   'BDC_OKCODE'    '=BU',
                            space   'BDC_CURSOR'    'AFVGD-LTXA1'.

            PERFORM call_transaction USING 'IW32' CHANGING lv_message lv_msgtyp.

            IF lv_message CS 'Error calculating costs for order'. "#EC NOTEXT
              WRITE:/ co_green, 'Order', <fs_ekkn>-aufnr, 'saved with notification'. "#EC NOTEXT
            ELSEIF lv_message CS 'No batch input data'.     "#EC NOTEXT
              WRITE:/ co_red, <fs_ekkn>-aufnr, lv_message.
            ELSEIF lv_msgtyp EQ 'S'.
              WRITE:/ co_green, <fs_ekkn>-aufnr, lv_message.
            ELSE.
              WRITE:/ co_red,<fs_ekkn>-aufnr, lv_message.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_ekkn>.
    ENDIF.
  ELSE.
    MESSAGE 'Please provide an MO No.' TYPE 'E'.            "#EC NOTEXT
  ENDIF.
ENDFORM.                    " STEP06
*&---------------------------------------------------------------------*
*&      Form  STEP09
*&---------------------------------------------------------------------*
*       Create New Service Entry Sheets per PO line item
*----------------------------------------------------------------------*
FORM step09 .
  DATA: lt_return   TYPE TABLE OF bapiret2.

  DATA: ls_entrysheetheader TYPE bapiessrc,
        lv_pdoc             TYPE selopt.

  FIELD-SYMBOLS: <fs_return> TYPE bapiret2.

  IF so_pdoc3 IS NOT INITIAL.
    IF pa_stxt IS NOT INITIAL.
      LOOP AT so_pdoc3 INTO lv_pdoc.
        CLEAR: ls_entrysheetheader.

        IF strlen( lv_pdoc-low ) EQ 15.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_pdoc-low(10)
            IMPORTING
              output = ls_entrysheetheader-po_number.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_pdoc-low+10(5)
            IMPORTING
              output = ls_entrysheetheader-po_item.

          ls_entrysheetheader-short_text = pa_stxt.

          CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
            EXPORTING
              entrysheetheader = ls_entrysheetheader
            TABLES
              return           = lt_return.

          LOOP AT lt_return ASSIGNING <fs_return>.
            IF <fs_return>-type EQ 'S' OR <fs_return>-type EQ 'I'.
              WRITE:/ co_green, ls_entrysheetheader-po_number, ls_entrysheetheader-po_item, <fs_return>-message.
            ELSE.
              WRITE:/ co_red, ls_entrysheetheader-po_number, ls_entrysheetheader-po_item, <fs_return>-message.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_return>.

          WAIT UP TO '0.5' SECONDS.
        ELSE.
          WRITE:/ co_red, lv_pdoc-low, 'Invalid length for Purchase Document No. and Item No.'. "#EC NOTEXT
        ENDIF.
      ENDLOOP.

    ELSE.
      MESSAGE 'Please provide the Short Text.' TYPE 'E'.    "#EC NOTEXT
    ENDIF.
  ELSE.
    MESSAGE 'Please provide a Purchase Document No. and Item No.' TYPE 'E'. "#EC NOTEXT
  ENDIF.

ENDFORM.                    " STEP09
*&---------------------------------------------------------------------*
*&      Form  STEP10
*&---------------------------------------------------------------------*
*       Accept New Service Entry Sheets per PO line item
*----------------------------------------------------------------------*
FORM step10 .
  DATA: lv_message       TYPE natxt,
        lv_msgtyp        TYPE char1,
        lv_pack_no       TYPE packno,
        lv_pack_no_count TYPE i,
        lv_ebeln         TYPE essr-ebeln,
        lv_ebelp         TYPE essr-ebelp.

  DATA: lt_entrysheet_no TYPE TABLE OF ty_entrysheet_no.

  FIELD-SYMBOLS: <fs_entrysheet_no> TYPE ty_entrysheet_no.

  IF so_es4 IS NOT INITIAL.
    SELECT lblni
    INTO TABLE lt_entrysheet_no
    FROM essr
    WHERE lblni IN so_es4.

    IF sy-subrc EQ 0.
      LOOP AT lt_entrysheet_no ASSIGNING <fs_entrysheet_no>.

        CLEAR: lv_pack_no, lv_pack_no_count.

        SELECT SINGLE ebeln ebelp
        INTO (lv_ebeln, lv_ebelp)
        FROM essr
        WHERE lblni EQ <fs_entrysheet_no>-entrysheet_no.

        IF sy-subrc EQ 0.
          SELECT MAX( packno )
          INTO lv_pack_no
          FROM eslh
          WHERE ebeln EQ lv_ebeln
            AND ebelp EQ lv_ebelp.

          IF sy-subrc EQ 0.
            SELECT COUNT( DISTINCT introw )
            INTO lv_pack_no_count
            FROM esll
            WHERE packno EQ lv_pack_no.

            IF sy-subrc EQ 0.
              mc_bdc_dynpro:  co_flag 'SAPLMLSR'      '0400',
                              space   'BDC_OKCODE'    '=SELP',
                              space   'BDC_CURSOR'    'RM11P-NEW_ROW',
                              co_flag 'SAPLMLSR'      '0340',
                              space   'BDC_CURSOR'    'RM11R-LBLNI',
                              space   'BDC_OKCODE'    '=ENTE',
                              space   'RM11R-LBLNI'   <fs_entrysheet_no>-entrysheet_no,
                              co_flag 'SAPLMLSR'      '0400',
                              space   'BDC_OKCODE'    '=AKCH',
                              space   'BDC_CURSOR'    'RM11P-NEW_ROW',
                              co_flag 'SAPLMLSR'      '0400',
                              space   'BDC_OKCODE'    '=VOKO',
                              co_flag 'SAPLMLSP'      '0500',
                              space   'RM11P-MENGE_VOLL'  'X',
                              space   'BDC_OKCODE'    '=OK',
                              co_flag 'SAPLMLSP'      '201',
                              space   'BDC_OKCODE'    '=BAM',
                              co_flag 'SAPLMLSP'      '201',
                              space   'BDC_OKCODE'    '=GRPD'.

              DO lv_pack_no_count TIMES.
                mc_bdc_dynpro:  co_flag 'SAPLMLSK'      '200',
                                space   'BDC_OKCODE'    '=BACK'.
              ENDDO.

              mc_bdc_dynpro:   co_flag 'SAPLMLSR'      '0400',
                                 space   'BDC_CURSOR'    'ESSR-TXZ01',
                                 space   'BDC_OKCODE'    '=ACCP',
                                 co_flag 'SAPLMLSR'      '0400',
                                 space   'BDC_OKCODE'    '=SAVE',
                                 space   'BDC_CURSOR'    'RM11P-NEW_ROW',
                                 co_flag 'SAPLSPO1'      '0300',
                                 space   'BDC_OKCODE'    '=YES'.

              PERFORM call_transaction USING 'ML81N' CHANGING lv_message lv_msgtyp.

              IF lv_message CS 'No batch input data'.
                WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
              ELSEIF lv_msgtyp EQ 'S'.
                WRITE:/ co_green, <fs_entrysheet_no>-entrysheet_no, lv_message.
              ELSE.
                WRITE:/ co_red, <fs_entrysheet_no>-entrysheet_no, lv_message.
              ENDIF.

              WAIT UP TO '0.5' SECONDS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_entrysheet_no>.
    ENDIF.
  ELSE.
    MESSAGE 'Please provide an Entry Sheet Number' TYPE 'E'. "#EC NOTEXT
  ENDIF.
ENDFORM.                    " STEP10