*&---------------------------------------------------------------------*
*& Report  ZRISU_CHANGE_ADDRESS
*& DESCRIPTION: Program for
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 01/14/2013
*& Reference  : TN#24953
*&---------------------------------------------------------------------*
*& Date      | Author ID|Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2013/01/14|GDIMALIWAT|24953      | Created                          *
*&---------------------------------------------------------------------*
REPORT zrisu_change_address MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_data,
        vkont      TYPE string,
        building   TYPE string,
        roomnumber TYPE string,
        floor      TYPE string,
        str_suppl1 TYPE string,
        str_suppl2 TYPE string,
        street     TYPE string,
        house_num1 TYPE string,
        house_num2 TYPE string,
        str_suppl3 TYPE string,
        location   TYPE string,
        city2      TYPE string,
        post_code1 TYPE string,
        city1      TYPE string,
        country    TYPE string,
        region     TYPE string,
       END OF ty_data,
       BEGIN OF ty_message,
         recno    TYPE syindex,
         tcode    TYPE sytcode,
         msgtyp   TYPE icon_d,
         msge     TYPE bdc_vtext1,
         vkont    TYPE string,
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
      GV_GPART  TYPE fkkvkp-gpart,
      GV_VKONT  TYPE fkkvkp-vkont,
      gv_icon   TYPE icon_d.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS: co_add     TYPE char1      VALUE 'A',
           co_rem     TYPE char1      VALUE 'R',
           co_updmode TYPE ctu_update VALUE 'L',
           co_dismode TYPE ctu_mode   VALUE 'N',"'N',
           co_success TYPE icon_d     VALUE '@08@',
           co_warning TYPE icon_d     VALUE '@09@',
           co_error   TYPE icon_d     VALUE '@0A@',
           co_sub_err TYPE balsubobj  VALUE 'ERROR'.

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
PARAMETERS: pa_data TYPE rlgrap-filename  DEFAULT '\\saprep01.mayniladsap.com\repuserdata\Glenn\ZRISU_CHANGE_ADDRESS\test.txt'.
*select-options: so_<name> for  <data obj>.
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
    APPEND INITIAL LINE TO gt_message ASSIGNING <fs_mess>.
    <fs_mess>-recno  = &1.
    <fs_mess>-tcode  = &2.
    <fs_mess>-msgtyp = &3.
    <fs_mess>-msge   = &4.
    <fs_mess>-vkont  = &5.
  END-OF-DEFINITION.
  "--Macro definition for BDC creation
  DEFINE m_bdc_dynpro.
    APPEND INITIAL LINE TO gt_bdcdata ASSIGNING <fs_bdc>.
    IF &1 EQ abap_true.
      <fs_bdc>-program  = &2.
      <fs_bdc>-dynpro   = &3.
      <fs_bdc>-dynbegin = &1.
    ELSE.
      <fs_bdc>-fnam     = &2.
      <fs_bdc>-fval     = &3.
    ENDIF.
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

   CLEAR GV_GPART.
   CLEAR GV_VKONT.

   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
     EXPORTING
       input         = <fs_data>-vkont
     IMPORTING
       OUTPUT        = GV_VKONT.

   SELECT SINGLE GPART
   INTO GV_GPART
   FROM FKKVKP
   WHERE VKONT eq GV_VKONT.

   IF sy-subrc eq 0.
     "PERFORM TEST.
     PERFORM UPDATE_BP_ADDRESS.
     PERFORM UPDATE_CONNOBJ_ADDRESS.
   ENDIF.
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

        SPLIT lv_string AT '|' INTO <fs_data>-vkont
                                    <fs_data>-building
                                    <fs_data>-roomnumber
                                    <fs_data>-floor
                                    <fs_data>-str_suppl1
                                    <fs_data>-str_suppl2
                                    <fs_data>-street
                                    <fs_data>-house_num1
                                    <fs_data>-house_num2
                                    <fs_data>-str_suppl3
                                    <fs_data>-location
                                    <fs_data>-city2
                                    <fs_data>-post_code1
                                    <fs_data>-city1
                                    <fs_data>-country
                                    <fs_data>-region.
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
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       Call transaction according to parameters passed
*----------------------------------------------------------------------*
*      -->PV_TCODE  Transaction Code
*      -->PV_VKONT  Contract Account Number
*----------------------------------------------------------------------*
FORM bdc_transaction USING VALUE(pv_tcode)
                            pv_vkont  TYPE string.

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
*    MESSAGE s006 WITH pv_tcode INTO gv_msge.
*    m_message gv_index pv_tcode co_success gv_msge pv_vkont.
*    WRITE:/ 'SUCCESS'.
    gv_icon = co_success.
    WRITE:/ gv_icon, pv_vkont, 'Connection Object Address was updated'.
*    IF lt_bdcmsgs[] IS INITIAL. "Blank means successful processing
*      IF lv_log IS INITIAL.
*
*      ENDIF.
*    ELSE.
*      PERFORM get_bdc_messages TABLES lt_bdcmsgs USING pv_tcode.
*      WRITE:/ 'GLENN 2'.
*    ENDIF.
  ELSE.
*    PERFORM get_bdc_messages TABLES lt_bdcmsgs USING pv_tcode pv_vkont.
    WRITE:/ gv_icon , pv_vkont, 'Connection Object Address was not updated'.
    LOOP AT lt_bdcmsgs ASSIGNING <fs_msgs>.
      PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr <fs_msgs>-msgtyp <fs_msgs>-msgv1 <fs_msgs>-msgv2 <fs_msgs>-msgv3 <fs_msgs>-msgv4
                           CHANGING gv_msge gv_icon.
      IF gv_msge IS NOT INITIAL.
        WRITE:/7 gv_icon, gv_msge.
      ENDIF.
    ENDLOOP.
  ENDIF.
  REFRESH gt_bdcdata.
ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BP_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form UPDATE_BP_ADDRESS .
  DATA: LV_ADDRESS       TYPE BAPIBUS1006_ADDRESS,
        LV_ADDRESSX      TYPE BAPIBUS1006_ADDRESS_X,
        LV_RETURN        TYPE TABLE OF BAPIRET2,
        LV_RETURN_LINES  TYPE string VALUE '0'.

  FIELD-SYMBOLS: <FS_RETURN>   TYPE BAPIRET2.

  "-- Transfer field values to Screen fields
  LV_ADDRESS-STR_SUPPL1   = <fs_data>-str_suppl1.
  LV_ADDRESS-STR_SUPPL2   = <fs_data>-str_suppl2.
  LV_ADDRESS-STREET       = <fs_data>-street.
  LV_ADDRESS-HOUSE_NO     = <fs_data>-house_num1.
  LV_ADDRESS-HOUSE_NO2    = <fs_data>-house_num2.
  LV_ADDRESS-STR_SUPPL3   = <fs_data>-str_suppl3.
  LV_ADDRESS-LOCATION     = <fs_data>-location.
  LV_ADDRESS-DISTRICT     = <fs_data>-city2.
  LV_ADDRESS-POSTL_COD1   = <fs_data>-post_code1.
  LV_ADDRESS-CITY         = <fs_data>-city1.
  LV_ADDRESS-COUNTRY      = <fs_data>-country.
  LV_ADDRESS-REGION       = <fs_data>-region.

  "-- Tag each screen field for update
  LV_ADDRESSX-STR_SUPPL1   = 'X'.
  LV_ADDRESSX-STR_SUPPL2   = 'X'.
  LV_ADDRESSX-STREET       = 'X'.
  LV_ADDRESSX-HOUSE_NO     = 'X'.
  LV_ADDRESSX-HOUSE_NO2    = 'X'.
  LV_ADDRESSX-STR_SUPPL3   = 'X'.
  LV_ADDRESSX-LOCATION     = 'X'.
  LV_ADDRESSX-DISTRICT     = 'X'.
  LV_ADDRESSX-POSTL_COD1   = 'X'.
  LV_ADDRESSX-CITY         = 'X'.
  LV_ADDRESSX-COUNTRY      = 'X'.
  LV_ADDRESSX-REGION       = 'X'.


  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner              = GV_GPART
      ADDRESSDATA                  = LV_ADDRESS
      ADDRESSDATA_X                = LV_ADDRESSX
*     DUPLICATE_MESSAGE_TYPE       =
      ACCEPT_ERROR                 = 'X'
    TABLES
      RETURN                       = LV_RETURN.

  "-- If there are returned Errors
  LOOP AT LV_RETURN ASSIGNING <FS_RETURN>.
*WRITE:/ 'Result: ', <FS_RETURN>-TYPE, <FS_RETURN>-NUMBER, <FS_RETURN>-MESSAGE.
    LV_RETURN_LINES = sy-tabix.
    gv_icon = co_error.
    WRITE:/ gv_icon , <fs_data>-vkont, 'Business Partner Address was not updated', <FS_RETURN>-TYPE, <FS_RETURN>-NUMBER, <FS_RETURN>-MESSAGE.
  ENDLOOP.

    IF LV_RETURN_LINES EQ '0'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
*      IF ls_return-message IS INITIAL.
*        MESSAGE s042 INTO ls_return-message WITH lv_inst_doc.
*      ENDIF.
      gv_icon = co_success.
      WRITE:/ gv_icon, <fs_data>-vkont, 'Business Partner Address was updated'.
    ENDIF.
endform.                    " UPDATE_BP_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CONNOBJ_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form UPDATE_CONNOBJ_ADDRESS .

  DATA: LV_ANLAGE   type EVER-ANLAGE,
        LV_VSTELLE  type EANL-VSTELLE,
        LV_HAUS     type EVBS-HAUS.

  "--Set Call Transaction Options
  gs_option-dismode = co_dismode.
  gs_option-updmode = co_updmode.
  gs_option-nobinpt = abap_true.

  CLEAR LV_ANLAGE.
  CLEAR LV_VSTELLE.
  CLEAR LV_HAUS.

  SELECT SINGLE ANLAGE
  INTO LV_ANLAGE
  FROM EVER
  WHERE VKONTO eq '000050000005'
    AND AUSZDAT eq '99991231'.

  IF sy-subrc eq 0.
    SELECT SINGLE VSTELLE
    INTO LV_VSTELLE
    FROM EANL
    WHERE ANLAGE eq LV_ANLAGE.

      IF sy-subrc eq 0.
        SELECT SINGLE HAUS
        INTO LV_HAUS
        FROM EVBS
        WHERE VSTELLE eq LV_VSTELLE.

          IF sy-subrc eq 0.
            m_bdc_dynpro: 'X' 'SAPLES55' '0100',
                          ' ' 'BDC_CURSOR' 'EHAUD-HAUS',
                          ' ' 'BDC_OKCODE' '/00',
                          ' ' 'EHAUD-HAUS' LV_HAUS,
                          'X' 'SAPLES55' '0200',
                          ' ' 'BDC_OKCODE' '=$2OC',
                          'X' 'SAPLES55' '0200',
                          ' ' 'BDC_OKCODE' '=SAVE',
*                          ' ' 'EHAUD-REGIOGROUP' '05210900',
                          ' ' 'BDC_CURSOR' 'ADDR1_DATA-LOCATION',
                          ' ' 'ADDR1_DATA-STR_SUPPL1' <fs_data>-str_suppl1,
                          ' ' 'ADDR1_DATA-STR_SUPPL2' <fs_data>-str_suppl2,
                          ' ' 'ADDR1_DATA-STREET' <fs_data>-street,
                          ' ' 'ADDR1_DATA-HOUSE_NUM1' <fs_data>-house_num1,
                          ' ' 'ADDR1_DATA-HOUSE_NUM2' <fs_data>-house_num2,
                          ' ' 'ADDR1_DATA-STR_SUPPL3' <fs_data>-str_suppl3,
                          ' ' 'ADDR1_DATA-LOCATION' <fs_data>-location,
                          ' ' 'ADDR1_DATA-CITY2' <fs_data>-city2,
                          ' ' 'ADDR1_DATA-POST_CODE1' <fs_data>-post_code1,
                          ' ' 'ADDR1_DATA-CITY1' <fs_data>-city1,
                          ' ' 'ADDR1_DATA-COUNTRY' <fs_data>-country,
                          ' ' 'ADDR1_DATA-REGION' <fs_data>-region,
                          ' ' 'ADDR1_DATA-TIME_ZONE' 'UTC+8'.
*                          ' ' 'ADDR1_DATA-REGIOGROUP' '05210900'.
            perform bdc_transaction using 'ES56' <fs_data>-vkont.
*            LOOP AT gt_message ASSIGNING <fs_mess>.
*              WRITE:/ <fs_mess>-vkont, <fs_mess>-recno, <fs_mess>-tcode, <fs_mess>-msgtyp, <fs_mess>-msge.
*            ENDLOOP.
         ENDIF.
      ENDIF.
   ENDIF.
endform.                    " UPDATE_CONNOBJ_ADDRESS
**&---------------------------------------------------------------------*
**&      Form  TEST
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*form TEST .
*    WRITE:/ GV_GPART,
*            <fs_data>-vkont,
*            <fs_data>-building,
*            <fs_data>-roomnumber,
*            <fs_data>-floor,
*            <fs_data>-str_suppl1,
*            <fs_data>-str_suppl2,
*            <fs_data>-street,
*            <fs_data>-house_num1,
*            <fs_data>-house_num2,
*            <fs_data>-str_suppl3,
*            <fs_data>-location,
*            <fs_data>-city2,
*            <fs_data>-post_code1,
*            <fs_data>-city1,
*            <fs_data>-country,
*            <fs_data>-region.
*endform.                    " TEST
**&---------------------------------------------------------------------*
**&      Form  GET_BDC_MESSAGES
**&---------------------------------------------------------------------*
**       Get bdc messages
**----------------------------------------------------------------------*
**      -->PT_BDCMSGS  BDC Messages
**      -->PV_TCODE    Transaction Code
**      -->PV_VKONT  Contract Account Number
**----------------------------------------------------------------------*
*FORM get_bdc_messages  TABLES pt_bdcmsgs TYPE tab_bdcmsgcoll
*                       USING  pv_tcode
*                              pv_vkont.
*  DATA: lv_msgtyp   TYPE icon_d.
*  LOOP AT pt_bdcmsgs ASSIGNING <fs_msgs>.
*    CLEAR: lv_msgtyp, gv_msge.
*    PERFORM format_message USING <fs_msgs>-msgid <fs_msgs>-msgnr <fs_msgs>-msgtyp <fs_msgs>-msgv1 <fs_msgs>-msgv2 <fs_msgs>-msgv3 <fs_msgs>-msgv4
*                           CHANGING gv_msge lv_msgtyp.
*    IF gv_msge IS NOT INITIAL.
*      m_message gv_index pv_tcode lv_msgtyp gv_msge pv_vkont.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " get_bdc_messages
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
FORM format_message  USING  VALUE(pv_msgid)
                            VALUE(pv_msgnr)
                            VALUE(pv_msgtyp)
                            VALUE(pv_msgv1)
                            VALUE(pv_msgv2)
                            VALUE(pv_msgv3)
                            VALUE(pv_msgv4)
                     CHANGING VALUE(pv_msgs)
                              VALUE(pv_type).

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
      NOT_FOUND = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
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