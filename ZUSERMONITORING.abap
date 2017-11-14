*&---------------------------------------------------------------------*
*& PROGRAM     : ZUSERMONITORING
*& DESCRIPTION : Automation of SAP License Monitoring
*
*&---------------------------------------------------------------------*
*& Module       : ISU - On Site Billing
*& Author       : JLIWAG
*& Designed by  :
*& Date Created : 12/18/2012
*& Reference    : TN#24130
*&---------------------------------------------------------------------*
*&    ID   | Reference  |    Date    |   Description
*&---------------------------------------------------------------------*
*& JLIWAG  |TN#24130    | 12/18/2012 | Initial Development
*&         |            |            |
*&---------------------------------------------------------------------*


REPORT zprg_usermonitoring.

* Data declarations
DATA : gt_userlist TYPE STANDARD TABLE OF zuseraudit,"Output table
       gt_maillist TYPE STANDARD TABLE OF zuser_email_list,"Output table
       gt_usr01 TYPE STANDARD TABLE OF zuseraudit,"USR01
       gt_userlist2 TYPE STANDARD TABLE OF zuseraudit,"USR01
       gt_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat,"Field catalog
       i_selected_rows TYPE lvc_t_row,"Selected Rows
       w_selected_rows TYPE lvc_s_row,
       i_modified TYPE STANDARD TABLE OF zuseraudit,"For getting modified rows
       i_modifiedmail TYPE STANDARD TABLE OF zuser_email_list,"For getting modified rows
       w_modified TYPE zuseraudit,
       w_modifiedmail TYPE zuser_email_list,
       ls_userlist TYPE zuseraudit,
       ls_userlistmail TYPE zuser_email_list,
       ls_userupdate TYPE zuseraudit,
       ls_userupdate2 TYPE zuseraudit,
       w_variant TYPE disvariant,
       o_docking TYPE REF TO cl_gui_docking_container,"Docking Container
       o_grid TYPE REF TO cl_gui_alv_grid."Grid


DATA: BEGIN  OF bnameusr OCCURS 0,
      bname LIKE usr01-bname,
      END OF bnameusr.



TABLES zuseraudit.
TABLES usr01.
TABLES zuser_email_list.

TABLES sscrfields.
DATA flag.

TYPES:  BEGIN OF t_dropdown,
        area TYPE zuseraudit-gsber,
        END OF t_dropdown,

        BEGIN OF ty_upload,
                 gsber        TYPE char20,
                 bname        TYPE char20,
                 uname        TYPE char50,
                 roles        TYPE char30,
                 remarks      TYPE char30,
                 valfrom      TYPE char8,
                 valto        TYPE char8,
                 update_tag   TYPE char8,
                 update_date  TYPE char1,
       END OF ty_upload,

       BEGIN OF ty_uploadmail,
                 mandt            TYPE char3,
                 com_head         TYPE char40,
                 ba_code          TYPE char20,
                 ba_name          TYPE char50,
                 com_head_email   TYPE char100,
                 com_head_copy    TYPE char1,
                 bo_head          TYPE char40,
                 bo_head_email    TYPE char100,
                 bo_head_copy     TYPE char1,
       END OF ty_uploadmail.


DATA: it_dropdown TYPE STANDARD TABLE OF  t_dropdown,
      wa_dropdown LIKE LINE OF  it_dropdown.

DATA: gt_upload   TYPE STANDARD TABLE OF ty_upload,
      gt_uploadmail type STANDARD TABLE OF ty_uploadmail.

DATA:   gt_raw        TYPE truxs_t_text_data.

DATA:   gv_update     TYPE ty_upload,
        gv_updatemail type ty_uploadmail.

FIELD-SYMBOLS : <fs_fieldcat> TYPE lvc_s_fcat,
                <fs_bnameusr> TYPE zuseraudit,
                <fs_bnameusr2> TYPE zuseraudit,
                <fs_upload> TYPE ty_upload,
                <fs_uploadmail> type ty_uploadmail.

TYPE-POOLS: vrm.

DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

CONSTANTS: co_separator TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.



*--------------------------------------------------------------------*
*     Parameters
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:    pa_mod  RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rusr,
               pa_upd  RADIOBUTTON GROUP grp1,
               pa_send  RADIOBUTTON GROUP grp1,
               pa_mail RADIOBUTTON GROUP grp1,
               pa_upl RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
"--  modify user list
SELECT-OPTIONS so_mod FOR zuseraudit-gsber NO INTERVALS MODIF ID p2.
PARAMETERS: pa_area(20) AS LISTBOX VISIBLE LENGTH 10 MODIF ID p3 .
PARAMETERS: pa_due LIKE sy-datum DEFAULT sy-datum MODIF ID p4.
SELECT-OPTIONS pa_rec FOR zuser_email_list-com_head_email NO INTERVALS MODIF ID p5.
SELECT-OPTIONS so_mail FOR zuser_email_list-com_head NO INTERVALS MODIF ID p6.
PARAMETERS: pa_sap AS CHECKBOX  MODIF ID p7,
            pa_list AS CHECKBOX MODIF ID p8.
PARAMETERS: pa_dis AS CHECKBOX DEFAULT 'X' MODIF ID p9.
PARAMETERS: filenam TYPE rlgrap-filename MODIF ID p10 DEFAULT  'C:\Users\indra\Desktop\Janzel\Documents\TN#24130\Upload_Userlist2.xls'.  "'C:\test_it02.xls'.
PARAMETERS: pa_uplu RADIOBUTTON GROUP grp2 MODIF ID p11  DEFAULT 'X' USER-COMMAND rusr,
            pa_upll RADIOBUTTON GROUP grp2 MODIF ID p12.
SELECTION-SCREEN END OF BLOCK b2.



INITIALIZATION.

  SELECT DISTINCT gsber
FROM zuseraudit
INTO TABLE it_dropdown WHERE gsber NE ' ' GROUP BY gsber.

  LOOP AT it_dropdown INTO wa_dropdown.
    value-key =  wa_dropdown-area.
    value-text = wa_dropdown-area.
    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'pa_area'
      values = list.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR filenam.
  PERFORM f4_filenam USING filenam.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen. "Modify Screen based on Selection

START-OF-SELECTION.
  DATA: lv_test TYPE string.


  CASE 'X'.
      "Radio button for update
    WHEN pa_upd.

      IF pa_list NE 'X' AND pa_sap NE 'X'.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel     = 'Incomplete Parameter'
            textline1 = 'No selected item from checkbox.'
            textline2 = 'Plese select any item from the checkbox.'.
      ELSE.

        SELECT bname FROM usr01 INTO TABLE gt_usr01.
        SELECT * FROM zuseraudit INTO TABLE gt_userlist2 WHERE update_tag NE 'X' .

        IF pa_list EQ 'X'.
          LOOP AT gt_usr01 ASSIGNING <fs_bnameusr2>.
            READ TABLE gt_userlist2 WITH KEY bname = <fs_bnameusr2>-bname TRANSPORTING NO FIELDS.
            IF sy-subrc EQ 4.
              WRITE: / <fs_bnameusr2>-bname, 'In SAP but not in user list '.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF pa_sap EQ 'X'.
          LOOP AT gt_userlist2 ASSIGNING <fs_bnameusr>.
            READ TABLE gt_usr01 WITH KEY bname = <fs_bnameusr>-bname TRANSPORTING NO FIELDS.
            IF sy-subrc EQ 4.
              WRITE:/ <fs_bnameusr>-bname, 'In AS list but not in SAP'.
            ENDIF.

          ENDLOOP.
        ENDIF.

      ENDIF.

      "Radio button for modifying data
    WHEN pa_mod.
      IF pa_area = ' '.
        IF pa_dis EQ 'X'.
          SELECT * FROM zuseraudit INTO TABLE gt_userlist WHERE bname IN so_mod AND update_tag NE 'X' .
        ELSE.
          SELECT * FROM zuseraudit INTO TABLE gt_userlist WHERE bname IN so_mod.
        ENDIF.
      ELSE.
        IF pa_dis EQ 'X'.
          SELECT * FROM zuseraudit INTO TABLE gt_userlist WHERE bname IN so_mod AND gsber EQ pa_area AND update_tag NE 'X'.
        ELSE.
          SELECT * FROM zuseraudit INTO TABLE gt_userlist WHERE bname IN so_mod AND gsber EQ pa_area.
        ENDIF.
      ENDIF.
      CALL SCREEN 9000.

      "Radio button for sending email notification
    WHEN pa_send.
      DATA: returncode TYPE c.

      IF sy-uname EQ 'KFLORES'.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Send Email Notification'
            text_question         = 'You are about to send SAP user list Continue?'
            display_cancel_button = ' '
          IMPORTING
            answer                = returncode.
        IF returncode = 1.
          PERFORM send_notification.
        ENDIF.
      ELSE.
        WRITE:/ 'You are not allowed to send email to users.'.
      ENDIF.

      "Radio button for maintaining email address of receivers
    WHEN pa_mail.
      SELECT   * FROM zuser_email_list INTO TABLE gt_maillist WHERE com_head IN so_mail.
      CALL SCREEN 9001.

    WHEN pa_upl.
      IF pa_uplu = 'X'.
        PERFORM upload_userlist.
      ELSEIF  pa_upll = 'X'.
        PERFORM upload_emaillist.
      ENDIF.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN pa_upd.
        IF screen-group1 ='P1' OR screen-group1 ='P2' OR screen-group1 ='P3' OR screen-group1 ='P4' OR screen-group1 ='P5' OR screen-group1 ='P6' OR screen-group1 ='P9' OR screen-group1 ='P10' OR screen-group1 ='P11' OR screen-group1 ='P12'.
*          SET TITLEBAR 'UPDATE ' WITH 'MODIFY'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN pa_mod.
        IF screen-group1 ='P1' OR screen-group1 ='P4' OR screen-group1 ='P5' OR screen-group1 ='P6' OR screen-group1 ='P7' OR screen-group1 ='P8' OR screen-group1 ='P10' OR screen-group1 ='P11' OR screen-group1 ='P12' .
*          SET TITLEBAR 'MODIFY USER DETAILS' WITH 'MODIFY'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN pa_send.
        IF screen-group1 ='P1' OR screen-group1 ='P2' OR screen-group1 ='P6' OR screen-group1 ='P7'  OR screen-group1 ='P8' OR screen-group1 ='P9' OR screen-group1 ='P10' OR screen-group1 ='P11' OR screen-group1 ='P12'.
*          SET TITLEBAR 'SEND EMAIL NOTIFICATION' WITH 'MODIFY'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN pa_mail.
        IF screen-group1 ='P1' OR screen-group1 ='P2' OR screen-group1 ='P3' OR screen-group1 ='P4' OR screen-group1 ='P5' OR screen-group1 ='P7' OR screen-group1 ='P8' OR screen-group1 ='P9' OR screen-group1 ='P10' OR screen-group1 ='P11' OR
screen-group1 ='P12'.
*          SET TITLEBAR 'SEND EMAIL NOTIFICATION' WITH 'MODIFY'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN pa_upl.
        IF screen-group1 ='P1' OR screen-group1 ='P2' OR screen-group1 ='P3' OR screen-group1 ='P4' OR screen-group1 ='P5' OR screen-group1 ='P7' OR screen-group1 ='P8' OR screen-group1 ='P9' OR screen-group1 ='P6'..
*          SET TITLEBAR 'SEND EMAIL NOTIFICATION' WITH 'MODIFY'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*

MODULE status_9000 OUTPUT.

  IF o_docking IS INITIAL.
    SET PF-STATUS 'ZSTATUS'. "GUI Status
    SET TITLEBAR 'MAINTENANCE LIST'.   "Title
* Creating Docking Container and grid
    PERFORM create_object.
* Filling the fieldcatalog table
    PERFORM create_fieldcat.
* Modifying the fieldcatalog table
    PERFORM modify_fieldcat.
* Registering edit
    PERFORM register_edit.
* Displaying the output
    PERFORM display_output.
  ENDIF.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*



MODULE user_command_9000 INPUT.

  DATA lv_ucomm TYPE sy-ucomm.
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'CANCEL' OR 'EXIT'.
      PERFORM free_objects.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN 'SAVE'.
      PERFORM save_database.
      CALL METHOD o_grid->refresh_table_display.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*

MODULE status_9001 OUTPUT.

  IF o_docking IS INITIAL.
    SET PF-STATUS 'ZSTATUS'. "GUI Status
    SET TITLEBAR 'USER LIST'.   "Title
* Creating Docking Container and grid
    PERFORM create_object.
* Filling the fieldcatalog table
    PERFORM create_fieldcatmail.
* Modifying the fieldcatalog table
    PERFORM modify_fieldcatmail.
* Registering edit
    PERFORM register_edit.
* Displaying the output
    PERFORM display_outputmail.
  ENDIF.
ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*



MODULE user_command_9001 INPUT.

  DATA lv_ucomm2 TYPE sy-ucomm.
  lv_ucomm2 = sy-ucomm.
  CASE lv_ucomm2.
    WHEN 'CANCEL' OR 'EXIT'.
      PERFORM free_objects.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN 'SAVE'.
      PERFORM save_databasemail.
      CALL METHOD o_grid->refresh_table_display.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT


*&---------------------------------------------------------------------*
*&      Form  create_object
*&---------------------------------------------------------------------*
*       Creating Docking Container and grid
*----------------------------------------------------------------------*

FORM create_object .
* Creating Docking Container
  CREATE OBJECT o_docking
    EXPORTING
      ratio = '95'.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid
      EXPORTING
        i_parent = o_docking.
  ENDIF.

ENDFORM.                    " create_object

*&---------------------------------------------------------------------*
*&      Form  create_fieldcat
*&---------------------------------------------------------------------*
*       Filling the fieldcatalog table
*----------------------------------------------------------------------*

FORM create_fieldcat .
* Filling the fieldcatalog table
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZUSERAUDIT'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
ENDFORM.                    " create_fieldcat

*&---------------------------------------------------------------------*
*&      Form  create_fieldcatmail
*&---------------------------------------------------------------------*
*       Filling the fieldcatalog table for mailing list
*----------------------------------------------------------------------*

FORM create_fieldcatmail .
* Filling the fieldcatalog table
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZUSER_EMAIL_LIST'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
ENDFORM.                    " create_fieldcat for mail


*&---------------------------------------------------------------------*
*&      Form  modify_fieldcat
*&---------------------------------------------------------------------*
*       Making the column as ediable
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  modify_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_fieldcat .
  LOOP AT gt_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
** Making a column as Editable


      WHEN 'GSBER'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-003.
        <fs_fieldcat>-col_pos = '1'.
      WHEN 'BNAME'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-009.
        <fs_fieldcat>-col_pos = '2'.
      WHEN 'UNAME'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-004.
        <fs_fieldcat>-col_pos = '3'.
      WHEN 'ROLES'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-005.
        <fs_fieldcat>-col_pos = '4'.
      WHEN 'REMARKS'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-008.
        <fs_fieldcat>-col_pos = '5'.
      WHEN 'VALFROM'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-006.
        <fs_fieldcat>-col_pos = '6'.
      WHEN 'VALTO'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-007.
        <fs_fieldcat>-col_pos = '7'.
      WHEN 'UPDATE_TAG'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-002.
        <fs_fieldcat>-col_pos = '8'.
      WHEN 'UPDATE_DATE'.
        <fs_fieldcat>-scrtext_l = text-010.
        <fs_fieldcat>-col_pos = '9'.


    ENDCASE.
  ENDLOOP.
ENDFORM.                    " modify_fieldcat

*&---------------------------------------------------------------------*
*&      Form  modify_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_fieldcatmail .
  LOOP AT gt_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
** Making a column as Editable

      WHEN 'BA_CODE'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-011.
        <fs_fieldcat>-col_pos = '1'.
      WHEN 'COM_HEAD'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-012.
        <fs_fieldcat>-col_pos = '2'.
      WHEN 'COM_HEAD_EMAIL'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-013.
        <fs_fieldcat>-col_pos = '3'.
      WHEN 'BO_HEAD_EMAIL'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-014.
        <fs_fieldcat>-col_pos = '4'.
      WHEN 'COM_HEAD_COPY'.
        <fs_fieldcat>-edit = 'X'.
        <fs_fieldcat>-scrtext_l = text-014.
        <fs_fieldcat>-col_pos = '4'.
      WHEN 'BO_HEAD_COPY' OR  'BO_HEAD' OR 'BA_NAME'.
        <fs_fieldcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " modify_fieldcat for mailing list


*&---------------------------------------------------------------------*
*&      Form  register_edit
*&---------------------------------------------------------------------*
*       Registering Edit
*----------------------------------------------------------------------*

FORM register_edit .
  CALL METHOD o_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
ENDFORM.                    " register_edit

*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       Displaying the output
*----------------------------------------------------------------------*

FORM display_output .
  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_userlist
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " display_output

*&---------------------------------------------------------------------*
*&      Form  display_outputmail
*&---------------------------------------------------------------------*
*       Displaying the output foe mailing list
*----------------------------------------------------------------------*

FORM display_outputmail .
  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_maillist
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " display_output

*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       Free Objects
*----------------------------------------------------------------------*

FORM free_objects .
  CALL METHOD o_grid->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD o_docking->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " free_objects

*&---------------------------------------------------------------------*
*&      Form  save_database
*&---------------------------------------------------------------------*
*       Save in database
*----------------------------------------------------------------------*
FORM save_database.
* Getting the selected rows index
  CALL METHOD o_grid->get_selected_rows
    IMPORTING
      et_index_rows = i_selected_rows.

* Through the index capturing the values of selected rows
  LOOP AT i_selected_rows INTO w_selected_rows.
    READ TABLE gt_userlist INTO ls_userlist INDEX w_selected_rows-index.
    IF sy-subrc EQ 0.
      ls_userlist-update_date = sy-datum.
      MOVE-CORRESPONDING ls_userlist TO w_modified.
      APPEND w_modified TO i_modified.
    ENDIF.
  ENDLOOP.
  MODIFY zuseraudit FROM TABLE i_modified.
ENDFORM.                    " save_database


*&---------------------------------------------------------------------*
*&      Form  save_databasemail
*&---------------------------------------------------------------------*
*       Save in database for mailing list
*----------------------------------------------------------------------*
FORM save_databasemail.
* Getting the selected rows index
  CALL METHOD o_grid->get_selected_rows
    IMPORTING
      et_index_rows = i_selected_rows.

* Through the index capturing the values of selected rows
  LOOP AT i_selected_rows INTO w_selected_rows.
    READ TABLE gt_maillist INTO ls_userlistmail INDEX w_selected_rows-index.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_userlistmail TO w_modifiedmail.
      APPEND w_modifiedmail TO i_modifiedmail.
    ENDIF.
  ENDLOOP.
  MODIFY zuser_email_list FROM TABLE i_modifiedmail.
ENDFORM.                    " save_database

*&---------------------------------------------------------------------*
*&      Form  send_notification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_notification.
  "-- Checks if Checkbox is Ticked

  DATA: lv_sender     TYPE so_rec_ext VALUE 'it.operation@MAYNILADWATER.COM.PH',    "Email Sender
        lv_receiver   TYPE so_recname ,                                           "Email Receiver
        lv_subject    TYPE so_obj_des VALUE 'SAP User List',                      "Email Subject
        it_messages   TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE,
        it_receivers  LIKE somlreci1         OCCURS 0 WITH HEADER LINE,
        lv_copy       TYPE c LENGTH 1,
        lv_ba_name    TYPE c LENGTH 50,
        lv_message    TYPE string .


  DATA: lv_port_range TYPE c LENGTH 15.

  DATA: lv_subrc      TYPE sysubrc.

  DATA:c TYPE string.

  DATA: v_str TYPE string.

  DATA: it_email      TYPE TABLE OF zuser_email_list.

  FIELD-SYMBOLS:
        <fs_email>    TYPE zuser_email_list.

  DATA :
  gs_zuseraudit TYPE zuseraudit,
  gt_zuseraudit TYPE TABLE OF zuseraudit,
  gv_soli_tab_line TYPE LINE OF soli_tab,
  gv_soli_tab TYPE soli_tab. " Email Body

  DATA:
    tab TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    str TYPE string.

  "-- Macro Definition for Adding Receivers
  DEFINE m_append_receivers.
    it_receivers-receiver = &1.
    if &2 eq 'X'.
      it_receivers-copy   = 'X'.
    endif.
    it_receivers-rec_type = 'U'.

    append it_receivers.
    clear  it_receivers.
  END-OF-DEFINITION.

  "-- Macro Definition for Adding Messages
  DEFINE m_append_messages.
    it_messages = &1.
    append  it_messages.
  END-OF-DEFINITION.

  "-- Macro Definition for Ranges
  DEFINE m_ranges.
    &1-sign   = 'I'.
    &1-option = 'EQ'.
    &1-low    = &3.
    append &1 to &2.
  END-OF-DEFINITION.

  "-- Macro Definition for Formatting Date from YYYYMMDD to MM/DD/YYYY
  DEFINE mc_format_date.
    concatenate &1+4(2)
                &1+6(2)
                &1+0(4)
         into &2 separated by '/'.
  END-OF-DEFINITION.



  "-- Populate Email Receivers
  CLEAR   it_receivers.
  REFRESH it_receivers.


  "-- Fetch from database table (Per BA)
  SELECT *
  FROM   zuser_email_list
    INTO CORRESPONDING FIELDS OF TABLE it_email
  WHERE  ba_code EQ pa_area.


  IF sy-subrc EQ 0.

    LOOP AT it_email ASSIGNING <fs_email>.
      IF <fs_email>-ba_name IS NOT INITIAL.
        CONCATENATE <fs_email>-ba_name `,` INTO lv_ba_name.
      ENDIF.

      m_append_receivers:
        <fs_email>-com_head_email <fs_email>-com_head_copy.

      IF <fs_email>-bo_head_email IS NOT INITIAL.
        m_append_receivers:
          <fs_email>-bo_head_email  <fs_email>-bo_head_copy.
      ENDIF.
    ENDLOOP.

*  m_append_receivers: 'janzel.its@mayniladwater.com.ph' ''.
*                      'glenn.its@mayniladwater.com.ph' '',
*                      'reena.its@mayniladwater.com.ph' ''.

    LOOP AT pa_rec.
      m_append_receivers: pa_rec-low ''.
    ENDLOOP.

    "-- Populate Email Body
    CLEAR   it_messages.
    REFRESH it_messages.

    mc_format_date: pa_due v_str.


    m_append_messages:
        'Dear Sir/Madam,',
        '',
        'Please be informed of the PROD licenses currently assigned to your department. As part of our monthly clean-up activities, we would like to request for your help to check our records for the following scenarios:',
        '',
        '1.   If the records presented below are indeed correct (number of users assigned to your department and their roles)',
        '2.   If your department have users that no longer need their PROD accounts, was transferred to a different department, or have any other reason that require transferring of licenses. ',
        '',
        'We  recommend the following:',
          'o  Transfer of license to the new user (kindly identify the required role for the new user)',
          'o  Deletion of account, while we wait for a new replacement',
        '3.   If there are accounts being used by a different user other than the one initially defined, we recommend transferring of accounts.',
        '',
        '4.   If there are PROD account users re-assigned to perform a different SAP function other than the role/s currently specified in the above tabulation, kindly indicate their current role so we can adjust the authorization in SAP.',
        '',
        'Should you have further queries or concerns, please notify us at ITS-Application Support. Thank you so much for your usual support  and cooperation.',
        '',
        'Please confirm once received.',
        '',
        'Note:',
        '--  If we do not receive any feedback we will assume that our information are correct and no changes are needed.  Expired temporary accounts will no longer be reactivated. Please give us your feedback before',
        v_str,
        ''.
    CONCATENATE 'AREA' co_separator  co_separator 'USER' co_separator  co_separator  co_separator 'ROLE' co_separator co_separator co_separator  'Remarks' INTO str.
    m_append_messages: str.


    SELECT *  FROM zuseraudit INTO TABLE gt_zuseraudit WHERE gsber = pa_area AND update_tag NE 'X' .
    LOOP AT gt_zuseraudit INTO gs_zuseraudit.
      CONCATENATE gs_zuseraudit-gsber co_separator co_separator  gs_zuseraudit-bname  co_separator  co_separator gs_zuseraudit-roles co_separator co_separator gs_zuseraudit-remarks INTO str.
      m_append_messages: str.
    ENDLOOP.

    m_append_messages:'',
            'Thank you!',
            'IT Process',
            'Outsourcing Services',
            'Indra Philippines, Inc.'.

    "-- Call Custom Function Module
    CALL FUNCTION 'ZFM_SEND_SIMPLE_EMAIL'
      EXPORTING
        sender       = lv_sender
        subject      = lv_subject
      TABLES
        it_messages  = it_messages
        it_receivers = it_receivers.
    WRITE:/ 'Email Sent to Users'.
  ELSE.
    WRITE:/ 'No Users in the Email List for the department', pa_area .

  ENDIF.
ENDFORM.                    " SEND_NOTIFICATION

*&---------------------------------------------------------------------*
*&      Form  F4_FILENAM
*&---------------------------------------------------------------------*
* This subroutine calls a function module to display the file
* open dialog box
*&---------------------------------------------------------------------*
FORM f4_filenam  USING    p_filenam.
  CALL FUNCTION 'F4_FILENAME'
*   EXPORTING
*     PROGRAM_NAME        = SYST-CPROG
*     DYNPRO_NUMBER       = SYST-DYNNR
*     FIELD_NAME          = ' '
   IMPORTING
     file_name           = p_filenam.
ENDFORM.                    " F4_FILENAM

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_userlist .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = gt_raw
      i_filename           = filenam
    TABLES
      i_tab_converted_data = gt_upload[].

  IF sy-subrc EQ 0.
    SORT gt_upload BY gsber.

    DELETE ADJACENT DUPLICATES FROM gt_upload.
    LOOP AT gt_upload ASSIGNING <fs_upload> .
      gv_update-gsber       = <fs_upload>-gsber.
      gv_update-bname       = <fs_upload>-bname.
      gv_update-uname       = <fs_upload>-uname.
      gv_update-roles       = <fs_upload>-roles.
      gv_update-remarks     = <fs_upload>-remarks.
      gv_update-valfrom     = <fs_upload>-valfrom.
      gv_update-valto       = <fs_upload>-valto.

      INSERT zuseraudit FROM gv_update.

      IF sy-subrc NE 0.
        WRITE: gv_update.
      ENDIF.
    ENDLOOP.

* Implement suitable error handling here
  ENDIF.
ENDFORM.                    "upload_userlist

*&---------------------------------------------------------------------*
*&      Form  upload_emaillist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_emaillist .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = gt_raw
      i_filename           = filenam
    TABLES
      i_tab_converted_data = gt_uploadmail[].

  IF sy-subrc EQ 0.
*    SORT gt_uploadmail BY gsber.

    DELETE ADJACENT DUPLICATES FROM gt_uploadmail.
    LOOP AT gt_uploadmail ASSIGNING <fs_uploadmail> .
      gv_updatemail-mandt            = <fs_uploadmail>-mandt.
      gv_updatemail-com_head         = <fs_uploadmail>-com_head.
      gv_updatemail-ba_code          = <fs_uploadmail>-ba_code.
      gv_updatemail-ba_name          = <fs_uploadmail>-ba_name.
      gv_updatemail-com_head_email   = <fs_uploadmail>-com_head_email.
      gv_updatemail-com_head_copy    = <fs_uploadmail>-com_head_copy.
      gv_updatemail-bo_head_email    = <fs_uploadmail>-bo_head_email.
      gv_updatemail-bo_head_copy     = <fs_uploadmail>-bo_head_copy.

      INSERT zuser_email_list FROM gv_updatemail.

      IF sy-subrc NE 0.
        WRITE: gv_updatemail.
      ENDIF.
    ENDLOOP.

* Implement suitable error handling here
  ENDIF.
ENDFORM.                    "upload_emaillist