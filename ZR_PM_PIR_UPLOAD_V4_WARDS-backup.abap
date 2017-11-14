*&---------------------------------------------------------------------*
*&  Include           ZR_PM_PIR_UPLOAD_V4_WARDS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  pir_alv_details_build
*&---------------------------------------------------------------------*
*       Build the new planning using the upload file and using the logic specified in the fs(or check logic bellow). -> CT_ALV
*       the word 'flag' is used in this whole program mostly as VERB! ^^V :D
*       Consolidate the entire raw upload file per matnr, berid, version, and the right month fields (from month1 to month 12) -> CT_ALV
*       ^(date on upload file is in the form YYYYMM, move them in the proper fields for display in this routine)^
*       Include PIR version 00 that needs to be deactivated in the return table -> CT_ALV
*          >> and flag the field 'deact' -> CT_ALV-DEACT
*       Decision whether the upload file is NEW or will serve as an UPDATE to an existing PIR is done in this routine.
*       The field 'isnew' is responsible for indicating whether the item is to be created or will serve as change to existing -> CT_ALV-ISNEW
*----------------------------------------------------------------------*
*      -->UT_PBIM            text
*      -->UT_PBED            text
*      -->CT_UPLOAD_SUMMARY  text
*      -->CT_ALV             text
*----------------------------------------------------------------------*
FORM pir_alv_details_build USING      ut_pbim           TYPE tt_pbim_ex
                                      ut_pbed           TYPE tt_pbed_ex
                           CHANGING   ct_upload_summary TYPE tt_alv_upsum
                                      ct_alv            TYPE tt_alv_main.

  DATA: ls_usum     LIKE LINE OF ct_upload_summary,
        lt_usum     LIKE STANDARD TABLE OF ls_usum,
        lt_usum_raw LIKE STANDARD TABLE OF ls_usum.

  DATA: ls_newplan LIKE LINE OF ct_upload_summary,
        lt_newplan LIKE STANDARD TABLE OF ls_newplan.

  DATA: ls_alv_tmp LIKE LINE OF ct_alv,
        lt_alv_tmp LIKE STANDARD TABLE OF ls_alv_tmp.

  DATA: ls_alv_fin LIKE LINE OF ct_alv,
        lt_alv_fin LIKE STANDARD TABLE OF ls_alv_fin.

  DATA: lv_tmp_fldname TYPE char20.

  DATA: lv_action_command TYPE char01. "D -> Deactivate version 00 and create New. "C -> Change "N -> Create NEW  "M -> Create/Deactivate V00,Create/Activate V01

  FIELD-SYMBOLS: <ls_pbim> LIKE LINE OF ut_pbim,
                 <ls_pbed> LIKE LINE OF ut_pbed.

  FIELD-SYMBOLS: <ls_newplan>   LIKE LINE OF lt_newplan,
                 <lv_month_val> TYPE any,
                 <ls_alv_tmp>   LIKE LINE OF lt_alv_tmp,
                 <ls_alv_tmp_read>   LIKE LINE OF lt_alv_tmp,
                 <ls_usum>      LIKE LINE OF lt_usum,
                 <ls_usum_read> LIKE LINE OF lt_usum.

  FIELD-SYMBOLS: <ls_upload_sum> LIKE LINE OF ct_upload_summary.


*& init.
  CLEAR: ct_alv.

*& translate existing PIRs to the form(type) of upload summary. for easier comparison ;)
  LOOP AT ut_pbim ASSIGNING <ls_pbim>.
    LOOP AT ut_pbed ASSIGNING <ls_pbed> WHERE bdzei = <ls_pbim>-bdzei.
      CLEAR: ls_usum.
      "-- pbim
      ls_usum-matnr    =   <ls_pbim>-matnr  .
      ls_usum-berid    =   <ls_pbim>-werks  .
      ls_usum-vrsn     =   <ls_pbim>-versb  .
      IF <ls_pbim>-vervs = 'X'.
        ls_usum-act_vsn  = <ls_pbim>-versb.
      ENDIF.
      "-- pbed
      ls_usum-rqty     =   <ls_pbed>-plnmg.
*      ls_usum-rdate    =   <ls_pbed>-perxx.
      ls_usum-rdate    =   <ls_pbed>-pdatu+0(6). "12052013 - OPacalso/KCao
      ls_usum-bdzei    =   <ls_pbed>-bdzei.
      ls_usum-year     =   <ls_pbed>-pdatu+0(4).
      COLLECT ls_usum INTO lt_usum.
    ENDLOOP.
  ENDLOOP.

**& quick fix -> scenario: since current program throws error(no version 00 found) immediately if first item doesnt have version 00,
**&                      : tag field active version if atleast 1 item for the whole year is found with active.
*  LOOP AT lt_usum ASSIGNING <ls_usum> WHERE act_vsn IS INITIAL.
*    READ TABLE lt_usum ASSIGNING <ls_usum_read>
*    WITH KEY matnr  =  <ls_usum>-matnr
*             berid  =  <ls_usum>-berid
*             year   =  <ls_usum>-year.
*    IF sy-subrc IS INITIAL.
*
*    ELSE.
*
*    ENDIF.
*  ENDLOOP.


*& check whether PIR is to be changed or created, also preserve the field act_vsn and assign the right bdzei(link of pbim and pbed) value.
*& flag field isnew depending on the result.
  SORT lt_usum BY matnr
                  berid
                  rdate
                  act_vsn DESCENDING. "Sort important, to retrieve the active version first if exist
  LOOP AT ct_upload_summary ASSIGNING <ls_upload_sum>
  WHERE iserror IS INITIAL.

    READ TABLE lt_usum ASSIGNING <ls_usum>    "check exist.
      WITH KEY matnr = <ls_upload_sum>-matnr
               berid = <ls_upload_sum>-berid
               .
    IF sy-subrc IS INITIAL.
      <ls_upload_sum>-isnew    = space. "item exist.
      <ls_upload_sum>-bdzei    = <ls_usum>-bdzei.

      "-- check for active version. search whole year
*      READ TABLE lt_usum ASSIGNING <ls_usum_read>
*        WITH KEY matnr = <ls_upload_sum>-matnr
*                 berid = <ls_upload_sum>-berid
*                 vrsn  = <ls_upload_sum>-vrsn
*                 year  = <ls_upload_sum>-year.
*      IF sy-subrc IS INITIAL.
*        <ls_upload_sum>-act_vsn  = <ls_usum_read>-act_vsn.
*      ENDIF.
      CLEAR: <ls_upload_sum>-act_vsn.
      LOOP AT lt_usum ASSIGNING <ls_usum_read>
      WHERE matnr = <ls_upload_sum>-matnr
        AND berid = <ls_upload_sum>-berid
        AND year  = <ls_upload_sum>-year
        AND act_vsn IS NOT INITIAL .

        <ls_upload_sum>-act_vsn  = <ls_usum_read>-act_vsn.
        EXIT.
      ENDLOOP.

    ELSE.
      <ls_upload_sum>-isnew    = 'X'.
      <ls_upload_sum>-act_vsn  = space.
      <ls_upload_sum>-bdzei    = space.
***********************************************************************
** quick fix
*      DATA: ls_pbim2 TYPE pbim.
*      CLEAR: ls_pbim2.
*      SELECT SINGLE
*        *
*        INTO ls_pbim2
*        FROM pbim
*        WHERE matnr = <ls_upload_sum>-matnr
*          AND werks = <ls_upload_sum>-berid
*          AND versb = <ls_upload_sum>-vrsn
*          AND vervs = 'X'
*          AND loevr <> 'D'
*        .
*      IF sy-subrc IS INITIAL.
*        <ls_upload_sum>-isnew = space.
*        <ls_upload_sum>-act_vsn = ls_pbim2-versb.
*      ELSE.
*        SELECT SINGLE
*          *
*          INTO ls_pbim2
*          FROM pbim
*          WHERE matnr = <ls_upload_sum>-matnr
*            AND werks = <ls_upload_sum>-berid
*            AND versb = <ls_upload_sum>-vrsn
*            AND loevr <> 'D'
*          .
*        IF sy-subrc IS INITIAL.
*          <ls_upload_sum>-isnew = space.
*        ELSE.
*          <ls_upload_sum>-isnew = 'X'.
*        ENDIF.
*      ENDIF.
***********************************************************************
    ENDIF.
  ENDLOOP.


*& consolidate both existing PIR and UPLOAD to form the -> NEW PLANNING!
  CLEAR: lt_newplan.
  APPEND LINES OF: lt_usum           TO lt_newplan,
                   ct_upload_summary TO lt_newplan.


*& get the summations of the consolidated table above(lt_newplan) with the correct month field assignments.
  LOOP AT lt_newplan ASSIGNING <ls_newplan>
  WHERE "iserror IS INITIAL AND
        rdate   IS NOT INITIAL.

    CLEAR: ls_alv_tmp.
    "-- build fieldname of the correct field(month) where planning value is to be assigned.
    CONCATENATE 'LS_ALV_TMP-' 'M' <ls_newplan>-rdate+4(2) '-VALUE' INTO lv_tmp_fldname.
    "-- assign the key for summa/collection.
    ls_alv_tmp-matnr     =   <ls_newplan>-matnr.
    ls_alv_tmp-berid     =   <ls_newplan>-berid.
    ls_alv_tmp-vrsn      =   <ls_newplan>-vrsn.
    ls_alv_tmp-isupload  =   <ls_newplan>-isupload.
    ls_alv_tmp-act_vsn   =   <ls_newplan>-act_vsn.
    ls_alv_tmp-year      =   <ls_newplan>-year.

    ls_alv_tmp-mdesc     =   <ls_newplan>-mdesc.
    ls_alv_tmp-uom       =   <ls_newplan>-uom.

    ls_alv_tmp-iserror   =   <ls_newplan>-iserror.

    ASSIGN (lv_tmp_fldname) TO <lv_month_val>.
    <lv_month_val>     =   <ls_newplan>-rqty.

    ADD 1 TO ls_alv_tmp-upload_ctr.

    COLLECT ls_alv_tmp INTO lt_alv_tmp.
  ENDLOOP.

*--------------------------------------------------------------------*
*& build the NEW PLANNING! -> lt_alv_fin.
*--------------------------------------------------------------------*
*& compute the change rate (assign to field rate of month)
*--------------------------------------------------------------------*
  CLEAR: lt_alv_fin.
  SORT lt_alv_tmp BY isupload
                     matnr
                     berid
                     vrsn.
  LOOP AT lt_alv_tmp ASSIGNING <ls_alv_tmp>
  WHERE isupload = 'X'. "Iterate through the consolidated upload file.

***********************************************************************
** quick fix
*    DATA: ls_pbim TYPE pbim.
*    CLEAR: ls_pbim.
*    SELECT SINGLE
*      *
*      INTO ls_pbim
*      FROM pbim
*      WHERE matnr = <ls_alv_tmp>-matnr
*        AND werks = <ls_alv_tmp>-berid
*        AND versb = <ls_alv_tmp>-vrsn
*        AND loevr <> 'D'
*      .
***********************************************************************
    DATA: lv_matnr type matnr.

    CLEAR: ls_alv_fin.
    MOVE : <ls_alv_tmp> TO ls_alv_fin.
    CLEAR: ls_alv_fin-isupload.

    "-- determine action to take according to the existing active version and the upload version.
    CLEAR: lv_action_command.
    CASE <ls_alv_tmp>-vrsn. "Check for version.
      WHEN '00'.
        CASE <ls_alv_tmp>-act_vsn. "Check for active version.
          WHEN '00'.
            lv_action_command = 'C'.
          WHEN '01'.

            "-- Start - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing
            SELECT SINGLE matnr
            INTO lv_matnr
            FROM zind_req
            WHERE matnr eq <ls_alv_tmp>-MATNR
              AND werks eq <ls_alv_tmp>-BERID
              AND versb eq '00'.

            IF sy-subrc eq 0.
              lv_action_command = 'E'.
              <ls_alv_tmp>-e_mess = 'Version 01 already exist, unable to edit version 00.'.
            ELSE.
              "-- No existing V00 in ZIND_REQ table, proceed with upload of V00
              lv_action_command = 'M'.
            ENDIF.
            "-- End - GDIMALIWAT 07/21/2014 - TN#63434
*            SHIFT <ls_alv_tmp>-matnr LEFT DELETING LEADING '0'.
*            REPLACE '&1' IN <ls_alv_tmp>-e_mess WITH <ls_alv_tmp>-matnr.
*            REPLACE '&2' IN <ls_alv_tmp>-e_mess WITH <ls_alv_tmp>-berid.
          WHEN space.
*            IF ls_pbim IS INITIAL.
*              lv_action_command = 'N'.
*            ELSE.
*              lv_action_command = 'C'.
*            ENDIF.
            lv_action_command = 'N'.
        ENDCASE.
      WHEN '01'.
        CASE <ls_alv_tmp>-act_vsn. "Check for active version.
          WHEN '00'.
            lv_action_command = 'D'.
          WHEN '01'.
            lv_action_command = 'C'.
          WHEN space.
            lv_action_command = 'E'.
            <ls_alv_tmp>-e_mess = 'Cannot upload version 01, version 00 is missing.'.
*            SHIFT <ls_alv_tmp>-matnr LEFT DELETING LEADING '0'.
*            REPLACE '&1' IN <ls_alv_tmp>-e_mess WITH <ls_alv_tmp>-matnr.
*            REPLACE '&2' IN <ls_alv_tmp>-e_mess WITH <ls_alv_tmp>-berid.
        ENDCASE.
    ENDCASE.


    "-- put into action the decision made above.
    CASE lv_action_command.
      WHEN 'D' OR 'N'.
        CASE lv_action_command.
            "-- flag deactivate indicator to deactivate this requirement later during bapi process.
          WHEN 'D'.
            READ TABLE lt_alv_tmp ASSIGNING <ls_alv_tmp_read>  "Search for the active version counterpart of the upload
              WITH KEY isupload =  space
                       matnr    =  <ls_alv_tmp>-matnr
                       berid    =  <ls_alv_tmp>-berid
                       vrsn     =  <ls_alv_tmp>-act_vsn.
            IF sy-subrc IS INITIAL.
              MOVE: <ls_alv_tmp_read>       TO ls_alv_fin,
                    'X'                     TO ls_alv_fin-deact,
                    'Will be deactivated'   TO ls_alv_fin-e_mess. " deactivate existing active
              APPEND ls_alv_fin TO lt_alv_fin.
            ELSE.
              ... "raise error, this should not happen.
            ENDIF.
          WHEN 'N'.
            ...
        ENDCASE.

        "-- append consolidated file as new requirement.
        CLEAR: ls_alv_fin, ls_alv_tmp.
        PERFORM rate_percentage_compute USING      <ls_alv_tmp>
                                                   ls_alv_tmp
                                          CHANGING ls_alv_fin.
        MOVE 'X'          TO ls_alv_fin-isnew.

      WHEN 'C'.
        "-- change, compute the final change of the requirement .
        READ TABLE lt_alv_tmp ASSIGNING <ls_alv_tmp_read>
          WITH KEY isupload =  space
                   matnr    =  <ls_alv_tmp>-matnr
                   berid    =  <ls_alv_tmp>-berid
                   vrsn     =  <ls_alv_tmp>-vrsn.
        IF sy-subrc IS INITIAL. "match found.
          "--compare the values for each month, compute the change rate and assign the new plan.
          PERFORM rate_percentage_compute USING    <ls_alv_tmp>
                                                   <ls_alv_tmp_read>
                                          CHANGING ls_alv_fin.
          MOVE: space TO ls_alv_fin-isnew.
        ELSE.
          ...
        ENDIF.

      WHEN 'M'.
        "-- Start - GDIMALIWAT 07/21/2014 - TN#63434
          MOVE: <ls_alv_tmp>-matnr      TO ls_alv_fin-matnr,
                <ls_alv_tmp>-berid      TO ls_alv_fin-berid,
                <ls_alv_tmp>-year       TO ls_alv_fin-year,
                '00'                    TO ls_alv_fin-vrsn,
                'X'                     TO ls_alv_fin-deact,
                'Will be deactivated'   TO ls_alv_fin-e_mess, " deactivate existing active
                '0.00'                  TO ls_alv_fin-M01-RATE,
                '0.00'                  TO ls_alv_fin-M01-VALUE,
                '0.00'                  TO ls_alv_fin-M02-RATE,
                '0.00'                  TO ls_alv_fin-M02-VALUE,
                '0.00'                  TO ls_alv_fin-M03-RATE,
                '0.00'                  TO ls_alv_fin-M03-VALUE,
                '0.00'                  TO ls_alv_fin-M04-RATE,
                '0.00'                  TO ls_alv_fin-M04-VALUE,
                '0.00'                  TO ls_alv_fin-M05-RATE,
                '0.00'                  TO ls_alv_fin-M05-VALUE,
                '0.00'                  TO ls_alv_fin-M06-RATE,
                '0.00'                  TO ls_alv_fin-M06-VALUE,
                '0.00'                  TO ls_alv_fin-M07-RATE,
                '0.00'                  TO ls_alv_fin-M07-VALUE,
                '0.00'                  TO ls_alv_fin-M08-RATE,
                '0.00'                  TO ls_alv_fin-M08-VALUE,
                '0.00'                  TO ls_alv_fin-M09-RATE,
                '0.00'                  TO ls_alv_fin-M09-VALUE,
                '0.00'                  TO ls_alv_fin-M10-RATE,
                '0.00'                  TO ls_alv_fin-M10-VALUE,
                '0.00'                  TO ls_alv_fin-M11-RATE,
                '0.00'                  TO ls_alv_fin-M11-VALUE,
                '0.00'                  TO ls_alv_fin-M12-RATE,
                '0.00'                  TO ls_alv_fin-M12-VALUE.
        "-- End - GDIMALIWAT 07/21/2014 - TN#63434
      WHEN OTHERS.
        ls_alv_fin-iserror = 'E'.
        ls_alv_fin-e_mess  = <ls_alv_tmp>-e_mess.
    ENDCASE.


    IF ls_alv_fin IS INITIAL.
      ...
    ELSE.
      APPEND ls_alv_fin TO lt_alv_fin.
    ENDIF.

  ENDLOOP. "^end of LOOP AT lt_alv_tmp ASSIGNING <ls_alv_tmp>
*& ^End of build the New Planning
**********************************************************************


*  "-- check increase rate. add rate to field month-rate.
*  LOOP AT lt_alv_tmp ASSIGNING <ls_alv_tmp>
*  WHERE isupload = 'X'. "isupload -> only checks upload items.
*
*    CLEAR: ls_alv_fin.
*    MOVE <ls_alv_tmp> TO ls_alv_fin.
*    CLEAR: ls_alv_fin-isupload.
*
*    "-- check matching line of upload and existing to be able to compare the values later..
*    READ TABLE lt_alv_tmp ASSIGNING <ls_alv_tmp_read>
*      WITH KEY isupload =  space
*               matnr    =  <ls_alv_tmp>-matnr
*               berid    =  <ls_alv_tmp>-berid
*               vrsn     =  <ls_alv_tmp>-vrsn
*               .
*    IF sy-subrc IS INITIAL. "match found.
*      "--compare the values for each month, compute the change rate and assign the new plan.
*      PERFORM rate_percentage_compute USING    <ls_alv_tmp>
*                                               <ls_alv_tmp_read>
*                                      CHANGING ls_alv_fin.
*    ELSE.
*      ... "no match. upload material is new, to be created.
*    ENDIF.
*
*    APPEND ls_alv_fin TO lt_alv_fin.
*  ENDLOOP.

  "-- assign to the return parameter
  IF lt_alv_fin IS INITIAL.
    CLEAR: ct_alv.
  ELSE.
    CLEAR: ct_alv.
    APPEND LINES OF lt_alv_fin TO ct_alv.
  ENDIF.


ENDFORM.                    "pir_alv_details_build


*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_ALV     text
*      -->UT_FCAT    text
*      -->US_LAYOUT  text
*----------------------------------------------------------------------*
FORM display_alv USING ut_alv TYPE tt_alv_real
                       ut_fcat
                       us_layout.

  DATA: lo_custom_contain TYPE REF TO cl_gui_custom_container,
        lo_alv_grid       TYPE REF TO cl_gui_alv_grid.


  IF ut_alv IS INITIAL.
    MESSAGE 'Nothing to process, check file for erroneous data.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.

  ENDIF.

  SORT ut_alv BY seq.


  CREATE OBJECT lo_custom_contain
    EXPORTING
      container_name = 'MAINPANEL'.

  CREATE OBJECT lo_alv_grid
    EXPORTING
      i_parent = lo_custom_contain.

  CALL METHOD lo_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout       = us_layout
    CHANGING
      it_outtab       = ut_alv
      it_fieldcatalog = ut_fcat.

  CREATE OBJECT lo_custom_contain
    EXPORTING
      container_name = 'MAINPANEL'.

  CALL SCREEN '0100'.

ENDFORM.                    "display_alv

*&---------------------------------------------------------------------*
*&      Form  build_alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CT_FLDCAT  text
*----------------------------------------------------------------------*
FORM build_alv_fieldcat   USING    ur_reptdate TYPE tt_reptd_range
                          CHANGING ct_fldcat TYPE lvc_t_fcat.

  DATA: ls_fldcat LIKE LINE OF ct_fldcat.

  DATA: lv_ctr TYPE i.

  CONSTANTS: lc_x TYPE char01 VALUE 'X'.

  FIELD-SYMBOLS: <ls_fcat> LIKE LINE OF ct_fldcat.

* Macro.
  DEFINE create_fcat_line. "create line.
    clear: ls_fldcat.

    add 1 to lv_ctr.

    ls_fldcat-col_pos     =    lv_ctr.
    ls_fldcat-fieldname   =    &1.
    ls_fldcat-outputlen   =    &2.
    ls_fldcat-scrtext_l   =    &3.
    ls_fldcat-scrtext_s   =    &4.
    ls_fldcat-no_zero     =    &5.
    ls_fldcat-emphasize   =    &6.

    ls_fldcat-fix_column  =    &7.

    append ls_fldcat to ct_fldcat.
  END-OF-DEFINITION.

  CLEAR: ct_fldcat, lv_ctr.

*** testing
*  create_fcat_line 'ZISPROJREL' '5' 'Released?' 'Released?' space space.
*  create_fcat_line 'ZISERROR'   '5' 'Error?'    'Error?' space space.

*  create_fcat_line 'SEQ'   '03' '#'  '#'  space space.

  create_fcat_line 'MATNR' '10' 'Material' 'Material'      'X'        space    space.
  create_fcat_line 'BERID' '05' 'MRP'      'MRP Area'      space      space    space.
  create_fcat_line 'VRSN'  '03' 'Ver'      'Version'       space      space    space.

  create_fcat_line 'YEAR'     '05' 'Year'     'Year'       space space    space   .
  create_fcat_line 'E_MESS'   '45' 'Note'     'Note'       space space    'X'     .

  create_fcat_line 'M01-VALUE'   '10' 'M 01'     'M 01'     space space   space    .
  create_fcat_line 'M02-VALUE'   '10' 'M 02'     'M 02'     space space   space    .
  create_fcat_line 'M03-VALUE'   '10' 'M 03'     'M 03'     space space   space    .
  create_fcat_line 'M04-VALUE'   '10' 'M 04'     'M 04'     space space   space    .
  create_fcat_line 'M05-VALUE'   '10' 'M 05'     'M 05'     space space   space    .
  create_fcat_line 'M06-VALUE'   '10' 'M 06'     'M 06'     space space   space    .
  create_fcat_line 'M07-VALUE'   '10' 'M 07'     'M 07'     space space   space    .
  create_fcat_line 'M08-VALUE'   '10' 'M 08'     'M 08'     space space   space    .
  create_fcat_line 'M09-VALUE'   '10' 'M 09'     'M 09'     space space   space    .
  create_fcat_line 'M10-VALUE'   '10' 'M 10'     'M 10'     space space   space    .
  create_fcat_line 'M11-VALUE'   '10' 'M 11'     'M 11'     space space   space    .
  create_fcat_line 'M12-VALUE'   '10' 'M 12'     'M 12'     space space   space    .




*  FIELD-SYMBOLS: <ls_reptdate> LIKE LINE OF ur_reptdate.
*
*  "-- hide certain fields
*  READ TABLE ur_reptdate ASSIGNING <ls_reptdate> INDEX 1.
*  IF sy-subrc IS INITIAL.
**    LOOP AT ct_fldcat ASSIGNING <ls_fcat> WHERE col_pos > 3.  "zxzx put this back if remove year iserror.
*    LOOP AT ct_fldcat ASSIGNING <ls_fcat> WHERE col_pos > 5.
*      IF <ls_fcat>-fieldname+1(2) GE <ls_reptdate>-low+4(2).
*        <ls_fcat>-no_out = space.
*      ELSE.
*        <ls_fcat>-no_out = 'X'.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.




ENDFORM.                    "build_alv_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_alv_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CS_LAYOUT  text
*----------------------------------------------------------------------*
FORM build_alv_layout  CHANGING cs_layout TYPE lvc_s_layo.

  CONSTANTS: lc_x TYPE char01 VALUE 'X'.
  CONSTANTS: lc_color_fname LIKE cs_layout-ctab_fname VALUE 'COLOR'.

  CLEAR: cs_layout.

  cs_layout-zebra = lc_x.
  cs_layout-ctab_fname = lc_color_fname.
  cs_layout-cwidth_opt = 'X'.

ENDFORM.                    "build_alv_layout


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR  'ZTITLE'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CONSTANTS: lc_back   LIKE sy-ucomm   VALUE 'BACK',
             lc_exit   LIKE sy-ucomm   VALUE 'EXIT',
             lc_cancel LIKE sy-ucomm   VALUE 'CANCEL',
             lc_upload LIKE sy-ucomm   VALUE 'UPLOAD'.

  DATA: lv_status TYPE char01 VALUE 'D',
        lv_ucomm3  TYPE sy-ucomm.

  CLEAR: lv_ucomm3.
  lv_ucomm3 = sy-ucomm.

  CASE sy-ucomm.

    WHEN lc_back OR lc_exit OR lc_cancel.
      LEAVE TO SCREEN 0.

    WHEN lc_upload OR 'UP_TEST'.

*      IF sy-uname = 'OPACALSO'.
*        CASE sy-ucomm.
*          WHEN 'UP_TEST'.
*            PERFORM posting_proper USING 'X'.
*          WHEN OTHERS.
*            PERFORM posting_proper USING space.
*        ENDCASE.
*
*      ELSE.
*        MESSAGE 'Functionality under construction, code rebuild on process for better experience. We apologize for the inconvenience.' TYPE 'I'.
*      ENDIF.
      DATA: lv_mess TYPE char70.
      CASE sy-ucomm.
        WHEN 'UP_TEST'.
          lv_mess = 'Proceed Test Run?'.
        WHEN OTHERS.
          lv_mess = 'Continue upload?'.
      ENDCASE.

      PERFORM ask_user_confirm_posting CHANGING lv_status
                                                lv_mess.
      CASE lv_status.
        WHEN '1'.
          ... "User ALLOWED further processing. may continue doing your stuff.
        WHEN '2'.
          LEAVE TO SCREEN 0.
        WHEN 'A'.
          RETURN.
        WHEN OTHERS.
          RETURN.
      ENDCASE.

      CASE lv_ucomm3.
        WHEN 'UP_TEST'.
          gv_istestmode = 'X'.
          PERFORM posting_proper
            USING 'X'   "istest mode?
                .
        WHEN OTHERS.
          gv_istestmode = space.
          PERFORM posting_proper
            USING space "istest mode?
                .
      ENDCASE.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT



*&---------------------------------------------------------------------*
*&      Form  color_format_add
*&---------------------------------------------------------------------*
*       Set respective colors of each cell before display of the ALV.
*       RED matnr, berid, version -> denotes PIR version 00 to be deactivated.
*       GREEN items on the quantities are NEW PIRS
*       YELLOW on qty means INCREASE EXCEEDS 20%
*       CYAN   on qty means DECREASE EXCEEDS 20%
*----------------------------------------------------------------------*
*      -->UT_PIR_DETAILS  text
*      -->CT_ALV_REAL     text
*----------------------------------------------------------------------*
FORM color_format_add  USING    ut_pir_details TYPE tt_alv_main
                       CHANGING ct_alv_real    TYPE tt_alv_real.

  TYPES: lty_alvreal LIKE LINE OF ct_alv_real,
         lty_month   TYPE lty_alvreal-m01.

  FIELD-SYMBOLS: <ls_pir_details> LIKE LINE OF ut_pir_details.

  FIELD-SYMBOLS: <ls_month>   TYPE lty_month.

  DATA: ls_alv_real LIKE LINE OF ct_alv_real.

  DATA: lv_fieldname TYPE char20,
        lv_numc2     TYPE numc2,
        lv_numc6     TYPE numc06.

  DATA: ls_color TYPE lvc_s_scol.

  CONSTANTS: BEGIN OF lc_color_struct_prime,
               col TYPE lvc_col VALUE '3', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_prime,

             BEGIN OF lc_color_struct_prime_lblue,
               col TYPE lvc_col VALUE '1', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_prime_lblue,

             BEGIN OF lc_color_struct_prime_green,
               col TYPE lvc_col VALUE '5', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_prime_green,

             BEGIN OF lc_color_struct_prime_red,
               col TYPE lvc_col VALUE '6', "color codes are as follows. Cyan, LightBlue, Yellow, darker than light blue, green, red, pink/cream
               int TYPE lvc_int VALUE '1', "Intensified flag. 1 or 0.
               inv TYPE lvc_inv VALUE space,
             END OF   lc_color_struct_prime_red.


  CHECK: ut_pir_details IS NOT INITIAL.
  CLEAR: ct_alv_real.


  "-- iterate thruogh each item to be displayed in the alv.
  LOOP AT ut_pir_details ASSIGNING <ls_pir_details>.

    CLEAR: ls_alv_real.
    MOVE-CORRESPONDING: <ls_pir_details> TO ls_alv_real.

    "-- add highlighting for deactivated items and errorneous.
    IF <ls_pir_details>-deact = 'X' OR <ls_pir_details>-iserror IS NOT INITIAL.
      IF <ls_pir_details>-iserror IS NOT INITIAL.
        CLEAR: ls_color.
        MOVE: 'MATNR' TO ls_color-fname,
              space   TO ls_color-nokeycol,
              lc_color_struct_prime_red
                      TO ls_color-color.
        APPEND ls_color TO ls_alv_real-color.

        MOVE: 'BERID' TO ls_color-fname.
        APPEND ls_color TO ls_alv_real-color.

        MOVE: 'VRSN'  TO ls_color-fname.
        APPEND ls_color TO ls_alv_real-color.
      ENDIF.

      IF <ls_pir_details>-deact = 'X'.
      ELSE.
        CLEAR: ls_color.
        MOVE: 'YEAR' TO ls_color-fname,
              space   TO ls_color-nokeycol,
              lc_color_struct_prime_red
                      TO ls_color-color.
        APPEND ls_color TO ls_alv_real-color.

        MOVE: 'E_MESS' TO ls_color-fname.
        APPEND ls_color TO ls_alv_real-color.

        DO 12 TIMES. "12 months
          "-- build fieldname/ (month fieldname)
          CLEAR: lv_fieldname.
          MOVE sy-index TO lv_numc2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_numc2
            IMPORTING
              output = lv_numc2.
          CONCATENATE 'M' lv_numc2 INTO lv_fieldname.

          CLEAR: ls_color.
          CONCATENATE lv_fieldname '-VALUE' INTO ls_color-fname.
          ls_color-nokeycol = space.

          "-- assign COLOR for each cell depending on the rate.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_pir_details> TO <ls_month>.
          ls_color-color = lc_color_struct_prime_red.
          APPEND ls_color TO ls_alv_real-color.
        ENDDO.
      ENDIF.

      APPEND ls_alv_real TO ct_alv_real.
      CONTINUE.
    ENDIF.

    DO 12 TIMES. "12 months

      "-- build fieldname/ (month fieldname)
      CLEAR: lv_fieldname.
      MOVE sy-index TO lv_numc2.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_numc2
        IMPORTING
          output = lv_numc2.
      CONCATENATE 'M' lv_numc2 INTO lv_fieldname.

***QUICK FIX**********************************************************
      CLEAR: lv_numc6.
      lv_numc6+0(4) = <ls_pir_details>-year.
      lv_numc6+4(2) = lv_numc2.
      CHECK lv_numc6 IN gr_report_date.
***QUICK FIX**********************************************************

      CLEAR: ls_color.
      CONCATENATE lv_fieldname '-VALUE' INTO ls_color-fname.
      ls_color-nokeycol = space.

      "-- assign COLOR for each cell depending on the rate.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_pir_details> TO <ls_month>.
      IF <ls_month>-value <> 0.
        IF <ls_month>-rate = '-1'.  "New Item.
          ls_color-color = lc_color_struct_prime_green.
        ELSEIF <ls_month>-rate > '0.2'. "Change is greater than +20%.
          ls_color-color = lc_color_struct_prime. "yellow
        ELSEIF <ls_month>-rate < '-0.2'. "Change is greate than -20%.
*          ls_color-color = lc_color_struct_prime_lblue. "change to green.
          ls_color-color = lc_color_struct_prime.
        ELSE.
          "no color. change is below 20% or none at all..
          CLEAR: ls_color-color.
        ENDIF.
        APPEND ls_color TO ls_alv_real-color.
      ENDIF.
    ENDDO.

    APPEND ls_alv_real TO ct_alv_real.
  ENDLOOP.


ENDFORM.                    "color_format_add


*&---------------------------------------------------------------------*
*&      Form  display_logs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_LOGS      text
*      -->CT_LOG_FCAT  text
*----------------------------------------------------------------------*
FORM display_logs  USING    value(ut_logs)     TYPE tt_logs
                   CHANGING ct_log_fcat        TYPE lvc_t_fcat.

  DATA: lv_asdfal TYPE char01.

  DATA: lobj_custom_container TYPE REF TO cl_gui_custom_container,
        lobj_alv_grid         TYPE REF TO cl_gui_alv_grid.

  DATA: ls_log_fcat LIKE LINE OF ct_log_fcat.

  DATA: lv_ctr      TYPE numc05 VALUE '0'.

  DEFINE add_to_fcat.
    clear: ls_log_fcat.

    add 1 to lv_ctr.
    ls_log_fcat-col_pos   = lv_ctr.

    ls_log_fcat-fieldname   =    &1.
    ls_log_fcat-outputlen   =    &2.
    ls_log_fcat-scrtext_l   =    &3.
    ls_log_fcat-scrtext_s   =    &4.
    ls_log_fcat-no_zero     =    &5.
    ls_log_fcat-emphasize   =    &6.

    append ls_log_fcat to ct_log_fcat.
  END-OF-DEFINITION. "add_to_fcat


  "-- build field cat.
  add_to_fcat: "'LOG_NO'    '05'     '#'              '#'              space   space,
               'MATNR'     '15'     'Material'       'Material'       'X'     space,
               'BERID'     '08'     'MRP Area'       'MRP Area'       space   space,
               'VRSN'      '08'     'Version'        'Version'        space   space,
               'TYPE'      '08'     'Message Type'   'Msg. Type'      space   space,
               'MESSAGE'   '70'     'Message'        'Message'        space   space.


  "-- build other ALV options.


  "-- show alv.
  CREATE OBJECT lobj_custom_container
    EXPORTING
      container_name = 'LOGS_PANEL'.

  CREATE OBJECT lobj_alv_grid
    EXPORTING
      i_parent = lobj_custom_container.

  CALL METHOD lobj_alv_grid->set_table_for_first_display
    CHANGING
      it_outtab       = ut_logs
      it_fieldcatalog = ct_log_fcat.

  CALL SCREEN '0101'.

ENDFORM.                    "display_logs


*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ZLOGS_STATUS'.
  SET TITLEBAR  'ZTITLE_LOGS'.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  DATA: lv_char255 TYPE char255.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
*      SET SCREEN '1000'.
*      LEAVE TO SCREEN 0.
      IF gv_istestmode = 'X'.
        SET PARAMETER ID gc_sy_status FIELD 'SPACE'.
        LEAVE TO SCREEN 0.
      ELSE.
        MOVE: 'Posting success!' TO lv_char255.
        SET PARAMETER ID gc_sy_status FIELD lv_char255.
*        EXPORT  lv_char255 TO MEMORY ID gc_sy_status.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0101  INPUT

*&---------------------------------------------------------------------*
*&      Form  build_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_alv_header.

*ALV Header declarations
  DATA: t_header TYPE slis_t_listheader WITH HEADER LINE.

  t_header-typ  = 'S'.
  t_header-key  = 'Key'.
  t_header-info = 'Info'.

  APPEND t_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header[].

ENDFORM.                    "TOP-OF-PAGE