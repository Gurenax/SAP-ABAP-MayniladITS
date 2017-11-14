*&---------------------------------------------------------------------*
*&  Include           ZR_PM_PIR_UPLOAD_V4_CARRY
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  post_main
*&---------------------------------------------------------------------*
*       The star of the show,
*       Do the dirty works here, USE BAPI and stuff.
*       Edit, Create, Update PIRs in this routine.
*       Do not forget to LOG! for display later.
*----------------------------------------------------------------------*
*      -->(UT_REQUIREMENTS)  Summary of the upload FILE
*      -->UT_EXIST_PBIM      List of existing PIRs
*      -->UT_EXIST_PBED      Further details of the existing PIRs above^
*      -->UV_ISTESTMODE      This decides the "COMMITMENT"
*      -->CT_LOGS            Reflect your logs here, success of failure, must contain em here.
*----------------------------------------------------------------------*
FORM post_main USING    ut_exist_pbim          TYPE tt_pbim_ex
                        ut_exist_pbed          TYPE tt_pbed_ex
                        uv_istestmode          TYPE char01
               CHANGING ut_requirements        TYPE tt_alv_real
                        ct_logs                TYPE tt_logs.

  DATA: ls_logs LIKE LINE OF ct_logs,
        lt_logs LIKE STANDARD TABLE OF ls_logs.

  DATA: lv_bapi_status TYPE char01. "Ind for bapi run status. S-> Success, E->Error.

  DATA: lv_deactivate TYPE char01,
        lv_create     TYPE char01,
        lv_change     TYPE char01.

  DATA: lv_succ_ctr   TYPE numc5.

  "-- Start - GDIMALIWAT 07/21/2014 - TN#63434
  DATA: lv_matnr       TYPE matnr,
        lv_special_v00 TYPE c LENGTH 1. "-- Flag to determine that we are uploading a V00 with an existing V01
  "-- End - GDIMALIWAT 07/21/2014 - TN#63434

  FIELD-SYMBOLS: <ls_requirement> LIKE LINE OF ut_requirements,
                 <ls_pbed>        LIKE LINE OF ut_exist_pbed.


**********
* MAIN LOOP
**********
  SORT ut_requirements BY iserror
                          matnr
                          berid
                          vrsn ASCENDING
                          deact.
  LOOP AT ut_requirements ASSIGNING <ls_requirement>
  WHERE iserror IS INITIAL.

* >> 12172013
  CLEAR: lv_bapi_status.
* << 12172013

* >> 01212014
  DATA: lv_yearxxx TYPE numc4.

  lv_yearxxx = p_rdate+0(4) + 1.
  IF <ls_requirement>-year <> lv_yearxxx
  AND <ls_requirement>-vrsn = '00'.
    "-- Start - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing V00
    SELECT SINGLE matnr
    INTO lv_matnr
    FROM zind_req
    WHERE matnr eq <ls_requirement>-MATNR
      AND werks eq <ls_requirement>-BERID
      AND versb eq '00'.
    IF sy-subrc eq 0.
      CONTINUE.
    ELSE.
      "-- Proceed
      lv_special_v00 = 'X'.
    ENDIF.
    "CONTINUE.
    "-- End - GDIMALIWAT 07/21/2014 - TN#63434 - Check if there is an existing V00
  ELSE.

  ENDIF.
* << 01212014

*& >>> Change in logic, OPACALSO, 11192013,
*    "-- check upload version, then decide for right actions to perform..
*    CLEAR: lv_deactivate, lv_create, lv_change.
*    CASE <ls_requirement>-vrsn. "version of upload file.
*      WHEN '00'.
*        CASE <ls_requirement>-act_vsn.
*          WHEN '00'.                            "version 00 with active_version 00.
*            MOVE 'X' TO lv_change.
*          WHEN '01'.                            "version 00 with active_version 01.
*            "zxzx should not happen? ask functional.
*            "Annual plan is already final?? Not allow to change? or still allow change?zxzx
*          WHEN space.                           "version 00 with NO active version.
*            "MOVE 'X' TO lv_create.
*            "What to do when no active version found?zxzx
*          WHEN OTHERS.
*            "other versions not supported.
*        ENDCASE.
*      WHEN '01'.
*        CASE <ls_requirement>-act_vsn.
*          WHEN '00'.
*            MOVE: 'X' TO lv_deactivate,
*                  'X' TO lv_create.
*          WHEN '01'.
*            MOVE: 'X' TO lv_change.
*          WHEN space.
*            "MOVE: 'X' TO lv_create.
*            "zxzx what to do when no active version
*          WHEN OTHERS.
*            "other versions not supported.
*        ENDCASE.
*      WHEN OTHERS.
*        "--no support for other versions.
*    ENDCASE.
*& ^^^<<< End of Change in logic, OPACALSO, 11192013.

    "-- Start - GDIMALIWAT 07/21/2014 - TN#63434
    IF lv_special_v00 eq 'X'.
      PERFORM change_requirement_deactivate     USING    <ls_requirement>
                                                     uv_istestmode
                                            CHANGING ct_logs
                                                     lv_bapi_status.
    ELSE.
      DATA: lv_isdo_deact TYPE char01.

      CASE <ls_requirement>-deact.
        WHEN 'X'.
          CLEAR: lv_isdo_deact.
          PERFORM really_wanna_deact  USING    <ls_requirement>
                                      CHANGING lv_deactivate.
          CLEAR: lv_create, lv_change.

        WHEN OTHERS.
          CLEAR: lv_deactivate.
          CASE <ls_requirement>-isnew.
            WHEN 'X'.
              READ TABLE ut_exist_pbim TRANSPORTING NO FIELDS
              WITH KEY matnr = <ls_requirement>-matnr
                       werks = <ls_requirement>-berid
                       versb = <ls_requirement>-vrsn.
              IF sy-subrc IS INITIAL.
                lv_create = space.
                lv_change = 'X'.
              ELSE.
                lv_create = 'X'.
                lv_change = space.
              ENDIF.

            WHEN space.
              lv_create = space.
              lv_change = 'X'.
          ENDCASE.
      ENDCASE.

      "-- execute decision from above process.
      IF lv_deactivate = 'X'.
        PERFORM deactivate_v_zero USING    <ls_requirement>
                                           uv_istestmode
                                  CHANGING ct_logs
                                           lv_bapi_status.
      ENDIF.

      IF lv_change     = 'X'.
        PERFORM change_requirement USING    <ls_requirement>
                                            uv_istestmode
                                   CHANGING ct_logs
                                            lv_bapi_status.
      ENDIF.

      IF lv_create     = 'X'.
        PERFORM create_new_req     USING    <ls_requirement>
                                            uv_istestmode
                                   CHANGING ct_logs
                                            lv_bapi_status.
      ENDIF.
    ENDIF.
*    DATA: lv_isdo_deact TYPE char01.
*
*    CASE <ls_requirement>-deact.
*      WHEN 'X'.
*        CLEAR: lv_isdo_deact.
*        PERFORM really_wanna_deact  USING    <ls_requirement>
*                                    CHANGING lv_deactivate.
*        CLEAR: lv_create, lv_change.
*
*      WHEN OTHERS.
*        CLEAR: lv_deactivate.
*        CASE <ls_requirement>-isnew.
*          WHEN 'X'.
*            READ TABLE ut_exist_pbim TRANSPORTING NO FIELDS
*            WITH KEY matnr = <ls_requirement>-matnr
*                     werks = <ls_requirement>-berid
*                     versb = <ls_requirement>-vrsn.
*            IF sy-subrc IS INITIAL.
*              lv_create = space.
*              lv_change = 'X'.
*            ELSE.
*              lv_create = 'X'.
*              lv_change = space.
*            ENDIF.
*
*          WHEN space.
*            lv_create = space.
*            lv_change = 'X'.
*        ENDCASE.
*    ENDCASE.
*
*    "-- execute decision from above process.
*    IF lv_deactivate = 'X'.
*      PERFORM deactivate_v_zero USING    <ls_requirement>
*                                         uv_istestmode
*                                CHANGING ct_logs
*                                         lv_bapi_status.
*    ENDIF.
*
*    IF lv_change     = 'X'.
*      PERFORM change_requirement USING    <ls_requirement>
*                                          uv_istestmode
*                                 CHANGING ct_logs
*                                          lv_bapi_status.
*    ENDIF.
*
*    IF lv_create     = 'X'.
*      PERFORM create_new_req     USING    <ls_requirement>
*                                          uv_istestmode
*                                 CHANGING ct_logs
*                                          lv_bapi_status.
*    ENDIF.
    "-- End - GDIMALIWAT 07/21/2014 - TN#63434

    "-- summary
    <ls_requirement>-posting_status = lv_bapi_status.

    CASE lv_bapi_status.
      WHEN 'S'.
        IF lv_deactivate = 'X'.
        ELSE.
          ADD <ls_requirement>-upload_ctr TO lv_succ_ctr.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.
* ^^^^ END OF MAIN LOOP
**********

  DATA: lv_message TYPE char70.

  MOVE:   '&1 lines successfuly posted.' TO lv_message.
  REPLACE '&1' IN lv_message WITH lv_succ_ctr.

  IF uv_istestmode IS INITIAL.
    MESSAGE lv_message TYPE 'S'.
  ELSE.
    MESSAGE 'No lines posted, program is on test mode.' TYPE 'S'.
  ENDIF.

ENDFORM.                    "post_main



*&---------------------------------------------------------------------*
*&      Form  deactivate_v_zero
*&---------------------------------------------------------------------*
*       The title says it all.
*       Use BAPI_REQUIREMENTS_CHANGE to deactivate.
*----------------------------------------------------------------------*
*      -->US_REQUIREMENT  Structure of the requirement to be deactivated
*      -->UV_ISTESTMODE   test or not u decide.
*      -->CT_LOGS         LOGS, meh.
*      -->CV_STATUS       i doubt this will be of use, we'll see latur
*----------------------------------------------------------------------*
FORM deactivate_v_zero USING    us_requirement TYPE ty_alv_real
                                uv_istestmode  TYPE char01
                       CHANGING ct_logs        TYPE tt_logs
                                cv_status      TYPE char01.

  DATA: lv_do_commitupdate TYPE char01.

  DATA: ls_logs LIKE LINE OF ct_logs.

  DATA: ls_req_sched TYPE bapisshdin,
        lt_req_sched LIKE STANDARD TABLE OF ls_req_sched.

  DATA: ls_item_ret  TYPE bapisitemr.

  DATA: lt_return    TYPE STANDARD TABLE OF bapireturn1.

  FIELD-SYMBOLS: <ls_return>  LIKE LINE OF lt_return.


  "-- determine whether execution is test mode
  IF uv_istestmode IS INITIAL.
    MOVE 'X' TO lv_do_commitupdate.
  ELSE.
    CLEAR:      lv_do_commitupdate.
  ENDIF.

  "-- build requirement schedules.
  PERFORM req_schedules_build_v2 USING      us_requirement
                                            'X'         "get existing only, do not merge with upload.
                                            space       "inc bapi.
                                 CHANGING   lt_req_sched
                                            ct_logs
                                            cv_status.

  CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
    EXPORTING
      material                 = us_requirement-matnr
      plant                    = us_requirement-berid
      requirementstype         = space
      version                  = '00'
      reqmtsplannumber         = space
      vers_activ               = space
      do_commit                = lv_do_commitupdate
      update_mode              = lv_do_commitupdate
      delete_old               = space
    IMPORTING
      requirement_item_out     = ls_item_ret
    TABLES
      requirements_schedule_in = lt_req_sched
      return                   = lt_return.


  PERFORM create_logging USING    us_requirement
                                  lt_return
                                  'Deactivation success!'
                         CHANGING ct_logs
                                  cv_status.

ENDFORM.                    "deactivate_v_zero

*&---------------------------------------------------------------------*
*&      Form  create_new_req
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_VERSION      text
*      -->US_REQUIREMENT  text
*----------------------------------------------------------------------*
FORM create_new_req    USING    us_requirement TYPE ty_alv_real
                                uv_istestmode  TYPE char01
                       CHANGING ct_logs TYPE tt_logs
                                cv_status.

  DATA: lv_do_commitupdate TYPE char01.

  DATA: ls_requirements_item TYPE bapisitemr.

  DATA: ls_sched_in TYPE bapisshdin,
        lt_sched_in LIKE STANDARD TABLE OF ls_sched_in.

  DATA: ls_ret TYPE bapireturn1,
        lt_ret LIKE STANDARD TABLE OF ls_ret.


  "-- build requirements_item details for bapi.
  CLEAR: ls_requirements_item.
  ls_requirements_item-material           =     us_requirement-matnr  .
  ls_requirements_item-plant              =     us_requirement-berid  .
  ls_requirements_item-version            =     us_requirement-vrsn   .
  ls_requirements_item-vers_activ         =     'X'                   .
  ls_requirements_item-mrp_area           =     us_requirement-berid  .

  "-- determine whether execution is test mode
  IF uv_istestmode IS INITIAL.
    MOVE 'X' TO lv_do_commitupdate.
  ELSE.
    CLEAR:      lv_do_commitupdate.
  ENDIF.

  "-- build requirement schedules.
  PERFORM req_schedules_build_v2 USING      us_requirement
                                            space
                                            'X'
                                 CHANGING   lt_sched_in
                                            ct_logs
                                            cv_status.

  CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
    EXPORTING
      requirements_item        = ls_requirements_item
      do_commit                = lv_do_commitupdate
      update_mode              = lv_do_commitupdate
    TABLES
      requirements_schedule_in = lt_sched_in
      return                   = lt_ret.


  PERFORM create_logging USING    us_requirement
                                  lt_ret
                                  'Requirements has been created successfuly.'
                         CHANGING ct_logs
                                  cv_status.

ENDFORM.                    "create_new_req

"-- Start - GDIMALIWAT 07/21/2014 - TN#63434
**&---------------------------------------------------------------------*
**&      Form  create_new_req_deactivate
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->UV_VERSION      text
**      -->US_REQUIREMENT  text
**----------------------------------------------------------------------*
*FORM create_new_req_deactivate    USING    us_requirement TYPE ty_alv_real
*                                           uv_istestmode  TYPE char01
*                                  CHANGING ct_logs TYPE tt_logs
*                                           cv_status.
*
*  DATA: lv_do_commitupdate TYPE char01.
*
*  DATA: ls_requirements_item TYPE bapisitemr.
*
*  DATA: ls_sched_in TYPE bapisshdin,
*        lt_sched_in LIKE STANDARD TABLE OF ls_sched_in.
*
*  DATA: ls_ret TYPE bapireturn1,
*        lt_ret LIKE STANDARD TABLE OF ls_ret.
*
*
*  "-- build requirements_item details for bapi.
*  CLEAR: ls_requirements_item.
*  ls_requirements_item-material           =     us_requirement-matnr  .
*  ls_requirements_item-plant              =     us_requirement-berid  .
*  ls_requirements_item-version            =     us_requirement-vrsn   .
*  ls_requirements_item-vers_activ         =     ''                   .
*  ls_requirements_item-mrp_area           =     us_requirement-berid  .
*
*  "-- determine whether execution is test mode
*  IF uv_istestmode IS INITIAL.
*    MOVE 'X' TO lv_do_commitupdate.
*  ELSE.
*    CLEAR:      lv_do_commitupdate.
*  ENDIF.
*
*  "-- build requirement schedules.
*  PERFORM req_schedules_build_v2 USING      us_requirement
*                                            space
*                                            'X'
*                                 CHANGING   lt_sched_in
*                                            ct_logs
*                                            cv_status.
*
*  CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
*    EXPORTING
*      requirements_item        = ls_requirements_item
*      do_commit                = lv_do_commitupdate
*      update_mode              = lv_do_commitupdate
*    TABLES
*      requirements_schedule_in = lt_sched_in
*      return                   = lt_ret.
*
*
*  PERFORM create_logging USING    us_requirement
*                                  lt_ret
*                                  'Requirements has been created successfuly.'
*                         CHANGING ct_logs
*                                  cv_status.
*
*ENDFORM.                    "create_new_req_deactivate
*&---------------------------------------------------------------------*
*&      Form  change_requirement_deactivate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_VERSION      text
*      -->US_REQUIREMENT  text
*----------------------------------------------------------------------*
FORM change_requirement_deactivate USING    us_requirement TYPE ty_alv_real
                                            uv_istestmode  TYPE char01
                                   CHANGING ct_logs        TYPE tt_logs
                                            cv_status      TYPE char01.

  DATA: lv_do_commitupdate TYPE char01.

  DATA: ls_req_out TYPE bapisitemr,
        lt_req_out LIKE STANDARD TABLE OF ls_req_out.

  DATA: ls_sched_in TYPE bapisshdin,
        lt_sched_in LIKE STANDARD TABLE OF ls_sched_in.

  DATA: lt_bapiret TYPE STANDARD TABLE OF bapireturn1.

  DATA: lv_isactive TYPE char01.


  "-- check if version active  zxzx test
*  CASE us_requirement-act_vsn.
*    WHEN us_requirement-vrsn.
*      MOVE 'X' TO lv_isactive.
*    WHEN OTHERS.
*      CLEAR lv_isactive.
*  ENDCASE.
  lv_isactive = ''.


  "-- determine whether execution is test mode
  IF uv_istestmode IS INITIAL.
    MOVE 'X' TO lv_do_commitupdate.
  ELSE.
    CLEAR: lv_do_commitupdate.
  ENDIF.


  "-- build requirement schedules.
  PERFORM req_schedules_build_v2 USING      us_requirement
                                            'X'
                                            'X'
                                 CHANGING   lt_sched_in
                                            ct_logs
                                            cv_status.

  PERFORM req_schedules_no_delete_add USING us_requirement
                                      CHANGING lt_sched_in.

  CHECK NOT cv_status = 'E'.


  CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
    EXPORTING
      material                 = us_requirement-matnr
      plant                    = us_requirement-berid
      requirementstype         = space
      version                  = us_requirement-vrsn
      reqmtsplannumber         = space
      vers_activ               = lv_isactive
      do_commit                = lv_do_commitupdate
      update_mode              = lv_do_commitupdate
*     delete_old               = space
      delete_old               = 'X'   "12052013 OPacalso/KCAO
    IMPORTING
      requirement_item_out     = ls_req_out
    TABLES
      requirements_schedule_in = lt_sched_in
      return                   = lt_bapiret.

  PERFORM create_logging USING    us_requirement
                                  lt_bapiret
                                  'Requirement change success.'
                         CHANGING ct_logs
                                  cv_status.

ENDFORM.                    "change_requirement_deactivate
"-- End - GDIMALIWAT 07/21/2014 - TN#63434

*&---------------------------------------------------------------------*
*&      Form  change_requirement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_VERSION      text
*      -->US_REQUIREMENT  text
*----------------------------------------------------------------------*
FORM change_requirement USING    us_requirement TYPE ty_alv_real
                                 uv_istestmode  TYPE char01
                        CHANGING ct_logs        TYPE tt_logs
                                 cv_status      TYPE char01.

  DATA: lv_do_commitupdate TYPE char01.

  DATA: ls_req_out TYPE bapisitemr,
        lt_req_out LIKE STANDARD TABLE OF ls_req_out.

  DATA: ls_sched_in TYPE bapisshdin,
        lt_sched_in LIKE STANDARD TABLE OF ls_sched_in.

  DATA: lt_bapiret TYPE STANDARD TABLE OF bapireturn1.

  DATA: lv_isactive TYPE char01.


  "-- check if version active  zxzx test
*  CASE us_requirement-act_vsn.
*    WHEN us_requirement-vrsn.
*      MOVE 'X' TO lv_isactive.
*    WHEN OTHERS.
*      CLEAR lv_isactive.
*  ENDCASE.
  lv_isactive = 'X'.


  "-- determine whether execution is test mode
  IF uv_istestmode IS INITIAL.
    MOVE 'X' TO lv_do_commitupdate.
  ELSE.
    CLEAR: lv_do_commitupdate.
  ENDIF.


  "-- build requirement schedules.
  PERFORM req_schedules_build_v2 USING      us_requirement
                                            'X'
                                            'X'
                                 CHANGING   lt_sched_in
                                            ct_logs
                                            cv_status.

  PERFORM req_schedules_no_delete_add USING us_requirement
                                      CHANGING lt_sched_in.

  CHECK NOT cv_status = 'E'.


  CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
    EXPORTING
      material                 = us_requirement-matnr
      plant                    = us_requirement-berid
      requirementstype         = space
      version                  = us_requirement-vrsn
      reqmtsplannumber         = space
      vers_activ               = lv_isactive
      do_commit                = lv_do_commitupdate
      update_mode              = lv_do_commitupdate
*     delete_old               = space
      delete_old               = 'X'   "12052013 OPacalso/KCAO
    IMPORTING
      requirement_item_out     = ls_req_out
    TABLES
      requirements_schedule_in = lt_sched_in
      return                   = lt_bapiret.

  PERFORM create_logging USING    us_requirement
                                  lt_bapiret
                                  'Requirement change success.'
                         CHANGING ct_logs
                                  cv_status.

ENDFORM.                    "change_requirement


*&---------------------------------------------------------------------*
*&      Form  create_logging
*&---------------------------------------------------------------------*
*       I am the link of internal process and your user.
*       I show them whats done and what not.
*       Please maintain me and keep me as descriptive as possible.
*       SankYOU!
*----------------------------------------------------------------------*
*      -->US_REQUIREMENT  text
*      -->UT_BAPIRET      text
*      -->UV_S_MESS       text
*      -->CT_LOGS         text
*      -->CV_STATUS       text
*----------------------------------------------------------------------*
FORM create_logging USING    us_requirement TYPE ty_alv_real
                             ut_bapiret     TYPE tt_bapiret1
                             uv_s_mess      TYPE char255
                    CHANGING ct_logs        TYPE tt_logs
                             cv_status      TYPE char01.

  DATA: ls_logs   LIKE LINE OF ct_logs.
  FIELD-SYMBOLS: <ls_return> LIKE LINE OF ut_bapiret.

  "-- create loggings.
  CLEAR: ls_logs, cv_status.
  MOVE-CORRESPONDING us_requirement TO ls_logs.
  ls_logs-log_no    =     us_requirement-seq.

  IF ut_bapiret IS INITIAL.
    ls_logs-type      =     'S'.
    ls_logs-message   =     uv_s_mess.
    APPEND ls_logs TO ct_logs.

    cv_status = 'S'.
  ELSE.
    LOOP AT ut_bapiret ASSIGNING <ls_return>.
      ls_logs-type    =   <ls_return>-type.
      ls_logs-message =   <ls_return>-message.

      APPEND ls_logs TO ct_logs.
    ENDLOOP.

    cv_status = 'E'.
  ENDIF.

ENDFORM.                    "create_logging

*&---------------------------------------------------------------------*
*&      Form  req_schedules_build_v2
*&---------------------------------------------------------------------*
*       The title says it all, again.
*       Use BAPI_REQUIREMENTS_GETDETAIL to retrieve the existing details.
*       A worth to note in this routine is the use of PARAMETER UV_GETEXIST_ONLY (See comment Bellow)
*----------------------------------------------------------------------*
*      -->US_REQUIREMENT    The requirement to be processed (PIR)
*      -->UV_GETEXIST_ONLY  Flag this to get only whats already existing, disregarding the UPLOAD FILE,
*                              > pass blank value otherwise (if u want to retrieve the schedule reflecting the upload file)
*      -->CT_SCHEDULES      I Contain the final output of the routine! THE SCHEDULES!
*      -->CT_LOGS           Logs, logs, logs, i see your progress.
*      -->CV_STATUS         idk if thisll be useful later.
*----------------------------------------------------------------------*
FORM req_schedules_build_v2 USING    us_requirement   TYPE      ty_alv_real
                                     uv_isinc_exist   TYPE char01
                                     uv_isinc_upload  TYPE char01
                            CHANGING ct_schedules     TYPE      tt_bapi_bapisshdin
                                     ct_logs
                                     cv_status.

  DATA: ls_req_sched LIKE LINE OF ct_schedules,
        lt_req_sched_final LIKE HASHED TABLE OF ls_req_sched
          WITH UNIQUE KEY date_type
                          req_date
*                          REQ_QTY
                          unit
                          bomexpl
                          prod_ves
          .

  DATA: ls_gd_item_out  TYPE bapisitmeo,
        lt_gd_item_out  LIKE STANDARD TABLE OF ls_gd_item_out.

  DATA: lt_gd_ret    TYPE STANDARD TABLE OF bapireturn1.

  DATA: lv_fieldname TYPE char20,
        lv_numc2     TYPE numc2.

  FIELD-SYMBOLS: <ls_gd_iout> LIKE LINE OF lt_gd_item_out.
  FIELD-SYMBOLS: <ls_month> LIKE us_requirement-m01.
  FIELD-SYMBOLS: <ls_sched> LIKE LINE OF ct_schedules.


  "-- build the sched line -> transpose to be compatible with whats being asked -> ct_schedules.
  CLEAR: ct_schedules.
  CLEAR: lt_gd_item_out.


  IF uv_isinc_upload = 'X'.
    DO 12 TIMES.
      "-- build fieldname to retrieve upload if has qty later.
      CLEAR: lv_fieldname.
      MOVE sy-index TO lv_numc2.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_numc2
        IMPORTING
          output = lv_numc2.
      CONCATENATE 'M' lv_numc2 INTO lv_fieldname.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE us_requirement TO <ls_month>.


      CONCATENATE us_requirement-year lv_numc2 '01' INTO ls_req_sched-req_date.
      ls_req_sched-date_type   =  3.  "3 for month...
      ls_req_sched-req_qty     =  <ls_month>-value.
      APPEND ls_req_sched TO ct_schedules.
    ENDDO.
  ELSE.

  ENDIF.


  IF uv_isinc_exist = 'X'.
    "-- fill sched table.
    CLEAR: lt_gd_ret.
    CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
      EXPORTING
        material         = us_requirement-matnr
        plant            = us_requirement-berid
        requirementstype = space
        version          = us_requirement-vrsn
        reqmtsplannumber = space
      TABLES
        requirements_out = lt_gd_item_out
        return           = lt_gd_ret.
    IF NOT lt_gd_ret IS INITIAL.
      PERFORM create_logging  USING us_requirement
                                    lt_gd_ret
                                    'Failed to retrieve requirement details!'
                           CHANGING ct_logs
                                    cv_status.
    ELSE.
      ...
    ENDIF.

    IF lt_gd_item_out IS INITIAL.
      ...
    ELSE.
      LOOP AT lt_gd_item_out ASSIGNING <ls_gd_iout>.
        CHECK <ls_gd_iout>-req_date+0(4) = us_requirement-year.
        CLEAR: ls_req_sched.
        ls_req_sched-date_type   =  3.  "3 for month...
        ls_req_sched-req_date    =  <ls_gd_iout>-req_date.
        ls_req_sched-req_qty     =  <ls_gd_iout>-req_qty.

        READ TABLE ct_schedules TRANSPORTING NO FIELDS
        WITH KEY req_date+0(6) = ls_req_sched-req_date+0(6).
        IF sy-subrc IS INITIAL.
          "do not insert
        ELSE.
          APPEND ls_req_sched TO ct_schedules.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    ...
  ENDIF.

*  DELETE ADJACENT DUPLICATES FROM ct_schedules COMPARING req_date.

  "-- consolidate.
  LOOP AT ct_schedules ASSIGNING <ls_sched>.
    COLLECT <ls_sched> INTO lt_req_sched_final.
  ENDLOOP.


  MOVE: lt_req_sched_final TO ct_schedules.


ENDFORM.                    "req_schedules_build_v2



*&---------------------------------------------------------------------*
*&      Form  REQ_SCHEDULES_NO_DELETE_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_US_REQUIREMENT  text
*      <--P_LT_SCHED_IN  text
*----------------------------------------------------------------------*
FORM req_schedules_no_delete_add  USING    us_requirement TYPE ty_alv_real
                                  CHANGING ct_sched_in    TYPE tt_bapi_bapisshdin.

  DATA: ls_sched_in LIKE LINE OF ct_sched_in
        "lt_sched_in LIKE STANDARD TABLE OF ls_sched_in
        .

  DATA: ls_sched_out TYPE bapisitmeo,
        lt_sched_out LIKE STANDARD TABLE OF ls_sched_out.

  DATA: lt_gd_ret   TYPE STANDARD TABLE OF bapireturn1.

  DATA: lr_datum      TYPE RANGE OF sy-datum,
        ls_datum      LIKE LINE OF lr_datum.

  FIELD-SYMBOLS: <ls_reptdate> LIKE LINE OF gr_report_date.

  FIELD-SYMBOLS: <ls_sched> LIKE LINE OF lt_sched_out.

* '> DELETE OPacalso/KCao - 12172013 - remove as of usapan sa old IT room.
* >> ADD OPacalso/KCao - 12172013 - no longer preserve old data. let it DIE! erase!.
*  CHECK us_requirement-vrsn = '00'.
*  CHECK p_rdate+4(2) NE 12.
* << ADD OPacalso/KCao - 12172013.
* '< DELETE OPacalso/KCao - 12172013 - remove as of usapan sa old IT room.

  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      material         = us_requirement-matnr
      plant            = us_requirement-berid
      requirementstype = space
      version          = us_requirement-vrsn
      reqmtsplannumber = space
    TABLES
      requirements_out = lt_sched_out
      return           = lt_gd_ret.

  CHECK lt_gd_ret IS INITIAL.


  CLEAR: lr_datum.
  CASE us_requirement-vrsn.
*    WHEN '01'
*      .
*      LOOP AT gr_report_date ASSIGNING <ls_reptdate>.
*        CLEAR: ls_datum.
*        MOVE-CORRESPONDING: <ls_reptdate> TO ls_datum.
*
*        ls_datum-low+6(2)    = '01'.
*        IF ls_datum-high EQ '000000' OR ls_datum-high IS INITIAL OR ls_datum-high EQ '00000000'.
*        ELSE.
*          ls_datum-high+6(2) = '31'.
*        ENDIF.
*
*        APPEND ls_datum TO lr_datum.
*      ENDLOOP.
    WHEN '00'
      OR '01'
      .
      CLEAR: ls_datum.
      READ TABLE gr_report_date ASSIGNING <ls_reptdate> INDEX 1.
      MOVE-CORRESPONDING <ls_reptdate> TO ls_datum.
      ls_datum-low+0(4)  = <ls_reptdate>-low+0(4).
      ls_datum-low+4(4)  = '0101'.
      ls_datum-high+0(4) = <ls_reptdate>-high+0(4).
      ls_datum-high+4(4) = '1231'.
      APPEND ls_datum TO lr_datum.
  ENDCASE.

  LOOP AT lt_sched_out ASSIGNING <ls_sched>.

* >> ADD OPACALSO 12172013, weird case in prod, line item with 00000000 date is returned.
    CHECK <ls_sched>-req_date <> '00000000'.
    CHECK <ls_sched>-req_date <> space.
* << ADD OPacalso 12172013

    CLEAR: ls_sched_in.

****QUICK FIX*********************************************************
    IF <ls_sched>-req_date IN lr_datum.
      ...
    ELSE.

      ls_sched_in-date_type   =  3.  "3 for month...
      ls_sched_in-req_date    =  <ls_sched>-req_date.
      ls_sched_in-req_qty     =  <ls_sched>-req_qty.

      APPEND ls_sched_in TO ct_sched_in.
    ENDIF.

  ENDLOOP.




ENDFORM.                    " REQ_SCHEDULES_NO_DELETE_ADD


FORM pre_posting_tasks  USING    ut_alv LIKE gt_alv_real.



ENDFORM.