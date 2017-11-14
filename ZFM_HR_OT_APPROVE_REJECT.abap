FUNCTION zfm_hr_ot_approve_reject.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_APPROVER_USER) TYPE  SY-UNAME OPTIONAL
*"     VALUE(I_REQUESTOR) TYPE  PERNR_D OPTIONAL
*"     VALUE(I_WIID) TYPE  SWW_WIID
*"  TABLES
*"      GT_OT TYPE  ZTY_HR_OTAPPROVE_REJECT
*"      GT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  TYPES:  BEGIN OF ty_dates,
          date                      TYPE sy-datum,
          hours                     TYPE i,
        END OF ty_dates.

  DATA: lv_objectkey                  TYPE swr_struct-object_key,
        lv_subrc                      TYPE sy-subrc,
        lv_hours(4)                   TYPE p DECIMALS 2,
        lv_total                      TYPE i,
        lv_hours2(4)                  TYPE p DECIMALS 2,
        lv_nxtday                     TYPE c,
        lv_event_id                   TYPE swr_struct-event_id,
        lc_tmp(2)                     TYPE c,
        lc_tmp2(2)                    TYPE c,
        lv_tmp(4)                     TYPE p DECIMALS 2,
        lv_tmp2(4)                    TYPE p DECIMALS 2,
        lv_tmp3(4)                    TYPE p DECIMALS 2,
        lv_timein                     TYPE uzeit,
        lv_deltamin                   TYPE mcwmit-be_ae,
        lv_dhours(6)                  TYPE c,
        lv_timeout                    TYPE uzeit,
        lv_exit                       TYPE c,
        lv_error                      TYPE c,
        lv_nonworking                 TYPE c,
        lv_reqmins                    TYPE i,
        lv_reqmins2                   TYPE i,
        lv_reqmins3                   TYPE i,
        lv_aprmins                    TYPE i,
        lv_tmphours(4)                TYPE p DECIMALS 2,
        lv_tmptime(4)                 TYPE p DECIMALS 2,
        lv_pndmins                    TYPE i,
        lv_tmpchar(2)                 TYPE c,
        lv_tmpchar2(2)                TYPE c,
        lv_tmphours2(5)               TYPE c,
        lv_ctrind                     TYPE i,
        ls_event_container            TYPE swr_cont,
        ls_dates                      TYPE ty_dates,
        ls_messages_line              TYPE swr_messag,
        ls_return                     TYPE bapiret2,
        ls_otpend                     TYPE zst_hr_ot,
        ls_ot                         TYPE zst_hr_otapprove_reject,
        ls_ot2                         TYPE zst_hr_otapprove_reject,
        ls_comments                   TYPE solisti1-line,
        ls_otpending                  TYPE zzt_hr_otdetails,
        ls_t550a                      TYPE t550a,
        ls_ottotal                    TYPE zzt_hr_otdetails,
        ls_day                        TYPE casdayattr,
        ls_ottmp                      TYPE zzt_hr_otdetails,
        ls_pa0001                     TYPE pa0001,                        " + CDELAPENA 02/05/2014
        ls_t001p                      TYPE t001p,                         " + CDELAPENA 02/05/2014
        lt_dates                      TYPE STANDARD TABLE OF ty_dates,
        lt_day                        TYPE STANDARD TABLE OF casdayattr,
        lt_event_container            TYPE STANDARD TABLE OF swr_cont,
        lt_messages_line              TYPE STANDARD TABLE OF swr_messag,
        lt_message_struct             TYPE STANDARD TABLE OF swr_mstruc,
        lt_otpending                  TYPE STANDARD TABLE OF zzt_hr_otdetails,
        lt_ottotal                    TYPE STANDARD TABLE OF zzt_hr_otdetails,
        lt_ottmp                      TYPE STANDARD TABLE OF zzt_hr_otdetails.
  CONSTANTS: co_e                     TYPE c VALUE 'E',
             co_x                     TYPE c VALUE 'X',
             co_mid(4)                TYPE p DECIMALS 2 VALUE '24',
             co_pending               TYPE zzotstatus VALUE 'PND',
             co_approved              TYPE zzotstatus VALUE 'APR'.

  SELECT *
    FROM zzt_hr_otdetails
    INTO TABLE lt_otpending
      FOR ALL ENTRIES IN gt_ot
      WHERE pernr EQ i_requestor
        AND otwfno EQ gt_ot-otwfno
        AND entryno EQ gt_ot-entryno
        AND otstatus EQ co_pending.
  IF sy-subrc EQ 0.

*-- Start Add CDELAPENA 02/05/2014
    CLEAR :      ls_pa0001,                                              " + CDELAPENA 02/05/2014
                 ls_t001p.                                               " + CDELAPENA 02/05/2014
    SELECT SINGLE *
      FROM pa0001
      INTO ls_pa0001
     WHERE pernr = i_requestor
       AND endda = '99991231'.

    IF ls_pa0001 IS NOT INITIAL.

      SELECT SINGLE *
        FROM t001p
        INTO ls_t001p
       WHERE werks = ls_pa0001-werks
         AND btrtl = ls_pa0001-btrtl.

    ENDIF.
*-- End Add CDELAPENA 02/05/2014

    LOOP AT gt_ot INTO ls_ot.
      CLEAR: lv_hours,
             lv_tmp,
             lv_tmp2,
             lv_tmp3,
             lv_hours2,
             lv_nxtday,
             lv_timein,
             lv_timeout,
             lv_nonworking,
             lv_deltamin,
             lv_reqmins,
             ls_t550a,
             ls_day,
             ls_otpending.

      PERFORM f_convert_time  USING ls_ot-apptime_in
                                  CHANGING lv_timein.
      PERFORM f_convert_time  USING ls_ot-apptime_out
                                  CHANGING lv_timeout.

      PERFORM f_convert_to_minutes  USING ls_ot-hours_to_appr
                                    CHANGING lv_reqmins.
      lv_hours = ls_ot-hours_to_appr.
      IF ls_ot-start_date EQ ls_ot-end_date.

*         Check if holiday
        IF ls_t001p IS NOT INITIAL.                                     " + CDELAPENA 02/05/2014
          CALL FUNCTION 'DAY_ATTRIBUTES_GET'
            EXPORTING
*             factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*             holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
              holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
              date_from                  = ls_ot-start_date
              date_to                    = ls_ot-start_date
            TABLES
              day_attributes             = lt_day
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.
            READ TABLE lt_day INTO ls_day INDEX 1.
*            IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.         " - CDELAPENA 02/05/2014
            IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
              lv_nonworking = co_x.
            ELSE.
            ENDIF.
          ENDIF.
*         get employee working hours
          CALL FUNCTION 'CATT_PERSONAL_WORKING_TIMES'
            EXPORTING
              pernr         = ls_ot-pernr
              date          = ls_ot-start_date
            IMPORTING
              r550a         = ls_t550a
*             PBEG1         =
*             PEND1         =
*             PREN1         =
*             PUNB1         =
*             PBEZ1         =
*             PBEG2         =
*             PEND2         =
*             PREN2         =
*             PUNB2         =
*             PBEZ2         =
*             RPTPSP        =
            EXCEPTIONS
              error_occured = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
*-- Start Add CDELAPENA 02/05/2014
          IF ls_t550a-sollz = 0.
            lv_nonworking = co_x.
          ENDIF.
*-- End Add CDELAPENA 02/05/2014
          IF lv_nonworking IS INITIAL.
*          IF lv_timein GE ls_t550a-sobeg AND lv_timein LE ls_t550a-soend.
*            CLEAR:ls_return.
*            ls_return-type = co_e.
*            ls_return-message = text-006.
*            APPEND ls_return TO gt_return.
*            lv_error = co_x.
*            EXIT.
*          ENDIF.
*          IF lv_timeout GE ls_t550a-sobeg AND lv_timeout LE ls_t550a-soend.
*            CLEAR:ls_return.
*            ls_return-type = co_e.
*            ls_return-message = text-006.
*            APPEND ls_return TO gt_return.
*            lv_error = co_x.
*            EXIT.
*          ENDIF.

            IF lv_timein EQ ls_t550a-sobeg AND lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein EQ ls_t550a-sobeg.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein GT ls_t550a-sobeg AND lv_timein LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timeout GT ls_t550a-sobeg AND lv_timeout LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout GT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- Start Add CDELAPENA 02/05/2014
            IF lv_timein BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.

            IF lv_timeout BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- End Add CDELAPENA 02/05/2014

          ELSE.
          ENDIF.
* Start - DV2K914479 - PMEDIOS - HR.TM.OT Application.v12
*          CLEAR: lv_nonworking,
*                 ls_day.
          CLEAR: ls_day.
* End - DV2K914479 - PMEDIOS - HR.TM.OT Application.v12
*       get total of ot for the day
          CLEAR:  lv_total,

                  ls_dates.
          REFRESH: lt_day,
                   lt_ottotal.
          SELECT *
            FROM zzt_hr_otdetails
              INTO TABLE lt_ottotal
              WHERE pernr EQ i_requestor
                AND ot_date EQ ls_ot-start_date
                AND otwfno NE ls_ot-otwfno
                AND ( otstatus EQ co_pending
                    OR otstatus EQ co_approved ).
          IF sy-subrc EQ 0.
            LOOP AT lt_ottotal INTO ls_ottotal.
              REFRESH: lt_ottmp.
              CLEAR:  lv_pndmins,
                      lv_aprmins,
                      lv_tmptime,
                      lv_tmphours,
                      lv_tmpchar,
                      lv_tmpchar2,
                      lv_tmphours2,
                      lv_ctrind.
              APPEND LINES OF lt_ottotal TO lt_ottmp.
              SORT lt_ottmp BY pernr otwfno entryno.
              LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                  AND otwfno EQ ls_ot-otwfno
                                  AND entryno EQ ls_ot-entryno.
                ADD 1 TO lv_ctrind.
                IF ls_ottmp-otstart_date EQ ls_ot-start_date.
                  lv_tmptime = ls_ot-otstart_time(2).
                  lv_tmphours = 24 - lv_tmptime.
                ELSE.
                  lv_tmpchar = ls_ot-otend_time(2).
                  lv_tmpchar2 = ls_ot-otend_time+2(2).
                  CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                  lv_tmphours = lv_tmphours2.
                ENDIF.
              ENDLOOP.
              lv_tmphours = ls_ottotal-req_no_of_hours.
              IF lv_ctrind GT 1.
                lv_tmphours = lv_tmphours.
              ELSE.
                lv_tmphours = ls_ot-req_no_of_hours.
              ENDIF.
              IF ls_ottotal-otstatus EQ co_approved.
                PERFORM f_convert_to_minutes  USING ls_ottotal-approvedhours
                                                CHANGING lv_aprmins.
                lv_total = lv_total + lv_aprmins.
              ELSEIF ls_ottotal-otstatus EQ co_pending.
                PERFORM f_convert_to_minutes  USING lv_tmphours
                                              CHANGING lv_pndmins.
                lv_total = lv_total + lv_pndmins.
              ENDIF.
            ENDLOOP.
          ENDIF.
          CLEAR: ls_dates.
          READ TABLE lt_dates INTO ls_dates WITH KEY date = ls_ot-start_date.
          lv_total = lv_total + lv_reqmins + ls_dates-hours.

          IF ls_dates IS NOT INITIAL.
            ls_dates-hours = ls_dates-hours + lv_reqmins.
            MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
          ELSE.
            ls_dates-date =  ls_ot-start_date.
            ls_dates-hours = lv_reqmins.
            APPEND ls_dates TO lt_dates.
          ENDIF.
          CALL FUNCTION 'DAY_ATTRIBUTES_GET'
            EXPORTING
*             factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*             holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
              holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
              date_from                  = ls_ot-start_date
              date_to                    = ls_ot-start_date
            TABLES
              day_attributes             = lt_day
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.
            READ TABLE lt_day INTO ls_day INDEX 1.
*               IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.        " - CDELAPENA 02/05/2014
            IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
              lv_nonworking = co_x.
            ELSE.
            ENDIF.
          ENDIF.
          IF lv_nonworking EQ co_x AND lv_total GT 480 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
            CLEAR: ls_return.
            ls_return-type = co_e.
            CONCATENATE text-002 ls_ot-start_date INTO ls_return-message SEPARATED BY space.
            APPEND ls_return TO gt_return.
            lv_error = co_x.
            EXIT.
          ELSEIF lv_nonworking IS INITIAL AND lv_total GT 240 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
            ls_return-type = co_e.
            CLEAR: ls_return.
            CONCATENATE text-002 ls_ot-start_date INTO ls_return-message SEPARATED BY space.
            APPEND ls_return TO gt_return.
            lv_error = co_x.
            EXIT.
          ENDIF.
          READ TABLE lt_otpending INTO ls_otpending WITH KEY entryno = ls_ot-entryno.
          IF sy-subrc EQ 0.

            ls_otpending-apptime_in = lv_timein.
            ls_otpending-apptime_out = lv_timeout.
            ls_otpending-projectedhours = lv_hours.
            ls_otpending-useraction = ls_ot-action.
            UPDATE zzt_hr_otdetails FROM ls_otpending.
          ENDIF.
        ELSE.
*        check if holiday
          CLEAR: lv_nonworking,
                 ls_day.
          CALL FUNCTION 'DAY_ATTRIBUTES_GET'
            EXPORTING
*             factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*             holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
              holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
              date_from                  = ls_ot-start_date
              date_to                    = ls_ot-start_date
            TABLES
              day_attributes             = lt_day
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
*Implement suitable error handling here
          ELSE.
            READ TABLE lt_day INTO ls_day INDEX 1.
*               IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.        " - CDELAPENA 02/05/2014
            IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
              lv_nonworking = co_x.
            ELSE.
            ENDIF.
          ENDIF.
          IF lv_nonworking IS INITIAL.
*         get employee working hours
            CALL FUNCTION 'CATT_PERSONAL_WORKING_TIMES'
              EXPORTING
                pernr         = ls_ot-pernr
                date          = ls_ot-start_date
              IMPORTING
                r550a         = ls_t550a
*               PBEG1         =
*               PEND1         =
*               PREN1         =
*               PUNB1         =
*               PBEZ1         =
*               PBEG2         =
*               PEND2         =
*               PREN2         =
*               PUNB2         =
*               PBEZ2         =
*               RPTPSP        =
              EXCEPTIONS
                error_occured = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
*   Implement suitable error handling here
            ENDIF.
* Start - DV2K914479 - PMEDIOS - HR.TM.OT Application.v12
            IF ls_t550a-sollz = 0.
              lv_nonworking = co_x.
            ENDIF.
* End - DV2K914479 - PMEDIOS - HR.TM.OT Application.v12

            IF lv_timein EQ ls_t550a-sobeg AND lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein EQ ls_t550a-sobeg.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein GT ls_t550a-sobeg AND lv_timein LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout GT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- Start Add CDELAPENA 02/05/2014
            IF lv_timein BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.

            IF lv_timeout BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- End Add CDELAPENA 02/05/2014
          ENDIF.
          CLEAR: ls_t550a,
                 ls_day,
                 lv_nonworking.

          CALL FUNCTION 'DAY_ATTRIBUTES_GET'
            EXPORTING
*             factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*             holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
              holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
              date_from                  = ls_ot-end_date
              date_to                    = ls_ot-end_date
            TABLES
              day_attributes             = lt_day
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
*Implement suitable error handling here
          ELSE.
            READ TABLE lt_day INTO ls_day INDEX 1.
*               IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.        " - CDELAPENA 02/05/2014
            IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
              lv_nonworking = co_x.
            ELSE.
            ENDIF.
          ENDIF.
          IF  lv_nonworking IS INITIAL.
            CALL FUNCTION 'CATT_PERSONAL_WORKING_TIMES'
              EXPORTING
                pernr         = ls_ot-pernr
                date          = ls_ot-end_date
              IMPORTING
                r550a         = ls_t550a
*               PBEG1         =
*               PEND1         =
*               PREN1         =
*               PUNB1         =
*               PBEZ1         =
*               PBEG2         =
*               PEND2         =
*               PREN2         =
*               PUNB2         =
*               PBEZ2         =
*               RPTPSP        =
              EXCEPTIONS
                error_occured = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.
*-- Start Add CDELAPENA 02/05/2014
            IF ls_t550a-sollz = 0.
              lv_nonworking = co_x.
            ENDIF.
*-- End Add CDELAPENA 02/05/2014
            IF lv_timein EQ ls_t550a-sobeg AND lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein EQ ls_t550a-sobeg.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timeout EQ ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timeout GT ls_t550a-sobeg AND lv_timeout LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout GT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            IF lv_timein LT ls_t550a-sobeg AND lv_timeout LT ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- Start Add CDELAPENA 02/05/2014
            IF lv_timein BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.

            IF lv_timeout BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
              CLEAR:ls_return.
              ls_return-type = co_e.
              ls_return-message = text-006.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
*-- End Add CDELAPENA 02/05/2014
          ENDIF.

          CLEAR:  lv_nonworking.
          LOOP AT lt_otpending INTO ls_otpending WHERE entryno = ls_ot-entryno.
            CLEAR: ls_ot2.
            READ TABLE gt_ot INTO ls_ot2 WITH KEY pernr = ls_otpending-pernr
                                                  otwfno = ls_otpending-otwfno
                                                  entryno = ls_otpending-entryno.
            IF ls_otpending-otstart_time GT lv_timein.
              lv_nxtday = co_x.
              EXIT.
            ELSE.
              CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
                EXPORTING
                  date_from       = ls_otpending-otstart_date
                  date_to         = ls_otpending-otstart_date
                  time_from       = lv_timein"ls_otpending-otstart_time
                  time_to         = '240000'
                IMPORTING
                  delta_time      = lv_deltamin
*                 DELTA_UNIT      =
                EXCEPTIONS
                  from_greater_to = 1
                  OTHERS          = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ELSE.
                CALL FUNCTION 'CONVERSION_EXIT_SDURA_OUTPUT'
                  EXPORTING
                    input  = lv_deltamin
                  IMPORTING
                    output = lv_dhours.

              ENDIF.

              CONDENSE lv_dhours NO-GAPS.
              SPLIT lv_dhours AT ':' INTO lc_tmp
                                          lc_tmp2.
              MOVE lc_tmp TO lv_tmp.
              MOVE lc_tmp2 TO lv_tmp2.
              lv_tmp3 = lv_tmp + ( lv_tmp2 / 100 ).
              lv_hours =  lv_tmp3.

              lv_tmp = lv_timeout(2).
              lv_tmp2 = lv_timeout+2(2).
              lv_tmp3 = lv_tmp + ( lv_tmp2 / 100 ).
              lv_hours2 = lv_tmp3.
            ENDIF.
          ENDLOOP.
          IF lv_nxtday EQ co_x.
            CLEAR: lv_total.
            REFRESH: lt_day,
                     lt_ottotal.
            SELECT *
              FROM zzt_hr_otdetails
                INTO TABLE lt_ottotal
                WHERE pernr EQ i_requestor
                  AND ot_date EQ ls_ot-end_date
                  AND otwfno NE ls_ot-otwfno
                  AND ( otstatus EQ co_pending
                      OR otstatus EQ co_approved ).
            IF sy-subrc EQ 0.
              LOOP AT lt_ottotal INTO ls_ottotal.
                CLEAR:  lv_pndmins,
                      lv_aprmins,
                      lv_tmptime,
                      lv_tmphours,
                      lv_tmpchar,
                        lv_tmpchar2,
                        lv_tmphours2,
                      lv_ctrind.
                REFRESH: lt_ottmp.
                APPEND LINES OF lt_ottotal TO lt_ottmp.
                SORT lt_ottmp BY pernr otwfno entryno.
                LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                    AND otwfno EQ ls_ot-otwfno
                                    AND entryno EQ ls_ot-entryno.
                  ADD 1 TO lv_ctrind.
                  IF ls_ottmp-otstart_date EQ ls_ot-start_date.
                    lv_tmptime = ls_ot-otstart_time(2).
                    lv_tmphours = 24 - lv_tmptime.
                  ELSE.
                    lv_tmpchar = ls_ot-otend_time(2).
                    lv_tmpchar2 = ls_ot-otend_time+2(2).
                    CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                    lv_tmphours = lv_tmphours2.
                  ENDIF.
                ENDLOOP.
                lv_tmphours = ls_ottotal-req_no_of_hours.
                IF lv_ctrind GT 1.
                  lv_tmphours = lv_tmphours.
                ELSE.
                  lv_tmphours = ls_ot-req_no_of_hours.
                ENDIF.
                IF ls_ottotal-otstatus EQ co_approved.
                  PERFORM f_convert_to_minutes  USING ls_ottotal-approvedhours
                                              CHANGING lv_aprmins.
                  lv_total = lv_total + lv_aprmins.
                ELSEIF ls_ottotal-otstatus EQ co_pending.
                  PERFORM f_convert_to_minutes  USING lv_tmphours
                                              CHANGING lv_pndmins.
                  lv_total = lv_total + lv_pndmins.
                ENDIF.
              ENDLOOP.
            ENDIF.
            CLEAR: ls_dates.
            READ TABLE lt_dates INTO ls_dates WITH KEY date = ls_ot-start_date.
            lv_total = lv_total + lv_reqmins + ls_dates-hours.
            IF ls_dates IS NOT INITIAL.
              ls_dates-hours = ls_dates-hours + lv_reqmins.
              MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
            ELSE.
              ls_dates-date =  ls_ot-start_date.
              ls_dates-hours = lv_reqmins.
              APPEND ls_dates TO lt_dates.
            ENDIF.
            CALL FUNCTION 'DAY_ATTRIBUTES_GET'
              EXPORTING
*               factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*               holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
                holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
                date_from                  = ls_ot-end_date
                date_to                    = ls_ot-end_date
              TABLES
                day_attributes             = lt_day
              EXCEPTIONS
                factory_calendar_not_found = 1
                holiday_calendar_not_found = 2
                date_has_invalid_format    = 3
                date_inconsistency         = 4
                OTHERS                     = 5.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ELSE.
              READ TABLE lt_day INTO ls_day INDEX 1.
*               IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.        " - CDELAPENA 02/05/2014
              IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
                lv_nonworking = co_x.
              ELSE.
              ENDIF.
            ENDIF.
            IF lv_nonworking EQ co_x AND lv_total GT 480 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              CLEAR: ls_return.
              ls_return-type = co_e.
              CONCATENATE text-002 ls_ot-end_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ELSEIF lv_nonworking IS INITIAL AND lv_total GT 240 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              ls_return-type = co_e.
              CLEAR: ls_return.
              CONCATENATE text-002 ls_ot-end_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            CLEAR: ls_otpending.
            READ TABLE lt_otpending INTO ls_otpending WITH KEY entryno = ls_ot-entryno
                                                               ot_date = ls_ot-end_date.
            IF sy-subrc EQ 0.
              ls_otpending-apptime_in = lv_timein.
              ls_otpending-apptime_out = lv_timeout.
              ls_otpending-projectedhours = lv_hours.
              ls_otpending-useraction = ls_ot-action.
              UPDATE zzt_hr_otdetails FROM ls_otpending.
            ENDIF.
          ELSE.
            CLEAR: lv_total.
            REFRESH: lt_day,
                     lt_ottotal.
            SELECT *
              FROM zzt_hr_otdetails
                INTO TABLE lt_ottotal
                WHERE pernr EQ i_requestor
                  AND ot_date EQ ls_ot-start_date
                  AND otwfno NE ls_ot-otwfno
                  AND ( otstatus EQ co_pending
                      OR otstatus EQ co_approved ).
            IF sy-subrc EQ 0.
              LOOP AT lt_ottotal INTO ls_ottotal.
                CLEAR:  lv_pndmins,
                     lv_aprmins,
                     lv_tmptime,
                     lv_tmphours,
                     lv_tmpchar,
                        lv_tmpchar2,
                        lv_tmphours2,
                     lv_ctrind.
                REFRESH: lt_ottmp.
                APPEND LINES OF lt_ottotal TO lt_ottmp.
                SORT lt_ottmp BY pernr otwfno entryno.
                LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                    AND otwfno EQ ls_ot-otwfno
                                    AND entryno EQ ls_ot-entryno.
                  ADD 1 TO lv_ctrind.
                  IF ls_ottmp-otstart_date EQ ls_ot-start_date.
                    lv_tmptime = ls_ot-otstart_time(2).
                    lv_tmphours = 24 - lv_tmptime.
                  ELSE.
                    lv_tmpchar = ls_ot-otend_time(2).
                    lv_tmpchar2 = ls_ot-otend_time+2(2).
                    CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                    lv_tmphours = lv_tmphours2.
                  ENDIF.
                ENDLOOP.
                lv_tmphours = ls_ottotal-req_no_of_hours.
                IF lv_ctrind GT 1.
                  lv_tmphours = lv_tmphours.
                ELSE.
                  lv_tmphours = ls_ot-req_no_of_hours.
                ENDIF.
                IF ls_ottotal-otstatus EQ co_approved.
                  PERFORM f_convert_to_minutes  USING ls_ottotal-approvedhours
                                              CHANGING lv_aprmins.
                  lv_total = lv_total + lv_aprmins.
                ELSEIF ls_ottotal-otstatus EQ co_pending.
                  PERFORM f_convert_to_minutes  USING lv_tmphours
                                              CHANGING lv_pndmins.
                  lv_total = lv_total + lv_pndmins.
                ENDIF.
              ENDLOOP.
            ENDIF.
            CLEAR: ls_dates.
            READ TABLE lt_dates INTO ls_dates WITH KEY date = ls_ot-start_date.
            CLEAR: lv_reqmins2.
            PERFORM f_convert_to_minutes  USING lv_hours
                                          CHANGING lv_reqmins2.
            lv_total = lv_total + lv_reqmins2 + ls_dates-hours.
            IF ls_dates IS NOT INITIAL.
              ls_dates-hours = ls_dates-hours + lv_reqmins2.
              MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
            ELSE.
              ls_dates-date =  ls_ot-start_date.
              ls_dates-hours = lv_reqmins2.
              APPEND ls_dates TO lt_dates.
            ENDIF.
            CALL FUNCTION 'DAY_ATTRIBUTES_GET'
              EXPORTING
*               factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*               holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
                holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
                date_from                  = ls_ot-start_date
                date_to                    = ls_ot-start_date
              TABLES
                day_attributes             = lt_day
              EXCEPTIONS
                factory_calendar_not_found = 1
                holiday_calendar_not_found = 2
                date_has_invalid_format    = 3
                date_inconsistency         = 4
                OTHERS                     = 5.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ELSE.
              READ TABLE lt_day INTO ls_day INDEX 1.
              IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.
                lv_nonworking = co_x.
              ELSE.
              ENDIF.
            ENDIF.
            IF lv_nonworking EQ co_x AND lv_total GT 480 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              CLEAR: ls_return.
              ls_return-type = co_e.
              CONCATENATE text-002 ls_ot-start_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ELSEIF lv_nonworking IS INITIAL AND lv_total GT 240 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              ls_return-type = co_e.
              CLEAR: ls_return.
              CONCATENATE text-002 ls_ot-start_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            CLEAR: ls_otpending.
            READ TABLE lt_otpending INTO ls_otpending WITH KEY entryno = ls_ot-entryno
                                                               ot_date = ls_ot-start_date.
            IF sy-subrc EQ 0.
              ls_otpending-apptime_in = lv_timein.
              ls_otpending-apptime_out = lv_timeout.
              ls_otpending-projectedhours = lv_hours.
              ls_otpending-useraction = ls_ot-action.
              UPDATE zzt_hr_otdetails FROM ls_otpending.
            ENDIF.
            CLEAR: lv_total.
            REFRESH: lt_day,
                     lt_ottotal.
            SELECT *
              FROM zzt_hr_otdetails
                INTO TABLE lt_ottotal
                WHERE pernr EQ i_requestor
                  AND ot_date EQ ls_ot-end_date
                  AND otwfno NE ls_ot-otwfno
                  AND ( otstatus EQ co_pending
                      OR otstatus EQ co_approved ).
            IF sy-subrc EQ 0.
              LOOP AT lt_ottotal INTO ls_ottotal.
                CLEAR:  lv_pndmins,
                     lv_aprmins,
                     lv_tmptime,
                     lv_tmphours,
                     lv_tmpchar,
                        lv_tmpchar2,
                        lv_tmphours2,
                     lv_ctrind.
                REFRESH: lt_ottmp.
                APPEND LINES OF lt_ottotal TO lt_ottmp.
                SORT lt_ottmp BY pernr otwfno entryno.
                LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                    AND otwfno EQ ls_ot-otwfno
                                    AND entryno EQ ls_ot-entryno.
                  ADD 1 TO lv_ctrind.
                  IF ls_ottmp-otstart_date EQ ls_ot-start_date.
                    lv_tmptime = ls_ot-otstart_time(2).
                    lv_tmphours = 24 - lv_tmptime.
                  ELSE.
                    lv_tmpchar = ls_ot-otend_time(2).
                    lv_tmpchar2 = ls_ot-otend_time+2(2).
                    CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                    lv_tmphours = lv_tmphours2.
                  ENDIF.
                ENDLOOP.
                lv_tmphours = ls_ottotal-req_no_of_hours.
                IF lv_ctrind GT 1.
                  lv_tmphours = lv_tmphours.
                ELSE.
                  lv_tmphours = ls_ot-req_no_of_hours.
                ENDIF.
                IF ls_ottotal-otstatus EQ co_approved.
                  PERFORM f_convert_to_minutes  USING ls_ottotal-approvedhours
                                              CHANGING lv_aprmins.
                  lv_total = lv_total + lv_aprmins.
                ELSEIF ls_ottotal-otstatus EQ co_pending.
                  PERFORM f_convert_to_minutes  USING lv_tmphours
                                              CHANGING lv_pndmins.
                  lv_total = lv_total + lv_pndmins.
                ENDIF.
              ENDLOOP.
            ENDIF.
            CLEAR: ls_dates.
            CLEAR: lv_reqmins3.
            PERFORM f_convert_to_minutes  USING lv_hours2
                                          CHANGING lv_reqmins3.
            READ TABLE lt_dates INTO ls_dates WITH KEY date = ls_ot-end_date.
            lv_total = lv_total + lv_reqmins3 + ls_dates-hours.
            IF ls_dates IS NOT INITIAL.
              ls_dates-hours = ls_dates-hours + lv_reqmins3.
              MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
            ELSE.
              ls_dates-date =  ls_ot-end_date.
              ls_dates-hours = lv_reqmins3.
              APPEND ls_dates TO lt_dates.
            ENDIF.
            CALL FUNCTION 'DAY_ATTRIBUTES_GET'
              EXPORTING
*               factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*               holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
                holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
                date_from                  = ls_ot-end_date
                date_to                    = ls_ot-end_date
              TABLES
                day_attributes             = lt_day
              EXCEPTIONS
                factory_calendar_not_found = 1
                holiday_calendar_not_found = 2
                date_has_invalid_format    = 3
                date_inconsistency         = 4
                OTHERS                     = 5.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ELSE.
              READ TABLE lt_day INTO ls_day INDEX 1.
*               IF ls_day-freeday EQ co_x OR ls_day-holiday EQ co_x.        " - CDELAPENA 02/05/2014
              IF ls_day-holiday EQ co_x.                                  " + CDELAPENA 02/05/2014
                lv_nonworking = co_x.
              ELSE.
              ENDIF.
            ENDIF.
            IF lv_nonworking EQ co_x AND lv_total GT 480 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              CLEAR: ls_return.
              ls_return-type = co_e.
              CONCATENATE text-002 ls_ot-end_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ELSEIF lv_nonworking IS INITIAL AND lv_total GT 240 AND ls_ot-purpose NE 16 AND ls_ot-action NE 'R'.
              ls_return-type = co_e.
              CLEAR: ls_return.
              CONCATENATE text-002 ls_ot-end_date INTO ls_return-message SEPARATED BY space.
              APPEND ls_return TO gt_return.
              lv_error = co_x.
              EXIT.
            ENDIF.
            CLEAR: ls_otpending.
            READ TABLE lt_otpending INTO ls_otpending WITH KEY entryno = ls_ot-entryno
                                                               ot_date = ls_ot-end_date.
            IF sy-subrc EQ 0.
              ls_otpending-apptime_in = lv_timein.
              ls_otpending-apptime_out = lv_timeout.
              ls_otpending-projectedhours = lv_hours2.
              ls_otpending-useraction = ls_ot-action.
              UPDATE zzt_hr_otdetails FROM ls_otpending.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      ls_event_container-value = ls_ot-entryno.
      APPEND ls_event_container TO lt_event_container.
    ENDLOOP.
    IF lv_error EQ co_x.
      EXIT.
    ENDIF.

    DATA lt_message TYPE STANDARD TABLE OF swr_messag.
    REFRESH lt_message.
    CALL FUNCTION 'SAP_WAPI_DECISION_COMPLETE'
      EXPORTING
        workitem_id          = i_wiid
       user                 = i_approver_user
        decision_key         = '0001'
*     IMPORTING
*       RETURN_CODE          =
*       NEW_STATUS           =
      TABLES
        message_lines        = lt_message
*       MESSAGE_STRUCT       =
              .


  ELSE.
    ls_return-type = co_e.
    ls_return-message = text-005.
    APPEND ls_return TO gt_return.
  ENDIF.








ENDFUNCTION.
