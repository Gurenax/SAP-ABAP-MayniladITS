FUNCTION zfm_hr_ot_application_create .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_USER) TYPE  SY-UNAME OPTIONAL
*"  TABLES
*"      GT_OT_DETAILS TYPE  ZT_HCM_OTAPPLICATION OPTIONAL
*"      GT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  TYPES:  BEGIN OF ty_dates,
            date                      TYPE sy-datum,
            hours                     TYPE i,
          END OF ty_dates.


  DATA: lv_pernr                      TYPE ess_emp-employeenumber,
        lv_subgrp                     TYPE ess_emp-employeesubgroup,
        lv_infotype                   TYPE prelp-infty,
        lv_objectkey                  TYPE swr_struct-object_key,
        lv_subrc                      TYPE sy-subrc,
        lv_event_id                   TYPE swr_struct-event_id,
        lv_currdate                   TYPE sy-datum,
        lv_reqhour(4)                 TYPE p DECIMALS 2,
        lv_reqmins                    TYPE i,
        lv_reqmins2                   TYPE i,
        lv_tmp(4)                     TYPE p DECIMALS 2,
        lv_tmp2(4)                    TYPE p DECIMALS 2,
        lv_tmp3(4)                    TYPE p DECIMALS 2,
        lv_hours2(4)                  TYPE p DECIMALS 2,
        lv_aprmins                    TYPE i,
        lv_pndmins                    TYPE i,
        lv_timein                     TYPE uzeit,
        lv_timeout                    TYPE uzeit,
        lv_starttime                  TYPE uzeit,
        lv_tmpchar(2)                 TYPE c,
        lv_tmpchar2(2)                 TYPE c,
        lv_endtime                    TYPE uzeit,
        lv_days                       TYPE i,
        lv_hours                      TYPE i,
        lv_otctr                      TYPE i,
        lv_deltamin                   TYPE mcwmit-be_ae,
        lv_error                      TYPE c,
        lv_otlastno(10)               TYPE n,
        lv_otwfno(10)                 TYPE n,
        lv_nonworking                 TYPE c,
        lv_serial_no                  TYPE nriv-nrlevel,
        lv_exit                       TYPE c,
        lv_ctrind                     TYPE i,
        lv_index                      TYPE i,
        lv_tmptime(4)                 TYPE p DECIMALS 2,
        lv_tmphours(4)                TYPE p DECIMALS 2,
        lv_tmphours2(5)               TYPE c,
        ls_pa0001                     TYPE pa0001,                        " + CDELAPENA 02/05/2014
        ls_t001p                      TYPE t001p,                         " + CDELAPENA 02/05/2014
        ls_return                     TYPE bapiret2,
        ls_event_container            TYPE swr_cont,
        ls_messages_line              TYPE swr_messag,
        ls_ot_details                 TYPE zst_hcm_otapplication,
        ls_dates                      TYPE ty_dates,
        ls_t550a                      TYPE t550a,
        ls_ot                         TYPE zzt_hr_otdetails,
        ls_otupd                      TYPE zzt_hr_otdetails,
        ls_otwf                       TYPE solisti1-line,"ZST_HR_OTAPPWF,
        ls_day                        TYPE casdayattr,
        ls_pa0007                     TYPE pa0007,
        ls_pa2003                     TYPE pa2003,
        ls_ottmp                      TYPE zzt_hr_otdetails,
        ls_otcheck                    TYPE zzt_hr_otdetails,
        lt_day                        TYPE STANDARD TABLE OF casdayattr,
        lt_event_container            TYPE STANDARD TABLE OF swr_cont,
        lt_messages_line              TYPE STANDARD TABLE OF swr_messag,
        lt_message_struct             TYPE STANDARD TABLE OF swr_mstruc,
        lt_otwf                       TYPE STANDARD TABLE OF zst_hr_otappwf,
        lt_ot                         TYPE STANDARD TABLE OF zzt_hr_otdetails,
        lt_ottmp                      TYPE STANDARD TABLE OF zzt_hr_otdetails,
        lt_otupd                      TYPE STANDARD TABLE OF zzt_hr_otdetails,
        lt_dates                      TYPE STANDARD TABLE OF ty_dates.

  CONSTANTS: co_1e                    TYPE ess_emp-nameofempsubgroup VALUE '1E',
             co_1a                    TYPE ess_emp-nameofempsubgroup VALUE '1A',
             co_1b                    TYPE ess_emp-nameofempsubgroup VALUE '1B',
             co_1c                    TYPE ess_emp-nameofempsubgroup VALUE '1C',
             co_1d                    TYPE ess_emp-nameofempsubgroup VALUE '1D',
             co_2002                  TYPE prelp-infty VALUE '2002',
             co_2007                  TYPE prelp-infty VALUE '2007',
             co_e                     TYPE c VALUE 'E',
             co_x                     TYPE c VALUE 'X',
             co_approved              TYPE zzotstatus VALUE 'APR',
             co_pending               TYPE zzotstatus VALUE 'PND',
             co_rejected              TYPE zzotstatus VALUE 'RJC',
             co_range                 TYPE inri-nrrangenr VALUE '01',
             co_object                TYPE inri-object VALUE 'ZOTREQNO'.

  CONDENSE i_user NO-GAPS.
  TRANSLATE i_user TO UPPER CASE.
*  DO.
*    IF lv_exit EQ co_x.
*      EXIT.
*    ENDIF.
*  ENDDO.

*  get user details
  CALL FUNCTION 'HR_GETEMPLOYEEDATA_FROMUSER'
    EXPORTING
      username                  = i_user
*     VALIDBEGIN                = SY-DATUM
    IMPORTING
      employeenumber            = lv_pernr
*     COUNTRYGROUPING           =
*     NAME                      =
*     BUSINESSAREA              =
*     NAMEOFBUSAREA             =
*     PERSONNELAREA             =
*     NAMEOFPERSAREA            =
*     PERSSUBAREA               =
*     NAMEOFPERSSUBAREA         =
*     EMPLOYEEGROUP             =
*     NAMEOFEMPGROUP            =
      employeesubgroup          = lv_subgrp
*     NAMEOFEMPSUBGROUP         =
*     ORGUNIT                   =
*     NAMEOFORGUNIT             =
*     POSITION                  =
*     NAMEOFPOSITION            =
*     COSTCENTER                =
*     NAMEOFCOSTCENTER          =
*     ADMINGROUP                =
*     PAYROLLADMIN              =
*     PERSONNELLADMIN           =
*     TIMEADMIN                 =
*     CONTROLLINGAREA           =
*     EMPLOYMENTSTATUS          =
*     PAYROLLAREA               =
*     NAMEOFPAYROLLAREA         =
*     PAYROLLSTATUS             =
*     COMPANYCODE               =
*     NAMEOFCOMPANYCODE         =
*     USERLOGONLANGUAGE         =
*     USERDATEFORMAT            =
*     USERDECIMALFORMAT         =
*     CURRENCY                  =
*     CURRENCYDECIMALS          =
*     SAPRELEASE                =
*     ACCOUNTEDTO               =
*     FIRSTNAME                 =
*     LASTNAME                  =
*     FIRSTNAMEROMAJI           =
*     LASTNAMEROMAJI            =
*     BIRTHDATE                 =
*     RETURN                    =
    EXCEPTIONS
      user_not_found            = 1
      countrygrouping_not_found = 2
      infty_not_found           = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    ls_return-type = co_e.
    ls_return-message = text-004.
    APPEND ls_return TO gt_return.
  ELSE.
    IF gt_ot_details[] IS INITIAL.
      ls_return-type = co_e.
      ls_return-message = text-003.
      APPEND ls_return TO gt_return.
    ELSE.


      IF lv_pernr IS NOT INITIAL.
        IF lv_subgrp EQ co_1e.
          lv_infotype = co_2002.
        ELSEIF lv_subgrp EQ co_1a OR lv_subgrp EQ co_1b OR lv_subgrp EQ co_1c OR  lv_subgrp EQ co_1d.
          lv_infotype = co_2007.
        ENDIF.
*      lock table ZZT_HR_OTDETAILS for entry number
        CALL FUNCTION 'ENQUEUE_EZZZT_HR_OTDET'
          EXPORTING
*           MODE_ZZT_HR_OTDETAILS = 'E'
*           MANDT                 = SY-MANDT
            pernr                 = lv_pernr
*           OTWFNO                =
*           OT_DATE               =
*           ENTRYNO               =
*           OTCTR                 =
*           X_PERNR               = ' '
*           X_OT_DATE             = ' '
*           X_ENTRYNO             = ' '
*           X_OTCTR               = ' '
*           _SCOPE                = '2'
*           _WAIT                 = ' '
*           _COLLECT              = ' '
          EXCEPTIONS
            foreign_lock          = 1
            system_failure        = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
*           Implement suitable error handling here
        ENDIF.
        SELECT *
            FROM zzt_hr_otdetails
            INTO TABLE lt_ot
            WHERE pernr EQ lv_pernr.
        IF sy-subrc EQ 0.

        ENDIF.
        SELECT SINGLE MAX( otwfno )
          FROM zzt_hr_otdetails
          INTO lv_otwfno
            WHERE pernr EQ lv_pernr.
        IF sy-subrc EQ 0.

        ENDIF.
        ADD 1 TO lv_otwfno.

*     checks availability of OT hours per date
        LOOP AT gt_ot_details INTO ls_ot_details.
          CLEAR:  lv_days,
                  lv_hours,
                  lv_otctr,
                  lv_currdate,
                  lv_reqhour,
                  lv_timein,
                  lv_aprmins,
                  lv_pndmins,
                  lv_nonworking,
                  lv_reqmins,
                  lv_timeout,
                  lv_starttime,
                  lv_endtime,
                  lv_index,
                  lv_error,
                  ls_dates,
                  ls_ot,
                  ls_t550a,
                  ls_day,
                  ls_pa2003,
                  ls_pa0007,
                  ls_otupd,
                  ls_otcheck,
                  ls_pa0001,                                              " + CDELAPENA 02/05/2014
                  ls_t001p.                                               " + CDELAPENA 02/05/2014

*-- Start Add CDELAPENA 02/05/2014
          SELECT SINGLE *
            FROM pa0001
            INTO ls_pa0001
           WHERE pernr = lv_pernr
             AND endda = '99991231'.

          IF ls_pa0001 IS NOT INITIAL.

            SELECT SINGLE *
              FROM t001p
              INTO ls_t001p
             WHERE werks = ls_pa0001-werks
               AND btrtl = ls_pa0001-btrtl.

          ENDIF.
*-- End Add CDELAPENA 02/05/2014

*         Check if holiday
          IF ls_t001p IS NOT INITIAL.                                     " + CDELAPENA 02/05/2014
            CALL FUNCTION 'DAY_ATTRIBUTES_GET'
              EXPORTING
*               factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*               holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
                holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
                date_from                  = ls_ot_details-start_date
                date_to                    = ls_ot_details-start_date
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
                " Do Nothing
              ENDIF.
            ENDIF.
          ENDIF.
*         get employee working hours
          CALL FUNCTION 'CATT_PERSONAL_WORKING_TIMES'
            EXPORTING
              pernr         = lv_pernr
              date          = ls_ot_details-start_date
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

          PERFORM f_convert_time USING ls_ot_details-otstart_time
                                 CHANGING lv_starttime.
          PERFORM f_convert_time USING ls_ot_details-otend_time
                                 CHANGING lv_endtime.
          PERFORM f_convert_time USING ls_ot_details-timein
                                 CHANGING lv_timein.
          PERFORM f_convert_time USING ls_ot_details-timeout
                                 CHANGING lv_timeout.

          ls_otupd-pernr = lv_pernr.
          ls_otupd-req_date = sy-datum.
          MOVE ls_ot_details-req_no_of_hours TO lv_reqhour.
          PERFORM f_convert_to_minutes  USING lv_reqhour
                                        CHANGING lv_reqmins.
          IF ls_ot_details-start_date EQ ls_ot_details-end_date.

            IF lv_nonworking IS INITIAL.
              IF lv_starttime EQ ls_t550a-sobeg AND lv_endtime EQ ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_starttime EQ ls_t550a-sobeg.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_endtime EQ ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_starttime GT ls_t550a-sobeg AND lv_starttime LT ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_endtime GT ls_t550a-sobeg AND lv_endtime LT ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_starttime LT ls_t550a-sobeg AND lv_endtime LT ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF lv_starttime LT ls_t550a-sobeg AND lv_endtime GT ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
*-- Start Add CDELAPENA 02/05/2014
              IF lv_starttime BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.

              IF lv_endtime BETWEEN ls_t550a-sobeg AND ls_t550a-soend.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-006.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
*-- End Add CDELAPENA 02/05/2014

            ELSE.
              " Do Nothing
            ENDIF.
*             Added checking of duplicate OT request. Check all entries in ZZT_HR_OTDETAILS for same or overlapping pending or approved entries
            CLEAR: ls_otcheck.
            LOOP AT lt_ot INTO ls_otcheck WHERE ot_date EQ ls_ot_details-start_date
                                            AND otstatus NE co_rejected.
              IF lv_starttime EQ ls_otcheck-otstart_time AND lv_endtime EQ ls_otcheck-otend_time.
                CLEAR:ls_return.
                ls_return-type = co_e.
                ls_return-message = text-007.
                APPEND ls_return TO gt_return.
                lv_error = co_x.
                EXIT.
              ENDIF.
              IF ls_otcheck-otstart_date EQ ls_otcheck-otend_date.
                IF lv_starttime EQ ls_otcheck-otstart_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime GT ls_otcheck-otstart_time AND lv_starttime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime GT ls_otcheck-otstart_time AND lv_endtime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime GT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.

              ELSE.
                IF ls_otcheck-ot_date EQ ls_otcheck-otstart_date.
                  IF lv_starttime GE ls_otcheck-otstart_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_endtime GT ls_otcheck-otstart_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime GT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime LT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ELSEIF ls_otcheck-ot_date EQ ls_otcheck-otend_date.
                  IF lv_starttime LT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_endtime LT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime GT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                  IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime LT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF lv_error EQ co_x.
              EXIT.
            ENDIF.
            READ TABLE lt_dates INTO ls_dates WITH KEY date = ls_ot_details-start_date.
            LOOP AT lt_ot INTO ls_ot WHERE ot_date = ls_ot_details-start_date.
              REFRESH: lt_ottmp.
              CLEAR:  lv_index,
                      lv_tmphours,
                      lv_tmptime,
                      lv_ctrind,
                      lv_tmpchar,
                      lv_tmpchar2,
                      lv_tmphours2,
                      ls_ottmp.
              lv_tmphours = ls_ot-req_no_of_hours.
              APPEND LINES OF lt_ot TO lt_ottmp.
              SORT lt_ottmp BY pernr otwfno entryno.
              LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                AND otwfno EQ ls_ot-otwfno
                                AND entryno EQ ls_ot-entryno.
                ADD 1 TO lv_ctrind.
                IF ls_ottmp-otstart_date EQ ls_ot_details-start_date.
                  lv_tmptime = ls_ot-otstart_time(2).
                  lv_tmphours = 24 - lv_tmptime.
                ELSE.
                  lv_tmpchar = ls_ot-otend_time(2).
                  lv_tmpchar2 = ls_ot-otend_time+2(2).
                  CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                  lv_tmphours = lv_tmphours2.
                ENDIF.
              ENDLOOP.
              IF lv_ctrind GT 1.
                lv_tmphours = lv_tmphours.
              ELSE.
                lv_tmphours = ls_ot-req_no_of_hours.
              ENDIF.
              IF ls_ot-otstatus EQ co_approved.
                PERFORM f_convert_to_minutes  USING ls_ot-approvedhours
                                              CHANGING lv_aprmins.
                lv_hours = lv_hours + lv_aprmins.
              ELSEIF ls_ot-otstatus EQ co_pending.
                PERFORM f_convert_to_minutes  USING lv_tmphours
                                              CHANGING lv_pndmins.
                lv_hours = lv_hours + lv_pndmins.
              ENDIF.
            ENDLOOP.
            lv_hours = lv_hours + ls_dates-hours + lv_reqmins.
            IF ls_ot-purpose EQ 16 OR ls_ot_details-purpose EQ 16.

            ELSE.
              IF lv_nonworking EQ co_x.
                IF lv_hours GT 480.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  CONCATENATE text-002 ls_ot_details-start_date INTO ls_return-message SEPARATED BY space.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
              ELSE.
                IF lv_hours GT 240.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  CONCATENATE text-002 ls_ot_details-start_date INTO ls_return-message SEPARATED BY space.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
              ENDIF.

            ENDIF.
            IF ls_dates IS NOT INITIAL.
              ls_dates-hours = ls_dates-hours + lv_reqmins.
              MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
            ELSE.
              ls_dates-date =  ls_ot_details-start_date.
              ls_dates-hours = lv_reqmins.
              APPEND ls_dates TO lt_dates.
            ENDIF.

            ADD 1 TO lv_otlastno.
            ADD 1 TO lv_otctr.
            ls_otupd-ot_date = ls_ot_details-start_date.
            ls_otupd-entryno = lv_otlastno.
            ls_otupd-otctr = lv_otctr.
            ls_otupd-timein = lv_timein.
            ls_otupd-otstart_date = ls_ot_details-start_date.
            ls_otupd-otend_date = ls_ot_details-end_date.
            ls_otupd-timeout = lv_timeout.
            ls_otupd-otstart_time = lv_starttime.
            ls_otupd-otend_time = lv_endtime.
            ls_otupd-req_no_of_hours = lv_reqhour.
            ls_otupd-purpose = ls_ot_details-purpose.
            ls_otupd-reason = ls_ot_details-reason.
            ls_otupd-otstatus = co_pending.
*            ls_otupd-useraction = ls_ot_details-action.
*          ls_otupd-APPROVEDHOURS = ls_ot_details-approvedhours.
            APPEND ls_otupd TO lt_otupd.
*          MODIFY zzt_hr_otdetails FROM ls_otupd.
          ELSE.
*             Added checking of duplicate OT request. Check all entries in ZZT_HR_OTDETAILS for same or overlapping pending or approved entries
            CLEAR: ls_otcheck.
            LOOP AT lt_ot INTO ls_otcheck WHERE ( ot_date EQ ls_ot_details-start_date
                                            OR    ot_date EQ ls_ot_details-end_date )
                                            AND otstatus NE co_rejected.
              IF ls_ot_details-start_date EQ ls_otcheck-otstart_date AND ls_ot_details-end_date EQ ls_otcheck-otend_date.
                IF lv_starttime EQ ls_otcheck-otstart_time AND lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime EQ ls_otcheck-otstart_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime GT ls_otcheck-otstart_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime GT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_otcheck-otstart_time AND lv_endtime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
              ELSEIF ls_otcheck-ot_date EQ ls_ot_details-start_date.
                IF lv_starttime EQ ls_otcheck-otstart_time AND lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime EQ ls_otcheck-otstart_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime GE ls_otcheck-otstart_time AND lv_starttime LT ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF ls_otcheck-otstart_date NE ls_otcheck-otend_date.
                  IF lv_starttime GT ls_otcheck-otstart_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ENDIF.
              ELSEIF ls_otcheck-ot_date EQ ls_ot_details-end_date.
                IF lv_starttime EQ ls_otcheck-otstart_time AND lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime EQ ls_otcheck-otstart_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime EQ ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime GT ls_otcheck-otstart_time AND lv_endtime LE ls_otcheck-otend_time.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-007.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF ls_otcheck-otstart_date NE ls_otcheck-otend_date.
                  IF lv_endtime LT ls_otcheck-otend_time.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    ls_return-message = text-007.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF lv_error = co_x.
              EXIT.
            ENDIF.
            CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
              EXPORTING
                begda    = ls_ot_details-start_date
                endda    = ls_ot_details-end_date
*               TAB_MODE = ' '
              IMPORTING
                days     = lv_days.

            lv_currdate = ls_ot_details-start_date.
            ADD 1 TO lv_otlastno.
            DO lv_days TIMES.
              CLEAR: ls_dates,
                     ls_ot,
                     ls_otupd,
                     ls_day,
                     lv_nonworking,
                     lv_hours,
                     lv_aprmins,
                     lv_pndmins.

              CALL FUNCTION 'DAY_ATTRIBUTES_GET'
                EXPORTING
*                 factory_calendar           = 'PH' " - CDELAPENA 02/05/2014
*                 holiday_calendar           = 'PH' " - CDELAPENA 02/05/2014
                  holiday_calendar           = ls_t001p-mofid               " + CDELAPENA 02/05/2014
                  date_from                  = lv_currdate
                  date_to                    = lv_currdate
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
*             added 01/08/2014
              IF lv_nonworking IS INITIAL.
                IF lv_starttime EQ ls_t550a-sobeg AND lv_endtime EQ ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime EQ ls_t550a-sobeg.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime EQ ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_endtime GT ls_t550a-sobeg AND lv_endtime LT ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime GT ls_t550a-sobeg AND lv_starttime LT ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_t550a-sobeg AND lv_endtime GT ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
                IF lv_starttime LT ls_t550a-sobeg AND lv_endtime LT ls_t550a-soend.
                  CLEAR:ls_return.
                  ls_return-type = co_e.
                  ls_return-message = text-006.
                  APPEND ls_return TO gt_return.
                  lv_error = co_x.
                  EXIT.
                ENDIF.
              ELSE.
              ENDIF.
              IF lv_error = co_x.
                EXIT.
              ENDIF.
*             end of add 01/08/2014
              ls_otupd-pernr = lv_pernr.
              ls_otupd-req_date = sy-datum.
              READ TABLE lt_dates INTO ls_dates WITH KEY date = lv_currdate.
              LOOP AT lt_ot INTO ls_ot WHERE ot_date = lv_currdate.
                REFRESH: lt_ottmp.
                CLEAR:  lv_index,
                        lv_tmphours,
                        lv_ctrind,
                        lv_tmptime,
                        lv_tmpchar,
                      lv_tmpchar2,
                      lv_tmphours2,
                        ls_ottmp.
                lv_tmphours = ls_ot-req_no_of_hours.
                APPEND LINES OF lt_ot TO lt_ottmp.
                SORT lt_ottmp BY pernr otwfno entryno.
                LOOP AT lt_ottmp INTO ls_ottmp WHERE pernr EQ ls_ot-pernr
                                AND otwfno EQ ls_ot-otwfno
                                AND entryno EQ ls_ot-entryno.
                  ADD 1 TO lv_ctrind.
                  IF ls_ottmp-otstart_date EQ ls_ot_details-start_date.
                    lv_tmptime = ls_ot-otstart_time(2).
                    lv_tmphours = 24 - lv_tmptime.
                  ELSE.
                    lv_tmpchar = ls_ot-otend_time(2).
                    lv_tmpchar2 = ls_ot-otend_time+2(2).
                    CONCATENATE lv_tmpchar '.' lv_tmpchar2 INTO lv_tmphours2.
                    lv_tmphours = lv_tmphours2.
                  ENDIF.
                ENDLOOP.
                IF lv_ctrind GT 1.
                  lv_tmphours = lv_tmphours.
                ELSE.
                  lv_tmphours = ls_ot-req_no_of_hours.
                ENDIF.
                IF ls_ot-otstatus EQ co_approved.
                  PERFORM f_convert_to_minutes  USING ls_ot-approvedhours
                                                CHANGING lv_aprmins.
                  lv_hours = lv_hours + lv_aprmins.
                ELSEIF ls_ot-otstatus EQ co_pending.
                  PERFORM f_convert_to_minutes  USING lv_tmphours
                                                CHANGING lv_pndmins.
                  lv_hours = lv_hours + lv_pndmins.
                ENDIF.
              ENDLOOP.
              ADD 1 TO lv_otctr.
              CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
                EXPORTING
                  date_from       = ls_ot_details-start_date
                  date_to         = ls_ot_details-start_date
                  time_from       = lv_starttime"ls_otpending-otstart_time
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

                IF lv_otctr EQ 1.
                  lv_reqmins2 = lv_deltamin.
                ELSE.
                  lv_tmp = lv_endtime(2).
                  lv_tmp2 = lv_endtime+2(2).
                  lv_tmp3 = lv_tmp + ( lv_tmp2 / 100 ).
                  lv_hours2 = lv_tmp3.
                  PERFORM f_convert_to_minutes  USING lv_hours2
                                         CHANGING lv_reqmins2.
                ENDIF.

              ENDIF.

              lv_hours = lv_hours + ls_dates-hours + lv_reqmins2.
              IF ls_ot-purpose EQ 16 OR ls_ot_details-purpose EQ 16.

              ELSE.
                IF lv_nonworking EQ co_x.
                  IF lv_hours GT 480.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    CONCATENATE text-002 lv_currdate INTO ls_return-message SEPARATED BY space.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ELSE.
                  IF lv_hours GT 240.
                    CLEAR:ls_return.
                    ls_return-type = co_e.
                    CONCATENATE text-002 lv_currdate INTO ls_return-message SEPARATED BY space.
                    APPEND ls_return TO gt_return.
                    lv_error = co_x.
                    EXIT.
                  ENDIF.
                ENDIF.

              ENDIF.
              IF ls_dates IS NOT INITIAL.
                ls_dates-hours = ls_dates-hours + lv_reqmins2.
                MODIFY TABLE lt_dates FROM ls_dates TRANSPORTING hours.
              ELSE.
                ls_dates-date =  lv_currdate.
                ls_dates-hours = lv_reqmins2.
                APPEND ls_dates TO lt_dates.
              ENDIF.


              ls_otupd-ot_date = lv_currdate.
              ls_otupd-entryno = lv_otlastno.
              ls_otupd-otctr = lv_otctr.
              ls_otupd-timein = lv_timein.
              ls_otupd-timeout = lv_timeout.
              ls_otupd-otstart_date = ls_ot_details-start_date.
              ls_otupd-otend_date = ls_ot_details-end_date.
              ls_otupd-otstart_time = lv_starttime.
              ls_otupd-otend_time = lv_endtime.
              ls_otupd-req_no_of_hours = lv_reqhour.
              ls_otupd-purpose = ls_ot_details-purpose.
              ls_otupd-reason = ls_ot_details-reason.
              ls_otupd-otstatus = co_pending.
*              ls_otupd-useraction = ls_ot_details-action.
*          ls_otupd-APPROVEDHOURS = ls_ot_details-approvedhours.
              APPEND ls_otupd TO lt_otupd.
*          MODIFY zzt_hr_otdetails FROM ls_otupd.
              lv_currdate = lv_currdate + 1.
            ENDDO.
          ENDIF.

        ENDLOOP.
        IF lv_error EQ co_x.
          EXIT.
        ELSE.
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*            EXPORTING
*              nr_range_nr             = co_range
*              object                  = co_object
**             QUANTITY                = '1'
**             SUBOBJECT               = ' '
**             TOYEAR                  = '0000'
**             IGNORE_BUFFER           = ' '
*            IMPORTING
*              number                  = lv_serial_no
**             QUANTITY                =
**             RETURNCODE              =
*            EXCEPTIONS
*              interval_not_found      = 1
*              number_range_not_intern = 2
*              object_not_found        = 3
*              quantity_is_0           = 4
*              quantity_is_not_1       = 5
*              interval_overflow       = 6
*              buffer_overflow         = 7
*              OTHERS                  = 8.
*          IF sy-subrc <> 0.
** Implement suitable error handling here
*          ENDIF.

          LOOP AT lt_otupd INTO ls_otupd.
*          MOVE-CORRESPONDING ls_otupd TO ls_otwf.
*          APPEND ls_otwf TO lt_otwf.
            CLEAR: ls_event_container-element.
            ls_event_container-element = 'GT_OTDETAILS'.
            ls_event_container-value = ls_otupd-entryno.
            APPEND ls_event_container TO lt_event_container.
            ls_otupd-otwfno = lv_otwfno.
            MODIFY lt_otupd FROM ls_otupd.
          ENDLOOP.
          MODIFY zzt_hr_otdetails FROM TABLE lt_otupd.

        ENDIF.
        CALL FUNCTION 'DEQUEUE_EZZZT_HR_OTDET'
*     EXPORTING
*       MODE_ZZT_HR_OTDETAILS       = 'E'
*       MANDT                       = SY-MANDT
*       PERNR                       =
*       OTWFNO                      =
*       OT_DATE                     =
*       ENTRYNO                     =
*       OTCTR                       =
*       X_PERNR                     = ' '
*       X_OT_DATE                   = ' '
*       X_ENTRYNO                   = ' '
*       X_OTCTR                     = ' '
*       _SCOPE                      = '3'
*       _SYNCHRON                   = ' '
*       _COLLECT                    = ' '
            .
        ls_event_container-element = 'USERID'.
        ls_event_container-value = i_user.
        APPEND ls_event_container TO lt_event_container.

        ls_event_container-element = 'PERNR'.
        ls_event_container-value = lv_pernr.
        APPEND ls_event_container TO lt_event_container.

        ls_event_container-element = 'OTWFNO'.
        ls_event_container-value = lv_otwfno.
        APPEND ls_event_container TO lt_event_container.

*        lv_objectkey = lv_pernr.
        CONCATENATE lv_otwfno lv_pernr INTO lv_objectkey.

        CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
          EXPORTING
            object_type       = 'ZHCMOTREQ'
            object_key        = lv_objectkey
            event             = 'OTREQUESTED'
            commit_work       = 'X'
            event_language    = sy-langu
            language          = sy-langu
            user              = i_user
*           IFS_XML_CONTAINER =
          IMPORTING
            return_code       = lv_subrc
            event_id          = lv_event_id
          TABLES
            input_container   = lt_event_container
            message_lines     = lt_messages_line
            message_struct    = lt_message_struct.
*          READ TABLE lt_messages_line INTO ls_messages_line INDEX 1.
*          IF ls_messages_line-msg_type EQ c_s.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = c_x.
*            COMMIT WORK AND WAIT.
*            ls_prheader-zhasworkflow = c_x.
*            ls_prheader-zstartworkflow = c_x.
*            ls_prheader-relgroup = ly_newrelgrp.
*            ls_prheader-relstrat = ly_newrelstrat.
*            MODIFY zmm_pr_table FROM ls_prheader.
*            REFRESH: gt_return.
*            CLEAR: ls_return.
*            ls_return-type = 'S'.
*            ls_return-id   = ''.
*            ls_return-number  = ''.
*            CONCATENATE 'Purchase Requisition ' i_number 'has been changed and routed for approval' INTO ls_return-message SEPARATED BY space.
*            APPEND ls_return TO gt_return.
*          ENDIF.
      ELSE.
        ls_return-type = co_e.
        ls_return-message = text-001.
        APPEND ls_return TO gt_return.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFUNCTION.
