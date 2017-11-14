*&---------------------------------------------------------------------*
*& Report  ZTEST_GLENN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest_glenn.

PARAMETER: pa_job TYPE tbtcjob-jobname DEFAULT 'TEST'.

DATA: lv_jobcount TYPE tbtcjob-jobcount.

CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname          = pa_job
  IMPORTING
    jobcount         = lv_jobcount
  EXCEPTIONS
    cant_create_job  = 1
    invalid_job_data = 2
    jobname_missing  = 3
    OTHERS           = 4.

IF sy-subrc EQ 0.
*  SUBMIT ZTEST_GLENN_PROGRAM
*     USER sy-uname
*     VIA JOB pa_job NUMBER lv_jobcount
*     AND RETURN.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
      authcknam               = sy-uname
      jobcount                = lv_jobcount
      jobname                 = pa_job
      report                  = 'ZTEST_GLENN_PROGRAM'
*     VARIANT                 = ' '
    EXCEPTIONS
      bad_priparams           = 1
      bad_xpgflags            = 2
      invalid_jobdata         = 3
      jobname_missing         = 4
      job_notex               = 5
      job_submit_failed       = 6
      lock_failed             = 7
      program_missing         = 8
      prog_abap_and_extpg_set = 9
      OTHERS                  = 10.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = pa_job
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
  ENDIF.


ELSE.
  WRITE:/ 'Error', sy-subrc.
ENDIF.