*&---------------------------------------------------------------------*
*& Report  ZTEST_IW45
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTEST_IW45.

TABLES: bseg.

DATA: gt_confirmations type table of BAPI_CONF_KEY.

DATA: lv_confirmation type BAPI_CONF_KEY-CONF_NO,
      lv_conf_counter type BAPI_CONF_KEY-CONF_CNT,
      lv_return       type BAPIRET2.

FIELD-SYMBOLS: <fs_confirmations> type bapi_conf_key.

SELECT-OPTIONS: s_ord FOR bseg-aufnr.


CALL FUNCTION 'BAPI_ALM_CONF_GETLIST'
* EXPORTING
*   OPERATION           =
*   SUBOPERATION        =
*   IN_PROCESS          =
*   COMPLETED           =
* IMPORTING
*   RETURN              =
  TABLES
    ORDER_RANGE         = s_ord
*   CONF_RANGE          =
    CONFIRMATIONS       = gt_confirmations.


IF gt_confirmations IS NOT INITIAL.
  LOOP AT GT_CONFIRMATIONS ASSIGNING <FS_CONFIRMATIONS> WHERE REVERSED IS INITIAL
                                                          AND REV_CONF_CNT IS INITIAL.
    CLEAR: lv_confirmation, lv_conf_counter.

    lv_confirmation = <FS_CONFIRMATIONS>-CONF_NO.
    lv_conf_counter = <FS_CONFIRMATIONS>-CONF_CNT.

  ENDLOOP.

  CALL FUNCTION 'BAPI_ALM_CONF_CANCEL'
    EXPORTING
      CONFIRMATION              = lv_confirmation
      CONFIRMATIONCOUNTER       = lv_conf_counter
*     POSTGDATE                 =
      CONFTEXT                  = 'Test Reason'
    IMPORTING
      RETURN                    = lv_return
*     LOCKED                    =
*     CREATED_CONF_NO           =
*     CREATED_CONF_COUNT        =
            .

  COMMIT WORK.

ENDIF.