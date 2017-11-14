REPORT zprg_glenn.

data: gv_report1     TYPE RLGRAP-FILENAME VALUE '\\172.18.1.240\repuserdata\10092012_PaymentFiles\Output\REPORT_PAYMENT_CENTER.XLS',
      gv_report2     TYPE RLGRAP-FILENAME VALUE '\\172.18.1.240\repuserdata\10092012_PaymentFiles\paytfile.xls'.

data: gv_string      TYPE string.


DATA : IT_TAB TYPE standard TABLE OF ALSMEX_TABLINE,
       IT_TAB2 TYPE standard TABLE OF ALSMEX_TABLINE,
       wa_tab type ALSMEX_TABLINE.


*OPEN DATASET gv_report1 FOR INPUT IN TEXT MODE ENCODING DEFAULT.
*IF sy-subrc EQ 0.
*  DO.
*    READ DATASET gv_report1 INTO wa_tab.
*    append wa_tab to IT_TAB.
**    READ DATASET gv_report1 INTO gv_string.
**    WRITE:/ gv_string.
*    IF sy-subrc <> 0. EXIT. ENDIF.
*  ENDDO.
*  CLOSE DATASET gv_report1.
*ENDIF.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
EXPORTING
  FILENAME = gv_report1
  I_BEGIN_COL = '1'
  I_BEGIN_ROW = '2'
  I_END_COL = '8'
  I_END_ROW = '23'
TABLES
  INTERN = IT_TAB
EXCEPTIONS
  INCONSISTENT_PARAMETERS = 1
  UPLOAD_OLE = 2
  OTHERS = 3.

IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

LOOP AT IT_TAB into wa_tab.
  CASE wa_tab-COL.
    WHEN '0001'.
      WRITE: wa_tab-VALUE.
    WHEN '0004'.
      WRITE: wa_tab-VALUE.
    WHEN '0005'.
      WRITE: wa_tab-VALUE.
  ENDCASE.
ENDLOOP.

ULINE:/.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
EXPORTING
  FILENAME = gv_report2
  I_BEGIN_COL = '1'
  I_BEGIN_ROW = '1'
  I_END_COL = '7'
  I_END_ROW = '22'
TABLES
  INTERN = IT_TAB2
EXCEPTIONS
  INCONSISTENT_PARAMETERS = 1
  UPLOAD_OLE = 2
  OTHERS = 3.

IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

LOOP AT IT_TAB2 into wa_tab.
  CASE wa_tab-COL.
    WHEN '0001'.
      WRITE: wa_tab-VALUE.
    WHEN '0006'.
      WRITE: wa_tab-VALUE.
    WHEN '0007'.
      WRITE: wa_tab-VALUE.
  ENDCASE.
ENDLOOP.