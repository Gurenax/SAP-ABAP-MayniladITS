*&---------------------------------------------------------------------*
*& Report  Z_TEST_CUSTOMER_DATA_PROVIDE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_test_customer_data_provide.

DATA: lv_date  TYPE sydatum,
      lv_count TYPE i.

DATA: lt_zsisu_customer_data TYPE TABLE OF zsisu_customer_data,
      lt_return TYPE TABLE OF bapiret2.

PARAMETERS: pa_date TYPE sy-datum.

CALL FUNCTION 'Z_CUSTOMER_DATA_PROVIDE'
  EXPORTING
    iv_change_date       = pa_date
  TABLES
    it_contract_accounts = lt_zsisu_customer_data
    t_return             = lt_return.

DESCRIBE TABLE lt_zsisu_customer_data LINES lv_count.

WRITE:/ lv_count, 'records processed'.