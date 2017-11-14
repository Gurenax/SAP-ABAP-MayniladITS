REPORT  ZTEST.

TABLES: zosb_pua.

DATA: ls_zosb_pua  TYPE zosb_pua.

ls_zosb_pua-MRID = '00000000000125357336'.
ls_zosb_pua-CAN = '000051829798'.
ls_zosb_pua-READING_DATE = '20150218'.
ls_zosb_pua-PUA = '0'.

INSERT INTO zosb_pua VALUES LS_ZOSB_PUA.

IF sy-subrc eq 0.
  WRITE:/ 'Success'.
ELSE.
  WRITE:/ 'Fail'.
ENDIF.