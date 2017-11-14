*&---------------------------------------------------------------------*
*& Report  ZTEST_CHANGE_DFKKOP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTEST_CHANGE_DFKKOP.

DATA: lv_opbel type dfkkop-opbel VALUE '000104999581'.

DATA: ls_fkkko type FKKKO.
DATA: lt_FKKOP type table of fkkop,
      lt_DFKKOPK type table of DFKKOPK,
      lt_FKKOPK type table of FKKOPK,
      lt_FKKOPW type table of FKKOPW.

FIELD-SYMBOLS: <fs_fkkop> TYPE fkkop,
               <fs_dfkkopk> type dfkkopk,
               <fs_fkkopk>  type fkkopk,
               <fs_fkkopw>  type fkkopw.

CALL FUNCTION 'FKK_DOC_HEADER_SELECT_BY_OPBEL'
  EXPORTING
    I_OPBEL              = lv_opbel
*   IX_OPBEL             = ' '
*   I_COUNT_FLAG         = ' '
*   I_CLEAR_TABLE        = 'X'
*   IX_SAMPLE_FLAG       = ' '
  IMPORTING
    E_FKKKO              = ls_fkkko
*   E_COUNT              =
* TABLES
*   TP_OPBEL_RANGE       =
*   TP_FKKKO             =
*   PT_SELECTTAB         =
*   PT_ORDERTAB          =
          .

CALL FUNCTION 'FKK_BP_LINE_ITEMS_SEL_BY_OPBEL'
  EXPORTING
    I_OPBEL              = lv_opbel
*   IX_SAMPLE_FLAG       = ' '
* IMPORTING
*   E_COUNT              =
  TABLES
*   PT_SELECTTAB         =
    PT_FKKOP             = LT_FKKOP
          .

SELECT *
  INTO TABLE lt_dfkkopk
  FROM dfkkopk
  WHERE opbel eq lv_opbel.

IF sy-subrc EQ 0.
  LOOP AT lt_dfkkopk ASSIGNING <FS_DFKKOPK>.
    APPEND INITIAL LINE TO lt_fkkopk ASSIGNING <FS_FKKOPK>.
    MOVE-CORRESPONDING <FS_DFKKOPK> TO <FS_FKKOPK>.
  ENDLOOP.
  UNASSIGN <FS_DFKKOPK>.
ENDIF.

SELECT *
  INTO TABLE lt_fkkopw
  FROM dfkkopw
  WHERE opbel eq lv_opbel.


LOOP AT lt_fkkop ASSIGNING <fs_fkkop>.
  <fs_fkkop>-MANSP = ''.
ENDLOOP.
UNASSIGN <fs_fkkop>.

LOOP AT lt_fkkopw ASSIGNING <fs_fkkopw>.
  <fs_fkkopw>-MANSP = ''.
ENDLOOP.
UNASSIGN <fs_fkkopw>.

CALL FUNCTION 'FKK_CHANGE_DOC_DB'
  EXPORTING
    I_FKKKO                      = ls_fkkko
*   I_UPDATE_TASK                =
*   I_CHANGE_CLEARED_ITEMS       = ' '
*   I_BWTRIGGER_DONE             = ' '
  TABLES
    T_FKKOP                      = lt_fkkop
    T_FKKOPK                     = lt_FKKOPK
    T_FKKOPW                     = lt_FKKOPW
*   T_FKKOP_OLD                  =
*   T_FKKOPW_OLD                 =
*   T_FKKOPWH_INS                =
*   T_FKKOPWH_UPD                =
*   T_FKKOPWH_DEL                =
          .


WRITE:/ 'Done'.