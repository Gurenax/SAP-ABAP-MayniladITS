*&---------------------------------------------------------------------*
*& Report  ZPRG_GLENN
*&
*&---------------------------------------------------------------------*	NOTE: CHECK ABWBL in DFKKOP 
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZPRG_GLENN.


PARAMETERS: pa_gpart type FKKEPOSS1-GPART default '381339',
            pa_ratpl type FKKEPOSS1-RATPL default '103595693'.

DATA: IT_FKKEPOSC like FKKEPOSC.

data:  IT_SELHEAD like FKKEPOSS1 occurs 0,
       IT_CHRTAB like FKKEPOS_CHR occurs 0,
       IT_POSTAB like FKKEPOS occurs 0.


DATA: WA_IT_CHRTAB like line of IT_CHRTAB,
      WA_IT_SELHEAD like line of IT_SELHEAD,
      WA_IT_FKKEPOSC like IT_FKKEPOSC,
      WA_IT_POSTAB like line of IT_POSTAB,
      WA_IT_POSTAB2 like line of IT_POSTAB.

DATA: lv_string type string.

FIELD-SYMBOLS: <fs_fkkop> type FKKOP,
               <fs_chrtab> type FKKEPOS_CHR.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         =  pa_gpart
  IMPORTING
    OUTPUT        =  WA_IT_SELHEAD-GPART.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         =  pa_ratpl
  IMPORTING
    OUTPUT        =  WA_IT_SELHEAD-RATPL.

  WA_IT_SELHEAD-SELNR = '0001'.
  WA_IT_SELHEAD-XAWOP = 'X'.
  WA_IT_SELHEAD-XAWAP = 'X'.
  WA_IT_SELHEAD-PRATP = 'X'.
  append WA_IT_SELHEAD to IT_SELHEAD.

  clear WA_IT_FKKEPOSC.
  WA_IT_FKKEPOSC-TCODE = 'FPL9'.
  WA_IT_FKKEPOSC-PGMID = 'SAPLFKP1'.
  WA_IT_FKKEPOSC-FUNCT = 'RATPL'.
  WA_IT_FKKEPOSC-XZEBR = 'X'.
  WA_IT_FKKEPOSC-XAWOP = 'X'.
  WA_IT_FKKEPOSC-XAWAP = 'X'.
  WA_IT_FKKEPOSC-STAKZ = 'X'.
  WA_IT_FKKEPOSC-NOLST = 'X'.
  move-corresponding WA_IT_FKKEPOSC to IT_FKKEPOSC.

  CALL FUNCTION 'FKK_LINE_ITEMS_WITH_SELECTIONS'
    EXPORTING
      I_FKKEPOSC              = IT_FKKEPOSC
      I_CALL_TRANSACTION      = 'X'
    TABLES
      T_SELHEAD               = IT_SELHEAD
      T_POSTAB                = IT_POSTAB
      T_CHRTAB                = IT_CHRTAB
    EXCEPTIONS
      NO_ITEMS_FOUND          = 1
      INVALID_SELECTION       = 2
      MAXIMAL_NUMBER_OF_ITEMS = 3
      OTHERS                  = 4.

  IF SY-SUBRC eq 0.
    CLEAR lv_string.
    CONCATENATE 'OPBEL       ' 'BUDAT   ' 'FAEDN   ' 'HTEXT' INTO lv_string SEPARATED BY '|' RESPECTING BLANKS.
    WRITE:/ lv_string.
    LOOP AT IT_CHRTAB ASSIGNING <FS_CHRTAB> WHERE NUM02 eq '10'.
      CLEAR lv_string.
      CONCATENATE <FS_CHRTAB>-OPBEL <FS_CHRTAB>-BUDAT <FS_CHRTAB>-FAEDN <FS_CHRTAB>-HTEXT INTO lv_string SEPARATED BY '|' RESPECTING BLANKS.
      WRITE:/ lv_string.
    ENDLOOP.
  ELSE.
    WRITE:/ 'ERROR', sy-subrc.
  ENDIF.