*&---------------------------------------------------------------------*
*& Report  ZTEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTEST.
TABLES: TE422, EABL.

TYPES: BEGIN OF ty_mru,
        termschl type te422-TERMSCHL,
        portion  type te422-portion,
       END OF ty_mru,

      BEGIN OF ty_anlage,
        anlage type eanlh-anlage,
      END OF ty_anlage.

DATA: lt_eabl       TYPE STANDARD TABLE OF eabl,
      lt_eabl_final TYPE STANDARD TABLE OF eabl,
      lt_mru        TYPE TABLE of ty_mru,
      lt_anlage     TYPE TABLE OF ty_anlage.

DATA: lv_logiknr TYPE egerh-logiknr,
      lv_equnr   TYPE egerh-EQUNR,
      lv_anlage  TYPE eanlh-anlage,
      lv_portion TYPE te422-portion,
      lv_count   TYPE i VALUE 0,
      lv_len     TYPE i,
      lv_day     TYPE c LENGTH 2,
      lv_adatsoll TYPE c LENGTH 8.

FIELD-SYMBOLS: <fs_eabl>    TYPE eabl,
               <fs_mru>     TYPE ty_mru,
               <fs_anlage>  TYPE ty_anlage.

*SELECT-OPTIONS: ,
*                so_asol FOR EABL-adatsoll.

PARAMETERS: pa_port    TYPE TE422-portion,
            pa_month   TYPE c LENGTH 2,
            pa_year    TYPE c LENGTH 4.

lv_len = strlen( pa_port ).
if lv_len eq 7.
  lv_len = lv_len - 2.
  lv_day = pa_port+lv_len(2).
else.
  lv_len = lv_len - 1.
  lv_day = pa_port+lv_len(2).
  concatenate '0' lv_day into lv_day.
endif.
WRITE:/ 'Day', lv_day.
CONCATENATE pa_year pa_month lv_day INTO lv_adatsoll.

REFRESH lt_anlage.

SELECT termschl portion
  INTO TABLE lt_mru
  FROM te422
 WHERE portion eq pa_port.

  IF sy-subrc eq 0.
    SORT lt_mru BY termschl portion ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_mru COMPARING TERMSCHL.

    LOOP AT lt_mru ASSIGNING <fs_mru>.
      SELECT anlage
        APPENDING TABLE lt_anlage
        FROM eanlh
       WHERE ableinh eq <fs_mru>-TERMSCHL.
    ENDLOOP.
    UNASSIGN <fs_mru>.

    SORT lt_anlage BY anlage ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_anlage COMPARING ANLAGE.

    LOOP AT lt_anlage ASSIGNING <fs_anlage>.
      CLEAR: lv_logiknr.
      SELECT SINGLE logiknr
        INTO lv_logiknr
        FROM eastl
       WHERE anlage eq <fs_anlage>-anlage
         AND bis eq '99991231'.

      IF sy-subrc eq 0.
        CLEAR: lv_equnr.
        SELECT SINGLE equnr
          INTO lv_equnr
          FROM egerh
         WHERE logiknr eq LV_LOGIKNR
           AND bis eq '99991231'.

        IF sy-subrc eq 0.
          SELECT *
          APPENDING TABLE lt_eabl
          FROM eabl
          WHERE equnr eq lv_equnr
            AND adatsoll eq lv_adatsoll
            AND ablstat EQ '0'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_anlage>.
  ENDIF.

  SORT lt_eabl BY ABLBELNR ascending.
  DESCRIBE TABLE lt_eabl LINES lv_count.
  WRITE:/ lv_count.