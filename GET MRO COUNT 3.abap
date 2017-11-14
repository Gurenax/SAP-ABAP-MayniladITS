*&---------------------------------------------------------------------*
*& Report  ZTEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest.
TABLES: te422, eabl.

TYPES: BEGIN OF ty_mru,
        termschl TYPE te422-termschl,
        portion  TYPE te422-portion,
       END OF ty_mru,

      BEGIN OF ty_anlage,
        anlage TYPE eanlh-anlage,
      END OF ty_anlage,

      BEGIN OF ty_output,
        ableinh   TYPE eanlh-ableinh,
        adatsoll  TYPE eabl-adatsoll,
      END OF ty_output.

DATA: lt_eabl       TYPE STANDARD TABLE OF eabl,
      lt_eabl_final TYPE STANDARD TABLE OF eabl,
      lt_mru        TYPE TABLE OF ty_mru,
      lt_anlage     TYPE TABLE OF ty_anlage,
      lt_output     TYPE TABLE OF ty_output.

DATA: lv_logiknr TYPE egerh-logiknr,
      lv_equnr   TYPE egerh-equnr,
      lv_anlage  TYPE eanlh-anlage,
      lv_ableinh TYPE eanlh-ableinh,
      lv_portion TYPE te422-portion,
      lv_count   TYPE i VALUE 0,
      lv_len     TYPE i,
      lv_day     TYPE c LENGTH 2,
      lv_adatsoll TYPE c LENGTH 8.

FIELD-SYMBOLS: <fs_eabl>    TYPE eabl,
               <fs_mru>     TYPE ty_mru,
               <fs_anlage>  TYPE ty_anlage,
               <fs_output>  TYPE ty_output.

*SELECT-OPTIONS: ,
*                so_asol FOR EABL-adatsoll.

PARAMETERS: pa_port    TYPE te422-portion,
            pa_month   TYPE c LENGTH 2,
            pa_year    TYPE c LENGTH 4.

lv_len = strlen( pa_port ).
IF lv_len EQ 7.
  lv_len = lv_len - 2.
  lv_day = pa_port+lv_len(2).
ELSE.
  lv_len = lv_len - 1.
  lv_day = pa_port+lv_len(2).
  CONCATENATE '0' lv_day INTO lv_day.
ENDIF.
WRITE:/ 'Day', lv_day.
CONCATENATE pa_year pa_month lv_day INTO lv_adatsoll.

REFRESH lt_anlage.

SELECT termschl portion
  INTO TABLE lt_mru
  FROM te422
 WHERE portion EQ pa_port.

IF sy-subrc EQ 0.
  SORT lt_mru BY termschl portion ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_mru COMPARING termschl.

  LOOP AT lt_mru ASSIGNING <fs_mru>.
    SELECT anlage
      APPENDING TABLE lt_anlage
      FROM eanlh
     WHERE ableinh EQ <fs_mru>-termschl.
  ENDLOOP.
  UNASSIGN <fs_mru>.

  SORT lt_anlage BY anlage ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_anlage COMPARING anlage.

  LOOP AT lt_anlage ASSIGNING <fs_anlage>.
    CLEAR: lv_logiknr.
    SELECT SINGLE logiknr
      INTO lv_logiknr
      FROM eastl
     WHERE anlage EQ <fs_anlage>-anlage
       AND bis EQ '99991231'.

    IF sy-subrc EQ 0.
      CLEAR: lv_equnr.
      SELECT SINGLE equnr
        INTO lv_equnr
        FROM egerh
       WHERE logiknr EQ lv_logiknr
         AND bis EQ '99991231'.

      IF sy-subrc EQ 0.
        SELECT *
        APPENDING TABLE lt_eabl
        FROM eabl
        WHERE equnr EQ lv_equnr
          AND adatsoll EQ lv_adatsoll
          AND ablstat EQ '0'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_anlage>.
ENDIF.

SORT lt_eabl BY ablbelnr ASCENDING.
DESCRIBE TABLE lt_eabl LINES lv_count.
WRITE:/ lv_count.

LOOP AT lt_eabl ASSIGNING <fs_eabl>.
  CLEAR: lv_logiknr.
  SELECT SINGLE logiknr
    INTO lv_logiknr
    FROM egerh
   WHERE equnr EQ <fs_eabl>-equnr
     AND bis EQ '99991231'.

  IF sy-subrc EQ 0.
    CLEAR: lv_anlage.
    SELECT SINGLE anlage
      INTO lv_anlage
      FROM eastl
     WHERE logiknr EQ lv_logiknr
       AND bis EQ '99991231'.

    IF sy-subrc EQ 0.
      CLEAR: lv_ableinh.
      SELECT SINGLE ableinh
        INTO lv_ableinh
        FROM eanlh
       WHERE anlage EQ lv_anlage
         AND bis EQ '99991231'.
    ENDIF.
  ENDIF.

  APPEND INITIAL LINE TO lt_output ASSIGNING <fs_output>.
  <fs_output>-ableinh = lv_ableinh.
  <fs_output>-adatsoll = <fs_eabl>-adatsoll.
ENDLOOP.
UNASSIGN <fs_eabl>.

SORT lt_output BY ableinh ASCENDING adatsoll ASCENDING.
CLEAR: lv_ableinh, lv_count.

LOOP AT lt_output ASSIGNING <fs_output>.
  IF lv_ableinh IS INITIAL.
    lv_count = 1.
    lv_ableinh = <fs_output>-ableinh.
  ELSEIF <fs_output>-ableinh EQ lv_ableinh.
    lv_count = lv_count + 1.
  ELSE.
    WRITE:/ lv_ableinh, <fs_output>-adatsoll, lv_count.
    lv_ableinh = <fs_output>-ableinh.
    lv_count = 1.
  ENDIF.
ENDLOOP.
WRITE:/ lv_ableinh, <fs_output>-adatsoll, lv_count.
UNASSIGN <fs_output>.