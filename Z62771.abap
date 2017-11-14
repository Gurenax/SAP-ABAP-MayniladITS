*&---------------------------------------------------------------------*
*& Report  Z62771
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z62771.


TYPES: BEGIN OF ty_bseg,
        belnr TYPE bseg-belnr,
        lifnr TYPE bseg-lifnr,
        dmbtr TYPE bseg-dmbtr,
        augbl TYPE bseg-augbl,
       END OF ty_bseg,

       BEGIN OF ty_output,
        belnr TYPE bseg-belnr,
        budat TYPE bkpf-budat,
        dmbtr TYPE bseg-dmbtr,
        augbl TYPE bseg-augbl,
        lifnr TYPE bseg-lifnr,
        name1 TYPE lfa1-name1,
        name2 TYPE lfa1-name2,
        bktxt TYPE bkpf-bktxt,
       END OF ty_output.


DATA: gt_bseg       TYPE TABLE OF ty_bseg,
      gt_output     TYPE TABLE OF ty_output.

DATA: gv_kostl      TYPE bseg-kostl,
      gv_dmbtr_str  TYPE string,
      gv_string     TYPE string.


FIELD-SYMBOLS: <fs_bseg>   TYPE ty_bseg,
               <fs_output> TYPE ty_output.

PARAMETERS: pa_kostl TYPE bseg-kostl      DEFAULT '901000200',
            pa_file  TYPE rlgrap-filename DEFAULT '\\172.18.1.240\repuserdata\Glenn\Output.txt'.

START-OF-SELECTION.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pa_kostl
    IMPORTING
      output = gv_kostl.

  SELECT belnr lifnr dmbtr augbl
  INTO TABLE gt_bseg
  FROM bseg
  WHERE kostl EQ gv_kostl.

  IF sy-subrc EQ 0.
    LOOP AT gt_bseg ASSIGNING <fs_bseg>.
      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.

      IF <fs_bseg>-belnr IS NOT INITIAL.
        SELECT SINGLE budat bktxt
        INTO (<fs_output>-budat, <fs_output>-bktxt)
        FROM bkpf
        WHERE belnr EQ <fs_bseg>-belnr.
      ENDIF.

      IF <fs_bseg>-lifnr IS NOT INITIAL.
        SELECT SINGLE name1 name2
        INTO (<fs_output>-name1, <fs_output>-name2)
        FROM lfa1
        WHERE lifnr EQ <fs_bseg>-lifnr.
      ENDIF.

      <fs_output>-belnr = <fs_bseg>-belnr.
      <fs_output>-lifnr = <fs_bseg>-lifnr.
      <fs_output>-dmbtr = <fs_bseg>-dmbtr.
      <fs_output>-augbl = <fs_bseg>-augbl.
    ENDLOOP.
    UNASSIGN <fs_bseg>.

    OPEN DATASET pa_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc eq 0.
      LOOP AT gt_output ASSIGNING <fs_output>.
        CLEAR: gv_dmbtr_str, gv_string.

        MOVE <fs_output>-dmbtr TO gv_dmbtr_str.

        CONCATENATE <fs_output>-belnr
                    <fs_output>-budat
                    gv_dmbtr_str
                    <fs_output>-augbl
                    <fs_output>-lifnr
                    <fs_output>-name1
                    <fs_output>-name2
                    <fs_output>-bktxt INTO gv_string SEPARATED BY '|'.

        TRANSFER gv_string to pa_file.
      ENDLOOP.
      UNASSIGN <fs_output>.
      CLOSE DATASET pa_file.
    ENDIF.
  ENDIF.