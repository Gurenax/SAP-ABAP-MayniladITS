*&---------------------------------------------------------------------*
*& Report  ZPRG_CHECK_TRANS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZPRG_CHECK_TRANS.

TABLES: dfkkko.

DATA: gt_dfkkko TYPE STANDARD TABLE OF dfkkko.
DATA: gv_vkont TYPE dfkkop-vkont,
      gv_hvorg TYPE dfkkop-hvorg,
      gv_tvorg TYPE dfkkop-tvorg,
      gv_string TYPE string.

FIELD-SYMBOLS: <fs_dfkkko> Type dfkkko.

CONSTANTS:  co_separator   TYPE c          VALUE cl_abap_char_utilities=>horizontal_tab.
SELECT-OPTIONS: so_fikey FOR dfkkko-fikey.
PARAMETERS:     pa_data  TYPE rlgrap-filename DEFAULT '\\saprep01.mayniladsap.com\repuserdata\Glenn\ZPRG_CHECK_TRANS\OUTPUT.txt'.

START-oF-SELECTION.

  OPEN DATASET pa_data FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc eq 0.
    SELECT *
    INTO TABLE gt_dfkkko
    FROM DFKKKO
    WHERE FIKEY IN so_fikey.

    IF sy-subrc eq 0.
      CONCATENATE 'SOURCE' 'DOCUMENT_NUMBER' 'CAN' 'MAIN_TRANS' 'SUB_TRANS' 'RECONKEY' INTO gv_string SEPARATED BY co_separator.
      TRANSFER gv_string TO pa_data.


      LOOP AT gt_dfkkko ASSIGNING <fs_dfkkko>.
        CLEAR: gv_vkont, gv_hvorg, gv_tvorg, gv_string.

        SELECT SINGLE vkont hvorg tvorg
          INTO (gv_vkont,gv_hvorg,gv_tvorg)
          FROM DFKKOP
          WHERE opbel eq <fs_dfkkko>-opbel.

        IF gv_hvorg IS NOT INITIAL.
          WRITE:/ 'OPBEL',<fs_dfkkko>-opbel, gv_vkont, gv_hvorg, gv_tvorg, <fs_dfkkko>-fikey.
          CONCATENATE 'OPBEL' <fs_dfkkko>-opbel gv_vkont gv_hvorg gv_tvorg <fs_dfkkko>-fikey INTO gv_string SEPARATED BY co_separator.
          TRANSFER gv_string TO pa_data.
        ELSE.
          SELECT SINGLE vkont hvorg tvorg
          INTO (gv_vkont,gv_hvorg,gv_tvorg)
          FROM DFKKOP
          WHERE augbl eq <fs_dfkkko>-opbel.

          WRITE:/ 'AUGBL',<fs_dfkkko>-opbel, gv_vkont, gv_hvorg, gv_tvorg, <fs_dfkkko>-fikey.
          CONCATENATE 'AUGBL' <fs_dfkkko>-opbel gv_vkont gv_hvorg gv_tvorg <fs_dfkkko>-fikey INTO gv_string SEPARATED BY co_separator.
          TRANSFER gv_string TO pa_data.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLOSE DATASET pa_data.
  ELSE.
    WRITE:/ 'File could not be opened.'.
  ENDIF.

END-OF-SELECTION.