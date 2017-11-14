*&---------------------------------------------------------------------*
*& Report  ZPRG_GLENN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZPRG_GLENN.

TABLES: dfkkko, dfkkop.
*
*02010413MP01
*03010413MP01
*04010413MP01
*05010413MP01
*06010413MP01
*07010413MP01
*08010413MP01
*09010413MP01
*10010413MP01
*11010413MP01
*12010413MP01
*17010413MP01
*18010413MP01

DATA: gt_dfkkko type standard table of dfkkko,
      gt_dfkkop type standard table of dfkkop,
      lv_vkont      type dfkkop-vkont,
      lv_augbl      type dfkkop-augbl,
      lv_betrw      type dfkkop-betrw,
      lv_betrw_str  type string,
      lv_string     type string.

FIELD-SYMBOLS: <fs_dfkkko> type dfkkko,
               <fs_dfkkop> type dfkkop.

CONSTANTS: co_separator TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

SELECT-OPTIONS: so_fikey for dfkkko-fikey,
                so_vkont for dfkkop-vkont.
PARAMETERS:     pa_data type rlgrap-filename default '\\saprep01.mayniladsap.com\repuserdata\Glenn\reversal.txt'.

IF so_fikey IS INITIAL.
  WRITE:/ 'Please input FIKEY'.
ELSE.
  OPEN DATASET pa_data FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc eq 0.
    SELECT *
    INTO TABLE gt_dfkkko
    FROM DFKKKO
    WHERE FIKEY IN so_fikey.

    LOOP AT gt_dfkkko ASSIGNING <fs_dfkkko>.
      CLEAR lv_vkont.
      CLEAR lv_augbl.
      CLEAR lv_betrw.
      CLEAR lv_betrw_str.

      SELECT SINGLE vkont betrw augbl
      INTO (lv_vkont, lv_betrw, lv_augbl)
      FROM DFKKOP
      WHERE opbel eq <fs_dfkkko>-opbel
        AND vkont IN so_vkont.

        IF sy-subrc eq 0.
          WRITE:/ 'OPBEL', <fs_dfkkko>-opbel, <fs_dfkkko>-budat, <fs_dfkkko>-bldat, lv_vkont, lv_betrw, lv_augbl.
          MOVE lv_betrw TO lv_betrw_str.
          CONCATENATE 'OPBEL' <fs_dfkkko>-opbel <fs_dfkkko>-budat <fs_dfkkko>-bldat lv_vkont lv_betrw_str lv_augbl INTO lv_string SEPARATED BY co_separator.
          TRANSFER lv_string to pa_data.
        ELSE.
          SELECT *
          INTO TABLE gt_dfkkop
          FROM DFKKOP
          WHERE augbl eq <fs_dfkkko>-opbel
            AND vkont IN so_vkont.

            IF sy-subrc eq 0.
              LOOP AT gt_dfkkop ASSIGNING <fs_dfkkop>.
                WRITE:/ 'AUGBL', <fs_dfkkko>-opbel, <fs_dfkkko>-budat, <fs_dfkkko>-bldat, <fs_dfkkop>-vkont, <fs_dfkkop>-betrw, <fs_dfkkop>-opbel.
                MOVE <fs_dfkkop>-betrw TO lv_betrw_str.
                CONCATENATE 'AUGBL' <fs_dfkkko>-opbel <fs_dfkkko>-budat <fs_dfkkko>-bldat <fs_dfkkop>-vkont lv_betrw_str <fs_dfkkop>-opbel INTO lv_string SEPARATED BY co_separator.
                TRANSFER lv_string to pa_data.
              ENDLOOP.
            ENDIF.
        ENDIF.
    ENDLOOP.
  ELSE.
    WRITE:/ 'File not found'.
  ENDIF.
ENDIF.