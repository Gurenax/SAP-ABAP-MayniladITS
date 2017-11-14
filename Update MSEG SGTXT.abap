*&---------------------------------------------------------------------*
*& Report  ZTEST_GLENN3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTEST_GLENN3.

TABLES: proj, mara.


TYPES: BEGIN OF t_proj,
        pspnr       TYPE proj-pspnr,
        pspid       TYPE proj-pspid,
        aufnr       TYPE afko-aufnr,
        rsnum       TYPE resb-rsnum,
        rspos       TYPE resb-rspos,
        matnr       TYPE resb-matnr,
        xblnr_mkpf  TYPE mseg-xblnr_mkpf,
       END OF t_proj,

       BEGIN OF t_mseg,
        mblnr       TYPE mseg-mblnr,
        zeile       TYPE mseg-zeile,
        mjahr       TYPE mseg-mjahr,
        bwart       TYPE mseg-bwart,
        smbln       TYPE mseg-smbln,
        aufnr       TYPE mseg-aufnr,
        shkzg       TYPE mseg-shkzg,
        sgtxt       TYPE mseg-sgtxt,
        tbnum       TYPE mseg-tbnum,
        lgnum       TYPE mseg-lgnum,
        menge       TYPE mseg-menge,
        matnr       TYPE mseg-matnr,
        budat_mkpf  TYPE mseg-budat_mkpf,
        xblnr_mkpf  TYPE mseg-xblnr_mkpf,
        matkl       TYPE mara-matkl,
        pspid       TYPE proj-pspid,
        reversed    TYPE c,
        check       TYPE c,
      END OF t_mseg.


DATA: gt_proj       TYPE TABLE OF t_proj,
      gt_mseg       TYPE TABLE OF t_mseg,
      gt_mseg_sel   TYPE TABLE OF t_mseg,
      gt_mseg_std   TYPE STANDARD TABLE OF mseg.

DATA: ls_proj TYPE t_proj.

DATA: r_xblnr_mkpf TYPE RANGE OF mseg-xblnr_mkpf WITH HEADER LINE.

DATA: gv_sgtxt     TYPE mseg-sgtxt.

FIELD-SYMBOLS: <fs_mseg>      TYPE t_mseg,
               <fs_proj>      TYPE t_proj,
               <fs_mseg_std>  TYPE mseg.


SELECT-OPTIONS: s_pspid FOR proj-pspid,
                s_matnr FOR mara-matnr.
PARAMETERS: pa_upd AS CHECKBOX.

DATA: gv_mvt       TYPE mseg-bwart.
DATA: gv_string    TYPE string.

START-OF-SELECTION.
*  gv_mvt    = '322'.
*  gv_sgtxt  = 'Tr_IMM_QI'.

  REFRESH gt_proj.
  SELECT a~pspnr
         a~pspid
         b~aufnr
         c~rsnum
         c~rspos
         c~matnr
    INTO TABLE gt_proj
    FROM proj AS a
    INNER JOIN afko AS b ON a~pspnr = b~pronr
    INNER JOIN resb AS c ON b~aufnr = c~aufnr
    WHERE a~pspid IN s_pspid
      AND c~matnr IN s_matnr.

IF sy-subrc EQ 0.

    r_xblnr_mkpf-sign = 'I'.
    r_xblnr_mkpf-option = 'EQ'.
    LOOP AT gt_proj ASSIGNING <fs_proj>.
      CONCATENATE <fs_proj>-rsnum '-'
                  <fs_proj>-rspos
                  INTO r_xblnr_mkpf-low.
      APPEND r_xblnr_mkpf.
      <fs_proj>-xblnr_mkpf = r_xblnr_mkpf-low.
    ENDLOOP.
    SORT r_xblnr_mkpf.
    DELETE ADJACENT DUPLICATES FROM r_xblnr_mkpf.
    DELETE r_xblnr_mkpf WHERE low IS INITIAL.

    SELECT a~mblnr
          a~zeile
          a~mjahr
          a~bwart
          a~smbln
          a~aufnr
          a~shkzg
          a~sgtxt
          a~tbnum
          a~lgnum
          a~menge
          a~matnr
          a~budat_mkpf
          a~xblnr_mkpf
          b~matkl
      INTO TABLE gt_mseg
      FROM mseg AS a
      INNER JOIN mara AS b ON a~matnr = b~matnr
      WHERE a~matnr IN s_matnr
        AND a~xblnr_mkpf IN r_xblnr_mkpf.
        "AND a~sgtxt LIKE gv_sgtxt.
    IF sy-subrc EQ 0.
      SORT gt_mseg BY mblnr mjahr.
      DELETE ADJACENT DUPLICATES FROM gt_mseg COMPARING mblnr.

      LOOP AT gt_mseg ASSIGNING <fs_mseg>.
        CLEAR: gv_string.

        CASE <fs_mseg>-bwart.
          WHEN '322'.                                    "For QI
            CONCATENATE 'Tr_IMM_QI/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN 'Z58'.                                    "Charged
            CONCATENATE 'Disp_Clrg/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN '303'.                                    "Trans Def
            CONCATENATE 'Tr_CD_Df/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN '303'.                                    "Trans Good
            CONCATENATE 'Tr_CD_Gd/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN '412'.                                    "Whse Good
            CONCATENATE 'Rcv_CD_Gd/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN '344'.                                    "Whse Def
            CONCATENATE 'Rcv_CD_Df/' <fs_mseg>-MBLNR '/' <fs_mseg>-ZEILE '/' INTO gv_sgtxt.
          WHEN OTHERS.
            gv_sgtxt  = 'Others'.
        ENDCASE.

        <fs_mseg>-SGTXT = gv_sgtxt.

        CONCATENATE <fs_mseg>-MBLNR <fs_mseg>-MATNR <fs_mseg>-BWART <fs_mseg>-XBLNR_MKPF <fs_mseg>-SGTXT INTO gv_string SEPARATED BY '|' RESPECTING BLANKS.
        WRITE:/ gv_string.

        IF pa_upd EQ 'X'.
          SELECT *
          INTO TABLE gt_mseg_std
            FROM MSEG
           WHERE mblnr EQ <fs_mseg>-MBLNR.

          IF sy-subrc eq 0.
            LOOP AT gt_mseg_std ASSIGNING <fs_mseg_std>.
              "CONCATENATE gv_sgtxt '_DUMMY' INTO <fs_mseg_std>-sgtxt.
              CASE <fs_mseg_std>-bwart.
                WHEN '322'.                                    "For QI
                  CONCATENATE 'Tr_IMM_QI/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN 'Z58'.                                    "Charged
                  CONCATENATE 'Disp_Clrg/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN '303'.                                    "Trans Def
                  CONCATENATE 'Tr_CD_Df/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN '303'.                                    "Trans Good
                  CONCATENATE 'Tr_CD_Gd/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN '412'.                                    "Whse Good
                  CONCATENATE 'Rcv_CD_Gd/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN '344'.                                    "Whse Def
                  CONCATENATE 'Rcv_CD_Df/' <fs_mseg_std>-MBLNR '/' <fs_mseg_std>-ZEILE '/' INTO <fs_mseg_std>-sgtxt.
                WHEN OTHERS.
                  <fs_mseg_std>-sgtxt  = 'Others'.
              ENDCASE.
            ENDLOOP.
            UNASSIGN <fs_mseg_std>.

            MODIFY mseg FROM TABLE gt_mseg_std.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_mseg>.


*      LOOP AT gt_mseg ASSIGNING <fs_mseg>.
*        READ TABLE gt_proj INTO ls_proj WITH KEY matnr = <fs_mseg>-matnr
*                                                 xblnr_mkpf = <fs_mseg>-xblnr_mkpf.
*        IF sy-subrc EQ 0.
*          <fs_mseg>-pspid = ls_proj-pspid.
*        ENDIF.
*      ENDLOOP.
*
*      gt_mseg_sel[] = gt_mseg[].
*
*      "Filter mat docs to be displayed
*      LOOP AT gt_mseg_sel ASSIGNING <fs_mseg>.
*        READ TABLE gt_mseg TRANSPORTING NO FIELDS WITH KEY smbln = <fs_mseg>-mblnr.
*        IF sy-subrc EQ 0.
*          <fs_mseg>-reversed = 'X'.
*        ENDIF.
*      ENDLOOP.
*      DELETE gt_mseg_sel WHERE smbln IS NOT INITIAL.
*      DELETE gt_mseg_sel WHERE reversed = 'X'.
    ENDIF.
*    DELETE gt_mseg_sel WHERE bwart NE gv_mvt.
  ENDIF.