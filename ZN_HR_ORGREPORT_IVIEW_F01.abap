*&---------------------------------------------------------------------*
*&  Include           ZN_HR_ORGREPORT_IVIEW_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ORG_UNIT_SHLP
*&---------------------------------------------------------------------*
*  Org Unit search help
*----------------------------------------------------------------------*
FORM f_org_unit_shlp.

  IF gt_orgeh_shlp[] IS INITIAL.
    SELECT objid
           begda
           endda
           stext
      INTO TABLE gt_orgeh_shlp
      FROM hrp1000
      WHERE otype EQ co_otype_org
        AND plvar EQ co_plvar_current
        AND langu EQ co_lang_en.
    IF sy-subrc EQ 0.
      SORT gt_orgeh_shlp BY objid ASCENDING.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'PA_ORGEH'
      value_org       = 'S'
    TABLES
      value_tab       = gt_orgeh_shlp
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.                                  "#EC FB_RC

ENDFORM.                    " F_ORG_UNIT_SHLP

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*  Get data
*----------------------------------------------------------------------*
FORM f_get_data.

  TYPES: BEGIN OF ty_objid_s,
           objid TYPE hrobjid,
         END OF ty_objid_s.

  DATA: lt_hrp1001      TYPE STANDARD TABLE OF ty_hrp1001_s,
        lt_hrp1001_b002 TYPE STANDARD TABLE OF ty_objid_s,
        lt_hrp1001_pos  TYPE STANDARD TABLE OF ty_objid_s,
        lt_hrp1001_pers TYPE STANDARD TABLE OF ty_objid_s,
        ls_hrp1001      TYPE ty_hrp1001_s,
        ls_objid        TYPE ty_objid_s,
        lv_level        TYPE i.

  CONSTANTS: lco_usrty_0010 TYPE usrty VALUE '0010',
             lco_lgart_1000 TYPE lgart VALUE '1000'.

  REFRESH: gt_hrp1001,
           gt_hrp1000,
           gt_pa0002,
           gt_pa0105,
           gt_pa0001,
           gt_pa0041,
           gt_pa0000,
           gt_pa0008,
           gt_t501t,
           gt_t503t,
           gt_t529u,
           gt_t502t,
           gt_t005t,
           lt_hrp1001.

* Get data of first level Org unit
  SELECT otype
         objid
         plvar
         rsign
         relat
         istat
         priox
         begda
         endda
         varyf
         seqnr
         sclas
         sobid
    INTO TABLE lt_hrp1001
    FROM hrp1001
    WHERE otype EQ co_otype_org
      AND objid EQ pa_orgeh
      AND plvar EQ co_plvar_current
      AND rsign EQ co_rsign_b
      AND relat IN (co_relat_002,
                    co_relat_003,
                    co_relat_012)
      AND begda LE pa_date
      AND endda GE pa_date.
  IF sy-subrc EQ 0.
    gt_hrp1001[] = lt_hrp1001[].
  ENDIF.

* Get data of succeeding Org unit levels
  lv_level = pa_level.
  WHILE lv_level GT 0.
    lv_level = lv_level - 1.

    REFRESH lt_hrp1001_b002.
    CLEAR ls_hrp1001.
    LOOP AT lt_hrp1001 INTO ls_hrp1001 WHERE rsign EQ co_rsign_b
                                         AND relat EQ co_relat_002
                                         AND sclas EQ co_otype_org.
      CLEAR ls_objid.
      ls_objid-objid = ls_hrp1001-sobid.
      APPEND ls_objid TO lt_hrp1001_b002.
    ENDLOOP.

    IF lt_hrp1001_b002[] IS NOT INITIAL.
      REFRESH lt_hrp1001.
      SELECT otype
             objid
             plvar
             rsign
             relat
             istat
             priox
             begda
             endda
             varyf
             seqnr
             sclas
             sobid
        INTO TABLE lt_hrp1001
        FROM hrp1001
        FOR ALL ENTRIES IN lt_hrp1001_b002
        WHERE otype EQ co_otype_org
          AND objid EQ lt_hrp1001_b002-objid
          AND plvar EQ co_plvar_current
          AND relat IN (co_relat_002,
                        co_relat_003,
                        co_relat_012)
          AND begda LE pa_date
          AND endda GE pa_date.
      IF sy-subrc EQ 0.
        APPEND LINES OF lt_hrp1001 TO gt_hrp1001.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDWHILE.

* Get the holder of the retrieved positions
  IF gt_hrp1001[] IS NOT INITIAL.
    SORT gt_hrp1001 BY otype ASCENDING
                       objid ASCENDING
                       rsign ASCENDING
                       relat ASCENDING
                       sclas ASCENDING
                       sobid ASCENDING.
    REFRESH lt_hrp1001_pos.
    CLEAR ls_hrp1001.
    LOOP AT gt_hrp1001 INTO ls_hrp1001 WHERE rsign EQ co_rsign_b
                                         AND ( relat EQ co_relat_003 OR
                                               relat EQ co_relat_012 )
                                         AND sclas EQ co_otype_pos.
      CLEAR ls_objid.
      ls_objid-objid = ls_hrp1001-sobid.
      APPEND ls_objid TO lt_hrp1001_pos.
    ENDLOOP.

    IF lt_hrp1001_pos[] IS NOT INITIAL.
      REFRESH lt_hrp1001.
      SELECT otype
             objid
             plvar
             rsign
             relat
             istat
             priox
             begda
             endda
             varyf
             seqnr
             sclas
             sobid
        INTO TABLE lt_hrp1001
        FROM hrp1001
        FOR ALL ENTRIES IN lt_hrp1001_pos
        WHERE otype EQ co_otype_pos
          AND objid EQ lt_hrp1001_pos-objid
          AND plvar EQ co_plvar_current
          AND rsign EQ co_rsign_a
          AND ( relat EQ co_relat_003 OR
                relat EQ co_relat_008 )
          AND begda LE pa_date
          AND endda GE pa_date.
      IF sy-subrc EQ 0.
        APPEND LINES OF lt_hrp1001 TO gt_hrp1001.
        SORT gt_hrp1001 BY otype ASCENDING
                           objid ASCENDING
                           rsign ASCENDING
                           relat ASCENDING
                           sclas ASCENDING
                           sobid ASCENDING.

        REFRESH lt_hrp1001_pers.
        CLEAR ls_hrp1001.
        LOOP AT lt_hrp1001 INTO ls_hrp1001 WHERE rsign EQ co_rsign_a
                                             AND relat EQ co_relat_008
                                             AND sclas EQ co_otype_pers.
          CLEAR ls_objid.
          ls_objid-objid = ls_hrp1001-sobid.
          APPEND ls_objid TO lt_hrp1001_pers.
        ENDLOOP.

        IF lt_hrp1001_pers[] IS NOT INITIAL.
*         Get data from infotype 0002
          SELECT pernr
                 subty
                 objps
                 sprps
                 endda
                 begda
                 seqnr
                 nachn
                 nach2 " DV2K915414 DTALOSIG
                 vorna
                 gesch
                 gbdat " DV2K915414 DTALOSIG
                 natio
                 famst
            INTO TABLE gt_pa0002
            FROM pa0002
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date.
          IF sy-subrc EQ 0.
            SORT gt_pa0002 BY pernr ASCENDING.

*           Get Nationality description
            SELECT *
              INTO TABLE gt_t005t
              FROM t005t
              FOR ALL ENTRIES IN gt_pa0002
              WHERE spras EQ co_lang_en
                AND land1 EQ gt_pa0002-natio.
            IF sy-subrc EQ 0.
              SORT gt_t005t BY land1 ASCENDING.
            ENDIF.

*           Get Marital Status description
            SELECT *
              INTO TABLE gt_t502t
              FROM t502t
              FOR ALL ENTRIES IN gt_pa0002
              WHERE sprsl EQ co_lang_en
                AND famst EQ gt_pa0002-famst.
            IF sy-subrc EQ 0.
              SORT gt_t502t BY famst ASCENDING.
            ENDIF.
          ENDIF.

*         Get data from infotype 0105
          SELECT pernr
                 subty
                 objps
                 sprps
                 endda
                 begda
                 seqnr
                 usrty
                 usrid_long
            INTO TABLE gt_pa0105
            FROM pa0105
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date
              AND usrty EQ lco_usrty_0010.
          IF sy-subrc EQ 0.
            SORT gt_pa0105 BY pernr ASCENDING.
          ENDIF.

*         Get data from infotype 0001
          SELECT pernr
                 subty
                 objps
                 sprps
                 endda
                 begda
                 seqnr
                 persg
                 persk
            INTO TABLE gt_pa0001
            FROM pa0001
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date.
          IF sy-subrc EQ 0.
            SORT gt_pa0001 BY pernr ASCENDING.

*           Get employee group description
            SELECT *
              INTO TABLE gt_t501t
              FROM t501t
              FOR ALL ENTRIES IN gt_pa0001
              WHERE sprsl EQ co_lang_en
                AND persg EQ gt_pa0001-persg.
            IF sy-subrc EQ 0.
              SORT gt_t501t BY persg ASCENDING.
            ENDIF.

*           Get employee subgroup description
            SELECT *
              INTO TABLE gt_t503t
              FROM t503t
              FOR ALL ENTRIES IN gt_pa0001
              WHERE sprsl EQ co_lang_en
                AND persk EQ gt_pa0001-persk.
            IF sy-subrc EQ 0.
              SORT gt_t503t BY persk ASCENDING.
            ENDIF.
          ENDIF.

*         Get data from infotype 0041
          SELECT *
            INTO TABLE gt_pa0041
            FROM pa0041
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date.
          IF sy-subrc EQ 0.
            SORT gt_pa0041 BY pernr ASCENDING.
          ENDIF.

*         Get data from infotype 0000
          SELECT pernr
                 subty
                 objps
                 sprps
                 endda
                 begda
                 seqnr
                 stat2
            INTO TABLE gt_pa0000
            FROM pa0000
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date.
          IF sy-subrc EQ 0.
            SORT gt_pa0000 BY pernr ASCENDING.

*           Get description of employee status
            SELECT *
              INTO TABLE gt_t529u
              FROM t529u
              FOR ALL ENTRIES IN gt_pa0000
              WHERE sprsl EQ co_lang_en
                AND statn EQ '2'
                AND statv EQ gt_pa0000-stat2.
            IF sy-subrc EQ 0.
              SORT gt_t529u BY statv ASCENDING.
            ENDIF.
          ENDIF.

*         Get data from infotype 0008
          SELECT pernr
                 subty
                 objps
                 sprps
                 endda
                 begda
                 seqnr
                 lga01
                 bet01
            INTO TABLE gt_pa0008
            FROM pa0008
            FOR ALL ENTRIES IN lt_hrp1001_pers
            WHERE pernr EQ lt_hrp1001_pers-objid
              AND endda GE pa_date
              AND begda LE pa_date
              AND lga01 EQ lco_lgart_1000.
          IF sy-subrc EQ 0.
            SORT gt_pa0008 BY pernr ASCENDING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   Get the Org unit description and Position description
    SELECT plvar
           otype
           objid
           istat
           begda
           endda
           langu
           seqnr
           stext
      INTO TABLE gt_hrp1000
      FROM hrp1000
      FOR ALL ENTRIES IN gt_hrp1001
      WHERE plvar EQ gt_hrp1001-plvar
        AND otype EQ gt_hrp1001-otype
        AND objid EQ gt_hrp1001-objid
        AND begda LE pa_date
        AND endda GE pa_date
        AND langu EQ co_lang_en.
    IF sy-subrc EQ 0.
      SORT gt_hrp1000 BY plvar ASCENDING
                         otype ASCENDING
                         objid ASCENDING.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_CONSOLIDATE_DATA
*&---------------------------------------------------------------------*
*  Consolidate data
*----------------------------------------------------------------------*
FORM f_consolidate_data.

  DATA: lt_gesch       TYPE STANDARD TABLE OF dd07v,
        lt_dummy       TYPE STANDARD TABLE OF dd07v,
        ls_hrp1001     TYPE ty_hrp1001_s,
        ls_hrp1001_top TYPE ty_hrp1001_s,
        ls_hrp1001_org TYPE ty_hrp1001_s,
        lr_org         TYPE RANGE OF hrp1001-objid,
        lr_org2        TYPE RANGE OF hrp1001-objid,
        ls_org         LIKE LINE OF lr_org,
        lv_exit        TYPE flag,
        lv_sobid       TYPE sobid,
        lv_reports_to  TYPE sobid.

  FIELD-SYMBOLS:
        <fs_output>    TYPE zst_hr_orgrep_to_orgplus.

* Get Gender key descriptions
  REFRESH lt_gesch.
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = 'GESCH'
    TABLES
      dd07v_tab_a   = lt_gesch
      dd07v_tab_n   = lt_dummy
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.                                    "#EC FB_RC

* Remove the B003 record of the one who leads each org unit (B012)
  CLEAR ls_hrp1001.
  LOOP AT gt_hrp1001 INTO ls_hrp1001 WHERE rsign EQ co_rsign_b
                                       AND relat EQ co_relat_012
                                       AND sclas EQ co_otype_pos.
    DELETE gt_hrp1001 WHERE objid EQ ls_hrp1001-objid
                        AND rsign EQ co_rsign_b
                        AND relat EQ co_relat_003
                        AND sclas EQ co_otype_pos
                        AND sobid EQ ls_hrp1001-sobid.
  ENDLOOP.

  REFRESH: gt_output,
           gt_hrp1001_top,
           gt_hrp1001_org,
           gt_hrp1001_pos.

  gt_hrp1001_top[] = gt_hrp1001[].
  gt_hrp1001_org[] = gt_hrp1001[].
  gt_hrp1001_pos[] = gt_hrp1001[].
  DELETE gt_hrp1001_top WHERE otype NE co_otype_org
                           OR objid NE pa_orgeh.
  DELETE gt_hrp1001_org WHERE otype NE co_otype_org.
  DELETE gt_hrp1001_pos WHERE otype NE co_otype_pos.

* Get the head of the top Org unit
  CLEAR ls_hrp1001_top.
  READ TABLE gt_hrp1001_top INTO ls_hrp1001_top WITH KEY rsign = co_rsign_b
                                                         relat = co_relat_012
                                                         sclas = co_otype_pos.
  IF sy-subrc EQ 0.
*   Details
    PERFORM f_details USING ls_hrp1001_top
                            co_mark_x
                            lt_gesch.
  ENDIF.

* Get the Positions inside the top Org unit
  CLEAR ls_hrp1001_top.
  LOOP AT gt_hrp1001_top INTO ls_hrp1001_top WHERE rsign EQ co_rsign_b
                                               AND relat EQ co_relat_003
                                               AND sclas EQ co_otype_pos.
*   Details
    PERFORM f_details USING ls_hrp1001_top
                            space
                            lt_gesch.
  ENDLOOP.

* Get the Org units inside the top Org unit
  REFRESH lr_org.
  CLEAR ls_hrp1001_top.
  LOOP AT gt_hrp1001_top INTO ls_hrp1001_top WHERE rsign EQ co_rsign_b
                                               AND relat EQ co_relat_002
                                               AND sclas EQ co_otype_org.
    CLEAR ls_hrp1001_org.
    LOOP AT gt_hrp1001_org INTO ls_hrp1001_org WHERE objid EQ ls_hrp1001_top-sobid.
      IF  ls_hrp1001_org-rsign EQ co_rsign_b
      AND ls_hrp1001_org-relat EQ co_relat_002
      AND ls_hrp1001_org-sclas EQ co_otype_org.
        CLEAR ls_org.
        ls_org-sign   = 'I'.
        ls_org-option = 'EQ'.
        ls_org-low    = ls_hrp1001_org-sobid.
        APPEND ls_org TO lr_org.

      ELSEIF ls_hrp1001_org-rsign EQ co_rsign_b
      AND  ( ls_hrp1001_org-relat EQ co_relat_003 OR ls_hrp1001_org-relat EQ co_relat_012 )
      AND    ls_hrp1001_org-sclas EQ co_otype_pos.
*       Details
        PERFORM f_details USING ls_hrp1001_org
                                space
                                lt_gesch.

      ENDIF.
    ENDLOOP.
  ENDLOOP.

* Get org units and position of the succeeding levels
  CLEAR lv_exit.
  WHILE lv_exit IS INITIAL.
    IF lr_org[] IS NOT INITIAL.
      REFRESH lr_org2.
      CLEAR ls_hrp1001_org.
      LOOP AT gt_hrp1001_org INTO ls_hrp1001_org WHERE objid IN lr_org.
        IF  ls_hrp1001_org-rsign EQ co_rsign_b
        AND ls_hrp1001_org-relat EQ co_relat_002
        AND ls_hrp1001_org-sclas EQ co_otype_org.
          CLEAR ls_org.
          ls_org-sign   = 'I'.
          ls_org-option = 'EQ'.
          ls_org-low    = ls_hrp1001_org-sobid.
          APPEND ls_org TO lr_org2.

        ELSEIF ls_hrp1001_org-rsign EQ co_rsign_b
        AND  ( ls_hrp1001_org-relat EQ co_relat_003 OR ls_hrp1001_org-relat EQ co_relat_012 )
        AND    ls_hrp1001_org-sclas EQ co_otype_pos.
*         Details
          PERFORM f_details USING ls_hrp1001_org
                                  space
                                  lt_gesch.

        ENDIF.
      ENDLOOP.

      REFRESH lr_org.
      lr_org[] = lr_org2[].

    ELSE.
      lv_exit = co_mark_x.
    ENDIF.
  ENDWHILE.

  "-- start - Added by RANDYSY TN#191 10/03/2014
  LOOP AT gt_output ASSIGNING <fs_output>." WHERE POSITION = '50012108'.

    "-- Checks of Position is same w/ Reports To
    IF <fs_output>-position EQ <fs_output>-reports_to.
*      break randysy.
      "-- Get Check Organizational unit of position
      SELECT SINGLE sobid
        INTO lv_sobid
      FROM   hrp1001
      WHERE  objid EQ <fs_output>-position
      AND    plvar EQ '01'
      AND    subty EQ 'A003'
      AND    begda LE pa_date
      AND    endda GE pa_date.

      IF sy-subrc EQ 0.
        "-- Get Check the Organizational Unit that the Org unit of the position is reporting to
        SELECT SINGLE sobid
          INTO lv_sobid
        FROM   hrp1001
        WHERE  objid EQ lv_sobid
        AND    plvar EQ '01'
        AND    subty EQ 'A002'
        AND    begda LE pa_date
        AND    endda GE pa_date.

        IF sy-subrc EQ 0.
          "-- 1st Try
          "-- Get position holder of the Organizational unit
          SELECT SINGLE sobid
            INTO lv_sobid
          FROM   hrp1001
          WHERE  objid EQ lv_sobid
          AND    plvar EQ '01'
          AND    subty EQ 'B012'
          AND    begda LE pa_date
          AND    endda GE pa_date.

          IF sy-subrc EQ 0.
            <fs_output>-reports_to = lv_sobid.
          ELSE.
            "-- 2nd Try
            "-- Get position holder of the Organizational unit
            SELECT SINGLE sobid
              INTO lv_sobid
            FROM   hrp1001
            WHERE  objid EQ lv_sobid
            AND    plvar EQ '01'
            AND    subty EQ 'B003'
            AND    begda LE pa_date
            AND    endda GE pa_date.

              IF sy-subrc EQ 0.
                <fs_output>-reports_to = lv_sobid.
              ELSE.
                "-- 3rd Try
                "-- Get position holder of the Organizational unit
                SELECT SINGLE sobid
                  INTO lv_sobid
                FROM   hrp1001
                WHERE  objid EQ lv_sobid
                AND    plvar EQ '01'
                AND    subty EQ 'A002'
                AND    begda LE pa_date
                AND    endda GE pa_date.

                IF sy-subrc EQ 0.
                  "-- 1st Try 2
                  "-- Get position holder of the Organizational unit
                  SELECT SINGLE sobid
                    INTO lv_sobid
                  FROM   hrp1001
                  WHERE  objid EQ lv_sobid
                  AND    plvar EQ '01'
                  AND    subty EQ 'B012'
                  AND    begda LE pa_date
                  AND    endda GE pa_date.

                  IF sy-subrc EQ 0.
                    <fs_output>-reports_to = lv_sobid.
                  ELSE.
                    "-- 2nd Try 2
                    "-- Get position holder of the Organizational unit
                    SELECT SINGLE sobid
                      INTO lv_sobid
                    FROM   hrp1001
                    WHERE  objid EQ lv_sobid
                    AND    plvar EQ '01'
                    AND    subty EQ 'B003'
                    AND    begda LE pa_date
                    AND    endda GE pa_date.

                    IF sy-subrc EQ 0.
                      <fs_output>-reports_to = lv_sobid.
                    ENDIF.
                  ENDIF.
                ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "-- Checks if Reports To is blank
    IF <fs_output>-reports_to IS INITIAL.
*      break randysy.

      "-- Get Check Organizational unit of position
      SELECT SINGLE sobid
        INTO lv_sobid
      FROM   hrp1001
      WHERE  objid EQ <fs_output>-position
      AND    plvar EQ '01'
      AND    subty EQ 'A003'
      AND    begda LE pa_date
      AND    endda GE pa_date.

      IF lv_sobid NE pa_orgeh.
*        break randysy.

        WHILE <fs_output>-reports_to IS INITIAL.
          IF sy-index LT 4.
            "-- Get Check the Organizational Unit that the Org unit of the position is reporting to
            SELECT SINGLE sobid
              INTO lv_sobid
            FROM   hrp1001
            WHERE  objid EQ lv_sobid
            AND    plvar EQ '01'
            AND    subty EQ 'A002'
            AND    begda LE pa_date
            AND    endda GE pa_date.

            IF sy-subrc EQ 0.
              "-- Get position holder of the Organizational unit
              SELECT SINGLE sobid
                INTO lv_reports_to
              FROM   hrp1001
              WHERE  objid EQ lv_sobid
              AND    plvar EQ '01'
              AND    subty EQ 'B012'
              AND    begda LE pa_date
              AND    endda GE pa_date.

              IF sy-subrc EQ 0.
                <fs_output>-reports_to = lv_reports_to.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDWHILE.

      ENDIF.
    ENDIF.

  ENDLOOP.
  "-- end - Added by RANDYSY TN#191 10/03/2014

ENDFORM.                    " F_CONSOLIDATE_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DETAILS
*&---------------------------------------------------------------------*
*  Details
*----------------------------------------------------------------------*
FORM f_details USING value(is_hrp1001) TYPE ty_hrp1001_s
                     value(iv_top)     TYPE flag
                     value(it_gesch)   TYPE dd07v_tab.

  DATA: lt_leadpos      TYPE STANDARD TABLE OF hrobject,
        lt_leadcomp     TYPE STANDARD TABLE OF hrp1001,     "DV2K915694
        lt_jobdesc      TYPE STANDARD TABLE OF hrp1000,     "DV2K915694
        lt_job_obj      TYPE STANDARD TABLE OF hrp1001,     "DV2K915694
        lt_leadcomp2    TYPE STANDARD TABLE OF hrobject,    "DV2K915694
        ls_output       TYPE zst_hr_orgrep_to_orgplus,
        ls_hrp1001_pos  TYPE ty_hrp1001_s,
        ls_hrp1000      TYPE ty_hrp1000_s,
        ls_pa0002       TYPE ty_pa0002_s,
        ls_pa0105       TYPE ty_pa0105_s,
        ls_pa0001       TYPE ty_pa0001_s,
        ls_pa0041       TYPE pa0041,
        ls_pa0000       TYPE ty_pa0000_s,
        ls_pa0008       TYPE ty_pa0008_s,
        ls_t501t        TYPE t501t,
        ls_t503t        TYPE t503t,
        ls_t529u        TYPE t529u,
        ls_leadpos      TYPE hrobject,
        ls_gesch        TYPE dd07v,
        ls_t502t        TYPE t502t,
        ls_t005t        TYPE t005t,
        ls_job_obj      TYPE hrp1001,                       "DV2K915694
        ls_leadorg      TYPE hrp1001,                       "DV2K915694
        ls_leadcomp     TYPE hrp1001,                       "DV2K915694
        ls_jobdesc      TYPE hrp1000,                       "DV2K915694
        ls_leadcomp2    TYPE hrobject,                      "DV2K915694
        lv_index        TYPE numc2,
        lv_field(5)     TYPE c,
        lv_age          TYPE cmp_loval.

  FIELD-SYMBOLS: <fs_darxx> TYPE datar,
                 <fs_datxx> TYPE dardt.

  CONSTANTS: lco_orighiredate TYPE datar VALUE 'Z1',
             lco_vacant(6)    TYPE c     VALUE 'VACANT',
             lco_head(5)      TYPE c     VALUE 'Head*',     "DV2K915694
             lco_sup(10)      TYPE c     VALUE 'Supervisor'. "DV2K915694

  CLEAR ls_output.

* Position
  ls_output-position = is_hrp1001-sobid.

* Department
  CLEAR ls_hrp1000.
  READ TABLE gt_hrp1000 INTO ls_hrp1000 WITH KEY plvar = is_hrp1001-plvar
                                                 otype = is_hrp1001-otype
                                                 objid = is_hrp1001-objid.
  IF sy-subrc EQ 0.
    ls_output-department = ls_hrp1000-stext.
  ENDIF.

* Reports To
  IF iv_top IS INITIAL.
*---Insert DTALOSIG DV2K915694
*---Check if head is assigned for the org unit "DV2K915694
    IF is_hrp1001-rsign EQ co_rsign_b AND is_hrp1001-relat EQ co_relat_012.
      SELECT SINGLE *
        FROM hrp1001
          INTO ls_leadorg
            WHERE otype EQ co_otype_org
              AND objid EQ is_hrp1001-objid
              AND plvar EQ co_plvar_current
              AND rsign EQ co_rsign_a
              AND relat EQ co_relat_002
              AND begda LE pa_date
              AND endda GE pa_date.
      IF sy-subrc EQ 0.
        SELECT *
          FROM hrp1001
            INTO TABLE lt_leadcomp
            WHERE otype EQ co_otype_org
              AND objid EQ ls_leadorg-sobid
              AND plvar EQ co_plvar_current
              AND begda LE pa_date
              AND endda GE pa_date.
        IF sy-subrc EQ 0.
          READ TABLE lt_leadcomp INTO ls_leadcomp WITH KEY  rsign = co_rsign_b
                                                            relat = co_relat_012.
          IF sy-subrc EQ 0.
            ls_output-reports_to = ls_leadcomp-sobid.
          ELSE.
            DELETE lt_leadcomp WHERE rsign NE co_rsign_b
                       AND relat NE co_relat_003.
            LOOP AT lt_leadcomp INTO ls_leadcomp.
              CLEAR: ls_leadcomp2.
              ls_leadcomp2-objid = ls_leadcomp-sobid.
              APPEND ls_leadcomp2 TO lt_leadcomp2.
            ENDLOOP.
*       Get job of position
            SELECT *
              FROM hrp1001
                INTO TABLE lt_job_obj
                FOR ALL ENTRIES IN lt_leadcomp2
                  WHERE otype EQ co_otype_pos
                    AND objid EQ lt_leadcomp2-objid
                    AND plvar EQ co_plvar_current
                    AND begda LE pa_date
                    AND endda GE pa_date
                    AND sclas EQ co_sclas.
            IF sy-subrc EQ 0.
              REFRESH: lt_leadcomp2.
              LOOP AT lt_job_obj INTO ls_job_obj.
                CLEAR: ls_leadcomp2.
                ls_leadcomp2-objid = ls_job_obj-sobid.
                APPEND ls_leadcomp2 TO lt_leadcomp2.
              ENDLOOP.
              SELECT *
              FROM hrp1000
                INTO TABLE lt_jobdesc
                FOR ALL ENTRIES IN lt_leadcomp2
                  WHERE plvar EQ co_plvar_current
                    AND otype EQ co_otype_job
                    AND objid EQ lt_leadcomp2-objid
                    AND begda LE pa_date
                    AND endda GE pa_date
                    AND langu EQ sy-langu.
              IF sy-subrc EQ 0.
                LOOP AT lt_job_obj INTO ls_job_obj.
                  CLEAR: ls_jobdesc.
                  READ TABLE lt_jobdesc INTO ls_jobdesc WITH KEY objid = ls_job_obj-sobid.
                  IF sy-subrc EQ 0.
                    SEARCH ls_jobdesc-stext FOR lco_head.
                    IF sy-subrc EQ 0.
                      ls_output-reports_to = ls_job_obj-objid.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
                IF ls_output-reports_to IS INITIAL.
                  LOOP AT lt_job_obj INTO ls_job_obj.
                    CLEAR: ls_jobdesc.
                    READ TABLE lt_jobdesc INTO ls_jobdesc WITH KEY objid = ls_job_obj-sobid.
                    IF sy-subrc EQ 0.
                      SEARCH ls_jobdesc-stext FOR lco_sup.
                      IF sy-subrc EQ 0.
                        ls_output-reports_to = ls_job_obj-objid.
                        EXIT.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.

        ENDIF.
      ENDIF.
    ELSEIF is_hrp1001-rsign EQ co_rsign_b AND is_hrp1001-relat EQ co_relat_003.
      SELECT *
          FROM hrp1001
            INTO TABLE lt_leadcomp
            WHERE otype EQ co_otype_org
              AND objid EQ is_hrp1001-objid
              AND plvar EQ co_plvar_current
              AND begda LE pa_date
              AND endda GE pa_date.
      IF sy-subrc EQ 0.
        READ TABLE lt_leadcomp INTO ls_leadcomp WITH KEY  rsign = co_rsign_b
                                                          relat = co_relat_012.
        IF sy-subrc EQ 0.
          ls_output-reports_to = ls_leadcomp-sobid.
        ELSE.
          DELETE lt_leadcomp WHERE rsign NE co_rsign_b
                     AND relat NE co_relat_003.
          LOOP AT lt_leadcomp INTO ls_leadcomp.
            CLEAR: ls_leadcomp2.
            ls_leadcomp2-objid = ls_leadcomp-sobid.
            APPEND ls_leadcomp2 TO lt_leadcomp2.
          ENDLOOP.
*       Get job of position
          SELECT *
            FROM hrp1001
              INTO TABLE lt_job_obj
              FOR ALL ENTRIES IN lt_leadcomp2
                WHERE otype EQ co_otype_pos
                  AND objid EQ lt_leadcomp2-objid
                  AND plvar EQ co_plvar_current
                  AND begda LE pa_date
                  AND endda GE pa_date
                  AND sclas EQ co_sclas.
          IF sy-subrc EQ 0.
            REFRESH: lt_leadcomp2.
            LOOP AT lt_job_obj INTO ls_job_obj.
              CLEAR: ls_leadcomp2.
              ls_leadcomp2-objid = ls_job_obj-sobid.
              APPEND ls_leadcomp2 TO lt_leadcomp2.
            ENDLOOP.
            SELECT *
            FROM hrp1000
              INTO TABLE lt_jobdesc
              FOR ALL ENTRIES IN lt_leadcomp2
                WHERE plvar EQ co_plvar_current
                  AND otype EQ co_otype_job
                  AND objid EQ lt_leadcomp2-objid
                  AND begda LE pa_date
                  AND endda GE pa_date
                  AND langu EQ sy-langu.
            IF sy-subrc EQ 0.
              LOOP AT lt_job_obj INTO ls_job_obj.
                CLEAR: ls_jobdesc.
                READ TABLE lt_jobdesc INTO ls_jobdesc WITH KEY objid = ls_job_obj-sobid.
                IF sy-subrc EQ 0.
                  SEARCH ls_jobdesc-stext FOR lco_head.
                  IF sy-subrc EQ 0.
                    ls_output-reports_to = ls_job_obj-objid.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              IF ls_output-reports_to IS INITIAL.
                LOOP AT lt_job_obj INTO ls_job_obj.
                  CLEAR: ls_jobdesc.
                  READ TABLE lt_jobdesc INTO ls_jobdesc WITH KEY objid = ls_job_obj-sobid.
                  IF sy-subrc EQ 0.
                    SEARCH ls_jobdesc-stext FOR lco_sup.
                    IF sy-subrc EQ 0.
                      ls_output-reports_to = ls_job_obj-objid.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.

      ENDIF.
    ELSE.
*---End of Insert DTALOSIG DV2K915694
      CALL FUNCTION 'RH_GET_LEADING_POSITION'
        EXPORTING
          plvar             = co_plvar_current
          otype             = co_otype_pos
          sobid             = is_hrp1001-sobid
        TABLES
          leading_pos       = lt_leadpos
        EXCEPTIONS
          no_lead_pos_found = 1
          OTHERS            = 2.
      IF sy-subrc EQ 0.
        CLEAR ls_leadpos.
        READ TABLE lt_leadpos INTO ls_leadpos WITH KEY plvar = co_plvar_current
                                                       otype = co_otype_pos.
        IF sy-subrc EQ 0.
          ls_output-reports_to = ls_leadpos-objid.
        ENDIF.
      ENDIF.
*---Insert DTALOSIG DV2K915694
    ENDIF.
*---End of Insert DTALOSIG DV2K915694
  ENDIF.

  CLEAR ls_hrp1001_pos.
  READ TABLE gt_hrp1001_pos INTO ls_hrp1001_pos WITH KEY objid = is_hrp1001-sobid
                                                         rsign = co_rsign_a
                                                         relat = co_relat_008
                                                         sclas = co_otype_pers. "#EC WARNOK
  IF sy-subrc EQ 0.
*   Employee ID
    ls_output-employee_id = ls_hrp1001_pos-sobid.

*   Job title
    CLEAR ls_hrp1000.
    READ TABLE gt_hrp1000 INTO ls_hrp1000 WITH KEY plvar = ls_hrp1001_pos-plvar
                                                   otype = ls_hrp1001_pos-otype
                                                   objid = ls_hrp1001_pos-objid.
    IF sy-subrc EQ 0.
      ls_output-job_title = ls_hrp1000-stext.
    ENDIF.

*   Employee details
    CLEAR ls_pa0002.
    READ TABLE gt_pa0002 INTO ls_pa0002 WITH KEY pernr = ls_output-employee_id
                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
*     Name
      CONCATENATE ls_pa0002-vorna
                  ls_pa0002-nach2   " DV2K915414 DTALOSIG
                  ls_pa0002-nachn
             INTO ls_output-employee_name SEPARATED BY space.
*----DV2K915414 Add DTALOSIG
*    Date of Birth
      ls_output-date_of_birth = ls_pa0002-gbdat.
*----DV2K915414 End of Add DTALOSIG
*     Gender
      CLEAR ls_gesch.
      READ TABLE it_gesch INTO ls_gesch WITH KEY domvalue_l = ls_pa0002-gesch.
      IF sy-subrc EQ 0.
        ls_output-gender = ls_gesch-ddtext.
      ENDIF.

*     Marital Status
      CLEAR ls_t502t.
      READ TABLE gt_t502t INTO ls_t502t WITH KEY famst = ls_pa0002-famst
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_output-marital_status = ls_t502t-ftext.
      ENDIF.

*     Nationality
      CLEAR ls_t005t.
      READ TABLE gt_t005t INTO ls_t005t WITH KEY land1 = ls_pa0002-natio
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_output-nationality = ls_t005t-natio.
      ENDIF.
    ENDIF.

**   E-mail address  " DV2K915414 DTALOSIG
*    CLEAR ls_pa0105.
*    READ TABLE gt_pa0105 INTO ls_pa0105 WITH KEY pernr = ls_output-employee_id
*                                        BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      ls_output-email = ls_pa0105-usrid_long.
*    ENDIF.

*   Job Grade and Worker Category
    CLEAR ls_pa0001.
    READ TABLE gt_pa0001 INTO ls_pa0001 WITH KEY pernr = ls_output-employee_id
                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR ls_t501t.
      READ TABLE gt_t501t INTO ls_t501t WITH KEY persg = ls_pa0001-persg
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_output-job_grade = ls_t501t-ptext.
      ENDIF.

*      CLEAR ls_t503t.
*      READ TABLE gt_t503t INTO ls_t503t WITH KEY persk = ls_pa0001-persk
*                                        BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        ls_output-work_category = ls_t503t-ptext.
*      ENDIF.
    ENDIF.

*   Hire date
    CLEAR ls_pa0041.
    READ TABLE gt_pa0041 INTO ls_pa0041 WITH KEY pernr = ls_output-employee_id.
    IF sy-subrc EQ 0.
      DO 12 TIMES.
        lv_index = sy-index.
        CLEAR lv_field.
        CONCATENATE 'DAR' lv_index INTO lv_field.
        UNASSIGN <fs_darxx>.
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_pa0041 TO <fs_darxx>.
        IF <fs_darxx> IS ASSIGNED.
          IF <fs_darxx> EQ lco_orighiredate.
            CLEAR lv_field.
            CONCATENATE 'DAT' lv_index INTO lv_field.
            UNASSIGN <fs_datxx>.
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_pa0041 TO <fs_datxx>.
            IF <fs_datxx> IS ASSIGNED.
              ls_output-hire_date = <fs_datxx>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.

*   Employee Status
    CLEAR ls_pa0000.
    READ TABLE gt_pa0000 INTO ls_pa0000 WITH KEY pernr = ls_output-employee_id
                                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR ls_t529u.
      READ TABLE gt_t529u INTO ls_t529u WITH KEY statv = ls_pa0000-stat2
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_output-emp_status = ls_t529u-text1.
      ENDIF.
    ENDIF.

**   Salary " DV2K915414 DTALOSIG
*    CLEAR ls_pa0008.
*    READ TABLE gt_pa0008 INTO ls_pa0008 WITH KEY pernr = ls_output-employee_id
*                                        BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      ls_output-salary = ls_pa0008-bet01.
*    ENDIF.

*   Age
    CLEAR lv_age.
    CALL FUNCTION 'HR_AUPBS_AGE'
      EXPORTING
        pernr            = ls_output-employee_id
        bsdte            = pa_date
      IMPORTING
        value            = lv_age
      EXCEPTIONS
        record_not_found = 1
        OTHERS           = 2.
    IF sy-subrc EQ 0.
      ls_output-age = lv_age.
    ENDIF.

    APPEND ls_output TO gt_output.

  ELSE.   " Include unassigned positions

*   Job title
    CLEAR ls_hrp1000.
    READ TABLE gt_hrp1000 INTO ls_hrp1000 WITH KEY plvar = is_hrp1001-plvar
                                                   otype = co_otype_pos
                                                   objid = is_hrp1001-sobid.
    IF sy-subrc EQ 0.
      ls_output-job_title = ls_hrp1000-stext.
    ENDIF.
*----DV2K915414 Add DTALOSIG
    CONDENSE ls_output-employee_name.
    IF  ls_output-employee_name IS INITIAL.
      ls_output-employee_name = lco_vacant.
    ENDIF.
*----DV2K915414 End of Add DTALOSIG
    APPEND ls_output TO gt_output.

  ENDIF.



ENDFORM.                    " F_DETAILS

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*  Display report
*----------------------------------------------------------------------*
FORM f_display_report USING it_output TYPE ty_output_t.

  FIELD-SYMBOLS: <fs_fieldcat> TYPE slis_fieldcat_alv.

  CONSTANTS: lco_structure TYPE tabname VALUE 'ZST_HR_ORGREP_TO_ORGPLUS'.

* Build field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = lco_structure
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc EQ 0.
    UNASSIGN <fs_fieldcat>.
    LOOP AT gt_fieldcat ASSIGNING <fs_fieldcat>.
      CLEAR: <fs_fieldcat>-reptext_ddic,
             <fs_fieldcat>-ref_tabname.
      CASE <fs_fieldcat>-fieldname.
        WHEN 'POSITION'.
          <fs_fieldcat>-key       = co_mark_x.
          <fs_fieldcat>-outputlen = 12.
          <fs_fieldcat>-seltext_s = 'Position'.             "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Position'.             "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Position'.             "#EC NOTEXT

        WHEN 'REPORTS_TO'.
          <fs_fieldcat>-key       = co_mark_x.
          <fs_fieldcat>-outputlen = 12.
          <fs_fieldcat>-seltext_s = 'Reports To'.           "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Reports To'.           "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Reports To'.           "#EC NOTEXT

        WHEN 'EMPLOYEE_ID'.
          <fs_fieldcat>-outputlen = 12.
          <fs_fieldcat>-seltext_s = 'Employee ID'.          "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Employee ID'.          "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Employee ID'.          "#EC NOTEXT

        WHEN 'EMPLOYEE_NAME'.
          <fs_fieldcat>-outputlen = 27.
          <fs_fieldcat>-seltext_s = 'Name'.                 "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Name'.                 "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Name'.                 "#EC NOTEXT

        WHEN 'JOB_TITLE'.
          <fs_fieldcat>-outputlen = 30.
          <fs_fieldcat>-seltext_s = 'Job Title'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Job Title'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Job Title'.            "#EC NOTEXT

        WHEN 'DEPARTMENT'.
          <fs_fieldcat>-outputlen = 25.
          <fs_fieldcat>-seltext_s = 'Department'.           "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Department'.           "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Department'.           "#EC NOTEXT

        WHEN 'EMAIL'.
          <fs_fieldcat>-outputlen = 30.
          <fs_fieldcat>-seltext_s = 'E-mail Address'.       "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'E-mail Address'.       "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'E-mail Address'.       "#EC NOTEXT

        WHEN 'JOB_GRADE'.
          <fs_fieldcat>-outputlen = 13.
          <fs_fieldcat>-seltext_s = 'Job Grade'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Job Grade'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Job Grade'.            "#EC NOTEXT

        WHEN 'WORK_CATEGORY'.
          <fs_fieldcat>-no_out = 'X'.
*          <fs_fieldcat>-outputlen = 16.
*          <fs_fieldcat>-seltext_s = 'Worker Category'.      "#EC NOTEXT
*          <fs_fieldcat>-seltext_m = 'Worker Category'.      "#EC NOTEXT
*          <fs_fieldcat>-seltext_l = 'Worker Category'.      "#EC NOTEXT

        WHEN 'HIRE_DATE'.
          <fs_fieldcat>-outputlen = 12.
          <fs_fieldcat>-seltext_s = 'Hire Date'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Hire Date'.            "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Hire Date'.            "#EC NOTEXT

        WHEN 'EMP_STATUS'.
          <fs_fieldcat>-outputlen = 18.
          <fs_fieldcat>-seltext_s = 'Employee Status'.      "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Employee Status'.      "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Employee Status'.      "#EC NOTEXT

        WHEN 'SALARY'.
          <fs_fieldcat>-outputlen = 15.
          <fs_fieldcat>-seltext_s = 'Salary'.               "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Salary'.               "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Salary'.               "#EC NOTEXT

        WHEN 'AGE'.
          <fs_fieldcat>-outputlen = 5.
          <fs_fieldcat>-seltext_s = 'Age'.                  "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Age'.                  "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Age'.                  "#EC NOTEXT

        WHEN 'GENDER'.
          <fs_fieldcat>-outputlen = 7.
          <fs_fieldcat>-seltext_s = 'Gender'.               "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Gender'.               "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Gender'.               "#EC NOTEXT

        WHEN 'MARITAL_STATUS'.
          <fs_fieldcat>-outputlen = 16.
          <fs_fieldcat>-seltext_s = 'Marital Status'.       "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Marital Status'.       "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Marital Status'.       "#EC NOTEXT

        WHEN 'NATIONALITY'.
          <fs_fieldcat>-outputlen = 15.
          <fs_fieldcat>-seltext_s = 'Nationality'.          "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Nationality'.          "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Nationality'.          "#EC NOTEXT

        WHEN 'DATE_OF_BIRTH'.
          <fs_fieldcat>-outputlen = 14.
          <fs_fieldcat>-seltext_s = 'Date of Birth'.        "#EC NOTEXT
          <fs_fieldcat>-seltext_m = 'Date of Birth'.        "#EC NOTEXT
          <fs_fieldcat>-seltext_l = 'Date of Birth'.        "#EC NOTEXT

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDIF.

* Display report
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_PF_STATUS_SET'
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat              = gt_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_output[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.                         "#EC FB_RC

ENDFORM.                    " F_DISPLAY_REPORT

*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_SET
*&---------------------------------------------------------------------*
*  Set PF status of ALV
*----------------------------------------------------------------------*
FORM f_pf_status_set USING ct_extab TYPE slis_t_extab.      "#EC CALLED

  SET PF-STATUS 'ZSTATUS'.

ENDFORM.                    " F_PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*  User command routine of ALV
*----------------------------------------------------------------------*
FORM f_user_command USING iv_ucomm    TYPE syucomm
                          is_selfield TYPE slis_selfield.   "#EC CALLED

  TYPES: BEGIN OF ty_excel_s,
           position(8)        TYPE c,
           reports_to(10)     TYPE c,
           employee_id(11)    TYPE c,
           employee_name(80)  TYPE c,
           job_title(40)      TYPE c,
           department(40)     TYPE c,
*           email(241)         TYPE c,  "DV2K915414 DTALOSIG
           job_grade(20)      TYPE c,
           "work_category(20)  TYPE c,
           hire_date(10)      TYPE c,
           emp_status(40)     TYPE c,
*           salary(18)         TYPE c,  "DV2K915414 DTALOSIG
           age(3)             TYPE c,
           gender(6)          TYPE c,
           marital_status(14) TYPE c,
           nationality(15)    TYPE c,
           date_of_birth(13)  TYPE c,    "DV2K915414 DTALOSIG
         END OF ty_excel_s.

  DATA: lt_excel    TYPE STANDARD TABLE OF ty_excel_s,
        ls_excel    TYPE ty_excel_s,
        ls_output   TYPE zst_hr_orgrep_to_orgplus,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_action   TYPE i,
        lv_strlen1  TYPE i,
        lv_strlen2  TYPE i,
        lv_salary   TYPE pad_amt7s.

  CASE iv_ucomm.
    WHEN 'EXCEL'.
      CLEAR ls_excel.
      ls_excel-position       = 'Position'.                 "#EC NOTEXT
      ls_excel-reports_to     = 'Reports To'.               "#EC NOTEXT
      ls_excel-employee_id    = 'Employee ID'.              "#EC NOTEXT
      ls_excel-employee_name  = 'Name'.                     "#EC NOTEXT
      ls_excel-job_title      = 'Job Title'.                "#EC NOTEXT
      ls_excel-department     = 'Department'.               "#EC NOTEXT
*      ls_excel-email          = 'E-mail Address'.           "#EC NOTEXT
      ls_excel-job_grade      = 'Job Grade'.                "#EC NOTEXT
      "ls_excel-work_category  = 'Worker Category'.          "#EC NOTEXT
      ls_excel-hire_date      = 'Hire Date'.                "#EC NOTEXT
      ls_excel-emp_status     = 'Employee Status'.          "#EC NOTEXT
*      ls_excel-salary         = 'Salary'.                   "#EC NOTEXT
      ls_excel-age            = 'Age'.                      "#EC NOTEXT
      ls_excel-gender         = 'Gender'.                   "#EC NOTEXT
      ls_excel-marital_status = 'Marital Status'.           "#EC NOTEXT
      ls_excel-nationality    = 'Nationality'.              "#EC NOTEXT
      ls_excel-date_of_birth  = 'Date of Birth'.            "#EC NOTEXT
      APPEND ls_excel TO lt_excel.

      CLEAR ls_output.
      LOOP AT gt_output INTO ls_output.
        CLEAR: ls_excel,
               lv_salary.
*        lv_salary = ls_output-salary.                      " DV2K915414 DTALOSIG
        MOVE-CORRESPONDING ls_output TO ls_excel.

        WRITE: ls_output-hire_date     TO ls_excel-hire_date     USING EDIT MASK '__/__/____',
               ls_output-date_of_birth TO ls_excel-date_of_birth USING EDIT MASK '__/__/____'.

        IF ls_excel-reports_to EQ '00000000'.
          CLEAR ls_excel-reports_to.
        ENDIF.

        IF ls_excel-employee_id EQ '00000000'.
          CLEAR ls_excel-employee_id.
        ENDIF.

*        IF ls_excel-hire_date EQ '00000000'.
        IF ls_excel-hire_date EQ '00/00/0000'.
          CLEAR ls_excel-hire_date.
        ENDIF.

        IF ls_excel-date_of_birth EQ '00/00/0000'.
          CLEAR ls_excel-date_of_birth.
        ENDIF.

        IF ls_excel-age EQ '00'.
          CLEAR ls_excel-age.
        ENDIF.

*        WRITE lv_salary TO ls_excel-salary CURRENCY 'PHP'. " DV2K915414 DTALOSIG
        APPEND ls_excel TO lt_excel.
      ENDLOOP.

      CLEAR: lv_filename,
             lv_path,
             lv_fullpath,
             lv_action.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        CHANGING
          filename             = lv_filename
          path                 = lv_path
          fullpath             = lv_fullpath
          user_action          = lv_action
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF  sy-subrc  EQ 0
      AND lv_action EQ cl_gui_frontend_services=>action_ok.
        lv_strlen1 = strlen( lv_fullpath ) - 3.
        lv_strlen2 = strlen( lv_fullpath ) - 4.
        IF  lv_fullpath+lv_strlen1(3) NE 'xls'
        AND lv_fullpath+lv_strlen1(3) NE 'XLS'
        AND lv_fullpath+lv_strlen2(3) NE 'xls'
        AND lv_fullpath+lv_strlen2(3) NE 'XLS'.
          CONCATENATE lv_fullpath
                      '.xls'
                 INTO lv_fullpath.                          "#EC NOTEXT
        ENDIF.

*       Download to Excel
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = lv_fullpath
            filetype                = 'DAT'
          TABLES
            data_tab                = lt_excel
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.                   "#EC FB_RC
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_USER_COMMAND