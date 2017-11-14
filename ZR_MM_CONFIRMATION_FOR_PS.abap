*&---------------------------------------------------------------------*
*& Report     : ZR_MM_CONFIRMATION_FOR_PS
*&              copied from ZR_MM_RECLASS_FOR_PS
*& Description: Confirmation Program for Project System (PS)
*&---------------------------------------------------------------------*
*& Created by : CDELAPENA
*& Created On : 11/10/2013
*& Reference  : N/A
*&---------------------------------------------------------------------*
*&  Date      |Author ID   | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 11/26/2012 | MDEVERA    | ARANGKADA  | Initial Development
*& 11/09/2013 | CDELAPENA  | ARANGKADA  | Enhance Program
*& 04/28/2014 | RANDYSY    | TN#60934   | Remove Auto-Posting
*&---------------------------------------------------------------------*

REPORT zr_mm_confirmation_for_ps NO STANDARD PAGE HEADING MESSAGE-ID zmwsi.

*&----------------------------------------------------------------------
*& Tables
**&---------------------------------------------------------------------
TABLES: mseg,proj. "bkpf, bseg, makt.

*&----------------------------------------------------------------------
*& Type Pools
*&---------------------------------------------------------------------

TYPE-POOLS: abap.

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------
TYPES: BEGIN OF ty_mseg,
        mblnr      TYPE mseg-mblnr,
        mjahr      TYPE mseg-mjahr,
        zeile      TYPE mseg-zeile,
        bwart      TYPE mseg-bwart,
        matnr      TYPE mseg-matnr,
        werks      TYPE mseg-werks,
        lgort      TYPE mseg-lgort,
        lifnr      TYPE mseg-lifnr,
        waers      TYPE mseg-waers,
        dmbtr      TYPE mseg-dmbtr,
        erfmg      TYPE mseg-erfmg,
        erfme      TYPE mseg-erfme,
        smbln      TYPE mseg-smbln,
        smblp      TYPE mseg-smblp,
        kostl      TYPE mseg-kostl,
        aufnr      TYPE mseg-aufnr,
        bukrs      TYPE mseg-bukrs,
        prctr      TYPE mseg-prctr,
        fistl      TYPE mseg-fistl,
        ps_psp_pnr TYPE mseg-ps_psp_pnr,
        mat_pspnr  TYPE mseg-mat_pspnr,
        nplnr      TYPE mseg-nplnr,
        budat_mkpf TYPE mseg-budat_mkpf,
        erfmg_open TYPE mseg-erfmg,
       END OF ty_mseg.

TYPES : BEGIN OF t_bkpf,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         blart TYPE bkpf-blart,
         bktxt TYPE bkpf-bktxt,
END OF t_bkpf.

TYPES : BEGIN OF t_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         dmbtr TYPE bseg-dmbtr,
         zuonr TYPE bseg-zuonr,
         aufnr TYPE bseg-aufnr,
  END OF t_bseg.

TYPES : BEGIN OF t_message,
        message(255),
        END OF t_message.

TYPES: BEGIN OF ty_mseg_rev,
         mblnr TYPE mseg-mblnr,
         zeile TYPE mseg-zeile,
         smbln TYPE mseg-smbln,
         smblp TYPE mseg-smblp,
END OF ty_mseg_rev.

TYPES: BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_mkpf,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        budat TYPE mkpf-budat,
        bktxt TYPE mkpf-bktxt,
       END OF ty_mkpf.

TYPES: BEGIN OF ty_final,
*& add asee 20121206
        cbox         TYPE c,
        node         TYPE icon-id.
        INCLUDE TYPE ty_mseg.
TYPES:  maktx TYPE makt-maktx,
        opqty     TYPE mseg-erfmg,
        used      TYPE mseg-erfmg,
        unret     TYPE mseg-erfmg,
        excess    TYPE mseg-erfmg,
        dfctve    TYPE mseg-erfmg,
        migo_qty  TYPE mseg-erfmg,
        migo_lgort TYPE mseg-lgort,
        stat_migo TYPE icon-id,
        mess_migo TYPE bapi_msg,
        stat_f02  TYPE icon-id,
        mess_f02  TYPE bapi_msg,
        stat_f43  TYPE icon-id,
        mess_f43  TYPE bapi_msg.
TYPES  END OF ty_final.

TYPES: BEGIN OF ty_glacct,
         matnr TYPE mara-matnr,
         komok TYPE t030-komok,
         hkont TYPE t030-konts,
      END OF ty_glacct.

TYPES: BEGIN OF ty_vendor,
         nplnr TYPE mseg-nplnr,
         lifnr TYPE ihpa-parnr,
       END OF ty_vendor.

TYPES: BEGIN OF ty_msegopenqty,
        matnr     TYPE mseg-matnr,
        werks     TYPE mseg-werks,
        lgort     TYPE mseg-lgort,
        nplnr     TYPE mseg-nplnr,
        mat_pspnr TYPE mseg-mat_pspnr,
        openqty   TYPE mseg-erfmg,
       END OF ty_msegopenqty.

*&---------------------------------------------------------------------
*& Data Definitions - Internal Tables (gt_<itab name>)
*&---------------------------------------------------------------------
DATA: gt_mseg        TYPE TABLE OF ty_mseg,
      gt_mseg_tmp    TYPE TABLE OF ty_msegopenqty,
      gt_mseg_rev    TYPE TABLE OF ty_mseg_rev,
      gt_mkpf        TYPE TABLE OF ty_mkpf,
      gt_final       TYPE TABLE OF ty_final,
      gt_vendor      TYPE TABLE OF ty_vendor,
      gt_acctgl      TYPE TABLE OF bapiacgl09,
      gt_currency    TYPE TABLE OF bapiaccr09,
      gt_extension2  TYPE TABLE OF bapiparex,
      gt_return      TYPE TABLE OF bapiret2,
      gt_acctpayable TYPE TABLE OF bapiacap09,
      gt_fcat        TYPE lvc_t_fcat,
      gt_makt        TYPE SORTED TABLE OF ty_makt     WITH NON-UNIQUE KEY matnr,
      gt_glacct      TYPE STANDARD TABLE OF ty_glacct WITH NON-UNIQUE KEY matnr komok,
      gt_ztmmrecps   TYPE TABLE OF ztmmrecps,
      gt_bkpf        TYPE TABLE OF t_bkpf             WITH HEADER LINE,
      gt_proj        TYPE STANDARD TABLE OF proj      WITH HEADER LINE,
      gt_prps        TYPE STANDARD TABLE OF prps      WITH HEADER LINE,
      gt_afko        TYPE STANDARD TABLE OF afko      WITH HEADER LINE.

*-- BDC Recording Tables and Work Areas
DATA : it_bdcdata LIKE TABLE OF bdcdata,
       wa_bdcdata TYPE bdcdata,
       it_message LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
       wa_message TYPE bdcmsgcoll.

**&---------------------------------------------------------------------
**& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
**&                    Structure/Work Area (gs_<name>)
**&---------------------------------------------------------------------
DATA: gv_flag_reversed  TYPE c,
    gv_plant_name     TYPE name1,
    gv_gridtitle      TYPE lvc_title,
    gs_final          TYPE ty_final,
    gs_final_add      TYPE ty_final,
    gs_layout         TYPE lvc_s_layo,
    gs_fcat           TYPE lvc_s_fcat,
    gs_docheader      TYPE bapiache09,
    gs_return         TYPE bapiret2,
    gv_mvttype        TYPE bwart,
    gv_mvttype_rev    TYPE bwart VALUE 'Z53', "-- Added by RANDYSY TN#59247 03/21/2014
    gr_bwart          TYPE RANGE OF bwart,
    gs_bwart          LIKE LINE OF gr_bwart,
    gr_bwart_rev      TYPE RANGE OF bwart,
    gv_addrec         TYPE numc2,
    gv_linecount      TYPE numc2,
    gv_material TYPE mseg-mblnr,
    gv_matdoc   TYPE mseg-matnr,
    gv_xblnr    TYPE string,
    gv_zeile    TYPE mseg-zeile,
    gv_erfmg    TYPE mseg-erfmg,
    gv_dmbtr    TYPE mseg-dmbtr.
*&---------------------------------------------------------------------
*& Data Definitions - Constants (co_<name>)
*&---------------------------------------------------------------------
CONSTANTS: co_e       TYPE c          VALUE 'E',
           co_x       TYPE c          VALUE 'X',
           co_p       TYPE c          VALUE 'P',
           co_add     TYPE char3      VALUE 'ADD',
           co_ieq     TYPE char3      VALUE 'IEQ',
           co_del     TYPE char3      VALUE 'DEL',
           co_tab     TYPE char10     VALUE '          ',
           co_error   TYPE icon-id    VALUE '@0A@',
           co_success TYPE icon-id    VALUE '@08@',
           co_gl      TYPE char2      VALUE 'GL',
           co_returns TYPE char2      VALUE 'RT',
           co_vendor  TYPE char2      VALUE 'VN',
           co_lf      TYPE ihpa-parvw VALUE 'LF',
           co_jv      TYPE blart      VALUE 'JV',
           co_ut      TYPE blart      VALUE 'UT',
           co_ka      TYPE blart      VALUE 'KA',
           co_ur      TYPE blart      VALUE 'UR',
           co_i0      TYPE mwskz      VALUE 'I0',
           co_vbr     TYPE t030-komok VALUE 'VBR',
           co_zvb     TYPE t030-komok VALUE 'ZVB',
           co_gbb     TYPE t030-ktosl VALUE 'GBB',
           co_81      TYPE bschl      VALUE '81',
           co_31      TYPE bschl      VALUE '31',
           co_21      TYPE bschl      VALUE '21',
           co_40      TYPE bschl      VALUE '40',
           co_50      TYPE bschl      VALUE '50',
           co_03      TYPE  bapi2017_gm_code  VALUE '03',
           co_q       TYPE  char1     VALUE 'Q',
           co_accit   TYPE bapiparex-structure VALUE 'ACCIT',
           co_zreclass_bwart     TYPE rvari_vnam VALUE 'ZRECLASS_BWART_PS',
           co_zreclass_bwart_rev TYPE rvari_vnam VALUE 'ZRECLASS_BWART_PS_REV',
           co_bwart              TYPE char5 VALUE 'BWART'.
*&---------------------------------------------------------------------
*& Data Definitions - Field Symbols
*&---------------------------------------------------------------------
FIELD-SYMBOLS: <fs_mseg>      LIKE LINE OF gt_mseg,
               <fs_mseg_rev>  LIKE LINE OF gt_mseg_rev,
               <fs_makt>      LIKE LINE OF gt_makt,
               <fs_mkpf>      LIKE LINE OF gt_mkpf,
               <fs_tfinal>    TYPE ty_final,
               <fs_fcat>      TYPE lvc_s_fcat,
               <fs_ztmmrecps> TYPE ztmmrecps.
**&---------------------------------------------------------------------
**& Program Selections
**&---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  so_matnr FOR  mseg-matnr.
SELECT-OPTIONS:  so_pspid FOR  proj-pspid OBLIGATORY.
*PARAMETERS:      pa_werks TYPE mseg-werks OBLIGATORY DEFAULT '5000'.
*                 so_nplnr FOR  mseg-nplnr,
*                 so_budat FOR  mseg-budat_mkpf.
SELECTION-SCREEN END OF BLOCK blk01.
*&---------------------------------------------------------------------
*& At Selection Screen
*&---------------------------------------------------------------------
AT SELECTION-SCREEN.
*  PERFORM check_valid_werks.
*&---------------------------------------------------------------------
*& Initialization
*&---------------------------------------------------------------------
INITIALIZATION.
  "--Get default variables
  PERFORM get_initial_values.
*&---------------------------------------------------------------------
*& Start of Selection
*&---------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM get_data_from_ztmmrecps. "Reclass Qty Lookup Table
  PERFORM get_data.
  PERFORM filter_data.
  PERFORM populate_fin_table.

**-- Post Outdated Items
  PERFORM auto_post.

*&---------------------------------------------------------------------
*& End of Selection
*&---------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Retrieve Data needed for processing.
*----------------------------------------------------------------------*
FORM get_data.

*-- Select from PROJ
  SELECT * FROM proj
    INTO TABLE gt_proj
   WHERE pspid IN so_pspid.

*-- Select from PRPS
  IF gt_proj[] IS NOT INITIAL.

    SELECT * FROM prps
      INTO TABLE gt_prps
       FOR ALL ENTRIES IN gt_proj
     WHERE psphi = gt_proj-pspnr.

    IF gt_prps[] IS NOT INITIAL.

      SELECT * FROM afko
        INTO TABLE gt_afko
        FOR ALL ENTRIES IN gt_proj
        WHERE pronr     EQ gt_proj-pspnr.

      IF gt_afko[] IS NOT INITIAL.

        SELECT mblnr
               mjahr
               zeile
               bwart
               matnr
               werks
               lgort
               lifnr
               waers
               dmbtr
               erfmg
               erfme
               smbln
               smblp
               kostl
               aufnr
               bukrs
               prctr
               fistl
               ps_psp_pnr
               mat_pspnr
               nplnr
               budat_mkpf
               erfmg
          FROM mseg
          INTO TABLE gt_mseg
           FOR ALL ENTRIES IN gt_afko
         WHERE matnr       IN so_matnr
           AND nplnr       EQ gt_afko-aufnr
          AND bwart        IN gr_bwart.

*  IF so_budat IS NOT INITIAL.
*    DELETE gt_mseg WHERE budat_mkpf NOT IN so_budat.
*  ENDIF.

*  IF so_wbs IS NOT INITIAL.
*    DELETE gt_mseg WHERE mat_pspnr NOT IN so_wbs.
*  ENDIF.

        IF gt_mseg[] IS NOT INITIAL.
*-Checking Criteria for a material if it is already reversed.
          SELECT mblnr
                 zeile
                 smbln
                 smblp
            FROM mseg
            INTO TABLE gt_mseg_rev
            FOR ALL ENTRIES IN gt_mseg
           WHERE smbln EQ gt_mseg-mblnr
             AND smblp EQ gt_mseg-zeile
             AND bwart EQ gv_mvttype_rev.

          IF gt_mseg_rev[] IS NOT INITIAL.
*     Table contains reversed documents.
            gv_flag_reversed = co_x.
          ENDIF.

*    if gv_plant_name is not initial.
*      concatenate 'plant'(002) pa_werks into gv_gridtitle separated by space.
*      concatenate gv_gridtitle gv_plant_name into gv_gridtitle separated by co_tab.
*    endif.

*-Obtaining Material Description from MAKT.
          SELECT DISTINCT matnr
                          maktx
            FROM makt
            INTO TABLE gt_makt
            FOR ALL ENTRIES IN gt_mseg
           WHERE matnr EQ gt_mseg-matnr.

          IF sy-subrc EQ 0.                                 "#EC NEEDED
            " Do nothing
          ENDIF.

*-Obtaining Posting Date(BUDAT) and Document Header Text(BKTXT) from MKPF.
          SELECT mblnr
                 mjahr
                 budat
                 bktxt
            FROM mkpf
            INTO TABLE gt_mkpf
            FOR ALL ENTRIES IN gt_mseg
           WHERE mblnr EQ gt_mseg-mblnr
             AND mjahr EQ gt_mseg-mjahr.

          IF sy-subrc EQ 0.                                 "#EC NEEDED
            SORT gt_mkpf BY mblnr mjahr.
          ENDIF.

          "--Get Vendor Data
          PERFORM get_vendor_details.

        ELSE.
          MESSAGE s208(00) WITH 'No "GI to Clearing" transaction posted'(003) DISPLAY LIKE co_e.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  filter_data
*&---------------------------------------------------------------------*
*       Filter data by removing reversed documents in list of Mat. Doc.
*----------------------------------------------------------------------*
FORM filter_data.

  CHECK gt_mseg IS NOT INITIAL.

* Filter data by removing reversed documents
  IF gv_flag_reversed EQ co_x.
    LOOP AT gt_mseg_rev ASSIGNING <fs_mseg_rev>.
      DELETE gt_mseg WHERE mblnr EQ <fs_mseg_rev>-smbln
                       AND zeile EQ <fs_mseg_rev>-smblp.
    ENDLOOP.
    UNASSIGN <fs_mseg_rev>.
  ENDIF.
  DELETE gt_mseg WHERE smbln IS NOT INITIAL.

  "--Synchronize Actual Open Qty with Reclass Open Qty
  PERFORM synch_open_qty.

  IF gt_mseg IS NOT INITIAL.
    "--GL Account Information Based on Material
    PERFORM get_glacct_data.
  ELSE.
    MESSAGE s398(00) WITH 'No data to display. Selected'(024) ' Item(s) have already been cleared/reversed'(025) DISPLAY LIKE co_e. "#EC MG_PAR_CNT
  ENDIF.

ENDFORM.                    " filter_data
*&---------------------------------------------------------------------*
*&      Form  POPULATE_FIN_TABLE
*&---------------------------------------------------------------------*
*       Fill up final output Table to be used for ALV display
*----------------------------------------------------------------------*
FORM populate_fin_table.

  CHECK gt_mseg IS NOT INITIAL.

  SORT gt_ztmmrecps BY  mblnr zeile budat_mkpf werks.

  LOOP AT gt_mseg ASSIGNING <fs_mseg>.
    gs_final-node = '@3P@'. "Initialize Main Nodes
*& add ASEE 20121205 conv routine
    PERFORM conv_cunit_input USING <fs_mseg>-erfme .
    MOVE-CORRESPONDING <fs_mseg> TO gs_final.
    READ TABLE gt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mseg>-matnr
                                           BINARY SEARCH.
    IF <fs_makt> IS ASSIGNED.
      gs_final-maktx = <fs_makt>-maktx.
      UNASSIGN <fs_makt>.
    ENDIF.
    READ TABLE gt_ztmmrecps ASSIGNING <fs_ztmmrecps> WITH KEY mblnr = <fs_mseg>-mblnr
                                                              zeile = <fs_mseg>-zeile
                                                         budat_mkpf = <fs_mseg>-budat_mkpf
                                                              werks = <fs_mseg>-werks
                                                         BINARY SEARCH.
    IF <fs_ztmmrecps> IS ASSIGNED.
      gs_final-mblnr      = <fs_ztmmrecps>-mblnr.
      gs_final-zeile      = <fs_ztmmrecps>-zeile.
      gs_final-budat_mkpf = <fs_ztmmrecps>-budat_mkpf.
      gs_final-werks      = <fs_ztmmrecps>-werks.
      gs_final-mat_pspnr  = <fs_ztmmrecps>-wbs.
      gs_final-aufnr      = <fs_ztmmrecps>-aufnr.
      gs_final-matnr      = <fs_ztmmrecps>-matnr.
      gs_final-maktx      = <fs_ztmmrecps>-maktx.
      gs_final-erfmg      = <fs_ztmmrecps>-erfmg.
      gs_final-opqty      = <fs_ztmmrecps>-openqty.
*      gs_final-opqty  = <fs_mseg>-erfmg_open.
*& add ASEE 20121205 conv routine
      PERFORM conv_cunit_input USING <fs_ztmmrecps>-une .
      gs_final-erfme      = <fs_ztmmrecps>-une.
      UNASSIGN <fs_ztmmrecps>.
    ELSE.
      gs_final-opqty  = <fs_mseg>-erfmg_open.
*      gs_final-opqty  = <fs_mseg>-erfmg.
    ENDIF.

    APPEND gs_final TO gt_final.
    CLEAR: gs_final.
  ENDLOOP.

*  DELETE gt_final WHERE opqty LE 0.           " CDELAPENA   12/10/2013

  IF gt_final IS INITIAL.
    MESSAGE s398(00) WITH 'No data to display. Selected'(024) ' Item(s) have already been cleared/reversed'(025) DISPLAY LIKE co_e. "#EC MG_PAR_CNT
  ENDIF.

  SORT gt_final BY mblnr zeile.

ENDFORM.                    " POPULATE_FIN_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       ALV Grid Display
*----------------------------------------------------------------------*
FORM display_alv .

  CHECK gt_final IS NOT INITIAL.
  "--Fieldcatalog and Layout
*  PERFORM create_fcat_and_layout.                 " CDELAPENA 12/02/2013

  SORT gt_final BY mess_f43 ASCENDING.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer       = 'X'
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_grid_title             = gv_gridtitle
      is_layout_lvc            = gs_layout
      i_html_height_top        = '95'
      it_fieldcat_lvc          = gt_fcat
    TABLES
      t_outtab                 = gt_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.                                         "#EC NEEDED
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " DISPLAY_FINAL_ALV
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       User Command Event
*----------------------------------------------------------------------*
*      -->R_UCOMM      User Command
*      -->RS_SELFIELD  Selected Field
*----------------------------------------------------------------------*
FORM user_command  USING r_ucomm LIKE sy-ucomm              "#EC CALLED
                         rs_selfield TYPE slis_selfield.

  DATA lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CALL METHOD lo_grid->check_changed_data.
  rs_selfield-refresh = abap_true.

  CASE r_ucomm.
    WHEN 'POST' OR '&DATA_SAVE' OR 'POST2'. "switch to save button as reclass button
      PERFORM determine_activity.
    WHEN 'SEL_ALL'.
      PERFORM ctrl_checkbox USING abap_true.
    WHEN 'DESEL_ALL'.
      PERFORM ctrl_checkbox USING abap_false.
    WHEN '&IC1'.  "double click event
*      BREAK ASEE .
      PERFORM set_msg_popup USING rs_selfield-fieldname
                                  rs_selfield-tabindex .
    WHEN 'REVERSE'.
      PERFORM reverse_document.
    WHEN OTHERS.
      "Do nothing
  ENDCASE.

  rs_selfield-refresh     = abap_true .
  rs_selfield-col_stable  = abap_true .
  rs_selfield-row_stable  = abap_true .

  CALL METHOD lo_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       Apply GUI Status
*----------------------------------------------------------------------*
*      -->RT_EXTAB   Exclusion Table
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC NEEDED
*& begin of change by asee 20121205 (remove reclass button)
  SET PF-STATUS 'ZPFSTATUS1'.
*& end of change by asee 20121205 (remove reclass button)
ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  CREATE_FCAT_AND_LAYOUT
*&---------------------------------------------------------------------*
*       Build Field Catalog and ALV Layout
*----------------------------------------------------------------------*
FORM create_fcat_and_layout.
*&---------------------------------------------------------------------
*& MACRO Definitions
*&---------------------------------------------------------------------
  "--MACRO: FieldCatalog
  DEFINE mc_fcat.
    case &1.
      when 'CBOX'.
        gs_fcat-outputlen = 2.
        gs_fcat-checkbox = abap_true.
        gs_fcat-no_out    = abap_true.
      when 'NODE'.
        gs_fcat-icon      = abap_true.
        gs_fcat-no_out    = abap_true.
        gs_fcat-outputlen = 5.
      when 'STAT_MIGO' or 'STAT_F02' or 'STAT_F43'.
        gs_fcat-icon      = abap_true.
        gs_fcat-outputlen = 5.
*& add asee 20121205 (hide fields)
        gs_fcat-no_out    = abap_true .
      when 'MATNR'.
        gs_fcat-no_zero  = abap_true.
      when 'MBLNR'.
        gs_fcat-outputlen  = 12.
*      when 'NPLNR'.
*        gs_fcat-no_zero  = abap_true.
      when 'MIGO_LGORT'.
        gs_fcat-ref_table  = 'T001L'.
        gs_fcat-ref_field  = 'LGORT'.
        gs_fcat-outputlen  = 8.
      when 'ZEILE'.
        gs_fcat-just      = 'R'.
      when 'ERFMG' or 'OPQTY' or 'USED' or 'UNRET' or 'DFCTVE' or 'MIGO_QTY'.
        gs_fcat-decimals  = 3.
        gs_fcat-qfieldname  = 'ERFME'.
      when 'MESS_MIGO' or 'MESS_F02' or 'MESS_F43'.
        gs_fcat-outputlen  = 50.
*& add asee 20121205 (hide fields)
        gs_fcat-no_out    = abap_true .
      when others.
        "
    endcase.
    gs_fcat-fieldname  = &1.
    gs_fcat-scrtext_l  = &2.
    gs_fcat-colddictxt = 'L'.
    gs_fcat-edit       = &3.
    append gs_fcat to gt_fcat.
    clear gs_fcat.
  END-OF-DEFINITION.

*--ALV FieldCatalog
  mc_fcat 'CBOX'        space                       abap_true.
  mc_fcat 'NODE'        space                       space.
  mc_fcat 'MBLNR'       'Mat. Doc.'(004)            space.
  mc_fcat 'ZEILE'       'Item'(005)                 space.
  mc_fcat 'BUDAT_MKPF'  'Posting Date'(006)         space.
  mc_fcat 'MATNR'       'Material'(007)             space.
  mc_fcat 'MAKTX'       'Material Description'(008) space.
  mc_fcat 'ERFMG'       'Quantity'(009)             space.
  mc_fcat 'OPQTY'       'Open Quantity'(010)        space.
  mc_fcat 'ERFME'       'UnE'                       space.
  mc_fcat 'NPLNR'       'Network'(029)              space.
  mc_fcat 'USED'        'Used'(012)                 abap_true.
  mc_fcat 'UNRET'       'Unreturned'(013)           abap_true.
*  mc_fcat 'DFCTVE'      'Defective'(014)            abap_true.
*  mc_fcat 'MIGO_QTY'    'Returns'(032)              abap_true.
*  mc_fcat 'MIGO_LGORT'  'Stor. Loc'(033)            abap_true.
  mc_fcat 'STAT_F02'    'Status'(015)               space.
  mc_fcat 'MESS_F02'    'Messages (Used)'(016)      space.
  mc_fcat 'STAT_F43'    'Status'(015)               space.
  mc_fcat 'MESS_F43'    'Messages (Excess/Defective)'(018)  space.
  mc_fcat 'STAT_MIGO'   'Status'(015)               space.
  mc_fcat 'MESS_MIGO'   'Messages (Returns)'(031)   space.

*--ALV Layout
  gs_layout-zebra        = abap_true.
  gs_layout-cwidth_opt   = abap_true.

ENDFORM.                    " CREATE_FCAT_AND_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GET_INITIAL_VALUES
*&---------------------------------------------------------------------*
*       Get Values from TVARVC
*----------------------------------------------------------------------*
FORM get_initial_values .

*  PERFORM get_single_val IN PROGRAM zua_subroutines_isu IF FOUND:
*
*                         USING co_zreclass_bwart
*                         CHANGING gv_mvttype,
*
*                         USING co_zreclass_bwart_rev
*                         CHANGING gv_mvttype_rev.

  CONCATENATE 'IEQ' 'Z52' INTO gs_bwart.
  APPEND gs_bwart TO gr_bwart.
  CLEAR gs_bwart.

*  CONCATENATE 'IEQ' 'Z53' INTO gs_bwart.
*  APPEND gs_bwart TO gr_bwart.

ENDFORM.                    " GET_INITIAL_VALUES
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_ACTIVITY
*&---------------------------------------------------------------------*
*       Validation for posting via transactions F-02 or F-65
*----------------------------------------------------------------------*
FORM determine_activity.

  DATA: lv_opqty  TYPE mseg-erfmg,
        lv_trqty  TYPE mseg-erfmg.

  "--Refresh Status/Messages
  PERFORM refresh_messages.

  LOOP AT gt_final ASSIGNING <fs_tfinal>.

    CLEAR: lv_opqty,
           lv_trqty.

    lv_opqty = <fs_tfinal>-opqty.
    lv_trqty = <fs_tfinal>-used + <fs_tfinal>-unret.

    "--Check Total Quantity: If Open Qty is lesser than the transaction quantity

    IF lv_opqty LT lv_trqty.

      <fs_tfinal>-stat_f02   = co_error.
      <fs_tfinal>-mess_f02   = text-017.
      " Unhide Column
      PERFORM unhide_fcat_column USING:  'STAT_F02',
                                         'MESS_F02'.
      PERFORM set_field_as_hotspot USING 'MESS_F02' .

    ELSE.

      "--Proceed to Actual Posting (Reclass)
      IF <fs_tfinal>-used IS NOT INITIAL.
        PERFORM post_gl_f02. " G/L Posting (Transaction F-02)
      ENDIF.

      IF <fs_tfinal>-unret IS NOT INITIAL.
        PERFORM post_gl_f65. " Vendor PARK (Transaction F-65)
      ENDIF.

      "--Post via MIGO
      IF <fs_tfinal>-migo_qty GT 0.
        IF <fs_tfinal>-migo_lgort IS INITIAL. "Stor Loc is REQUIRED if there's a RETURN Qty
          <fs_tfinal>-stat_migo = co_error.
          <fs_tfinal>-mess_migo = text-030.
          PERFORM unhide_fcat_column USING: 'STAT_MIGO',
                                            'MESS_MIGO'.
          PERFORM set_field_as_hotspot USING 'MESS_MIGO' .
        ELSE.
          PERFORM create_matdoc_migo.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " DETERMINE_ACTIVITY
*&---------------------------------------------------------------------*
*&      Form  POST_GL_F02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post_gl_f02 .
  PERFORM prepare_data_for_f02.
  PERFORM post_accounting_document USING co_gl.
ENDFORM.                    " POST_GL_F02
*&---------------------------------------------------------------------*
*&      Form  POST_GL_F65
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post_gl_f65 .
  PERFORM prepare_data_for_f65.
  PERFORM post_accounting_document USING co_vendor.
ENDFORM.                    " POST_GL_F65
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_FOR_F02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_data_for_f02 .

  PERFORM build_header_data    USING co_gl.
  PERFORM build_glacct_data    USING co_gl.
  PERFORM build_currency_data  USING co_gl.
  PERFORM build_extension_data USING co_gl.

ENDFORM.                    " PREPARE_DATA_FOR_F02
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_FOR_F43
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_data_for_f65 .

  PERFORM build_header_data    USING co_vendor.
  PERFORM build_glacct_data    USING co_vendor.
  PERFORM build_currency_data  USING co_vendor.
  PERFORM build_extension_data USING co_vendor.
  PERFORM build_vendor_data.

ENDFORM.                    " PREPARE_DATA_FOR_F43
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER_DATA
*&---------------------------------------------------------------------*
*       Header Data
*----------------------------------------------------------------------*
*       -> PV_ID  Identifier (G/L or Vendor)
*----------------------------------------------------------------------*
FORM build_header_data USING pv_id TYPE char2.

  DATA : lv_used  TYPE string,
         lv_unret  TYPE string.

*-- Transfer data
  lv_used  = <fs_tfinal>-used.
  lv_unret = <fs_tfinal>-unret.

  CLEAR gs_docheader.

*--Accounting Document Type
  IF pv_id EQ co_gl.
    gs_docheader-doc_type   = co_ut.                   " CDELAPENA 11/11/2013
  ELSEIF pv_id EQ co_vendor.
    gs_docheader-doc_type   = co_ur.                   " CDELAPENA 11/11/2013
  ENDIF.

  gs_docheader-doc_date   = sy-datum.
  gs_docheader-username   = sy-uname.
  gs_docheader-comp_code  = <fs_tfinal>-bukrs.
  gs_docheader-pstng_date = sy-datum.
  gs_docheader-fis_period = sy-datum+4(2).

*-- Document Header text
  IF pv_id EQ co_gl.

    CONDENSE lv_used.
    CONCATENATE lv_used <fs_tfinal>-matnr+12(6) INTO gs_docheader-header_txt
    SEPARATED BY '/'.

  ELSEIF pv_id EQ co_vendor.

    CONDENSE lv_unret.
    CONCATENATE  lv_unret <fs_tfinal>-matnr+12(6) INTO gs_docheader-header_txt
    SEPARATED BY '/'.

*    READ TABLE gt_mkpf ASSIGNING <fs_mkpf> WITH KEY mblnr = <fs_tfinal>-mblnr
*                                                    mjahr = <fs_tfinal>-mjahr
*                                           BINARY SEARCH.
*    IF <fs_mkpf> IS ASSIGNED.
*      gs_docheader-header_txt = <fs_mkpf>-bktxt.
*    ENDIF.
*
*    UNASSIGN <fs_mkpf>.

  ENDIF.

*-- Reference Number
  CONCATENATE <fs_tfinal>-mblnr <fs_tfinal>-zeile
              INTO gs_docheader-ref_doc_no.

ENDFORM.                    " BUILD_HEADER_DATA
*&---------------------------------------------------------------------*
*       GL Account Data
*----------------------------------------------------------------------*
*       -> PV_ID  Identifier (G/L or Vendor)
*----------------------------------------------------------------------*
FORM build_glacct_data USING pv_id TYPE char2.

  DATA ls_acctgl TYPE bapiacgl09.

  FIELD-SYMBOLS <fs_glacct> TYPE ty_glacct.

  REFRESH gt_acctgl.

  IF pv_id EQ co_gl.

    "--Line Item1
    READ TABLE gt_glacct ASSIGNING <fs_glacct> WITH KEY matnr = <fs_tfinal>-matnr
                                                        komok = co_vbr
                                                        BINARY SEARCH.
    READ TABLE so_pspid INDEX 1.

    IF <fs_glacct> IS ASSIGNED.
      ls_acctgl-itemno_acc  = 1.
      ls_acctgl-gl_account  = <fs_glacct>-hkont.
      ls_acctgl-profit_ctr  = <fs_tfinal>-prctr.
      ls_acctgl-orderid     = <fs_tfinal>-aufnr.
      ls_acctgl-network     = <fs_tfinal>-nplnr.
      ls_acctgl-funds_ctr   = <fs_tfinal>-fistl.
      ls_acctgl-alloc_nmbr  = <fs_tfinal>-mblnr.

      CONCATENATE text-034 so_pspid-low INTO ls_acctgl-item_text.                " CDELAPENA 11/11/2013

      ls_acctgl-ref_key_1   = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_2   = <fs_tfinal>-zeile.
      IF  <fs_tfinal>-kostl IS NOT INITIAL.
        ls_acctgl-costcenter  = <fs_tfinal>-kostl.
      ENDIF.
*      IF <fs_tfinal>-mat_pspnr IS NOT INITIAL.
*        ls_acctgl-wbs_element = <fs_tfinal>-mat_pspnr.
*        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*          EXPORTING
*            input  = <fs_tfinal>-mat_pspnr
*          IMPORTING
*            output = ls_acctgl-wbs_element.
*      ENDIF.
      IF <fs_tfinal>-nplnr IS NOT INITIAL.
        ls_acctgl-network     = <fs_tfinal>-nplnr.
      ENDIF.
      APPEND ls_acctgl TO gt_acctgl.
      CLEAR ls_acctgl.
      UNASSIGN <fs_glacct>.
    ENDIF.

    "--Line Item2
    READ TABLE gt_glacct ASSIGNING <fs_glacct> WITH KEY matnr = <fs_tfinal>-matnr
                                                        komok = co_zvb
                                                        BINARY SEARCH.
    IF <fs_glacct> IS ASSIGNED.
      ls_acctgl-itemno_acc  = 2.
      ls_acctgl-gl_account  = <fs_glacct>-hkont.
      ls_acctgl-profit_ctr  = <fs_tfinal>-prctr.
      ls_acctgl-orderid     = <fs_tfinal>-aufnr.
      ls_acctgl-alloc_nmbr  = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_1   = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_2   = <fs_tfinal>-zeile.
      IF  <fs_tfinal>-kostl IS NOT INITIAL.
        ls_acctgl-costcenter  = <fs_tfinal>-kostl.
      ENDIF.
*    IF <fs_tfinal>-mat_pspnr IS NOT INITIAL.
*      ls_acctgl-wbs_element = <fs_tfinal>-mat_pspnr.
*        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*          EXPORTING
*            input  = <fs_tfinal>-mat_pspnr
*          IMPORTING
*            output = ls_acctgl-wbs_element.
*    ENDIF.
      IF <fs_tfinal>-nplnr IS NOT INITIAL.
        ls_acctgl-network     = <fs_tfinal>-nplnr.
      ENDIF.
      IF pv_id EQ co_gl.
        ls_acctgl-funds_ctr   = <fs_tfinal>-fistl.
      ENDIF.
      CONCATENATE text-034 so_pspid-low INTO ls_acctgl-item_text.
      CONDENSE ls_acctgl-item_text.      " CDELAPENA 11/11/2013
      APPEND ls_acctgl TO gt_acctgl.
      CLEAR ls_acctgl.
      UNASSIGN <fs_glacct>.
    ENDIF.
  ENDIF.

*-- Start add CDELAPENA 11/25/2013

  IF pv_id EQ co_vendor.

*-- Read project Definition
    READ TABLE so_pspid INDEX 1.

*-- GL Account 2
    READ TABLE gt_glacct ASSIGNING <fs_glacct> WITH KEY matnr = <fs_tfinal>-matnr
                                                        komok = co_zvb
                                                        BINARY SEARCH.
    IF <fs_glacct> IS ASSIGNED.
      ls_acctgl-itemno_acc  = 2.
      ls_acctgl-gl_account  = <fs_glacct>-hkont.
      ls_acctgl-profit_ctr  = <fs_tfinal>-prctr.
      ls_acctgl-orderid     = <fs_tfinal>-aufnr.
      ls_acctgl-alloc_nmbr  = <fs_tfinal>-mblnr.
      CONCATENATE text-035 so_pspid-low INTO ls_acctgl-item_text SEPARATED BY space.
      IF  <fs_tfinal>-kostl IS NOT INITIAL.
        ls_acctgl-costcenter  = <fs_tfinal>-kostl.
      ENDIF.
      IF <fs_tfinal>-ps_psp_pnr IS NOT INITIAL.
        ls_acctgl-wbs_element = <fs_tfinal>-ps_psp_pnr.
      ENDIF.
      IF <fs_tfinal>-nplnr IS NOT INITIAL.
        ls_acctgl-network  = <fs_tfinal>-nplnr.
      ENDIF.

*    ls_acctgl-funds_ctr   = <fs_alvout>-fistl.
      ls_acctgl-ref_key_1   = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_2   = <fs_tfinal>-zeile.
      APPEND ls_acctgl TO gt_acctgl.
      CLEAR ls_acctgl.

*-- GL Account 3
      IF  <fs_tfinal>-kostl IS NOT INITIAL.
        ls_acctgl-costcenter  = <fs_tfinal>-kostl.
      ENDIF.
      IF <fs_tfinal>-ps_psp_pnr IS NOT INITIAL.
        ls_acctgl-wbs_element = <fs_tfinal>-ps_psp_pnr.
      ENDIF.
      IF <fs_tfinal>-nplnr IS NOT INITIAL.
        ls_acctgl-network  = <fs_tfinal>-nplnr.
      ENDIF.

      ls_acctgl-itemno_acc  = 3.
      ls_acctgl-gl_account  = '1160402000'.                 "1160402000
      ls_acctgl-ref_key_1   = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_2   = <fs_tfinal>-zeile.
      ls_acctgl-profit_ctr  = <fs_tfinal>-prctr.
      ls_acctgl-orderid     = <fs_tfinal>-aufnr.
      ls_acctgl-alloc_nmbr  = <fs_tfinal>-mblnr.
      CONCATENATE text-035 so_pspid-low INTO ls_acctgl-item_text SEPARATED BY space.
      APPEND ls_acctgl TO gt_acctgl.
      CLEAR ls_acctgl.

*-- GL Account 4
      IF  <fs_tfinal>-kostl IS NOT INITIAL.
        ls_acctgl-costcenter  = <fs_tfinal>-kostl.
      ENDIF.
      IF <fs_tfinal>-ps_psp_pnr IS NOT INITIAL.
        ls_acctgl-wbs_element = <fs_tfinal>-ps_psp_pnr.
      ENDIF.
      IF <fs_tfinal>-nplnr IS NOT INITIAL.
        ls_acctgl-network  = <fs_tfinal>-nplnr.
      ENDIF.

      ls_acctgl-itemno_acc  = 4.
      ls_acctgl-gl_account  = '9400100000'.                 "1160402000
      ls_acctgl-ref_key_1   = <fs_tfinal>-mblnr.
      ls_acctgl-ref_key_2   = <fs_tfinal>-zeile.
      ls_acctgl-profit_ctr  = <fs_tfinal>-prctr.
      ls_acctgl-orderid     = <fs_tfinal>-aufnr.
      ls_acctgl-alloc_nmbr  = <fs_tfinal>-mblnr.
      ls_acctgl-tax_code    = 'I0'.
      CONCATENATE text-035 so_pspid-low INTO ls_acctgl-item_text SEPARATED BY space.
      APPEND ls_acctgl TO gt_acctgl.
      CLEAR ls_acctgl.
    ENDIF.
  ENDIF.

*-- End add CDELAPENA 11/25/2013
ENDFORM.                    " BUILD_GLACCT_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_CURRENCY_DATA
*&---------------------------------------------------------------------*
*       Currencies and Amount Data
*----------------------------------------------------------------------*
*       -> PV_ID  Identifier (G/L or Vendor)
*----------------------------------------------------------------------*
FORM build_currency_data USING pv_id TYPE char2.

  DATA ls_currency  TYPE bapiaccr09.
  DATA lv_qty       TYPE mseg-erfmg.
  DATA lv_matkl     TYPE mara-matkl.
  DATA lv_infnr     TYPE eina-infnr.
  DATA lt_eipa      TYPE STANDARD TABLE OF eipa WITH HEADER LINE.
  DATA lv_latest_p  TYPE eipa-preis.
  DATA lv_vat       TYPE eipa-preis.
  DATA lv_penalty   TYPE eipa-preis.

  CLEAR :  lv_qty,
           lv_matkl,
           lv_infnr,
           lt_eipa,
           lv_latest_p,
           lv_vat,
           lv_penalty.

  REFRESH gt_currency.

  "--Initialize Quantity
  IF pv_id EQ co_gl.
    lv_qty = <fs_tfinal>-used.
  ELSEIF pv_id EQ co_vendor.
    lv_qty = <fs_tfinal>-unret.
  ENDIF.

  IF pv_id EQ co_gl.
    "--Line Item1
    TRY.
        ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.
        ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.
      CATCH cx_sy_zerodivide.                           "#EC NO_HANDLER
      CATCH cx_sy_arithmetic_overflow.
        CLEAR ls_currency-amt_doccur.
    ENDTRY.

    ls_currency-itemno_acc = 1.
    ls_currency-currency   = <fs_tfinal>-waers.
    APPEND ls_currency TO gt_currency.
    CLEAR ls_currency.

    "--Line Item2
    TRY.
        ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.
        ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.
        ls_currency-amt_doccur = ls_currency-amt_doccur * -1.
      CATCH cx_sy_zerodivide.                           "#EC NO_HANDLER
      CATCH cx_sy_arithmetic_overflow.
        CLEAR ls_currency-amt_doccur.
    ENDTRY.

    ls_currency-itemno_acc = 2.
    ls_currency-currency   = <fs_tfinal>-waers.
    APPEND ls_currency TO gt_currency.
    CLEAR ls_currency.

  ENDIF.

*-- Start add Delapena 11/20/2013

  IF pv_id EQ co_vendor.

    SELECT SINGLE MAX( infnr )
      FROM eina
      INTO lv_infnr
     WHERE matnr = <fs_tfinal>-matnr
       AND loekz NE 'X'.

    IF lv_infnr IS NOT INITIAL.

      SELECT *
        FROM eipa
        INTO TABLE lt_eipa
       WHERE infnr = lv_infnr.

      IF lt_eipa[] IS NOT INITIAL.

        SORT lt_eipa[] DESCENDING BY bedat ebeln ebelp.

        READ TABLE lt_eipa INDEX 1.

        lv_latest_p = lt_eipa-preis / lt_eipa-peinh.

      ENDIF.

    ENDIF.

*-- Identify Water/Non-water Meter
    SELECT SINGLE matkl
      FROM mara
      INTO lv_matkl
     WHERE matnr = <fs_tfinal>-matnr.

    IF lv_matkl = '1200'.
*-- For Water Meter
      "--Line Item1 - Dr. Accounts Payable (Unit Price + Input VAT + Penalty)
      TRY.
          ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.

          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.

          lv_vat = ls_currency-amt_doccur * '.12'.
*-- Commented 06/17/2014 by GDIMALIWAT for TESTING
*          lv_penalty = lv_latest_p * '1.20'.
*-- Added 06/17/2014 by GDIMALIWAT for TESTING
          lv_penalty = lv_latest_p * '2.00'. " 200%

          lv_penalty = lv_penalty * lv_qty.

          ls_currency-amt_doccur = ls_currency-amt_doccur +
                                   lv_vat +
                                   lv_penalty.

        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 1.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item2 - Cr. Penalty 9400100000 - (200% of latest PO price + weighted average price)
      TRY.
          ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.
          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.
          ls_currency-amt_doccur = ls_currency-amt_doccur * -1.
        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 2.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item3 - Cr. Input VAT - (12% of Weighted average price)
      TRY.
          ls_currency-amt_doccur = lv_vat * -1.
*          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.   " cdelapena 12/13/2013
        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 3.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item4 - Cr. Penalty 9400100000 - (200% of latest PO price)
      TRY.
*-- Commented 06/17/2014 by GDIMALIWAT for TESTING
*          lv_penalty = lv_latest_p * '1.20'.
*-- Added 06/17/2014 by GDIMALIWAT for TESTING
          lv_penalty = lv_latest_p * '2.00'. " 200%

          ls_currency-amt_doccur = lv_penalty * lv_qty.
          ls_currency-amt_doccur = ls_currency-amt_doccur * -1.

        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 4.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

    ELSE.
*-- For NON-Water Meter

      "--Line Item1 - Dr. Accounts Payable (Latest PO price + Input VAT + Penalty 20%)
      TRY.
          ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.
          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.

          lv_vat = ls_currency-amt_doccur * '.12'.

*-- Commented 06/17/2014 by GDIMALIWAT for TESTING
*          lv_penalty = lv_latest_p * '.20'.
*-- Added 06/17/2014 by GDIMALIWAT for TESTING
          lv_penalty = lv_latest_p * '1.20'. " 20%

          lv_penalty = lv_penalty * lv_qty.

          ls_currency-amt_doccur = ls_currency-amt_doccur +
                                   lv_vat +
                                   lv_penalty.

        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 1.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item2 - Cr. Clearing - (weighted average price)
      TRY.
          ls_currency-amt_doccur = <fs_tfinal>-dmbtr / <fs_tfinal>-erfmg.
          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.
          ls_currency-amt_doccur = ls_currency-amt_doccur * -1.
        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 2.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item3 - Cr. Input VAT - (12% of Weighted average price)
      TRY.
          ls_currency-amt_doccur = lv_vat * -1.
*          ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.   " cdelapena 12/13/2013
        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 3.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

      "--Line Item4 - Cr. Penalty 9400100000 - (20% of latest PO price)
      TRY.
          ls_currency-amt_doccur = lv_penalty * -1.
*           ls_currency-amt_doccur = ls_currency-amt_doccur * lv_qty.  " cdelapena 12/13/2013
        CATCH cx_sy_zerodivide.                         "#EC NO_HANDLER
        CATCH cx_sy_arithmetic_overflow.
          CLEAR ls_currency-amt_doccur.
      ENDTRY.

      ls_currency-itemno_acc = 4.
      ls_currency-currency   = <fs_tfinal>-waers.
      APPEND ls_currency TO gt_currency.
      CLEAR ls_currency.

    ENDIF.

  ENDIF.

ENDFORM.                    " BUILD_CURRENCY_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXTENSION_DATA
*&---------------------------------------------------------------------*
*       Additional Posting Values
*----------------------------------------------------------------------*
*       -> PV_ID  Identifier (G/L or Vendor)
*----------------------------------------------------------------------*
FORM build_extension_data USING pv_id TYPE char2.

  DATA : ls_extension2 TYPE bapiparex.
  DATA : ls_accit      TYPE accit,
         ls_acchd      TYPE acchd.

  REFRESH gt_extension2.
*** Commented 06/17/2014 by GDIMALIWAT for TESTING
*  IF pv_id EQ co_vendor.
*    "edit header change posting to parking
*    ls_extension2-structure  = 'ACCHD'.
*    ls_extension2-valuepart1 = ls_acchd.                          " #EC ENHOK
*    ls_extension2-valuepart2 = 'BAPI_PARK'.
*    ls_extension2-valuepart3 = '2'. "ls_acchd-status_new.         " #EC ENHOK
*    APPEND ls_extension2 TO gt_extension2.
*    CLEAR ls_extension2.
*    CLEAR ls_acchd.
*  ENDIF.

  "--Item 1
  ls_accit-posnr = 1.

  IF pv_id EQ co_gl.
    ls_accit-bschl = co_40.
  ELSEIF pv_id EQ co_vendor.
    ls_accit-bschl = co_21.
  ENDIF.

  ls_extension2-structure  = 'ACCIT'.
  ls_extension2-valuepart1 = ls_accit.                      "#EC ENHOK
  ls_extension2-valuepart2 = ls_accit-bschl.                "#EC ENHOK
  APPEND ls_extension2 TO gt_extension2.
  CLEAR ls_extension2.
  CLEAR ls_accit.

  "--Item 2
  ls_accit-posnr = 2.

  IF pv_id EQ co_gl.
    ls_accit-bschl = co_50.
  ELSEIF pv_id EQ co_vendor.
    ls_accit-bschl = co_50.
  ENDIF.

  ls_accit-bschl = co_50.
  ls_extension2-structure  = 'ACCIT'.
  ls_extension2-valuepart1 = ls_accit.                      "#EC ENHOK
  ls_extension2-valuepart2 = ls_accit-bschl.                "#EC ENHOK
  APPEND ls_extension2 TO gt_extension2.
  CLEAR ls_extension2.
  CLEAR ls_accit.

  IF pv_id EQ co_vendor.
    "--Item 3
    ls_accit-posnr = 3.

    ls_accit-bschl = co_50.
    ls_extension2-structure  = 'ACCIT'.
    ls_extension2-valuepart1 = ls_accit.                    "#EC ENHOK
    ls_extension2-valuepart2 = ls_accit-bschl.              "#EC ENHOK
    APPEND ls_extension2 TO gt_extension2.
    CLEAR ls_extension2.
    CLEAR ls_accit.

    "--Item 4
    ls_accit-posnr = 4.

    ls_accit-bschl = co_50.
    ls_extension2-structure  = 'ACCIT'.
    ls_extension2-valuepart1 = ls_accit.                    "#EC ENHOK
    ls_extension2-valuepart2 = ls_accit-bschl.              "#EC ENHOK
    APPEND ls_extension2 TO gt_extension2.
    CLEAR ls_extension2.
    CLEAR ls_accit.
  ENDIF.

ENDFORM.                    " BUILD_EXTENSION_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_ACCOUNTING_DOCUMENT
*&---------------------------------------------------------------------*
*       Create Accounting Doc
*----------------------------------------------------------------------*
*       -> PV_ID  Identifier (G/L or Vendor)
*----------------------------------------------------------------------*
FORM post_accounting_document  USING pv_id TYPE char2.

  DATA lv_objkey TYPE bapiache09-obj_key.

  DO 2 TIMES. "-- Inserted by RANDYSY 02/19/2014 TN#57141
    REFRESH gt_return.

    "--Ensure Data Correctness
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = gs_docheader
      TABLES
        accountgl      = gt_acctgl
        accountpayable = gt_acctpayable
        currencyamount = gt_currency
        extension2     = gt_extension2
        return         = gt_return.
  ENDDO. "-- Inserted by RANDYSY 02/19/2014 TN#57141

  LOOP AT gt_return INTO gs_return WHERE type = co_e.       "#EC NEEDED
    " Get the last error.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CASE pv_id.
      WHEN co_gl.
        <fs_tfinal>-stat_f02   = co_error.
        IF gs_return-id EQ 'FMAVC'. " FM Related
          CONCATENATE gs_return-message '.'
                      INTO <fs_tfinal>-mess_f02.
          CONCATENATE <fs_tfinal>-mess_f02 gs_return-message_v3
                      INTO <fs_tfinal>-mess_f02 SEPARATED BY space.
        ELSE.
          <fs_tfinal>-mess_f02 = gs_return-message.
        ENDIF.
        PERFORM unhide_fcat_column USING: 'STAT_F02',
                                          'MESS_F02'.
*& set message as hotspot in alv
        PERFORM set_field_as_hotspot USING 'MESS_F02' .

      WHEN co_vendor.
        <fs_tfinal>-stat_f43   = co_error.
        IF gs_return-id EQ 'FMAVC'. " FM Related
          CONCATENATE gs_return-message '.'
                      INTO <fs_tfinal>-mess_f43.
          CONCATENATE <fs_tfinal>-mess_f43 gs_return-message_v3
                      INTO <fs_tfinal>-mess_f43 SEPARATED BY space.
        ELSE.
          <fs_tfinal>-mess_f43 = gs_return-message.
        ENDIF.
        PERFORM unhide_fcat_column USING: 'STAT_F43',
                                          'MESS_F43'.
*& set message as hotspot in alv
        PERFORM set_field_as_hotspot USING 'MESS_F43' .

      WHEN OTHERS.
        " DO nothing
    ENDCASE.
*    CLEAR: <fs_tfinal>-used,
*           <fs_tfinal>-dfctve,
*           <fs_tfinal>-excess.
  ELSE.

    "--Actual Posting
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = gs_docheader
      IMPORTING
        obj_key        = lv_objkey
      TABLES
        accountgl      = gt_acctgl
        return         = gt_return
        accountpayable = gt_acctpayable
        currencyamount = gt_currency
        extension2     = gt_extension2.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    "--Update Open Qty in table ZTMMRECPS
    PERFORM update_table_ztmmrecps USING pv_id.

    CASE pv_id.
      WHEN co_gl.
        <fs_tfinal>-stat_f02   = co_success.
        CONCATENATE text-027 lv_objkey+0(10) text-019
               INTO <fs_tfinal>-mess_f02 SEPARATED BY space.

        PERFORM unhide_fcat_column USING: 'STAT_F02',
                                          'MESS_F02'.
*& set message as hotspot in alv
        PERFORM set_field_as_hotspot USING 'MESS_F02' .

      WHEN co_vendor.
        <fs_tfinal>-stat_f43   = co_success.
        CONCATENATE text-027 lv_objkey+0(10) text-019
               INTO <fs_tfinal>-mess_f43 SEPARATED BY space.

        PERFORM unhide_fcat_column USING: 'STAT_F43',
                                          'MESS_F43'.
*& set message as hotspot in alv
        PERFORM set_field_as_hotspot USING 'MESS_F43' .

      WHEN OTHERS.
        " DO nothing
    ENDCASE.
  ENDIF.

*  CLEAR: <fs_tfinal>-used,
*         <fs_tfinal>-dfctve,
*         <fs_tfinal>-excess.
  REFRESH:   gt_acctgl,
             gt_acctpayable,
             gt_currency,
             gt_extension2,
             gt_return.
ENDFORM.                    " POST_ACCOUNTING_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VENDOR_DATA
*&---------------------------------------------------------------------*
*       Accounts Payable Data (Vendor)
*----------------------------------------------------------------------*
FORM build_vendor_data .

  DATA ls_acctpayable TYPE bapiacap09.

  FIELD-SYMBOLS <fs_vendor> TYPE ty_vendor.

  REFRESH gt_acctpayable.

  READ TABLE gt_mkpf ASSIGNING <fs_mkpf> WITH KEY mblnr = <fs_tfinal>-mblnr
                                                  mjahr = <fs_tfinal>-mjahr
                                         BINARY SEARCH.
  IF <fs_mkpf> IS ASSIGNED.
    ls_acctpayable-bline_date = <fs_mkpf>-budat.
  ENDIF.
  UNASSIGN <fs_mkpf>.

  READ TABLE gt_vendor ASSIGNING  <fs_vendor> WITH KEY nplnr = <fs_tfinal>-nplnr.
  IF <fs_vendor> IS ASSIGNED.
    ls_acctpayable-vendor_no  = <fs_vendor>-lifnr.
    UNASSIGN <fs_vendor>.
  ENDIF.

  READ TABLE so_pspid INDEX 1.

  "--Item 1
  ls_acctpayable-itemno_acc = 1.
  CONCATENATE text-035 so_pspid-low INTO ls_acctpayable-item_text.
  ls_acctpayable-ref_key_1  = <fs_tfinal>-mblnr.
  ls_acctpayable-ref_key_2  = <fs_tfinal>-zeile.
  APPEND ls_acctpayable TO gt_acctpayable.

ENDFORM.                    " BUILD_VENDOR_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GLACCT_DATA
*&---------------------------------------------------------------------*
*       GL Account Information Based on Material
*----------------------------------------------------------------------*
FORM get_glacct_data.

  TYPES: BEGIN OF lty_mbew,
          matnr TYPE mara-matnr,
          bklas TYPE mbew-bklas,
         END OF lty_mbew.

  TYPES: BEGIN OF lty_t030,
          bklas TYPE t030-bklas,
          komok TYPE t030-komok,
          konts TYPE t030-konts,
          konth TYPE t030-konth,
         END OF lty_t030.

  DATA lt_mbew   TYPE TABLE OF lty_mbew.
  DATA lt_t030   TYPE STANDARD TABLE OF lty_t030 WITH NON-UNIQUE KEY bklas.
  DATA ls_glacct TYPE ty_glacct.

  FIELD-SYMBOLS <fs_mbew> TYPE lty_mbew.
  FIELD-SYMBOLS <fs_t030> TYPE lty_t030.

  "--Get GL Account Detail
  SELECT matnr
         bklas
         INTO TABLE lt_mbew
         FROM mbew AS ew
         FOR ALL ENTRIES IN gt_mseg
         WHERE matnr EQ gt_mseg-matnr
           AND bwkey EQ gt_mseg-werks.
  IF sy-subrc EQ 0.
    SELECT bklas
           komok
           konts
           konth
           INTO TABLE lt_t030
           FROM t030
           FOR ALL ENTRIES IN lt_mbew
           WHERE ktosl EQ co_gbb
             AND komok IN (co_vbr,co_zvb)
             AND bklas EQ lt_mbew-bklas.
    IF sy-subrc EQ 0.
      LOOP AT lt_mbew ASSIGNING <fs_mbew>.
        LOOP AT lt_t030 ASSIGNING <fs_t030> WHERE bklas = <fs_mbew>-bklas.
          ls_glacct-matnr = <fs_mbew>-matnr.
          ls_glacct-komok = <fs_t030>-komok.
          IF <fs_t030>-komok EQ co_vbr.
            ls_glacct-hkont = <fs_t030>-konts.
          ELSEIF <fs_t030>-komok EQ co_zvb.
            ls_glacct-hkont = <fs_t030>-konth.
          ENDIF.
          APPEND ls_glacct TO gt_glacct.
          CLEAR ls_glacct.
        ENDLOOP.
        SORT gt_glacct BY matnr komok.
        UNASSIGN <fs_t030>.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_GLACCT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_ZTMMRECPS
*&---------------------------------------------------------------------*
*       Retrieve Material Quantities that were reclassed
*----------------------------------------------------------------------*
FORM get_data_from_ztmmrecps.

  SELECT *
    FROM ztmmrecps
    INTO TABLE gt_ztmmrecps
    WHERE matnr      IN so_matnr.
*      AND werks      EQ pa_werks
*      AND wbs        IN so_wbs
*      AND budat_mkpf IN so_budat.

  IF sy-subrc EQ 0.                                         "#EC NEEDED
*    DELETE gt_ztmmrecps WHERE openqty LE 0.
  ENDIF.

ENDFORM.                    " GET_DATA_FROM_ZTMMRECPS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_ZTMMRECPS
*&---------------------------------------------------------------------*
*       Update Material Quantity based on Material, Plant, Network
*----------------------------------------------------------------------*
FORM update_table_ztmmrecps USING pv_idz TYPE char2.

  DATA lv_otherqty  TYPE mseg-erfmg.
  DATA ls_ztmmrecps TYPE ztmmrecps.

  FIELD-SYMBOLS <fs_ztmmrecps> TYPE ztmmrecps.

  CASE pv_idz.
    WHEN co_gl.
      "--Total of Other QTY's (Used Qty)
      lv_otherqty  =  <fs_tfinal>-used.
    WHEN co_vendor.
      "--Total of Other QTY's
      lv_otherqty  =  <fs_tfinal>-unret.
    WHEN co_returns.
      "--Total of Other QTY's (Returns Qty)
      lv_otherqty  =  <fs_tfinal>-migo_qty.
    WHEN OTHERS.
      " Do nothing
  ENDCASE.

  READ TABLE gt_ztmmrecps ASSIGNING <fs_ztmmrecps> WITH KEY mblnr = <fs_tfinal>-mblnr
                                                            zeile = <fs_tfinal>-zeile
                                                       budat_mkpf = <fs_tfinal>-budat_mkpf
                                                            werks = <fs_tfinal>-werks
                                                       BINARY SEARCH.
  IF <fs_ztmmrecps> IS ASSIGNED.
*    <fs_ztmmrecps>-openqty = <fs_ztmmrecps>-openqty - lv_otherqty.
    <fs_ztmmrecps>-openqty = <fs_tfinal>-opqty - lv_otherqty.
    IF <fs_ztmmrecps>-openqty LE 0.
      <fs_ztmmrecps>-openqty = 0.
    ENDIF.

    "--Update Reclassed Qty
    CASE pv_idz.
      WHEN co_gl.
        ADD <fs_tfinal>-used TO <fs_ztmmrecps>-usedqty.
      WHEN co_vendor.
*        ADD: <fs_tfinal>-unret TO <fs_ztmmrecps>-usedqty. Q1 11/20/2013
      WHEN OTHERS.
        " Do nothing
    ENDCASE.

    <fs_tfinal>-opqty      = <fs_ztmmrecps>-openqty. " Update the Qty in the ALV Display
    ls_ztmmrecps           = <fs_ztmmrecps>.
    UNASSIGN <fs_ztmmrecps>.
  ELSE.
    MOVE-CORRESPONDING <fs_tfinal> TO  ls_ztmmrecps.        "#EC ENHOK
    CASE pv_idz.
      WHEN co_gl.
        ls_ztmmrecps-usedqty = <fs_tfinal>-used.
      WHEN co_vendor.
        ls_ztmmrecps-defectiveqty = <fs_tfinal>-dfctve.
        ls_ztmmrecps-excessqty    = <fs_tfinal>-excess.
      WHEN OTHERS.
        " Do nothing
    ENDCASE.
    ls_ztmmrecps-openqty    = <fs_tfinal>-opqty - lv_otherqty.
    IF ls_ztmmrecps-openqty LE 0.
      ls_ztmmrecps-openqty = 0.
    ENDIF.
    <fs_tfinal>-opqty       = ls_ztmmrecps-openqty.
    ls_ztmmrecps-une        = <fs_tfinal>-erfme.
    ls_ztmmrecps-network    = <fs_tfinal>-nplnr.
    ls_ztmmrecps-wbs        = <fs_tfinal>-mat_pspnr.
    ls_ztmmrecps-name1      = gv_plant_name.
    ls_ztmmrecps-lastchdate = sy-datum.
    APPEND ls_ztmmrecps TO gt_ztmmrecps. " Update Internal Table for new entries
  ENDIF.

  DELETE gt_final WHERE erfmg LE 0. " Remove 0 Qty's
  MODIFY ztmmrecps FROM ls_ztmmrecps.
  COMMIT WORK AND WAIT.

ENDFORM.                    " UPDATE_TABLE_ZTMMRECPS
*&---------------------------------------------------------------------*
*&      Form  CHECK_VALID_WERKS
*&---------------------------------------------------------------------*
*       Checking if PLANT (werks) is a valid entry.
*----------------------------------------------------------------------*
FORM check_valid_werks.
  TYPES: BEGIN OF lty_check,
            werks TYPE werks,
            name1 TYPE name1,
         END OF lty_check.

  DATA: ls_check TYPE lty_check.

*  SELECT SINGLE werks
*                name1
*      FROM t001w
*      INTO ls_check
*      WHERE werks EQ pa_werks.

  IF sy-subrc EQ 0.
*-Getting Plant name from table T001W
    gv_plant_name = ls_check-name1.
  ELSE.
    MESSAGE e208(00) WITH 'Please enter a valid Plant'(022).
  ENDIF.
ENDFORM.                    " CHECK_VALID_WERKS
*&---------------------------------------------------------------------*
*&      Form  CONV_CUNIT_INPUT
*&---------------------------------------------------------------------*
* This subroutine conversion ERFME to user's system login language (UoM))
*----------------------------------------------------------------------*
* <-> IN/OUT ERFME
*----------------------------------------------------------------------*
FORM conv_cunit_input USING lp_erfme LIKE mseg-erfme .

  "convert display UoM
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = lp_erfme
      language       = sy-langu
    IMPORTING
*     LONG_TEXT      =
      output         = lp_erfme
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " CONV_CUNIT_INPUT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page .

  DATA lt_alvheader TYPE slis_t_listheader.
  DATA ls_alvheader TYPE slis_listheader.

  ls_alvheader-typ  = 'S'.
  ls_alvheader-key = 'LEGEND:'.
  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.

  ls_alvheader-typ  = 'S'.
  ls_alvheader-key  = space.
  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.

  ls_alvheader-typ  = 'S'.
  ls_alvheader-key  = space.
  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.

  ls_alvheader-typ  = 'S'.
  ls_alvheader-info = 'USED - Used Quantity'.
  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.

  ls_alvheader-typ  = 'S'.
  ls_alvheader-info = 'Unreturned - Unreturned'.
  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.
*
*  ls_alvheader-typ  = 'S'.
*  ls_alvheader-info = 'DEFECTIVE - Due to Mishandling'.
*  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.
*
*  ls_alvheader-typ  = 'S'.
*  ls_alvheader-info = 'RETURNS - Within grace period; Defective Manufacturing'.
*  APPEND ls_alvheader TO lt_alvheader. CLEAR ls_alvheader.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_alvheader.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FCAT  text
*----------------------------------------------------------------------*
FORM change_fcat_post  TABLES pt_fcat STRUCTURE lvc_s_fcat.

*& begin add asee 20121205
  FIELD-SYMBOLS: <ls_fcat> LIKE gs_fcat .

*& begin modify asee of fcat
  UNASSIGN: <ls_fcat>.
  LOOP AT pt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'STAT_F02' OR 'STAT_F43'.
        <ls_fcat>-no_out = abap_false .
      WHEN 'MESS_F02' OR 'MESS_F43'.
        <ls_fcat>-no_out = abap_false .
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FCAT
*&---------------------------------------------------------------------*
*&      Form  ADD_ROW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_row_alv .
  PERFORM check_selected_record USING co_add CHANGING gs_final_add.
  PERFORM input_number_of_records.
  PERFORM insert_additional_rows.
  PERFORM ctrl_checkbox USING space.
ENDFORM.                    " ADD_ROW_ALV
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED_RECORD
*&---------------------------------------------------------------------*
*       Checking of Selected record in the ALV
*----------------------------------------------------------------------*
*       -> PV_OPER    Add or Delete Records
*       -> PS_ALVOUT  Reference Record
*----------------------------------------------------------------------*
FORM check_selected_record USING pv_oper TYPE char3
                           CHANGING ps_alvout LIKE gs_final.

  DATA lv_counter TYPE i.

  CLEAR gv_linecount.

  READ TABLE gt_final TRANSPORTING NO FIELDS WITH KEY cbox = abap_true.
  IF sy-subrc NE 0.
    CASE pv_oper.
      WHEN co_add.
        MESSAGE 'Please select record reference' TYPE co_e.
      WHEN co_del.
        MESSAGE 'Please select record to be deleted' TYPE co_e.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  IF pv_oper EQ co_add.
    UNASSIGN: <fs_tfinal> .
    LOOP AT gt_final ASSIGNING <fs_tfinal> WHERE cbox = abap_true.
      ADD 1 TO lv_counter.
      ps_alvout = <fs_tfinal>.
      IF lv_counter GT 1.
        MESSAGE 'Please select only 1 record reference' TYPE co_e.
        EXIT.
      ENDIF.
      "--Count lines with similar material/item
      LOOP AT gt_final TRANSPORTING NO FIELDS WHERE node  = space " Non-Main Node
                                                AND matnr = <fs_tfinal>-matnr
                                                AND zeile = <fs_tfinal>-zeile.
        ADD 1 TO gv_linecount.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CHECK_SELECTED_RECORD
*&---------------------------------------------------------------------*
*&      Form  INPUT_NUMBER_OF_RECORDS
*&---------------------------------------------------------------------*
*       Pop-up for user to input number of additional rows
*----------------------------------------------------------------------*
FORM input_number_of_records .

  DATA lv_trgtxt TYPE text80.
  DATA lv_strlen TYPE i.
  DATA lv_errflg TYPE c.
  DATA lv_sum    TYPE i.

  CLEAR gv_addrec.

  CALL FUNCTION 'ADA_POPUP_TEXT_INPUT'
    EXPORTING
      sourcetext   = 'Number of Records:'
      titel        = 'Add New Records'
      start_column = 25
      start_row    = 6
    CHANGING
      targettext   = lv_trgtxt.

  IF lv_trgtxt IS NOT INITIAL.
    lv_strlen = strlen( lv_trgtxt ).
    "--Numeric Values Check
    IF lv_trgtxt CO '0123456789'.
    ENDIF.
    IF sy-fdpos NE lv_strlen.
      MESSAGE 'Enter numeric values only' TYPE co_e.
    ENDIF.
    "--Set Maximum Limit (14)
    IF lv_strlen GT 2.
      lv_errflg = abap_true.
    ELSE.
      IF lv_trgtxt+0(2) GT 14.
        lv_errflg = abap_true.
      ELSE.
        gv_addrec =  lv_trgtxt+0(2).
      ENDIF.
    ENDIF.
    IF  lv_errflg EQ abap_true.
      MESSAGE 'Please enter a number from 1 to 14 only' TYPE co_e.
    ENDIF.
    lv_sum = gv_linecount + gv_addrec.
    IF lv_sum GT 15.
      MESSAGE 'Limit exceeded. Please enter a lower value or delete a record' TYPE co_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_NUMBER_OF_RECORDS
*&---------------------------------------------------------------------*
*&      Form  INSERT_ADDITIONAL_ROWS
*&---------------------------------------------------------------------*
*       Add ALV rows
*----------------------------------------------------------------------*
FORM insert_additional_rows .

  IF gv_addrec IS NOT INITIAL.
    DO gv_addrec TIMES.
      CLEAR gs_final_add-node.
      APPEND gs_final_add TO gt_final.
    ENDDO.
  ENDIF.
  SORT gt_final BY mblnr matnr node DESCENDING.

ENDFORM.                    " INSERT_ADDITIONAL_ROWS
*&---------------------------------------------------------------------*
*&      Form  CTRL_CHECKBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ctrl_checkbox USING pv_in TYPE c.
  UNASSIGN: <fs_tfinal>.
  LOOP AT gt_final ASSIGNING <fs_tfinal>.
    <fs_tfinal>-cbox = pv_in.
  ENDLOOP.
  UNASSIGN <fs_tfinal>.
ENDFORM.                    " ctrl_CHECKBOX
*&---------------------------------------------------------------------*
*&      Form  DEL_ROW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM del_row_alv .
  PERFORM check_selected_record USING co_del CHANGING gs_final_add.
  DELETE gt_final WHERE cbox EQ abap_true AND
                        node EQ space.
  PERFORM ctrl_checkbox USING space.
ENDFORM.                    " DEL_ROW_ALV
*&---------------------------------------------------------------------*
*&      Form  UNHIDE_FCAT_COLUMN
*&---------------------------------------------------------------------*
*       Show hidden fieldcat column
*----------------------------------------------------------------------*
*      -->PV_FNAME   Field Name
*----------------------------------------------------------------------*
FORM unhide_fcat_column  USING pv_fname TYPE lvc_fname.

  READ TABLE gt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = pv_fname.
  IF <fs_fcat> IS ASSIGNED.
    CLEAR <fs_fcat>-no_out.
    UNASSIGN <fs_fcat>.
  ENDIF.

ENDFORM.                    " UNHIDE_FCAT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_AS_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2137   text
*----------------------------------------------------------------------*
FORM set_field_as_hotspot  USING pv_fname TYPE lvc_fname.

  READ TABLE gt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = pv_fname.
  IF <fs_fcat> IS ASSIGNED.
    <fs_fcat>-hotspot = abap_true .
    UNASSIGN <fs_fcat>.
  ENDIF.

ENDFORM.                    " SET_FIELD_AS_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_messages .

  PERFORM hide_fcat_column USING: 'STAT_F02',
                                  'MESS_F02',
                                  'STAT_MIGO',
                                  'MESS_MIGO',
                                  'STAT_F43',
                                  'MESS_F43'.

*  DELETE gt_final WHERE opqty LE 0. " Remove 0 Open Qty's    CDELAPENA 12/10/2013

  LOOP AT gt_final ASSIGNING <fs_tfinal> WHERE stat_f02 NE space OR
                                               stat_f43 NE space.
    CLEAR: <fs_tfinal>-stat_f02,
           <fs_tfinal>-stat_f43,
           <fs_tfinal>-mess_f02,
           <fs_tfinal>-mess_f43.
  ENDLOOP.
  UNASSIGN <fs_tfinal>.

ENDFORM.                    " REFRESH_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_vendor_details .
  SELECT ko~aufnr
         pa~parnr
         INTO TABLE gt_vendor
         FROM afko AS ko
         INNER JOIN proj AS oj
         ON ( ko~pronr =  oj~pspnr )
         INNER JOIN ihpa AS pa
         ON ( pa~objnr = oj~objnr AND
              pa~parvw = co_lf )
         FOR ALL ENTRIES IN gt_mseg
         WHERE ko~aufnr = gt_mseg-nplnr.
  IF sy-subrc EQ 0.
  ENDIF.
ENDFORM.                    " GET_VENDOR_DETAILS
*&---------------------------------------------------------------------*
*&      Form  SET_MSG_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_FIELDNAME  text
*      -->P_RS_SELFIELD_TABINDEX  text
*----------------------------------------------------------------------*
FORM set_msg_popup  USING pv_fieldname TYPE slis_selfield-fieldname
                          pv_tabindex  TYPE slis_selfield-tabindex .

  FIELD-SYMBOLS: <ls_final> LIKE LINE OF gt_final .

  UNASSIGN: <ls_final> .
  CASE pv_fieldname.
    WHEN 'MESS_F02'.
      READ TABLE gt_final ASSIGNING <ls_final> INDEX pv_tabindex.
      IF sy-subrc EQ 0 .
        MESSAGE <ls_final>-mess_f02 TYPE 'I'.
      ENDIF.

    WHEN 'MESS_F43'.
      READ TABLE gt_final ASSIGNING <ls_final> INDEX pv_tabindex .
      IF sy-subrc EQ 0.
        MESSAGE <ls_final>-mess_f43 TYPE 'I'.
      ENDIF.

    WHEN 'MESS_MIGO'.
      READ TABLE gt_final ASSIGNING <ls_final> INDEX pv_tabindex .
      IF sy-subrc EQ 0.
        MESSAGE <ls_final>-mess_migo TYPE 'I'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " SET_MSG_POPUP
*&---------------------------------------------------------------------*
*&      Form  HIDE_FCAT_COLUMN
*&---------------------------------------------------------------------*
*       Hide fieldcat column
*----------------------------------------------------------------------*
*      -->PV_FNAME   Field Name
*----------------------------------------------------------------------*
FORM hide_fcat_column USING pv_fname TYPE lvc_fname.

  READ TABLE gt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = pv_fname.
  IF <fs_fcat> IS ASSIGNED.
    <fs_fcat>-no_out = abap_true.
    UNASSIGN <fs_fcat>.
  ENDIF.

ENDFORM.                    " HIDE_FCAT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  SYNCH_OPEN_QTY
*&---------------------------------------------------------------------*
*       Synchronize Open Quantity
*----------------------------------------------------------------------*
FORM synch_open_qty .

  DATA ls_mseg_tmp LIKE LINE OF gt_mseg_tmp.
*  DATA lv_quotient  TYPE i.
  DATA lv_quotient  TYPE mseg-erfmg.
*  DATA lv_remainder TYPE i.
  DATA lv_remainder TYPE mseg-erfmg.
  DATA lv_countr    TYPE i.
  DATA lv_loop1     TYPE c.
*  DATA lv_openqtytmp TYPE i.
  DATA lv_openqtytmp TYPE mseg-erfmg.

  SORT gt_mseg BY matnr werks lgort mat_pspnr nplnr.

  "--Recompute Actual Open Qty
  LOOP AT gt_mseg ASSIGNING <fs_mseg>.

    MOVE-CORRESPONDING <fs_mseg> TO ls_mseg_tmp.
    ls_mseg_tmp-openqty = <fs_mseg>-erfmg.

    IF <fs_mseg>-bwart EQ gv_mvttype_rev. "Subtract Reversed Matdoc to the Total Qty
      ls_mseg_tmp-openqty = ls_mseg_tmp-openqty * -1.
    ENDIF.
    COLLECT ls_mseg_tmp INTO gt_mseg_tmp.
    CLEAR ls_mseg_tmp.

  ENDLOOP.

  "--Keep only Movement Type "GI to Clearing" for Material Data itab
  DELETE gt_mseg WHERE bwart = gv_mvttype_rev.

  SORT gt_mseg_tmp  BY matnr werks lgort mat_pspnr nplnr.
  SORT gt_ztmmrecps BY matnr werks lgort wbs network.

  "--Apply the Actual Open Qty to the Reclassed Open Qty
  LOOP AT gt_mseg_tmp INTO ls_mseg_tmp.

    "--Count Relevant Records in Material Data
    CLEAR: lv_openqtytmp, lv_countr.
    LOOP AT gt_mseg ASSIGNING <fs_mseg> WHERE matnr     = ls_mseg_tmp-matnr
                                          AND werks     = ls_mseg_tmp-werks
                                          AND lgort     = ls_mseg_tmp-lgort
                                          AND mat_pspnr = ls_mseg_tmp-mat_pspnr
                                          AND nplnr     = ls_mseg_tmp-nplnr.
      ADD 1 TO lv_countr.
      ADD <fs_mseg>-erfmg TO lv_openqtytmp.
    ENDLOOP.
    UNASSIGN <fs_mseg>.

****Subtract first the Reclassed Qty (Used,Excess,Defective)*****************
    LOOP AT gt_ztmmrecps ASSIGNING <fs_ztmmrecps> WHERE matnr   = ls_mseg_tmp-matnr
                                                    AND werks   = ls_mseg_tmp-werks
                                                    AND lgort   = ls_mseg_tmp-lgort
                                                    AND wbs     = ls_mseg_tmp-mat_pspnr
                                                    AND network = ls_mseg_tmp-nplnr.
      " Subtract Reclassed QTY from the Open QTY
      ls_mseg_tmp-openqty = ls_mseg_tmp-openqty
                            - <fs_ztmmrecps>-usedqty
                            - <fs_ztmmrecps>-defectiveqty
                            - <fs_ztmmrecps>-excessqty.
    ENDLOOP.
    IF <fs_ztmmrecps> IS ASSIGNED.
      UNASSIGN <fs_ztmmrecps>.
    ENDIF.
*******************************************************************************

    CLEAR: lv_remainder, lv_quotient.
    IF ls_mseg_tmp-openqty LE 0.
      lv_remainder = 0.
      lv_quotient  = 0.
    ELSE.
      lv_remainder = ls_mseg_tmp-openqty MOD lv_countr.
      lv_quotient  = ls_mseg_tmp-openqty DIV lv_countr.
    ENDIF.

    "--Update Open QTY in Material Data Itab only if there's a difference in qty
    IF ls_mseg_tmp-openqty NE lv_openqtytmp.
      CLEAR lv_loop1.
      LOOP AT gt_mseg ASSIGNING <fs_mseg> WHERE matnr     = ls_mseg_tmp-matnr
                                            AND werks     = ls_mseg_tmp-werks
                                            AND lgort     = ls_mseg_tmp-lgort
                                            AND mat_pspnr = ls_mseg_tmp-mat_pspnr
                                            AND nplnr     = ls_mseg_tmp-nplnr.

        IF lv_remainder LE 0 OR lv_countr LE 0.
          <fs_mseg>-erfmg_open = lv_quotient.
        ELSE.
          IF lv_loop1 IS INITIAL. "First Relevant data contains the remainder
            <fs_mseg>-erfmg_open = lv_quotient + lv_remainder.
            lv_loop1 = abap_true.
          ELSE.
            <fs_mseg>-erfmg_open = lv_quotient.
          ENDIF.
        ENDIF.
      ENDLOOP.
      UNASSIGN <fs_mseg>.
    ENDIF.

  ENDLOOP.
  SORT gt_ztmmrecps BY mblnr zeile budat_mkpf werks.

ENDFORM.                    " SYNCH_OPEN_QTY
*&---------------------------------------------------------------------*
*&      Form  CREATE_MATDOC_MIGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_matdoc_migo .

  DATA: lt_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2,
        ls_goodsmvt_item TYPE bapi2017_gm_item_create,
        ls_goodsmvt_headret TYPE bapi2017_gm_head_ret,
        ls_materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
        ls_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year,
        ls_goodsmvt_header TYPE bapi2017_gm_head_01,
        lv_goodsmvt_code   TYPE bapi2017_gm_code.

  ls_goodsmvt_header-pstng_date  = sy-datum.
  ls_goodsmvt_header-doc_date    = sy-datum.
  ls_goodsmvt_header-pr_uname    = sy-uname.
  lv_goodsmvt_code               = co_03.   "Mimic MB1A Transaction

  UNPACK: <fs_tfinal>-nplnr TO ls_goodsmvt_item-network,
          <fs_tfinal>-matnr TO ls_goodsmvt_item-material.

  ls_goodsmvt_item-plant          = <fs_tfinal>-werks.
  ls_goodsmvt_item-stge_loc       = <fs_tfinal>-migo_lgort.
  ls_goodsmvt_item-spec_stock     = co_q.
  ls_goodsmvt_item-move_type      = gv_mvttype_rev.
  ls_goodsmvt_item-entry_qnt      = <fs_tfinal>-migo_qty.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = <fs_tfinal>-mat_pspnr
    IMPORTING
      output = ls_goodsmvt_item-val_wbs_elem.

  APPEND ls_goodsmvt_item TO lt_goodsmvt_item.

  "--Check Data Correctness
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = lv_goodsmvt_code
      testrun          = abap_true
    IMPORTING
      goodsmvt_headret = ls_goodsmvt_headret
      materialdocument = ls_materialdocument
      matdocumentyear  = ls_matdocumentyear
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return.

  IF lt_return IS NOT INITIAL. "With Error
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc EQ 0.
      <fs_tfinal>-stat_migo = co_error.
      <fs_tfinal>-mess_migo = ls_return-message.
    ENDIF.
    PERFORM unhide_fcat_column USING: 'STAT_MIGO',
                                      'MESS_MIGO'.
    PERFORM set_field_as_hotspot USING 'MESS_MIGO' .

  ELSE. " Successful
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_goodsmvt_header
        goodsmvt_code    = lv_goodsmvt_code
      IMPORTING
        goodsmvt_headret = ls_goodsmvt_headret
        materialdocument = ls_materialdocument
        matdocumentyear  = ls_matdocumentyear
      TABLES
        goodsmvt_item    = lt_goodsmvt_item
        return           = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    "--Update Open Qty in table ZTMMRECPS
    PERFORM update_table_ztmmrecps USING co_returns.

    <fs_tfinal>-stat_migo = co_success.
    CONCATENATE text-004 ls_materialdocument text-019 INTO <fs_tfinal>-mess_migo
                                                      SEPARATED BY space.
    PERFORM unhide_fcat_column USING: 'STAT_MIGO',
                                      'MESS_MIGO'.
    PERFORM set_field_as_hotspot USING 'MESS_MIGO' .

  ENDIF.
ENDFORM.                    " CREATE_MATDOC_MIGO
*&---------------------------------------------------------------------*
*&      Form  REVERSE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_document .

  DATA : lt_fields   TYPE TABLE OF sval                       WITH HEADER LINE,
         gt_zrevdoc  TYPE TABLE OF zrevdoc,
         lt_message  TYPE TABLE OF t_message,
         lt_fieldcat TYPE STANDARD TABLE OF slis_fieldcat_alv WITH HEADER LINE.

  DATA : ls_mseg     TYPE mseg,
         lv_msg      TYPE string,
         ls_messge   TYPE t_message,
         lv_choice(10),
         gs_layout   TYPE slis_layout_alv.

*-- Material
  lt_fields-tabname = 'MSEG'.
  lt_fields-fieldname = 'MBLNR'.
  lt_fields-field_obl = 'X'.
  APPEND lt_fields.
  CLEAR lt_fields.

*-- Material Doc
  lt_fields-tabname = 'MSEG'.
  lt_fields-fieldname = 'MATNR'.
  lt_fields-field_obl = 'X'.
  APPEND lt_fields.
  CLEAR lt_fields.

*-- FM to get lv_zeile Material and Mat doc.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = ' '
      popup_title     = 'Document Details'
      start_column    = '5'
      start_row       = '5'
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc = 0.

    CLEAR :  gv_material,
             gv_matdoc,
             ls_mseg,
             gv_erfmg.

*-- Transfer material
    READ TABLE lt_fields INDEX 1.
    gv_material = lt_fields-value.
    CLEAR lt_fields.

*-- Transfer Matdoc
    READ TABLE lt_fields INDEX 2 .
    gv_matdoc = lt_fields-value.
    CLEAR lt_fields.

*-- Read from MSEG
    SELECT SINGLE zeile erfmg dmbtr FROM mseg
      INTO (gv_zeile, gv_erfmg, gv_dmbtr)
      WHERE mblnr = gv_material
      AND   matnr = gv_matdoc.

    IF gv_zeile IS NOT INITIAL.

      CONCATENATE gv_material gv_zeile INTO gv_xblnr.

      SELECT bukrs
             belnr
             gjahr
             blart
             bktxt
        FROM bkpf
        INTO TABLE gt_bkpf
        WHERE xblnr = gv_xblnr
          AND ( xreversal NE '1' AND             " 01/23/2014 CDELAPENA
                xreversal NE '2' ).              " 01/23/2014 CDELAPENA

*-- Display output data in BSEG to ALV
      IF gt_bkpf[] IS NOT INITIAL.

        DELETE ADJACENT DUPLICATES FROM gt_bkpf COMPARING belnr bktxt.

        PERFORM display_alv_rev TABLES gt_bkpf.
      ELSE.
        MESSAGE s208(00) WITH 'No data to be Reversed' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " REVERSE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4315   text
*      -->P_4316   text
*----------------------------------------------------------------------*
FORM bdc_screen  USING value(p_program) TYPE bdcdata-program
                       value(p_dynpro)  TYPE bdcdata-dynpro.

  CLEAR wa_bdcdata.
  wa_bdcdata-program  = p_program.
  wa_bdcdata-dynpro   = p_dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.


ENDFORM.                    " BDC_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4320   text
*      -->P_4321   text
*----------------------------------------------------------------------*
FORM bdc_fields  USING value(p_fnam) TYPE bdcdata-fnam
                       value(p_fval)." TYPE bdcdata-fval.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam  = p_fnam.
  wa_bdcdata-fval  = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " BDC_FIELDS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_rev TABLES p_lt_bseg.

  DATA : lt_index_rows TYPE lvc_t_row,
         lt_row_no     TYPE lvc_t_roid,
         lw_row_no     TYPE lvc_s_roid,
         ref1          TYPE REF TO cl_gui_alv_grid.

  DATA : lt_fieldcat TYPE lvc_t_fcat,
         ls_fieldcat LIKE lvc_s_fcat,
         ls_layout   TYPE lvc_s_layo, "slis_layout_alv,
         lv_program  TYPE sy-repid.

  lv_program = sy-repid.

*-- Define Field Catalog
  ls_fieldcat-fieldname = 'BELNR'.
  ls_fieldcat-tabname   = 't_bkpf'.
  ls_fieldcat-scrtext_l = 'Document No.' .

  APPEND ls_fieldcat TO lt_fieldcat .

  ls_fieldcat-fieldname = 'BKTXT'.
  ls_fieldcat-tabname   = 't_bkpf'.
  ls_fieldcat-scrtext_l = 'Qty./Material No.' .

  APPEND ls_fieldcat TO lt_fieldcat .

*-- ALV Layout
  ls_layout-sel_mode          = 'A'.
  ls_layout-zebra             = 'X'.
  ls_layout-cwidth_opt        = 'X'.

*-- Execute ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = lv_program
      i_callback_pf_status_set = 'SET_PF_RV'
      i_callback_user_command  = 'FRM_USERCOMMAND_RV'
      i_structure_name         = 't_data_edit'
      i_grid_title             = 'Document List'
      is_layout_lvc            = ls_layout
      it_fieldcat_lvc          = lt_fieldcat
    TABLES
      t_outtab                 = gt_bkpf[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV_REV
*&---------------------------------------------------------------------*
*&      Form  frm_usercommand_RV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM frm_usercommand_rv USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN 'REVERSAL'.
      PERFORM reverse_document_post.
  ENDCASE.

ENDFORM.                    "frm_usercommand_edit
*&---------------------------------------------------------------------*
*&      Form  set_pf_rv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_rv USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTATUS_RV'.

ENDFORM.                    "set_pf_rv
*&---------------------------------------------------------------------*
*&      Form  REVERSE_DOCUMENT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_document_post.

  DATA : lt_index_rows TYPE lvc_t_row,
         lt_row_no     TYPE lvc_t_roid,
         lw_row_no     TYPE lvc_s_roid,
         lv_date       TYPE string.

  DATA : ref1          TYPE REF TO cl_gui_alv_grid,
         ref_grid      TYPE REF TO cl_gui_alv_grid,
         gs_bseg       TYPE t_bseg,
         gs_zrevdoc    TYPE zrevdoc,
         gs_ztmmrecps  TYPE ztmmrecps,
         lv_bukrs      TYPE bseg-bukrs,
         lv_belnr      TYPE bseg-belnr,
         lv_gjahr      TYPE bseg-gjahr,
         lv_round      TYPE p DECIMALS 0.

*-- Date Manipulation

  CONCATENATE sy-datum+4(2) '/'
              sy-datum+6(2) '/'
              sy-datum+0(4) INTO lv_date.

  CONDENSE lv_date.

*-- Refresh ALV
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CALL METHOD ref1->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = lt_row_no.

  IF sy-subrc = 0.

*-- Fill BDC Structure
    READ TABLE lt_row_no INTO lw_row_no INDEX 1.
    READ TABLE gt_bkpf   INDEX lw_row_no-row_id.

    IF sy-subrc = 0.

      PERFORM bdc_screen USING 'SAPMF05A' '0105'.
      PERFORM bdc_fields USING:
          'BDC_CURSOR'    'BSIS-BUDAT',
          'BDC_OKCODE'    'BU',
          'RF05A-BELNS'   gt_bkpf-belnr,
          'BKPF-BUKRS'    gt_bkpf-bukrs,
          'RF05A-GJAHS'   gt_bkpf-gjahr,
          'UF05A-STGRD'   '01',
          'BSIS-BUDAT'    lv_date,
          'BSIS-MONAT'    sy-datum+4(2).

      PERFORM bdc_screen USING 'SAPMF05A' '0105'.
      PERFORM bdc_fields USING:
           'BDC_OKCODE'    '/ERW',
           'BDC_CURSOR'    'RF05A-BELNS'.

      PERFORM bdc_screen USING 'SAPLSPO1' '0200'.
      PERFORM bdc_fields USING:
            'BDC_OKCODE'    '=YES'.

*-- Call Transaction FB80 - Reversal
      CALL TRANSACTION 'FB08'
          USING it_bdcdata
          UPDATE 'S'
          MESSAGES INTO it_message
          MODE 'N'.

*-- Get Message
      IF sy-subrc = 0.

        READ TABLE it_message INDEX 1.

        SELECT SINGLE
               bukrs
               belnr
               gjahr
               dmbtr
               zuonr
               aufnr
          INTO gs_bseg
          FROM bseg
         WHERE bukrs = gt_bkpf-bukrs
           AND belnr = gt_bkpf-belnr
           AND gjahr = gt_bkpf-gjahr
           AND hkont LIKE '159070%'.

        gs_zrevdoc-belnr   = it_message-msgv1.
        gs_zrevdoc-zmatnr  = gv_matdoc.
        gs_zrevdoc-aufnr   = gs_bseg-aufnr.
        gs_zrevdoc-zuonr   = gs_bseg-zuonr.
        gs_zrevdoc-xblnr   = gv_xblnr.
        gs_zrevdoc-zeile   = gv_zeile.
        gs_zrevdoc-bdmbtr  = gs_bseg-dmbtr.
        gs_zrevdoc-mdmbtr  = gv_dmbtr.
        gs_zrevdoc-unprc   = gs_zrevdoc-mdmbtr / gv_erfmg.
        gs_zrevdoc-erfmg   = gs_zrevdoc-erfmg + ( gs_bseg-dmbtr / gs_zrevdoc-unprc ).

        IF gs_zrevdoc-unprc > 0.
          gs_zrevdoc-revqty  = gs_zrevdoc-bdmbtr / gs_zrevdoc-unprc.
        ENDIF.

*-- Modify ZREVDOC
        MODIFY zrevdoc FROM gs_zrevdoc.

*-- Modify ZTMMRECPS

        SELECT SINGLE *
          FROM ztmmrecps
          INTO gs_ztmmrecps
          WHERE mblnr EQ gv_material
            AND zeile EQ gv_zeile.

        IF sy-subrc = 0.

          gs_ztmmrecps-openqty = gs_ztmmrecps-openqty + ( gs_zrevdoc-bdmbtr / gs_zrevdoc-unprc ).

          PERFORM round_qty CHANGING gs_ztmmrecps-openqty.  " Round up quantity value

          IF gt_bkpf-blart = 'UT' OR gt_bkpf-blart = 'JV'.

            gs_ztmmrecps-usedqty = gs_ztmmrecps-usedqty - ( gs_zrevdoc-bdmbtr / gs_zrevdoc-unprc ).

            PERFORM round_qty CHANGING gs_ztmmrecps-usedqty.  " Round up quantity value

          ENDIF.

          MODIFY ztmmrecps FROM gs_ztmmrecps.

        ENDIF.

        DELETE gt_bkpf INDEX lw_row_no-row_id.

        COMMIT WORK.

*-- Refresh Tables
        IF ref_grid IS INITIAL.
          CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
            IMPORTING
              e_grid = ref_grid.
        ENDIF.

        IF NOT ref_grid IS INITIAL.
          CALL METHOD ref_grid->check_changed_data.
        ENDIF.

      ELSE.

        ROLLBACK WORK.

        READ TABLE it_message INTO wa_message WITH KEY msgtyp = 'E'.

        CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
          EXPORTING
            titel = 'Reversal Message'
            msgid = wa_message-msgid
            msgty = 'E'
            msgno = sy-msgno.

      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH it_bdcdata.
ENDFORM.                    " REVERSE_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&      Form  AUTO_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM auto_post .

  DATA: lv_opqty_a  TYPE mseg-erfmg,
        lv_trqty_a  TYPE mseg-erfmg,
        lv_matkl_a  TYPE mara-matkl,
        lv_budat_wm TYPE sy-datum,
        lv_budat_nw TYPE sy-datum.

  CHECK gt_final[] IS NOT INITIAL.
  "--Fieldcatalog and Layout
  PERFORM create_fcat_and_layout.

  "--Refresh Status/Messages
  PERFORM refresh_messages.

"-- Commented by RANDYSY 04/28/2014 TN#60934

*  LOOP AT gt_final ASSIGNING <fs_tfinal>.
*
*    CLEAR : gt_afko,
*            gt_proj.
*
*    READ TABLE gt_afko WITH KEY aufnr = <fs_tfinal>-nplnr.
*
*    IF gt_afko IS NOT INITIAL.
*      READ TABLE gt_proj WITH KEY pspnr = gt_afko-pronr.
*    ENDIF.
*
**-- Identify Water/Non-water Meter
*    SELECT SINGLE matkl
*      FROM mara
*      INTO lv_matkl_a
*     WHERE matnr = <fs_tfinal>-matnr.
*
*    CASE lv_matkl_a.
*
*      WHEN '1200'.
*
**-- For Water Meter ( 45 Days )
*
*        IF gt_proj-plsez LE sy-datum.
*
*          lv_budat_wm = <fs_tfinal>-budat_mkpf + 45.
*
*          IF gt_proj-plsez GE lv_budat_wm.
*
*            <fs_tfinal>-unret = <fs_tfinal>-opqty.   " Post All Open Items
*
*            IF <fs_tfinal>-opqty IS NOT INITIAL.
*
*              PERFORM post_gl_f65.                          " Vendor PARK (Transaction F-65)
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*      WHEN OTHERS.
*
**-- For NON-Water Meter ( 30 Days )
*        lv_budat_nw = <fs_tfinal>-budat_mkpf + 30.
*
*
*        IF gt_proj-plsez LE sy-datum.
*
*          IF gt_proj-plsez GE lv_budat_nw.
*
*            <fs_tfinal>-unret = <fs_tfinal>-opqty.    " Post All Open Items
*
*            IF <fs_tfinal>-opqty IS NOT INITIAL.
*
*              PERFORM post_gl_f65.                           " Vendor PARK (Transaction F-65)
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*    ENDCASE.
*
*  ENDLOOP.
*
*  UNASSIGN <fs_tfinal>.

"-- Commented by RANDYSY 04/28/2014 TN#60934

  SORT gt_final BY opqty DESCENDING.

ENDFORM.                    " AUTO_POST
*&---------------------------------------------------------------------*
*&      Form  ROUND_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_ZTMMRECPS_USEDQTY  text
*----------------------------------------------------------------------*
FORM round_qty  CHANGING p_gs_ztmmrecps_usedqty.

  DATA : lv_round TYPE p DECIMALS 0.

  CLEAR lv_round.

  CALL FUNCTION 'ROUND'
    EXPORTING
      input         = p_gs_ztmmrecps_usedqty
    IMPORTING
      output        = lv_round
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_gs_ztmmrecps_usedqty = lv_round.

ENDFORM.                    " ROUND_QTY