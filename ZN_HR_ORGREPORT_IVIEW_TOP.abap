*&---------------------------------------------------------------------*
*&  Include           ZN_HR_ORGREPORT_IVIEW_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*/ Types definitions
TYPES: BEGIN OF ty_orgeh_shlp_s,
         objid TYPE hrobjid,
         begda TYPE begdatum,
         endda TYPE enddatum,
         stext TYPE stext,
       END OF ty_orgeh_shlp_s,

       BEGIN OF ty_hrp1001_s,
         otype TYPE otype,
         objid TYPE hrobjid,
         plvar TYPE plvar,
         rsign TYPE rsign,
         relat TYPE relat,
         istat TYPE istat_d,
         priox TYPE priox,
         begda TYPE begdatum,
         endda TYPE enddatum,
         varyf TYPE varyf,
         seqnr TYPE seqnr,
         sclas TYPE sclas,
         sobid TYPE sobid,
       END OF ty_hrp1001_s,

       BEGIN OF ty_hrp1000_s,
         plvar TYPE plvar,
         otype TYPE otype,
         objid TYPE hrobjid,
         istat TYPE istat_d,
         begda TYPE begdatum,
         endda TYPE enddatum,
         langu TYPE langu,
         seqnr TYPE seqnr,
         stext TYPE stext,
       END OF ty_hrp1000_s,

       BEGIN OF ty_pa0002_s,
         pernr TYPE persno,
         subty TYPE subty,
         objps TYPE objps,
         sprps TYPE sprps,
         endda TYPE endda,
         begda TYPE begda,
         seqnr TYPE seqnr,
         nachn TYPE pad_nachn,
         nach2 TYPE pad_nach2,  "DV2K915414 DTALOSIG
         vorna TYPE pad_vorna,
         gesch TYPE gesch,
         gbdat TYPE gbdat,      "DV2K915414 DTALOSIG
         natio TYPE natsl,
         famst TYPE famst,
       END OF ty_pa0002_s,

       BEGIN OF ty_pa0105_s,
         pernr      TYPE persno,
         subty      TYPE subty,
         objps      TYPE objps,
         sprps      TYPE sprps,
         endda      TYPE endda,
         begda      TYPE begda,
         seqnr      TYPE seqnr,
         usrty      TYPE usrty,
         usrid_long TYPE comm_id_long,
       END OF ty_pa0105_s,

       BEGIN OF ty_pa0001_s,
         pernr TYPE persno,
         subty TYPE subty,
         objps TYPE objps,
         sprps TYPE sprps,
         endda TYPE endda,
         begda TYPE begda,
         seqnr TYPE seqnr,
         persg TYPE persg,
         persk TYPE persk,
       END OF ty_pa0001_s,

       BEGIN OF ty_pa0000_s,
         pernr TYPE persno,
         subty TYPE subty,
         objps TYPE objps,
         sprps TYPE sprps,
         endda TYPE endda,
         begda TYPE begda,
         seqnr TYPE seqnr,
         stat2 TYPE stat2,
       END OF ty_pa0000_s,

       BEGIN OF ty_pa0008_s,
         pernr TYPE persno,
         subty TYPE subty,
         objps TYPE objps,
         sprps TYPE sprps,
         endda TYPE endda,
         begda TYPE begda,
         seqnr TYPE seqnr,
         lga01 TYPE lgart,
         bet01 TYPE pad_amt7s,
       END OF ty_pa0008_s,

       ty_output_t  TYPE STANDARD TABLE OF zst_hr_orgrep_to_orgplus.

*-----------------------------------------------------------------------
*/ Data definitions - Internal tables
DATA: gt_orgeh_shlp  TYPE STANDARD TABLE OF ty_orgeh_shlp_s, "#EC NEEDED
      gt_hrp1001     TYPE STANDARD TABLE OF ty_hrp1001_s,   "#EC NEEDED
      gt_hrp1001_top TYPE STANDARD TABLE OF ty_hrp1001_s,   "#EC NEEDED
      gt_hrp1001_org TYPE STANDARD TABLE OF ty_hrp1001_s,   "#EC NEEDED
      gt_hrp1001_pos TYPE STANDARD TABLE OF ty_hrp1001_s,   "#EC NEEDED
      gt_hrp1000     TYPE STANDARD TABLE OF ty_hrp1000_s,   "#EC NEEDED
      gt_pa0002      TYPE STANDARD TABLE OF ty_pa0002_s,    "#EC NEEDED
      gt_pa0105      TYPE STANDARD TABLE OF ty_pa0105_s,    "#EC NEEDED
      gt_pa0001      TYPE STANDARD TABLE OF ty_pa0001_s,    "#EC NEEDED
      gt_pa0041      TYPE STANDARD TABLE OF pa0041,         "#EC NEEDED
      gt_pa0000      TYPE STANDARD TABLE OF ty_pa0000_s,    "#EC NEEDED
      gt_pa0008      TYPE STANDARD TABLE OF ty_pa0008_s,    "#EC NEEDED
      gt_t501t       TYPE STANDARD TABLE OF t501t,          "#EC NEEDED
      gt_t503t       TYPE STANDARD TABLE OF t503t,          "#EC NEEDED
      gt_t529u       TYPE STANDARD TABLE OF t529u,          "#EC NEEDED
      gt_t502t       TYPE STANDARD TABLE OF t502t,          "#EC NEEDED
      gt_t005t       TYPE STANDARD TABLE OF t005t,          "#EC NEEDED
      gt_output      TYPE ty_output_t,                      "#EC NEEDED
      gt_fieldcat    TYPE slis_t_fieldcat_alv.              "#EC NEEDED

*-----------------------------------------------------------------------
*/ Data definitions - Variables


*-----------------------------------------------------------------------
*/ Data definitions - Constants
CONSTANTS: co_msgtyp_s      TYPE msgty VALUE 'S',
           co_msgtyp_e      TYPE msgty VALUE 'E',
           co_mark_x        TYPE flag  VALUE 'X',
           co_lang_en       TYPE spras VALUE 'E',
           co_otype_org     TYPE otype VALUE 'O',
           co_otype_pos     TYPE otype VALUE 'S',
           co_otype_pers    TYPE otype VALUE 'P',
           co_plvar_current TYPE plvar VALUE '01',
           co_rsign_a       TYPE rsign VALUE 'A',
           co_rsign_b       TYPE rsign VALUE 'B',
           co_relat_002     TYPE relat VALUE '002',
           co_relat_003     TYPE relat VALUE '003',
           co_relat_008     TYPE relat VALUE '008',
           co_relat_012     TYPE relat VALUE '012',
           co_sclas         TYPE sclas VALUE 'C', "DV2K915694
           co_otype_job     TYPE otype VALUE 'C'. "DV2K915694

*-----------------------------------------------------------------------
*/ Selection screen
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
PARAMETERS: pa_orgeh TYPE orgeh OBLIGATORY,
            pa_level TYPE numc2 OBLIGATORY,
            pa_date  TYPE begdatum OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK blk1.