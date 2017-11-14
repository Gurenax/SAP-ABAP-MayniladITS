*&---------------------------------------------------------------------*
*& Report  ZRFI_AMD_UPLOAD
*& DESCRIPTION: Massive Upload of Asset Master Data
*&---------------------------------------------------------------------*
*& Created by : GDIMALIWAT
*& Created On : 01/22/2014
*& Reference  : TN#54954
*&---------------------------------------------------------------------*
*& Date      | Author ID  | Ticket No. | Description
*&---------------------------------------------------------------------*
*& 2014/01/22| GDIMALIWAT | 54954       | Created
*&---------------------------------------------------------------------*
REPORT zrfi_amd_upload MESSAGE-ID zisu.

*INCLUDE
*TABLES:
*TYPE-POOLS:
*&---------------------------------------------------------------------*
*& Type Definitions - Variables (ty_<name>)
*&
TYPES: BEGIN OF ty_upload,
        bukrs               TYPE c LENGTH 5, "anla-bukrs,                        "Company Code
        anln1               TYPE c LENGTH 12,"anla-anln1,                        "Main Asset Number
        anln2               TYPE c LENGTH 4, "anla-anln2,                        "Asset Subnumber
        anlkl               TYPE c LENGTH 8,"anla-anlkl,                         "Asset Class
        txt50               TYPE c LENGTH 50,"anla-txt50,                        "Asset description
        txa50               TYPE c LENGTH 50,"anla-txa50,                        "Asset description (2)
        ordtx2              TYPE c LENGTH 4, "t087t-ordtx,                       "Generic Description (Asset Item Desc)
        zzbrand             TYPE c LENGTH 40,"anlu-zzbrand,                      "Brand
        zztype              TYPE c LENGTH 40,"anlu-zztype,                       "Type
        sernr               TYPE c LENGTH 18,"anla-sernr,                        "Serial Number
        zzold_prop_num      TYPE c LENGTH 20,"anlu-zzold_prop_num,               "Old Property Number
        "aktiv               TYPE c LENGTH 10,"anla-aktiv,                        "Asset Capitalization Date
        "anbtr               TYPE c LENGTH 16,"anep-anbtr,                        "Acquisition Value
        zz_acqui_date       TYPE c LENGTH 10,"anlu-zz_acqui_date,                "Acquisition/Delivery Date
        zzacqui_cost        TYPE c LENGTH 16,"anlu-zzacqui_cost,                 "Acquisition Cost
        kostl               TYPE c LENGTH 10,"anlz-kostl,                        "Cost Center
        gdlgrp              TYPE c LENGTH 8,"anla-gdlgrp,                        "Evaluation group 5
        zzcontract_num      TYPE c LENGTH 30,"anlu-zzcontract_num,               "Contract Number
        zzcontract_desc     TYPE c LENGTH 40,"anlu-zzcontract_desc,              "Contract Description
        zz_plate_no         TYPE c LENGTH 16,"anlu-zz_plate_no,                  "Plate No.
        zz_engine_no        TYPE c LENGTH 30,"anlu-zz_engine_no,                 "Engine No.
        zz_chassis_no       TYPE c LENGTH 30,"anlu-zz_chassis_no,                "Chassis No.
        zz_rr_no            TYPE c LENGTH 30,"anlu-zz_rr_no,                     "RR No.
        zz_rr_date          TYPE c LENGTH 30,"anlu-zz_rr_date,                   "RR Date
        zz_aadf_no          TYPE c LENGTH 30,"anlu-zz_aadf_no,                   "AADF No.
        zz_aadf_date        TYPE c LENGTH 30,"anlu-zz_aadf_date,                 "AADF Date
        zz_or_no            TYPE c LENGTH 30,"anlu-zz_or_no,                     "OR No.
        zz_or_date          TYPE c LENGTH 30,"anlu-zz_or_date,                   "OR Date
        zzlocation          TYPE c LENGTH 40,"anlu-zzlocation,                   "Location
        zz_invtag_no        TYPE c LENGTH 30,"anlu-zz_invtag_no,                 "Inventory Tag No.
        ivdat               TYPE c LENGTH 10, "anla-ivdat,                       "Last inventory on
        inken               TYPE c LENGTH 1, "anla-inken,                        "Inventory Note
        invzu               TYPE c LENGTH 15,"anla-invzu,                        "Inventory List
        ordtx1              TYPE c LENGTH 4, "t087t-ordtx,                       "Status
        zz_former_acct      TYPE c LENGTH 30,"anlu-zz_former_acct,               "Former Accountable
        zzengas_code        TYPE c LENGTH 30,"anlu-zzengas_code,                 "Engas Code
        zzartcle_code       TYPE c LENGTH 30,"anlu-zzartcle_code,                "Article Code
        zzasset_category    TYPE c LENGTH 16,"anlu-zzasset_category,             "Asset Category
        zzloan_package      TYPE c LENGTH 10,"anlu-zzloan_package,               "Loan Package
        zz_loan_currency    TYPE c LENGTH 3, "anlu-zz_loan_currency,             "Loan Package Currency
        zz_proof_owner      TYPE c LENGTH 20,"anlu-zz_proof_owner,               "Proof of Ownership
        zz_lot_no           TYPE c LENGTH 30,"anlu-zz_lot_no,                    "Lot No.
        zz_survey_no        TYPE c LENGTH 30,"anlu-zz_survey_no,                 "Survey No.
        zz_zone             TYPE c LENGTH 30,"anlu-zz_zone,                      "Zone/Grid
        zz_remarks          TYPE c LENGTH 30,"anlu-zz_remarks,                   "Remarks
       END OF ty_upload,

       BEGIN OF ty_file,
        data  TYPE string,
       END OF ty_file.

*&---------------------------------------------------------------------*
*& Data Definitions - Internal Tables (gt_<itab name>)
*&
DATA: gt_upload     TYPE TABLE OF ty_upload, "#EC NEEDED
      gt_file       TYPE TABLE OF ty_file.   "#EC NEEDED

*&---------------------------------------------------------------------*
*& Data Definitions - Range (gr_<name>), Variables (gv_<name>)
*&                    Structure/Work Area (gs_<name>)
"DATA: gv_<name>  TYPE <datatype>.

*&----------------------------------------------------------------------
*
*& Data Definitions - Constants (co_<name>)
*&
CONSTANTS:  co_green      TYPE icon_d     VALUE '@08@',
            co_red        TYPE icon_d     VALUE '@0A@',
            co_separator  TYPE c LENGTH 1 VALUE `|`.

*&---------------------------------------------------------------------*
*& Data Definitions - Field Symbols (<fx_<name>>)
*&
FIELD-SYMBOLS:  <fs_upload>   TYPE ty_upload, "#EC NEEDED
                <fs_file>     TYPE ty_file.   "#EC NEEDED

*&---------------------------------------------------------------------*
*& Program Selections (pa_<parameter name> so_<select options name>)
*&
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-000.
PARAMETERS: pa_file     TYPE rlgrap-filename.
"pa_mode     TYPE ctu_mode DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK blk01.

*&---------------------------------------------------------------------*
*& At Selection Screen Events
*&
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  PERFORM get_file.
*AT SELECTION-SCREEN.
*
*AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& Initialization
*&
*INITIALIZATION.

*&---------------------------------------------------------------------*
*& Load of Program - at start of program
*&
*LOAD-OF-PROGRAM.

*&---------------------------------------------------------------------*
*& Start of Selection - Begin Main Program Processing
*&
START-OF-SELECTION.

  PERFORM read_data.
  PERFORM update_data.

*&---------------------------------------------------------------------*
*& End of Selection - Perform end Processing
*&
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       Open file dialog box
*----------------------------------------------------------------------*
FORM get_file .
  DATA: file_table TYPE filetable,
        rc         TYPE i,
        lv_title   TYPE string.

  lv_title = text-002.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      default_extension = 'txt'
*     DEFAULT_FILENAME  =
      file_filter       = '*.*|*.TXT'
*     WITH_ENCODING     =
*     INITIAL_DIRECTORY =
*     MULTISELECTION    =
    CHANGING
      file_table        = file_table
      rc                = rc.

  IF sy-subrc EQ 0.
    READ TABLE file_table INTO pa_file INDEX 1.
  ENDIF.
ENDFORM.                    " GET_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read data from input file
*----------------------------------------------------------------------*
FORM read_data .

  DATA: lv_file         TYPE string.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pa_file
    IMPORTING
      output = lv_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
    TABLES
      data_tab                = gt_file
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc EQ 0.
    LOOP AT gt_file ASSIGNING <fs_file>.
      APPEND INITIAL LINE TO gt_upload ASSIGNING <fs_upload>.
      SPLIT <fs_file>-data AT co_separator INTO <fs_upload>-bukrs
                                                <fs_upload>-anln1
                                                <fs_upload>-anln2
                                                <fs_upload>-anlkl
                                                <fs_upload>-txt50
                                                <fs_upload>-txa50
                                                <fs_upload>-ordtx2
                                                <fs_upload>-zzbrand
                                                <fs_upload>-zztype
                                                <fs_upload>-sernr
                                                <fs_upload>-zzold_prop_num
                                                "<fs_upload>-aktiv
                                                "<fs_upload>-anbtr
                                                <fs_upload>-zz_acqui_date
                                                <fs_upload>-zzacqui_cost
                                                <fs_upload>-kostl
                                                <fs_upload>-gdlgrp
                                                <fs_upload>-zzcontract_num
                                                <fs_upload>-zzcontract_desc
                                                <fs_upload>-zz_plate_no
                                                <fs_upload>-zz_engine_no
                                                <fs_upload>-zz_chassis_no
                                                <fs_upload>-zz_rr_no
                                                <fs_upload>-zz_rr_date
                                                <fs_upload>-zz_aadf_no
                                                <fs_upload>-zz_aadf_date
                                                <fs_upload>-zz_or_no
                                                <fs_upload>-zz_or_date
                                                <fs_upload>-zzlocation
                                                <fs_upload>-zz_invtag_no
                                                <fs_upload>-ivdat
                                                <fs_upload>-inken
                                                <fs_upload>-invzu
                                                <fs_upload>-ordtx1
                                                <fs_upload>-zz_former_acct
                                                <fs_upload>-zzengas_code
                                                <fs_upload>-zzartcle_code
                                                <fs_upload>-zzasset_category
                                                <fs_upload>-zzloan_package
                                                <fs_upload>-zz_loan_currency
                                                <fs_upload>-zz_proof_owner
                                                <fs_upload>-zz_lot_no
                                                <fs_upload>-zz_survey_no
                                                <fs_upload>-zz_zone
                                                <fs_upload>-zz_remarks.
    ENDLOOP.
    UNASSIGN <fs_file>.

  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       Run the AS02 BDC
*----------------------------------------------------------------------*
FORM update_data .

  DATA: ls_bukrs                TYPE bapi1022_1-comp_code,
        ls_anln1                TYPE bapi1022_1-assetmaino,
        ls_anln2                TYPE bapi1022_1-assetsubno,
        ls_generaldata          TYPE bapi1022_feglg001,
        ls_generaldatax         TYPE bapi1022_feglg001x,
        ls_inventory            TYPE bapi1022_feglg011,
        ls_inventoryx           TYPE bapi1022_feglg011x,
        ls_timedependentdata    TYPE bapi1022_feglg003,
        ls_timedependentdatax   TYPE bapi1022_feglg003x,
        ls_allocations          TYPE bapi1022_feglg004,
        ls_allocationsx         TYPE bapi1022_feglg004x,
        ls_return               TYPE bapiret2,
        ls_anlu                 TYPE anlu.

  CONSTANTS:  co_x TYPE c LENGTH 1 VALUE 'X'.

  LOOP AT gt_upload ASSIGNING <fs_upload>.
    CLEAR: ls_bukrs,
           ls_anln1,
           ls_anln2,
           ls_generaldata,
           ls_generaldatax,
           ls_inventory,
           ls_inventoryx,
           ls_timedependentdata,
           ls_timedependentdatax,
           ls_allocations,
           ls_allocationsx,
           ls_return,
           ls_anlu.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_upload>-bukrs
      IMPORTING
        output = ls_bukrs.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_upload>-anln1
      IMPORTING
        output = ls_anln1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_upload>-anln2
      IMPORTING
        output = ls_anln2.

    SELECT SINGLE *
    INTO ls_anlu
    FROM anlu
    WHERE bukrs EQ ls_bukrs
      AND anln1 EQ ls_anln1
      AND anln2 EQ ls_anln2.

    IF sy-subrc EQ 0.
      ls_anlu-zzold_prop_num    = <fs_upload>-zzold_prop_num.
      ls_anlu-zzbrand           = <fs_upload>-zzbrand.
      ls_anlu-zztype            = <fs_upload>-zztype.
      ls_anlu-zzacqui_cost      = <fs_upload>-zzacqui_cost.
      ls_anlu-zzengas_code      = <fs_upload>-zzengas_code.
      ls_anlu-zzartcle_code     = <fs_upload>-zzartcle_code.
      ls_anlu-zzcontract_num    = <fs_upload>-zzcontract_num.
      ls_anlu-zzcontract_desc   = <fs_upload>-zzcontract_desc.
      ls_anlu-zzlocation        = <fs_upload>-zzlocation.
      ls_anlu-zzasset_category  = <fs_upload>-zzasset_category.
      ls_anlu-zzloan_package    = <fs_upload>-zzloan_package.
      ls_anlu-zz_loan_currency  = <fs_upload>-zz_loan_currency.
      ls_anlu-zz_proof_owner    = <fs_upload>-zz_proof_owner.
      ls_anlu-zz_lot_no         = <fs_upload>-zz_lot_no.
      ls_anlu-zz_survey_no      = <fs_upload>-zz_survey_no.
      ls_anlu-zz_acqui_date     = <fs_upload>-zz_acqui_date.
      ls_anlu-zz_plate_no       = <fs_upload>-zz_plate_no.
      ls_anlu-zz_engine_no      = <fs_upload>-zz_engine_no.
      ls_anlu-zz_chassis_no     = <fs_upload>-zz_chassis_no.
      ls_anlu-zz_aadf_no        = <fs_upload>-zz_aadf_no.
      ls_anlu-zz_aadf_date      = <fs_upload>-zz_aadf_date.
      ls_anlu-zz_or_no          = <fs_upload>-zz_or_no.
      ls_anlu-zz_or_date        = <fs_upload>-zz_or_date.
      ls_anlu-zz_zone           = <fs_upload>-zz_zone.
      ls_anlu-zz_rr_no          = <fs_upload>-zz_rr_no.
      ls_anlu-zz_rr_date        = <fs_upload>-zz_rr_date.
      ls_anlu-zz_invtag_no      = <fs_upload>-zz_invtag_no.
      ls_anlu-zz_former_acct    = <fs_upload>-zz_former_acct.
      ls_anlu-zz_remarks        = <fs_upload>-zz_remarks.
      ls_anlu-zzdate_modified   = sy-datum.

      MODIFY anlu
      FROM ls_anlu.

      IF sy-subrc EQ 0.
        COMMIT WORK.

        ls_generaldata-descript         = <fs_upload>-txt50.          "Asset Description
        ls_generaldata-descript2        = <fs_upload>-txa50.          "Additional asset description

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_upload>-sernr
          IMPORTING
            output = ls_generaldata-serial_no.

        ls_generaldatax-descript        = co_x.
        ls_generaldatax-descript2       = co_x.
        ls_generaldatax-serial_no       = co_x.

        CONCATENATE <fs_upload>-ivdat+6(4) <fs_upload>-ivdat(2) <fs_upload>-ivdat+3(2) INTO ls_inventory-date.          "Last inventory date
        ls_inventory-note               = <fs_upload>-invzu.          "Supplementary inventory specifications
        ls_inventory-include_in_list    = <fs_upload>-inken.          "Inventory indicator
        ls_inventoryx-date              = co_x.
        ls_inventoryx-note              = co_x.
        ls_inventoryx-include_in_list   = co_x.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_upload>-kostl
          IMPORTING
            output = ls_timedependentdata-costcenter.   "Cost Center

        ls_timedependentdatax-costcenter = co_x.


        ls_allocations-evalgroup1  = <fs_upload>-ordtx1.
        ls_allocations-evalgroup2  = <fs_upload>-ordtx2.
        ls_allocations-evalgroup5  = <fs_upload>-gdlgrp. "Evaluation Group 5
        ls_allocationsx-evalgroup1 = co_x.
        ls_allocationsx-evalgroup2 = co_x.
        ls_allocationsx-evalgroup5 = co_x.

        CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
          EXPORTING
            companycode        = ls_bukrs
            asset              = ls_anln1
            subnumber          = ls_anln2
            generaldata        = ls_generaldata
            generaldatax       = ls_generaldatax
            inventory          = ls_inventory
            inventoryx         = ls_inventoryx
            timedependentdata  = ls_timedependentdata
            timedependentdatax = ls_timedependentdatax
            allocations        = ls_allocations
            allocationsx       = ls_allocationsx
          IMPORTING
            return             = ls_return.

        IF ls_return-type EQ 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          WRITE:/ co_green, text-001, <fs_upload>-anln1, ls_return-message.
        ELSE.
          WRITE:/ co_red, text-003, <fs_upload>-anln1, text-004, ls_return-message.
        ENDIF.
      ELSE.
        WRITE:/ co_red, text-003, <fs_upload>-anln1, text-004, text-005.
      ENDIF.
    ELSE.
      WRITE:/ co_red, text-003, <fs_upload>-anln1, text-004, text-006.
    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_upload>.
ENDFORM.                    " UPDATE_DATA