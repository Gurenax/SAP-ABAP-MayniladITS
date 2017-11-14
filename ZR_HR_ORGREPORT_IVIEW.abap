*&---------------------------------------------------------------------*
*& Report      : ZR_HR_ORGREPORT_TO_ORGPLUS                            *
*& Description : Org Management Report to OrgPlus                      *
*& Created by  : Patrich Medios                                        *
*& Created on  : 03/28/2014                                            *
*& Reference   : Customized                                            *
*&---------------------------------------------------------------------*
*& Revision History                                                    *
*&---------------------------------------------------------------------*
*& Date        Author ID       Ticket No.   Description
*& 10/03/2014  RANDYSY         TN191        Fix Reports To Field
*& 10/10/2014  GDIMALIWAT      TN191        Added More Fixes: Reports To Field
*&---------------------------------------------------------------------*
REPORT zr_hr_orgreport_iview.

*-----------------------------------------------------------------------
*/ Program INCLUDE members
INCLUDE: zn_hr_orgreport_iview_top,
         zn_hr_orgreport_iview_f01.

*-----------------------------------------------------------------------
*/ At selection
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_orgeh.
* Org Unit search help
  PERFORM f_org_unit_shlp.

*-----------------------------------------------------------------------
*/ Start of selection
START-OF-SELECTION.
* Get data
  PERFORM f_get_data.
* Consolidate data
  PERFORM f_consolidate_data.

*-----------------------------------------------------------------------
*/ End of selection
END-OF-SELECTION.
  IF gt_output[] IS NOT INITIAL.
*   Display report
    PERFORM f_display_report USING gt_output.
  ELSE.
*   No data found
    MESSAGE text-e01 TYPE co_msgtyp_s DISPLAY LIKE co_msgtyp_e.
  ENDIF.