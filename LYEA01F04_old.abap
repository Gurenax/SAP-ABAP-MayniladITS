*----------------------------------------------------------------------*
***INCLUDE LEA01F04 .
*----------------------------------------------------------------------*
*&      Form  IZW_ADJUST_BY_INACH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*orm izw_adjust_by_inach using xy_obj type isu2a_billing_data.
*
* data h_izw type isu2a_billing_data-izw.
* data wnach type isu2a_nach.
* data wzw   type isu2a_zw.
* data wzw2  type isu2a_zw.
*
* Es wird hier die IZW ggf. an die Nachberechnungszeiträume aus der
* INACH angepaßt. Zum Verständnis des Algorithmus ist zu beachten,
* daß die INACH vom ältesten zum jüngsten Zeitraum sortiert ist.
* if not xy_obj-inach[] is initial.
*   Es sind Nachberechnungsperioden vorhanden --> IZW zwischenspeichern
*   h_izw[] = xy_obj-izw[].
*   IZW löschen -- wird aus H_IZW neu aufgebaut
*   refresh xy_obj-izw[].
*   Neuaufbau je INACH-Zeitraum
*   loop at xy_obj-inach into wnach.
*     Es werden alle IZW-Zeitscheiben des INACH-Zeitraums selektiert
*     loop at h_izw into wzw
*       where ab  <= wnach-bis
*       and   bis >= wnach-ab.
*       Zeitraum an INACH-Zeitraum anpassen
*       wzw2 = wzw.
*       if wzw2-ab < wnach-ab.
*         wzw2-ab = wnach-ab.
*       endif.
*       if wzw2-bis > wnach-bis.
*         wzw2-bis = wnach-bis.
*       endif.
*       Nummer des INACH-Zeitraums merken und Eintrag hinzufügen
*       wzw2-backno = wnach-backno.
*       wzw2-pertyp = co_back_billing.
*       append wzw2 to xy_obj-izw.
*       Anpassen des verbleibenden IZW-Zeitraums
*       if wzw-bis > wnach-bis.
*         IZW-Zeitraum geht über den INACH-Zeitraum hinaus
*         wzw-ab = wnach-bis + 1.
*         modify h_izw from wzw.
*       else.
*         IZW-Zeitraum endet im INACH-Zeitraum
*         delete h_izw.
*       endif.
*     endloop.                         "h_izw
*   endloop.                           "xy_obj-inach
*   Die noch in der Tabelle H_IZW verbliebenden Einträge müssen
*   noch hinzugefügt werden. Dies sind genau die IZW-Zeiträume
*   die für den Abrechnungs- und Abschlagszeitraum relevant sind.
*   (Deren genaue Anpassung kann erst später stattfinden, wenn der
*   Abrechnungs- und Abschlagszeitraum bekannt ist.)
*   append lines of h_izw to xy_obj-izw.
* endif.
* sort xy_obj-izw by logikzw ab.       "!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*ndform.                               " IZW_ADJUST_BY_INACH
*&---------------------------------------------------------------------*
*&      Form  IGER_ADJUST_BY_INACH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*orm iger_adjust_by_inach using xy_obj type isu2a_billing_data.
*
* data h_iger type isu2a_billing_data-iger.
* data wnach  type isu2a_nach.
* data wger   type isu2a_ger.
* data wger2  type isu2a_ger.
*
* Es wird hier die IGER ggf. an die Nachberechnungszeiträume aus der
* INACH angepaßt. Zum Verständnis des Algorithmus ist zu beachten,
* daß die INACH vom ältesten zum jüngsten Zeitraum sortiert ist.
* if not xy_obj-inach[] is initial.
*   Es sind Nachberechnungsperioden vorhanden --> IGER zwischenspeichern
*   h_iger[] = xy_obj-iger[].
*   IGER löschen -- wird aus H_IGER neu aufgebaut
*   refresh xy_obj-iger[].
*   Neuaufbau je INACH-Zeitraum
*   loop at xy_obj-inach into wnach.
*     Es werden alle IGER-Zeitscheiben des INACH-Zeitraums selektiert
*     loop at h_iger into wger
*       where ab  <= wnach-bis
*       and   bis >= wnach-ab.
*       Zeitraum an INACH-Zeitraum anpassen
*       wger2 = wger.
*       if wger2-ab < wnach-ab.
*         wger2-ab = wnach-ab.
*       endif.
*       if wger2-bis > wnach-bis.
*         wger2-bis = wnach-bis.
*       endif.
*       Nummer des INACH-Zeitraums merken und EIntrag hinzufügen
*       wger2-backno = wnach-backno.
*       wger2-pertyp = co_back_billing.
*       append wger2 to xy_obj-iger.
*       Anpassen des verbleibenden iger-Zeitraums
*       if wger-bis > wnach-bis.
*         iger-Zeitraum geht über den INACH-Zeitraum hinaus
*         wger-ab = wnach-bis + 1.
*         modify h_iger from wger.
*       else.
*         iger-Zeitraum endet im INACH-Zeitraum
*         delete h_iger.
*       endif.
*     endloop.                         "h_iger
*   endloop.                           "xy_obj-inach
*   Die noch in der Tabelle H_IGER verbliebenden Einträge müssen
*   noch hinzugefügt werden. Dies sind genau die iger-Zeiträume
*   die für den Abrechnungs- und Abschlagszeitraum relevant sind.
*   (Deren genaue Anpassung kann erst später stattfinden, wenn der
*   Abrechnungs- und Abschlagszeitraum bekannt ist.)
*   append lines of h_iger to xy_obj-iger.
* endif.
*
*ndform.                               " IGER_ADJUST_BY_INACH
*&---------------------------------------------------------------------*
*&      Form  TABLES_ADJUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*orm tables_adjust using xy_obj type isu2a_billing_data.
*** IZW an Abrechnungs- und Abschlagszeitraum anpassen  " WWT XX99
*reak weidlich.
*   perform izw_adjust using xy_obj.                    " WWT XX99
* IGER an Abrechnungs- und Abschlagszeitraum anpassen
* perform iger_adjust using xy_obj.
*
*ndform.                               " TABLES_ADJUST
*&---------------------------------------------------------------------*
*&      Form  IZW_ADJUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*orm izw_adjust using xy_obj type isu2a_billing_data.
*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* data cc_izw type isu2a_billing_data-izw.    " WWT Test
* cc_izw =  xy_obj-izw.                       " WWT Test
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*
* data h_izw type isu2a_billing_data-izw.
* data wzw   type isu2a_zw.
* data wzw2  type isu2a_zw.
*
* Es wird hier die IZW an den Abrechnungszeitraum (und ggf. an den
* Abschlagszeitraum) angepaßt.
* Zunächst werden alle IZW-Einträge ab dem Beginn der
* Abrechnungsperiode selektiert (also alle außer die der
* Nachberechnungszeiträume). Diese werden zwischengespeichert
* und aus der IZW entfernt.
* loop at xy_obj-izw into wzw
*   where ab  >= xy_obj-date-begperiod.
*   append wzw to h_izw.
*   delete xy_obj-izw.
* endloop.                             "xy_obj-izw
* Verarbeitung der Einträge
* loop at h_izw into wzw.
*   if wzw-ab  <= xy_obj-abr-endabrpe.
*     Eintrag schneidet den Abrechnungszeitraum
*     wzw2 = wzw.
*     Zeitraum anpassen
*     if wzw2-bis > xy_obj-abr-endabrpe.
*       wzw2-bis = xy_obj-abr-endabrpe.
*     endif.
*     Kennzeichen für Abrechnungszeitraum
*     wzw2-pertyp = co_normal_billing.
*     angepaßten Eintrag übernehmen
*     append wzw2 to xy_obj-izw.
*   endif.
*   if not xy_obj-abs-bbp_sim is initial.
*     Es soll eine verbrauchsabhängige Abschlagshochrechnung
*     durchgeführt werden
*     if wzw-bis >= xy_obj-abs-begabspe and
*        wzw-ab  <= xy_obj-abs-endabspe.
*       Eintrag schneidet Abschlagszeitraum
*       wzw2 = wzw.
*       Zeitraum anpassen
*       if wzw2-ab < xy_obj-abs-begabspe.
*         wzw2-ab = xy_obj-abs-begabspe.
*       endif.
*       if wzw2-bis > xy_obj-abs-endabspe.
*         wzw2-bis = xy_obj-abs-endabspe.
*       endif.
*       Kennzeichen für Abschlagszeitraum setzen
*       wzw2-pertyp = co_budget_billing.
*       angepaßten Eintrag übernehmen
*       append wzw2 to xy_obj-izw.
*     endif.
*   endif.
* endloop.
*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*  xy_obj-izw  =  cc_izw .                      " WWT Test
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*
*ndform.                               " IZW_ADJUST
*&---------------------------------------------------------------------*
*&      Form  OUTSORT
*&---------------------------------------------------------------------*
*       Ausstuerung
*----------------------------------------------------------------------*
FORM outsort CHANGING p_obj  TYPE isu2a_billing_data
                      p_sobj TYPE isu2a_data_collector.

  DATA ausgrup LIKE etta-ausgrup.

  mac_break1 'OUTSORT'.                                    "#EC NOBREAK

  IF NOT p_sobj-ever-ausgrup IS INITIAL.
    ausgrup = p_sobj-ever-ausgrup.
  ELSE.
    ausgrup = p_obj-eap_etta-ausgrup.
  ENDIF.

  CALL FUNCTION 'ISU_BILL_OUTSORT'
    EXPORTING
      x_ausgrup    = ausgrup
      x_abrvorg    = p_obj-st-abrvorg
      x_manoutsort = p_sobj-ever-manoutsort
      x_outcount   = p_sobj-ever-outcount
    CHANGING
      xy_obj       = p_obj
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    mac_err_repeat general_fault.
  ELSE.
    APPEND LINES OF p_obj-bill_outsort    TO p_sobj-billoutsort.
    APPEND LINES OF p_obj-bill_manoutsort TO p_sobj-bill_manoutsort.
  ENDIF.

ENDFORM.                               " OUTSORT
*&---------------------------------------------------------------------*
*&      Form  DATE_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM date_fill CHANGING xy_obj TYPE isu2a_billing_data.

* Begin der Abrechnungsperiode allgemein merken, da die Datümer
* aus der ABR u.U. gelöscht werden (vgl. Abschlagssimulation)
  xy_obj-date-begperiod = xy_obj-abr-begabrpe.
* Beginndatum für die Datenanalyse
  IF xy_obj-trig-etrg-abrvorg = co_endabr.
*   Endabrechnung
    xy_obj-date-begana = xy_obj-abr-begend.
  ELSE.
*   'normale' Abrechnung
    IF xy_obj-abr-begnach IS INITIAL.
      xy_obj-date-begana = xy_obj-abr-begabrpe.
    ELSE.
      xy_obj-date-begana = xy_obj-abr-begnach.
    ENDIF.
  ENDIF.

ENDFORM.                               " DATE_FILL
*&---------------------------------------------------------------------*
*&      Form  ILOGIKZW_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ilogikzw_create USING xy_obj TYPE isu2a_billing_data.

  CALL FUNCTION 'ISU_ILOGIKZW_CREATE'
    EXPORTING
      x_izw      = xy_obj-izw
    IMPORTING
      y_ilogikzw = xy_obj-ilogikzw
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    mac_err_repeat general_fault.
  ENDIF.

ENDFORM.                               " ILOGIKZW_CREATE
*&---------------------------------------------------------------------*
*&      Form  DATES_CHECK_FOR_BCHECK
*&---------------------------------------------------------------------*
FORM dates_check_for_bcheck
                     CHANGING xy_obj TYPE isu2a_billing_data.

  IF xy_obj-abr-begabrpe > xy_obj-abr-endabrpe.
*   Geben Sie als Prüfdatum ein Datum ab dem &1 an
    mac_err '088' 'AJ' xy_obj-abr-begabrpe xy_obj-abr-endabrpe
                       space space general_fault.
  ENDIF.

ENDFORM.                               " DATES_CHECK_FOR_BCHECK
*&---------------------------------------------------------------------*
*&      Form  DATES_CHECK_FOR_BBP
*&---------------------------------------------------------------------*
FORM dates_check_for_bbp
                     CHANGING xy_obj TYPE isu2a_billing_data.

  IF xy_obj-abs-begabspe > xy_obj-abs-endabspe.
*   Geben Sie als Hochrechnungsdatum ein Datum ab dem &1 an
    mac_err '089' 'AJ' xy_obj-abs-begabspe xy_obj-abs-endabspe
                       space space general_fault.
  ENDIF.

ENDFORM.                               " DATES_CHECK_FOR_BBP
*&---------------------------------------------------------------------*
*&      Form  DATES_CHECK_FOR_SIM
*&---------------------------------------------------------------------*
FORM dates_check_for_sim USING x_obj TYPE isu2a_billing_data.

  IF x_obj-abr-begabrpe > x_obj-abr-endabrpe.
*   Geben Sie einen gültigen Simulationszeitraum an
    mac_err '089' 'AJ' x_obj-abr-begabrpe x_obj-abr-endabrpe
                       space space general_fault.
  ENDIF.

ENDFORM.                               " DATES_CHECK_FOR_SIM
*&---------------------------------------------------------------------*
*&      Form  DEMAND_PROLONG
*&---------------------------------------------------------------------*
*      -->xy_obj                                                       *
*----------------------------------------------------------------------*
FORM demand_prolong USING xy_obj TYPE isu2a_billing_data.
  mac_break1 'DEMPROLONG'.                                 "#EC NOBREAK
  CALL FUNCTION 'ISU_DEMAND_PROLONG'
    EXPORTING
      x_izw         = xy_obj-izw
      x_izwst       = xy_obj-izwst
    CHANGING
      xy_ivb        = xy_obj-ivb
    EXCEPTIONS
      general_fault = 1
      OTHERS        = 2.
  IF sy-subrc = 2.
    mac_msg_putx_wp co_msg_programming_error 898 'E9' 'sy-subrc'
                    sy-subrc 'ISU_DEMAND_PROLONG' space space.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE a898(e9). ENDIF.
    SET EXTENDED CHECK ON.
  ELSEIF sy-subrc <> 0.
    mac_msg_repeat co_msg_error space.
  ENDIF.
ENDFORM.                               " DEMAND_PROLONG
*&---------------------------------------------------------------------*
*&      Form  PERIODIC_BILLING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM periodic_billing_data USING    x_te420 LIKE te420
                                    x_sobj  TYPE isu2a_data_collector
                           CHANGING xy_obj  TYPE isu2a_billing_data.

  DATA wte417       LIKE te417.
  DATA wte418       LIKE te418.
  DATA weanl        LIKE v_eanl.
  DATA datum        LIKE te417-termtdat.
  DATA lv_ableinh   TYPE ableinh.
  DATA wate422      LIKE te422.
  DATA wate420      LIKE te420.
  DATA wate418      LIKE te418.
  DATA s_te418      LIKE te418.
  DATA s_subrc      LIKE sy-subrc.               "note 1108022

* Sollablesedatum der nächsten Periode bestimmen
  mac_msg_log_off.                               "note 1108022
  CALL FUNCTION 'ISU_S_BIPR_MRR_DETERMINE'
       EXPORTING
            x_portion     = xy_obj-trig-etrg-portion
            x_adatsoll    = xy_obj-trig-etrg-adatsoll
            x_mrunit      = xy_obj-trig-etrg-ableinh
            x_abrvorg     = xy_obj-trig-etrg-abrvorg
*            x_portion     = xy_obj-schedule-portion
*            x_adatsoll    = xy_obj-schedule-adatsoll
*            x_mrunit      = xy_obj-schedule-ableinh
       IMPORTING
            y_act_te417   = wte417
            y_next_te418  = wte418
       EXCEPTIONS
            not_found     = 1
            not_qualified = 2
            system_error  = 3
            OTHERS        = 4.

  s_subrc = sy-subrc.                                "note 1108022
  mac_msg_log_on.                                    "note 1108022
  IF s_subrc                     = 1         AND     "note 1108022
     xy_obj-st-abrvorg           = co_zwiabr AND
     xy_obj-eap_etta-biadvance   = 'X'       AND
     NOT xy_obj-eap_etta-dypercon IS INITIAL.
    PERFORM periodic_bdata_intbill USING    x_te420
                                   CHANGING xy_obj
                                            wte417
                                            wte418.
    xy_obj-schedule-next_adatsoll = wte418-adatsoll. "note 887549
    EXIT.                                            "note 887549
  ELSEIF s_subrc <> 0.                               "note 1108022
    mac_msg_last_repeat co_msg_error general_fault.
  ENDIF.

* Sollablesedatum der 'nächsten' Periode merken
  xy_obj-schedule-next_adatsoll = wte418-adatsoll.

* Prüfen, ob sich die Portion zum PTERMTDAT + 1 geändert hat
  IF x_sobj-ever-portion IS INITIAL.
    datum = wte417-termtdat + 1.
*   Ableseeinheit aus Anlage entnehmen
    LOOP AT x_sobj-inst-ieanl INTO weanl
      WHERE ab  <= datum
      AND   bis >= datum.
      EXIT.
    ENDLOOP.
    mac_err_read 'IEANL' space 'PERIODIC_BILLING_DATA'.
    lv_ableinh = weanl-ableinh.
    IF lv_ableinh <> xy_obj-trig-etrg-ableinh.
*   Ableseeinheit hat sich geändert. Ist die Portion die
*   gleiche geblieben?
      PERFORM mr_unit_select USING    lv_ableinh
                             CHANGING wate422.
      IF wate422-portion <> xy_obj-trig-etrg-portion.
*       Daten zur Portion ermitteln
        PERFORM portion_select USING    wate422-portion
                               CHANGING wate420.
*       Wie bei Zwischenabrechnung den vorherigen Turnustermin
*       bestimmen
*       HS, 12.1.05: Abweichend zum Vorgehen bei Zwischenabrechnung
*       wird nicht mit dem ADATSOLL nach dem Ableseeinheitensatz
*       gesucht, sondern mit TERMTDAT+1, da ansonsten beim Wechsel
*       zwischen zwei Abeleseeinheiten mit identischen Terminen
*       die falschen Sätze gelesen werden.
        CALL FUNCTION 'ISU_DB_TE418_SELECT_DAT_PV'
          EXPORTING
            x_termschl = lv_ableinh
*            x_datum    = xy_obj-trig-etrg-adatsoll
            x_datum    = datum
            x_ablesgr  = co_gr_turnus
          IMPORTING
            y_te418    = wate418
          EXCEPTIONS
            not_found  = 1
            OTHERS     = 4.
        IF sy-subrc > 1.                          "note 982412
          mac_err_repeat general_fault.
*       >>>>> note982412
        ELSEIF sy-subrc = 1.
*         Offenbar hat die neue Ableseeinheit keinen Vorgänger-
*         terminsatz. In diesem Fall den nächsten Termin über
*         den folgenden Baustein ermitteln.
          CALL FUNCTION 'ISU_DB_TE418_SELECT_DAT_NX'
            EXPORTING
              x_termschl = lv_ableinh
              x_datum    = datum
            IMPORTING
              y_te418    = s_te418
            EXCEPTIONS
              not_found  = 1
              OTHERS     = 4.
          IF sy-subrc <> 0.
            mac_err_repeat general_fault.
          ENDIF.
        ELSEIF sy-subrc = 0.
*       ENDIF.
*       <<<<< note 982412
*         Terminsatz der nächsten Periode aus der neuen
*         Portion bestimmen
          CALL FUNCTION 'ISU_S_BIPR_MRR_DETERMINE'
            EXPORTING
              x_portion     = wate422-portion
              x_adatsoll    = wate418-adatsoll
              x_mrunit      = lv_ableinh
              x_abrvorg     = xy_obj-trig-etrg-abrvorg
            IMPORTING
              y_next_te418  = s_te418
            EXCEPTIONS
              not_found     = 1
              not_qualified = 2
              system_error  = 3
              OTHERS        = 4.
          IF sy-subrc <> 0.
            mac_err_repeat general_fault.
          ENDIF.
        ENDIF.
*       Abschlagszyklus aus neuer Portion übernehmen
        xy_obj-schedule-abszyk = wate420-abszyk.
        xy_obj-schedule-next_adatsoll = s_te418-adatsoll.
      ENDIF.      "wate422-portion <> xy_obj-trig-etrg-portion
    ENDIF.      "lv_ableinh <> xy_obj-trig-etrg-ableinh.
  ENDIF.      "EVER-PORTION

* Termtdat zur aktuellen Periode der Portion merken
  xy_obj-schedule-ptermtdat     = wte417-termtdat.
  xy_obj-schedule-zuorddat      = wte417-zuorddat.
  xy_obj-schedule-thgdat        = wte417-thgdat.
* letzte Periode eines Nachberechnungszeitraums?
  IF xy_obj-eap_etta-backbi     = co_backbi_floating.
*   nur relevant für gleitende Nachberechnung
    CALL FUNCTION 'ISU_END_OF_BACKBI_PERIOD_CHECK'
      EXPORTING
        x_end_of_first_period = x_te420-termerst
        x_end_of_period       = wte417-termtdat
        x_period_length       = x_te420-periodew
        x_period_length_d     = x_te420-perioded
        x_period_type         = x_te420-periodet
        x_number_of_periods   = xy_obj-eap_etta-numperbb
      IMPORTING
        y_bbp_end             = xy_obj-fl-endofbb
      EXCEPTIONS
        parameter_fault       = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.
  ENDIF.
* letzte Periode eines Endabrechnungszeitraums?
  IF xy_obj-eap_etta-perendbi = co_pebi_integrated OR
     xy_obj-eap_etta-perendbi = co_pebi_separate.
*   nur relevant, wenn Endabrechnung aktiv
    CALL FUNCTION 'ISU_END_OF_BACKBI_PERIOD_CHECK'
      EXPORTING
        x_end_of_first_period = x_te420-termerst
        x_end_of_period       = wte417-termtdat
        x_period_length       = x_te420-periodew
        x_period_length_d     = x_te420-perioded
        x_period_type         = x_te420-periodet
        x_number_of_periods   = xy_obj-eap_etta-numperpeb
      IMPORTING
        y_bbp_end             = xy_obj-fl-endofpeb
      EXCEPTIONS
        parameter_fault       = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.
  ENDIF.
* Prüfen, ob die Turnusabrechnung gemäß Terminsteuerung
* durchgeführt werden (nur für Nach- und Endabrechnung)
  PERFORM periodic_bill_order_check USING xy_obj.

ENDFORM.                               " PERIODIC_BILLING_DATA
*&---------------------------------------------------------------------*
*&      Form  MR_UNIT_AND_PORTION_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mr_unit_and_portion_check
                         USING    x_sobj TYPE isu2a_data_collector
                         CHANGING x_obj TYPE isu2a_billing_data.

  DATA change_ok TYPE kennzx.

* Es wird geprüft, ob sich die Portion oder Ableseeinheit innerhalb
* einer Nach- oder Endabrechnungsperiode ändert. Diese Änderung
* wird nicht erlaubt.
* Man beachte folgende Zusammenhänge:
* Innerhalb einer Nach- oder Endabrechnungsperiode sollte sicher
* gestellt werden, daß alle durch die Terminsteuerung induzierten
* Turnusabrechnungen durchgeführt werden. Dies wird in einer
* weiteren Prüfung sichergestellt.
* Eine wichtige Vorraussetzung für diese Prüfung ist, daß sich
* die Portion (bzw. Ableseeinheit) innerhalb der Nach- bzw.
* Endabrechnungsperiode nicht ändert.
  IF NOT x_obj-pbill IS INITIAL.
*   Vorbeleg vorhanden
    IF x_obj-pbill-erch-backbi <> co_backbi_no.
*     Nachberechnung ist aktiv
      IF x_obj-pbill-erch-endofbb IS INITIAL.
*       Die Nachberechnungsperiode ist noch nicht abgeschlossen
        IF x_obj-trig-etrg-portion <> x_obj-pbill-erch-portion.
          PERFORM check_portion_change USING    x_obj-trig-etrg-portion
                                                x_obj-pbill-erch-portion
                                       CHANGING change_ok.
          IF change_ok IS INITIAL.
*           Die Portion hat sich innerhalb der Nachberechnungsperiode
            mac_err '103' 'AJ' x_obj-pbill-erch-portion
                               x_obj-trig-etrg-portion
                               x_obj-pbill-erch-begnach
                               space
                               general_fault.
          ENDIF.
        ENDIF.
        IF x_obj-trig-etrg-ableinh <> x_obj-pbill-erch-ableinh.
*         Bei abweichender Portion im Vertrag ist Wechsel in ABLEINH
*         fuer Abrechnung nicht relevant.
          CHECK x_sobj-ever-portion IS INITIAL.
          PERFORM check_mr_unit_change USING    x_obj-trig-etrg-ableinh
                                                x_obj-pbill-erch-ableinh
                                       CHANGING change_ok.
          IF change_ok IS INITIAL.
*           Die Ableseeinheit hat sich innerhalb der Nachberechnungsper.
            mac_err '104' 'AJ' x_obj-pbill-erch-ableinh
                               x_obj-trig-etrg-ableinh
                               x_obj-pbill-erch-begnach
                               space
                               general_fault.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF x_obj-pbill-erch-perendbi <> co_pebi_no.
*     Endabrechnung ist aktiv
      IF x_obj-pbill-erch-endofpeb IS INITIAL.
*       Die Endabrechnungsperiode ist noch nicht abgeschlossen
        IF x_obj-trig-etrg-portion <> x_obj-pbill-erch-portion.
          PERFORM check_portion_change USING    x_obj-trig-etrg-portion
                                                x_obj-pbill-erch-portion
                                       CHANGING change_ok.
          IF change_ok IS INITIAL.
*           Die Portion hat sich innerhalb der Endabrechnungsperiode
            mac_err '105' 'AJ' x_obj-pbill-erch-portion
                               x_obj-trig-etrg-portion
                               x_obj-pbill-erch-begend
                               space
                               general_fault.
          ENDIF.
        ENDIF.
        IF x_obj-trig-etrg-ableinh <> x_obj-pbill-erch-ableinh.
*         Bei abweichender Portion im Vertrag ist Wechsel in ABLEINH
*         fuer Abrechnung nicht relevant.
          CHECK x_sobj-ever-portion IS INITIAL.
          PERFORM check_mr_unit_change USING    x_obj-trig-etrg-ableinh
                                                x_obj-pbill-erch-ableinh
                                       CHANGING change_ok.
          IF change_ok IS INITIAL.
*           Die Ableseeinheit hat sich innerhalb der Endabrechnungsper.
            mac_err '106' 'AJ' x_obj-pbill-erch-ableinh
                               x_obj-trig-etrg-ableinh
                               x_obj-pbill-erch-begend
                               space
                               general_fault.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " MR_UNIT_AND_PORTION_CHECK
*&---------------------------------------------------------------------*
*&      Form  PERIODIC_BILL_ORDER_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM periodic_bill_order_check USING x_obj TYPE isu2a_billing_data.

  DATA datum  LIKE erch-ptermtdat.
  DATA wte417 LIKE te417.

* Durch die nachfolgende Prüfung soll gewährleistet werden, daß
* innerhalb einer Nach- bzw. Endabrechnungsperiode alle
* Turnusabrechnungen durchgeführt werden, die durch die
* Terminsteuerung (Terminsätze der Portion) vorgegeben werden.
* Man beachte hierbei folgende Vorraussetzungen:
* - Innerhalb eines Nach- bzw. Endabrechnungszeitraums werden
*   Zwischenabrechnungen unterbunden (d.h. es gibt nur
*   Turnusabrechnungen)
* - Innerhalb einer Nach- bzw. Endabrechnungsperiode darf sich die
*   Zuordnung zu einer Portion bzw. Ableseeinheit nicht ändern.
  IF NOT x_obj-pbill IS INITIAL.
*   Vorbeleg vorhanden
    IF ( x_obj-pbill-erch-backbi   <> co_backbi_no AND
         x_obj-pbill-erch-endofbb  IS INITIAL          ) OR
       ( x_obj-pbill-erch-perendbi <> co_pebi_no   AND
         x_obj-pbill-erch-endofpeb IS INITIAL          ).
*     Nach- oder Endabrechnungsperiode ist noch nicht abgeschlossen
*     --> ausgehend von dem Terminsatz, mit dem die letzte
*     Turnusabrechnung erzeugt wurde, wird der nachfolgende
*     Terminsatz zu der Portion bestimmt.
      datum = x_obj-pbill-erch-ptermtdat + 1.
      CALL FUNCTION 'ISU_DB_TE417_SELECT_PERIOD'
        EXPORTING
          x_termschl = x_obj-pbill-erch-portion
          x_datum    = datum
          x_abrvorg  = co_turabr
        IMPORTING
          y_te417    = wte417
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0.
        mac_err_repeat general_fault.
      ENDIF.
*     Der so ermittelte Terminsatz muß dem entsprechen, der
*     zu dem aktuellen Turnus-Trigger ermittelt wurde.
      IF wte417-termtdat <> x_obj-schedule-ptermtdat.
*       Die Turnusabrechnung kann noch nicht durchgeführt werden
        mac_err '107' 'AJ' x_obj-trig-etrg-abrdats
                           x_obj-schedule-portion
                           x_obj-schedule-ptermtdat
                           wte417-termtdat
                           general_fault.
        IF 1 = 2.
          MESSAGE e107(aj) WITH space space space space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " PERIODIC_BILL_ORDER_CHECK
*&---------------------------------------------------------------------*
*&      Form  BILLING_ACTIVITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM billing_activity_check
                      CHANGING x_obj TYPE isu2a_billing_data.

  IF NOT x_obj-pbill IS INITIAL.
*   Vorbeleg vorhanden
    IF x_obj-pbill-erch-backbi   <> co_backbi_no AND
       x_obj-pbill-erch-endofbb  IS INITIAL.
*     Nachberechnungsperiode ist noch nicht abgeschlossen
      IF x_obj-st-abrvorg = co_zwiabr.
*       Zwischenabrechnung ist nicht erlaubt
        mac_err '108' 'AJ' x_obj-pbill-erch-begnach space space space
                           general_fault.
      ENDIF.
    ENDIF.
*   XXO ZWEA
*    IF X_OBJ-PBILL-ERCH-PERENDBI <> CO_PEBI_NO AND
*       X_OBJ-PBILL-ERCH-ENDOFPEB IS INITIAL.
**     Endabrechnungsperiode ist noch nicht abgeschlossen
*      IF X_OBJ-ST-ABRVORG = CO_ZWIABR.
**       Zwischenabrechnung ist nicht erlaubt
*        MAC_ERR '109' 'AJ' X_OBJ-PBILL-ERCH-BEGEND SPACE SPACE SPACE
*                           GENERAL_FAULT.
*      ENDIF.
*    ENDIF.
*   XXO ZWEA
  ENDIF.
  PERFORM period_end_bill_check USING x_obj.

ENDFORM.                               " BILLING_ACTIVITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  PERIOD_END_BILL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM period_end_bill_check USING x_obj TYPE isu2a_billing_data.

  DATA ietrg    LIKE etrg OCCURS 0.
  DATA wetrg    LIKE etrg.
  DATA subrc    LIKE sy-subrc.

  IF x_obj-st-abrvorg <> co_endabr.
    IF NOT x_obj-pbill IS INITIAL.
*     Vorbeleg vorhanden
      IF NOT x_obj-pbill-erch-endofpeb IS INITIAL.
*       Der Vorbeleg ist der letzte einer Endabrechnungsperiode.
*       --> Prüfen, ob die Endabrechnung zu der Endabrechnungsperiode
*       des Vorbelegs schon durchgeführt wurde
        CALL FUNCTION 'MSG_ACTION'
          EXPORTING
            x_action = co_msg_log_off.
        CALL FUNCTION 'ISU_DB_ERCH_SINGLE_PERENDBI'
          EXPORTING
            x_vertrag = x_obj-st-vertrag
            x_begend  = x_obj-pbill-erch-begend
          EXCEPTIONS
            not_found = 1.
        subrc = sy-subrc.
        CALL FUNCTION 'MSG_ACTION'
          EXPORTING
            x_action = co_msg_log_on.
        IF subrc <> 0.
*         Die Endabrechnung wurde noch nicht durchgeführt
*         --> Den Endabrechnungstrigger lesen
          CALL FUNCTION 'MSG_ACTION'
            EXPORTING
              x_action = co_msg_log_off.
          CALL FUNCTION 'ISU_DB_ETRG_SELECT_INSTALL'
            EXPORTING
              x_anlage  = x_obj-st-anlage
              x_abrvorg = co_endabr
            TABLES
              ty_etrg   = ietrg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          subrc = sy-subrc.
          CALL FUNCTION 'MSG_ACTION'
            EXPORTING
              x_action = co_msg_log_on.
          IF subrc > 1.
*           Fehlermeldung ausgeben
            mac_msg_last_repeat co_msg_error general_fault.
          ENDIF.
*         Priorität des Endabrechnungstriggers prüfen
          READ TABLE ietrg INTO wetrg
               WITH KEY begend = x_obj-pbill-erch-begend.
          IF sy-subrc = 0.
            IF NOT wetrg-endprio IS INITIAL.
*             Führen Sie zuerst die Endabrechnung aus
              mac_err '110' 'AJ' x_obj-abr-begabrpe
                                 x_obj-pbill-erch-begend
                                 x_obj-pbill-erch-endabrpe
                                 wetrg-abrdats
                                 general_fault.
            ENDIF.
          ELSEIF x_obj-pbill-erch-perendbi <> co_pebi_integrated.
*           Es fehlt ein Endabrechnungsauftrag (nur wenn keine
*           integrierte Endabrechnung durchgeführt wurde)
            mac_err '111' 'AJ' x_obj-abr-begabrpe
                               x_obj-pbill-erch-begend
                               x_obj-pbill-erch-endabrpe
                               space
                               general_fault.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " PERIOD_END_BILL_CHECK
*&---------------------------------------------------------------------*
*&      Form  IRATECAT_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM iratecat_fill USING    x_obj     TYPE isu2a_billing_data
                            x_sobj    TYPE isu2a_data_collector
                   CHANGING y_iratecat TYPE isu20_facts-iratecat.

  DATA date LIKE erch-endabrpe.

*  IF NOT x_obj-abs-bbp_sim IS INITIAL.
*    date = x_obj-abs-endabspe.
*  ELSEIF NOT x_obj-vor-vorbi IS INITIAL.
*    date = x_obj-vor-endvorpe.
*  ELSE.
*    date = x_obj-abr-endabrpe.
*  ENDIF.
* Der Hochrechnungszeitraum für ein Zahlungsschema kann deutlich vom
* Abrechnungszeitraum abweichen. Insbesondere kann ENDABSPE vor
* ENDABRPE liegen. Darum wird hier das größte der drei Daten genommen.
  date = x_obj-abr-endabrpe.
  IF NOT x_obj-abs-bbp_sim IS INITIAL
    AND x_obj-abs-endabspe GT date.
    date = x_obj-abs-endabspe.
  ENDIF.
  IF NOT x_obj-vor-vorbi IS INITIAL
    AND x_obj-vor-endvorpe GT date.
    date = x_obj-vor-endvorpe.
  ENDIF.

  CALL FUNCTION 'ISU_RATECAT_DATA_FOR_INSTLN'
    EXPORTING
      x_anlage       = x_obj-st-anlage
      x_ab           = x_obj-date-begana
      x_bis          = date
      x_use_t_v_eanl = x_sobj-inst-use_ieanl
    IMPORTING
      y_iratecat     = y_iratecat
    TABLES
      xyt_v_eanl     = x_sobj-inst-ieanl
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    mac_err_repeat general_fault.
  ENDIF.
* Zulässigkeit von Tariftypwechsel prüfen
  PERFORM rate_cat_change_check USING x_obj
                                      y_iratecat.

ENDFORM.                               " IRATECAT_FILL
*&---------------------------------------------------------------------*
*&      Form  RATE_CAT_CHANGE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rate_cat_change_check USING x_obj      TYPE isu2a_billing_data
                                 x_iratecat TYPE isu20_facts-iratecat.

  DATA wratecat TYPE isu20_ratecat_list.

* Prüfen, ob ein Tariftypwechsel vorliegt
*>>>Die Prüfung muß noch angepaßt werden, wenn gleichzeitig mit
*>>>gleitender Nachberechnung oder Endabrechnung auch eine
*>>>Abschlagshochrechnung erfolgen kann. Dann darf sich u.U.
*>>>zum Beginn des Abschlagszeitraums der Tariftyp wechsel.
*>>>Genau dann, wenn mit dem Abschlagszeitraum eine neue Nach-
*>>>oder Endabrechnungsperiode beginnt.
  LOOP AT x_iratecat INTO wratecat
    WHERE tariftyp <> x_obj-eap_etta-tariftyp.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    IF x_obj-st-abrvorg = co_endabr.
*     Ein Tariftypwechsel in der Endabrechnungsperiode ist nicht erlaubt
      mac_err '113' 'AJ' x_obj-abr-begabrpe x_obj-abr-endabrpe
                         wratecat-tariftyp  x_obj-eap_etta-tariftyp
                         general_fault.
    ELSE.
      IF x_obj-eap_etta-backbi <> co_backbi_no.
*       Ein Tariftypwechsel in der Nachberechnungsperiode ist nicht erl
        mac_err '114' 'AJ' x_obj-date-begperiod x_obj-abr-endabrpe
                           wratecat-tariftyp    x_obj-eap_etta-tariftyp
                           general_fault.

      ENDIF.
      IF x_obj-eap_etta-perendbi <> co_pebi_no.
*       Ein Tariftypwechsel in der Endabrechnungsperiode ist nicht erlau
        mac_err '115' 'AJ' x_obj-abr-begabrpe x_obj-abr-endabrpe
                           wratecat-tariftyp   x_obj-eap_etta-tariftyp
                           general_fault.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " RATE_CAT_CHANGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  ISAIS_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM isais_create USING xy_obj  TYPE isu2a_billing_data
                        xy_sobj TYPE isu2a_data_collector.

  DATA iseason   TYPE isu20_iseason.
  DATA wseason   TYPE isu20_season.
  DATA isaison   TYPE isu2a_isaison.
  DATA wsaison   TYPE isu2a_saison.
  DATA wsaison2  TYPE isu2a_saison.
  DATA wsais     TYPE isu2a_sais.
  DATA s_ab      LIKE wsaison-ab.

  READ TABLE xy_sobj-isaison INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
*   Es sind Saisonzeiträume vorhanden
    LOOP AT xy_sobj-isaison INTO wsaison.
      READ TABLE iseason INTO wseason
           WITH KEY saison = wsaison-saison.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING wsaison TO wseason.
        APPEND wseason TO iseason.
      ELSEIF wseason-beginn <> wsaison-beginn OR
             wseason-ende   <> wsaison-ende.
*       Die Saisondefinitionen zur Saison sind unterschiedlich -->
*       Tariftypen mit unterschiedlicher Saisondefinition ermitteln
        LOOP AT xy_sobj-isaison INTO wsaison2
          WHERE   saison   =  wsaison-saison
          AND     tariftyp <> wsaison-tariftyp
          AND   ( ab <> wsaison-ab OR bis <> wsaison-bis ).
*         Die Saisondefinition für die Saison &1 ist nicht eindeutig
          mac_err '126' 'AJ' wsaison-saison    wsaison-tariftyp
                             wsaison2-tariftyp xy_obj-st-anlage
                             general_fault.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*   Prüfen, ob die Saisondefinitionen über den gesamten Zeitraum
*   hinweg konsistent sind. Je Tariftyp(-zeitraum) wurde die
*   Konsistenz bereits geprüft. Eine Inkonsistenz kann sich somit
*   nur aus einem Tariftypwechsel und damit aus einem Wechsel der
*   Saisondefinitionen ergeben.
*   HS, 17.1.2003:
*   Der folgende Check auf Saisons wird mit der Änderung im Aufbau
*   der SAISON in der Tarifanalyse überflüssig.
*    CALL FUNCTION 'ISU_SEASONS_CHECK'
*      EXPORTING
*        X_ISEASON      = ISEASON
*      EXCEPTIONS
*        NOT_CONSISTENT = 1
*        OTHERS         = 2.
*    IF SY-SUBRC <> 0.
**>>>>>Die Fehlerausgabe sollte noch verbessert werden. Es fehlen
**>>>>>detailierte Informationen zu den inkonsistenten Definitionen.
*      MAC_ERR_REPEAT GENERAL_FAULT.
*    ENDIF.
*   Aus der ISAISON wird hier die ISAIS aufgebaut (die ISAIS ist
*   eine Verdichtung der ISAISON) über den gesamten
*   Betrachtungszeitraum -- also über alle IABRZEIT-Zeiträume
*   hinweg. Die ISAISON ist nach Abgrenzungen der IABRZEIT
*   und Tariftypwechseln abgegrenzt.
*   Man beachte, daß die Tabelle ISAISON immer auf den gesamten
*   Saisonzeitraum ausgedehnt wurde (auch wenn der entsprechende
*   Tariftyp nicht für den gesamten Zeitraum zur Anwendung kommt).
*
*   HS, 12.3.2003 - die Korrekturen in den Hinweisen 576980 und
*   588094 haben dazu geführt, dass die ISAISON nicht mehr so wie
*   vorher aufgebaut ist. Daher muss auch hier der Aufbau der ISAIS
*   umgebaut werden.
*    LOOP AT ISEASON INTO WSEASON.
**     je Saisonbezeichnung
*      LOOP AT XY_SOBJ-ISAISON INTO WSAISON
*        WHERE SAISON = WSEASON-SAISON.
**       Prüfen, ob zu der Saison der Saisonzeitraum schon
**       hinzugefügt wurde
*        READ TABLE XY_OBJ-ISAIS TRANSPORTING NO FIELDS
*          WITH KEY SAISON = WSAISON-SAISON
*                   AB     = WSAISON-AB
*                   BIS    = WSAISON-BIS.
*        IF SY-SUBRC <> 0.
**         Saisonzeitraum wurde noch nicht hinzugefügt
*          WSAIS-SAISON = WSAISON-SAISON.
*          WSAIS-AB     = WSAISON-AB.
*          WSAIS-BIS    = WSAISON-BIS.
*          APPEND WSAIS TO XY_OBJ-ISAIS.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
    isaison = xy_sobj-isaison.
    SORT isaison BY ab.
    LOOP AT isaison INTO wsaison.
      s_ab = wsaison-ab - 1.
      LOOP AT xy_obj-isais
        INTO wsais
        WHERE saison = wsaison-saison
        AND   ab     <= wsaison-bis
        AND   bis    >= s_ab.
        IF wsais-bis <= wsaison-bis.
          wsais-bis = wsaison-bis.
          MODIFY xy_obj-isais FROM wsais.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING wsaison TO wsais.
        APPEND wsais TO xy_obj-isais.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " ISAIS_CREATE
*&---------------------------------------------------------------------*
*&      Form  MRRESULT_FOR_BACK_BILLING
*&---------------------------------------------------------------------*
FORM mrresult_for_back_billing
            CHANGING  xy_sobj        TYPE isu2a_data_collector
                      xy_obj         TYPE isu2a_billing_data
                      xy_irednach    TYPE isu2a_inach
                      xy_bbp_results LIKE regen-kennzx.

  DATA  wnach           TYPE   isu2a_nach.
  DATA  wa_izw          TYPE   isu2a_zw.
  DATA  izw_back        LIKE   wa_izw OCCURS 0.
  DATA  izwst_back      TYPE   isu2a_billing_data-izwst.
  DATA  wa_izwst        TYPE   isu2a_zwst.
  DATA  izw_1           TYPE   isu2a_billing_data-izw.
  DATA  idnach          TYPE   isu2a_inach.
  DATA  vnach           TYPE   isu2a_nach.
  DATA  idtfill         LIKE   sy-tfill.
  DATA  wabrzeit        TYPE   isu2a_abrzeit.
  DATA  date_begperiod  LIKE   erch-begabrpe.
  DATA  f_de_c          TYPE   regen-kennzx.
  DATA  f_dn_c          TYPE   regen-kennzx.
  DATA  f_de            TYPE   regen-kennzx.
  DATA  f_dn            TYPE   regen-kennzx.
  DATA  wperiod         TYPE   isu2c_period.
  DATA  copy_izwst      TYPE   isu2a_izwst.
  DATA  copy_izwst_dpc  TYPE   isu2a_izwst.
  DATA  wactperiod      TYPE   isu2c_actperiod.
  DATA  tfill           TYPE   sy-tfill.


  mac_break1 'BACKEABL'.                                   "#EC NOBREAK


* Prüfen, ob die Ableseergebnisse der Korrekturperioden benötigt
* werden (also, ob die IZWST aufgebaut werden muß)


  IF NOT xy_obj-fl-estb_mode IS INITIAL.
    IF xy_obj-st-spartyp <> co_spartyp_gas.               "note 935652
*   Falls DPC-ESTB vorliegt, KEINE besondere Nacharbeiten durchführen
      CLEAR: xy_bbp_results.
    ELSE.
**     Gas: Dummy IZWST für Gasdatum aufbauen
      DESCRIBE TABLE xy_obj-bbp_bills LINES idtfill.
      IF idtfill <> 0.
        APPEND LINES OF xy_obj-inach FROM 1 TO idtfill TO idnach.
      ELSE.
*     Übernahme der gewöhnlichen INACH in die reduzierte IDNACH
        idnach[] = xy_obj-inach[].
      ENDIF.
*     Tabelle IDNACH für Gasabrechnung weitergeben
      xy_irednach[] = idnach[].
    ENDIF.
    EXIT.
  ENDIF.

  IF xy_obj-eap_etta-dypercon IS INITIAL.
    PERFORM backbi_results_necessary USING    xy_obj
                                     CHANGING xy_bbp_results.
  ELSE.
***   Aufzubauende Periode suchen, die kleineres Abdatum als der
***   aktuell abzurechnende Abrechnungszeitraum hat.
***   Nur dann müssen weitere Ableseergebnisse aufgebaut werden.
***   Alternativ ist "XY_OBJ-DATE-BEGPERIOD > XY_OBJ-DATE-BEGANA"
***   mögliches Kriterium.
**    LOOP AT XY_OBJ-IPERIOD INTO WPERIOD
**      WHERE PBASCAT <> CO_ACT_BILL_PER
**      AND   AB      <  XY_OBJ-DATE-BEGPERIOD.
**      XY_BBP_RESULTS = 'X'.
**      EXIT.
**    ENDLOOP.
    READ TABLE xy_obj-iactperiod INDEX 1 INTO wactperiod.
    IF wactperiod-actperiod = co_3000 OR
       wactperiod-actperiod = co_4000.
      xy_bbp_results = 'X'.
    ENDIF.
  ENDIF.
  CHECK NOT xy_bbp_results IS INITIAL.

  IF     xy_obj-trig-etrg-abrvorg =  co_endabr OR
     NOT xy_obj-eap_etta-dypercon IS INITIAL.

*   Bei Endabrechnung mit Nachberechnung kann es sein, dass die
*   Zählwerkstabelle xy_obj-izwstbf[] noch nicht gefüllt ist.
*   Dies muss hier nachgeholt werden.

    IF NOT xy_obj-eap_etta-dypercon IS INITIAL.
*     Bei dynamischer Periodensteuerung muss aus technischen Gründen
*     XY_OBJ-DATE-BEGPERIOD für Aufruf 'ISU_IZW_ADJUST'
*     temporär umgegeschossen werden
      date_begperiod = xy_obj-date-begperiod.
      xy_obj-date-begperiod = xy_obj-date-begana.
      CLEAR: xy_sobj-max_ieabl.
      CLEAR: xy_sobj-max_ieablg.
    ENDIF.

    CALL FUNCTION 'ISU_IZW_ADJUST'
      EXPORTING
        x_obj          = xy_obj
      CHANGING
        xy_izw_abr_inf = izw_1
      EXCEPTIONS
        general_fault  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ELSE.
      IF NOT xy_obj-eap_etta-dypercon IS INITIAL.
*       Originalwert XY_OBJ-DATE-BEGPERIOD zurückspeichern
        xy_obj-date-begperiod = date_begperiod.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
      EXPORTING
        x_izw         = xy_obj-izw
        x_iablterm    = xy_obj-iablterm
        x_abr         = xy_obj-abr
        x_inst        = xy_sobj-inst
        x_vertrag     = xy_obj-st-vertrag
        x_eabl_kz     = '2'
        x_izw_1       = izw_1
        x_begana      = xy_obj-date-begana
        x_einzdat     = xy_sobj-ever-einzdat
        x_max_ieabl   = xy_sobj-max_ieabl
        x_max_ieablg  = xy_sobj-max_ieablg
      IMPORTING
        y_izwstbf     = xy_obj-izwstbf
        y_ieabl       = xy_obj-ieabl
        y_ieablg      = xy_obj-ieablg
      EXCEPTIONS
        general_fault = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.

  ENDIF.                               " xy_obj-trig-etrg-abrvorg


  IF NOT xy_obj-eap_etta-dypercon IS INITIAL.
*   Bei dynamischer Periodensteuerung IZWST zunächst nur für elementare
*   Zeiträume, i.e. die Korrekturperioden im Sinne der Nachberechnung,
*   aufbauen. Aus diesen können dann alle anderen benötigten Zeiträume
*   zusammengesetzt werden.
*   Diese elementaren Zeiträume werden gerade durch die Vorbelege
*   repräsentiert.
*   Aufbau einer reduzierten IDNACH anstelle der gewöhnlichen INACH
    DESCRIBE TABLE xy_obj-bbp_bills LINES idtfill.
    IF idtfill <> 0.
      APPEND LINES OF xy_obj-inach FROM 1 TO idtfill TO idnach.
*     Für späteres weiteres Aufsetzen in Tabelle IDNACH den
*     Wert von IDTFILL um 1 erhöhen.
      ADD 1 TO idtfill.
    ENDIF.
  ELSE.
*   Übernahme der gewöhnlichen INACH in die reduzierte IDNACH
    idnach[] = xy_obj-inach[].
  ENDIF.
* Tabelle IDNACH für Gasabrechnung weitergeben
  xy_irednach[] = idnach[].

* ----------------------------------------------------------------
*
* Es wird erwartet, dass nur die echten Korrekturperioden
* in INACH stehen, d.h. bei der ersten Turnusrechnung der
* grossen Nachberechnungsperiode sollte INACH leer sein,
* bei insgesamt n Abrechnungen sollten bei der letzten,
* also n-ten Turnusrechnung genau n-1 Korrekturperioden
* in INACH stehen.
  LOOP AT idnach INTO wnach.
*   -----------------------------------------------------
*   Zu jeder Korrekturperiode lokale IZW erstellen
    REFRESH: izw_back.
    CALL FUNCTION 'ISU_IZW_ADJUST'
      EXPORTING
        x_obj         = xy_obj
        x_backno      = wnach-backno
      CHANGING
        xy_izw_nach   = izw_back
      EXCEPTIONS
        general_fault = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.

    DESCRIBE TABLE izw_back LINES tfill.
*   IZWST für den Nachberechnungszeitraum nur aufbauen, wenn
*   IZW_BACK gefüllt.
    IF tfill > 0.

*     ----------------------------------------------------------------
*     ALle Ableseergebnisse aus Puffer zuweisen
      izwst_back[] = xy_obj-izwstbf[].
*     ----------------------------------------------------------------
      CALL FUNCTION 'ISU_BACKBI_IZWST_GET'
        EXPORTING
          x_sobj        = xy_sobj
          x_izw_back    = izw_back
          x_backno      = wnach-backno
          x_obj         = xy_obj
        CHANGING
          xy_izwst_back = izwst_back
        EXCEPTIONS
          general_fault = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        mac_err_repeat general_fault.
      ENDIF.
*
      IF xy_obj-eap_etta-dypercon IS INITIAL.
*       Lokale IZWST an globale IZWST anhängen
        LOOP AT izwst_back INTO wa_izwst.
*         Kennzeichen Nachberechnungszeitraum setzen
          wa_izwst-pertyp  =  co_back_billing.
          wa_izwst-backno  =  wnach-backno.
          APPEND wa_izwst TO xy_obj-izwst.
        ENDLOOP.                           " izwst_back
      ELSE.
*      IF NOT xy_obj-eap_etta-dypercon IS INITIAL.
        LOOP AT izwst_back INTO wa_izwst.
*       Kennzeichen Nachberechnungszeitraum setzen
          wa_izwst-pertyp  =  co_back_billing.
          wa_izwst-backno  =  wnach-backno.
          APPEND wa_izwst TO copy_izwst_dpc.
        ENDLOOP.                           " izwst_back
*       Bei dynamischer Periodensteuerung Nachberechnungszeiträumen
*       mit anderer BACKNO, aber gleichem Zeitraum, den gleichen
*       IZWST-Abschnitt hinzukopieren
        LOOP AT xy_obj-inach FROM idtfill INTO vnach
          WHERE ab  = wnach-ab
          AND   bis = wnach-bis.
          LOOP AT izwst_back INTO wa_izwst.
            wa_izwst-pertyp  =  co_back_billing.
            wa_izwst-backno  =  vnach-backno.
            APPEND wa_izwst TO copy_izwst_dpc.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
*   -----------------------------------------------------

    ENDIF. " tfill

  ENDLOOP.                             "  idnach


  IF NOT xy_obj-eap_etta-dypercon IS INITIAL.

*** >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** ??? Evt. GROSSE Abschnitte s.w.u. nicht immer aufbauen ?????
*** READ TABLE XY_OBJ-IACTPERIOD INDEX 1 INTO WACTPERIOD.
*** IF WACTPERIOD-ACTPERIOD = CO_3000.
***  ?????
*** ENDIF.
*** <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    LOOP AT xy_obj-iperiod INTO wperiod.
      IF wperiod-pbascat <> co_past_one_ts."Note 953491
        IF (  wperiod-useestmr IS INITIAL ).
*         Falls nicht explizit gewünscht wird, das auch geschätzte
*         Ergebnisse beachtet werden sollen, KOMPRIMIERTEN
*         Tabellenabschnitt für Zeitraum DE aufbauen
          f_de_c = 'X'.
        ELSE.
*         Falls für IRGENDEINE Periode auch geschätzte Ergebnisse
*         beachtet werden sollen, Gesamttabelle Zeitraum DE aufbauen
          f_de = 'X'.
        ENDIF.
      ELSE."Note 953491
        IF (  wperiod-useestmr IS INITIAL ).
*         Falls nicht explizit gewünscht wird, das auch geschätzte
*         Ergebnisse beachtet werden sollen, KOMPRIMIERTEN
*         Tabellenabschnitt für Zeitraum DN aufbauen
          f_dn_c = 'X'.
        ELSE.
*         Falls für Periodentyp CO_PAST_ONE_TS auch geschätzte
*         Ergebnisse beachtet werden sollen, Gesamttabelle Zeitraum DN
*         aufbauen
          f_dn = 'X'.
        ENDIF.
      ENDIF."Note 953491
    ENDLOOP.

    IF NOT f_dn_c IS INITIAL OR NOT f_dn IS INITIAL.
*     COPY_IZWST mit ALLEN Ableseergebnissen zum Zeitraum aufbauen
      LOOP AT xy_obj-iabrzeit INTO wabrzeit
        WHERE pertyp = co_dy_back_billing.
        LOOP AT idnach INTO vnach
          WHERE ab  >= wabrzeit-ab
          AND   bis <= wabrzeit-bis.
          LOOP AT copy_izwst_dpc INTO wa_izwst
            WHERE backno = vnach-backno
            AND   pertyp = co_back_billing.
            wa_izwst-pertyp = wabrzeit-pertyp.
            CLEAR wa_izwst-backno.
            APPEND wa_izwst TO copy_izwst.
          ENDLOOP.
        ENDLOOP.
        EXIT.
      ENDLOOP.
      IF NOT f_dn IS INITIAL.
*       ALLE Zeilen an Objekttabelle IZWST anhängen
        APPEND LINES OF copy_izwst TO xy_obj-izwst.
      ENDIF.
      IF NOT f_dn_c IS INITIAL.
*       COPY_IZWST komprimieren durch Streichung geschätzter
*       Ableseergebnisse, soweit möglich
        PERFORM copy_izwst_compress  USING xy_obj
                                           xy_obj-iablterm
                                  CHANGING copy_izwst.
*       Komprimierte COPY_IZWST an Objekttabelle IZWST anhängen
        APPEND LINES OF copy_izwst TO xy_obj-izwst.
      ENDIF.
    ENDIF.

    IF NOT f_de_c IS INITIAL OR NOT f_de IS INITIAL.
      REFRESH: copy_izwst.
      LOOP AT idnach INTO vnach.
        LOOP AT copy_izwst_dpc INTO wa_izwst
          WHERE backno = vnach-backno
          AND   pertyp = co_back_billing.
          wa_izwst-pertyp = co_dy_per_end_billing.
          CLEAR wa_izwst-backno.
          APPEND wa_izwst TO copy_izwst.
        ENDLOOP.
      ENDLOOP.
      LOOP AT xy_obj-izwst INTO wa_izwst
        WHERE backno IS INITIAL
        AND   pertyp = co_normal_billing.
        wa_izwst-pertyp = co_dy_per_end_billing.
        CLEAR wa_izwst-backno.
        APPEND wa_izwst TO copy_izwst.
      ENDLOOP.
      IF NOT f_de IS INITIAL.
*       ALLE Zeilen an Objekttabelle IZWST anhängen
        APPEND LINES OF copy_izwst TO xy_obj-izwst.
      ENDIF.
      IF NOT f_de_c IS INITIAL.
*       COPY_IZWST komprimieren durch Streichung geschätzter
*       Ableseergebnisse, soweit möglich
        PERFORM copy_izwst_compress  USING xy_obj
                                           xy_obj-iablterm
                                  CHANGING copy_izwst.
*       Komprimierte COPY_IZWST an Objekttabelle IZWST anhängen
        APPEND LINES OF copy_izwst TO xy_obj-izwst.
      ENDIF.
    ENDIF.

  ENDIF.

  mac_break1 'BACKEABL'.                                   "#EC NOBREAK

ENDFORM.                               " MRRESULT_FOR_BACK_BILLING

*&---------------------------------------------------------------------*
*&      Form  BACKBI_RESULTS_NECESSARY
*&---------------------------------------------------------------------*
FORM backbi_results_necessary
                    USING    x_obj         TYPE isu2a_billing_data
                    CHANGING y_bbp_results LIKE regen-kennzx.

  DATA wnach   TYPE isu2a_nach.
  DATA wss     TYPE isu2a_ss.
  DATA wetrf   LIKE etrf.
  DATA operand LIKE te221-operand.

* Es wird geprüft, ob für die Korrekturperioden eine Analyse der
* Ableseergebnisse (Aufbau der IZWST) notwendig ist.
* Dies ist genau dann der Fall, wenn mindestens in einer
* Korrekturperiode ein Schritt gefunden wird, der als Eingabeoperanden
* den Zählwerksoperand des entsprechenden Tarifs besitzt.
  LOOP AT x_obj-inach INTO wnach.
    LOOP AT x_obj-iss INTO wss
      WHERE backno = wnach-backno.
      CALL FUNCTION 'ISU_DB_ETRF_SINGLE'
        EXPORTING
          x_tarifnr = wss-tarifnr
        IMPORTING
          y_etrf    = wetrf
        EXCEPTIONS
          OTHERS    = 1.
      IF sy-subrc <> 0.
        mac_err_repeat general_fault.
      ENDIF.
      mac_do_einop_exit operand wss.
      IF operand = wetrf-operand.
        y_bbp_results = 'X'.
        EXIT.
      ENDIF.
      mac_enddo_einop.
      IF NOT y_bbp_results IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF NOT y_bbp_results IS INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " BACKBI_RESULTS_NECESSARY

*&---------------------------------------------------------------------*
*&      Form  THGAS_FOR_BACK_BILLING
*&---------------------------------------------------------------------*
FORM thgas_for_back_billing
                 USING     x_bbp_results   LIKE  regen-kennzx
                           x_irednach      TYPE  isu2a_inach
                           x_sobj          TYPE  isu2a_data_collector
                 CHANGING  xy_obj          TYPE  isu2a_billing_data.

  DATA  wnach         TYPE   isu2a_nach.
  DATA  wa_izw        TYPE   isu2a_zw.
  DATA  izw_back      LIKE   wa_izw OCCURS 0.
  DATA  wa_izwst      TYPE   isu2a_zwst.
  DATA  izwst_back    TYPE   isu2a_billing_data-izwst.
  DATA  ithg_back     TYPE   isu2a_billing_data-ithg.
  DATA  wthg          TYPE   isufg_thg .
  DATA  guse          TYPE   selcontrol.
  DATA  ls_period     TYPE   isu2c_period.
  DATA  lv_pertyp     TYPE   pertyp.
  DATA  idtfill       TYPE   sytfill.
  DATA: ls_zwst       TYPE   isu2a_zwst,
        ls_bill       TYPE   isu2a_bill_doc,
        ls_billold    TYPE   isu2a_bill_doc,
        lv_tfill      TYPE   sytfill,
        lv_thgdatumva TYPE   thgdatum,
        lv_thgdatum   TYPE   thgdatum.

  CHECK NOT x_bbp_results IS INITIAL
  OR ( NOT xy_obj-fl-estb_mode IS INITIAL
       AND xy_obj-st-spartyp = co_spartyp_gas ).      "note 935652

* --------------------------------------------------------------------
* Bemerkung SUE/WWT/HN 14.09.2000
* Auch bei dynamischer Periodensteuerung wird nur eine reduzierte
* ITHG aufgebaut, die ZEITLICH den gesamten Zeitraum abdeckt,
* aber eigentlich noch nicht "periodengerecht" ist.
* Alle Zugriffe auf die ITHG, z.B. in der Mengenaufteilung,
* erfolgen wie bisher nur ZEITBEZOGEN, nicht wie bei IZWST
* "periodengerecht"
* Dies ist nur eine kleine Einschränkung und wird ab 4.63 verbessert.
* ---------------------------------------------------------------------

  LOOP AT x_irednach INTO wnach.
*   -----------------------------------------------------
    REFRESH:  izw_back   ,
              izwst_back .
*   -----------------------------------------------------
*   ALle Ableseergebnisse zur Korrekturperiode einlesen
    IF xy_obj-fl-estb_mode IS INITIAL.
      LOOP AT xy_obj-izwst INTO wa_izwst
        WHERE backno = wnach-backno.
        APPEND wa_izwst TO izwst_back.
      ENDLOOP.                           " xy_izwst
    ELSE.
      LOOP AT xy_obj-iperiod INTO ls_period
      WHERE pbascat = co_past_billdoc.
        PERFORM create_izwst_back_nb USING    xy_obj
                                              wnach
                                     CHANGING izwst_back.
        EXIT.
      ENDLOOP.
    ENDIF.
*    CHECK NOT izwst_back IS INITIAL." Note 1140991
*   -----------------------------------------------------
*   Zu jeder Korrekturperiode lokale IZW erstellen
    CALL FUNCTION 'ISU_IZW_ADJUST'
      EXPORTING
        x_obj         = xy_obj
        x_backno      = wnach-backno
      CHANGING
        xy_izw_nach   = izw_back
      EXCEPTIONS
        general_fault = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.
*   -----------------------------------------------------
*   Gasabrechnung für jeweilige Korrekturperiode durchführen
    CALL FUNCTION 'ISU_GAS'
      EXPORTING
        x_izw             = izw_back
        x_izwst           = izwst_back
        x_guse            = 'BI'
        x_pertyp          = co_back_billing
        x_inst            = x_sobj-inst
        x_gas_substitutes = xy_obj-st-biparam-gas_substitutes
      CHANGING
        xy_ithg           = ithg_back
      EXCEPTIONS
        general_fault     = 1
        OTHERS            = 2.
*
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.
*
*   Lokale ITHG an globale ITHG anhängen
    LOOP AT ithg_back INTO wthg.
*     Kennzeichen Nachberechnungszeitraum korrekt setzen
      wthg-backno  =  wnach-backno.
      APPEND wthg TO xy_obj-ithg.
    ENDLOOP.                           " ithg_back
*   -----------------------------------------------------
  ENDLOOP.                             "  X_IREDNACH

  IF NOT xy_obj-eap_etta-dypercon IS INITIAL.
*   Gas billing for special DPC correction periods
    IF xy_obj-fl-estb_mode IS INITIAL.
      LOOP AT xy_obj-iperiod INTO ls_period.
        IF ls_period-pbascat = co_past_act_ts.
*         Gas billing for past and actual period in one timeslice
          REFRESH izw_back.
          CALL FUNCTION 'ISU_IZW_ADJUST'
            EXPORTING
              x_obj         = xy_obj
            CHANGING
              xy_izw_dpc    = izw_back
            EXCEPTIONS
              general_fault = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            mac_err_repeat general_fault.
          ENDIF.
          lv_pertyp = co_dy_per_end_billing.

          CLEAR: izwst_back, ithg_back.
          LOOP AT xy_obj-izwst INTO wa_izwst
              WHERE pertyp = co_dy_per_end_billing.
            APPEND wa_izwst TO izwst_back.
          ENDLOOP.                           " xy_izwst
        ELSEIF ls_period-pbascat = co_past_one_ts.
*         Gas billing past in one timeslice
          REFRESH izw_back.
          CALL FUNCTION 'ISU_IZW_ADJUST'
            EXPORTING
              x_obj           = xy_obj
            CHANGING
              xy_izw_nach_dpc = izw_back
            EXCEPTIONS
              general_fault   = 1
              OTHERS          = 2.
          IF sy-subrc <> 0.
            mac_err_repeat general_fault.
          ENDIF.
          lv_pertyp = co_dy_back_billing.
          CLEAR: izwst_back, ithg_back.
          LOOP AT xy_obj-izwst INTO wa_izwst
              WHERE pertyp = co_dy_back_billing.
            APPEND wa_izwst TO izwst_back.
          ENDLOOP.                           " xy_izwst
        ELSE.
          CONTINUE.
        ENDIF.
        CALL FUNCTION 'ISU_GAS'
          EXPORTING
            x_izw             = izw_back
            x_izwst           = izwst_back
            x_guse            = 'BI'
            x_pertyp          = lv_pertyp
            x_inst            = x_sobj-inst
            x_gas_substitutes = xy_obj-st-biparam-gas_substitutes
          CHANGING
            xy_ithg           = ithg_back
          EXCEPTIONS
            general_fault     = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
          mac_err_repeat general_fault.
        ENDIF.
*       Lokale ITHG an globale ITHG anhängen
        APPEND LINES OF ithg_back TO xy_obj-ithg.
      ENDLOOP.
    ENDIF.
  ENDIF.
  IF xy_obj-fl-estb_mode = 'X'.
*   DPC with estimation in billing -> Create dummy entries in IZWST_BACK
*   with correct gas date for gas billing
    LOOP AT xy_obj-iperiod INTO ls_period.
      REFRESH izwst_back.
      IF ls_period-pbascat = co_past_one_ts.
*       Get IZW/IZWST entries of period DN
        PERFORM adjust_izw_izwst_dn USING    xy_obj
                                    CHANGING izw_back
                                             izwst_back.
        lv_pertyp = co_dy_back_billing.
      ELSEIF ls_period-pbascat = co_act_bill_per.
*       Get IZW/IZWST entries of actual billing period
        PERFORM adjust_izw_izwst_no USING    xy_obj
                                    CHANGING izw_back
                                             izwst_back.
        lv_pertyp = co_normal_billing.
      ELSE.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'ISU_GAS'
        EXPORTING
          x_izw             = izw_back
          x_izwst           = izwst_back
          x_guse            = 'BI'
          x_pertyp          = lv_pertyp
          x_inst            = x_sobj-inst
          x_gas_substitutes = xy_obj-st-biparam-gas_substitutes
        CHANGING
          xy_ithg           = ithg_back
        EXCEPTIONS
          general_fault     = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        mac_err_repeat general_fault.
      ENDIF.
*     Lokale ITHG an globale ITHG anhängen
      APPEND LINES OF ithg_back TO xy_obj-ithg.
    ENDLOOP.
  ENDIF.

* Die Sortierung erfolgt ganz normal ohne besondere Berücksichtigung
* der Periodenabschnitte
  SORT xy_obj-ithg BY logikzw bis.

ENDFORM.                               " THGAS_FOR_BACK_BILLING

*&---------------------------------------------------------------------*
*&      Form  PERIODIC_BDATA_INTBILL
*&---------------------------------------------------------------------*

FORM periodic_bdata_intbill USING   x_te420 LIKE te420
                           CHANGING xy_obj  TYPE isu2a_billing_data
                                    y_te417 LIKE te417
                                    y_te418 LIKE te418.
  DATA: wte418 LIKE te418,
        sav_adatsoll LIKE sy-datum,
        subrc LIKE sy-subrc.

*>>>> Note 876320
  CALL FUNCTION 'ISU_DB_TE418_SLCT_N_ADATSOLL_T'
    EXPORTING
      x_termschl    = xy_obj-schedule-ableinh
      x_datum       = xy_obj-trig-etrg-adatsoll
    IMPORTING
      y_te418       = y_te418
    EXCEPTIONS
      not_found     = 1
      system_error  = 2
      not_qualified = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    IF 1 = 2. MESSAGE e103(tu). ENDIF.
    mac_msg_putx co_msg_error '103' 'TU'
     xy_obj-schedule-ableinh space space space not_found.
  ENDIF.
*<<<< Note 876320
ENDFORM.                    " PERIODIC_BDATA_INTBILL
*&---------------------------------------------------------------------*
*&      Form  date_correct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_XY_OBJ  text
*      <--P_XY_SOBJ  text
*----------------------------------------------------------------------*
FORM date_correct  CHANGING xy_obj  TYPE isu2a_billing_data
                            xy_sobj TYPE isu2a_data_collector.

  DATA: begana LIKE xy_obj-abr-begabrpe.
* Im Fall des Zahlungsschemas kann der Analysezeitraum mit dem Beginn
* des Abschlagszeitraums noch früher liegen:
  IF NOT xy_obj-abs-begabspe IS INITIAL AND
    xy_obj-abs-begabspe < xy_obj-date-begana.
    xy_obj-date-begana = xy_obj-abs-begabspe.
*   nur in diesem Fall sind auch die Ableseergebnisse noch einmal
*   nachzulesen:
    begana = xy_obj-date-begana - 1.
    PERFORM maximal_inst_eabl_for_billing
                                    USING  begana
                                           xy_obj-st-anlage
                                 CHANGING  xy_sobj-inst
                                           xy_sobj-max_ieabl
                                           xy_sobj-max_ieablg.
  ENDIF.

ENDFORM.                    " date_correct
*&---------------------------------------------------------------------*
*&      Form  ISTAFO_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IRATECAT  text
*      <--P_XY_OBJ  text
*----------------------------------------------------------------------*
FORM istafo_fill  USING    x_iratecat TYPE isu20_facts-iratecat
                  CHANGING xy_obj     TYPE isu2a_billing_data.

  DATA: wratecat      TYPE isu20_ratecat_list,
        wstafo        TYPE isu2a_stafo,
        wetta         TYPE etta,
        lv_stagruttyp TYPE e_stgrut.

* read STAFO for all relevant rate categories
  LOOP AT x_iratecat
    INTO wratecat.
    CLEAR wstafo.
    wstafo-tariftyp = wratecat-tariftyp.
    IF wratecat-tariftyp EQ xy_obj-eap_etta-tariftyp.
*     rate category at end of billing period is fully known.
      lv_stagruttyp = xy_obj-eap_etta-stagruttyp.
    ELSE.
*     read the head of rate category
      CALL FUNCTION 'ISU_DB_ETTA_SINGLE'
        EXPORTING
          x_tariftyp = wratecat-tariftyp
        IMPORTING
          y_etta     = wetta
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0.
        mac_err_repeat general_fault.
      ENDIF.
      lv_stagruttyp = wetta-stagruttyp.
    ENDIF.
*   now read STAFO
    CALL FUNCTION 'ISU_LIS_UPDATE_GROUP_DETERMINE'
      EXPORTING
        x_sparte     = xy_obj-st-sparte
        x_stagruver  = xy_obj-st-stagruver
        x_stagruttyp = lv_stagruttyp
      IMPORTING
        y_stafo      = wstafo-stafo
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      mac_err_repeat general_fault.
    ENDIF.
    APPEND wstafo TO xy_obj-istafo.
  ENDLOOP.

* final sort
  SORT xy_obj-istafo BY tariftyp.
  DELETE ADJACENT DUPLICATES FROM xy_obj-istafo.

ENDFORM.                    " ISTAFO_FILL