*-------------------------------------------------------------------
***INCLUDE LEA01F02 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  INSTALLATION_FOR_CONTRACT
*&---------------------------------------------------------------------*
FORM INSTALLATION_FOR_CONTRACT
          TABLES  YT_MAX_EVER            STRUCTURE  EVER
          USING
                  VALUE(X_VERTRAG)       LIKE       EVER-VERTRAG
        CHANGING
                        Y_ANLAGE         LIKE       EVER-ANLAGE
                  VALUE(Y_INF_BEGANA)    LIKE       ERCH-ENDABRPE
                  VALUE(Y_PREVIOUS_BILL) LIKE       ERCH .

  DATA WEVER          LIKE  EVER.
  DATA NEXT_CONTRACT  LIKE  EVER-VERTRAG.
  DATA LAST_ENDABRPE  LIKE  ERCH-ENDABRPE.
  DATA COMPDAT        TYPE  D.
  DATA IETRG          LIKE  ETRG OCCURS 0.
  DATA SUBRC          LIKE SYST-SUBRC.

* vertrag lesen
  CALL FUNCTION 'ISU_DB_EVER_SINGLE'
    EXPORTING
      X_VERTRAG    = X_VERTRAG
    IMPORTING
      Y_EVER       = WEVER
    EXCEPTIONS
      NOT_FOUND    = 1
      SYSTEM_ERROR = 2
      OTHERS       = 3.
  IF SY-SUBRC <> 0.
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ELSE.
    Y_ANLAGE = WEVER-ANLAGE.
    IF Y_ANLAGE IS INITIAL.
*     Der Vertrag &1 ist keiner Anlage zugeordnet
      MAC_MSG_PUT 'E095(AJ)' X_VERTRAG SPACE SPACE SPACE
                             GENERAL_FAULT.
    ENDIF.
  ENDIF.
* Prüfen, ob Vertrag schon abgerechnet werden kann; das ist nicht
* der Fall, wenn es noch andere zeitlich vorangehende Verträge zur
* Anlage gibt, die noch nicht schlussabgerechnet sind
  CALL FUNCTION 'ISU_NEXT_CONTRACT_FOR_INSTLN'
    EXPORTING
      X_ANLAGE          = Y_ANLAGE
    IMPORTING
      Y_NEXT_CONTRACT   = NEXT_CONTRACT
      Y_LAST_ENDABRPE   = LAST_ENDABRPE
      Y_PREVIOUS_BILL   = Y_PREVIOUS_BILL
    TABLES
      XYT_EVER          = YT_MAX_EVER
    EXCEPTIONS
      NO_CONTRACT_FOUND = 1
      GENERAL_FAULT     = 2
      OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ENDIF.
  IF NEXT_CONTRACT <> X_VERTRAG.
*   Der als abzurechnen übergebene Vertrag ist nicht der nächste
*   abzurechnende Vertrag zur Anlage
    IF WEVER-AUSZDAT <= LAST_ENDABRPE.
*     Zu dem übergebenen Vertrag wurde bereit ein Auszug erfaßt und
*     abgerechnet
      IF X_VERTRAG = Y_PREVIOUS_BILL-VERTRAG.
*       Der übergebene Vertrag ist der zuletzt zu der Anlage
*       abgerechnete --> prüfen, ob es zu dem Vertrag noch einen
*       Endabrechnungstrigger gibt, der abzurechnen ist
        CALL FUNCTION 'MSG_ACTION'
          EXPORTING
            X_ACTION = CO_MSG_LOG_OFF.
        CALL FUNCTION 'ISU_DB_ETRG_SELECT_ANLAGE'
          EXPORTING
            X_ANLAGE  = Y_ANLAGE
            X_ABRVORG = CO_ENDABR
          TABLES
            T_ETRG    = IETRG
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
        SUBRC = SY-SUBRC.
        CALL FUNCTION 'MSG_ACTION'
          EXPORTING
            X_ACTION = CO_MSG_LOG_ON.
        IF SUBRC > 1.
          MAC_MSG_LAST_REPEAT CO_MSG_ERROR GENERAL_FAULT.
        ENDIF.
*       Prüfen, ob es einen Endabrechnungstrigger mit gleichem
*       Beginn der Endabrechnungsperiode gibt, wie zur letzten
*       Abrechnung des übergebenen Vertrags (wenn ja, dann
*       kann zu dem übergebenen Vertrag eine Abrechnung durchgeführt
*       werden)
        READ TABLE IETRG TRANSPORTING NO FIELDS
             WITH KEY BEGEND = Y_PREVIOUS_BILL-BEGEND.
        IF SY-SUBRC <> 0.
*         Der Vertrag &1 wurde bereits schlußabgerechnet
          MAC_MSG_PUT 'E719(AH)' X_VERTRAG SPACE SPACE SPACE
                                 CONTRACT_FINAL_BILLED.
        ENDIF.
      ELSE.
*       Der Vertrag &1 wurde bereits schlußabgerechnet
        MAC_MSG_PUT 'E719(AH)' X_VERTRAG SPACE SPACE SPACE
                               CONTRACT_FINAL_BILLED.
      ENDIF.
    ELSE.
*     Zur Anlage &1 muß zunächst der Vertrag &2 abgerechnet werden
      MAC_MSG_PUT 'E385(AH)' Y_ANLAGE NEXT_CONTRACT X_VERTRAG SPACE
                             WRONG_CONTRACT_ORDER.
    ENDIF.
  ENDIF.

* Die folgende Ermittlung Y_INF_BEGANA des ältesten Beginns der
* Datenanalyse dient vor allem dem performanten Einlesen der
* Ableseergebnisse für die Abrechnung
  IF NOT Y_PREVIOUS_BILL IS INITIAL.
    Y_INF_BEGANA = Y_PREVIOUS_BILL-ENDABRPE.
*   Im Falle der Nachberechnung muss der älteste Beginn der
*   Datenanalyse vorgezogen werden, aber nur dann, wenn die letzte
*   Periodenabrechnung noch nicht gelaufen ist
    IF  (  NOT  Y_PREVIOUS_BILL-BEGNACH   IS  INITIAL  AND
                Y_PREVIOUS_BILL-ENDOFBB   IS  INITIAL      ).
      COMPDAT = Y_PREVIOUS_BILL-BEGNACH - 1.
      IF Y_INF_BEGANA > COMPDAT.
        Y_INF_BEGANA = COMPDAT.
      ENDIF.
    ENDIF.
*   Im Falle der Endabrechnung muss der älteste Beginn der
*   Datenanalyse immer vorgezogen werden, da man an dieser Stelle
*   noch nicht weiss, ob die Endabrechnung selbst schon gelaufen ist
    IF NOT Y_PREVIOUS_BILL-BEGEND IS INITIAL.
      COMPDAT = Y_PREVIOUS_BILL-BEGEND - 1.
      IF Y_INF_BEGANA > COMPDAT.
        Y_INF_BEGANA = COMPDAT.
      ENDIF.
    ENDIF.
  ELSE.
*   Falls es keinen Vorbeleg gibt, wird der älteste Beginn der
*   Datenanalyse auf fixes Datum 01.01.1900 gelegt
    Y_INF_BEGANA = CO_DATE_FINITE.
  ENDIF.

ENDFORM.                               " INSTALLATION_FOR_CONTRACT
*&---------------------------------------------------------------------*
*&      Form  ETRG_ORDER_CONTROL
*&---------------------------------------------------------------------*
FORM ETRG_ORDER_CONTROL
        TABLES  XT_MAX_EVER              STRUCTURE  EVER
                YT_ETRG_SEQ              STRUCTURE  ETRG_SEQ
        USING   VALUE(X_ANLAGE)          LIKE       EANL-ANLAGE
                VALUE(X_VERTRAG)         LIKE       EVER-VERTRAG
                VALUE(X_LAST_ENDABRPE) LIKE ERCH-ENDABRPE   "N 800248
                VALUE(X_MAX_INST)        TYPE       ISU2A_INST_STRUC
                VALUE(X_MAX_IEABL)       TYPE       ISU2A_IEABL
                VALUE(X_MAX_IEABLG)      TYPE       ISU2A_IEABLG.

  DATA:  T_EQUNR       TYPE  ISU17_METERDOCU_EQUDAT_TAB.
  DATA:  W_EQUNR       TYPE  ISU17_METERDOCU_EQUDAT.
  DATA:  W_EGER        LIKE  V_EGER.
  DATA:  S_SYST        LIKE  SYST.
  DATA:  DATE_STR(10)  TYPE  C .
* >>>>> note 939496
  DATA:  wever         type ever.
  data:  last_endabrpe type abzeitsch.

  if x_last_endabrpe is initial.
*   first billing for this installation get the oldest move-in date
*   of the contracts that have not been finally billed:
    loop at xt_max_ever
      into wever
      where billfinit is initial.
      last_endabrpe = wever-einzdat - 1.
    endloop.
  else.
    last_endabrpe = x_last_endabrpe.
  endif.
* <<<<< note 939496

  CALL FUNCTION 'MSG_ACTION'
    EXPORTING
      X_ACTION = CO_MSG_LOG_OFF.

  CALL FUNCTION 'ISU_ETRG_ORDER_CONTROL'
    EXPORTING
      X_ANLAGE            = X_ANLAGE
      X_MAX_INST          = X_MAX_INST
      X_MAX_IEABL         = X_MAX_IEABL
      X_MAX_IEABLG        = X_MAX_IEABLG
      X_USE_INST_EABL     = 'X'
      X_USE_MAX_EVER      = 'X'
      X_LAST_ENDABRPE     = LAST_ENDABRPE    "note 939496
      X_USE_ENDABRPE      = 'X'
    TABLES
      XT_MAX_EVER         = XT_MAX_EVER
      TY_ALL_ETRG_SEQ     = YT_ETRG_SEQ
      TY_EQUNR            = T_EQUNR
    EXCEPTIONS
      GENERAL_FAULT       = 1
      NO_TRIGGER_EXISTING = 2
      ORDER_VIOLATION     = 3
      OTHERS              = 4.
  IF SY-SUBRC <> 0.  "nur für Syntax-Prüfung
    S_SYST = SYST.
  ENDIF.
* Syst-Variable retten
  S_SYST = SYST.
* Protokoll wieder einschalten
  CALL FUNCTION 'MSG_ACTION'
    EXPORTING
      X_ACTION = CO_MSG_LOG_ON.

  IF S_SYST-SUBRC = 2.

    IF NOT X_VERTRAG IS INITIAL.
*     Keine Trigger zur Anlage, insbesondere nicht zum Vertrag
*     gefunden
      MAC_MSG_PUT 'E001(AJ)' X_VERTRAG SPACE SPACE SPACE
                             GENERAL_FAULT.
    ELSE.
*     Keine Trigger zur Anlage gefunden
      MAC_MSG_PUT 'E027(AJ)' X_ANLAGE  SPACE SPACE SPACE
                             GENERAL_FAULT.
    ENDIF.

  ELSEIF S_SYST-SUBRC <> 0.

*   -----------------------------------------------------------------
*
*   Falls die Tabelle T_EQUNR gefüllt ist, Liste von Geräten/Zw'en
*   mit fehlenden/fehlerhaften Ableseergebnissen ausgeben:
*
    DESCRIBE TABLE T_EQUNR LINES SY-TFILL.
    IF SY-TFILL > 0.
*     ---------------- Listüberschrift ------------------------
*     Bei folgenden Geräten/Zählwerken fehlen Ableseergebnisse:
      MAC_MSG_PUT 'E707(AH)' SPACE SPACE SPACE SPACE SPACE.
      IF 1 = 2. MESSAGE E707(AH). ENDIF.
*     ---------------- Liste ----------------------------------
      LOOP AT T_EQUNR INTO W_EQUNR.
        WRITE W_EQUNR-ADATSOLL TO DATE_STR DD/MM/YYYY.
*       Gerät ermitteln sofern möglich
        CALL FUNCTION 'ISU_DB_EGER_SELECT_CUTOFFDATE'
             EXPORTING
                  X_EQUNR      = W_EQUNR-EQUNR
                  X_DATE       = W_EQUNR-ADATSOLL
*                 x_actual     = 'X'           Änderung K.K. 11.9.98
             IMPORTING
                  Y_EGER       = W_EGER
             EXCEPTIONS
                  NOT_FOUND    = 1
                  SYSTEM_ERROR = 2
                  OTHERS       = 3.
        IF SY-SUBRC = 0.
*         Gerät ermittelbar -->>
*         Gerät &1 Zw &2 Sollablesedatum &3
          MAC_MSG_PUT 'E708(AH)'
            W_EGER-GERAET W_EQUNR-ZWNUMMER DATE_STR SPACE SPACE.
          IF 1 = 2. MESSAGE E708(AH) WITH SPACE SPACE SPACE. ENDIF.
        ELSE.
*         Gerät nicht ermittelbar (direkte Fehlerausgabe) ->>
*         Equinummmer &1 Zw &2 Sollablesedatum &3
          MAC_MSG_PUT 'E709(AH)'
            W_EQUNR-EQUNR W_EQUNR-ZWNUMMER DATE_STR SPACE SPACE.
          IF 1 = 2. MESSAGE E709(AH) WITH SPACE SPACE SPACE. ENDIF.
        ENDIF.                         " sy-subrc
      ENDLOOP.                         " t_equnr
    ENDIF.                             " sy-tfill

*   -----------------------------------------------------------------
*   Abschliessend generelle Fehlermeldung aus FB wiederholen
    MAC_MSG_PUTX  CO_MSG_ERROR S_SYST-MSGNO S_SYST-MSGID
                  S_SYST-MSGV1 S_SYST-MSGV2 S_SYST-MSGV3 S_SYST-MSGV4
                  GENERAL_FAULT.
*   -----------------------------------------------------------------

  ENDIF.                               " S_syst-subrc

ENDFORM.                               " ETRG_ORDER_CONTROL
*&---------------------------------------------------------------------*
*&      Form  ETRG_SELECTION
*&---------------------------------------------------------------------*
FORM ETRG_SELECTION  TABLES  XT_ETRG_SEQ       STRUCTURE  ETRG_SEQ
                             XT_ETRG           STRUCTURE  ETRG
                             XYT_ETRG_MOD      STRUCTURE  ETRG_SEQ
* <<<<< begin of note 890147
                             XYT_ETRG_AUSS     STRUCTURE  ETRG_SEQ
* >>>>> end of note 890147
                     USING   VALUE(X_ANLAGE)   LIKE       EANL-ANLAGE
                             VALUE(X_FILTER)   TYPE       ISUSE_FILTER.

  CALL FUNCTION 'ISU_ETRG_SELECTION'
    EXPORTING
      X_ANLAGE        = X_ANLAGE
      X_FILTER        = X_FILTER
    TABLES
      X_ALL_ETRG_SEQ  = XT_ETRG_SEQ
      X_ETRG          = XT_ETRG
      XY_ETRG_MOD     = XYT_ETRG_MOD
* <<<<< begin of note 890147
      XY_ETRG_AUSS    = XYT_ETRG_AUSS
* >>>>> end of note 890147
    EXCEPTIONS
      GENERAL_FAULT   = 1
      ORDER_VIOLATION = 2
      NO_SELECTION    = 3
      OTHERS          = 4.
  IF SY-SUBRC <> 0.
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ENDIF.

ENDFORM.                               " ETRG_SELECTION
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_BILL
*&---------------------------------------------------------------------*
FORM TRIGGER_BILL TABLES XT_MAX_EVER              STRUCTURE EVER
                  USING VALUE(X_EVER)             LIKE EVER
                        VALUE(X_SIMULATION)       LIKE REGEN-KENNZX
                        VALUE(X_NO_SUCCESS_MSG)   LIKE REGEN-KENNZX
                        VALUE(X_BILLINGRUNNO)     LIKE ERCH-BILLINGRUNNO
                        VALUE(X_ETRG)              LIKE ETRG_SEQ
                        VALUE(X_USE_PREVIOUS_ERCH) LIKE REGEN-KENNZX
                        VALUE(X_PREVIOUS_ERCH)     LIKE ERCH
                        VALUE(X_MAX_INST)          TYPE ISU2A_INST_STRUC
                        VALUE(X_MAX_IEABL)         TYPE ISU2A_IEABL
                        VALUE(X_MAX_IEABLG)        TYPE ISU2A_IEABLG
                        VALUE(X_NO_UPDATE)         LIKE REGEN-KENNZX
                        VALUE(X_NO_INVOICE)        TYPE NINVOICE
                        X_INSTGR_ENQ TYPE REF TO CL_ISU_BI_INSTGR_ENQ
                        X_PREVIOUS_IERCHP          TYPE ERCHP_TAB
               CHANGING       Y_BELNR              LIKE ERCH-BELNR
                              Y_BILL_DOC           TYPE ISU2A_BILL_DOC
                              Y_ERROR              LIKE REGEN-KENNZX
                              Y_IRTPINTF           TYPE T_ERTPINTF.


  DATA:  USE_PREVIOUS_BILL  TYPE  ISU2A_BILL_DOC_USE.
  DATA:  PREVIOUS_BILL      TYPE  ISU2A_BILL_DOC.
  DATA:  W_ETRG             LIKE  ETRG.

* Falls ein Vorbeleg-Kopf explizit übergeben wurde, in speziellen
* Feldleisten weiterreichen
* When a move-out/in has occurred for this installation, then must also
* ensure that the previous doc for the old contract is NOT passed. (MN)
  IF ( NOT X_USE_PREVIOUS_ERCH IS INITIAL ) AND
     ( X_PREVIOUS_ERCH-VERTRAG = X_EVER-VERTRAG ).
    USE_PREVIOUS_BILL-USE_ERCH = 'X'.
    PREVIOUS_BILL-ERCH = X_PREVIOUS_ERCH.
    READ TABLE X_PREVIOUS_IERCHP WITH KEY BELNR = X_PREVIOUS_ERCH-BELNR
    TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      USE_PREVIOUS_BILL-USE_IERCHP = 'X'.
      PREVIOUS_BILL-IERCHP = X_PREVIOUS_IERCHP.
    ENDIF.
  ENDIF.

* Ab hier wieder normale ETRG-Struktur statt ETRG_SEQ benutzen
  MOVE-CORRESPONDING X_ETRG TO W_ETRG.

  CALL FUNCTION 'ISU_SINGLE_TRIGGER_BILL'
    EXPORTING
      X_VERTRAG           = X_EVER-VERTRAG
      X_ETRG              = W_ETRG
      X_SIMULATION        = X_SIMULATION
      X_NO_SUCCESS_MSG    = X_NO_SUCCESS_MSG
      X_BILLINGRUNNO      = X_BILLINGRUNNO
      X_MAX_INST          = X_MAX_INST
      X_USE_PREVIOUS_BILL = USE_PREVIOUS_BILL
      X_PREVIOUS_BILL     = PREVIOUS_BILL
      X_MAX_IEABL         = X_MAX_IEABL
      X_MAX_IEABLG        = X_MAX_IEABLG
      X_USE_EVER          = 'X'
      X_EVER              = X_EVER
      X_IALLEVER          = XT_MAX_EVER[]
      X_NO_UPDATE         = X_NO_UPDATE
      X_NO_INVOICE        = X_NO_INVOICE
      X_PROV_ENDABRPE     = X_ETRG-ORDERDAT
      X_INSTGR_ENQ        = X_INSTGR_ENQ
    IMPORTING
      Y_BELNR             = Y_BELNR
      Y_BILL_DOC          = Y_BILL_DOC
      Y_IRTPINTF          = Y_IRTPINTF
    EXCEPTIONS
      GENERAL_FAULT       = 1
      OTHERS              = 2.
  IF SY-SUBRC <> 0.
    ROLLBACK WORK.
    MAC_ERR_REPEAT SPACE.
    Y_ERROR = 'X'.
  ENDIF.

ENDFORM.                               " TRIGGER_BILL
*&---------------------------------------------------------------------*
*&      Form  CONTRACT_DEQUEUE
* HS, 13.2.2004: Form deaktiviert, da nicht verwendet
*&---------------------------------------------------------------------*
*FORM contract_dequeue USING value(x_vertrag) LIKE ever-vertrag
*                            value(x_simulation) LIKE regen-kennzx.
** Hilfstabelle für das Sperren
*  DATA: BEGIN OF ikeys OCCURS 1,
*          mandt     LIKE   sy-mandt,
*          vertrag   LIKE   ever-vertrag,
*        END OF ikeys.
** nur, wenn nicht im Simulationsmodus
*  IF x_simulation IS INITIAL.
**   Vertrag entsperren
*    ikeys-mandt   = sy-mandt.
*    ikeys-vertrag = x_vertrag.
*    APPEND ikeys.
**   Baustein zum Entsperren
*    CALL FUNCTION 'ISU_MASTER_DATA_DEQUEUE'
*      EXPORTING
*        x_objtype = 'EVER'
*      TABLES
*        xt_keys   = ikeys.
*
*  ENDIF.                               "x_simulatio
*ENDFORM.                               " CONTRACT_DEQUEUE
*
*&---------------------------------------------------------------------*
*&      Form  IZWST_CONSTRUCT_FOR_BCHECK
*&---------------------------------------------------------------------*
FORM IZWST_CONSTRUCT_FOR_BCHECK
           CHANGING XY_OBJ    TYPE ISU2A_BILLING_DATA
                    XY_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

  DATA ABR_TRIG      TYPE   ISU2A_ABR_TRIG.
  DATA COMPR_IZW     TYPE   ISU2A_BILLING_DATA-IZW.
  DATA IZW_1         TYPE   ISU2A_BILLING_DATA-IZW.
  DATA WA_IZWST      TYPE   ISU2A_ZWST.
  DATA BEGANA_ESTB   TYPE   D.

  IF XY_OBJ-FL-ESTB_MODE IS INITIAL.
    CALL FUNCTION 'ISU_IZW_ADJUST'
      EXPORTING
        X_OBJ         = XY_OBJ
      CHANGING
        XY_IZW_ABR    = IZW_1
      EXCEPTIONS
        GENERAL_FAULT = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
*
*** read table xy_obj-izw index 1 transporting no fields.  " XX99
    READ TABLE IZW_1 INDEX 1 TRANSPORTING NO FIELDS.        " XX99
    IF SY-SUBRC <> 0. EXIT. ENDIF.

    MAC_BREAK1 'IZWST'.                  " Aufbau IZWST BCheck
    MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_METER_READING_RESULTS_GET'
*   Alle eventuell relevanten Ableseergebnisse einlesen
    CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
         EXPORTING
              X_IZW           = XY_OBJ-IZW
              X_IABLTERM      = XY_OBJ-IABLTERM
              X_ABR           = XY_OBJ-ABR
              X_INST          = XY_SOBJ-INST
              X_VERTRAG       = XY_OBJ-ST-VERTRAG
              X_EABL_KZ       = '1'
              X_IZW_1         = IZW_1
***         x_begana        = xy_obj-date-begana    " WWT X11A
              X_BEGANA        = XY_OBJ-ABR-BEGABRPE         " WWT X11A
* >>>>> AOP 007, Note 159968
              X_EINZDAT       = XY_SOBJ-EVER-EINZDAT
* <<<<< AOP 007, Note 159968
         IMPORTING
              Y_IZWST         = XY_OBJ-IZWST
              Y_COMPR_IZW     = COMPR_IZW
              Y_IEABL         = XY_OBJ-IEABL
              Y_IEABLG        = XY_OBJ-IEABLG
         EXCEPTIONS
              GENERAL_FAULT   = 1
              OTHERS          = 2.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.

    MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_METER_READING_RESULTS_GET'
    " Anfang FB 'ISU_IZWST_LE_MARGIN_DETERMINE'

*   IZWST teilaufbauen mit korrektem linken Rand
*   abr_trig-etrg = x_etrg.  " formal, eventuell ETRG unnötig
    CALL FUNCTION 'ISU_IZWST_LE_MARGIN_DETERMINE'
         EXPORTING
              X_ABR_TRIG      = ABR_TRIG
              X_PBILL         = XY_OBJ-PBILL
              X_VERTRAG       = XY_OBJ-ST-VERTRAG
***         x_izw           = xy_obj-izw                " WWT X11A
              X_IZW           = IZW_1                       " WWT X11A
              X_EINZDAT       = XY_SOBJ-EVER-EINZDAT
         CHANGING
              XY_IZWST        = XY_OBJ-IZWST
              XY_COMPR_IZW    = COMPR_IZW
         EXCEPTIONS
              GENERAL_FAULT   = 1
              BIG_CHECK_ERROR = 2
              OTHERS          = 3.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
    MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_IZWST_LE_MARGIN_DETERMINE'

    MAC_BREAK1 'SIMABL'. " Anfang FB ISU_SIM_IZWST_ESTABLISH
*
*   Hier muß jetzt die Hochrechnung der Zählerstände für den
*   Simulationszeitraum durchgeführt werden  --> IZWST
*   Erwartet werden wie immer für diesen FB eine exakt passende IZW
*   sowie ein Kz KZ_VOR_EXIST = 'X' , dass Start-Ablesedatümer für
*   die einzelnen Zählwerke vorhanden sein müssen
*
    CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
         EXPORTING
              X_IABLTERM      = XY_OBJ-IABLTERM
              X_VERTRAG       = XY_OBJ-ST-VERTRAG
              X_KZ_VOR_EXIST  = 'X'
***         x_izw           = xy_obj-izw                " WWT XX99
              X_IZW           = IZW_1                       " WWT XX99
              X_IEABL         = XY_OBJ-IEABL
              X_IEABLG        = XY_OBJ-IEABLG
              X_ANLAGE        = XY_OBJ-ST-ANLAGE
              X_INST          = XY_SOBJ-INST
              X_ABR           = XY_OBJ-ABR
              X_ST            = XY_OBJ-ST
* >>>>> AOP xxx,  Note 149045
              X_SPECIAL_BEGIN = 'X'
              X_ABRDATS_LOW   = XY_OBJ-PBILL-ERCH-ABRDATS
              X_ENDABRPE_LOW  = XY_OBJ-PBILL-ERCH-ENDABRPE
* <<<<< AOP xxx,  Note 149045
* >>>>> AOP xxx,  Note 163207
              X_ABRVORG_LOW   = XY_OBJ-PBILL-ERCH-ABRVORG
* <<<<< AOP xxx,  Note 163207
              X_EVER          = XY_SOBJ-EVER
              X_SIMULATION    = CO_SIM_BIGCHECK        "note 1011255
         CHANGING
              XY_IZWST        = XY_OBJ-IZWST
              XY_MABLBELNR    = XY_OBJ-MABLBELNR
         EXCEPTIONS
              GENERAL_FAULT   = 1
              OTHERS          = 2.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
*
    MAC_BREAK1 'SIMABL'.                 " Ende FB ISU_SIM_IZWST_ESTABLISH

*   Alle Einträge mit bis dato initialem Periodentyp gehören
*   zum regulären BigCheck-Zeitraum
*   Nun korrekten Periodentyp eintragen
    LOOP AT XY_OBJ-IZWST INTO WA_IZWST
      WHERE PERTYP IS INITIAL.
      WA_IZWST-PERTYP = CO_NORMAL_BILLING.
      MODIFY XY_OBJ-IZWST FROM WA_IZWST.
    ENDLOOP.
  ELSE.
*   Verarbeitung mit ggfs. simuliertem linken und rechten Rand,
*   da nicht notwendig für alle Zählwerke Ableseergebnisse in DB EABL
*   vorausgesetzt werden
*   Shift start date to end of last billing period + 1
*    PERFORM DPC_IZWST_VIA_SIM_GET CHANGING XY_OBJ XY_SOBJ.
      PERFORM IZWST_CONSTRUCT_FOR_S_SIMUL CHANGING xy_OBJ xy_SOBJ.

  ENDIF.

ENDFORM.                               " IZWST_CONSTRUCT_FOR_BCHECK

*&---------------------------------------------------------------------*
*&      Form  IZWST_CONSTRUCT_FOR_S_SIMUL
*&---------------------------------------------------------------------*
FORM IZWST_CONSTRUCT_FOR_S_SIMUL
           CHANGING  XY_OBJ    TYPE ISU2A_BILLING_DATA
                     XY_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

  INCLUDE IEASMODE.
  INCLUDE IELABLGR.

  DATA ABR_TRIG  TYPE  ISU2A_ABR_TRIG.
  DATA IZW_1     TYPE  ISU2A_BILLING_DATA-IZW.
  DATA WA_IZWST  TYPE  ISU2A_ZWST.
  DATA WZWST     TYPE  ISU2A_ZWST.

  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_ABR    = IZW_1
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
***read table xy_obj-izw index 1 transporting no fields.   " XX99
  READ TABLE IZW_1 INDEX 1 TRANSPORTING NO FIELDS.          " XX99
  IF SY-SUBRC <> 0. EXIT. ENDIF.

  MAC_BREAK1 'IZWST'.                  " Aufbau IZWST S-Simulation
  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_METER_READING_RESULTS_GET'
* Alle eventuell relevanten Ableseergebnisse einlesen
  CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
    EXPORTING
      X_IZW         = XY_OBJ-IZW
      X_IABLTERM    = XY_OBJ-IABLTERM
      X_ABR         = XY_OBJ-ABR
      X_INST        = XY_SOBJ-INST
      X_VERTRAG     = XY_OBJ-ST-VERTRAG
      X_EABL_KZ     = '0'
      X_IZW_1       = IZW_1
      X_BEGANA      = XY_OBJ-ABR-BEGABRPE
      X_EINZDAT     = XY_SOBJ-EVER-EINZDAT
      X_MAX_IEABL   = XY_SOBJ-MAX_IEABL  "$$IV
      X_MAX_IEABLG  = XY_SOBJ-MAX_IEABLG  "$$IV
      X_ANLAGE      = XY_OBJ-ST-ANLAGE
      X_BIPARAM     = XY_OBJ-ST-BIPARAM
    IMPORTING
      Y_IZWST       = XY_OBJ-IZWST
      Y_IEABL       = XY_OBJ-IEABL
      Y_IEABLG      = XY_OBJ-IEABLG
    CHANGING
      XY_MABLBELNR  = XY_OBJ-MABLBELNR
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
  MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_METER_READING_RESULTS_GET'

* When the simulation is started by QUANTI26, only real results
* should be taken into account. Delete all estimated meter reading
* results.
*  IF XY_OBJ-FL-SIMULATION = CO_SIM_PERIOD_BILL.
*    LOOP AT XY_OBJ-IZWST INTO WZWST.
*      IF WZWST-ABLESTYP  <> CO_MRTY_UTILITY
*      AND WZWST-ABLESTYP <> CO_MRTY_CUSTOMER
*      AND WZWST-ABLESTYP <> CO_MRTY_INTERNET
*      AND NOT WZWST-ABLESTYP IS INITIAL.
*        DELETE XY_OBJ-IEABL
*        WHERE ABLBELNR = WZWST-ABLBELNR.
*        DELETE XY_OBJ-IEABLG
*        WHERE ABLBELNR = WZWST-ABLBELNR.
*      ENDIF.
*      IF  WZWST-ABLESTYPVA <> CO_MRTY_UTILITY
*      AND WZWST-ABLESTYPVA <> CO_MRTY_CUSTOMER
*      AND WZWST-ABLESTYPVA <> CO_MRTY_INTERNET
*      AND NOT WZWST-ABLESTYPVA IS INITIAL.
*        DELETE XY_OBJ-IEABL
*        WHERE ABLBELNR = WZWST-ABLBELNRVA.
*        DELETE XY_OBJ-IEABLG
*        WHERE ABLBELNR = WZWST-ABLBELNRVA.
*      ENDIF.
*      IF ( ( WZWST-ABLESTYP   <> CO_MRTY_UTILITY
*      AND  WZWST-ABLESTYP   <> CO_MRTY_CUSTOMER
*      AND  WZWST-ABLESTYP   <> CO_MRTY_INTERNET
*      AND NOT WZWST-ABLESTYP IS INITIAL )
*      OR ( WZWST-ABLESTYPVA <> CO_MRTY_UTILITY
*      AND  WZWST-ABLESTYPVA <> CO_MRTY_CUSTOMER
*      AND  WZWST-ABLESTYPVA <> CO_MRTY_INTERNET
*      AND NOT WZWST-ABLESTYPVA IS INITIAL ) )
*      AND WZWST-ABLESGRV    <> CO_GR_EINZUG.
*        DELETE XY_OBJ-IZWST.
*      ENDIF.
*    ENDLOOP.                             "x_izwst
*  ENDIF. "XY_OBJ-FL-SIMULATION


  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_SIM_IZWST_ESTABLISH'
* Der folgende FB erwartet eine genau für den Simulationszeitraum
* aufbereitete IZW; es kann ferner ein Kz KZ_VOR_EXIST = ' ' erwartet
* werden, welches besagt, dass die Ableseergebnisse zu Beginn der
* Zeiträume nicht notwendig vorhanden sein müssen
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
       EXPORTING
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_VERTRAG       = XY_OBJ-ST-VERTRAG
            X_KZ_VOR_EXIST  = SPACE
***         x_izw           = xy_obj-izw           " WWT XX99
            X_IZW           = IZW_1                         " WWT XX99
            X_KZ_BEG_ADJUST = 'X'
            X_IEABL         = XY_OBJ-IEABL
            X_IEABLG        = XY_OBJ-IEABLG
            X_ANLAGE        = XY_OBJ-ST-ANLAGE
            X_INST          = XY_SOBJ-INST
            X_ABR           = XY_OBJ-ABR
            X_ST            = XY_OBJ-ST
            X_READ_ALL_EABL = 'X'
            X_EVER          = XY_SOBJ-EVER
            X_SIMULATION    = CO_SIM_PERIOD          "note 1011255
       CHANGING
            XY_IZWST        = XY_OBJ-IZWST
            XY_MABLBELNR    = XY_OBJ-MABLBELNR
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
  MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_SIM_IZWST_ESTABLISH'

  IF XY_OBJ-FL-ESTB_MODE IS INITIAL.
*   Alle Einträge mit bis dato initialem Periodentyp gehören
*   zum regulären Simulationszeitraum
*   Nun korrekten Periodentyp eintragen
    LOOP AT XY_OBJ-IZWST INTO WA_IZWST
      WHERE PERTYP IS INITIAL.
      WA_IZWST-PERTYP = CO_NORMAL_BILLING.
      MODIFY XY_OBJ-IZWST FROM WA_IZWST.
    ENDLOOP.
  ELSE.
*   Muster-IZWST komprimieren und mit speziellem Periodentyp 'DE'
*   versorgen
    PERFORM TEMPLATE_IZWST_FINAL_WORK CHANGING XY_OBJ.
  ENDIF.

ENDFORM.                               " IZWST_CONSTRUCT_FOR_S_SIMUL

*&---------------------------------------------------------------------*
*&      Form  IZWST_CONSTRUCT_FOR_BUDGET_BE
*&---------------------------------------------------------------------*
FORM IZWST_CONSTRUCT_FOR_BUDGET_BE
           CHANGING XY_OBJ    TYPE ISU2A_BILLING_DATA
                    XY_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

  DATA ABR_TRIG      TYPE   ISU2A_ABR_TRIG.
  DATA COMPR_IZW     TYPE   ISU2A_BILLING_DATA-IZW.
  DATA H_ABR         TYPE   ISU2A_ABR.
  DATA IZW_1         TYPE   ISU2A_BILLING_DATA-IZW.
  DATA WA_IZWST      TYPE   ISU2A_ZWST.

* >>>>> AOP notes 424813 und 424814   " von DAIB  03.08.2001
* Die folgende Kennzeichen dienen zu der normalen Abschlagsberechnung.
* Die Änderungen werden durch die Sonderheit von Abschlagshochrechnung
* ohne Ablesungserfassung durchgeführt.
  DATA NO_MR_RESULTE LIKE   REGEN-KENNZX VALUE '1'.
  DATA KZ_VOR_EXIST  LIKE   REGEN-KENNZX VALUE 'X'.
  DATA SPECIAL_BEGIN LIKE   REGEN-KENNZX VALUE 'X'.
  DATA KZ_BEG_ADJUST LIKE   REGEN-KENNZX VALUE SPACE.
  DATA READ_ALL_EABL LIKE   REGEN-KENNZX VALUE SPACE.
* <<<<<  AOP notes 424813 und 424814.

  H_ABR-BEGABRPE = XY_OBJ-DATE-BEGPERIOD.
* h_abr-randli   = xy_obj-date-begperiod.
  H_ABR-ENDABRPE = XY_OBJ-DATE-ENDPERIOD.
* h_abr-randre   = xy_obj-date-endperiod.

* >>>>> AOP notes 424813 und 424814.
  IF NOT XY_OBJ-ABS-BBP_NO_MR IS INITIAL.
*   Die Abschlagshochrechnung darf ohne Ableseergebnisse (inkl.
*   Einzugsableseergebnisse) ausgeführt werden. Bei diesem Verfahren
*   wird der Ablauf wie S-Simulation fortgesetzt.
    CLEAR: KZ_VOR_EXIST,
           SPECIAL_BEGIN.
    KZ_BEG_ADJUST = 'X'.
    READ_ALL_EABL = 'X'.
    NO_MR_RESULTE = '0'.
  ENDIF.
* <<<<<  AOP notes 424813 und 424814.

  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_ABS    = IZW_1
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

*** read table xy_obj-izw index 1 transporting no fields.   "XX99
  READ TABLE IZW_1 INDEX 1 TRANSPORTING NO FIELDS.          "XX99
  IF SY-SUBRC <> 0. EXIT. ENDIF.

  MAC_BREAK1 'IZWST'.                  " Aufbau IZWST Aplan-Hochrechnung
  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_METER_READING_RESULTS_GET'
* Alle eventuell relevanten Ableseergebnisse einlesen
  CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
       EXPORTING
            X_IZW           = XY_OBJ-IZW
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_ABR           = H_ABR
            X_INST          = XY_SOBJ-INST
            X_VERTRAG       = XY_OBJ-ST-VERTRAG

* >>>>> AOP notes 424813 und 424814.
***         X_EABL_KZ       = '1'
            X_EABL_KZ       = NO_MR_RESULTE
* <<<<< AOP notes 424813 und 424814.

            X_IZW_1         = IZW_1
***         x_begana        = xy_obj-date-begana    " WWT X11A
* >>>>> WWT, 20.12.99
* Bem.: Lt.K.S. ist nur xy_obj-abs-begabspe immer richtig gefüllt,
*       während xy_obj-abr-begabrpe initial ist
*****       x_begana        = xy_obj-abr-begabrpe   " WWT X11A
            X_BEGANA        = XY_OBJ-ABS-BEGABSPE
* <<<<< WWT, 20.12.99
* >>>>> AOP 007, Note 159968
            X_EINZDAT       = XY_SOBJ-EVER-EINZDAT
* <<<<< AOP 007, Note 159968
            X_MAX_IEABL     = XY_SOBJ-MAX_IEABL
            X_MAX_IEABLG    = XY_SOBJ-MAX_IEABLG
       IMPORTING
            Y_IZWST         = XY_OBJ-IZWST
            Y_COMPR_IZW     = COMPR_IZW
            Y_IEABL         = XY_OBJ-IEABL
            Y_IEABLG        = XY_OBJ-IEABLG
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
  MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_METER_READING_RESULTS_GET'


  " Anfang FB 'ISU_IZWST_LE_MARGIN_DETERMINE'

* >>>>>> AOP notes 424813 und 424814.
* Wenn im berücksichtigten Zeitraum keine Ableseergebnisse eingelesen
* wurden, kann IZWST nicht vorläufig aufgebaut werden. Der Fall ist beim
* Anlegen der Abschlagsplane ohne Ableseerbenisse erlaubt. Dann wird
* die Bestimmung des linken Rands sinnlos.
  IF NO_MR_RESULTE <> '0'.
* <<<<<< AOP notes 424813 und 424814.

* IZWST teilaufbauen mit korrektem linken Rand
* abr_trig-etrg = x_etrg. " formal, eventuell ETRG unnötig
    CALL FUNCTION 'ISU_IZWST_LE_MARGIN_DETERMINE'
         EXPORTING
              X_ABR_TRIG      = ABR_TRIG
              X_PBILL         = XY_OBJ-PBILL
              X_VERTRAG       = XY_OBJ-ST-VERTRAG
***         x_izw           = xy_obj-izw                " WWT X11A
              X_IZW           = IZW_1                       " WWT X11A
              X_EINZDAT       = XY_SOBJ-EVER-EINZDAT
         CHANGING
              XY_IZWST        = XY_OBJ-IZWST
              XY_COMPR_IZW    = COMPR_IZW
         EXCEPTIONS
              GENERAL_FAULT   = 1
              BIG_CHECK_ERROR = 2
              OTHERS          = 3.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
    MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_IZWST_LE_MARGIN_DETERMINE'
*
* >>>>> AOP notes 424813 und 424814.
  ENDIF.
* <<<<< AOP notes 424813 und 424814.



  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_SIM_IZWST_ESTABLISH'
* IZWST via IZW aufbauen; Ableseergebnisse zu Beginn müssen
* vorhanden sein, d.h. KZ_VOR_EXIST = 'X' zu übergeben
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
       EXPORTING
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_VERTRAG       = XY_OBJ-ST-VERTRAG
* >>>>> AOP notes 424813 und 424814.
***         X_KZ_VOR_EXIST  = 'X'
            X_KZ_VOR_EXIST  =  KZ_VOR_EXIST
            X_KZ_BEG_ADJUST =  KZ_BEG_ADJUST
            X_READ_ALL_EABL =  READ_ALL_EABL
* <<<<< AOP notes 424813 und 424814.
***         x_izw           = xy_obj-izw               " WWT XX99
            X_IZW           = IZW_1                         " WWT XX99
            X_IEABL         = XY_OBJ-IEABL
            X_IEABLG        = XY_OBJ-IEABLG
            X_ANLAGE        = XY_OBJ-ST-ANLAGE
            X_INST          = XY_SOBJ-INST
            X_ABR           = H_ABR
            X_ST            = XY_OBJ-ST
* >>>>> AOP xxx,  Note 149045
* >>>>> AOP notes 424813 und 424814.
***         X_SPECIAL_BEGIN = 'X'
            X_SPECIAL_BEGIN = SPECIAL_BEGIN
* <<<<< AOP notes 424813 und 424814.
            X_ABRDATS_LOW   = XY_OBJ-PBILL-ERCH-ABRDATS
            X_ENDABRPE_LOW  = XY_OBJ-PBILL-ERCH-ENDABRPE
* <<<<< AOP xxx,  Note 149045
* >>>>> AOP xxx,  Note 163207
            X_ABRVORG_LOW   = XY_OBJ-PBILL-ERCH-ABRVORG
* <<<<< AOP xxx,  Note 163207
            X_EVER          = XY_SOBJ-EVER
            X_SIMULATION    = CO_SIM_BBPEXTR     "note 1011255
       CHANGING
            XY_IZWST        = XY_OBJ-IZWST
            XY_MABLBELNR    = XY_OBJ-MABLBELNR
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
  MAC_BREAK1 'SIMABL'. " Ende FB 'ISU_SIM_IZWST_ESTABLISH'

* Alle Einträge mit bis dato initialem Periodentyp gehören
* zum regulären Hochrechnungszeitraum
* Nun korrekten Periodentyp eintragen
  LOOP AT XY_OBJ-IZWST INTO WA_IZWST
    WHERE PERTYP IS INITIAL.
    WA_IZWST-PERTYP = CO_BUDGET_BILLING.
    MODIFY XY_OBJ-IZWST FROM WA_IZWST.
  ENDLOOP.

ENDFORM.                               " IZWST_CONSTRUCT_FOR_BUDGET_BE

*&---------------------------------------------------------------------*
*&      Form  IZWST_EXTEND_FOR_BPP
*&---------------------------------------------------------------------*
FORM IZWST_EXTEND_FOR_BPP USING XY_OBJ   TYPE ISU2A_BILLING_DATA
                                XY_SOBJ  TYPE ISU2A_DATA_COLLECTOR.
  INCLUDE IEAAKTIV.
  DATA:
*        Workarea für Zählwerkstabelle
         WA_IZW       TYPE   ISU2A_ZW                     ,
*        Zählwerkstabelle für Abschlagszeitraum
         BPP_IZW      LIKE   WA_IZW OCCURS 0              ,
*        Workarea für Zählerstandstabelle
         WA_IZWST     TYPE   ISU2A_ZWST                   ,
*        Zählerstandstabelle für Abschlagszeitraum
         BPP_IZWST    TYPE   ISU2A_BILLING_DATA-IZWST     .

  DATA:  SPECIAL_BEGIN  LIKE REGEN-KENNZX VALUE 'X',
         VOR_EXIST      LIKE REGEN-KENNZX VALUE 'X',
         ABRDATS_LOW    LIKE ETRG-ABRDATS,
         ENDABRPE_LOW   LIKE ERCH-ENDABRPE,
         ABRVORG_LOW    LIKE ERCH-ABRVORG,
         S_DATE         TYPE ABZEITSCH.

  MAC_BREAK1 'IZWST'.
  MAC_BREAK1 'SIMABL'. " Beginn Routine izwst_extend_for_bpp
* Für den folgenden Aufruf braucht man eine Übergabetabelle BPP_IZW,
* welche einen exakten Ausschnitt der IZW für den Abschlagszeit-
* raum darstellt.
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_ABS    = BPP_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
* Ferner muss im Fall des Abschlagszeitraums die unbearbeitete
* Originaltabelle XY_OBJ-IZWSTBF übergeben werden, da sonst einige
* der verwendeten Verarbeitungsroutinen auf falschen Datümern
* aufsetzen würden. Nur so kann die Verarbeitung für alle
* Simulationsarten einheitlich gehalten werden.
  BPP_IZWST[] = XY_OBJ-IZWSTBF[].
*
* Kennzeichen mitgeben, dass Ableseergebnisse zu Beginn des
* Abschlagszeitraums vorhanden sein müssen, zumindest, wenn
* ein Gerät durchgängig eingebaut war, KZ_VOR_EXIST = 'X'
*

* XXO IB_PS
* Bei Hochrechnung eines Zahlungsschemas bei Zwischenabrechnung
* schliesst der Hochrechnungszeitraum nicht unbedingt direkt an den
* Abrechnungszeitraum an - X_SPECIAL_BEGINN muss leer bleiben.
  IF XY_OBJ-ST-ABRVORG = CO_ZWIABR        AND
     XY_OBJ-ST-WFKKVKP-KZABSVER EQ CO_ABSVER_PS.
    IF XY_OBJ-ST-ABRDATS_PS IS INITIAL.
      CLEAR: SPECIAL_BEGIN, VOR_EXIST.
      ABRDATS_LOW  = XY_OBJ-TRIG-ETRG-ABRDATS.
      ENDABRPE_LOW = XY_OBJ-ABR-ENDABRPE.
      ABRVORG_LOW  = XY_OBJ-TRIG-ETRG-ABRVORG.
    ELSE.
      ABRDATS_LOW  = XY_OBJ-ST-ABRDATS_PS.
      ENDABRPE_LOW = XY_OBJ-ST-ENDABRPE_PS.
      ABRVORG_LOW  = XY_OBJ-ST-ABRVORG_PS.
    ENDIF.
  ELSE.
    ABRDATS_LOW  = XY_OBJ-TRIG-ETRG-ABRDATS.
    ENDABRPE_LOW = XY_OBJ-ABR-ENDABRPE.
    ABRVORG_LOW  = XY_OBJ-TRIG-ETRG-ABRVORG.
  ENDIF.
* XXO IB_PS

* If in BAdI ISU_BI_BBP_EXT_PER the begin of the estimation period
* has been changed, the fields SPECIAL_BEGIN and VOR_EXIST have to
* be cleared.
  S_DATE = xy_obj-abs-begabspe - 1.
  if s_date <> xy_obj-abr-endabrpe.
    clear: vor_exist, special_begin.
  endif.

  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_SIM_IZWST_ESTABLISH'
* Aufruf des FB 'ISU_SIM_IZWST_ESTABLISH', welcher für alle
* Simulationsarten, hier für den Abschlagszeitraum, die
* zugehörige Zählerstandstabelle erstellt
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
       EXPORTING
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_VERTRAG       = XY_OBJ-ST-VERTRAG
*           X_KZ_VOR_EXIST  = 'X'
            X_KZ_VOR_EXIST  = VOR_EXIST        "XXO IB_PS
            X_IZW           = BPP_IZW
            X_KZ_BEG_ADJUST = 'X'
            X_IEABL         = XY_OBJ-IEABL
            X_IEABLG        = XY_OBJ-IEABLG
            X_ANLAGE        = XY_OBJ-ST-ANLAGE
            X_INST          = XY_SOBJ-INST
            X_ABR           = XY_OBJ-ABR
            X_ST            = XY_OBJ-ST
* >>>>> AOP xxx,  Note 149045
*            X_SPECIAL_BEGIN = 'X'
            X_SPECIAL_BEGIN = SPECIAL_BEGIN     "XXO IB_PS
*           X_ABRDATS_LOW   = XY_OBJ-TRIG-ETRG-ABRDATS
            X_ABRDATS_LOW   = ABRDATS_LOW
*           X_ENDABRPE_LOW  = XY_OBJ-ABR-ENDABRPE
            X_ENDABRPE_LOW  = ENDABRPE_LOW
* <<<<< AOP xxx,  Note 149045
* >>>>> AOP xxx,  Note 163207
            X_ABRVORG_LOW   = ABRVORG_LOW
* <<<<< AOP xxx,  Note 163207
            X_EVER          = XY_SOBJ-EVER
            X_SIMULATION    = CO_SIM_BBPEXTR     "note 1011255
       CHANGING
            XY_IZWST        = BPP_IZWST
            XY_MABLBELNR    = XY_OBJ-MABLBELNR
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
  MAC_BREAK1 'SIMABL'. " Ende  FB 'ISU_SIM_IZWST_ESTABLISH'

* Tabelle der Zählerstände für Abschlagszeitraum in bereits
* vorhandene Tabelle IZWST einfügen und abschliessend sortieren
  LOOP AT BPP_IZWST INTO WA_IZWST.
*   Kennzeichen für Abschlagszeitraum oder
*   Vorausabrechnungszeitraumsetzen
    WA_IZWST-PERTYP = CO_BUDGET_BILLING.

    APPEND WA_IZWST TO XY_OBJ-IZWST.
  ENDLOOP.                                                  " BPP_izwst

  PERFORM SORT_IZWST CHANGING XY_OBJ-IZWST.

  MAC_BREAK1 'SIMABL'. " Ende  Routine IZWST_EXTEND_FOR_BPP

ENDFORM.                               " IZWST_EXTEND_FOR_BPP
*&---------------------------------------------------------------------*
*&      Form  IZWST_EXTEND_FOR_VOR
*&---------------------------------------------------------------------*
FORM IZWST_EXTEND_FOR_VOR USING XY_OBJ   TYPE ISU2A_BILLING_DATA
                                XY_SOBJ  TYPE ISU2A_DATA_COLLECTOR.



  DATA:
*        Workarea für Zählwerkstabelle
         WA_IZW       TYPE   ISU2A_ZW                     ,
*        Zählwerkstabelle für Vorausabrechnungszeitraum
         VOR_IZW      LIKE   WA_IZW OCCURS 0              ,
*        Workarea für Zählerstandstabelle
         WA_IZWST     TYPE   ISU2A_ZWST                   ,
*        Zählerstandstabelle für Vorausabrechnungszeitraum
         VOR_IZWST    TYPE   ISU2A_BILLING_DATA-IZWST     .

  MAC_BREAK1 'IZWST'.
  MAC_BREAK1 'SIMABL'. " Beginn Routine izwst_extend_for_bpp
* Für den folgenden Aufruf braucht man eine Übergabetabelle VOR_IZW,
* welche einen exakten Ausschnitt der IZW für den
* Vorausabrechnungszeitraum darstellt.
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_VOR    = VOR_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
* Ferner muss im Fall des Vorausabrechnungszeitraum die unbearbeitete
* Originaltabelle XY_OBJ-IZWSTBF übergeben werden, da sonst einige
* der verwendeten Verarbeitungsroutinen auf falschen Datümern
* aufsetzen würden. Nur so kann die Verarbeitung für alle
* Simulationsarten einheitlich gehalten werden.
  VOR_IZWST[] = XY_OBJ-IZWSTBF[].
*
* Kennzeichen mitgeben, dass Ableseergebnisse zu Beginn des
* Vorausabrechnungszeitraum vorhanden sein müssen, zumindest, wenn
* ein Gerät durchgängig eingebaut war, KZ_VOR_EXIST = 'X'
*
  MAC_BREAK1 'SIMABL'. " Anfang FB 'ISU_SIM_IZWST_ESTABLISH'
* Aufruf des FB 'ISU_SIM_IZWST_ESTABLISH', welcher für alle
* Simulationsarten, hier für den Vorausabrechnungszeitraum, die
* zugehörige Zählerstandstabelle erstellt
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
    EXPORTING
      X_IABLTERM      = XY_OBJ-IABLTERM
      X_VERTRAG       = XY_OBJ-ST-VERTRAG
      X_KZ_VOR_EXIST  = 'X'
      X_IZW           = VOR_IZW
      X_KZ_BEG_ADJUST = 'X'
      X_IEABL         = XY_OBJ-IEABL
      X_IEABLG        = XY_OBJ-IEABLG
      X_ANLAGE        = XY_OBJ-ST-ANLAGE
      X_INST          = XY_SOBJ-INST
      X_ABR           = XY_OBJ-ABR
      X_ST            = XY_OBJ-ST
      X_SPECIAL_BEGIN = 'X'
      X_ABRDATS_LOW   = XY_OBJ-TRIG-ETRG-ABRDATS
      X_ENDABRPE_LOW  = XY_OBJ-ABR-ENDABRPE
      X_ABRVORG_LOW   = XY_OBJ-TRIG-ETRG-ABRVORG
      X_EVER          = XY_SOBJ-EVER
      X_SIMULATION    = CO_SIM_PERIOD         "note 1011255
    CHANGING
      XY_IZWST        = VOR_IZWST
      XY_MABLBELNR    = XY_OBJ-MABLBELNR
    EXCEPTIONS
      GENERAL_FAULT   = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
* >>>>> DPC-ESTB
    IF XY_OBJ-FL-ESTB_MODE  IS INITIAL.
*     ---------------------------------------------------------------
*     Fehler aus vorangehendem FB wiederholen
      MAC_ERR_REPEAT SPACE.
*     Genauere Herkunft des Fehlers schildern ->
*     Ableseergebnisermittlung für Vorausperiode fehlerhaft...
      MAC_MSG_PUTX CO_MSG_ERROR '658' 'AJ'
         XY_OBJ-VOR-BEGVORPE XY_OBJ-VOR-ENDVORPE SPACE SPACE
         GENERAL_FAULT.
      IF 1 = 2. MESSAGE E658(AJ) WITH SPACE SPACE. ENDIF.
*     ---------------------------------------------------------------
    ELSE.
*     --------------------------------------------------------------
*     Fehler aus vorangehendem FB wiederholen
      MAC_ERR_REPEAT SPACE.
*     Genauere Herkunft des Fehlers schildern ->
*     DPC-ESTB: Ableseergebnisermittlung für Vorausperiode fehlerhaft...
      MAC_MSG_PUTX CO_MSG_ERROR '657' 'AJ'
         XY_OBJ-VOR-BEGVORPE XY_OBJ-VOR-ENDVORPE SPACE SPACE
         GENERAL_FAULT.
      IF 1 = 2. MESSAGE E657(AJ) WITH SPACE SPACE. ENDIF.
*     ---------------------------------------------------------------
    ENDIF.
* <<<<< DPC-ESTB
  ENDIF.
  MAC_BREAK1 'SIMABL'. " Ende  FB 'ISU_SIM_IZWST_ESTABLISH'

* Tabelle der Zählerstände für Vorausabrechnungszeitraum in bereits
* vorhandene Tabelle IZWST einfügen und abschliessend sortieren
  LOOP AT VOR_IZWST INTO WA_IZWST.
*   Kennzeichen für Vorausabrechnungszeitraumsetzen
    WA_IZWST-PERTYP = CO_ADVANCE_BILLING.
    APPEND WA_IZWST TO XY_OBJ-IZWST.
  ENDLOOP.                                                  " vor_izwst

  PERFORM SORT_IZWST CHANGING XY_OBJ-IZWST.

  MAC_BREAK1 'SIMABL'. " Ende  Routine IZWST_EXTEND_FOR_VOR


ENDFORM.                               " IZWST_EXTEND_FOR_VOR

*&---------------------------------------------------------------------*
*&      Form  IZWST_CONSTRUCT_FOR_BILLING
*&---------------------------------------------------------------------*
FORM IZWST_CONSTRUCT_FOR_BILLING USING
                 VALUE(X_ETRG)         LIKE  ETRG
                       XY_OBJ          TYPE  ISU2A_BILLING_DATA
                       XY_SOBJ         TYPE  ISU2A_DATA_COLLECTOR.

  DATA ABR_TRIG  TYPE ISU2A_ABR_TRIG.
  DATA COMPR_IZW TYPE ISU2A_BILLING_DATA-IZW.
  DATA IZW_1     TYPE ISU2A_BILLING_DATA-IZW.
  DATA WA_IZWST  TYPE ISU2A_ZWST.


  MAC_BREAK1 'IZWST'. " Triggerabrechnung (Abrechnungszeitraum)

  ABR_TRIG-ETRG = X_ETRG.

  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ          = XY_OBJ
    CHANGING
      XY_IZW_ABR_INF = IZW_1
    EXCEPTIONS
      GENERAL_FAULT  = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

  CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
    EXPORTING
      X_IZW         = XY_OBJ-IZW
      X_IABLTERM    = XY_OBJ-IABLTERM
      X_ABR         = XY_OBJ-ABR
      X_INST        = XY_SOBJ-INST
      X_VERTRAG     = XY_OBJ-ST-VERTRAG
      X_EABL_KZ     = '2'
      X_IZW_1       = IZW_1
      X_BEGANA      = XY_OBJ-DATE-BEGANA
      X_MAX_IEABL   = XY_SOBJ-MAX_IEABL
      X_MAX_IEABLG  = XY_SOBJ-MAX_IEABLG
      X_EINZDAT     = XY_SOBJ-EVER-EINZDAT
    IMPORTING
      Y_IZWST       = XY_OBJ-IZWST
      Y_IZWSTBF     = XY_OBJ-IZWSTBF
      Y_COMPR_IZW   = COMPR_IZW
      Y_IEABL       = XY_OBJ-IEABL
      Y_IEABLG      = XY_OBJ-IEABLG
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

  IF COMPR_IZW[] IS INITIAL.
*   RTP-Abrechnung:
*   Falls Zählwerkstabelle COMPR_IZW an dieser Stelle keine Einträge
*   mehr enthält, Sofortausstieg!
*   Es könnte sein, das in anderen (Nachberechnungs-)zeiträumen
*   noch Geräte installiert waren, die hier noch aus der IZWST
*   gelöscht werden müssen.
    REFRESH XY_OBJ-IZWST.
    EXIT.
  ENDIF.

* IZWST teilaufbauen mit korrektem linken Rand
  CALL FUNCTION 'ISU_IZWST_LE_MARGIN_DETERMINE'
    EXPORTING
      X_ABR_TRIG      = ABR_TRIG
      X_PBILL         = XY_OBJ-PBILL
      X_VERTRAG       = XY_OBJ-ST-VERTRAG
      X_IZW           = IZW_1
      X_EINZDAT       = XY_SOBJ-EVER-EINZDAT
    CHANGING
      XY_IZWST        = XY_OBJ-IZWST
      XY_COMPR_IZW    = COMPR_IZW
    EXCEPTIONS
      GENERAL_FAULT   = 1
      BIG_CHECK_ERROR = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
* Der FB 'ISU_IZWST_RI_MARGIN_DETERMINE' komplettiert die IZWST für den
* Abrechnungszeitraum und bestimmt deren rechten Rand
  CALL FUNCTION 'ISU_IZWST_RI_MARGIN_DETERMINE'
    EXPORTING
      X_IABLTERM    = XY_OBJ-IABLTERM
      X_ABR_TRIG    = ABR_TRIG
      X_VERTRAG     = XY_OBJ-ST-VERTRAG
      X_IZW         = IZW_1
      X_INST        = XY_SOBJ-INST
*     >>>>> Japan, WWT 08.11.02
      X_IEABLG      = XY_OBJ-IEABLG
      X_IEABL       = XY_OBJ-IEABL
*     <<<<< Japan, WWT 08.11.02
    CHANGING
      XY_IZWST      = XY_OBJ-IZWST
      XY_ABR        = XY_OBJ-ABR
      XY_COMPR_IZW  = COMPR_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

* Alle Einträge mit bis dato initialem Periodentyp gehören
* zum regulären Abrechnungszeitraum
* Nun korrekten Periodentyp eintragen
  LOOP AT XY_OBJ-IZWST INTO WA_IZWST
    WHERE PERTYP IS INITIAL.
    WA_IZWST-PERTYP = CO_NORMAL_BILLING.
    MODIFY XY_OBJ-IZWST FROM WA_IZWST.
  ENDLOOP.

ENDFORM.                               " IZWST_CONSTRUCT_FOR_BILLING

*&---------------------------------------------------------------------*
*&      Form  IGER_ADJUST
*&---------------------------------------------------------------------*
*orm iger_adjust using xy_obj type isu2a_billing_data.
*
* data h_iger type isu2a_billing_data-iger.
* data wger   type isu2a_ger.
* data wger2  type isu2a_ger.
*
* Es wird hier die IGER an den Abrechnungszeitraum (und ggf. an den
* Abschlagszeitraum) angepaßt.
* Zunächst werden alle IGER-Einträge ab dem Beginn der
* Abrechnungsperiode selektiert (also alle außer die der
* Nachberechnungszeiträume). Diese werden zwischengespeichert
* und aus der IGER entfernt.
* loop at xy_obj-iger into wger
*   where ab  >= xy_obj-date-begperiod.
*   append wger to h_iger.
*   delete xy_obj-iger.
* endloop.                             "xy_obj-iger
* Verarbeitung der Einträge
* loop at h_iger into wger.
*   if wger-ab  <= xy_obj-abr-endabrpe.
*     Eintrag schneidet den Abrechnungszeitraum
*     wger2 = wger.
*     Zeitraum anpassen
*     if wger2-bis > xy_obj-abr-endabrpe.
*       wger2-bis = xy_obj-abr-endabrpe.
*     endif.
*     Kennzeichen für Abrechnungszeitraum
*     wger2-pertyp = co_normal_billing.
*     angepaßten Eintrag übernehmen
*     append wger2 to xy_obj-iger.
*   endif.
*   if not xy_obj-abs-bbp_sim is initial.
*     Es soll eine verbrauchsabhängige Abschlagshochrechnung
*     durchgeführt werden
*     if wger-bis >= xy_obj-abs-begabspe and
*        wger-ab  <= xy_obj-abs-endabspe.
*       Eintrag schneidet den Abschlagszeitraum
*       wger2 = wger.
*       Zeitraum anpassen
*       if wger2-ab < xy_obj-abs-begabspe.
*         wger2-ab = xy_obj-abs-begabspe.
*       endif.
*       if wger2-bis > xy_obj-abs-endabspe.
*         wger2-bis = xy_obj-abs-endabspe.
*       endif.
*       Kennzeichen für Abschlagszeitraum setzen
*       wger2-pertyp = co_budget_billing.
*       angepaßten Eintrag übernehmen
*       append wger2 to xy_obj-iger.
*     endif.
*   endif.
* endloop.
*
*ndform.                               " IGER_ADJUST
*&---------------------------------------------------------------------*
*&      Form  IMONTH_CREATE
*&---------------------------------------------------------------------*
FORM IMONTH_CREATE USING XY_OBJ TYPE ISU2A_BILLING_DATA.

  DATA H_ISS    TYPE ISU2A_ISS.
  DATA WSS      TYPE ISU2A_SS.
  DATA WZEIT    TYPE ISU2A_ZEIT.
  DATA WABRZEIT TYPE ISU2A_ABRZEIT.

  MAC_BREAK1 'IMONTH'.

* Verarbeitung je IABRZEIT-Zeitraum
  LOOP AT XY_OBJ-IABRZEIT INTO WABRZEIT.
*   Es werden alle ISS-Einträge des Zeitraums selektiert.
*   Für diese Einträge wird die IMONTH aufgebaut.
    REFRESH H_ISS.
    LOOP AT XY_OBJ-IZEIT INTO WZEIT
      WHERE NO_ABRZEIT = WABRZEIT-NO_ABRZEIT.
      IF NOT WZEIT-CSFROM IS INITIAL.
*       u.U. enthält die Korrekturperiode keinen Schritt der
*       nachzuberechnen ist; in diesem Fall ist die ISS für diesen
*       Zeitraum leer
        READ TABLE XY_OBJ-ISS TRANSPORTING NO FIELDS
             WITH KEY CSNO = WZEIT-CSFROM
             BINARY SEARCH.
        MAC_ERR_READ 'ISS' SPACE 'IMONTH_CREATE'.
        LOOP AT XY_OBJ-ISS INTO WSS FROM SY-TABIX.
          APPEND WSS TO H_ISS.
          IF WSS-CSNO = WZEIT-CSTO.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*   Aufbau IMONTH
    CALL FUNCTION 'ISU_IMONTH_CREATE'
      EXPORTING
        X_ISS         = H_ISS
        X_STICHTAG    = XY_OBJ-SCHEDULE-STICHTAG
        X_AB          = WABRZEIT-AB
        X_BIS         = WABRZEIT-BIS
      CHANGING
        XY_IMONTH     = XY_OBJ-IMONTH
      EXCEPTIONS
        GENERAL_FAULT = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
  ENDLOOP.
* wegen performanterem Zugriff
  SORT XY_OBJ-IMONTH BY CSNO YEAR MONTH.

ENDFORM.                               " IMONTH_CREATE
*&---------------------------------------------------------------------*
*&      Form  MSG_BREAK_POINT_OFF
*&---------------------------------------------------------------------*
FORM MSG_BREAK_POINT_OFF.

  CALL FUNCTION 'MSG_ACTION'
    EXPORTING
      X_ACTION             = CO_MSG_BREAK_POINT_OFF
    EXCEPTIONS
      ACTION_NOT_SUPPORTED = 1
      HANDLE_INVALID       = 2
      NOT_FOUND            = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MAC_MSG_OTHERS SY-SUBRC 'MSG_ACTION'.
  ENDIF.

ENDFORM.                               " MSG_BREAK_POINT_OFF
*&---------------------------------------------------------------------*
*&      Form  SCHEDULING_DATA
*&---------------------------------------------------------------------*
*       Hier werden alle in der Abrechnung benötigten Daten der
*       Terminsteuerung ermittelt.
*       Dabei sind folgende Zusammenhänge zu berücksichtigen:
*       - Der Zugriff auf die Terminsteuerung erfolgt immer in
*         Abhängigkeit der Daten aus dem Trigger (Portion,
*         Ableseeinheit, Adatsoll). Änderungen an diesen Daten
*         nach der Vorbereitung finden keine Berücksichtigung.
*       - Aus den Daten Portion, Ableseeinheit und Adatsoll
*         ergibt sich die Zuordnung der Abrechnung zu der
*         geplanten Abrechnungsperiode (Terminsatz der Portion)
*         und der Ableseperiode (insbesondere das nächste Adatsoll,
*         welches für die Abschlagshochrechnung benötigt wird).
*       - Es wird hier auch das Termtdat des Terminsatzes der
*         Portion zur aktuellen Abrechnungsperiode ermittelt.
*         Dieses Datum wird in den Beleg geschrieben. Das Datum
*         wird in der Fakturierung benötigt, um zu einer
*         Turnusabrechnung die zu verrechnenden Abschlagspläne
*         zu ermitteln. (Dieses Datum ist nur für Turnusabrechnungen
*         relevant.)
*----------------------------------------------------------------------*
FORM SCHEDULING_DATA CHANGING XY_OBJ  TYPE ISU2A_BILLING_DATA
                              XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

  DATA WTE420  LIKE TE420.
  DATA WTE422  LIKE TE422.
  DATA WEANL   LIKE V_EANL.

* Ableseeinheit bestimmen
  IF NOT XY_OBJ-TRIG-ETRG-ABLEINH IS INITIAL.
*   Ableseeinheit aus Trigger entnehmen
    XY_OBJ-SCHEDULE-ABLEINH = XY_OBJ-TRIG-ETRG-ABLEINH.
  ELSE.
*   Ableseeinheit aus Anlage entnehmen
    LOOP AT XY_SOBJ-INST-IEANL INTO WEANL
      WHERE AB  <= XY_OBJ-DATE-ENDPERIOD
      AND   BIS >= XY_OBJ-DATE-ENDPERIOD.
      EXIT.
    ENDLOOP.
    MAC_ERR_READ 'IEANL' SPACE 'SCHEDULING_DATA'.
    XY_OBJ-SCHEDULE-ABLEINH = WEANL-ABLEINH.
  ENDIF.
* Daten zur Ableseeinheit ermitteln
  PERFORM MR_UNIT_SELECT USING    XY_OBJ-SCHEDULE-ABLEINH
                         CHANGING WTE422.
* Stichtag der Ableseeinheit merken
  XY_OBJ-SCHEDULE-STICHTAG = WTE422-STICHTAG.
* Kalender ID merken
  XY_OBJ-SCHEDULE-IDENT    = WTE422-IDENT.
* Portion bestimmen
  IF NOT XY_OBJ-TRIG-ETRG-PORTION IS INITIAL.
*   Portion aus Trigger entnhemen.
    XY_OBJ-SCHEDULE-PORTION = XY_OBJ-TRIG-ETRG-PORTION.
  ELSEIF NOT XY_SOBJ-EVER-PORTION IS INITIAL.
*   Portion aus Vertrag entnehmen
    XY_OBJ-SCHEDULE-PORTION = XY_SOBJ-EVER-PORTION.
  ELSE.
*   Portion aus Ableseeinheit entnehmen
    XY_OBJ-SCHEDULE-PORTION = WTE422-PORTION.
  ENDIF.
* Daten zur Portion ermitteln
  PERFORM PORTION_SELECT USING    XY_OBJ-SCHEDULE-PORTION
                         CHANGING WTE420.
* Abschlagszyklus der Portion merken
  XY_OBJ-SCHEDULE-ABSZYK = WTE420-ABSZYK.
* Periodenlänge und -typ
  XY_OBJ-SCHEDULE-PERIODEW = WTE420-PERIODEW.
  XY_OBJ-SCHEDULE-PERIODET = WTE420-PERIODET.
* Karenz für 30-D-P billing
  XY_OBJ-SCHEDULE-PTOLERFROM = WTE420-PTOLERFROM.
  XY_OBJ-SCHEDULE-PTOLERTO   = WTE420-PTOLERTO.
* Art des erweiterten Karenzverfahrens
  XY_OBJ-SCHEDULE-INTCNTRTYPE = WTE420-INTCNTRTYPE.
  IF ( XY_OBJ-ST-ABRVORG =  CO_TURABR
  AND  XY_OBJ-FL-SIMULATION <> CO_SIM_BSPRORAT )
* Realize billing in advance for DPC interim billing
  OR ( XY_OBJ-ST-ABRVORG = CO_ZWIABR
       AND XY_OBJ-EAP_ETTA-BIADVANCE = 'X'
       AND NOT XY_OBJ-EAP_ETTA-DYPERCON IS INITIAL
       AND ( XY_OBJ-FL-SIMULATION = CO_SIM_NO
             OR XY_OBJ-FL-SIMULATION = CO_SIM_BSPRORAT
             OR XY_OBJ-FL-SIMULATION = CO_SIM_TRIGGER  ) ).
*   Spezielle Daten aus der Terminsteuerung für eine Turnusabrechnung
    PERFORM PERIODIC_BILLING_DATA USING    WTE420
                                           XY_SOBJ
                                  CHANGING XY_OBJ.
* XXO ZWEA
  ELSEIF     XY_OBJ-ST-ABRVORG = CO_ZWIABR        AND
       XY_OBJ-EAP_ETTA-PERENDBI NE CO_PEBI_NO     AND
*      XY_OBJ-FL-SIMULATION     NE CO_SIM_BIGCHECK.
       ( XY_OBJ-FL-SIMULATION = CO_SIM_NO
         OR XY_OBJ-FL-SIMULATION = CO_SIM_BSPRORAT
         OR XY_OBJ-FL-SIMULATION = CO_SIM_TRIGGER  ).
    PERFORM INTERIM_PEB_BILL_DATA CHANGING XY_OBJ.
* XXO ZWEA
  ELSEIF     XY_OBJ-ST-ABRVORG = CO_ZWIABR        AND
         NOT XY_OBJ-TRIG-ETRG-ABSCHLPAN IS INITIAL.
*   Spezielle Daten aus der Terminsteuerung für eine Zwischenabrechnung
*   mit Abschlagsplananpassung
    PERFORM INTERIM_BILLING_DATA CHANGING XY_OBJ.
  ELSEIF XY_OBJ-ST-ABRVORG = CO_SCHABR OR    "Schlußabrechnung
         XY_OBJ-ST-ABRVORG = CO_GEBABR OR    "Gebietsabgabe
         XY_OBJ-ST-ABRVORG = CO_VWEABR OR                   "Dereg
         XY_OBJ-ST-ABRVORG = CO_WDEBIT."Debitorenwechsel
*   Durch die Abrechnungsvorgänge wird das Ende der Nach- bzw.
*   Endabrechnungsperiode erzwungen
    PERFORM FINAL_BILLING_DATA CHANGING XY_OBJ.
  ENDIF.
* Zuordnungsdatum füllen, sofern noch nicht geschehen
  IF XY_OBJ-SCHEDULE-ZUORDDAT IS INITIAL.
    XY_OBJ-SCHEDULE-ZUORDDAT = XY_OBJ-TRIG-ETRG-ZUORDDAT.
  ENDIF.
* Lösung für Variante IF04, damit ZUORDDA immer gefüllt
  IF XY_OBJ-SCHEDULE-ZUORDDAT IS INITIAL.
    XY_OBJ-SCHEDULE-ZUORDDAT = XY_OBJ-ABR-ENDABRPE.
  ENDIF.
  IF XY_OBJ-SCHEDULE-ZUORDDAT IS INITIAL.
    XY_OBJ-SCHEDULE-ZUORDDAT = XY_OBJ-ABS-ENDABSPE.
  ENDIF.
* Wert für die Abschlagshochrechnung Entsorgungswirtschaft bestimmen
  IF NOT XY_SOBJ-EVER-EXTRAPOLWASTE IS INITIAL.
*   Wert aus Vertrag entnehmen
    XY_OBJ-SCHEDULE-EXTRAPOLWASTE = XY_SOBJ-EVER-EXTRAPOLWASTE.
  ELSEIF NOT WTE420-EXTRAPOLWASTE IS INITIAL.
*   Wert aus Portion entnhemen.
    XY_OBJ-SCHEDULE-EXTRAPOLWASTE = WTE420-EXTRAPOLWASTE.
  ENDIF.

ENDFORM.                               " SCHEDULING_DATA
*&---------------------------------------------------------------------*
*&      Form  ABSZYK_DETERMINE
*&---------------------------------------------------------------------*
FORM ABSZYK_DETERMINE USING XY_OBJ  TYPE ISU2A_BILLING_DATA
                            XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

  DATA: EXTR_YES LIKE REGEN-KENNZX,
        ABSZYK   LIKE TE420-ABSZYK.

* Abschlagsverfahren aus Vertragskonto zieht vor EVER-ABSZYK
  IF XY_OBJ-ST-WFKKVKP-KZABSVER EQ CO_ABSVER_NULL
    OR XY_OBJ-ST-WFKKVKP-KZABSVER EQ CO_ABSVER_BBP.
    XY_OBJ-ABS-ABSZYK = CO_BBP_NO.
  ELSEIF XY_OBJ-ST-WFKKVKP-KZABSVER EQ CO_ABSVER_PS.
*   Bei Zahlungsschema soll immer ein Hochrechnungszeitraum
*   aufgebaut werden, es sei denn im Vertrag sitzt ABSZYK = '00'.
    IF XY_SOBJ-EVER-ABSZYK EQ CO_BBP_NO.
      XY_OBJ-ABS-ABSZYK = CO_BBP_NO.
    ELSE.
      XY_OBJ-ABS-ABSZYK = CO_BBP_ONE.
    ENDIF.
  ELSE.
    ABSZYK = XY_OBJ-SCHEDULE-ABSZYK.
    CALL FUNCTION 'ISU_BBP_EXTRAPOLATION_CHECK'
      EXPORTING
        X_EVER_ABSZYK  = XY_SOBJ-EVER-ABSZYK
        X_EVER_PYPLT   = XY_SOBJ-EVER-PYPLT
        X_TE420_ABSZYK = ABSZYK
      IMPORTING
        Y_EXTR_YES     = EXTR_YES.
    IF NOT EXTR_YES IS INITIAL.
      IF XY_SOBJ-EVER-ABSZYK IS INITIAL.
*       Abschlagszyklus aus der Terminsteuerung
        XY_OBJ-ABS-ABSZYK = XY_OBJ-SCHEDULE-ABSZYK.
      ELSE.
*       Abschlagszyklus aus dem Vertrag
        XY_OBJ-ABS-ABSZYK = XY_SOBJ-EVER-ABSZYK.
      ENDIF.
    ELSE.
      XY_OBJ-ABS-ABSZYK = CO_BBP_NO.
    ENDIF.
  ENDIF.

ENDFORM.                               " ABSZYK_DETERMINE
*&---------------------------------------------------------------------*
*&      Form  BBP_EXTRAPOLATION_CONTROL
*&---------------------------------------------------------------------*
FORM BBP_EXTRAPOLATION_CONTROL
                       CHANGING XY_OBJ  TYPE ISU2A_BILLING_DATA
                                XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

  DATA: subrc  LIKE sy-subrc.
* Abschlagszyklus bestimmen
  PERFORM ABSZYK_DETERMINE USING XY_OBJ
                                 XY_SOBJ.
* Steuerung der Abschlagsberechnung in Abhängigkeit des Abschlagszyklus
  IF   XY_OBJ-ABS-ABSZYK        <> CO_BBP_NO     AND
     ( XY_OBJ-TRIG-ETRG-ABRVORG =  CO_TURABR OR
       XY_OBJ-TRIG-ETRG-ABRVORG =  CO_ZWIABR    ).
*   Den Zeitraum für die Abschlagshochrechnung festlegen
    IF XY_OBJ-TRIG-ETRG-ABRVORG =  CO_ZWIABR.
*     Bei einer Zwischenabrechnung wird nur dann eine
*     Abschlagshochrechnung durchgeführt, wenn im Trigger das
*     Kennzeichen zur Abschlagsplananpassung gesetzt ist
      CHECK NOT XY_OBJ-TRIG-ETRG-ABSCHLPAN IS INITIAL.
    ENDIF.
*   Beginn des Abschlagszeitraum
    IF XY_OBJ-ABS-BEGABSPE IS INITIAL.
*     Bei Zwischenabrechnung mit Abschlagsplananpassung und
*     Zahlungsschema wird BEGABSPE schon vorbelegt.
      XY_OBJ-ABS-BEGABSPE = XY_OBJ-ABR-ENDABRPE + 1.
    ENDIF.
*   Ende des Abschlagszeitraum --> nächstes Sollablesedatum
    IF XY_OBJ-ABS-ENDABSPE IS INITIAL.
*     Bei Zwischenabrechnung mit Abschlagsplananpassung und
*     Zahlungsschema wird ENDABSPE schon vorbelegt.
      XY_OBJ-ABS-ENDABSPE = XY_OBJ-SCHEDULE-NEXT_ADATSOLL.
*   >>>>> Japan, WWT 12.11.2002
*     Possibly adjustment of ENDABSPE due to SBP/JBP
      PERFORM JBP_ENDPER_SET USING XY_OBJ-TRIG-ETRG-ABRVORG
                                   XY_OBJ-SCHEDULE-NEXT_ADATSOLL
                          CHANGING XY_OBJ-ABS-ENDABSPE.
*   <<<<< Japan, WWT 12.11.2002
    ENDIF.
*   Call BAdI to change the calculated begin and end of estimation
*   period.
    IF NOT cl_isu_bi_exithandler=>ref_exit_bbp_per IS INITIAL.
*     switch off writing into protocoll - any error message written
*     within the BAdI should not be written into protocoll
      CALL FUNCTION 'MSG_ACTION'
        EXPORTING
          x_action = co_msg_log_off.

      CALL METHOD
        cl_isu_bi_exithandler=>ref_exit_bbp_per->CHANGE_PERIOD
        EXPORTING
          X_OBJ         = xy_obj
          X_SOBJ        = xy_sobj
        CHANGING
          XY_BEGBBPPE   = xy_obj-abs-begabspe
          XY_ENDBBPPE   = xy_obj-abs-endabspe
        EXCEPTIONS
          GENERAL_FAULT = 1
          others        = 2.
      subrc = sy-subrc.
*     switch on protocol again
      CALL FUNCTION 'MSG_ACTION'
        EXPORTING
          x_action = co_msg_log_on.
      IF subrc <> 0.
        mac_err sy-msgno sy-msgid sy-msgv1
          sy-msgv2 sy-msgv3 sy-msgv4 general_fault.
      ENDIF.
*     if in this BAdI one of the dates has been cleared, no
*     budget billing plan should be built.
      IF XY_OBJ-ABS-BEGABSPE IS INITIAL
        OR XY_OBJ-ABS-ENDABSPE IS INITIAL.
        CLEAR XY_OBJ-ABS.
        XY_OBJ-ABS-ABSZYK = CO_BBP_NO.
        EXIT.
      ENDIF.
    ENDIF.
*   Prüfen, ob der Abschlagszeitraum korrekt ist
    IF XY_OBJ-ABS-BEGABSPE > XY_OBJ-ABS-ENDABSPE.
*     Es wurde kein gültiger Abschlagszeitraum ermittelt
      MAC_ERR '099' 'AJ' XY_OBJ-ABS-BEGABSPE XY_OBJ-ABS-ENDABSPE
                    XY_OBJ-TRIG-ETRG-ABLEINH XY_OBJ-TRIG-ETRG-PORTION
                    GENERAL_FAULT.
    ENDIF.
*   Prüfen, ob der Vertrag für den gesamten Abschlagszeitraum
*   gültig ist
    PERFORM CONTRACT_DATES_CHECK USING    XY_SOBJ-EVER
                                          XY_OBJ-FL-SIMULATION
                                          'X'
                                 CHANGING XY_OBJ-ABS-BEGABSPE
                                          XY_OBJ-ABS-ENDABSPE.
*   Wenn im obigen Form die Daten gecleart wurden, darf kein
*   Abschlag aufgebaut werden.
    IF XY_OBJ-ABS-BEGABSPE IS INITIAL
      AND XY_OBJ-ABS-ENDABSPE IS INITIAL.
      CLEAR XY_OBJ-ABS.
      XY_OBJ-ABS-ABSZYK = CO_BBP_NO.
      EXIT.
    ENDIF.
    IF XY_OBJ-EAP_ETTA-ABSSTEU = CO_BBPE_SIMULATION.
*     Es muß eine Abschlagshochrechnung erfolgen
      XY_OBJ-ABS-BBP_SIM = 'X'.
    ENDIF.
*   Prüfen, ob eine Abschlagshochrechnung zulässig ist
    PERFORM BBP_EXTRAPOLATION_CHECK CHANGING XY_OBJ.
  ELSE.
*   Nur für den Fall, dass Daten schon vorbelegt waren (s.o.)
    CLEAR: XY_OBJ-ABS-BEGABSPE, XY_OBJ-ABS-ENDABSPE.
  ENDIF.

ENDFORM.                               " BBP_EXTRAPOLATION_CONTROL
*&---------------------------------------------------------------------*
*&      Form  BI_ADVANCE_CONTROL
*&---------------------------------------------------------------------*
FORM BI_ADVANCE_CONTROL
                CHANGING XY_OBJ  TYPE ISU2A_BILLING_DATA
                         XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

* Steuerung der Vorausabrechnung in Abhängigkeit Kz. im Tariftyp
*  if not xy_obj-eap_etta-biadvance is initial
*  and    xy_obj-trig-etrg-abrvorg  =  co_turabr.
  IF NOT XY_OBJ-EAP_ETTA-BIADVANCE IS INITIAL
  AND (   XY_OBJ-TRIG-ETRG-ABRVORG  =  CO_TURABR  OR
        ( XY_OBJ-TRIG-ETRG-ABRVORG  =  CO_ZWIABR
          AND NOT XY_OBJ-EAP_ETTA-DYPERCON  IS INITIAL ) ).
*   Beginn des Vorausabrechnungszeitraums
    XY_OBJ-VOR-BEGVORPE = XY_OBJ-ABR-ENDABRPE + 1.
*   Ende des Vorausberechnungszeitraums --> nächstes Sollablesedatum
    XY_OBJ-VOR-ENDVORPE = XY_OBJ-SCHEDULE-NEXT_ADATSOLL.
*   Possibly adjustment of VOR-ENDVORPE due to SBP/JBP
    PERFORM JBP_ENDPER_SET USING XY_OBJ-TRIG-ETRG-ABRVORG
                                 XY_OBJ-SCHEDULE-NEXT_ADATSOLL
                        CHANGING XY_OBJ-VOR-ENDVORPE.
*   Falls ein Auszug in der Zukunft vorliegt und das Auszugsdatum
*   vor dem nächsten Sollablesedatum liegt, wird dieses als Ende
*   des Vorausabrechnungszeitraums gesetzt.
    IF XY_OBJ-VOR-ENDVORPE > XY_SOBJ-EVER-AUSZDAT.
      XY_OBJ-VOR-ENDVORPE = XY_SOBJ-EVER-AUSZDAT.
    ENDIF.
*   Prüfen, ob der Vorausabrechnungszeitraum korrekt ist
    IF XY_OBJ-VOR-BEGVORPE > XY_OBJ-VOR-ENDVORPE.
      MAC_ERR '451' 'AJ' XY_OBJ-VOR-BEGVORPE XY_OBJ-VOR-ENDVORPE
                    XY_OBJ-TRIG-ETRG-ABLEINH XY_OBJ-TRIG-ETRG-PORTION
                    GENERAL_FAULT.
      IF 1 = 2. MESSAGE E451(AJ) WITH SPACE SPACE SPACE SPACE. ENDIF.
    ENDIF.
*   Prüfen, ob der Vertrag für den gesamten Vorausabrechnungszeitraum
*   gültig ist
*   über den dritten Parameter könnte man noch steuern, dass keine
*   Vorausabrechnung durchgeführt wird, wenn der Vertrag nicht für
*   den gesamten Zeitraum gültig ist.
    PERFORM CONTRACT_DATES_CHECK USING    XY_SOBJ-EVER
                                          XY_OBJ-FL-SIMULATION
                                          SPACE    "s.o.
                                 CHANGING XY_OBJ-VOR-BEGVORPE
                                          XY_OBJ-VOR-ENDVORPE.
*   Es muß eine Vorausabrechnung erfolgen
    XY_OBJ-VOR-VORBI = 'X'.
*   Prüfen, ob eine Vorausabrechnung zulässig ist
*    PERFORM BI_ADVANCE_CHECK CHANGING XY_OBJ.
  ENDIF.

ENDFORM.                               " BI_ADVANCE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  METER_READING_DATA_ANALYZE
*&---------------------------------------------------------------------*
FORM METER_READING_DATA_ANALYZE
                     CHANGING XY_OBJ         TYPE  ISU2A_BILLING_DATA
                              XY_SOBJ        TYPE  ISU2A_DATA_COLLECTOR.


* Falls DPC mit Schätzung der Ableseergebnisse in der Abrechnung
* involviert ist, muss der Aufbau der Ableseergebnisse später
* erfolgen.
  CHECK XY_OBJ-FL-ESTB_MODE IS INITIAL.

* Normale Verarbeitung der Ableseergebnisse für aktuelle Turnusperiode
  CALL FUNCTION 'ISU_METER_READING_DATA_ANALYZE'
    CHANGING
      XY_OBJ        = XY_OBJ
      XY_SOBJ       = XY_SOBJ
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ENDIF.

ENDFORM.                               " METER_READING_DATA_ANALYZE

*&---------------------------------------------------------------------*
*&      Form  BBP_METER_READING_DATA_ANALYZE
*&---------------------------------------------------------------------*
FORM BBP_METER_READING_DATA_ANALYZE
                       CHANGING XY_OBJ  TYPE ISU2A_BILLING_DATA
                                XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

  IF NOT XY_OBJ-ABS-BBP_SIM IS INITIAL.
*  Kombination DPC-ESTB mit Abschlag z.Zt.verbieten
*    IF NOT xy_obj-fl-estb_mode IS INITIAL.
**     Dynamische Periodenperiodensteuerung &1 geht nicht mit Abschlag
*      mac_msg_putx co_msg_error '655' 'AJ' xy_obj-eap_etta-dypercon
*      space space space general_fault.
*      IF 1 = 2. MESSAGE e655(aj). ENDIF.
*    ENDIF.
    IF   XY_OBJ-FL-ESTB_MODE IS INITIAL  OR
     (   XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_ORIG            AND
         XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_CHANGED_BY_MASS AND
         XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_SET_MANUAL       ).
*     ---------------------------------------------------------------
*     Normalfall OHNE DPC-ESTB oder DPC-ESTB mit scharf definiertem
*     rechten Rand ( d.h. ALLE Ableseergebnisse zum aktuellen
*     Abrechnungsauftrag auf DB EABL vorhanden )
*     Ausdehnung der IZWST auf Abschlagszeitraum
      PERFORM IZWST_EXTEND_FOR_BPP USING XY_OBJ
                                         XY_SOBJ.
*     ----------------------------------------------------------------
    ELSE.
*     ----------------------------------------------------------------
*     Besondere Verarbeitung bei DPC-ESTB mit evtl. unscharfem rechten
*     Rand ( d.h. NICHT NOTWENDIG alle Ableseergebnisse zum aktuellen
*     Abrechnungsauftrag auf DB EABL vorhanden )
      PERFORM DPC_BPP_IZWST_VIA_SIM_GET CHANGING XY_OBJ XY_SOBJ.
*     ----------------------------------------------------------------
    ENDIF.
  ENDIF.

ENDFORM.                               " BBP_METER_READING_DATA_ANALYZE

*&---------------------------------------------------------------------*
*&      Form  ADV_METER_READING_DATA_ANALYZE
*&---------------------------------------------------------------------*
FORM ADV_METER_READING_DATA_ANALYZE
                       CHANGING XY_OBJ  TYPE ISU2A_BILLING_DATA
                                XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.

* >>>>> DPC-ESTB  Neubearbeitung
  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK


* Verarbeitung nur bei Vorausabrechnung
  CHECK NOT XY_OBJ-VOR-VORBI IS INITIAL.

  IF   XY_OBJ-FL-ESTB_MODE IS INITIAL  OR
      (  XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_ORIG            AND
         XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_CHANGED_BY_MASS AND
         XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_SET_MANUAL       ).
*   ---------------------------------------------------------------
*   Normalfall OHNE DPC-ESTB oder DPC-ESTB mit scharf definiertem
*   rechten Rand ( d.h. ALLE Ableseergebnisse zum aktuellen
*   Abrechnungsauftrag auf DB EABL vorhanden )
*   Ausdehnung der IZWST auf Vorausabrechnungszeitraum
    PERFORM IZWST_EXTEND_FOR_VOR USING XY_OBJ
                                       XY_SOBJ.
*   ----------------------------------------------------------------
  ELSE.
*   ----------------------------------------------------------------
*   Besondere Verarbeitung bei DPC-ESTB mit evtl. unscharfem rechten
*   Rand ( d.h. NICHT NOTWENDIG alle Ableseergebnisse zum aktuellen
*   Abrechnungsauftrag auf DB EABL vorhanden )
    PERFORM DPC_ADV_IZWST_VIA_SIM_GET CHANGING XY_OBJ XY_SOBJ.
*   ----------------------------------------------------------------
  ENDIF.


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK
* <<<<< DPC-ESTB

ENDFORM.                               " ADV_METER_READING_DATA_ANALYZE

*&---------------------------------------------------------------------*
*&      Form  BILLING_BLOCK
*&---------------------------------------------------------------------*
FORM BILLING_BLOCK
                   CHANGING XY_OBJ    TYPE ISU2A_BILLING_DATA
                            XY_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

* Bei Simulation Warnung
  IF NOT XY_OBJ-FL-SIMULATION IS INITIAL.
    PERFORM BILLING_BLOCK_CHECK USING 'X' CHANGING XY_OBJ XY_SOBJ.
  ELSE.
    PERFORM BILLING_BLOCK_CHECK USING SPACE CHANGING XY_OBJ XY_SOBJ.
  ENDIF.

ENDFORM.                               " BILLING_BLOCK

*&---------------------------------------------------------------------*
*&      Form  BILLING_BLOCK_CHECK
*&---------------------------------------------------------------------*
FORM BILLING_BLOCK_CHECK
                   USING    VALUE(X_WARNING) TYPE KENNZX
                   CHANGING XY_OBJ    TYPE ISU2A_BILLING_DATA
                            XY_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

  DATA WTE021 TYPE TE021.
* edited by robert
  clear: XY_SOBJ-EVER-ABRSPERR,
         XY_SOBJ-EVER-MANABR,
         XY_OBJ-ST-ABRSPERR.

* Prüfen, ob der Vertrag zur Abrechnung gesperrt ist
  IF NOT XY_OBJ-ST-ABRSPERR IS INITIAL.
*   Die Abrechnungssperre zieht nur dann, wenn keine Freigabe
*   erfolgt ist
    IF XY_OBJ-ST-ABRFREIG IS INITIAL.
      CALL FUNCTION 'ISU_DB_TE021_SINGLE'
        EXPORTING
          X_ABRSPERR = XY_OBJ-ST-ABRSPERR
        IMPORTING
          Y_TE021    = WTE021
        EXCEPTIONS
          OTHERS     = 1.
      IF SY-SUBRC <> 0.
        MAC_ERR_REPEAT_BC GENERAL_FAULT.                    "#EC *
      ENDIF.
      IF     XY_OBJ-FL-SIMULATION = CO_SIM_BSPRORAT
         AND NOT WTE021-MSIM_OK IS INITIAL.
        X_WARNING = 'X'.
      ELSEIF     XY_OBJ-FL-SIMULATION = CO_SIM_BBPEXTR
             AND NOT WTE021-BBP_OK IS INITIAL.
        X_WARNING = 'X'.
      ENDIF.
      IF NOT CL_ISU_BI_EXITHANDLER=>EXIT_BLOCK_BILLING IS INITIAL.
        CALL METHOD
          CL_ISU_BI_EXITHANDLER=>EXIT_BLOCK_BILLING->BLOCK_BILLING_CHECK
          EXPORTING
            X_TE021      = WTE021
            X_SIMULATION = XY_OBJ-FL-SIMULATION
            X_EVER       = XY_SOBJ-EVER
            X_EVERH_TAB  = XY_SOBJ-IEVERH
            X_IEANL      = XY_SOBJ-INST-IEANL
            X_ST         = XY_OBJ-ST
          CHANGING
            XY_WARNING   = X_WARNING.
      ENDIF.
      IF X_WARNING IS INITIAL.
*       Der Vertrag &1 ist zur Abrechnung gesperrt
        IF 1 = 2. MESSAGE E063(AJ) WITH SPACE SPACE. ENDIF.
        MAC_ERR '063' 'AJ' XY_OBJ-ST-VERTRAG XY_OBJ-ST-ABRSPERR SPACE
                           SPACE GENERAL_FAULT.
      ELSE.
*       Der Vertrag &1 ist zur Abrechnung gesperrt
        IF 1 = 2. MESSAGE W303(AJ) WITH SPACE SPACE. ENDIF.
        MAC_WAR_BC '303' 'AJ'
           XY_OBJ-ST-VERTRAG XY_OBJ-ST-ABRSPERR
           SPACE SPACE GENERAL_FAULT.                       "#EC *
      ENDIF.
    ENDIF.
  ENDIF.
* Prüfen, ob der Vertrag nur manuelle abgerechnet werden darf
  IF NOT XY_SOBJ-EVER-MANABR IS INITIAL.
*   Der Vertrag &1 ist zur maschinellen Abrechnung gesperrt
    IF 1 = 2. MESSAGE E087(AJ) WITH SPACE. ENDIF.
    MAC_ERR '087' 'AJ' XY_OBJ-ST-VERTRAG SPACE SPACE SPACE
                       GENERAL_FAULT.
  ENDIF.

ENDFORM.                               " BILLING_BLOCK_CHECK
*&---------------------------------------------------------------------*
*&      Form  CONCCNTR_READ
*&---------------------------------------------------------------------*
*       Lesen der Tabelle der Konzessionsabgabe
*----------------------------------------------------------------------*
FORM CONCCNTR_READ USING    P_XY_OBJ TYPE ISU2A_BILLING_DATA
                            P_SOBJ   TYPE ISU2A_DATA_COLLECTOR.

  CALL FUNCTION 'ISU_CONCCNTR_READ'
    EXPORTING
      X_FL          = P_XY_OBJ-FL
      X_USE_T_EANL  = P_SOBJ-INST-USE_IEANL
      X_T_EANL      = P_SOBJ-INST-IEANL
      X_ISS         = P_XY_OBJ-ISS
      X_OBJ         = P_XY_OBJ
    IMPORTING
      Y_IFEE        = P_XY_OBJ-IFEE
    CHANGING
      X_ST          = P_XY_OBJ-ST
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT_BC GENERAL_FAULT.                        "#EC *
  ENDIF.

ENDFORM.                               " CONCCNTR_READ
*&---------------------------------------------------------------------*
*&      Form  FRANCHISE_FEE_PREPARE
*&---------------------------------------------------------------------*
*       Vorbereiten der Tabellen mit Konzessionsabgabe
*----------------------------------------------------------------------*
FORM FRANCHISE_FEE_PREPARE USING  P_XY_OBJ  TYPE ISU2A_BILLING_DATA
                                  P_XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.


  CALL FUNCTION 'ISU_FRANCHISE_FEE_PREPARE'
    EXPORTING
      X_ST            = P_XY_OBJ-ST
      X_IFEE          = P_XY_OBJ-IFEE
      X_IANLH         = P_XY_OBJ-ST-IANLH
      X_FL            = P_XY_OBJ-FL
    CHANGING
      XY_BC_CONTR     = BC_CONTR
      XY_IPREI        = P_XY_OBJ-IPREI
      XY_IOPER        = P_XY_OBJ-IOPER
    EXCEPTIONS
      IPREI_NOT_FOUND = 1
      IOPER_NOT_FOUND = 2
      GENERAL_FAULT   = 3
      OTHERS          = 4.

  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT_BC GENERAL_FAULT.                        "#EC *
  ENDIF.

ENDFORM.                               " FRANCHISE_FEE_PREPARE
*&---------------------------------------------------------------------*
*&      Form  BB_AND_PEB_FIELDS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BB_AND_PEB_FIELDS_CHECK USING XY_OBJ TYPE ISU2A_BILLING_DATA.

* Art der Nachberechnung prüfen
  IF NOT XY_OBJ-PBILL IS INITIAL.
*   Vorbeleg vorhanden
    IF XY_OBJ-PBILL-ERCH-BACKBI <> CO_BACKBI_NO AND
       XY_OBJ-PBILL-ERCH-ENDOFBB IS INITIAL.
*     Vorbeleg gehört zur aktuellen NB-Periode --> Art der NB
*     muß übereinstimmen
      IF XY_OBJ-PBILL-ERCH-BACKBI <> XY_OBJ-EAP_ETTA-BACKBI.
*       Die Art der Nachberechnung wurde geändert
        MAC_ERR '072' 'AJ' XY_OBJ-EAP_ETTA-TARIFTYP
                           XY_OBJ-ABR-BEGNACH  SPACE SPACE
                           GENERAL_FAULT.
      ENDIF.
*     Anzahl der Perioden prüfen
      IF XY_OBJ-PBILL-ERCH-NUMPERBB <> XY_OBJ-EAP_ETTA-NUMPERBB.
*       Die Länge der Nachberechnungsperiode hat sich geändert
        MAC_ERR '074' 'AJ' XY_OBJ-EAP_ETTA-TARIFTYP
                           XY_OBJ-ABR-BEGNACH
                           XY_OBJ-PBILL-ERCH-NUMPERBB
                           XY_OBJ-EAP_ETTA-NUMPERBB
                           GENERAL_FAULT.
      ENDIF.
    ENDIF.
  ENDIF.
* ggf. BEGNACH initialisieren
  IF XY_OBJ-EAP_ETTA-BACKBI = CO_BACKBI_NO.
*   keine Nachberechnung aktiv --> BEGNACH wird nicht benötigt
    CLEAR XY_OBJ-ABR-BEGNACH.
  ELSEIF XY_OBJ-ABR-BEGNACH IS INITIAL.
*   BEGNACH wird benötigt --> Beginn der Abrechnungsperiode
*   (kann evtl. vorkommen, wenn ein Tariftypwechsel stattgefunden
*    hat)
    XY_OBJ-ABR-BEGNACH = XY_OBJ-ABR-BEGABRPE.
  ENDIF.
  IF XY_OBJ-EAP_ETTA-BACKBI <> CO_BACKBI_NO
    AND XY_OBJ-ST-ABRVORG = CO_ZWIABR.
*   Zwischenabrechnung ist nicht erlaubt
    MAC_ERR '108' 'AJ' XY_OBJ-ABR-BEGNACH SPACE SPACE SPACE
                       GENERAL_FAULT.
    IF 1 = 2. MESSAGE E108(AJ) WITH SPACE. ENDIF.
  ENDIF.
* Art der Endabrechnung prüfen
  IF NOT XY_OBJ-PBILL IS INITIAL.
*   Vorbeleg vorhanden
    IF XY_OBJ-PBILL-ERCH-PERENDBI <> CO_PEBI_NO AND
       XY_OBJ-PBILL-ERCH-ENDOFPEB IS INITIAL.
*     Vorbeleg gehört zur aktuellen EA-Periode --> Art der EA
*     muß übereinstimmen
      IF XY_OBJ-PBILL-ERCH-PERENDBI <> XY_OBJ-EAP_ETTA-PERENDBI.
*       Die Art der Endabrechnung wurde geändert
        MAC_ERR '073' 'AJ' XY_OBJ-EAP_ETTA-TARIFTYP
                           XY_OBJ-ABR-BEGEND  SPACE SPACE
                           GENERAL_FAULT.
      ENDIF.
*     Anzahl der Perioden prüfen
      IF XY_OBJ-PBILL-ERCH-NUMPERPEB <> XY_OBJ-EAP_ETTA-NUMPERPEB.
*       Die Länge der Endabrechnungsperiode hat sich geändert
        MAC_ERR '075' 'AJ' XY_OBJ-EAP_ETTA-TARIFTYP
                           XY_OBJ-ABR-BEGEND
                           XY_OBJ-PBILL-ERCH-NUMPERPEB
                           XY_OBJ-EAP_ETTA-NUMPERPEB
                           GENERAL_FAULT.
      ENDIF.
    ENDIF.
  ENDIF.
* ggf. BEGEND initialisieren
  IF XY_OBJ-EAP_ETTA-PERENDBI = CO_PEBI_NO.
*   keine Endabrechnung aktiv --> BEGEND wird nicht benötigt
    CLEAR XY_OBJ-ABR-BEGEND.
  ELSEIF XY_OBJ-ABR-BEGEND IS INITIAL.
*   BEGEND wird benötigt --> Beginn der Abrechnungsperiode
*   (kann evtl. vorkommen, wenn ein Tariftypwechsel stattgefunden
*    hat)
    XY_OBJ-ABR-BEGEND = XY_OBJ-ABR-BEGABRPE.
  ENDIF.
* XXO ZWEA
*  IF XY_OBJ-EAP_ETTA-PERENDBI <> CO_PEBI_NO
*    AND XY_OBJ-ST-ABRVORG = CO_ZWIABR.
**   Zwischenabrechnung ist nicht erlaubt
*    MAC_ERR '109' 'AJ' XY_OBJ-ABR-BEGEND SPACE SPACE SPACE
*                       GENERAL_FAULT.
*    IF 1 = 2. MESSAGE E109(AJ) WITH SPACE. ENDIF.
*  ENDIF.
* XXO ZWEA

ENDFORM.                               " BB_AND_PEB_FIELDS_CHECK
*&---------------------------------------------------------------------*
*&      Form  INACH_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INACH_CREATE CHANGING XY_OBJ TYPE ISU2A_BILLING_DATA.

  CALL FUNCTION 'ISU_INACH_CREATE'
    EXPORTING
      X_BACKBILLING = 'X'
    CHANGING
      XY_OBJ        = XY_OBJ
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0 .
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

ENDFORM.                               " INACH_CREATE

*&---------------------------------------------------------------------*
*&      Form  MAXIMAL_INST_EABL_FOR_BILLING
*&---------------------------------------------------------------------*
FORM MAXIMAL_INST_EABL_FOR_BILLING
           USING      X_INF_BEGANA   LIKE  ERCH-BEGABRPE
                      X_ANLAGE       LIKE  EANL-ANLAGE
           CHANGING   Y_MAX_INST     TYPE  ISU2A_INST_STRUC
                      Y_MAX_IEABL    TYPE  ISU2A_IEABL
                      Y_MAX_IEABLG   TYPE  ISU2A_IEABLG.

  MAC_BREAK1 'INST'.                                       "#EC NOBREAK
* Import der Anlagenstruktur samt Ablesungen ab Aufsetzdatum
  CALL FUNCTION 'ISU_DB_INST_EABL_FOR_BILLING'
       EXPORTING
            X_ANLAGE      = X_ANLAGE
            X_AB_EABL     = X_INF_BEGANA
            X_BIS_EABL    = CO_DATE_INFINITE
       IMPORTING
*           y_inst        = y_max_inst
            Y_IEABL       = Y_MAX_IEABL
            Y_IEABLG      = Y_MAX_IEABLG
       CHANGING
            XY_INST       = Y_MAX_INST
       EXCEPTIONS
            GENERAL_FAULT = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
*   Fehler aus FB wiederholen
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ENDIF.                               " sy-subrc

ENDFORM.                               " MAXIMAL_INST_EABL_FOR_BILLING

*&---------------------------------------------------------------------*
*&      Form  INSTALLATION_ENQUEUE
*&---------------------------------------------------------------------*
FORM INSTALLATION_ENQUEUE
           USING   VALUE(P_X_ANLAGE)       LIKE   EANL-ANLAGE
                   VALUE(P_X_SIMULATION)   LIKE   REGEN-KENNZX.

* Hilfstabelle für das Sperren
  DATA: BEGIN OF IKEYS OCCURS 1,
          MANDT     LIKE   SY-MANDT,
          ANLAGE    LIKE   EANL-ANLAGE,
        END OF IKEYS.

* Nur sperren, wenn nicht im Simulationsmodus
  IF P_X_SIMULATION IS INITIAL.
*   Anlage sperren
    IKEYS-MANDT   = SY-MANDT.
    IKEYS-ANLAGE  = P_X_ANLAGE.
    APPEND IKEYS.
*   Baustein zum Sperren
* Bemerkung K.K. 07.06.1999:
* Durch x_full = 'X' wird neben der Anlage auch das Vertragskonto
* gesperrt. Mit x_full = ' ' könnten wir Datenbankzugriffe auf
* EVER, V_EANL, FKKVKP und FKKVK und einen ENQUEUE sparen. Problem:
* Die Fakturierung sperrt nur das Vertragskonto. Bei x_full = ' ' wäre
* die Abrechnung eines Vertrags möglich, während die Fakturierung einen
* Rechnungsbeleg des gleichen Vertrags bearbeitet.
* HK 10.6.99: umgestellt auf Sperrung nur Anlage zur Vermeidung von
* gegenseitigem Sperren bei Vertragskonten mit mehreren Vertraegen
    CALL FUNCTION 'ISU_MASTER_DATA_ENQUEUE'
         EXPORTING
              X_OBJTYPE      = 'EANL'
*             x_full         = 'X'
         TABLES
              XT_KEYS        = IKEYS
         EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
    IF SY-SUBRC <> 0.
      MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
    ENDIF.
  ENDIF.                               " p_x_simulation is initial

ENDFORM.                               " INSTALLATION_ENQUEUE

*&---------------------------------------------------------------------*
*&      Form  INSTALLATION_DEQUEUE
*&---------------------------------------------------------------------*
FORM INSTALLATION_DEQUEUE
        USING  VALUE(P_X_ANLAGE)      LIKE  EANL-ANLAGE
               VALUE(P_X_SIMULATION)  LIKE  REGEN-KENNZX
               X_INSTGR_ENQ TYPE REF TO CL_ISU_BI_INSTGR_ENQ.

* Hilfstabelle für das Sperren
  DATA: BEGIN OF IKEYS OCCURS 1,
          MANDT     LIKE   SY-MANDT,
          ANLAGE    LIKE   EANL-ANLAGE,
        END OF IKEYS.

* Nur, wenn nicht im Simulationsmodus
  IF P_X_SIMULATION IS INITIAL.
*   Anlage entsperren
    IKEYS-MANDT   = SY-MANDT.
    IKEYS-ANLAGE  = P_X_ANLAGE.
    APPEND IKEYS.
*   Baustein zum Entsperren
    CALL FUNCTION 'ISU_MASTER_DATA_DEQUEUE'
      EXPORTING
        X_OBJTYPE = 'EANL'
      TABLES
        XT_KEYS   = IKEYS
      EXCEPTIONS
        OTHERS    = 1.
    IF SY-SUBRC <> 0.
*    ------------------------------------------------------------
*    Bem.: Die folgende Fehlerbehandlung garantiert ein ordentliches
*          Protokoll, sofern der vorangehende FB zumindest Aus-
*          nahmen auslöst, was wegen der offensichtlich nicht
*          deklarierten Ausnahmen in der Schnittstelle vom Autor zwar
*          NICHT beabsichtigt scheint, aber im Coding dennoch
*          definiert sein könnte (ohne Syntaxfehler!).
*          Unabhängig von dort benutztem Nachrichtenobjekt oder
*          gewöhnlicher Fehlerausgabe gelangen dann dortige
*          Nachrichten hier ins Protokoll.
*          > Aber:
*          > Bei einer Message ohne EXCEPTION kommt es hier zum
*          > harten Abbruch der Abrechnung: alle vorangegangenen
*          > Abrechnungen sind committed und im Protokoll; die
*          > aktuelle ist committed, falls Abrechnungsteil fehlerfrei,
*          > aber der Entsperrfehler erscheint nur als harter Fehler;
*          > falls beim aktuellen Trigger sowohl Abrechnungsfehler
*          > als auch Entsperrfehler auftritt, ist beides nicht mehr
*          > im Protokoll?!
*          > Mit P.Zuse die Sperrbausteine auf solche Messages
*          > analysieren und Notwendigkeit klären!?
*    ------------------------------------------------------------
*     -> Sperrfehler
*     Message direkt wiederholen, ohne Makro mac_msg_repeat zu
*     benutzen, da Protokollprobleme s.o
      MAC_MSG_PUTX_WP CO_MSG_ERROR SY-MSGNO SY-MSGID
        SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 GENERAL_FAULT.
    ENDIF.                             " sy-subrc
*    Dequeue all sub installations
    IF NOT X_INSTGR_ENQ IS INITIAL.
      CALL METHOD X_INSTGR_ENQ->DEQUEUE_SUBINST
        EXCEPTIONS
          OTHERS = 1.
      IF SY-SUBRC <> 0.
        MAC_MSG_PUTX_WP CO_MSG_ERROR SY-MSGNO SY-MSGID
          SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 GENERAL_FAULT.
      ENDIF.
    ENDIF.
  ENDIF.                               " p_x_simulation

ENDFORM.                               " INSTALLATION_DEQUEUE

*&---------------------------------------------------------------------*
*&      Form  CONTRACTS_OF_INSTALLATION
*&---------------------------------------------------------------------*
FORM CONTRACTS_OF_INSTALLATION
          TABLES
                  YT_MAX_EVER             STRUCTURE  EVER
          USING
                  VALUE(X_ANLAGE)         LIKE       EANL-ANLAGE
        CHANGING
                  VALUE(Y_INF_BEGANA)     LIKE       ERCH-ENDABRPE
                  VALUE(Y_PREVIOUS_BILL)  TYPE      ISU2A_BILL_DOC "ERCH
                  VALUE(Y_NEXT_CONTRACT)  LIKE       EVER-VERTRAG
                  VALUE(Y_LAST_ENDABRPE)  LIKE       ERCH-ENDABRPE
                  Y_KEYDATE_INSTGR        TYPE D.

  DATA  COMPDAT  TYPE   D.
  DATA W_EVER TYPE EVER.

  CLEAR:  Y_NEXT_CONTRACT ,
          Y_LAST_ENDABRPE ,
          Y_PREVIOUS_BILL .

* Prüfen, ob Vertrag schon abgerechnet werden kann; das ist nicht
* der Fall, wenn es noch andere zeitlich vorangehende Verträge zur
* Anlage gibt, die noch nicht schlussabgerechnet sind
  CALL FUNCTION 'ISU_NEXT_CONTRACT_FOR_INSTLN'
    EXPORTING
      X_ANLAGE        = X_ANLAGE
      X_ACTUAL        = 'X'
    IMPORTING
      Y_NEXT_CONTRACT = Y_NEXT_CONTRACT
      Y_LAST_ENDABRPE = Y_LAST_ENDABRPE
      Y_PREVIOUS_BILL = Y_PREVIOUS_BILL-ERCH
    TABLES
      XYT_EVER        = YT_MAX_EVER
    EXCEPTIONS
      GENERAL_FAULT   = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MAC_MSG_REPEAT CO_MSG_ERROR GENERAL_FAULT.
  ENDIF.

* Y_KEYDATE_INSTGR will be used to determine the installation group
* properties of the current installation. Any date that is within the
* billing period (which will be finally determinded later) is fine.
  READ TABLE YT_MAX_EVER INTO W_EVER
    WITH KEY VERTRAG = Y_NEXT_CONTRACT.
  IF SY-SUBRC = 0.
*   Contract found. Billing will start at EINZDAT or - if THIS
*   contract has been billed before - after the last ENDABRPE
    Y_KEYDATE_INSTGR = W_EVER-EINZDAT.
    IF     NOT Y_LAST_ENDABRPE IS INITIAL
       AND Y_LAST_ENDABRPE >= W_EVER-EINZDAT.
      Y_KEYDATE_INSTGR = Y_LAST_ENDABRPE + 1.
    ENDIF.
  ELSE.
*   There is no contract, billing will stop later.
    CLEAR Y_KEYDATE_INSTGR.
  ENDIF.

* Die folgende Ermittlung Y_INF_BEGANA des ältesten Beginns der
* Datenanalyse dient vor allem dem performanten Einlesen der
* Ableseergebnisse für die Abrechnung
  IF NOT Y_PREVIOUS_BILL-ERCH IS INITIAL.
    Y_INF_BEGANA     = Y_PREVIOUS_BILL-ERCH-ENDABRPE.
*   Im Falle der Nachberechnung muss der älteste Beginn der
*   Datenanalyse vorgezogen werden, aber nur dann, wenn die letzte
*   Periodenabrechnung noch nicht gelaufen ist
    IF  (  NOT  Y_PREVIOUS_BILL-ERCH-BEGNACH   IS  INITIAL  AND
                Y_PREVIOUS_BILL-ERCH-ENDOFBB   IS  INITIAL      ).
      COMPDAT = Y_PREVIOUS_BILL-ERCH-BEGNACH - 1.
      IF Y_INF_BEGANA > COMPDAT.
        Y_INF_BEGANA = COMPDAT.
      ENDIF.
    ENDIF.
*   Im Falle der Endabrechnung muss der älteste Beginn der
*   Datenanalyse immer vorgezogen werden, da man an dieser Stelle
*   noch nicht weiss, ob die Endabrechnung selbst schon gelaufen ist
    IF NOT Y_PREVIOUS_BILL-ERCH-BEGEND IS INITIAL.
      COMPDAT = Y_PREVIOUS_BILL-ERCH-BEGEND - 1.
      IF Y_INF_BEGANA > COMPDAT.
        Y_INF_BEGANA = COMPDAT.
      ENDIF.
    ENDIF.
* >>>>> DPC-ESTB
*   Beginn der Datenanalyse muss für DPC-ESTB besonders gesetzt werden,
*   um alle benötigten Ableseergebnisse performant einlesen zu können
    PERFORM GET_SPECIAL_BEGIN_FOR_DPC_EST
                                   CHANGING Y_INF_BEGANA
                                            Y_PREVIOUS_BILL.
* <<<<< DPC-ESTB
  ELSE.
*   Falls es keinen Vorbeleg gibt, wird der älteste Beginn der
*   Datenanalyse auf fixes Datum 01.01.1900 gelegt
    Y_INF_BEGANA = CO_DATE_FINITE.
  ENDIF.

ENDFORM.                               " CONTRACTS_OF_INSTALLATION

*&---------------------------------------------------------------------*
*&      Form  CHECK_NEXT_CONTRACT
*&---------------------------------------------------------------------*
FORM CHECK_NEXT_CONTRACT
        TABLES  XT_MAX_EVER               STRUCTURE  EVER
                XT_ETRG_SEQ               STRUCTURE  ETRG_SEQ
        USING   VALUE(X_ANLAGE)           LIKE       EANL-ANLAGE
                VALUE(X_VERTRAG)          LIKE       EVER-VERTRAG
                VALUE(X_ORDERDAT)         LIKE       ETRG_SEQ-ORDERDAT
                VALUE(X_NEXT_CONTRACT)    LIKE       EVER-VERTRAG
                VALUE(X_LAST_ENDABRPE)    LIKE       ERCH-ENDABRPE
     CHANGING   VALUE(XY_PREVIOUS_ERCH)   LIKE       ERCH
                VALUE(XY_ERROR)           LIKE       REGEN-KENNZX
                VALUE(XY_NO_READ_PE_CNC)  LIKE       REGEN-KENNZX.

  CALL FUNCTION 'ISU_CHECK_NEXT_CONTRACT'
    EXPORTING
      X_VERTRAG             = X_VERTRAG
      X_ANLAGE              = X_ANLAGE
      X_ORDERDAT            = X_ORDERDAT
      X_NEXT_CONTRACT       = X_NEXT_CONTRACT
      X_LAST_ENDABRPE       = X_LAST_ENDABRPE
    TABLES
      XT_MAX_EVER           = XT_MAX_EVER
      XT_ETRG_SEQ           = XT_ETRG_SEQ
    CHANGING
      XY_PREVIOUS_ERCH      = XY_PREVIOUS_ERCH
      XY_NO_READ_PE_CNC     = XY_NO_READ_PE_CNC
    EXCEPTIONS
      GENERAL_FAULT         = 1
      WRONG_CONTRACT_ORDER  = 2
      CONTRACT_FINAL_BILLED = 3
      OTHERS                = 4.
  IF SY-SUBRC <> 0.
*   Die Fehlerausgabe findet erst nach Verlassen dieser Routine nach
*   der Auswertung des Kennzeichens XY_ERROR statt. Die SYST-Felder
*   stehen dann noch zur Verfügung
    XY_ERROR = 'X'.
  ENDIF.                               " sy-subrc

ENDFORM.                               " CHECK_NEXT_CONTRACT


*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>>>>>> DPC-ESTB ( komplett neu )
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*&---------------------------------------------------------------------*
*&      Form  PROVISORY_ENDABRPE_SET
*&---------------------------------------------------------------------*
FORM PROVISORY_ENDABRPE_SET
                 USING  X_PROV_ENDABRPE  LIKE  ERCH-ENDABRPE
              CHANGING  XY_OBJ           TYPE  ISU2A_BILLING_DATA
                        XY_SOBJ          TYPE  ISU2A_DATA_COLLECTOR.


  READ TABLE XY_OBJ-IZW INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC = 0.
    IF XY_OBJ-TRIG-ETRG-ABRVORG <> CO_ENDABR.
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*     Die Routine RATECAT_DATA_DETERMINE benötigt letztlich das Ende
*     der Abrechnungsperiode als Zugriffsdatum. Genauer hat man:
*     obj-date-endperiod = obj-abr-endabrpe. Letzteres wurde früher
*     an dieser Stelle aus der Ablesedatenanalyse bestimmt, hier wird
*     es direkt aus X_PROV_ENDABRPE übergeben, das aus den Analysen
*     in ETRG_ORDER_CONTROL in FB ISU_ALL_TRIGG_OF_INST_BILL_INT
*     als ORDERDAT zunächst in der Tabelle IETRG_MOD gespeichert wird
*     und im Hauptabrechnungszweig in FORM TRIGGER_BILL in LEA01F02 an
*     ISU_SINGLE_TRIGGER_BILL als X_PROV_ENDABRPE übergeben wird
      XY_OBJ-ABR-ENDABRPE = X_PROV_ENDABRPE.
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF XY_OBJ-ABR-ENDABRPE IS INITIAL.
*       Kann nur auftreten, falls auch RTP-Abrechnung im Spiel ist
        IF XY_OBJ-ABR-BEGABRPE <= XY_OBJ-TRIG-ETRG-ADATSOLL.
          XY_OBJ-ABR-ENDABRPE = XY_OBJ-TRIG-ETRG-ADATSOLL.
        ELSE.
*         Der Trigger zum Datum &1 kann nicht abgerechnet werden
          MAC_ERR '091' 'AJ' XY_OBJ-TRIG-ETRG-ABRDATS XY_OBJ-ST-ANLAGE
                XY_OBJ-ABR-BEGABRPE XY_OBJ-TRIG-ETRG-ADATSOLL
                GENERAL_FAULT.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Keine abrechnungsrelevanten Zählwerke vorhanden -->
*   Ende der Abrechnungsperiode setzen
* >>>>> Japan, WWT 28.10.2002
    PERFORM FLAT_ENDABRPE_SET USING XY_OBJ-TRIG-ETRG-SPARTE
                                    XY_OBJ-TRIG-ETRG-ADATSOLL
                                    XY_OBJ-ABR-BEGABRPE
                                    XY_OBJ-TRIG-ETRG-ABRDATS
                                    XY_OBJ-ST-ANLAGE
                           CHANGING XY_OBJ-ABR-ENDABRPE.
**  IF XY_OBJ-ABR-BEGABRPE <= XY_OBJ-TRIG-ETRG-ADATSOLL.
**    XY_OBJ-ABR-ENDABRPE = XY_OBJ-TRIG-ETRG-ADATSOLL.
**  ELSE.
**    Der Trigger zum Datum &1 kann nicht abgerechnet werden
**    MAC_ERR '091' 'AJ' XY_OBJ-TRIG-ETRG-ABRDATS XY_OBJ-ST-ANLAGE
**            XY_OBJ-ABR-BEGABRPE XY_OBJ-TRIG-ETRG-ADATSOLL
**            GENERAL_FAULT.
**  ENDIF.
* <<<<< Japan, WWT 28.10.2002

  ENDIF.

ENDFORM.                    " PROVISORY_ENDABRPE_SET

*&---------------------------------------------------------------------*
*&      Form DPC_IZWST_GET
*&---------------------------------------------------------------------*
FORM DPC_IZWST_GET
         CHANGING  XY_OBJ        TYPE  ISU2A_BILLING_DATA
                   XY_SOBJ       TYPE  ISU2A_DATA_COLLECTOR.

  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK


* Verarbeitung nur, falls DPC-ESTB vorliegt
  CHECK NOT XY_OBJ-FL-ESTB_MODE IS INITIAL.

  IF (  XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_ORIG            AND
        XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_CHANGED_BY_MASS AND
        XY_OBJ-TRIG-ETRG-ESTINBILL <>  CO_ESTINBILL_SET_MANUAL       ).
*   Verarbeitung mit exaktem linken und exaktem rechten Rand
*   beruhend auf echten Ableseergebnissen in der DB EABL
    PERFORM DPC_IZWST_STANDARD_GET CHANGING XY_OBJ XY_SOBJ.
  ELSE.
*   Verarbeitung mit ggfs. simuliertem linken und rechten Rand,
*   da nicht notwendig für alle Zählwerke Ableseergebnisse in DB EABL
*   vorausgesetzt werden
    PERFORM DPC_IZWST_VIA_SIM_GET CHANGING XY_OBJ XY_SOBJ.
  ENDIF.


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK

ENDFORM.                    " DPC_IZWST_GET

*&---------------------------------------------------------------------*
*&      Form  get_act_period
*&---------------------------------------------------------------------*
FORM GET_ACT_PERIOD CHANGING XY_OBJ TYPE ISU2A_BILLING_DATA.

  INCLUDE IEABASDYPER.
  DATA: WBASDYPERASS TYPE  ISU2C_BASDYPERASS.
  DATA: WACTPERIOD   TYPE  ISU2C_ACTPERIOD.

  IF NOT XY_OBJ-EAP_ETTA-DYPERCON IS INITIAL.
*   Falls DPC mit Schätzung der Ableseergebnisse in der Abrechnung
*   involviert ist, muss der Aufbau der Ableseergebnisse später
*   erfolgen. Um dies entscheiden zu können, erfolgt bereits hier eine
*   Voranalyse bezüglich DPC
    CALL FUNCTION 'ISU_DB_BASDYPERASS_SELECT_ALL'
      EXPORTING
        X_DYPERCON    = XY_OBJ-EAP_ETTA-DYPERCON
      IMPORTING
        Y_BASDYPERASS = XY_OBJ-IBASDYPERASS
      EXCEPTIONS
        NOT_FOUND     = 1
        SYSTEM_ERROR  = 2
        NOT_QUALIFIED = 3
        OTHERS        = 4.
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.
    LOOP AT XY_OBJ-IBASDYPERASS INTO WBASDYPERASS
      WHERE BASDYPER = CO_BASDYPER_ESTB.
      XY_OBJ-FL-ESTB_MODE = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_act_period

*&---------------------------------------------------------------------*
*&      Form  DPC_IZWST_STANDARD_GET
*&---------------------------------------------------------------------*
FORM DPC_IZWST_STANDARD_GET
         CHANGING  XY_OBJ        TYPE  ISU2A_BILLING_DATA
                   XY_SOBJ       TYPE  ISU2A_DATA_COLLECTOR.


  DATA  WA_IZW        TYPE   ISU2A_ZW.
  DATA  IZW_DPC_INF   LIKE   WA_IZW OCCURS 0.
  DATA  COMPR_IZW     TYPE   ISU2A_BILLING_DATA-IZW.


  MAC_BREAK1 'DPCEABL'.

* Die übergebene IZW überdeckt zwar [OBJ-DATE-BEGANA_ESTB,Unendlich],
* der folgende FB beachtet ausserdem aber Einfluss der RTP-Abrechnung
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ          = XY_OBJ
    CHANGING
      XY_IZW_DPC_INF = IZW_DPC_INF
    EXCEPTIONS
      GENERAL_FAULT  = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

* Puffer MAX_IEABL und MAX_IEABLG initialisieren
  PERFORM REFRESH_BUFFER_MR CHANGING XY_SOBJ.

* Alle eventuell relevanten Ableseergebnisse einlesen und aufbereiten
  CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
    EXPORTING
      X_IZW         = IZW_DPC_INF
      X_IABLTERM    = XY_OBJ-IABLTERM
      X_ABR         = XY_OBJ-ABR
      X_INST        = XY_SOBJ-INST
      X_VERTRAG     = XY_OBJ-ST-VERTRAG
      X_EABL_KZ     = '2'
      X_IZW_1       = IZW_DPC_INF
      X_BEGANA      = XY_OBJ-DATE-BEGANA_ESTB
      X_MAX_IEABL   = XY_SOBJ-MAX_IEABL
      X_MAX_IEABLG  = XY_SOBJ-MAX_IEABLG
      X_EINZDAT     = XY_SOBJ-EVER-EINZDAT
    IMPORTING
      Y_IZWST       = XY_OBJ-IZWST
      Y_IZWSTBF     = XY_OBJ-IZWSTBF
      Y_COMPR_IZW   = COMPR_IZW
      Y_IEABL       = XY_OBJ-IEABL
      Y_IEABLG      = XY_OBJ-IEABLG
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

  IF COMPR_IZW[] IS INITIAL.
*   OBJ-IZWST kann in diesem Fall zuviel Einträge enthalten
*   und muss gelöscht werden. Ggfs.nur OBJ-IZWSTBF interessant
    REFRESH XY_OBJ-IZWST.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ISU_DPC_IZWST_STANDARD_GET'
    EXPORTING
      X_IZW_DPC     = IZW_DPC_INF
    CHANGING
      XY_SOBJ       = XY_SOBJ
      XY_OBJ        = XY_OBJ
      XY_COMPR_IZW  = COMPR_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

* Muster-IZWST komprimieren und mit speziellem Periodentyp 'DE'
* versorgen
  PERFORM TEMPLATE_IZWST_FINAL_WORK CHANGING XY_OBJ.


ENDFORM.                    " DPC_IZWST_STANDARD_GET


*&---------------------------------------------------------------------*
*&      Form  DPC_IZWST_VIA_SIM_GET
*&---------------------------------------------------------------------*
FORM DPC_IZWST_VIA_SIM_GET
         CHANGING  XY_OBJ        TYPE  ISU2A_BILLING_DATA
                   XY_SOBJ       TYPE  ISU2A_DATA_COLLECTOR.

  DATA  ABR_TRIG   TYPE  ISU2A_ABR_TRIG.
  DATA  IZW_DPC    TYPE  ISU2A_BILLING_DATA-IZW.
  DATA  COMPR_IZW  TYPE  ISU2A_BILLING_DATA-IZW.

*>>>> Note 884177
  DATA: LV_ENDABRPE TYPE ENDABRPE.
  LV_ENDABRPE = XY_OBJ-ABR-ENDABRPE.
  PERFORM DET_EXTRAPOLATION_DATE
                        USING    XY_OBJ
                                 XY_SOBJ
                                 LV_ENDABRPE
                        CHANGING XY_OBJ-ABR-ENDABRPE.
*<<<< Note 884177

* Zählwerkstabelle für [OBJ-DATE-BEGANA_ESTB,ENDABRPE] erstellen und
* Einfluss der RTP-Abrechnung beachten
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_DPC    = IZW_DPC
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

* Puffer MAX_IEABL und MAX_IEABLG initialisieren
  PERFORM REFRESH_BUFFER_MR CHANGING XY_SOBJ.

* Alle eventuell relevanten Ableseergebnisse einlesen und aufbereiten.
* Es wird erwartet, dass am linken Rand = XY_OBJ-DATE-BEGANA_ESTB
* echte Ablesungen vorhanden sind.
  CALL FUNCTION 'ISU_METER_READING_RESULTS_GET'
    EXPORTING
      X_IZW         = XY_OBJ-IZW
      X_IABLTERM    = XY_OBJ-IABLTERM
      X_ABR         = XY_OBJ-ABR
      X_INST        = XY_SOBJ-INST
      X_VERTRAG     = XY_OBJ-ST-VERTRAG
      X_EABL_KZ     = '1'
      X_IZW_1       = IZW_DPC
      X_BEGANA      = XY_OBJ-DATE-BEGANA_ESTB
      X_EINZDAT     = XY_SOBJ-EVER-EINZDAT
      X_MAX_IEABL   = XY_SOBJ-MAX_IEABL
      X_MAX_IEABLG  = XY_SOBJ-MAX_IEABLG
    IMPORTING
      Y_IZWST       = XY_OBJ-IZWST
      Y_IZWSTBF     = XY_OBJ-IZWSTBF
      Y_COMPR_IZW   = COMPR_IZW
      Y_IEABL       = XY_OBJ-IEABL
      Y_IEABLG      = XY_OBJ-IEABLG
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

  IF COMPR_IZW[] IS INITIAL.
*   OBJ-IZWST kann in diesem Fall zuviel Einträge enthalten
*   und muss gelöscht werden. Ggfs.nur OBJ-IZWSTBF interessant
    REFRESH XY_OBJ-IZWST.
*>>>> Note 904986
    IF XY_OBJ-ABR-ENDABRPE <> LV_ENDABRPE.
      XY_OBJ-ABR-ENDABRPE = LV_ENDABRPE.
    ENDIF.
*<<<< Note 904986
    EXIT.
  ENDIF.

* Tabelle IZWST komplettieren, ggfs. fehlende Ablesungen simulieren
  CALL FUNCTION 'ISU_DPC_IZWST_VIA_SIM_GET'
    EXPORTING
      X_IZW_DPC     = IZW_DPC
    CHANGING
      XY_SOBJ       = XY_SOBJ
      XY_OBJ        = XY_OBJ
      XY_COMPR_IZW  = COMPR_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.

*>>>> Note 884177
  IF XY_OBJ-ABR-ENDABRPE <> LV_ENDABRPE.
    XY_OBJ-ABR-ENDABRPE = LV_ENDABRPE.
  ENDIF.
*<<<< Note 884177

* Muster-IZWST komprimieren und mit speziellem Periodentyp 'DE'
* versorgen
  PERFORM TEMPLATE_IZWST_FINAL_WORK CHANGING XY_OBJ.


ENDFORM.                    " DPC_IZWST_VIA_SIM_GET

*&---------------------------------------------------------------------*
*&      Form  refresh_buffer_mr
*&---------------------------------------------------------------------*
FORM REFRESH_BUFFER_MR CHANGING XY_SOBJ TYPE ISU2A_DATA_COLLECTOR.


* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* WWT 22.11.01:
* Wegen Umstellung, d.h. Komplett-Einlesen gleich zu Beginn in
* isu_all_trigg_of_inst_bill_int ist REFRESH doch unnötig
  EXIT.
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


* ----------------------------------------------------------------------
* WARNUNG WWT 07.11.01: FOLGENDER REFRESH IST ABSOLUT NOTWENDIG
* Genaue Begründung:
* ==================
* Bisher importierte Ableseergebnisse könnten unvollständig sein!
* Daher durch Refresh die Buffer initialisieren, um im folgenden
* FB ein komplettes Neu-Einlesen ab dem DPC-ESTB-relevanten
* Anfangsdatum XY_OBJ-DATE-BEGANA_ESTB zu erzwingen. Dieses Datum
* kann VOR dem normalerweise üblichen XY_OBJ-DATE-BEGANA liegen.
* Da zudem XY_OBJ-DATE-BEGANA beim allerersten Einlesen in der Routine
* MAXIMAL_INST_EABL_FOR_BILLING in ISU_ALL_TRIGG_OF_INST_BILL_INT
* für DPC-ESTB noch NICHT wohldefiniert ist, macht es auch keinen
* Sinn, die Datümer XY_OBJ-DATE-BEGANA und XY_OBJ-DATE-BEGANA_ESTB
* hier vergleichen zu wollen und nur für ">" neu zu lesen.
* Nach heutigem Stand ist XY_OBJ-DATE-BEGANA_ESTB auf jeden Fall
* das älteste Datum.
*  REFRESH:  xy_sobj-max_ieabl[],
*            xy_sobj-max_ieablg[].
*

ENDFORM.                    " refresh_buffer_mr


*&---------------------------------------------------------------------*
*&      Form  DPC_ADV_IZWST_VIA_SIM_GET
*&---------------------------------------------------------------------*
FORM DPC_ADV_IZWST_VIA_SIM_GET
         CHANGING  XY_OBJ        TYPE  ISU2A_BILLING_DATA
                   XY_SOBJ       TYPE  ISU2A_DATA_COLLECTOR.

  DATA:
*        Workarea für Zählwerkstabelle
         WA_IZW       TYPE   ISU2A_ZW                     ,
*        Zählwerkstabelle für Vorausabrechnungszeitraum
         VOR_IZW      LIKE   WA_IZW OCCURS 0              ,
*        Workarea für Zählerstandstabelle
         WA_IZWST     TYPE   ISU2A_ZWST                   ,
*        Zählerstandstabelle für Vorausabrechnungszeitraum
         VOR_IZWST    TYPE   ISU2A_BILLING_DATA-IZWST     .


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK


* Für den folgenden Aufruf braucht man eine Übergabetabelle VOR_IZW,
* welche einen exakten Ausschnitt der IZW für den
* Vorausabrechnungszeitraum darstellt.
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_VOR    = VOR_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
*
* Ferner muss im Fall des Vorausabrechnungszeitraum die unbearbeitete
* Originaltabelle XY_OBJ-IZWSTBF übergeben werden, da sonst einige
* der verwendeten Verarbeitungsroutinen auf falschen Datümern
* aufsetzen würden. Nur so kann die Verarbeitung für alle
* Simulationsarten einheitlich gehalten werden.
  VOR_IZWST[] = XY_OBJ-IZWSTBF[].
*
* Aufruf des FB 'ISU_SIM_IZWST_ESTABLISH', welcher für alle
* Simulationsarten, hier für den Vorausabrechnungszeitraum, die
* zugehörige Zählerstandstabelle erstellt
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
       EXPORTING
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_VERTRAG       = XY_OBJ-ST-VERTRAG
            X_KZ_VOR_EXIST  = SPACE
            X_IZW           = VOR_IZW
            X_KZ_BEG_ADJUST = 'X'
            X_IEABL         = XY_OBJ-IEABL
            X_IEABLG        = XY_OBJ-IEABLG
            X_ANLAGE        = XY_OBJ-ST-ANLAGE
            X_INST          = XY_SOBJ-INST
            X_ABR           = XY_OBJ-ABR
            X_ST            = XY_OBJ-ST
*           -------------------------------------------------
            X_SPECIAL_BEGIN = 'X'
            X_ABRDATS_LOW   = XY_OBJ-TRIG-ETRG-ABRDATS
            X_ENDABRPE_LOW  = XY_OBJ-ABR-ENDABRPE
            X_ABRVORG_LOW   = XY_OBJ-TRIG-ETRG-ABRVORG
*           -------------------------------------------------
            X_EVER          = XY_SOBJ-EVER
       CHANGING
            XY_IZWST        = VOR_IZWST
            XY_MABLBELNR    = XY_OBJ-MABLBELNR
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
*   Fehler aus vorangehendem FB wiederholen
    MAC_ERR_REPEAT SPACE.
*   Genauere Herkunft des Fehlers schildern ->
*   DPC-ESTB: Ableseergebnisermittlung für Vorausperiode fehlerhaft...
    MAC_MSG_PUTX CO_MSG_ERROR '656' 'AJ'
       XY_OBJ-VOR-BEGVORPE XY_OBJ-VOR-ENDVORPE SPACE SPACE
       GENERAL_FAULT.
    IF 1 = 2. MESSAGE E656(AJ) WITH SPACE SPACE. ENDIF.
  ENDIF.

* Tabelle der Zählerstände für Vorausabrechnungszeitraum in bereits
* vorhandene Tabelle IZWST einfügen und abschliessend sortieren
  LOOP AT VOR_IZWST INTO WA_IZWST.
*   Kennzeichen für Vorausabrechnungszeitraumsetzen
    WA_IZWST-PERTYP = CO_ADVANCE_BILLING.
    APPEND WA_IZWST TO XY_OBJ-IZWST.
  ENDLOOP.

  PERFORM SORT_IZWST CHANGING XY_OBJ-IZWST.


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK

ENDFORM.                    " DPC_ADV_IZWST_VIA_SIM_GET

*&---------------------------------------------------------------------*
*&      Form REDUCE_IZW
*&---------------------------------------------------------------------*
FORM REDUCE_IZW CHANGING XY_OBJ TYPE ISU2A_BILLING_DATA.

  DATA  WZW        TYPE  ISU2A_ZW.
  DATA  WEGER      TYPE  ISU2A_GER.
  DATA  WSERIAL    TYPE  ISU2A_HAUPT_NEBEN.
  DATA  WSPERR     TYPE  ISU2A_SPERR.
  DATA  WABLTERM   TYPE  ISU2A_ABLTERM.

* Verarbeitung nur, falls DPC-ESTB vorliegt
  CHECK NOT XY_OBJ-FL-ESTB_MODE IS INITIAL.

* Objekttabelle OBJ-IZW nun wieder auf XY_OBJ-DATE-BEGANA beschränken,
* da sie nur für die Verarbeitung in den vorliegenden Routinen von
* DPC_IZWST_GET bis hin zur Routine THGAS_FOR_BILLING maximal ausgedehnt
* bis OBJ-DATE-BEGANA_ESTB benötigt wurde
  LOOP AT XY_OBJ-IZW INTO WZW.
    IF WZW-BIS  >=  XY_OBJ-DATE-BEGANA.
      IF WZW-AB < XY_OBJ-DATE-BEGANA.
        WZW-AB = XY_OBJ-DATE-BEGANA.
      ENDIF.
      MODIFY XY_OBJ-IZW FROM WZW.
    ELSE.
      DELETE XY_OBJ-IZW.
    ENDIF.
  ENDLOOP.

* Bei den anderen OBJ-Tabellen IGER, IABLTERM, ISERIAL, ISPERR
* ist die neuerliche Beschneidung nach Absprache mit Klaus Kistl
* am 21.11.01 nicht nötig.
* Ggfs. muesste nochmals 'ISU_INSTALLATION_STRUC_ANALYZE' mit
* korrektem Zeitintervall aufgerufen werden. Dies ist nicht allzu teuer.

ENDFORM.                    " REDUCE_IZW

*&---------------------------------------------------------------------*
*&      Form  GET_ERCHP
*&---------------------------------------------------------------------*
FORM GET_ERCHP  CHANGING  XY_PBILL  TYPE  ISU2A_BILL_DOC.


  IF NOT XY_PBILL-ERCH-ERCHP_V IS INITIAL.

*   ERCHP zum Vorbeleg einlesen
    CALL FUNCTION 'ISU_DB_ERCHP_SELECT_BILL'
      EXPORTING
        X_BELNR       = XY_PBILL-ERCH-BELNR
        X_SORT        = 'X'
      TABLES
        T_ERCHP       = XY_PBILL-IERCHP
      EXCEPTIONS
        NOT_FOUND     = 1
        SYSTEM_ERROR  = 2
        NOT_QUALIFIED = 3
        OTHERS        = 4.
*
    IF SY-SUBRC <> 0.
      MAC_ERR_REPEAT GENERAL_FAULT.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_ERCHP


*&---------------------------------------------------------------------*
*&      Form  GET_BEGANA_ESTB
*&---------------------------------------------------------------------*
FORM GET_BEGANA_ESTB  USING  X_IERCHP        TYPE  ISU2C_IERCHP
                   CHANGING  XY_BEGANA_ESTB  LIKE  ERCH-ENDABRPE.

  DATA: WERCHP    TYPE ISU2C_ERCHP.
  DATA: S_AB      LIKE ERCH-BEGABRPE.
  DATA: FIRST     LIKE REGEN-KENNZX VALUE 'X'.

* Ältestes AB in X_IERCHP bestimmen
  LOOP AT X_IERCHP INTO WERCHP.
    IF FIRST = 'X'.
      CLEAR FIRST.
      IF S_AB < WERCHP-AB.
        S_AB = WERCHP-AB.
      ENDIF.
    ELSEIF WERCHP-AB < S_AB.
      S_AB = WERCHP-AB.
    ENDIF.
  ENDLOOP.

* Spezieller Beginn der Datenanalyse für DPC-ESTB
  XY_BEGANA_ESTB = S_AB.

* Für Einlesen der Ableseergebnisse nochmals einen Tag abziehen
  XY_BEGANA_ESTB =  XY_BEGANA_ESTB - 1.


ENDFORM.                    " GET_BEGANA_ESTB

*&---------------------------------------------------------------------*
*&      Form  GET_SPECIAL_BEGIN_FOR_DPC_EST
*&---------------------------------------------------------------------*
FORM GET_SPECIAL_BEGIN_FOR_DPC_EST
        CHANGING  XY_INF_BEGANA    LIKE  ERCH-ENDABRPE
                  XY_PREVIOUS_BILL TYPE  ISU2A_BILL_DOC.

  DATA: BEGANA_ESTB  LIKE  ERCH-ENDABRPE.

  IF XY_PREVIOUS_BILL-ERCH-BASDYPER = CO_BASDYPER_ESTB.

*   Anhangstabelle ERCHP zum Vorbeleg lesen
    PERFORM GET_ERCHP CHANGING XY_PREVIOUS_BILL.

*   BEGANA_ESTB für DPC-ESTB ermitteln
    PERFORM GET_BEGANA_ESTB USING XY_PREVIOUS_BILL-IERCHP
                         CHANGING BEGANA_ESTB.

*   Minimales Abdatum aus BEGANA_ESTB, bis dato bestimmtem XY_INF_BEGANA
*   und BEGABRPE des Vorbelegs ermitteln:
    IF (      XY_INF_BEGANA  > BEGANA_ESTB  AND
         NOT BEGANA_ESTB   IS INITIAL           ).
      XY_INF_BEGANA = BEGANA_ESTB.
    ENDIF.
    IF (     XY_INF_BEGANA  > XY_PREVIOUS_BILL-ERCH-BEGABRPE AND
         NOT XY_PREVIOUS_BILL-ERCH-BEGABRPE IS INITIAL                ).
      XY_INF_BEGANA = XY_PREVIOUS_BILL-ERCH-BEGABRPE.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_SPECIAL_BEGIN_FOR_DPC_EST

*&---------------------------------------------------------------------*
*&      Form  TEMPLATE_IZWST_FINAL_WORK
*&---------------------------------------------------------------------*
FORM TEMPLATE_IZWST_FINAL_WORK
   CHANGING  XY_OBJ  TYPE  ISU2A_BILLING_DATA.

  DATA  WA_IZWST       TYPE ISU2A_ZWST.
  DATA: COPY_IZWST     TYPE ISU2A_IZWST,
        WPERIOD        TYPE ISU2C_PERIOD,
        FIRST_USEESTMR TYPE C VALUE 'X',
        FIRST          TYPE C VALUE 'X',
        WZW            TYPE ISU2A_ZW.

*-----------------------------------------------------------------------
* Bemerkung WWT 23.11.2001:
* Muster-IZWST auf jeden Fall komprimieren. Das Kennzeichen USEESTMR
* der Tabelle OBJ-IPERIOD wird nicht wie in LEA01F04 in der Routine
* MRRESULT_FOR_BACK_BILLING interpretiert, da wir bei DPC-ESTB
* "geschätzte Ableseergebnisse auf der DB" nicht in die Neuschätzung
* im Rahmen der Abgrenzung einfliessen lassen möchten.
* >>> Denn gerade bei DPC-ESTB zielt die ganze Strategie darauf ab,
* >>> nur auf der Grundlage ECHTER Ableseergebnisse Schätzungen
* >>> durchzuführen.
* Falls es anders laufen soll, müsste wie in MRRESULT_FOR_BACK_BILLING
* durch Abloopen der OBJ-IPERIOD festgestellt werden, wie jeweils
* USEESTMR gesetzt ist und nach Bedarf eine un-komprimierte und/oder
* komprimierte Muster-IZWST erstellt werden.
* Der Zugriff in IZWST_IABN_ACCESS_GET in LEA00F04 müsste dann ebenfalls
* verfeinert werden ...
* Ferner in THGAS_FOR_BILLING wäre zu entscheiden, welches Exemplar
* an die Gasabrechnung weitergereicht wird ...
* ----------------------------------------------------------------------

* Copy IZWST
  COPY_IZWST = XY_OBJ-IZWST.
  LOOP AT XY_OBJ-IPERIOD INTO WPERIOD.
    IF  FIRST          IS INITIAL
    AND FIRST_USEESTMR IS INITIAL.
      EXIT.
    ENDIF.
    IF WPERIOD-USEESTMR IS INITIAL.
      CHECK FIRST = 'X'.
*     Muster-IZWST komprimieren
      PERFORM COPY_IZWST_COMPRESS  USING XY_OBJ
                                         XY_OBJ-IABLTERM
                                CHANGING COPY_IZWST.
      CLEAR FIRST.
    ELSEIF FIRST_USEESTMR = 'X'.
      CLEAR FIRST_USEESTMR.
    ENDIF.
  ENDLOOP.
  IF FIRST IS INITIAL
  AND FIRST_USEESTMR IS INITIAL.
    APPEND LINES OF COPY_IZWST TO XY_OBJ-IZWST.
  ELSEIF FIRST IS INITIAL.
    XY_OBJ-IZWST = COPY_IZWST.
  ENDIF.

* Alle Einträge mit bis dato initialem Periodentyp müssen
* jetzt mit korrektem Periodentyp versorgt werden
  LOOP AT XY_OBJ-IZWST INTO WA_IZWST
    WHERE PERTYP IS INITIAL.
    WA_IZWST-PERTYP   = CO_DY_PER_END_BILLING.
    MODIFY XY_OBJ-IZWST FROM WA_IZWST.
  ENDLOOP.

* Set gasdate for extrapolated meter readings
  LOOP AT XY_OBJ-IZWST INTO WA_IZWST
  WHERE EXTPKZ <> SPACE.
    LOOP AT XY_OBJ-IZW INTO WZW
    WHERE LOGIKZW  = WA_IZWST-LOGIKZW
    AND   AB      <= WA_IZWST-BIS
    AND   BIS     >= WA_IZWST-BIS.
      EXIT.
    ENDLOOP.
    CHECK WZW-SPARTYP = CO_SPARTYP_GAS.
    CALL FUNCTION 'ISU_ALLOCATION_DATES_SET'
      EXPORTING
        X_ABRVORG         = XY_OBJ-TRIG-ETRG-ABRVORG
        X_KEYDATE         = XY_OBJ-TRIG-ETRG-ADATSOLL
        X_DYNKZ           = '3'
        X_AKLASSE         = XY_OBJ-EAP_ETTA-AKLASSE
        X_Y1              = 38
        X_Y2              = 4
        X_Z1              = 70
        X_Z2              = 5
       X_NO_DIALOG       = 'X'
*       X_PORTION         =
     IMPORTING
       Y_THGDATUM        = WA_IZWST-THGDATUM.
    MODIFY XY_OBJ-IZWST FROM WA_IZWST.
  ENDLOOP.

ENDFORM.                    " TEMPLATE_IZWST_FINAL_WORK

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*<<<<< DPC-ESTB ( komplett neu )
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


* >>>>> Japan, WWT 29.10.2002
INCLUDE IEJBPFUNC.
* <<<<< Japan, WWT 29.10.2002

*&---------------------------------------------------------------------*
*&      Form  DPC_BPP_IZWST_VIA_SIM_GET
*&---------------------------------------------------------------------*

FORM DPC_BPP_IZWST_VIA_SIM_GET
         CHANGING  XY_OBJ        TYPE  ISU2A_BILLING_DATA
                   XY_SOBJ       TYPE  ISU2A_DATA_COLLECTOR.

  DATA:
*        Workarea für Zählwerkstabelle
         WA_IZW       TYPE   ISU2A_ZW                     ,
*        Zählwerkstabelle für Vorausabrechnungszeitraum
         ABS_IZW      LIKE   WA_IZW OCCURS 0              ,
*        Workarea für Zählerstandstabelle
         WA_IZWST     TYPE   ISU2A_ZWST                   ,
*        Zählerstandstabelle für Vorausabrechnungszeitraum
         ABS_IZWST    TYPE   ISU2A_BILLING_DATA-IZWST     .


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK


* Für den folgenden Aufruf braucht man eine Übergabetabelle VOR_IZW,
* welche einen exakten Ausschnitt der IZW für den
* Vorausabrechnungszeitraum darstellt.
  CALL FUNCTION 'ISU_IZW_ADJUST'
    EXPORTING
      X_OBJ         = XY_OBJ
    CHANGING
      XY_IZW_ABS    = ABS_IZW
    EXCEPTIONS
      GENERAL_FAULT = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MAC_ERR_REPEAT GENERAL_FAULT.
  ENDIF.
* Ferner muss im Fall des Vorausabrechnungszeitraum die unbearbeitete
* Originaltabelle XY_OBJ-IZWSTBF übergeben werden, da sonst einige
* der verwendeten Verarbeitungsroutinen auf falschen Datümern
* aufsetzen würden. Nur so kann die Verarbeitung für alle
* Simulationsarten einheitlich gehalten werden.
  ABS_IZWST[] = XY_OBJ-IZWSTBF[].
*
* Aufruf des FB 'ISU_SIM_IZWST_ESTABLISH', welcher für alle
* Simulationsarten, hier für den Vorausabrechnungszeitraum, die
* zugehörige Zählerstandstabelle erstellt
  CALL FUNCTION 'ISU_SIM_IZWST_ESTABLISH'
       EXPORTING
            X_IABLTERM      = XY_OBJ-IABLTERM
            X_VERTRAG       = XY_OBJ-ST-VERTRAG
            X_KZ_VOR_EXIST  = SPACE
            X_IZW           = ABS_IZW
            X_KZ_BEG_ADJUST = 'X'
            X_IEABL         = XY_OBJ-IEABL
            X_IEABLG        = XY_OBJ-IEABLG
            X_ANLAGE        = XY_OBJ-ST-ANLAGE
            X_INST          = XY_SOBJ-INST
            X_ABR           = XY_OBJ-ABR
            X_ST            = XY_OBJ-ST
*           -------------------------------------------------
            X_SPECIAL_BEGIN = 'X'
            X_ABRDATS_LOW   = XY_OBJ-TRIG-ETRG-ABRDATS
            X_ENDABRPE_LOW  = XY_OBJ-ABR-ENDABRPE
            X_ABRVORG_LOW   = XY_OBJ-TRIG-ETRG-ABRVORG
*           -------------------------------------------------
            X_EVER          = XY_SOBJ-EVER
       CHANGING
            XY_IZWST        = ABS_IZWST
            XY_MABLBELNR    = XY_OBJ-MABLBELNR
       EXCEPTIONS
            GENERAL_FAULT   = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
*   Fehler aus vorangehendem FB wiederholen
    MAC_ERR_REPEAT SPACE.
*   Genauere Herkunft des Fehlers schildern ->
*   DPC-ESTB: Ableseergebnisermittlung für Vorausperiode fehlerhaft...
    MAC_MSG_PUTX CO_MSG_ERROR '656' 'AJ'
       XY_OBJ-VOR-BEGVORPE XY_OBJ-VOR-ENDVORPE SPACE SPACE
       GENERAL_FAULT.
    IF 1 = 2. MESSAGE E656(AJ) WITH SPACE SPACE. ENDIF.
  ENDIF.

* Tabelle der Zählerstände für Vorausabrechnungszeitraum in bereits
* vorhandene Tabelle IZWST einfügen und abschliessend sortieren
  LOOP AT ABS_IZWST INTO WA_IZWST.
*   Kennzeichen für Vorausabrechnungszeitraumsetzen
    WA_IZWST-PERTYP = CO_BUDGET_BILLING.
    APPEND WA_IZWST TO XY_OBJ-IZWST.
  ENDLOOP.

  PERFORM SORT_IZWST CHANGING XY_OBJ-IZWST.


  MAC_BREAK1 'DPCEABL'.                                    "#EC NOBREAK

ENDFORM.                    " DPC_BPP_IZWST_VIA_SIM_GET
*&---------------------------------------------------------------------*
*&      Form  DET_EXTRAPOLATION_DATE
*&---------------------------------------------------------------------*

FORM DET_EXTRAPOLATION_DATE
                   USING    X_OBJ      TYPE ISU2A_BILLING_DATA
                            X_SOBJ     TYPE ISU2A_DATA_COLLECTOR
                            X_ENDABRPE TYPE ENDABRPE
                   CHANGING Y_ENDABRPE TYPE ENDABRPE.

  DATA: LV_DPCESTIMATE TYPE KENNZX,
        LV_NEXTACT     TYPE KENNZX,
        LV_ENDABRPE    TYPE ENDABRPE,
        LS_EANL        TYPE V_EANL,
        LV_DATE        TYPE ABZEITSCH,
        LT_TE418       TYPE TABLE OF TE418 WITH HEADER LINE,
        LS_TE418       TYPE TE418,
        LV_ABLEINH     TYPE ABLEINH_TS,
        NAME(13)       TYPE C VALUE 'Z_DPCESTIMATE',
        NEXTACT(9)     TYPE C VALUE 'Z_NEXTACT',
        LV_FROM        TYPE ABZEITSCH,
        LV_TO          TYPE BISZEITSCH,
        LV_FOUND       TYPE KENNZX.
  FIELD-SYMBOLS: <F>, <G> .
  INCLUDE ZXDPCESTIMATE.

  ASSIGN (NAME) TO <F>.
  IF SY-SUBRC = 0.
    LV_DPCESTIMATE = <F>.
    IF LV_DPCESTIMATE <> 'X'.
      CLEAR LV_DPCESTIMATE.
    ENDIF.
  ENDIF.

  CHECK LV_DPCESTIMATE = 'X'.

  ASSIGN (NEXTACT) TO <G>.
  IF SY-SUBRC = 0.
    LV_NEXTACT = <G>.
    IF LV_NEXTACT <> 'X'.
      CLEAR LV_NEXTACT.
    ENDIF.
  ENDIF.

  IF LV_NEXTACT = CO_TRUE.
    IF NOT X_OBJ-TRIG-ETRG-ABLEINH IS INITIAL.
*     Take over meter reading unit from billing order
      LV_ABLEINH = X_OBJ-TRIG-ETRG-ABLEINH.
    ELSE.
*     Take over MR unit of installation
      LOOP AT X_SOBJ-INST-IEANL INTO LS_EANL
        WHERE AB  <= X_OBJ-DATE-ENDPERIOD
        AND   BIS >= X_OBJ-DATE-ENDPERIOD.
        EXIT.
      ENDLOOP.
      MAC_ERR_READ 'IEANL' SPACE 'SCHEDULING_DATA'.
      LV_ABLEINH = LS_EANL-ABLEINH.
    ENDIF.
    LV_TO   = X_OBJ-ABR-ENDABRPE + 365.
    LV_FROM = X_OBJ-ABR-ENDABRPE + 1.
    CALL FUNCTION 'ISU_DB_TE418_SELECT_PERIOD'
      EXPORTING
        X_TERMSCHL             = LV_ABLEINH
        X_DATUM_VON            = LV_FROM
        X_DATUM_BIS            = LV_TO
     TABLES
       TY_TE418               = LT_TE418
     EXCEPTIONS
       NOT_FOUND              = 1
       SYSTEM_ERROR           = 2
       NOT_QUALIFIED          = 3
       INVALID_INTERVAL       = 4
       OTHERS                 = 5.

    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT LT_TE418 INTO LS_TE418
    WHERE ESTINBILL <> '01'
    AND   ABLESART  =  '01'.
*     Use first planned real MR after end of billing period
      Y_ENDABRPE = LS_TE418-ADATSOLL.
      LV_FOUND = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.
* Use date of last actual meter reading + 1 year
* as estimation date if required or if LV_FOUND
* is initial.
  IF LV_FOUND IS INITIAL.
    Y_ENDABRPE = X_OBJ-DATE-BEGANA_ESTB + 365.
  ENDIF.
  IF X_ENDABRPE > Y_ENDABRPE.
*   Switch back to original ENDABRPE
    Y_ENDABRPE = X_ENDABRPE.
  ELSEIF Y_ENDABRPE > X_SOBJ-EVER-AUSZDAT.
*   Use date of move-out
    Y_ENDABRPE = X_SOBJ-EVER-AUSZDAT.
  ENDIF.

ENDFORM.                    " DET_EXTRAPOLATION_DATE