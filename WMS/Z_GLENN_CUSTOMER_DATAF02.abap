DATA: gs_contract_accounts TYPE zsisu_customer_data.




*----------------------------------------------------------------------*
***INCLUDE LZFG_CUSTOMER_DATAF02 .
*----------------------------------------------------------------------*
FORM get_business_partner_details  TABLES   it_return
                                   USING    iv_business_partner        " Business partner
                                            iv_anlage                  " Installation
                                   CHANGING cv_first_name              " First Name
                                            cv_last_name               " Last Name
                                            cv_middle_name             " Middle Name
                                            cv_social_insurance_number " Social Insurance Number
                                            cv_account_class           " Account Class
                                            cv_company_name            " Company Name
                                            cv_country                 " COUNTRY
                                            cv_region                  " Province
                                            cv_city                    " City/Municipality/Town
                                            cv_district                " District
                                            cv_street5                 " Barangay/Barrio
                                            cv_street                  " Street
                                            cv_street2                 " Area/Zone/Park/Industrial
                                            cv_street3                 " Sitio/Purok
                                            cv_street4                 " Subdivision/Village/Townhouse
                                            cv_house_num_supplement    " House number supplement
                                            cv_house_number            " House Number
                                            cv_postal_code             " ZIP/Postal Code
                                            cv_building                " Building (Number or Code)
                                            cv_floor_in_building       " Floor in building
                                            cv_room_apartment_num      " Room / Unit No.
                                            cv_email                   " Account Name
                                            cv_mobile_phone            " Mobile Phone
                                            cv_sex                     " Sex
                                            cv_telephone               " Telephone number
                                            cv_marital_status.         " Marital status

  TYPES: BEGIN OF ty_adr2,
            tel_number TYPE ad_tlnmbr,
            r3_user    TYPE ad_flgmob,
         END OF ty_adr2.

  DATA: lt_adr2   TYPE STANDARD TABLE OF ty_adr2,
        ls_adr2   TYPE ty_adr2,
        lv_adrnum TYPE ad_addrnum.

  DATA: ls_partnerdata TYPE bapiisubpd,
        ls_return      TYPE bapiret2,
        lt_return      TYPE STANDARD TABLE OF bapiret2,
        lt_address     TYPE STANDARD TABLE OF bapiisubpa,
        ls_addressdata TYPE bapiisubpa,
        lt_email_add   TYPE STANDARD TABLE OF bapiadsmtp,
        ls_email_add   TYPE bapiadsmtp.

  " Get the Business partner details
  CALL FUNCTION 'BAPI_ISUPARTNER_GETDETAIL'
    EXPORTING
      partner     = iv_business_partner
    IMPORTING
      partnerdata = ls_partnerdata
      return      = ls_return
    TABLES
      taddress    = lt_address.
  IF NOT ls_return IS INITIAL.
    " Fill the return table
    APPEND ls_return TO it_return.

    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-015.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    READ TABLE lt_address INTO ls_addressdata INDEX 1.
    IF sy-subrc = 0.
      cv_street               = ls_addressdata-street.      " Street
      cv_house_number         = ls_addressdata-house_num1.  " House Number
      cv_street5              = ls_addressdata-location.    " Barangay/Barrio
      cv_district             = ls_addressdata-city2.       " District
      cv_postal_code          = ls_addressdata-post_code1.  " ZIP/Postal Code
      cv_city                 = ls_addressdata-city1.       " City/Municipality/Town
      cv_country              = ls_addressdata-country.     " COUNTRY
      cv_region               = ls_addressdata-region.      " Province

      cv_street2              = ls_addressdata-str_suppl1.  " Area/Zone/Park/Industrial
      cv_street3              = ls_addressdata-str_suppl2.  " Sitio/Purok
      cv_street4              = ls_addressdata-str_suppl3.  " Subdivision/Village/Townhouse
      cv_house_num_supplement = ls_addressdata-house_num2.  " House number supplement
      cv_building             = ls_addressdata-building.    " Building (Number or Code)
      cv_floor_in_building    = ls_addressdata-floor.       " Floor in building
      cv_room_apartment_num   = ls_addressdata-roomnumber.  " Room / Unit No.

    ENDIF.

    " Fill up the parameters of the BAPI and pass the values to be change
    cv_first_name              = ls_partnerdata-name_first.     " First Name
    cv_last_name               = ls_partnerdata-name_last.      " Last Name
    cv_middle_name             = ls_partnerdata-namemiddle.     " Middle Name
    cv_social_insurance_number = ls_partnerdata-soc_secure.     " Social Insurance Number
    cv_account_class           = ls_partnerdata-account_class.  " Account Class
    cv_company_name            = ls_partnerdata-name_org1.      " Company Name
    cv_marital_status          = ls_partnerdata-marriage_state. " marital status

    " Sex
    IF ls_partnerdata-sex_m = abap_true.
      cv_sex = 'M'.
    ELSEIF ls_partnerdata-sex_f = abap_true.
      cv_sex = 'F'.
    ENDIF.

    SELECT SINGLE addrnumber
      INTO lv_adrnum
      FROM but020
      WHERE partner = iv_business_partner.
    IF sy-subrc = 0.
      " Get mobile number and telephone number
      SELECT tel_number
             r3_user
        INTO TABLE lt_adr2
        FROM adr2
        WHERE addrnumber = lv_adrnum.
      IF sy-subrc = 0.
        " Get mobile phone number
        READ TABLE lt_adr2 INTO ls_adr2 WITH KEY r3_user = '3'.
        IF sy-subrc = 0.
          cv_mobile_phone = ls_adr2-tel_number.
        ENDIF.

        " Get Telephone number
        READ TABLE lt_adr2 INTO ls_adr2 WITH KEY r3_user = '1'.
        IF sy-subrc = 0.
          cv_telephone = ls_adr2-tel_number.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  " Get the email address
  CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
    EXPORTING
      businesspartner      = iv_business_partner
    TABLES
      e_maildatanonaddress = lt_email_add
      return               = lt_return.
  IF lt_return IS INITIAL.

    READ TABLE lt_email_add INTO ls_email_add WITH KEY std_no = abap_true.
    IF sy-subrc = 0.
      cv_email = ls_email_add-e_mail.
    ENDIF.

  ELSE.

    APPEND LINES OF lt_return TO it_return.

    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-020.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

ENDFORM.                    " GET_BUSINESS_PARTNER_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_DEVICE_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_RETURN  text
*      -->IV_ANLAGE  text
*      <--CV_MANUFACTURER  text
*      <--CV_CONSTRUCT_YEAR  text
*      <--CV_BUSINESS_AREA  text
*      <--CV_COMPANY_CODE  text
*      <--CV_SORT  text
*----------------------------------------------------------------------*
FORM get_device_details  TABLES   it_return
                         USING    iv_anlage
                                  iv_equnr
                                  iv_devloc
                                  iv_logiknr
                         CHANGING cv_manufacturer
                                  cv_construct_year
                                  cv_sort
                                  cv_additional_info
                                  cv_location
                                  cv_rate_type
                                  cv_size_dimens
                                  cv_serial_number.

  DATA: ls_auto TYPE isu01_devloc_auto.

  DATA: ls_data_general   TYPE bapi_itob,
        ls_data_specific  TYPE bapi_itob_eq_only,
        ls_return         TYPE bapiret2,
        ls_auto_dev_rate  TYPE  isu70_devicerate_auto.

  FIELD-SYMBOLS: <fs_reg> TYPE reg70_r.

  CONSTANTS: co_bis TYPE biszeitsch VALUE '99991231'.

  CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
    EXPORTING
      equipment         = iv_equnr
    IMPORTING
      data_general_exp  = ls_data_general
      data_specific_exp = ls_data_specific
      return            = ls_return.

  IF ls_return IS INITIAL.

    cv_manufacturer   = ls_data_general-manfacture.
    cv_construct_year = ls_data_general-constyear.
    cv_sort           = ls_data_general-sortfield.
    cv_location       = ls_data_general-maintloc.

    cv_size_dimens    = ls_data_general-obj_size.
    cv_serial_number  = ls_data_specific-serialno.

  ELSE.

    APPEND ls_return TO it_return.

    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-016.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  " Get the Details of the Device location
  CALL FUNCTION 'ISU_S_DEVLOC_PROVIDE'
    EXPORTING
      x_devloc      = iv_devloc
      x_wmode       = '1'
    IMPORTING
      y_auto        = ls_auto
    EXCEPTIONS
      not_found     = 1
      foreign_lock  = 2
      general_fault = 3
      OTHERS        = 4.
  IF sy-subrc = 0.

    cv_additional_info = ls_auto-egpld-stortzus. " Addition to device location

  ELSE.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 INTO ls_return-message
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ls_return-type   = co_error.
    ls_return-id     = sy-msgid.
    ls_return-number = sy-msgno.
    IF NOT ls_return-message IS INITIAL.
      APPEND ls_return TO it_return.
    ENDIF.
    CLEAR ls_return.

    ls_return-type = co_error.
    ls_return-message = text-016.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  " Get the device rate type
  CALL FUNCTION 'ISU_O_DEVICERATE_OPEN'
    EXPORTING
      x_anlage         = iv_anlage
      x_stichtag       = sy-datum
      x_wmode          = '1'
      x_upd_online     = abap_true
      x_no_dialog      = abap_true
      x_called_by_ec30 = abap_true
    IMPORTING
      y_auto           = ls_auto_dev_rate
    EXCEPTIONS
      not_found        = 1
      invalid          = 2
      foreign_lock     = 3
      input_error      = 4
      system_error     = 5
      internal_error   = 6
      not_qualified    = 7
      cancelled        = 8
      OTHERS           = 9.
  IF sy-subrc = 0.

    READ TABLE ls_auto_dev_rate-reg ASSIGNING <fs_reg> WITH KEY bis = co_bis.
    IF sy-subrc = 0.
      cv_rate_type = <fs_reg>-tarifart.
    ENDIF.

  ELSE.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               INTO ls_return-message
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ls_return-type   = co_error.
    ls_return-id     = sy-msgid.
    ls_return-number = sy-msgno.
    IF NOT ls_return-message IS INITIAL.
      APPEND ls_return TO it_return.
    ENDIF.
    CLEAR ls_return.

    ls_return-type = co_error.
    ls_return-message = text-017.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.


ENDFORM.                    " GET_DEVICE_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_UTILITY_INSTLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_RETURN  text
*      -->IV_ANLAGE  text
*      <--CV_BILLING_CLASS  text
*      <--CV_RATE_CATEGORY  text
*      <--CV_MRU_NUM  text
*      <--CV_INSTALLATION_TYPE  text
*----------------------------------------------------------------------*
FORM get_utility_instll  TABLES   it_return
                         USING    iv_anlage
                         CHANGING cv_billing_class
                                  cv_rate_category
                                  cv_mru_num
                                  cv_installation_type.

  DATA: ls_auto   TYPE isu01_instln_auto,
        ls_return TYPE bapiret2.

  CALL FUNCTION 'ISU_S_INSTLN_PROVIDE'
    EXPORTING
      x_anlage        = iv_anlage
      x_keydate       = sy-datum
      x_wmode         = '1'
      x_no_dialog     = abap_true
    IMPORTING
      y_auto          = ls_auto
    EXCEPTIONS
      not_found       = 1
      invalid_keydate = 2
      foreign_lock    = 3
      not_authorized  = 4
      invalid_wmode   = 5
      general_fault   = 6
      OTHERS          = 7.
  IF sy-subrc = 0.

    " Pass the data to be change to the parameters
    cv_rate_category     = ls_auto-data-tariftyp.   " Rate category
    cv_billing_class     = ls_auto-data-aklasse.    " Billing class
    cv_installation_type = ls_auto-data-anlart.     " Installation type
    cv_mru_num           = ls_auto-data-ableinh.    " MRU

  ELSE.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 INTO ls_return-message
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ls_return-type   = co_error.
    ls_return-id     = sy-msgid.
    ls_return-number = sy-msgno.
    IF NOT ls_return-message IS INITIAL.
      APPEND ls_return TO it_return.
    ENDIF.
    CLEAR ls_return.


    ls_return-type = co_error.
    ls_return-message = text-018.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

ENDFORM.                    " GET_UTILITY_INSTLL
*&---------------------------------------------------------------------*
*&      Form  GET_CONTRACT_ACCOUNT_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_RETURN  text
*      -->IV_CONTRACT_ACCNT_NUM  text
*      -->IV_BUSINESS_PARTNER  text
*      <--CV_LEGACY_NUMBER  text
*      <--CV_ACCOUNT_DETERMINATION  text
*      <--CV_ACCOUNT_CLASS  text
*----------------------------------------------------------------------*
FORM get_contract_account_details  TABLES   it_return
                                   USING    iv_contract_accnt_num
                                            iv_business_partner
                                   CHANGING cv_legacy_number
                                            cv_account_determination
                                            cv_account_class
                                            cv_business_area
                                            cv_company_code.

  DATA: lt_can_data  TYPE STANDARD TABLE OF bapiisuvkp,
        ls_can_data  TYPE bapiisuvkp,
        ls_return    TYPE bapiret2,
        lt_can_lock  TYPE STANDARD TABLE OF bapidfkklocks.

  CALL FUNCTION 'BAPI_ISUACCOUNT_GETDETAIL'
    EXPORTING
      contractaccount      = iv_contract_accnt_num
      partner              = iv_business_partner
    IMPORTING
      return               = ls_return
    TABLES
      tcontractaccountdata = lt_can_data
      tctraclockdetail     = lt_can_lock.
  IF ls_return IS INITIAL.

    READ TABLE lt_can_data INTO ls_can_data INDEX 1.
    IF  sy-subrc = 0.
      cv_legacy_number         = ls_can_data-account_old.
      cv_account_determination = ls_can_data-account_determ_id.
      cv_account_class         = ls_can_data-account_class.
      cv_business_area         = ls_can_data-bus_area.
      cv_company_code          = ls_can_data-resp_comp_code.
    ENDIF.

  ELSE.

    APPEND ls_return TO it_return.

    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-019.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

ENDFORM.                    " GET_CONTRACT_ACCOUNT_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_PREMISE_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_RETURN  text
*      -->IV_ANLAGE  text
*      <--CV_PREMISE_TYPE  text
*      <--CV_NUMBER_OF_PERSONS  text
*----------------------------------------------------------------------*
FORM get_premise_details  TABLES   it_return
                          USING    iv_anlage
                                   iv_vstelle
                          CHANGING cv_premise_type
                                   cv_number_of_persons
                                   cv_regio_group.

  DATA: ls_auto       TYPE isu01_premise_auto,
        ls_return     TYPE bapiret2.

  " Initialize the FM for update
  CALL FUNCTION 'ISU_O_PREMISE_OPEN'
    EXPORTING
      x_vstelle          = iv_vstelle
      x_wmode            = '1'
      x_upd_online       = abap_true
      x_no_dialog        = abap_true
    IMPORTING
      y_auto             = ls_auto
    EXCEPTIONS
      not_found          = 1
      existing           = 2
      foreign_lock       = 3
      invalid_key        = 4
      number_error       = 5
      system_error       = 6
      not_authorized     = 7
      invalid_connobj    = 8
      data_inconsistency = 9
      OTHERS             = 10.
  IF sy-subrc = 0.

    cv_premise_type      = ls_auto-evbsd-vbsart.  " Premise type
    cv_number_of_persons = ls_auto-evbsd-anzpers. " Number of persons

    " Get RSG
    SELECT SINGLE regiogroup
      INTO cv_regio_group
      FROM ehauisu
      WHERE haus = ls_auto-evbsd-haus.

  ELSE.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 INTO ls_return-message
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ls_return-type   = co_error.
    ls_return-id     = sy-msgid.
    ls_return-number = sy-msgno.
    IF NOT ls_return-message IS INITIAL.
      APPEND ls_return TO it_return.
    ENDIF.

    CLEAR ls_return.

    ls_return-type = co_error.
    ls_return-message = text-022.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.


ENDFORM.                    " GET_PREMISE_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_BUSINESS_PARTNERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_RETURN  text
*      -->IT_CONTRACT_ACCOUNTS  text
*----------------------------------------------------------------------*
FORM get_changed_business_partners  TABLES it_contract_accounts
                                    USING  iv_change_date.

  TYPES: BEGIN OF ty_but000,
          partner TYPE bu_partner,
          chdat   TYPE bu_chdat,
         END OF ty_but000,

         BEGIN OF ty_fkkvkp,
           vkont TYPE vkont_kk,
           gpart TYPE gpart_kk,
         END OF ty_fkkvkp.

  DATA: lt_partners  TYPE STANDARD TABLE OF ty_but000,
        lt_contracts TYPE STANDARD TABLE OF ty_fkkvkp.

  FIELD-SYMBOLS: <fs_contracts> TYPE ty_fkkvkp.

  " Get all the Business partners that was changed using the specified date
  SELECT partner
         chdat
    INTO TABLE lt_partners
    FROM but000
    WHERE chdat = iv_change_date.
  IF sy-subrc = 0.

    " Get the contract account number for each Business partner
    SELECT vkont
           gpart
      INTO TABLE lt_contracts
      FROM fkkvkp
      FOR ALL ENTRIES IN lt_partners
      WHERE gpart = lt_partners-partner.

    CLEAR gs_contract_accounts.

    LOOP AT lt_contracts ASSIGNING <fs_contracts>.

      CLEAR gs_contract_accounts.
      gs_contract_accounts-contract_account = <fs_contracts>-vkont.
      APPEND gs_contract_accounts TO it_contract_accounts.


    ENDLOOP.



  ENDIF.



ENDFORM.                    " GET_CHANGED_BUSINESS_PARTNERS
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_PREMISE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*----------------------------------------------------------------------*
FORM get_changed_premise  TABLES it_contract_accounts
                          USING  iv_change_date.

  TYPES: BEGIN OF ty_evbs,
          vstelle TYPE vstelle,
          aedat   TYPE aedat,
          anlage  TYPE anlage,
          vkonto  TYPE vkont_kk,
         END OF ty_evbs.

  DATA: lt_premise TYPE STANDARD TABLE OF ty_evbs.

  FIELD-SYMBOLS: <fs_premise> TYPE ty_evbs.

  SELECT a~vstelle
         a~aedat
         b~anlage
         c~vkonto
    INTO TABLE lt_premise
    FROM ( ( evbs AS a
         INNER JOIN eanl AS b ON a~vstelle = b~vstelle )
         INNER JOIN ever AS c ON c~anlage  = b~anlage  )
    WHERE a~aedat = iv_change_date.
  IF sy-subrc = 0.

    LOOP AT lt_premise ASSIGNING <fs_premise>.

      CLEAR gs_contract_accounts.
      gs_contract_accounts-contract_account = <fs_premise>-vkonto.
      APPEND gs_contract_accounts TO it_contract_accounts.


    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_CHANGED_PREMISE
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_INSTALLATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_installation TABLES it_contract_accounts
                              USING  iv_change_date.

  TYPES: BEGIN OF ty_installation,
          anlage TYPE anlage,
          aedat  TYPE aedat,
          vkonto TYPE vkont_kk,
         END OF ty_installation.

  DATA: lt_installation TYPE STANDARD TABLE OF ty_installation.

  FIELD-SYMBOLS: <fs_installation> TYPE ty_installation.

  SELECT a~anlage
         a~aedat
         b~vkonto
    INTO TABLE lt_installation
    FROM eanl AS a INNER JOIN
         ever AS b
    ON a~anlage = b~anlage
    WHERE a~aedat = iv_change_date.
  IF sy-subrc = 0.

    LOOP AT lt_installation ASSIGNING <fs_installation>.

      CLEAR gs_contract_accounts.
      gs_contract_accounts-contract_account = <fs_installation>-vkonto.
      APPEND gs_contract_accounts TO it_contract_accounts.


    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_CHANGED_INSTALLATION
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_DEVICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_device TABLES it_contract_accounts
                        USING  iv_change_date.

  TYPES: BEGIN OF ty_device,
          equnr   TYPE equnr,
          logiknr TYPE logiknr,
          anlage  TYPE anlage,
         END OF ty_device,

         BEGIN OF ty_ever,
           anlage TYPE anlage,
           vkonto TYPE vkont_kk,
         END OF ty_ever,

         BEGIN OF ty_easts,
           anlage TYPE anlage,
           bis    TYPE biszeitsch,
           vkonto TYPE vkont_kk,
         END OF ty_easts.

  DATA: lt_device TYPE STANDARD TABLE OF ty_device,
        lt_temp   TYPE STANDARD TABLE OF ty_ever,
        lt_easts  TYPE STANDARD TABLE OF ty_easts.

  FIELD-SYMBOLS: <fs_temp>  TYPE ty_ever,
                 <fs_easts> TYPE ty_easts.

  CONSTANTS: co_bis TYPE biszeitsch VALUE '99991231'.

  SELECT a~equnr
         b~logiknr
         c~anlage
    INTO TABLE lt_device
    FROM ( ( equi AS a
         INNER JOIN egerh AS b ON a~equnr   = b~equnr )
         INNER JOIN eastl AS c ON c~logiknr = b~logiknr )
    WHERE a~aedat = iv_change_date AND
          b~bis   = co_bis.
  IF sy-subrc = 0.

    SELECT anlage
           vkonto
      INTO TABLE lt_temp
      FROM ever
      FOR ALL ENTRIES IN lt_device
      WHERE anlage = lt_device-anlage.
    IF sy-subrc = 0.

      LOOP AT lt_temp ASSIGNING <fs_temp>.

        CLEAR gs_contract_accounts.
        gs_contract_accounts-contract_account = <fs_temp>-vkonto.
        APPEND gs_contract_accounts TO it_contract_accounts.

      ENDLOOP.

    ENDIF.

  ENDIF.

  " Change in billing data
  SELECT a~anlage
         a~bis
         b~vkonto
    INTO TABLE lt_easts
    FROM easts AS a INNER JOIN
         ever  AS b
    ON    a~anlage = b~anlage
    WHERE a~aedat  = iv_change_date AND
          a~bis    = co_bis.
  IF  sy-subrc = 0.

    LOOP AT lt_easts ASSIGNING <fs_easts>.

      CLEAR gs_contract_accounts.
      gs_contract_accounts-contract_account = <fs_easts>-vkonto.
      APPEND gs_contract_accounts TO it_contract_accounts.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_CHANGED_DEVICE
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_CAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_can TABLES it_contract_accounts
                     USING  iv_change_date.

  TYPES: BEGIN OF ty_can,
          vkont TYPE vkont_kk,
          aedat TYPE aedat,
         END OF ty_can.

  DATA: lt_can TYPE STANDARD TABLE OF ty_can.

  FIELD-SYMBOLS: <fs_can> TYPE ty_can.

  SELECT vkont
         aedat
    INTO TABLE lt_can
    FROM fkkvk
    WHERE aedat = iv_change_date.
  IF sy-subrc = 0.

    LOOP AT lt_can ASSIGNING <fs_can>.
      CLEAR gs_contract_accounts.
      gs_contract_accounts-contract_account = <fs_can>-vkont.
      APPEND gs_contract_accounts TO it_contract_accounts.
    ENDLOOP.

  ENDIF.



ENDFORM.                    " GET_CHANGED_CAN
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_CONNECTION_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_connection_obj TABLES it_contract_accounts
                                USING  iv_change_date.

  TYPES: BEGIN OF ty_con_obj,
          aedat  TYPE aedat,
          anlage TYPE anlage,
         END OF ty_con_obj,

         BEGIN OF ty_contract,
          anlage TYPE anlage,
          vkonto TYPE vkont_kk,
         END OF ty_contract.

  DATA: lt_con_obj  TYPE STANDARD TABLE OF ty_con_obj,
        lt_contract TYPE STANDARD TABLE OF ty_contract.

  FIELD-SYMBOLS: <fs_contract> TYPE ty_contract.

  SELECT a~aedat
         c~anlage
    INTO TABLE lt_con_obj
    FROM ( ( iflot AS a
         INNER JOIN evbs AS b ON a~tplnr   = b~haus )
         INNER JOIN eanl AS c ON c~vstelle = b~vstelle )
    WHERE a~aedat = iv_change_date.
  IF sy-subrc = 0.

    SELECT anlage
           vkonto
      INTO TABLE lt_contract
      FROM ever
      FOR ALL ENTRIES IN lt_con_obj
      WHERE anlage = lt_con_obj-anlage.
    IF sy-subrc = 0.

      LOOP AT lt_contract ASSIGNING <fs_contract>.

        CLEAR gs_contract_accounts.
        gs_contract_accounts-contract_account = <fs_contract>-vkonto.
        APPEND gs_contract_accounts TO it_contract_accounts.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.                    " GET_CHANGED_CONNECTION_OBJ

*&---------------------------------------------------------------------*
*&      Form  get_changed_meter_removal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_meter_removal TABLES it_contract_accounts
                               USING  iv_change_date.

  TYPES: BEGIN OF ty_regnumber,
           logikzw TYPE logikzw,
         END OF ty_regnumber,

         BEGIN OF ty_instal,
           anlage  TYPE anlage,
           vkont   TYPE vkont_kk,
         END OF ty_instal.

  DATA:  lt_regnumber   TYPE TABLE OF ty_regnumber,
         lt_instal      TYPE TABLE OF ty_instal.

  FIELD-SYMBOLS:
         <fs_instal>    TYPE ty_instal.

  CONSTANTS:
         co_bis         TYPE biszeitsch VALUE '99991231'.

  "-- Get Logical Device Numbers
  SELECT b~logikzw
    INTO TABLE lt_regnumber
  FROM   egerh AS a INNER JOIN etdz AS b ON a~equnr EQ b~equnr
  WHERE  a~ausbdat EQ iv_change_date.

  IF sy-subrc EQ 0.
    "-- Get Installations and Contract Accts.
    SELECT a~anlage b~vkonto
      INTO TABLE lt_instal
    FROM   easts AS a INNER JOIN ever AS b ON a~anlage EQ b~anlage
    FOR ALL ENTRIES IN lt_regnumber
    WHERE  a~logikzw EQ lt_regnumber-logikzw
    AND    a~bis     EQ co_bis.

    IF sy-subrc EQ 0.
      LOOP AT lt_instal ASSIGNING <fs_instal>.
        gs_contract_accounts-contract_account = <fs_instal>-vkont.
        APPEND gs_contract_accounts TO it_contract_accounts.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_changed_meter_removal

*&---------------------------------------------------------------------*
*&      Form  get_changed_move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_CONTRACT_ACCOUNTS  text
*      -->IV_CHANGE_DATE  text
*----------------------------------------------------------------------*
FORM get_changed_move_out TABLES it_contract_accounts
                          USING  iv_change_date.

  TYPES: BEGIN OF ty_moveout,
           vkont      TYPE vkont_kk,
         END OF ty_moveout.

  DATA:  lt_moveout   TYPE TABLE OF ty_moveout.

  FIELD-SYMBOLS:
         <fs_moveout> TYPE ty_moveout.

  "-- Get Move-Out Data
  SELECT vkont
    INTO TABLE lt_moveout
  FROM   eaus
  WHERE  aedat EQ iv_change_date.

  IF sy-subrc EQ 0.
    LOOP AT lt_moveout ASSIGNING <fs_moveout>.
      gs_contract_accounts-contract_account = <fs_moveout>-vkont.
      APPEND gs_contract_accounts TO it_contract_accounts.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_changed_move_out