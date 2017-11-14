  CONSTANTS: co_error   TYPE char1 VALUE 'E',
             co_save    TYPE char4 VALUE 'SAVE'.


*----------------------------------------------------------------------*
***INCLUDE LZFG_CUSTOMER_DATAF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  change_business_partner_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_business_partner_detail TABLES it_return
                                    USING  iv_business_partner        " Business partner
                                           iv_bp_type                 " BP Type
                                           iv_first_name              " First Name
                                           iv_last_name               " Last Name
                                           iv_middle_name             " Middle Name
                                           iv_regio_group             " RSG
                                           iv_social_insurance_number " Social Insurance Number
                                           iv_account_class           " Account Class
                                           iv_company_name            " Company Name
                                           iv_country                 " COUNTRY
                                           iv_region                  " Province
                                           iv_city                    " City/Municipality/Town
                                           iv_district                " District
                                           iv_street5                 " Barangay/Barrio
                                           iv_street                  " Street
                                           iv_street2                 " Area/Zone/Park/Industrial
                                           iv_street3                 " Sitio/Purok
                                           iv_street4                 " Subdivision/Village/Townhouse
                                           iv_house_num_supplement    " House number supplement
                                           iv_house_number            " House Number
                                           iv_postal_code             " ZIP/Postal Code
                                           iv_building                " Building (Number or Code)
                                           iv_floor_in_building       " Floor in building
                                           iv_room_apartment_num      " Room / Unit No.
                                           iv_email                   " Account Name
                                           iv_mobile_phone            " Mobile Phone
                                           iv_sex                     " Sex
                                           iv_telephone               " Telephone
                                           iv_marital_status.         " marital status

  " Data declarations for table parameters of bapi
  DATA: lt_telefondatanonaddress  TYPE STANDARD TABLE OF bapiadtel,
        lt_telefondatanonaddressx TYPE STANDARD TABLE OF bapiadtelx,
        lt_e_maildatanonaddress   TYPE STANDARD TABLE OF bapiadsmtp,
        lt_e_maildatanonaddressx  TYPE STANDARD TABLE OF bapiadsmtx,
        ls_maildatanonaddress     TYPE bapiadsmtp,
        ls_maildatanonaddressx    TYPE bapiadsmtx.

  DATA: lt_bapiadtel  TYPE STANDARD TABLE OF bapiadtel,
        lt_bapiadtelx TYPE STANDARD TABLE OF bapiadtelx,
        ls_bapiadtel  TYPE bapiadtel,
        ls_bapiadtelx TYPE bapiadtelx,
        lt_bapiadsmtp TYPE STANDARD TABLE OF bapiadsmtp,
        lt_bapiadsmtx TYPE STANDARD TABLE OF bapiadsmtx,
        ls_bapiadsmtp TYPE bapiadsmtp,
        ls_bapiadsmtx TYPE bapiadsmtx,
        lt_return     TYPE STANDARD TABLE OF bapiret2.

  DATA: ls_addressdata   TYPE bapibus1006_address,
        ls_addressdata_x TYPE bapibus1006_address_x,
        ls_return        TYPE bapiret2.

  DATA: lv_addrnumber    TYPE ad_addrnum,
        lv_tel_number    TYPE ad_tlnmbr,
        lv_mobile_number TYPE ad_tlnmbr,
        lv_smtp_addr     TYPE ad_smtpadr,
        ls_partnerdata   TYPE bapiisubpd,
        ls_partnerdatax  TYPE bapiisubpdx.

  CONSTANTS: co_male   TYPE char1     VALUE 'M',
             co_female TYPE char1     VALUE 'F'.

  ls_addressdata-street     = iv_street.       " Street
  ls_addressdata-house_no   = iv_house_number. " House Number
  ls_addressdata-location   = iv_street5.      " Barangay/Barrio
  ls_addressdata-district   = iv_district.     " District
  ls_addressdata-postl_cod1 = iv_postal_code.  " ZIP/Postal Code
  ls_addressdata-city       = iv_city.         " City/Municipality/Town
  ls_addressdata-country    = iv_country.      " COUNTRY
  ls_addressdata-region     = iv_region.       " Province

  ls_addressdata-str_suppl1 = iv_street2.  " Area/Zone/Park/Industrial
  ls_addressdata-str_suppl2 = iv_street3.  " Sitio/Purok
  ls_addressdata-str_suppl3 = iv_street4.  " Subdivision/Village/Townhouse
  ls_addressdata-house_no2  = iv_house_num_supplement. " House number supplement
  ls_addressdata-building   = iv_building. " Building (Number or Code)
  ls_addressdata-floor      = iv_floor_in_building. " Floor in building
  ls_addressdata-room_no    = iv_room_apartment_num. " Room / Unit No.

  "Set the Change flag
  ls_addressdata_x-street     = abap_true.
  ls_addressdata_x-house_no   = abap_true.
  ls_addressdata_x-location   = abap_true.
  ls_addressdata_x-district   = abap_true.
  ls_addressdata_x-postl_cod1 = abap_true.
  ls_addressdata_x-city       = abap_true.
  ls_addressdata_x-country    = abap_true.
  ls_addressdata_x-region     = abap_true.

  ls_addressdata_x-str_suppl1   = abap_true.
  ls_addressdata_x-str_suppl2   = abap_true.
  ls_addressdata_x-str_suppl3   = abap_true.
  ls_addressdata_x-house_no2    = abap_true.
  ls_addressdata_x-building     = abap_true.
  ls_addressdata_x-floor        = abap_true.
  ls_addressdata_x-room_no      = abap_true.

  SELECT SINGLE addrnumber
    INTO lv_addrnumber
    FROM but020
    WHERE partner = iv_business_partner.
  IF sy-subrc = 0.
    " Get the current mobile number
    SELECT SINGLE tel_number
    INTO lv_mobile_number
    FROM adr2
    WHERE addrnumber = lv_addrnumber AND
          r3_user    = '3'.

    " Get the current landline telephone number
    SELECT SINGLE tel_number
    INTO lv_tel_number
    FROM adr2
    WHERE addrnumber = lv_addrnumber AND
          r3_user    = '1'.

    " Get the current email address
    SELECT SINGLE smtp_addr
    INTO lv_smtp_addr
    FROM adr6
    WHERE addrnumber = lv_addrnumber.

  ENDIF.

  " set the mobile phone number
  ls_bapiadtel-telephone = iv_mobile_phone. " Mobile Phone
  ls_bapiadtel-r_3_user  = '3'.
  ls_bapiadtel-std_no    = abap_true.
  APPEND ls_bapiadtel TO lt_bapiadtel.

  " check if there is a change in the mobile phone number
  IF NOT lv_mobile_number EQ iv_mobile_phone AND
     NOT lv_mobile_number IS INITIAL.

    "set the change flag for the mobile phone number
    ls_bapiadtelx-telephone = abap_true.
    ls_bapiadtelx-r_3_user  = abap_true.
    ls_bapiadtelx-updateflag = 'U'.
    ls_bapiadtelx-std_no     = abap_true.

  ELSEIF NOT lv_tel_number EQ iv_mobile_phone AND
             lv_tel_number IS INITIAL.

    "set the change flag for the mobile phone number
    ls_bapiadtelx-telephone = abap_true.
    ls_bapiadtelx-r_3_user  = abap_true.
    ls_bapiadtelx-updateflag = 'I'.
    ls_bapiadtelx-std_no     = abap_true.

  ENDIF.
  APPEND ls_bapiadtelx TO lt_bapiadtelx.

  " Landline telephone number
  ls_bapiadtel-telephone = iv_telephone.
  ls_bapiadtel-r_3_user  = '1'.
  ls_bapiadtel-std_no    = abap_true.
  APPEND ls_bapiadtel TO lt_bapiadtel.

  " check if there is a change in the landling phone number
  IF NOT lv_tel_number EQ iv_telephone AND
     NOT lv_tel_number IS INITIAL.

    "set the change flag for the landling phone number
    ls_bapiadtelx-telephone = abap_true.
    ls_bapiadtelx-r_3_user  = abap_true.
    ls_bapiadtelx-updateflag = 'U'.
    ls_bapiadtelx-std_no     = abap_true.

  ELSEIF NOT lv_tel_number EQ iv_mobile_phone AND
             lv_tel_number IS INITIAL.

    "set the change flag for the landling phone number
    ls_bapiadtelx-telephone = abap_true.
    ls_bapiadtelx-r_3_user  = abap_true.
    ls_bapiadtelx-updateflag = 'I'.
    ls_bapiadtelx-std_no     = abap_true.

  ENDIF.
  APPEND ls_bapiadtelx TO lt_bapiadtelx.

  " set the email address new value
  ls_bapiadsmtp-e_mail   = iv_email.    " Email
  ls_bapiadsmtp-r_3_user = abap_true.
  ls_bapiadsmtp-std_no   = abap_true.
  APPEND ls_bapiadsmtp TO lt_bapiadsmtp.


  " check if there is a change in the email address
  IF NOT lv_smtp_addr EQ iv_email AND
     NOT lv_smtp_addr IS INITIAL.

    "set the change flag for the email address
    ls_bapiadsmtx-r_3_user   = abap_true.
    ls_bapiadsmtx-e_mail     = abap_true.
    ls_bapiadsmtx-updateflag = 'U'.
    ls_bapiadsmtx-std_no     = abap_true.

  ELSEIF NOT lv_smtp_addr EQ iv_email AND
             lv_smtp_addr IS INITIAL.

    "set the change flag for the email address
    ls_bapiadsmtx-r_3_user   = abap_true.
    ls_bapiadsmtx-e_mail     = abap_true.
    ls_bapiadsmtx-updateflag = 'I'.
    ls_bapiadsmtx-std_no     = abap_true.

  ENDIF.

  APPEND ls_bapiadsmtx TO lt_bapiadsmtx.

  " Call the FM to change the business partner address details
  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner = iv_business_partner
      addressdata     = ls_addressdata
      addressdata_x   = ls_addressdata_x
    TABLES
      bapiadtel       = lt_bapiadtel
      bapiadsmtp      = lt_bapiadsmtp
      bapiadtel_x     = lt_bapiadtelx
      bapiadsmt_x     = lt_bapiadsmtx
      return          = lt_return.
  IF lt_return[] IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ELSE.

    " Fill the return table
    APPEND LINES OF lt_return TO it_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR ls_return.
    CONCATENATE text-006
                iv_business_partner
                INTO ls_return-message
                SEPARATED BY space.
    ls_return-type = co_error.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  " Proceed with the update of business partner Identification details

  " Fill up the parameters of the BAPI and pass the values to be change
  ls_partnerdata-bpkind         = iv_bp_type.                 " BP Type
  ls_partnerdata-name_first     = iv_first_name.              " First Name
  ls_partnerdata-name_last      = iv_last_name.               " Last Name

  ls_partnerdata-namemiddle     = iv_middle_name.             " Middle Name
  ls_partnerdata-regiogroup     = iv_regio_group.             " RSG
  ls_partnerdata-soc_secure     = iv_social_insurance_number. " Social Insurance Number
  ls_partnerdata-account_class  = iv_account_class.           " Account Class
  ls_partnerdata-name_org1      = iv_company_name.            " Company Name
  ls_partnerdata-marriage_state = iv_marital_status.          " marital status

  " Sex
  IF iv_sex = co_male.
    ls_partnerdata-sex_m = abap_true.
    ls_partnerdata-sex_f = abap_false.
  ELSEIF iv_sex = co_female.
    ls_partnerdata-sex_m = abap_false.
    ls_partnerdata-sex_f = abap_true.
  ENDIF.

  ls_partnerdatax-bpkind         = abap_true. " BP Type
  ls_partnerdatax-name_first     = abap_true. " First Name
  ls_partnerdatax-name_last      = abap_true. " Last Name
  ls_partnerdatax-namemiddle     = abap_true. " Middle Name
  ls_partnerdatax-regiogroup     = abap_true. " RSG
  ls_partnerdatax-soc_secure     = abap_true. " Social Insurance Number
  ls_partnerdatax-account_class  = abap_true. " Account Class
  ls_partnerdatax-marriage_state = abap_true. " marital status

  IF iv_bp_type = '0002'. " Organization
    ls_partnerdatax-name_first = abap_false.
    ls_partnerdatax-name_last  = abap_false.
    ls_partnerdatax-namemiddle = abap_false.
    ls_partnerdatax-name_org1  = abap_true.
  ELSE. " Individual
    ls_partnerdatax-name_first = abap_true.
    ls_partnerdatax-name_last  = abap_true.
    ls_partnerdatax-namemiddle = abap_true.
    ls_partnerdatax-name_org1  = abap_false.
  ENDIF.

  ls_partnerdatax-sex_m = abap_true.
  ls_partnerdatax-sex_f = abap_true.

  " Change the email address ( Address independent )
  ls_maildatanonaddress-e_mail   = iv_email.
  ls_maildatanonaddress-r_3_user = abap_true.
  ls_maildatanonaddress-std_no   = abap_true.
  APPEND ls_maildatanonaddress TO lt_e_maildatanonaddress.

  "set the change flag for the email address
  ls_maildatanonaddressx-r_3_user   = abap_true.
  ls_maildatanonaddressx-e_mail     = abap_true.
  ls_maildatanonaddressx-updateflag = 'I'.
  ls_maildatanonaddressx-std_no     = abap_true.
  APPEND ls_maildatanonaddressx TO lt_e_maildatanonaddressx.

  " Call the BAPI to change the Business partner details
  CLEAR ls_return.
  CALL FUNCTION 'BAPI_ISUPARTNER_CHANGE'
    EXPORTING
      partner                = iv_business_partner
      partnerdata            = ls_partnerdata
      partnerdatax           = ls_partnerdatax
    IMPORTING
      return                 = ls_return
    TABLES
      telefondatanonaddress  = lt_telefondatanonaddress
      telefondatanonaddressx = lt_telefondatanonaddressx
      e_maildatanonaddress   = lt_e_maildatanonaddress
      e_maildatanonaddressx  = lt_e_maildatanonaddressx.
  IF ls_return-type = co_error.

    " Fill the return table
    APPEND ls_return TO it_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR ls_return.
    CONCATENATE text-006
                iv_business_partner
                INTO ls_return-message
                SEPARATED BY space.
    ls_return-type = co_error.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDIF.

ENDFORM.                    " change_business_partner_detail
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DEVICE_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_CONTRACT_ACCNT_NUM  text
*----------------------------------------------------------------------*
FORM change_device_details TABLES   it_return
                           USING    iv_manufacturer
                                    iv_anlage
                                    iv_construct_year
                                    iv_business_area
                                    iv_company_code
                                    iv_sort
                           CHANGING cv_devloc.

  DATA: lv_logiknr TYPE logiknr,
        lv_equnr   TYPE equnr.

  DATA: ls_data_general   TYPE bapi_itob,
        ls_data_generalx  TYPE bapi_itobx,
        ls_data_specific  TYPE bapi_itob_eq_only,
        ls_data_specificx TYPE bapi_itob_eq_onlyx,
        ls_return         TYPE bapiret2.

  CONSTANTS: co_bis TYPE biszeitsch VALUE '99991231'.

  " Get the Logical device number
  SELECT SINGLE logiknr
    INTO lv_logiknr
    FROM eastl
    WHERE anlage = iv_anlage.
  IF sy-subrc = 0.
    " Get the device number and the device location
    SELECT SINGLE equnr
                  devloc
      INTO (lv_equnr,cv_devloc)
      FROM egerh
      WHERE logiknr = lv_logiknr AND
            bis     = co_bis.
  ENDIF.

  ls_data_general-manfacture = iv_manufacturer.
  ls_data_general-constyear  = iv_construct_year.
  ls_data_general-bus_area   = iv_business_area.
  ls_data_general-planplant  = iv_business_area.
  ls_data_general-comp_code  = iv_company_code.
  ls_data_general-sortfield  = iv_sort.

  ls_data_generalx-manfacture = abap_true.
  ls_data_generalx-obj_size   = abap_true.
  ls_data_generalx-constyear  = abap_true.
  ls_data_generalx-bus_area   = abap_true.
  ls_data_generalx-planplant  = abap_true.
  ls_data_generalx-comp_code  = abap_true.
  ls_data_generalx-sortfield  = abap_true.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
    EXPORTING
      equipment      = lv_equnr
      data_general   = ls_data_general
      data_generalx  = ls_data_generalx
      data_specific  = ls_data_specific
      data_specificx = ls_data_specificx
    IMPORTING
      return         = ls_return.
  IF ls_return IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

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
    ls_return-message = text-003.
    APPEND ls_return TO it_return.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFORM.                    " CHANGE_DEVICE_DETAILS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PREMISE_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PREMISE  text
*      -->P_IV_PREMISE_TYPE  text
*      -->P_IV_NUMBER_OF_PERSONS  text
*----------------------------------------------------------------------*
FORM change_premise_details TABLES   it_return
                            USING    iv_anlage
                                     iv_premise_type
                                     iv_number_of_persons
                                     iv_floor_in_building
                                     iv_room_apartment_num
                                     iv_street3
                                     iv_street5
                                     iv_house_num_supplement.

  DATA: lv_premise    TYPE vstelle,
        ls_obj        TYPE isu01_premise,
        ls_auto       TYPE isu01_premise_auto,
        lv_update     TYPE e_update,
        ls_return     TYPE bapiret2.

  " Get the premise number
  SELECT SINGLE vstelle
    INTO lv_premise
    FROM eanl
    WHERE anlage = iv_anlage.

  " Initialize the FM for update
  CALL FUNCTION 'ISU_O_PREMISE_OPEN'
    EXPORTING
      x_vstelle          = lv_premise
      x_wmode            = '2'
      x_upd_online       = abap_true
      x_no_dialog        = abap_true
    IMPORTING
      y_obj              = ls_obj
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
  IF sy-subrc <> 0.

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
    ls_return-message = text-007.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    " Fill the parameters and pass the data to be changed
    ls_auto-evbsd-vbsart     = iv_premise_type.       " Premise type
    ls_auto-evbsd-anzpers    = iv_number_of_persons.  " Number of persons
    ls_auto-evbsd-floor      = iv_floor_in_building.  " Floor number
    ls_auto-evbsd-roomnumber = iv_room_apartment_num. " Room number

    ls_auto-evbsd-str_erg2  = iv_street3.
    ls_auto-evbsd-str_erg4  = iv_street5.
    ls_auto-evbsd-haus_num2 = iv_house_num_supplement.

    ls_auto-evbsd_use    = abap_true.
    ls_auto-evbsd_okcode = abap_true.
    ls_auto-evbsd_done   = abap_true.

    ls_obj-contr-save_okcode = co_save.
    ls_obj-auto-evbsd_use    = abap_true.
    ls_obj-auto-evbsd_okcode = abap_true.
    ls_obj-auto-evbsd_done   = abap_true.

    " Call the FM to udpate the premise
    CALL FUNCTION 'ISU_S_PREMISE_CHANGE'
      EXPORTING
        x_vstelle      = lv_premise
        x_upd_online   = abap_true
        x_no_dialog    = abap_true
        x_auto         = ls_auto
        x_obj          = ls_obj
      IMPORTING
        y_db_update    = lv_update
      EXCEPTIONS
        not_found      = 1
        foreign_lock   = 2
        input_error    = 3
        general_fault  = 4
        not_authorized = 5
        OTHERS         = 6.
    IF sy-subrc <> 0.

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
      ls_return-message = text-007.
      APPEND ls_return TO it_return.
      EXIT.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_PREMISE_DETAILS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_UTILITY_INSTALLATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_utility_installation TABLES   it_return
                                 USING    iv_anlage
                                          iv_billing_class
                                          iv_rate_category
                                          iv_mru_num
                                          iv_installation_type.

  DATA: lv_ab          TYPE abzeitsch,
        ls_obj         TYPE isu01_instln,
        ls_auto        TYPE isu01_instln_auto,
        lv_db_update   TYPE regen-db_update,
        lv_exit_type   TYPE regen-exit_type,
        lt_eanl        TYPE STANDARD TABLE OF v_eanl,
        ls_return      TYPE bapiret2.

  " Get the Valid from date
  SELECT SINGLE ab
    INTO lv_ab
    FROM eanlh
    WHERE anlage = iv_anlage.

  " Get the Installation details
  CALL FUNCTION 'ISU_S_INSTLN_PROVIDE'
    EXPORTING
      x_anlage        = iv_anlage
      x_keydate       = sy-datum
      x_wmode         = '2'
      x_no_dialog     = abap_true
    IMPORTING
      y_auto          = ls_auto
      y_obj           = ls_obj
    EXCEPTIONS
      not_found       = 1
      invalid_keydate = 2
      foreign_lock    = 3
      not_authorized  = 4
      invalid_wmode   = 5
      general_fault   = 6
      OTHERS          = 7.
  IF sy-subrc <> 0.

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
    ls_return-message = text-002.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    " Pass the data to be change to the parameters
    ls_auto-data-tariftyp = iv_rate_category. "Rate category
    ls_auto-data-aklasse  = iv_billing_class. "Billing class

    ls_auto-data-anlart  = iv_installation_type.
    ls_auto-data-ableinh = iv_mru_num.

    ls_auto-contr-use-data   = abap_true.
    ls_auto-contr-use-okcode = abap_true.
    ls_auto-contr-okcode     = co_save.

    CLEAR ls_obj-contr-begabrpe.

    " Call the FM to perform the update
    CALL FUNCTION 'ISU_S_INSTLN_CHANGE'
      EXPORTING
        x_anlage       = iv_anlage
        x_keydate      = lv_ab
        x_upd_online   = abap_true
        x_no_dialog    = abap_true
        x_auto         = ls_auto
        x_obj          = ls_obj
      IMPORTING
        y_db_update    = lv_db_update
        y_exit_type    = lv_exit_type
      TABLES
        yt_new_eanl    = lt_eanl
      EXCEPTIONS
        not_found      = 1
        foreign_lock   = 2
        not_authorized = 3
        cancelled      = 4
        input_error    = 5
        general_fault  = 6
        OTHERS         = 7.
    IF sy-subrc <> 0.

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
      ls_return-message = text-002.
      APPEND ls_return TO it_return.
      EXIT.

    ELSE.

      COMMIT WORK.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_UTILITY_INSTALLATION
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DEVICE_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_RETURN  text
*      -->P_IV_BUSINESS_PARTNER  text
*      <--P_LV_ERROR_FLAG  text
*----------------------------------------------------------------------*
FORM change_device_location  TABLES   it_return
                             USING    iv_devloc
                                      iv_additional_info.

  DATA: ls_obj    TYPE isu65_devloc,
        ls_auto   TYPE isu01_devloc_auto,
        ls_return TYPE bapiret2.

  DATA: lv_db_update  TYPE regen-db_update,
        lv_exit_type  TYPE regen-exit_type,
        ls_new_egpl  	TYPE egpl.

  " Get the Details of the Device location
  CALL FUNCTION 'ISU_S_DEVLOC_PROVIDE'
    EXPORTING
      x_devloc      = iv_devloc
      x_wmode       = '2'
    IMPORTING
      y_obj         = ls_obj
      y_auto        = ls_auto
    EXCEPTIONS
      not_found     = 1
      foreign_lock  = 2
      general_fault = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.

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
    ls_return-message = text-010.
    APPEND ls_return TO it_return.
    EXIT.
  ENDIF.

  " Fill the parameter and the data to be changed

  ls_auto-egpld-stortzus = iv_additional_info. " Addition to device location

  ls_auto-egpld-wmode      = '2'.
  ls_auto-egpld_okcode     = co_save.
  ls_auto-egpld_use        = abap_true.
  ls_auto-egpld_done       = abap_true.
  ls_obj-contr-wmode       = '2'.
  ls_obj-auto-egpld_use    = abap_true.
  ls_obj-auto-egpld_okcode = co_save.
  ls_obj-auto-egpld_done   = abap_true.

  " Call the FM to Save the changes in the device location
  CALL FUNCTION 'ZISU_S_DEVLOC_CHANGE'
    EXPORTING
      x_devloc      = iv_devloc
      x_upd_online  = abap_true
      x_no_dialog   = abap_true
      x_auto        = ls_auto
      x_obj         = ls_obj
    IMPORTING
      y_db_update   = lv_db_update
      y_exit_type   = lv_exit_type
      y_new_egpl    = ls_new_egpl
    EXCEPTIONS
      not_found     = 1
      foreign_lock  = 2
      general_fault = 3
      input_error   = 4
      status        = 5
      OTHERS        = 6.
  IF sy-subrc <> 0.

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
    ls_return-message = text-010.
    APPEND ls_return TO it_return.
    EXIT.
  ENDIF.

ENDFORM.                    " CHANGE_DEVICE_LOCATION
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DEVICE_LOCATION_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_RETURN  text
*      -->P_LV_DEVLOC  text
*      -->P_IV_ADDITIONAL_INFO  text
*      -->P_IV_LOCATION  text
*      <--P_LV_ERROR_FLAG  text
*----------------------------------------------------------------------*
FORM change_device_location_details TABLES   it_return
                                    USING    iv_devloc
                                             iv_location
                                             iv_construct_year
                                             iv_company_code
                                             iv_manufacturer
                                             iv_business_area
                                             iv_sort.

  DATA: ls_data_general   TYPE  bapi_itob,
        ls_data_generalx  TYPE  bapi_itobx,
        ls_data_specific  TYPE  bapi_itob_fl_only,
        ls_data_specificx TYPE  bapi_itob_fl_onlyx.

  DATA: ls_retrn_data_general TYPE bapi_itob,
        ls_retrn_specific     TYPE bapi_itob_fl_only,
        ls_return             TYPE bapiret2.

  " Fill the parameters and pass the data to be changed
  ls_data_general-maintloc   = iv_location.
  ls_data_general-constyear  = iv_construct_year.
  ls_data_general-manfacture = iv_manufacturer.
  ls_data_general-comp_code  = iv_company_code.
  ls_data_general-bus_area   = iv_business_area.
  ls_data_general-planplant  = iv_business_area.
  ls_data_general-sortfield  = iv_sort.

  " Set the changed flag
  ls_data_generalx-maintloc   = abap_true.
  ls_data_generalx-constyear  = abap_true.
  ls_data_generalx-obj_size   = abap_true.
  ls_data_generalx-manfacture = abap_true.
  ls_data_generalx-comp_code  = abap_true.
  ls_data_generalx-bus_area   = abap_true.
  ls_data_generalx-planplant  = abap_true.
  ls_data_generalx-sortfield  = abap_true.

  CALL FUNCTION 'BAPI_FUNCLOC_CHANGE'
    EXPORTING
      functlocation     = iv_devloc
      data_general      = ls_data_general
      data_generalx     = ls_data_generalx
      data_specific     = ls_data_specific
      data_specificx    = ls_data_specificx
    IMPORTING
      data_general_exp  = ls_retrn_data_general
      data_specific_exp = ls_retrn_specific
      return            = ls_return.
  IF ls_return IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ELSE.

    APPEND ls_return TO it_return.
    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-004.
    APPEND ls_return TO it_return.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFORM.                    " CHANGE_DEVICE_LOCATION_DETAILS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_CONTRACT_ACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_contract_account TABLES   it_return
                             USING    iv_contract_accnt_num
                                      iv_business_partner
                                      iv_legacy_number
                                      iv_account_determination
                                      iv_business_area
                                      iv_account_class
                                      iv_bp_type
                                      iv_first_name
                                      iv_last_name
                                      iv_company_name
                                      iv_company_code.

  DATA: ls_can_data	  TYPE bapiisuvkp,
        ls_can_dataxt TYPE bapiisuvkpx,
        ls_return	    TYPE bapiret2.

  " Fill the bapi parameters and set the changed flag of the fields to be changed
  ls_can_data-account_old        = iv_legacy_number.
  ls_can_data-account_determ_id  = iv_account_determination.
  ls_can_data-bus_area           = iv_business_area.
  ls_can_data-account_class      = iv_account_class.
  ls_can_data-exempt_no          = iv_company_code.
  ls_can_data-resp_comp_code     = iv_company_code.
  ls_can_data-standard_comp_code = iv_company_code.

  CASE iv_bp_type.
    WHEN '0001'. "Individual
      CONCATENATE iv_first_name
                  iv_last_name
             INTO ls_can_data-ca_name
             SEPARATED BY space.
    WHEN '0002'. " Organization
      ls_can_data-ca_name = iv_company_name.
  ENDCASE.

  ls_can_dataxt-account_old        = abap_true.
  ls_can_dataxt-account_determ_id  = abap_true.
  ls_can_dataxt-bus_area           = abap_true.
  ls_can_dataxt-account_class      = abap_true.
  ls_can_dataxt-ca_name            = abap_true.
  ls_can_dataxt-exempt_no          = abap_true.
  ls_can_dataxt-resp_comp_code     = abap_true.
  ls_can_dataxt-standard_comp_code = abap_true.

  " Call the bapi
  CALL FUNCTION 'BAPI_ISUACCOUNT_CHANGE'
    EXPORTING
      contractaccount      = iv_contract_accnt_num
      partner              = iv_business_partner
      contractaccountdata  = ls_can_data
      contractaccountdatax = ls_can_dataxt
    IMPORTING
      return               = ls_return.
  IF ls_return IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ELSE.

    APPEND ls_return TO it_return.
    CLEAR ls_return.
    ls_return-type = co_error.
    ls_return-message = text-005.
    APPEND ls_return TO it_return.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFORM.                    " CHANGE_CONTRACT_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DEVICE_RATE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_device_rate_type TABLES   it_return
                             USING    iv_anlage
                                      iv_rate_type.

  DATA: ls_obj         TYPE isu70_devicerate,
        ls_auto	       TYPE	isu70_devicerate_auto,
        ls_time	       TYPE	isu07_timeslice_tab,
        lv_wmode       TYPE regen-wmode,
        lv_select_date TYPE	abzeitsch,
        lv_db_update   TYPE  regen-db_update,
        lv_exit_type   TYPE  regen-exit_type,
        ls_return      TYPE bapiret2.

  FIELD-SYMBOLS: <fs_reg> TYPE reg70_r.

  " Get the device details and initialize the FM
  CALL FUNCTION 'ISU_O_DEVICERATE_OPEN'
    EXPORTING
      x_anlage         = iv_anlage
      x_stichtag       = sy-datum
      x_wmode          = '2'
      x_upd_online     = abap_true
      x_no_dialog      = abap_true
      x_called_by_ec30 = abap_true
    IMPORTING
      y_obj            = ls_obj
      y_auto           = ls_auto
      y_time           = ls_time
      y_wmode          = lv_wmode
      y_select_date    = lv_select_date
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
  IF sy-subrc <> 0.

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
    ls_return-message = text-008.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    " Set the FM parameters and pass the values to be changed
    ls_auto-contr-okcode = co_save.
    ls_auto-contr-use-okcode = abap_true.

    DELETE ls_auto-reg WHERE bis NE '99991231'.
    READ TABLE ls_auto-reg ASSIGNING <fs_reg> WITH KEY bis = '99991231'.
    IF sy-subrc = 0.
      <fs_reg>-tarifart = iv_rate_type.
    ENDIF.

    DELETE ls_obj-obj-reg_all WHERE bis NE '99991231'.
    READ TABLE ls_obj-obj-reg_all ASSIGNING <fs_reg> WITH KEY bis = '99991231'.
    IF sy-subrc = 0.
      <fs_reg>-tarifart = iv_rate_type.
    ENDIF.

    ls_obj-auto-contr-use-okcode     = abap_true.
    ls_obj-auto-contr-okcode         = co_save.
    ls_obj-auto-meterdoc-eabl_okcode = co_save.
    ls_obj-auto-meterdoc-eabl_done   = abap_true.
    ls_obj-auto-consumpt-okcode      = co_save.

    " Call the FM to update the rate type
    CALL FUNCTION 'ISU_S_DEVICERATE_CHANGE'
      EXPORTING
        x_anlage         = iv_anlage
        x_stichtag       = sy-datum
        x_upd_online     = abap_true
        x_no_dialog      = abap_true
        x_auto           = ls_auto
        x_obj            = ls_obj
        x_no_entry_check = abap_true
      IMPORTING
        y_db_update      = lv_db_update
        y_exit_type      = lv_exit_type
      EXCEPTIONS
        not_found        = 1
        foreign_lock     = 2
        cancelled        = 3
        input_error      = 4
        general_fault    = 5
        OTHERS           = 6.
    IF sy-subrc <> 0.

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
      ls_return-message = text-008.
      APPEND ls_return TO it_return.
      EXIT.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_DEVICE_RATE_TYPE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_CONNECTION_OBJT_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_connection_objt_details TABLES it_return
                                    USING  iv_anlage
                                           iv_regio_group
                                           iv_street
                                           iv_house_number
                                           iv_street5
                                           iv_district
                                           iv_postal_code
                                           iv_city
                                           iv_country
                                           iv_region
                                           iv_street2
                                           iv_street3
                                           iv_street4
                                           iv_house_num_supplement
                                           iv_building
                                           iv_floor_in_building
                                           iv_room_apartment_num
                                           iv_mobile_phone
                                           iv_telephone.

  DATA: lv_premise          TYPE vstelle,
        lv_connection_obj   TYPE haus,
        ls_obj              TYPE isu01_connobj,
        ls_auto             TYPE isu01_connobj_auto,
        lv_db_update        TYPE e_update,
        ls_return           TYPE bapiret2,
        ls_auto_temp        TYPE isu01_connobj_auto,
        ls_address_data     TYPE addr1_data,
        lv_addrnum          TYPE ad_addrnum,
        lv_returncode       TYPE szad_field-returncode,
        lv_data_has_changed TYPE t_boole,
        lt_addr_error       TYPE STANDARD TABLE OF addr_error.

  DATA: lt_adtel TYPE STANDARD TABLE OF adtel,
        lt_error TYPE STANDARD TABLE OF addr_error.

  FIELD-SYMBOLS: <fs_adtel> TYPE adtel.

  " Get the premise and the connection object number
  SELECT SINGLE vstelle
    INTO lv_premise
    FROM eanl
    WHERE anlage = iv_anlage.
  IF sy-subrc = 0.
    SELECT SINGLE haus
      INTO lv_connection_obj
      FROM evbs
      WHERE vstelle = lv_premise.
  ENDIF.

  " Initialize the FM
  CALL FUNCTION 'ISU_O_CONNOBJ_OPEN'
    EXPORTING
      x_haus         = lv_connection_obj
      x_wmode        = '2'
      x_upd_online   = abap_true
      x_no_dialog    = abap_true
      x_auto         = ls_auto_temp
    IMPORTING
      y_obj          = ls_obj
      y_auto         = ls_auto
    EXCEPTIONS
      not_found      = 1
      existing       = 2
      foreign_lock   = 3
      invalid_key    = 4
      number_error   = 5
      system_error   = 6
      not_authorized = 7
      not_customized = 8
      status         = 9
      OTHERS         = 10.
  IF sy-subrc <> 0.

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
    ls_return-message = text-011.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  ls_auto-ehaud_use    = abap_true.
  ls_auto-ehaud_okcode = co_save.
  ls_auto-ehaud_done   = abap_true.
  ls_auto-objadr_done  = abap_true.

  ls_auto-ehaud-regiogroup = iv_regio_group.

  MOVE-CORRESPONDING ls_obj-addr1_data_old TO ls_auto-addr_data.
  ls_auto-addr_data-regiogroup = iv_regio_group.

  " Call the FM to change the attribute of the connection object( RSG)
  CALL FUNCTION 'ZISU_S_CONNOBJ_CHANGE'
    EXPORTING
      x_haus         = lv_connection_obj
      x_upd_online   = abap_true
      x_no_dialog    = abap_true
      x_auto         = ls_auto
      x_obj          = ls_obj
    IMPORTING
      y_db_update    = lv_db_update
    EXCEPTIONS
      not_found      = 1
      foreign_lock   = 2
      general_fault  = 3
      invalid_key    = 4
      not_authorized = 5
      input_error    = 6
      status         = 7
      OTHERS         = 8.
  IF sy-subrc <> 0.

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
    ls_return-message = text-011.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  lv_addrnum = ls_obj-addrobj-addrnumber.

  MOVE-CORRESPONDING ls_auto-addr_data TO ls_address_data.

  ls_address_data-street     = iv_street.       " Street
  ls_address_data-house_num1 = iv_house_number. " House Number
  ls_address_data-location   = iv_street5.      " Barangay/Barrio
  ls_address_data-city2      = iv_district.     " District
  ls_address_data-post_code1 = iv_postal_code.  " ZIP/Postal Code
  ls_address_data-city1      = iv_city.         " City/Municipality/Town
  ls_address_data-country    = iv_country.      " COUNTRY
  ls_address_data-region     = iv_region.       " Province

  ls_address_data-str_suppl1 = iv_street2.      " Area/Zone/Park/Industrial
  ls_address_data-str_suppl2 = iv_street3.      " Sitio/Purok
  ls_address_data-str_suppl3 = iv_street4.      " Subdivision/Village/Townhouse
  ls_address_data-house_num2 = iv_house_num_supplement. " House number supplement
  ls_address_data-building   = iv_building.             " Building (Number or Code)
  ls_address_data-floor      = iv_floor_in_building.    " Floor in building
  ls_address_data-roomnumber = iv_room_apartment_num.   " Room / Unit No.

  " Update the address details of the Connection object
  CALL FUNCTION 'ADDR_UPDATE'
    EXPORTING
      address_data      = ls_address_data
      address_number    = lv_addrnum
    IMPORTING
      address_data      = ls_address_data
      returncode        = lv_returncode
      data_has_changed  = lv_data_has_changed
    TABLES
      error_table       = lt_addr_error
    EXCEPTIONS
      address_not_exist = 1
      parameter_error   = 2
      version_not_exist = 3
      internal_error    = 4
      OTHERS            = 5.
  IF sy-subrc = 0.
    " Save the changes to the database
    CALL FUNCTION 'ADDR_MEMORY_SAVE'
      EXCEPTIONS
        address_number_missing = 1
        person_number_missing  = 2
        internal_error         = 3
        database_error         = 4
        reference_missing      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
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
      ls_return-message = text-012.
      APPEND ls_return TO it_return.
      EXIT.

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
    ls_return-message = text-012.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  " Change the mobile phone number
  CALL FUNCTION 'ADDR_COMM_GET'
    EXPORTING
      address_number    = lv_addrnum
      table_type        = 'ADTEL'
      iv_current_state  = space
    TABLES
      comm_table        = lt_adtel
      error_table       = lt_error
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      internal_error    = 3
      OTHERS            = 4.
  IF sy-subrc = 0.
    " Insert or update the mobile phone number
    READ TABLE lt_adtel ASSIGNING <fs_adtel> INDEX 1.
    IF sy-subrc = 0.

      <fs_adtel>-home_flag  = abap_true.
      <fs_adtel>-tel_number = iv_mobile_phone.
      <fs_adtel>-r3_user    = '3'.
      <fs_adtel>-updateflag = 'U'.

    ELSEIF lt_adtel IS INITIAL. " If no communication data is available then Insert

      APPEND INITIAL LINE TO lt_adtel ASSIGNING <fs_adtel>.

      <fs_adtel>-home_flag  = abap_true.
      <fs_adtel>-tel_number = iv_mobile_phone.
      <fs_adtel>-r3_user    = '3'.
      <fs_adtel>-updateflag = 'I'.

    ENDIF.

    " Insert or update the telephone number
    READ TABLE lt_adtel ASSIGNING <fs_adtel> WITH KEY r3_user = '1'.
    IF sy-subrc = 0.

      <fs_adtel>-home_flag  = abap_true.
      <fs_adtel>-tel_number = iv_telephone.
      <fs_adtel>-r3_user    = '1'.
      <fs_adtel>-updateflag = 'U'.

    ELSE. " If no communication data is available then Insert

      APPEND INITIAL LINE TO lt_adtel ASSIGNING <fs_adtel>.

      <fs_adtel>-home_flag  = abap_true.
      <fs_adtel>-tel_number = iv_telephone.
      <fs_adtel>-r3_user    = '1'.
      <fs_adtel>-updateflag = 'I'.

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
    ls_return-message = text-012.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      address_number     = lv_addrnum
      table_type         = 'ADTEL'
      iv_time_dependence = abap_true
    TABLES
      comm_table         = lt_adtel
      error_table        = lt_error
    EXCEPTIONS
      parameter_error    = 1
      address_not_exist  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc = 0.

    " Save the changes to the database
    CALL FUNCTION 'ADDR_MEMORY_SAVE'
      EXCEPTIONS
        address_number_missing = 1
        person_number_missing  = 2
        internal_error         = 3
        database_error         = 4
        reference_missing      = 5
        OTHERS                 = 6.

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
    ls_return-message = text-012.
    APPEND ls_return TO it_return.
    EXIT.

  ENDIF.

ENDFORM.                    " CHANGE_CONNECTION_OBJT_DETAILS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_UTILITY_CONTRCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_RETURN  text
*      -->P_IV_REGIO_GROUP  text
*----------------------------------------------------------------------*
FORM change_utility_contrct TABLES it_return
                            USING  iv_regio_group
                                   iv_business_area
                                   iv_contract_accnt_num.

  DATA: lv_contract  TYPE vertrag,
        ls_obj       TYPE isu01_contract,
        ls_auto	     TYPE isu01_contract_auto,
        ls_return    TYPE bapiret2,
        lv_db_update TYPE e_update.

  SELECT SINGLE vertrag
    INTO lv_contract
    FROM ever
    WHERE vkonto = iv_contract_accnt_num.

  CALL FUNCTION 'ISU_S_CONTRACT_PROVIDE'
    EXPORTING
      x_vertrag    = lv_contract
      x_wmode      = '2'
    IMPORTING
      y_obj        = ls_obj
      y_auto       = ls_auto
    EXCEPTIONS
      not_found    = 1
      foreign_lock = 2
      key_invalid  = 3
      system_error = 4
      OTHERS       = 5.
  IF sy-subrc <> 0.

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

    CONCATENATE text-014
                lv_contract
                INTO ls_return-message
                SEPARATED BY space.
    ls_return-type = co_error.
    APPEND ls_return TO it_return.
    EXIT.

  ELSE.

    CASE iv_business_area.
      WHEN '0200'. " South Caloocan
        ls_auto-everd-cokey = '905120000'.
      WHEN '0300'. " Novaliches-Valenzuela
        ls_auto-everd-cokey = '905130000'.
      WHEN '0400'. " Quirino-Roosevelt
        ls_auto-everd-cokey = '904080000'.
      WHEN '0500'. " Sampaloc
        ls_auto-everd-cokey = '904080000'.
      WHEN '0600'. " Tondo
        ls_auto-everd-cokey = '906100000'.
      WHEN '0700'. " South Manila-Pasay/Makati
        ls_auto-everd-cokey = '907110000'.
      WHEN '0800'. " Cavite
        ls_auto-everd-cokey = '907130000'.
      WHEN '0900'. " Fairview-Commonwealth
        ls_auto-everd-cokey = '904100000'.
      WHEN '1000'. " Malabon-Navotas
        ls_auto-everd-cokey = '905140000'.
      WHEN '1100'. " Paranaque
        ls_auto-everd-cokey = '907120000'.
      WHEN '1200'. " North Caloocan
        ls_auto-everd-cokey = '905110000'.
      WHEN '1700'. " Muntinlupa-Las Pinas
        ls_auto-everd-cokey = '907140000'.
      WHEN '1800'. " KAM
        ls_auto-everd-cokey = '900020200'.
      WHEN '5080'. " Cavite
        ls_auto-everd-cokey = '1800000000'.
    ENDCASE.

    ls_auto-everd-gsber = iv_business_area.
    ls_auto-everd-regiogroup = iv_regio_group.

    ls_auto-everd_okcode = co_save.
    ls_auto-everd_use    = abap_true.
    ls_auto-everd_done   = abap_true.

    CALL FUNCTION 'ISU_S_CONTRACT_CHANGE'
      EXPORTING
        x_vertrag      = lv_contract
        x_upd_online   = abap_true
        x_no_dialog    = abap_true
        x_auto         = ls_auto
        x_obj          = ls_obj
      IMPORTING
        y_db_update    = lv_db_update
      EXCEPTIONS
        not_found      = 1
        foreign_lock   = 2
        key_invalid    = 3
        input_error    = 4
        system_error   = 5
        not_authorized = 6
        OTHERS         = 7.
    IF sy-subrc <> 0.
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

      CONCATENATE text-014
                  lv_contract
                  INTO ls_return-message
                  SEPARATED BY space.
      ls_return-type = co_error.
      APPEND ls_return TO it_return.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_UTILITY_CONTRCT