*&---------------------------------------------------------------------*
*& Report  Z_GLENN_CUSTOMER_DATA_PROVIDE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_glenn_customer_data_provide.

INCLUDE: z_glenn_customer_dataf01,
         z_glenn_customer_dataf02.

TYPES: BEGIN OF ty_partners,
          anlage TYPE anlage,
          vkonto TYPE vkont_kk,
          gpart  TYPE gpart_kk,
         END OF ty_partners,

         BEGIN OF ty_device,
          equnr   TYPE equnr,
          devloc  TYPE devloc,
          logiknr TYPE logiknr,
          vkonto  TYPE vkont_kk,
         END OF ty_device,

         BEGIN OF ty_premise,
           vstelle TYPE vstelle,
           vkonto  TYPE vkont_kk,
         END OF ty_premise.

DATA: lt_partner TYPE STANDARD TABLE OF ty_partners,
      ls_return  TYPE bapiret2,
      lt_device  TYPE STANDARD TABLE OF ty_device,
      lt_premise TYPE STANDARD TABLE OF ty_premise.

DATA: it_contract_accounts TYPE TABLE OF zsisu_customer_data,
      t_return             TYPE TABLE OF bapiret2.


FIELD-SYMBOLS: <fs_contract_accounts> TYPE zsisu_customer_data.


FIELD-SYMBOLS: <fs_partner> TYPE ty_partners,
               <fs_device>  TYPE ty_device,
               <fs_premise> TYPE ty_premise.

PARAMETERS: pa_date TYPE  eadat DEFAULT sy-datum.

START-OF-SELECTION.
  " Get all the business partners that was changed in the specified date
  PERFORM get_changed_business_partners TABLES it_contract_accounts
                                        USING  pa_date.

  " Get all the premise that was changed in the specified date
  PERFORM get_changed_premise TABLES it_contract_accounts
                              USING  pa_date.

  " Get all the utility installation that was changed in the specified date
  PERFORM get_changed_installation TABLES it_contract_accounts
                                   USING  pa_date.

  " Get all the device that was changed in the specified date
  PERFORM get_changed_device TABLES it_contract_accounts
                             USING  pa_date.

  " Get all the CAN that was changed in the specified date
  PERFORM get_changed_can TABLES it_contract_accounts
                          USING  pa_date.

  " Get all the connection object that was changed in the specified date
  PERFORM get_changed_connection_obj TABLES it_contract_accounts
                                     USING  pa_date.

  " Get all the Meter that was removed in the specified date
  PERFORM get_changed_meter_removal TABLES it_contract_accounts
                                    USING  pa_date.

  " Get All Moved-Out in the specified Date
  PERFORM get_changed_move_out TABLES it_contract_accounts
                               USING  pa_date.

  " Sort the table and delete all adjacent duplicates
  SORT it_contract_accounts BY contract_account.
  DELETE ADJACENT DUPLICATES FROM it_contract_accounts COMPARING contract_account.

  IF it_contract_accounts[] IS INITIAL.

    CONCATENATE text-021
                pa_date
                INTO ls_return-message
                SEPARATED BY space.
    ls_return-type = 'E'.
    APPEND ls_return TO t_return.
    EXIT.

  ENDIF.

  SELECT a~anlage
         a~vkonto
         b~gpart
   INTO TABLE lt_partner
   FROM ever   AS a INNER JOIN
        fkkvkp AS b
   ON a~vkonto = b~vkont
   FOR ALL ENTRIES IN it_contract_accounts
   WHERE a~vkonto = it_contract_accounts-contract_account.

  " Get the Logical device number
  SELECT a~equnr
         a~devloc
         b~logiknr
         c~vkonto
    INTO TABLE lt_device
    FROM ( ( egerh AS a
         INNER JOIN eastl AS b ON a~logiknr = b~logiknr )
         INNER JOIN ever  AS c ON c~anlage  = b~anlage  )
    FOR ALL ENTRIES IN it_contract_accounts
    WHERE a~bis    = '99991231' AND
          c~vkonto = it_contract_accounts-contract_account.

  " Get the premise number
  SELECT vstelle
         vkonto
    INTO TABLE lt_premise
    FROM eanl AS a INNER JOIN
         ever AS b
    ON a~anlage = b~anlage
    FOR ALL ENTRIES IN it_contract_accounts
    WHERE b~vkonto = it_contract_accounts-contract_account.

  LOOP AT it_contract_accounts ASSIGNING <fs_contract_accounts>.

    READ TABLE lt_partner ASSIGNING <fs_partner>
      WITH KEY vkonto = <fs_contract_accounts>-contract_account.
    IF sy-subrc = 0.

      " Get the Business partner details
      PERFORM get_business_partner_details TABLES   t_return
                                           USING    <fs_partner>-gpart        " Business partner
                                                    <fs_partner>-anlage       " Installation
                                           CHANGING <fs_contract_accounts>-first_name              " First Name
                                                    <fs_contract_accounts>-last_name               " Last Name
                                                    <fs_contract_accounts>-middle_name             " Middle Name
                                                    <fs_contract_accounts>-social_insurance_number " Social Insurance Number
                                                    <fs_contract_accounts>-account_class           " Account Class
                                                    <fs_contract_accounts>-company_name            " Company Name
                                                    <fs_contract_accounts>-country                 " COUNTRY
                                                    <fs_contract_accounts>-region                  " Province
                                                    <fs_contract_accounts>-city                    " City/Municipality/Town
                                                    <fs_contract_accounts>-district                " District
                                                    <fs_contract_accounts>-street5                 " Barangay/Barrio
                                                    <fs_contract_accounts>-street                  " Street
                                                    <fs_contract_accounts>-street2                 " Area/Zone/Park/Industrial
                                                    <fs_contract_accounts>-street3                 " Sitio/Purok
                                                    <fs_contract_accounts>-street4                 " Subdivision/Village/Townhouse
                                                    <fs_contract_accounts>-house_num_supplement    " House number supplement
                                                    <fs_contract_accounts>-house_number            " House Number
                                                    <fs_contract_accounts>-postal_code             " ZIP/Postal Code
                                                    <fs_contract_accounts>-building                " Building (Number or Code)
                                                    <fs_contract_accounts>-floor_in_building       " Floor in building
                                                    <fs_contract_accounts>-room_apartment_num      " Room / Unit No.
                                                    <fs_contract_accounts>-email                   " Account Name
                                                    <fs_contract_accounts>-mobile_phone            " Mobile Phone
                                                    <fs_contract_accounts>-sex                     " Sex
                                                    <fs_contract_accounts>-telephone               " Telephone number
                                                    <fs_contract_accounts>-marital_status.          " Marital status

      " Get the device details
      READ TABLE lt_device ASSIGNING <fs_device>
        WITH KEY vkonto = <fs_contract_accounts>-contract_account.
      IF sy-subrc = 0.
        PERFORM get_device_details TABLES   t_return
                                   USING    <fs_partner>-anlage                        " Installation
                                            <fs_device>-equnr
                                            <fs_device>-devloc
                                            <fs_device>-logiknr
                                   CHANGING <fs_contract_accounts>-manufacturer        " Manufacturer
                                            <fs_contract_accounts>-construct_year      " Construct year
                                            <fs_contract_accounts>-sort                " Search Term 1
                                            <fs_contract_accounts>-additional_info     " Additional info
                                            <fs_contract_accounts>-location            " Location
                                            <fs_contract_accounts>-rate_type           " Rate Type
                                            <fs_contract_accounts>-size_dimens         " Size/Dimention
                                            <fs_contract_accounts>-serial_number.      " Serial number
      ENDIF.


      " Get the Utility Installation details
      PERFORM get_utility_instll TABLES   t_return
                                 USING    <fs_partner>-anlage       " Installation
                                 CHANGING <fs_contract_accounts>-billing_class       " Billing Class
                                          <fs_contract_accounts>-rate_category       " Rate Category
                                          <fs_contract_accounts>-mru_num             " MRU
                                          <fs_contract_accounts>-installation_type.  " Installation type

      " Get CAN details
      PERFORM get_contract_account_details TABLES   t_return
                                           USING    <fs_contract_accounts>-contract_account      " Contract account number
                                                    <fs_partner>-gpart                           " Business partner
                                           CHANGING <fs_contract_accounts>-legacy_number         " Legacy number
                                                    <fs_contract_accounts>-account_determination " account determination
                                                    <fs_contract_accounts>-account_class         " Account Class
                                                    <fs_contract_accounts>-business_area         " Business Area
                                                    <fs_contract_accounts>-company_code.         " Company code.

      " Get premise details
      READ TABLE lt_premise ASSIGNING <fs_premise>
       WITH KEY vkonto = <fs_contract_accounts>-contract_account.
      IF sy-subrc = 0.
        PERFORM get_premise_details TABLES   t_return
                                    USING    <fs_partner>-anlage                       " Installation
                                             <fs_premise>-vstelle                      " Premise
                                    CHANGING <fs_contract_accounts>-premise_type       " Premise type
                                             <fs_contract_accounts>-number_of_persons  " Number of persons
                                             <fs_contract_accounts>-regio_group.       " RSG
      ENDIF.

    ENDIF.

  ENDLOOP.