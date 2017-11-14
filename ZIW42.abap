report ZIW42
       no standard page heading line-size 255.

include bdcrecx1.

start-of-selection.

perform open_group.

perform bdc_dynpro      using 'SAPLCMFU' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'CMFUD-AUFNR'.
perform bdc_field       using 'CMFUD-AUFNR'
                              '4501643'.
perform bdc_field       using 'RM11P-NEW_ROW'
                              '10'.
perform bdc_dynpro      using 'SAPLCMFU' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'CMFUD-AUFNR'.
perform bdc_field       using 'CMFUD-AUFNR'
                              '4501643'.
perform bdc_field       using 'RM11P-NEW_ROW'
                              '10'.
perform bdc_dynpro      using 'SAPLSLVC_FULLSCREEN' '0500'.
perform bdc_field       using 'BDC_OKCODE'
                              '=&F03'.
perform bdc_transaction using 'IW42'.

perform close_group.