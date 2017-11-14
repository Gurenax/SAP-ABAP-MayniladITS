*&---------------------------------------------------------------------*
*& Report  ZMRUCREATE
*&
*&---------------------------------------------------------------------*
*& Developed by : Randy B. Sy
*& Date Created : 12152011
*&
*&
*&---------------------------------------------------------------------*

REPORT ZMRUCREATE NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPE-POOLS :
        SLIS,
        TRUXS.

***Required BDC OBJECTS.
TABLES: T100.
DATA: BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
DATA: SESSION, SMALLLOG.
DATA: MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA: CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N'.
DATA: CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'L'.
DATA: E_GROUP(12).
DATA: E_GROUP_OPENED.
DATA: E_USER LIKE SY-UNAME.
DATA: E_KEEP.
DATA: E_HDATE LIKE SY-DATUM.
***End of Required BDC OBJECTS.


DATA: IT_UPLOAD   LIKE TABLE OF ALSMEX_TABLINE WITH HEADER LINE.

TYPES: BEGIN OF TY_EXCEL,
        SCHLUESS2 LIKE REA41-SCHLUESS2,
        V_ABL     LIKE REA41-V_ABL,
        TERMTEXT  LIKE TE422-TERMTEXT,
        PORTION   LIKE TE422-PORTION,
        STICHTAG  LIKE TE422-STICHTAG,
        IDENT     LIKE TE422-IDENT,
        SAPKAL    LIKE TE422-SAPKAL,
        ABLESER   LIKE TE422-ABLESER,
       END OF TY_EXCEL.

DATA:  IT_EXCEL   TYPE TABLE OF TY_EXCEL,
       WA_EXCEL   TYPE TY_EXCEL.

DATA:  FILE_ERROR TYPE C.

*INCLUDE BDCRECX1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
      P_INFILE         LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_INFILE.
  PERFORM SELECT_FILE USING P_INFILE.

*
*
START-OF-SELECTION.

  PERFORM UPLOAD_EXCEL.
  IF FILE_ERROR NE '1'.
    PERFORM LOAD_BDC.
  ELSE.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        TEXTLINE1 = 'Please check uploaded file.'.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  SELECT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_INFILE  text
*----------------------------------------------------------------------*
FORM SELECT_FILE USING P_INFILE TYPE LOCALFILE.
  DATA :
    LV_SUBRC  LIKE SY-SUBRC,
    LT_IT_TAB TYPE FILETABLE.

  " Display File Open Dialog control/screen
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE     = 'Select Source Excel File'
      DEFAULT_FILENAME = '*.xls'
      MULTISELECTION   = ' '
    CHANGING
      FILE_TABLE       = LT_IT_TAB
      RC               = LV_SUBRC.

  " Write path on input area
  LOOP AT LT_IT_TAB INTO P_INFILE.
  ENDLOOP.

ENDFORM.                    " SELECT_FILE


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_EXCEL .
  FILE_ERROR = ``.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_INFILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 14
      I_END_ROW               = 5000
    TABLES
      INTERN                  = IT_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC NE 0.
*      WRITE: / 'File Error.'.

    FILE_ERROR = '1'.

*      MESSAGE i000.
*      EXIT.
  ENDIF.

  IF IT_UPLOAD IS INITIAL.
*      WRITE: / 'No data uploaded.'.

    FILE_ERROR = '1'.
  ELSE.
    SORT IT_UPLOAD BY ROW COL.

    LOOP AT IT_UPLOAD.
      CASE IT_UPLOAD-COL.
        WHEN 1.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-SCHLUESS2.
        WHEN 2.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-V_ABL.
        WHEN 3.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-TERMTEXT.
        WHEN 4.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-PORTION.
        WHEN 5.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-STICHTAG.
        WHEN 6.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-IDENT.
        WHEN 7.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-SAPKAL.
        WHEN 8.
          MOVE IT_UPLOAD-VALUE TO WA_EXCEL-ABLESER.
      ENDCASE.

      AT END OF ROW.
        APPEND WA_EXCEL TO IT_EXCEL.
        CLEAR  WA_EXCEL.
      ENDAT.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPLOAD_EXCEL

*&---------------------------------------------------------------------*
*&      Form  LOAD_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_BDC .
  LOOP AT IT_EXCEL INTO WA_EXCEL.
*    WRITE:/ WA_EXCEL-SCHLUESS2,
*    WA_EXCEL-V_ABL,
*    WA_EXCEL-TERMTEXT,
*    WA_EXCEL-PORTION,
*    WA_EXCEL-STICHTAG,
*    WA_EXCEL-IDENT,
*    WA_EXCEL-SAPKAL,
*    WA_EXCEL-ABLESER.

*    PERFORM OPEN_GROUP.

    PERFORM BDC_DYNPRO      USING 'SAPLE15A' '0104'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'REA41-V_ABL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'REA41-SCHLUESS2'
*                                '18001280'.
                                  WA_EXCEL-SCHLUESS2.
    PERFORM BDC_FIELD       USING 'REA41-V_ABL'
*                                '01000280'.
                                  WA_EXCEL-V_ABL.
    PERFORM BDC_DYNPRO      USING 'SAPLE15A' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'TE422-TERMTEXT'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SAVE'.
    PERFORM BDC_FIELD       USING 'TE422-TERMTEXT'
*                                'KAM MRU - 13_BG18'.
                                  WA_EXCEL-TERMTEXT.
    PERFORM BDC_FIELD       USING 'TE422-PORTION'
*                                '13_BG18'.
                                  WA_EXCEL-PORTION.
    PERFORM BDC_FIELD       USING 'TE422-STICHTAG'
*                                '15'.
                                  WA_EXCEL-STICHTAG.
    PERFORM BDC_FIELD       USING 'TE422-IDENT'
*                                'PH'.
                                  WA_EXCEL-IDENT.
    PERFORM BDC_FIELD       USING 'TE422-SAPKAL'
*                                '2'.
                                  WA_EXCEL-SAPKAL.
    PERFORM BDC_FIELD       USING 'TE422-ABLESER'
*                                'Y49'.
                                  WA_EXCEL-ABLESER.

    PERFORM BDC_TRANSACTION USING 'E41H'.

*    PERFORM CLOSE_GROUP.



  ENDLOOP.
ENDFORM.                    " LOAD_BDC

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> '/'.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
* batch input session
  IF SESSION = 'X'.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE     = TCODE
      TABLES
        DYNPROTAB = BDCDATA.
    IF SMALLLOG <> 'X'.
      WRITE: / 'BDC_INSERT'(I03),
               TCODE,
               'returncode:'(I05),
               SY-SUBRC,
               'RECORD:',
               SY-INDEX.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH MESSTAB.
    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   CTUMODE
                     UPDATE CUPDATE
                     MESSAGES INTO MESSTAB.
    L_SUBRC = SY-SUBRC.
    IF SMALLLOG <> 'X'.
      WRITE: / 'CALL_TRANSACTION',
               TCODE,
               'returncode:'(I05),
               L_SUBRC,
               'RECORD:',
               SY-INDEX.
      LOOP AT MESSTAB.
        SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                  AND   ARBGB = MESSTAB-MSGID
                                  AND   MSGNR = MESSTAB-MSGNR.
        IF SY-SUBRC = 0.
          L_MSTRING = T100-TEXT.
          IF L_MSTRING CS '&1'.
            REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
          ELSE.
            REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
          ENDIF.
          CONDENSE L_MSTRING.
          WRITE: / MESSTAB-MSGTYP, L_MSTRING(250).
        ELSE.
          WRITE: / MESSTAB.
        ENDIF.
      ENDLOOP.
      SKIP.
    ENDIF.
** Erzeugen fehlermappe ************************************************
    IF L_SUBRC <> 0 AND E_GROUP <> SPACE.
      IF E_GROUP_OPENED = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
          EXPORTING
            CLIENT   = SY-MANDT
            GROUP    = E_GROUP
            USER     = E_USER
            KEEP     = E_KEEP
            HOLDDATE = E_HDATE.
        E_GROUP_OPENED = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          TCODE     = TCODE
        TABLES
          DYNPROTAB = BDCDATA.
    ENDIF.
  ENDIF.
  REFRESH BDCDATA.
ENDFORM.                    "BDC_TRANSACTION