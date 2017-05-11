*&---------------------------------------------------------------------*
* Module      : MM                      Package : ZMM01
* Program Name: ZMMQ005                 T-Code  : ZMMQ005
* Description : Material Report
* Author      : Jack_lin
* Create Date : 2016/09/16
* Spec. Logic :
*&=====================================================================*
* Modification Log - History
*&=====================================================================*
* Date      Modifier  Change_Request Description
* 17/03/08  jack_lin  DEVK903081     增加三碼布種 & Sheico色號欄位
* 17/03/15  jack_lin  DEVK903259     寫法修改
* 17/04/10  jack_lin  DEVK903259     增加建刪料號報表
* 17/04/14  jack_lin  DEVK903424     增加table讓使用者決定異動類型
*&---------------------------------------------------------------------*
REPORT zmmq005.
******* Macros *********************************************************
DEFINE ins_cls.
  insert &1 into table &1.
  clear &1.
END-OF-DEFINITION.
DEFINE ins_cls2.
  insert &1 into table &2.
  clear &1.
END-OF-DEFINITION.
DEFINE cls.
  clear &1. refresh &1.
END-OF-DEFINITION.
DEFINE col_cls.
  collect &1. clear &1.
END-OF-DEFINITION.
DEFINE apd_cls.
  append &1 TO &2 . clear &1.
END-OF-DEFINITION.
DEFINE zero.
***** 1: 補零  2. 去零
  case &2.
    when '1'.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = &1
        importing
          output = &1.
    when '2'.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = &1
        importing
          output = &1.
  endcase.
END-OF-DEFINITION.
DATA: BEGIN OF opitdata OCCURS 0,    "table宣告
        matnr    LIKE mara-matnr,
        mtart    LIKE mara-mtart,
        matkl    LIKE mara-matkl,
        maktx    LIKE makt-maktx,
        zfmaktx  LIKE makt-maktx,
        tdline(101)   TYPE c ,
        meins    LIKE mara-meins,
        j_3apgnr LIKE mara-j_3apgnr,
        wrkst    LIKE mara-wrkst,
        prdha    LIKE mara-prdha,
        ersda    LIKE mara-ersda,
        atwrt    LIKE ausp-atwrt,
        atwrt1    LIKE ausp-atwrt,
        lvorm    LIKE mara-lvorm,
        mstae    LIKE mara-mstae,
        doknr    LIKE drad-doknr,
      END OF opitdata,
      BEGIN OF maradata OCCURS 0,
        matnr    LIKE mara-matnr,
        mtart    LIKE mara-mtart,
        matkl    LIKE mara-matkl,
        maktx    LIKE makt-maktx,
        meins    LIKE mara-meins,
        j_3apgnr LIKE mara-j_3apgnr,
        wrkst    LIKE mara-wrkst,
        prdha    LIKE mara-prdha,
        ersda    LIKE mara-ersda,
        lvorm    LIKE mara-lvorm,
        mstae    LIKE mara-mstae,
      END OF maradata,
      BEGIN OF mkpfdata OCCURS 0,
        mblnr LIKE mkpf-mblnr,
        budat LIKE mkpf-budat,
        bwart LIKE mseg-bwart,
        matnr LIKE mseg-matnr,
        werks LIKE mseg-werks,
      END OF mkpfdata,
      BEGIN OF mskadata OCCURS 0,
        matnr LIKE mska-matnr,
        werks LIKE mska-werks,
        kalab LIKE mska-kalab,
        kains LIKE mska-kains,
      END OF mskadata,
      BEGIN OF marddata OCCURS 0,
        matnr LIKE mard-matnr,
        werks LIKE mard-werks,
        lgort LIKE mard-lgort,
        lfgja LIKE mard-lfgja,
        lfmon LIKE mard-lfmon,
        labst LIKE mard-labst,
        umlme LIKE mard-umlme,
        insme LIKE mard-insme,
        einme LIKE mard-einme,
        speme LIKE mard-speme,
        retme LIKE mard-retme,
      END OF marddata,
      BEGIN OF mkoldata OCCURS 0,
        matnr LIKE mkol-matnr,
        werks LIKE mkol-werks,
        lgort LIKE mkol-lgort,
        charg LIKE mkol-charg,
        sobkz LIKE mkol-sobkz,
        lifnr LIKE mkol-lifnr,
        lfgja LIKE mkol-lfgja,
        lfmon LIKE mkol-lfmon,
        slabs LIKE mkol-slabs,
        sinsm LIKE mkol-sinsm,
        seinm LIKE mkol-seinm,
        sspem LIKE mkol-sspem,
      END OF mkoldata,
      BEGIN OF mslbdata OCCURS 0,
        matnr LIKE mslb-matnr,
        werks LIKE mslb-werks,
        charg LIKE mslb-charg,
        sobkz LIKE mslb-sobkz,
        lifnr LIKE mslb-lifnr,
        lfgja LIKE mslb-lfgja,
        lfmon LIKE mslb-lfmon,
        lblab LIKE mslb-lblab,
        lbins LIKE mslb-lbins,
        lbein LIKE mslb-lbein,
      END OF mslbdata,
      BEGIN OF mskudata OCCURS 0,
        matnr LIKE msku-matnr,
        werks LIKE msku-werks,
        charg LIKE msku-charg,
        sobkz LIKE msku-sobkz,
        kunnr LIKE msku-kunnr,
        lfgja LIKE msku-lfgja,
        lfmon LIKE msku-lfmon,
        kulab LIKE msku-kulab,
        kuins LIKE msku-kuins,
        kuein LIKE msku-kuein,
      END OF mskudata,
      BEGIN OF mchbdata OCCURS 0,
        matnr LIKE mchb-matnr,
        werks LIKE mchb-werks,
        clabs LIKE mchb-clabs,
        cinsm LIKE mchb-cinsm,
      END OF mchbdata,
      BEGIN OF ekpodata OCCURS 0,
        matnr LIKE ekpo-matnr,
        aedat LIKE ekpo-aedat,
      END OF ekpodata,
      BEGIN OF deletedata OCCURS 0,
        matnr LIKE mara-matnr,
      END OF deletedata,
      BEGIN OF zerostkdata OCCURS 0,
        matnr LIKE mara-matnr,
      END OF zerostkdata.
      DATA: tdname TYPE stxh-tdname,                   "為了長文做的宣告
            xline  TYPE TABLE OF tline WITH HEADER LINE,
            wline  TYPE tline.
      DATA: lfgja  LIKE marv-lfgja,
            lfmon  LIKE marv-lfmon,
            lfmonl LIKE marv-lfmon,
            lfday  LIKE marv-lfmon,
            vmgja  LIKE marv-vmgja,
            vmmon  LIKE marv-vmmon,
            calc_date TYPE p0001-begda.
** BDC 程式所需的 Data
DATA: bdc_it_data TYPE TABLE OF bdcdata,
      bdc_wa_data TYPE bdcdata.
DATA: bdc_it_msg TYPE TABLE OF bdcmsgcoll,
      bdc_wa_msg TYPE bdcmsgcoll.
DATA: l_opt TYPE ctu_params.
*選擇條件
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE bk1_name.
  SELECT-OPTIONS: matnr FOR opitdata-matnr,
                  wrkst FOR opitdata-wrkst,
                  lvorm FOR opitdata-lvorm,
                  mstae FOR opitdata-mstae,
                  doknr FOR opitdata-doknr.
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE bk2_name.
  PARAMETERS: d01 AS CHECKBOX USER-COMMAND cmd,
              d01_1 RADIOBUTTON GROUP rb1 USER-COMMAND cmd1,
              d01_2 RADIOBUTTON GROUP rb1,
              zero AS CHECKBOX.
  SELECT-OPTIONS: budat FOR maradata-ersda.
  PARAMETER : mon TYPE marv-lfmon DEFAULT 0.
SELECTION-SCREEN END OF BLOCK bk2.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF d01 EQ 'X'.
       screen-active = 1.
       IF screen-name = 'MON'.
          screen-input = '0'.
       ENDIF.
    ELSEIF screen-name CS 'd01_1' OR screen-name CS 'd01_2'
        OR screen-name CS 'zero'.
       screen-active = 0.
    ELSE.
       IF screen-name = 'BUDAT-LOW' OR
          screen-name = 'BUDAT-HIGH' OR
          screen-name = 'MON'.
          screen-input = '0'.
       ENDIF.
    ENDIF.
    IF d01_1 EQ 'X'.
       IF screen-name = 'MON'.
          screen-input = '0'.
       ENDIF.
    ELSEIF d01_2 EQ 'X'.
       IF screen-name = 'BUDAT-LOW' OR
          screen-name = 'BUDAT-HIGH'.
          screen-input = '0'.
       ENDIF.
       IF screen-name = 'MON'.
          screen-input = '1'.
       ENDIF.
       IF screen-name = 'ZERO'.
          screen-input = '0'.
          zero = ' '.
       ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
******* Include *******************************************************
******* Initialization ************************************************
INITIALIZATION.
  bk1_name = '選擇條件'(000).
  bk2_name = '建刪查詢'(000).
     lfgja = sy-datum+0(4).
     lfmon = sy-datum+4(2).
     lfday = sy-datum+6(2).
     lfmonl = lfmon - 2.
     budat-low =  lfgja && lfmonl && lfday.
     budat-high = sy-datum.
     APPEND budat.
*** Start of Selection **************************
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM delete_list.
************************* End of Selection ****************************
END-OF-SELECTION.
  PERFORM output_data.
*&---------------------------------------------------------------------
*&      Form  GET_DATA
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM get_data.
  DATA: dataindex LIKE sy-tabix.
  REFRESH opitdata. CLEAR opitdata.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE maradata
                               FROM mara AS a
                               JOIN makt AS c
                               ON a~matnr EQ c~matnr
                              WHERE a~matnr IN matnr
                                AND a~lvorm IN lvorm
                                AND a~wrkst IN wrkst
                                AND a~mstae IN mstae
                                AND a~mtart NE 'ZF'
                                AND a~mtart NE 'ZS'
                                AND a~mtart NE 'ZX'
                                AND c~spras EQ 'E'.
*
  LOOP AT maradata.
  dataindex = sy-tabix.
    PERFORM get_mat_data.
    PERFORM get_text.
  ENDLOOP.
*
  DELETE opitdata WHERE doknr NOT IN doknr.
  SORT opitdata BY matnr.
*
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_mat_data.
*
   tdname = maradata-matnr.
*抓中文敘述
  SELECT maktx INTO opitdata-zfmaktx FROM makt
                         WHERE matnr EQ maradata-matnr
                         AND spras EQ 'M'.
    EXIT.
  ENDSELECT.
*抓圖檔
   SELECT doknr INTO opitdata-doknr FROM drad
                         WHERE drad~objky EQ maradata-matnr.
    EXIT.
  ENDSELECT.
*2017/03/08 三碼布種
  SELECT atwrt  INTO opitdata-atwrt FROM ausp
                         WHERE objek EQ maradata-matnr
                         AND atinn EQ '2984'.
    EXIT.
  ENDSELECT.
*2017/03/08 Sheico色號
  SELECT atwrt  INTO opitdata-atwrt1 FROM ausp
                         WHERE objek EQ maradata-matnr
                         AND atinn EQ '2485'.
    EXIT.
  ENDSELECT.
ENDFORM.                    " GENERATE_PO_DATA
*&--------------------------------------------------------------------*
*&      Form  Read_text
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM get_text.
CALL FUNCTION 'READ_TEXT'
  EXPORTING
*   CLIENT                        = SY-MANDT
    id                            = 'GRUN'
    language                      = 'M'
    name                          = tdname
    object                        = 'MATERIAL'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
*   OLD_LINE_COUNTER              =
  TABLES
    lines                         = xline[]
 EXCEPTIONS
   id                            = 1
   language                      = 2
   name                          = 3
   not_found                     = 4
   object                        = 5
   reference_check               = 6
   wrong_access_to_archive       = 7
   OTHERS                        = 8.
IF sy-subrc EQ 0.
    LOOP AT xline WHERE tdline NE ' '.
    opitdata-tdline = opitdata-tdline && xline-tdline.
    ENDLOOP.
ENDIF.
 MOVE-CORRESPONDING maradata  TO opitdata.
 APPEND opitdata. CLEAR opitdata.
ENDFORM.
*&--------------------------------------------------------------------*
*&      Form  delete_list
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM delete_list.
  IF d01 = 'X'.
    "無領用(以當下往前算)
   IF d01_1 = 'X'.
     "撈出有庫存的資料剔除
     SELECT * INTO CORRESPONDING FIELDS OF TABLE mskadata
                                           FROM  mska
                                           WHERE mska~kalab GT 0
                                              OR mska~kains GT 0.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE mchbdata
                                           FROM  mchb
                                           WHERE mchb~clabs GT 0
                                              OR mchb~cumlm GT 0
                                              OR mchb~cinsm GT 0
                                              OR mchb~ceinm GT 0
                                              OR mchb~cspem GT 0
                                              OR mchb~cretm GT 0.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE marddata
                                           FROM  mard
                                           WHERE mard~labst GT 0
                                              OR mard~umlme GT 0
                                              OR mard~insme GT 0
                                              OR mard~einme GT 0
                                              OR mard~speme GT 0
                                              OR mard~retme GT 0.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE mslbdata
                                           FROM  mslb
                                           WHERE mslb~lblab GT 0
                                              OR mslb~lbins GT 0.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE mskudata
                                           FROM  msku
                                           WHERE msku~kulab GT 0
                                              OR msku~kuins GT 0.
     LOOP AT mskadata.
      deletedata-matnr = mskadata-matnr.
      APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
     LOOP AT mchbdata.
      deletedata-matnr = mchbdata-matnr.
      APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
     LOOP AT marddata.
      deletedata-matnr = marddata-matnr.
      APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
     LOOP AT mslbdata.
      deletedata-matnr = mslbdata-matnr.
      APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
     LOOP AT mskudata.
      deletedata-matnr = mskudata-matnr.
      APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
    IF zero = 'X'.
    "撈出有領用記錄的料號
     SELECT * INTO CORRESPONDING FIELDS OF TABLE mkpfdata
                                        FROM mkpf
                                       INNER JOIN mseg
                                          ON mkpf~mblnr EQ mseg~mblnr
                                         AND mkpf~mjahr EQ mseg~mjahr
                                       INNER JOIN zmmt010
                                          ON mseg~bwart EQ zmmt010~bwart
                                       WHERE mkpf~budat IN budat
                                         AND zmmt010~on_off EQ 'X'.
     LOOP AT mkpfdata.
       deletedata-matnr = mkpfdata-matnr.
       APPEND deletedata. CLEAR deletedata.
     ENDLOOP.
     DELETE opitdata WHERE mstae EQ '97'
                        OR mstae EQ '98'.
     "剔除撈出來重覆的料號
     DELETE ADJACENT DUPLICATES FROM deletedata.
     "剔除上面撈出非建議刪的料號
     LOOP AT deletedata.
      DELETE opitdata WHERE matnr EQ deletedata-matnr.
     ENDLOOP.
*
    ELSE.
*
    SELECT * INTO CORRESPONDING FIELDS OF TABLE zerostkdata
                                       FROM mara
                                       WHERE mara~mtart NE 'ZF'
                                         AND mara~mtart NE 'ZS'
                                         AND mara~mtart NE 'ZX'.
*
    LOOP AT deletedata.
      DELETE zerostkdata WHERE matnr EQ deletedata-matnr.
    ENDLOOP.
    "撈出有領用記錄的料號
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mkpfdata
                                        FROM mkpf
                                       INNER JOIN mseg
                                          ON mkpf~mblnr EQ mseg~mblnr
                                         AND mkpf~mjahr EQ mseg~mjahr
                                       INNER JOIN zmmt010
                                          ON mseg~bwart EQ zmmt010~bwart
                                       WHERE mkpf~budat IN budat
                                         AND zmmt010~on_off EQ 'X'.
    LOOP AT mkpfdata.
      zerostkdata-matnr = mkpfdata-matnr.
      APPEND zerostkdata. CLEAR zerostkdata.
    ENDLOOP.
      DELETE opitdata WHERE mstae EQ '98'.
     "剔除撈出來重覆的料號
     DELETE ADJACENT DUPLICATES FROM zerostkdata.
     "剔除上面撈出非建議刪的料號
     LOOP AT zerostkdata.
      DELETE opitdata WHERE matnr EQ zerostkdata-matnr.
     ENDLOOP.
    ENDIF.
   ENDIF.
   "無採購且無庫存(以物料建立日期至今)
   IF d01_2 EQ 'X'.
   "撈出有庫存的資料(不包含PX廠)
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mskadata
                                          FROM  mska
                                          WHERE mska~kalab GT 0
                                             OR mska~kains GT 0
                                            AND mska~werks NE 'PX'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mchbdata
                                          FROM  mchb
                                          WHERE mchb~clabs GT 0
                                             OR mchb~cumlm GT 0
                                             OR mchb~cinsm GT 0
                                             OR mchb~ceinm GT 0
                                             OR mchb~cspem GT 0
                                             OR mchb~cretm GT 0
                                            AND mchb~werks NE 'PX'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE marddata
                                          FROM  mard
                                          WHERE mard~labst GT 0
                                             OR mard~umlme GT 0
                                             OR mard~insme GT 0
                                             OR mard~einme GT 0
                                             OR mard~speme GT 0
                                             OR mard~retme GT 0
                                            AND mard~werks NE 'PX'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mslbdata
                                          FROM  mslb
                                          WHERE mslb~lblab GT 0
                                             OR mslb~lbins GT 0
                                            AND mslb~werks NE 'PX'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mskudata
                                          FROM  msku
                                          WHERE msku~kulab GT 0
                                             OR msku~kuins GT 0
                                            AND msku~werks NE 'PX'.
    LOOP AT mskadata.
     deletedata-matnr = mskadata-matnr.
     APPEND deletedata. CLEAR deletedata.
    ENDLOOP.
    LOOP AT mchbdata.
     deletedata-matnr = mchbdata-matnr.
     APPEND deletedata. CLEAR deletedata.
    ENDLOOP.
    LOOP AT marddata.
     deletedata-matnr = marddata-matnr.
     APPEND deletedata. CLEAR deletedata.
    ENDLOOP.
    LOOP AT mslbdata.
     deletedata-matnr = mslbdata-matnr.
     APPEND deletedata. CLEAR deletedata.
    ENDLOOP.
    LOOP AT mskudata.
     deletedata-matnr = mskudata-matnr.
     APPEND deletedata. CLEAR deletedata.
    ENDLOOP.
   CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date            = sy-datum
      days            = 0
      months          = mon
      signum          = '-'
      years           = 0
    IMPORTING
      calc_date       = calc_date.
       LOOP AT opitdata.
         "撈出有開過採購單的料號
         SELECT * INTO CORRESPONDING FIELDS OF TABLE ekpodata
                                               FROM  ekko
                                               JOIN  ekpo
                                      ON ekko~ebeln EQ ekpo~ebeln
                                   WHERE ekpo~matnr EQ opitdata-matnr
                                     AND ekko~aedat GE opitdata-ersda
                                     AND ekko~aedat LE calc_date.
         LOOP AT ekpodata.
           deletedata-matnr = ekpodata-matnr.
           APPEND deletedata. CLEAR deletedata.
         ENDLOOP.
         "撈出料號建立日至今有領用記錄的料號(101 & 501 & 951)
         SELECT * INTO CORRESPONDING FIELDS OF TABLE mkpfdata
                                               FROM mkpf
                                         INNER JOIN mseg
                                     ON mkpf~mblnr EQ mseg~mblnr
                                    AND mkpf~mjahr EQ mseg~mjahr
                                  WHERE mseg~matnr EQ opitdata-matnr
                                    AND mkpf~budat GE opitdata-ersda
                                    AND mkpf~budat LE calc_date
                                    AND mseg~bwart IN ('101','501',
                                                       '951').
         LOOP AT mkpfdata.
           deletedata-matnr = mkpfdata-matnr.
           APPEND deletedata. CLEAR deletedata.
         ENDLOOP.
       ENDLOOP.
   DELETE opitdata WHERE mstae EQ '97'
                      OR mstae EQ '98'.
   "剔除撈出來重覆的料號
   DELETE ADJACENT DUPLICATES FROM deletedata.
   "剔除上面撈出非建議刪的料號
   LOOP AT deletedata.
    DELETE opitdata WHERE matnr EQ deletedata-matnr.
   ENDLOOP.
   ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_data.
  DATA: lvcfcatg TYPE lvc_t_fcat,
        lvcslyot TYPE lvc_s_layo,
        lvctsort TYPE lvc_t_sort.

  PERFORM alv_set_fieldcatalog CHANGING lvcfcatg.
  PERFORM alv_set_layout CHANGING lvcslyot.
*
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                =
*     I_BUFFER_ACTIVE                   =
      i_callback_program                = sy-cprog
      i_callback_pf_status_set          = 'GUI_STATUS'
      i_callback_user_command           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      is_layout_lvc                     = lvcslyot   "寬度調整
      it_fieldcat_lvc                   = lvcfcatg
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS_LVC             =
*      it_sort_lvc                       = lvctsort
*     IT_FILTER_LVC                     =
*     IT_HYPERLINK                      =
*     IS_SEL_HIDE                       =
      i_default                         = 'X'
      i_save                            = 'A'
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT_LVC                      =
*     IS_REPREP_ID_LVC                  =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*     IT_ALV_GRAPHICS                   =
*     IT_EXCEPT_QINFO_LVC               =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = opitdata
    EXCEPTIONS
      program_error                     = 1
      OTHERS                            = 2
            .
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " OUTPUT_DATA

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_set_fieldcatalog CHANGING lvcfcatg TYPE lvc_t_fcat.
  DATA: fdcatgdt TYPE slis_t_fieldcat_alv.
  FIELD-SYMBOLS: <fdcatgln> LIKE lvc_s_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name               = sy-cprog
      i_internal_tabname           = 'OPITDATA'
*     I_STRUCTURE_NAME             =
*     I_CLIENT_NEVER_DISPLAY       = 'X'
      i_inclname                   = sy-cprog
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE              =
    CHANGING
      ct_fieldcat                  = fdcatgdt
    EXCEPTIONS
      inconsistent_interface       = 1
      program_error                = 2
      OTHERS                       = 3
            .
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv       = fdcatgdt
*     IT_SORT_ALV           =
*     IT_FILTER_ALV         =
*     IS_LAYOUT_ALV         =
    IMPORTING
      et_fieldcat_lvc       = lvcfcatg
*     ET_SORT_LVC           =
*     ET_FILTER_LVC         =
*     ES_LAYOUT_LVC         =
    TABLES
      it_data               = opitdata
    EXCEPTIONS
      it_data_missing       = 1
      OTHERS                = 2
            .
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*
  LOOP AT lvcfcatg ASSIGNING <fdcatgln>.
    CASE <fdcatgln>-fieldname.
      WHEN 'MATNR'.
        <fdcatgln>-key = 'X'.
        <fdcatgln>-colddictxt = 'M'.
      WHEN 'MTART'.
        <fdcatgln>-reptext   = 'Material Type'.
        <fdcatgln>-scrtext_l = 'Material Type'.
        <fdcatgln>-scrtext_m = 'Material Type'.
        <fdcatgln>-scrtext_s = 'Material Type'.
      WHEN 'MATKL'.
        <fdcatgln>-reptext   = 'Material Group'.
        <fdcatgln>-scrtext_l = 'Material Group'.
        <fdcatgln>-scrtext_m = 'Material Group'.
        <fdcatgln>-scrtext_s = 'Material Group'.
      WHEN 'MAKTX'.
        <fdcatgln>-reptext   = 'Material Description(EN)'.
        <fdcatgln>-scrtext_l = 'Material Description(EN)'.
        <fdcatgln>-scrtext_m = 'Material Description(EN)'.
        <fdcatgln>-scrtext_s = 'Material Description(EN)'.
      WHEN 'ZFMAKTX'.
        <fdcatgln>-reptext   = 'Material Description(ZF)'.
        <fdcatgln>-scrtext_l = 'Material Description(ZF)'.
        <fdcatgln>-scrtext_m = 'Material Description(ZF)'.
        <fdcatgln>-scrtext_s = 'Material Description(ZF)'.
      WHEN 'TDLINE'.
        <fdcatgln>-reptext   = 'Basic Data Text'.
        <fdcatgln>-scrtext_l = 'Basic Data Text'.
        <fdcatgln>-scrtext_m = 'Basic Data Text'.
        <fdcatgln>-scrtext_s = 'Basic Data Text'.
      WHEN 'MEINS'.
        <fdcatgln>-reptext   = 'Base Unit'.
        <fdcatgln>-scrtext_l = 'Base Unit'.
        <fdcatgln>-scrtext_m = 'Base Unit'.
        <fdcatgln>-scrtext_s = 'Base Unit'.
      WHEN 'J_3APGNR'.
        <fdcatgln>-reptext   = 'Master Grid'.
        <fdcatgln>-scrtext_l = 'Master Grid'.
        <fdcatgln>-scrtext_m = 'Master Grid'.
        <fdcatgln>-scrtext_s = 'Master Grid'.
      WHEN 'WRKST'.
        <fdcatgln>-reptext   = 'AS400 Material'.
        <fdcatgln>-scrtext_l = 'AS400 Material'.
        <fdcatgln>-scrtext_m = 'AS400 Material'.
        <fdcatgln>-scrtext_s = 'AS400 Material'.
      WHEN 'PRDHA'.
        <fdcatgln>-reptext   = 'Product hierarchy'.
        <fdcatgln>-scrtext_l = 'Product hierarchy'.
        <fdcatgln>-scrtext_m = 'Product hierarchy'.
        <fdcatgln>-scrtext_s = 'Product hierarchy'.
      WHEN 'ERSDA'.
        <fdcatgln>-reptext   = 'Created date'.
        <fdcatgln>-scrtext_l = 'Created date'.
        <fdcatgln>-scrtext_m = 'Created date'.
        <fdcatgln>-scrtext_s = 'Created date'.
      WHEN 'LVORM'.
        <fdcatgln>-reptext   = 'Deletion Flag'.
        <fdcatgln>-scrtext_l = 'Deletion Flag'.
        <fdcatgln>-scrtext_m = 'Deletion Flag'.
        <fdcatgln>-scrtext_s = 'Deletion Flag'.
       WHEN 'MSTAE'.
        <fdcatgln>-reptext   = 'Material Status'.
        <fdcatgln>-scrtext_l = 'Material Status'.
        <fdcatgln>-scrtext_m = 'Material Status'.
        <fdcatgln>-scrtext_s = 'Material Status'.
       WHEN 'DOKNR'.
        <fdcatgln>-reptext   = 'Picture Document'.
        <fdcatgln>-scrtext_l = 'Picture Document'.
        <fdcatgln>-scrtext_m = 'Picture Document'.
        <fdcatgln>-scrtext_s = 'Picture Document'.
       WHEN 'ATWRT'.
        <fdcatgln>-reptext   = 'Fabric Code'.
        <fdcatgln>-scrtext_l = 'Fabric Code'.
        <fdcatgln>-scrtext_m = 'Fabric Code'.
        <fdcatgln>-scrtext_s = 'Fabric Code'.
       WHEN 'ATWRT1'.
        <fdcatgln>-reptext   = 'Sheico Color Code'.
        <fdcatgln>-scrtext_l = 'Sheico Color Code'.
        <fdcatgln>-scrtext_m = 'Sheico Color Code'.
        <fdcatgln>-scrtext_s = 'Sheico Color Code'.
      WHEN OTHERS.
        <fdcatgln>-key = space.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " ALV_SET_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_set_layout CHANGING lvcslyot TYPE lvc_s_layo.
  lvcslyot-zebra = 'X'.
  lvcslyot-cwidth_opt = 'X'.
  lvcslyot-no_merging = 'X'.
ENDFORM.                    " ALV_SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  GUI_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gui_status USING excldtbl TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_LIST' EXCLUDING excldtbl.
ENDFORM.                    " GUI_STATUS
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command USING fucncode LIKE sy-ucomm
                        selfield TYPE slis_selfield.
  CASE fucncode.
    WHEN '&IC1'.
      CASE selfield-fieldname.
        WHEN 'MATNR'.
          READ TABLE opitdata INDEX selfield-tabindex.
           IF sy-subrc EQ 0.
            SET PARAMETER ID 'MAT' FIELD opitdata-matnr.
            SET PARAMETER ID 'MXX' FIELD 'K'.
            CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
           ENDIF.
          WHEN 'DOKNR'.
          READ TABLE opitdata INDEX selfield-tabindex.
           IF sy-subrc EQ 0.
             IF opitdata-doknr NE ' '.
                PERFORM set_bdc.
             ENDIF.
           ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  bdc_init
*&---------------------------------------------------------------------*
*       初始化 BDC 作業變數
*----------------------------------------------------------------------*
FORM bdc_init.
  CLEAR: bdc_wa_data, bdc_wa_msg.
  cls:   bdc_it_data, bdc_it_msg.
ENDFORM.                    "bdc_init
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       Start new screen
*----------------------------------------------------------------------*
*      -->FIN_PROGRAM  程式名稱
*      -->FIN_DYNPRO   程式畫面編號
*----------------------------------------------------------------------*
FORM bdc_dynpro USING fin_program TYPE csequence
                      fin_dynpro TYPE bdc_dynr.
  CLEAR bdc_wa_data.
  bdc_wa_data-program  = fin_program.
  bdc_wa_data-dynpro   = fin_dynpro.
  bdc_wa_data-dynbegin = 'X'.
  APPEND bdc_wa_data TO bdc_it_data.
ENDFORM.                    "bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       Insert field
*----------------------------------------------------------------------*
*      -->FIN_EMPTY  若是傳入空白，則 fin_val 必須要有值，才會進到 BDC 參數
*      -->FIN_NAM    畫面欄位名稱
*      -->FIN_VAL    畫面欄位值
*----------------------------------------------------------------------*
FORM bdc_field USING fin_empty TYPE char1
                     fin_nam TYPE csequence
                     fin_val TYPE simple.
  DATA: l_fval TYPE bdc_fval.

  IF fin_empty IS INITIAL AND fin_val IS INITIAL.
    RETURN.
  ENDIF.

  WRITE fin_val TO l_fval NO-GROUPING.
  CLEAR bdc_wa_data.
  bdc_wa_data-fnam = fin_nam.
  CONDENSE l_fval.
  bdc_wa_data-fval = l_fval.
  APPEND bdc_wa_data TO bdc_it_data.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  SET_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_bdc.
 DATA:l_msg_text TYPE bapi_msg.
      PERFORM bdc_init.
      PERFORM call_bdc USING opitdata-doknr.
      CALL TRANSACTION 'CV03N' USING bdc_it_data  "使用BDC
                               OPTIONS FROM l_opt
                               MESSAGES INTO bdc_it_msg.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc USING l_doknr TYPE doknr.

    PERFORM bdc_dynpro USING 'SAPLCV110'  '0100'.
    PERFORM bdc_field USING space : 'BDC_OKCODE' '/00',
                                    'DRAW-DOKNR' l_doknr,
                                    'DRAW-DOKAR' 'Z01',
                                    'DRAW-DOKTL' '000',
                                    'DRAW-DOKVR' '00'.
    PERFORM bdc_dynpro USING 'SAPLCV110'  '0101'.
    PERFORM bdc_field USING space : 'BDC_OKCODE' '=F_SHOW'.
    PERFORM bdc_dynpro USING 'SAPLCV110'  '0101'.
    PERFORM bdc_field USING space : 'BDC_OKCODE' '/EEXIT'.
*
    CLEAR l_opt.
    l_opt-dismode = 'N'.  "Processing Mode
    l_opt-updmode = 'A'.  "Update mode
    l_opt-cattmode =''.   "CATT mode

    l_opt-defsize = 'X'.  "Default size
    l_opt-nobinpt = ''.
    l_opt-nobiend = ''.
ENDFORM.
