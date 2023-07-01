*&---------------------------------------------------------------------*
*& Include          ZSUBROUTINES
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed  USING
                           p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA :
    ls_mod_cell TYPE lvc_s_modi,
    ls_mod      TYPE ty_mod.
break xabap3 .

*     grid->refresh_table_display( ) .
  READ TABLE p_er_data_changed->mt_mod_cells INTO ls_mod_cell INDEX 1 .
  READ TABLE p_er_data_changed->mt_good_cells INTO DATA(ls_good_cell) INDEX 1 .

  BREAK xabap3 .

  READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX ls_mod_cell-row_id.
  IF ls_good_cell-fieldname = 'MBLNR' .
    "check the validity of the entered document ..
    SELECT mblnr FROM mseg INTO TABLE @DATA(lt_matdoc) WHERE bwart  = '309' AND werks = @<fs_data>-plant .

    IF line_exists( lt_matdoc[ mblnr = ls_good_cell-value ] ) .
    ELSE .

      MESSAGE 'Invalid Material Document!' TYPE 'I'.
    ENDIF .

  ENDIF .
"  grid->refresh_table_display( ) .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .
  IF lv_edit IS INITIAL .

    SELECT werks AS plant mchb~matnr werks lgort charg makt~maktx   mara~meins
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM mchb INNER JOIN makt ON mchb~matnr = makt~matnr
      INNER JOIN mara ON mchb~matnr = mara~matnr
      WHERE werks = p_plant AND mchb~matnr IN ('' , '')
      AND lgort = '' AND makt~spras = 'EN' ."AND CLABS > 0 .

    SELECT charg , werks , matnr , clabs , cinsm , cspem FROM mchb INTO TABLE @DATA(lt_qty)
      WHERE werks = @p_plant AND mchb~matnr IN ('' , '') AND lgort = '' .

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      <fs_data>-date_d = p_date."sy-datum .

      IF p_time IS NOT INITIAL.
        <fs_data>-time = p_time .
      ELSE .
        <fs_data>-time = sy-uzeit .
      ENDIF .

      READ TABLE lt_qty INTO DATA(ls_qty) WITH KEY charg = <fs_data>-charg
                                                   matnr = <fs_data>-matnr
                                                   werks = <fs_data>-plant .
      IF sy-subrc = 0 .

        <fs_data>-clabs = ls_qty-clabs + ls_qty-cinsm + ls_qty-cspem  .
      ENDIF .
    ENDLOOP .

    DELETE  gt_data WHERE clabs = 0 .

  ELSE .
    BREAK xabap3.
    FREE gt_data .
    SELECT SINGLE del_ind FROM zbatch_temp_h INTO @DATA(lv_del_ind) WHERE zbatch_temp_h~tempdoc = @p_doc .
    IF lv_del_ind IS NOT INITIAL .
      MESSAGE 'Deleted Document!' TYPE 'E' DISPLAY LIKE 'W' .
    ELSE .
      SELECT * FROM zbatch_temp_h INNER JOIN zbatch_temp_d ON zbatch_temp_h~tempdoc  = zbatch_temp_d~tempdoc
        INTO CORRESPONDING FIELDS OF TABLE gt_data
        WHERE zbatch_temp_h~tempdoc = p_doc AND del_ind NE 'X'.
    ENDIF .

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_ENTRIES
*&---------------------------------------------------------------------*
FORM save_entries .
  DATA : ls_batch_h          TYPE zbatch_temp_h,
         lt_batch_d          TYPE TABLE OF zbatch_temp_d,
         ls_batch_d          TYPE  zbatch_temp_d,
         lv_number_range(10) TYPE n,
         lv_rc               TYPE inri-returncode ,
         Lv_row              type int4 VALUE 0 .

*& LOCK TABLE
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = 'ZBATCH_TEMP_H'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

*- header
  ls_batch_h-plant  = p_plant .
  ls_batch_h-date_h = sy-datum .
  ls_batch_h-time   = sy-uzeit .
  ls_batch_h-xuser   = sy-uname .
  BREAK xabap3 .

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZTEMP'
    IMPORTING
      number                  = lv_number_range
      returncode              = lv_rc
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF  lv_rc IS INITIAL .
    ls_batch_h-tempdoc   = lv_number_range .
  ENDIF.

*- items
  IF gt_data[] IS NOT INITIAL .
    " temp can't be {0}
    IF line_exists( gt_data[ tempc = 0 remarks = '' ]  ) .
      MESSAGE 'Provide Temperature or Notes for all Batches!' TYPE 'E' .
    ENDIF .

        "check the validity of all documents ..
  PERFORM check_material_documents USING lv_row .

    MOVE-CORRESPONDING gt_data TO lt_batch_d .
    CLEAR ls_batch_d .

    LOOP AT lt_batch_d INTO ls_batch_d.
      ls_batch_d-tempdoc = lv_number_range .
      MODIFY lt_batch_d FROM  ls_batch_d .
    ENDLOOP .

    INSERT zbatch_temp_h FROM  ls_batch_h .
    INSERT  zbatch_temp_d FROM TABLE  lt_batch_d .
    IF sy-subrc = 0 .
      MESSAGE |Document { lv_number_range } Saved Successfuly | TYPE 'S' .
    ENDIF .
  ENDIF .

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable = 'E'
      tabname      = 'ZBATCH_TEMP_H'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_DOCUMENT
*&---------------------------------------------------------------------*
FORM delete_document .
  DATA ans(1) .
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'POPUP TO CONFIRM'
      text_question         = 'Are You sure you want to clear Document'
      text_button_1         = 'Sure'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'CANCEL'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = ans.

  IF ans = 1 .

    IF lv_edit = 'X' .
      UPDATE zbatch_temp_h SET  del_ind = 'X' WHERE tempdoc = p_doc .
*      DELETE FROM zbatch_temp_h WHERE tempdoc = p_doc .
*      DELETE FROM zbatch_temp_d WHERE tempdoc = p_doc .
      IF sy-subrc = 0 .
        MESSAGE 'Document Deleted Successfuly ' TYPE 'S' .
        FREE : gt_fcat[] , gt_data[] .
        CLEAR :  gs_layout , lv_edit .
        CALL METHOD grid->refresh_table_display.
        LEAVE TO SCREEN 0.
      ENDIF .

    ELSE .
      MESSAGE 'Enter Document To Clear!' TYPE 'E'.
    ENDIF .

  ELSE .
    LEAVE TO SCREEN 0100 .

  ENDIF .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EDIT_DOCUMENT
*&---------------------------------------------------------------------*
FORM edit_document .
  DATA  : lt_batch_d TYPE TABLE OF zbatch_temp_d,
          ls_batch_d TYPE         zbatch_temp_d ,
          lv_row     type  int4 VALUE 0.


*& LOCK TABLE
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = 'ZBATCH_TEMP_D'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF gt_data[] IS NOT INITIAL .
      " temp can't be {0}
    IF line_exists( gt_data[ tempc = 0 remarks = '' ]  ) .
      MESSAGE 'Provide Temperature or Notes for all Batches!' TYPE 'E' .
    ENDIF .

    PERFORM check_material_documents USING lv_row .

    MOVE-CORRESPONDING gt_data TO lt_batch_d .
    CLEAR ls_batch_d .

    LOOP AT lt_batch_d INTO ls_batch_d.
      ls_batch_d-tempdoc = p_doc .
      MODIFY zbatch_temp_d FROM ls_batch_d.

    ENDLOOP .
    MESSAGE 'Lines Updated Succesfuly ' TYPE 'S' .
  ENDIF .

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable = 'E'
      tabname      = 'ZBATCH_TEMP_D'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_ON_F4
*&---------------------------------------------------------------------*
FORM handle_on_f4  USING    p_e_fieldname TYPE lvc_fname
                            p_es_row_no TYPE lvc_s_roid
                            p_er_event_data TYPE REF TO cl_alv_event_data .


  DATA  : lt_map TYPE TABLE OF  dselc,
          ls_map TYPE           dselc.

  DATA  : lt_return TYPE TABLE OF ddshretval .
  BREAK xabap3 .

  CASE p_e_fieldname.
    WHEN 'MBLNR' .

      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX p_es_row_no-row_id .

      SELECT mblnr , BUDAT_MKPF FROM mseg INTO TABLE @DATA(lt_matdoc) WHERE bwart  = '309' AND werks = @<fs_line>-plant .

      CLEAR ls_map .
      ls_map-fldname = 'F0001'.
      ls_map-dyfldname = 'MBLNR'.
      APPEND ls_map TO lt_map.

      CLEAR ls_map .
      ls_map-fldname = 'F0002'.
      ls_map-dyfldname = 'BUDAT_MKPF'.
      APPEND ls_map TO lt_map.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'MBLNR'
          value_org       = 'S'
        TABLES
          value_tab       = lt_matdoc
          dynpfld_mapping = lt_map
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      READ TABLE lt_return INTO DATA(ls_return) WITH KEY fieldname = 'F0001' .
*     DATA(LS_RETURN) = LT_RETURN[  fieldname = 'F0001' ] .

      IF ls_return IS NOT INITIAL .
        DATA lv_doc TYPE mblnr .
        lv_doc   = ls_return-fieldval  .

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_doc
          IMPORTING
            output = lv_doc.

        <fs_line>-mblnr =  lv_doc .

      ENDIF.

  ENDCASE .
  grid->refresh_table_display( ).
  p_er_event_data->m_event_handled = 'X'.

ENDFORM.
form check_material_documents using p_row type int4 .
          "check the validity of all documents ..
    SELECT DISTINCT werks as plant , mblnr FROM mseg INTO TABLE @DATA(lt_matdoc)
      for ALL ENTRIES IN @gt_data WHERE bwart  = '309' AND werks = @gt_data-plant .

    loop at gt_data ASSIGNING FIELD-SYMBOL(<fs_data>)  .
      p_row = p_row + 1 .
        read table lt_matdoc ASSIGNING FIELD-SYMBOl(<fs_tmp>)
         with key plant = <fs_data>-plant mblnr = <fs_data>-mblnr .

        if sy-subrc ne 0 and <fs_data>-mblnr is NOT INITIAL  .
           message |Enter Valid Document in line { p_row } | type 'E' .
           exit .
          endif .
      ENDLOOP .

  ENDFORM .
