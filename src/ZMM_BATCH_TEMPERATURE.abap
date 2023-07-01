*&---------------------------------------------------------------------*
*& Report ZMM_BATCH_TEMPERATURE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_batch_temperature.
INCLUDE zglobal_variables  .

INITIALIZATION .
  IF sy-langu = 'A' .
    %b001000_block_1000 = '###### ##### #######'.
    %b002005_block_1000 = '##### #######'.
    %p003008_1000 = '##### #######'.
    %_p_plant_%_app_%-text = '######' .
    %_p_date_%_app_%-text = '#######' .
    %_p_doc_%_app_%-text = '#######' .
    %_p_time_%_app_%-text = '#####' .
  ENDIF .

AT SELECTION-SCREEN .

  CASE sy-ucomm.
    WHEN 'BEDIT' .
      IF p_doc IS NOT INITIAL .
        lv_edit = 'X' .
        CALL SCREEN 0100 .
      ELSE  .
        MESSAGE 'Enter Document To Edit!' TYPE 'E' .
      ENDIF .
  ENDCASE .

START-OF-SELECTION.
  IF p_plant IS INITIAL OR p_date IS INITIAL  .
    MESSAGE 'Fill Mandatory Fields ( PLANT , DATE )' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ELSE .
    CALL SCREEN 0100 .
  ENDIF .

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STAT'.
  SET TITLEBAR 'TITLE'.

  DATA lt_f4 TYPE lvc_t_f4 .
  DATA ls_f4 LIKE LINE OF lt_f4 .
  DATA : lr_event_handler TYPE REF TO lcl_event_handler .
  DATA : lr_event_handler_h TYPE REF TO lcl_event_handler .

  CREATE OBJECT lr_event_handler .
*& Create grid container ..
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'BATCHES'.

    CREATE OBJECT grid
      EXPORTING
        i_parent = g_custom_container. " Parent Container .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZTEMPERATURE'
      CHANGING
        ct_fieldcat            = gt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.


    LOOP AT gt_fcat INTO gs_fcat .
      IF  gs_fcat-col_pos > 7 .
        gs_fcat-edit = 'X' .
      ENDIF .

      IF gs_fcat-fieldname = 'MBLNR' .
        gs_fcat-f4availabl = 'X'.
      ENDIF .

      IF gs_fcat-fieldname = 'CLABS' AND sy-langu = 'E'.
        gs_fcat-scrtext_l = 'All Stock Qty'.
        gs_fcat-scrtext_m = 'All Stock Qty'.
        gs_fcat-scrtext_s = 'Stock.Qty'.
        gs_fcat-reptext   = 'All Stock Qty'.
      ENDIF .
      IF  sy-langu = 'A'.
        CASE  gs_fcat-fieldname .
          WHEN  'CLABS' .
            gs_fcat-scrtext_l = '#### ##### ######'.
            gs_fcat-scrtext_m = '#### #######'.
            gs_fcat-scrtext_s = '#######'.
            gs_fcat-reptext   = '#### #######'.
          WHEN 'TEMPC' .
            gs_fcat-scrtext_l = '#### #######'.
            gs_fcat-scrtext_m = '#### #######'.
            gs_fcat-scrtext_s = '#######'.
            gs_fcat-reptext   = '#### #######'.
          WHEN 'REMARKS'.
            gs_fcat-scrtext_l = '#########'.
            gs_fcat-scrtext_m = '#########'.
            gs_fcat-scrtext_s = '#########'.
            gs_fcat-reptext   = '#########'.

        ENDCASE .
      ENDIF .
      MODIFY gt_fcat  FROM gs_fcat .
    ENDLOOP .

    PERFORM get_data .

    CLEAR ls_f4  .
    ls_f4-fieldname  = 'MBLNR'.
    ls_f4-register   = 'X'.
    APPEND ls_f4 TO lt_f4.

    CALL METHOD grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER lr_event_handler->handle_on_f4 FOR grid .

    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*    SET HANDLER lr_event_handler->check_changed_data FOR grid .
    SET HANDLER lr_event_handler->handle_data_changed FOR grid .
    

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        i_structure_name              = 'ZTEMPERATURE'
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_data
        it_fieldcatalog               = gt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF .

ENDMODULE.
CLASS lcl_event_handler IMPLEMENTATION .
*Check Data Changed
  METHOD check_changed_data.
    
  ENDMETHOD .
*  Handle Data Changed
  METHOD handle_data_changed .

    PERFORM handle_data_changed
     USING er_data_changed .

  ENDMETHOD.                    "handle_data_changed
  "HANDLE F4
  METHOD handle_on_f4.

    PERFORM handle_on_f4
     USING e_fieldname
           es_row_no
           er_event_data.

  ENDMETHOD .
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm .
    WHEN 'EXIT'   OR
         'BACK'   OR
         'CANCEL'.

      FREE : gt_fcat[] , gt_data[] .
      CLEAR : gs_layout , lv_edit .
      LEAVE TO SCREEN 0.

    WHEN 'SAVE' .

*& READ VALUES FROM SCREEN
      IF grid IS INITIAL.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = grid.
      ENDIF.

      IF NOT grid IS INITIAL.
        CALL METHOD grid->check_changed_data.
      ENDIF.

      IF lv_edit IS INITIAL .
        PERFORM save_entries .
      ELSE .
        PERFORM edit_document .
      ENDIF .

      CALL METHOD grid->refresh_table_display.

    WHEN 'DELETE'.
      
      PERFORM delete_document .

  ENDCASE .
ENDMODULE.

*& all functions implementation
INCLUDE zsubroutines .
