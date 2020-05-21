REPORT zzap_get_unit_test_data_p.

"**********************************************************
"Date: 18-05-2020
"Author: Alwin van de Put
"**********************************************************

CLASS zcx_return_exc DEFINITION
*  public
  INHERITING FROM cx_static_check
  CREATE PROTECTED.

  PUBLIC SECTION.

    TYPES:
      gtt_bdc_messages  TYPE STANDARD TABLE OF bdcmsgcoll WITH DEFAULT KEY.

    TYPES:
      gtt_bapireturn TYPE STANDARD TABLE OF bapireturn WITH DEFAULT KEY.

    TYPES:
      BEGIN OF gts_code_position,
        program_name TYPE syrepid,
        include_name TYPE syrepid,
        source_line  TYPE i,
      END OF gts_code_position.

    METHODS constructor
      IMPORTING
        !textid           LIKE textid OPTIONAL
        !previous         LIKE previous OPTIONAL
        !gt_bapiret2      TYPE bapiret2_t OPTIONAL
        !gs_bapiret2      TYPE bapiret2 OPTIONAL
        !gs_code_position TYPE gts_code_position OPTIONAL.

    CLASS-METHODS create_empty
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_system_message
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapireturn_struc
      IMPORTING
        !is_return       TYPE bapireturn
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapireturn_table
      IMPORTING
        !it_return          TYPE gtt_bapireturn
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapiret1_struc
      IMPORTING
        !is_return       TYPE bapiret1
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapiret1_table
      IMPORTING
        !it_return          TYPE bapiret1_tab
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapiret2_struc
      IMPORTING
        !is_return       TYPE bapiret2
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_bapiret2_table
      IMPORTING
        !it_return          TYPE bapiret2_t
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return_exc.


    CLASS-METHODS create_by_bdc_message_table
      IMPORTING
        !it_bdc_messages    TYPE gtt_bdc_messages
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return_exc.

    CLASS-METHODS create_by_text
      IMPORTING
        !iv_type         TYPE bapi_mtype DEFAULT 'E'
        !iv_message      TYPE bapi_msg
        !iv_field_name   TYPE bapiret2-field OPTIONAL
        !iv_variable_1   TYPE symsgv OPTIONAL
        !iv_variable_2   TYPE symsgv OPTIONAL
        !iv_variable_3   TYPE symsgv OPTIONAL
        !iv_variable_4   TYPE symsgv OPTIONAL
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return_exc.

    CLASS-METHODS split_long_value_to_bapiret2
      IMPORTING !iv_type           TYPE bapi_mtype DEFAULT 'E'
                !iv_id             TYPE symsgid
                !iv_number         TYPE symsgno
                !iv_long_value     TYPE char200
      RETURNING VALUE(rs_bapiret2) TYPE bapiret2.

    METHODS get_bapiret2_struc
      RETURNING VALUE(rs_bapiret2) TYPE bapiret2.

    METHODS get_bapiret2_table
      RETURNING VALUE(rt_bapiret2) TYPE bapiret2_t.

    METHODS get_code_position
      RETURNING VALUE(rs_code_position) TYPE gts_code_position.

    METHODS add_system_message.

    METHODS add_bapiret2_struc
      IMPORTING
        !is_return TYPE bapiret2.

    METHODS add_bapiret2_table
      IMPORTING
        !it_return TYPE bapiret2_t.

    METHODS add_bapiret1_struc
      IMPORTING
        !is_bapiret1 TYPE bapiret1.

    METHODS add_bapiret1_table
      IMPORTING
        !it_return TYPE bapiret1_tab.

    METHODS add_bapireturn_struc
      IMPORTING
        !is_bapireturn TYPE bapireturn.

    METHODS add_bapireturn_table
      IMPORTING
        !it_return TYPE gtt_bapireturn.

    METHODS add_by_text
      IMPORTING
        !iv_type       TYPE bapi_mtype DEFAULT 'E'
        !iv_message    TYPE bapi_msg
        !iv_field_name TYPE bapiret2-field OPTIONAL
        !iv_variable_1 TYPE symsgv OPTIONAL
        !iv_variable_2 TYPE symsgv OPTIONAL
        !iv_variable_3 TYPE symsgv OPTIONAL
        !iv_variable_4 TYPE symsgv OPTIONAL.

    CLASS-METHODS map_bapireturn_to_bapiret2
      IMPORTING
        !is_bapireturn     TYPE bapireturn
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2.

    CLASS-METHODS map_bapiret1_to_bapiret2
      IMPORTING
        !is_bapiret1       TYPE bapiret1
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2.

    CLASS-METHODS set_bapiret2_message_field
      CHANGING cs_bapiret2 TYPE bapiret2.

    CLASS-METHODS map_bdc_mess_to_bapiret2
      IMPORTING
        !is_bdc_message    TYPE bdcmsgcoll
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2.

    METHODS raise_exception_for_fm
      RAISING
        zcx_return_exc.

    METHODS if_message~get_text
        REDEFINITION.

  PROTECTED SECTION.

    DATA gs_bapiret2 TYPE bapiret2.

    DATA gt_bapiret2 TYPE bapiret2_t.

    DATA gs_code_position TYPE gts_code_position.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcx_return_exc IMPLEMENTATION.

  METHOD add_bapiret2_struc.

    IF gs_bapiret2 IS INITIAL OR
       gs_bapiret2-type NA 'EAX'.

      gs_bapiret2 = is_return.

    ENDIF.

    APPEND is_return TO gt_bapiret2.

  ENDMETHOD.

  METHOD add_bapiret2_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapiret2_struc( <ls_return>  ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_bapireturn_struc.

    DATA(ls_bapiret2) =
      zcx_return_exc=>map_bapireturn_to_bapiret2( is_bapireturn ).

    add_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.

  METHOD add_bapireturn_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapireturn_struc( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.

  METHOD add_bapiret1_struc.

    DATA(ls_bapiret2) = map_bapiret1_to_bapiret2( is_bapiret1 ).

    add_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.

  METHOD add_bapiret1_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapiret1_struc( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.

  METHOD add_by_text.

    "Example:
    "iv_type         = 'e'
    "iv_message      = 'sales order &1 not found.'
    "iv_variable_1   = '100001
    "iv_variable_2   = ''
    "iv_variable_3   = ''
    "iv_variable_4   = ''

    DATA:
      lv_message TYPE bapi_msg,
      ls_return  TYPE bapiret2.

    lv_message = iv_message.

    DO 4 TIMES.


      DATA lv_placeholder_name TYPE c LENGTH 2.
      DATA lv_variable_name TYPE c LENGTH 15.

      lv_placeholder_name  = '&' && sy-index.

      lv_variable_name  = 'iv_variable_' && sy-index.

      ASSIGN (lv_variable_name)
        TO FIELD-SYMBOL(<lv_variable>).

      REPLACE ALL OCCURRENCES OF lv_placeholder_name
        IN lv_message
        WITH <lv_variable>
        IN CHARACTER MODE.

      DATA lv_return_var_name TYPE c LENGTH 15.

      lv_return_var_name = 'MESSAGE_V' && sy-index.

      ASSIGN COMPONENT lv_return_var_name
        OF STRUCTURE ls_return
        TO FIELD-SYMBOL(<lv_return_variable>).

      <lv_return_variable> = <lv_variable>.

    ENDDO.

    ls_return-type    = iv_type.
    ls_return-message = lv_message.
    ls_return-field   = iv_field_name.

    add_bapiret2_struc( ls_return ).

  ENDMETHOD.

  METHOD add_system_message.

    DATA:
      ls_bapiret2 TYPE bapiret2.

    ls_bapiret2-type        = sy-msgty.
    ls_bapiret2-id          = sy-msgid.
    ls_bapiret2-number      = sy-msgno.

    ls_bapiret2-message_v1  = sy-msgv1.
    ls_bapiret2-message_v2  = sy-msgv2.
    ls_bapiret2-message_v3  = sy-msgv3.
    ls_bapiret2-message_v4  = sy-msgv4.

    set_bapiret2_message_field(
      CHANGING cs_bapiret2 = ls_bapiret2 ).

    add_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.

  METHOD constructor.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.

    me->gt_bapiret2 = gt_bapiret2.

    me->gs_bapiret2 = gs_bapiret2.

    me->gs_code_position = gs_code_position.

  ENDMETHOD.

  METHOD create_by_bapiret1_struc.

    DATA(ls_bapiret2) = map_bapiret1_to_bapiret2( is_return ).

    rx_return = create_by_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.

  METHOD create_by_bapiret2_struc.

    IF is_return-type CA 'XAE'.

      rx_return = NEW #( ).

      rx_return->add_bapiret2_struc( is_return ).

    ENDIF.

  ENDMETHOD.

  METHOD create_by_bapiret2_table.

    DATA:
      lv_error_ind  TYPE abap_bool.

    "Has return table an error?
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      IF <ls_return>-type CA 'XAE'.

        lv_error_ind = abap_true.
        EXIT.

      ENDIF.

    ENDLOOP.

    IF lv_error_ind = abap_true.

*      IF iv_restartable_ind = abap_false.

      rx_return = NEW #( ).

*      ELSE.
*
*        rx_return = NEW zcx_restartable_error( ).
*
*      ENDIF.

      rx_return->add_bapiret2_table( it_return ).

    ENDIF.

  ENDMETHOD.

  METHOD create_by_bapireturn_struc.

    DATA(ls_bapiret2) = map_bapireturn_to_bapiret2( is_return ).

    rx_return = create_by_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.

  METHOD create_by_bapireturn_table.

    DATA lt_bapiret2          TYPE bapiret2_t.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      APPEND INITIAL LINE TO lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).

      <ls_bapiret2> = map_bapireturn_to_bapiret2( <ls_return> ).

    ENDLOOP.

    rx_return =
      zcx_return_exc=>create_by_bapiret2_table(
        it_return          = lt_bapiret2
        iv_restartable_ind = iv_restartable_ind ).

  ENDMETHOD.

  METHOD create_by_bapiret1_table.

    DATA:
      lv_error_ind  TYPE abap_bool.

    "Has return table an error?
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      IF <ls_return>-type CA 'XAE'.

        lv_error_ind = abap_true.
        EXIT.

      ENDIF.

    ENDLOOP.

    IF lv_error_ind = abap_true.

*      IF iv_restartable_ind = abap_false.

      rx_return = NEW #( ).

*      ELSE.
*
*        rx_return = NEW zcx_restartable_error( ).
*
*      ENDIF.

      rx_return->add_bapiret1_table( it_return ).

    ENDIF.

  ENDMETHOD.

  METHOD create_by_bdc_message_table.

    DATA lt_bapiret2          TYPE bapiret2_t.

    LOOP AT it_bdc_messages
      ASSIGNING FIELD-SYMBOL(<ls_bdc_message>).

      APPEND INITIAL LINE TO lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).

      <ls_bapiret2> = map_bdc_mess_to_bapiret2( <ls_bdc_message> ).

    ENDLOOP.

    rx_return =
      zcx_return_exc=>create_by_bapiret2_table(
        it_return          = lt_bapiret2
        iv_restartable_ind = iv_restartable_ind ).

  ENDMETHOD.

  METHOD create_empty.

    rx_return = NEW #( ).

  ENDMETHOD.

  METHOD create_by_system_message.

    rx_return = NEW #( ).

    rx_return->add_system_message( ).

  ENDMETHOD.

  METHOD create_by_text.

    rx_return = NEW #( ).

    rx_return->add_by_text(
      iv_type         = iv_type
      iv_message      = iv_message
      iv_field_name   = iv_field_name
      iv_variable_1   = iv_variable_1
      iv_variable_2   = iv_variable_2
      iv_variable_3   = iv_variable_3
      iv_variable_4   = iv_variable_4 ).

  ENDMETHOD.

  METHOD if_message~get_text.

    IF gs_bapiret2-message IS INITIAL.

      set_bapiret2_message_field(
        CHANGING cs_bapiret2 = gs_bapiret2 ).

    ENDIF.

    result = gs_bapiret2-message.

  ENDMETHOD.

  METHOD get_bapiret2_struc.

    rs_bapiret2 = gs_bapiret2.

  ENDMETHOD.

  METHOD get_bapiret2_table.

    rt_bapiret2 = gt_bapiret2.

  ENDMETHOD.

  METHOD get_code_position.

    rs_code_position = gs_code_position.

  ENDMETHOD.

  METHOD map_bapireturn_to_bapiret2.

    rs_bapiret2-type       = is_bapireturn-type.

    "Example value field code: IS504
    rs_bapiret2-id      = is_bapireturn-code+0(2).
    rs_bapiret2-number  = is_bapireturn-code+2(3).

    rs_bapiret2-message    = is_bapireturn-message.
    rs_bapiret2-log_no     = is_bapireturn-log_no.
    rs_bapiret2-log_msg_no = is_bapireturn-log_msg_no.
    rs_bapiret2-message_v1 = is_bapireturn-message_v1.
    rs_bapiret2-message_v2 = is_bapireturn-message_v2.
    rs_bapiret2-message_v3 = is_bapireturn-message_v3.
    rs_bapiret2-message_v4 = is_bapireturn-message_v4.

  ENDMETHOD.

  METHOD map_bapiret1_to_bapiret2.

    rs_bapiret2-type       = is_bapiret1-type.
    rs_bapiret2-id         = is_bapiret1-id.
    rs_bapiret2-number     = is_bapiret1-number.

    rs_bapiret2-message    = is_bapiret1-message.
    rs_bapiret2-log_no     = is_bapiret1-log_no.
    rs_bapiret2-log_msg_no = is_bapiret1-log_msg_no.
    rs_bapiret2-message_v1 = is_bapiret1-message_v1.
    rs_bapiret2-message_v2 = is_bapiret1-message_v2.
    rs_bapiret2-message_v3 = is_bapiret1-message_v3.
    rs_bapiret2-message_v4 = is_bapiret1-message_v4.

  ENDMETHOD.

  METHOD set_bapiret2_message_field.

    MESSAGE
      ID cs_bapiret2-id
      TYPE cs_bapiret2-type
      NUMBER cs_bapiret2-number
      WITH
        cs_bapiret2-message_v1
        cs_bapiret2-message_v2
        cs_bapiret2-message_v3
        cs_bapiret2-message_v4
      INTO cs_bapiret2-message.

  ENDMETHOD.

  METHOD map_bdc_mess_to_bapiret2.

    rs_bapiret2-type       = is_bdc_message-msgtyp.
    rs_bapiret2-id      = is_bdc_message-msgid.
    rs_bapiret2-number  = is_bdc_message-msgnr.

    rs_bapiret2-message_v1 = is_bdc_message-msgv1.
    rs_bapiret2-message_v2 = is_bdc_message-msgv2.
    rs_bapiret2-message_v3 = is_bdc_message-msgv3.
    rs_bapiret2-message_v4 = is_bdc_message-msgv4.

    set_bapiret2_message_field(
      CHANGING cs_bapiret2 = rs_bapiret2 ).

  ENDMETHOD.

  METHOD raise_exception_for_fm.

    "This method was created for raising exceptions within function modules
    RAISE EXCEPTION me.

  ENDMETHOD.

  METHOD split_long_value_to_bapiret2.

    TYPES:
      BEGIN OF ltv_variable,
        text TYPE c LENGTH 50,
      END OF ltv_variable.

    "Set key
    rs_bapiret2-type   = iv_type.
    rs_bapiret2-id     = iv_id.
    rs_bapiret2-number = iv_number.

    "Set text
    DATA:
      lv_text_string TYPE string,
      lt_text        TYPE STANDARD TABLE OF ltv_variable.

    lv_text_string = iv_long_value.

    CALL FUNCTION 'convert_CODE_to_table'
      EXPORTING
        i_string         = lv_text_string
        i_tabline_length = 50
      TABLES
        et_table         = lt_text.

    LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<ls_variable>).

      CASE sy-tabix.
        WHEN 1.
          rs_bapiret2-message_v1 = <ls_variable>-text.
        WHEN 2.
          rs_bapiret2-message_v2 = <ls_variable>-text.
        WHEN 3.
          rs_bapiret2-message_v3 = <ls_variable>-text.
        WHEN 4.
          rs_bapiret2-message_v4 = <ls_variable>-text.
      ENDCASE.

    ENDLOOP.

    "Set message
    set_bapiret2_message_field(
      CHANGING cs_bapiret2 = rs_bapiret2 ).

  ENDMETHOD.

ENDCLASS.

CLASS zcx_return_exc_vw DEFINITION
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS display_system_message
      IMPORTING
        !ix_return TYPE REF TO zcx_return_exc .
    CLASS-METHODS display_system_mess_as_info
      IMPORTING
        !ix_return TYPE REF TO zcx_return_exc .
    CLASS-METHODS display_system_mess_as_warning
      IMPORTING
        !ix_return TYPE REF TO zcx_return_exc .
    CLASS-METHODS write_messages_to_screen
      IMPORTING
        !ix_return TYPE REF TO zcx_return_exc .

ENDCLASS.

CLASS zcx_return_exc_vw IMPLEMENTATION.

  METHOD display_system_message.

    DATA(ls_return) = ix_return->get_bapiret2_struc( ).

    IF ls_return-id IS INITIAL.

      MESSAGE
        ls_return-message
        TYPE    ls_return-type.

    ELSE.

      MESSAGE
        ID      ls_return-id
        TYPE    ls_return-type
        NUMBER  ls_return-number
        WITH    ls_return-message_v1
                ls_return-message_v2
                ls_return-message_v3
                ls_return-message_v4.

    ENDIF.

  ENDMETHOD.


  METHOD display_system_mess_as_info.

    DATA(ls_return) = ix_return->get_bapiret2_struc( ).

    IF ls_return-id IS INITIAL.

      MESSAGE
        ls_return-message
        TYPE    'I'
        DISPLAY LIKE ls_return-type.

    ELSE.

      MESSAGE
        ID      ls_return-id
        TYPE    'I'
        NUMBER  ls_return-number
        WITH    ls_return-message_v1
                ls_return-message_v2
                ls_return-message_v3
                ls_return-message_v4
        DISPLAY LIKE ls_return-type.

    ENDIF.

  ENDMETHOD.


  METHOD display_system_mess_as_warning.

    DATA(ls_return) = ix_return->get_bapiret2_struc( ).

    IF ls_return-id IS INITIAL.

      MESSAGE
        ls_return-message
        TYPE    'W'
        DISPLAY LIKE ls_return-type.

    ELSE.

      MESSAGE
        ID      ls_return-id
        TYPE    'W'
        NUMBER  ls_return-number
        WITH    ls_return-message_v1
                ls_return-message_v2
                ls_return-message_v3
                ls_return-message_v4
        DISPLAY LIKE ls_return-type.

    ENDIF.

  ENDMETHOD.


  METHOD write_messages_to_screen.

    WRITE:
      'T'(010),
      'ID                  '(011),
      'No.'(012),
      'Message'(013).

    DATA(lt_return) = ix_return->get_bapiret2_table( ).

    LOOP AT lt_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      WRITE: /
        <ls_return>-type,
        <ls_return>-id,
        <ls_return>-number,
        <ls_return>-message.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS zab_abap_variable_bo DEFINITION.

  PUBLIC SECTION.

    TYPES:
      gtv_variable_name   TYPE c LENGTH 30,
      gtv_life_time       TYPE c LENGTH 20,
      gtv_variable_kind   LIKE cl_abap_typedescr=>kind,
      gtv_changeable_kind TYPE c LENGTH 20.

    CONSTANTS:
      gcc_class_life_time           TYPE gtv_life_time VALUE 'CLASS',
      gcc_method_life_time          TYPE gtv_life_time VALUE 'METHOD',
      gcc_importing_param_life_time TYPE gtv_life_time VALUE 'IMPORTING',
      gcc_exporting_param_life_time TYPE gtv_life_time VALUE 'EXPORTING',
      gcc_changing_param_life_time  TYPE gtv_life_time VALUE 'CHANGING',
      gcc_returning_param_life_time TYPE gtv_life_time VALUE 'RETURNING',
      gcc_structure_field_life_time TYPE gtv_life_time VALUE 'STRUCTURE_FIELD',

      gcc_kind_table                LIKE cl_abap_typedescr=>kind  VALUE cl_abap_typedescr=>kind_table,
      gcc_kind_structure            LIKE cl_abap_typedescr=>kind  VALUE cl_abap_typedescr=>kind_struct,
      gcc_kind_value                LIKE cl_abap_typedescr=>kind  VALUE cl_abap_typedescr=>kind_elem,
      gcc_kind_ref                  LIKE cl_abap_typedescr=>kind  VALUE cl_abap_typedescr=>kind_ref,

      gcc_type_chng_kind            TYPE gtv_changeable_kind VALUE 'TYPE',
      gcc_constant_chng_kind        TYPE gtv_changeable_kind VALUE 'CONSTANT',
      gcc_static_chng_kind          TYPE gtv_changeable_kind VALUE 'STATIC',
      gcc_variable_chng_kind        TYPE gtv_changeable_kind VALUE 'VARIABLE'.

ENDCLASS.


CLASS zab_abap_naming_bo DEFINITION
  INHERITING FROM zab_abap_variable_bo.

  PUBLIC SECTION.

    TYPES:
      gtv_naming_name TYPE c LENGTH 24,

      BEGIN OF gts_naming,
        life_time       TYPE gtv_life_time,
        variable_kind   TYPE gtv_variable_kind,
        changeable_kind TYPE gtv_changeable_kind,
        prefix          TYPE string,
        postfix         TYPE string,
      END OF gts_naming,
      gtt_naming TYPE STANDARD TABLE OF gts_naming.

    CONSTANTS:
      gcc_dsag_2016_short_naming TYPE gtv_naming_name VALUE 'DSAG 2016 SHORT',
      gcc_long_naming            TYPE gtv_naming_name VALUE 'DSAG 2016 LONG'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_current_naming_name
      RETURNING VALUE(rv_current_naming_name) TYPE gtv_naming_name.

    CLASS-METHODS get_naming_list
      RETURNING VALUE(rt_naming_list) TYPE vrm_values.

    CLASS-METHODS get_instance
      RETURNING VALUE(rr_instance_bo) TYPE REF TO zab_abap_naming_bo.

    METHODS get_naming
      IMPORTING
                life_time          TYPE gtv_life_time
                variable_kind      TYPE gtv_variable_kind
                changeable_kind    TYPE gtv_changeable_kind
      RETURNING VALUE(rs_naming_s) TYPE gts_naming
      RAISING   zcx_return_exc.


    CLASS-METHODS set_naming_name
      IMPORTING iv_naming_name TYPE gtv_naming_name.

  PROTECTED SECTION.

    CLASS-DATA:
      gv_default_naming_name TYPE gtv_naming_name VALUE gcc_dsag_2016_short_naming,

      gv_current_naming_name TYPE gtv_naming_name,

      gr_instance_bo         TYPE REF TO zab_abap_naming_bo.

    DATA gt_naming TYPE gtt_naming.

    METHODS create_naming_list.

ENDCLASS.

CLASS zab_abap_naming_bo IMPLEMENTATION.

  METHOD class_constructor.

    set_naming_name(
      iv_naming_name = gv_default_naming_name ).

  ENDMETHOD.

  METHOD get_naming_list.

    rt_naming_list = VALUE #(
      (
        key = gcc_dsag_2016_short_naming
        text = 'DSAG 2016 short naming'
      )
      (
        key = gcc_long_naming
        text = 'Long naming'
      )

    ).

  ENDMETHOD.

  METHOD get_current_naming_name.

    rv_current_naming_name = gv_current_naming_name.

  ENDMETHOD.

  METHOD get_instance.

    rr_instance_bo = gr_instance_bo.

  ENDMETHOD.

  METHOD set_naming_name.

    CLEAR gr_instance_bo.

    gr_instance_bo = NEW #( ).

    gr_instance_bo->gv_current_naming_name = iv_naming_name.

    gr_instance_bo->create_naming_list( ).

  ENDMETHOD.

  METHOD get_naming.

    READ TABLE gt_naming
      WITH KEY
        life_time       = life_time
        variable_kind   = variable_kind
        changeable_kind = changeable_kind
      INTO rs_naming_s.

    IF sy-subrc <> 0.

      "Code error
      DATA(lr_return_exc) = zcx_return_exc=>create_by_text(
        iv_message    = |Naming not found. Life time: { life_time }, variable kind: { variable_kind }, Changeable kind: { changeable_kind }.| ).

      RAISE EXCEPTION lr_return_exc.

    ENDIF.

  ENDMETHOD.

  METHOD create_naming_list.

    TYPES:
      gtv_convention_name TYPE c LENGTH 20.

*    DATA(lv_convention_name) = gcc_long_naming.
    DATA(lv_convention_name) = gv_current_naming_name.

    gt_naming = VALUE #(

      ""*****************************************************************************
      "Structure field
      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_constant_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_structure_field_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_static_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Class - Types
      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GTT_'
            WHEN gcc_dsag_2016_short_naming THEN 'T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GTS_'
            WHEN gcc_dsag_2016_short_naming THEN 'T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GTV_'
            WHEN gcc_dsag_2016_short_naming THEN 'T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GTR_'
            WHEN gcc_dsag_2016_short_naming THEN 'T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Class - Variables
      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GT_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GS_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GS_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GR_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_constant_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GCC_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_class_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_static_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'GV_'
            WHEN gcc_dsag_2016_short_naming THEN '' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )


      ""*****************************************************************************
      "Method - Types
      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LTT_'
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LTS_'
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LTV_'
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_type_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LTR_'
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Method - Variable
      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LT_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LS_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LV_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LR_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_constant_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LCC_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_method_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_static_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'LV_'
            WHEN gcc_dsag_2016_short_naming THEN '_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Importing - Variable
      (
        life_time     = gcc_importing_param_life_time
        variable_kind = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'IT_'
            WHEN gcc_dsag_2016_short_naming THEN 'I_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time     = gcc_importing_param_life_time
        variable_kind = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'IS_'
            WHEN gcc_dsag_2016_short_naming THEN 'I_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time     = gcc_importing_param_life_time
        variable_kind = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'IV_'
            WHEN gcc_dsag_2016_short_naming THEN 'I_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_importing_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'IR_'
            WHEN gcc_dsag_2016_short_naming THEN 'I_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Exporting - Variable
      (
        life_time     = gcc_exporting_param_life_time
        variable_kind = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'ET_'
            WHEN gcc_dsag_2016_short_naming THEN 'E_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_exporting_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'ES_'
            WHEN gcc_dsag_2016_short_naming THEN 'E_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_exporting_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'EV_'
            WHEN gcc_dsag_2016_short_naming THEN 'E_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time        = gcc_exporting_param_life_time
        variable_kind    = cl_abap_typedescr=>kind_ref
        changeable_kind  = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'ER_'
            WHEN gcc_dsag_2016_short_naming THEN 'E_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Changing - Variable
      (
        life_time       = gcc_changing_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'CT_'
            WHEN gcc_dsag_2016_short_naming THEN 'C_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_changing_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'CS_'
            WHEN gcc_dsag_2016_short_naming THEN 'C_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_changing_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'CV_'
            WHEN gcc_dsag_2016_short_naming THEN 'C_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_changing_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'CR_'
            WHEN gcc_dsag_2016_short_naming THEN 'C_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      ""*****************************************************************************
      "Returning - Variable
      (
        life_time       = gcc_returning_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_table
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'RT_'
            WHEN gcc_dsag_2016_short_naming THEN 'R_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_T' )
      )

      (
        life_time       = gcc_returning_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_struct
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'RS_'
            WHEN gcc_dsag_2016_short_naming THEN 'R_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '_S' )
      )

      (
        life_time       = gcc_returning_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_elem
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'RV_'
            WHEN gcc_dsag_2016_short_naming THEN 'R_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

      (
        life_time       = gcc_returning_param_life_time
        variable_kind   = cl_abap_typedescr=>kind_ref
        changeable_kind = gcc_variable_chng_kind
        prefix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN 'RR_'
            WHEN gcc_dsag_2016_short_naming THEN 'R_' )
        postfix =
          SWITCH #( lv_convention_name
            WHEN gcc_long_naming  THEN ''
            WHEN gcc_dsag_2016_short_naming THEN '' )
      )

    ).

  ENDMETHOD.

ENDCLASS.


CLASS zab_abap_variable_name_bo DEFINITION
  INHERITING FROM zab_abap_variable_bo.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF gts_data,
        name            TYPE gtv_variable_name,
        life_time       TYPE gtv_life_time,
        variable_kind   TYPE gtv_variable_kind,
        changeable_kind TYPE gtv_changeable_kind,
      END OF gts_data.

    CLASS-METHODS create_by_data
      IMPORTING is_data                    TYPE gts_data
      RETURNING VALUE(rr_abap_variable_bo) TYPE REF TO zab_abap_variable_name_bo.

    METHODS get_variable_name
      IMPORTING iv_lower_case_ind       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_variable_name) TYPE gtv_variable_name
      RAISING   zcx_return_exc.

  PROTECTED SECTION.

    DATA gs_data TYPE gts_data.

ENDCLASS.

CLASS zab_abap_variable_name_bo IMPLEMENTATION.


  METHOD create_by_data.

    rr_abap_variable_bo = NEW #( ).

    rr_abap_variable_bo->gs_data = is_data.

  ENDMETHOD.

  METHOD get_variable_name.

    DATA(ls_naming) =
      zab_abap_naming_bo=>get_instance( )->get_naming(
        life_time       = gs_data-life_time
        variable_kind   = gs_data-variable_kind
        changeable_kind = gs_data-changeable_kind ).

    rv_variable_name = ls_naming-prefix && gs_data-name && ls_naming-postfix.

    IF iv_lower_case_ind = abap_true.

      rv_variable_name = to_lower( rv_variable_name ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS zab_abap_data_element_bo2 DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_field,
        name         TYPE string,
        data_element TYPE REF TO zab_abap_data_element_bo2,
      END OF gts_field,
      gtt_field_list TYPE STANDARD TABLE OF gts_field WITH EMPTY KEY.

    CLASS-METHODS:
      get_instance_by_ddic_name
        IMPORTING iv_name                TYPE rollname
        RETURNING VALUE(rr_data_element) TYPE REF TO zab_abap_data_element_bo2
        RAISING   zcx_return_exc,
      get_instance_by_descr
        IMPORTING iv_name                TYPE string
                  ir_abap_elemdescr      TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(rr_data_element) TYPE REF TO zab_abap_data_element_bo2
        RAISING   zcx_return_exc.

    METHODS:
      get_abap_elem_descr
        RETURNING VALUE(rr_type_descr) TYPE REF TO cl_abap_elemdescr,
      get_ddic_field_ind
        RETURNING VALUE(rv_ddic_field_ind) TYPE abap_bool,
      get_internal_data_type
        RETURNING VALUE(rv_internal_data_type) TYPE char1. "Todo: char1 specifiek maken
*      get_struct_field_list
*        RETURNING VALUE(rt_field_list) TYPE gtt_field_list
*        RAISING   zcx_return_exc,

    TYPES:
      BEGIN OF gts_non_ddic_field,
        internal_type TYPE char1,  "todo: specifiek maken
        length        TYPE dfies-leng,
        decimals      TYPE dfies-decimals,
        output_length TYPE dfies-outputlen,
      END OF gts_non_ddic_field.

    METHODS
      get_non_ddic_field
        RETURNING VALUE(rs_dd_field) TYPE gts_non_ddic_field
        RAISING   zcx_return_exc.

    METHODS:
      get_ddic_field
        RETURNING VALUE(rs_dd_field) TYPE dfies
        RAISING   zcx_return_exc,
      get_ddic_check_table
        RETURNING VALUE(rv_check_table) TYPE komp_check
        RAISING   zcx_return_exc,
      get_dummy_value
        RETURNING VALUE(rv_text) TYPE string
        RAISING   zcx_return_exc,
      get_sql_value
        IMPORTING iv_value       TYPE any
        RETURNING VALUE(rv_text) TYPE string
        RAISING   zcx_return_exc.

  PROTECTED SECTION.
    DATA:
      gr_abap_elem_descr TYPE REF TO cl_abap_elemdescr,
      gv_name            TYPE string.

ENDCLASS.


CLASS zab_abap_data_element_bo2 IMPLEMENTATION.

*  METHOD get_instance_by_value.
*
*    rr_data_element = NEW #( ).
*
*    data lr_typedescr type ref to cl_abap_typedescr.
*
*  endmethod.

  METHOD get_instance_by_ddic_name.

    rr_data_element = NEW #( ).

    rr_data_element->gv_name = iv_name.

    DATA lr_typedescr TYPE REF TO cl_abap_typedescr.

    CALL METHOD cl_abap_typedescr=>describe_by_name
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lr_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.

      "Code error
      DATA(lr_return_exc) = zcx_return_exc=>create_by_text(
        iv_message    = |Data type { iv_name } is not found.| ).

      RAISE EXCEPTION lr_return_exc.

    ENDIF.

    rr_data_element->gr_abap_elem_descr = CAST #( lr_typedescr ).

  ENDMETHOD.

  METHOD get_instance_by_descr.

    rr_data_element = NEW #( ).

    rr_data_element->gr_abap_elem_descr = ir_abap_elemdescr.

    DATA(lv_is_ddic_type_ind) = rr_data_element->get_ddic_field_ind( ).

    IF lv_is_ddic_type_ind = abap_false.

      "No name

    ELSE.

      DATA(ls_ddic_field) = ir_abap_elemdescr->get_ddic_field( ).

      rr_data_element->gv_name = ls_ddic_field-rollname.

    ENDIF.

  ENDMETHOD.

  METHOD get_abap_elem_descr.

    rr_type_descr = me->gr_abap_elem_descr.

  ENDMETHOD.

  METHOD get_ddic_field_ind.

    DATA(lv_is_ddic_type_ind) = me->gr_abap_elem_descr->is_ddic_type( ).

  ENDMETHOD.

  METHOD get_internal_data_type.  "TODO: deze methode verwijderen

    DATA(lv_ddic_field_ind) = me->get_ddic_field_ind( ).

    IF lv_ddic_field_ind = abap_true.

      DATA(ls_ddic_field) = me->gr_abap_elem_descr->get_ddic_field( ).

      rv_internal_data_type = ls_ddic_field-inttype.

    ELSE.

      DATA lr_data TYPE REF TO data.

      CREATE DATA lr_data
         TYPE HANDLE me->gr_abap_elem_descr.

      ASSIGN lr_data->*
        TO FIELD-SYMBOL(<lv_value>).

      DESCRIBE FIELD <lv_value>
        TYPE DATA(lv_type).

      rv_internal_data_type = lv_type.

    ENDIF.

  ENDMETHOD.
*
*  METHOD get_struct_field_list.
*
*    DATA(lr_structure_descr) =
*      CAST cl_abap_structdescr( me->gr_abap_elem_descr ).
*
*    DATA(lt_dd_field_list) =
*      lr_structure_descr->get_ddic_field_list( ).
*
*    LOOP AT lt_dd_field_list
*      ASSIGNING FIELD-SYMBOL(<ls_dd_field>).
*
*      APPEND INITIAL LINE TO rt_field_list
*        ASSIGNING FIELD-SYMBOL(<ls_field>).
*
*      <ls_field>-name = <ls_dd_field>-fieldname.
*
*      <ls_field>-data_element =
*        zab_abap_data_element_bo2=>get_instance_by_ddic_name( <ls_dd_field>-rollname ).
*
*    ENDLOOP.
*
*  ENDMETHOD.

  METHOD get_non_ddic_field.

    DATA lr_data TYPE REF TO data.

    CREATE DATA lr_data
       TYPE HANDLE me->gr_abap_elem_descr.

    ASSIGN lr_data->*
      TO FIELD-SYMBOL(<lv_value>).

    DESCRIBE FIELD <lv_value>
      TYPE DATA(lv_type).

    CASE lv_type.

      WHEN 'P'.

        DESCRIBE FIELD <lv_value>
          LENGTH DATA(lv_length) IN BYTE MODE
          DECIMALS DATA(lv_decimals)
          OUTPUT-LENGTH DATA(lv_output_length).

      WHEN OTHERS.

        DESCRIBE FIELD <lv_value>
          TYPE lv_type
          LENGTH lv_length IN CHARACTER MODE
          OUTPUT-LENGTH lv_output_length.

    ENDCASE.
*DESCRIBE FIELD dobj
*  [OUTPUT-LENGTH olen]
*  [HELP-ID hlp]
*  [EDIT MASK mask].


    rs_dd_field-internal_type = lv_type.
    rs_dd_field-length        = lv_length.
    rs_dd_field-decimals      = lv_decimals.
    rs_dd_field-output_length = lv_output_length.

  ENDMETHOD.

  METHOD get_ddic_field.

    TRY.

        DATA(lr_element) = CAST cl_abap_elemdescr( me->gr_abap_elem_descr ).

      CATCH cx_root INTO DATA(lx_root).

        "Code error
        DATA(lr_return_exc) = zcx_return_exc=>create_by_text(
          iv_message    = |get_ddic_field error:  { lx_root->get_text( ) }| ).

        RAISE EXCEPTION lr_return_exc.

    ENDTRY.

    rs_dd_field = lr_element->get_ddic_field( ).

  ENDMETHOD.

  METHOD get_ddic_check_table.

    DATA(ls_dd_field) = get_ddic_field( ).

    "Get check table from structure
    IF ls_dd_field-checktable IS NOT INITIAL.

      rv_check_table = ls_dd_field-checktable.
      RETURN.

    ENDIF.

    DATA:
      ls_dd01v       TYPE dd01v,
      lt_dd07v_tab_a TYPE STANDARD TABLE OF dd07v,
      lt_dd07v_tab_n TYPE STANDARD TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name   = ls_dd_field-domname
*       GET_STATE     = 'M  '
*       LANGU         = SY-LANGU
*       PRID          = 0
*       WITHTEXT      = 'X'
      IMPORTING
        dd01v_wa_a    = ls_dd01v
*       DD01V_WA_N    =
*       GOT_STATE     =
      TABLES
        dd07v_tab_a   = lt_dd07v_tab_a
        dd07v_tab_n   = lt_dd07v_tab_n
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.

      DATA(lr_return_exc) = zcx_return_exc=>create_by_system_message( ).

      RAISE EXCEPTION lr_return_exc.

    ENDIF.

    IF ls_dd01v-entitytab IS NOT INITIAL.

      rv_check_table = ls_dd01v-entitytab.
      RETURN.

    ENDIF.

    IF lt_dd07v_tab_a[] IS NOT INITIAL.

      DATA(lv_count) = lines( lt_dd07v_tab_a ).

      DATA(lv_count_text) = |{ lv_count }|.

      rv_check_table = lv_count_text && | fixed values|.

    ENDIF.

  ENDMETHOD.

  METHOD get_dummy_value.

    DATA(lv_internal_data_type) = get_internal_data_type( ).

    CASE lv_internal_data_type.

      WHEN 'C' OR 'g'.
        "C Character String
        "g  Character string with variable length (ABAP type STRING)

        rv_text = ''''''.

      WHEN 'D'.
        "D Date (Date: YYYYMMDD)

        rv_text = '00000000'.

      WHEN 'T'.
        "T  Time (Time: HHMMSS)

        rv_text = '000000'.

      WHEN 'N'.
        "N Character String with Digits Only
        rv_text = '0'.

      WHEN 'N'.
        "I Integer number (4-byte integer with sign)
        rv_text = '0'.

      WHEN 'P'.
        "P Packed number
        rv_text = '''0.0'''.

      WHEN 'I' OR'b' OR 's' OR 'F' OR 'X' OR
        'a' OR 'e'.
        "I
        "b  1-byte integer, integer number <= 254
        "s  2-byte integer, only for length field before LCHR or LRAW
        "F  Floating point number to accuracy of 8 bytes
        "X  Byte Seq. (heXadecimal), in DDIC metadata also for INT1/2/4

        "a  Decimal Floating Point Number, 16 Digits
        "e  Decimal Floating Point Number, 34 Digits

        rv_text = '0'.

      WHEN OTHERS.
        "y  Byte sequence with variable length (ABAP type XSTRING)
        "u  Structured type, flat
        "v  Structured type, deep
        "h  Table type
        "  Character string (old Dictionary type VARC)
        "r  Reference to class/interface
        "l  Reference to data object

        "j  Static Boxed Components
        "k  Generic Boxed Components

        rv_text = |? initial value unknown for datatype: | && lv_internal_data_type.

    ENDCASE.

  ENDMETHOD.

  METHOD get_sql_value.

    DATA(lv_internal_data_type) = me->get_internal_data_type( ).

    CASE lv_internal_data_type.

      WHEN 'C'.
        "C Character String

        rv_text = |'| && iv_value && |'|.

      WHEN 'D'.
        "D Date (Date: YYYYMMDD)

        rv_text = |'| && iv_value && |'|.

      WHEN 'T'.
        "T  Time (Time: HHMMSS)

        rv_text = |'| && iv_value && |'|.

      WHEN 'N'.
        "N Character String with Digits Only
        rv_text = |'| && iv_value && |'|.

      WHEN 'N'.
        "I Integer number (4-byte integer with sign)
        rv_text = iv_value.

      WHEN 'P'.
        "P Packed number
        rv_text = |'| && iv_value && |'|.

      WHEN 'I' OR'b' OR 's' OR 'F'.
        "I
        "b  1-byte integer, integer number <= 254
        "s  2-byte integer, only for length field before LCHR or LRAW
        "F  Floating point number to accuracy of 8 bytes
        rv_text = iv_value.

      WHEN OTHERS.

        "X  Byte Seq. (heXadecimal), in DDIC metadata also for INT1/2/4
        "g  Character string with variable length (ABAP type STRING)
        "y  Byte sequence with variable length (ABAP type XSTRING)
        "u  Structured type, flat
        "v  Structured type, deep
        "h  Table type
        "  Character string (old Dictionary type VARC)
        "r  Reference to class/interface
        "l  Reference to data object
        "a  Decimal Floating Point Number, 16 Digits
        "e  Decimal Floating Point Number, 34 Digits
        "j  Static Boxed Components
        "k  Generic Boxed Components

        rv_text = |? syntax unknown for datatype: | && lv_internal_data_type.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS zab_abap_text_bo DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_spaces
        IMPORTING iv_tab_character_count TYPE i
                  iv_tab_count           TYPE i
        RETURNING VALUE(rv_text)         TYPE string,
      add_trailing_space
        IMPORTING iv_text        TYPE any
                  iv_length      TYPE i
        RETURNING VALUE(rv_text) TYPE string.

  PROTECTED SECTION.
    DATA:
      gv_tab_character_count TYPE i.

ENDCLASS.

CLASS zab_abap_text_bo IMPLEMENTATION.

  METHOD get_spaces.

    DO iv_tab_count TIMES.

      DO iv_tab_character_count TIMES.

        rv_text = rv_text && | |.

      ENDDO.

    ENDDO.

  ENDMETHOD.

  METHOD add_trailing_space.

    DATA(lv_text_length) = strlen( iv_text ).

    IF lv_text_length > iv_length.
      rv_text = iv_text.
      RETURN.
    ENDIF.

    DATA(lv_spaces_length) = iv_length - lv_text_length.

    DATA(lv_spaces) =
      zab_abap_text_bo=>get_spaces(
        iv_tab_character_count = 1
        iv_tab_count = lv_spaces_length ).

    rv_text = iv_text && lv_spaces.

  ENDMETHOD.

ENDCLASS.

CLASS zab_deep_to_flat_data_cvt2 DEFINITION.

  PUBLIC SECTION.


    TYPES:
      gtv_level TYPE i,
      gtv_key   TYPE int4,

      BEGIN OF gts_data_component,
        level                    TYPE gtv_level,
        key                      TYPE gtv_key,
        parent_key               TYPE gtv_key,

        kind                     TYPE abap_typecategory,
        name                     TYPE string,

        table_line_count         TYPE i,
        line_index               TYPE i,
        initial_value_ind        TYPE abap_bool,
        internal_value           TYPE string,
        external_value           TYPE string,

        rollname                 TYPE rollname,
        domname                  TYPE domname,

*        checktable               TYPE tabname,         "Based on the structure
        domain_value_table       TYPE dd01v-entitytab, "Based on the domain
        domain_fixed_value_count TYPE int4,

        leng                     TYPE ddleng,
*        intlen                   TYPE intlen,
        outputlen                TYPE outputlen,
        decimals                 TYPE decimals,

        datatype                 TYPE dynptype,
        inttype                  TYPE inttype,

        fieldtext                TYPE as4text,
        reptext                  TYPE reptext,
        scrtext_s                TYPE scrtext_s,
        scrtext_m                TYPE scrtext_m,
        scrtext_l                TYPE scrtext_l,

        convexit                 TYPE convexit,
        keyflag                  TYPE keyflag,
        lowercase                TYPE lowercase,

        abap_elemdescr           TYPE REF TO cl_abap_elemdescr,

      END OF gts_data_component,
      gtt_data_components TYPE STANDARD TABLE OF gts_data_component WITH DEFAULT KEY.


    METHODS convert_data
      IMPORTING iv_root_name              TYPE string
                is_data                   TYPE any
                iv_empty_table_add_line_ind type abap_bool
      RETURNING VALUE(rt_data_components) TYPE gtt_data_components
      RAISING   zcx_return_exc.

  PROTECTED SECTION.

    data:
      gv_empty_table_add_line_ind type abap_bool.

    METHODS add_field_data
      IMPORTING level              TYPE gtv_level
                key                TYPE gtv_key
                parent_key         TYPE gtv_key
                kind               TYPE abap_typecategory
                field_descr        TYPE REF TO cl_abap_datadescr
                field_name         TYPE string
                line_index         TYPE i
                field_line_count   TYPE i         OPTIONAL
                field_value        TYPE any
      CHANGING  ct_data_components TYPE gtt_data_components
      RAISING   zcx_return_exc.

    METHODS write_table_data
      IMPORTING iv_level           TYPE gtv_level
                iv_parent_key      TYPE gtv_key
                iv_name            TYPE string
                iv_line_index      TYPE i
                ir_type            TYPE REF TO cl_abap_datadescr
                it_data            TYPE ANY TABLE
      CHANGING  cv_key             TYPE gtv_key
                ct_data_components TYPE gtt_data_components
      RAISING   zcx_return_exc.

    METHODS write_structure_data
      IMPORTING iv_level           TYPE gtv_level
                iv_parent_key      TYPE gtv_key
                iv_name            TYPE string
                iv_line_index      TYPE i
                ir_type            TYPE REF TO cl_abap_datadescr
                is_data            TYPE any
      CHANGING  cv_key             TYPE gtv_key
                ct_data_components TYPE gtt_data_components
      RAISING   zcx_return_exc.

    METHODS write_any_data
      IMPORTING iv_level           TYPE gtv_level
                iv_parent_key      TYPE gtv_key
                iv_name            TYPE string
                iv_line_index      TYPE i OPTIONAL
                ir_type            TYPE REF TO cl_abap_datadescr
                i_data             TYPE any
      CHANGING  cv_key             TYPE gtv_key
                ct_data_components TYPE gtt_data_components
      RAISING   zcx_return_exc.

ENDCLASS.

CLASS zab_deep_to_flat_data_cvt2 IMPLEMENTATION.

  METHOD convert_data.

    gv_empty_table_add_line_ind = iv_empty_table_add_line_ind.

    DATA lr_data TYPE REF TO data.

    DATA lr_root TYPE REF TO cl_abap_datadescr.

    lr_root = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( is_data ) ).

    DATA lv_key TYPE gtv_key.

    lv_key = 1.

    write_any_data(
      EXPORTING
        iv_level           = 1
        iv_parent_key      = 0
        iv_name            = 'Root'
        ir_type            = lr_root
        i_data             = is_data
      CHANGING
        cv_key             = lv_key
        ct_data_components = rt_data_components ).

  ENDMETHOD.

  METHOD write_any_data.

    CASE ir_type->kind.

      WHEN cl_abap_typedescr=>kind_elem.

        add_field_data(
          EXPORTING level              = iv_level
                    key                = cv_key
                    parent_key         = iv_parent_key
                    kind               = cl_abap_typedescr=>kind_elem
                    field_descr        = ir_type
                    field_name         = iv_name
                    line_index         = iv_line_index
                    field_value        = i_data
          CHANGING  ct_data_components = ct_data_components ).

      WHEN cl_abap_typedescr=>kind_struct.

        write_structure_data(
          EXPORTING
            iv_level      = iv_level
            iv_parent_key = iv_parent_key
            iv_name       = iv_name
            iv_line_index = iv_line_index
            ir_type       = ir_type
            is_data       = i_data
        CHANGING
          cv_key             = cv_key
          ct_data_components = ct_data_components ).

      WHEN cl_abap_typedescr=>kind_table.

        write_table_data(
          EXPORTING
            iv_parent_key      = iv_parent_key
            iv_level           = iv_level
            iv_name            = iv_name
            iv_line_index      = iv_line_index
            ir_type            = ir_type
            it_data            = i_data
          CHANGING
            cv_key             = cv_key
            ct_data_components = ct_data_components ).

    ENDCASE.

  ENDMETHOD.

  METHOD write_structure_data.

    "Add component
    add_field_data(
      EXPORTING level              = iv_level
                key                = cv_key
                parent_key         = iv_parent_key
                kind               = cl_abap_typedescr=>kind_struct
                field_descr        = ir_type
                field_name         = iv_name
                line_index         = iv_line_index
                field_value        = ''
      CHANGING  ct_data_components = ct_data_components ).

    DATA(lr_struct) = CAST cl_abap_structdescr( ir_type ).

    DATA(lt_components) = lr_struct->get_components( ).

    DATA lv_level TYPE i.

    lv_level = iv_level + 1.

    DATA(lv_parent_key) = cv_key.

    LOOP AT lt_components
      ASSIGNING FIELD-SYMBOL(<ls_component>).

      cv_key = cv_key + 1.

      DATA(lv_field_name) =
        zab_abap_text_bo=>add_trailing_space(
          iv_text = <ls_component>-name
          iv_length = 30 ).

      ASSIGN COMPONENT <ls_component>-name
        OF STRUCTURE is_data
        TO FIELD-SYMBOL(<l_sub_field_data>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      write_any_data(
        EXPORTING
          iv_level           = lv_level
          iv_parent_key      = lv_parent_key
          iv_name            = <ls_component>-name
          ir_type            = <ls_component>-type
          i_data             = <l_sub_field_data>
        CHANGING
          cv_key             = cv_key
          ct_data_components = ct_data_components ).

    ENDLOOP.

  ENDMETHOD.

  METHOD write_table_data.

    "Add component
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    ASSIGN it_data TO <lt_table>.

    add_field_data(
      EXPORTING level              = iv_level
                key                = cv_key
                parent_key         = iv_parent_key
                kind               = cl_abap_typedescr=>kind_table
                field_descr        = ir_type
                field_name         = iv_name
                line_index         = iv_line_index
                field_line_count   = lines( <lt_table> )
                field_value        = ''
      CHANGING  ct_data_components = ct_data_components ).

    "Level
    DATA lv_level TYPE i.

    lv_level = iv_level + 1.

    "Get line type
    DATA(lr_table_type) = CAST cl_abap_tabledescr( ir_type ).

    DATA(lr_line_type) = lr_table_type->get_table_line_type( ).

    "Parent key
    DATA(lv_parent_key) = cv_key.


    "Create initial line?
    FIELD-SYMBOLS <lt_table_data> TYPE ANY TABLE.

    IF gv_empty_table_add_line_ind = abap_true.

      IF lines( it_data ) = 0.

        "Table
        DATA lr_table_data TYPE REF TO data.

        CREATE DATA lr_table_data TYPE HANDLE lr_table_type.

        lr_table_type->get_table_line_type( ).

        ASSIGN lr_table_data->* TO <lt_table_data>.

        "Line
        DATA lr_line_data TYPE REF TO data.

        CREATE DATA lr_line_data TYPE HANDLE lr_line_type.

        FIELD-SYMBOLS <ls_line> TYPE any.

        ASSIGN lr_line_data->* TO <ls_line>.

        "Append / insert
        CASE lr_table_type->table_kind.

          WHEN cl_abap_tabledescr=>tablekind_std.

            FIELD-SYMBOLS <lt_std_table> TYPE STANDARD TABLE.

            ASSIGN lr_table_data->* TO <lt_std_table>.

            APPEND <ls_line> TO <lt_std_table>.

          WHEN cl_abap_tabledescr=>tablekind_sorted.

            FIELD-SYMBOLS <lt_sorted_table> TYPE SORTED TABLE.

            ASSIGN lr_table_data->* TO <lt_sorted_table>.

            APPEND <ls_line> TO <lt_sorted_table>.

          WHEN cl_abap_tabledescr=>tablekind_hashed.

            FIELD-SYMBOLS <lt_hashed_table> TYPE HASHED TABLE.

            ASSIGN lr_table_data->* TO <lt_hashed_table>.

            INSERT <ls_line> INTO TABLE <lt_hashed_table>.

          WHEN OTHERS.
            BREAK-POINT. "Not yet supported.

        ENDCASE.

      ENDIF.

    ELSE.

      ASSIGN it_data TO <lt_table_data>.

    ENDIF.

    "Loop at records
    LOOP AT <lt_table_data>
      ASSIGNING FIELD-SYMBOL(<ls_data>).

      DATA(lv_tabix) = sy-tabix.

      cv_key = cv_key + 1.

      write_any_data(
        EXPORTING
          iv_level           = lv_level
          iv_parent_key      = lv_parent_key
          iv_name            = ''
          iv_line_index      = sy-tabix
          ir_type            = lr_line_type
          i_data             = <ls_data>
        CHANGING
          cv_key             = cv_key
          ct_data_components = ct_data_components ).

    ENDLOOP.

  ENDMETHOD.

  METHOD add_field_data.

    APPEND INITIAL LINE TO ct_data_components
      ASSIGNING FIELD-SYMBOL(<ls_data_component>).

    <ls_data_component>-level            = level.
    <ls_data_component>-key              = key.
    <ls_data_component>-parent_key       = parent_key.
    <ls_data_component>-kind             = kind.
    <ls_data_component>-name             = field_name.
    <ls_data_component>-table_line_count = field_line_count.
    <ls_data_component>-line_index       = line_index.

    "Set value
    <ls_data_component>-internal_value    = field_value.
    CONDENSE <ls_data_component>-internal_value.

    IF field_value IS INITIAL.
      <ls_data_component>-initial_value_ind = abap_true.
    ENDIF.

    DATA lv_char TYPE c LENGTH 900.
    WRITE field_value TO lv_char LEFT-JUSTIFIED.
    <ls_data_component>-external_value = lv_char.

    "Set meta data
    DATA(lv_is_ddic_type_ind) = field_descr->is_ddic_type( ).

    CASE field_descr->kind.

      WHEN cl_abap_typedescr=>kind_elem.

        DATA(lr_elem_descr) = CAST cl_abap_elemdescr( field_descr ).

        CASE lv_is_ddic_type_ind.

          WHEN abap_true.

            DATA(ls_ddic) = lr_elem_descr->get_ddic_field( ).

            <ls_data_component>-rollname      = ls_ddic-rollname.
            <ls_data_component>-domname       = ls_ddic-domname.
*            <ls_data_component>-checktable    = ls_ddic-checktable.

            <ls_data_component>-leng          = ls_ddic-leng.
            <ls_data_component>-decimals      = ls_ddic-decimals.
*            <ls_data_component>-intlen        = ls_ddic-intlen.
            <ls_data_component>-outputlen     = ls_ddic-outputlen.

            <ls_data_component>-datatype      = ls_ddic-datatype.
            <ls_data_component>-inttype       = ls_ddic-inttype.

            <ls_data_component>-convexit      = ls_ddic-convexit.
            <ls_data_component>-keyflag       = ls_ddic-keyflag.
            <ls_data_component>-lowercase     = ls_ddic-lowercase.

            IF ls_ddic-domname IS NOT INITIAL.

              "Fixed values
              DATA lt_dd07v TYPE STANDARD TABLE OF dd07v.

              CALL FUNCTION 'DD_DOMVALUES_GET'
                EXPORTING
                  domname        = ls_ddic-domname
                  text           = ''
                  langu          = ''
                TABLES
                  dd07v_tab      = lt_dd07v
                EXCEPTIONS
                  wrong_textflag = 1
                  OTHERS         = 2.

              IF sy-subrc = 0.

              ENDIF.

              <ls_data_component>-domain_fixed_value_count = lines( lt_dd07v ).

              REFRESH lt_dd07v.

              DATA ls_dd01v TYPE dd01v.

              CALL FUNCTION 'DDIF_DOMA_GET'
                EXPORTING
                  name      = ls_ddic-domname
*                 STATE     = 'A'
*                 LANGU     = ' '
                IMPORTING
*                 GOTSTATE  =
                  dd01v_wa  = ls_dd01v
                TABLES
                  dd07v_tab = lt_dd07v
*               EXCEPTIONS
*                 ILLEGAL_INPUT       = 1
*                 OTHERS    = 2
                .
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.

              <ls_data_component>-domain_value_table = ls_dd01v-entitytab.

              REFRESH lt_dd07v.

            ENDIF.

          WHEN OTHERS.

            DATA(lr_abap_data_element_bo) = zab_abap_data_element_bo2=>get_instance_by_descr(
              iv_name           = <ls_data_component>-name
              ir_abap_elemdescr = lr_elem_descr ).

            DATA(ls_non_ddic_field) = lr_abap_data_element_bo->get_non_ddic_field( ).

            <ls_data_component>-leng          = ls_non_ddic_field-length.
            <ls_data_component>-decimals      = ls_non_ddic_field-decimals.
            <ls_data_component>-outputlen     = ls_non_ddic_field-output_length.

*            <ls_data_component>-datatype      = ls_ddic-datatype.
            <ls_data_component>-inttype       = ls_non_ddic_field-internal_type.

        ENDCASE.

        <ls_data_component>-abap_elemdescr = lr_elem_descr.

      WHEN cl_abap_typedescr=>kind_struct.

        DATA(lr_struct_descr) = CAST cl_abap_structdescr( field_descr ).

        CASE lv_is_ddic_type_ind.

          WHEN abap_true.

            DATA(lv_relative_name) =  lr_struct_descr->get_relative_name( ).

            DATA(lv_tabname) = CONV ddobjname( lv_relative_name ).

            CALL FUNCTION 'DDIF_NAMETAB_GET'
              EXPORTING
                tabname   = lv_tabname
                all_types = 'X'
*               LFIELDNAME        = ' '
*               GROUP_NAMES       = ' '
*               UCLEN     =
              IMPORTING
*               X030L_WA  = p_header
*               DTELINFO_WA       =
*               TTYPINFO_WA       =
*               DDOBJTYPE =
                dfies_wa  = ls_ddic
*               LINES_DESCR       =
*             TABLES
*               X031L_TAB =
*               DFIES_TAB =
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

        ENDCASE.

      WHEN cl_abap_typedescr=>kind_table.

        DATA(lr_table_descr) = CAST cl_abap_tabledescr( field_descr ).

        CASE lv_is_ddic_type_ind.

          WHEN abap_true.

            lv_relative_name =  lr_table_descr->get_relative_name( ).

            lv_tabname = CONV ddobjname( lv_relative_name ).

            CALL FUNCTION 'DDIF_NAMETAB_GET'
              EXPORTING
                tabname   = lv_tabname
                all_types = 'X'
*               LFIELDNAME        = ' '
*               GROUP_NAMES       = ' '
*               UCLEN     =
              IMPORTING
*               X030L_WA  = p_header
*               DTELINFO_WA       =
*               TTYPINFO_WA       =
*               DDOBJTYPE =
                dfies_wa  = ls_ddic
*               LINES_DESCR       =
*             TABLES
*               X031L_TAB =
*               DFIES_TAB =
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

        ENDCASE.

    ENDCASE.

    <ls_data_component>-fieldtext     = ls_ddic-fieldtext.
    <ls_data_component>-reptext       = ls_ddic-reptext.
    <ls_data_component>-scrtext_s     = ls_ddic-scrtext_s.
    <ls_data_component>-scrtext_m     = ls_ddic-scrtext_m.
    <ls_data_component>-scrtext_l     = ls_ddic-scrtext_l.

    CLEAR ls_ddic.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_deepstruc_to_abapcode_cvt DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_comment_show,
        field_text_show_ind           TYPE abap_bool,
        element_name_show_ind         TYPE abap_bool,
        domain_name_show_ind          TYPE abap_bool,
        value_table_name_show_ind     TYPE abap_bool,
        conversion_exit_name_show_ind TYPE abap_bool,
        fixed_value_count_show_ind    TYPE abap_bool,
        internal_length_show_ind      TYPE abap_bool,
        output_length_show_ind        TYPE abap_bool,
        decimal_count_show_ind        TYPE abap_bool,
        data_type_name_show_ind       TYPE abap_bool,
        internal_data_type_name_ind   TYPE abap_bool,
        initial_value_ind_show_ind    TYPE abap_bool,
      END OF gts_comment_show,

      BEGIN OF gts_parameters,
        empty_table_add_line_ind type abap_bool,
        show_empty_fields_ind    TYPE abap_bool,
        comment_show             tYPE gts_comment_show,

      END OF gts_parameters.

    TYPES:
      gtt_lines TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS convert
      IMPORTING is_data         TYPE any
                is_parameters   TYPE gts_parameters
      RETURNING VALUE(rt_lines) TYPE gtt_lines
      RAISING   zcx_return_exc.

  PROTECTED SECTION.

    CLASS-METHODS add_sub_elements
      IMPORTING iv_type         TYPE char1
                iv_level        TYPE i
                it_lines        TYPE zab_deep_to_flat_data_cvt2=>gtt_data_components
                is_parameters   TYPE gts_parameters
      CHANGING  cv_line         TYPE i
      RETURNING VALUE(rt_lines) TYPE gtt_lines
      RAISING   zcx_return_exc.

ENDCLASS.

CLASS lcl_deepstruc_to_abapcode_cvt IMPLEMENTATION.

  METHOD convert.

    "Convert deep structure to flat data
    DATA(lt_flat_data) =
      NEW zab_deep_to_flat_data_cvt2( )->convert_data(
        iv_root_name                = 'GET_DATA( )'
        is_data                     = is_data
        iv_empty_table_add_line_ind = is_parameters-empty_table_add_line_ind ).

    "Convert Flat data to ABAP data code
    DATA lv_line TYPE i.

    lv_line = 0.

    rt_lines =
      add_sub_elements(
        EXPORTING iv_type             = 'R'
                  iv_level            = 0
                  it_lines            = lt_flat_data
                  is_parameters       = is_parameters
        CHANGING  cv_line   = lv_line ).

    IF rt_lines[] IS NOT INITIAL.

      READ TABLE rt_lines
        ASSIGNING FIELD-SYMBOL(<lv_line>)
        INDEX lines( rt_lines ).

      <lv_line> = <lv_line> && |.|.

    ENDIF.

  ENDMETHOD.

  METHOD add_sub_elements.

    DATA(lv_end) = abap_false.

    DATA(lv_sub_level) = iv_level + 1.

    WHILE lv_end = abap_false.

      cv_line = cv_line + 1.

      READ TABLE it_lines
        ASSIGNING FIELD-SYMBOL(<ls_element>)
        INDEX cv_line.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      DATA(lv_line) = ||.

      DATA(lv_spaces_text) = zab_abap_text_bo=>get_spaces(
        iv_tab_character_count = <ls_element>-level
         iv_tab_count          = 2 ).

      CASE <ls_element>-kind.

        WHEN 'S' OR 'T'.

          IF <ls_element>-level <= iv_level.
            cv_line = cv_line - 1.
            RETURN.
          ENDIF.

          DATA(lt_sub_lines) =
            add_sub_elements(
              EXPORTING iv_type          = <ls_element>-kind
                        iv_level         = <ls_element>-level
                        it_lines         = it_lines
                        is_parameters = is_parameters
              CHANGING  cv_line          = cv_line ).

          IF lt_sub_lines[] IS NOT INITIAL.

            ""*********************************************************************
            "Begin Structure / table name
            ""*********************************************************************
            lv_line = lv_spaces_text.

            IF <ls_element>-name IS NOT INITIAL.
              lv_line = lv_line && to_lower( <ls_element>-name ) && | = VALUE #|.
            ENDIF.

            lv_line = lv_line &&  |( |.

            APPEND lv_line TO rt_lines.

            ""*********************************************************************
            "Sub lines
            ""*********************************************************************
            APPEND LINES OF lt_sub_lines TO rt_lines.

            ""*********************************************************************
            "End Structure / table name
            ""*********************************************************************
            lv_line = lv_spaces_text && |)|.

            APPEND lv_line TO rt_lines.

          ENDIF.

        WHEN 'E'.

          IF <ls_element>-level <> lv_sub_level.
            BREAK-POINT.
          ENDIF.

          IF is_parameters-show_empty_fields_ind = abap_false.

            IF <ls_element>-initial_value_ind = abap_true.
              CONTINUE.
            ENDIF.

          ENDIF.

          ""********************************************************************
          "Set description
          ""********************************************************************
          IF is_parameters-comment_show IS NOT INITIAL.

            CLEAR lv_line.
            APPEND lv_line TO rt_lines.

            IF is_parameters-comment_show-field_text_show_ind = abap_true.
              IF <ls_element>-fieldtext IS NOT INITIAL.
                lv_line = lv_line && |Descr.: | && <ls_element>-fieldtext.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-element_name_show_ind = abap_true.
              IF <ls_element>-rollname IS NOT INITIAL.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Element name: | &&  <ls_element>-rollname.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-domain_name_show_ind = abap_true.
              IF <ls_element>-domname IS NOT INITIAL.
                lv_line = lv_line &&
                COND string( WHEN lv_line <> || THEN |, | ) &&
                |Dom.name: | && <ls_element>-domname.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-value_table_name_show_ind = abap_true.
              IF <ls_element>-domain_value_table IS NOT INITIAL.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Value table: | && <ls_element>-domain_value_table.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-conversion_exit_name_show_ind = abap_true.
              IF <ls_element>-convexit IS NOT INITIAL.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Conversion exit: | && <ls_element>-convexit.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-fixed_value_count_show_ind = abap_true.
              IF <ls_element>-domain_fixed_value_count IS NOT INITIAL.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Fixed value count: | && <ls_element>-domain_fixed_value_count.
              ENDIF.
            ENDIF.

            DATA lv_length TYPE string.

            IF is_parameters-comment_show-internal_length_show_ind = abap_true.
              IF <ls_element>-leng IS NOT INITIAL.
                lv_length  = |{ <ls_element>-leng ALPHA = OUT }|.
                lv_length  = condense( lv_length ).
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Internal length: { lv_length }|.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-output_length_show_ind = abap_true.
              IF <ls_element>-outputlen IS NOT INITIAL.
                lv_length  = |{ <ls_element>-outputlen ALPHA = OUT }|.
                lv_length  = condense( lv_length ).
*              lv_length  = replace( val = lv_length sub = | | with = || ).
*              SHIFT lv_length RIGHT DELETING TRAILING SPACE.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Output length: { lv_length }|.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-decimal_count_show_ind = abap_true.
              IF <ls_element>-decimals IS NOT INITIAL.
                DATA lv_value TYPE string.
                lv_value  = |{ <ls_element>-decimals ALPHA = OUT }|.
                lv_value  = condense( lv_value ).
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Decimals: | && lv_value.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-data_type_name_show_ind = abap_true.
              IF <ls_element>-datatype IS NOT INITIAL.
                lv_line = lv_line &&
                  COND string( WHEN lv_line <> || THEN |, | ) &&
                  |Data type: | && <ls_element>-datatype.
              ENDIF.
            ENDIF.

            IF is_parameters-comment_show-internal_data_type_name_ind = abap_true.
              lv_line = lv_line &&
                COND string( WHEN lv_line <> || THEN |, | ) &&
                |Internal data type: | && <ls_element>-inttype.
            ENDIF.

            IF is_parameters-comment_show-initial_value_ind_show_ind = abap_true.
              lv_line = lv_line &&
                COND string( WHEN lv_line <> || THEN |, | ) &&
                |Initial value 'X'/'': | && <ls_element>-initial_value_ind.
            ENDIF.

            lv_line = lv_spaces_text &&  |"| && lv_line.

            APPEND lv_line TO rt_lines.

*        reptext                  TYPE reptext,
*        scrtext_s                TYPE scrtext_s,
*        scrtext_m                TYPE scrtext_m,
*        scrtext_l                TYPE scrtext_l,

          ENDIF.

          ""********************************************************************
          "Set field
          ""********************************************************************
          DATA(lr_ddic_data_element_bo) =
            zab_abap_data_element_bo2=>get_instance_by_descr(
              iv_name           = <ls_element>-name
              ir_abap_elemdescr = <ls_element>-abap_elemdescr ).

          lv_value = lr_ddic_data_element_bo->get_sql_value( <ls_element>-internal_value ).

          DATA(lv_spaces_after_sign) = 30 - strlen( <ls_element>-name ).

          DATA(lv_spaces_after_sign_text) = zab_abap_text_bo=>get_spaces(
            iv_tab_character_count = lv_spaces_after_sign
             iv_tab_count          = 1 ).

          lv_line = lv_spaces_text &&
            to_lower( <ls_element>-name )  &&
            lv_spaces_after_sign_text &&
            | = | &&
            lv_value.

          APPEND lv_line TO rt_lines.

        WHEN OTHERS.
          BREAK-POINT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.

ENDCLASS.



CLASS zui01_mvc_object_abs DEFINITION.

  PUBLIC SECTION.

    TYPES:
      gtv_mvc_object_name TYPE char50,
      gtv_action_name     TYPE syst-ucomm.

ENDCLASS.

CLASS zui01_mvc_object_abs IMPLEMENTATION.

ENDCLASS.

CLASS zui01_mvc_action DEFINITION
  INHERITING FROM zui01_mvc_object_abs.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_data,
        object_name TYPE gtv_mvc_object_name,
        action_name TYPE gtv_action_name,
      END OF gts_data.

    CONSTANTS:
      gcc_validate_action         TYPE gtv_action_name VALUE 'VALIDATE'.

    METHODS:
      constructor
        IMPORTING is_data TYPE gts_data,
      get_data
        RETURNING VALUE(rs_data) TYPE gts_data.

  PROTECTED SECTION.

    DATA:
      gs_data TYPE gts_data.

ENDCLASS.

CLASS zui01_mvc_action IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    gs_data = is_data.

  ENDMETHOD.

  METHOD get_data.

    rs_data = gs_data.

  ENDMETHOD.

ENDCLASS.

CLASS zui01_mvc_comp_object_abs DEFINITION
  INHERITING FROM zui01_mvc_object_abs.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_object_name TYPE gtv_mvc_object_name,

      get_name
        RETURNING VALUE(rv_mvc_object_name) TYPE gtv_mvc_object_name.

    EVENTS:
      action
        EXPORTING VALUE(er_action) TYPE REF TO zui01_mvc_action.

  PROTECTED SECTION.

    DATA:
      gv_mvc_object_name TYPE gtv_mvc_object_name.

ENDCLASS.

CLASS zui01_mvc_comp_object_abs IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    gv_mvc_object_name = iv_object_name.

  ENDMETHOD.

  METHOD get_name.

    rv_mvc_object_name = gv_mvc_object_name.

  ENDMETHOD.

ENDCLASS.

CLASS zui01_mvc_view_vw DEFINITION
  ABSTRACT
  INHERITING FROM zui01_mvc_comp_object_abs.

  PUBLIC SECTION.

    METHODS set_data
      IMPORTING is_data               TYPE any
                iv_view_changed       TYPE abap_bool
                iv_controller_changed TYPE abap_bool.

    METHODS refresh.

    METHODS close_screen.

    METHODS destruct.

    METHODS:
      execute_action
        IMPORTING iv_action_name TYPE gtv_action_name,
      raise_action
        IMPORTING iv_action_name TYPE gtv_action_name.

    METHODS get_view_changed_ind
      RETURNING VALUE(rv_view_changed_ind) TYPE abap_bool.

    METHODS get_controller_changed_ind
      RETURNING VALUE(rv_controller_changed_ind) TYPE abap_bool.

  PROTECTED SECTION.

    DATA:
      gv_at_initialization_ind TYPE abap_bool,

      gv_view_changed          TYPE abap_bool,
      gv_controller_changed    TYPE abap_bool.

    METHODS:
      validate.

ENDCLASS.

CLASS zui01_mvc_view_vw IMPLEMENTATION.

  METHOD set_data.

*    FIELD-SYMBOLS <ls_data> TYPE any.
*
*    ASSIGN gr_data->*
*      TO <ls_data>.
*
*    <ls_data> = is_data.

*    gs_data = is_data.

  ENDMETHOD.

  METHOD refresh.

  ENDMETHOD.

  METHOD destruct.

  ENDMETHOD.

  METHOD close_screen.

    SET SCREEN 0.

  ENDMETHOD.

  METHOD validate.

    "Advice: do not use this. Use Validation in de business logic layer for reuse purposes.

  ENDMETHOD.

  METHOD execute_action.

    raise_action(
      iv_action_name     = iv_action_name ).

  ENDMETHOD.

  METHOD raise_action.

    DATA(lr_action) =
      NEW zui01_mvc_action( VALUE #(
        object_name = me->gv_mvc_object_name
        action_name = iv_action_name ) ).

    RAISE EVENT action
      EXPORTING er_action = lr_action.

  ENDMETHOD.

  METHOD get_view_changed_ind.

    rv_view_changed_ind = gv_view_changed.

    CLEAR gv_controller_changed.

  ENDMETHOD.

  METHOD get_controller_changed_ind.

    rv_controller_changed_ind = gv_controller_changed.

    CLEAR gv_controller_changed.

  ENDMETHOD.

ENDCLASS.

CLASS zui01_text_edit_vw2 DEFINITION
  INHERITING FROM zui01_mvc_view_vw
  CREATE PROTECTED.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_data,
        text TYPE string,
      END OF gts_data.

    CONSTANTS:
      gcc_close_action TYPE gtv_action_name VALUE 'CLOSE',
      gcc_back_action  TYPE gtv_action_name VALUE 'BACK'.

    CLASS-METHODS:
      create_in_default_container
        IMPORTING iv_mvc_object_name     TYPE gtv_mvc_object_name
        RETURNING VALUE(rr_text_edit_vw) TYPE REF TO zui01_text_edit_vw2.

    METHODS:
      init,
      set_data_2
        IMPORTING is_data TYPE gts_data,
      show
        IMPORTING iv_focus_ind            TYPE abap_bool DEFAULT abap_false
                  iv_select_all_lines_ind TYPE abap_bool DEFAULT abap_false
        RAISING   zcx_return_exc,

      close,
      pbo,
      pai.

    EVENTS:
      on_new_data
        EXPORTING VALUE(gs_data) TYPE gts_data,
      on_action
        EXPORTING VALUE(ev_action) TYPE gtv_action_name.

  PROTECTED SECTION.

    DATA:
      gs_data                  TYPE gts_data,

      gr_gui_container         TYPE REF TO cl_gui_container,
      gr_text_editor           TYPE REF TO cl_gui_textedit,

      gr_text_editor_container TYPE REF TO cl_gui_custom_container,

      gv_repid                 LIKE sy-repid.

    CONSTANTS: c_line_length TYPE i VALUE 256.

    DATA gv_default_container_ind TYPE abap_bool.

ENDCLASS.

CLASS zui01_text_edit_vw2 IMPLEMENTATION.

  METHOD create_in_default_container.

    rr_text_edit_vw = NEW #(
      iv_object_name = iv_mvc_object_name ).

    rr_text_edit_vw->gr_gui_container = cl_gui_container=>screen0.

    rr_text_edit_vw->gv_default_container_ind = abap_true.

  ENDMETHOD.

  METHOD set_data_2.

    gs_data = is_data.

  ENDMETHOD.

  METHOD init.

  ENDMETHOD.

  METHOD show.

    IF gv_default_container_ind = abap_true.

      "Create text editor
      IF gr_text_editor IS INITIAL.

        gv_repid = sy-repid.

        gr_gui_container = cl_gui_container=>screen0.

        CREATE OBJECT gr_text_editor
          EXPORTING
            parent                     = gr_gui_container
            wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
            wordwrap_position          = 200
            wordwrap_to_linebreak_mode = cl_gui_textedit=>true
          EXCEPTIONS
            OTHERS                     = 1.

      ENDIF.

      "Set text
      gr_text_editor->set_textstream( gs_data-text  ).

      IF sy-subrc NE 0.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = gv_repid
            txt2  = space
            txt1  = 'Set text editor text stream failed'(001).

      ENDIF.

      "Set focus
      IF iv_focus_ind = abap_true.

        gr_text_editor->set_focus( gr_text_editor ).

      ENDIF.

      IF iv_select_all_lines_ind = abap_true.

        gr_text_editor->get_line_count(
          IMPORTING lines = DATA(line_count) ).

        gr_text_editor->select_lines(
          to_line = line_count ).

        MESSAGE 'Push button Copy or Ctrl + C.' TYPE 'S'.

      ENDIF.

      "Call screen
      CALL SELECTION-SCREEN 1000.

    ELSE.

      "Code error
      DATA(lr_return_exc) = zcx_return_exc=>create_by_text(
        iv_message    = |Without default container is not yet supported.| ).

      RAISE EXCEPTION lr_return_exc.

    ENDIF.

  ENDMETHOD.

  METHOD close.

    "Destroy text editor control
    IF NOT gr_text_editor IS INITIAL.

      CALL METHOD gr_text_editor->free
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc NE 0.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = gv_repid
            txt2  = space
            txt1  = 'Free text editor failed'(005).
      ENDIF.

      "Free ABAP object also
      FREE gr_text_editor.

    ENDIF.

    "Destroy container
    IF NOT gr_text_editor_container IS INITIAL.

      CALL METHOD gr_text_editor_container->free
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc <> 0.
        "No error handling
      ENDIF.

      "Free ABAP object also
      FREE gr_text_editor_container.

    ENDIF.

    "Finally flush
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = gv_repid
          txt2  = space
          txt1  = 'Flush cl_gui_cfw failed'(002).
    ENDIF.

  ENDMETHOD.

  METHOD pbo.

    IF gr_text_editor IS INITIAL.

      init( ).

    ENDIF.                               " Editor is initial

* remember: there is an automatic flush at the end of PBO!

  ENDMETHOD.

  METHOD pai.

    DATA lv_action TYPE gtv_action_name.

    CASE sy-ucomm.

      WHEN 'EXIT'.

        lv_action = gcc_close_action.

      WHEN 'BACK'.

        lv_action = gcc_back_action.

    ENDCASE.

    RAISE EVENT on_action
      EXPORTING ev_action = lv_action.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK filter
  WITH FRAME
  TITLE filter_t.

  PARAMETERS:
    p_empty AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN END OF BLOCK filter.


SELECTION-SCREEN BEGIN OF BLOCK show
  WITH FRAME
  TITLE show_t.

  PARAMETERS:
    p_fldtxt AS CHECKBOX DEFAULT abap_false,
    p_elemnm AS CHECKBOX DEFAULT abap_false,
    p_domnm  AS CHECKBOX DEFAULT abap_false,
    p_valtab AS CHECKBOX DEFAULT abap_false,
    p_convex AS CHECKBOX DEFAULT abap_false,
    p_fixval AS CHECKBOX DEFAULT abap_false,
    p_intlen AS CHECKBOX DEFAULT abap_false,
    p_outlen AS CHECKBOX DEFAULT abap_false,
    p_declen AS CHECKBOX DEFAULT abap_false,
    p_dtnm   AS CHECKBOX DEFAULT abap_false,
    p_indtnm AS CHECKBOX DEFAULT abap_false,
    p_inival AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN END OF BLOCK show.

CLASS lcl_main_controller DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_data_object,
        data_object              TYPE REF TO data,
        empty_table_add_line_ind TYPE abap_bool,
      END OF gts_data_object,
      gtt_data_objects TYPE STANDARD TABLE OF gts_data_object.

    METHODS show_abap_data_lines
      IMPORTING it_data_objects TYPE gtt_data_objects.

  PROTECTED SECTION.

    DATA:
      gr_text_edit_vw        TYPE REF TO zui01_text_edit_vw2.


ENDCLASS.

CLASS lcl_main_controller IMPLEMENTATION.

  METHOD show_abap_data_lines.

    TRY.

        DATA lt_all_abap_lines TYPE lcl_deepstruc_to_abapcode_cvt=>gtt_lines.

        LOOP AT it_data_objects
          ASSIGNING FIELD-SYMBOL(<ls_data>).

          ASSIGN <ls_data>-data_object->* TO FIELD-SYMBOL(<l_data>).

          IF lt_all_abap_lines[] IS NOT INITIAL.

            APPEND INITIAL LINE TO lt_all_abap_lines
              ASSIGNING FIELD-SYMBOL(<ls_line>).

            APPEND INITIAL LINE TO lt_all_abap_lines
              ASSIGNING <ls_line>.

          ENDIF.

          DATA(lr_converter) = NEW lcl_deepstruc_to_abapcode_cvt( ).

          DATA lv_show_empty_fields_ind TYPE abap_bool.

          IF <ls_data>-empty_table_add_line_ind = abap_true.
            lv_show_empty_fields_ind = abap_true.
          ELSE.
            lv_show_empty_fields_ind = p_empty.
          ENDIF.

          DATA(ls_convert_parameters) = VALUE lcl_deepstruc_to_abapcode_cvt=>gts_parameters(
            empty_table_add_line_ind      =  <ls_data>-empty_table_add_line_ind
            show_empty_fields_ind         = lv_show_empty_fields_ind
            comment_show = VALUE #(
              field_text_show_ind           = p_fldtxt
              element_name_show_ind         = p_elemnm
              domain_name_show_ind          = p_domnm
              value_table_name_show_ind     = p_valtab
              conversion_exit_name_show_ind = p_convex
              fixed_value_count_show_ind    = p_fixval
              internal_length_show_ind      = p_intlen
              output_length_show_ind        = p_outlen
              decimal_count_show_ind        = p_declen
              data_type_name_show_ind       = p_dtnm
              internal_data_type_name_ind   = p_indtnm
              initial_value_ind_show_ind    = p_inival
            ) ).

          DATA(lt_abap_lines) = lr_converter->convert(
            is_data             = <l_data>
            is_parameters       = ls_convert_parameters ).

          APPEND LINES OF lt_abap_lines
            TO lt_all_abap_lines.

        ENDLOOP.

        DATA lv_abap_code_text TYPE string.

        LOOP AT lt_all_abap_lines
          ASSIGNING FIELD-SYMBOL(<lv_abap_line>).

          IF sy-tabix > 1.
            lv_abap_code_text =
            lv_abap_code_text && cl_abap_char_utilities=>cr_lf.
          ENDIF.

          lv_abap_code_text =
            lv_abap_code_text && <lv_abap_line>.

        ENDLOOP.

        "Create, show and close Text edit
        gr_text_edit_vw = zui01_text_edit_vw2=>create_in_default_container(
          iv_mvc_object_name = 'ABAP_LINES' ).

*    SET HANDLER on_action
*      FOR gr_text_edit_vw.

        gr_text_edit_vw->set_data_2(
          VALUE #(
            text = lv_abap_code_text ) ).

        gr_text_edit_vw->show(
          iv_focus_ind            = abap_true
*          iv_select_all_lines_ind = abap_true
           ).

        CLEAR gr_text_edit_vw.



      CATCH zcx_return_exc INTO DATA(lx_return).

        zcx_return_exc_vw=>write_messages_to_screen( lx_return ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

DATA gr_controller TYPE REF TO lcl_main_controller.

INITIALIZATION.
  gr_controller = NEW lcl_main_controller( ).

  %_p_empty_%_app_%-text  = 'Empty fields'.

  filter_t = 'Filter'.
  show_t   = 'Show meta data fields'.

  %_p_fldtxt_%_app_%-text  = 'Field description'.
  %_p_elemnm_%_app_%-text  = 'Element name'.
  %_p_domnm_%_app_%-text   = 'Domain name'.
  %_p_valtab_%_app_%-text  = 'Value table name'.
  %_p_convex_%_app_%-text  = 'Conversion exit name'.
  %_p_fixval_%_app_%-text  = 'Fixed value count'.
  %_p_intlen_%_app_%-text  = 'Internal length'.
  %_p_outlen_%_app_%-text  = 'Output length'.
  %_p_declen_%_app_%-text  = 'Decimal count'.
  %_p_dtnm_%_app_%-text    = 'Data type name'.
  %_p_indtnm_%_app_%-text  = 'Internal data type name'.
  %_p_inival_%_app_%-text  = 'Intial value indicator'.

START-OF-SELECTION.

  "*********************************************************
  "SAP blog
  "URL: <TODO>
  "*********************************************************

  "*********************************************************
  "Expected return data example - Method return structure
  "*********************************************************

  DATA(lr_element_descr) =
    CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'BUKRS' ) ).

  DATA(ls_bukrs_ddic_field) = lr_element_descr->get_ddic_field( ).

*  "*********************************************************
*  "Execution data example - Deep structure
*  "- It fills tables with initial one empty record and
*  "  selection screen parameter p_empty is ingnored.
*  "*********************************************************
*
*  TYPES:
*    BEGIN OF gts_sales_order,
*      order_header_in TYPE bapisdhead,
*      order_items_in  TYPE STANDARD TABLE OF bapiitemin WITH DEFAULT KEY,
*    END OF gts_sales_order.
*
*  DATA ls_sales_order TYPE gts_sales_order.

  "*********************************************************
  "Convert Deep structure data to ABAP data code
  "*********************************************************

  gr_controller->show_abap_data_lines(
    it_data_objects = VALUE #(

      ( data_object              = REF #( ls_bukrs_ddic_field )
        empty_table_add_line_ind = abap_false )

*      ( data_object              = REF #( ls_sales_order )
*        empty_table_add_line_ind = abap_true )

    ) ).
