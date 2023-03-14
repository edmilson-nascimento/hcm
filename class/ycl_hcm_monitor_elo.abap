CLASS ycl_hcm_monitor_elo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="pt">Construtor</p>
    METHODS constructor
      IMPORTING
        !is_filter TYPE yhcm00010 .
    "! <p class="shorttext synchronized" lang="pt">Preparar dados da Tela Inicial/Select Screen</p>
    CLASS-METHODS prepare_initial_ss
      EXPORTING
        !ex_bukrs TYPE t001-bukrs
        !et_date  TYPE re_t_rsoderf  .
    METHODS get_data .
    METHODS import_data .
    METHODS display_data .
    METHODS check_allow_process
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS process_data .
    METHODS has_valid_data
      RETURNING
        VALUE(result) TYPE abap_bool .

  PROTECTED SECTION.

    METHODS on_link_click
      FOR EVENT link_click OF
                cl_salv_events_table
      IMPORTING row column .

    METHODS on_user_command
      FOR EVENT added_function OF
                cl_salv_events
      IMPORTING e_salv_function.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_icon_ref,
        status TYPE ythcm0001-status,
        icon   TYPE icon-id,
      END OF ty_icon_ref,
      tab_icon_ref TYPE SORTED TABLE OF ty_icon_ref
                   WITH UNIQUE KEY status .

    DATA:
      filter        TYPE yhcm00010,
      icon_ref      TYPE tab_icon_ref,
      imported_data TYPE STANDARD TABLE OF ythcm0001,
      output        TYPE yhcm00009_t,

      go_salv_table TYPE REF TO cl_salv_table.


    METHODS get_empresa_elo
      IMPORTING
        empresa       TYPE yhcm00010-empresa
      RETURNING
        VALUE(result) TYPE yhcm00010-empresa_elo .
    METHODS set_icon_ref .
    METHODS search_data .
    METHODS organiza_data .
    METHODS get_status_text
      IMPORTING
        !data         TYPE yhcm00009-status
      RETURNING
        VALUE(result) TYPE yhcm00009-status_text .
    METHODS get_pernr_text
      IMPORTING
        !data         TYPE yhcm00009-pernr
      RETURNING
        VALUE(result) TYPE yhcm00009-ename .
    METHODS get_type_text
      IMPORTING
        !data         TYPE yhcm00009-type
      RETURNING
        VALUE(result) TYPE yhcm00009-type_text .
    METHODS prepare_layout .
    METHODS prepare_link_click .
    METHODS confirm_event
      RETURNING VALUE(result) TYPE abap_bool .
    METHODS get_selected_line
      RETURNING VALUE(result) TYPE yhcm00009 .

ENDCLASS.



CLASS ycl_hcm_monitor_elo IMPLEMENTATION.


  METHOD constructor .

    IF ( is_filter IS INITIAL ) .
      RETURN .
    ENDIF .

    me->filter = CORRESPONDING #( is_filter ) .
    me->filter-empresa_elo = me->get_empresa_elo( is_filter-empresa ) .

    me->set_icon_ref( ).

  ENDMETHOD .


  METHOD prepare_initial_ss .

    CONSTANTS:
      lc_empresa TYPE t001-bukrs VALUE 'SD00' .

    DATA:
      first_day_of_month TYPE d,
      last_day_of_month  TYPE d.

    CLEAR:
      ex_bukrs, et_date .

    ex_bukrs = lc_empresa .

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = sy-datum
      IMPORTING
        last_day_of_month = last_day_of_month
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    first_day_of_month = |{ sy-datum(6) }01| .

    et_date = VALUE #( (
      sign   = rsmds_c_sign-including
      option = rsmds_c_option-between
      low    = first_day_of_month
      high   = last_day_of_month ) ) .

  ENDMETHOD .


  METHOD get_data .

    " Verificar excessoes

    me->search_data( ) .
    me->organiza_data( ).

  ENDMETHOD .


  METHOD import_data .
  ENDMETHOD.


  METHOD display_data .

    DATA:
      r_container TYPE REF TO cl_gui_container .


    IF ( lines( me->output ) EQ 0 ) .
      RETURN .
    ENDIF .

    TRY .
        cl_salv_table=>factory(
*        EXPORTING
*           list_display = if_salv_c_bool_sap=>true
*            container_name = 'CONTAINER'
*            r_container    = r_container
          IMPORTING
            r_salv_table = me->go_salv_table
          CHANGING
            t_table      = me->output ) .
      CATCH cx_salv_msg .
        RETURN .
    ENDTRY.

    IF ( me->go_salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(events) = me->go_salv_table->get_event( ).
    IF ( events IS NOT BOUND ) .
      RETURN .
    ENDIF .
    SET HANDLER me->on_user_command FOR events .

    go_salv_table->set_screen_status(
      pfstatus      = 'STANDARD_FULLSCREEN'
      report        = 'YHCM0012'
      set_functions = go_salv_table->c_functions_all ) .

    DATA(lo_display) = go_salv_table->get_display_settings( ) .
    IF ( lo_display IS BOUND ) .
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .
    ENDIF .

    me->prepare_layout( ) .

    me->prepare_link_click( ) .

    go_salv_table->display( ).

  ENDMETHOD .


  METHOD check_allow_process.
  ENDMETHOD.


  METHOD process_data .


    DATA:
      class TYPE char10,
      meth  TYPE char10,
      data  TYPE p2001.

    " Recupera a linha para processamento
    DATA(line) = me->get_selected_line( ) .
    IF ( line IS INITIAL ) .
      RETURN .
    ENDIF .

    " Verificar se existe tipo de registro para processamento
*    if ( line-type is initial ) .
*    return .
*    raise EXCEPTION zcx_excelption.
*    endif .
    " Processar registro

    DATA(log) = NEW ycl_hcm_elo_maintenance(
    )->yif_hcm_elo_data_maintenance~maintains_infotype_2001( data = data ) .

*TRY.
*    CALL METHOD (class)=>(meth)
*      PARAMETER-TABLE
*        ptab
*      EXCEPTION-TABLE
*        etab.
*    CASE sy-subrc.
*      WHEN 1.
*        ...
*      ...
*    ENDCASE.
*  CATCH cx_sy_dyn_call_error INTO exc_ref.
*    MESSAGE exc_ref->get_text( ) TYPE 'I'.
*ENDTRY.

    " Salvar mensagem de log/processamento
    " Exibir mensagem de processamento

  ENDMETHOD.


  METHOD has_valid_data .

    result = COND #( WHEN lines( me->imported_data ) EQ 0
                     THEN abap_off
                     ELSE abap_on ) .

  ENDMETHOD .


  METHOD on_link_click .
  ENDMETHOD .


  METHOD on_user_command .

    CONSTANTS:
      lc_process_seleted TYPE salv_de_function VALUE 'PROCESS'.

    BREAK-POINT .

    CASE e_salv_function .

      WHEN lc_process_seleted .

        IF ( me->confirm_event( ) ) .

          DATA(record) = me->get_selected_line( ) .

          DATA(return) =
            NEW ycl_hcm_elo_maintenance( )->mainten_2001( record ) .

        ENDIF .

      WHEN OTHERS .

    ENDCASE .


  ENDMETHOD .


  METHOD get_empresa_elo .

    DATA:
      lc_begin_key TYPE tvarvc-name VALUE 'ZIF_ELO_EMPRESA_' .

    IF ( empresa IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(key) = |{ lc_begin_key }{ empresa }| .

    SELECT name, type, numb, sign, opti, low
      FROM tvarvc
     WHERE name EQ @key
      INTO @DATA(line) .
    ENDSELECT .

    result = COND #( WHEN sy-subrc EQ 0
                     THEN line-low
                     ELSE '' ) .

  ENDMETHOD .


  METHOD set_icon_ref .


    me->icon_ref = VALUE #(
      ( status = '00'   " Nao integrado
        icon = '@EB@' ) " Semáforo apagado; IndefinidoK

      ( status = '01'   " Integrado
        icon = '@09@' ) " Semáforo verde; avançar; OK

      ( status = '02'   " Nao integrado
        icon = '@0A@' ) " Semáf.verm.; parar; incorreto

      ( status = '03'   " Processado com erro
        icon = '@0A@' ) " Semáf.verm.; parar; incorreto

      ( status = '04'   " Reprocessado com sucesso
        icon = '@08@' ) " Semáforo verde; avançar; OK
    ).

  ENDMETHOD .


  METHOD search_data .

    CLEAR me->imported_data .

    SELECT *
     UP TO 100 ROWS
      FROM ythcm0001
      INTO TABLE @me->imported_data .

  ENDMETHOD .



  METHOD organiza_data .

    CLEAR me->output .

    IF ( lines( me->imported_data ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->output = VALUE yhcm00009_t(
      FOR l IN me->imported_data (
        icon        = VALUE #( me->icon_ref[ status = l-status ]-icon OPTIONAL )
        status      = l-status
        status_text = me->get_status_text( l-status )
        gjahr       = l-gjahr
        id          = l-id
        pernr       = l-pernr
        ename       = me->get_pernr_text( l-pernr )
        type        = l-type
        type_text   = me->get_type_text( l-type )
        credat      = l-credat
        cretim      = l-cretim
        crenam      = l-crenam
        chadat      = l-chadat
        chatim      = l-chatim
        chanam      = l-chanam
        lognumber   = l-lognumber ) ) .


  ENDMETHOD .


  METHOD get_status_text .

    CONSTANTS:
      lc_domain TYPE domname VALUE 'YHCMDD_STATUS' .

    DATA:
      values_tab TYPE dd07vtab .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = lc_domain
      TABLES
        values_tab      = values_tab
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    IF ( sy-subrc EQ 0 ) .
      result = VALUE #( values_tab[ domvalue_l = data ]-ddtext OPTIONAL ) .
    ENDIF.

  ENDMETHOD .


  METHOD get_pernr_text .
  ENDMETHOD .


  METHOD get_type_text .

    CONSTANTS:
      lc_domain TYPE domname VALUE 'YHCMDD_TIPO_INTERFACE_ELO' .

    DATA:
      values_tab TYPE dd07vtab .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = lc_domain
      TABLES
        values_tab      = values_tab
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    IF ( sy-subrc EQ 0 ) .
      result = VALUE #( values_tab[ domvalue_l = data ]-ddtext OPTIONAL ) .
    ENDIF.

  ENDMETHOD .


  METHOD prepare_layout .

    DATA(lo_selections) = me->go_salv_table->get_selections( ).
    IF ( lo_selections IS BOUND ) .
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>cell ) .
    ENDIF .

    DATA(lo_layout) = me->go_salv_table->get_layout( ).
    IF ( lo_layout IS NOT BOUND ) .
      RETURN .
    ENDIF .

    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    lo_layout->set_key(
      VALUE salv_s_layout_key( report = 'YHCM0012'  ) ) .

    " Enable Save Default Layout
    lo_layout->set_default( abap_on ).

    " Recuperando layout da tela
    IF ( me->filter-variant IS INITIAL ) .
      RETURN .
    ENDIF .

    lo_layout->set_initial_layout( value = me->filter-variant ).

  ENDMETHOD .


  METHOD prepare_link_click .

    DATA:
      lo_columns TYPE REF TO cl_salv_columns,
      lo_column  TYPE REF TO cl_salv_column_table.

    IF ( go_salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    lo_columns ?= go_salv_table->get_columns( ) .
    IF ( lo_columns IS NOT BOUND ) .
      RETURN .
    ENDIF .

    " Otimizar largura da columa
    lo_columns->set_optimize( cl_salv_display_settings=>true ) .

    " hotpost de Ordem
    TRY .
        lo_column ?= lo_columns->get_column( 'LOGNUMBER' ) .
      CATCH cx_salv_not_found.
        RETURN .
    ENDTRY .
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    DATA(lo_events) = go_salv_table->get_event( ) .
    IF ( lo_events IS NOT BOUND ) .
      RETURN .
    ENDIF .

    SET HANDLER me->on_link_click FOR lo_events.

  ENDMETHOD .


  METHOD confirm_event .

    CONSTANTS:
      lc_yes      TYPE char1 VALUE '1',
      lc_icon_yes TYPE iconname VALUE 'ICON_OKAY',
      lc_icon_no  TYPE iconname VALUE 'ICON_CANCEL'.

    DATA:
      answer TYPE char1 .

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-001 " Confirmar
*       diagnose_object       = space
        text_question         = TEXT-q01 " Deseja processar o item selecionado?
        text_button_1         = TEXT-002 " Sim
        icon_button_1         = lc_icon_yes
        text_button_2         = TEXT-003 " Nao
        icon_button_2         = lc_icon_no
        default_button        = '2'
        display_cancel_button = abap_off
*       userdefined_f1_help   = space
*       start_column          = 25
*       start_row             = 6
*       popup_type            =
*       iv_quickinfo_button_1 = space
*       iv_quickinfo_button_2 = space
      IMPORTING
        answer                = answer.

    result = COND #( WHEN answer EQ lc_yes
                     THEN abap_on
                     ELSE abap_off ).

  ENDMETHOD .


  METHOD get_selected_line .

    IF ( me->go_salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(selections) = me->go_salv_table->get_selections( ) .
    IF ( selections IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(records) = selections->get_selected_rows( ) .
    DATA(record) = VALUE #( records[ 1 ] OPTIONAL ) .

    result = VALUE #( me->output[ record ] OPTIONAL ) .

  ENDMETHOD .


ENDCLASS.
