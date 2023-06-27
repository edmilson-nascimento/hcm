CLASS ycl_hcm_elo_monitor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      lc_display TYPE yhcm00010-tipo VALUE '1',
      lc_import  TYPE yhcm00010-tipo VALUE '2',
      lc_export  TYPE yhcm00010-tipo VALUE '3'.

    "! <p class="shorttext synchronized" lang="pt">Constructor method</p>
    METHODS constructor
      IMPORTING
        !is_filter TYPE yhcm00010 .
    "! <p class="shorttext synchronized" lang="pt">Preparar dados da Tela Inicial/Select Screen</p>
    CLASS-METHODS prepare_initial_ss
      EXPORTING
        !ex_bukrs TYPE t001-bukrs
        !et_date  TYPE re_t_rsoderf
        !ex_dtini TYPE d
        !ex_dtfin TYPE d.
    "! <p class="shorttext synchronized" lang="pt">Ocultar campo da tela inicial</p>
    CLASS-METHODS hide_field
      IMPORTING
        !im_field TYPE screen-name .
    "! <p class="shorttext synchronized" lang="pt">Manter (exibe/oculta) campos conforme o criterio de selecao</p>
    CLASS-METHODS handle_ss_options
      EXPORTING
        !display TYPE check
        !import  TYPE check
        !export  TYPE check .
    "! <p class="shorttext synchronized" lang="pt">Preparar dados de filtro de infotypes/Select Screen</p>
    CLASS-METHODS prepare_infotypes_ss
      IMPORTING
        !ausencias       TYPE check
        !presencas       TYPE check
        !contg_ausencias TYPE check
        !remunemp        TYPE check
      RETURNING
        VALUE(result)    TYPE infty_tab .
    "! <p class="shorttext synchronized" lang="pt">Preparar dados de filtro de Nro de Pessoa</p>
    CLASS-METHODS get_pernr
      IMPORTING
        !im_type      TYPE yhcm00010-tipo
        !im_display   TYPE cchry_pernr_range
        !im_export    TYPE cchry_pernr_range
        !im_import    TYPE cchry_pernr_range
      RETURNING
        VALUE(result) TYPE yhcm00010-pernr .

    "! <p class="shorttext synchronized" lang="pt">Retorna informação da empresa (de-para ELO/SAP)</p>
    CLASS-METHODS get_empresa_elo
      IMPORTING
        empresa       TYPE yhcm00010-empresa
      RETURNING
        VALUE(result) TYPE yhcm00010-empresa_elo .

    "! <p class="shorttext synchronized" lang="pt">Buscar dados em banco de dados e prepara-los</p>
    METHODS get_data .
    "! <p class="shorttext synchronized" lang="pt">Importar dados da Interface ELO</p>
    METHODS import_data .
    "! <p class="shorttext synchronized" lang="pt">Exportar dados para Interface ELO</p>
    METHODS export_data .
    "! <p class="shorttext synchronized" lang="pt">Exibir dados em tela (ALV)</p>
    METHODS display_data .
    "! <p class="shorttext synchronized" lang="pt">Verifica se existem dados validados</p>
    METHODS has_valid_data
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="pt">Exibe mensagem de erro</p>
    METHODS display_messages .

  PROTECTED SECTION.
    "! <p class="shorttext synchronized" lang="pt">Evento para LINK_CLICK no link do ALV</p>
    METHODS on_link_click
      FOR EVENT link_click OF
                cl_salv_events_table
      IMPORTING row column .
    "! <p class="shorttext synchronized" lang="pt">Evento para ADDED_FUNCTION no user command do ALV</p>
    METHODS on_user_command
      FOR EVENT added_function OF
                cl_salv_events
      IMPORTING e_salv_function.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_icon_ref,
        data TYPE char3,
        icon TYPE icon-id,
      END OF ty_icon_ref,
      tab_icon_ref TYPE SORTED TABLE OF ty_icon_ref
                   WITH UNIQUE KEY data .

    DATA:
      "! <p class="shorttext synchronized" lang="pt">Filtro da tela de seleção</p>
      filter     TYPE yhcm00010,
      "! <p class="shorttext synchronized" lang="pt">Icones referentes a exib.</p>
      icon_ref   TYPE tab_icon_ref,
      "! <p class="shorttext synchronized" lang="pt">Status de acordo com filtro informado</p>
      status_ref TYPE RANGE OF ythcm0001-status,
      "! <p class="shorttext synchronized" lang="pt">Log de processamento</p>
      log_data   TYPE STANDARD TABLE OF ythcm0002,
      "! <p class="shorttext synchronized" lang="pt">Mensagens de processamento</p>
      messages   TYPE bapiret2_t,
      "! <p class="shorttext synchronized" lang="pt">Tabela de saida do relatorio</p>
      output     TYPE yhcm00009_t,
      salv_table TYPE REF TO cl_salv_table.


    "! <p class="shorttext synchronized" lang="pt">Retorna icone referente ao status do processado atual</p>
    METHODS set_icon_ref .
    "! <p class="shorttext synchronized" lang="pt">Configura dados de erro de acordo com status da interface</p>
    METHODS set_status_ref .
    "! <p class="shorttext synchronized" lang="pt">Busca os dados no BD de acordo com o informado</p>
    METHODS search_data .
    "! <p class="shorttext synchronized" lang="pt">Organiza os dados que foram recuperados do BD</p>
    METHODS organize_data .
    "! <p class="shorttext synchronized" lang="pt">Retorna a descr. da Empresa</p>
    METHODS get_bukrs_text
      IMPORTING
        !data         TYPE yhcm00009-bukrs
      RETURNING
        VALUE(result) TYPE yhcm00009-butxt .
    "! <p class="shorttext synchronized" lang="pt">Retorna a descr. do Status</p>
    METHODS get_status_text
      IMPORTING
        !data         TYPE yhcm00009-status
      RETURNING
        VALUE(result) TYPE yhcm00009-status_text .
    "! <p class="shorttext synchronized" lang="pt">Retorna o nome da Pessoa</p>
    METHODS get_pernr_text
      IMPORTING
        !data         TYPE yhcm00009-pernr
      RETURNING
        VALUE(result) TYPE yhcm00009-ename .
    "! <p class="shorttext synchronized" lang="pt">Retorna a descr. da Direcao da Interface</p>
    METHODS get_direct_text
      IMPORTING
        !data         TYPE yhcm00009-direct
      RETURNING
        VALUE(result) TYPE yhcm00009-direct_text.
    "! <p class="shorttext synchronized" lang="pt">Retorna a descr. do Tipo de registro</p>
    METHODS get_type_text
      IMPORTING
        !data         TYPE yhcm00009-type
      RETURNING
        VALUE(result) TYPE yhcm00009-type_text .
    "! <p class="shorttext synchronized" lang="pt">Retorna o desc. do domio de acordo com o dados passados</p>
    METHODS get_domain_text
      IMPORTING
        !domain       TYPE domname
        !data         TYPE dd07v-domvalue_l
      RETURNING
        VALUE(result) TYPE dd07v-ddtext  .
    "! <p class="shorttext synchronized" lang="pt">Mantem rotinas de Layout do ALV report</p>
    METHODS prepare_fields .
    "! <p class="shorttext synchronized" lang="pt">Exibe/Oculta campos para layout padrao</p>
    METHODS prepare_layout .
    "! <p class="shorttext synchronized" lang="pt">Mantem rotinas de link/hotspost do ALV report</p>
    METHODS prepare_link_click .
    "! <p class="shorttext synchronized" lang="pt">Questionar confirmacao do processamento de registros</p>
    METHODS confirm_event
      RETURNING VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="pt">Seleconar linha marcada para processamento no ALV</p>
    METHODS get_selected_line
      RETURNING VALUE(result) TYPE yhcm00009 .

    "! <p class="shorttext synchronized" lang="pt">Seleconar linha marcada para processamento no ALV</p>
    METHODS prepare_records_selected
      IMPORTING
        !im_record  TYPE yhcm00009
      EXPORTING
        ex_header   TYPE ythcm0001_t
        ex_imported TYPE ythcm0003_t .

    "! <p class="shorttext synchronized" lang="pt">Exibir log de processamento completo do registro</p>
    METHODS display_log
      IMPORTING
        !data TYPE yhcm00009 .
    "! <p class="shorttext synchronized" lang="pt">Exibir detalhes atravez da tcode PA20</p>
    METHODS display_pa20
      IMPORTING
        !data TYPE yhcm00009 .
    "! <p class="shorttext synchronized" lang="pt">Exibir mensagem em tela apos o fim do processamento de reg.</p>
    METHODS display_message
      IMPORTING
        !data TYPE bapireturn1 .
    "! <p class="shorttext synchronized" lang="pt">Informa mensagens de log de Importação de dados</p>
    METHODS set_export_message
      IMPORTING
        !data TYPE p0001-pernr
      CHANGING
        !log  TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Processa os dados que foram exportados</p>
    METHODS set_import_message .

ENDCLASS.



CLASS ycl_hcm_elo_monitor IMPLEMENTATION.


  METHOD confirm_event .

    CONSTANTS:
      lc_yes      TYPE char1    VALUE '1',
      lc_icon_yes TYPE iconname VALUE 'ICON_OKAY',
      lc_icon_no  TYPE iconname VALUE 'ICON_CANCEL'.

    DATA:
      answer TYPE char1 .

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-001 " Confirmar
        text_question         = TEXT-q01 " Deseja processar o item selecionado?
        text_button_1         = TEXT-002 " Sim
        icon_button_1         = lc_icon_yes
        text_button_2         = TEXT-003 " Nao
        icon_button_2         = lc_icon_no
        default_button        = '2'
        display_cancel_button = abap_off
      IMPORTING
        answer                = answer.

    result = COND #( WHEN answer EQ lc_yes
                     THEN abap_on
                     ELSE abap_off ).

  ENDMETHOD .


  METHOD constructor .

    IF ( is_filter IS INITIAL ) .
      RETURN .
    ENDIF .

    me->filter = CORRESPONDING #( is_filter ) .
    me->filter-empresa_elo = me->get_empresa_elo( is_filter-empresa ) .

    me->set_icon_ref( ) .
    me->set_status_ref( ) .

  ENDMETHOD .


  METHOD display_data .

    IF ( lines( me->output ) EQ 0 ) .
      RETURN .
    ENDIF .

    TRY .
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->salv_table
          CHANGING
            t_table      = me->output ) .
      CATCH cx_salv_msg .
        RETURN .
    ENDTRY.

    IF ( me->salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(events) = me->salv_table->get_event( ).
    IF ( events IS NOT BOUND ) .
      RETURN .
    ENDIF .

    SET HANDLER me->on_user_command FOR events .
    SET HANDLER me->on_link_click FOR events .

    salv_table->set_screen_status(
      pfstatus      = 'STANDARD_FULLSCREEN'
      report        = 'YHCM0012'
      set_functions = salv_table->c_functions_all
    ) .

    DATA(lo_display) = salv_table->get_display_settings( ) .
    IF ( lo_display IS BOUND ) .
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .
    ENDIF .

    me->prepare_fields( ) .
    me->prepare_layout( ) .
    me->prepare_link_click( ) .
    salv_table->display( ).

  ENDMETHOD .


  METHOD display_log .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( NOT line_exists( me->log_data[ gjahr = data-gjahr
                                        id    = data-id ] ) ) .
      RETURN .
    ENDIF .

    DATA(log) = VALUE bal_t_logn(
      FOR l IN me->log_data
      WHERE ( gjahr EQ data-gjahr AND
              id    EQ data-id )
      ( l-lognumber ) ).

    ycl_hcm_application_log=>show_saved( log ) .

  ENDMETHOD .


  METHOD display_message .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      EXPORTING
        i_msgid = data-id
        i_msgty = data-type
        i_msgno = data-number
        i_msgv1 = data-message_v1
        i_msgv2 = data-message_v2
        i_msgv3 = data-message_v3
        i_msgv4 = data-message_v4.

  ENDMETHOD .


  METHOD display_messages .

    IF ( lines( me->messages ) EQ 0 )  .
      RETURN .
    ENDIF .

    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = me->messages.

  ENDMETHOD .


  METHOD display_pa20 .

    CONSTANTS
      lc_display TYPE hrbc_pernr-activity VALUE 'DIS' .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(lt_bdcdata) = VALUE bdcdata_tab(
      ( program  = 'SAPMP50A'
        dynpro   = '1000'
        dynbegin = 'X' )

      ( fnam     = 'BDC_OKCODE'
        fval     = '=LIST' )

      ( fnam     = 'RP50G-PERNR'
        fval     = data-pernr )

      ( fnam     = 'RP50G-TIMR6'
        fval     = 'X' )

      ( fnam     = 'RP50G-BEGDA'
        fval     =  |0101{ data-gjahr }| )

      ( fnam     = 'RP50G-ENDDA'
        fval     = |3112{ data-gjahr }| )

      ( fnam     = 'RP50G-CHOIC'
        fval     = |{ data-type }| )
    ).

    CALL TRANSACTION 'PA20' USING lt_bdcdata MODE 'E'.

  ENDMETHOD .


  METHOD export_data .

    DATA:
      title TYPE balnrext VALUE 'ELO Monitor' .

    IF ( me->filter IS INITIAL ) .
      RETURN .
    ENDIF .

    LOOP AT me->filter-pernr INTO DATA(employee) .

      DATA(log_single) = ycl_hcm_elo_exportation=>regist_employee( employee ) .
      me->set_export_message( EXPORTING data = employee
                              CHANGING  log  = log_single ) .

      me->messages = VALUE #( BASE me->messages ( LINES OF log_single ) ) .

    ENDLOOP .


    DATA(log) =
      VALUE ycl_hcm_application_log=>ty_header_log(
        title     = title
        object    = ycl_hcm_application_log=>ty_log-object
        subobject = ycl_hcm_application_log=>ty_log-subobject
        alprog    = ycl_hcm_application_log=>ty_log-alprog
    ) .

    DATA(log_processed) =
      ycl_hcm_application_log=>save_bapiret2_tab(
        EXPORTING
          is_header  = log
          it_message = me->messages
      ) .

    me->messages =
      ycl_hcm_application_log=>get_log_bapiret2(
        EXPORTING
          is_log_number = log_processed
      ) .


  ENDMETHOD .


  METHOD get_bukrs_text .

    DATA:
      detail TYPE bapi0002_2 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
      EXPORTING
        companycodeid      = data
      IMPORTING
        companycode_detail = detail.

    result = detail-comp_name .

  ENDMETHOD.


  METHOD get_data .

    IF ( me->filter IS INITIAL ) .
      RETURN .
    ENDIF .

    me->search_data( ) .
    me->organize_data( ).

    IF ( lines( me->output ) EQ 0 ) .
      me->messages = VALUE #(
        ( type       = if_xo_const_message=>error
          id         = ycl_hcm_application_log=>message-id
          number     = '010' ) " Dados nao encontrados
      ) .

    ENDIF .

  ENDMETHOD .


  METHOD get_direct_text .

    CONSTANTS:
      lc_domain TYPE domname VALUE 'YHCMDD_DIRECT_INTERFACE_ELO' .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->get_domain_text( domain = lc_domain
                           data   = CONV #( data ) ) .

  ENDMETHOD .


  METHOD get_domain_text .

    DATA:
      values_tab TYPE dd07vtab .

    IF ( domain IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = domain
      TABLES
        values_tab      = values_tab
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    IF ( sy-subrc EQ 0 ) .
      result = VALUE #( values_tab[ domvalue_l = data ]-ddtext OPTIONAL ) .
    ENDIF.

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


  METHOD get_pernr_text .

    CONSTANTS:
      infty TYPE infty VALUE '0002' .

    DATA:
      infty_tab TYPE TABLE OF p0002 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = data
        infty           = infty
      TABLES
        infty_tab       = infty_tab
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.

    IF ( sy-subrc EQ 0 ) .
      result = VALUE #( infty_tab[ 1 ]-cname OPTIONAL ) .
    ENDIF.

  ENDMETHOD .


  METHOD get_selected_line .

    IF ( me->salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(selections) = me->salv_table->get_selections( ) .
    IF ( selections IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(rows) = selections->get_selected_rows( ) .
    IF ( lines( rows ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(record) = VALUE #( rows[ 1 ] OPTIONAL ) .
    IF ( record EQ 0 ) .
      RETURN .
    ENDIF .

    result = VALUE #( me->output[ record ] OPTIONAL ) .

  ENDMETHOD .


  METHOD get_status_text .

    CONSTANTS:
      lc_domain TYPE domname VALUE 'YHCMDD_STATUS' .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->get_domain_text( domain = lc_domain
                           data   = CONV #( data ) ) .

  ENDMETHOD .


  METHOD get_type_text .

    CONSTANTS:
      lc_domain TYPE domname VALUE 'YHCMDD_TIPO_INTERFACE_ELO' .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->get_domain_text( domain = lc_domain
                           data   = CONV #( data ) ) .

  ENDMETHOD .


  METHOD handle_ss_options .

    CONSTANTS:
      lc_display_group TYPE screen-group1 VALUE 'DIS',
      lc_import_group  TYPE screen-group1 VALUE 'IMP',
      lc_export_group  TYPE screen-group1 VALUE 'EXP'.

    LOOP AT SCREEN.

      IF ( display EQ abap_false ) .

        IF ( screen-group1 EQ lc_display_group ) .
          screen-active = '0'.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

      ENDIF .

      IF ( import EQ abap_false ) .

        IF ( screen-group1 EQ lc_import_group ) .
          screen-active = '0'.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

      ENDIF .

      IF ( export EQ abap_false ) .

        IF ( screen-group1 EQ lc_export_group ) .
          screen-active = '0'.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

      ENDIF .

    ENDLOOP.

*   hide_field( 'P_EXPORT' ) .

  ENDMETHOD .


  METHOD has_valid_data .

    result = COND #( WHEN lines( me->output ) EQ 0
                     THEN abap_off
                     ELSE abap_on ) .
  ENDMETHOD .


  METHOD import_data .

    DATA:
      title TYPE balnrext VALUE 'ELO Monitor' .

    IF ( me->filter IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( lines( me->filter-infotipos ) EQ 0 ) .
      " Não foi informado nenhum infotype para importação de dados.
      IF ( 0 EQ 1 ). MESSAGE i015(zhcm) . ENDIF .
      me->messages = VALUE #(
        ( type       = if_xo_const_message=>error
          id         = ycl_hcm_application_log=>message
          number     = 0015 )
      ) .
      RETURN .
    ENDIF .

    me->messages =
      NEW ycl_hcm_elo_importation( me->filter )->export_salary_time( ) .

*   me->set_import_message( ) .

    DATA(log) =
      VALUE ycl_hcm_application_log=>ty_header_log(
        title     = title
        object    = ycl_hcm_application_log=>ty_log-object
        subobject = ycl_hcm_application_log=>ty_log-subobject
        alprog    = ycl_hcm_application_log=>ty_log-alprog
    ) .

    DATA(log_processed) =
      ycl_hcm_application_log=>save_bapiret2_tab(
        EXPORTING
          is_header  = log
          it_message = me->messages
      ) .

    me->messages =
      ycl_hcm_application_log=>get_log_bapiret2(
        EXPORTING
          is_log_number = log_processed
      ) .

  ENDMETHOD.


  METHOD on_link_click .

    DATA:
      log_process     TYPE salv_de_column VALUE 'LOGNUMBER',
      personal_number TYPE salv_de_column VALUE 'PERNR'.

    DATA(line) = VALUE #( me->output[ row ] OPTIONAL ) .
    IF ( line IS INITIAL ) .
      RETURN.
    ENDIF .

    CASE column .
      WHEN log_process .
        me->display_log( line ) .
      WHEN personal_number .
        me->display_pa20( line ) .
      WHEN OTHERS .
    ENDCASE .

  ENDMETHOD .


  METHOD on_user_command .

    CONSTANTS:
      lc_process_seleted TYPE salv_de_function VALUE 'PROCESS',
      lc_refresh_seleted TYPE salv_de_function VALUE 'REFRESH'.

    DATA:
      temp_header   TYPE ythcm0001_t,
      temp_imported TYPE ythcm0003_t,
      log           TYPE bapiret2_t.

    CASE e_salv_function .

      WHEN lc_process_seleted .

        IF ( me->confirm_event( ) EQ abap_false ) .
          RETURN .
        ENDIF .

        DATA(record) = me->get_selected_line( ) .
        IF ( record IS INITIAL ) .
          RETURN .
        ENDIF .

        me->prepare_records_selected(
          EXPORTING
            im_record   = record
          IMPORTING
            ex_header   = temp_header
            ex_imported = temp_imported
        ) .

        CASE record-type .

          WHEN ycl_hcm_elo_maintenance=>infotypes-ausencias .
            log =
              ycl_hcm_elo_maintenance=>mainten_2001_imported(
                EXPORTING
                  header       = temp_header
                  impored_type = temp_imported
                  no_commit    = abap_off
              ).

          WHEN ycl_hcm_elo_maintenance=>infotypes-presencas .
            log =
              ycl_hcm_elo_maintenance=>mainten_2002_imported(
                EXPORTING
                  header       = temp_header
                  imported_type = temp_imported
                  no_commit    = abap_off
              ).

          WHEN ycl_hcm_elo_maintenance=>infotypes-infos_remun_emp .

            " Verifica colisão
            ycl_hcm_elo_maintenance=>check_2010_imported(
              CHANGING ch_header        = temp_header
                       ch_imported_type = temp_imported
                       ch_log           = log ) .

            IF ( lines( temp_header )   GT 0 ) AND
               ( lines( temp_imported ) GT 0 ) .

              log =
                ycl_hcm_elo_maintenance=>mainten_2010_imported(
                  EXPORTING
                    header        = temp_header
                    imported_type = temp_imported
                    no_commit     = abap_off
                ).

            ENDIF .

            me->messages = log .
            me->display_messages( ).

          WHEN OTHERS .

        ENDCASE .

        " 018 Dados processados. Favor verificar log.
        MESSAGE i018(zhcm) .

        " Atualizar os dados de tela apos processamento
        me->get_data( ).
        me->salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ) .

      WHEN lc_refresh_seleted .

        me->get_data( ).
        me->salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ) .

      WHEN OTHERS .

    ENDCASE .


  ENDMETHOD .


  METHOD organize_data .

    CONSTANTS:
      lc_log TYPE char3 VALUE 'LOG' .

    IF ( lines( me->output ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(temp_data) = me->output .

    me->output = VALUE yhcm00009_t(
      FOR l IN temp_data (
        id          = l-id
        gjahr       = l-gjahr
        icon        = VALUE #( me->icon_ref[ data = l-status ]-icon OPTIONAL )
        bukrs       = l-bukrs
        butxt       = me->get_bukrs_text( l-bukrs )
        pernr       = l-pernr
        ename       = me->get_pernr_text( l-pernr )
        direct      = l-direct
        direct_text = me->get_direct_text( l-direct )
        code_type   = l-code_type
        type        = l-type
        type_text   = me->get_type_text( l-type )
        subtype     = l-subtype
        status      = l-status
        status_text = me->get_status_text( l-status )
        start_date  = l-start_date
        start_time  = l-start_time
        end_date    = l-end_date
        end_time    = l-end_time
        tpsituacao  = l-tpsituacao
        tpsituacao_text = ''
        credat      = l-credat
        cretim      = l-cretim
        crenam      = l-crenam
        chadat      = l-chadat
        chatim      = l-chatim
        chanam      = l-chanam
        lognumber   = VALUE #( me->icon_ref[ data = lc_log ]-icon OPTIONAL ) ) ) .

  ENDMETHOD .


  METHOD prepare_fields .

    DATA:
      salv_columns TYPE REF TO cl_salv_columns,
      salv_column  TYPE REF TO cl_salv_column_table,
      hide_columns TYPE salv_t_column.

    IF ( me->salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    salv_columns ?= salv_table->get_columns( ) .
    IF ( salv_columns IS NOT BOUND ) .
      RETURN .
    ENDIF .

    " Colunas que ficam ocultas inicialmente
    hide_columns = VALUE #(
      ( 'ID' )
      ( 'GJAHR' )
      ( 'BUKRS' )
      ( 'BUTXT' )

      ( 'DIRECT' )
      ( 'DIRECT_TEXT' )

      ( 'CHADAT' )
      ( 'CHATIM' )
      ( 'CHANAM' )

    ).

    LOOP AT hide_columns INTO DATA(column) .
      TRY .
          salv_column ?= salv_columns->get_column( column ) .
        CATCH cx_salv_not_found.
          RETURN .
      ENDTRY .
      salv_column->set_visible( value = if_salv_c_bool_sap=>false ) .
    ENDLOOP .

  ENDMETHOD .


  METHOD prepare_infotypes_ss .

    IF ( ausencias EQ abap_true ) .
      result = VALUE #( ( ycl_hcm_elo_infotypes=>infotypes-ausencias ) ) .
    ENDIF .

    IF ( presencas EQ abap_true ) .
      result = VALUE #( BASE result ( ycl_hcm_elo_infotypes=>infotypes-presencas ) ) .
    ENDIF .

    IF ( contg_ausencias EQ abap_true ) .
      result = VALUE #( BASE result ( ycl_hcm_elo_infotypes=>infotypes-contg_ausencias ) ) .
    ENDIF .

    IF ( remunemp EQ abap_true ) .
      result = VALUE #( BASE result ( ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp ) ) .
    ENDIF .

    SORT result .

  ENDMETHOD .


  METHOD get_pernr .

    DATA:
      lt_pernr TYPE cchry_pernr_range .

    CASE im_type .
      WHEN lc_display .
        lt_pernr = im_display .
      WHEN lc_import .
        lt_pernr = im_import .
      WHEN lc_export .
        lt_pernr = im_export .
      WHEN OTHERS .
    ENDCASE .

    IF ( lines( lt_pernr ) EQ 0 ) .
      RETURN .
    ENDIF .

    SELECT pernr
      FROM pa0003
     WHERE pernr IN @lt_pernr
      INTO TABLE @DATA(list) .
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .

    SORT list ASCENDING BY pernr .

    result = VALUE #(
      FOR l IN list
      ( l-pernr )
    ).

  ENDMETHOD .


  METHOD prepare_initial_ss .

    CONSTANTS:
      lc_empresa TYPE t001-bukrs VALUE 'SD00' .

    DATA:
      first_day_of_month TYPE d,
      last_day_of_month  TYPE d.

    CLEAR:
      ex_bukrs, et_date, ex_dtini, ex_dtfin .

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

    ex_dtini = first_day_of_month .
    ex_dtfin = last_day_of_month .

    DATA(restrict) = VALUE sscr_restrict(
       opt_list_tab = VALUE sscr_opt_list_tab(
                        ( name    = 'OBJECTKEY1'
                          options = VALUE #( eq = abap_on ) )
                        ( name    = 'OBJECTKEY2'
                          options = VALUE #( eq = abap_on ) )
                        ( name    = 'OBJECTKEY3'
                          options = VALUE #( bt = abap_on ) )
       )
       ass_tab      = VALUE sscr_ass_tab(
                      ( kind    = 'S'
                        name    = 'S_PERNR'
                        sg_main = 'I'
                        sg_addy = space
                        op_main = 'OBJECTKEY1' )
                      ( kind    = 'S'
                        name    = 'S_PERIMP'
                        sg_main = 'I'
                        sg_addy = space
                        op_main = 'OBJECTKEY2' )
                      ( kind    = 'S'
                        name    = 'S_DATAR'
                        sg_main = 'I'
                        sg_addy = space
                        op_main = 'OBJECTKEY3' )
      )
    ).

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction = restrict.

*   hide_field( 'P_EXPORT' ) .

  ENDMETHOD .


  METHOD hide_field .

    IF ( im_field IS INITIAL ) .
      RETURN .
    ENDIF .

    LOOP AT SCREEN .
      IF ( screen-name EQ im_field ) .
        screen-input = '0' .
        MODIFY SCREEN .
      ENDIF .
    ENDLOOP.

  ENDMETHOD .


  METHOD prepare_layout .

    DATA:
      salv_columns TYPE REF TO cl_salv_columns,
      salv_column  TYPE REF TO cl_salv_column_table.

    IF ( me->salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    salv_columns ?= salv_table->get_columns( ) .
    IF ( salv_columns IS NOT BOUND ) .
      RETURN .
    ENDIF .

    " Otimizar largura da columa
    salv_columns->set_optimize( cl_salv_display_settings=>true ) .

    " Hotpost de Personal Number
    TRY .
        salv_column ?= salv_columns->get_column( 'PERNR' ) .
      CATCH cx_salv_not_found.
        RETURN .
    ENDTRY .
    salv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    salv_column->set_alignment( if_salv_c_alignment=>centered ) .



    DATA(selections) = me->salv_table->get_selections( ).
    IF ( selections IS BOUND ) .
*     selections->set_selection_mode( if_salv_c_selection_mode=>cell ) .
      selections->set_selection_mode( if_salv_c_selection_mode=>single ).
    ENDIF .

    DATA(layout) = me->salv_table->get_layout( ).
    IF ( layout IS NOT BOUND ) .
      RETURN .
    ENDIF .

    layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    layout->set_key( VALUE salv_s_layout_key( report = 'YHCM0012'  ) ) .
    " Enable Save Default Layout
    layout->set_default( abap_on ).

    " Recuperando layout da tela
    IF ( me->filter-variant IS INITIAL ) .
      RETURN .
    ENDIF .
    layout->set_initial_layout( value = me->filter-variant ).

  ENDMETHOD .


  METHOD prepare_link_click .

    DATA:
      salv_columns TYPE REF TO cl_salv_columns,
      salv_column  TYPE REF TO cl_salv_column_table.

    IF ( me->salv_table IS NOT BOUND ) .
      RETURN .
    ENDIF .

    salv_columns ?= me->salv_table->get_columns( ) .
    IF ( salv_columns IS NOT BOUND ) .
      RETURN .
    ENDIF .

    " Otimizar largura da columa
    salv_columns->set_optimize( cl_salv_display_settings=>true ) .

    " Hotpost de Personal Number
    TRY .
        salv_column ?= salv_columns->get_column( 'PERNR' ) .
      CATCH cx_salv_not_found.
        RETURN .
    ENDTRY .
    salv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    salv_column->set_alignment( if_salv_c_alignment=>centered ) .

    " Hotpost de Log de processamento
    TRY .
        salv_column ?= salv_columns->get_column( 'LOGNUMBER' ) .
      CATCH cx_salv_not_found.
        RETURN .
    ENDTRY .
    salv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    salv_column->set_alignment( if_salv_c_alignment=>centered ) .

    DATA(lo_events) = salv_table->get_event( ) .
    IF ( lo_events IS NOT BOUND ) .
      RETURN .
    ENDIF .

    SET HANDLER me->on_link_click FOR lo_events.

  ENDMETHOD .


  METHOD prepare_records_selected .

    CLEAR:
      ex_header, ex_imported .

    IF ( im_record IS INITIAL ) .
      RETURN .
    ENDIF .

    SELECT *
      FROM ythcm0001
     WHERE gjahr EQ @im_record-gjahr
       AND id    EQ @im_record-id
      INTO TABLE @ex_header .

    SELECT *
      FROM ythcm0003
     WHERE gjahr EQ @im_record-gjahr
       AND id    EQ @im_record-id
      INTO TABLE @ex_imported .

  ENDMETHOD .


  METHOD search_data .

    CLEAR:
      me->output, me->log_data .

    IF ( me->filter IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( lines( me->filter-data_process ) GT 0 ) .

      SELECT h~id, h~gjahr, h~bukrs, h~pernr,
             h~direct, h~type, h~subtype, h~status,

             i~code_type,
             i~start_date, i~start_time,
             i~end_date, i~end_time, i~tpsituacao,

             h~credat, h~cretim, h~crenam
*            h~chadat, h~chatim, h~chanam
        FROM ythcm0001      AS h " Header
       INNER JOIN ythcm0003 AS i " Import
          ON h~id    EQ i~id
         AND h~gjahr EQ i~gjahr
       WHERE h~bukrs EQ @filter-empresa
*        AND ( ( i~credat IN @me->filter-data_process ) OR
*              ( i~chadat IN @me->filter-data_process ) )
         AND i~credat IN @me->filter-data_process
         AND h~status IN @me->status_ref
        INTO CORRESPONDING FIELDS OF TABLE @me->output .

    ENDIF .

    IF ( lines( me->filter-data_info ) GT 0 ) .

      SELECT h~id, h~gjahr, h~bukrs, h~pernr,
             h~direct, h~type, h~subtype, h~status,

             i~code_type,
             i~start_date, i~start_time,
             i~end_date, i~end_time, i~tpsituacao,

             h~credat, h~cretim, h~crenam
*            h~chadat, h~chatim, h~chanam
        FROM ythcm0001      AS h " Header
       INNER JOIN ythcm0003 AS i " Import
          ON h~id    EQ i~id
         AND h~gjahr EQ i~gjahr
       WHERE h~bukrs EQ @filter-empresa
         AND ( ( i~start_date  IN @me->filter-data_info ) OR
               ( i~end_date    IN @me->filter-data_info ) )
         AND h~status IN @me->status_ref
        INTO CORRESPONDING FIELDS OF TABLE @me->output .

    ENDIF .

    IF ( lines( me->output ) GT 0 ) .

      " Caso haja algum filtro de Nro Pessoa
      IF ( lines( me->filter-pernr ) GT 0 ) .

        DATA(range_filter) = VALUE cchry_pernr_range(
          FOR l IN me->filter-pernr
          ( sign   = if_fsbp_const_range=>sign_include
            option = if_fsbp_const_range=>option_equal
            low    = l ) ) .

        DELETE me->output WHERE pernr NOT IN range_filter .

      ENDIF .

      SELECT *
        FROM ythcm0002
         FOR ALL ENTRIES IN @me->output
       WHERE id    EQ @me->output-id
         AND gjahr EQ @me->output-gjahr
        INTO TABLE @me->log_data .

    ENDIF .

  ENDMETHOD .


  METHOD set_export_message .


    CONSTANTS:
      BEGIN OF message,
        id        TYPE bapiret2-id VALUE 'ZHCM',
        interface TYPE bapiret2-number VALUE '012', " Interface '&'.
        success   TYPE bapiret2-number VALUE '013', " Registro do funcionario & exportado com sucesso.
        error     TYPE bapiret2-number VALUE '014', " Erro ao exportar registro do funcionario &.
      END OF message .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(message_log) = log .

    IF ( 0 EQ 1 ). MESSAGE i012(zhcm) . ENDIF.
    log = VALUE bapiret2_t(
      ( type       = if_xo_const_message=>info
        id         = message-id
        number     = message-interface
        message_v1 = ycl_hcm_elo_exportation=>interfaces-set_regist_employee )
    ) .

    IF ( line_exists( message_log[ type = if_xo_const_message=>error ] ) ) . " Retornou erro?

      IF ( 0 EQ 1 ). MESSAGE e014(zhcm) . ENDIF.
      log = VALUE bapiret2_t( BASE log
        ( type       = if_xo_const_message=>error
          id         = message-id
          number     = message-error
          message_v1 = data )
      ).

    ELSE .

      IF ( 0 EQ 1 ). MESSAGE s013(zhcm) . ENDIF.
      log = VALUE bapiret2_t( BASE log
        ( type       = if_xo_const_message=>success
          id         = message-id
          number     = message-success
          message_v1 = data )
      ).

    ENDIF .

    log = VALUE #( BASE log ( LINES OF message_log ) ) .

  ENDMETHOD .


  METHOD set_icon_ref .

    me->icon_ref = VALUE #(
      ( data = '00'     " Nao integrado
        icon = '@EB@' ) " Semáforo apagado; IndefinidoK
      ( data = '01'     " Integrado
        icon = '@08@' ) " Semáforo verde; avançar; OK
      ( data = '02'     " Nao integrado
        icon = '@0A@' ) " Semáf.verm.; parar; incorreto

      ( data = '03'     " Processado com erro
        icon = '@0A@' ) " Semáf.verm.; parar; incorreto

      ( data = '04'     " Reprocessado com sucesso
        icon = '@08@' ) " Semáforo verde; avançar; OK

      ( data = '11'     " Processado com sucesso
        icon = '@08@' ) " Semáforo verde; avançar; OK
      ( data = '12'     " Processado com erro
        icon = '@0A@' ) " Semáf.verm.; parar; incorreto
      ( data = '13'     " Reprocessado com sucesso
        icon = '@08@' ) " Semáforo verde; avançar; OK

      ( data = 'LOG'    " Log
        icon = '@0L@' ) " Exibir nota
    ).

  ENDMETHOD .


  METHOD set_status_ref .

*00  Nao integrado
*01  Integrado com sucesso
*02  Integrado com erro
*11  Processado com sucesso
*12  Processado com erro
*13  Reprocessado com sucesso

    CASE me->filter-message .
      WHEN if_xo_const_message=>error .
        me->status_ref = VALUE #(
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '00' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '02' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '12' )
        ).
      WHEN if_xo_const_message=>success .
        me->status_ref = VALUE #(
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '01' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '11' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '13' )
        ).
      WHEN OTHERS .
        me->status_ref = VALUE #(
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '00' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '01' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '02' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '11' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '12' )
         ( sign   = rsmds_c_sign-including
           option = rsmds_c_option-equal
           low    = '13' )
        ).
    ENDCASE .

    IF ( lines( me->status_ref ) GT 0 ) .
      SORT me->status_ref ASCENDING BY low .
    ENDIF .

  ENDMETHOD .


  METHOD set_import_message .

    CONSTANTS:
      BEGIN OF message,
        id         TYPE bapiret2-id VALUE 'ZHCM',
        number_e_1 TYPE bapiret2-number VALUE '002', " Erro ao importar dados da Interface ELO.
        number_e_2 TYPE bapiret2-number VALUE '003', " Falha ao executar interface &.
        number_e_3 TYPE bapiret2-number VALUE '004', " Excep. & & & & .
        number_s_1 TYPE bapiret2-number VALUE '005', " Dados importados com sucesso da Interface ELO.
        number_s_2 TYPE bapiret2-number VALUE '006', " Interface & executada com sucesso.
        number_s_3 TYPE bapiret2-number VALUE '007', " Interface &. Período & .
      END OF message .

    DATA(periodo) = CONV bapiret2-message_v1(
      |{ me->filter-start_date DATE = USER } ate | &&
      |{ me->filter-end_date DATE = USER }| ).

    DATA(message_log) = me->messages .

    IF ( line_exists( me->messages[ type = if_xo_const_message=>error ] ) ) . " Retornou erro?
      me->messages = VALUE bapiret2_t(
        ( type       = if_xo_const_message=>error
          id         = message-id
          number     = message-number_e_1 )
        ( type       = if_xo_const_message=>error
          id         = message-id
          number     = message-number_e_2
          message_v1 = ycl_hcm_elo_importation=>interfaces-get_salary_time )
      ).
    ELSE .
      me->messages = VALUE bapiret2_t(
        ( type       = if_xo_const_message=>success
          id         = message-id
          number     = message-number_s_2
          message_v1 = ycl_hcm_elo_importation=>interfaces-get_salary_time )
        ( type       = if_xo_const_message=>success
          id         = message-id
          number     = message-number_s_3
          message_v1 = ycl_hcm_elo_importation=>interfaces-get_salary_time
          message_v2 = periodo )
      ).
      " Verificando se foram importados dados
      IF ( NOT line_exists( message_log[ type = if_xo_const_message=>warning ] ) ) . " Retornou Warning?
        APPEND VALUE #(
          type       = if_xo_const_message=>success
          id         = message-id
          number     = message-number_s_1 ) TO me->messages .
      ENDIF .

    ENDIF .

    me->messages = VALUE #( BASE me->messages ( LINES OF message_log ) ) .

  ENDMETHOD .

ENDCLASS.