CLASS ycl_hcm_elo_importation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF interfaces,
        get_salary_time TYPE zwsuser-company_code VALUE 'ExportSalaryTime',
      END OF interfaces .

    "! <p class="shorttext synchronized" lang="pt">Testa se interface esta ativa e com as corretas config</p>
    CLASS-METHODS test_interface
      IMPORTING
        !start        TYPE d DEFAULT '01012022'
        !end          TYPE d DEFAULT '01012024'
      RETURNING
        VALUE(result) TYPE bapiret2-message .

    "! <p class="shorttext synchronized" lang="pt">Inicializa e guarda os dados de filtro</p>
    METHODS constructor
      IMPORTING
        !is_filter TYPE yhcm00010 .

    "! <p class="shorttext synchronized" lang="pt">Importa dados do servico ExportSalaryTime ELO</p>
    METHODS export_salary_time
      RETURNING
        VALUE(result) TYPE bapiret2_t .

    CLASS-METHODS get_group_absence
      RETURNING
        VALUE(result) TYPE ptesa_subtype_selection .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF message,
        "! <p class="shorttext synchronized" lang="pt">Dados importados com sucesso da Interface ELO.</p>
        imported_successfully TYPE bapiret2-number VALUE 005,
        "! <p class="shorttext synchronized" lang="pt">Dados não encontrados para filtro informado.</p>
        not_found             TYPE bapiret2-number VALUE 010,
        "! <p class="shorttext synchronized" lang="pt">Nenhum dado foi importado pela interface.</p>
        no_record             TYPE bapiret2-number VALUE 011,
        "! <p class="shorttext synchronized" lang="pt">Registro do funcionario & inválido.</p>
        invalid_employee      TYPE bapiret2-number VALUE 016,
        "! <p class="shorttext synchronized" lang="pt">Registro & inválido para rubricas/subtypes.</p>
        invalid_subtype       TYPE bapiret2-number VALUE 021,
        "! <p class="shorttext synchronized" lang="pt">& & & &</p>
        general               TYPE bapiret2-number VALUE 000,
        "! <p class="shorttext synchronized" lang="pt">Não existe Tipo de Segmento valido para filtro. Favor ve...</p>
        segment_type          TYPE bapiret2-number VALUE 017,
        "! <p class="shorttext synchronized" lang="pt">Erro ao salvar dados importados da Inteface.</p>
        save_error            TYPE bapiret2-number VALUE 022,
        "! <p class="shorttext synchronized" lang="pt">Dados importados da Inteface salvos com sucesso.</p>
        save_success          TYPE bapiret2-number VALUE 023,
      END OF message,
      company_code TYPE zemployee_filtering_criteria-company_code VALUE 'DESCONTAO'.


    DATA:
      "! <p class="shorttext synchronized" lang="pt">Armazena mensagens de processamento da classe</p>
      messages TYPE bapiret2_t,
      "! <p class="shorttext synchronized" lang="pt">Dados passados pela tela da tcode YHCM0001</p>
      filter   TYPE yhcm00010,
      "! <p class="shorttext synchronized" lang="pt">Header de Importção</p>
      header   TYPE ythcm0001_t,
      "! <p class="shorttext synchronized" lang="pt">Dados importados (e tratados)</p>
      imported TYPE ythcm0003_t.

    "! <p class="shorttext synchronized" lang="pt">Retorna critérios de seleção/dados de filtro da Interface</p>
    METHODS get_filtering_criteria
      RETURNING VALUE(result) TYPE zsalary_export_criteria .
    "! <p class="shorttext synchronized" lang="pt">Retorna critérios de seleção/dados de filtro de Nr Pessoal</p>
    METHODS get_employee_codes
      RETURNING VALUE(result) TYPE zsalary_export_criteria-employee_codes .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de acesso (user,pass and etc)</p>
    METHODS get_user_access
      RETURNING VALUE(result) TYPE zwsuser .
    "! <p class="shorttext synchronized" lang="pt">Retorna objeto Proxy</p>
    METHODS get_proxy
      RETURNING VALUE(result) TYPE REF TO yelo_co_ielo_web_service .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de entrada da interafce</p>
    METHODS get_input
      RETURNING VALUE(result) TYPE zielo_web_service_export_sala1 .
    "! <p class="shorttext synchronized" lang="pt">Retorna Tipos de segmento para filtros</p>
    METHODS get_code_types
      RETURNING VALUE(result) TYPE zarray_of_segment_type  .
    "! <p class="shorttext synchronized" lang="pt">Mapea os dados importados para as tabelas Y no SAP</p>
    METHODS map_salary_time
      CHANGING
        !data TYPE zsalary_time_tab .
    "! <p class="shorttext synchronized" lang="pt">Retorna a chave TEMPORARIA para criação de novo registro</p>
    METHODS get_id_temp
      RETURNING
        VALUE(result) TYPE ythcm0001-id .
    "! <p class="shorttext synchronized" lang="pt">Retorna a chave unida para criação de novo registro</p>
    METHODS get_id
      IMPORTING
        !year         TYPE nryear
      RETURNING
        VALUE(result) TYPE ythcm0001-id .
    "! <p class="shorttext synchronized" lang="pt">Retorna a Data a partir de um valor DateTime</p>
    METHODS get_date_from_datetime
      IMPORTING
        !data         TYPE ythcm0003-end_date_time
      RETURNING
        VALUE(result) TYPE ythcm0003-end_date .
    "! <p class="shorttext synchronized" lang="pt">Retorna a Hora a partir de um valor DateTime</p>
    METHODS get_time_from_datetime
      IMPORTING
        !data         TYPE ythcm0003-end_date_time
      RETURNING
        VALUE(result) TYPE ythcm0003-end_time .
    "! <p class="shorttext synchronized" lang="pt">Retorna Tipo para o Code Type informado</p>
    METHODS get_type
      IMPORTING
        !data         TYPE ythcm0003-code_type
      RETURNING
        VALUE(result) TYPE ythcm0001-type.
    "! <p class="shorttext synchronized" lang="pt">Prepara os dados p/ persistência (ins, upd)</p>
    METHODS prepare_data .
    "! <p class="shorttext synchronized" lang="pt">Prepara os dados agrupando Ausencia de dias</p>
    METHODS group_absence .
    "! <p class="shorttext synchronized" lang="pt">Persiste os dados importados da interface</p>
    METHODS save_imported
      RETURNING
        VALUE(result) TYPE bapiret2_t.
    "! <p class="shorttext synchronized" lang="pt">Processar os dados importados da interface</p>
    METHODS mainten_imported
      RETURNING
        VALUE(result) TYPE bapiret2_t.
    "! <p class="shorttext synchronized" lang="pt">Coleta mensagem de log</p>
    METHODS set_log
      IMPORTING
        !type   TYPE bapiret2-type DEFAULT 'S'
        !number TYPE bapiret2-number
        !m1     TYPE bapiret2-message_v1 OPTIONAL .
    "! <p class="shorttext synchronized" lang="pt">Valida se o funcionário existe no SAP</p>
    METHODS is_valid_employees_code
      CHANGING
        !data TYPE zsalary_time_tab .
    "! <p class="shorttext synchronized" lang="pt">Valida se rubricas/subtypes existem no SAP</p>
    METHODS is_valid_subtypes
      CHANGING
        !data TYPE zsalary_time_tab .
    "! <p class="shorttext synchronized" lang="pt">Valida se o funcionário existe no SAP</p>
    METHODS prepare_by_infotype
      IMPORTING
        !im_infotype TYPE inftypa
        !im_header   TYPE ythcm0001_t
        !im_imported TYPE ythcm0003_t
      EXPORTING
        !ex_header   TYPE ythcm0001_t
        !ex_imported TYPE ythcm0003_t .
    "! <p class="shorttext synchronized" lang="pt">Retorna o Tipo de Situação dos campos adicionais</p>
    METHODS get_tp_situacao
      IMPORTING
        !data         TYPE zsalary_time
      RETURNING
        VALUE(result) TYPE ythcm0003-tpsituacao .
ENDCLASS.



CLASS ycl_hcm_elo_importation IMPLEMENTATION.


  METHOD constructor .

    IF ( is_filter IS INITIAL ) .
      RETURN .
    ENDIF .

    me->filter = is_filter .

  ENDMETHOD .


  METHOD export_salary_time .

    DATA(proxy) = me->get_proxy( ) .
    IF ( proxy IS NOT BOUND ) .
      result = me->messages .
      RETURN .
    ENDIF .

    DATA(input) = me->get_input( ) .
    IF ( input IS INITIAL ) .
      result = me->messages .
      RETURN .
    ENDIF .

    " 1. Importar dados da Inteface
    TRY .
        proxy->export_salary_time( EXPORTING input  = input
                                   IMPORTING output = DATA(output) ) .
      CATCH cx_ai_system_fault INTO DATA(error).
        result = VALUE #( BASE me->messages (
          LINES OF ycl_hcm_application_log=>map_excep_to_bapiret2( error->errortext ) ) ) .
        RETURN .
    ENDTRY .

    " Caso não houveram dados para a data e infotipo requisitados
    IF ( lines( output-salary_time_info-salary_time ) EQ 0 ) .
      IF ( 0 EQ 1 ). MESSAGE i010(zhcm). ENDIF.
      me->set_log( type   = if_xo_const_message=>info
                   number = me->message-not_found ) .
      IF ( 0 EQ 1 ). MESSAGE i011(zhcm). ENDIF.
      me->set_log( type   = if_xo_const_message=>warning
                   number = me->message-no_record ) .
      result = me->messages .
      RETURN .
    ENDIF .

    me->map_salary_time( CHANGING data = output-salary_time_info-salary_time ) .

    me->prepare_data( ) .

    " 2. Salvar dados nas tabela do cliente YTHCM0001 / YTHCM0003
    me->messages = VALUE #( BASE me->messages ( LINES OF me->save_imported( ) ) ) .

    " 3. Mater infotipos salvos nas tabelas
    me->messages = VALUE #( BASE me->messages ( LINES OF me->mainten_imported( ) ) ) .

    result = me->messages .

  ENDMETHOD .


  METHOD get_group_absence .


    result = VALUE ptesa_subtype_selection(
      ( '1000' ) " Férias Anuais
      ( '1001' ) " Casamento
      ( '1005' ) " Baixa Médica
      ( '1006' ) " Licença pré-maternidade
      ( '1007' ) " Licença Maternidade
      ( '1008' ) " Nascimento de filho (Pai)
      ( '1011' ) " Assistência Familiar Dias
      ( '1019' ) " Falta dispensa serviço - D
      ( '1024' ) " Just C/ Remuneração D
      ( '1026' ) " Just S/ Remuneração D
      ( '1030' ) " Provas escolares D
      ( '1032' ) " Obrigaçao Legal/mIlitar D
      ( '1035' ) " Ativ. Sindicais D
      ( '1037' ) " Ativ Culturais D
      ( '1041' ) " Licença sem vencimento
    ) .

    SORT result .

  ENDMETHOD .


  METHOD get_code_types .

    TYPES:
      BEGIN OF ty_types,
        infty TYPE infty,
        code  TYPE zsegment_type,
      END OF ty_types,
      tab_types TYPE STANDARD TABLE OF ty_types
                WITH DEFAULT KEY .

    IF ( lines( me->filter-infotipos ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(code_types) = VALUE tab_types(
      ( infty = '0014' code  = 'AIIowance' )
      ( infty = '0015' code  = 'AIIowance' )
      ( infty = '2001' code  = 'PaidAbsence' )
      ( infty = '2002' code  = 'PaidWork' ) " 2002  PaidWork  C - Presenças
      ( infty = '2001' code  = 'UnpaidAbsence' )
      ( infty = '2010' code  = 'Overtime' ) " 2010 Overtime B-Trabalho Extraordinário
    ).

    DATA(range_infotipos) = VALUE infty_range_tab(
      FOR r IN me->filter-infotipos (
        sign   = if_fsbp_const_range=>sign_include
        option = if_fsbp_const_range=>option_equal
        low    = r
      )
    ) .

    SORT range_infotipos ASCENDING BY low .

    DATA(segment_type) = VALUE zsegment_type_tab(
      FOR t IN code_types
      WHERE ( infty IN range_infotipos )
      ( t-code )
    ).

    result = VALUE zarray_of_segment_type(
      segment_type = segment_type
    ).

  ENDMETHOD .


  METHOD get_date_from_datetime .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CONVERT TIME STAMP CONV xsddatetime_z( CONV num14( data ) )
       TIME ZONE space
       INTO DATE result .

  ENDMETHOD.


  METHOD get_filtering_criteria .

    CONSTANTS:
      BEGIN OF code_target,
        any TYPE zcode_target_type VALUE 'Any',
      END OF code_target,

      BEGIN OF code_time_type,
        any TYPE zcode_criteria_type VALUE 'Any',
      END OF code_time_type,

      BEGIN OF export_time_format,
        startandendtime TYPE ztime_format_type VALUE 'IncludeStartAndEndTimeForAllSegments',
      END OF export_time_format,

      BEGIN OF target_type,
        any TYPE ztarget_criteria_type VALUE 'Any',
      END OF target_type .

    DATA(code_types) = me->get_code_types( ) .
    IF ( lines( code_types-segment_type ) EQ 0 ) .
      IF ( 0 EQ 1 ). MESSAGE i017(zhcm). ENDIF .
      me->set_log( EXPORTING type   = if_xo_const_message=>error
                             number = me->message-segment_type
      ) .
      RETURN .
    ENDIF .

    IF ( me->filter-start_date IS INITIAL ) OR
       ( me->filter-end_date IS INITIAL ) .
      RETURN .
    ENDIF .

    CONVERT DATE me->filter-start_date
       INTO TIME STAMP DATA(start_date)
       TIME ZONE space .

    CONVERT DATE me->filter-end_date
       INTO TIME STAMP DATA(end_date)
       TIME ZONE space .

    DATA(employee_codes) = me->get_employee_codes( ).

    DATA(fields) = VALUE zstring_tab( ( CONV sxms_value( 'DR.TpSituacao' ) ) ) .
    DATA(additional_fields) = VALUE zarray_ofstring(
      string     = fields
    ).

    result = VALUE zsalary_export_criteria(
      code_target                    = 'Any'
      code_time_type                 = 'Any'
      code_types                     = code_types
      company_code                   = me->company_code
      employee_codes                 = employee_codes
      end_date                       = end_date
      export_time_format             = export_time_format-startandendtime
      group_sequential_absences      = abap_on
      ignore_rest_days_while_groupin = abap_off
      only_complete_day_absences     = abap_off
      only_partial_day_absences      = abap_off
      start_date                     = start_date
      target_type                    = 'Any'
      additional_fields              = additional_fields
    ).

  ENDMETHOD .


  METHOD get_employee_codes .

    IF ( lines( me->filter-pernr ) EQ 0 ) .
      RETURN .
    ENDIF .

    result-string = VALUE #(
      FOR p IN me->filter-pernr
      ( |{ p ALPHA = OUT }| )
    ).

  ENDMETHOD .


  METHOD get_id .

    CONSTANTS:
      BEGIN OF nro,
        nr_range_nr TYPE nrnr VALUE '1',
        object      TYPE nrobj VALUE 'YHCM_IFELO',
        quantity    TYPE nrquan VALUE '1',
      END OF nro .

    IF ( year IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = nro-nr_range_nr
        object      = nro-object
        quantity    = '1'
        toyear      = year
      IMPORTING
        number      = result.

  ENDMETHOD .


  METHOD get_id_temp .

    DATA:
      random TYPE string .

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 10
      IMPORTING
        random_string = random.

    result = CONV #( random ) .

  ENDMETHOD .


  METHOD get_input .

    DATA(filtering_criteria) = me->get_filtering_criteria( ) .
    IF ( filtering_criteria IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(user) = me->get_user_access( ) .
    IF ( user IS INITIAL ) .
      RETURN .
    ENDIF .

    result = VALUE zielo_web_service_export_sala1(
      filtering_criteria = filtering_criteria
      user               = user
    ).

  ENDMETHOD .


  METHOD get_proxy .

    CONSTANTS:
      logical_port TYPE prx_logical_port_name VALUE 'YLP_IELO_WEB_SERVICE' .

    TRY .
        result = NEW yelo_co_ielo_web_service(
          logical_port_name = logical_port ) .
      CATCH cx_ai_system_fault INTO DATA(error).
        me->messages = ycl_hcm_application_log=>map_excep_to_bapiret2( error->errortext ).
    ENDTRY.

  ENDMETHOD .


  METHOD get_time_from_datetime .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CONVERT TIME STAMP CONV xsddatetime_z( CONV num14( data ) )
       TIME ZONE space
       INTO TIME result .

  ENDMETHOD.


  METHOD get_type .

    TYPES:
      BEGIN OF ty_compare,
        code_type TYPE ythcm0003-code_type,
        type      TYPE ythcm0001-type,
      END OF ty_compare,
      tab_compare TYPE STANDARD TABLE OF ty_compare
                  WITH KEY code_type .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(compare) = VALUE tab_compare(
      ( code_type = 'Invalid'               type = '' )
      ( code_type = 'UnpaidWork'            type = '' )
      ( code_type = 'UnpaidAbsence'         type = '2001' )
      ( code_type = 'Tolerance'             type = '' )
      ( code_type = 'TerminalJustification' type = '' )
      ( code_type = 'Penalty'               type = '' )
      ( code_type = 'PaidWork'              type = '2002' )
      ( code_type = 'PaidAbsence'           type = '2001' )
      ( code_type = 'Overtime'              type = '2010' )
      ( code_type = 'AIIowance'             type = '0014' ) " 0014 / 0015
*     ( code_type = 'AIIowance'             type = '0015' ) " 0014 / 0015
    ).

    result = VALUE #( compare[ code_type = data ]-type OPTIONAL ) .

  ENDMETHOD.


  METHOD get_user_access .

    result =
      NEW ycl_hcm_elo_maintenance( )->yif_hcm_elo_data_maintenance~get_user_access( ) .

  ENDMETHOD .


  METHOD is_valid_employees_code .

    DATA:
      " Numeros de registros validos (ja criados no SAP)
      valid_employees TYPE pernr_tab,
      string_employee TYPE zsalary_time-employee_code.

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(employees) = VALUE pernr_tab( FOR e IN data ( CONV #( e-employee_code ) ) ) .
    SORT employees ASCENDING BY table_line .
    DELETE ADJACENT DUPLICATES FROM employees COMPARING table_line .

    SELECT pernr
      FROM pa0003
       FOR ALL ENTRIES IN @employees
     WHERE pernr EQ @employees-table_line
     ORDER BY PRIMARY KEY
      INTO TABLE @valid_employees .

    IF ( sy-subrc NE 0 ) .
      CLEAR data .
      RETURN .
    ENDIF .

    DATA(data_temp) = data .
    CLEAR data .

    LOOP AT data_temp ASSIGNING FIELD-SYMBOL(<employee>) .

      IF ( line_exists( valid_employees[ table_line = CONV pernr_d( <employee>-employee_code ) ] ) ) .
        APPEND <employee> TO data .
        CONTINUE .
      ENDIF .

      IF ( 0 EQ 1 ). MESSAGE i016(zhcm). ENDIF .
      me->set_log( type   = if_xo_const_message=>error
                   number = me->message-invalid_employee
                   m1     = |{ <employee>-employee_code ALPHA = OUT }|
      ).

    ENDLOOP .

  ENDMETHOD .


  METHOD is_valid_subtypes .

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(subtypes) = VALUE subtyp_tab( FOR s IN data ( CONV #( s-salary_code ) ) ) .
    SORT subtypes ASCENDING BY table_line .
    DELETE ADJACENT DUPLICATES FROM subtypes COMPARING table_line .

    IF ( lines( subtypes ) EQ 0 ) .
      RETURN .
    ENDIF .

    SELECT subty
      FROM t554s
       FOR ALL ENTRIES IN @subtypes
     WHERE subty EQ @subtypes-table_line
       AND endda GE @sy-datum
       AND begda LE @sy-datum
      INTO TABLE @DATA(valid_subtypes) .

    DATA(data_temp) = data .
    CLEAR data .

    LOOP AT data_temp ASSIGNING FIELD-SYMBOL(<subtype>) .

      IF ( line_exists( valid_subtypes[ subty = CONV subty( <subtype>-salary_code ) ] ) ) .
        APPEND <subtype> TO data .
        CONTINUE .
      ENDIF .
      IF ( 0 EQ 1 ). MESSAGE i021(zhcm). ENDIF .
      me->set_log( type   = if_xo_const_message=>error
                   number = me->message-invalid_subtype
                   m1     = |{ <subtype>-salary_code ALPHA = OUT }|
      ).

    ENDLOOP .

  ENDMETHOD .


  METHOD mainten_imported .

    DATA:
      log TYPE bapiret2_t .

    IF ( lines( me->header )   EQ 0 ) OR
       ( lines( me->imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT me->filter-infotipos ASSIGNING FIELD-SYMBOL(<infotype>).

      CASE <infotype> .

        WHEN ycl_hcm_elo_infotypes=>infotypes-ausencias .

          me->prepare_by_infotype(
            EXPORTING
              im_infotype = ycl_hcm_elo_infotypes=>infotypes-ausencias
              im_header   = me->header
              im_imported = me->imported
            IMPORTING
              ex_header   = DATA(local_header)
              ex_imported = DATA(local_imported)
          ).

          IF ( lines( local_header )   EQ 0 ) OR
             ( lines( local_imported ) EQ 0 ) .
            CONTINUE .
          ENDIF .

          log = ycl_hcm_elo_maintenance=>mainten_2001_imported(
                  EXPORTING
                    header       = local_header
                    impored_type = local_imported
                ).

          result = VALUE #( BASE result ( LINES OF log ) ) .

        WHEN ycl_hcm_elo_infotypes=>infotypes-presencas .

          me->prepare_by_infotype(
            EXPORTING
              im_infotype = ycl_hcm_elo_infotypes=>infotypes-presencas
              im_header   = me->header
              im_imported = me->imported
            IMPORTING
              ex_header   = local_header
              ex_imported = local_imported
          ).

          IF ( lines( local_header ) EQ 0 ) OR
             ( lines( local_imported ) EQ 0 ) .
            CONTINUE .
          ENDIF .

          log = ycl_hcm_elo_maintenance=>mainten_2002_imported(
                  EXPORTING
                    header        = local_header
                    imported_type = local_imported
                ).

          result = VALUE #( BASE result ( LINES OF log ) ) .

        WHEN ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp .

          me->prepare_by_infotype(
            EXPORTING
              im_infotype = ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp
              im_header   = me->header
              im_imported = me->imported
            IMPORTING
              ex_header   = local_header
              ex_imported = local_imported
          ).

          " Verifica colisão
          ycl_hcm_elo_maintenance=>check_2010_imported(
            CHANGING ch_header        = local_header
                     ch_imported_type = local_imported
                     ch_log           = log ) .

          IF ( lines( local_header ) EQ 0 ) OR
             ( lines( local_imported ) EQ 0 ) .
            result = VALUE #( BASE result ( LINES OF log ) ) .
            CONTINUE .
          ENDIF .

          log = ycl_hcm_elo_maintenance=>mainten_2010_imported(
                  EXPORTING
                    header        = local_header
                    imported_type = local_imported
                ).

          result = VALUE #( BASE result ( LINES OF log ) ) .

        WHEN OTHERS .

      ENDCASE .

    ENDLOOP .

    IF ( line_exists( log[ type = if_xo_const_message=>success ] ) ) .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_on
*      IMPORTING
*         return =
        .
    ENDIF .

  ENDMETHOD .


  METHOD map_salary_time .

    " um ID temporario sera criado para ligar as duas tabelas
    DATA:
      temp_id TYPE ythcm0003-id VALUE 0000000000 .

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->is_valid_employees_code( CHANGING data = data ).
*   me->is_valid_subtypes( CHANGING data = data ).

    TRY .
        me->imported = VALUE ythcm0003_t(
          FOR l IN data
            ( gjahr           = |{ sy-datum(4) }|
              id              = me->get_id_temp( )
              code_in_hours   = l-code_in_hours
              code_type       = l-code_type
              duration        = COND #( WHEN l-code_in_hours EQ abap_true
                                        THEN l-duration )
              employee_code   = CONV numc08( l-employee_code )
              end_date_time   = l-end_date_time
              end_date        = me->get_date_from_datetime( l-end_date_time )
              end_time        = me->get_time_from_datetime( l-end_date_time )
              number_of_days  = l-number_of_days
              salary_code     = condense( l-salary_code )
              start_date_time = l-start_date_time
              start_time      = me->get_time_from_datetime( l-start_date_time )
              start_date      = me->get_date_from_datetime( l-start_date_time )
              tpsituacao      = me->get_tp_situacao( l )
              credat          = sy-datum
              cretim          = sy-uzeit
              crenam          = sy-uname
            )
          )  .
      CATCH cx_sy_conversion_no_number INTO DATA(error) .
        CLEAR me->imported .
        IF ( 0 EQ 1 ). MESSAGE i000(zhcm). ENDIF .
        me->set_log( type   = if_xo_const_message=>error
                     number = me->message-general
                     m1     = CONV #( error->get_text( ) )
        ).
        RETURN .
    ENDTRY .

    DELETE me->imported WHERE id IS INITIAL .

    me->header = VALUE ythcm0001_t(
      FOR i IN me->imported (
        gjahr   = i-gjahr
        id      = i-id
        bukrs   = me->filter-empresa
        pernr   = i-employee_code
        direct  = 'CONSUMER'
        type    = me->get_type( i-code_type )
        subtype = i-salary_code
        status  = '01' "  Integrado com sucesso
        credat  = i-credat
        cretim  = i-cretim
        crenam  = i-crenam
    ) ) .

  ENDMETHOD.


  METHOD prepare_by_infotype .

    TYPES:
      tab_header TYPE RANGE OF ythcm0001-id .

    CLEAR:
      ex_header, ex_imported .

    IF ( im_infotype IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( lines( im_header )   EQ 0 ) OR
       ( lines( im_imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    ex_header = VALUE #(
      FOR h IN im_header
      WHERE ( type EQ im_infotype )
      ( CORRESPONDING #( h ) )
    ).

    " Recupera todos os ID's referentes a esse infotype
    DATA(headers) = VALUE tab_header(
      FOR l IN ex_header (
        sign   = rsmds_c_sign-including
        option = rsmds_c_option-equal
        low    = l-id
      )
    ) .

    " Recupera a chave para correspondecia do ano
    DATA(year) = VALUE #( ex_header[ 1 ]-gjahr OPTIONAL ) .

    ex_imported = VALUE #(
      FOR i IN im_imported
      WHERE ( id IN headers AND gjahr EQ year )
      ( CORRESPONDING #( i ) )
    ).

  ENDMETHOD .


  METHOD get_tp_situacao .

    CONSTANTS:
      lc_field TYPE string VALUE 'DR.TpSituacao' .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( lines( data-additional_fields-key_value_ofstringstring ) EQ 0 ) .
      RETURN .
    ENDIF .

    ASSIGN data-additional_fields-key_value_ofstringstring[ key = lc_field ]
        TO FIELD-SYMBOL(<tp_situacao>) .
    IF ( <tp_situacao> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    result = <tp_situacao>-value .

  ENDMETHOD .


  METHOD prepare_data .

    IF ( lines( me->imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Agrupar ausencias que tem o "Dia Completo" marcado
    me->group_absence( ) .
    IF ( lines( me->imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT me->imported ASSIGNING FIELD-SYMBOL(<imported>) .
      ASSIGN me->header[ id = <imported>-id ] TO FIELD-SYMBOL(<header>) .
      IF ( <header> IS ASSIGNED ) .
        " Atribuindo novo ID para novos registros
        <header>-id   =
        <imported>-id = me->get_id( <imported>-gjahr ) .
        UNASSIGN <header> .
      ENDIF .
    ENDLOOP .

  ENDMETHOD.


  METHOD group_absence .

    CONSTANTS:
      BEGIN OF lc_tpsituacao,
        dia_completo TYPE ythcm0003-tpsituacao VALUE '1',
      END OF lc_tpsituacao .


    DATA(lt_ausencias) = get_group_absence( ) .

    IF ( lines( me->imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Criando uma tabel temporaria para verificar os itens e agrupamentos
    DATA(lt_imported_temp) = me->imported .
    CLEAR me->imported .

    " Deve-se ter uma data inicial e final ordenadas para correto funcionamento
    SORT lt_imported_temp ASCENDING BY employee_code salary_code start_date end_date .

    LOOP AT lt_imported_temp INTO DATA(ls_temp) .

      " Verificando se deve ser desconsiderado todo o dia
      IF ( NOT line_exists( lt_ausencias[ table_line = ls_temp-salary_code ] ) ) .
        APPEND ls_temp TO me->imported .
        CONTINUE .
      ENDIF .

      " Verificando se o registro esta marcado para todo o dia
      IF ( ls_temp-tpsituacao NE lc_tpsituacao-dia_completo ) .
        APPEND ls_temp TO me->imported .
        CONTINUE .
      ENDIF .

      " Verificando se ja existe um registro para esse item
      ASSIGN me->imported[ employee_code = ls_temp-employee_code
                           salary_code   = ls_temp-salary_code ]
      TO FIELD-SYMBOL(<fs_imported>) .
      IF ( <fs_imported> IS NOT ASSIGNED ) .
        " Caso não encontre, possivelmente é o primeiro registro do intervalo de data
        APPEND ls_temp TO me->imported .
        CONTINUE .
      ENDIF .

      " Se a data final for diferente, seria outro dia e deve somar o contador de dia
*     <fs_imported>-dia_completo = <fs_imported>-dia_completo + ls_temp-dia_completo .

*      " Verificando se o intervalo é superior a um dia
*      IF ( ( ls_temp-end_date - <fs_imported>-end_date ) GT 1 ) .
*        APPEND ls_temp TO me->imported .
*        CONTINUE .
*      ENDIF .

      " O registro ja existe, então, sera apenas atualizado a data final do evento
      <fs_imported>-end_date      = ls_temp-end_date .
      <fs_imported>-end_time      = ls_temp-end_time .
      <fs_imported>-end_date_time = ls_temp-end_date_time .
      UNASSIGN <fs_imported> .

    ENDLOOP .


    "  Atualizando dados de header
    DATA(lt_header) = me->header .
    CLEAR me->header .

    LOOP AT me->imported INTO DATA(ls_imported) .
      ASSIGN lt_header[ id = ls_imported-id ] TO FIELD-SYMBOL(<header>) .
      IF ( <header> IS ASSIGNED ) .
        APPEND <header> TO me->header .
        UNASSIGN <header> .
      ENDIF .
    ENDLOOP .


  ENDMETHOD .


  METHOD save_imported .

    IF ( lines( me->header ) EQ 0 ) .
      RETURN .
    ENDIF .

    IF ( lines( me->imported ) EQ 0 ) .
      RETURN .
    ENDIF .

    MODIFY ythcm0001 FROM TABLE me->header .
    IF ( sy-subrc EQ 0 ) .
      MODIFY ythcm0003 FROM TABLE me->imported .

      IF ( sy-subrc EQ 0 ) .

        COMMIT WORK AND WAIT .

        IF ( 0 EQ 1 ). MESSAGE i005(zhcm). ENDIF .
        IF ( 0 EQ 1 ). MESSAGE i023(zhcm). ENDIF .
        result = VALUE #(
          ( type       = if_xo_const_message=>success
            id         = ycl_hcm_application_log=>message-id
            number     = me->message-imported_successfully
            row        = 1
          )
          ( type       = if_xo_const_message=>success
            id         = ycl_hcm_application_log=>message-id
            number     = me->message-save_success
            row        = 2
          )
        ).

      ENDIF .

    ENDIF .

  ENDMETHOD.


  METHOD set_log .

    me->messages = VALUE #( BASE me->messages
      ( type       = COND #( WHEN type IS INITIAL
                             THEN if_xo_const_message=>info
                             ELSE type )
        id         = ycl_hcm_application_log=>message-id
        number     = number
        message_v1 = m1 )
    ).

  ENDMETHOD .


  METHOD test_interface .

    TYPES:
      BEGIN OF ty_result,
        code_in_hours   TYPE zsalary_time-code_in_hours,
        code_type       TYPE zsalary_time-code_type,
        duration        TYPE zsalary_time-duration,
        employee_code   TYPE zsalary_time-employee_code,
        start_date_time TYPE zsalary_time-start_date_time,
        end_date_time   TYPE zsalary_time-end_date_time,
        number_of_days  TYPE zsalary_time-number_of_days,
        salary_code     TYPE zsalary_time-salary_code,
        tpsituacao      TYPE ythcm0003-tpsituacao,
      END OF ty_result,
      tab_result TYPE STANDARD TABLE OF ty_result
                 WITH DEFAULT KEY .

    DATA:
      obj_proxy    TYPE REF TO yelo_co_ielo_web_service,
      logical_port TYPE  prx_logical_port_name VALUE 'YLP_IELO_WEB_SERVICE',
      input        TYPE zielo_web_service_list_employ1.

    TRY .
        obj_proxy = NEW yelo_co_ielo_web_service(
          logical_port_name = logical_port
        ) .
      CATCH cx_ai_system_fault.
    ENDTRY .

    IF ( obj_proxy IS NOT BOUND ) .
      result = 'Erro ao criar proxy.' .
      RETURN .
    ENDIF.


    IF ( start IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( end IS INITIAL ) .
      RETURN .
    ENDIF .

    CONVERT DATE start
       INTO TIME STAMP DATA(start_date)
       TIME ZONE space .

    CONVERT DATE end
       INTO TIME STAMP DATA(end_date)
       TIME ZONE space .

    DATA(user) = VALUE zwsuser(
      company_code    = 'DESCONTAO'
      hashed_password = '' " false
      password        = '1234'
      user_code       = 'sapuser'
    ).


    DATA(fields) = VALUE zstring_tab( ( CONV sxms_value( 'DR.TpSituacao' ) ) ) .
    DATA(additional_fields) = VALUE zarray_ofstring(
      string     = fields
    ).

    DATA(sal_filtering_criteria) = VALUE zsalary_export_criteria(
      code_target                    = 'Any'
      code_time_type                 = 'Any'
      company_code                   = 'DESCONTAO'
      end_date                       = end_date
      export_time_format             = 'IncludeStartAndEndTimeForAllSegments'
      group_sequential_absences      = abap_on
      ignore_rest_days_while_groupin = abap_off
      only_complete_day_absences     = abap_off
      only_partial_day_absences      = abap_off
      start_date                     = start_date
      target_type                    = 'Any'
      additional_fields              = additional_fields
    ).

    DATA(sal_input) = VALUE zielo_web_service_export_sala1(
      filtering_criteria = sal_filtering_criteria
      user               = user
    ).

    TRY .
        obj_proxy->export_salary_time(
          EXPORTING
            input  = sal_input
          IMPORTING
            output = DATA(output)
        ).

      CATCH cx_ai_system_fault INTO DATA(lo_error).
        result = lo_error->errortext .
        result = COND #( WHEN lo_error->get_longtext( ) IS INITIAL
                         THEN lo_error->get_text( )
                         ELSE lo_error->get_longtext( ) ) .
        RETURN .
    ENDTRY .

    DATA(out) = VALUE tab_result(
      FOR s IN output-salary_time_info-salary_time (
*      ( code_in_hours   = s-code_in_hours
*        code_type       = s-code_type
*        duration        = s-duration
*        employee_code   = s-employee_code
*        end_date_time   = s-end_date_time
*        number_of_days  = s-number_of_days
*        salary_code     = s-salary_code
*        start_date_time = s-start_date_time
*       tpsituacao      = new ycl_hcm_elo_importation( )->get_tp_situacao( s )
        CORRESPONDING #( s )
      )
    ).

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(alv)
                                CHANGING  t_table = out ).
        alv->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN'
                                report        = 'SAPLKKBL'
                                set_functions = alv->c_functions_all ) .

        DATA(lo_display) = alv->get_display_settings( ) .
        IF ( lo_display IS BOUND ) .
          lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .
        ENDIF .
        alv->display( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD .
ENDCLASS.