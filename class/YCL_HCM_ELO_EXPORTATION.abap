CLASS ycl_hcm_elo_exportation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF interfaces,
        set_regist_employee TYPE zwsuser-company_code VALUE 'RegistEmployee',
      END OF interfaces,

      BEGIN OF inftyp,
        p0000 TYPE infty VALUE '0000',
        p0001 TYPE infty VALUE '0001',
        p0002 TYPE infty VALUE '0002',
        p0105 TYPE infty VALUE '0105',
        p0337 TYPE infty VALUE '0337',
        p2010 TYPE infty VALUE '2010',
      END OF inftyp .

    "! <p class="shorttext synchronized" lang="pt">Construtor</p>
    METHODS constructor
      IMPORTING
        !data TYPE pa0001-pernr .
    "! <p class="shorttext synchronized" lang="pt">Exporta dados para servico RegistEmployee ELO</p>
    CLASS-METHODS regist_employee
      IMPORTING
        !data         TYPE pa0001-pernr
      RETURNING
        VALUE(result) TYPE bapiret2_t .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      "! <p class="shorttext synchronized" lang="pt">Armazena mensagens de processamento da classe</p>
      messages TYPE bapiret2_t,
      "! <p class="shorttext synchronized" lang="pt">Nro de pessoal que esta sendo processado</p>
      employee TYPE p0001-pernr,
      "! <p class="shorttext synchronized" lang="pt">Header de Importção</p>
      header   TYPE ythcm0001_t.

    METHODS regist_employee_single
      RETURNING
        VALUE(result) TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Retorna objeto Proxy</p>
    METHODS get_proxy
      RETURNING VALUE(result) TYPE REF TO yelo_co_ielo_web_service .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de entrada da interafce</p>
    METHODS get_input
      RETURNING VALUE(result) TYPE zielo_web_service_regist_empl1  .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee</p>
    METHODS get_employee
      RETURNING VALUE(result) TYPE zemployee .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee p/ export(inftype 0000)</p>
    METHODS get_detail_0000
      RETURNING VALUE(result) TYPE p0000 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee p/ export(inftype 0001)</p>
    METHODS get_detail_0001
      RETURNING VALUE(result) TYPE p0001 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee p/ export(inftype 0002)</p>
    METHODS get_detail_0002
      RETURNING VALUE(result) TYPE p0002 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee p/ export(inftype 0105)</p>
    METHODS get_detail_0105
      RETURNING VALUE(result) TYPE p0105 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee p/ export(inftype 0337)</p>
    METHODS get_detail_0337
      RETURNING VALUE(result) TYPE p0337 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de acesso (user,pass and etc)</p>
    METHODS get_user_access
      RETURNING VALUE(result) TYPE zwsuser .
    "! <p class="shorttext synchronized" lang="pt">Retorna um datetime convertido</p>
    METHODS map_to_datetime
      IMPORTING
        !data         TYPE d
      RETURNING
        VALUE(result) TYPE xsddatetime_z .
    "! <p class="shorttext synchronized" lang="pt">Retorna um decimal convertido</p>
    METHODS map_to_decimal
      IMPORTING
        !data         TYPE d
      RETURNING
        VALUE(result) TYPE xsddatetime_z .
    "! <p class="shorttext synchronized" lang="pt">Retorna a foto do colaborar (formato RAW)</p>
    METHODS get_photo
      RETURNING VALUE(result) TYPE xstring .
    "! <p class="shorttext synchronized" lang="pt">Adiciona uma mensagem ao log de processamento</p>
    METHODS add_log
      IMPORTING
        !msg TYPE bapiret2_t .
ENDCLASS.



CLASS ycl_hcm_elo_exportation IMPLEMENTATION.


  METHOD constructor .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    me->employee = data .

  ENDMETHOD .


  METHOD regist_employee .

    result =
      NEW ycl_hcm_elo_exportation( data )->regist_employee_single( ).

  ENDMETHOD .


  METHOD regist_employee_single .

    CONSTANTS:
      lc_true TYPE char1 VALUE '1' .

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

    TRY .
        proxy->regist_employee(
          EXPORTING
            input = input
          IMPORTING
            output = DATA(output)
        ) .
      CATCH cx_ai_system_fault INTO DATA(error).
        result = VALUE #( BASE me->messages (
          LINES OF ycl_hcm_application_log=>map_excep_to_bapiret2( error->errortext ) ) ) .
        RETURN .
    ENDTRY .

    CASE output-regist_employee_result-base-operation_success .

      WHEN lc_true .
        result = VALUE #( BASE me->messages (
          LINES OF ycl_hcm_application_log=>map_excep_to_bapiret2(
            type = if_xo_const_message=>success
            message = |{ output-regist_employee_result-base-operation_result_description
                       } - { me->employee ALPHA = OUT }| ) ) ) .

      WHEN '-1' .
        result = VALUE #( BASE me->messages (
          LINES OF ycl_hcm_application_log=>map_excep_to_bapiret2(
            type = if_xo_const_message=>error
            message = |{ output-regist_employee_result-base-operation_result_description
                       } - { me->employee ALPHA = OUT }| ) ) ) .

      WHEN OTHERS .

    ENDCASE .

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


  METHOD get_input .

    DATA(employee) = me->get_employee( ) .
    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(user) = me->get_user_access( ) .
    IF ( user IS INITIAL ) .
      RETURN .
    ENDIF .

    result = VALUE zielo_web_service_regist_empl1(
      employee   = employee
      user       = user
    ).

  ENDMETHOD .


  METHOD get_employee .

    CONSTANTS:
      company_code TYPE zemployee_filtering_criteria-company_code VALUE 'DESCONTAO',
      masc         TYPE char1 VALUE 'M',
      femi         TYPE char1 VALUE 'F'.

    DATA(infotype_0000) = me->get_detail_0000( ) .
    IF ( infotype_0000 IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(infotype_0001) = me->get_detail_0001( ) .
    DATA(infotype_0002) = me->get_detail_0002( ) .
    DATA(infotype_0105) = me->get_detail_0105( ) .
    DATA(infotype_0337) = me->get_detail_0337( ) .

    result = VALUE zemployee(

      active              = abap_on
      company_code        = company_code

      start_contract_date = me->map_to_datetime( infotype_0001-begda )
      end_contract_date   = me->map_to_datetime( infotype_0001-endda )

      employee_code       = |{ infotype_0001-pernr ALPHA = OUT }|
      name                = infotype_0002-cname
      abbreviated_name    = |{ infotype_0002-cname }|

      professional_category_code = infotype_0337-prcat
      cost_central_code          = infotype_0001-kostl

      additional_code1           = infotype_0001-orgeh

      start_date                 = infotype_0001-begda
      end_date                   = infotype_0001-endda

      email                      = infotype_0105-usrid_long
      birth_date                 = me->map_to_datetime( infotype_0002-gbdat )
      photo                      = me->get_photo( )
      gender  = COND #( WHEN infotype_0002-gesch EQ '1' THEN masc
                        WHEN infotype_0002-gesch EQ '2' THEN femi
                        ELSE abap_off )
    ) .

  ENDMETHOD .


  METHOD get_detail_0000 .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0000( employee ) .

  ENDMETHOD .


  METHOD get_detail_0001 .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0001( employee ) .

  ENDMETHOD .


  METHOD get_detail_0002 .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0002( employee ) .

  ENDMETHOD .


  METHOD get_detail_0105 .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0105( employee ) .

  ENDMETHOD .


  METHOD get_detail_0337 .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0337( employee ) .

  ENDMETHOD .


  METHOD get_user_access .

    result =
      NEW ycl_hcm_elo_maintenance( )->yif_hcm_elo_data_maintenance~get_user_access( ) .

  ENDMETHOD .


  METHOD get_photo .

    DATA:
      exists       TYPE char1,
      connect_info TYPE toav0,
      length       TYPE int4,
      message      TYPE bapiret2,
      document     TYPE STANDARD TABLE OF tbl1024,
      buffer       TYPE xstring.

    IF ( me->employee IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'HR_IMAGE_EXISTS'
      EXPORTING
        p_pernr               = me->employee
*       p_tclas               = 'A'
*       p_begda               = '18000101'
*       p_endda               = '99991231'
      IMPORTING
        p_exists              = exists
        p_connect_info        = connect_info
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    CALL FUNCTION 'ALINK_RFC_TABLE_GET'
      EXPORTING
        im_docid    = connect_info-arc_doc_id
        im_crepid   = connect_info-archiv_id
*       im_compid   =
      IMPORTING
        ex_length   = length
        ex_message  = message
      TABLES
        ex_document = document.
    IF ( lines( document ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Alterar o tipo de menasgem, pois esse valor nao seria impeditivo
    IF ( message-type EQ if_xo_const_message=>error ) .
      message-type = if_xo_const_message=>warning .
      me->add_log( msg = VALUE #( ( message ) ) ) .
      RETURN .
    ENDIF .

    DESCRIBE TABLE document LINES DATA(last_line) .

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = length
        first_line   = 1
        last_line    = last_line
      IMPORTING
        buffer       = buffer
      TABLES
        binary_tab   = document
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    result = buffer .

  ENDMETHOD .


  METHOD add_log .

    IF ( msg IS INITIAL ) .
      RETURN .
    ENDIF .

    me->messages = VALUE #(
      BASE me->messages ( LINES OF msg )
    ).

  ENDMETHOD .


  METHOD map_to_datetime .

    DATA:
      local_date TYPE string .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    TRY .
        cl_bs_soa_convert_xsddatetime=>map_xsddatetime_z_out(
          EXPORTING
            iv_date         = data
            iv_time         = '000000'
*           iv_timestamp    =
            iv_timezone     = space
          IMPORTING
            ev_xsd_datetime = result
        ) .

      CATCH cx_bs_soa_exception .

    ENDTRY .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CONVERT DATE data
       INTO TIME STAMP result TIME ZONE space .

  ENDMETHOD .


  METHOD map_to_decimal .

    DATA:
      local_date TYPE string .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result = data .

  ENDMETHOD .

ENDCLASS.