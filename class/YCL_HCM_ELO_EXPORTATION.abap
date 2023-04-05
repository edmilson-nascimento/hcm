CLASS ycl_hcm_elo_exportation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF interfaces,
        set_regist_employee TYPE zwsuser-company_code VALUE 'RegistEmployee',
      END OF interfaces .

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
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Employee para serem exportados</p>
    METHODS get_employee_detail
      RETURNING VALUE(result) TYPE p0001 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de acesso (user,pass and etc)</p>
    METHODS get_user_access
      RETURNING VALUE(result) TYPE zwsuser .


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

    IF ( output-regist_employee_result-base-operation_success EQ lc_true ) .
      result = VALUE #( BASE me->messages (
        LINES OF ycl_hcm_application_log=>map_excep_to_bapiret2(
          type = if_xo_const_message=>success
          message = |{ output-regist_employee_result-base-operation_result_description
                     } - { me->employee ALPHA = OUT }| ) ) ) .
    ENDIF .

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
      company_code TYPE zemployee_filtering_criteria-company_code VALUE 'DESCONTAO'.

    DATA(list_sap) = me->get_employee_detail( ) .
    IF ( list_sap IS INITIAL ) .
      RETURN .
    ENDIF .

    result = VALUE zemployee(
      abbreviated_name    = list_sap-sname
      active              = abap_true
      company_code        = company_code
      employee_code       = |{ list_sap-pernr ALPHA = OUT }|
      end_date            = list_sap-endda
      name                = list_sap-ename
      start_date          = list_sap-begda
    ).

  ENDMETHOD .


  METHOD get_employee_detail .

    IF ( employee IS INITIAL ) .
      RETURN .
    ENDIF .

    result = NEW ycl_hcm_elo_maintenance( )->read_0001( employee ) .

  ENDMETHOD .


  METHOD get_user_access .

    CONSTANTS:
      BEGIN OF user,
        company_code    TYPE zwsuser-company_code VALUE 'DESCONTAO',
        hashed_password TYPE zwsuser-hashed_password VALUE '',
        password        TYPE zwsuser-password VALUE '1234',
        user_code       TYPE zwsuser-user_code VALUE 'sapuser',
      END OF user .

    result = VALUE zwsuser(
      company_code    = user-company_code
      hashed_password = user-hashed_password
      password        = user-password
      user_code       = user-user_code
    ).

  ENDMETHOD .

ENDCLASS.