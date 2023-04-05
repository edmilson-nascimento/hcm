CLASS ycl_hcm_elo_infotypes DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_hcm_elo_data_maintenance .

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="pt">Infotipos contemplados</p>
      BEGIN OF infotypes,
        remun_deduc_period TYPE ythcm0001-type VALUE '0014',
        pag_complementar   TYPE ythcm0001-type VALUE '0015',
        "! <p class="shorttext synchronized" lang="pt">Ausências</p>
        ausencias          TYPE ythcm0001-type VALUE '2001',
        presencas          TYPE ythcm0001-type VALUE '2002',
        substituicoes      TYPE ythcm0001-type VALUE '2003',
        contg_ausencias    TYPE ythcm0001-type VALUE '2006',
        "! <p class="shorttext synchronized" lang="pt">Trabalho Extraordinário</p>
        infos_remun_emp    TYPE ythcm0001-type VALUE '2010',
        mestre             TYPE ythcm0001-type VALUE '0001',
      END OF infotypes,

      "! <p class="shorttext synchronized" lang="pt">Tipos de Ausências (infotype 2001)</p>
      BEGIN OF ausencias,
        "! <p class="shorttext synchronized" lang="pt">1004 - Doença (consulta médica)</p>
        doenca TYPE subty VALUE '1004',
        "! <p class="shorttext synchronized" lang="pt">1029 - Greve</p>
        greve  TYPE subty VALUE '1029',
      END OF ausencias,

      "! <p class="shorttext synchronized" lang="pt">Tipos de Presenças</p>
      BEGIN OF presencas,
        formacao_interna TYPE subty VALUE '2000',
        trabalho_remoto  TYPE subty VALUE '2005',
      END OF presencas,

      "! <p class="shorttext synchronized" lang="pt">Tipos de ações contemplados</p>
      BEGIN OF operation,
        copiar               TYPE actio VALUE 'COP',
        eliminar             TYPE actio VALUE 'DEL',
        exibir               TYPE actio VALUE 'DIS',
        bloquear_desbloquear TYPE actio VALUE 'EDQ',
        "! <p class="shorttext synchronized" lang="pt">Criar um novo registro de Infotype</p>
        criar                TYPE actio VALUE 'INS',
        delimitar            TYPE actio VALUE 'LIS9',
        modificar            TYPE actio VALUE 'MOD',
        criar_c_medidas      TYPE actio VALUE 'INSS',
      END OF operation.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_hcm_elo_infotypes IMPLEMENTATION.


  METHOD yif_hcm_elo_data_maintenance~dequeue_employee.

    DATA:
      return_message TYPE bapiret1 .

    IF ( number IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = number
      IMPORTING
        return = return_message.

    result = COND #(
      WHEN return_message-type EQ if_xo_const_message=>error
      THEN abap_off
      ELSE abap_on ) .

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~enqueue_employee.

    DATA:
      return TYPE bapireturn1 .

    IF ( number IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = number
      IMPORTING
        return = return.

    result = COND #(
      WHEN return-type EQ if_xo_const_message=>error
      THEN abap_off
      ELSE abap_on ) .

    me->yif_hcm_elo_data_maintenance~return_message = return .


  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~get_message .

    result = yif_hcm_elo_data_maintenance~return_message .

  ENDMETHOD .


  METHOD yif_hcm_elo_data_maintenance~maintains_infotypes .

    DATA:
      key TYPE bapipakey .

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty     = infty
        number    = number
        subtype   = subtype
        record    = record
        operation = operation
      IMPORTING
        return    = result
        key       = key.

  ENDMETHOD .


  METHOD yif_hcm_elo_data_maintenance~maintains_infotype_2001.

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(messsage) =
      me->yif_hcm_elo_data_maintenance~maintains_infotypes(
        infty     = me->infotypes-ausencias
        number    = data-pernr
        subtype   = me->ausencias-doenca
        record    = data
        operation = me->operation-criar ) .

    " Nao se retorna mensagem quando nao se tem erro
    IF ( messsage IS INITIAL ) .
      " 009 Infotipo 2001 processado com sucesso.
      messsage = VALUE bapireturn1(
        type   = if_xo_const_message=>success
        id     = ycl_hcm_application_log=>message-id
        number = 009 ) .
    ENDIF .

    me->yif_hcm_elo_data_maintenance~set_message( messsage ).

    IF ( messsage-type EQ if_xo_const_message=>error ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

    result = messsage .

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~maintains_infotype_2002.

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(messsage) =
      me->yif_hcm_elo_data_maintenance~maintains_infotypes(
        infty     = me->infotypes-presencas
        number    = data-pernr
        subtype   = data-subty
        record    = data
        operation = me->operation-criar ) .

    " Nao se retorna mensagem quando nao se tem erro
    IF ( messsage IS INITIAL ) .
      messsage = VALUE bapireturn1(
        type       = if_xo_const_message=>success
        id         = 'PG'
        number     = 102 ) .
    ENDIF .

    me->yif_hcm_elo_data_maintenance~set_message( messsage ).
    result = messsage .

    IF ( messsage-type EQ if_xo_const_message=>error ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~maintains_infotype_2010.

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(messsage) =
      me->yif_hcm_elo_data_maintenance~maintains_infotypes(
        infty     = me->infotypes-infos_remun_emp
        number    = data-pernr
        subtype   = data-subty
        record    = data
        operation = me->operation-criar ) .

    " Nao se retorna mensagem quando nao se tem erro
    IF ( messsage IS INITIAL ) .
      messsage = VALUE bapireturn1(
        type       = if_xo_const_message=>success
        id         = 'PG'
        number     = 102 ) .
    ENDIF .

    me->yif_hcm_elo_data_maintenance~set_message( messsage ).
    result = messsage .

    IF ( messsage-type EQ if_xo_const_message=>error ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~read_infotype_0001 .

    DATA:
      infty_tab TYPE p0001_tab .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = data
        infty           = me->infotypes-mestre
        begda           = sy-datum
        bypass_buffer   = abap_on
      TABLES
        infty_tab       = infty_tab
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.

    IF (  sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    result = VALUE #( infty_tab[ 1 ] OPTIONAL ) .

  ENDMETHOD .


  METHOD yif_hcm_elo_data_maintenance~save_log .

    DATA:
      lc_object    TYPE bal_s_log-object VALUE 'YHCM',
      lc_subobject TYPE bal_s_log-subobject VALUE 'YHCM0001',
      lc_program   TYPE balprog VALUE 'YHCM0012'.

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(header) =
      VALUE ycl_hcm_application_log=>ty_header_log(
        title     = |{ data-gjahr }.{ data-id }.{ data-pernr }|
        object    = lc_object
        subobject = lc_subobject
        alprog    = lc_program ) .

    DATA(message) = me->yif_hcm_elo_data_maintenance~get_message( ) .

    DATA(log_number) =
      ycl_hcm_application_log=>save_single(
        is_header  = header
        is_message = message ) .

    DATA(log_data) = VALUE ythcm0002(
        gjahr     = data-gjahr
        id        = data-id
        lognumber = log_number
        credat    = sy-datum
        cretim    = sy-uzeit
        crenam    = sy-uname ) .

    MODIFY ythcm0002 FROM log_data .

    result = log_number .

  ENDMETHOD .


  METHOD yif_hcm_elo_data_maintenance~set_message .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    me->yif_hcm_elo_data_maintenance~return_message = data .
  ENDMETHOD .
ENDCLASS.