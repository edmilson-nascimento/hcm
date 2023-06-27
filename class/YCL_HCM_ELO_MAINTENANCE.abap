CLASS ycl_hcm_elo_maintenance DEFINITION
  PUBLIC
  INHERITING FROM ycl_hcm_elo_infotypes
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2001</p>
    METHODS mainten_2001
      IMPORTING
        !data         TYPE yhcm00009
        !no_commit    TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2002</p>
    METHODS mainten_2002
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
        !no_commit    TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2010</p>
    METHODS mainten_2010
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
        !no_commit    TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2001(via job)</p>
    CLASS-METHODS mainten_2001_imported
      IMPORTING
        !header       TYPE ythcm0001_t
        !impored_type TYPE ythcm0003_t
        !no_commit    TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2002(via job)</p>
    CLASS-METHODS mainten_2002_imported
      IMPORTING
        !header        TYPE ythcm0001_t
        !imported_type TYPE ythcm0003_t
        !no_commit     TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result)  TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Verifica dados para infotipo 2010(via job)</p>
    CLASS-METHODS check_2010_imported
      CHANGING
        !ch_header        TYPE ythcm0001_t
        !ch_imported_type TYPE ythcm0003_t
        !ch_log           TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2010(via job)</p>
    CLASS-METHODS mainten_2010_imported
      IMPORTING
        !header        TYPE ythcm0001_t
        !imported_type TYPE ythcm0003_t
        !no_commit     TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result)  TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de conexao</p>
    METHODS get_user_access
      RETURNING
        VALUE(result) TYPE zwsuser .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0000(via job)</p>
    METHODS read_0000
      IMPORTING
        !data         TYPE p0000-pernr
      RETURNING
        VALUE(result) TYPE p0000 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0001(via job)</p>
    METHODS read_0001
      IMPORTING
        !data         TYPE p0001-pernr
      RETURNING
        VALUE(result) TYPE p0001 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0002(via job)</p>
    METHODS read_0002
      IMPORTING
        !data         TYPE p0002-pernr
      RETURNING
        VALUE(result) TYPE p0002 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0105(via job)</p>
    METHODS read_0105
      IMPORTING
        !data         TYPE p0000-pernr
      RETURNING
        VALUE(result) TYPE p0105 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0337(via job)</p>
    METHODS read_0337
      IMPORTING
        !data         TYPE p0000-pernr
      RETURNING
        VALUE(result) TYPE p0337 .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 2010</p>
    METHODS read_2010
      IMPORTING
        !im_pernr     TYPE p0000-pernr
        !im_begda     TYPE p0000-begda
        !im_endda     TYPE p0000-endda
      RETURNING
        VALUE(result) TYPE p2010_tab .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      message_id TYPE bapiret2-id VALUE 'ZHCM'.

    "! <p class="shorttext synchronized" lang="pt">Retorna dados preparados para o tipo YHCM00009</p>
    METHODS merge_data_imported
      IMPORTING
        !header       TYPE ythcm0001_t
        !impored_type TYPE ythcm0003_t
      RETURNING
        VALUE(result) TYPE yhcm00009_t .

    "! <p class="shorttext synchronized" lang="pt">Retorna dados preparados para o tipo P2001</p>
    METHODS prepare_2001
      IMPORTING
        !data         TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE p2001 .

    METHODS prepare_2002
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
      RETURNING
        VALUE(result) TYPE p2002 .

    METHODS prepare_2010
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
      RETURNING
        VALUE(result) TYPE p2010 .

    METHODS mainten_x
      IMPORTING
        !type          TYPE yhcm00009-type
        !record        TYPE any
        !imported_data TYPE yhcm00009
        !no_commit     TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result)  TYPE bapiret2_t .

    METHODS update_record_status
      IMPORTING
        !data TYPE yhcm00009
        !log  TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Retorna o registro da tabela YTHCM0003</p>
    METHODS get_imported_data
      IMPORTING
        !data         TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE ythcm0003  .

    "! <p class="shorttext synchronized" lang="pt">Manter infoTypes</p>
    METHODS maintains_infotypes
      IMPORTING
        !data         TYPE any
        !no_commit    TYPE sap_bool DEFAULT 'X'
      RETURNING
        VALUE(result) TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Mapear tipo de mensagem (BAPIRETURN1 to BAPIRE2_T)</p>
    METHODS map_msg
      IMPORTING
        !data         TYPE bapireturn1
      RETURNING
        VALUE(result) TYPE bapiret2_t .

    "! <p class="shorttext synchronized" lang="pt">Verificar se existe colisão para o item 2010</p>
    CLASS-METHODS is_collision
      IMPORTING
        !data         TYPE ythcm0003
      RETURNING
        VALUE(result) TYPE sap_bool .

ENDCLASS.



CLASS ycl_hcm_elo_maintenance IMPLEMENTATION.


  METHOD mainten_2001 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(prepared_data) = me->prepare_2001( data ) .
    IF ( prepared_data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->mainten_x( type          = ycl_hcm_elo_infotypes=>infotypes-ausencias
                     record        = prepared_data
                     imported_data = data
                     no_commit     = no_commit ) .

  ENDMETHOD .


  METHOD mainten_2002 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(prepared_data) = me->prepare_2002( data      = data
                                            aditional = aditional ) .

    IF ( prepared_data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->mainten_x( type          = ycl_hcm_elo_infotypes=>infotypes-presencas
                     record        = prepared_data
                     imported_data = data
                     no_commit     = no_commit ) .

  ENDMETHOD .


  METHOD mainten_2010 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(prepared_data) = me->prepare_2010( data      = data
                                            aditional = aditional ) .
    IF ( prepared_data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->mainten_x( type          = ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp
                     record        = prepared_data
                     imported_data = data
                     no_commit     = no_commit ) .

  ENDMETHOD .


  METHOD mainten_2001_imported .

    IF ( lines( header ) EQ 0 ) .
      RETURN .
    ENDIF .

    IF ( lines( impored_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(data) =
       NEW ycl_hcm_elo_maintenance( )->merge_data_imported(
         EXPORTING
           header       = header
           impored_type = impored_type ) .

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .
      DATA(log) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2001( data      = item
                                                      no_commit = no_commit ) .
      result = VALUE #( BASE result ( LINES OF log ) ) .
    ENDLOOP .

    " Caso seja colisão, deve ser tratado como INFO
    LOOP AT result ASSIGNING FIELD-SYMBOL(<log>)
      WHERE type   EQ if_xo_const_message=>error
        AND id     EQ 'PG'
        AND number EQ 080 .
      <log>-type = if_xo_const_message=>info .
    ENDLOOP .

  ENDMETHOD.


  METHOD mainten_2002_imported .

    IF ( lines( header ) EQ 0 ) OR
       ( lines( imported_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(data) =
       NEW ycl_hcm_elo_maintenance( )->merge_data_imported(
         EXPORTING
           header       = header
           impored_type = imported_type
       ).

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .
      DATA(aditional) = VALUE #( imported_type[ gjahr = item-gjahr
                                                id    = item-id ] OPTIONAL ).
      DATA(log) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2002( data      = item
                                                      aditional = aditional
                                                      no_commit = no_commit ) .

      result = VALUE #( BASE result ( LINES OF log ) ) .

    ENDLOOP .

  ENDMETHOD.


  METHOD check_2010_imported .

    DATA:
      lt_header        TYPE ythcm0001_t,
      ls_header        TYPE ythcm0001,
      lt_imported_type TYPE ythcm0003_t,
      ls_message       TYPE bapiret2.


    IF ( lines( ch_header ) EQ 0 ) OR
       ( lines( ch_imported_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT ch_imported_type INTO DATA(ls_imported) .

      IF ( is_collision( ls_imported ) EQ abap_true ) .

        " mensagem de colisão
        IF ( 0 EQ 1 ). MESSAGE i025(zhcm). ENDIF .
        "Registro ja salvo (colisão). Item será ignorado. (Nro &, IT &)
        ls_message = VALUE bapiret2(
          type       = if_xo_const_message=>info
          id         = message_id
          number     = 025
          message_v1 = |{ ls_imported-employee_code ALPHA = OUT }|
          message_v2 = |{ ls_imported-salary_code }| ) .

        " Armazenando nova mensagem
        ch_log =  VALUE #( BASE ch_log ( ls_message ) ).

        CONTINUE .
      ENDIF .

      APPEND ls_imported TO lt_imported_type .
      ls_header = VALUE #( ch_header[ id = ls_imported-id ] OPTIONAL ) .
      IF ( ls_header IS NOT INITIAL ) .
        APPEND ls_header TO lt_header .
        CLEAR ls_header .
      ENDIF .

    ENDLOOP .

    ch_header        = lt_header .
    ch_imported_type = lt_imported_type .

  ENDMETHOD .


  METHOD mainten_2010_imported .

    IF ( lines( header ) EQ 0 ) .
      RETURN .
    ENDIF .

    IF ( lines( imported_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(data) =
       NEW ycl_hcm_elo_maintenance( )->merge_data_imported(
         EXPORTING
           header       = header
           impored_type = imported_type
       ).

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .

      DATA(aditional) = VALUE #( imported_type[ gjahr = item-gjahr
                                                id    = item-id ] OPTIONAL ).
      DATA(log) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2010( data      = item
                                                      aditional = aditional
                                                      no_commit = no_commit ) .

      result = VALUE #( BASE result ( LINES OF log ) ) .

    ENDLOOP .

  ENDMETHOD.


  METHOD get_user_access .

    result = me->yif_hcm_elo_data_maintenance~get_user_access( ) .

  ENDMETHOD .


  METHOD read_0000 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_0000( data ) .

  ENDMETHOD .


  METHOD read_0001 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_0001( data ) .

  ENDMETHOD .


  METHOD read_0002 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_0002( data ) .

  ENDMETHOD .


  METHOD read_0105 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_0105( data ) .

  ENDMETHOD .


  METHOD read_0337 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_0337( data ) .

  ENDMETHOD .


  METHOD read_2010 .

    IF ( im_pernr IS INITIAL ) OR
       ( im_begda IS INITIAL ) OR
       ( im_endda IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~read_infotype_2010(
        im_pernr = im_pernr
        im_begda = im_begda
        im_endda = im_endda
      ) .

  ENDMETHOD .


  METHOD mainten_x .

    CONSTANTS:
      BEGIN OF messages,
        " 024 Pessoa & bloqueada para processamento. Registro não processado.
        user_locked TYPE bapiret2-number VALUE 024,
      END OF messages .

    IF ( record IS INITIAL ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE record TO FIELD-SYMBOL(<pernr>).
    IF ( <pernr> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    IF ( me->yif_hcm_elo_data_maintenance~enqueue_employee( <pernr> ) EQ abap_false ) .

      result = VALUE #(
        (
          type       = if_xo_const_message=>error
          id         = ycl_hcm_application_log=>message-id
          number     = messages-user_locked
          message_v1 = <pernr>
        )
      ) .

      RETURN .

    ENDIF .

    result = me->maintains_infotypes( data      = record
                                      no_commit = no_commit ) .

    me->yif_hcm_elo_data_maintenance~dequeue_employee( <pernr> ) .

    me->update_record_status( data = imported_data
                              log  = result ) .

    ycl_hcm_application_log=>save_imported(
      pernr = <pernr>
      gjahr = imported_data-gjahr
      id    = imported_data-id
      msg   = result
    ).

  ENDMETHOD .


  METHOD merge_data_imported .

    IF ( lines( header ) EQ 0 ) .
      RETURN .
    ENDIF .

    IF ( lines( impored_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    result = VALUE yhcm00009_t(
      FOR i IN impored_type
      FOR h IN header FROM line_index( header[ id    = i-id
                                               gjahr = i-gjahr ] )
         WHERE ( id    = i-id AND
                 gjahr = i-gjahr )
        ( id          = h-id
          gjahr       = h-gjahr
          bukrs       = h-bukrs
          pernr       = h-pernr
*         ename       = h-ename
          direct      = h-direct
          code_type   = i-code_type
          type        = h-type
          subtype     = h-subtype
          status      = h-status
          start_date  = i-start_date
          start_time  = i-start_time
          end_date    = i-end_date
          end_time    = i-end_time
          credat      = i-credat
          cretim      = i-cretim
          crenam      = i-crenam
       )
    ) .


  ENDMETHOD .


  METHOD prepare_2001 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(record) = me->get_imported_data( data ) .
    IF ( record IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(lt_ausencias) = ycl_hcm_elo_importation=>get_group_absence( ) .

    " Verificando se deve ser considerado todo o dia
    IF ( line_exists( lt_ausencias[ table_line = record-salary_code ] ) ) .

      result = VALUE p2001(
        pernr = record-employee_code
        infty = infotypes-ausencias
        subty = CONV subty( record-salary_code )
        endda = record-end_date
        begda = record-start_date
        alldf = abap_on
        uname = sy-uname
      ) .

    ELSE .

      result = VALUE p2001(
        pernr = record-employee_code
        infty = infotypes-ausencias
        subty = CONV subty( record-salary_code )
        endda = record-end_date
        begda = record-start_date
        uname = sy-uname
        beguz = record-start_time
        enduz = record-end_time
      ) .

    ENDIF .


  ENDMETHOD .


  METHOD prepare_2002 .

    result = VALUE p2002(
      pernr = data-pernr
      infty = infotypes-presencas
      subty = data-subtype
      endda = aditional-end_date
      begda = aditional-start_date
      stdaz = aditional-duration
    ) .

  ENDMETHOD .


  METHOD prepare_2010 .

    result = VALUE p2010(
      pernr = data-pernr
      infty = ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp
      subty = data-subtype
      stdaz = aditional-duration
      endda = aditional-end_date
      begda = aditional-start_date
    ) .

  ENDMETHOD .


  METHOD update_record_status .

    CONSTANTS:
      lc_next_step_ok    TYPE ythcm0001-status VALUE '11', " Processado com sucesso
      lc_next_step_error TYPE ythcm0001-status VALUE '12'. " Processado com erro

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(msgty) = COND #( WHEN line_exists( log[ type = if_xo_const_message=>error ] )
                          THEN if_xo_const_message=>error
                          ELSE if_xo_const_message=>success ) .

    DATA(record) = CORRESPONDING ythcm0001( data ) .

    record-status = COND #( WHEN msgty EQ if_xo_const_message=>error
                            THEN lc_next_step_error
                            ELSE lc_next_step_ok ) .

    MODIFY ythcm0001 FROM record .

  ENDMETHOD .


  METHOD get_imported_data .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    SELECT SINGLE *
      FROM ythcm0003
     WHERE id    EQ @data-id
       AND gjahr EQ @data-gjahr
      INTO @result .

  ENDMETHOD .

  METHOD maintains_infotypes .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT 'INFTY' OF STRUCTURE data
    TO FIELD-SYMBOL(<infty>).
    IF ( <infty> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE data
    TO FIELD-SYMBOL(<pernr>).
    IF ( <pernr> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE data
    TO FIELD-SYMBOL(<subty>).
    IF ( <subty> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    DATA(message) =
      me->yif_hcm_elo_data_maintenance~maintains_infotypes(
        infty     = <infty>
        number    = <pernr>
        subtype   = <subty>
        record    = data
        operation = me->operation-criar
        no_commit = no_commit ) .

    result = me->map_msg( message ) .

  ENDMETHOD .


  METHOD map_msg .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result = VALUE #(
      ( type       = data-type
        id         = data-id
        number     = data-number
        message    = data-message
        log_no     = data-log_no
        log_msg_no = data-log_msg_no
        message_v1 = data-message_v1
        message_v2 = data-message_v2
        message_v3 = data-message_v3
        message_v4 = data-message_v4
      )
    ) .

  ENDMETHOD .


  METHOD is_collision .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(record_2010) = NEW ycl_hcm_elo_maintenance( )->read_2010(
      im_pernr = data-employee_code
      im_begda = data-start_date
      im_endda = data-end_date
    ) .

    " Não existe outro registro no mesmo periodo
    IF ( lines( record_2010 ) EQ 0 ) .
      RETURN .
    ENDIF .

    result = abap_on .

  ENDMETHOD .


ENDCLASS.