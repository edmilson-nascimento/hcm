CLASS ycl_hcm_elo_maintenance DEFINITION
  PUBLIC
  INHERITING FROM ycl_hcm_elo_infotypes
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2001</p>
    METHODS mainten_2001
      IMPORTING
        !data         TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE bapireturn1 .
    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2002</p>
    METHODS mainten_2002
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
      RETURNING
        VALUE(result) TYPE bapireturn1 .
    "! <p class="shorttext synchronized" lang="pt">Mantem dado para infotipo 2010</p>
    METHODS mainten_2010
      IMPORTING
        !data         TYPE yhcm00009
        !aditional    TYPE ythcm0003
      RETURNING
        VALUE(result) TYPE bapireturn1 .
    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2001(via job)</p>
    CLASS-METHODS mainten_2001_imported
      IMPORTING
        !header       TYPE ythcm0001_t
        !impored_type TYPE ythcm0003_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2002(via job)</p>
    CLASS-METHODS mainten_2002_imported
      IMPORTING
        !header       TYPE ythcm0001_t
        !impored_type TYPE ythcm0003_t .
    "! <p class="shorttext synchronized" lang="pt">Mantem dados para infotipo 2010(via job)</p>
    CLASS-METHODS mainten_2010_imported
      IMPORTING
        !header       TYPE ythcm0001_t
        !impored_type TYPE ythcm0003_t .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados para infotipo 0001(via job)</p>
    METHODS read_0001
      IMPORTING
        !data         TYPE p0001-pernr
      RETURNING
        VALUE(result) TYPE p0001 .

  PROTECTED SECTION.

  PRIVATE SECTION.

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
      RETURNING
        VALUE(result)  TYPE bapireturn1  .

    METHODS update_record_status
      IMPORTING
        !data TYPE yhcm00009 .

    "! <p class="shorttext synchronized" lang="pt">Retorna o registro da tabela YTHCM0003</p>
    METHODS get_imported_data
      IMPORTING
        !data         TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE ythcm0003  .

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
                     imported_data = data ) .

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
                     imported_data = data ) .

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
                     imported_data = data ) .

  ENDMETHOD .


  METHOD mainten_2001_imported .

    DATA:
      messages TYPE bapiret2_t .

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
           impored_type = impored_type
       ).

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .
      DATA(return) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2001( item ) .
      messages = VALUE #( BASE messages ( CORRESPONDING #( return ) ) ) .
    ENDLOOP .

  ENDMETHOD.


  METHOD mainten_2002_imported .

    DATA:
      messages TYPE bapiret2_t .

    IF ( lines( header ) EQ 0 ) OR
       ( lines( impored_type ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(data) =
       NEW ycl_hcm_elo_maintenance( )->merge_data_imported(
         EXPORTING
           header       = header
           impored_type = impored_type
       ).

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .
      DATA(aditional) = VALUE #( impored_type[ gjahr = item-gjahr
                                               id    = item-id ] OPTIONAL ).
      DATA(return) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2002( data      = item
                                                      aditional = aditional ) .
      messages = VALUE #( BASE messages ( CORRESPONDING #( return ) ) ) .
    ENDLOOP .

  ENDMETHOD.


  METHOD mainten_2010_imported .

    DATA:
      messages TYPE bapiret2_t .

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
           impored_type = impored_type
       ).

    IF ( lines( data ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT data INTO DATA(item) .

      DATA(aditional) = VALUE #( impored_type[ gjahr = item-gjahr
                                               id    = item-id ] OPTIONAL ).
      DATA(return) =
        NEW ycl_hcm_elo_maintenance( )->mainten_2010( data      = item
                                                      aditional = aditional ) .
      messages = VALUE #( BASE messages ( CORRESPONDING #( return ) ) ) .
    ENDLOOP .

  ENDMETHOD.


  METHOD read_0001 .

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    result = me->yif_hcm_elo_data_maintenance~read_infotype_0001( data ).

  ENDMETHOD .


  METHOD mainten_x .

    IF ( record IS INITIAL ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE record TO FIELD-SYMBOL(<pernr>).
    IF ( <pernr> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    IF ( me->yif_hcm_elo_data_maintenance~enqueue_employee( <pernr> ) EQ abap_false ) .
      me->yif_hcm_elo_data_maintenance~save_log( record ).
      RETURN .
    ENDIF .

    " Processando o infotype correspondente
    CASE type .

      WHEN infotypes-ausencias .
        result =
          me->yif_hcm_elo_data_maintenance~maintains_infotype_2001( record ) .
      WHEN infotypes-presencas .
        result =
          me->yif_hcm_elo_data_maintenance~maintains_infotype_2002( record ) .
      WHEN infotypes-infos_remun_emp .
        result =
          me->yif_hcm_elo_data_maintenance~maintains_infotype_2010( record ) .

      WHEN OTHERS .

    ENDCASE .

    me->yif_hcm_elo_data_maintenance~dequeue_employee( <pernr> ) .

    IF ( me->yif_hcm_elo_data_maintenance~save_log( imported_data ) ) .
    ENDIF .

    me->update_record_status( imported_data ) .

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
          ename       = h-pernr
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
          chadat      = i-chadat
          chatim      = i-chatim
          chanam      = i-chanam
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

  ENDMETHOD .


  METHOD prepare_2002 .

    result = VALUE p2002(
      pernr = data-pernr
      infty = infotypes-presencas
      subty = data-subtype
      endda = sy-datum
      begda = sy-datum
      stdaz = aditional-duration
    ) .

  ENDMETHOD .


  METHOD prepare_2010 .

    result = VALUE p2010(
      pernr = data-pernr
      infty = ycl_hcm_elo_infotypes=>infotypes-infos_remun_emp
      subty = data-subtype
      endda = sy-datum
      begda = sy-datum
      stdaz = aditional-duration
    ) .

  ENDMETHOD .


  METHOD update_record_status .

    CONSTANTS:
      lc_next_step_ok    TYPE ythcm0001-status VALUE '11', " Processado com sucesso
      lc_next_step_error TYPE ythcm0001-status VALUE '12'. " Processado com erro

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(message_log) = me->yif_hcm_elo_data_maintenance~get_message( ) .

    DATA(msgty) = COND #( WHEN message_log-type EQ if_xo_const_message=>error
                          THEN if_xo_const_message=>error
                          ELSE if_xo_const_message=>success ) .


    DATA(record) = CORRESPONDING ythcm0001( data ) .

    record-status = COND #( WHEN msgty EQ if_xo_const_message=>error
                            THEN lc_next_step_error
                            ELSE lc_next_step_ok ) .

    record-chadat    = sy-datum .
    record-chatim    = sy-uzeit .
    record-chanam    = sy-uname .

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


ENDCLASS.