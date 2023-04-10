CLASS ycl_hcm_elo_maintenance DEFINITION
  PUBLIC
  INHERITING FROM ycl_hcm_elo_infotypes
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS mainten_2001
      IMPORTING
        !is_data      TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE bapiret1 .


  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS prepare_2001
      IMPORTING
        !is_data      TYPE yhcm00009
      RETURNING
        VALUE(result) TYPE p2001 .

ENDCLASS.



CLASS ycl_hcm_elo_maintenance IMPLEMENTATION.



  METHOD mainten_2001 .

    IF ( is_data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(prepared_data) = me->prepare_2001( is_data ) .
    IF ( prepared_data IS INITIAL ) .
      RETURN .
    ENDIF .

    result =
      me->yif_hcm_elo_data_maintenance~maintains_infotype_2001( prepared_data ) .

  ENDMETHOD .


  METHOD prepare_2001 .
    DATA:
      number  TYPE pernr_d VALUE '00001169',
      infty   TYPE infty VALUE '2001', " Ausências
      subtype TYPE subty VALUE '1004'. " Doença (consulta médica)

    result = VALUE p2001(
        pernr = number
        infty = infty
        subty = subtype
        endda = sy-datum
        begda = sy-datum - 1
    ).

  ENDMETHOD .


ENDCLASS.
