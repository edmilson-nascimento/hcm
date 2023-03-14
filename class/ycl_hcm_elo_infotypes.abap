CLASS ycl_hcm_elo_infotypes DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_hcm_elo_data_maintenance .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_hcm_elo_infotypes IMPLEMENTATION.

  METHOD yif_hcm_elo_data_maintenance~enqueue_employee.

    DATA:
      return_message TYPE bapiret1 .

    IF ( number IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = number
      IMPORTING
        return = return_message.

    result = COND #(
      WHEN return_message-type EQ if_xo_const_message=>error
      THEN abap_off
      ELSE abap_on ) .

  ENDMETHOD.


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


  METHOD yif_hcm_elo_data_maintenance~maintains_infotypes .
  ENDMETHOD .


  METHOD yif_hcm_elo_data_maintenance~maintains_infotype_2001.

    IF ( data IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(record) = data .

*COP Copiar
*DEL Eliminar
*DIS Exibir
*EDQ Bloquear/desbloquear
*INS Criar
*LIS9  Delimitar
*MOD Modificar
*INSS  Criar c/medidas não há redifinição p/modificar

    DATA:
      operation TYPE actio VALUE `INS`, " Criar
      infty     TYPE infty VALUE '2001', " Ausências
      subtype   TYPE subty VALUE '1004', " Doença (consulta médica)

      return    TYPE bapireturn1,
      key       TYPE bapipakey.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = record-pernr
      IMPORTING
        return = return.
    IF ( return-type = if_xo_const_message=>error ) .
      RETURN .
    ENDIF .


    CLEAR return .

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty     = infty
        number    = record-pernr
        subtype   = subtype
        record    = record
        operation = operation
      IMPORTING
        return    = return
        key       = key.

    IF ( return-type = if_xo_const_message=>error ) .
      RETURN .
    ENDIF .


    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

    CLEAR return .

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = record-pernr
      IMPORTING
        return = return.

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~save_log .
  ENDMETHOD .


ENDCLASS.
