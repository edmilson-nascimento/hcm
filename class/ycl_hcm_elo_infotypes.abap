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

  ENDMETHOD.


  METHOD yif_hcm_elo_data_maintenance~save_log .
  ENDMETHOD .


ENDCLASS.
