*&---------------------------------------------------------------------*
*& Report ztest
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest.


DATA:
  pernr  TYPE pernr_d VALUE '00001009',
  infty  TYPE infty VALUE '2010',
  subty  TYPE subty VALUE '7001',
  return TYPE bapireturn1,
  key    TYPE bapipakey.

DATA(record_2010) = VALUE p2010(
  pernr = pernr
  infty = infty
  subty = subty
  stdaz = '4.44'
* endda = ( sy-datum - 6 )
  begda = ( sy-datum - 2 )
) .

CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
  EXPORTING
    number = pernr
  IMPORTING
    return = return.

IF ( return-type EQ if_xo_const_message=>error ) .
  WRITE:/ 'Bloqueado' .
  RETURN .
ENDIF .

CLEAR return .

CALL FUNCTION 'HR_INFOTYPE_OPERATION'
  EXPORTING
    infty       = infty
    number      = pernr
    subtype     = subty
*   validityend = record_2010-begda
*   validitybegin = record_2010-begda
    record      = record_2010
    operation   = 'INS'
    nocommit    = abap_false
*   dialog_mode = '2' " Exibir o SHDB em tela
  IMPORTING
    return      = return
    key         = key.

WRITE:/ 'return: ', return.
WRITE:/ 'key: ', key.
WRITE:/ '.' .

WRITE:/ 'EMPLOYEENO'   , 15 key-employeeno .
WRITE:/ 'SUBTYPE'      , 15 key-subtype .
WRITE:/ 'OBJECTID'     , 15 key-objectid .
WRITE:/ 'LOCKINDIC'    , 15 key-lockindic .
WRITE:/ 'VALIDEND'     , 15 key-validend .
WRITE:/ 'VALIDBEGIN'   , 15 key-validbegin .
WRITE:/ 'RECORDNR '    , 15 key-recordnr .

*KEY-VALIDEND

CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
  EXPORTING
    number = pernr.