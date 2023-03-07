*COP Copiar
*DEL Eliminar
*DIS Exibir
*EDQ Bloquear/desbloquear
*INS Criar
*LIS9  Delimitar
*MOD Modificar
*INSS  Criar c/medidas não há redifinição p/modificar

DATA:
  number    TYPE pernr_d VALUE '00001169',
  infty     TYPE infty value '2001', " Ausências
  subtype   TYPE subty value '1004', " Doença (consulta médica)

  record    type P2001,
  operation type actio value `INS`, " Criar

  return    TYPE bapireturn1,
  key       type bapipakey.

CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
  EXPORTING
    number = number
  IMPORTING
    return = return.
IF ( return-type = if_xo_const_message=>error ) .
  RETURN .
ENDIF .

record = value P2001(
    PERNR = number
    INFTY = infty
    SUBTY = subtype
*    OBJPS =
*    SPRPS =
    ENDDA = sy-datum
    BEGDA = sy-datum - 1
*    SEQNR =
*    AEDTM =
*    UNAME =
*    HISTO =
*    ITXEX =
*    REFEX =
*    ORDEX =
*    ITBLD =
*    PREAS =
*    FLAG1 =
*    FLAG2 =
*    FLAG3 =
*    FLAG4 =
*    RESE1 =
*    RESE2 =
*    GRPVL =
*    LGART =
*    OPKEN =
*    BETRG =
*    WAERS =
*    ANZHL =
*    ZEINH =
*    INDBW =
*    ZUORD =
*    ESTDT =
*    PABRJ =
*    PABRP =
*    UWDAT =
*    ITFTT =
*    PSKEY =
*    PSHD1 =
).

clear return .

CALL FUNCTION 'HR_INFOTYPE_OPERATION'
  EXPORTING
    infty     = infty
    number    = number
    subtype   = subtype
*   objectid  =
*   lockindicator    =
*   validityend      =
*   validitybegin    =
*   recordnumber     =
    record    = record
    operation = operation
*   tclas     = 'A'
*   dialog_mode      = '0'
*   nocommit  =
*   view_identifier  =
*   secondary_record =
  IMPORTING
    return    = return
    key       = key.


IF ( return-type = if_xo_const_message=>error ) .
  RETURN .
ENDIF .


call function 'BAPI_TRANSACTION_COMMIT'
*  EXPORTING
*    WAIT   =
*  IMPORTING
*    RETURN =
  .

clear return .

CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
  EXPORTING
    number = number
  IMPORTING
    return = return.
