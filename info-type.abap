
DATA:
  lr_proxy           TYPE REF TO zco_ielo_web_service,
  logical_port_name  TYPE  prx_logical_port_name VALUE 'ZLP_IELO_WEB_SERVICE',
  input              TYPE zielo_web_service_list_employ1,
  output             TYPE zielo_web_service_list_employe,

  filtering_criteria TYPE zemployee_filtering_criteria.

TRY .

    lr_proxy = NEW zco_ielo_web_service(
*     logical_port_name =
     logical_port_name = logical_port_name
    ) .
  CATCH cx_ai_system_fault.
ENDTRY .

IF ( lr_proxy IS BOUND ) .

  filtering_criteria = VALUE zemployee_filtering_criteria(
*     CONTROLLER            =
*     active                = '0'
      company_code          = 'DESCONTAO'
*      EMPLOYEE_CODES        =
*      EMPLOYEE_FILTER       =
*      END_ACTIVITY_FROM     =
*      END_ACTIVITY_UNTIL    =
*      END_CONTRACT_FROM     =
*      END_CONTRACT_UNTIL    =
*      EXPORT_ALL_FIELDS     =
*      INCLUDE_DESCRIPTIONS  =
*      ONLY_COMPUTE_TIME_RCS =
*      RETURN_FIELDS         =
*      START_ACTIVITY_FROM   =
*      START_ACTIVITY_UNTIL  =
*      START_CONTRACT_FROM   =
*      START_CONTRACT_UNTIL  =
  ).

  input = VALUE zielo_web_service_list_employ1(
*     CONTROLLER         =
*     EMPLOYEES_INFO     =
     filtering_criteria = filtering_criteria
*     USER               =
  ) .


  TRY .
      lr_proxy->list_employees(
        EXPORTING
          input  = input
        IMPORTING
          output = output
      ).

    CATCH cx_ai_system_fault INTO DATA(lo_error).

      WRITE:/ lo_error->errortext .

  ENDTRY .

*      WRITE:/ 'Records: ', lines( output-list_employees_result ) .

ENDIF .




































