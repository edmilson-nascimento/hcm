METHOD save_log .
    *
    *    DATA:
    *      lc_object    TYPE bal_s_log-object VALUE 'ZHCM',
    *      lc_subobject TYPE bal_s_log-subobject VALUE 'ZHCM0001',
    *      lc_program   TYPE balprog VALUE 'YHCM0012'.
    *
    *    IF ( data IS INITIAL ) .
    *      RETURN .
    *    ENDIF .
    *
    *    DATA(header) =
    *      VALUE ycl_hcm_application_log=>ty_header_log(
    *        title     = |{ data-gjahr }.{ data-id }{ data-pernr }|
    *        object    = lc_object
    *        subobject = lc_subobject
    *        alprog    = lc_program ) .
    *
    *    DATA(log_number) =
    *      ycl_hcm_application_log=>save_all(
    *        is_header  = header
    *        is_message = message ) .
    *
    *    DATA(log_data) = VALUE ythcm0002(
    *        gjahr     = data-gjahr
    *        id        = data-id
    *        lognumber = log_number
    *        credat    = sy-datum
    *        cretim    = sy-uzeit
    *        crenam    = sy-uname ) .
    *
    *    MODIFY ythcm0002 FROM data .
    *
    *    result = log_number .
    
      ENDMETHOD .