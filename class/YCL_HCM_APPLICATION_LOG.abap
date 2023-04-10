
CLASS ycl_hcm_application_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_header_log,
        title     TYPE bal_s_log-extnumber,
        object    TYPE bal_s_log-object,
        subobject TYPE bal_s_log-subobject,
        alprog    TYPE bal_s_log-alprog,
        handle    TYPE balloghndl,
        lognumber TYPE balognr,
      END OF ty_header_log .

    METHODS constructor
      IMPORTING
        !iv_title     TYPE bal_s_log-extnumber
        !iv_object    TYPE bal_s_log-object    DEFAULT 'ZHCM'
        !iv_subobject TYPE bal_s_log-subobject DEFAULT 'ZHCM0001'
        !iv_alprog    TYPE bal_s_log-alprog .

    METHODS add
      IMPORTING
        !is_message TYPE bal_s_msg .

    METHODS add_bapiret1
      IMPORTING
        !is_message TYPE bapiret1 .

    METHODS add_bapiret2
      IMPORTING
        !is_message TYPE bapiret2 .

    METHODS save .

    METHODS show .

    CLASS-METHODS show_saved
      IMPORTING
        !log_numbers TYPE bal_t_logn
        !option       TYPE i DEFAULT 0 .

    METHODS get_lognumber
      RETURNING
        VALUE(result) TYPE balognr .

    CLASS-METHODS get_handle_saved
      IMPORTING
        !iv_lognumber TYPE balhdr-lognumber
      RETURNING
        VALUE(result) TYPE balhdr-log_handle .

    CLASS-METHODS get_messages_saved
      IMPORTING
        !iv_lognumber TYPE balhdr-lognumber
      RETURNING
        VALUE(result) TYPE balm_t .

    CLASS-METHODS save_all
      IMPORTING
        !is_header    TYPE ty_header_log
        !is_message   TYPE bapiret1
      RETURNING
        VALUE(result) TYPE balognr .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      gs_header TYPE ty_header_log .

    METHODS create .

    METHODS set_lognumber
      IMPORTING
        !iv_lognumber TYPE balognr .

ENDCLASS.


CLASS ycl_hcm_application_log IMPLEMENTATION.


  METHOD constructor .

    me->gs_header = VALUE ty_header_log(
      title     = iv_title
      object    = iv_object
      subobject = iv_subobject
      alprog    = iv_alprog
    ).

    me->create( ) .

  ENDMETHOD.


  METHOD add .

    IF ( me->gs_header-handle IS INITIAL ) .
      RETURN.
    ENDIF .

    IF ( is_message IS INITIAL ) .
      RETURN.
    ENDIF .

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = me->gs_header-handle
        i_s_msg      = is_message.

  ENDMETHOD.


  METHOD add_bapiret1 .

    IF ( is_message IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(bal_message) = VALUE bal_s_msg(
      msgty     = is_message-type
      msgid     = is_message-id
      msgno     = is_message-number
      msgv1     = is_message-message_v1
      msgv2     = is_message-message_v2
      msgv3     = is_message-message_v3
      msgv4     = is_message-message_v4 ) .

    me->add( bal_message ) .

  ENDMETHOD .


  METHOD add_bapiret2 .
  ENDMETHOD .


  METHOD save.

    DATA:
      lt_lognumbers TYPE bal_t_lgnm.

    IF ( me->gs_header-handle IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(lt_handles) = VALUE bal_t_logh( ( me->gs_header-handle ) ).

    CALL FUNCTION 'BAL_DB_SAVE_PREPARE'
      EXPORTING
        i_replace_in_all_logs = abap_on
*       i_t_replace_in_these_logs     =
*       i_t_replace_message_variables =
*       i_t_replace_context_fields    =
      EXCEPTIONS
        log_not_found         = 1
        OTHERS                = 2.

    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .


    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
*       i_in_update_task = space
*       i_save_all       = lv_save_all
        i_t_log_handle   = lt_handles
*       i_2th_connection = space
*       i_2th_connect_commit = space
*       i_link2job       = 'x'
      IMPORTING
        e_new_lognumbers = lt_lognumbers
*       e_second_connection  =
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF ( sy-subrc EQ 0 ) .
      me->gs_header-lognumber = VALUE #(
        lt_lognumbers[ 1 ]-lognumber OPTIONAL ) .
    ENDIF.

  ENDMETHOD.


  METHOD show.

    DATA:
      lt_handles         TYPE bal_t_logh,
      ls_display_profile TYPE bal_s_prof.

    " Get standard display profile
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF ( sy-subrc NE 0 ) .
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_display_profile
        i_t_log_handle      = lt_handles.

  ENDMETHOD.


  METHOD show_saved.

*    DATA:
*      lt_handles         TYPE bal_t_logh,
*      ls_handles         TYPE balloghndl,
*      ls_display_profile TYPE bal_s_prof.
*
*    IF ( handle IS NOT INITIAL  ) .
*
*      CALL FUNCTION 'BAL_LOG_EXIST'
*        EXPORTING
*          i_log_handle  = handle
*        EXCEPTIONS
*          log_not_found = 1
*          OTHERS        = 2.
*
*      IF (  sy-subrc EQ 0 ) .
*
*        APPEND ls_handles TO lt_handles .
*        CLEAR  ls_handles .
*
*      ENDIF.
*
*    ENDIF .
*
*    IF ( lines( handles ) EQ 0  ) .
*    ELSE .
*
*      LOOP AT handles INTO ls_handles .
*
*        CALL FUNCTION 'BAL_LOG_EXIST'
*          EXPORTING
*            i_log_handle  = ls_handles
*          EXCEPTIONS
*            log_not_found = 1
*            OTHERS        = 2.
*
*        IF (  sy-subrc EQ 0 ) .
*
*          APPEND ls_handles TO lt_handles .
*          CLEAR  ls_handles .
*
*        ENDIF.
*
*      ENDLOOP.
*
**     get standard display profile
*      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
*        IMPORTING
*          e_s_display_profile = ls_display_profile
*        EXCEPTIONS
*          OTHERS              = 1.
*      IF ( sy-subrc NE 0 ) .
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*
*      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*        EXPORTING
*          i_s_display_profile  = ls_display_profile
*          i_t_log_handle       = lt_handles
**         i_t_msg_handle       =
**         i_s_log_filter       =
**         i_s_msg_filter       =
**         i_t_log_context_filter        =
**         i_t_msg_context_filter        =
**         i_amodal             = space
**         i_srt_by_timstmp     = space
**         i_msg_context_filter_operator = 'a'
**        importing
**         e_s_exit_command     =
*        EXCEPTIONS
*          profile_inconsistent = 1
*          internal_error       = 2
*          no_data_available    = 3
*          no_authority         = 4
*          OTHERS               = 5.
*
*      IF sy-subrc NE 0 .
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDIF.
*
*    FREE:
*      lt_handles, ls_display_profile .




    DATA:
      t_lognumber         TYPE bal_t_logn,

      e_s_log_filter      TYPE bal_s_lfil,
      l_t_log_header      TYPE balhdr_t,
      l_t_log_handle      TYPE bal_t_logh,
      i_log_handle        TYPE balloghndl,
      read_from_db_hdr(1) TYPE c,
      l_t_log_loaded      TYPE bal_t_logh,
      l_t_locked          TYPE balhdr_t,
      i_s_display_profile TYPE  bal_s_prof,
      l_s_display_profile TYPE bal_s_prof,
*      i_variant_report    TYPE  sy-repid VALUE 'SBAL_DISPLAY',
      number_of_protocols LIKE  sy-dbcnt VALUE 4,
      i_srt_by_timstmp    TYPE  boolean
      .

    t_lognumber = VALUE bal_t_logn( ( '00000000000411051200' ) ) .
    t_lognumber = log_numbers .

*    call function 'BAL_FILTER_CREATE'
*      exporting
**       i_object       = object
**       i_subobject    = subobject
**       i_extnumber    = '00qwPT6L7jgsjQu6zl7SuW'
**       i_aldate_from  = sy-datum
**       i_aldate_to    = sy-datum
**       i_altime_from  = i_altime_from
**       i_altime_to    = i_altime_to
**       i_probclass_from = i_probclass_from
**       i_probclass_to = i_probclass_to
**       i_alprog       = i_alprog
**       i_altcode      = i_altcode
**       i_aluser       = i_aluser
**       i_almode       = i_almode
*        i_t_lognumber  = t_lognumber
*      importing
*        e_s_log_filter = e_s_log_filter.
*
**if ( handle is initial ) .
**e_s_log_filter =
**  value #( log_handle ( handle ) ) ) .
**ENDIF .
**
**E_S_LOG_FILTER-LOG_HANDLE[1]-LOW
*
*    call function 'BAL_DB_SEARCH'
*      exporting
**       i_client           = SY-MANDT
*        i_s_log_filter     = e_s_log_filter
**       i_t_sel_field      = i_t_sel_field
**       i_tzone            = i_tzone
*      importing
*        e_t_log_header     = l_t_log_header
*      exceptions
*        log_not_found      = 1
*        no_filter_criteria = 2
*        others             = 3.
*    if sy-subrc <> 0.
*    endif.



    DATA:
      lt_lognumbers  TYPE szal_lognumbers,
      lt_header_data TYPE TABLE OF balhdr.

*    IF ( iv_lognumber IS NOT INITIAL ) .
*      lt_lognumbers = VALUE #( ( item = iv_lognumber ) ) .
*    ELSE .
*      lt_lognumbers = VALUE #( ( item = '00000000000411051200' ) ) .
*    ENDIF .

    lt_lognumbers = log_numbers .

    CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
*      exporting
*        put_into_memory    = SPACE
*      importing
*        number_of_logs     =
      TABLES
        lognumbers  = lt_lognumbers
        header_data = lt_header_data
*       header_parameters  =
*       messages    =
*       message_parameters =
*       contexts    =
*       t_exceptions       =
      .

    l_t_log_handle = VALUE #( ( lt_header_data[ 1 ]-log_handle ) ) .

*
*    clear l_t_log_handle.
*    "loop at l_t_log_header assigning field-symbol(<l_s_log_header>) .
*    loop at lt_header_data assigning field-symbol(<l_s_log_header>) .
*      call function 'BAL_LOG_EXIST'
*        exporting
*          i_log_handle  = <l_s_log_header>-log_handle
*        exceptions
*          log_not_found = 1.
*      if sy-subrc = 0.
*        insert <l_s_log_header>-log_handle into table l_t_log_handle.
*        delete l_t_log_header.
*      endif.
*    endloop.
*
*
*    call function 'BAL_DB_LOAD'
*      exporting
*        i_t_log_header         = l_t_log_header
*        i_do_not_load_messages = read_from_db_hdr
*        i_lock_handling        = 1
*      importing
*        e_t_log_handle         = l_t_log_loaded
*        e_t_locked             = l_t_locked
*      exceptions
*        others                 = 0.
*    insert lines of l_t_log_loaded into table l_t_log_handle.
*
*    describe table l_t_locked lines sy-tfill.
*    if sy-tfill > 0.
*      message s263(bl) with sy-tfill.
*    endif.

    IF NOT i_s_display_profile IS INITIAL.
      l_s_display_profile = i_s_display_profile.
    ELSE.


*      IF number_of_protocols = 1.
*        CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
*          IMPORTING
*            e_s_display_profile = l_s_display_profile
*          EXCEPTIONS
*            OTHERS              = 0.
*      ELSE.
*        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
*          IMPORTING
*            e_s_display_profile = l_s_display_profile
*          EXCEPTIONS
*            OTHERS              = 0.
*      ENDIF.

      CASE option .
        WHEN 0 .
          CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 1 .
          CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 2 .
          CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 3 .
        WHEN OTHERS .
      ENDCASE .
    ENDIF.


    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle      = l_t_log_handle
        i_s_display_profile = l_s_display_profile
        i_srt_by_timstmp    = i_srt_by_timstmp
      EXCEPTIONS
        no_authority        = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD get_lognumber .

    IF ( me->gs_header IS INITIAL ) .
      RETURN .
    ENDIF .

    result = me->gs_header-lognumber .

  ENDMETHOD .


  METHOD get_handle_saved .

    DATA:
      lt_lognumbers  TYPE szal_lognumbers,
      lt_header_data TYPE TABLE OF balhdr.

    CLEAR result .

    IF ( iv_lognumber IS NOT INITIAL ) .

      lt_lognumbers = VALUE #( ( item = iv_lognumber ) ) .

      CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
*        exporting
*          put_into_memory    = SPACE
*        importing
*          number_of_logs     =
        TABLES
          lognumbers  = lt_lognumbers
          header_data = lt_header_data
*         header_parameters  =
*         messages    =
*         message_parameters =
*         contexts    =
*         t_exceptions       =
        .

      IF ( lines( lt_header_data ) GT 0 ) .

        result = VALUE #( lt_header_data[ 1 ]-log_handle OPTIONAL ) .

*        " Informando dados do log ja criado
*        me->gv_object    = value #( lt_header_data[ 1 ]-object optional ) .
*        me->gv_subobject = value #( lt_header_data[ 1 ]-subobject optional ) .
*        me->gv_handle    = value #( lt_header_data[ 1 ]-log_handle optional ) .
*        me->gv_alprog    = sy-cprog .

      ENDIF .

    ENDIF .


  ENDMETHOD .


  METHOD get_messages_saved .

    DATA:
      lt_lognumbers  TYPE szal_lognumbers,
      lt_header_data TYPE TABLE OF balhdr,
      lt_messages    TYPE emma_message_tab.

    CLEAR:
      result .

    IF ( iv_lognumber IS NOT INITIAL ) .

      lt_lognumbers = VALUE #( ( item = iv_lognumber ) ) .

      CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
*        exporting
*          put_into_memory    = SPACE
*        importing
*          number_of_logs     =
        TABLES
          lognumbers  = lt_lognumbers
          header_data = lt_header_data
*         header_parameters  =
          messages    = lt_messages
*         message_parameters =
*         contexts    =
*         t_exceptions       =
        .

      IF ( lines( lt_messages ) GT 0 ) .

        result = lt_messages .

      ENDIF .

    ENDIF .

  ENDMETHOD .


  METHOD save_all .

    IF ( is_header IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( is_message IS INITIAL ) .
      RETURN .
    ENDIF.

    DATA(lo_app) =
      NEW ycl_hcm_application_log( iv_title     = is_header-title
                                   iv_object    = is_header-object
                                   iv_subobject = is_header-subobject
                                   iv_alprog    = is_header-alprog ) .

    IF ( lo_app IS BOUND ) .

      lo_app->add_bapiret1( is_message ) .
      lo_app->save( ) .
      result = lo_app->get_lognumber( ).
      FREE lo_app .

    ENDIF .

  ENDMETHOD .



  METHOD create.

    DATA(ls_log) = VALUE bal_s_log(
      extnumber  = me->gs_header-title
      object     = me->gs_header-title
      subobject  = me->gs_header-title
      aldate     = sy-datum
      altime     = sy-uzeit
      aluser     = sy-uname
      altcode    = sy-tcode
      alprog     = me->gs_header-alprog
      del_before = abap_on
    ) .

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = me->gs_header-handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'BAL_DB_LOGNUMBER_GET'
      EXPORTING
        i_log_handle = me->gs_header-handle
      IMPORTING
        e_lognumber  = me->gs_header-lognumber.

  ENDMETHOD.


  METHOD set_lognumber .

    IF ( iv_lognumber IS NOT INITIAL ) .
      me->gs_header-lognumber = iv_lognumber .
    ENDIF .

  ENDMETHOD .



ENDCLASS.

