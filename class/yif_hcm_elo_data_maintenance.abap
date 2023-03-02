INTERFACE yif_hcm_elo_data_maintenance
  PUBLIC .

  METHODS enqueue_employee
    IMPORTING
      !number       TYPE pernr_d
    RETURNING
      VALUE(result) TYPE abap_bool .

  METHODS dequeue_employee
    IMPORTING
      !number       TYPE pernr_d
    RETURNING
      VALUE(result) TYPE abap_bool .

  METHODS maintains_infotypes
    IMPORTING
      !data         TYPE p2001
    RETURNING
      VALUE(result) TYPE bapireturn1 .

  METHODS maintains_infotype_2001
    IMPORTING
      !data         TYPE p2001
    RETURNING
      VALUE(result) TYPE bapireturn1 .

  METHODS map_log
    IMPORTING
      !data         TYPE bapireturn1
    RETURNING
      VALUE(result) TYPE bal_s_msg .

ENDINTERFACE.
