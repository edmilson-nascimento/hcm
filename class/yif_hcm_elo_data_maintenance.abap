INTERFACE yif_hcm_elo_data_maintenance
  PUBLIC .

  DATA:
    "! <p class="shorttext synchronized" lang="pt">Dados de log de processamento</p>
    return_message TYPE bapireturn1 .

  "! <p class="shorttext synchronized" lang="pt">Retorna dados de acesso (user,pass and etc)</p>
  METHODS get_user_access
    RETURNING
      VALUE(result) TYPE zwsuser .

  "! <p class="shorttext synchronized" lang="pt">Bloquear registro de Pessoa para processamento</p>
  METHODS enqueue_employee
    IMPORTING
      !number       TYPE pernr_d
    RETURNING
      VALUE(result) TYPE abap_bool .

  "! <p class="shorttext synchronized" lang="pt">Desbloquear registro de Pessoa para processamento</p>
  METHODS dequeue_employee
    IMPORTING
      !number       TYPE pernr_d
    RETURNING
      VALUE(result) TYPE abap_bool .

  "! <p class="shorttext synchronized" lang="pt">Manter infoTypes</p>
  METHODS maintains_infotypes
    IMPORTING
      infty         TYPE prelp-infty
      number        TYPE p0001-pernr
      subtype       TYPE p0001-subty
      record        TYPE any
      operation     TYPE pspar-actio
      no_commit     TYPE sap_bool DEFAULT 'X'
    RETURNING
      VALUE(result) TYPE bapireturn1 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 0000</p>
  METHODS read_infotype_0000
    IMPORTING
      !im_pernr     TYPE p0000-pernr
    RETURNING
      VALUE(result) TYPE p0000 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 0001</p>
  METHODS read_infotype_0001
    IMPORTING
      !im_pernr     TYPE p0000-pernr
    RETURNING
      VALUE(result) TYPE p0001 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 0002</p>
  METHODS read_infotype_0002
    IMPORTING
      !im_pernr     TYPE p0000-pernr
    RETURNING
      VALUE(result) TYPE p0002 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 0105</p>
  METHODS read_infotype_0105
    IMPORTING
      !im_pernr     TYPE p0000-pernr
    RETURNING
      VALUE(result) TYPE p0105 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 0337</p>
  METHODS read_infotype_0337
    IMPORTING
      !im_pernr     TYPE p0000-pernr
    RETURNING
      VALUE(result) TYPE p0337 .

  "! <p class="shorttext synchronized" lang="pt">Ler infoType 2010</p>
  METHODS read_infotype_2010
    IMPORTING
      !im_pernr     TYPE p0000-pernr
      !im_begda     TYPE p0000-begda
      !im_endda     TYPE p0000-endda
    RETURNING
      VALUE(result) TYPE p2010_tab .


ENDINTERFACE.