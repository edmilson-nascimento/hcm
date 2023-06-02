# Routines HCM modules
HCM data maintenance (infotypes)

Essa solução ira seguir o modelo de arquitetura proposto por ~Ogrão~ [Leandro Figueiro](https://www.linkedin.com/in/leandro-figueiredo-09560113/) usando interface e classe (final e abstrata). Serão tratados diferentes infotypes mas usando uma mesma modelagem.

## Arquitetura ##

Para essa solução serão criados os seguintes objetos

- [Interface](https://github.com/edmilson-nascimento/hcm/blob/main/class/yif_hcm_elo_data_maintenance.abap) com os métodos
- Classe [abstrata](https://github.com/edmilson-nascimento/hcm/blob/main/class/ycl_hcm_elo_infotypes.abap) com base na interface (e fazendo implementação dos metodos)
- Classe [final](https://github.com/edmilson-nascimento/hcm/blob/main/class/ycl_hcm_elo_maintenance.abap) com detalhes de cada infotype e tratamento de dados antes e depois do processament0