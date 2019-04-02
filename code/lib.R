read_projectdata <- function() {
  readr::read_csv(
    here::here("data/atividade-por-perfil-20190322.csv"),
    col_types = cols(
      .default = col_double(),
      id_parlamentar = col_character(),
      casa = col_character(),
      nome_eleitoral = col_character(),
      partido = col_character(),
      UF = col_character(),
      twitter = col_character()
    )
  )
}
