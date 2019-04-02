library(tidyverse)

tweets = read_csv(here::here("data/tweets-2019.csv.zip"))

perfis = read_csv(here::here("data/perfis-20190322.csv"),
                  col_types = cols(.default = col_character())) %>%
  select(id_parlamentar, casa, nome_eleitoral, partido, UF, twitter) %>%
  filter(!duplicated(id_parlamentar))

# Variáveis que queremos POR TWEET
tweets = tweets %>%
  mutate(
    conteudo = if_else(is_retweet |
                         is_quote, "retweet_ou_quote", "proprio"),
    engajamento_tweet = if_else(is.na(favorite_count), 0, favorite_count) + if_else(is.na(retweet_count), 0, retweet_count), 
    seguidores = followers_count, 
    segue = friends_count
  ) 

# Agrega POR PERFIL
atividade = tweets %>%
  mutate(is_proprio = conteudo == "proprio") %>% 
  group_by(screen_name) %>%
  summarise(
    seguidores = median(seguidores),
    segue = median(segue),
    n_proprio = sum(is_proprio),
    n_retweet = sum(!is_proprio),
    engaj_total = sum(engajamento_tweet),
    engaj_total_proprio = sum(engajamento_tweet * is_proprio),
    engaj_total_retweet = sum(engajamento_tweet * (!is_proprio)),
    engaj_mediano = median(engajamento_tweet),
    engaj_mediano_proprio = median(engajamento_tweet * is_proprio ),
    engaj_mediano_retweet = median(engajamento_tweet* (!is_proprio)),
    engaj_max = max(engajamento_tweet), 
    engaj_max_proprio = max(engajamento_tweet * is_proprio), 
    engaj_max_retweet = max(engajamento_tweet * (!is_proprio)), 
  )

fill_metric_inactive = function(x, handle){
  if_else(!is.na(handle) & is.na(x), 0, x)
}

# Cruza e coloca zeros nos perfis de quem não tuitou
atividade_perfis = perfis %>%
  left_join(atividade, by = c("twitter" = "screen_name")) %>%
  mutate_at(vars(starts_with("engaj")), funs(if_else(!is.na(twitter) &
                                                       is.na(.), 0, .))) %>%
  mutate_at(vars(starts_with("n_")), funs(if_else(
    !is.na(twitter) & is.na(.), 0.0, as.double(.)
  )))

atividade_perfis %>% 
  write_csv(here::here("data/atividade-por-perfil-20190322.csv"))
