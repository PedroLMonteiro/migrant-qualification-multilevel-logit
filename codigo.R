### Nível de desenvolvimento do país de origem e qualificação dos migrantes no Brasil ###

# 0. Pacotes necessários:
library(readxl) # Para tratamento dos dados.
library(dplyr) # Para tratamento dos dados.
library(tidyr) # Para tratamento dos dados.
library(performance) # biblioteca que vamos usar para calcular o ICC.
library(lme4) # biblioteca que vamos calcular o logit multinível e o modelo nulo.
library(modelsummary) # Para criar as tabelas
library(marginaleffects) # Para calcular os efeitos marginais

### 1. Importando e Tratando o Banco de dados:

dados <- read.csv2("dados/RAIS_CTPS_CAGED_2019.csv")
View(dados)
str(dados)

dados <- dados %>%
  select(pais,
         status_migratorio,
         tipo_mov_desagregado,
         nivel_instrucao,
         faixa_etaria,
         sexo,
         raca_cor,
         uf,
         faixa_horas_contrat,
         salario_mensal,
         cbo_2002)

### 1.1. Transformando as variáveis "pais" e "status_migratório" para formato númerico:

# Convertendo país em variável categórica
dados$pais <- factor(dados$pais)
levels(dados$pais)

dados <- dados %>% filter(status_migratorio == "Permanente", #  Considerando na amostra somente migrantes com vínculo permanente. A ideia é considerar migrantes que possuem chances de ficar mais tempo no país e consequentemente contribuir no processo de desenvolvimento econômico brasileiro
                          tipo_mov_desagregado == 1 # Considerando somente admissão por primeiro emprego, ou seja, migrantes que ingressam no Brasil em 2019. Esse filtro se justifica, pois é interessante levar em consideração o processo migratório mais atual, assim como as condições mais recentes do mercado de trabalho.  No caso do diferencial de desenvolvimento, isso faz muita diferença, pois se considerarmos todos os migrantes existem migrantes de países que o status de desenvolvimento em relação ao Brasil, mensurado pelo IDH, sofreu modificações importantes nos últimos anos.
)

### 1.2. Construindo a variável de qualificação do migrante
# 1 = ensino superior completo
# 0 = demais categorias inferiores a 6

table(dados$nivel_instrucao, useNA = "ifany")

dados$qualificado <- ifelse(
  dados$nivel_instrucao == 6, 1,
  ifelse(dados$nivel_instrucao < 6, 0, NA)
)

table(dados$qualificado, useNA = "ifany")

### 1.3. Considerando a população economicamente ativa (Excluindo as faixas etárias 1 e 7)

table(dados$faixa_etaria, useNA = "ifany")

dados <- dados %>%
  filter(!faixa_etaria %in% c(1, 7))

table(dados$faixa_etaria, useNA = "ifany")


### 1.4. Criando dummies para as faixas etárias
# Categoria de referência: idade_1 (18 a 24 anos)/idade_2 (25 a 29 anos)/idade_3 (30 a 39 anos)/idade_4 (40 a 49 anos)/idade_5 (50 a 64 anos)

dados$idade_1 <- ifelse(dados$faixa_etaria == 2, 1,
                        ifelse(dados$faixa_etaria %in% 3:6, 0, NA))

dados$idade_2 <- ifelse(dados$faixa_etaria == 3, 1,
                        ifelse(dados$faixa_etaria %in% c(2, 4:6), 0, NA))

dados$idade_3 <- ifelse(dados$faixa_etaria == 4, 1,
                        ifelse(dados$faixa_etaria %in% c(2, 3, 5, 6), 0, NA))

dados$idade_4 <- ifelse(dados$faixa_etaria == 5, 1,
                        ifelse(dados$faixa_etaria %in% c(2:4, 6), 0, NA))

dados$idade_5 <- ifelse(dados$faixa_etaria == 6, 1,
                        ifelse(dados$faixa_etaria %in% 2:5, 0, NA))

table(dados$idade_1, useNA = "ifany")
table(dados$idade_2, useNA = "ifany")
table(dados$idade_3, useNA = "ifany")
table(dados$idade_4, useNA = "ifany")
table(dados$idade_5, useNA = "ifany")

### 1.5. Gerando dummy de sexo (homem = 1 para homens; 0 para mulheres)

table(dados$sexo, useNA = "ifany")

dados$homem <- ifelse(dados$sexo == 1, 1,
                      ifelse(dados$sexo == 2, 0, NA))

table(dados$homem, useNA = "ifany")


###  1.6. Gerando dummy de cor/raça
# branco = 1 para brancos e amarelos
# branco = 0 para pretos, pardos e indígenas
# Excluindo categoria 9

table(dados$raca_cor, useNA = "ifany")

dados <- dados %>%
  filter(raca_cor != 9)

dados$branco <- ifelse(dados$raca_cor %in% c(2, 6), 1,
                       ifelse(dados$raca_cor %in% c(1, 4, 8), 0, NA))

table(dados$branco, useNA = "ifany")

### 1.7. Criando dummies regionais (Códigos de UF do IBGE)

table(dados$uf, useNA = "ifany")

dados$norte <- ifelse(dados$uf %in% 11:17, 1,
                      ifelse(dados$uf %in% c(21:29, 31:35, 41:43, 50:53), 0, NA))

dados$nordeste <- ifelse(dados$uf %in% 21:29, 1,
                         ifelse(dados$uf %in% c(11:17, 31:35, 41:43, 50:53), 0, NA))

dados$sudeste <- ifelse(dados$uf %in% 31:35, 1,
                        ifelse(dados$uf %in% c(11:17, 21:29, 41:43, 50:53), 0, NA))

dados$sul <- ifelse(dados$uf %in% 41:43, 1,
                    ifelse(dados$uf %in% c(11:17, 21:29, 31:35, 50:53), 0, NA))

dados$centroeste <- ifelse(dados$uf %in% 50:53, 1,
                           ifelse(dados$uf %in% c(11:17, 21:29, 31:35, 41:43), 0, NA))

table(dados$norte, useNA = "ifany")
table(dados$nordeste, useNA = "ifany")
table(dados$sudeste, useNA = "ifany")
table(dados$sul, useNA = "ifany")
table(dados$centroeste, useNA = "ifany")

### 1.8. Criando dummy para horas contratadas
# 1 = categoria 6 (mais de 40 horas, conforme sua definição)
# 0 = categorias inferiores a 6

table(dados$faixa_horas_contrat, useNA = "ifany")

dados$horamais40 <- ifelse(
  dados$faixa_horas_contrat == 6, 1,
  ifelse(dados$faixa_horas_contrat < 6, 0, NA)
)

table(dados$horamais40, useNA = "ifany")

### 1.9. Criando grupos ocupacionais a partir da CBO 2002

# Grupo 1: Membros superiores do poder público, dirigentes e gerentes
dados$grupo1 <- ifelse(
  dados$cbo_2002 < 200000, 1,
  ifelse(dados$cbo_2002 >= 200000, 0, NA)
)
table(dados$grupo1, useNA = "ifany")

# Grupo 2: Profissionais das ciências e das artes
dados$grupo2 <- ifelse(
  dados$cbo_2002 >= 200000 & dados$cbo_2002 < 300000, 1,
  ifelse(dados$cbo_2002 < 200000 | dados$cbo_2002 >= 300000, 0, NA)
)
table(dados$grupo2, useNA = "ifany")

# Grupo 3: Técnicos de nível médio
dados$grupo3 <- ifelse(
  dados$cbo_2002 >= 300000 & dados$cbo_2002 < 400000, 1,
  ifelse(dados$cbo_2002 < 300000 | dados$cbo_2002 >= 400000, 0, NA)
)
table(dados$grupo3, useNA = "ifany")

# Grupo 4: Trabalhadores de serviços administrativos
dados$grupo4 <- ifelse(
  dados$cbo_2002 >= 400000 & dados$cbo_2002 < 500000, 1,
  ifelse(dados$cbo_2002 < 400000 | dados$cbo_2002 >= 500000, 0, NA)
)
table(dados$grupo4, useNA = "ifany")

# Grupo 5: Trabalhadores dos serviços, vendedores em lojas e mercados
dados$grupo5 <- ifelse(
  dados$cbo_2002 >= 500000 & dados$cbo_2002 < 600000, 1,
  ifelse(dados$cbo_2002 < 500000 | dados$cbo_2002 >= 600000, 0, NA)
)
table(dados$grupo5, useNA = "ifany")

# Grupo 6: Trabalhadores agropecuários, florestais e da pesca
dados$grupo6 <- ifelse(
  dados$cbo_2002 >= 600000 & dados$cbo_2002 < 700000, 1,
  ifelse(dados$cbo_2002 < 600000 | dados$cbo_2002 >= 700000, 0, NA)
)
table(dados$grupo6, useNA = "ifany")

# Grupo 7: Trabalhadores da produção de bens e serviços industriais
dados$grupo7 <- ifelse(
  dados$cbo_2002 >= 700000 & dados$cbo_2002 < 900000, 1,
  ifelse(dados$cbo_2002 < 700000 | dados$cbo_2002 >= 900000, 0, NA)
)
table(dados$grupo7, useNA = "ifany")

# Grupo 8: Trabalhadores em servicos de reparacao e manutencao
dados$grupo8 <- ifelse(
  dados$cbo_2002 >= 900000, 1,
  ifelse(dados$cbo_2002 < 900000, 0, NA)
)
table(dados$grupo8, useNA = "ifany")

### 1.10. Criando a tabela de IDH e calculando a difIDH com o Brasil no ano de 2019.

# Tabela de IDH dos paises de origem em 2019
IDH_paises <- data.frame(
  pais = c(
    "ALEMANHA", "ANGOLA", "ARGENTINA", "ARGELIA", "ARMENIA",
    "AUSTRALIA", "BANGLADESH", "BENIN", "BOLIVIA", "BULGARIA",
    "BELGICA", "CABO VERDE", "CAMAROES", "CANADA", "CHILE",
    "CHINA", "COLOMBIA", "CONGO", "COREIA DO SUL", "COSTA RICA",
    "CUBA", "EGITO", "EQUADOR", "ESPANHA", "ESTADO DA PALESTINA",
    "ESTADOS UNIDOS", "FILIPINAS", "FINLANDIA", "FRANCA", "GANA",
    "GUATEMALA", "GUINE", "GUINE BISSAU", "HAITI", "HOLANDA",
    "HONDURAS", "IRAQUE", "IRLANDA", "IRA", "ISLANDIA", "ISRAEL",
    "ITALIA", "JAPAO", "KUWAIT", "LIBANO", "LIBIA", "MARROCOS",
    "MOCAMBIQUE", "MEXICO", "NICARAGUA", "NIGERIA", "NORUEGA",
    "NOVA ZELANDIA", "NIGER", "PAQUISTAO", "PARAGUAI", "PERU",
    "POLONIA", "PORTUGAL", "REINO UNIDO",
    "REPUBLICA DEMOCRATICA DO CONGO", "REPUBLICA DOMINICANA",
    "ROMENIA", "RUSSIA", "SENEGAL", "SERRA LEOA", "SUICA",
    "SAO TOME E PRINCIPE", "SERVIA", "SIRIA", "TOGO", "TUNISIA",
    "TURQUIA", "UCRANIA", "URUGUAI", "VENEZUELA", "AUSTRIA", "INDIA"
  ),
  
  IDH = c(
    0.947, 0.581, 0.845, 0.748, 0.776,
    0.944, 0.632, 0.545, 0.718, 0.816,
    0.931, 0.665, 0.563, 0.929, 0.851,
    0.761, 0.767, 0.574, 0.916, 0.810,
    0.783, 0.707, 0.759, 0.904, 0.708,
    0.926, 0.718, 0.938, 0.901, 0.611,
    0.663, 0.477, 0.480, 0.510, 0.944,
    0.634, 0.674, 0.955, 0.783, 0.949,
    0.919, 0.892, 0.919, 0.806, 0.744,
    0.724, 0.686, 0.456, 0.779, 0.660,
    0.539, 0.957, 0.931, 0.394, 0.557,
    0.728, 0.777, 0.880, 0.864, 0.932,
    0.480, 0.742, 0.828, 0.824, 0.512,
    0.452, 0.955, 0.625, 0.806, 0.567,
    0.515, 0.740, 0.820, 0.779, 0.817,
    0.711, 0.922, 0.645
  )
)

# Garantindo que o nome do pais esteja como texto e removendo espacos extras
dados$pais <- trimws(as.character(dados$pais))

# Associando o IDH de cada pais ao banco de dados
dados$IDH <- IDH_paises$IDH[
  match(dados$pais, IDH_paises$pais)
]

# Criando a diferenca entre o IDH do pais de origem e o IDH do Brasil em 2019
dados$difIDH <- dados$IDH - 0.765 # IDH do Brasil em 2019: 0.765

# Conferindo os resultados
table(is.na(dados$IDH))

dados %>%
  filter(is.na(IDH)) %>%
  count(pais, sort = TRUE)

dados <- dados %>%
  filter(!is.na(IDH))

dados %>%
  select(pais, IDH, difIDH) %>%
  distinct() %>%
  arrange(pais) %>%
  View()

### 2. Fazendo as analises descritivas e criando tabelas de cada uma.
### 2.1. Origem dos imigrantes inseridos no mercado de trabalho formal brasileiro em 2019.

dados %>%
  count(pais, sort = TRUE) %>%
  mutate(
    pais    = as.character(pais),
    Percent = 100 * n / sum(n),
    Cum     = cumsum(Percent)
  ) %>%
  bind_rows(summarise(., pais = "TOTAL", n = sum(n), Percent = 100, Cum = NA)) %>%
  transmute(
    País    = pais,
    `Freq.` = format(n, big.mark = ",", trim = TRUE),
    Percent = formatC(Percent, format = "f", digits = 2),
    `Cum.`  = if_else(is.na(Cum), "", formatC(Cum, format = "f", digits = 2))
  ) %>%
  datasummary_df(
    title = "Tabela 1: Origem dos migrantes inseridos no mercado de trabalho formal Brasileiro em 2019.",
    notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG.") %>%
  tinytable::style_tt(align = "c")  # Serve para centralizar todas as informacoes

### 2.2. Estatisticas descritivas da amostra.

Obs <- function(x) as.character(sum(!is.na(x)))

datasummary(
  qualificado + homem + branco + idade_2 + idade_3 + idade_4 + idade_5 +
    horamais40 + norte + nordeste + centroeste + sul + grupo2 + difIDH
  ~ Obs + Mean + SD + Min + Max,
  data = dados,
  fmt  = 3,
  title = "Tabela 2: Estatísticas descritivas da amostra",
  notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG.") %>%
  tinytable::style_tt(align = "c")  # Serve para centralizar todas as informacoes

### 2.3. Tabela com o nivel de instrucao por pais.

top10 <- dados %>%
  filter(nivel_instrucao == 6) %>%
  count(pais, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(pais) %>%
  as.character()

dados %>%
  mutate(
    pais_grp = if_else(as.character(pais) %in% top10,
                       as.character(pais), "DEMAIS PAÍSES"),
    pais_grp = factor(pais_grp, levels = c(top10, "DEMAIS PAÍSES"))
  ) %>%
  with(addmargins(table(pais_grp, nivel_instrucao), margin = c(1, 2))) %>%  # linha e coluna de total
  as.data.frame.matrix() %>%
  tibble::rownames_to_column("País") %>%
  rename(Total = Sum) %>%
  mutate(País = if_else(País == "Sum", "TOTAL", País)) %>%
  datasummary_df(fmt = 0,
                 title = "Tabela 3: Origem dos imigrantes alocados no mercado de trabalho formal brasileiro por nível de instrução no ano de 2019",
                 notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG."
                 ) %>%
  tinytable::group_tt(j = list("Nível de instrução" = 2:7)) %>%
  tinytable::style_tt(i =-1:0, bold = TRUE) %>%
  tinytable::style_tt(j = 7, bold = TRUE) %>%
  tinytable::style_tt(align = "c")  # Serve para centralizar todas as informacoes

### 2.4. Qualificacao dos migrantes (percentual na amostra).

dados %>%
  count(qualificado) %>%
  mutate(
    qualificado = as.character(qualificado),
    Percent   = 100 * n / sum(n),
    Cum       = cumsum(Percent)
  ) %>%
  bind_rows(summarise(., qualificado = "TOTAL", n = sum(n), Percent = 100, Cum = NA)) %>%
  transmute(
    Qualificação = qualificado,
    `Freq.`      = format(n, big.mark = ","),
    Percent      = formatC(Percent, format = "f", digits = 2),
    `Cum.`       = if_else(is.na(Cum), "", formatC(Cum, format = "f", digits = 2))
  ) %>%
  datasummary_df(
    title = "Tabela 3.1: Qualificação dos migrantes (percentual na amostra).",
    notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG."
  ) %>%
  tinytable::style_tt(i = 0, bold = TRUE) %>%
  tinytable::style_tt(i = 3, bold = TRUE)%>%
  tinytable::style_tt(align = "c")  # Serve para centralizar todas as informacoes

### 2.5. Qualificacao dos migrantes (Qntd por pais).

dados %>%
  with(addmargins(table(pais, qualificado), margin = c(1, 2))) %>%  # linha e coluna de total
  as.data.frame.matrix() %>%
  tibble::rownames_to_column("País") %>%
  rename(TOTAL = Sum) %>%
  mutate(País = if_else(País == "Sum", "TOTAL", País)) %>%
  datasummary_df(
    fmt   = 0,
    title = "Tabela 3.2: Qualificação dos migrantes (número por país).",
    notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG."
  ) %>%
  tinytable::group_tt(j = list("Qualificado" = 2:3))%>%
  tinytable::style_tt(align = "c")

### 2.6. Criando uma funcao para relacionar as variaveis com a variavel de qualificacao e criar a tabela (iria ser uma tarefa repetitiva demais)

tab_qualificado <- function(v, titulo = NULL, fonte = NULL) {
  
  tab <- dados %>%
    with(addmargins(table(qualificado, dados[[v]]), margin = c(1, 2))) %>%
    as.data.frame.matrix() %>%
    tibble::rownames_to_column("qualificado") %>%
    rename(Total = Sum) %>%
    mutate(qualificado = if_else(qualificado == "Sum", "Total", qualificado))
  
  grupo <- list(2:3)
  names(grupo) <- v                      # o título sobre as colunas 0 e 1 é o nome da variável
  
  tab %>%
    datasummary_df(fmt = 0, title = titulo, notes = fonte) %>%
    tinytable::group_tt(j = grupo) %>%
    tinytable::style_tt(i = -1:0, bold = TRUE) %>%    # título + cabeçalho
    tinytable::style_tt(i = 3, bold = TRUE) %>%       # linha Total
    tinytable::style_tt(j = 4, bold = TRUE) %>%       # coluna Total
    tinytable::style_tt(align = "c")
}

### 2.6.1. Qualificacao do migrante por grupo ocupacional/idade/regiao/cor/genero/horas trabalhadas.

tab_qualificado("grupo1")
tab_qualificado("grupo2")
tab_qualificado("grupo3")
tab_qualificado("grupo4")
tab_qualificado("grupo5")
tab_qualificado("grupo6")
tab_qualificado("grupo7")
tab_qualificado("grupo8")

tab_qualificado("idade_1")
tab_qualificado("idade_2")
tab_qualificado("idade_3")
tab_qualificado("idade_4")
tab_qualificado("idade_5")

tab_qualificado("norte")
tab_qualificado("nordeste")
tab_qualificado("sudeste")
tab_qualificado("sul")
tab_qualificado("centroeste")

tab_qualificado("branco")
tab_qualificado("homem")
tab_qualificado("horamais40")

### 2.7. Criando uma tabela que relaciona a variavel dependendente, qualificacao, com as demais variaveis.
### 2.7.1. Definindo as Variaveis explicativas que vou relacionar (Nome para a tabela = Nome nos dados)

variaveis <- c("Ciências_Artes" = "grupo2",
          "Idade 1" = "idade_1", "Idade 2" = "idade_2", "Idade 3" = "idade_3",
          "Idade 4" = "idade_4", "Idade 5" = "idade_5",
          "Norte" = "norte", "Nordeste" = "nordeste", "Sudeste" = "sudeste",
          "Sul" = "sul", "Centroeste" = "centroeste",
          "Branco" = "branco", "Homem" = "homem", "Horamais40" = "horamais40")

### 2.7.2.  Calculando, para cada variavel, total (x = 1), qualificados (x = 1 e qualificado = 1) e a razao.

resumo <- bind_rows(lapply(names(variaveis), function(rotulo) {
  x <- dados[[ variaveis[[rotulo]] ]]
  q <- dados$qualificado
  total <- sum(x == 1, na.rm = TRUE)
  qualif <- sum(x == 1 & q == 1, na.rm = TRUE)
  data.frame(
    Variáveis = rotulo,
    Total = total,
    Qualificados = qualif,
    Média = qualif / total
  )
}))

# 2.7.3 Formatando a tabela dessas informacoes.

tabela_variaveisxqualificados <- resumo %>%
  mutate(Total = format(Total, big.mark = ",", trim = TRUE),
         Qualificados = format(Qualificados, big.mark = ",", trim = TRUE),
         Média = formatC(Média, format = "f", digits = 4)) %>%
  rename("Total de migrantes por variável" = Total,
         "Qualificados" = Qualificados,
         "Média" = Média) %>%
  datasummary_df(
    title = "Tabela 4: Média de indivíduos com ensino superior completo condicionada às variáveis incluídas no modelo econométrico",
    notes = "Fonte: Elaboração própria com base nos dados harmonizados do RAIS-CTPS-CAGEG."
  ) %>%
  tinytable::style_tt(i = 0, bold = TRUE) %>%
  tinytable::style_tt(align = "c")

tabela_variaveisxqualificados

### 3. Analise econometrica.
# Modelo estimado sera o Logit Multinivel, que considera dois niveis: 1 (variaveis relativas a 
# caracteristicas do migrante e de seu posto de trabalho) e 2 (país de origem do migrante).

# Primeiro, precisamos estimar o modelo nulo (sem controles) para verificar a adequacao do metodo e estimar a correlacao intraclasse.

### 3.1. Modelo nulo: só intercepto + efeito aleatório de país.

modelo_nulo <- glmer(qualificado ~ 1 + (1 | pais),
            data    = dados,
            family  = binomial(link = "logit"),
            nAGQ    = 10)          # quadratura adaptativa: mais precisa que o padrão (Laplace)

### 3.2. Calculando o ICC (coeficiente de correlacao intraclasse).

performance::icc(modelo_nulo)

# Intercepto: -0.5448
# Erro padrao: 0.2228
# Variancia do intercepto aleatorio (variancia entre paises): 2.2580
# ICC: Coeficiente de correlacao Intraclasse: 0.407 (40,7% das variacoes nas chances do imigrante ser qualificado
# sao influenciadas pelo pais de origem do imigrante, o que nos permite inferir que o pais do qual o imigrante adveio
# pode influenciar em sua formacao, ou seja, ha uma caracteristica referente ao pais, o que indica utilizar o modelo multinivel)

### 3.3. Modelo completo.

modelo_completo <- glmer(qualificado ~ homem + branco + idade_2 + idade_3 + idade_4 + idade_5 +
              horamais40 + norte + nordeste + centroeste + sul + grupo2 + difIDH +
              (1 | pais),
            data    = dados,
            family  = binomial,
            nAGQ    = 10,
            control = glmerControl(optimizer = "bobyqa"))

### 3.4. Calculando os efeitos marginais do modelo completo.

efeitos_marginais <- avg_slopes(modelo_completo)
summary(efeitos_marginais)

### 3.5. Fazendo a Tabela 

modelsummary(list("Modelo nulo" = modelo_nulo, "Modelo completo" = modelo_completo, "Efeitos Marginais" = efeitos_marginais),
             stars = TRUE,
             title = "Resultados Econométricos e efeito marginal do Modelo Completo")

################################################################################