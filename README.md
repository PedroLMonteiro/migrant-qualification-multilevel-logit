# Nível de desenvolvimento do país de origem e qualificação dos migrantes no Brasil

Código de replicação do artigo:

> [MONTEIRO, P. L. M. D.; TEIXEIRA, E. C.]. **[Diferencial de desenvolvimento em relação ao país de origem e qualificação dos migrantes no Brasil]**. *Revista Econômica Internacional (REI)*, v. [16], n. [1], [2025].
> Disponível em: https://revista.uepb.edu.br/REI/en/article/view/3800

## Sobre o projeto

Este repositório analisa como o diferencial de IDH entre o país de origem e o Brasil
se associa à probabilidade de um migrante admitido no mercado de trabalho formal
brasileiro em 2019 ter ensino superior completo. A análise estima um **logit multinível**.

## Dados

- **Fonte:** dados harmonizados RAIS-CTPS-CAGED, 2019. Podem ser encontrados em: [Portal de microdados do OBMigra](https://www.gov.br/mj/pt-br/assuntos/seus-direitos/migracoes/portal-de-imigracao-laboral/obmigra-1/microdados).
- Os dados utilizados neste trabalho estão em [`RAIS_CTPS_CAGED_2019`](dados/), junto do layout da base.

## Metodologia

1. **Construção das variáveis:** qualificação (ensino superior completo), faixas
   etárias, sexo, cor/raça, macrorregiões, jornada (mais de 40 horas), grupos
   ocupacionais (CBO 2002) e diferencial de IDH (IDH do país de origem menos o IDH
   do Brasil, 0,765).
2. **Análise descritiva:** origem dos migrantes, estatísticas descritivas, nível de
   instrução por país e qualificação segundo as características do modelo.
3. **Modelo nulo e ICC**, para verificar a adequação da abordagem multinível.
4. **Logit multinível** com intercepto aleatório por país e `difIDH` como variável
   de nível 2, com erros-padrão robustos e efeitos marginais.

## Estrutura do repositório

```
.
├── codigo.R              # script completo da análise
├── artigo_migracao.Rproj # projeto do RStudio
├── dados/                # Dados da RAIS_CTPS_CAGED_2019 e o layout da base
└── .gitignore
```

## Como reproduzir

1. Clone o repositório e abra `artigo_migracao.Rproj` no RStudio.
2. Coloque a base em `dados/RAIS_CTPS_CAGED_2019.csv`.
3. Instale os pacotes:

```r
install.packages(c("dplyr", "tidyr", "modelsummary", "tinytable",
                   "lme4", "performance", "marginaleffects"))
```

4. Rode `codigo.R` na ordem. O script gera as tabelas do artigo.
