# git4stats

Assistente de Git para estudantes de Estatística que usam RStudio.

## Para que serve?

Este pacote ajuda você a configurar Git em projetos RStudio, fazer o primeiro commit e entender o estado do seu projeto sem precisar começar pelo terminal.

## Histórico de commits

| Versão  | Commit      | Mensagem         |
| ------- | ----------- | ---------------- |
| 1.0.1   | `52f721b`   | Versão 1.0.1     |
| 1.0.0   | `32ed5db`   | Versão 1.0.0     |
| Initial | `d386859`   | Commit inicial   |

## Instalação

Para instalar localmente a partir da pasta do pacote:

```r
install.packages(".", repos = NULL, type = "source")
```

Se preferir, use `devtools::install()` ou `pak::pkg_install(local = TRUE)` em ambiente de desenvolvimento.

## Uso rápido

Pelo RStudio, o caminho recomendado é abrir:

`Addins > git4stats`

Isso abre o painel principal para criar projetos, configurar Git, conectar GitHub, pré-visualizar relatórios e formatar arquivos.

Para uso direto pelo console:

```r
git4stats_panel()
check_git_setup()
init_git_project()
create_r_gitignore()
use_stats_project()
first_commit()
git_status_pretty()
connect_github_repo("https://github.com/usuario/repositorio.git")
check_github_auth()
push_first_time()
create_stats_project("meu-projeto", template = "trabalho_disciplina")
```

## Usando pelo RStudio

Depois de instalar, vá em:

`Addins > git4stats`

O painel principal organiza as ações em:

- Projeto
- Arquivos
- Mudanças
- Git
- GitHub
- Relatórios
- Formatação

Addins antigos continuam disponíveis por compatibilidade:

- `Addins > Verificar Git do projeto`
- `Addins > Configurar Git neste projeto`
- `Addins > Fazer primeiro commit`
- `Addins > Ver status Git`
- `Addins > Pre-visualizar knit`
- `Addins > Live preview knit`
- `Addins > Formatar arquivo atual`
- `Addins > Formatar projeto atual`
- `Addins > Gerenciar projetos estatisticos`

## Conceitos básicos

- Commit: salvar uma versão do projeto
- Push: enviar versões para o GitHub
- Pull: baixar mudanças feitas por colegas

## Exemplo de fluxo completo

```r
check_git_setup()
init_git_project()
create_r_gitignore(include_data = FALSE)
use_stats_project(include_data = FALSE)
first_commit("Primeiro commit")
git_status_pretty()
connect_github_repo("https://github.com/usuario/repositorio.git")
check_github_auth()
push_first_time()
format_active_file("scripts/analise.R")
format_project_files()
preview_knit("reports/relatorio.Rmd")
preview_knit("reports/relatorio.Rmd", style = TRUE)
live_preview_knit("reports/relatorio.Rmd")
create_stats_project("meu-projeto", template = "projeto_grupo")
```

## Funções principais

- `git4stats_panel()`: abre o painel principal do pacote.
- `check_git_setup()`: faz um diagnóstico didático do Git no computador e no projeto atual.
- `init_git_project()`: inicializa Git com branch principal `main`.
- `create_r_gitignore()`: cria ou atualiza `.gitignore` sem duplicar linhas.
- `use_stats_project()`: monta uma estrutura simples para análise estatística.
- `create_stats_project()`: cria projeto novo com `.Rproj`, `.gitignore` e template inicial.
- `import_project_file()`: copia ou move um arquivo baixado para dentro do projeto.
- `git_changed_files()`: lista arquivos com mudanças no projeto.
- `git_diff_file()`: mostra o diff de um arquivo.
- `find_rstudio_projects()`: procura projetos `.Rproj` em uma pasta.
- `open_stats_project()`: abre rapidamente um projeto já existente.
- `format_active_file()`: formata um arquivo suportado com `styler`.
- `format_project_files()`: formata os arquivos suportados de um projeto inteiro.
- `first_commit()`: adiciona arquivos ao histórico e cria o primeiro commit.
- `git_status_pretty()`: traduz o status do Git para linguagem amigável.
- `git_setup_wizard()`: abre um assistente visual com `shiny` + `miniUI`.
- `project_manager_addin()`: abre um gerenciador visual para criar e trocar de projeto.
- `connect_github_repo()`: liga o projeto local a um remote no GitHub.
- `check_github_auth()`: testa se o remote GitHub está acessível sem pedir token em texto aberto.
- `push_first_time()`: envia os commits locais para o GitHub com `git push -u`.
- `preview_knit()`: gera uma pre-visualizacao HTML de um `.Rmd`, `.Rmarkdown` ou `.qmd`.
- `live_preview_knit()`: atualiza a pre-visualizacao automaticamente.

Para arquivos `.Rmd`, o preview usa `rmarkdown` + Pandoc. Para `.qmd`, usa o Quarto CLI.
No live preview, `.Rmd` e `.Rmarkdown` atualizam quando o arquivo salvo muda. Para `.qmd`, o pacote delega para `quarto preview`.
Para formatacao estilo “Prettier”, o pacote usa `styler` como dependencia sugerida.

## Uso avancado pelo console

As funções individuais continuam disponíveis para automação, testes e usuários que preferem terminal. Para estudantes iniciando, prefira o painel `git4stats_panel()`.

## Templates de projeto

- `trabalho_disciplina`
- `iniciacao_cientifica`
- `tcc`
- `artigo_quarto`
- `analise_exploratoria`
- `projeto_grupo`

Cada template cria `README.md`, `.Rproj`, `.gitignore`, diretórios padrão e alguns arquivos iniciais para acelerar o começo do trabalho.

## Roadmap

- Conexão com GitHub
- Criação de repositório remoto
- Fluxo para trabalhos em grupo

## Roadmap técnico para versão 0.2

Funções planejadas:

```r
create_github_repo()
pull_safely()
```

Integrações planejadas:

- `gitcreds` para credenciais
- `gh` para API do GitHub
- `usethis` para criação e conexão de repositórios
- mensagens específicas para erros comuns de autenticação

## Observações de segurança

- O MVP não pede token do GitHub em texto aberto.
- O pacote não executa comandos destrutivos como `reset --hard`, `clean -fd` ou `force push`.
- Arquivos existentes não são sobrescritos em massa; a criação de estrutura usa comportamento seguro.
