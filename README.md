# statgit

**English version below**

---

## Português 🇧🇷

### Visão geral

O **statgit** é um pacote R com uma interface Shiny integrada ao RStudio que ajuda estudantes de Estatística a usar controle de versão com Git de forma acessível e intuitiva, sem precisar sair do editor ou aprender comandos no terminal.

Desenvolvido com foco em **educação**, o pacote oferece:
- **Configuração assistida** de Git e GitHub em português
- **Painel visual** para organizar projetos, fazer commits e conectar repositórios
- **Formação automática** de código com `styler`
- **Pré-visualização de relatórios** em R Markdown e Quarto
- **Tradução completa** da interface para português e inglês

### Por que foi criado?

A ideia surgiu da observação de que muitos estudantes de Estatística encontram dificuldades ao começar a usar Git e GitHub. Os motivos são comuns:

1. **Barreira de entrada**: Terminal e comandos Git parecem intimidadores para quem nunca usou
2. **Falta de contexto educacional**: Documentação é técnica, não didática
3. **Isolamento do fluxo de trabalho**: Alternância frequente entre RStudio e terminal quebra o ritmo
4. **Segurança**: Copiar/colar tokens em texto aberto é uma má prática

O **statgit** resolve isso colocando todas essas ferramentas dentro de um painel visual bem organizado, com mensagens em linguagem acessível, sem que o aluno tenha que abrir um terminal ou decorar sintaxe.

### Para quem é?

- Estudantes de cursos de Estatística que estão começando com controle de versão
- Professores que querem ensinar Git sem perder aula com instalações ou troubleshooting
- Pesquisadores que usam RStudio e querem manter projetos organizados com repositórios no GitHub

### Funcionalidades principais

- **Painel unificado** para todas as ações (visão geral, gerenciar projeto, arquivos, Git e GitHub)
- **Seleção de idioma** em português e inglês
- **Assistentes visuais** para Git (setup), primeiro commit, conexão com GitHub
- **Organização automática** de projetos a partir de modelos
- **Formatação de código** integrada com `styler`
- **Pré-visualização ao vivo** de relatórios R Markdown e Quarto
- **Status Git traduzido** para linguagem clara e amigável

### Instalação

#### Versão de desenvolvimento

```r
devtools::install_github("geovani-ariel/statgit")
```

#### Localmente a partir do código-fonte

```r
install.packages(".", repos = NULL, type = "source")
```

### Uso rápido

#### Pelo RStudio (recomendado)

1. Instale o pacote
2. Vá a `Addins` no menu superior
3. Clique em `statgit`
4. A interface abrirá em um painel lateral

#### Pelo console R

```r
library(statgit)

# Abre o painel principal
statgit()

# Funções individuais (também usáveis via console)
git_check()                    # diagnóstico do Git
git_init()                     # inicializa repositório
git_status()                   # status em linguagem clara
git_commit_all("Mensagem")     # commit de todos os arquivos
git_push()                     # envia para GitHub
github_connect("https://...")  # conecta ao remote
project_create("nome", template = "trabalho_disciplina")  # novo projeto
code_format_all()              # formata todo o código
```

### Painel principal

O painel se organiza em 4 abas principais:

1. **Visão Geral**
   - Timeline de commits recentes
   - Status geral do projeto
   - Ações rápidas

2. **Gerenciar Projeto**
   - Criar projetos a partir de modelos
   - Organizar estrutura existente
   - Localizar e abrir projetos `.Rproj`

3. **Arquivos e Código**
   - Ver diffs de arquivos alterados
   - Importar/criar novos arquivos
   - Formatar código com `styler`
   - Pré-visualizar relatórios

4. **Git e GitHub**
   - Wizard de configuração inicial
   - Gerenciar staging de arquivos
   - Fazer commits
   - Conectar/desconectar do GitHub
   - Push e pull

Todas as abas estão disponíveis em **português** e **inglês**. A escolha fica salva na sessão.

### Complementos (addins) do RStudio

Após instalar, estes complementos aparecem em `Addins`:

- **statgit** — abre o painel principal
- **Verificar Git do projeto** — diagnóstico rápido
- **Configurar Git neste projeto** — wizard de setup
- **Fazer primeiro commit** — cria o commit inicial de forma guiada
- **Ver status Git** — mostra status em linguagem clara
- **Pré-visualizar knit** — gera preview HTML do arquivo ativo
- **Live preview knit** — atualiza preview automaticamente
- **Formatar arquivo atual** — aplica `styler` ao arquivo ativo
- **Formatar projeto atual** — aplica `styler` a todo o código
- **Gerenciar projetos estatísticos** — localiza e abre `.Rproj`

### Funções principais

| Função | O que faz |
| --- | --- |
| `statgit()` | Abre o painel principal |
| `git_check()` | Diagnóstico didático do Git |
| `git_init()` | Inicializa repositório com branch `main` |
| `git_ignore()` | Cria/atualiza `.gitignore` |
| `git_status()` | Status traduzido para linguagem clara |
| `git_commit()` / `git_commit_all()` | Cria commits |
| `git_changed()` | Lista arquivos alterados |
| `git_diff()` | Mostra diff de um arquivo |
| `git_stage()` / `git_unstage()` | Gerencia staging |
| `git_discard()` | Descarta mudanças de um arquivo |
| `git_push()` / `git_pull()` | Sincroniza com GitHub |
| `github_connect()` | Conecta a um remote |
| `github_check()` | Testa acesso ao remote |
| `project_create()` | Cria projeto a partir de modelo |
| `project_organize()` | Organiza estrutura atual |
| `code_format()` / `code_format_all()` | Formata código |
| `report_preview()` | Preview HTML de `.Rmd` ou `.qmd` |
| `report_live_preview()` | Preview automático ao salvar |

### Modelos de projeto

Use `project_create()` ou a aba de gerenciar projeto para escolher:

- `analise_exploratoria` — análise de dados estruturada
- `trabalho_disciplina` — trabalho de disciplina
- `iniciacao_cientifica` — iniciação científica
- `tcc` — trabalho de conclusão de curso
- `artigo_quarto` — artigo com Quarto
- `projeto_grupo` — projeto colaborativo

### Observações de segurança

- Não pede tokens do GitHub em texto aberto
- Não executa comandos destrutivos (`reset --hard`, `force push`, etc.)
- Nunca sobrescreve arquivos existentes sem confirmação
- Funciona com credenciais armazenadas localmente no Git

### Conceitos básicos (para iniciantes)

- **Repositório**: pasta com controle de versão Git
- **Commit**: "fotografia" do projeto em um momento específico
- **Branch**: ramificação do desenvolvimento (padrão: `main`)
- **Push**: envia commits para o GitHub
- **Pull**: baixa mudanças do GitHub
- **Staging**: preparar arquivos antes de fazer commit

### Requisitos do sistema

- R ≥ 4.1.0
- RStudio (versão recente)
- Git instalado no computador
- Pandoc (para preview de R Markdown)
- Quarto (opcional, para preview de `.qmd`)

### Contribuindo

Encontrou um bug? Tem uma sugestão? Abra uma issue em [https://github.com/geovani-ariel/statgit/issues](https://github.com/geovani-ariel/statgit/issues)

### Licença

GPL-3 — Software livre e aberto para fins educacionais.

---

## 🇺🇸 English

### Overview

**statgit** is an R package with a Shiny interface integrated into RStudio that helps Statistics students use version control with Git in an accessible and intuitive way, without needing to leave the editor or learn terminal commands.

Developed with a focus on **education**, the package offers:
- **Guided setup** of Git and GitHub with clear instructions
- **Visual panel** to organize projects, make commits, and connect repositories
- **Automatic code formatting** with `styler`
- **Live preview** of R Markdown and Quarto reports
- **Full interface translation** to Portuguese and English

### Why was it created?

The idea came from observing that many Statistics students struggle when they first start using Git and GitHub. The common reasons are:

1. **Steep learning curve**: Terminal and Git commands seem intimidating to beginners
2. **Lack of educational context**: Documentation is technical, not didactic
3. **Workflow disruption**: Frequent switching between RStudio and terminal breaks your flow
4. **Security concerns**: Copying/pasting tokens in plain text is a bad practice

**statgit** solves this by placing all these tools inside a well-organized visual panel with accessible language, so students never have to open a terminal or memorize syntax.

### Who is it for?

- Statistics students who are starting with version control
- Teachers who want to teach Git without losing class time to setup issues
- Researchers using RStudio who want to keep projects organized with GitHub repositories

### Main features

- **Unified panel** for all actions (overview, manage project, files, Git and GitHub)
- **Language selection** in Portuguese and English
- **Visual wizards** for Git setup, first commit, GitHub connection
- **Automatic project organization** from templates
- **Integrated code formatting** with `styler`
- **Live preview** of R Markdown and Quarto reports
- **Translated Git status** in clear, friendly language

### Installation

#### Development version

```r
devtools::install_github("geovani-ariel/statgit")
```

#### Locally from source

```r
install.packages(".", repos = NULL, type = "source")
```

### Quick start

#### From RStudio (recommended)

1. Install the package
2. Go to `Addins` in the top menu
3. Click `statgit`
4. The interface opens in a side panel

#### From the R console

```r
library(statgit)

# Open the main panel
statgit()

# Individual functions (also usable from console)
git_check()                    # Git diagnostic
git_init()                     # initialize repository
git_status()                   # status in clear language
git_commit_all("Message")      # commit all files
git_push()                     # push to GitHub
github_connect("https://...")  # connect to remote
project_create("name", template = "trabalho_disciplina")  # new project
code_format_all()              # format all code
```

### Main panel

The panel is organized into 4 main tabs:

1. **Overview**
   - Timeline of recent commits
   - Overall project status
   - Quick actions

2. **Manage Project**
   - Create projects from templates
   - Organize existing structure
   - Find and open `.Rproj` files

3. **Files and Code**
   - View diffs of changed files
   - Import/create new files
   - Format code with `styler`
   - Preview reports

4. **Git and GitHub**
   - Initial setup wizard
   - Manage file staging
   - Make commits
   - Connect/disconnect from GitHub
   - Push and pull

All tabs are available in **Portuguese** and **English**. The language choice is saved in your session.

### RStudio addins

After installation, these addins appear in the `Addins` menu:

- **statgit** — opens the main panel
- **Verificar Git do projeto** — quick diagnostic
- **Configurar Git neste projeto** — setup wizard
- **Fazer primeiro commit** — guided initial commit
- **Ver status Git** — status in clear language
- **Pré-visualizar knit** — HTML preview of active file
- **Live preview knit** — auto-updates preview on save
- **Formatar arquivo atual** — applies `styler` to active file
- **Formatar projeto atual** — applies `styler` to all code
- **Gerenciar projetos estatísticos** — find and open `.Rproj`

### Main functions

| Function | What it does |
| --- | --- |
| `statgit()` | Opens the main panel |
| `git_check()` | Diagnostic of Git setup |
| `git_init()` | Initialize repository with `main` branch |
| `git_ignore()` | Create/update `.gitignore` |
| `git_status()` | Status in clear language |
| `git_commit()` / `git_commit_all()` | Create commits |
| `git_changed()` | List changed files |
| `git_diff()` | Show diff of a file |
| `git_stage()` / `git_unstage()` | Manage staging |
| `git_discard()` | Discard changes to a file |
| `git_push()` / `git_pull()` | Sync with GitHub |
| `github_connect()` | Connect to a remote |
| `github_check()` | Test remote access |
| `project_create()` | Create project from template |
| `project_organize()` | Organize existing structure |
| `code_format()` / `code_format_all()` | Format code |
| `report_preview()` | HTML preview of `.Rmd` or `.qmd` |
| `report_live_preview()` | Auto-preview on save |

### Project templates

Use `project_create()` or the manage project tab to choose:

- `analise_exploratoria` — structured data analysis
- `trabalho_disciplina` — course assignment
- `iniciacao_cientifica` — undergraduate research
- `tcc` — thesis/final project
- `artigo_quarto` — Quarto article
- `projeto_grupo` — collaborative project

### Security notes

- Does not ask for GitHub tokens in plain text
- Does not execute destructive commands (`reset --hard`, `force push`, etc.)
- Never overwrites existing files without confirmation
- Works with credentials stored locally in Git

### Basic concepts (for beginners)

- **Repository**: folder with Git version control
- **Commit**: "snapshot" of the project at a specific moment
- **Branch**: development path (default: `main`)
- **Push**: send commits to GitHub
- **Pull**: download changes from GitHub
- **Staging**: prepare files before committing

### System requirements

- R ≥ 4.1.0
- RStudio (recent version)
- Git installed on your computer
- Pandoc (for R Markdown preview)
- Quarto (optional, for `.qmd` preview)

### Contributing

Found a bug? Have a suggestion? Open an issue at [https://github.com/geovani-ariel/statgit/issues](https://github.com/geovani-ariel/statgit/issues)

### License

GPL-3 — Free and open-source software for educational purposes.
