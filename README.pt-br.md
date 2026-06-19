# statgit

[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

*Leia em outros idiomas: [English](README.md), [Português](README.pt-br.md).*

## Visão geral

O **statgit** é um pacote R com uma interface Shiny integrada ao RStudio que ajuda estudantes de Estatística a usar controle de versão com Git de forma acessível e intuitiva, sem precisar sair do editor ou aprender comandos no terminal.

Se você chegou aqui pelo GitHub ou por indicação, a ideia é simples: o **statgit** coloca dentro do RStudio um fluxo guiado para começar com Git, organizar projetos e conectar repositórios sem depender do terminal para as tarefas mais comuns.

Desenvolvido com foco em **educação**, o pacote oferece:
- **Configuração assistida** de Git e GitHub em português
- **Painel visual** para organizar projetos, fazer commits e conectar repositórios
- **Formatação automática** de código com `styler`
- **Tradução completa** da interface para português e inglês

## Por que foi criado?

O pacote foi criado para reduzir a barreira de entrada de Git e GitHub em disciplinas e projetos feitos no RStudio. Em vez de exigir comandos de terminal desde o início, ele oferece um fluxo guiado e visual para tarefas frequentes, com mensagens em linguagem acessível.

## Para quem é?

- Estudantes de cursos de Estatística que estão começando com controle de versão
- Professores que querem ensinar Git sem perder aula com instalações ou troubleshooting
- Pesquisadores que usam RStudio e querem manter projetos organizados com repositórios no GitHub

## Funcionalidades principais

- **Painel unificado** para todas as ações (visão geral, gerenciar projeto, arquivos, Git e GitHub)
- **Seleção de idioma** em português e inglês
- **Assistentes visuais** para Git (setup), primeiro commit, conexão com GitHub
- **Visualização de diffs** dentro do RStudio para inspecionar mudanças de arquivos sem depender de ferramentas externas
- **Organização automática** de projetos a partir de modelos
- **Formatação de código** integrada com `styler`
- **Status Git traduzido** para linguagem clara e amigável

## Se você quer testar rápido

1. Instale o pacote
2. Abra o RStudio
3. Vá em `Addins`
4. Clique em `statgit`
5. Use a aba **Git e GitHub** para verificar Git, criar o primeiro commit e conectar um remote

## Fluxo recomendado para uma primeira aula

1. Abrir o painel com `Addins > statgit`
2. Rodar o diagnóstico do Git
3. Configurar nome e email, se necessário
4. Inicializar Git no projeto
5. Fazer o primeiro commit
6. Conectar um remote GitHub
7. Enviar o histórico com `Push`

## Instalação

### CRAN

```r
install.packages("statgit")
```

Use esse caminho quando o pacote estiver disponível no CRAN.

### Versão de desenvolvimento

```r
remotes::install_github("geovani-ariel/statgit")
```

Use esse caminho enquanto a versão do CRAN ainda não estiver disponível ou para testar mudanças recentes.

### Localmente a partir do código-fonte

```r
install.packages(".", repos = NULL, type = "source")
```

## Uso rápido

### Pelo RStudio (recomendado)

1. Instale o pacote e reinicie o RStudio se necessário
2. Vá a `Addins` no menu superior
3. Clique em `statgit`
4. A interface abrirá em um painel lateral

Esse é o fluxo principal do pacote. As funções de console existem para uso programático ou complementar.

### Pelo console R

```r
library(statgit)

# Abre o painel principal
statgit()

# Funções individuais (uso programático ou complementar)
git_check()                    # diagnóstico do Git
git_init()                     # inicializa repositório
git_status()                   # status em linguagem clara
git_commit_all("Mensagem")     # commit de todos os arquivos
git_push()                     # envia commits ao remote
github_connect("https://...")  # conecta um remote GitHub
project_create("nome", template = "trabalho_disciplina")  # novo projeto
code_format_all()              # formata todo o código
```

## Painel principal

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

4. **Git e GitHub**
   - Wizard de configuração inicial
   - Gerenciar staging de arquivos
   - Fazer commits
   - Conectar ao GitHub
   - Push e pull

Todas as abas estão disponíveis em **português** e **inglês**. A escolha fica salva na sessão.

## Complementos (addins) do RStudio

Após instalar, estes complementos aparecem em `Addins`:

- **statgit** — abre o painel principal
- **Verificar Git do projeto** — diagnóstico rápido
- **Configurar Git neste projeto** — wizard de setup
- **Fazer primeiro commit** — cria o commit inicial de forma guiada
- **Ver status Git** — mostra status em linguagem clara
- **Formatar arquivo atual** — aplica `styler` ao arquivo ativo
- **Formatar projeto atual** — aplica `styler` a todo o código
- **Gerenciar projetos estatísticos** — localiza e abre `.Rproj`

## Funções principais

| Função | O que faz |
| --- | --- |
| `statgit()` | Abre o painel principal |
| `git_check()` | Diagnóstico didático do Git |
| `git_init()` | Inicializa repositório com branch `main` |
| `git_status()` | Status traduzido para linguagem clara |
| `git_commit_all()` | Cria commit com todos os arquivos rastreados/preparados |
| `git_push()` / `git_pull()` | Sincroniza com o remote configurado |
| `github_connect()` | Conecta um remote GitHub |
| `github_check()` | Testa acesso ao remote atual |
| `project_create()` | Cria projeto a partir de modelo |
| `project_organize()` | Organiza estrutura atual |
| `code_format()` / `code_format_all()` | Formata código |

## Modelos de projeto

Use `project_create()` ou a aba de gerenciar projeto para escolher:

- `analise_exploratoria` — análise de dados estruturada
- `trabalho_disciplina` — trabalho de disciplina
- `iniciacao_cientifica` — iniciação científica
- `tcc` — trabalho de conclusão de curso
- `artigo_quarto` — artigo com Quarto
- `projeto_grupo` — projeto colaborativo

## Observações de segurança

- Não solicita tokens do GitHub em texto aberto
- Usa as credenciais Git já configuradas localmente
- Operações destrutivas como `reset --hard` e `force push` não fazem parte do fluxo principal
- Não sobrescreve arquivos existentes sem confirmação

## Quando usar e quando não usar

Use o **statgit** para aprender Git dentro do RStudio, guiar estudantes no primeiro contato com controle de versão, organizar projetos acadêmicos e executar fluxos comuns como diagnóstico, commit, conexão com GitHub, `push` e `pull`.

Para fluxos avançados de colaboração, resolução complexa de conflitos, reescrita de histórico, múltiplas branches de longa duração ou automações de CI/CD, use Git diretamente pelo terminal ou por uma ferramenta especializada.

## Problemas comuns

| Problema | O que fazer |
| --- | --- |
| Git não foi encontrado | Instale o Git e reabra o RStudio antes de rodar o diagnóstico novamente |
| O GitHub pediu autenticação | Configure a autenticação local do Git/GitHub fora do pacote; o **statgit** usa essas credenciais |
| O `push` foi rejeitado porque o remote tem commits novos | Rode `Fetch` ou `Pull`, resolva eventuais conflitos e tente enviar novamente |
| O projeto aberto no painel não é o projeto ativo do RStudio | Use o botão de sincronização do painel ou abra explicitamente o projeto correto |

## Requisitos do sistema

- R ≥ 4.1.0
- RStudio (necessário para a experiência principal com addins e painel)
- Git instalado no computador
- Pandoc para pré-visualização de arquivos `.Rmd`
- Quarto opcional para pré-visualização de arquivos `.qmd`

## Limitações e escopo

- O pacote foi pensado para uso interativo, especialmente dentro do RStudio
- Nem todos os fluxos avançados de Git são cobertos pela interface
- Recursos que dependem de `shiny`, `miniUI`, `htmltools`, `styler`, `rmarkdown` ou Quarto só ficam disponíveis quando essas ferramentas estão instaladas

## Sobre o criador

O pacote é desenvolvido por **Geovani Ariel Bueno Paschoini**, com foco em ensino de Estatística, organização de projetos e adoção mais acessível de Git/GitHub no contexto do RStudio.

## Contribuindo

Encontrou um bug? Tem uma sugestão? Abra uma issue em [https://github.com/geovani-ariel/statgit/issues](https://github.com/geovani-ariel/statgit/issues)

## Licença

GPL-3 — Software livre e aberto para fins educacionais.

