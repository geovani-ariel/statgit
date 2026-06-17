# Diagnóstico de Interface - git4stats
**Documento de Avaliação UX/UI**  
Data: 2025-06-17  
Contexto: Painel Shiny para estudantes de Estatística aprendendo Git

---

## 1. DIAGNÓSTICO DA INTERFACE ATUAL

### Estrutura existente
A interface é um painel Shiny (gadget) em modo dark com:

- **Navegação horizontal (Top)**: 5 módulos principais em abas (Visão Geral, Gerenciar Projeto, Arquivos e Código, Git e GitHub, Relatórios)
- **Header/Summary sticky**: Mostra nome do projeto, status Git, commits, remote, branch, pendências em pills coloridas
- **Main content**: Conteúdo dinâmico por módulo, com formulários, tabs internas e ações
- **Footer expandível**: Terminal de execução com logs de ações
- **Tema**: Dark mode (Codex/Antigravity aesthetic) com cores:
  - Verde (#10B981) para sucesso
  - Amarelo (#F59E0B) para aviso
  - Vermelho (#EF4444) para erro
  - Azul (#3B82F6) para informação

### Fluxo principal do usuário

**Para iniciante (novo projeto):**
1. Abre o painel
2. Vê "Visão Geral" com status atual e ação recomendada
3. Vai para "Gerenciar Projeto" → "Criar" para novo projeto
4. Vai para "Git e GitHub" para seguir 5 passos estruturados
5. Usa "Arquivos e Código" para importar/criar arquivos
6. Usa "Relatórios" para preview

**Para usuário recorrente:**
1. Abre o painel
2. Vê "Visão Geral" mostrando status rápido
3. Vai para "Arquivos e Código" → "Versionar" para ver mudanças
4. Faz commit ou push via Git e GitHub

### Telas/Componentes principais

**Módulo 1: Visão Geral (Overview)**
- Caixa destacada com "Ação Recomendada" (fundo azul, texto branco)
- Timeline de commits (últimos 10) com autor, mensagem, hash, tempo
- Mostra histórico de forma visual

**Módulo 2: Gerenciar Projeto**
- Tab "Criar": Formulário para novo projeto com template selector (6 opções em grid 2x3)
- Tab "Abrir": Formulário para abrir existente + lista de projetos recentes

**Módulo 3: Arquivos e Código**
- Tab "Importar": Escolher arquivo + pasta destino + checkboxes
- Tab "Criar": Nome, tipo, destino, conteúdo inicial + sugestões
- Tab "Gerenciar" (subtabs):
  - Formatar: Scripts com styler
  - Renomear: Mover/renomear arquivos
  - Organizar: Criar estrutura padrão
  - Excluir: Deletar com confirmação
- Tab "Versionar": Diff visual, stage, commit, discard

**Módulo 4: Git e GitHub**
- Layout grid 2 colunas
- Coluna esquerda: 3 passos (Identidade, Git local, Salvar versão)
- Coluna direita: 2 passos (Conectar GitHub, Enviar)
- Cada passo tem título numerado com estado (pending/complete/blocked)
- Disks ativos/desativados com razão de bloqueio
- Feedback de sucesso (pills verdes) quando etapa completa

**Módulo 5: Relatórios**
- Seletor de arquivo .qmd/.Rmd
- Botões: Gerar Preview, Live Preview

### Informações apresentadas

**Sempre visível (Summary bar):**
- Nome do projeto (azul, grande)
- Caminho do projeto (cinza pequeno)
- Pills de status: Git, Histórico, GitHub, Modificações

**Em context (varia por módulo):**
- Arquivos com mudanças (cores: verde=staged, amarelo=modified, vermelho=untracked)
- Estado de cada passo de Git (check mark=complete, lock=blocked, number=pending)
- Razão de bloqueio em cada etapa
- Timeline de commits com metadata

### Próxima ação aparente para o usuário

**Na Overview**: Caixa destacada com "Ação Recomendada" + implementação de `next_step_message()`
**Em Git**: Ordem visual clara com numeração (1-5)
**Em Arquivos/Mudanças**: Context-dependent (mostra próximo passo se houver)

---

## 2. PONTOS FORTES QUE DEVEM SER PRESERVADOS

### Decisões de arquitetura que funcionam bem

✅ **Navegação por módulos (5 abas)**: Essencial. Mantém navegação simples, consistente.  
✅ **Layout dark mode com acentos coloridos**: Profissional, legível, adequado a público técnico.  
✅ **Summary bar sticky**: Sempre mostra contexto do projeto. Referência constante sem perder espaço.  
✅ **Pills de status**: Comunicação visual clara de estado. Cores funcionam bem (verde/amarelo/vermelho).  
✅ **Numeração de passos em Git (1-5)**: Clareza didática. Estudante vê progressão natural.  
✅ **Caixa de "Ação Recomendada"**: Reduz incerteza. Orienta próximo passo sem sobrecarregar.  
✅ **Diff visual embutido**: Permite inspecionar mudanças sem terminal.  
✅ **Timeline de commits**: Contexto de histórico sem sair da interface.  
✅ **Modais de confirmação em ações perigosas**: Proteção contra erros (delete, discard).  
✅ **Execution log expandível**: Feedback de operações sem sobrecarregar tela principal.  
✅ **Tree view de arquivos**: Visualiza estrutura do projeto intuitivamente.  
✅ **Templates visuais de projeto**: Economia de tempo, menos decisões.  

**Nada deste deve mudar de arquitetura.**

---

## 3. PROBLEMAS PRIORIZADOS DE UX/UI

### Críticos (afetam compreensão ou segurança)

#### P1: Hierarquia de informação no módulo de Versionar (Arquivos)
**Problema**: Quando há mudanças, o módulo "Versionar" mostra seções em ordem:
1. "O que mudou? (Diff)"
2. "Salvar nova versão (Commit)"
3. "Zona de Perigo"
4. "Resumo"
5. "Diff visual"

**Por que é problema**: A seção "Zona de Perigo" aparece depois de "Commit" e antes do "Resumo". Visualmente sugere que é uma ação normal, não uma alternativa perigosa. Estudante iniciante pode clicar "Descartar alterações" por distração sem entender a consequência.

**Impacto**: Risco de perda de dados. Ação irreversível não parece claramente separada.

---

#### P2: Texto técnico em jargão Git
**Problema**: Termos como aparecem sem tradução contextual:
- "untracked" (aparece em badges verde como "não rastreado" - bom!)
- "modified" (aparece em badges como "modificado" - bom!)
- "staged/preparado" (aparece bem)
- Mas em explicações longas: "Controle Fino de Mudanças", "repositório remoto", "branch", "hash"

**Por que é problema**: Estudante da disciplina de Estatística pode não ter contexto. Termos técnicos aparecem quando há erro ou bloqueio, aumentando confusão.

**Impacto**: Barreira cognitiva. Usuário tem que traduzir + entender Git + entender a interface.

---

#### P3: Razões de bloqueio pouco claras
**Problema**: Quando um passo está bloqueado (lock icon), aparece um texto como:
- "Git não encontrado no computador."
- "Inicialize o Git antes de criar commits."
- "Configure nome e email antes de criar commits."

Mas não mostra **por que isso importa** ou **qual é o impacto real**.

**Por que é problema**: Não é óbvio por que Nome/Email é pré-requisito. Estudante pode pular ou ignorar.

**Impacto**: Usuário não aprende o mínimo de Git necessário.

---

#### P4: Feedback de sucesso é rápido demais
**Problema**: Quando faz um commit ou push, mensagem aparece no log (terminal expandível) mas não na área principal. Usuário não sabe se funcionou sem abrir o log ou esperar refresh.

**Por que é problema**: Ação executada sem confirmação visual clara na área de trabalho. Padrão de UX quebrado.

**Impacto**: Incerteza após ação. Usuário pode repetir a ação ou ficar confuso.

---

### Altos (afetam clareza, navegação ou aprendizado)

#### P5: Pills de status no header não explicam o que fazer
**Problema**: Pills mostram estado (Git ativo, 0 commits, GitHub não conectado, 3 modificações) mas não indicam qual precisa atenção AGORA.

**Por que é problema**: Em um projeto novo, há múltiplas pills em estado "warn" ou "error". Qual é prioritário?

**Impacto**: Usuário fica em paralisia de decisão. Não sabe por onde começar se não lê a caixa "Ação Recomendada".

---

#### P6: Fluxo de Git e GitHub é sequencial demais
**Problema**: Passos de Git (1-5) devem ser feitos em ordem rigorosa. Mas UI não mostra:
- Por que essa ordem importa
- O que cada passo HABILITA para o próximo
- Relação de dependência visual

**Por que é problema**: Parece um wizard mecânico, não um aprendizado. Estudante não entende a lógica por trás.

**Impacto**: Usuário segue passos sem compreender o sistema. Quando algo quebra, fica perdido.

---

#### P7: Estados ambíguos em arquivos com mudanças
**Problema**: Arquivo pode estar:
- Modificado (não staged)
- Modificado e staged
- Novo (untracked)
- Novo e staged
- Removido
- Removido e staged

Badges mostram isso, mas a combinação pode ser confusa. Exemplo: Um arquivo é "new" + "preparado"? Qual é a cor?

**Por que é problema**: 4 estados visuais (verde, amarelo, vermelho, + combinações) podem não ser suficientes ou podem ficar confusos.

**Impacto**: Usuário não tem confiança se arquivo está realmente pronto para commit.

---

#### P8: Mensagem de conclusão de projeto limpo é exitosa demais
**Problema**: Quando não há mudanças, aparece:
> "🎉 Seu projeto está limpo! Você não possui nenhuma mudança pendente no momento."

Com fundo verde, estilo celebração.

**Por que é problema**: Estudante pode pensar que já terminou tudo e não precisa continuar trabalhando. Ou que não precisa fazer commit. A mensagem é ambígua sobre o que fazer a seguir.

**Impacto**: Confusão sobre o objetivo real (trabalhar + salvar versões, não apenas manter projeto "limpo").

---

### Médios (melhoram profissionalismo, mas não quebram funcionalidade)

#### P9: Espaçamento inconsistente entre seções
**Problema**: Algumas seções têm `margin-bottom: 24px`, outras `16px`. Alguns inputs têm `gap: 16px`, outros `gap: 10px`.

**Por que é problema**: Interface parece não refinada. Não é "quebrado", mas dá sensação de prototipagem.

**Impacto**: Menor confiança percebida no produto. Sente-se inacabado para profissional.

---

#### P10: Cores de pills não são totalmente acessíveis
**Problema**: 
- Pill `.warn` tem fundo amarelo + border/texto amarelo (contraste baixo em alguns monitores)
- Pill `.error` tem fundo vermelho escuro + texto vermelho (OK, mas border é problema)

**Por que é problema**: Usuário com visão reduzida pode não distinguir status.

**Impacto**: Acessibilidade reduzida. Não segue WCAG AAA.

---

#### P11: Scroll no módulo de Arquivos fica confuso
**Problema**: Quando há muitos arquivos, tree view fica longo. Tree card não tem delimitação clara de fim. Usuário não sabe se terminou de ver tudo.

**Por que é problema**: Falta de closure visual. Interface não mostra fim da lista.

**Impacto**: Usuário pode perder arquivo se não ver no scroll.

---

---

## 4. RECOMENDAÇÕES DE MELHORIA (inspiradas em GitLens)

### Princípios do GitLens que aplicamos aqui

**GitLens torna Git visível, contextual e acionável.** Usamos:

1. **Visibilidade de estado**: Pills/badges sempre presentes, cores consistentes
2. **Contexto antes de ação**: "Ação Recomendada" + mensagens de bloqueio
3. **Histórico acessível**: Timeline de commits + diff visual
4. **Redução de incerteza**: Confirmações modais em ações perigosas
5. **Hierarquia visual clara**: Numeração (1-5), cores, tamanhos de fonte

### Melhorias propostas

#### M1: Separar "Zona de Perigo" visualmente
**Mudança proposta:**
- Mover seção "Zona de Perigo" para DEPOIS do "Diff visual" (fim do módulo)
- Adicionar border esquerda VERMELHA `border-left: 4px solid #DC2626`
- Adicionar warning subtil: "⚠️ Ações irreversíveis:"
- Manter botão vermelho, mas deixar claro que é alternativa, não fluxo principal

**Por que melhora:**
- Estrutura narrativa: Primeiro você vê as mudanças, depois comita, depois (se errou) descarta
- Separação visual clara: Vermelho à esquerda sinaliza perigo
- Aprendizado: Usuário entende que discard é exceção, não regra

**Risco:**
- Seção fica mais abaixo, pode não ser vista. MITIGADO: Keep footer sticky ou manter scroll awareness

---

#### M2: Adicionar explicação de CONTEXTO para cada passo de Git
**Mudança proposta:**

Para cada passo (1-5), adicionar uma linha curta sob o título:

```
[1] ✓ Identidade
└─ Seu nome aparecerá no histórico como quem fez a mudança

[2] ✗ Git local
└─ Inicialize Git para criar versões do seu projeto

[3] 🔒 Salvar versão
└─ Bloqueado: configure nome e email antes de criar versões

[4] ⊙ Conectar GitHub
└─ Crie backup das versões na nuvem (opcional no início)

[5] ✗ Enviar
└─ Compartilhe versões com colegas ou professores
```

**Implementação:**
- Adicionar `<small>` com cor `#71717A` (cinza) abaixo cada `git_step_title()`
- 1 frase curta, máx 80 caracteres
- Explica o PORQUÊ, não o COMO

**Por que melhora:**
- Contexto pedagógico: Estudante entende lógica de Git, não apenas passos
- Reduz jargão: Traduz para linguagem do domínio (versão, histórico, backup, compartilhar)
- Motiva: Explica benefício de cada passo

**Risco:**
- Adiciona 5-10 linhas no módulo Git. MITIGADO: Linhas são pequenas, layout grid absorve bem

---

#### M3: Adicionar status visual de "Pendências à frente"
**Mudança proposta:**

Na summary bar, em vez de simplesmente mostrar `"3 Modificações"`, adicionar ícone de ação:

```
✎ Pendências
3 arquivos mudaram

[Próximo passo recomendado]
└─ Ver e salvar versão (Commit)
```

Clicável para ir direto à seção "Versionar".

**Implementação:**
- Adicionar link/botão na pill de "Modificações"
- Se há modificações, destacar em amarelo + `cursor: pointer`
- Ao clicar, jumpa para módulo "Arquivos" > aba "Versionar"

**Por que melhora:**
- Reduz busca: Usuário não precisa navegar para saber próximo passo
- Ativa CTA: Chama atenção se há trabalho não salvo
- Fluxo intuitivo: Modificações → Commit é a ação natural

**Risco:**
- Pode ser intrusivo se mudar de projeto frequentemente. MITIGADO: Apenas mostra se há genuínamente mudanças

---

#### M4: Melhorar cores de pills de status para acessibilidade
**Mudança proposta:**

Aumentar contraste em pills `.warn` e `.error`:

**Antes:**
```css
.tr-pill.warn {
  border-color: #D97706;        /* amarelo 600 */
  background: rgba(217, 119, 6, 0.1);
}
.tr-pill.warn .tr-pill-value { 
  color: #FBBF24;               /* amarelo 300 */
}
```

**Depois:**
```css
.tr-pill.warn {
  border-color: #B45309;        /* amarelo 700 (mais escuro) */
  background: rgba(217, 119, 6, 0.15);
}
.tr-pill.warn .tr-pill-value { 
  color: #FCD34D;               /* amarelo 400 (mais claro) */
}
```

**Similar para `.error`:**
```css
.tr-pill.error {
  border-color: #991B1B;        /* vermelho 800 */
  background: rgba(220, 38, 38, 0.15);
}
.tr-pill.error .tr-pill-value { 
  color: #FCA5A5;               /* vermelho 300 */
}
```

**Por que melhora:**
- Contraste WCAG AAA em todos os monitores
- Cores ainda reconhecíveis
- Mais profissional (menos "neon")

**Risco:**
- Mudança visual pequena. Sem risco.

---

#### M5: Remover/redesenhar emoji em seções de vazio
**Mudança proposta:**

Quando não há commits ou mudanças, remover:
- 🎉 (celebração)
- 🔍 (lupa)
- ⚠️ (warning)

E substituir por ícones Font Awesome (já usados na interface):
- `shiny::icon("check-circle")` para sucesso
- `shiny::icon("exclamation-circle")` para aviso
- `shiny::icon("info-circle")` para informação

**Implementação:**
```r
shiny::div(
  style = "padding: 40px 20px; text-align: center; color: #1a7f37; background: #dafbe1; border-radius: 6px; margin-top: 15px;",
  shiny::icon("check-circle", style = "font-size: 32px; margin-bottom: 12px;"),
  shiny::h3("Seu projeto está limpo!"),
  shiny::p("Você não possui nenhuma mudança pendente no momento.")
)
```

**Por que melhora:**
- Consistência: Mesmo ícone set em toda interface
- Profissionalismo: Emojis parecem casuais em contexto educacional
- Acessibilidade: Leitores de tela interpretam Font Awesome melhor

**Risco:**
- Usuários podem achar ícones menos "divertidos". Aceitável para público educacional.

---

#### M6: Adicionar "Blocos de Estado" na Overview
**Mudança proposta:**

Na página "Visão Geral", adicionar seção visual ANTES do timeline, resumindo:

```
┌─────────────────────────────────────────────────────────┐
│ ESTADO DO SEU PROJETO                                   │
├─────────┬──────────┬─────────┬──────────┬──────────────┤
│ Git     │ Histórico│ Remote  │ Branch   │ Modificações │
│ ✓ Ativo │ ✓ 5+    │ ✗ None  │ ⊙ main  │ ⚠️ 3 arquivos│
├─────────┴──────────┴─────────┴──────────┴──────────────┤
│ PRÓXIMO PASSO: Conecte um repositório GitHub            │
│ [Ir para Git e GitHub]                                  │
└─────────────────────────────────────────────────────────┘
```

**Implementação:**
- Componente novo após caixa de "Ação Recomendada"
- Grid 5 colunas, cada coluna é um card pequeno
- Cada card tem: label, valor, icon, cor
- Abaixo, sugestão de "Próximo passo" com botão
- Se projeto novo, versão simplificada (apenas Git + História)

**Por que melhora:**
- Dashboard visual: Resumo em 1 olhada vs. ler pills descentralizadas
- Sequência narrativa: Estado → Ação Recomendada → Timeline
- Reduz scroll: Informação densa mas legível

**Risco:**
- Adiciona conteúdo em Overview. MITIGADO: Comprime bem em grid, não quebra layout

---

#### M7: Melhorar microcopy em mensagens de bloqueio
**Mudança proposta:**

Em vez de:
```
"Configure nome e email antes de criar commits."
```

Usar:
```
"Para salvar uma versão, preciso saber seu nome (você aparecerá 
no histórico). Configure na aba 'Identidade' → Salvar identidade."
```

**Implementação:**
- Editar textos em `disabled_reason_ui()` e descrições de passos
- Adicionar contexto de por que importa
- Adicionar breadcrumb ou referência de onde fazer

**Por que melhora:**
- Pedagogia: Explica lógica, não apenas regra
- Acionável: Diz exatamente onde ir para resolver
- Reduz frustração: Usuário sente que há motivo, não apenas restrição

**Risco:**
- Textos ficam mais longos. MITIGADO: Usar `<small>` ou reduzir tamanho de fonte

---

---

## 5. AJUSTES DE MICROCOPY

### Termos a revisar/clarificar

| Termo Atual | Contexto | Proposta | Razão |
|-------------|----------|----------|-------|
| "Controle Fino de Mudanças" | Título de módulo | "Inspecionar e salvar mudanças" | Menos técnico |
| "untracked" | Badge verde em arquivo | "não rastreado" ✓ | Já está bem |
| "modified" | Badge amarelo | "alterado" ou "modificado" ✓ | Já está bem |
| "Zona de Perigo" | Seção de delete | "⚠️ Cuidado: Ações irreversíveis" | Mais claro o risco |
| "Discard" | Botão vermelho | "Descartar alterações (Apagar para sempre)" ✓ | Já está bem |
| "Push" | Ação | "Enviar para GitHub" ✓ | Já está bem |
| "Pull" | Ação | "Baixar mudanças" ✓ | Já está bem |
| "Remoto" | Remote | "Repositório no GitHub" | Menos técnico para iniciantes |
| "branch" | Em pills/descrição | "versão de trabalho" ou deixar "branch" se explicado | Pedagogical choice |
| "hash" | Em timeline | "Código identificador" ou "ID" | Mais acessível |

---

## 6. SUGESTÕES DE REFINAMENTO VISUAL

### Espaçamento e Consistência

**Problema**: Gaps variados (10px, 12px, 16px, 18px)

**Solução** (implementar em CSS):
```css
/* Define escala harmônica */
--gap-xs: 8px;    /* espaço mínimo entre inline elements */
--gap-sm: 12px;   /* espaço entre form inputs */
--gap-md: 16px;   /* espaço entre seções internas */
--gap-lg: 24px;   /* espaço entre major seções */
--gap-xl: 32px;   /* espaço entre blocos independentes */
```

Auditoria de uso:
- `tr-checkbox-group`: usar `--gap-md` (16px) ✓ já correto
- `tr-git-layout`: usar `--gap-lg` (18px) → mudar para 24px para consistency
- Form inputs: usar `--gap-md` (16px) consistentemente
- Buttons row: usar `--gap-sm` (10px) ✓ OK para compactação

---

### Tipografia

**Problema**: Hierarchy de fonts poderia ser mais clara

**Proposta:**
- `h4` (títulos de seção): 16px, weight 600 ✓ OK
- `h5` (subtítulos): 14px, weight 600 ✓ OK
- Body text: 14px, weight 400 ✓ OK
- Small labels: 12px, weight 500 ✓ OK
- Monospace (code/paths): 12px, `font-family: Menlo` ✓ OK

**Adicionar escala para hints/labels:**
```css
.label-hint {
  font-size: 11px;
  color: #A1A1AA;
  font-weight: 400;
  line-height: 1.4;
}
```

Usar em: bloqueio de razões, descrições de passo, espaço abaixo título de seção

---

### Refinamento de interações

**Problema**: Falta feedback visual sutil em hover/focus em alguns componentes

**Solução**:
- Form inputs: adicionar transition `border-color 0.15s ease` ✓ já tem
- Buttons: adicionar `transform: translateY(-1px)` em hover (lift effect subtil)
- Pills: adicionar `transition: all 0.15s ease` para mudança de cor ao hover (se clicável)
- Tree items: adicionar `transition: background 0.1s ease` ✓ já tem

---

---

## 7. MUDANÇAS PROPOSTAS COM JUSTIFICATIVA

### Mudança 1: Reorganizar seções de "Arquivos e Código" > "Versionar"

**Problema identificado:**
Ordem atual: Diff → Commit → Zona de Perigo → Resumo → Diff Visual.  
"Zona de Perigo" aparece como ação sequencial, não como alternativa.

**Mudança proposta:**
Reordenar: Diff → Commit → Diff Visual → Zona de Perigo

**Justificação:**
- Fluxo narrativo correto: vê mudanças → comita → (optionally) revisa e descarta
- Zona de Perigo não é next step, é contingency
- Hierarquia visual: final da página = menos importante
- GitLens principle: ações destrutivas no fim, com warning claro

**Risco:**
- Usuário que quer descartar precisa scrollar. MITIGADO: Keep "Zona de Perigo" title visible com background bold

---

### Mudança 2: Adicionar sub-descrição para cada passo de Git

**Problema identificado:**
Passos 1-5 parecem mecânicos. Estudante não entende por que essa ordem ou impacto.

**Mudança proposta:**
Adicionar linha pequena sob cada `git_step_title()`:

```r
git_step_title("1", "Identidade", complete = ...) %>%
  + shiny::div(
      class = "tr-step-description",
      "Seu nome aparecerá no histórico de versões"
    )
```

**CSS novo:**
```css
.tr-step-description {
  font-size: 11px;
  color: #71717A;
  margin-bottom: 12px;
  line-height: 1.4;
  font-weight: 400;
}
```

**Justificação:**
- Pedagogia: Explica lógica de Git em 1-2 linhas
- Microcopy clara: Usa linguagem de "versão", "histórico", não "commit hash"
- Reduz jargão: Traduz termos técnicos para contexto de estudante

**Risco:**
- Adiciona ~30 pixels altura no módulo. MITIGADO: Texto é pequeno, layout grid absorve

---

### Mudança 3: Destacar "Próximo passo" em pills de status quando há pendências

**Problema identificado:**
Pills de status mostram estado mas não orientam ação. Em projeto novo, múltiplas pills em warn/error.

**Mudança proposta:**
Quando há modificações pendentes, pill "Modificações" fica clicável e com CTA:

```html
<div class="tr-pill warn interactive">
  <span class="tr-pill-label">Pendências</span>
  <span class="tr-pill-value">3 arquivos</span>
  <span class="tr-pill-action">→ Ver</span>
</div>
```

**CSS novo:**
```css
.tr-pill.interactive {
  cursor: pointer;
  transition: all 0.15s ease;
}
.tr-pill.interactive:hover {
  transform: translateX(2px);
  border-color: #FBBF24;
}
.tr-pill-action {
  font-size: 10px;
  color: #F59E0B;
  font-weight: 600;
}
```

**JavaScript:**
```javascript
$(".tr-pill.interactive").on("click", function() {
  // Navigate to Arquivos > Versionar
  $("#module-nav").find("input[value='files']").click();
  // Jump to Versionar tab
  // ...
});
```

**Justificação:**
- UX pattern: CTA clicável reduz friction
- Orientação: Usuário não fica em paralisia de decisão
- Eficiência: 1 clique vs. navegar + achar tab

**Risco:**
- Adiciona click handler. MITIGADO: Handler é simples, sem side effects
- Pode parecer agressivo se modificações são esperadas. MITIGADO: Apenas ativa se há > 0 mudanças

---

### Mudança 4: Melhorar acessibilidade de pills (contraste)

**Problema identificado:**
Pills `.warn` e `.error` têm contraste baixo em alguns monitores.

**Mudança proposta:**
Aumentar `border-color` para tom mais escuro, aumentar `color` para tom mais claro.

**Implementação** (em CSS):
```css
.tr-pill.warn {
  border-color: #B45309;  /* era #D97706 */
  background: rgba(217, 119, 6, 0.15);  /* era 0.1 */
}
.tr-pill.warn .tr-pill-value { 
  color: #FCD34D;  /* era #FBBF24 */
}

.tr-pill.error {
  border-color: #991B1B;  /* era #DC2626 */
  background: rgba(220, 38, 38, 0.15);  /* era 0.1 */
}
.tr-pill.error .tr-pill-value { 
  color: #FCA5A5;  /* era #F87171 */
}
```

**Justificação:**
- Acessibilidade: Atinge WCAG AAA em contraste de cores
- Consistência: Amarelo e vermelho agora são harmonicamente escuros
- Profissionalismo: Menos "neon", mais sutil

**Risco:**
- Mudança visual pequena, sem risco funcional

---

### Mudança 5: Remover/substituir emojis por ícones consistentes

**Problema identificado:**
Emojis (🎉, 🔍, ⚠️) aparecem em mensagens de estado. Ícones não correspondem ao resto da interface (Font Awesome).

**Mudança proposta:**
Substituir:
- 🎉 → `shiny::icon("check-circle")` verde
- 🔍 → `shiny::icon("search")`
- ⚠️ → `shiny::icon("exclamation-triangle")`
- ✓ (check) → manter (é simples e universal)

**Implementação**:
```r
# Em lugar de:
shiny::h3("🎉 Seu projeto está limpo!")

# Usar:
shiny::div(
  class = "tr-state-success",
  shiny::icon("check-circle", style = "font-size: 28px; margin-bottom: 12px;"),
  shiny::h3("Seu projeto está limpo!")
)
```

**CSS novo:**
```css
.tr-state-success {
  text-align: center;
  color: #1a7f37;
  margin-bottom: 12px;
}
.tr-state-success i {
  color: #34D399;
  display: block;
}
```

**Justificação:**
- Consistência: Mesmo ícone set em toda interface
- Profissionalismo: Emojis parecem casuais em contexto educacional
- Acessibilidade: Font Awesome é acessível; emojis podem ser ambíguos

**Risco:**
- Usuários podem achar ícones menos "divertidos". Aceitável para público acadêmico.

---

---

## 8. PLANO INCREMENTAL DE IMPLEMENTAÇÃO

### Fase 1: Crítico (2-3 horas implementação)
1. ✅ M1: Reorganizar "Zona de Perigo" em "Versionar"
2. ✅ M4: Melhorar acessibilidade de pills (cores)
3. ✅ M5: Substituir emojis por ícones

**Por quê**: Afetam segurança, clareza e profissionalismo. Rápidas de implementar.

### Fase 2: Alto (4-5 horas)
4. ✅ M2: Adicionar sub-descrição em passos de Git
5. ✅ Revisar/aprimorar microcopy conforme Seção 5

**Por quê**: Pedagógico. Melhora compreensão sem quebrar fluxo existente.

### Fase 3: Médio (3-4 horas)
6. ✅ M3: Adicionar "Próximo passo" em pill de modificações
7. ✅ M6: Adicionar "Blocos de Estado" na Overview

**Por quê**: Reduzem fricção, melhoram navegação.

### Fase 4: Refinamento (2-3 horas)
8. ✅ Auditoria de espaçamento (Seção 6)
9. ✅ Auditoria de transições/hover
10. ✅ Testes de acessibilidade

**Por quê**: Polimento final. Profissionalismo.

---

---

## 9. CRITÉRIOS DE ACEITE

A otimização será considerada bem-sucedida se:

- [ ] Interface continua reconhecível (não foi redesenhada do zero)
- [ ] Usuário novo consegue identificar o "próximo passo" em < 10s
- [ ] Estado do projeto é compreendido em 1 olhada (pills + summary)
- [ ] Passos de Git (1-5) têm contexto claro (por que essa ordem)
- [ ] Ações perigosas (delete, discard) estão claramente separadas visualmente
- [ ] Textos são menos técnicos (versão, salvar, histórico em vez de commit, branch, hash)
- [ ] Cores e ícones são consistentes em toda interface
- [ ] Contraste atinge WCAG AA mínimo, AAA onde possível
- [ ] Feedback de ação é claro (notificação visual, não apenas log)
- [ ] Seções não parecem "prototipadas" (espaçamento, alinhamento)

---

## 10. PRÓXIMOS PASSOS

1. **Validação**: Você revisa este documento e confirma priorização
2. **Implementação Phase 1**: Reorganizar Zona de Perigo, cores, ícones
3. **Teste de UI**: Abrir painel, confirmar que mudanças visuais funcionam
4. **Implementação Phase 2+3**: Microcopy, descrições de passo, CTA
5. **Testes de UX**: Pedir feedback de 1-2 usuários iniciantes (estudantes)
6. **Iteração**: Ajustar conforme feedback

---

**Fim do diagnóstico.**
