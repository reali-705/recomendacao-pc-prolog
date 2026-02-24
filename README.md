# Sistema Especialista de Recomendação de Hardware (PC Builder)

**Instituição:** Universidade Federal do Pará (UFPA)  
**Disciplina:** Programação em Lógica  
**Professor:** Josivan Rodrigues dos Reis  
**Equipe:**

- Alessandro Reali Lopes Silva
- Jhonata Bezerra Figueiredo
- Kaleo Nabor Pimentel da Cunha

---

## 1. Contextualização e Problemática

A montagem de computadores desktop é um processo que envolve um alto grau de complexidade combinatória e risco financeiro. Para um usuário leigo, escolher componentes no mercado atual exige cruzar dezenas de especificações técnicas (gerações de processadores, soquetes de placa-mãe, padrões de memória volátil e requisitos de potência térmica/elétrica).

**A Problemática:** Escolhas incorretas geram dois cenários de falha críticos:

1. **Incompatibilidade Física/Lógica:** Componentes que não operam juntos (ex: processador AM4 em placa-mãe LGA1700, ou memórias DDR5 em slots DDR4).
2. **Gargalo de Sistema (Bottleneck):** Desperdício de orçamento ao parear componentes de alto desempenho com peças de entrada, limitando a eficiência computacional do sistema.

**A Solução:** Desenvolvemos um Sistema Especialista em Prolog que atua como um consultor automatizado. O algoritmo resolve esse Problema de Satisfação de Restrições (CSP - *Constraint Satisfaction Problem*), filtrando uma base de dados de hardware através de múltiplos níveis de análise para entregar configurações otimizadas e garantidas contra incompatibilidades.

---

## 2. Níveis de Análise e Questionamentos do Sistema

Para gerar uma recomendação válida, o motor de inferência submete a base de conhecimento a três camadas rígidas de questionamentos (filtros lógicos):

### Nível 1: Análise de Desempenho Alvo (Heurística de Resolução)

O sistema não recomenda peças aleatórias. A primeira restrição é baseada no caso de uso do cliente:

- *Pergunta do Sistema:* "Qual a resolução gráfica que o usuário deseja atingir (1080p, 1440p, 4K)?"
- *Lógica:* Cada resolução define um *threshold* (limite mínimo) rígido. Para 1080p, o sistema exige uma GPU com no mínimo 15.000 pontos no G3DMark, 16GB de RAM e fonte de 500W.

### Nível 2: Análise de Compatibilidade (Restrições de Hardware)

Esta é a validação estrutural da máquina. O algoritmo cruza as propriedades das peças para evitar falhas de montagem:

- *Pergunta do Sistema:* "O soquete da Placa-Mãe é idêntico ao exigido pela CPU?"
- *Pergunta do Sistema:* "A arquitetura da RAM (DDR4 ou DDR5) é suportada nativamente pela Placa-Mãe E pela geração do Processador?"
- *Lógica:* O Prolog utiliza unificação de variáveis para garantir que `SoqueteMB == SoqueteCPU` e `TipoRAM_MB == TipoRAM`. Se a unificação falhar, o *backtracking* descarta a árvore de possibilidades e busca outra peça.

### Nível 3: Análise de Viabilidade Financeira (Otimização de Orçamento)

- *Pergunta do Sistema:* "O somatório do custo das peças selecionadas cabe no bolso do usuário?"
- *Lógica:* O sistema soma o custo de GPU, CPU, Placa-Mãe, RAM, SSD e Fonte. Foi implementada uma heurística de tolerância de 20% (`OrcamentoComMargem is OrcamentoTotal * 1.2`) para evitar que o sistema descarte uma configuração muito superior por uma diferença ínfima de valor, retornando sempre as combinações com a menor variância (diferença) em relação ao alvo financeiro original.

---

## 3. Principais Regras e Estruturas Utilizadas

O motor de recomendação está centralizado no predicado `pc_recomendacao/3`. A execução segue a ordem lógica:

1. **Definição de Requisitos Base:** Consulta `requisitos_resolucao` para buscar os requisitos mínimos de G3DMark, RAM e Fonte baseados na entrada do usuário.
2. **Filtragem e Unificação:** Busca na base de fatos (ex: `gpu(...)`, `cpu(...)`) componentes que satisfaçam os limiares mínimos (`>=`).
3. **Validação Cruzada:** A regra `ram_compativel_cpu/2` é acionada para garantir compatibilidade da controladora de memória.
4. **Cálculo de Custo:** As variáveis de preço são agregadas (`PrecoTotal is ...`).
5. **Ordenação (*Sorting*):** O predicado `melhores_recomendacoes/3` utiliza o `findall` para gerar todas as árvores válidas de configuração e ordena o retorno através de `sort_by_difference`, entregando apenas o *Top 3* de configurações para evitar sobrecarga cognitiva no usuário final.

---

## 4. Manual de Execução Rápida

Para executar a simulação e testar as regras apresentadas:

1. Instale o ambiente SWI-Prolog.
2. No diretório raiz do projeto, inicialize a interface via terminal:

```bash
swipl -s main.pl
```

3. O Menu Principal será carregado. Para testar o núcleo do algoritmo de recomendação, selecione a Opção 1 ou utilize comandos interativos, como:

```Prolog
?- melhores_recomendacoes(5000, '1080p', Top3).
```

---
