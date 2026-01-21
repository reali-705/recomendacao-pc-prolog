# Sistema de Recomendação de Componentes para PC

## 📚 Informações Acadêmicas

**Instituição:** Universidade Federal do Pará (UFPA)  
**Disciplina:** Programação em Lógica  
**Professor:** Josivan Rodrigues dos Reis  

**Integrantes do Grupo:**

- Alessandro Reali Lopes Silva
- Jhonata Bezerra Figueiredo
- Kaleo Nabor Pimentel da Cunha

---

## 📋 Sobre o Projeto

Sistema especialista em Prolog para recomendação de componentes de PC baseado em orçamento e resolução de jogos desejada (1080p, 1440p ou 4K).

### Componentes Recomendados:

- GPU (Placa de Vídeo)
- RAM (Memória)
- SSD (Armazenamento)
- Fonte de Alimentação
- Placa-Mãe

---

## 🚀 Como Executar

### 1. Pré-requisitos

- **SWI-Prolog** instalado ([Download](https://www.swi-prolog.org/Download.html))
- Verificar se está nas variáveis de ambiente: `C:\Program Files\swipl\bin`

### 2. Iniciar o Sistema

Abra o terminal na pasta do projeto e execute:

```powershell
swipl -s main.pl
```

Você verá a mensagem:

```prolog
=== SISTEMA DE RECOMENDACAO DE PC ===
Consulte usando: "melhores_recomendacoes(Orcamento, Resolucao, Top3)."
Exemplo: melhores_recomendacoes(5000, '1080p', Top3).

?- 
```

---

## 💻 Comandos de Consulta

### 1. Buscar as 3 Melhores Recomendações

```prolog
?- melhores_recomendacoes(5000, '1080p', Top3).
```

**Parâmetros:**

- `5000` - Orçamento em Reais (BRL)
- `'1080p'` - Resolução desejada (`'1080p'`, `'1440p'` ou `'4k'`)
- `Top3` - Variável que receberá as recomendações

### 2. Exibir Recomendação Formatada

Após obter uma recomendação, exiba-a formatada:

```prolog
?- melhores_recomendacoes(5000, '1080p', [PC|_]), mostrar_recomendacao(PC).
```

**Saída esperada:**

```prolog
=== RECOMENDACAO PARA 1080p ===
Preco Total: R$ 4869.59 (Diferença do orçamento: R$ 130.41)

Componentes:
- GPU: NVIDIA RTX 4060 Ti (8GB, 19540 pontos) - R$ 2799.90
- RAM: Corsair Vengeance LPX (16GB, 3200MHz) - R$ 249.90
- SSD: Kingston NV2 (1000GB, 3500 MB/s) - R$ 299.99
- Fonte: Cooler Master MWE Gold 850 V3 (850W, 80 Plus Gold) - R$ 449.99
- Placa-mae: ASUS TUF Gaming B550-PLUS (AM4, B550) - R$ 899.90
```

### 3. Consultas Auxiliares

#### Recomendar GPU por desempenho

```prolog
?- recomendar_gpu_desempenho(20000, GPU).
```

#### Recomendar Fonte por potência

```prolog
?- recomendar_fonte(500, 'qualquer', Fonte).
```

#### Recomendar SSD por capacidade

```prolog
?- recomendar_ssd(500, 'qualquer', SSD).
```

#### Verificar compatibilidade RAM/Placa-Mãe

```prolog
?- compativel_ram_placa('TUF Gaming B550-PLUS', RAM).
```

---

## 📂 Estrutura do Projeto

```bash
trab-prolog/
├── main.pl          # Arquivo principal (ponto de entrada)
├── dados.pl         # Base de dados de componentes
├── regras.pl        # Regras de recomendação
└── README.md        # Documentação
```

---

## 🎮 Requisitos por Resolução

| Resolução | G3DMark Mín. | RAM Mín. | SSD Mín. | Fonte Mín. |
| --- | --- | --- | --- | --- |
| 1080p | 15.000 | 16 GB | 500 GB | 500W |
| 1440p | 25.000 | 16 GB | 1000 GB | 650W |
| 4K | 35.000 | 32 GB | 1000 GB | 750W |

---

## 🛠️ Comandos Úteis do SWI-Prolog

| Comando | Descrição |
| --- | --- |
| `halt.` | Sair do interpretador |
| `Ctrl + D` | Sair (atalho) |
| `Ctrl + C` → `e` | Sair |
| `Ctrl + C` → `a` | Abortar consulta atual |
| `listing(predicado).` | Mostrar definição de predicado |
| `trace.` | Ativar modo debug |
| `notrace.` | Desativar modo debug |

---

## 📝 Exemplos de Uso

```prolog
% Exemplo 1: PC para 1080p com orçamento de R$ 5000
?- melhores_recomendacoes(5000, '1080p', Top3).

% Exemplo 2: PC para 1440p com orçamento de R$ 8000
?- melhores_recomendacoes(8000, '1440p', [PC|_]), mostrar_recomendacao(PC).

% Exemplo 3: PC para 4K com orçamento de R$ 12000
?- melhores_recomendacoes(12000, '4k', Top3).
```
