# konane-scala-javafx

# Kōnane - Jogo Multiparadigma (Scala 3 & JavaFX) ♟️

Este projeto consiste na implementação do histórico jogo de tabuleiro havaiano **Kōnane**[cite: 4]. O desenvolvimento foi estruturado seguindo uma arquitetura clássica de camadas, dividindo a lógica de negócio e a camada de apresentação através de diferentes paradigmas de programação[cite: 4].

Trabalho desenvolvido no âmbito da unidade curricular de **Programação Multiparadigma** no **ISCTE-IUL**[cite: 4].

---

## 📐 Arquitetura e Paradigmas Aplicados

O projeto tira partido do melhor de dois mundos (desenvolvimento multiparadigma) para garantir performance, legibilidade e manutenibilidade[cite: 4]:

### 1. Camada de Negócio (Programação Funcional Pura em Scala 3)
Desenvolvida estritamente sob os princípios de imutabilidade e funções puras (sem efeitos secundários)[cite: 4]:
*   **Imutabilidade:** O estado do tabuleiro e a lista de coordenadas livres são tratados como dados imutáveis em todas as transições de jogo[cite: 4].
*   **Geração de Estado Puro (`MyRandom`):** Implementação de geradores de números pseudoaleatórios puros para garantir decisões aleatórias reproduzíveis no computador[cite: 4].
*   **Recursividade:** Substituição completa de ciclos imperativos (`for`/`while`) por recursividade (focando em *tail recursion* para otimização de memória)[cite: 4].
*   **Coleções Paralelas:** Utilização de `ParMap` para processamento paralelo das coordenadas do tabuleiro, otimizando o desempenho de validações de jogadas[cite: 4].

### 2. Camada de Apresentação (Programação Orientada a Eventos)
Duas interfaces distintas que comunicam de forma desacoplada com a camada de negócio[cite: 4]:
*   **TUI (Text-based User Interface):** Interface de linha de comandos interativa construída para configuração de tabuleiro, definição de tempos e execução de jogadas sequenciais[cite: 4].
*   **GUI (Graphical User Interface em JavaFX):** Interface gráfica interativa (tabuleiro 6x6) orientada a eventos, com deteção inteligente de jogadas válidas ao passar o cursor e suporte a ações rápidas com cliques[cite: 4].

---

## 🌟 Funcionalidades Implementadas

*   **Validação de Capturas Múltiplas:** Motor de jogo capaz de processar saltos ortogonais sucessivos para capturar pedras adversárias numa única jogada (com opção de parar ou continuar a capturar)[cite: 4].
*   **Persistência de Estado (Guardar/Carregar):** Capacidade de manter e restaurar o estado de jogos não terminados entre diferentes execuções do programa[cite: 4].
*   **Sistema de Desfazer (*Undo*):** Permite anular a última movimentação do jogador e do computador, revertendo o tabuleiro ao estado anterior[cite: 4].
*   **Temporizador Limite:** Controlo de tempo máximo configurável por jogada através de monitorização de tempo ativo (`System.currentTimeMillis()`)[cite: 4].
*   **Inteligência Artificial Básica:** Oponente computacional capaz de jogar de forma aleatória inteligente através de funções de ordem superior[cite: 4].

---

## 🛠️ Tipos de Dados Obrigatórios

O motor do jogo foi construído estritamente sob a seguinte modelação de tipos em Scala 3[cite: 4]:
scala
type Coord2D = (Int, Int) // (linha, coluna)
type Board = ParMap[Coord2D, Stone]

enum Stone:
  case Black, White
```[cite: 4]
```
---

## 🚀 Como Executar

### Pré-requisitos
*   Java JDK 17 (ou superior)
*   SBT (Scala Build Tool)

### 1. Compilar o Projeto
Na raiz do projeto (onde se encontra o ficheiro `build.sbt`), executa:
```bash
sbt compile
```
### 2. Executar a Aplicação
Para iniciar a aplicação (onde poderás escolher jogar via TUI ou GUI):
sbt run
