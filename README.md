# Библиотека методов дискретного программирования

*А знаете где механизм сопоставления с образцом почти такой же классный? В Erlang! Там вовсе нет типизации!*

- [Книга по искусственному интеллекту](https://aima.cs.berkeley.edu/)
- [Примеры на Python к этой книге](https://github.com/aimacode/aima-python)

## Состав библиотеки

* Поиск в дискретном пространстве состояний
* Преобразование булевых выражений
* Эмуляторы абстрактных машин
* SAT/SMT решатели

## Особенности

- [ ] Примеры с решением CSP проблем
- [ ] DPLL и CDCL алгоритмы (Hard)
- [ ] Интеграция с SMTLIBv2 (Hard)

## Конфигурация

```sh

```

## Сборка

```sh
git clone https://github.com/squeakbug/j4fsolver
cd j4fsolver
cabal install
```

## Запуск

```sh
cabal run
```

## Тестирование

```sh
cabal test
```

## Ссылки

### Haskell

* [Examples from awesome book](https://github.com/chris-taylor/aima-haskell)
* [Nix-style-builds](https://cabal.readthedocs.io/en/3.4/nix-local-build-overview.html#nix-style-builds)

### Boolean expressions transformations

* [boolexpr](https://github.com/boolexpr/boolexpr)

### Поиск

* [Введение монады Search и трансформера монад SearchT](https://github.com/ennocramer/monad-dijkstra)
* [Реализация A* монады](https://github.com/ChrisPenner/astar-monad)

### Constraint solver

* [Mad Props](https://github.com/ChrisPenner/mad-props)
* [Python](https://github.com/python-constraint/python-constraint)
* [Prolog](https://www.swi-prolog.org/pldoc/man?section=clp)
* [Oz](https://github.com/mozart/mozart2)

### Graph rewriting / Subgraph isomorphism problem

* [Wiki article](https://en.wikipedia.org/wiki/Graph_rewriting)
* [Clean language](https://en.wikipedia.org/wiki/Clean_(programming_language))

* [unipatterns](https://github.com/ChrisPenner/unipatterns)

### Logic unification and reduction 

* https://github.com/creusot-rs/creusot

### Category theory

* [Comonads explains](https://github.com/ChrisPenner/comonads-by-example)
* [catalyst](https://github.com/ChrisPenner/catalyst)

### SAT/SMT

- [SMT-LIB](https://smt-lib.org/)
- [bitwuzla](https://bitwuzla.github.io/)
- [lean4](https://github.com/leanprover/lean4)
- https://github.com/leanprover-community/mathlib4
- https://github.com/sarsko/CreuSAT

- [Реализация DPLL и BF sat-решателей](https://www.gibiansky.com/blog/verification/writing-a-sat-solver/index.html)

- [Поддерживаемая библиотека для работы с булевыми формулами на Rust с документацией и примерами + SAT](https://github.com/booleworks/logicng-rs)
- [Неподдерживаемая библотека для работы с ДРД](https://github.com/cfallin/boolean_expression)

- [R. Bryant Symbolic Boolean Manipulation with Ordered Binary Decision Diagrams, ACM Computing Surveys, 1992](https://dl.acm.org/doi/pdf/10.1145/136035.136043)
- [R. Bryant, Graph-Based Algorithms for Boolean Function Manipulation, IEEE Transactions on Computers, 1986](https://www.cs.cmu.edu/~bryant/pubdir/ieeetc86.pdf)
- [L. Fortune, J. Hopcroft, and E. M. Schmidt, The complexity of equivalence and containment for free single variable program schemes 1978](https://dn790007.ca.archive.org/0/items/DTIC_ADA058448/DTIC_ADA058448.pdf)

### Compute models

* [The Art of the Propagator by Edvard Kmett](https://github.com/ekmett/propagators)
* [Propagation Networks by Alexey Andreyevich Radul](https://groups.csail.mit.edu/genesis/papers/radul%202009.pdf), 

### Defuzzification

* [Lotfi A. Zadeh](https://en.wikipedia.org/wiki/Lotfi_A._Zadeh)

--- ---

"I have always liked the concept of universities as they were in Ancient Greece, where folks **who had something cool** to say would just come and **say it**. It wasn't about recognition; the impetus was the thought that you were resonating with ideas."

[Professor Donald E. Knuth](https://web.archive.org/web/20140604193847/http://scpd.stanford.edu/knuth/index.jsp)
