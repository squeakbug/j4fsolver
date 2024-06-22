# SMT решатель

Сравнение времени решения SAT-проблемы для различных алгоритмов

## Особенности / План действий

- [ ] Примеры с решением CSP проблем
- [ ] DPLL и CDCL алгоритмы (Hard)
- [ ] Интеграция с SMTLIBv2 (Hard)

## Сборка

```sh
git clone https://github.com/squeakbug/j4fsolver
cd j4fsolver
cargo build --release
```

## Запуск тестов

```sh
cargo test
```

## Ссылки

- [Поддерживаемая библиотека для работы с булевыми формулами на Rust с документацией и примерами + SAT](https://github.com/booleworks/logicng-rs)
- [Неподдерживаемая библотека для работы с ДРД](https://github.com/cfallin/boolean_expression)

- [R. Bryant Symbolic Boolean Manipulation with Ordered Binary Decision Diagrams, ACM Computing Surveys, 1992](https://dl.acm.org/doi/pdf/10.1145/136035.136043)
- [R. Bryant, Graph-Based Algorithms for Boolean Function Manipulation, IEEE Transactions on Computers, 1986](https://www.cs.cmu.edu/~bryant/pubdir/ieeetc86.pdf)
- [L. Fortune, J. Hopcroft, and E. M. Schmidt, The complexity of equivalence and containment for free single variable program schemes 1978](https://dn790007.ca.archive.org/0/items/DTIC_ADA058448/DTIC_ADA058448.pdf)

--- ---

"I have always liked the concept of universities as they were in Ancient Greece, where folks **who had something cool** to say would just come and **say it**. It wasn't about recognition; the impetus was the thought that you were resonating with ideas."

[Professor Donald E. Knuth](https://web.archive.org/web/20140604193847/http://scpd.stanford.edu/knuth/index.jsp)
