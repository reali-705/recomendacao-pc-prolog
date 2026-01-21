:- consult('dados.pl').
:- consult('regras.pl').

:- initialization(main).

main :-
    writeln('=== SISTEMA DE RECOMENDACAO DE PC ==='),
    writeln('Consulte usando: "melhores_recomendacoes(Orcamento, Resolucao, Top3)."'),
    writeln('Exemplo: melhores_recomendacoes(5000, "1080p", Top3).'),
    nl.

