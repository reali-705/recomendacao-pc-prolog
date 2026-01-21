% Carregar módulos de dados
:- consult('dados/componentes.pl').
:- consult('dados/requisitos.pl').

% Carregar módulos de regras
:- consult('regras/filtros.pl').
:- consult('regras/compatibilidade.pl').
:- consult('regras/recomendacao.pl').

% Carregar utilitários
:- consult('util/exibicao.pl').

% Inicialização
:- initialization(main).

main :-
    nl,
    writeln('##### SISTEMA DE RECOMENDACAO DE PC - SWI-PROLOG #####'),
    nl,
    writeln(' COMANDOS DISPONIVEIS:'),
    nl,
    writeln('  1. Buscar top 3 recomendacoes:'),
    writeln('     ?- melhores_recomendacoes(5000, ''1080p'', Top3).'),
    nl,
    writeln('  2. Mostrar recomendacao formatada:'),
    writeln('     ?- mostrar_melhores(5000, ''1080p'').'),
    nl,
    writeln('  3. Filtrar GPU por desempenho:'),
    writeln('     ?- recomendar_gpu_desempenho(20000, GPU).'),
    nl,
    writeln(' RESOLUCOES DISPONIVEIS: ''1080p'', ''1440p'', ''4k'''),
    nl,
    nl.
