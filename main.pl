% SISTEMA DE RECOMENDAÇÃO DE COMPONENTES PARA PC - MAIN
% Arquivo: main.pl
% Para executar: swipl -s main.pl

:- ensure_loaded(dados).
:- ensure_loaded(regras).
:- ensure_loaded(menu).

% Para iniciar o programa diretamente
:- initialization(menu_principal).