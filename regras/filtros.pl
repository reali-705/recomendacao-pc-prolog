% Recomenda GPU por desempenho mínimo (G3DMark).
recomendar_gpu_desempenho(DesempenhoMin, gpu(Marca, Modelo, Mem, Ano, Perf, Preco)) :-
    gpu(Marca, Modelo, Mem, Ano, Perf, Preco),
    Perf >= DesempenhoMin.

% Recomenda fonte por potência mínima e certificação específica.
recomendar_fonte(PotenciaMin, Certificacao, fonte(Marca, Modelo, Pot, Cert, Preco)) :-
    fonte(Marca, Modelo, Pot, Cert, Preco),
    Pot >= PotenciaMin,
    (Certificacao == 'qualquer' -> true; Cert == Certificacao).

% Recomenda SSD por capacidade mínima e interface.
recomendar_ssd(CapacidadeMin, Interface, ssd(Marca, Modelo, Cap, Int, Vel, Preco)) :-
    ssd(Marca, Modelo, Cap, Int, Vel, Preco),
    Cap >= CapacidadeMin,
    (Interface == 'qualquer' -> true; sub_atom(Int, _, _, _, Interface)).

% Recomenda RAM por capacidade mínima.
recomendar_ram(CapacidadeMin, ram(Marca, Modelo, Cap, Vel, Tipo, Preco)) :-
    ram(Marca, Modelo, Cap, Vel, Tipo, Preco),
    Cap >= CapacidadeMin.

% Recomenda componente genérico por preço máximo.
recomendar_por_preco(TipoComponente, PrecoMax, Componente) :-
    chamar_componente(TipoComponente, Componente),
    extrair_preco(Componente, Preco),
    Preco =< PrecoMax.

% Auxiliar: extrai o último argumento (preço) de um termo.
extrair_preco(Componente, Preco) :-
    Componente =.. [_|Args],
    last(Args, Preco).

% Auxiliar: chama o predicado apropriado para cada tipo de componente.
chamar_componente(gpu, gpu(M, Mod, Mem, A, Perf, P)) :- 
    gpu(M, Mod, Mem, A, Perf, P).
chamar_componente(ram, ram(M, Mod, C, V, T, P)) :- 
    ram(M, Mod, C, V, T, P).
chamar_componente(ssd, ssd(M, Mod, C, I, V, P)) :- 
    ssd(M, Mod, C, I, V, P).
chamar_componente(fonte, fonte(M, Mod, Pot, Cert, P)) :- 
    fonte(M, Mod, Pot, Cert, P).
chamar_componente(placa_mae, placa_mae(M, Mod, S, Chip, T, P)) :- 
    placa_mae(M, Mod, S, Chip, T, P).
