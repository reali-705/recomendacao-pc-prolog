% Core genérico: recomenda componente com base em restrições por chave.
% Restricoes é uma lista de Chave-Operador-Valor, ex: [g3dmark-(>=)-20000, preco-(=<)-3500].
recomendar_por_chaves(TipoComponente, Restricoes, DictComponenteRetornado) :-
    chamar_componente_dict(TipoComponente, DictComponenteRetornado),
    satisfaz_chaves(DictComponenteRetornado, Restricoes).

% Variante que devolve todas as correspondências em lista (não ordena).
filtrar_componentes(TipoComponente, Restricoes, ListaComponentes) :-
    findall(ComponenteAtual, recomendar_por_chaves(TipoComponente, Restricoes, ComponenteAtual), ListaComponentes).

satisfaz_chaves(_, []).
satisfaz_chaves(DictAtual, [ChaveAtual-OperadorAtual-ValorEsperado | RestricoesPendentes]) :-
    get_dict(ChaveAtual, DictAtual, ValorAtualDict),
    aplicar_operador(OperadorAtual, ValorAtualDict, ValorEsperado),
    satisfaz_chaves(DictAtual, RestricoesPendentes).

aplicar_operador(>=, ValorAtual, ValorEsperado) :- ValorAtual >= ValorEsperado.
aplicar_operador(=<, ValorAtual, ValorEsperado) :- ValorAtual =< ValorEsperado.
aplicar_operador(>, ValorAtual, ValorEsperado)  :- ValorAtual > ValorEsperado.
aplicar_operador(<, ValorAtual, ValorEsperado)  :- ValorAtual < ValorEsperado.
aplicar_operador(==, ValorAtual, ValorEsperado) :- ValorAtual == ValorEsperado.
aplicar_operador(contem, TextoAtual, SubstringBuscada) :- atom(TextoAtual), sub_atom(TextoAtual, _, _, _, SubstringBuscada).

% Wrappers amigáveis (mantêm interface simples)

% GPU por desempenho mínimo
recomendar_gpu_desempenho(DesempenhoMinimo, PlacaVideoRetornada) :-
    recomendar_por_chaves(gpu, [g3dmark-(>=)-DesempenhoMinimo], PlacaVideoRetornada).

% Fonte por potência mínima e certificação
recomendar_fonte(PotenciaMinima, CertificacaoDesejada, FontePoder) :-
    ( CertificacaoDesejada == 'qualquer' ->
        recomendar_por_chaves(fonte, [potencia-(>=)-PotenciaMinima], FontePoder)
    ;   recomendar_por_chaves(fonte, [potencia-(>=)-PotenciaMinima, certificacao-(==)-CertificacaoDesejada], FontePoder)
    ).

% SSD por capacidade (e opcional interface)
recomendar_ssd(CapacidadeMinima, ArmazenamentoRetornado) :-
    recomendar_por_chaves(ssd, [capacidade-(>=)-CapacidadeMinima], ArmazenamentoRetornado).

recomendar_ssd(CapacidadeMinima, InterfaceDesejada, ArmazenamentoRetornado) :-
    ( InterfaceDesejada == 'qualquer' ->
        recomendar_ssd(CapacidadeMinima, ArmazenamentoRetornado)
    ;   recomendar_por_chaves(ssd, [capacidade-(>=)-CapacidadeMinima, interface-contem-InterfaceDesejada], ArmazenamentoRetornado)
    ).

% RAM por capacidade mínima
recomendar_ram(CapacidadeMinima, MemoriaRetornada) :-
    recomendar_por_chaves(ram, [capacidade-(>=)-CapacidadeMinima], MemoriaRetornada).

% Componente por preço máximo (qualquer tipo)
recomendar_por_preco(TipoComponente, PrecoMaximo, ComponenteRetornado) :-
    recomendar_por_chaves(TipoComponente, [preco-(=<)-PrecoMaximo], ComponenteRetornado).

% Auxiliar compatível com termos e dicts
extrair_preco(ComponenteOuDict, PrecoExtraido) :-
    (   is_dict(ComponenteOuDict)
    ->  get_dict(preco, ComponenteOuDict, PrecoExtraido)
    ;   ComponenteOuDict =.. [_|ArgumentosList], last(ArgumentosList, PrecoExtraido)
    ).
