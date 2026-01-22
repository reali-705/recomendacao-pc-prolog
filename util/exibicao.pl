% Mostra uma recomendação de PC de forma formatada.
mostrar_recomendacao(pc(Resolucao, PrecoTotal, DiferencaOrcamento, PlacaVideo, Memoria, Armazenamento, FontePoder, PlacaMae)) :-
    % Extrair dados dos dicts
    get_dict(marca, PlacaVideo, MarcaPlacaVideo),
    get_dict(modelo, PlacaVideo, ModeloPlacaVideo),
    get_dict(memoria, PlacaVideo, MemoriaPlacaVideo),
    get_dict(g3dmark, PlacaVideo, DesempenhoG3DMark),
    get_dict(preco, PlacaVideo, PrecoPlacaVideo),
    
    get_dict(marca, Memoria, MarcaMemoria),
    get_dict(modelo, Memoria, ModeloMemoria),
    get_dict(capacidade, Memoria, CapacidadeMemoria),
    get_dict(velocidade, Memoria, VelocidadeMemoria),
    get_dict(preco, Memoria, PrecoMemoria),
    
    get_dict(marca, Armazenamento, MarcaArmazenamento),
    get_dict(modelo, Armazenamento, ModeloArmazenamento),
    get_dict(capacidade, Armazenamento, CapacidadeArmazenamento),
    get_dict(velocidade, Armazenamento, VelocidadeArmazenamento),
    get_dict(preco, Armazenamento, PrecoArmazenamento),
    
    get_dict(marca, FontePoder, MarcaFontePoder),
    get_dict(modelo, FontePoder, ModeloFontePoder),
    get_dict(potencia, FontePoder, PotenciaFontePoder),
    get_dict(certificacao, FontePoder, CertificacaoFontePoder),
    get_dict(preco, FontePoder, PrecoFontePoder),
    
    get_dict(marca, PlacaMae, MarcaPlacaMae),
    get_dict(modelo, PlacaMae, ModeloPlacaMae),
    get_dict(soquete, PlacaMae, SoquetePlacaMae),
    get_dict(chipset, PlacaMae, ChipsetPlacaMae),
    get_dict(preco, PlacaMae, PrecoPlacaMae),
    
    % Formatar saída
    format('~n=== RECOMENDACAO PARA ~w ===', [Resolucao]),
    format('~nPreco Total: R$ ~2f (Diferenca do orcamento: R$ ~2f)', [PrecoTotal, DiferencaOrcamento]),
    format('~n~nComponentes:'), 
    format('~n- GPU: ~w ~w (~wGB, ~w pontos) - R$ ~2f', 
           [MarcaPlacaVideo, ModeloPlacaVideo, MemoriaPlacaVideo, DesempenhoG3DMark, PrecoPlacaVideo]),
    format('~n- RAM: ~w ~w (~wGB, ~wMHz) - R$ ~2f', 
           [MarcaMemoria, ModeloMemoria, CapacidadeMemoria, VelocidadeMemoria, PrecoMemoria]),
    format('~n- SSD: ~w ~w (~wGB, ~w MB/s) - R$ ~2f', 
           [MarcaArmazenamento, ModeloArmazenamento, CapacidadeArmazenamento, VelocidadeArmazenamento, PrecoArmazenamento]),
    format('~n- Fonte: ~w ~w (~wW, ~w) - R$ ~2f', 
           [MarcaFontePoder, ModeloFontePoder, PotenciaFontePoder, CertificacaoFontePoder, PrecoFontePoder]),
    format('~n- Placa-mae: ~w ~w (~w, ~w) - R$ ~2f~n', 
           [MarcaPlacaMae, ModeloPlacaMae, SoquetePlacaMae, ChipsetPlacaMae, PrecoPlacaMae]).

% Mostra múltiplas recomendações numeradas.
mostrar_top3([], _).
mostrar_top3([RecomendacaoAtual|RestanteRecomendacoes], NumeroOpcao) :-
    format('~n========== OPCAO ~w ==========~n', [NumeroOpcao]),
    mostrar_recomendacao(RecomendacaoAtual),
    ProximaOpcao is NumeroOpcao + 1,
    mostrar_top3(RestanteRecomendacoes, ProximaOpcao).

% Exibe todas as 3 melhores recomendações de uma vez.
mostrar_melhores(OrcamentoTotal, Resolucao) :-
    melhores_recomendacoes(OrcamentoTotal, Resolucao, Top3MelhoresRecomendacoes),
    length(Top3MelhoresRecomendacoes, QuantidadeRecomendacoes),
    nl,
    nl,
    format('=== ENCONTRADAS ~w RECOMENDACOES PARA ~w ===', [QuantidadeRecomendacoes, Resolucao]),
    format('Orcamento: R$ ~w~n', [OrcamentoTotal]),
    nl,
    mostrar_top3(Top3MelhoresRecomendacoes, 1).
