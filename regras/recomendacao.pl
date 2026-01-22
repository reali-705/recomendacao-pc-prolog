% ========================================
% REGRA PRINCIPAL: Recomendação completa de PC
% Refatorada para usar filtros genéricos com dicts
% ========================================
pc_recomendacao(OrcamentoTotal, Resolucao, RecomendacaoPC) :-
    % 1. Obter requisitos mínimos para a resolução
    requisitos_resolucao(Resolucao, DesempenhoG3DMarkMinimo, CapacidadeMemoriaMinima, CapacidadeArmazenamentoMinima, PotenciaFonteMinima),
    
    % 2. Selecionar GPU usando filtro genérico
    recomendar_por_chaves(gpu, [g3dmark-(>=)-DesempenhoG3DMarkMinimo], PlacaVideo),
    get_dict(preco, PlacaVideo, PrecoPlacaVideo),
    
    % 3. Selecionar RAM usando filtro genérico
    recomendar_por_chaves(ram, [capacidade-(>=)-CapacidadeMemoriaMinima], Memoria),
    get_dict(tipo, Memoria, TipoMemoria),
    get_dict(preco, Memoria, PrecoMemoria),
    
    % 4. Selecionar SSD usando filtro genérico
    recomendar_por_chaves(ssd, [capacidade-(>=)-CapacidadeArmazenamentoMinima], Armazenamento),
    get_dict(preco, Armazenamento, PrecoArmazenamento),
    
    % 5. Selecionar Fonte usando filtro genérico
    recomendar_por_chaves(fonte, [potencia-(>=)-PotenciaFonteMinima], FontePoder),
    get_dict(preco, FontePoder, PrecoFontePoder),
    
    % 6. Selecionar Placa-mãe com tipo RAM compatível
    recomendar_por_chaves(placa_mae, [tipo_ram-(==)-TipoMemoria], PlacaMae),
    get_dict(preco, PlacaMae, PrecoPlacaMae),
    
    % 7. Calcular preço total
    PrecoTotalPC is PrecoPlacaVideo + PrecoMemoria + PrecoArmazenamento + PrecoFontePoder + PrecoPlacaMae,
    
    % 8. Verificar orçamento (com margem de 20%)
    OrcamentoComMargem is OrcamentoTotal * 1.2,
    PrecoTotalPC =< OrcamentoComMargem,
    
    % 9. Calcular diferença do orçamento
    DiferencaOrcamento is abs(PrecoTotalPC - OrcamentoTotal),
    
    % 10. Construir estrutura de retorno (com dicts)
    RecomendacaoPC = pc(
        Resolucao,
        PrecoTotalPC,
        DiferencaOrcamento,
        PlacaVideo,
        Memoria,
        Armazenamento,
        FontePoder,
        PlacaMae
    ).

% Busca as 3 melhores recomendações ordenadas por proximidade do orçamento.
melhores_recomendacoes(OrcamentoTotal, Resolucao, Top3MelhoresRecomendacoes) :-
    findall(DiferencaOrcamento-RecomendacaoPC, 
            (pc_recomendacao(OrcamentoTotal, Resolucao, RecomendacaoPC),
             RecomendacaoPC = pc(_, _, DiferencaOrcamento, _, _, _, _, _)),
            ParesOrdenacao),
    keysort(ParesOrdenacao, RecomendacoesOrdenadas),
    (   RecomendacoesOrdenadas = [_-RecomendacaoPC1, _-RecomendacaoPC2, _-RecomendacaoPC3|_]
    ->  Top3MelhoresRecomendacoes = [RecomendacaoPC1, RecomendacaoPC2, RecomendacaoPC3]
    ;   findall(RecomendacaoPC, member(_-RecomendacaoPC, RecomendacoesOrdenadas), Top3MelhoresRecomendacoes)
    ).

% Auxiliar: calcula preço total de uma lista de componentes.
calcular_preco_total([], 0).
calcular_preco_total([ComponenteAtual | RestanteComponentes], PrecoTotalAcumulado) :-
    extrair_preco(ComponenteAtual, PrecoComponente),
    calcular_preco_total(RestanteComponentes, PrecoTotalRestante),
    PrecoTotalAcumulado is PrecoComponente + PrecoTotalRestante.
