% REGRA PRINCIPAL: Recomendação completa de PC
pc_recomendacao(OrcamentoTotal, Resolucao, PC) :-
    % 1. Obter requisitos mínimos para a resolução
    requisitos_resolucao(Resolucao, G3DMark_Min, RAM_Min, SSD_Min, Fonte_W_Min),
    
    % 2. Selecionar GPU que atende aos requisitos
    gpu(MarcaGPU, ModeloGPU, MemGPU, _AnoGPU, G3DMark, PrecoGPU), 
    G3DMark >= G3DMark_Min,
    
    % 3. Selecionar RAM
    ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, TipoRAM, PrecoRAM),
    CapacidadeRAM >= RAM_Min,
    
    % 4. Selecionar SSD
    ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, _InterfaceSSD, VelSSD, PrecoSSD),
    CapacidadeSSD >= SSD_Min,
    
    % 5. Selecionar Fonte
    fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
    PotenciaFonte >= Fonte_W_Min,
    
    % 6. Selecionar Placa-mãe
    placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, TipoRAM_MB, PrecoMB),
    
    % 7. Verificar compatibilidade
    TipoRAM_MB = TipoRAM,
    
    % 8. Calcular preço total
    PrecoTotal is PrecoGPU + PrecoRAM + PrecoSSD + PrecoFonte + PrecoMB,
    
    % 9. Verificar orçamento (com margem de 20%)
    OrcamentoComMargem is OrcamentoTotal * 1.2,
    PrecoTotal =< OrcamentoComMargem,
    
    % 10. Calcular diferença do orçamento
    Diferenca is abs(PrecoTotal - OrcamentoTotal),
    
    % 11. Construir estrutura de retorno
    PC = pc(
        Resolucao,
        PrecoTotal,
        Diferenca,
        gpu(MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU),
        ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, PrecoRAM),
        ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, VelSSD, PrecoSSD),
        fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
        placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, PrecoMB)
    ).

% Busca as 3 melhores recomendações ordenadas por proximidade do orçamento.
melhores_recomendacoes(Orcamento, Resolucao, Top3) :-
    findall(PC, pc_recomendacao(Orcamento, Resolucao, PC), Todos),
    sort(3, @=<, Todos, Top3).

% Auxiliar: calcula preço total de uma lista de componentes.
calcular_preco_total([], 0).
calcular_preco_total([Componente | Resto], Total) :-
    extrair_preco(Componente, Preco),
    calcular_preco_total(Resto, Subtotal),
    Total is Preco + Subtotal.
