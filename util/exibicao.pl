% Mostra uma recomendação de PC de forma formatada.
mostrar_recomendacao(pc(Res, Total, Diff, GPU, RAM, SSD, Fonte, PlacaMae)) :-
    GPU = gpu(MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU),
    RAM = ram(MarcaRAM, ModeloRAM, CapRAM, VelRAM, PrecoRAM),
    SSD = ssd(MarcaSSD, ModeloSSD, CapSSD, VelSSD, PrecoSSD),
    Fonte = fonte(MarcaFonte, ModeloFonte, PotFonte, CertFonte, PrecoFonte),
    PlacaMae = placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, PrecoMB),
    
    format('~n=== RECOMENDAÇÃO PARA ~w ===', [Res]),
    format('~nPreço Total: R$ ~2f (Diferença do orçamento: R$ ~2f)', [Total, Diff]),
    format('~n~nComponentes:'),
    format('~n- GPU: ~w ~w (~wGB, ~w pontos) - R$ ~2f', 
           [MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU]),
    format('~n- RAM: ~w ~w (~wGB, ~wMHz) - R$ ~2f', 
           [MarcaRAM, ModeloRAM, CapRAM, VelRAM, PrecoRAM]),
    format('~n- SSD: ~w ~w (~wGB, ~w MB/s) - R$ ~2f', 
           [MarcaSSD, ModeloSSD, CapSSD, VelSSD, PrecoSSD]),
    format('~n- Fonte: ~w ~w (~wW, ~w) - R$ ~2f', 
           [MarcaFonte, ModeloFonte, PotFonte, CertFonte, PrecoFonte]),
    format('~n- Placa-mãe: ~w ~w (~w, ~w) - R$ ~2f~n', 
           [MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, PrecoMB]).

% Mostra múltiplas recomendações (Top 3).
mostrar_top3([]).
mostrar_top3([PC|Resto]) :-
    mostrar_recomendacao(PC),
    mostrar_top3(Resto).

% Exibe todas as 3 melhores recomendações de uma vez.
mostrar_melhores(Orcamento, Resolucao) :-
    melhores_recomendacoes(Orcamento, Resolucao, Top3),
    length(Top3, N),
    format('~n~n╔════════════════════════════════════════════════════╗', []),
    format('~n║  ENCONTRADAS ~w RECOMENDAÇÕES PARA ~w  ║', [N, Resolucao]),
    format('~n╚════════════════════════════════════════════════════╝~n', []),
    mostrar_top3(Top3).
