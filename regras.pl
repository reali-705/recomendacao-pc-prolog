% --- 3. REGRAS AUXILIARES (PARA SUPORTE INTERNO) --- %
chamar_predicado(ssd, [M, Mod, C, I, V, P]) :- ssd(M, Mod, C, I, V, P).
chamar_predicado(fonte, [M, Mod, Pot, Cert, P]) :- fonte(M, Mod, Pot, Cert, P).
chamar_predicado(gpu, [M, Mod, Mem, A, Perf, P]) :- gpu(M, Mod, Mem, A, Perf, P).
chamar_predicado(ram, [M, Mod, C, V, T, P]) :- ram(M, Mod, C, V, T, P).
chamar_predicado(placa_mae, [M, Mod, S, Chip, T, P]) :- placa_mae(M, Mod, S, Chip, T, P).

calcular_preco_total([], 0).
calcular_preco_total([Componente | Resto], Total) :-
    last(Componente, Preco),
    calcular_preco_total(Resto, Subtotal),
    Total is Preco + Subtotal.

% REGRA GERAL: Recomenda qualquer componente com preço menor ou igual ao máximo.
recomendar_preco(TipoComponente, PrecoMax, Componente) :-
    chamar_predicado(TipoComponente, Atributos),
    nth1(6, Atributos, Preco),
    Preco =< PrecoMax,
    Componente = Atributos.

% REGRA PARA PLACA DE VÍDEO: Recomenda por desempenho mínimo (G3DMark).
recomendar_gpu_desempenho(DesempenhoMin, gpu(Marca, Modelo, Mem, Ano, Perf, PrecoBRL)) :-
    gpu(Marca, Modelo, Mem, Ano, Perf, PrecoBRL),
    Perf >= DesempenhoMin.

% REGRA PARA FONTE: Recomenda por potência mínima e certificação específica.
recomendar_fonte(PotenciaMin, Certificacao, fonte(Marca, Modelo, Pot, Cert, PrecoBRL)) :-
    fonte(Marca, Modelo, Pot, Cert, PrecoBRL),
    Pot >= PotenciaMin,
    (Certificacao == 'qualquer' -> true; Cert == Certificacao).

% REGRA PARA SSD: Recomenda por capacidade mínima e interface.
recomendar_ssd(CapacidadeMin, Interface, ssd(Marca, Modelo, Cap, Int, Vel, PrecoBRL)) :-
    ssd(Marca, Modelo, Cap, Int, Vel, PrecoBRL),
    Cap >= CapacidadeMin,
    (Interface == 'qualquer' -> true; sub_atom(Int, _, _, _, Interface)).

% REGRA PARA COMPATIBILIDADE BÁSICA: Sugere RAM compatível com uma placa-mãe.
compativel_ram_placa(NomePlacaMae, ram(RamMarca, RamModelo, Cap, Vel, Tipo, PrecoBRL)) :-
    placa_mae(_, NomePlacaMae, _, _, TipoRAM, _),
    ram(RamMarca, RamModelo, Cap, Vel, Tipo, PrecoBRL),
    Tipo == TipoRAM.

% REGRA PRINCIPAL: Recomendação completa de PC
pc_recomendacao(OrcamentoTotal, Resolucao, Recomendacoes) :-
    % 1. Obter requisitos mínimos para a resolução
    requisitos_resolucao(Resolucao, G3DMark_Min, RAM_Min, SSD_Min, Fonte_W_Min),
    
    % 2. Encontrar componentes que atendam aos requisitos mínimos
    gpu(MarcaGPU, ModeloGPU, MemGPU, AnoGPU, G3DMark, PrecoGPU), 
    
    G3DMark >= G3DMark_Min,
    
    ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, TipoRAM, PrecoRAM),
    CapacidadeRAM >= RAM_Min,
    
    ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, InterfaceSSD, VelSSD, PrecoSSD),
    CapacidadeSSD >= SSD_Min,
    
    fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
    PotenciaFonte >= Fonte_W_Min,
    
    placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, TipoRAM_MB, PrecoMB),
    TipoRAM_MB = TipoRAM,
    
    % 3. Calcular preço total (TODOS JÁ EM BRL - SEM CONVERSÃO!)
    PrecoTotal is PrecoGPU + PrecoRAM + PrecoSSD + PrecoFonte + PrecoMB,
    
    % 4. Verificar se está dentro do orçamento (com margem de 20%)
    OrcamentoComMargem is OrcamentoTotal * 1.2,
    PrecoTotal =< OrcamentoComMargem,
    
    % 5. Calcular diferença para ordenação
    Diferenca is abs(PrecoTotal - OrcamentoTotal),
    
    % 6. Retornar todas as recomendações encontradas
    Recomendacoes = [
        pc(
            Resolucao,
            PrecoTotal,
            Diferenca,
            gpu(MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU),  % Preço já em BRL
            ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, PrecoRAM),
            ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, VelSSD, PrecoSSD),
            fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
            placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, PrecoMB)
        )
    ].

% REGRA PARA BUSCAR AS 3 MELHORES RECOMENDAÇÕES
melhores_recomendacoes(Orcamento, Resolucao, Top3) :-
    findall(PC, pc_recomendacao(Orcamento, Resolucao, [PC]), Todos),
    sort(3, @=<, Todos, Top3).

% REGRA PARA MOSTRAR RECOMENDAÇÕES DE FORMA LEGÍVEL
mostrar_recomendacao(pc(Res, Total, Diff, GPU, RAM, SSD, Fonte, PlacaMae)) :-
    format('~n=== RECOMENDAÇÃO PARA ~w ===', [Res]),
    format('~nPreço Total: R$ ~2f (Diferença do orçamento: R$ ~2f)', [Total, Diff]),
    format('~n~nComponentes:'),
    format('~n- GPU: ~w ~w (~wGB, ~w pontos) - R$ ~2f', 
           [GPU%gpu.marca, GPU%gpu.modelo, GPU%gpu.memoria, GPU%gpu.desempenho, GPU%gpu.preco]),
    format('~n- RAM: ~w ~w (~wGB, ~wMHz) - R$ ~2f', 
           [RAM%ram.marca, RAM%ram.modelo, RAM%ram.capacidade, RAM%ram.velocidade, RAM%ram.preco]),
    format('~n- SSD: ~w ~w (~wGB, ~w MB/s) - R$ ~2f', 
           [SSD%ssd.marca, SSD%ssd.modelo, SSD%ssd.capacidade, SSD%ssd.velocidade, SSD%ssd.preco]),
    format('~n- Fonte: ~w ~w (~wW, ~w) - R$ ~2f', 
           [Fonte%fonte.marca, Fonte%fonte.modelo, Fonte%fonte.potencia, Fonte%fonte.certificacao, Fonte%fonte.preco]),
    format('~n- Placa-mãe: ~w ~w (~w, ~w) - R$ ~2f~n', 
           [PlacaMae%placa_mae.marca, PlacaMae%placa_mae.modelo, 
            PlacaMae%placa_mae.soquete, PlacaMae%placa_mae.chipset, PlacaMae%placa_mae.preco]).
