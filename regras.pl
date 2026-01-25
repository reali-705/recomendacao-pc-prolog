% SISTEMA DE RECOMENDAÇÃO DE COMPONENTES PARA PC - REGRAS ESSENCIAIS
% Arquivo: regras.pl
% Contém todas as regras essenciais do projeto

:- use_module(library(lists)).

:- ensure_loaded(dados).

% --- REGRA GERAL PARA CONSULTA POR PREÇO ---
recomendar_preco(cpu, PrecoMax, cpu(Marca, Modelo, Sock, Perf, Nuc, Clock, VR, MS, TDP, Preco)) :-
    cpu(Marca, Modelo, Sock, Perf, Nuc, Clock, VR, MS, TDP, Preco),
    Preco =< PrecoMax.

recomendar_preco(gpu, PrecoMax, gpu(Marca, Modelo, Mem, Perf, Preco)) :-
    gpu(Marca, Modelo, Mem, Perf, Preco),
    Preco =< PrecoMax.

recomendar_preco(ram, PrecoMax, ram(Marca, Modelo, Cap, Vel, Tipo, Preco)) :-
    ram(Marca, Modelo, Cap, Vel, Tipo, Preco),
    Preco =< PrecoMax.

recomendar_preco(ssd, PrecoMax, ssd(Marca, Modelo, Cap, Vel, Preco)) :-
    ssd(Marca, Modelo, Cap, Vel, Preco),
    Preco =< PrecoMax.

recomendar_preco(fonte, PrecoMax, fonte(Marca, Modelo, Pot, Cert, Preco)) :-
    fonte(Marca, Modelo, Pot, Cert, Preco),
    Preco =< PrecoMax.

recomendar_preco(placa_mae, PrecoMax, placa_mae(Marca, Modelo, Sock, Chip, T, Preco)) :-
    placa_mae(Marca, Modelo, Sock, Chip, T, Preco),
    Preco =< PrecoMax.

% --- REGRAS PARA RECOMENDAÇÃO COMPLETA DE PC (OPÇÃO 1) ---

% REGRA PRINCIPAL: Recomendação completa de PC
pc_recomendacao(OrcamentoTotal, Resolucao, [PC]) :-
    % 1. Obter requisitos mínimos para a resolução
    requisitos_resolucao(Resolucao, G3DMark_Min, RAM_Min, SSD_Min, Fonte_W_Min, CPU_Min),

    % 2. Encontrar componentes que atendam aos requisitos mínimos
    gpu(MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU),
    G3DMark >= G3DMark_Min,

    ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, TipoRAM, PrecoRAM),
    CapacidadeRAM >= RAM_Min,

    ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, VelSSD, PrecoSSD),
    CapacidadeSSD >= SSD_Min,

    fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
    PotenciaFonte >= Fonte_W_Min,

    cpu(MarcaCPU, ModeloCPU, SoqueteCPU, NivelCPU, NucCPU, ClockCPU, VelMaxCPU, MemSupCPU, TDPCPU, PrecoCPU),
    NivelCPU >= CPU_Min,

    placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, TipoRAM_MB, PrecoMB),
    
    % 3. Verificar compatibilidade básica
    SoqueteMB == SoqueteCPU,
    TipoRAM_MB == TipoRAM,
    
    % 4. Verificar compatibilidade RAM com CPU
    ram_compativel_cpu(TipoRAM, cpu(MarcaCPU, ModeloCPU, SoqueteCPU, NivelCPU, NucCPU, ClockCPU, VelMaxCPU, MemSupCPU, TDPCPU, PrecoCPU)),
    
    % 5. Calcular preço total
    PrecoTotal is PrecoGPU + PrecoRAM + PrecoSSD + PrecoFonte + PrecoCPU + PrecoMB,

    % 6. Verificar se está dentro do orçamento (com margem de 20%)
    OrcamentoComMargem is OrcamentoTotal * 1.2,
    PrecoTotal =< OrcamentoComMargem,

    % 7. Calcular diferença para ordenação
    Diferenca is abs(PrecoTotal - OrcamentoTotal),

    % 8. Criar estrutura de retorno
    PC = pc(
            Resolucao,
            PrecoTotal,
            Diferenca,
            cpu(MarcaCPU, ModeloCPU, NivelCPU, PrecoCPU),
            gpu(MarcaGPU, ModeloGPU, MemGPU, G3DMark, PrecoGPU),
            ram(MarcaRAM, ModeloRAM, CapacidadeRAM, VelocidadeRAM, PrecoRAM),
            ssd(MarcaSSD, ModeloSSD, CapacidadeSSD, VelSSD, PrecoSSD),
            fonte(MarcaFonte, ModeloFonte, PotenciaFonte, CertificacaoFonte, PrecoFonte),
            placa_mae(MarcaMB, ModeloMB, SoqueteMB, ChipsetMB, PrecoMB)
        ).

% REGRA PARA BUSCAR AS MELHORES RECOMENDAÇÕES
melhores_recomendacoes(Orcamento, Resolucao, Top3) :-
    findall(PC, pc_recomendacao(Orcamento, Resolucao, [PC]), Todos),
    sort_by_difference(Todos, Ordenados),
    take_first(Ordenados, 3, Top3).

% Ordena pela diferença do orçamento
sort_by_difference(List, Sorted) :-
    predsort(compare_by_difference, List, Sorted).

compare_by_difference(Order, pc(_, _, Diff1, _, _, _, _, _, _), pc(_, _, Diff2, _, _, _, _, _, _)) :-
    compare(Order, Diff1, Diff2).

% Pega os primeiros N elementos
take_first(List, N, FirstN) :-
    length(List, Len),
    (Len >= N ->
        length(FirstN, N),
        append(FirstN, _, List)
    ;
        FirstN = List
    ).

% REGRA PARA MOSTRAR RECOMENDAÇÕES DE FORMA LEGÍVEL
mostrar_recomendacao(pc(Res, Total, Diff, 
                         cpu(CPU_Marca, CPU_Modelo, CPU_Desempenho, CPU_Preco),
                         gpu(GPU_Marca, GPU_Modelo, GPU_Memoria, GPU_Desempenho, GPU_Preco),
                         ram(RAM_Marca, RAM_Modelo, RAM_Capacidade, RAM_Velocidade, RAM_Preco),
                         ssd(SSD_Marca, SSD_Modelo, SSD_Capacidade, SSD_Velocidade, SSD_Preco),
                         fonte(Fonte_Marca, Fonte_Modelo, Fonte_Potencia, Fonte_Certificacao, Fonte_Preco),
                         placa_mae(Placa_Marca, Placa_Modelo, Placa_Soquete, Placa_Chipset, Placa_Preco))) :-
    format('~n=== RECOMENDAÇÃO PARA ~w ===', [Res]),
    format('~nPreço Total: R$ ~2f (Diferença do orçamento: R$ ~2f)', [Total, Diff]),
    format('~n~nComponentes:'),
    format('~n- CPU: ~w ~w (Desempenho: ~w) - R$ ~2f',
           [CPU_Marca, CPU_Modelo, CPU_Desempenho, CPU_Preco]),
    format('~n- GPU: ~w ~w (~wGB, ~w pontos) - R$ ~2f',
           [GPU_Marca, GPU_Modelo, GPU_Memoria, GPU_Desempenho, GPU_Preco]),
    format('~n- RAM: ~w ~w (~wGB, ~wMHz) - R$ ~2f',
           [RAM_Marca, RAM_Modelo, RAM_Capacidade, RAM_Velocidade, RAM_Preco]),
    format('~n- SSD: ~w ~w (~wGB, ~w MB/s) - R$ ~2f',
           [SSD_Marca, SSD_Modelo, SSD_Capacidade, SSD_Velocidade, SSD_Preco]),
    format('~n- Fonte: ~w ~w (~wW, ~w) - R$ ~2f',
           [Fonte_Marca, Fonte_Modelo, Fonte_Potencia, Fonte_Certificacao, Fonte_Preco]),
    format('~n- Placa-mãe: ~w ~w (~w, ~w) - R$ ~2f~n',
           [Placa_Marca, Placa_Modelo, Placa_Soquete, Placa_Chipset, Placa_Preco]).