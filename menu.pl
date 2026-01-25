% SISTEMA DE RECOMENDAÇÃO DE COMPONENTES PARA PC - MENU INTERATIVO
% Arquivo: menu.pl
% Para executar: swipl -s menu.pl

:- ensure_loaded(dados).
:- ensure_loaded(regras).

% Menu principal
menu_principal :-
    repeat,
    format('~n=== SISTEMA DE RECOMENDAÇÃO DE COMPONENTES PARA PC ==='),
    format('~n1. Recomendação de PC por preço e resolução'),
    format('~n2. Consultar peça por preço (TODAS as peças)'),
    format('~n3. Consultar GPU por qualidade (1080p, 1440p, 4k, ultrawide)'),
    format('~n4. Consultar fonte por potência mínima'),
    format('~n5. Consultar SSD por capacidade mínima'),
    format('~n6. Consultar RAM por capacidade e tipo DDR'),
    format('~n7. Consultar CPU por soquete'),
    format('~n8. Consultar placa-mãe por soquete'),
    format('~n9. Testar regras individuais (testes automáticos)'),
    format('~n0. Sair'),
    format('~n~nOpção: '),
    read_line_to_string(user_input, OpcaoStr),
    atom_number(OpcaoStr, Opcao),
    (   Opcao == 0
    ->  format('~nSaindo... Até logo!~n'), !
    ;   executar_opcao(Opcao)
    ).

% Executa a opção escolhida
executar_opcao(1) :-
    format('~n--- RECOMENDAÇÃO COMPLETA DE PC ---'),
    format('~nOrçamento total (R$): '),
    read_line_to_string(user_input, OrcamentoStr),
    atom_number(OrcamentoStr, Orcamento),
    format('Resolução (1080p, 1440p, 4k, ultrawide): '),
    read_line_to_string(user_input, ResolucaoStr),
    % Converter para minúsculas
    string_lower(ResolucaoStr, ResolucaoLower),
    atom_string(Resolucao, ResolucaoLower),
    format('~nBuscando recomendações...~n'),
    (   melhores_recomendacoes(Orcamento, Resolucao, Top3),
        Top3 \= []
    ->  format('~n=== TOP 3 RECOMENDAÇÕES ===~n'),
        forall(member(PC, Top3), mostrar_recomendacao(PC))
    ;   format('~nNenhuma recomendação encontrada para os critérios fornecidos.~n'),
        format('Tente aumentar o orçamento ou escolher uma resolução menor.~n')
    ).

executar_opcao(2) :-
    format('~n--- CONSULTAR PEÇA POR PREÇO MÁXIMO ---'),
    format('~nTipo de peça (cpu, gpu, ram, ssd, fonte, placa_mae): '),
    read_line_to_string(user_input, TipoStr),
    % Converter para minúsculas
    string_lower(TipoStr, TipoLower),
    atom_string(Tipo, TipoLower),
    format('Preço máximo (R$): '),
    read_line_to_string(user_input, PrecoStr),
    atom_number(PrecoStr, PrecoMax),
    format('~n======================================~n'),
    consultar_por_preco(Tipo, PrecoMax).

executar_opcao(3) :-
    format('~n--- CONSULTAR GPU POR QUALIDADE ---'),
    format('~nQualidade (1080p, 1440p, 4k, ultrawide): '),
    read_line_to_string(user_input, ResolucaoStr),
    % Converter para minúsculas
    string_lower(ResolucaoStr, ResolucaoLower),
    atom_string(Resolucao, ResolucaoLower),
    format('~n======================================~n'),
    consultar_gpu_qualidade(Resolucao).

executar_opcao(4) :-
    format('~n--- CONSULTAR FONTE POR POTÊNCIA ---'),
    format('~nPotência mínima (W): '),
    read_line_to_string(user_input, PotenciaStr),
    atom_number(PotenciaStr, PotenciaMin),
    format('~n======================================~n'),
    consultar_fonte_potencia(PotenciaMin).

executar_opcao(5) :-
    format('~n--- CONSULTAR SSD POR CAPACIDADE ---'),
    format('~nCapacidade mínima (GB): '),
    read_line_to_string(user_input, CapacidadeStr),
    atom_number(CapacidadeStr, CapacidadeMin),
    format('~n======================================~n'),
    consultar_ssd_capacidade(CapacidadeMin).

executar_opcao(6) :-
    format('~n--- CONSULTAR RAM POR CAPACIDADE E TIPO DDR ---'),
    format('~nCapacidade mínima (GB): '),
    read_line_to_string(user_input, CapacidadeStr),
    atom_number(CapacidadeStr, CapacidadeMin),
    format('Tipo DDR (DDR4 ou DDR5): '),
    read_line_to_string(user_input, TipoDDRStr),
    % Converter para maiúsculas (DDR4, DDR5)
    string_upper(TipoDDRStr, TipoDDRUpper),
    atom_string(TipoDDR, TipoDDRUpper),
    format('~n======================================~n'),
    consultar_ram_capacidade_tipo(CapacidadeMin, TipoDDR).

executar_opcao(7) :-
    format('~n--- CONSULTAR CPU POR SOQUETE ---'),
    format('~nSoquete (AM4, AM5, LGA1700): '),
    read_line_to_string(user_input, SoqueteStr),
    % Converter para maiúsculas (AM4, AM5, LGA1700)
    string_upper(SoqueteStr, SoqueteUpper),
    atom_string(Soquete, SoqueteUpper),
    format('~n======================================~n'),
    consultar_cpu_soquete(Soquete).

executar_opcao(8) :-
    format('~n--- CONSULTAR PLACA-MÃE POR SOQUETE ---'),
    format('~nSoquete (AM4, AM5, LGA1700): '),
    read_line_to_string(user_input, SoqueteStr),
    % Converter para maiúsculas (AM4, AM5, LGA1700)
    string_upper(SoqueteStr, SoqueteUpper),
    atom_string(Soquete, SoqueteUpper),
    format('~n======================================~n'),
    consultar_placa_soquete(Soquete).

executar_opcao(9) :-
    format('~n--- TESTE AUTOMÁTICO DE REGRAS ---'),
    format('~nExecutando testes pré-configurados...~n'),
    format('~n======================================~n'),
    testar_regras_automatico.

executar_opcao(_) :-
    format('~nOpção inválida! Digite um número entre 0 e 9.~n').

% ========== FUNÇÕES DE CONSULTA COM LEGENDAS ==========

% Consulta qualquer peça por preço máximo (CORRIGIDO)
consultar_por_preco(Tipo, PrecoMax) :-
    % Exibe legenda baseada no tipo (APENAS UMA VEZ)
    exibir_legenda_tipo(Tipo),
    format('~nResultados para ~w até R$ ~2f:~n', [Tipo, PrecoMax]),
    format('--------------------------------------------------~n'),
    % Coleta todos os componentes
    findall(Comp, recomendar_preco(Tipo, PrecoMax, Comp), Componentes),
    % Exibe todos os componentes
    (   Componentes \= []
    ->  forall(member(Comp, Componentes), format('~w~n', [Comp])),
        format('~nFim da lista.~n')
    ;   format('Nenhum componente encontrado.~n~nFim da lista.~n')
    ).

% Consulta GPU por qualidade/resolução (CORRIGIDO)
consultar_gpu_qualidade(Resolucao) :-
    (   requisitos_resolucao(Resolucao, G3DMark_Min, _, _, _, _)
    ->  format('Legenda: gpu(Marca, Modelo, MemoriaGB, G3DMark, PrecoBRL)~n'),
        format('Requisito mínimo para ~w: ~w pontos G3DMark~n', [Resolucao, G3DMark_Min]),
        format('--------------------------------------------------~n'),
        % Coleta todas as GPUs
        findall(gpu(Marca, Modelo, Mem, Perf, Preco),
                (gpu(Marca, Modelo, Mem, Perf, Preco), Perf >= G3DMark_Min),
                GPUs),
        % Exibe todas as GPUs
        (   GPUs \= []
        ->  forall(member(gpu(Marca, Modelo, Mem, Perf, Preco), GPUs),
                format('gpu(~w, ~w, ~w, ~w, ~2f)~n', [Marca, Modelo, Mem, Perf, Preco])),
            format('~nFim da lista.~n')
        ;   format('Nenhuma GPU encontrada para esta resolução.~n~nFim da lista.~n')
        )
    ;   format('~nResolução inválida! Use: 1080p, 1440p, 4k, ultrawide~n')
    ).

% Consulta fonte por potência mínima (CORRIGIDO)
consultar_fonte_potencia(PotenciaMin) :-
    format('Legenda: fonte(Marca, Modelo, PotenciaW, Certificacao, PrecoBRL)~n'),
    format('Fontes com ~wW ou mais:~n', [PotenciaMin]),
    format('--------------------------------------------------~n'),
    % Coleta todas as fontes
    findall(fonte(Marca, Modelo, Pot, Cert, Preco),
            (fonte(Marca, Modelo, Pot, Cert, Preco), Pot >= PotenciaMin),
            Fontes),
    % Exibe todas as fontes
    (   Fontes \= []
    ->  forall(member(fonte(Marca, Modelo, Pot, Cert, Preco), Fontes),
            format('fonte(~w, ~w, ~w, ~w, ~2f)~n', [Marca, Modelo, Pot, Cert, Preco])),
        format('~nFim da lista.~n')
    ;   format('Nenhuma fonte encontrada.~n~nFim da lista.~n')
    ).

% Consulta SSD por capacidade mínima (CORRIGIDO)
consultar_ssd_capacidade(CapacidadeMin) :-
    format('Legenda: ssd(Marca, Modelo, CapacidadeGB, VelocidadeMBs, PrecoBRL)~n'),
    format('SSDs com ~wGB ou mais:~n', [CapacidadeMin]),
    format('--------------------------------------------------~n'),
    % Coleta todos os SSDs
    findall(ssd(Marca, Modelo, Cap, Vel, Preco),
            (ssd(Marca, Modelo, Cap, Vel, Preco), Cap >= CapacidadeMin),
            SSDs),
    % Exibe todos os SSDs
    (   SSDs \= []
    ->  forall(member(ssd(Marca, Modelo, Cap, Vel, Preco), SSDs),
            format('ssd(~w, ~w, ~w, ~w, ~2f)~n', [Marca, Modelo, Cap, Vel, Preco])),
        format('~nFim da lista.~n')
    ;   format('Nenhum SSD encontrado.~n~nFim da lista.~n')
    ).

% Consulta RAM por capacidade e tipo DDR (CORRIGIDO)
consultar_ram_capacidade_tipo(CapacidadeMin, TipoDDR) :-
    format('Legenda: ram(Marca, Modelo, CapacidadeGB, VelocidadeMHz, TipoDDR, PrecoBRL)~n'),
    format('RAM com ~wGB ou mais do tipo ~w:~n', [CapacidadeMin, TipoDDR]),
    format('--------------------------------------------------~n'),
    % Coleta todas as RAMs
    findall(ram(Marca, Modelo, Cap, Vel, Tipo, Preco),
            (ram(Marca, Modelo, Cap, Vel, Tipo, Preco), 
             Cap >= CapacidadeMin, Tipo == TipoDDR),
            RAMs),
    % Exibe todas as RAMs
    (   RAMs \= []
    ->  forall(member(ram(Marca, Modelo, Cap, Vel, Tipo, Preco), RAMs),
            format('ram(~w, ~w, ~w, ~w, ~w, ~2f)~n', [Marca, Modelo, Cap, Vel, Tipo, Preco])),
        format('~nFim da lista.~n')
    ;   format('Nenhuma RAM encontrada com esses critérios.~n~nFim da lista.~n')
    ).

% Consulta CPU por soquete (CORRIGIDO)
consultar_cpu_soquete(Soquete) :-
    % Verificar se o soquete é válido
    (   member(Soquete, ['AM4', 'AM5', 'LGA1700'])
    ->  % Informações sobre compatibilidade RAM (APENAS UMA VEZ)
        (   Soquete == 'AM4' 
        ->  format('NOTA: Soquete AM4 suporta DDR4~n')
        ;   Soquete == 'AM5' 
        ->  format('NOTA: Soquete AM5 suporta DDR5~n')
        ;   Soquete == 'LGA1700' 
        ->  format('NOTA: Soquete LGA1700 suporta DDR4 ou DDR5 (depende da CPU e placa-mãe)~n')
        ;   true
        ),
        format('Legenda: cpu(Marca, Modelo, Soquete, NivelDesempenho, Núcleos, ClockGHz, VelMaxRAM, MemSuportada, TDP, PrecoBRL)~n'),
        format('CPUs para soquete ~w:~n', [Soquete]),
        format('--------------------------------------------------~n'),
        % Coleta todas as CPUs
        findall(cpu(Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco),
                (cpu(Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco),
                 Sock == Soquete),
                CPUs),
        % Exibe todas as CPUs
        (   CPUs \= []
        ->  forall(member(cpu(Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco), CPUs),
                format('cpu(~w, ~w, ~w, ~w, ~w, ~2f, ~w, ~w, ~w, ~2f)~n', 
                       [Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco])),
            format('~nFim da lista.~n')
        ;   format('Nenhuma CPU encontrada para este soquete.~n~nFim da lista.~n')
        )
    ;   format('Soquete inválido! Use: AM4, AM5 ou LGA1700~n')
    ).

% Consulta placa-mãe por soquete (CORRIGIDO)
consultar_placa_soquete(Soquete) :-
    % Verificar se o soquete é válido
    (   member(Soquete, ['AM4', 'AM5', 'LGA1700'])
    ->  % Informações sobre compatibilidade RAM (APENAS UMA VEZ)
        (   Soquete == 'AM4' 
        ->  format('NOTA: Placas AM4 suportam DDR4~n')
        ;   Soquete == 'AM5' 
        ->  format('NOTA: Placas AM5 suportam DDR5~n')
        ;   Soquete == 'LGA1700' 
        ->  format('NOTA: Placas LGA1700 podem suportar DDR4 ou DDR5 (verifique o modelo)~n')
        ;   true
        ),
        format('Legenda: placa_mae(Marca, Modelo, Soquete, Chipset, TipoRAM, PrecoBRL)~n'),
        format('Placas-mãe para soquete ~w:~n', [Soquete]),
        format('--------------------------------------------------~n'),
        % Coleta todas as placas-mãe
        findall(placa_mae(Marca, Modelo, Sock, Chipset, TipoRAM, Preco),
                (placa_mae(Marca, Modelo, Sock, Chipset, TipoRAM, Preco),
                 Sock == Soquete),
                Placas),
        % Exibe todas as placas-mãe
        (   Placas \= []
        ->  forall(member(placa_mae(Marca, Modelo, Sock, Chipset, TipoRAM, Preco), Placas),
                format('placa_mae(~w, ~w, ~w, ~w, ~w, ~2f)~n', [Marca, Modelo, Sock, Chipset, TipoRAM, Preco])),
            format('~nFim da lista.~n')
        ;   format('Nenhuma placa-mãe encontrada para este soquete.~n~nFim da lista.~n')
        )
    ;   format('Soquete inválido! Use: AM4, AM5 ou LGA1700~n')
    ).

% Teste automático de todas as regras (ATUALIZADO)
testar_regras_automatico :-
    format('~n=== INICIANDO TESTES AUTOMÁTICOS ===~n'),
    
    format('~n1. Testando recomendar_preco para GPU com R$ 3000:'),
    format('~n--------------------------------------------------~n'),
    testar_recomendar_preco_gpu_3000,
    
    format('~n2. Testando GPU por qualidade 1080p:'),
    format('~n--------------------------------------------------~n'),
    testar_gpu_qualidade_1080p,
    
    format('~n3. Testando fonte por potência mínima 600W:'),
    format('~n--------------------------------------------------~n'),
    testar_fonte_potencia_600,
    
    format('~n4. Testando SSD por capacidade mínima 1000GB:'),
    format('~n--------------------------------------------------~n'),
    testar_ssd_capacidade_1000,
    
    format('~n5. Testando RAM por capacidade 16GB e tipo DDR4:'),
    format('~n--------------------------------------------------~n'),
    testar_ram_capacidade_tipo_16_ddr4,
    
    format('~n6. Testando CPU por soquete AM4:'),
    format('~n--------------------------------------------------~n'),
    testar_cpu_soquete_am4,
    
    format('~n7. Testando placa-mãe por soquete AM4:'),
    format('~n--------------------------------------------------~n'),
    testar_placa_soquete_am4,
    
    format('~n=== TODOS OS TESTES CONCLUÍDOS ===~n').

% Teste 1: recomendar_preco para GPU
testar_recomendar_preco_gpu_3000 :-
    findall(GPU, recomendar_preco(gpu, 3000, GPU), GPUs),
    (   GPUs \= []
    ->  forall(member(GPU, GPUs), format('~w~n', [GPU]))
    ;   format('Nenhuma GPU encontrada.~n')
    ).

% Teste 2: GPU por qualidade 1080p
testar_gpu_qualidade_1080p :-
    % Usar '1080p' em minúsculas
    (   requisitos_resolucao('1080p', G3DMark_Min, _, _, _, _)
    ->  findall(gpu(Marca, Modelo, Mem, Perf, Preco),
                (gpu(Marca, Modelo, Mem, Perf, Preco), Perf >= G3DMark_Min),
                GPUs),
        (   GPUs \= []
        ->  forall(member(gpu(M, Mod, MemGB, P, Pr), GPUs),
                format('gpu(~w, ~w, ~w, ~w, ~2f)~n', [M, Mod, MemGB, P, Pr]))
        ;   format('Nenhuma GPU encontrada.~n')
        )
    ;   format('Resolução inválida.~n')
    ).

% Teste 3: Fonte por potência mínima 600W
testar_fonte_potencia_600 :-
    findall(fonte(Marca, Modelo, Pot, Cert, Preco),
            (fonte(Marca, Modelo, Pot, Cert, Preco), Pot >= 600),
            Fontes),
    (   Fontes \= []
    ->  forall(member(fonte(M, Mod, P, C, Pr), Fontes),
            format('fonte(~w, ~w, ~w, ~w, ~2f)~n', [M, Mod, P, C, Pr]))
    ;   format('Nenhuma fonte encontrada.~n')
    ).

% Teste 4: SSD por capacidade mínima 1000GB
testar_ssd_capacidade_1000 :-
    findall(ssd(Marca, Modelo, Cap, Vel, Preco),
            (ssd(Marca, Modelo, Cap, Vel, Preco), Cap >= 1000),
            SSDs),
    (   SSDs \= []
    ->  forall(member(ssd(M, Mod, C, V, Pr), SSDs),
            format('ssd(~w, ~w, ~w, ~w, ~2f)~n', [M, Mod, C, V, Pr]))
    ;   format('Nenhum SSD encontrado.~n')
    ).

% Teste 5: RAM por capacidade 16GB e tipo DDR4
testar_ram_capacidade_tipo_16_ddr4 :-
    findall(ram(Marca, Modelo, Cap, Vel, Tipo, Preco),
            (ram(Marca, Modelo, Cap, Vel, Tipo, Preco), 
             Cap >= 16, Tipo == 'DDR4'),
            RAMs),
    (   RAMs \= []
    ->  forall(member(ram(M, Mod, C, V, T, Pr), RAMs),
            format('ram(~w, ~w, ~w, ~w, ~w, ~2f)~n', [M, Mod, C, V, T, Pr]))
    ;   format('Nenhuma RAM encontrada.~n')
    ).

% Teste 6: CPU por soquete AM4
testar_cpu_soquete_am4 :-
    findall(cpu(Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco),
            (cpu(Marca, Modelo, Sock, Desempenho, Nuc, Clock, VRAM, MemSup, TDP, Preco),
             Sock == 'AM4'),
            CPUs),
    (   CPUs \= []
    ->  forall(member(cpu(M, Mod, S, D, N, Cl, V, MS, T, Pr), CPUs),
            format('cpu(~w, ~w, ~w, ~w, ~w, ~2f, ~w, ~w, ~w, ~2f)~n', 
                   [M, Mod, S, D, N, Cl, V, MS, T, Pr]))
    ;   format('Nenhuma CPU encontrada.~n')
    ).

% Teste 7: Placa-mãe por soquete AM4
testar_placa_soquete_am4 :-
    findall(placa_mae(Marca, Modelo, Sock, Chipset, TipoRAM, Preco),
            (placa_mae(Marca, Modelo, Sock, Chipset, TipoRAM, Preco),
             Sock == 'AM4'),
            Placas),
    (   Placas \= []
    ->  forall(member(placa_mae(M, Mod, S, C, T, Pr), Placas),
            format('placa_mae(~w, ~w, ~w, ~w, ~w, ~2f)~n', [M, Mod, S, C, T, Pr]))
    ;   format('Nenhuma placa-mãe encontrada.~n')
    ).

% Exibe legenda para cada tipo de componente (APENAS UMA VEZ)
exibir_legenda_tipo(cpu) :-
    format('Legenda: cpu(Marca, Modelo, Soquete, NivelDesempenho, Núcleos, ClockGHz, VelMaxRAM, MemSuportada, TDP, PrecoBRL)~n').
exibir_legenda_tipo(gpu) :-
    format('Legenda: gpu(Marca, Modelo, MemoriaGB, G3DMark, PrecoBRL)~n').
exibir_legenda_tipo(ram) :-
    format('Legenda: ram(Marca, Modelo, CapacidadeGB, VelocidadeMHz, TipoDDR, PrecoBRL)~n').
exibir_legenda_tipo(ssd) :-
    format('Legenda: ssd(Marca, Modelo, CapacidadeGB, VelocidadeMBs, PrecoBRL)~n').
exibir_legenda_tipo(fonte) :-
    format('Legenda: fonte(Marca, Modelo, PotenciaW, Certificacao, PrecoBRL)~n').
exibir_legenda_tipo(placa_mae) :-
    format('Legenda: placa_mae(Marca, Modelo, Soquete, Chipset, TipoRAM, PrecoBRL)~n').
exibir_legenda_tipo(_) :-
    format('Legenda: (formato específico do componente)~n').

% Inicia o programa
:- initialization(menu_principal).