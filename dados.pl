% SISTEMA DE RECOMENDAÇÃO DE COMPONENTES PARA PC - BASE DE DADOS
% Arquivo: dados.pl
% Contém todos os fatos sobre componentes de PC

% ==================== CPUs (PROCESSADORES) ====================
% cpu(Marca, Modelo, Soquete, NivelDesempenho, Núcleos, ClockGHz, VelMaxRAM, MemSuportada, TDP, PrecoBRL)
% NivelDesempenho: Pontuação relativa para comparação (baseada em benchmarks)

% AMD Socket AM4 (DDR4)
cpu('AMD', 'Ryzen 5 5500', 'AM4', 14500, 6, 4.2, 3200, 'DDR4', 65, 499.90).
cpu('AMD', 'Ryzen 5 5600', 'AM4', 17500, 6, 4.4, 3200, 'DDR4', 65, 679.90).
cpu('AMD', 'Ryzen 5 5600X', 'AM4', 19500, 6, 4.6, 3200, 'DDR4', 65, 899.90).
cpu('AMD', 'Ryzen 7 5700X', 'AM4', 22500, 8, 4.6, 3200, 'DDR4', 65, 1199.90).
cpu('AMD', 'Ryzen 7 5800X', 'AM4', 24500, 8, 4.7, 3200, 'DDR4', 105, 1499.90).
cpu('AMD', 'Ryzen 7 5800X3D', 'AM4', 28500, 8, 4.5, 3200, 'DDR4', 105, 1699.90).

% Intel Socket LGA1700 (DDR4/DDR5)
cpu('Intel', 'Core i3-13100F', 'LGA1700', 12000, 4, 4.5, 4800, 'DDR4/DDR5', 89, 549.90).
cpu('Intel', 'Core i5-13400F', 'LGA1700', 21000, 10, 4.6, 4800, 'DDR4/DDR5', 148, 1099.90).
cpu('Intel', 'Core i5-13600K', 'LGA1700', 28000, 14, 5.1, 5600, 'DDR4/DDR5', 181, 1699.90).
cpu('Intel', 'Core i7-13700K', 'LGA1700', 32000, 16, 5.4, 5600, 'DDR4/DDR5', 253, 2299.90).

% AMD Socket AM5 (DDR5 - Nova Geração)
cpu('AMD', 'Ryzen 5 7600X', 'AM5', 25000, 6, 5.3, 5200, 'DDR5', 105, 1499.90).
cpu('AMD', 'Ryzen 7 7700X', 'AM5', 29000, 8, 5.4, 5200, 'DDR5', 105, 2199.90).

% ==================== GPUs (PLACAS DE VÍDEO) ====================
% gpu(Marca, Modelo, MemoriaGB, G3DMark, PrecoBRL)
gpu('NVIDIA', 'RTX 4060', 8, 17800, 2199.90).
gpu('NVIDIA', 'RTX 4060 Ti', 8, 19540, 2799.90).
gpu('AMD', 'RX 7600', 8, 18300, 1899.90).
gpu('AMD', 'RX 7700 XT', 12, 23190, 3299.90).
gpu('NVIDIA', 'RTX 4070', 12, 31230, 3899.90).
gpu('AMD', 'RX 7800 XT', 16, 28760, 3799.90).
gpu('NVIDIA', 'RTX 4070 Ti', 12, 38050, 4899.90).
gpu('NVIDIA', 'RTX 4080', 16, 45680, 7299.90).
gpu('AMD', 'RX 7900 XT', 20, 38500, 5999.90).

% ==================== RAM (MEMÓRIA) ====================
% ram(Marca, Modelo, CapacidadeGB, VelocidadeMHz, TipoDDR, PrecoBRL)
ram('Kingston', 'FURY Beast', 16, 3200, 'DDR4', 229.90).
ram('Corsair', 'Vengeance LPX', 16, 3200, 'DDR4', 249.90).
ram('ADATA', 'XPG Spectrix D50', 16, 3200, 'DDR4', 279.90).
ram('Kingston', 'FURY Beast', 32, 3200, 'DDR4', 459.90).
ram('Corsair', 'Vengeance RGB', 32, 3600, 'DDR4', 489.90).
ram('Corsair', 'Vengeance', 16, 4800, 'DDR5', 399.90).
ram('Kingston', 'FURY Beast', 32, 5200, 'DDR5', 689.90).
ram('G.Skill', 'Trident Z5', 32, 6000, 'DDR5', 899.90).

% ==================== SSDs ====================
% ssd(Marca, Modelo, CapacidadeGB, VelocidadeMBs, PrecoBRL)
ssd('Kingston', 'NV2', 500, 3500, 199.90).
ssd('Crucial', 'BX500', 480, 540, 189.90).
ssd('Kingston', 'NV2', 1000, 3500, 299.99).
ssd('Western Digital', 'Blue SN580', 1000, 4150, 349.90).
ssd('Samsung', '980', 1000, 3500, 399.90).
ssd('Samsung', '990 Pro', 1000, 7450, 699.90).
ssd('Kingston', 'NV2', 2000, 3500, 599.90).

% ==================== FONTES ====================
% fonte(Marca, Modelo, PotenciaW, Certificacao, PrecoBRL)
fonte('Fortrek', 'Black Hawk', 500, '80 Plus Bronze', 229.90).
fonte('MSI', 'MAG A550BN', 550, '80 Plus Bronze', 279.90).
fonte('Cooler Master', 'MWE Bronze V2', 650, '80 Plus Bronze', 349.90).
fonte('Gigabyte', 'UD750GM', 750, '80 Plus Gold', 579.90).
fonte('Cooler Master', 'MWE Gold 850 V3', 850, '80 Plus Gold', 449.99).
fonte('Corsair', 'RM850x', 850, '80 Plus Gold', 699.90).
fonte('NZXT', 'C850', 850, '80 Plus Gold', 799.90).
fonte('Corsair', 'RM1000x', 1000, '80 Plus Gold', 999.90).

% ==================== PLACAS-MÃE ====================
% placa_mae(Marca, Modelo, Soquete, Chipset, TipoRAM, PrecoBRL)
% AMD AM4 (DDR4)
placa_mae('ASRock', 'A520M-HDV', 'AM4', 'A520', 'DDR4', 429.90).
placa_mae('ASRock', 'B450M-HDV', 'AM4', 'B450', 'DDR4', 459.90).
placa_mae('Gigabyte', 'B550M DS3H', 'AM4', 'B550', 'DDR4', 649.90).
placa_mae('ASUS', 'TUF Gaming B550-PLUS', 'AM4', 'B550', 'DDR4', 899.90).
placa_mae('MSI', 'MPG B550 Gaming Plus', 'AM4', 'B550', 'DDR4', 949.90).

% Intel LGA1700 (DDR4/DDR5)
placa_mae('ASRock', 'H610M-HDV', 'LGA1700', 'H610', 'DDR4', 499.90).
placa_mae('Gigabyte', 'B760M DS3H', 'LGA1700', 'B760', 'DDR4', 749.90).
placa_mae('MSI', 'PRO B760M-P', 'LGA1700', 'B760', 'DDR5', 849.90).
placa_mae('ASUS', 'TUF Gaming B760-PLUS', 'LGA1700', 'B760', 'DDR5', 1299.90).
placa_mae('MSI', 'PRO Z790-P', 'LGA1700', 'Z790', 'DDR5', 1499.90).

% AMD AM5 (DDR5)
placa_mae('ASRock', 'B650M-HDV', 'AM5', 'B650', 'DDR5', 1199.90).
placa_mae('Gigabyte', 'B650 Gaming X', 'AM5', 'B650', 'DDR5', 1499.90).

% ==================== TABELA DE COMPATIBILIDADE DE CHIPSET ====================
% compatibilidade_chipset(ChipsetPlacaMae, MarcaCPU, FamiliaCPU)
% AMD
compatibilidade_chipset('A520', 'AMD', 'Ryzen 3xxx').
compatibilidade_chipset('A520', 'AMD', 'Ryzen 5xxx').
compatibilidade_chipset('B450', 'AMD', 'Ryzen 2xxx').
compatibilidade_chipset('B450', 'AMD', 'Ryzen 3xxx').
compatibilidade_chipset('B450', 'AMD', 'Ryzen 5xxx').
compatibilidade_chipset('B550', 'AMD', 'Ryzen 3xxx').
compatibilidade_chipset('B550', 'AMD', 'Ryzen 5xxx').
compatibilidade_chipset('B650', 'AMD', 'Ryzen 7xxx').

% Intel
compatibilidade_chipset('H610', 'Intel', 'Core i-12xxx').
compatibilidade_chipset('H610', 'Intel', 'Core i-13xxx').
compatibilidade_chipset('B660', 'Intel', 'Core i-12xxx').
compatibilidade_chipset('B660', 'Intel', 'Core i-13xxx').
compatibilidade_chipset('B760', 'Intel', 'Core i-12xxx').
compatibilidade_chipset('B760', 'Intel', 'Core i-13xxx').
compatibilidade_chipset('Z790', 'Intel', 'Core i-12xxx').
compatibilidade_chipset('Z790', 'Intel', 'Core i-13xxx').

% ==================== REQUISITOS POR RESOLUÇÃO ====================
% requisitos_resolucao(Resolucao, G3DMark_Min, RAM_Min_GB, SSD_Min_GB, Fonte_Min_W, CPU_Min_Perf)
requisitos_resolucao('1080p', 15000, 16, 500, 500, 12000).     % Full HD - eSports
requisitos_resolucao('1440p', 25000, 16, 1000, 650, 18000).    % Quad HD - High-End
requisitos_resolucao('4k', 35000, 32, 1000, 750, 24000).       % Ultra HD - Ultra
requisitos_resolucao('ultrawide', 30000, 32, 1000, 750, 22000). % UWQHD - Imersivo

% ==================== LÓGICA DE COMPATIBILIDADE DE CHIPSET ====================
% Determina a família da CPU baseada no modelo
determinar_familia_cpu('AMD', ModeloCPU, Familia) :-
    sub_atom(ModeloCPU, 0, 6, _, 'Ryzen '),
    sub_atom(ModeloCPU, 6, 1, _, Digito),
    atom_concat('Ryzen ', Digito, FamiliaTemp),
    atom_concat(FamiliaTemp, 'xxx', Familia).

determinar_familia_cpu('Intel', ModeloCPU, Familia) :-
    sub_atom(ModeloCPU, 0, 5, _, 'Core '),
    split_string(ModeloCPU, "-", "", [_, Numero|_]),
    sub_atom(Numero, 0, 2, _, Prefixo),
    atom_concat('Core i-', Prefixo, FamiliaTemp),
    atom_concat(FamiliaTemp, 'xxx', Familia).

% Verifica compatibilidade entre CPU e chipset da placa-mãe
cpu_compativel_placa(cpu(Marca, Modelo, Soquete, _, _, _, _, _, _, _), 
                     placa_mae(_, _, SoquetePlaca, Chipset, _, _)) :-
    Soquete == SoquetePlaca,
    determinar_familia_cpu(Marca, Modelo, FamiliaCPU),
    compatibilidade_chipset(Chipset, Marca, FamiliaCPU).

% Verifica se tipo de RAM é compatível com CPU
ram_compativel_cpu(TipoRAM, cpu(_, _, _, _, _, _, _, MemSupCPU, _, _)) :-
    (sub_atom(MemSupCPU, _, _, _, '/') ->
        split_string(MemSupCPU, "/", "", ListaTipos),
        member(TipoRAM, ListaTipos)
    ;
        TipoRAM == MemSupCPU
    ).