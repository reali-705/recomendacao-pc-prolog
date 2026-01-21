% Verifica se RAM é compatível com placa-mãe pelo tipo DDR.
compativel_ram_placa(ram(_, _, _, _, TipoRAM, _), placa_mae(_, _, _, _, TipoRAM_MB, _)) :-
    TipoRAM == TipoRAM_MB.

% Verifica se uma configuração completa é compatível.
configuracao_compativel(
    gpu(_, _, _, _, _, _),
    ram(_, _, _, _, TipoRAM, _),
    ssd(_, _, _, _, _, _),
    fonte(_, _, _, _, _),
    placa_mae(_, _, _, _, TipoRAM_MB, _)
) :-
    TipoRAM == TipoRAM_MB.
