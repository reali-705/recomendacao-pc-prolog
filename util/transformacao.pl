% Conversores termo -> dict por tipo
componente_para_dict(gpu(MarcaPlacaVideo, ModeloPlacaVideo, MemoriaPlacaVideo, AnoPlacaVideo, DesempenhoG3DMark, PrecoPlacaVideo), gpu{
    marca:MarcaPlacaVideo, modelo:ModeloPlacaVideo, memoria:MemoriaPlacaVideo, ano:AnoPlacaVideo, g3dmark:DesempenhoG3DMark, preco:PrecoPlacaVideo
}).

componente_para_dict(ram(MarcaMemoria, ModeloMemoria, CapacidadeMemoria, VelocidadeMemoria, TipoMemoria, PrecoMemoria), ram{
    marca:MarcaMemoria, modelo:ModeloMemoria, capacidade:CapacidadeMemoria, velocidade:VelocidadeMemoria, tipo:TipoMemoria, preco:PrecoMemoria
}).

componente_para_dict(ssd(MarcaArmazenamento, ModeloArmazenamento, CapacidadeArmazenamento, InterfaceArmazenamento, VelocidadeArmazenamento, PrecoArmazenamento), ssd{
    marca:MarcaArmazenamento, modelo:ModeloArmazenamento, capacidade:CapacidadeArmazenamento, interface:InterfaceArmazenamento, velocidade:VelocidadeArmazenamento, preco:PrecoArmazenamento
}).

componente_para_dict(fonte(MarcaFontePoder, ModeloFontePoder, PotenciaFontePoder, CertificacaoFontePoder, PrecoFontePoder), fonte{
    marca:MarcaFontePoder, modelo:ModeloFontePoder, potencia:PotenciaFontePoder, certificacao:CertificacaoFontePoder, preco:PrecoFontePoder
}).

componente_para_dict(placa_mae(MarcaPlacaMae, ModeloPlacaMae, SoquetePlacaMae, ChipsetPlacaMae, TipoMemoriaPlacaMae, PrecoPlacaMae), placa_mae{
    marca:MarcaPlacaMae, modelo:ModeloPlacaMae, soquete:SoquetePlacaMae, chipset:ChipsetPlacaMae, tipo_ram:TipoMemoriaPlacaMae, preco:PrecoPlacaMae
}).

% Enumeradores que ja devolvem dicts
chamar_componente_dict(gpu, DictPlacaVideo) :-
    gpu(MarcaPlacaVideo, ModeloPlacaVideo, MemoriaPlacaVideo, AnoPlacaVideo, DesempenhoG3DMark, PrecoPlacaVideo),
    componente_para_dict(gpu(MarcaPlacaVideo, ModeloPlacaVideo, MemoriaPlacaVideo, AnoPlacaVideo, DesempenhoG3DMark, PrecoPlacaVideo), DictPlacaVideo).

chamar_componente_dict(ram, DictMemoria) :-
    ram(MarcaMemoria, ModeloMemoria, CapacidadeMemoria, VelocidadeMemoria, TipoMemoria, PrecoMemoria),
    componente_para_dict(ram(MarcaMemoria, ModeloMemoria, CapacidadeMemoria, VelocidadeMemoria, TipoMemoria, PrecoMemoria), DictMemoria).

chamar_componente_dict(ssd, DictArmazenamento) :-
    ssd(MarcaArmazenamento, ModeloArmazenamento, CapacidadeArmazenamento, InterfaceArmazenamento, VelocidadeArmazenamento, PrecoArmazenamento),
    componente_para_dict(ssd(MarcaArmazenamento, ModeloArmazenamento, CapacidadeArmazenamento, InterfaceArmazenamento, VelocidadeArmazenamento, PrecoArmazenamento), DictArmazenamento).

chamar_componente_dict(fonte, DictFontePoder) :-
    fonte(MarcaFontePoder, ModeloFontePoder, PotenciaFontePoder, CertificacaoFontePoder, PrecoFontePoder),
    componente_para_dict(fonte(MarcaFontePoder, ModeloFontePoder, PotenciaFontePoder, CertificacaoFontePoder, PrecoFontePoder), DictFontePoder).

chamar_componente_dict(placa_mae, DictPlacaMae) :-
    placa_mae(MarcaPlacaMae, ModeloPlacaMae, SoquetePlacaMae, ChipsetPlacaMae, TipoMemoriaPlacaMae, PrecoPlacaMae),
    componente_para_dict(placa_mae(MarcaPlacaMae, ModeloPlacaMae, SoquetePlacaMae, ChipsetPlacaMae, TipoMemoriaPlacaMae, PrecoPlacaMae), DictPlacaMae).
