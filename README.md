# Input data format

The script reads one Excel workbook with three sheets, one row per patient in each sheet. Column names are matched ignoring case, accents and extra spaces (for example `Codificação AlfaN` and `codificacao alfan` are equivalent).

`example\\\\\\\_synthetic\\\\\\\_data.xlsx` is randomly generated and only shows the structure.

## Sheet 1: clinical data

|Column|Required|Notes|
|-|-|-|
|`Codificação AlfaN`|yes|Patient code, the same in the three sheets|
|`Tipo de eletrodo`|yes|'Lateral wall' or 'Perimodiolar'|
|`Diabetes`, `Hipertensão`, `Obesidade`, `Tabaco`, `Alcool`, `Dislipidemia`|yes|`Sim` / `Não`|
|`Idade da implantação`|no|Age at implantation (years)|
|`Etiologia`|no|Grouped as infectious, degenerative, traumatic, iatrogenic, genetic, idiopathic or other|
|`Abordagem`|no|Surgical approach (Table 7, Figure 4)|
|`Marca`|no|Implant brand (Table 7, Figure 4)|

## Sheet 2: VNG (pre and post implantation)

|Column|Required|Notes|
|-|-|-|
|`Codificação AlfaN`|yes|Patient code|
|`Sexo`|yes|`F` / `M` or `Feminino` / `Masculino`|
|`Ouvido`|yes|Implanted ear: `Dto`, `D`, `OD`, `Direito` (right) or `Esq`, `E`, `OE`, `Esquerdo` (left)|
|`SPV\\\\\\\_OD\\\\\\\_fria\\\\\\\_pré`, `SPV\\\\\\\_OE\\\\\\\_fria\\\\\\\_pré`, `SPV\\\\\\\_OD\\\\\\\_quente\\\\\\\_pré`, `SPV\\\\\\\_OE\\\\\\\_quente\\\\\\\_pré`|yes|Slow phase velocity (deg/s) of the four caloric irrigations before implantation|
|same four columns with suffix `\\\\\\\_pós`|for pre/post analyses|After implantation|
|`SPV\\\\\\\_total\\\\\\\_pré`, `SPV\\\\\\\_total\\\\\\\_pós`|no|Total SPV|
|`Fraqueza\\\\\\\_Unilateral\\\\\\\_(%)\\\\\\\_pré` / `\\\\\\\_pós`|no|Labyrinthine predominance; recalculated from the SPV values|
|`Preponderância\\\\\\\_direcional\\\\\\\_(%)\\\\\\\_pré` / `\\\\\\\_pós`|no|Directional preponderance|
|`Data de Nascimento`, `Data de imp`|no|Dates used to derive age|
|`Exames VNG pós (1-FEITO; 2-NÃO)`|no|Flag for post-operative VNG done|

## Sheet 3: pre-operative audiometry

|Column|Required|Notes|
|-|-|-|
|`Codificação AlfaN`|yes|Patient code|
|`HT 250K`, `HT 500K`, `HT 1K`, `HT 2K`, `HT 4K`|yes|Hearing thresholds (dB HL)|
|`LPTA`, `PTA (dB)`|yes|Low pure tone average and pure tone average (dB HL)|
|`Discriminação Máxima`|yes|Maximum speech discrimination (%)|
|`STR (%)`|yes|Speech recognition threshold|



