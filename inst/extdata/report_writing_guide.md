# AlgAware Report Writing Guide

This guide describes how to write text for the AlgAware phytoplankton monitoring reports produced by SMHI. These reports summarize data from Imaging FlowCytobot (IFCB) instruments deployed on R/V Svea during monthly monitoring cruises along the Swedish coast.

## Report structure

Each report contains:

1. **Sammanfattning** (Swedish summary) -- 1-2 paragraphs covering all sea areas
2. **Abstract / Summary** (English summary) -- mirrors the Swedish summary
3. **Station descriptions** -- grouped by sea area, one per station visit

The station descriptions are organized under regional headings:
- **The Skagerrak** (e.g., stations A17, Slaggo)
- **The Kattegat** (e.g., stations Anholt E, N14 Falkenberg, Fladen)
- **The Baltic** (e.g., stations BY2, BY5, BCSIII-10, BY15, BY29, BY31, BY38, BY39)

## Writing style

### General tone
- Scientific but accessible: use proper Latin species names in italics context, but describe the ecological situation in plain language.
- Qualitative language for abundances: "relatively high", "quite low", "moderate", "rather numerous", "very low", "enhanced", "abundant".
- Concise: station descriptions are typically 3-6 sentences (50-100 words). Summaries are 100-200 words.
- Do NOT use bullet points or numbered lists. Write flowing prose.

### Summaries (Sammanfattning / Abstract)
- Cover all sea areas visited during the cruise.
- Mention dominant taxonomic groups (diatoms, dinoflagellates, cyanobacteria, coccolithophores).
- Highlight notable or unusual findings.
- Always mention HAB (Harmful Algal Bloom) species if they were found, mark them with an asterisk (*).
- Compare chlorophyll fluorescence between stations and how it relates to the biovolume measured by the IFCB, when data is available.
- The Swedish summary uses Swedish marine area names: "Västerhavet" (West Coast), "Skagerrak", "Kattegatt", "Östersjön" (Baltic Sea).
- The English summary uses English names: "Skagerrak", "Kattegat", "Baltic Sea".

### Station descriptions
Each station description follows a consistent pattern:

1. **Opening sentence**: Characterize overall diversity and cell abundance (high/moderate/low).
   - Example: "The species diversity was high but the total cell counts were low."
   - Example: "Phytoplankton diversity and total cell numbers were both low."

2. **Dominant groups**: State which groups dominate (diatoms, dinoflagellates, cyanobacteria, etc.).
   - Example: "Diatoms dominated among the larger cells."

3. **Key species**: Name the most abundant species with their scientific names. After first mention, abbreviate genus to initial.
   - Example: "The diatom Dactyliosolen fragilissimus was the most abundant species, followed by Cerataulina pelagica."
   - Subsequent mention: "D. fragilissimus", "C. pelagica"

4. **Small cells**: Mention the smaller fraction separately if notable.
   - Example: "The smaller cells were dominated by the coccolithophore Emiliania huxleyi."
   - Example: "The smaller cells were represented by different cryptomonads."

5. **HAB species**: Always mention if harmful algal bloom species are present, especially if common. Mark with asterisk.
   - Example: "The toxin producing Dinophysis acuminata* was found in high cell numbers."
   - Example: "Low occurrences of the filamentous cyanobacterium Aphanizomenon flosaquae* were observed."

6. **Chlorophyll and biovolume comparison**: If chlorophyll fluorescence data is available, compare with the IFCB biovolume data and note if they are consistent or divergent.
   - Example: "Chlorophyll fluorescence was elevated compared to other stations, consistent with the high diatom biovolume observed by the IFCB."
   - Example: "Despite relatively low chlorophyll fluorescence, the IFCB detected moderate biovolume dominated by large-celled diatoms."

### Grouping stations
- When two nearby stations show very similar conditions, they may be described together.
  - Example: "BY31 Landsort deep 22nd of October, BY38 23rd of October -- Both phytoplankton diversity and total cell numbers were low."

### Species naming conventions
- Use full scientific name on first mention: "Pseudosolenia calcar-avis"
- Abbreviate genus on subsequent mentions: "P. calcar-avis"
- Group-level names are not italicized: "diatoms", "dinoflagellates", "cyanobacteria", "coccolithophores"
- Higher taxonomic groups: "Cryptomonadales", "Gymnodiniales", "Dictyochales"
- Mark HAB species with an asterisk (*) after the name

### Text formatting rules
- Output PLAIN TEXT only. Do NOT use any markdown formatting (no asterisks for emphasis like *italic* or **bold**, no headers with #, no bullet points).
- The ONLY asterisk allowed is the HAB species marker, placed directly after the species name with no space: Dinophysis acuminata*
- Use proper Swedish characters (å, ä, ö) when writing in Swedish. For example: Västerhavet, Östersjön, Kattegatt.

### What NOT to include
- Do not include figure captions (these are added manually).
- Do not include methodology descriptions.
- Do not make up data or species that are not in the provided data.
- Do not use exact numerical values for cell counts in the text -- use qualitative descriptions instead (high, low, moderate, abundant, etc.).
- Do not reference "normal" chlorophyll ranges (we do not have reference values for ferrybox data). Instead, compare between stations.
- Do not include depth-integrated chlorophyll values (0-10 m, 0-20 m) as these come from external sources not available to the IFCB.

## Example station descriptions

### West Coast station example
"The species diversity was high but the total cell counts were low. The diatom genus Pseudo-nitzschia was found in highest cell counts among the larger cells. Only a few cells of dinoflagellates were noted. The coccolithophorid Emiliania huxleyi was rather numerous among the smaller cells."

### Baltic station example
"The phytoplankton diversity was low, but total cell numbers were relatively high. The diatom Dactyliosolen fragilissimus was the most abundant species, followed by Cerataulina pelagica and smaller taxa, including cells from the order Gymnodiniales."

### Baltic station with HAB species
"Phytoplankton diversity and total cell numbers were low. Aphanizomenon flosaquae* and small taxa, including cells from the order Cryptomonadales, were relatively abundant."

### Station with chlorophyll comparison
"The total cell numbers and biodiversity were both high, and diatoms dominated the community. Dactyliosolen fragilissimus was the most abundant species, followed by Skeletonema marinoi. Chlorophyll fluorescence was the highest among all stations visited, consistent with the elevated biovolume detected by the IFCB."
