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
- Write one paragraph per region (West Coast and Baltic Sea), clearly separated.
- Cover all sea areas visited during the cruise.
- Mention dominant taxonomic groups based on the group information provided in the prompt data.
- Highlight notable or unusual findings.
- Always mention potentially harmful taxa if they were found, mark them with an asterisk (*).
- Compare chlorophyll fluorescence between stations and how it relates to the biovolume measured by the IFCB, when data is available.
- The Swedish summary uses Swedish marine area names: "Västerhavet" (West Coast), "Skagerrak", "Kattegatt", "Östersjön" (Baltic Sea).
- The English summary uses English names: "Skagerrak", "Kattegat", "Baltic Sea".

### Station descriptions
Each station description follows a consistent pattern:

1. **Opening sentence**: Characterize overall diversity and cell abundance (high/moderate/low).
   - Example: "The species diversity was high but the total cell counts were low."
   - Example: "Phytoplankton diversity and total cell numbers were both low."

2. **Dominant groups**: State which groups dominate using the explicit detailed group assignments provided in the prompt data. Use the exact grouping supplied there, including groups such as cryptomonads and Mesodinium when present. Do not infer group membership from scientific names.
   - Example: "Diatoms dominated among the larger cells."

3. **Key species by group**: Describe the most abundant species organized by the collapsed four-group assignments provided in the prompt data: **diatoms**, **dinoflagellates**, **cyanobacteria**, and **others**. The "others" group includes all taxa outside the first three groups, for example coccolithophores, silicoflagellates, euglenophytes, cryptomonads, and Mesodinium. Present the groups in order of their abundance at the station -- mention the most dominant group first. Only mention groups that are actually present in the data; do not mention absent groups. When describing the "others" group, name the taxa by their specific identity rather than calling them "others". After first mention of a species, abbreviate genus to initial.
   - Example: "Among the diatoms, Dactyliosolen fragilissimus was the most abundant species, followed by Cerataulina pelagica. Heterocapsa rotundata was the most common dinoflagellate."
   - Example: "The cyanobacterium Aphanizomenon flosaquae* dominated the community. Among the diatoms, Skeletonema marinoi and Chaetoceros spp. were found in lower numbers."
   - Example with others: "Diatoms dominated the community, with Skeletonema marinoi as the most abundant species. The coccolithophore Emiliania huxleyi and the silicoflagellate Dictyocha speculum were also present."
   - Subsequent mention: "D. fragilissimus", "C. pelagica"

4. **Small cells**: Mention the smaller fraction separately if notable (cryptomonads, small flagellates, coccolithophores when in the small size fraction).
   - Example: "The smaller cells were dominated by the coccolithophore Emiliania huxleyi."
   - Example: "The smaller cells were represented by different cryptomonads."

5. **Potentially harmful taxa**: Always mention if potentially harmful taxa are present, especially if common. Mark with asterisk.
   - If a potentially harmful taxon is one of the dominant taxa by biovolume or cell counts, it MUST be named as dominant in the community description (steps 2–3) as well as mentioned in this section. Do not relegate a dominant taxon to this section only because it carries a HAB flag.
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
- The classifier categories "unclassified" and "unicells" are always written in lower case, even mid-sentence. Never write "Unclassified" or "Unicells" unless one of these words opens a sentence.
- Higher taxonomic groups: "Cryptomonadales", "Gymnodiniales", "Dictyochales"
- Mark potentially harmful taxa with an asterisk (*) after the name
- Species-group qualifiers (sflag): some taxa in the data are recorded at genus or group level and carry a qualifier such as "spp.", "sp.", or "group" (e.g. "Chaetoceros spp.", "Dinophysis sp.", "Pseudo-nitzschia group"). Always include this qualifier when writing the name. The qualifier is plain text and not italicized; only the genus/species epithet is in italics.

## Terminology

### Harmful taxa terminology
- Never use the term "HAB species" or "HAB-arter".
- In English: say "potentially harmful taxon" (singular) or "potentially harmful taxa" (plural).
- In Swedish: say "potentiellt skadligt taxon" (singular) or "potentiellt skadliga taxa" (plural). Use "potentiellt skadlig art" only when referring to a specific species.

### Warning level exceedances
Some potentially harmful taxa have recommended warning levels (cells/L). When the data indicates that a taxon has exceeded its warning level at a station, this MUST be explicitly stated in both the relevant station description and in both summaries (Swedish and English).
- In English: "The abundance of [taxon]* was X cells/L at [station], exceeding the recommended warning level of Y cells/L."
- In Swedish: "Förekomsten av [taxon]* uppgick till X celler/L vid [station], vilket överstiger den rekommenderade varningsnivån på Y celler/L."
- Warning level exceedances are always serious findings and must be clearly communicated, regardless of whether the taxon is mentioned elsewhere in the description.

### Swedish terminology
When writing in Swedish, use the correct Swedish terms:
- "klorofyll" (not "chlorophyll")
- "klorofyllfluorescens" (not "chlorophyll fluorescence")
- "biovolym" (not "biovolume")
- "kiselalger" (not "diatomeer" or "diatoméer") for diatoms
- Use proper Swedish characters: å, ä, ö (e.g. Västerhavet, Östersjön, Kattegatt)

## Text formatting rules
- Output PLAIN TEXT only. Do NOT use any markdown formatting (no asterisks for emphasis like *italic* or **bold**, no headers with #, no bullet points).
- The ONLY asterisk allowed is the potentially harmful taxon marker, placed directly after the species name with no space: Dinophysis acuminata*

### What NOT to include
- Do not include figure captions (these are added separately).
- Do not include methodology descriptions.
- Do not make up data or species that are not in the provided data.
- Do not use exact numerical values for cell counts in the text -- use qualitative descriptions instead (high, low, moderate, abundant, etc.). Exception: when a taxon exceeds its recommended warning level, you MUST state the actual abundance in cells/L (e.g. "2 000 cells/L, exceeding the warning level of 1 500 cells/L").
- Do not reference "normal" chlorophyll ranges (we do not have reference values for ferrybox data). Instead, compare between stations.
- Do not include depth-integrated chlorophyll values (0-10 m, 0-20 m) as these come from external sources not available to the IFCB.

## Example station descriptions

### West Coast station example
"The species diversity was high but the total cell counts were low. Among the diatoms, the genus Pseudo-nitzschia was found in highest cell counts. Only a few cells of dinoflagellates were noted. The smaller cells were dominated by the coccolithophore Emiliania huxleyi."

### Baltic station example
"The phytoplankton diversity was low, but total cell numbers were relatively high. Among the diatoms, Dactyliosolen fragilissimus was the most abundant species, followed by Cerataulina pelagica. The smaller cells were represented by cells from the order Gymnodiniales."

### Baltic station with potentially harmful taxon
"Phytoplankton diversity and total cell numbers were low. The cyanobacterium Aphanizomenon flosaquae* was relatively abundant. The smaller cells were dominated by cryptomonads."

### Station with chlorophyll comparison
"The total cell numbers and biodiversity were both high. Among the diatoms, Dactyliosolen fragilissimus was the most abundant species, followed by Skeletonema marinoi. Heterocapsa rotundata was the most common dinoflagellate. Chlorophyll fluorescence was the highest among all stations visited, consistent with the elevated biovolume detected by the IFCB."
