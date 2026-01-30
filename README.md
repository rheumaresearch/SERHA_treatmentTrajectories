# SERHA_treatmentTrajectories

## Metodologia i eines

Aquest projecte s’emmarca dins del projecte **SERhA** i té com a objectiu la gestió, anàlisi i visualització dels itineraris farmacològics de pacients a partir de dades clíniques.

## Estructura del repositori

El repositori està organitzat en diferents carpetes segons la seva funció dins del projecte:

### `code/`
Conté el codi principal del projecte escrit en R.

- **`CodiSunburst.R`**  
  Script principal del projecte, on es desenvolupa tot el flux de treball. El codi està estructurat en tres blocs diferenciats:
  - **Bloc 1:** Neteja i preparació de les dades clíniques.
  - **Bloc 2:** Generació dels gràfics interactius de tipus *sunburst*.
  - **Bloc 3:** Desenvolupament de l’aplicació Shiny per a la visualització integrada dels resultats.

- **`0.analysis_main.R`**  
  Script d’inicialització del projecte, on es carreguen els paquets necessaris i es defineix l’entorn de treball utilitzat al llarg de l’anàlisi.

### `lib/`
Carpeta generada automàticament durant l’execució del projecte. Conté fitxers interns necessaris per al correcte funcionament de l’entorn de treball i de les dependències utilitzades en R.

### `resultats/`
Inclou els resultats finals del projecte, corresponents als **quatre gràfics interactius de tipus *sunburst*** exportats en format **HTML**, que representen els diferents nivells d’agrupació dels itineraris farmacològics.

## Gestió de dades clíniques

Les dades clíniques es gestionen mitjançant **REDCap**, una plataforma segura orientada a la recerca biomèdica. REDCap constitueix la font principal de dades del projecte, incloent informació sobre tractaments previs i actuals dels pacients.

## Entorn de treball
El processament i l’anàlisi de dades s’han dut a terme amb el llenguatge **R**, utilitzant **RStudio** com a entorn de desenvolupament. S’han emprat paquets específics per al tractament, anàlisi i visualització de dades clíniques.

### Neteja i tractament de dades
Les dades exportades de REDCap han estat sotmeses a un procés de neteja i estructuració que ha inclòs:
- Tractament de valors faltants
- Correcció d’inconsistències temporals
- Reestructuració del format de les dades
- Fusió de taules de tractaments previs i actuals
- Classificació i eliminació de duplicats
- Ordenació cronològica dels tractaments

Els AINEs han estat exclosos de l’anàlisi per manca d’informació temporal fiable.

### Visualització dels itineraris farmacològics
Els itineraris farmacològics s’han representat mitjançant gràfics interactius de tipus **sunburst**, generats amb R i exportats en format **HTML**.  
S’han desenvolupat diferents visualitzacions segons el nivell d’agrupació dels tractaments (FAMM biològics, dianes terapèutiques, corticoides, etc.).

Els gràfics permeten explorar de manera interactiva els itineraris i mostren la freqüència relativa de cada patró terapèutic.

### Aplicació Shiny
Els HTMLs generats s’han integrat en una aplicació **Shiny**, que agrupa totes les visualitzacions i taules informatives en un únic entorn web interactiu, facilitant l’exploració de dades per part de l’equip clínic i investigador.
