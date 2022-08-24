

library(DiagrammeR)

Extraction <- "graph TB
A((NAIADES)) -->|Extraction|B[Données Biologiques]
subgraph EXTRACTION DES DONNEES
A -->|Extraction|C[Données Chimiques]
B --> D[1 échantillon par an par station]
C --> E[12 échantillons par an par station]
E -->|-60/+15 jours|G{Fusion}
D --> |1 date|G
G ==> H((BASE DONNEES 2022))
end
style A fill:#f9f,stroke:#333,stroke-width:4px
style H fill:#f9f,stroke:#333,stroke-width:4px
style G fill:#f96,stroke:#333,stroke-width:4px

A1((Fichier DREAL)) --> B1[Taxons contributifs 2007]
subgraph TRAITEMENT DREAL
B1 --> C1>Code 4 lettres mis à jour]
A1 --> D1[Nouveaux taxons]
D1 --> E1>Profil de 2007 associé]
D1 --> F1>Profil à déterminer]
end
E1 --> G1{Associer le profil}
subgraph LIEN DREAL BASE 2022
F1 ==> H1{Analyse de co-occurence}
H1 --> I1[Association d'un profil existant: Seuil à déterminer]
I1 --> I12[Profil associé]
I1 --> I13[Pas de profil trouvé]
C1 --> H1
G1 --> J1((NOUVELLE BASE IBD))
I12 --> J1
I13 ==> K{Calcul d'un profil écologique}
K --> L[Profil calculé]
L --> J1
H --> H1
end
style A1 fill:#f9f,stroke:#333,stroke-width:4px
style J1 fill:#f9f,stroke:#333,stroke-width:4px
style G1 fill:#f96,stroke:#333,stroke-width:4px
style H1 fill:#f96,stroke:#333,stroke-width:4px
style K fill:#f96,stroke:#333,stroke-width:4px
"


mermaid(Extraction)


Source <- "graph TB
Source --> X[Traitement Dreal: Script Traitement des Profils DREAL]
Source --> Y[Extraction des donnees: Fichier Extraction des donnees, 2 fonctions, 1 script]
Source --> Z[Lien DREAL base 2022: Script Analyse de coocurrence + ...]"

mermaid(Source)


