# OCaml Quest: Functional Sorcery

OCaml Quest est un RPG éducatif où la magie repose sur la programmation fonctionnelle: types, récursion, pattern matching et résolution de défis OCaml intégrés au gameplay.

## Objectif du projet

Le jeu propose une progression scénarisée dans plusieurs scènes (maison, ville, école, bibliothèque, classe) pour apprendre l'OCaml en jouant.

## Architecture technique

- ECS (Entity Component System): séparation claire entre données (components) et logique (systems).
- Collisions AABB: gestion des obstacles, des interactions et des transitions de zones.
- Calques de collision par scène: chaque map expose ses rectangles de collision.
- Sauvegarde versionnée: compatibilité avec les anciennes sauvegardes.

## Fonctionnalités clés

- Déplacements et interactions PNJ/objets dans plusieurs scènes.
- Dialogues et événements scénarisés.
- Défis de code OCaml avec validation en jeu.
- Système de portes et transitions entre scènes.
- Tests automatisés et tests de régression cinématiques.

## Prérequis

- [opam](https://opam.ocaml.org/)
- Dune

## Installation

```bash
git clone https://github.com/karuacat/Projet-PFA.git
cd Projet-PFA
opam install . --deps-only
```

## Build et exécution (SDL)

```bash
# Compilation
dune build @sdl

# Lancement
./prog/game_sdl.exe
```

## Tests

Deux suites sont disponibles dans `test/`:

- `test_game.ml`: collisions/maps + défis de code.
- `test_cinematics.ml`: régressions sur les déplacements scriptés (waypoints/targets).

Exécuter tous les tests:

```bash
dune runtest
```

## Contrôles en jeu

- `ZQSD`: déplacement
- `Espace`: interaction contextuelle
- `E`: interaction alternative contextuelle
- `Echap`: menu pause / fermeture de panneaux
- `Entrée`: validation dans certains écrans

## Structure du dépôt

```text
Projet-PFA/
├── lib/
│   ├── ecs/                 # Noyau ECS
│   └── gfx/                 # Abstraction graphique (SDL / JSOO)
├── prog/
│   ├── game_sdl.ml          # Entrée native
│   └── game_js.ml           # Entrée web
├── ressources/              # Sprites, polices, assets
├── src/
│   ├── components/          # Données des entités
│   ├── core/                # État global, maps, logique centrale
│   └── systems/             # Moteurs gameplay (draw, collision, IA, etc.)
├── test/
│   ├── test_game.ml
│   └── test_cinematics.ml
├── Rapport_Projet_PFA.pdf
└── README.md
```
