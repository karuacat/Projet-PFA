# 🧙‍♂️ OCaml Quest: Functional Sorcery

**OCaml Quest** est un RPG éducatif où la syntaxe est votre grimoire et les fonctions sont vos sorts. Dans ce monde, la magie n'est pas faite de mana, mais de types, de récursion et de filtrage par motif.

L'objectif est d'apprendre la programmation fonctionnelle en résolvant des énigmes et en combattant des ennemis au travers d'un interpréteur OCaml intégré au gameplay.

---

## 🛠 Architecture Technique

Le moteur est conçu avec une approche moderne et rigoureuse pour garantir extensibilité et performance :

* **Pattern ECS (Entity Component System) :** Une séparation stricte entre les données (Composants) et la logique (Systèmes). Cela permet une gestion fluide et modulaire de centaines d'entités (joueur, monstres, sorts).
* **AABB Collision (Axis-Aligned Bounding Box) :** Un système de collision géométrique simple et efficace, idéal pour un RPG en 2D, gérant les interactions entre le joueur et l'environnement.
* **Paradigme Fonctionnel :** Utilisation intensive de l'immutabilité et des types algébriques de données (ADT) pour modéliser l'état du monde de manière sûre.



---

## ✨ Fonctionnalités

* **Système de Sorts :** Écrivez du code OCaml réel pour interagir avec l'environnement (ex: `let open_door = unlock gate;;`).
* **Moteur de Rendu :** Propulsé par SDL.
* **Progression Pédagogique :** Des quêtes allant des bases des variables jusqu'aux foncteurs et aux GADTs.

---

## ⚙️ Installation & Lancement

Assurez-vous d'avoir [OPAM](https://opam.ocaml.org/) installé sur votre machine.

```bash
# Cloner le dépôt
git clone [https://github.com/karuacat/Projet-PFA.git](https://github.com/karuacat/Projet-PFA.git)
cd Projet-PFA

# Installer les dépendances
opam install . --deps-only

# Compiler et lancer le jeu
dune build @sdl <- pour compiler
./prog/game_sdl.exe <- pour lancer le jeu
```

## 📂 Project Structure

```text
PROJET-PFA/
├── lib/            # Bibliothèques internes
|   ├── ecs/        # Moteur Entity-Component-System
|   └── gfx/        # Abstraction graphique
├── prog/
|   ├── game_js.ml  # Version Web
|   └── game_sdl.ml # Version Native
├── ressources/     # Sprites, textures et polices
├── src/            # Logique centrale du jeu
|   ├── components/ # Définitions des données des entités
|   ├── core/       # Boucle principale et état
│   └── systems/    # Logique métier (IA, Physique, Rendu)
└── index.html      # Point d'entrée pour la version Web
```
