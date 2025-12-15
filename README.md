# prolog-puissance4
Le projet consiste à créer une IA qui joue à Puissance 4 en prolog. 

## Prérequis
- SWI-Prolog doit être installé sur votre système

## Pour lancer le projet

1. **Démarrez SWI-Prolog** :
   ```bash
   swipl
   ```

2. **Chargez le fichier board.pl** :
   ```prolog
   ?- [board].
   ```

3. **Initialisez le plateau** :
   ```prolog
   ?- init_board(Board).
   ```

4. **Affichez le plateau** :
   ```prolog
   ?- init_board(Board), display_board(Board).
   ```

Cela affichera un plateau vide de Puissance 4 avec 7 colonnes et 6 lignes.

## Tests unitaires avec PLUnit

1. **Vérifier que PLUnit est disponible** :
```prolog
   ?- use_module(library(plunit)).
```

2. **Charger les fichiers de tests** :
```prolog
   ?- [test/test_ia_v2].
```

3. **Lancer tous les tests** :
```prolog
   ?- run_tests.
```