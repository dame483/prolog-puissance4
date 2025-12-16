# prolog-puissance4
Le projet consiste à créer une IA qui joue à Puissance 4 en prolog.

## Prérequis
- SWI-Prolog doit être installé sur votre système

## Pour lancer le projet

1. **Démarrez SWI-Prolog** :
   ```bash
   swipl
   ```

2. **Choisissez la version de l’IA** en modifiant le prédicat `choose_column/3` dans le fichier game.pl :
   ```prolog
   % Choix IA pour x
   choose_column(Board, 'x', Col)

   % Choix IA pour o
   choose_column(Board, 'o', Col)
   ```

3. **Chargez le fichier de lancement du jeu** :
   ```prolog
   ?- [game].
   ```
4. **Initialisez le jeu** :
   ```prolog
   ?- init_game.
   ```
4. **Lancez le jeu avec le premier joueur de votre choix** :
   ```prolog
   ?- play('x').
   ```


## Tests unitaires avec PLUnit

1. **Vérifier que PLUnit est disponible** :
```prolog
   ?- use_module(library(plunit)).
```

2. **Charger les fichiers de tests** :
```prolog
   ?- [test/nom_du_test].
```

3. **Lancer tous les tests** :
```prolog
   ?- run_tests.
```
