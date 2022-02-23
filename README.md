# Corrections des dm d'informatiques

## Disclaimers

Les contributeurs et moi même ne nous engagont par dans la justesse des correction. Ces scripts ne sont en aucun representatifs de la note réel obtenu leur de l'évaluation. Cependant ils ont pour but d'aider au test des fonctions du dm.

## Comment les utiliser ?

### Prérequis
Pour utiliser les scripts il faut :
Utiliser bash et avoir installer sur celui-ci Ocaml et expect.

Pour l'installation d'Ocaml sur bash : <a link ="https://ocaml.org/docs/install.fr.html"> Regarder la documentation officielle </a>

Pour installer expect sur bash :
Dans le cas d'Ubuntu

```bash
sudo apt install expect
```

Si votre distribution n'est pas Ubuntu alors il faut vous réferer au gestionnaire de package des autres distributions et installer **expect**.

### Utilisation

Il faut se rendre dans le fichier contenant le fichier .exp et le .sh correspondant au dm voulu que vous trouverez dans les fichier dm présent dans cette page github.

Ensuite il faut mettre dans ce même fichier code ocaml correspondant.

Il vous suffit desormais d'éxécuter la commande suivante :

```bash
./test_dm12.sh dm12.ml
```

Bien évidemment ici le dm12 sert d'illustration et dans le cas d'un autre dm il suffit de remplacer le 12 par le numéro correspondant.
Dans tous les cas il faut éxécuter le fichier en .sh (bash) suivi en argument du nom du fichier.

## Comment contribuer ?

Pour contribuer il vous suffit de fork le repo et de proposer des amélioration par un pull request.

### Le fichier expect
La seule partie à modifier dans ce fichier est normalement la partie concernant les tests
donc la partie :

```expect
set tests {}
```

Le fonctionnement des tests est assez simple il vous suffit d'indiquer un code Ocaml qui renvoie `true` si le test est valider et `false` sinon.

#### Exemple :

```expect
question_1_test_a 
{
    let a = ['a'] in
    let b = ['a';'b';'c'] in
    precede a b = true
}
```