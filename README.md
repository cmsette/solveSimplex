# solveSimplex
solveSimplex is an evolutionary game theory solver for R. 

This program describes the equilibrium dynamics of single-population evolutionary game theory games of 2-5 strategies. All strategies are assumed to exist in admixture, and engage in continuous time replicator dynamics.

This code solves for any internal and face equilibria present in the system and classifies the equilibrium types (source, saddle, sink). Analysis of face and edge games allows for better understanding of equilibrium dynamics in the system and invasion pathways for individual strategies.

Methods were developed following Friedman and Sinervo (2016).