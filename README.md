# solveSimplex
solveSimplex is an evolutionary game theory solver for R. 

This program describes the equilibrium dynamics of single-population evolutionary game theory games of 2-5 strategies. All strategies are assumed to exist in admixture, and engage in continuous time replicator dynamics. Analysis of face and edge games allows for better understanding of equilibrium dynamics in the system and invasion pathways for individual strategies. 

This code:
1) Solves for all internal and edge equilibria present (rows in Equilibria solutions table).
2) Assesses stability of equilibrium (Eq) (Source, Saddle, or Sink) using the eigenvalue technique (Friedman and Sinervo, 2016). (Saddle# indicates the number of positive eigenvalues.)
3) Identifies games and faces with strict-sense intransitivity (Int).
4) Identifies strategies with strict-sense apostasis (Apo), negative frequency-dependence, and anti-apostasis (A_Apo), positive frequency-dependence.
5) Allows for analysis of face games as independent games in order to assess possibility of hysteresis in the system.

Download the R workspace: https://github.com/cmsette/solveSimplex/blob/master/solveSimplex.Rproj

Or the code: https://github.com/cmsette/solveSimplex/tree/master/R
