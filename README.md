# Portfolio Optimizaton
A set of simple scripts for defining and managing a portfolio of financial assets.

## What does it do?
The scripts contain a definition of S4 objects Asset and Portfolio. The classes contain methods for: 
- loading assets and their prices from yahoo.finance service or SQL server database
- finding the weights of an optimal asset portfolio given either the desired risk or desired benefitfit.

Please see documentation on S4 classes here https://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf

## How to deploy and operate?
The scripts can be used either as-is in an R project or they can be budled into a package (recommended). 

## What does it include?
- asset.R, "Asset" S4 class with related methods
- portfolio.R, "Portfolio" S4 class with related methods
