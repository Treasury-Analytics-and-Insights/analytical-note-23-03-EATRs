# Introduction
The code in this repository analyses effective average tax rates, or "EATRs". The results are presented in the Treasury Analytical Note: [Tax and Transfer Progressivity in New Zealand: Part 2 Results (AN 23/03)](https://www.treasury.govt.nz/publications/an/an-23-03).

# Overview
The scripts are organised in the following general order:

1. Create various datasets
    1. Run the TAWA model (including ACC income)
    2. ACC income data
    3. Run the TAWA model (excluding ACC income)
    4. PIE income data
    5. Wealth data
    6. Categorise Wealth data
    7. Subset Wealth data
    8. Capital gains
        1. Investment gains
        2. Owner-occupied gains
        3. Housing by territorial authority & number of bedrooms
    9. Housing costs
    10. Imputed rents
    11. Expenditure data
        1. HES19 GST data
        2. Impute GST onto HES18
2. Assign Accommodation Supplement using the TAWA take-up rate
3. Analyse the data
    1. Create person-level dataset
    2. Create family-level dataset
    3. Analysis of family EATRs
    4. Analysis of components of family EATRs
    5. Gini coefficients of EATR income distributions
4. Create output for checking and release

# Requirements
This code is intended to be run in the IDI (Stats NZ's "Integrated Data Infrastructure"), and has some dependencies on packages and data developed by the Treasury's Analytics & Insights team.

Several of these scripts require access to particular input data and scripts for
running TAWA (the Treasury's Tax and Welfare Analysis model), including
the R packages `TAWArun` and `TAWApost`. The input data could be substituted with
Stats-derived data by an expert user, but this is not explicitly supported.

# Disclaimer
This code can be modified and customised by users to meet the needs of specific
projects, and in all cases the Analytics and Insights Team,
Te Tai Ōhanga, New Zealand Treasury must be acknowledged as a source.
While all care and diligence has been used in developing this code,
The Treasury gives no warranty it is error free and will not be liable for any
loss or damage suffered as a result of its use, either directly or indirectly.