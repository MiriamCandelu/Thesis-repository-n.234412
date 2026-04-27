Women's Empowerment and Public Service Outcomes
A Multidimensional Cross-Country Analysis
Master's thesis — International Affairs (Human Rights and Global Governance), Hertie School, AY 2025–2026.
Author: Miriam Candelù · Supervisor: Prof. Ruth Ditlmann, PhD
This repository contains the dataset and the R analysis scripts used in the thesis. The thesis itself examines how three dimensions of women's empowerment — educational, legal-economic, and political — relate to five public service outcomes (under-five mortality, child stunting, access to basic drinking water, mean years of schooling, and government effectiveness) across 115–119 low- and middle-income countries in 2023.
Repository contents
FileDescriptionDataset.xlsxCountry-level dataset (136 rows, 21 columns). One row per country, cross-sectional, mostly 2023 with a small number of values from 2021–2022 to fill gaps.EducationRFile.ROLS regressions for the educational empowerment channel (independent variable: female mean years of schooling, ages 25+).EconEmpowermentRFile.ROLS regressions for the economic empowerment channel (independent variable: WPEA sub-index of the World Bank's Women, Business and the Law index; female labor force participation included as a comparison variable).PoliticEmpowermentRFile.ROLS regressions for the political empowerment channel (independent variable: share of seats held by women in the lower chamber of national parliament).
Each script is structured the same way: load packages, subset the data, run descriptive statistics and correlations, estimate a sequence of nested OLS models on each of the dependent variables, and run diagnostic checks (Breusch-Pagan for heteroskedasticity, VIF for multicollinearity, residual normality, outlier checks via Cook's distance and DFBETAS). For the water-access models, heteroskedasticity-robust standard errors are used.
Data sources

World Bank — GDP per capita (PPP), urban population share, conflict/fragility classification, net ODA received, government effectiveness, under-five mortality, water access.
World Bank Gender Data Portal & Women, Business and the Law (WBL) — gender-specific legal indicators; the WPEA sub-index averages the Workplace, Pay, Entrepreneurship, and Assets domains.
UNDP Human Development Data — female and total mean years of schooling.
Inter-Parliamentary Union (IPU) — share of women in parliament.
FAO / World Bank — child stunting.

Sample is restricted to low- and middle-income countries per the World Bank 2023 classification.
Variables in Dataset.xlsx

Identifiers: Country, Country_code
Empowerment (independent variables): Wemp1_edu_female, WEmp2_WBL, WEmp2_WBL_WPEA (and its four components: Work, Pay, Enterpreneurship, Assets), WEmp2_Laborforce_participation, WEmp3_women_in_parliament
Public service outcomes (dependent variables): Health_mortality_under5, Stunted_under5, Water_access, Mean_year_schooling_total, Government_effectiveness
Controls: GDP_percapita_PPP, Urban_population, Conflict_Fragility, Net_ODAreceived, Region

Reproducing the analysis

Open the relevant script in RStudio.
Install the packages listed at the top of the script (psych, ggplot2, lmtest, sandwich, modelsummary, corrplot, stargazer, readxl, tidyverse, etc.).
Update the setwd() and read_excel() paths near the top of each script to point to your local copy of Dataset.xlsx.
Run the script top to bottom. Tables are exported via stargazer to HTML files in the working directory.

The three scripts are independent of each other and can be run in any order.
Method, briefly
OLS regression with a parallel model structure — same controls, same dependent variables, three different empowerment indicators estimated separately. The inferential logic is comparative: where results converge across the three channels for the same outcome, the cumulative weight of association strengthens the empowerment hypothesis; where they diverge, the divergence itself is informative about which mechanisms cross-sectional data can detect. Limitations (cross-sectional design, reverse causality, omitted variables) are discussed in §4.5 of the thesis.
