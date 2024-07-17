#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# JNFS2024 - Wataru Yoshida
# A Master R Script for the paper below
# 吉田航，2024，「結婚・出産がもたらす女性内賃金格差の規定要因―働き方の分布と報酬による要因分解―」『人口問題研究』80(2): 205-226
# https://doi.org/10.50870/0002000320
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

ScriptDir <- "01_RScript/"

## 1 Preparation ----
source(paste0(ScriptDir, "01_Libraries&Parameters.R"))
source(paste0(ScriptDir, "02_ImportData.R"))
source(paste0(ScriptDir, "03_CleanVariables.R"))
source(paste0(ScriptDir, "04_KeepNecessaryVariables.R"))
source(paste0(ScriptDir, "05_KeepNecessaryCases.R"))
source(paste0(ScriptDir, "06_PrepareforDecomposition.R"))
source(paste0(ScriptDir, "07_ChangeLabels.R"))

## 2 Basic Analyses ----
source(paste0(ScriptDir, "08_1_DescStats_IVs.R"))
source(paste0(ScriptDir, "08_2_DescStats_Wage.R"))
source(paste0(ScriptDir, "08_3_DescStats_Worktime.R")) #not in use for the paper
source(paste0(ScriptDir, "08_4_DescStats_Income.R")) #not in use for the paper
source(paste0(ScriptDir, "09_Coefplot.R"))

## 3 Decomposition ----
source(paste0(ScriptDir, "10_1_Decompose_Wage.R"))
source(paste0(ScriptDir, "10_2_Decompose_Worktime.R")) #not in use for the paper
source(paste0(ScriptDir, "10_3_Decompose_Income.R")) #not in use for the paper

