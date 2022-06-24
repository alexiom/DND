########################################################################################
# Title: Sharpshooter Plots for Arthur                                                 #
# Author: Alex Marsh                                                                   #
# Date: 06/23/2022                                                                     #
# Notes:                                                                               #
########################################################################################

codedir   = "/Users/alexmarsh/Documents/Personal/DnD/DND_Code/" # set path to DnD code
CharName  = "Arthur"                                            # set character name

## --- Include code that simulates combat DGP for character
include(codedir*CharName*"CombatDGP.jl")

## --- Include code that creates Sharpshooter Plots
include(codedir*"CreateSharpshooterPlots.jl")


# ==================================================================================== #
#                         Generate Data and Create Plot                                #
# ==================================================================================== #

## --- Set Character Level
CharLevel = 4

## --- Generate Data From DGP 
DamageArray = SimulateCombatDGP(CharLevel)

## --- Create Sharpshooter Plot
CreateSharpshooterPlot(DamageArray,CharName,CharLevel) 