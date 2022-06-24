########################################################################################
# Title: Create Sharpshooter Plots                                                     #
# Author: Alex Marsh                                                                   #
# Date: 08/21/2021                                                                     #
# Notes: Requires the following packages,                                              #
#        - CategoricalArrays                                                           #                                                              #
#        - Plots.                                                                      #
########################################################################################

## --- Load Packages Needed to Make Plots
using CategoricalArrays, Plots


function CreateSharpshooterPlot(Data,CharName,CharLevel)

    N_AC = size(Data)[2]
    AC1s = ones(N_AC)

    ## --- Create Data For Plot

    #  Armor class values for x axis 
    AC = collect(range(10,length = N_AC))

    # Expected Damage by row for each slice of DamageArray
    EDamage = vec(mean(Data,dims=1))

    # Create Plot Groups 

    ## Create Plot Group Levels in the Order of Data Generation
    GroupLevels = CategoricalArray(
                ["Sharpshooter",
                "No Sharpshooter",
                "Sharpshooter: Advantage",
                "No Sharpshooter: Advantage"])

    ## Repeat each element of GroupLevels N_AC times and set levels
    PlotGroups = repeat(GroupLevels, inner = N_AC)
    levels!(PlotGroups, GroupLevels)

    # Create DataFrame with plot data 

    PlotDF = DataFrame(
        x     = repeat(AC,4),
        y     = EDamage,
        group = PlotGroups)

    ## --- Create objects for plot

    # Create plot title
    PlotTitle = CharName*"'s Expected Damage Per Round: Level "*string(CharLevel)

    # Calculate Y axis max value
    Ymax = ceil(maximum(EDamage))
    Ymax = Ymax + (5 - (Ymax % 5)) #round it to the nearest 5

    ## --- Create Sharpshooter Plot
    plot(PlotDF.x,PlotDF.y,group=PlotDF.group,
        title=PlotTitle,
        xlabel="Armor Class",
        ylabel="Expected Damage",
        xticks=AC,
        yticks=(0:5:Ymax))
    
    ylims!((0,Ymax))
end

