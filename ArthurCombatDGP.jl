########################################################################################
# Title: Sharpshooter Plots for Arthur                                                 #
# Author: Alex Marsh                                                                   #
# Date: 08/21/2021                                                                     #
# Notes: Requires the following packages,                                              #
#        - DataFrames                                                                  #
#        - Random                                                                      #
#        - Statistics                                                                  #
#        - Plots.                                                                      #
#        Requires user inputs.                                                         #
########################################################################################

#load packages needed
using Random, Statistics, DataFrames

# ==================================================================================== #   
#              Define constants, functions, user input varaibles                       #
# ==================================================================================== # 

function RollDice(n,k)
    floor.(rand(n).*k) .+ 1
end

# ==================================================================================== #   
#               Character Table for Class, Subclass, and Choices                       #
# ==================================================================================== # 

#store character table
CharTab = DataFrame(
    levels   = [1:1:20;],
    profbone = repeat(2:6,inner=4),
    atkmod   = [repeat([3],inner=3);repeat([4],inner=8);repeat([5],inner=9)]
)

# ==================================================================================== #   
#                               Simulation DGP Function                                #
# ==================================================================================== # 

function SimulateCombatDGP(CharLevel, MW_Mod=1,gun="pistol",SteelDef=false,Nsim=100000)
    
    ## --- Arguments
    # - CharLevel: The character level being simulated.
    # - MW_Mod: The magic weapon attack and damage modifier. Default to 1.
    # - gun: A string that is either "pistol" or "musket" to indicate gun type.
    # - SteelDef: Whether the Steel Defender should be added to the simulation.
    # - Nsim: The number of simulations. Default value is 100,000.

    if gun == "pistol"
        ddk      = 10 # set number of sides of damage dice
        NWpnDice = 1  # set number of damage dice
    elseif gun == "musket"
        ddk = 12      # set number of sides od damage dice
        NWpnDice = 1  # set number of damage dice
    else 
        error("gun must be pistol or musket.") 
    end

    ## --- Set Global Parameters
    dims = 4  #number of dimensions on the plot i.e. the number of lines 

    ## --- Character set up
    atkmod   = CharTab[CharLevel,"atkmod"][1]      #store dex modifier
    prof     = CharTab[CharLevel,"profbone"][1] #store the proficiency bonus
    NAtt     = 1+(CharLevel > 4)                #store the number of attacks per round
    arch_FS  = (CharLevel > 5)*2                # archery fighting style
    hitmod   = atkmod + prof + MW_Mod + arch_FS          # calc to-hit mod
    spellmod = atkmod + prof                             # spell attack modifier
    AC       = [10:1:(20 + hitmod + 1);]                   #store armor class sequence
    damage   = Array{Float64}(undef, Nsim,length(AC),dims) #intialize damage array

    #loop over the number of dimensions i.e. lines
    for dim in 1:dims
    
        SS_mod = (dim % 2 == 1)*(-5) #calculate sharpshooter mod     

        #loop over all armor classes
        for ac in AC .- minimum(AC) .+ 1

            #simualte the rounds of combat
            for n in 1:Nsim

                dam = 0 #intialize the damage for the round

                #loop over the number of attacks for the round
                for att in 1:NAtt
                    roll  = maximum(RollDice(1 + 1*(dim > 2),20)) #roll 2 dice     
                
                    #see if its a hit
                    hit = (roll ≠ 1.) & (roll + hitmod + SS_mod >= AC[ac] || roll == 20.) 

                    if hit

                        # roll damage dice: if crit, double the dice
                        Ndice = NWpnDice*(1 + ( roll == 20. ) )

                        # base pistol dice
                        dam = dam + sum(RollDice(Ndice,ddk)) 

                        # add damage modifiers
                        dam = dam + 
                            atkmod +               # attack mod added to damage
                            10. * (dim % 2 == 1) + # Sharpshooter bonus
                            MW_Mod                 # magic damage bonus
                    end

                    if SteelDef
                        ## --- Roll to hit
                        roll = maximum(RollDice(1, 20))

                        # Remember 1s are automiss and 20s are autohits
                        hit = (roll ≠ 1.) & ((roll + spellmod >= AC[ac]) || (roll == 20.))

                        if hit
                            dam = dam + sum(RollDice(1+(1. + roll == 20.),8)) + prof
                        end
                    end
                end
                damage[n,ac,dim] = dam  #store damage
            end
        end
    end

    return damage
end
