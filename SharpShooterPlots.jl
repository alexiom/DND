########################################################################################
# Title: Sharpshooter Plots for Rindir                                                 #
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
using DataFrames, Random, Statistics, Plots

#======================================================================================#   
#              Define constants, functions, user input varaibles                       #
#======================================================================================# 

const Nsim = 100000 #number of simulations 
const dims = 4      #number of dimensions on the plot i.e. the number of lines 

function roll_dice(n,k)
    floor.(rand(n).*k) .+ 1
end

#store Rindir's level
print("What level is Rindir?")
CharLevel = strip(readline())
CharLevel = parse(Int64,CharLevel,base=10)
if !(CharLevel in [1:1:20;])
    error("Character level must be 1 through 20.")
end

#It is one of Rindir's favored enemies?
print("Is the creature a favored enemy?")
FE = lowercase(strip(readline()))
if !(FE == "yes" || FE == "no")
    error("Favored Enemy input must be yes or no.")
end
FE = ifelse(FE=="yes",1,0)

#Is hunter's mark active?
print("Is Hunter's Mark active?")
HM = lowercase(strip(readline()))
if !(HM == "yes" || HM == "no")
    error("Hunter's Mark input must be yes or no.")
end
HM = ifelse(HM=="yes",1,0)

#======================================================================================#   
#               Character Table for Class, Subclass, and Choices                       #
#======================================================================================# 

#store character table
CharTab = DataFrame(
    levels   = [1:1:20;],
    profbone = repeat(2:6,inner=4),
    reshoot  = repeat(0:1,inner=10),
    nattacks = [repeat([1],inner=4);repeat([2],inner=16)],
    dex      = [repeat([3],inner=3);repeat([4],inner=8);repeat([5],inner=9)],
    FE_Mod   = [repeat([2],inner=5);repeat([4],inner=15)]
)

#store constants from table 
dex     = CharTab[CharLevel,"dex"][1]      #store dex modifier
NAtt    = CharTab[CharLevel,"nattacks"][1] #store the number of attacks per round
prof    = CharTab[CharLevel,"profbone"][1] #store the proficiency bonus
reshoot = CharTab[CharLevel,"reshoot"][1]  #store if the reshoot feature is active
FE_mod  = CharTab[CharLevel,"FE_Mod"][1]   #store favored enemy damage bonus

AC     = [10:1:(20+prof+dex+2+1);]                   #store armor class sequence
damage = Array{Float64}(undef, Nsim,length(AC),dims) #intialize damage array

#======================================================================================#   
#                                 Simulation Loop                                      #
#======================================================================================# 

#loop over the number of dimensions i.e. lines
for dim in 1:dims
    local rs = true                                 #intialize reshoot resource
    modif    = dex + prof + 2 + (dim % 2 == 1)*(-5) #calculate modifier     

    #loop over all armor classes
    for ac in AC .- minimum(AC) .+ 1

        #simualte the rounds of combat
        for n in 1:Nsim
            dam = 0 #intialize the damage for the round

            #loop over the number of attacks for the round
            for att in 1:(NAtt+1*(dim>2))
                roll  = roll_dice(2,20)                        #roll 2 dice
                roll  = ifelse(dim > 2, maximum(roll),roll[1]) #take max if first round       
                
                #see if its a hit
                hit   = roll ≠ 1. && (roll + modif >= AC[ac] || roll == 20.) 

                #if miss and reshoot is availible, try again
                if !hit && reshoot == 1 && rs
                    roll  = roll_dice(2,20)                        #roll 2 dice
                    roll  = ifelse(dim > 2, maximum(roll),roll[1]) #take max if 1st round 

                    #see if its a hit
                    hit   = roll ≠ 1. && (roll + modif >= AC[ac] || roll == 20.) 
                    rs    = false #set reshoot resource to false
                end

                #if it's a hit, roll damage
                if hit
                    crit = ifelse(roll==20,2,1)              #if a crit, roll 2X the dice
                    dam = dam + sum(roll_dice(1*crit,8)) +   #base damage
                        sum(roll_dice(1*crit,6))*HM +        #hunter's mark damage
                        sum(roll_dice(1*crit,8))*(att==3) +  #dread ambusher damage
                        dex + 10*(dim % 2 == 1) + FE_mod*FE  #add new damage
                end

                damage[n,ac,dim] = dam  #store damage
                rs               = true #reset reshoot resource
            end
        end
    end
end

#======================================================================================#   
#                                   Create Plots                                       #
#======================================================================================# 

damageSS      = mean(eachrow(damage[:,:,1])) #E[damage] with Sharpshooter
damageNoSS    = mean(eachrow(damage[:,:,2])) #E[damage] without Sharpshooter
damageSS1st   = mean(eachrow(damage[:,:,3])) #E[damage] with Sharpshooter 1st round
damageNoSS1st = mean(eachrow(damage[:,:,4])) #E[damage] without Sharpshooter 1st round

#calculate the maximum value for the Y axis of the plot
Ymax = ceil(maximum([damageSS;damageNoSS;damageSS1st;damageNoSS1st]))
Ymax = Ymax + (10 - (Ymax % 10)) #round it to the nearest 10

plot(AC,damageSS1st,
    labels="Sharpshooter: 1st Round",
    title="Rindir's Expected Damage Per Round by Armor Class",
    xlabel="Armor Class",ylabel="Expected Damage",
    xticks=(minimum(AC):1:maximum(AC)),yticks=(0:10:Ymax))

plot!(AC,mean(eachrow(damage[:,:,4])),labels="No Sharpshooter: 1st Round")
plot!(AC,mean(eachrow(damage[:,:,1])),labels="Sharpshooter")
plot!(AC,mean(eachrow(damage[:,:,2])),labels="No Sharpshooter")