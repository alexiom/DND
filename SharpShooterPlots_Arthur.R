################################################################################
# Author: Alex Marsh                                                           #
# Title: Sharpshooter Plots For Arthur (Battlesmith Artificer)                 #
# Date: 06/23/2022                                                             #
# Notes: Requires the following packages - ggplot2                             #
################################################################################


#==============================================================================#
#                             Script Preparation                               #
#==============================================================================#

#------------------------------------------------------------------------------#
#                              Library packages                                #
#------------------------------------------------------------------------------#

library(ggplot2)

#------------------------------------------------------------------------------#
#                   Parameters, Globals, Toggles, and Options                  #
#------------------------------------------------------------------------------#

## --- Set Character Level: INTENDED TO BE MANUALLY CHANGED
CharLevel = 4

## --- Set Weapon Options
MW_Mod    = 1      # magic weapon to-hit and damage modifier
ddk       = 10     # number of sides on damage dice
NWpnDice  = 1      # number of dice rolled for standard weapon damage
SteelDef  = FALSE   # should the Steel Defender also attack? 

## --- Set Global Parameters
Nsim      = 100000 # number of simulations
dims      = 4      # number of lines for plot

## --- Define Functions for Better Code
RollDice = function(n, k) {
  # Description: Roll n k-sided dice
  # Arguments:
  #  - n: number of dice to roll
  #  - k: number of sides for each dice
  
  sample(1:k, n, replace = T)
}

#=============================================================================#
#                               Character Table                               #
#=============================================================================#

# NOTE: Will have to change this for other classes and/or choices for ASI

# profbone is proficiency bonus
# atkmod is the modifier of the attack stat. For Arthur, it is Int

CharTab = data.frame(
  levels   = 1:20,
  profbone = c(rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4)),
  atkmod   = c(rep(3, 3), rep(4, 8), rep(5, 9))
)

#store parameters
#NOTE: max AC is chosen by 20 + ALL MODIFIERS + 1
atkmod   = CharTab[CharLevel, "atkmod"]              # atk stat modifier
prof     = CharTab[CharLevel, "profbone"]            # proficiency bonus
NAtt     = 1+(CharLevel > 4)                         # number of attacks
arch_FS  = (CharLevel > 5)*2                         # archery fighting style
hitmod   = atkmod + prof + MW_Mod + arch_FS          # calc to-hit mod
spellmod = atkmod + prof                             # spell attack modifier
AC       = 10:(20 + hitmod + 1)                      # AC sequence
damage   = array(0, dim = c(Nsim, length(AC), dims)) # damage array to store sim

#NOTE: if dim == 3 or 4, we are rolling advantage

## --- Set progress bar
nit = 0 #initialize counter for progress bar
pb  = txtProgressBar(min = nit, max = dims*length(AC)*Nsim, style = 3)

## --- Loop over each dimension i.e. line for plot
for(dim in 1:dims){
  
  ## --- Set sharpshooter modifier
  SS_mod = (dim %% 2 == 1)*(-5) #if odd, use sharpshooter
  
  ## --- Loop for each value of AC
  for(en in AC-min(AC)+1){
    
    ## --- Simulation Loop
    for(n in 1:Nsim){
      
      ## --- Iteration prep
      dam = 0                    # intialize damage
      nit = nit + 1              # advance counter
    
      setTxtProgressBar(pb, nit) # update progress bar
      
      ## --- Attack Loop
      for(a in 1:NAtt){
        
        ## --- Roll to hit
        #      Note: If dim %in% 3:4, rolling with advantage
        roll = max(RollDice(1 + 1*(dim > 2), 20))
        
        #      Remember 1s are automiss and 20s are autohits
        hit = roll != 1  & ((roll + hitmod + SS_mod >= AC[en]) | (roll == 20))
        
        ## --- Roll damage if attack hit
        if(hit){
          
          # roll damage dice: if crit, double the dice
          Ndice = NWpnDice*(1 + ( roll == 20 ) )
          
          # base pistol dice
          dam = dam + sum(RollDice(Ndice,ddk)) 
          
          # add damage modifiers
          dam = dam + 
            atkmod +             # attack mod added to damage
            10*(dim %% 2 == 1) + # Sharpshooter bonus
            MW_Mod               # magic damage bonus
        }
      }
      
      ## --- Steel Defender attack of SteelDef == TRUE
      if(SteelDef){
        
        ## --- Roll to hit
        roll = RollDice(1, 20)
        
        # Remember 1s are automiss and 20s are autohits
        hit = roll != 1  & ((roll + spellmod >= AC[en]) | (roll == 20))
        
        ## --- Roll damage if attack hits
        if(hit){
          dam = dam + sum(RollDice(1*(1+roll==20),8)) + prof
        }
      }
      
      ## --- Store Damage
      damage[n,en,dim] = dam    # store damage
    }
  }
}

#=============================================================================#
#                                 Make Plot                                   #
#=============================================================================#

# calculate expected damage by each dimension
EDamage = as.numeric(sapply(1:dims,function(x){apply(damage[,,x],2,mean)}))

# 
SS_Toggle = c(rep("Sharpshooter",length(AC)),
              rep("No Sharpshooter",length(AC)),
              rep("Sharpshooter: Advantage",length(AC)),
              rep("No Sharpshooter: Advantage",length(AC)))

# make plot data for ggplot
PlotData = data.frame(
  "AC"         = rep(AC,dims),
  "MeanDamage" = EDamage,
  "SS_Toggle"  = factor(SS_Toggle,levels = 
                          c("Sharpshooter: Advantage",
                            "No Sharpshooter: Advantage",
                            "Sharpshooter",
                            "No Sharpshooter"))
)

ybreaks = seq(0,(ceiling(max(EDamage)) %/% 5)*5,by=5)

## --- Save Final Plot
FinalPlot = 
  
  ## --- Set base ggplot with plot data
  ggplot(data=PlotData,aes(x=AC,y=MeanDamage,color=SS_Toggle)) + 
  
  ## --- Set plot/geom type
  geom_line() + 
  
  ## --- Scaling axes and coloring
  scale_color_discrete(name="Sharpshooter Toggle") + 
  scale_x_continuous(breaks = AC) + 
  scale_y_continuous(breaks = ybreaks,limits = c(0,ceiling(max(EDamage)))) +
  
  ## --- Set titles for plot and axes
  ggtitle(paste0("Arthur's Damage Per Round: Level ",CharLevel)) + 
  xlab("Armor Class") + ylab("Expected Damage") +
  
  ## --- Format plot
  theme_bw() +
  theme(legend.position = c(0.8,0.8),
        plot.title = element_text(hjust = 0.5)) 

print(FinalPlot)