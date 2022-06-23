###############################################################################
#Author: Alex Marsh                                                           #
#Title: Sharpshooter Plots For Rindir (UA Ranger, XGtE Gloomstalker)          #
#Date: 03/28/2021                                                             #
#Notes: Requires data.table and ggplot2 packages                              #
###############################################################################


#==============================================================================#
#                             Script Preparation                               #
#==============================================================================#

#------------------------------------------------------------------------------#
#                              Library packages                                #
#------------------------------------------------------------------------------#

library(data.table)
library(ggplot2)

#------------------------------------------------------------------------------#
#                              Define Functions                                #
#------------------------------------------------------------------------------#

RollDice = function(n,k){
  sample(1:k,n,replace=T)
}

CARA = function(x,a=1){
  if(a == 0){
    x
  }else{
    (1-exp(-a*x))/a
  }
}

#=============================================================================#
#                           Toggles and Options                               #
#=============================================================================#

Nsim      = 100000 #number of simulations
CharLevel = 13     #character level
FE        = 0      #favored enemy toggle
HM        = 0      #hunter's mark toggle
dims      = 4      #number of lines for plot
bless     = 0      #bless toggle
MW        = 1      #magic weapon toggle

if(MW == 1 & HM == 1){
  stop("Magic Weapon and Hunter's Market would almost never both be activated")
}

#=============================================================================#
#                               Character Table                               #
#=============================================================================#

#NOTE: Will have to change this for other classes and/or choices for ASI

#profbone is proficiency bonus
#reshoot is whether I have the gloomstalker reattack if miss
#nattacks is for multiattack
#dex is my dex modifier as my ability score increases
#FE_Mod is favored enemy modifier

CharTab = data.table(
  levels   = 1:20,
  profbone = c(rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4)),
  reshoot  = c(rep(0,10),rep(1,10)),
  nattacks = c(rep(1,4,),rep(2,16)),
  dex      = c(rep(3,3),rep(4,8),rep(5,9)),
  FE_Mod   = c(rep(2,5),rep(4,15))
)

#store parameters
#NOTE: max AC is chosen by 20 + ALL MODIFIERS + 1 
dex     = CharTab[levels==CharLevel,dex]         #dex modifier
NAtt    = CharTab[levels==CharLevel,nattacks]    #number of attacks
prof    = CharTab[levels==CharLevel,profbone]    #proficiency bonus
reshoot = CharTab[levels==CharLevel,reshoot]     #do I get to reshoot?
FE_Mod  = CharTab[levels==CharLevel,FE_Mod]      #Favored Enemy Modifier
AC      = 10:(20+prof+dex+2+1)                   #AC sequence
damage  = array(0,dim = c(Nsim,length(AC),dims)) #damage array to store sims

#NOTE: if dim == 3 or 4, we are in the first round

#set progress bar
pb  = txtProgressBar(min = 0, max = dims*length(AC)*Nsim, style = 3)
nit = 0 #intialize counter for progress bar

for(dim in 1:dims){
  rs = 1 #whether the reshoot resource is available
  for(en in AC-min(AC)+1){
    for(n in 1:Nsim){
      
      nit = nit + 1              #advance counter
      setTxtProgressBar(pb, nit) #update progress bar
      
      dam = 0 #intialize damage
      
      for(a in 1:(NAtt+1*(dim>2))){
        
        #in the first round, I get advantage
        roll = RollDice(2,20)
        roll = ifelse(dim>2,max(roll),roll[1]) 
        
        hitmod = dex + prof + 2 + MW*2 + 
          bless*RollDice(1,4) + (dim %% 2 == 1)*(-5) 
        
        #remember 1s are automiss and 20s are autohits
        hit = roll != 1  & ((roll + hitmod >= AC[en]) | (roll == 20))
        hit = as.numeric(hit)

        
        #if I have the reshoot ability and haven't used it, use it
        if(hit==0 & reshoot==1 & rs == 1){
          
          #roll the d20s
          roll   = RollDice(2,20)
          roll   = ifelse(dim>2,max(roll),roll[1])
          
          #calculate the to-hit mod
          hitmod = dex + prof + 2 + MW*2 + 
            bless*RollDice(1,4) + (dim %% 2 == 1)*(-5)
          
          #check if it hits
          hit    = roll != 1  & ((roll + hitmod >= AC[en]) | (roll == 20))
          hit    = as.numeric(hit) #make it a 0 or 1
          rs     = 0               #expend resource
        }
        
        #if hit, roll damage
        if(hit == 1){
          
          #roll damage dice
          # if crit, double the dice
          
          if(roll==20){
            dam = dam + 
              sum(RollDice(2,8)) +      #base longbow dice
              sum(RollDice(2,6))*HM +   #Hunter's Mark dice
              sum(RollDice(2,8))*(a==3) #Dread Ambusher Dice
          }
          else{
            dam = dam + 
              RollDice(1,8) +      #base longbow dice
              RollDice(1,6)*HM +   #Hunter's Mark dice
              RollDice(1,8)*(a==3) #Dread Ambusher Dice
          }
          
          #add damage modifiers
          dam = dam + 
            dex +                #Dex mod added to damage
            10*(dim %% 2 == 1) + #Sharpshooter bonus
            FE_Mod*FE +          #Favored Enemy bonus
            MW*2                 #Magic Weapon bonus
        }
      }
      damage[n,en,dim] = dam #store damage
      rs               = 1   #reset reshoot resource
  }
  }
}

#=============================================================================#
#                                 Make Plot                                   #
#=============================================================================#

yvars = numeric()
for(i in 1:dims){
  yvars = c(yvars,apply(damage[,,i],2,mean))
}

#make plot data for ggplot
plot_data = data.table(
  "AC" = rep(AC,dims),
  "MeanDamage" = yvars,
  "SharpshooterToggle"=c(rep("Sharpshooter",length(AC)),
                         rep("No Sharpshooter",length(AC)),
                         rep("Sharpshooter: 1st Round",length(AC)),
                         rep("No Sharpshooter: 1st Round",length(AC)))
)

#factor sharpshooter toggle for ordering on legend
plot_data[,SharpshooterToggle:=factor(SharpshooterToggle,levels = 
                                        c("Sharpshooter: 1st Round",
                                          "No Sharpshooter: 1st Round",
                                          "No Sharpshooter","Sharpshooter"))]
xbreaks = seq(min(AC),max(AC),by=1)
ybreaks = seq((floor(min(yvars)) %/% 5)*5,
              (ceiling(max(yvars)) %/% 5)*5,by=5)

ggplot(data=plot_data,aes(x=AC,y=MeanDamage,colour=SharpshooterToggle)) + 
  geom_line() + ggtitle(paste0("Rindir's Damage Per Round: Level ",CharLevel)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=xbreaks) + scale_y_continuous(breaks=ybreaks) +
  xlab("Armor Class")+ylab("Expected Damage")