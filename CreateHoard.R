################################################################################
# Title: Create Hoard from Fizban's Treasury of Dragons                        #
# Author: Alex Marsh                                                           #
# Date: 03/11/2022                                                             #
# Notes: Requires the following packages:                                      #
################################################################################

################################################################################
#                                DESCRIPTION                                   #
# Code to randomly create a hoard as described in Fizban's Treasury of Dragons #
# If you wish to roll magic items from the magic item tables in the DMG, you   #
# must source them from the file "DMG_MagicItemTables.R".                      #
################################################################################

DnD_CodePath = "/Users/alexmarsh/Documents/Personal/DnD/Code/"

RollDice = function(n,k){
  sample(1:k,n,replace=T)
}

RollTable = function(n, items, lbs, ubs){
  rolls = RollDice(n,100)
  ids = sapply(rolls,function(x){which(x >= lbs & x <= ubs)})
  ids = sort(ids)
  items[ids]
}
  

CreateHoard = function(type,RollMagicItemsDMG = TRUE){
  #SET UP
  types = c("wyrmling","young","adult","ancient") # set types of dragons
  type  = trimws(tolower(type))                   # prepare type string
  
  #make sure given type is one of the four drgaon types
  if(!(type %in% types)){
    ErrorMessage = paste0("Dragon type must be one of the following: ",
                          paste(types,collapse = ", "),".")
    stop(ErrorMessage)
  }
  
  #store the type as an integer
  ## so wyrmling will be 1, young 2, adult 3, and ancient 4
  type_int = which(type == types)
  
  #if we are rolling on the DMG magic item tables, source those tables
  if(RollMagicItemsDMG){
    source(paste0(DnD_CodePath,"DMG_MagicItemTables.R"))
  }
  
  # CREATE TABLES
  ## Create Mundane Item Table
  MundaneItems = c("A painting by an artist long forgotten by everyone except the dragon.",
                   "A hogshead (large cask) containing 65 gallons of clean drinking water.",
                   "Several embroidered throw pillows depicting wyrmling dragons.",
                   "A funerary urn containing remaings the dragon can't identify.",
                   "A set of seven candlesticks bearing a god's holy symbol.",
                   "A tarnished brazier with pleasant-smelling ash.",
                   "A drum for use in religious rites, with a forebodied echo to its beat.",
                   "A stuffed Monstrosity appropriate to the local terrain.",
                   "The skull of a Fiend or Celestial.",
                   "A spinning wheel.",
                   "An hourglass filled with sparkling sand.",
                   "A crude flute with a pleasing sound.",
                   "Hundreds of thousands of fake coins interspersed with the real treasure.",
                   "A treatise on alchemy etched on steel cylinders.",
                   "The battle standard of one of the dragon's ancient foes.",
                   "A sketchbook from another world of the Material Plane, depicting unfamiliar creatures and one very familiar dragon.",
                   "A set of irregular polyhedral dice (with 9, 13, 25, and 34 sides).",
                   "A map showing the dragon's lair in relation to the villages and other long-gone landmarks.",
                   "A kneeling bench, with anyone addressing the dragon is required to use.",
                   "A scroll containing a long epic poem in praise of the dragon.",
                   "A star chart showing Bahamut and a one-headed Tiamat as constellations, with Elegy for the First World written between the stars.",
                   "A large, noisy wind chime.",
                   "A small shrine with a statuette, a brazier, and an altar dedicated to a god worshiped by many of the dragon's minions.",
                   "A jar with a dead illithid tadpole floating in preserving chemicals.",
                   "An extensive historical record in the form of carefully knotted strings.")
  
  ### Mundane item lower bounds
  MI_LBs =  seq(1,97,4) 
  
  ### Mundane item upper bounds
  MI_UBs = seq(4,100,4)
  
  ## Create Magic Item Table
  ### List of Magic Items
  MagicItems = c("Common minor item (Magic Item Table A)",
                 "Uncommon minor item (Magic Item Table B)",
                 "Rare minor item (Magic Item Table C)",
                 "Very rare minor item (Magic Item Table D)",
                 "Legendary minor item (Magic Item Table E)",
                 "Uncommon major item (Magic Item Table F)",
                 "Rare major item (Magic Item Table G)",
                 "Very rare major item (Magic Item Table H)",
                 "Legendary major item (Magic Item Table I)")
  
  ### Lower Bounds for Magic Items
  MagicLBs = matrix(c(1,35,62,101,101,78,97,101,101,
                      1,22,50,65,101,73,92,98,101,
                      1,7,19,42,65,70,73,81,92,
                      101,101,1,13,57,101,68,74,83),ncol=4)
  
  ### Upper Bounds for Magic Items
  MagicUBs = matrix(c(34,61,77,101,101,96,100,101,101,
                      21,49,64,72,101,91,97,100,101,
                      6,18,41,64,69,72,80,91,100,
                      101,101,12,56,67,101,73,82,100),ncol=4)
  
  ## Create Gem Table
  ### Store gem gp values
  GemVals = c(10,50,100,500,1000,5000)
  
  ### Lower bounds for Gem Values
  GemLBs = matrix(c(1,44,100,101,101,101,
                    1,52,76,100,101,101,
                    1,19,37,55,78,100,
                    1,15,29,43,59,94),ncol=4)
  
  ### Upper bounds for Gem Values
  GemUBs = matrix(c(43,99,100,101,101,101,
                    51,75,99,100,101,101,
                    18,36,54,77,99,100,
                    14,28,42,58,93,100),ncol=4)
  
  
  ## Create Art Table
  ### Store art gp values
  ArtVals = c(25,250,750,2500,7500)
  
  ### Lower bounds for art values
  ArtLBs  = matrix(c(1,96,101,101,101,
                    1,54,100,101,101,
                    1,50,76,100,101,
                    1,23,43,59,94),ncol=4)
  
  ### Upper bounds for art values
  ArtUBs  = matrix(c(95,100,101,101,101,
                    53,99,100,101,101,
                    49,75,99,100,101,
                    22,42,58,93,100),ncol=4)
  
  # BEGIN CREATING HOARD
  
  ## generate coins
  ### copper pieces
  cp = sum(RollDice(12,6))*100 #number of cp
  
  ### silver pieces
  n  = 4 + 2*(type_int>1)                           #number of dice for sp
  sp = sum(RollDice(n,6))*10^(type=="wyrmling")*100 #number of sp
  
  ### gold pieces
  n = ifelse(type_int==1,4,
             ifelse(type_int==2,12,
                    ifelse(type_int==3,8,6))) #number of dice for gp
  gp = sum(RollDice(n,6))*10^type_int         #number of gp
  
  ### platinum pieces
  n = ifelse(type_int==1,4,
             ifelse(type_int==2,6,
                    ifelse(type_int==3,10,12)))  #number of dice for 
  pp = sum(RollDice(n,6))*10^(type_int-1)        #number of pp
  
  
  
  ## generate mundane items
  
  k = 6 + 2*(type_int %% 2)     #set sides of dice
  n = 1 + 1*(type_int > 2)      #set number of dice
  N_MI = sum(RollDice(n,k)) #how many mundane items to generate
  
  ### roll for each mundane item to generate
  MIs = RollTable(N_MI,MundaneItems,MI_LBs,MI_UBs) 

  
  
  ## generate gems
  
  ### set number of gems to generate
  N_G  = ifelse(type_int==1,sum(RollDice(2,6)),sum(RollDice(6,6)))
  lbs  = GemLBs[,type_int] #set lower bounds from Gem LBs matrix
  ubs  = GemUBs[,type_int] #set upper bounds from Gem UBs matrix
  Gems = RollTable(N_G,GemVals,lbs,ubs)
  
  ### aggregate across gem values
  GemTable = matrix(c(GemVals,rep(0,length(GemVals))),ncol=2)
  
  #for each value of GemVals, count how many we got of that value
  for(i in 1:length(GemVals)){
    GemTable[i,2] = sum(Gems == GemVals[i])
  }
  Gems = paste(GemTable[,2], "gems worth",GemTable[,1],"gp")
  
  
  
  ## generate art objects
  
  n = ifelse(type_int==1,1,ifelse(type_int==3,3,2))  #set number of dice
  k = ifelse(type_int==3,6,ifelse(type_int==4,10,4)) #set number of dice sides
  N_art = sum(RollDice(n,k))                         #store num of art objects
  lbs = ArtLBs[,type_int] #set lower bounds from Art LB table
  ubs = ArtUBs[,type_int] #set upper bounds from Art UB table
  Art = RollTable(N_art,ArtVals,lbs,ubs) #roll on Art Table
  
  ### aggregate across art values
  ArtTable = matrix(c(ArtVals,rep(0,length(ArtVals))),ncol=2)
  ### count number of times each type of art appears
  for(i in 1:length(ArtVals)){
    ArtTable[i,2] = sum(Art == ArtVals[i])
  }
  Art = paste(ArtTable[,2], "art objects worth",ArtTable[,1],"gp")
  
  
  
  # generate magic items
  
  n = 1 + 1*(type_int == 4) #set number of dice to roll
  k = 8 - 2*(type_int == 4) #set number of dice sides
  N_m = sum(RollDice(n,k))  #number of magic items to generate
  lbs = MagicLBs[,type_int] #set lower bounds from MagicLB matrix
  ubs = MagicUBs[,type_int] #set upper bounds from MagicUB matrix
  MagItems = RollTable(N_m,MagicItems,lbs,ubs) #roll for magic items
  
  #If we don't want to roll on DMG tables, just return 
  
  if(RollMagicItemsDMG){
    # strip Table letter from Magic Item strings
    ItemTable = substr(MagItems,nchar(MagItems)-1,nchar(MagItems)-1)
    
    #initialize character vector to save rolled items in
    RolledMagItems = rep("",)
    
    #loop through each item and roll
    for(i in 1:length(ItemTable)){
      TabLet = ItemTable[i] #store the letter of the table
      
      #assign table to current table, lower bounds to LBs, upper bounds to UBs
      assign("CurrentTable",get(paste0("Table",TabLet)))
      assign("LBs",get(paste0(TabLet,"_LBs")))
      assign("UBs",get(paste0(TabLet,"_UBs")))
      
      #roll a 1d100
      roll = RollDice(1,100)
      
      ## there are two items that require one more roll:
      ## magic armor and a figurine
      ## The magic armor is a 76 on Table I
      ## The figuring is either a 12, 13, or 14 on Table G
      
      # if the roll was magic armor, then if figurine, then everything else
      if(paste0(TabLet,roll)=="I76"){
        
        #roll for specific piece of magical armor
        roll_item = RollTable(1,MagicArmorTable,MagicArmor_LBs,MagicArmor_UBs)
        RolledMagItems[i] = roll_item
        
      } else if(paste0(TabLet,roll) %in% paste0("G",12:14)){
        
        #roll for specific figurine
        roll_item = RollTable(1,FigurineTable,Figurine_LBs,Figurine_UBs)
        RolledMagItems[i] = roll_item
        
      } else{
        
        #store which magic item
        roll_id = which(roll >= LBs & roll <= UBs)
        RolledMagItems[i] = CurrentTable[roll_id]
        
      }
    }
    MagItems = RolledMagItems #replace generic magic items with rolled
  }
  
  #CREATE FINAL HOARD
  ## Note that the output is a list
  Hoard = list("copper pieces" = cp,
               "silver pieces" = sp,
               "gold pieces" = gp,
               "platnium pieces" = pp,
               "MundaneItems" = MIs,
               "Gems" = Gems,
               "Art" = Art,
               "Magic Items" = MagItems)
  
  #calculate how much the coins are worth in gp
  gp_val = cp/1000+sp/10+gp+pp*10
  
  #calculate how much the total valuables (minus magic items) are worth
  total_val = gp_val + 
    sum(ArtTable[,1]*ArtTable[,2]) + 
    sum(GemTable[,1]*GemTable[,2]) 
  
  #print total values for perspective
  print(paste0("Total currency worth ",gp_val," gp"))
  print(paste0("Total valuable worth ",total_val," gp"))
  
  #return hoard list
  Hoard
}
