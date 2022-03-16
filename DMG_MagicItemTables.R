################################################################################
# Title: Rollable Tables from The DMG for Magic Items                          #
# Author: Alex Marsh                                                           #
# Date: 03/10/2022                                                             #
# Notes:                                                                       #
################################################################################

################################################################################
#                                DESCRIPTION                                   #
# Each magic item table from the Dungeon Master's Guide is saved as an object  #
# in the global environment. A table is deconstructed into a "TableX"          #
# character vector that lists the objects in order on the table, "X_LBs" a     #
# vector of integers for the lower bounds of the item's roll, and "X_UBs" a    #
# vector of integers for the upper bounds of the item's roll where "X" is the  #
# letter of the table.                                                         #
#                                                                              #
# Note: There are two items (the figurine from Table G and the Magic Armor     #
# from Table I) that require additional rows. I create separate tables for     #
# and use if-else statements in the code when rolling.                         #
################################################################################

#==============================================================================#
#                                 Table A                                      #
#==============================================================================#

TableA = c("Potion of Healing","Spell Scroll (Cantrip)", "Potion of Climbing",
           "Spell Scroll (Level 1)","Spell Scroll (Level 2)",
           "Potion of Greater Healing","Bag of Holding","Driftglobe")

A_LBs = c(01,51,61,71,91,95,99,100)
A_UBs = c(50,60,70,90,94,98,99,100)	  

TableB = c("Potion of greater healing","Potion of fire breath",
           "Potion of resistance","Ammunition, +1",
           "Potion of animal friendship","Potion of hill giant strength",
           "Potion of growth","Potion of water breathing",
           "Spell scroll (2nd level)","Spell scroll (3rd level)",
           "Bag of holding","Keoghtoms ointment","Oil of slipperiness",
           "Dust of disappearance","Dust of dryness",
           "Dust of sneezing and choking","Elemental gem",
           "Philter of love","Alchemy jug","Cap of water breathing",
           "Cloak of the manta ray","Driftglobe","Goggles of night",
           "Helm of comprehending languages","Immovable rod",
           "Lantern of revealing","Mariners armor","Mithral armor",
           "Potion of poison","Ring of swimming","Robe of useful items",
           "Rope of climbing","Saddle of the cavalier",
           "Wand of magic detection","Wand of secrets")

#==============================================================================#
#                                 Table B                                      #
#==============================================================================#

B_LBs = c(01,16,23,30,35,40,45,50,55,60,65,68,71,74,76,78,80,82,84,
          85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100)

B_UBs = c(15,22,29,34,39,44,49,54,59,64,67,70,73,75,77,79,81,83,84,
          85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100)

TableC = c("Potion of superior healing","Spell scroll (4thlevel)",
           "Ammunition, +2","Potion of clairvoyance","Potion of diminution",
           "Potion of gaseous form","Potion of frost giant strength",
           "Potion of stone giant strength","Potion of heroism",
           "Potion of invulnerability","Potion of mind reading",
           "Spell scroll (5thlevel)","Elixir of health","Oil of etherealness",
           "Potion of fire giant strength","Quaals feather token",
           "Scroll of protection","Bag of beans","Bead of force",
           "Chime of opening","Decanter of endless water",
           "Eyes of minute seeing","Folding boat","Hewards handy haversack",
           "Horseshoes of speed","Necklace of fireballs","Periapt of health",
           "Sending Stones")

#==============================================================================#
#                                 Table C                                      #
#==============================================================================#

C_LBs = c(01,16,23,28,33,38,43,48,53,58,63,68,73,76,79,82,
          85,88,90,92,93,94,95,96,97,98,99,100)

C_UBs = c(15,22,27,32,37,42,47,52,57,62,67,72,75,78,81,84,
          87,89,91,92,93,94,95,96,97,98,99,100)

#==============================================================================#
#                                 Table D                                      #
#==============================================================================#

TableD = c("Potion of supreme healing","Potion of invisibility",
           "Potion of speed","Spell scroll (6thlevel)",
           "Spell scroll (7thlevel)","Ammunition, +3","Oil of sharpness",
           "Potion of flying","Potion of cloud giant strength",
           "Potion of longevity","Potion of vitality",
           "Spell scroll (8thlevel)","Horseshoes of a zephyr",
           "Nolzurs marvelous pigments","Bag of devouring","Portable hole")

D_LBs = c(01,21,31,41,51,58,63,68,73,78,83,88,93,96,99,100)
D_UBs = c(20,30,40,50,57,62,67,72,77,82,87,92,95,98,99,100)

#==============================================================================#
#                                 Table E                                      #
#==============================================================================#

TableE = c("Spell scroll (8thlevel)","Potion of storm giant strength",
           "Potion of supreme healing","Spell scroll (9th level)",
           "Universal solvent","Arrow of slaying","Sovereign glue")

E_LBs = c(01,31,56,71,86,94,99)
E_UBs = c(30,55,70,85,93,98,100)

#==============================================================================#
#                                 Table F                                      #
#==============================================================================#

TableF = c("Weapon, +1","Shield,+ 1","Sentinel shield",
           "Amulet of proof against detection and location",
           "Boots of elvenkind","Boots of striding and springing",
           "Bracers of archery","Brooch of shielding","Broom of flying",
           "Cloak of elvenkind","Cloak of protection","Gauntlets of ogre power",
           "Hat of disguise","Javelin of lightning","Pearl of power",
           "Rod of the pact keeper, + 1","Slippers of spider climbing",
           "Staff of the adder","Staff of the python","Sword of vengeance",
           "Trident of fish command","Wand of magic missiles",
           "Wand of the war mage, + 1","Wand of web","Weapon of warning",
           "Adamantine armor (chain mail)","Adamantine armor (chain shirt)",
           "Adamantine armor (scale mail)","Bag of tricks (gray)",
           "Bag of tricks (rust)","Bag of tricks (tan)",
           "Boots of the winterlands","Circlet of blasting","Deck of illusions",
           "Eversmoking bottle","Eyes of charming","Eyes of the eagle",
           "Figurine of wondrous power (silver raven)","Gem of brightness",
           "Gloves of missile snaring","Gloves of swimming and climbing",
           "Gloves of thievery","Headband of intellect","Helm of telepathy",
           "Instrument of the bards (Doss lute)",
           "Instrument of the bards (Fochlucan bandore)",
           "Instrument of the bards (Mac-Fuimidh cittern)",
           "Medallion of thoughts","Necklace of adaptation",
           "Periapt of wound closure","Pipes of haunting","Pipes of the sewers",
           "Ring of jumping","Ring of mind shielding","Ring of warmth",
           "Ring of water walking","Quiver of Ehlonna","Stone of good luck",
           "Wind fan","Winged boots")

F_LBs = c(01,16,19,22,24,26,27,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,
          60,62,64,66:100)

F_UBs = c(15,18,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,
          61,63,65,66:100)

#==============================================================================#
#                                 Table G                                      #
#==============================================================================#

TableG = c("Weapon, +2","Figurine of wondrous power (roll d8)",
           "Adamantine armor (breastplate)","Adamantine armor (splint)",
           "Amulet of health","Armor of vulnerability","Arrow-catching shield",
           "Belt of dwarvenkind","Belt of hill giant strength","Berserker axe",
           "Boots of levitation","Boots of speed",
           "Bowl of commanding water elementals","Bracers of defense",
           "Brazier of commanding fire elementals","Cape of the mountebank",
           "Censer of controlling air elementals","Armor, +1 chain mail",
           "Armor of resistance (chain mail)",
           "Armor of resistance (chain shirt)","Armor,+ 1 chain shirt",
           "Cloak of displacement","Cloak of the bat","Cube of force",
           "Daerns instant fortress","Dagger of venom","Dimensional shackles",
           "Dragon slayer","Elven chain","Flame tongue","Gem of seeing",
           "Giant slayer","Clamoured studded leather","Helm of teleportation",
           "Horn of blasting","Horn of Valhalla (silver or brass)",
           "Instrument of the bards (Canaithmandolin)",
           "Instrument ofthe bards (Cii lyre)","loun stone (awareness)",
           "loun stone (protection)","loun stone (reserve)",
           "loun stone (sustenance)","Iron bands of Bilarro",
           "Armor, + 1 leather","Armor of resistance (leather)",
           "Mace of disruption","Mace of smiting","Mace of terror",
           "Mantle of spell resistance","Necklace of prayer beads",
           "Periapt of proof against poison","Ring of animal influence",
           "Ring of evasion","Ring of feather falling","Ring of free action",
           "Ring of protection","Ring of resistance","Ring of spell storing",
           "Ring of the ram","Ring of X-ray vision","Robe of eyes",
           "Rod of rulership","Rod of the pact keeper, +2",
           "Rope of entanglement","Armor, +1 scale mail",
           "Armor of resistance (scale mail)","Shield, +2",
           "Shield of missile attraction","Staff of charming",
           "Staff of healing","Staff of swarming insects",
           "Staff of the woodlands","Staff of withering",
           "Stone of controlling earthelementals","Sun blade",
           "Sword of life stealing","Sword of wounding","Tentacle rod",
           "Vicious weapon","Wand of binding","Wand of enemy detection",
           "Wand of fear","Wand of fireballs","Wand of lightning bolts",
           "Wand of paralysis","Wand of the war mage, +2","Wand of wonder",
           "Wings of flying")

G_LBs = c(1,12,15:100)
G_UBs = c(11,14,15:100)

#==============================================================================#
#                                 Table H                                      #
#==============================================================================#

TableH = c("Weapon, +3","Amulet of the planes","Carpet of flying",
           "Crystal ball (very rare version)","Ring of regeneration",
           "Ring of shooting stars","Ring of telekinesis",
           "Robe of scintillating colors","Robe of stars","Rod of absorption",
           "Rod of alertness","Rod of security","Rod of the pact keeper, +3",
           "Scimitar of speed","Shield, +3","Staff of fire","Staff of frost",
           "Staff of power","Staff of striking",
           "Staff of thunder and lightning","Sword of sharpnes",
           "Wand of polymorph","Wand of the war mage, + 3",
           "Adamantine armor (half plate)","Adamantine armor (plate)",
           "Animated shield","Belt of fire giant strength",
           "Belt of frost (or stone) giant strength","Armor, + 1 breastplate",
           "Armor of resistance (breastplate)","Candle of invocation",
           "Armor, +2 chain mail","Armor, +2 chain shirt","Cloak of arachnida",
           "Dancing sword","Demon armor","Dragon scale mail","Dwarven plate",
           "Dwarven thrower","Efreeti bottle",
           "Figurine of wondrous power (obsidian steed)","Frost brand",
           "Helm of brilliance","Horn ofValhalla (bronze)",
           "Instrument of the bards (Anstruthharp)","loun stone (absorption)",
           "loun stone (agility)","loun stone (fortitude)",
           "loun stone (insight)","loun stone (intellect)",
           "loun stone (leadership)","loun stone (strength)",
           "Armor, +2 leather","Manual of bodily health",
           "Manual of gainful exercise","Manual of golems",
           "Manual of quickness of action","Mirror of life trapping",
           "Nine lives stealer","Oathbow","Armor, +2 scale mail",
           "Spellguard shield","Armor, + 1 splint",
           "Armor of resistance (splint)","Armor, + 1 studded leather",
           "Armor of resistance (studded leather)","Tome of clear thought",
           "Tome of leadership and influence","Tome of understanding")

H_LBs = c(1,seq(11,53,2),55:100)
H_UBs = c(seq(10,54,2),55:100)

#==============================================================================#
#                                 Table I                                      #
#==============================================================================#

TableI = c("Defender","Hammer of thunderbolts","Luck Blade",
           "Sword of answering","Holy avenger","Ring of djinni summoning",
           "Ring of invisibility","Ring of spell turning","Rod of lordly might",
           "Vorpal sword","Belt of cloud giant strength",
           "Armor, +2 breastplate","Armor, +3 chain mail",
           "Armor, +3 chain shirt","Cloak of invisibility",
           "Crystal ball (legendary version)","Armor, + 1 half plate",
           "Iron flask","Armor, +3 leather","Armor, +1 plate",
           "Robe of the archmagi","Rod of resurrection","Armor, +1 scale mail",
           "Scarab of protection","Armor, +2 splint",
           "Armor, +2 studded leather","Well of many worlds",
           "Magic armor (roll dl2)","Apparatus of Kwalish",
           "Armor of invulnerability","Belt of storm giant strength",
           "Cubic gate","Deck of many things","Efreeti chain",
           "Armor of resistance (half plate)","Horn ofValhalla (iron)",
           "Instrument of the bards (OIIamh harp)",
           "loun stone (greater absorption)","loun stone (mastery)",
           "loun stone (regeneration)","Plate armor of etherealness",
           "Plate armor of resistance","Ring of air elemental command",
           "Ring of earthelemental command","Ring of fire elemental command",
           "Ring of three wishes","Ring of water elemental command",
           "Sphere of annihilation","Talisman of pure good",
           "Talisman of the sphere","Talisman of ultimate evil",
           "Tome of the stilled tongue")

I_LBs = c(seq(1,16,5),seq(21,39,3),seq(42,74,2),76:100)
I_UBs = c(seq(5,20,5),seq(23,42,3),seq(43,75,2),76:100)

#==============================================================================#
#                           Magic Armor Table                                  #
#==============================================================================#

MagicArmorTable = c("Armor, +2 half plate","Armor, +2 plate",
                    "Armor, +3 studded leather","Armor, +3 breastplate",
                    "Armor, +3 splint","Armor, +3 half plate","Armor, +3 plate")

MagArm_LBs = c(seq(1,11,2),12)
MagArm_UBs = sort(c(seq(2,12,2),11))


#==============================================================================#
#                           Figurine Table Table                               #
#==============================================================================#

FigurineTable = c("Bronze griffon","Ebony fly","Golden lions","Ivory goats",
                  "Marble elephant","Onyx dog","Serpentine owl")

Figurine_LBs = c(1:6,8)
Figurine_UBs = c(1:5,7,8)

#==============================================================================#
#                             Roll on Table Code                               #
#==============================================================================#

RollMagicItemDMG = function(TabLet){
  
  RollDice = function(n,k){
    sample(1:k,n,replace=T)
  }
  
  
  TabLet = toupper(TabLet)
  
  if(!(TabLet %in% LETTERS[1:9])){
    ErrorMessage = paste(LETTERS[1:9],collapse = ",")
    ErrorMessage = paste0("Input must be: ", ErrorMessage)
    stop(ErrorMessage)
  }
  
  assign("CurrentTable",get(paste0("Table",TabLet)))
  assign("LBs",get(paste0(TabLet,"_LBs")))
  assign("UBs",get(paste0(TabLet,"_UBs")))
  
  roll = RollDice(1,100)
  
  if(paste0(TabLet,roll)=="I76"){
    
    MagArmRoll = RollDice(1,12)
    roll_id    = which(MagArmRoll >= MagArm_LBs & MagArmRoll <= MagArm_UBs)
    Item       = MagicArmorTable[roll_id]
    
  } else if(paste0(TabLet,roll) %in% paste0("G",12:14)){
    
    FigRoll = RollDice(1,8)
    roll_id = which(FigRoll >= Figurine_LBs & FigRoll <= Figurine_UBs)
    Item    = FigurineTable[roll_id]
    
  } else{
    
    roll_id = which(roll >= LBs & roll <= UBs)
    Item    = CurrentTable[roll_id]
    
  }
  
  return(Item)
}
