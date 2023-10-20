library(stringr)
library(tm)
library(wordcloud)
library(xlsx)

setwd("F:/Melas_Traits/0_functions")
list.files(pattern=".R$") -> files
sapply(files, source)


#################################################
### Dirs
#################################################

setwd("F:/Melas_Traits")
getwd() -> wd

desc.dir <- "F:/Melas_Traits/3_descriptions_by_spp"
traits.out <- "F:/Melas_Traits/4_traits"

#################################################
### Import Descriptions
#################################################

setwd(desc.dir)

readDescriptions("Descriptions_singles.txt") -> descriptions
length(descriptions)

setwd(traits.out)


#################################################
### Extract - Habit
#################################################

### Size

final.unit = "m"

trait <- extractTrait.size.habit(descriptions, final.unit)

### Discrete

getCommonTerms(trait$description) -> d.class

write.csv(d.class, "Frequent_terms_habit.csv", row.names = F)

# Import classification

read.csv("Frequent_terms_habit_classification.csv") -> d.class

extractTrait.discrete(trait, d.class) -> habit.d

### Export

data.frame(checked="", species=trait$species, habit=habit.d, trait[,-1]) -> trait

write.csv(trait, "Trait_Habit-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Leaves
#################################################

### Size

target = c("leaf", "leaves", "blade", "lamina", "hoja")
non.target = c("placent", "hypanthia", "calyx", "inflorescence")
final.unit = "cm"

trait <- extractTrait.size(descriptions, target, non.target, final.unit, length.only = F)

### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Leaves-to_check_to_update.csv", row.names = F, na="")

#################################################
### Extract - Petioles
#################################################

### Size

target = c("petiole", "peciolo")
non.target = c("placent", "hypanthia", "calyx", "inflorescence")
final.unit = "cm"

trait <- extractTrait.size(descriptions, target, non.target, final.unit, length.only = T)

### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Petioles-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Number of veins
#################################################

### Fix "+"

descriptions -> descriptions.veins

for (i in 1:length(descriptions.veins)) {
  descriptions.veins[[i]] -> x
  x[1] -> desc0
  grep("+", desc0, fixed=T) -> g0
  if (length(g0) > 0) {
    strsplit(desc0, " ")[[1]] -> desc0
    grep("+", desc0, fixed=T) -> g0
    for (k in 1:length(g0)) {
      g0[k] -> g1
      desc0[g1] -> f0
      f0 <- sum(as.numeric(strsplit(f0, "+", fixed=T)[[1]]))
      if (is.na(f0)==F) {
        desc0[g1] <- f0
      }
    }
    paste(desc0, collapse=" ") -> desc0
    desc0 -> x[1]
    descriptions.veins[[i]] <- x
  }
}

descriptions[[847]]
descriptions.veins[[847]]

### Count

target = c("veins", "nerve", "nervura", "plinerv", "basinerv")
non.target = c("trichome", "tricoma")

trait <- extractTrait.count(descriptions.veins, target, non.target)

### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Number_of_veins-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Petal
#################################################

### Size

target = "petal"
non.target = c("antepetal", "petaloid")
final.unit = "mm"

trait <- extractTrait.size(descriptions, target, non.target, final.unit, length.only = F)

### Discrete

exclude = c("petal", "glabrous")

getCommonTerms(trait$description, exclude) -> d.class

write.csv(d.class, "Frequent_terms_petal.csv", row.names = F)

# Import classification

read.csv("Frequent_terms_petal_classification.csv") -> d.class

extractTrait.discrete(trait, d.class) -> petal.d

### Export

data.frame(checked="", species=trait$species, color=petal.d, trait[,-1]) -> trait

write.csv(trait, "Trait_Petal-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Number of petals
#################################################

### Fix "pentamerous"

descriptions -> descriptions.petals

for (i in 1:length(descriptions.petals)) {
  descriptions.petals[[i]] -> desc0
  desc0[1] -> x
  sub("trimerous", "3-merous", x, ignore.case = T) -> x
  sub("tetramerous", "4-merous", x, ignore.case = T) -> x
  sub("pentamerous", "5-merous", x, ignore.case = T) -> x
  sub("hexamerous", "6-merous", x, ignore.case = T) -> x
  sub("heptamerous", "7-merous", x, ignore.case = T) -> x
  sub("octamerous", "8-merous", x, ignore.case = T) -> x
  sub("nonamerous", "9-merous", x, ignore.case = T) -> x
  sub("decamerous", "10-merous", x, ignore.case = T) -> x
  sub("trimera", "3-merous", x, ignore.case = T) -> x
  sub("tetramera", "4-merous", x, ignore.case = T) -> x
  sub("pentamera", "5-merous", x, ignore.case = T) -> x
  sub("hexamera", "6-merous", x, ignore.case = T) -> x
  sub("heptamera", "7-merous", x, ignore.case = T) -> x
  sub("octamera", "8-merous", x, ignore.case = T) -> x
  sub("nonamera", "9-merous", x, ignore.case = T) -> x
  sub("decamera", "10-merous", x, ignore.case = T) -> x
  desc0[1] <- x
  descriptions.petals[[i]] <- desc0
}


### Count

target = c("flower", "flor", "petal", "mera", "merous")
non.target = c("antepetal", "petaloid", "flowered",
               "erect", "pendent", "infloresc",
               "flowering", "florece", "bract", 
               "solitar")

trait <- extractTrait.count(descriptions.petals, target, non.target)

### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Petal_number-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Styles
#################################################

### Size

target = c("style", "estilete", "estilo")
non.target = c("ovary", "filament")
final.unit = "mm"

trait <- extractTrait.size(descriptions, target, non.target, final.unit, length.only = T)

### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Style-to_check_to_update.csv", row.names = F, na="")


#################################################
### Extract - Seeds
#################################################

### Size

target = c("seed", "semente", "semilla")
non.target = c("placent", "seeded", "seedlings")
final.unit = "mm"

trait <- extractTrait.size(descriptions, target, non.target, final.unit, length.only = F)


### Export

data.frame(checked="", species=trait$species, trait[,-1]) -> trait

write.csv(trait, "Trait_Seeds-to_check_to_update.csv", row.names = F, na="")


