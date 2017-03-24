occupation <- read.csv("../Data/occupation.csv")
occupation$State <- as.character(occupation$Area.name)
occupation$Territory <- as.logical(gsub(".* Territory", "TRUE", occupation$State))
occupation$Area.name <- gsub(" Territory", "", occupation$Area.name)
occupation$Area.name <- gsub(" Department", "", occupation$Area.name)
occupation$Area.name <- gsub(" Terr.", "", occupation$Area.name)


ages <- read.csv("../Data/ages-ipums.csv")
ages <- ages %>% mutate(
  STATEICP = replace(STATEICP, STATEICP == "Louisianna", "Louisiana"),
  STATEICP = replace(as.character(STATEICP), STATEICP %in% c("South Dakota", "North Dakota"), "Dakota")
)
ages <- ages %>% group_by(STATEICP) %>% summarize(total=sum(total), above10 = sum(above10))

ages.sex <- read.csv("../Data/ages-sex-ipums.csv")
ages.sex <- ages.sex %>% mutate(
  STATEICP = replace(STATEICP, STATEICP == "Louisianna", "Louisiana"),
  STATEICP = replace(as.character(STATEICP), STATEICP %in% c("South Dakota", "North Dakota"), "Dakota")
) %>% group_by(STATEICP, SEX) %>% summarize(above10=sum(above10))
ages.sex <- ages.sex %>%
  spread(key=SEX, value=above10)
names(ages.sex)[2:3] <- c("above10.Female", "above10.Male")

# all good!
# anti_join(occupation, ages, by=c("Area.name"="STATEICP")) %>% count(State)
occupation2 <- left_join(occupation, ages, by=c("Area.name"="STATEICP"))
occupation2$Total.Above10.Est <- with(occupation2, above10/total*Total.Population)
occupation3 <- left_join(occupation2, ages.sex, by=c("Area.name"="STATEICP"))
occupation3 <- occupation3 %>% mutate(
  Total.Above10.Female = above10.Female/above10*Total.Above10.Est,
  Total.Above10.Male = above10.Male/above10*Total.Above10.Est,
  Employed.Female = Agriculture.Female+Manufacturing.Female+Trade.Female+Service.Female+School.Female,
  Employed.Male = Agriculture.Male+Manufacturing.Male+Trade.Male+Service.Male+School.Male,
  Unaccounted.Female = Total.Above10.Female - Employed.Female,
  Unaccounted.Male = pmax(Total.Above10.Male - Employed.Male,0)
)

occ3 <- occupation3 %>% gather(key="Occupation.Sex", value="Number", c(2:11,24:25)) %>% separate(Occupation.Sex, into=c("Occupation", "Sex"))
occ3 <- occ3 %>% mutate(
  Occupation = factor(Occupation, levels= c("Agriculture", "Manufacturing",
                                            "Trade", "Service", "School", "Unaccounted")),
  Sex = factor(Sex, levels=c("Male", "Female"))
)
write.csv(occ3, file="../Data/occ3.csv", row.names=FALSE)
