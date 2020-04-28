#################
# COVID-19 research
# Citation: Goldstein ND, Tran NK. The emerging state of COVID-19 research as assessed through preprint servers. Manuscript in preparation.
# 4/10/20 -- Neal D. Goldstein and Nguyen K. Tran
#################


#### Load Packages ####

library(jsonlite)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(maps)
library(maptools)
library(sf)
library(viridis)
library(scales)


#### Original Datasets ####
#### WHO Reported COVID cases: https://covid19.who.int/
#### World Bank 2018 population density: https://data.worldbank.org/indicator/EN.POP.DNST
#### Rxiv publication: "https://connect.medrxiv.org/relate/collection_json.php?grp=181"


#### Import Datasets ####

# Publication data from Rxiv
rawdata = fromJSON("https://connect.medrxiv.org/relate/collection_json.php?grp=181")

# WHO COVID data
case = read.csv("WHO_COVID.csv")

# geocodes of publications 
load("geocoded.RData")

# Final Dataset for Mapping:
  # 1) region: country name
  # 2) n_articles: number of articles per country
  # 3) n_cases: numnber of cases per country
  # 4) long: longitude of country
  # 5) lat: latitude of country 
  # 6) pop2020: estimated population size for 2020 per country 
  # 7) area: estimated land mass 
  # 8) density: populatifon density (people/sq km)

final_data = read.csv("covid_github.csv")



#### Extract first and last author info for geocoding ####

# save dataframe
covid = rawdata$rels 

# recode dates
covid$rel_date = as.Date(covid$rel_date)

# separate first and last author names and institutions from lists 
covid$first_author_inst = lapply(covid$rel_authors, function(x) x[1,"author_inst"])
covid$first_author_name = lapply(covid$rel_authors, function(x) x[1,"author_name"])
covid$last_author_inst = lapply(covid$rel_authors, function(x) x[nrow(x),"author_inst"])
covid$last_author_name = lapply(covid$rel_authors, function(x) x[nrow(x),"author_name"])
covid$num_inst = lapply(covid$rel_authors, function(x) length(unique(x[1:nrow(x),"author_inst"])))

# replace nulls with NAs
covid$first_author_inst[sapply(covid$first_author_inst, is.null)] <- "NA"
covid$first_author_name[sapply(covid$first_author_name, is.null)] <- "NA"
covid$last_author_inst[sapply(covid$last_author_inst, is.null)] <- "NA"
covid$last_author_name[sapply(covid$last_author_name, is.null)] <- "NA"

# subset to variables of interest 
temp = covid[,c(2,7,5,13,9:12)]

# unlist 
for (i in 4:8) {
  temp[,i] = unlist(temp[,i])
}

# replace blanks cells with NA
for (i in 5:8) {
  temp[,i] = ifelse(temp[,i]=="","NA",temp[,i])
}

rm(i)

# export dataset for geocoding 
# write.csv(temp, file = "")


#### Text Mining ####

# extract titles and abstracts 
abstract <- data.frame(date = as.Date(covid$rel_date), title = as.character(covid$rel_title), abstract = as.character(covid$rel_abs))

# combine titles and abstracts
abstract$obs = paste(abstract$title, abstract$abstract, sep = " ")

# split title and abstract into regular expression, remove stop words, aggregates days into weeks
tidy_abstract <- abstract %>%
  mutate(line = row_number()) %>%
  unnest_tokens(output = "word", input = obs, token = "regex") %>%
  anti_join(stop_words) %>%
  filter(word != "covid-19") %>%
  filter(word != "covid-19.") %>%
  filter(word != "(covid-19)") %>%
  filter(word != "keywords") %>%
  filter(word != "&#34") %>%
  filter(word != "Abstract") %>%
  filter(word != "Background") %>%
  filter(word != "background") %>%
  filter(word != "Methods") %>%
  filter(word != "Results") %>%
  filter(word != "Discussion") %>%
  filter(word != "(probable") %>%
  filter(word != "abbreviated") %>%
  filter(word != "ncp") %>%
  filter(word != "time-varying") %>%
  filter(word != "10-3") %>%
  filter(word != "10-4,") %>%
  filter(word != "2019-ncov") %>%
  filter(word != "repurposable") %>%
  filter(word != "browser") %>%
  filter(word != "(excluding") %>%
  filter(word != "sars-cov-2") %>%
  filter(word != "disturbances") %>%
  filter(word != "instant") %>%
  filter(word != "-1") %>%
  filter(word != "damping") %>%
  filter(word != "march") %>%
  filter(word != "suitably,") %>%
  filter(word != "2.24") %>%
  filter(word != "3.13") %>%
  filter(word != "(u") %>%
  filter(word != "10-2)") %>%
  filter(word != "10-3)") %>%
  mutate(mo = strftime(date, "%m")) %>%
  mutate(yr = strftime(date, "%Y")) %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = F)) %>%
  arrange(date)

# combine similar regular expressions  
tidy_abstract1 = tidy_abstract[,c(1,5:8)]
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="promed" | tidy_abstract1$word == "promed." | tidy_abstract1$word == "promed)"| tidy_abstract1$word == "mail", "promed-mail", tidy_abstract1$word)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="filtering,", "filtering", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="bats)" | tidy_abstract1$word=="bats-hosts-reservoir" | tidy_abstract1$word1=="reservoir-people", "bats-hosts-reservoir-people", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="valinomycin,", "valinomycin", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="hcovs.", "hcov-host", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="(program", "program", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="(reservoir)", "reservoir", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="(reservoir)", "reservoir", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="(r0=2.3)" | tidy_abstract1$word=="(r0=2.3),", "r0=2.3", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="plvp.", "plvp", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="melatonin,", "melatonin", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="cep_c30;", "cep_c30", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="steps:", "steps", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="pandemic.", "pandemic", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="toxins,", "toxins", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="bunyaviruses", "bunyavirus", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="ionophore," |tidy_abstract1$word=="ionophores" , "ionophore", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="encephalitis.", "encephalitis", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="[90%", "90%", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="la" | tidy_abstract1$word=="crosse", "lacv", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="specification.", "specification", tidy_abstract1$word1)
tidy_abstract1$word1 = ifelse(tidy_abstract1$word=="(scenario", "scenario", tidy_abstract1$word1)


##### Get Top 10 Expressions per Week ####

# recode week to character variable 
tidy_abstract$week = as.character(tidy_abstract$week)

# Caculate tf-idf statistic 
temp = merge_covid1 %>%
  count(week, word1, sort = T) %>%
  bind_tf_idf(word1, week, n) %>%
  group_by(week) %>%
  top_n(10)
temp = temp[order(temp$week, -temp$tf_idf),]
temp1 = by(temp, temp$week, head, n=10, simplify = T)
temp1 = Reduce(rbind, temp1)

# plot the top 10 regular expressions by week 
ggplot(temp1, aes(reorder(word1, tf_idf), tf_idf, fill = week)) +
  geom_col(show.legend = F) +
  facet_wrap(~week, scales = "free") +
  coord_flip()

# export datat to determine research themes each week
# write.csv(temp1, file = "")


#### COVID Reported Cases/Deaths ####

# recode dates
case$date = as.Date(case$?..day, format = "%m/%d/%Y")

# subset relevant case data
newcase = case[,c(9,2,7)]

# reshape from long to wide 
newcase_wide = newcase %>%
  reshape(timevar = "Country", idvar = "date", direction = "wide")

# sum across rows to get total 
newcase_wide$tot_case = rowSums(newcase_wide[,c(2:214),], na.rm = T)

# reorder based on dates
newcase_wide = newcase_wide[order(as.Date(newcase_wide$date, "%Y-%m-%d")),]

# subset to relevants dates for analysis 
newcase_wide = newcase_wide[newcase_wide$date!="2020-04-19" & newcase_wide$date!="2020-04-20" & newcase_wide$date!="2020-04-21" &
                              newcase_wide$date!="2020-04-22" & newcase_wide$date!="2020-04-23" & newcase_wide$date!="2020-04-24" &
                              newcase_wide$date!="2020-04-25" & newcase_wide$date!="2020-04-26" & newcase_wide$date!="2020-04-27",c(1,215)]

# extract number of publications over time
pub = data.frame(table(covid$rel_date))
names(pub) = c("date","num_pub")
pub$date = as.Date(pub$date)

# merge reported cases with number of publications 
merge_covid = merge(newcase_wide, pub, by = "date", all = T)
merge_covid[is.na(merge_covid)] <- 0

# aggregregate by week 
merge_covid2 = aggregate(cbind(tot_case, num_pub) ~ week, merge_covid, FUN = sum)


#### Figure 1: Time Series Plot ####

# generate data frame with weekly research themes
theme = data.frame(theme1=rep(as.factor(1:10),c(2,1,1,1,1,1,3,2,2,1)),
                   start=c("2020-01-08","2020-01-13","2020-01-20","2020-01-27","2020-02-03","2020-02-10","2020-02-17","2020-02-24","2020-03-02","2020-03-09","2020-03-16","2020-03-23","2020-03-30","2020-04-06","2020-04-13"),
                   end=c("2020-01-13","2020-01-20","2020-01-27","2020-02-03","2020-02-10","2020-02-17","2020-02-24","2020-03-02","2020-03-09","2020-03-16","2020-03-23","2020-03-30","2020-04-06","2020-04-13","2020-04-18"))


# plot time series figure 
# png("PNAS_Figure_1.png",height=2*5*600,width=2*5*1300,res=1200)
ggplot(merge_covid2) +
  geom_line(aes(x=date, y=case_den, color = "#D3D3D3"), size = 1.5) +
  geom_rect(aes(xmin=as.Date(start),xmax=as.Date(end),fill=theme1), ymin = -Inf, ymax = Inf, data = theme) +
  geom_vline(aes(xintercept = as.Date(start)), data = theme, colour = "grey50", alpha = 0.5) +
  scale_fill_viridis_d(alpha = 0.2, name = "Weekly Themes",labels = c("Pathogensis","Outbreak Origin","Epidemiology","Pathophysiology","Outbreak Response","Outbreak Origin & Response","Treatment","Clinical","Public Health","Public Health and Treatment"), guide = "legend") +
  labs(fill="") +
  geom_line(aes(x=date, y=num_pub*22.91276, col = "black"), size = 0.8, alpha = 1) +
  scale_y_continuous(name = "Reported Cases per Density (people/sq km)", sec.axis = sec_axis(~./22.91276, name = "Number of Manuscripts"),labels = function(x) format(x, scientific = FALSE)) +
  scale_color_identity(name = "Weekly Trends", labels = c("Cases","Manuscripts"), guide = "legend") +
  xlab("Date") +
  geom_vline(xintercept = as.Date("2020-03-11"), linetype="longdash", size = 1.2) + 
  theme_bw() +
  theme(
    text = element_text(color = "#22211d", size = 15), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = "right",
    panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = "#f5f5f4")
  ) 

# dev.off()


#### Aggregate centroids of each manuscript to their country ####

# subset to longitude/latitude 
research_data = research_data[,c(15,16)]

# remove missing data 
research_data = na.omit(research_data)

# convert to simple feature object
point_sf = st_as_sf(research_data, coords = c("lng_analysis", "lat_analysis"), crs = 4326)

# get world map data
worldmap = maps::map("world", fill = TRUE, plot = FALSE)

# convert world to sp class
IDs = sapply(strsplit(worldmap$names, ":"), "[", 1L)
world_sp = map2SpatialPolygons(worldmap, IDs = IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))

# convert world_sp to simple feature object
world_sf = st_as_sf(world_sp)

# add country ID
world_sf = world_sf %>%
  mutate(region = map_chr(1:length(world_sp@polygons), function(i){
    world_sp@polygons[[i]]@ID
  }))

# use st_within for all points
result_all <- st_within(point_sf, world_sf, sparse = FALSE)

# count the number of articles in each country
world_sf = world_sf %>%
  mutate(n_articles = apply(result_all, 2, sum))

# remove geometry 
world_df <- world_sf
st_geometry(world_df) <- NULL

# merge with COVID case data 
newcase = newcase[newcase$date!="2020-04-19" & newcase$date!="2020-04-20" & newcase$date!="2020-04-21" &
                              newcase$date!="2020-04-22" & newcase$date!="2020-04-23" & newcase$date!="2020-04-24" &
                              newcase$date!="2020-04-25" & newcase$date!="2020-04-26" & newcase$date!="2020-04-27",]
newcase1 = aggregate(Confirmed ~ Country, newcase, FUN = sum)
final_data = merge(newcase1, world_df, by.x = "Country", by.y = "region", all = T)


#### Figure 2: Choropleth Map ####

# Get world data frame
world_data <- map_data("world")

# remove Antarctica
world_data = world_data[world_data$region!="Antarctica",]

# reocde countries with 0 manuscripts as NA
final_data$n_articles1 = ifelse(final_data$n_articles==0,NA,final_data$n_articles)

# calculate case per population density 
final_data$case_den = final_data$n_cases / final_data$density

# Create categories 
final_data$den_cat = cut(final_data$case_den, 15)

# Merge world_data and world_df
world_data2 <- world_data %>%
  left_join(final_data, by = c("region"))

# plot figure 2
p = ggplot() + 
  geom_polygon(data = world_data2, aes(x = long.x, y = lat.x, group = group, fill = den_cat), size = 0, alpha=0.9, col = "white") +
  coord_fixed(1.3) +
  theme_bw() +
  ylim(-60,85) +
  theme(
    text = element_text(color = "#22211d", size = 15), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = "right",
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y=element_blank(),axis.title.x=element_blank(),
    axis.ticks = element_blank(),
    legend.key = element_rect(fill = "#f5f5f4")
  ) 

p = p +  scale_fill_manual(values = c("#440154FF","#453781FF", "#33638DFF","#238A8DFF","#29AF7FFF"),
                           name = "Report Cases per Density \n (people/sq km)", 
                           labels = c("0 - 1,320","1,321 - 2,640", "3,960 - 5,280", "7,920 - 9,240", "18,500 - 19,800")) 

# png("PNAS_Figure_2.png",height=2*5*600,width=2*5*1300,res=1200)
p + geom_point(data = data, aes(x = long, y = lat, size = n_articles1), color = alpha("#FDE725FF", 0.6)) +
  scale_size_continuous(name="Number of Manuscripts",
                        labels = c("0 - 100","101 - 200", "201 - 300", "401 - 500", "500 - 600"),
                        breaks = c(100,200,300,400,500))
# dev.off()
