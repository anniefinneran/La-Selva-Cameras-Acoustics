##This script is for analyzing camera trap data from RET project
# Author: Annie Finneran
# Start Date: July 5th, 2023
# Last Edited: July 5th, 2023
################################################################################################################################################


#bring in data from wildlife insights and prep for analysis
#camera locations and names
deployments_raw <- read.csv("Data/CT_data/deployments.csv")
#subset to what we want
deployments <- deployments_raw[,c(1,2)]

#image data
images_raw <- read.csv("Data/CT_data/images.csv")
#subset to what we want 
images <- images_raw[,c(2,4,9,10,11,12,13,14,16)]

#bring in data about sites 
site_info_raw <- read.csv("Data/Site_Data.csv")
#subset to what we want 
site_info <- site_info_raw[,c(3,4,7,8,9,10,11)]

#combine variables into single dataframe for analysis 
camera_df_prep <- merge(deployments, images, by="deployment_id")
camera_df <- merge(camera_df_prep, site_info, by="deployment_id")



##STATISTICAL TESTS 

# # OF DIFFERENT SPECIES#####
# applying aggregate function
sites_species_summary <- aggregate(species~deployment_id,camera_df, function(x) length(unique(x)))
summary_with_land_data <- merge(sites_species_summary,site_info,by="deployment_id")
# stat test for number of unique species 
t.test(species ~ Forest_Type, data = summary_with_land_data, var.equal = TRUE)
#significant at p=0.1, with higher mean in old growth 
#look if sampling group mattered
t.test(species ~ group, data = summary_with_land_data, var.equal = TRUE)
#hmmm similar result as above, so need to find a way to control for this in the future

###### of IMAGEs######
sites_images_summary <- aggregate(filename~deployment_id,camera_df, function(x) length(unique(x)))
images_summary_with_land_data <- merge(sites_images_summary,site_info,by="deployment_id")
t.test(filename ~ Forest_Type, data = images_summary_with_land_data , var.equal = TRUE)
#significant at the 0.01 level, with a lot more images seen in the old growth forests
#look if sampling group mattered
t.test(filename ~ group, data = images_summary_with_land_data, var.equal = TRUE)
#not significant woohooo, this makes me feel better

#CARNIOVRES
carnviores <- subset(camera_df, order=="Carnivora")

#of species 
carnviores_summary <- aggregate(species~deployment_id,carnviores, function(x) length(unique(x)))
carnivores_summary_with_land_data <- merge(carnviores_summary,site_info,by="deployment_id")
# stat test for number of unique species 
t.test(species ~ Forest_Type, data = carnivores_summary_with_land_data, var.equal = TRUE)
#not significant
#by group
t.test(species ~ group, data = carnivores_summary_with_land_data, var.equal = TRUE)
#not significant

#of images
c_images_summary <- aggregate(filename~deployment_id,carnviores, function(x) length(unique(x)))
Cimages_summary_with_land_data <- merge(c_images_summary,site_info,by="deployment_id")
t.test(filename ~ Forest_Type, data = Cimages_summary_with_land_data , var.equal = TRUE)
#not significant 
#by group
t.test(filename ~ group, data = Cimages_summary_with_land_data , var.equal = TRUE)
#signifcant at the 0.1 level, with more in group 2

###CATS!###
cats <- subset(camera_df, family=="Felidae")
#of species 
cats_summary <- aggregate(species~deployment_id,cats, function(x) length(unique(x)))
cats_summary_with_land_data <- merge(cats_summary,site_info,by="deployment_id")
# stat test for number of unique species 
t.test(species ~ Forest_Type, data = cats_summary_with_land_data, var.equal = TRUE)
#not significant
#by group
t.test(species ~ group, data = cats_summary_with_land_data, var.equal = TRUE)
#signficant at the 0.05 level with more in group 1

#of images
cats_images_summary <- aggregate(filename~deployment_id,cats, function(x) length(unique(x)))
Catsimages_summary_with_land_data <- merge(cats_images_summary,site_info,by="deployment_id")
t.test(filename ~ Forest_Type, data = Catsimages_summary_with_land_data , var.equal = TRUE)
#not significant 
#by group
t.test(filename ~ group, data = Catsimages_summary_with_land_data , var.equal = TRUE)
#not significant



##PECCARIES####
peccary <- subset(camera_df, genus=="Pecari" | genus=="Tayassu")
#of images
peccary_images_summary <- aggregate(filename~deployment_id,peccary, function(x) length(unique(x)))
peccary_images_summary_with_land_data <- merge(peccary_images_summary,site_info,by="deployment_id")
t.test(filename ~ Forest_Type, data = peccary_images_summary_with_land_data , var.equal = TRUE)
#signficant at p=0.05, with more in the old growth forest
#by group
t.test(filename ~ group, data = peccary_images_summary_with_land_data , var.equal = TRUE)
#not significant


###Food Web Visualizations
library(igraph)

# create data:
predator_prey_raw <- read.csv("Data/Predator_Prey.csv",sep=",")
#remove rownames 
predator_prey <- predator_prey_raw[,-1]
predator_prey <- data.matrix(predator_prey)
rownames(predator_prey) <- predator_prey_raw$X
predator_prey

#create network
network <- graph_from_adjacency_matrix(predator_prey)

plot(network, layout=layout.sphere)
