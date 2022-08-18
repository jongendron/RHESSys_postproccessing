# Check growth basin daily output
# aggregates to yearly average | sum
# assigns sim year values
# Jonathan Gendron 08/11/2022
library(tidyverse)
source(file = normalizePath("C:/Ubuntu/rhessys/dev/scripts/functions_rhessys_analysis1.R"))

flist = grep(pattern = "*grow_patch.yearly",list.files("./"),value = T)
flist1 = grep("cold",flist,value = T) %>% sort(decreasing = T)
flist2 = grep("warm",flist,value = T) %>% sort(decreasing = T)
flist = c(flist1,flist2)

n = length(flist)
vlist = load_id(flist[1],NA)[-c(1:5)]

tagtmp = flist %>% strsplit(.,split = "_")
tag1 = lapply(tagtmp,function(v1){v1[1:2] %>% paste(.,collapse="_")}) %>% unlist() # simname
tag2 = lapply(tagtmp,function(v1){v1[3]}) %>% unlist() # ndep
tag2 = lapply(tag2,function(v1){v1 %>% str_split(.,"ndep") %>% .[[1]] %>% .[2]}) %>% 
  unlist() %>% as.numeric()

datlist = list() # yearly canopy stores

for(i in 1:n){
  print(i)
  
  file0 = flist[i]
  id = load_id(file0,vlist)
  
  datlist[[i]] = load_rhessys(file0, Load_id = id) %>%
    agg_rhessys(., Grouplist = c("syr"), Agglist = vlist, Method = "mean", Scale = "yearly")
  
  datlist[[i]] = datlist[[i]] %>%
    mutate(tag1 = tag1[i],
           tag2 = tag2[i]
    )
  
#  veginit simulations add the last year of soilinit
  if(i == 1){
    maxyr = max(datlist[[i]]$syr,na.rm = T)
  } else {
    datlist[[i]]$syr = datlist[[i]]$syr + maxyr
  }
  
}



## Plotting annual fluxes

dat1 = datlist %>% bind_rows()
dat1$tag1 = dat1$tag1 %>% factor()
dat1$tag2 = as.character(dat1$tag2)
#dat1$tag2 = factor(dat1$tag2)
#dat1$tag2 = dat1$tag2 %>% factor(., levels = tag2list)

vlist2 = c("plant_c","plant_n","soil_c","soil_n","nitrate","sminn")

plot_single(dat1, Varlist = vlist2, Facet = c("var"), Color = "tag1", Linetype = "tag2",Scales = "free_y",Ncol = 2,Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Ndep\n(kg-N/m2/yr)",
    linetype = "ndep\n(kgN/m2/yr)"
    )
  

