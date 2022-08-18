# Check growth basin daily output
# aggregates to yearly average | sum
# assigns sim year values
# Jonathan Gendron 08/11/2022
library(tidyverse)
source(file = normalizePath("C:/Ubuntu/rhessys/dev/scripts/functions_rhessys_analysis1.R"))

flist = grep(pattern = "*grow_basin.daily",list.files("./"),value = T)
flist1 = grep("cold",flist,value = T)
flist2 = grep("warm",flist,value = T) %>% sort(decreasing = T)
flist = c(flist1,flist2)

n = length(flist)
vlist = c("n_dep","n_fix","uptake","denitrif","nitrif",
          "streamflow_NO3","streamflow_NH4","streamflow_DON",
          "gwNO3out","gwNH4out","gwDONout")

tagtmp = flist %>% strsplit(.,split = "_")
tag1 = lapply(tagtmp,function(v1){v1[1]}) %>% unlist() # simname
tag2 = lapply(tagtmp,function(v1){v1[3]}) %>% unlist() # ndep
tag2 = lapply(tag2,function(v1){v1 %>% str_split(.,"ndep") %>% .[[1]] %>% .[2]}) %>% 
  unlist() %>% as.numeric()

datlist = list() # yearly fluxes
#datlist2 = list() # monthly fluxes 

for(i in 1:n){
  print(i)
  
  file0 = flist[i]
  id = load_id(file0,vlist)
  #id2 = load_id(file0,c("month",vlist))
  
  datlist[[i]] = load_rhessys(file0, Load_id = id) %>%
    agg_rhessys(., Grouplist = c("syr"), Agglist = vlist, Method = "sum", Scale = "daily")
  
  datlist[[i]] = datlist[[i]] %>%
    mutate(tag1 = tag1[i],
           tag2 = tag2[i]
    )
  
  #datlist2[[i]] = load_rhessys(file0, Load_id = id2) %>%
  #  agg_rhessys(., Grouplist = c("month"), Agglist = vlist, Method = "sum", Scale = "daily")
  
  #datlist2[[i]] = datlist2[[i]] %>%
  #  mutate(tag1 = tag1[i],
  #         tag2 = tag2[i]
  #  )
  
  # # veginit simulations add the last year of soilinit
  # if(i == 1){
  #   maxyr = max(datlist[[i]]$syr,na.rm = T)
  # } else {
  #   datlist[[i]]$syr = datlist[[i]]$syr + maxyr
  # }
  
}

## Plotting annual fluxes

dat1 = datlist %>% bind_rows()
dat1$tag1 = dat1$tag1 %>% factor()
#dat1$tag2 = dat1$tag2 %>% factor(., levels = tag2list)
dat1$tag2 = dat1$tag2 %>% factor()

vlist2 = vlist[c(1,4,6,7:11)]

#plot_single(dat1, Varlist = vlist2, Facet = c("var","tag1"), Color = "tag2", Scales = "free_y",Ncol = 2, Correction = T)
plot_single(dat1, Varlist = vlist2, Facet = c("var","tag1"), Color = "tag2", Scales = "fixed",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Ndep\n(kg-N/m2/yr)"
  )

plot_single(dat1, Varlist = vlist2, Facet = c("tag2","tag1"), Color = "var", Scales = "free_y",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Variable\n(kg-N/m2/yr)"
  )
            

## Custom Monthly total plot ##

# dat2 = datlist2 %>% bind_rows()
# dat2$tag1 = dat2$tag1 %>% factor()
# dat2$tag2 = dat2$tag2 %>% factor(., levels = c(
#   #"ndep0.025","ndep0.01","ndep0.005","ndep0.0025"
#   "ndep0.001"
# ))
# 
# plot_single(dat2, Xaxis = "month",Varlist = "denitrif", Color = "tag1", Type = "bar")
