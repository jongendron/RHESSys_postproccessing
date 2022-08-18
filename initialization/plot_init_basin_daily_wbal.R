# Check growth basin daily output
# aggregates to yearly average | sum
# assigns sim year values
# Jonathan Gendron 08/11/2022
library(tidyverse)
source(file = normalizePath("C:/Ubuntu/rhessys/dev/scripts/functions_rhessys_analysis1.R"))

flist = grep(pattern = "*basin.daily",list.files("./"),value = T) %>% .[-grep("grow",.)]
flist1 = grep("cold",flist,value = T) %>% sort(decreasing = T)
flist2 = grep("warm",flist,value = T) %>% sort(decreasing = T)
flist = c(flist1,flist2)

n = length(flist)
vlist = c("precip","streamflow","return","baseflow","evap","trans","gw.Qout",
          "rz_storage","unsat_stor","gw.storage",
          "detention_store","litter_store","canopy_store"
)
vlist2 = c("precip","streamflow","et","gw.Qout","dst","err")

tagtmp = flist %>% strsplit(.,split = "_")
tag1 = lapply(tagtmp,function(v1){v1[1]}) %>% unlist() # simname
tag2 = lapply(tagtmp,function(v1){v1[3]}) %>% unlist() # ndep is usually 3rd (sometimes 4th)
tag2 = lapply(tag2,function(v1){v1 %>% str_split(.,"ndep") %>% .[[1]] %>% .[2]}) %>% 
  unlist() %>% as.numeric()

datlist = list() # yearly fluxes

for(i in 1:n){
  print(i)
  
  file0 = flist[i]
  id = load_id(file0,vlist)
  
  datlist[[i]] = load_rhessys(file0, Load_id = id) %>%
    mutate(
      streamflow = streamflow + return + baseflow,
      et = evap + trans,
      detention_store = c(NA,diff(detention_store)),
           canopy_store = c(NA,diff(canopy_store)),
           litter_store = c(NA,diff(litter_store)),
           rz_storage = c(NA,diff(rz_storage)),
           unsat_stor = c(NA,diff(unsat_stor)),
           gw.storage = c(NA,diff(gw.storage)),
           dst = detention_store + canopy_store + litter_store + rz_storage + unsat_stor + gw.storage,
           err = precip - (streamflow + et + gw.Qout + dst)
           )
  
  datlist[[i]] = datlist[[i]] %>%
    agg_rhessys(., Grouplist = c("syr"), Agglist = vlist2, Method = "sum", Scale = "daily") %>%
    mutate(tag1 = tag1[i],tag2 = tag2[i]) %>%
    dplyr::select(tag1,tag2,syr,everything())
  
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
            
## Custom Decadal plot ##

dat2 = dat1
dat2$sdec = floor(dat2$syr/10) + 1
dat2 = dat2 %>% dplyr::select(-syr) %>% group_by(tag1,tag2,sdec) %>% summarise_all(sum,na.rm = T)

plot_single(dat2, Xaxis = "sdec",Varlist = vlist2, Facet = c("tag2","tag1"), Color = "var", Scales = "free_y",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Decade",
    y = NULL,
    color = "Variable\n(kg-N/m2/yr)"
  )

## Custom bar plot ##

dat3 = dat1 %>% ungroup() %>%
  dplyr::select(-syr) %>%
  group_by(tag1,tag2) %>%
  summarise_all(mean,na.rm = T)


plot_single(dat3,Xaxis="var",Varlist = vlist2, Color = "tag2", Scale = "fixed", Legend = "right",Type = "bar") +
  labs(
    x = NULL,
    y = "Flux of Water (mm/yr)",
    fill = "Ndep\n(kg-N/m2/yr)",
    color = "Ndep\n(kg-N/m2/yr)"
  )


ggplot(dat3) +
  geom_bar(stat = "identity", position = "dodge", mapping = aes(x=))