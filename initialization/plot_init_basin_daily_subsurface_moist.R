# Check growth basin daily output
# aggregates to yearly average | sum
# assigns sim year values
# Jonathan Gendron 08/11/2022
library(tidyverse)
source(file = normalizePath("C:/Ubuntu/rhessys/dev/scripts/functions_rhessys_analysis1.R"))

flist = grep(pattern = "*basin.daily",list.files("./"),value = T) %>% .[-grep("grow",.)]
flist1 = grep("unscaled",flist,value = T) %>% sort(decreasing = T)
id1 = 1:length(flist1)
flist2 = grep("cold_ndep",flist,value = T) %>% sort(decreasing = T)
id2 = 1:length(flist2) + length(id1)
flist = c(flist1,flist2)

n = length(flist)
vlist = c("rz_storage","unsat_stor","gw.storage","rootdepth","sat_def","sat_def_z")
vlist2 = c("rz_storage","unsat_stor","vadose_store","gw.storage","rootdepth","sat_def","sat_def_z")
vlist3 = c("rz_storage","unsat_stor","vadose_store","gw.storage")

# Tags for the data
tagtmp = flist %>% strsplit(.,split = "_")
tag1 = c(
  lapply(tagtmp[id1],function(v1){v1[1:3] %>% paste(.,collapse="_")}) %>% unlist(),
  lapply(tagtmp[id2],function(v1){v1[1:2] %>% paste(.,collapse="_")}) %>% unlist()
)
tag2 = c(
  lapply(tagtmp[id1],function(v1){v1[4]}) %>% unlist(),
  lapply(tagtmp[id2],function(v1){v1[3]}) %>% unlist()
) %>%
  lapply(.,function(v1){v1 %>% str_split(.,"ndep") %>% .[[1]] %>% .[2]}) %>% 
  unlist() %>% as.numeric()

datlist = list() # yearly fluxes

for(i in 1:n){
  print(i)
  
  file0 = flist[i]
  id = load_id(file0,vlist)
  
  datlist[[i]] = load_rhessys(file0, Load_id = id) %>%
    mutate(
      vadose_store = rz_storage + unsat_stor
           )
  
  datlist[[i]] = datlist[[i]] %>%
    agg_rhessys(., Grouplist = c("syr"), Agglist = vlist2, Method = "mean", Scale = "daily") %>%
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

plot_single(dat1, Varlist = vlist3, Facet = c("var","tag1"), Color = "tag2", Scales = "fixed",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Ndep\n(kg-N/m2/yr)"
  )

plot_single(dat1, Varlist = c("rootdepth","sat_def_z","sat_def"), Facet = c("var","tag1"), Color = "tag2", Scales = "fixed",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Ndep\n(kg-N/m2/yr)"
  )

dat1$tag2 = as.character(dat1$tag2)
plot_single(dat1, Varlist = vlist3, Facet = c("var"), Color = "tag1", Scales = "fixed",Ncol = 2, Correction = T,
            Legend = "right") +
  labs(
    x = "Simulation Year",
    y = NULL,
    color = "Ndep\n(kg-N/m2/yr)"
  )


dat2 = dat1 %>%
  dplyr::filter(tag2 == "0.1",syr > 25) %>%
  dplyr::select(tag1,tag2,syr,rz_storage,unsat_stor,vadose_store,gw.storage,rootdepth,sat_def_z) %>%
  pivot_longer(cols = rz_storage:sat_def_z, names_to = "var") %>%
  mutate(
    var = factor(var,levels = c("rz_storage","unsat_stor","gw.storage","vadose_store","rootdepth","sat_def_z"))
  )

ggplot(dat2) + geom_line(mapping = aes(x=syr,y=value,color=tag1)) + facet_wrap(~var,scales = "free_y")

dat3 = dat1 %>%
  dplyr::filter(tag2 == "0.1", syr <= 1000) %>% #, tag1 %in% c("cal_cold","cal_cold_unscaled")) %>%
  dplyr::select(tag1,tag2,syr,rz_storage,unsat_stor,vadose_store,gw.storage,rootdepth,sat_def_z) %>%
  pivot_longer(cols = rz_storage:sat_def_z, names_to = "var") %>%
  mutate(
    var = factor(var,levels = c("rz_storage","unsat_stor","gw.storage","vadose_store","rootdepth","sat_def_z"))
  )

ggplot(dat3) + geom_line(mapping = aes(x=syr,y=value,color=var)) + facet_wrap(~tag1,scales = "fixed")


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