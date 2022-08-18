# Plotting Functions for Soil and vegetation initialization
# Requires tidyverse package, including stringr
library(tidyverse)

#####################################################
### Create column class vector for load functions ###
### This function ...                             ###
### (1) restrits what columns to load             ###
### (2) defines the class to load each column as  ###
#####################################################

load_id = function(File, Varlist = c("year")){
  
  hdr = readLines(File,n=1) %>%
    stringr::str_split(.,pattern=" ") %>%
    .[[1]]
  
  if(!is.na(list(Varlist))){
    id = rep("NULL",times = length(hdr))
    idx = match(Varlist,hdr)
    id[idx] = "numeric" # year soilc, soiln, plantc, plantn, overstory_leafc overstory_height
    return(id)
  } else {
    print(hdr)
    return(hdr)
  }
}

# Load RHESSys output file
load_rhessys = function(File, Load_id){
  dtbl = read.table(File, header=T, colClasses = Load_id) %>% as_tibble()
  return(dtbl)
}

####################################
### Uniform Aggregation function ###
### will compute either the sum  ###
### or mean of all variable      ###
### columns                      ###
####################################

agg_rhessys = function(Data, Grouplist = c("syr"), Agglist, Method = "mean", Scale = "yearly"){
  data1 = Data
  
  data1 = switch(Scale,
         "daily" = data1 %>%
           mutate(sday = 1:nrow(.),
                  syr = floor(sday/365.25) + 1,
                  sdec = floor(syr/10) + 1
                  ),
         "yearly" = data1 %>%
           mutate(syr = 1:nrow(.),
                  sdec = floor(syr/10) + 1
                  )
           )
  
  keeplist = c(Grouplist,Agglist)
  hdr = colnames(data1)
  
  if(!all(keeplist %in% hdr)){ # check if all Grouplist are available
    print("Missing the following data column(s) in the dataset:")
    print(keeplist[!(keeplist %in% hdr)])
    stopifnot(all(keeplist %in% hdr))
  }
  
  data1 = data1 %>%
    dplyr::select(all_of(keeplist)) %>%
    group_by_at(all_of(Grouplist)) %>%
    summarise_all(.,
                  .funs = function(x){
                    y = switch(Method,
                               mean = mean(x,na.rm = T),
                               sum = sum(x,na.rm = T))
                    return(y)
                    })
  return(data1)
}


##############################  
### Single plot function   ###
### formats to long format ###
### if in wide             ###
##############################

plot_single = function(Data,Xaxis = "syr", Varlist, Color = "var", Facet = NA, 
                       Type = "line", Correction = T, Format_long = T,
                       Legend = "bottom", Scales = "fixed", Ncol = NA, Linetype = NA){
  
  data1 = Data
  avail = colnames(data1)
  
  if(Format_long == T){
    
    keeplist = c(Xaxis,Varlist)
    keeplist = keeplist[!is.na(keeplist)]
    
    if(Xaxis == "var"){
      rm = match(c("var"),keeplist)
      keeplist = keeplist[-(rm)]
    }
    
    # Stop function if Xaxis, Varlist, Facet or Color request does not exist in dataset
    if(!all(keeplist %in% avail)){ # check if all Grouplist are available
      print("Error 1: Missing the following data column(s) in the dataset:")
      print(keeplist[!(keeplist %in% avail)])
      print(head(data1))
      stopifnot(all(keeplist %in% avail))
    }
    
    data1 = data1 %>%
      pivot_longer(cols = all_of(Varlist), names_to = "var", values_to = "value")
    
  }
  
  keeplist2 = c(Xaxis,Color,Facet,Linetype,"var","value")
  keeplist2 = keeplist2[!(duplicated(keeplist2))]
  keeplist2 = keeplist2[!is.na(keeplist2)]
  
  avail = colnames(data1)
  
  # Stop function if var and value columns don't exist
  if(!all(keeplist2 %in% avail)){ # check if all Grouplist are available
    print("Error 2: Missing the following data column(s) in the dataset:")
    print(keeplist2[!(keeplist2 %in% avail)])
    print(head(data1))
    stopifnot(all(keeplist2 %in% avail))
  }
  
  # Stop function if all Varlist not available in var column
  avail = data1$var %>% unique()
  if(!all(Varlist %in% avail)){ # check if all Grouplist are available
    print("Error 3: Missing the following Variables in the dataset column var:")
    print(Varlist[!(Varlist %in% avail)])
    print(head(data1))
    stopifnot(all(Varlist %in% avail))
  }
  
  data1 = data1 %>% 
    dplyr::select(all_of(keeplist2)) %>%
    dplyr::filter(var %in% Varlist) %>%
    mutate(var = factor(var,Varlist))
    
  # Remove last day, month, year, or decade of data (when sims stop before end of time cycle)
  if(Correction == T & is.numeric(data1[[Xaxis]])){
    max0 = max(data1[[Xaxis]],na.rm = T)
    data1 = data1[(data1[[Xaxis]] < max0),]
  }
  
  # Group by color if exists
  if(!is.na(Color)){
    #plot0 = ggplot(data1, mapping = aes_string(group = Color, color = Color, fill = Color))
    plot0 = ggplot(data1, mapping = aes_string(color = Color, fill = Color, linetype = "tag2"))
  } else{
    plot0 = ggplot(data1)
  }

  # Facet if requested
  if(!is.na(list(Facet))){
    if(is.na(Facet[2])){
      Facet[2] = Facet[1]
      Facet[1] = "."
    }
    if(!is.na(Ncol)){
      plot0 = plot0 + facet_wrap(as.formula(paste(Facet[1],"~", Facet[2])), scales = Scales, ncol = Ncol)
    } else {
      plot0 = plot0 + facet_wrap(as.formula(paste(Facet[1],"~", Facet[2])), scales = Scales)
    }
    
  }
  
  plot0 = plot0 + theme(legend.position = Legend)
  
  plot0 = switch(Type,
                 "line" = plot0 + geom_line(mapping = aes_string(x=Xaxis, y="value")),
                 "bar" = plot0 + 
                   geom_bar(position = "dodge", stat = "identity", mapping = aes_string(x=Xaxis, y="value"))
                )
  
  return(plot0)
}
            