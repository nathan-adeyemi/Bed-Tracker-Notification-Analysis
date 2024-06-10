
siteInfo <- readRDS(file = file.path("..","Policy_Interventions_to_Improve_Mental_Healthcare_Access","simulations","function_requirements","ip_facilities_info.rds"))

# Create function and read in files for matching facility name strings
str_fuzzy_match <-
  function(string,
           df = aliases,
           names_col = 'name',
           alias_col = 'aliases', 
           return_dist = F) {
    if(return_dist){
      return(min(sapply()))
    }else{
      return(unlist(df[, ..names_col])[unlist(lapply(lapply(
      map2(.x = string, .y = df[, ..alias_col], .f = stringdist), which.min
    ), function(i)
      return(ifelse(
        is_empty(i), NA_integer_, i
      ))))])
    }
  }

all_names <- readxl::read_excel(path = file.path("Data","HCCIS","hosplist.xlsx"),
                        sheet = "Unique Facility Aliases") %>%
  apply(2, FUN = function(x) {
    as.character(na.omit(x)) %>%
      {
        function(i) i[i != "0"]
      }() %>%
      unique() %>%
      tolower()
  }) %>%
  names()

aliases <-data.table(readxl::read_excel(
  path  = file.path("Data", "HCCIS", "hosplist.xlsx"),
  sheet = "Unique Facility Aliases"
))
aliases <- rbindlist(lapply(X = colnames(aliases),
                            FUN = function(col){
                       unique_names <- unlist(na.omit(unique(aliases[,..col])))
                       return(data.table(name = rep(col, length(unique_names)), aliases = unique_names))
                     }), 
                     fill = TRUE)

# Read, combine and format emails --------------------
email_names = list.files('Data/individual_notifications')
names_logical =  sapply(email_names,FUN = function(n) grepl("[[:alpha:]]", substr(n,1,1)))
email_names = email_names[names_logical]

data = rbindlist(lapply(X = email_names, FUN = function(path) data.table(readxl::read_xlsx(file.path('Data','individual_notifications',path),skip = 1))))
colnames(data) = gsub(" ","_",colnames(data))
data = data[,`:=`(date_time = as_datetime(Updated,format = "%b %d %Y %I:%M%p",tz = 'America/Chicago'),
                Facility_Name = str_fuzzy_match(string = Facility_Name),
                Service_For = gsub("[^[:alpha:]]",'',Service_For),
                Updated = NULL)]


data = data[siteInfo, `:=`(Bed_Group = Bed_Group), on = c(Facility_Name = 'Facility_name', Service_For = 'Age')
              ][siteInfo[,.SD[1],by = Bed_Group],`:=`(total_beds = total_beds,
                                                      fac_num = Site),on = c(Facility_Name = 'Facility_name')
                ][,.SD[!duplicated(.SD)], by = .(Facility_Name,date_time,Available_Capacity,Bed_Group)
                  ][,list(Facility_Name, Bed_Group, Available_Capacity, date_time,total_beds,fac_num)]

saveRDS(object = data, file = file.path('Data','combined_notifications.rds'))
