## This routine produces an export file of program rule variables (PRV) based on all
## data elements in a given DHIS2 program.

## Outputs a JSON file and CSV file of PRV types
## CSV can be manually modified to change e.g. PRV name

## By default all PRV types are produced except calculated values.
## PRV names are the DE Shortnames in camelcase with a suffix for the PRV type

## PRV objects also have a property for the program stage where the DE is found
## Data Elements used in multiple stages are not exported

## Program UID entered below or as an argument

program_id<-"lxAQ7Zs9VYR"


####Load required packages
packages<-c("dplyr","tibble", "purrr", "jsonlite","readr","R.utils", 
            "stringr","assertthat","httr","tidyr")
install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

install_or_load_pack(packages)

# to make program id an argument
args<-commandArgs(TRUE)
print(args)

if( !is.null(args)){
  program_id<-args[1]
}

# Extract login info from directory
if (!file.exists("auth.json")){
stop("Please add auth.json to directory") 
} else {
  baseurl<-chuck(fromJSON("auth.json"), "dhis","baseurl")
  username<-chuck(fromJSON("auth.json"), "dhis","username")
}

##test login
loginDHIS2<-function(baseurl,username,password){
  url<-paste0(baseurl,"api/me")
  r<-GET(url,authenticate(username,password))
  assert_that(r$status_code == 200L)}

if(loginDHIS2(baseurl,username,
              chuck(fromJSON("auth.json"), "dhis","password"))){
  print("successfully logged in")
}else{
  stop("could not log in! Please check url, username and password in auth.json")
}



#get program stage data elements
url<-paste0(baseurl,"api/programStages?filter=program.id:eq:",
          program_id,
          "&fields=id,name,programStageDataElements[dataElement[id]")

ps_de<-fromJSON(content(GET(url),type="text", encoding="UTF-8"))

# convert into a table
de<-pluck(ps_de, "programStages","programStageDataElements")
de_list<-pluck(ps_de,"programStages") %>% 
  as_tibble() %>% 
  unnest(cols=c(programStageDataElements)) %>% 
  select("ps_name"=name,"ps_id"=id,"de_id"=dataElement) %>% 
  unnest(cols=c(de_id))
  
# add names to data element UIDs along with a base for PRV name
# infer PRV type based on DE value type
url<-paste0(baseurl,"api/dataElements?paging=false&",
                    "domainType=TRACKER&fields=id,shortName,name,code,valueType")
de_names<-fromJSON(content(GET(url),type="text", encoding="UTF-8")) %>% 
  pluck(1) %>% as_tibble() %>% 
  mutate(prv_base=str_remove_all(str_to_title(shortName), "[^[:alnum:]]")) %>% 
  mutate(prv_base=str_trunc(prv_base, width=220)) %>% 
  mutate(prv_length=str_length(prv_base)) %>% # User may want to shorten PRV names...
  select("de_name"=name,everything())
  

# define PRV types
# COMMENT OUT PRV TYPE OR CHANGE PRV SUFFIX AS NEEDED
prv_types<-
  tribble(
    ~Name,    ~Type,
    "latest", "DATAELEMENT_NEWEST_EVENT_PROGRAM",
    "latest_ps", "DATAELEMENT_NEWEST_EVENT_PROGRAM_STAGE",
    "curr", "DATAELEMENT_CURRENT_EVENT",
    "prev", "DATAELEMENT_PREVIOUS_EVENT"
  ) %>% mutate("join"=1)


# create a df of PRV of each type based on the DE in the program
prv_df<-de_names %>% mutate("join"=1) %>% 
  inner_join(de_list, by="id") %>% 
  left_join(prv_types,by=c("join"), relationship="many-to-many") %>% 
  # remove DE that appear in more than one PS
  add_count(id) %>% filter(n<5)
  

csv_name<-str_c("programRuleVariables_by_DE_",program_id,".csv")

### COMMENT OUT THIS LINE IF YOU MAKE MANUAL CHANGES TO CSV FILE BEFORE IMPORT
prv_df %>% write_csv(csv_name)

print("CSV of PRV outputted")

# reconvert the csv back into df
prv_df<-read_csv(csv_name)

# convert into json format
prv_exp<-prv_df %>% 
  mutate(name=str_c(prv_base,"_",Name)) %>% 
  select(name,"de_id"=id,ps_id,"programRuleVariableSourceType"=Type) %>% 
  mutate(useCodeForOptionSet=TRUE) %>% 
  mutate(id=program_id) %>% 
  nest(program=id) %>% 
  rename(id=ps_id) %>% 
  nest(programStage=id) %>% 
  rename(id=de_id) %>% 
  nest(dataElement=id) %>% 
  mutate(program = purrr::map(program, as.list)) %>% 
  mutate(programStage = purrr::map(programStage, as.list)) %>% 
  mutate(dataElement = purrr::map(dataElement, as.list))
  
  
# Put it all under a list
exp<-list("programRuleVariables"=prv_exp)

# title the export file
exp_name<-str_c("autoPRV_",program_id,"_IMPORT_",as.character(Sys.Date()),".json")


#output json file
exp %>% toJSON(auto_unbox = TRUE,pretty=TRUE) %>% 
  write_lines(exp_name)

print("Success: PRV metadata file in folder")

#TEST DIRECT IMPORT
# r<-httr::POST(paste0(baseurl,"api/metadata.json"), 
#               body=upload_file(path="test_export4.json"),content_type_json())
# content(r) %>% View

