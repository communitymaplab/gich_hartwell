#Script for loading/updating data from Ona

library(sf)
library(ona)
library(tidyverse)

parcel_points_data <-as.tibble(onaDownload("Hartwell_Hsurvey_v1","communitymapuga","godawgs!")) %>%
  rename(ownname=own_name,
         ownaddress=own_addr,
         owncity=own_city,
         ownstate=own_state)

sample<-read_csv("parcels_hartwell_own.csv") %>%
  rename(parcel_num=Parcel_No,
         address=fulladd,
         ownname=own_name,
         ownaddress=own_addr,
         owncity=own_city,
         ownstate=own_state)

#####Revised entries--skipped for now
#Identify parcels with both new and revised entries
# parcel_points_data_rev<-parcel_points_data %>%
#   mutate(new_revised=as.character(new_revised)) %>%
#   group_by(parcel_num,new_revised) %>%
#   summarise(parcel_count=n()) %>%
#   spread(new_revised,parcel_count) %>%
#   filter(new_entry>0 & revised_entry>0)
# 
# parcels_revised<-data.frame(cbind(parcel_points_data_rev$parcel_num,1))
# names(parcels_revised)<-c("parcel_num","rev")
# 
# #Select most recent observation of revised entries and then add back to group
# parcel_points_data_rev1<-parcel_points_data %>%
#   left_join(parcels_revised) %>%
#   filter(rev==1) %>%
#   group_by(parcel_num) %>%
#   filter(end==max(end))
# 
# parcel_points_data<-parcel_points_data %>%
#   left_join(parcels_revised) %>%
#   filter(is.na(rev==TRUE)) %>%
#   bind_rows(parcel_points_data_rev1)

#Add in metadata
prop_type_dict<-read_csv("hartwell_shiny/data/prop_type_dict.csv")

parcel_points_data<-parcel_points_data %>%
  #left_join(sample) %>%
  left_join(prop_type_dict) %>%
  mutate(date=format(Sys.Date(),format="%Y_%m_%d"),
         #address_ver=as.character(address_ver),
         #address_rev=as.character(address_rev),
         #address_add1=as.character(address_add),
         lat=y,
         long=x,
         cond_factor=factor(condition,levels=c("well maintained","sound","minor repairs needed",
                                               "moderate rehabilitation needed",
                                               "substantial rehabilitation needed","dilapidated"))
         #address=if_else(search_type=="search_prop_add",address_add1,
        #                 if_else(address_ver=="no",address_rev,address_parcel)))
  )

#General property info
parcel_points_data<-parcel_points_data %>%
  mutate(str1=if_else(gen_prop_info.occupied=="TRUE","Occupied",""),
         str2=if_else(gen_prop_info.unoccupied=="TRUE","Unoccupied",""),
         str3=if_else(gen_prop_info.for_sale=="TRUE","For sale",""),
         str4=if_else(gen_prop_info.unknown=="TRUE","Status unknown","")) %>%
  unite(col="genprop",str1:str4,sep=" ",remove=TRUE)

#Foundation info
parcel_points_data<-parcel_points_data %>%
  mutate(found_good=if_else(foundation=="found_good","TRUE","FALSE"),
         found_partreplace=if_else(foundation=="found_partreplace","TRUE","FALSE"),
         found_compreplace=if_else(foundation=="found_compreplace","TRUE","FALSE"),
         found_cracked=if_else(foundation=="found_cracked","TRUE","FALSE"))

parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(found_good=="TRUE","<li>Good",""),
         str2=if_else(found_partreplace=="TRUE","<li>Partial replacement needed",""),
         str3=if_else(found_compreplace=="TRUE","<li>Complete replacement needed",""),
         str4=if_else(found_cracked=="TRUE","<li>Cracked",""),
         str99="</ul>") %>%
  unite(col="found_text",str0:str99,sep=" ",remove=TRUE)

#Exterior info
parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(exterior.ext_good=="TRUE","<li>Good",""),
         str2=if_else(exterior.ext_repaint=="TRUE","<li>Repainting needed",""),
         str3=if_else(exterior.ext_cracked=="TRUE","<li>Cracked/minor dry rot",""),
         str4=if_else(exterior.ext_needs_replace=="TRUE","<li>Needs replacement",""),
         str5=if_else(exterior.ext_chimney=="TRUE","<li>Chimney needs repair",""),
         str6=if_else(exterior.ext_nosiding=="TRUE","<li>Missing/no siding",""),
         str7=if_else(exterior.ext_notvis=="TRUE","<li>Not visible",""),
         str99="</ul>") %>%
  unite(col="exterior",str0:str99,sep=" ",remove=TRUE)

#Windows/doors info
parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(windows_doors.window_good=="TRUE","<li>Good",""),
         str2=if_else(windows_doors.window_repaint=="TRUE","<li>Repainting needed",""),
         str3=if_else(windows_doors.window_crackedpanes=="TRUE","<li>Cracked window panes",""),
         str4=if_else(windows_doors.window_minreplace=="TRUE","<li>1-3 windows need replacement",""),
         str5=if_else(windows_doors.window_majreplace=="TRUE","<li>More than 3 windows need replacement",""),
         str99="</ul>") %>%
  unite(col="windows",str0:str99,sep=" ",remove=TRUE)

#Stairs_rails info
parcel_points_data<-parcel_points_data %>%
  mutate(stairs_good=if_else(stairs_rails=="stairs_good","TRUE","FALSE"),
         stairs_cracked=if_else(stairs_rails=="stairs_cracked","TRUE","FALSE"),
         stairs_majorrepair=if_else(stairs_rails=="stairs_majorrepair","TRUE","FALSE"),
         stairs_repaint=if_else(stairs_rails=="stairs_repaint","TRUE","FALSE"))

parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(stairs_good=="TRUE","<li>Good",""),
         str2=if_else(stairs_cracked=="TRUE","<li>Cracked/minor repairs needed",""),
         str3=if_else(stairs_majorrepair=="TRUE","<li>Major repair needed",""),
         str4=if_else(stairs_repaint=="TRUE","<li>Repainting needed",""),
         str99="</ul>") %>%
  unite(col="stairs_text",str0:str99,sep=" ",remove=TRUE)

#Roofing info
parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(roofing.roof_good=="TRUE","<li>Good",""),
         str2=if_else(roofing.roof_gutters=="TRUE","<li>Gutters need repair",""),
         str3=if_else(roofing.roof_shingles=="TRUE","<li>Cracked/peeling shingles",""),
         str4=if_else(roofing.roof_reroof_part=="TRUE","<li>Partial re-roofing needed",""),
         str5=if_else(roofing.roof_reroof_tot=="TRUE","<li>Total re-roofing needed",""),
         str6=if_else(roofing.roof_newstructure=="TRUE","<li>>New roofing structure needed",""),
         str99="</ul>") %>%
  unite(col="roof",str0:str99,sep=" ",remove=TRUE)

#Lot assessment
parcel_points_data<-parcel_points_data %>%
  mutate(str0="",
         str1=if_else(lot_assess.lot_satis=="TRUE","<br>Satisfactory",""),
         str2=if_else(lot_assess.lot_weeds=="TRUE","<br>Lawn overgrown/weeds",""),
         str3=if_else(lot_assess.lot_missingcover=="TRUE","<br>Missing ground cover/grass",""),
         str4=if_else(lot_assess.lot_trees=="TRUE","<br>Dead/hazardous trees",""),
         str5=if_else(lot_assess.lot_inop_vehicle=="TRUE","<br>Inoperable vehicle in yard",""),
         str6=if_else(lot_assess.lot_junk=="TRUE","<br>Major cleanup/junk in yard",""),
         str6=if_else(lot_assess.lot_porchstorage=="TRUE","<br>Porch used as storage",""),
         str6=if_else(lot_assess.lot_graffiti=="TRUE","<br>Graffiti on house/property",""),
         str99="") %>%
  unite(col="lot",str0:str99,sep=" ",remove=TRUE)

#Create textbox
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

parcel_points_data1<-parcel_points_data %>%
  replace_na(list(photo1 = "blank", photo2 = "blank", photo3="blank",
                  photo4="blank",comments="blank")) %>%
  mutate(str01=paste("<strong>Parcel ID: </strong>",parcel_num),
         str02=paste("<br><strong>Parcel address: </strong>", address),
         str02a=paste("<br><strong>Owner: </strong>", ownname),
         str02b=paste("<br><strong>Owner's city: </strong>", owncity,", ",ownstate),
         str03=paste("<br><strong>Property type: </strong>",prop_type_name),
         str04=paste("<br><strong>Property status: </strong>",genprop),
         str05=paste("<br><strong>Property conditions: </strong>",proper(condition)),
         str06="<br><br><strong><u>Structural conditions</u></strong>",
         str07=paste("<br><b><i>Foundation: </b></i>",found_text),
         str08=paste("<b><i>Exterior: </b></i>",exterior),
         str09=paste("<b><i>Windows/doors: </b></i>",windows),
         str10=paste("<b><i>Stairs/rails: </b></i>",stairs_text),
         str11=paste("<b><i>Roof: </b></i>",roof),
         str12="<strong><u>Lot assessment</u></strong>",
         str13=paste(lot),
         str14=if_else(comments=="blank","",paste("<strong><u><br><br>Other comments 
                                                  </strong></u><br>",comments)),
         str90=if_else(photo1!="blank",paste("<br><br><strong>Photos</strong><br><a href='",
                                             photo1,"' target='_blank'>Picture 1</a>"),""),
         str91=if_else(photo2!="blank",paste("<br><a href='",
                                             photo2,"' target='_blank'>Picture 2</a>"),""),
         str92=if_else(photo3!="blank",paste("<br><a href='",
                                             photo3,"' target='_blank'>Picture 3</a>"),""),
         str93=if_else(photo4!="blank",paste("<br><a href='",
                                             photo4,"' target='_blank'>Picture 4</a>"),"")
         ) %>%
  unite(col="textbox",str01:str93,sep="",remove=TRUE)

#####################
# ID duplicates and add jitter
#####################

#Identify duplicates
parcel_count<-parcel_points_data1 %>% dplyr::count(parcel_num)

parcel_points_data2<-parcel_points_data1 %>%
  left_join(parcel_count)

#Separate duplicates and add jitter
parcel_points_data_dup<-parcel_points_data2 %>%
  filter(n>1) 

xrand<-runif(nrow(parcel_points_data_dup),-0.0002,0.0002)
yrand<-runif(nrow(parcel_points_data_dup),-0.0002,0.0002)

parcel_points_data_dup<-parcel_points_data_dup %>%
  bind_cols(data.frame(xrand),data.frame(yrand)) %>%
  mutate(x=x+xrand,
         y=y+yrand) %>%
  select(-xrand,-yrand)

parcel_points_data3<-parcel_points_data2 %>%
  filter(n==1) %>%
  bind_rows(parcel_points_data_dup)

parcel_points<-parcel_points_data3
#parcel_points<-st_as_sf(parcel_points_data3,coords=c("X","Y"),crs=4326,remove=FALSE)

#####################
# Create sortable parcel list with issues
#####################

#Select just the issue variables

parcel_points_issue<-parcel_points_data3 %>%
  filter(n==1) %>%
  select(parcel_num,address,prop_type_name,genprop,condition,comments,X_id,
         ownname,ownaddress,owncity,ownstate,
         lat,long,
         photo1,photo2,photo3,photo4,
         found_partreplace:found_cracked,
         windows_doors.window_repaint:windows_doors.window_majreplace,
         stairs_cracked:stairs_repaint,
         roofing.roof_gutters:roofing.roof_newstructure,
         lot_assess.lot_weeds:lot_assess.lot_graffiti,
         exterior.ext_repaint:exterior.ext_nosiding)

#Create a long list of properties for each specific issue
parcel_points_issue_long <- parcel_points_issue %>%
  gather(found_partreplace:exterior.ext_nosiding,key="var",value="value") %>%
  replace_na(list(value="FALSE")) %>%
  filter(value=="TRUE") %>%
  select(-value) 

#################
#Read in issue categories and define color scheme
################

var_alias<-read_csv("hartwell_shiny/Data/var_alias.csv")

color1<-"#1b9e77"
color2<-"#377eb8"
color3<-"#4daf4a"
color4<-"#984ea3"
color5<-"#ff7f00"
color6<-"#e6ab02"

var_cat<-c("Exterior","Windows/doors","Roof","Lot","Foundation","Stairs")
color<-c(color1,color2,color3,color4,color5,color6)
catcolor<-data.frame(cbind(var_cat,color))

var_alias<-left_join(var_alias,catcolor)

#####################
# Create issue table
#####################

parcel_issues<-parcel_points_data3 %>%
  filter(prop_type_name!="Non-residential" & prop_type_name!="Vacant") %>% 
  #Select just property characteristics
  select(parcel_num,X_id,
         found_partreplace:found_cracked,
         windows_doors.window_repaint:windows_doors.window_majreplace,
         stairs_cracked:stairs_repaint,
         roofing.roof_gutters:roofing.roof_newstructure,
         lot_assess.lot_weeds:lot_assess.lot_graffiti,
         exterior.ext_repaint:exterior.ext_nosiding) %>%
  #Collapse issues to long format
  gather(found_partreplace:exterior.ext_nosiding,key="var",value="value") %>%
  replace_na(list(value="FALSE")) %>%
  #Change true/false to 0,1
  mutate(value1=if_else(value=="FALSE",0,1)) %>%
  #Create issue count and pct 
  dplyr::group_by(var) %>%
  dplyr::summarise(total=n(),
                   issue_count=sum(value1),
                   issue_pct=(round(issue_count/total*100,2))) %>%
  #Add alias names
  left_join(var_alias) 

parcel_issues<-parcel_issues[order(-parcel_issues$issue_pct),]


#Write tables
parcel_points<-filter(parcel_points,long<0) #Filter for missing coordinates

write_csv(parcel_points_issue_long,"hartwell_shiny/Data/parcel_points_issue.csv")
write_csv(parcel_points,"hartwell_shiny/Data/parcel_points.csv")
write_csv(parcel_issues,"hartwell_shiny/Data/parcel_issues.csv")
write_csv(parcel_points_issue_long,"hartwell_shiny/Data/parcel_points_issue_long.csv")
