library(tidyverse)
library(sf)

#Read in data and identify duplicates
parcels<-st_read("data/Parcels2019Feb5JOIN.shp")
parcels_combine<-parcels %>%
  group_by(Parcel_No) %>%
  summarise() %>%
  st_transform(4326)

parcels_unique<-parcels %>%
  st_set_geometry(NULL) %>%
  select(Parcel_No,REALKEY,HOUSE_NO,CENSUS_CAT,STDIRECT,STREET_NAM,STTYPE,HOMEEXEMPT) %>%
  distinct()

parcels<-parcels_combine %>%
  left_join(parcels_unique)

parcels_data_sf<-parcels %>%
  mutate(STREET_NAM=paste(toupper(substring(STREET_NAM, 1,1)), tolower(substring(STREET_NAM, 2)),sep=""),
         STTYPE=paste(toupper(substring(STTYPE, 1,1)), tolower(substring(STTYPE, 2)),sep=""),
         fulladd=paste(HOUSE_NO," ",STDIRECT," ",STREET_NAM," ",STTYPE,sep=""),
         fulladd=gsub(" NA "," ",fulladd)) %>%
  select(Parcel_No,fulladd,REALKEY,HOMEEXEMPT,CENSUS_CAT) %>%
  st_transform(4326) 

st_write(parcels_data_sf,"data/hartwell_cleanparcels1.shp")

parcels_points_sf<-st_centroid(parcels_data_sf)

coords<-as_tibble(st_coordinates(parcels_points_sf))

parcels_point<-parcels_points_sf %>%
  st_set_geometry(NULL) %>%
  bind_cols(coords) %>%
  distinct()

parcels_count<-parcels_point %>%
  count(Parcel_No)

write_csv(parcels_point,"parcels_hartwell.csv")

#Paste owners info

parcels_point<-read_csv("parcels_hartwell.csv")

parcelsown<-st_read("data/ParcelsOwner2019Feb5.shp") %>%
  select(Parcel_No,LASTNAME,ADDRESS1:ZIP_1) %>%
  st_set_geometry(NULL) %>%
  distinct() %>%
  unite(own_addr,ADDRESS1:ADDRESS3) %>%
  rename(own_name=LASTNAME,
         own_city=CITY,
         own_state=STATE,
         own_zip=ZIP_1) %>%
  right_join(parcels_point)
  
write_csv(parcelsown,"parcels_hartwell_own.csv")
