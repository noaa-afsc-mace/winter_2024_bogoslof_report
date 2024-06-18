# create consistent naming conventions to group for plotting

# this function is a BOGOSLOF-ONLY version of the same function get_nice_region_names that is used in the Shelikof reports;
# it simply uses geographic regions as defined in Bogoslof reports
# Samalga: west of 168 30 W and south of 55 N
# Umnak: between 168 30 W and south of 55 N
# Unalaska: between 167 W and 166 W and south of 55 N


get_nice_region_names <- function(longitude, latitude) {
  
  if (longitude <= -168.2 & latitude < 55.0) {
    region <- "Samalga"
  }
  
  else if (longitude > -168.2 & longitude < -167.0 & latitude < 55.0){
    region <- "Umnak"
  }
  
  else if (longitude >= -167.0 & latitude < 55.0) {
    region <- "Unalaska"
  }
  
  else if (latitude >= 55.0) {
    region <- "EBS Shelf"
  }
  
  else {
    region <- "other"
  }
  
  
  
  return(region)
}
