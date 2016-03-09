##0: Load the data in RStudio
refine<- read.csv("refine_original.csv", header = TRUE) 
##1: Clean up brand names
clean <- (refine)
clean$company[1:6] <- "philips"
clean$company[7:13] <- "akzo"
clean$company[14:16] <- "philips"
clean$company[17:21] <- "van houten"
clean$company[22:25] <- "unilever"
##2: Separate product code and number
library (tidyr)
cleanANDsep <- separate(clean,Product.code...number,c("product_code","product_number"),sep = "-", remove=FALSE)
##3: Add product categories
library (dplyr)
cleanANDsepANDprod <- mutate(cleanANDsep, category = ifelse(product_code == "p","Smartphone", ifelse(product_code == "v","TV", ifelse(product_code == "x", "Laptop", ifelse(product_code == "q","Tablet","")))))
##4: Add full address for geocoding
geocoded <- cleanANDsepANDprod %>% mutate(., full_address = paste(address, city, country, sep = ", "))
##5: Create dummy variables for company and product category
dummyvars <- mutate(geocoded, 
  company_philips = ifelse( company == "philips", 1, 0),
  company_akzo = ifelse( company == "akzo", 1, 0),
  company_van_houten = ifelse( company == "van houten", 1, 0),
  company_unilever = ifelse( company == "unilever", 1, 0),
  product_smartphone = ifelse( product_code == "p", 1, 0),
  product_tv = ifelse( product_code == "v", 1, 0),
  product_laptop = ifelse( product_code == "x", 1, 0),
  product_tablet = ifelse( product_code == "q", 1, 0))
##6: Submit the project on Github
write.csv(dummyvars, "refine_clean.csv")