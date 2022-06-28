#Things that need editing before each use:
# 1. Working directory: click "Session" -> "Set Working Directory" -> "Choose Directory" -> select the folder where you have the excel files that you want to work with for this session.
# 2. Your file names: Use the "Find/Replace" tab to easily change the date/file name in the file names when you are working with a different set of data that are processed on a different date
# 3. Then number of images included for 'doesntLD' object below or for any other organelle object
# 4. Your specific experiment number/code when writing 'clean' files at very end

library(tidyverse)
library(readr)
library(dplyr)


#Read ALL the excel files present in the folder into R at once. This step might take more than a few seconds depending on the size of your files.
data_files <- list.files(getwd())
for(i in 1:length(data_files)) {
  assign(data_files[i],
         read.csv(paste0(getwd(),"/", data_files[i])))
}


##selecting size and shape metrics from the individual organelle files:

LS <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LS.csv` %>% 
  select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity, AreaShape_FormFactor, AreaShape_MajorAxisLength, AreaShape_MinorAxisLength, AreaShape_Compactness) %>% 
  group_by(ImageNumber) %>% 
  summarise(LS_pixel_area = sum(AreaShape_Area), LS_med_size = median(AreaShape_Area),  LS_mean_size = mean(AreaShape_Area), 
    LS_mean_ecty = mean(AreaShape_Eccentricity), LS_med_ecty = median (AreaShape_Eccentricity), LS_mean_FormF= mean(AreaShape_FormFactor), LS_med_FormF= median (AreaShape_FormFactor) LS_mean_MajAxis= mean(AreaShape_MajorAxisLength), LS_mean_MinAxis= mean(AreaShape_MinorAxisLength), 
    LS_mean_Compact= mean(AreaShape_Compactness))

MT <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_MT.csv` %>% 
  select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity, AreaShape_FormFactor, AreaShape_MajorAxisLength, AreaShape_MinorAxisLength) %>% 
  group_by(ImageNumber) %>% 
  summarise(MT_pixel_area = sum(AreaShape_Area), MT_med_size = median(AreaShape_Area), MT_mean_size = mean(AreaShape_Area),
    MT_mean_ecty = mean(AreaShape_Eccentricity), MT_mean_FormF= mean(AreaShape_FormFactor), MT_mean_MajAxis= mean(AreaShape_MajorAxisLength), MT_mean_MinAxis= mean(AreaShape_MinorAxisLength))

GL <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_GL.csv` %>% 
  select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity, AreaShape_FormFactor, AreaShape_MajorAxisLength, AreaShape_MinorAxisLength) %>% 
  group_by(ImageNumber) %>% 
  summarise(GL_pixel_area = sum(AreaShape_Area), GL_med_size = median(AreaShape_Area), GL_mean_size = mean(AreaShape_Area),
    GL_mean_ecty = mean(AreaShape_Eccentricity),GL_mean_FormF= mean(AreaShape_FormFactor), GL_mean_MajAxis= mean(AreaShape_MajorAxisLength), GL_mean_MinAxis= mean(AreaShape_MinorAxisLength))

ER <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_ER.csv` %>% 
    select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity) %>% 
    group_by(ImageNumber) %>% 
  summarise(ER_pixel_area = sum(AreaShape_Area), ER_mean_ecty = mean(AreaShape_Eccentricity))

PO <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_PO.csv` %>% 
  select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity, AreaShape_FormFactor, AreaShape_MajorAxisLength, AreaShape_MinorAxisLength) %>% 
  group_by(ImageNumber) %>% 
  summarise(PO_pixel_area = sum(AreaShape_Area), PO_med_size = median(AreaShape_Area), PO_mean_size = mean(AreaShape_Area), PO_mean_ecty = mean(AreaShape_Eccentricity), PO_mean_FormF= mean(AreaShape_FormFactor), PO_mean_MajAxis= mean(AreaShape_MajorAxisLength), PO_mean_MinAxis= mean(AreaShape_MinorAxisLength))

#LD <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LD.csv` %>% 
#select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity) %>% 
#group_by(ImageNumber) %>% summarise(LD_pixel_area = sum(AreaShape_Area), LD_med_size = median(AreaShape_Area), LD_mean_ecty = mean(AreaShape_Eccentricity))

incomplete_LD <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LD.csv` %>% 
  select(ImageNumber, AreaShape_Area, AreaShape_Eccentricity, AreaShape_FormFactor, AreaShape_MajorAxisLength, AreaShape_MinorAxisLength) %>% 
  group_by(ImageNumber) %>% 
  summarise(
    LD_pixel_area = sum(AreaShape_Area), LD_med_size = median(AreaShape_Area), LD_mean_size = mean(AreaShape_Area), 
    LD_mean_ecty = mean(AreaShape_Eccentricity), LD_mean_FormF= mean(AreaShape_FormFactor), LD_mean_MajAxis= mean(AreaShape_MajorAxisLength), LD_mean_MinAxis= mean(AreaShape_MinorAxisLength))
number_have_LD <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LD.csv` %>%
  select(ImageNumber, AreaShape_Area, ObjectNumber) %>% group_by(ImageNumber) %>% summarise()
#change the number of the pictures: put the TOTAL number of images
doesntLD <- subset(data.frame(ImageNumber = 1:16), !(ImageNumber %in% number_have_LD$ImageNumber))
unordered_LD <- doesntLD %>% add_column(LD_pixel_area = NA, LD_med_size = NA, LD_mean_size = NA, LD_mean_ecty = NA, LD_mean_FormF= NA, LD_mean_MajAxis= NA, LD_mean_MinAxis= NA) %>% rbind(incomplete_LD)
LD <- unordered_LD[with(unordered_LD, order(ImageNumber)),]


                              ## Analyse the Proximity of each individual organelle vs each other
#Lysosomes Proximity
LS_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LS.csv` %>% 
  select(ImageNumber, Neighbors_FirstClosestDistance_ER_Adjacent, Neighbors_FirstClosestDistance_GL_Adjacent, Neighbors_FirstClosestDistance_MT_Adjacent, Neighbors_FirstClosestDistance_LD_Adjacent, Neighbors_FirstClosestDistance_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_LS_Prox_ER= mean(Neighbors_FirstClosestDistance_ER_Adjacent),
            mean_perc_LS_Prox_MT= mean(Neighbors_FirstClosestDistance_MT_Adjacent),
            mean_perc_LS_Prox_PO= mean(Neighbors_FirstClosestDistance_PO_Adjacent),
            mean_perc_LS_Prox_GL= mean(Neighbors_FirstClosestDistance_GL_Adjacent),
            mean_perc_LS_Prox_LD= mean(Neighbors_FirstClosestDistance_LD_Adjacent))

#Mitochondria Proximity 
MT_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_MT.csv` %>% 
  select(ImageNumber, Neighbors_FirstClosestDistance_ER_Adjacent, Neighbors_FirstClosestDistance_GL_Adjacent, Neighbors_FirstClosestDistance_LS_Adjacent, Neighbors_FirstClosestDistance_LD_Adjacent, Neighbors_FirstClosestDistance_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_MT_Prox_ER= mean(Neighbors_FirstClosestDistance_ER_Adjacent),
            mean_perc_MT_Prox_LS= mean(Neighbors_FirstClosestDistance_LS_Adjacent),
            mean_perc_MT_Prox_PO= mean(Neighbors_FirstClosestDistance_PO_Adjacent),
            mean_perc_MT_Prox_GL= mean(Neighbors_FirstClosestDistance_GL_Adjacent),
            mean_perc_MT_Prox_LD= mean(Neighbors_FirstClosestDistance_LD_Adjacent))

#Golgi Proximity 
GL_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_GL.csv` %>% 
  select(ImageNumber, Neighbors_FirstClosestDistance_ER_Adjacent, Neighbors_FirstClosestDistance_MT_Adjacent, Neighbors_FirstClosestDistance_LS_Adjacent, Neighbors_FirstClosestDistance_LD_Adjacent, Neighbors_FirstClosestDistance_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_GL_Prox_ER= mean(Neighbors_FirstClosestDistance_ER_Adjacent),
            mean_perc_GL_Prox_LS= mean(Neighbors_FirstClosestDistance_LS_Adjacent),
            mean_perc_GL_Prox_PO= mean(Neighbors_FirstClosestDistance_PO_Adjacent),
            mean_perc_GL_Prox_MT= mean(Neighbors_FirstClosestDistance_MT_Adjacent),
            mean_perc_GL_Prox_LD= mean(Neighbors_FirstClosestDistance_LD_Adjacent))

#Peroxisomes Proximity 
PO_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_PO.csv` %>% 
  select(ImageNumber, Neighbors_FirstClosestDistance_ER_Adjacent, Neighbors_FirstClosestDistance_MT_Adjacent, Neighbors_FirstClosestDistance_LS_Adjacent, Neighbors_FirstClosestDistance_LD_Adjacent, Neighbors_FirstClosestDistance_GL_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_PO_Prox_ER= mean(Neighbors_FirstClosestDistance_ER_Adjacent),
            mean_perc_PO_Prox_LS= mean(Neighbors_FirstClosestDistance_LS_Adjacent),
            mean_perc_PO_Prox_GL= mean(Neighbors_FirstClosestDistance_GL_Adjacent),
            mean_perc_PO_Prox_MT= mean(Neighbors_FirstClosestDistance_MT_Adjacent),
            mean_perc_PO_Prox_LD= mean(Neighbors_FirstClosestDistance_LD_Adjacent))

#ER Proximity
ER_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_ER.csv` %>% 
  select(ImageNumber, Neighbors_FirstClosestDistance_GL_Adjacent, Neighbors_FirstClosestDistance_MT_Adjacent, Neighbors_FirstClosestDistance_LS_Adjacent, Neighbors_FirstClosestDistance_LD_Adjacent, Neighbors_FirstClosestDistance_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_ER_Prox_GL= mean(Neighbors_FirstClosestDistance_GL_Adjacent),
            mean_perc_ER_Prox_LS= mean(Neighbors_FirstClosestDistance_LS_Adjacent),
            mean_perc_ER_Prox_PO= mean(Neighbors_FirstClosestDistance_PO_Adjacent),
            mean_perc_ER_Prox_MT= mean(Neighbors_FirstClosestDistance_MT_Adjacent),
            mean_perc_ER_Prox_LD= mean(Neighbors_FirstClosestDistance_LD_Adjacent))

#LD Proximity
#LD_Proximity <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LD.csv` %>% 
#select(ImageNumber, Neighbors_FirstClosestDistance_ER_Adjacent, Neighbors_FirstClosestDistance_MT_Adjacent, Neighbors_FirstClosestDistance_LS_Adjacent, Neighbors_FirstClosestDistance_GL_Adjacent, Neighbors_FirstClosestDistance_PO_Adjacent) %>% group_by(ImageNumber) %>%
#summarize(mean_perc_LD_Prox_ER= mean(Neighbors_FirstClosestDistance_ER_Adjacent),
## mean_perc_LD_Prox_LS= mean(Neighbors_FirstClosestDistance_LS_Adjacent),
# mean_perc_LD_Prox_PO= mean(Neighbors_FirstClosestDistance_PO_Adjacent),
# mean_perc_LD_Prox_MT= mean(Neighbors_FirstClosestDistance_MT_Adjacent),
# mean_perc_LD_Prox_GL= mean(Neighbors_FirstClosestDistance_GL_Adjacent))

IndividualOrganelle_Proximity_data <- cbind (LS_Proximity, MT_Proximity, GL_Proximity, PO_Proximity, ER_Proximity)%>% subset(select = which(!duplicated(names(.)))) %>%
  select(ImageNumber,
         mean_perc_LS_Prox_ER, mean_perc_LS_Prox_MT, mean_perc_LS_Prox_PO, mean_perc_LS_Prox_GL, mean_perc_LS_Prox_LD,
         mean_perc_MT_Prox_ER, mean_perc_MT_Prox_LS, mean_perc_MT_Prox_PO, mean_perc_MT_Prox_GL, mean_perc_MT_Prox_LD,
         mean_perc_GL_Prox_ER, mean_perc_GL_Prox_LS, mean_perc_GL_Prox_PO, mean_perc_GL_Prox_MT, mean_perc_GL_Prox_LD,
         mean_perc_PO_Prox_ER, mean_perc_PO_Prox_LS, mean_perc_PO_Prox_GL, mean_perc_PO_Prox_MT, mean_perc_PO_Prox_LD,
         mean_perc_ER_Prox_GL, mean_perc_ER_Prox_LS, mean_perc_ER_Prox_PO, mean_perc_ER_Prox_MT, mean_perc_ER_Prox_LD)


                             ##Analyse Individual organelle size and shape

area_counts <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>% 
  summarize(ImageNumber = ImageNumber, CY_pixel_area = AreaOccupied_AreaOccupied_CY, SO_pixel_area = AreaOccupied_AreaOccupied_SO, NU_pixel_area = AreaOccupied_AreaOccupied_NU, GL_count = Count_GL, LS_count = Count_LS, PO_count = Count_PO, MT_count = Count_MT, LD_count = Count_LD)
meta <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>% 
  summarise(ImageNumber = ImageNumber, group = Metadata_SampleName, ImageName = FileName_SomaGreyScale)
SO <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_SO.csv` %>% summarise(ImageNumber = ImageNumber, SO_ecty = AreaShape_Eccentricity)


#Generate a table collecting different measurements per each organelle, and normalised on Cytoplasmic and Soma Area:
IndividualOrganelle_SizeAndShape_data <- cbind(meta, area_counts, ER, PO, GL, MT, LS, LD, SO) %>% subset(select = which(!duplicated(names(.)))) %>%
  mutate(NU_SO_perc_area = (NU_pixel_area/SO_pixel_area)*100, 
         LS_SO_perc_area = (LS_pixel_area/SO_pixel_area)*100,
         MT_SO_perc_area = (MT_pixel_area/SO_pixel_area)*100,
         GL_SO_perc_area = (GL_pixel_area/SO_pixel_area)*100, 
         PO_SO_perc_area = (PO_pixel_area/SO_pixel_area)*100, 
         ER_SO_perc_area = (ER_pixel_area/SO_pixel_area)*100, 
         LD_SO_perc_area = (LD_pixel_area/SO_pixel_area)*100,
         
         LS_CY_perc_area = (LS_pixel_area/CY_pixel_area)*100,
         GL_CY_perc_area = (GL_pixel_area/CY_pixel_area)*100, 
         MT_CY_perc_area = (MT_pixel_area/CY_pixel_area)*100,
         PO_CY_perc_area = (PO_pixel_area/SO_pixel_area)*100, 
         ER_CY_perc_area = (ER_pixel_area/CY_pixel_area)*100,
         LD_CY_perc_area = (LD_pixel_area/CY_pixel_area)*100,
         
         Uncovered_pixel_area = SO_pixel_area - (NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area)) %>%
  select(group, ImageNumber, SO_pixel_area, SO_ecty, CY_pixel_area, NU_pixel_area, NU_SO_perc_area, 
         LS_count, LS_pixel_area, LS_SO_perc_area, LS_CY_perc_area, LS_med_size, LS_mean_size,	LS_mean_ecty, LS_mean_FormF, LS_mean_MajAxis, LS_mean_MinAxis, LS_mean_Compact,
         MT_count, MT_pixel_area, MT_SO_perc_area, MT_CY_perc_area, MT_med_size,	MT_mean_size, MT_mean_ecty, MT_mean_FormF, MT_mean_MajAxis, MT_mean_MinAxis,
         GL_count, GL_pixel_area, GL_SO_perc_area, GL_CY_perc_area, GL_med_size,	GL_mean_size, GL_mean_ecty, GL_mean_FormF, GL_mean_MajAxis, GL_mean_MinAxis,
         PO_count, PO_pixel_area, PO_SO_perc_area, PO_CY_perc_area, PO_med_size,	PO_mean_size, PO_mean_ecty, PO_mean_FormF, PO_mean_MajAxis, PO_mean_MinAxis,
         ER_pixel_area, ER_CY_perc_area, ER_SO_perc_area, ER_mean_ecty,
         LD_count, LD_pixel_area, LD_SO_perc_area, LD_CY_perc_area, LD_med_size, LD_mean_size, LD_mean_ecty, LD_mean_FormF, LD_mean_MajAxis, LD_mean_MinAxis,
         Uncovered_pixel_area)

IndividualOrganelle_SizeAndShape_NormAllAreas_data <- `IndividualOrganelle_SizeAndShape_data` %>% subset(select = which(!duplicated(names(.)))) %>%
  mutate(NU_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         LS_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         MT_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         GL_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         PO_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         ER_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100,
         LD_normArea=(NU_pixel_area/(NU_pixel_area + LS_pixel_area + MT_pixel_area + GL_pixel_area + PO_pixel_area + ER_pixel_area + LD_pixel_area + Uncovered_pixel_area))*100) %>% subset(select = which(!duplicated(names(.)))) %>%
  select(group, ImageNumber, SO_pixel_area, SO_ecty, CY_pixel_area, NU_pixel_area, NU_SO_perc_area, 
         LS_count, LS_pixel_area, LS_SO_perc_area, LS_CY_perc_area, LS_med_size, LS_mean_size,	LS_mean_ecty, LS_mean_FormF, LS_mean_MajAxis, LS_mean_MinAxis, LS_mean_Compact,
         MT_count, MT_pixel_area, MT_SO_perc_area, MT_CY_perc_area, MT_med_size,	MT_mean_size, MT_mean_ecty, MT_mean_FormF, MT_mean_MajAxis, MT_mean_MinAxis,
         GL_count, GL_pixel_area, GL_SO_perc_area, GL_CY_perc_area, GL_med_size,	GL_mean_size, GL_mean_ecty, GL_mean_FormF, GL_mean_MajAxis, GL_mean_MinAxis,
         PO_count, PO_pixel_area, PO_SO_perc_area, PO_CY_perc_area, PO_med_size,	PO_mean_size, PO_mean_ecty, PO_mean_FormF, PO_mean_MajAxis, PO_mean_MinAxis,
         ER_pixel_area, ER_CY_perc_area, ER_SO_perc_area, ER_mean_ecty, 
         LD_count, LD_pixel_area, LD_SO_perc_area, LD_CY_perc_area, LD_med_size, LD_mean_size, LD_mean_ecty, LD_mean_FormF, LD_mean_MajAxis, LD_mean_MinAxis,
         Uncovered_pixel_area, NU_normArea, LS_normArea, MT_normArea, GL_normArea, PO_normArea, ER_normArea, LD_normArea)

                             
     ##Analyse the contacts size and number

organelle_2MCS_overlap <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>% 
summarise(ImageNumber = ImageNumber, pixel_area_GL_ER = AreaOccupied_AreaOccupied_GL_ER, pixel_area_GL_LD = AreaOccupied_AreaOccupied_GL_LD, pixel_area_LS_ER = AreaOccupied_AreaOccupied_LS_ER, pixel_area_MT_ER = AreaOccupied_AreaOccupied_MT_ER, pixel_area_PO_ER = AreaOccupied_AreaOccupied_PO_ER, pixel_area_PO_LD = AreaOccupied_AreaOccupied_PO_LD, pixel_area_LS_GL = AreaOccupied_AreaOccupied_LS_GL, pixel_area_LS_LD = AreaOccupied_AreaOccupied_LS_LD, pixel_area_MT_GL = AreaOccupied_AreaOccupied_MT_GL, pixel_area_MT_LD = AreaOccupied_AreaOccupied_MT_LD, pixel_area_GL_PO = AreaOccupied_AreaOccupied_GL_PO, pixel_area_LS_MT = AreaOccupied_AreaOccupied_LS_MT, pixel_area_LS_PO = AreaOccupied_AreaOccupied_LS_PO, pixel_area_MT_PO = AreaOccupied_AreaOccupied_MT_PO, pixel_area_ER_LD = AreaOccupied_AreaOccupied_ER_LD, Count_GL_ER = Count_GL_ER, Count_LS_ER = Count_LS_ER, Count_MT_ER = Count_MT_ER, Count_PO_ER = Count_PO_ER, Count_LS_GL = Count_LS_GL, Count_MT_GL = Count_MT_GL, Count_GL_PO = Count_GL_PO, Count_LS_MT = Count_LS_MT, Count_LS_PO = Count_LS_PO, Count_MT_PO = Count_MT_PO, Count_MT_LD = Count_MT_LD, Count_LS_LD = Count_LS_LD, Count_PO_LD = Count_PO_LD,  Count_ER_LD = Count_ER_LD, Count_GL_LD = Count_GL_LD)

organelle_3MCS_overlap <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>%
  summarise(ImageNumber = ImageNumber, pixel_area_GL_ER_LD = AreaOccupied_AreaOccupied_GL_ER_LD, pixel_area_GL_PO_ER =AreaOccupied_AreaOccupied_GL_PO_ER, pixel_area_GL_PO_LD = AreaOccupied_AreaOccupied_GL_PO_LD, pixel_area_LS_ER_LD = AreaOccupied_AreaOccupied_LS_ER_LD, pixel_area_LS_GL_ER = AreaOccupied_AreaOccupied_LS_GL_ER, pixel_area_LS_GL_LD = AreaOccupied_AreaOccupied_LS_GL_LD, pixel_area_LS_GL_PO = AreaOccupied_AreaOccupied_LS_GL_PO, pixel_area_LS_MT_ER = AreaOccupied_AreaOccupied_LS_MT_ER, pixel_area_LS_MT_GL = AreaOccupied_AreaOccupied_LS_MT_GL, pixel_area_LS_MT_LD = AreaOccupied_AreaOccupied_LS_MT_LD, pixel_area_LS_MT_PO = AreaOccupied_AreaOccupied_LS_MT_PO, pixel_area_LS_PO_ER = AreaOccupied_AreaOccupied_LS_PO_ER, pixel_area_LS_PO_LD = AreaOccupied_AreaOccupied_LS_PO_LD, pixel_area_MT_ER_LD = AreaOccupied_AreaOccupied_MT_ER_LD, pixel_area_MT_GL_ER = AreaOccupied_AreaOccupied_MT_GL_ER, pixel_area_MT_GL_LD = AreaOccupied_AreaOccupied_MT_GL_LD, pixel_area_MT_GL_PO = AreaOccupied_AreaOccupied_MT_GL_PO, pixel_area_MT_PO_ER = AreaOccupied_AreaOccupied_MT_PO_ER, pixel_area_MT_PO_LD = AreaOccupied_AreaOccupied_MT_PO_LD, pixel_area_PO_ER_LD = AreaOccupied_AreaOccupied_PO_ER_LD, Count_GL_ER_LD = Count_GL_ER_LD, Count_GL_PO_ER = Count_GL_PO_ER, Count_GL_PO_LD = Count_GL_PO_LD, Count_LS_ER_LD = Count_LS_ER_LD, Count_LS_GL_ER = Count_LS_GL_ER, Count_LS_GL_LD = Count_LS_GL_LD, Count_LS_GL_PO = Count_LS_GL_PO, Count_LS_MT_ER = Count_LS_MT_ER, Count_LS_MT_GL = Count_LS_MT_GL, Count_LS_MT_LD = Count_LS_MT_LD, Count_LS_MT_PO = Count_LS_MT_PO, Count_LS_PO_ER = Count_LS_PO_ER, Count_LS_PO_LD = Count_LS_PO_LD, Count_MT_ER_LD = Count_MT_ER_LD, Count_MT_GL_ER = Count_MT_GL_ER, Count_MT_GL_LD = Count_MT_GL_LD, Count_MT_GL_PO = Count_MT_GL_PO, Count_MT_PO_ER = Count_MT_PO_ER, Count_MT_PO_LD = Count_MT_PO_LD, Count_PO_ER_LD = Count_PO_ER_LD)

organelle_4MCS_overlap <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>%
  summarise(ImageNumber = ImageNumber, 
            pixel_area_GL_PO_ER_LD = AreaOccupied_AreaOccupied_GL_PO_ER_LD, pixel_area_LS_MT_GL_ER = AreaOccupied_AreaOccupied_LS_MT_GL_ER, pixel_area_LS_MT_GL_LD = AreaOccupied_AreaOccupied_LS_MT_GL_LD, pixel_area_LS_MT_GL_PO = AreaOccupied_AreaOccupied_LS_MT_GL_PO, pixel_area_LS_MT_PO_ER = AreaOccupied_AreaOccupied_LS_MT_PO_ER, pixel_area_LS_MT_PO_LD = AreaOccupied_AreaOccupied_LS_MT_PO_LD, pixel_area_MT_GL_ER_LD = AreaOccupied_AreaOccupied_MT_GL_ER_LD, pixel_area_MT_PO_ER_LD = AreaOccupied_AreaOccupied_MT_PO_ER_LD, pixel_area_MT_GL_PO_ER = AreaOccupied_AreaOccupied_MT_GL_PO_ER, pixel_area_MT_GL_PO_LD = AreaOccupied_AreaOccupied_MT_GL_PO_LD, 
            Count_GL_PO_ER_LD = Count_GL_PO_ER_LD, Count_LS_MT_GL_ER = Count_LS_MT_GL_ER, Count_LS_MT_GL_LD = Count_LS_MT_GL_LD, Count_LS_MT_GL_PO = Count_LS_MT_GL_PO, Count_LS_MT_PO_ER = Count_LS_MT_PO_ER, Count_LS_MT_PO_LD = Count_LS_MT_PO_LD, Count_MT_GL_ER_LD = Count_MT_GL_ER_LD, Count_MT_GL_PO_ER = Count_MT_GL_PO_ER, Count_MT_GL_PO_LD = Count_MT_GL_PO_LD, Count_MT_PO_ER_LD = Count_MT_PO_ER_LD)

organelle_5MCS_overlap <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>%
  summarise(ImageNumber = ImageNumber, pixel_area_LS_MT_GL_ER_LD = AreaOccupied_AreaOccupied_LS_MT_GL_ER_LD, pixel_area_LS_MT_GL_PO_ER = AreaOccupied_AreaOccupied_LS_MT_GL_PO_ER, pixel_area_LS_MT_GL_PO_LD = AreaOccupied_AreaOccupied_LS_MT_GL_PO_LD, pixel_area_MT_GL_PO_ER_LD = AreaOccupied_AreaOccupied_MT_GL_PO_ER_LD, Count_LS_MT_GL_ER_LD = Count_LS_MT_GL_ER_LD, Count_LS_MT_GL_PO_ER = Count_LS_MT_GL_PO_ER, Count_LS_MT_GL_PO_LD = Count_LS_MT_GL_PO_LD, Count_MT_GL_PO_ER_LD = Count_MT_GL_PO_ER_LD)

organelle_6MCS_overlap <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_Image.csv` %>%
  summarise(ImageNumber = ImageNumber, pixel_area_LS_MT_GL_PO_ER_LD = AreaOccupied_AreaOccupied_LS_MT_GL_PO_ER_LD, Count_LS_MT_GL_PO_ER_LD = Count_LS_MT_GL_PO_ER_LD)

Contacts_allMCS_overlap_data <- cbind(meta, area_counts, ER, PO, GL, MT, LS, LD, organelle_2MCS_overlap, organelle_3MCS_overlap, organelle_4MCS_overlap, organelle_5MCS_overlap, organelle_6MCS_overlap) %>% subset(select = which(!duplicated(names(.)))) %>%
  mutate (normArea_LS_ER_byER=(pixel_area_LS_ER/ER_pixel_area)*100 , normArea_LS_ER_byLS=(pixel_area_LS_ER/LS_pixel_area)*100, normArea_LS_MT_byLS=(pixel_area_LS_MT/LS_pixel_area)*100, normArea_LS_MT_byMT=(pixel_area_LS_MT/MT_pixel_area)*100, normArea_LS_GL_byGL=(pixel_area_LS_GL/GL_pixel_area)*100, normArea_LS_GL_byLS=(pixel_area_LS_GL/LS_pixel_area)*100, normArea_LS_GL_byGL=(pixel_area_LS_GL/GL_pixel_area)*100, normArea_LS_PO_byLS=(pixel_area_LS_PO/LS_pixel_area)*100, normArea_LS_PO_byPO=(pixel_area_LS_MT/PO_pixel_area)*100, normArea_LS_LD_byLS=(pixel_area_LS_LD/LS_pixel_area)*100, normArea_LS_LD_byLD=(pixel_area_LS_LD/LD_pixel_area)*100,
          normArea_GL_ER_byGL=(pixel_area_GL_ER/GL_pixel_area)*100, normArea_GL_ER_byER=(pixel_area_GL_ER/ER_pixel_area)*100, normArea_GL_PO_byGL=(pixel_area_GL_PO/GL_pixel_area)*100, normArea_GL_PO_byPO=(pixel_area_GL_PO/PO_pixel_area)*100, normArea_GL_LD_byGL=(pixel_area_GL_LD/GL_pixel_area)*100, normArea_GL_LD_byLD=(pixel_area_GL_LD/LD_pixel_area)*100,
          normArea_MT_ER_byER=(pixel_area_MT_ER/ER_pixel_area)*100, normArea_MT_ER_byMT=(pixel_area_MT_ER/MT_pixel_area)*100, normArea_MT_GL_byGL=(pixel_area_MT_GL/GL_pixel_area)*100, normArea_MT_GL_byMT=(pixel_area_MT_GL/MT_pixel_area)*100, normArea_MT_PO_byPO=(pixel_area_MT_PO/PO_pixel_area)*100, normArea_MT_PO_byMT=(pixel_area_MT_PO/MT_pixel_area)*100, normArea_MT_LD_byMT=(pixel_area_MT_LD/MT_pixel_area)*100, normArea_MT_LD_byLD=(pixel_area_MT_LD/LD_pixel_area)*100,
          normArea_PO_ER_byER=(pixel_area_PO_ER/ER_pixel_area)*100, normArea_PO_ER_byPO=(pixel_area_PO_ER/PO_pixel_area)*100, normArea_PO_LD_byLD=(pixel_area_PO_LD/LD_pixel_area)*100, normArea_PO_LD_byPO=(pixel_area_PO_LD/PO_pixel_area)*100,
          
          normArea_LS_MT_byCY = (pixel_area_LS_MT/CY_pixel_area)*100, normArea_LS_GL_byCY = (pixel_area_LS_GL/CY_pixel_area)*100, normArea_LS_ER_byCY = (pixel_area_LS_ER/CY_pixel_area)*100, normArea_LS_PO_byCY = (pixel_area_LS_PO/CY_pixel_area)*100,normArea_LS_LD_byCY = (pixel_area_LS_LD/CY_pixel_area)*100,
          normArea_MT_GL_byCY = (pixel_area_MT_GL/CY_pixel_area)*100, normArea_MT_PO_byCY = (pixel_area_MT_PO/CY_pixel_area)*100, normArea_MT_ER_byCY = (pixel_area_MT_ER/CY_pixel_area)*100, normArea_MT_LD_byCY = (pixel_area_MT_LD/CY_pixel_area)*100,
          normArea_GL_PO_byCY = (pixel_area_GL_PO/CY_pixel_area)*100, normArea_GL_ER_byCY = (pixel_area_GL_ER/CY_pixel_area)*100, normArea_GL_LD_byCY = (pixel_area_GL_LD/CY_pixel_area)*100,
          normArea_PO_ER_byCY = (pixel_area_PO_ER/CY_pixel_area)*100, normArea_PO_LD_byCY = (pixel_area_PO_LD/CY_pixel_area)*100,
          normArea_ER_LD_byCY = (pixel_area_ER_LD/CY_pixel_area)*100,

          normArea_GL_ER_LD_byGL=(pixel_area_GL_ER_LD/GL_pixel_area)*100, normArea_GL_ER_LD_byER=(pixel_area_GL_ER_LD/ER_pixel_area)*100, normArea_GL_ER_LD_byLD=(pixel_area_GL_ER_LD/LD_pixel_area)*100, normArea_GL_ER_LD_byCY=(pixel_area_GL_ER_LD/CY_pixel_area)*100,
          normArea_GL_PO_ER_byGL=(pixel_area_GL_PO_ER/GL_pixel_area)*100, normArea_GL_PO_ER_byPO=(pixel_area_GL_PO_ER/PO_pixel_area)*100, normArea_GL_PO_ER_byER=(pixel_area_GL_PO_ER/ER_pixel_area)*100, normArea_GL_PO_ER_byCY=(pixel_area_GL_PO_ER/CY_pixel_area)*100,
          normArea_GL_PO_LD_byGL=(pixel_area_GL_PO_LD/GL_pixel_area)*100, normArea_GL_PO_LD_byPO=(pixel_area_GL_PO_LD/PO_pixel_area)*100, normArea_GL_PO_LD_byLD=(pixel_area_GL_PO_LD/LD_pixel_area)*100, normArea_GL_PO_LD_byCY=(pixel_area_GL_PO_LD/CY_pixel_area)*100,
          normArea_LS_ER_LD_byLS=(pixel_area_LS_ER_LD/LS_pixel_area)*100, normArea_LS_ER_LD_byER=(pixel_area_LS_ER_LD/ER_pixel_area)*100, normArea_LS_ER_LD_byLD=(pixel_area_LS_ER_LD/LD_pixel_area)*100, normArea_LS_ER_LD_byCY=(pixel_area_LS_ER_LD/CY_pixel_area)*100,
          normArea_LS_GL_LD_byLS=(pixel_area_LS_GL_LD/LS_pixel_area)*100, normArea_LS_GL_LD_byGL=(pixel_area_LS_GL_LD/GL_pixel_area)*100, normArea_LS_GL_LD_byLD=(pixel_area_LS_GL_LD/LD_pixel_area)*100, normArea_LS_GL_LD_byCY=(pixel_area_LS_GL_LD/CY_pixel_area)*100,
          normArea_LS_GL_ER_byLS=(pixel_area_LS_GL_LD/LS_pixel_area)*100, normArea_LS_GL_ER_byGL=(pixel_area_LS_GL_LD/GL_pixel_area)*100, normArea_LS_GL_ER_byER=(pixel_area_LS_GL_LD/ER_pixel_area)*100, normArea_LS_GL_ER_byCY=(pixel_area_LS_GL_LD/CY_pixel_area)*100,
          normArea_LS_GL_PO_byLS=(pixel_area_LS_GL_PO/LS_pixel_area)*100, normArea_LS_GL_PO_byGL=(pixel_area_LS_GL_PO/GL_pixel_area)*100, normArea_LS_GL_PO_byPO=(pixel_area_LS_GL_PO/PO_pixel_area)*100, normArea_LS_GL_PO_byCY=(pixel_area_LS_GL_PO/CY_pixel_area)*100,
          normArea_LS_MT_ER_byLS=(pixel_area_LS_MT_ER/LS_pixel_area)*100, normArea_LS_MT_ER_byMT=(pixel_area_LS_MT_ER/MT_pixel_area)*100, normArea_LS_MT_ER_byER=(pixel_area_LS_MT_ER/ER_pixel_area)*100, normArea_LS_MT_ER_byCY=(pixel_area_LS_MT_ER/CY_pixel_area)*100,
          normArea_LS_MT_GL_byLS=(pixel_area_LS_MT_GL/LS_pixel_area)*100, normArea_LS_MT_GL_byMT=(pixel_area_LS_MT_GL/MT_pixel_area)*100, normArea_LS_MT_GL_byGL=(pixel_area_LS_MT_GL/GL_pixel_area)*100, normArea_LS_MT_GL_byCY=(pixel_area_LS_MT_GL/CY_pixel_area)*100,
          normArea_LS_MT_LD_byLS=(pixel_area_LS_MT_LD/LS_pixel_area)*100, normArea_LS_MT_LD_byMT=(pixel_area_LS_MT_LD/MT_pixel_area)*100, normArea_LS_MT_LD_byLD=(pixel_area_LS_MT_LD/LD_pixel_area)*100, normArea_LS_MT_LD_byCY=(pixel_area_LS_MT_LD/CY_pixel_area)*100,
          normArea_LS_MT_PO_byLS=(pixel_area_LS_MT_PO/LS_pixel_area)*100, normArea_LS_MT_PO_byMT=(pixel_area_LS_MT_PO/MT_pixel_area)*100, normArea_LS_MT_PO_byPO=(pixel_area_LS_MT_PO/PO_pixel_area)*100, normArea_LS_MT_PO_byCY=(pixel_area_LS_MT_PO/CY_pixel_area)*100,
          normArea_LS_PO_ER_byLS=(pixel_area_LS_PO_ER/LS_pixel_area)*100, normArea_LS_PO_ER_byPO=(pixel_area_LS_PO_ER/PO_pixel_area)*100, normArea_LS_PO_ER_byER=(pixel_area_LS_PO_ER/ER_pixel_area)*100, normArea_LS_PO_ER_byCY=(pixel_area_LS_PO_ER/CY_pixel_area)*100,
          normArea_LS_PO_LD_byLS=(pixel_area_LS_PO_LD/LS_pixel_area)*100, normArea_LS_PO_LD_byPO=(pixel_area_LS_PO_LD/PO_pixel_area)*100, normArea_LS_PO_LD_byLD=(pixel_area_LS_PO_LD/LD_pixel_area)*100, normArea_LS_PO_LD_byCY=(pixel_area_LS_PO_LD/CY_pixel_area)*100,
          normArea_MT_ER_LD_byMT=(pixel_area_MT_ER_LD/MT_pixel_area)*100, normArea_MT_ER_LD_byER=(pixel_area_MT_ER_LD/ER_pixel_area)*100, normArea_MT_ER_LD_byLD=(pixel_area_MT_ER_LD/LD_pixel_area)*100, normArea_MT_ER_LD_byCY=(pixel_area_MT_ER_LD/CY_pixel_area)*100,
          normArea_MT_GL_ER_byMT=(pixel_area_MT_GL_ER/MT_pixel_area)*100, normArea_MT_GL_ER_byGL=(pixel_area_MT_GL_ER/GL_pixel_area)*100, normArea_MT_GL_ER_byER=(pixel_area_MT_GL_ER/ER_pixel_area)*100, normArea_MT_GL_ER_byCY=(pixel_area_MT_GL_ER/CY_pixel_area)*100,
          normArea_MT_GL_LD_byMT=(pixel_area_MT_GL_LD/MT_pixel_area)*100, normArea_MT_GL_LD_byGL=(pixel_area_MT_GL_LD/GL_pixel_area)*100, normArea_MT_GL_LD_byLD=(pixel_area_MT_GL_LD/LD_pixel_area)*100, normArea_MT_GL_LD_byCY=(pixel_area_MT_GL_LD/CY_pixel_area)*100,
          normArea_MT_GL_PO_byMT=(pixel_area_MT_GL_PO/MT_pixel_area)*100, normArea_MT_GL_PO_byGL=(pixel_area_MT_GL_PO/GL_pixel_area)*100, normArea_MT_GL_PO_byPO=(pixel_area_MT_GL_PO/PO_pixel_area)*100, normArea_MT_GL_PO_byCY=(pixel_area_MT_GL_PO/CY_pixel_area)*100,
          normArea_MT_PO_ER_byMT=(pixel_area_MT_PO_ER/MT_pixel_area)*100, normArea_MT_PO_ER_byPO=(pixel_area_MT_PO_ER/PO_pixel_area)*100, normArea_MT_PO_ER_byER=(pixel_area_MT_PO_ER/ER_pixel_area)*100, normArea_MT_PO_ER_byCY=(pixel_area_MT_PO_ER/CY_pixel_area)*100,
          normArea_MT_PO_LD_byMT=(pixel_area_MT_PO_LD/MT_pixel_area)*100, normArea_MT_PO_LD_byPO=(pixel_area_MT_PO_LD/PO_pixel_area)*100, normArea_MT_PO_LD_byLD=(pixel_area_MT_PO_LD/LD_pixel_area)*100, normArea_MT_PO_LD_byCY=(pixel_area_MT_PO_LD/CY_pixel_area)*100,
          normArea_PO_ER_LD_byPO=(pixel_area_PO_ER_LD/PO_pixel_area)*100, normArea_PO_ER_LD_byER=(pixel_area_PO_ER_LD/ER_pixel_area)*100, normArea_PO_ER_LD_byLD=(pixel_area_PO_ER_LD/LD_pixel_area)*100, normArea_PO_ER_LD_byCY=(pixel_area_PO_ER_LD/CY_pixel_area)*100,
          
          normArea_GL_PO_ER_LD_byGL=(pixel_area_GL_PO_ER_LD/GL_pixel_area)*100, normArea_GL_PO_ER_LD_byPO=(pixel_area_GL_PO_ER_LD/PO_pixel_area)*100, normArea_GL_PO_ER_LD_byER=(pixel_area_GL_PO_ER_LD/ER_pixel_area)*100, normArea_GL_PO_ER_LD_byLD=(pixel_area_GL_PO_ER_LD/LD_pixel_area)*100, normArea_GL_PO_ER_LD_byCY=(pixel_area_GL_PO_ER_LD/CY_pixel_area)*100,
          normArea_LS_MT_GL_ER_byLS=(pixel_area_LS_MT_GL_ER/LS_pixel_area)*100, normArea_LS_MT_GL_ER_byMT=(pixel_area_LS_MT_GL_ER/MT_pixel_area)*100, normArea_LS_MT_GL_ER_byGL=(pixel_area_LS_MT_GL_ER/GL_pixel_area)*100, normArea_LS_MT_GL_ER_byER=(pixel_area_LS_MT_GL_ER/ER_pixel_area)*100, normArea_LS_MT_GL_ER_byCY=(pixel_area_LS_MT_GL_ER/CY_pixel_area)*100,
          normArea_LS_MT_GL_LD_byLS=(pixel_area_LS_MT_GL_LD/LS_pixel_area)*100, normArea_LS_MT_GL_LD_byMT=(pixel_area_LS_MT_GL_LD/MT_pixel_area)*100, normArea_LS_MT_GL_LD_byGL=(pixel_area_LS_MT_GL_LD/GL_pixel_area)*100, normArea_LS_MT_GL_LD_byLD=(pixel_area_LS_MT_GL_LD/LD_pixel_area)*100, normArea_LS_MT_GL_LD_byCY=(pixel_area_LS_MT_GL_LD/CY_pixel_area)*100,
          normArea_LS_MT_GL_PO_byLS=(pixel_area_LS_MT_GL_PO/LS_pixel_area)*100, normArea_LS_MT_GL_PO_byMT=(pixel_area_LS_MT_GL_PO/MT_pixel_area)*100, normArea_LS_MT_GL_PO_byGL=(pixel_area_LS_MT_GL_PO/GL_pixel_area)*100, normArea_LS_MT_GL_PO_byPO=(pixel_area_LS_MT_GL_PO/PO_pixel_area)*100, normArea_LS_MT_GL_PO_byCY=(pixel_area_LS_MT_GL_PO/CY_pixel_area)*100,
          normArea_LS_MT_PO_ER_byLS=(pixel_area_LS_MT_PO_ER/LS_pixel_area)*100, normArea_LS_MT_PO_ER_byMT=(pixel_area_LS_MT_PO_ER/MT_pixel_area)*100, normArea_LS_MT_PO_ER_byPO=(pixel_area_LS_MT_PO_ER/PO_pixel_area)*100, normArea_LS_MT_PO_ER_byER=(pixel_area_LS_MT_PO_ER/ER_pixel_area)*100, normArea_LS_MT_PO_ER_byCY=(pixel_area_LS_MT_PO_ER/CY_pixel_area)*100,
          normArea_LS_MT_PO_LD_byLS=(pixel_area_LS_MT_PO_ER/LS_pixel_area)*100, normArea_LS_MT_PO_LD_byMT=(pixel_area_LS_MT_PO_ER/MT_pixel_area)*100, normArea_LS_MT_PO_LD_byPO=(pixel_area_LS_MT_PO_ER/PO_pixel_area)*100, normArea_LS_MT_PO_LD_byLD=(pixel_area_LS_MT_PO_LD/LD_pixel_area)*100, normArea_LS_MT_PO_LD_byCY=(pixel_area_LS_MT_PO_LD/CY_pixel_area)*100,
          normArea_MT_GL_ER_LD_byMT=(pixel_area_MT_GL_ER_LD/MT_pixel_area)*100, normArea_MT_GL_ER_LD_byGL=(pixel_area_MT_GL_ER_LD/GL_pixel_area)*100, normArea_MT_GL_ER_LD_byER=(pixel_area_MT_GL_ER_LD/ER_pixel_area)*100, normArea_MT_GL_ER_LD_byLD=(pixel_area_MT_GL_ER_LD/LD_pixel_area)*100, normArea_MT_GL_ER_LD_byCY=(pixel_area_MT_GL_ER_LD/CY_pixel_area)*100,
          normArea_MT_PO_ER_LD_byMT=(pixel_area_MT_PO_ER_LD/MT_pixel_area)*100, normArea_MT_PO_ER_LD_byPO=(pixel_area_MT_PO_ER_LD/PO_pixel_area)*100, normArea_MT_PO_ER_LD_byER=(pixel_area_MT_PO_ER_LD/ER_pixel_area)*100, normArea_MT_PO_ER_LD_byLD=(pixel_area_MT_PO_ER_LD/LD_pixel_area)*100, normArea_MT_PO_ER_LD_byCY=(pixel_area_MT_PO_ER_LD/CY_pixel_area)*100,
          normArea_MT_GL_PO_ER_byMT=(pixel_area_MT_GL_PO_ER/MT_pixel_area)*100, normArea_MT_GL_PO_ER_byGL=(pixel_area_MT_GL_PO_ER/GL_pixel_area)*100, normArea_MT_GL_PO_ER_byPO=(pixel_area_MT_GL_PO_ER/PO_pixel_area)*100, normArea_MT_GL_PO_ER_byER=(pixel_area_MT_GL_PO_ER/ER_pixel_area)*100, normArea_MT_GL_PO_ER_byCY=(pixel_area_MT_GL_PO_ER/CY_pixel_area)*100,
          normArea_MT_GL_PO_LD_byMT=(pixel_area_MT_GL_PO_LD/MT_pixel_area)*100, normArea_MT_GL_PO_LD_byGL=(pixel_area_MT_GL_PO_LD/GL_pixel_area)*100, normArea_MT_GL_PO_LD_byPO=(pixel_area_MT_GL_PO_LD/PO_pixel_area)*100, normArea_MT_GL_PO_LD_byLD=(pixel_area_MT_GL_PO_LD/LD_pixel_area)*100, normArea_MT_GL_PO_LD_byCY=(pixel_area_MT_GL_PO_LD/CY_pixel_area)*100,
          
          normArea_LS_MT_GL_ER_LD_byLS=(pixel_area_LS_MT_GL_ER_LD/LS_pixel_area)*100, normArea_LS_MT_GL_ER_LD_byMT=(pixel_area_LS_MT_GL_ER_LD/MT_pixel_area)*100, normArea_LS_MT_GL_ER_LD_byGL=(pixel_area_LS_MT_GL_ER_LD/GL_pixel_area)*100, normArea_LS_MT_GL_ER_LD_byER=(pixel_area_LS_MT_GL_ER_LD/ER_pixel_area)*100, normArea_LS_MT_GL_ER_LD_byLD=(pixel_area_LS_MT_GL_ER_LD/LD_pixel_area)*100, normArea_LS_MT_GL_ER_LD_byCY=(pixel_area_LS_MT_GL_ER_LD/CY_pixel_area)*100,
          normArea_LS_MT_GL_PO_ER_byLS=(pixel_area_LS_MT_GL_PO_ER/LS_pixel_area)*100, normArea_LS_MT_GL_PO_ER_byMT=(pixel_area_LS_MT_GL_PO_ER/MT_pixel_area)*100, normArea_LS_MT_GL_PO_ER_byGL=(pixel_area_LS_MT_GL_PO_ER/GL_pixel_area)*100, normArea_LS_MT_GL_PO_ER_byPO=(pixel_area_LS_MT_GL_PO_ER/PO_pixel_area)*100, normArea_LS_MT_GL_PO_ER_byER=(pixel_area_LS_MT_GL_PO_ER/ER_pixel_area)*100, normArea_LS_MT_GL_PO_ER_byCY=(pixel_area_LS_MT_GL_PO_ER/CY_pixel_area)*100,
          normArea_LS_MT_GL_PO_LD_byLS=(pixel_area_LS_MT_GL_PO_LD/LS_pixel_area)*100, normArea_LS_MT_GL_PO_LD_byMT=(pixel_area_LS_MT_GL_PO_LD/MT_pixel_area)*100, normArea_LS_MT_GL_PO_LD_byGL=(pixel_area_LS_MT_GL_PO_LD/GL_pixel_area)*100, normArea_LS_MT_GL_PO_LD_byPO=(pixel_area_LS_MT_GL_PO_LD/PO_pixel_area)*100, normArea_LS_MT_GL_PO_LD_byLD=(pixel_area_LS_MT_GL_PO_LD/LD_pixel_area)*100, normArea_LS_MT_GL_PO_LD_byCY=(pixel_area_LS_MT_GL_PO_LD/CY_pixel_area)*100,
          normArea_MT_GL_PO_ER_LD_byMT=(pixel_area_MT_GL_PO_ER_LD/MT_pixel_area)*100, normArea_MT_GL_PO_ER_LD_byGL=(pixel_area_MT_GL_PO_ER_LD/GL_pixel_area)*100, normArea_MT_GL_PO_ER_LD_byPO=(pixel_area_MT_GL_PO_ER_LD/PO_pixel_area)*100, normArea_MT_GL_PO_ER_LD_byER=(pixel_area_MT_GL_PO_ER_LD/ER_pixel_area)*100, normArea_MT_GL_PO_ER_LD_byLD=(pixel_area_MT_GL_PO_ER_LD/LD_pixel_area)*100, normArea_MT_GL_PO_ER_LD_byCY=(pixel_area_MT_GL_PO_ER_LD/CY_pixel_area)*100,
          
          normArea_LS_MT_GL_PO_ER_LD_byLS=(pixel_area_LS_MT_GL_PO_ER_LD/LS_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byMT=(pixel_area_LS_MT_GL_PO_ER_LD/MT_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byGL=(pixel_area_LS_MT_GL_PO_ER_LD/GL_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byPO=(pixel_area_LS_MT_GL_PO_ER_LD/PO_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byER=(pixel_area_LS_MT_GL_PO_ER_LD/ER_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byLD=(pixel_area_LS_MT_GL_PO_ER_LD/LD_pixel_area)*100, normArea_LS_MT_GL_PO_ER_LD_byCY=(pixel_area_LS_MT_GL_PO_ER_LD/CY_pixel_area)*100,
          
          number_all_2MCS= (Count_LS_MT + Count_LS_GL + Count_LS_PO + Count_LS_ER + Count_LS_LD + Count_MT_GL + Count_MT_PO + Count_MT_ER + Count_MT_LD + Count_GL_PO + Count_GL_ER + Count_GL_LD + Count_PO_ER + Count_PO_LD + Count_ER_LD),
          number_all_3MCS= (Count_GL_ER_LD + Count_GL_PO_ER + Count_GL_PO_LD + Count_LS_ER_LD + Count_LS_GL_LD + Count_LS_GL_LD + Count_LS_GL_PO + Count_LS_MT_ER + Count_LS_MT_GL + Count_LS_MT_LD + Count_LS_MT_PO + Count_LS_PO_ER + Count_LS_PO_LD + Count_MT_ER_LD + Count_MT_GL_ER + Count_MT_GL_LD + Count_MT_GL_PO + Count_MT_PO_ER + Count_MT_PO_LD + Count_PO_ER_LD),
          number_all_4MCS= (Count_GL_PO_ER_LD + Count_LS_MT_GL_ER + Count_LS_MT_GL_LD + Count_LS_MT_GL_PO + Count_LS_MT_PO_ER + Count_LS_MT_PO_LD + Count_MT_GL_ER_LD + Count_MT_GL_PO_ER + Count_MT_GL_PO_LD + Count_MT_PO_ER_LD),
          number_all_5MCS= (Count_LS_MT_GL_ER_LD + Count_LS_MT_GL_PO_ER + Count_LS_MT_GL_PO_LD + Count_MT_GL_PO_ER_LD),
          number_all_6MCS= (Count_LS_MT_GL_PO_ER_LD)
          ) %>%
  select(group, ImageNumber, 
            normArea_LS_ER_byER, normArea_LS_ER_byLS, normArea_LS_MT_byLS, normArea_LS_MT_byMT, normArea_LS_GL_byGL, normArea_LS_GL_byLS, normArea_LS_GL_byGL, normArea_LS_PO_byLS, normArea_LS_PO_byPO, normArea_LS_LD_byLS, normArea_LS_LD_byLD,
            normArea_GL_ER_byGL, normArea_GL_ER_byER, normArea_GL_PO_byGL, normArea_GL_PO_byPO, normArea_GL_LD_byGL, normArea_GL_LD_byLD,
            normArea_MT_ER_byER, normArea_MT_ER_byMT, normArea_MT_GL_byGL, normArea_MT_GL_byMT, normArea_MT_PO_byPO, normArea_MT_PO_byMT, normArea_MT_LD_byMT, normArea_MT_LD_byLD,
            normArea_PO_ER_byER, normArea_PO_ER_byPO, normArea_PO_LD_byLD, normArea_PO_LD_byPO,
            
            normArea_GL_ER_LD_byGL, normArea_GL_ER_LD_byER, normArea_GL_ER_LD_byLD,
            normArea_GL_PO_ER_byGL, normArea_GL_PO_ER_byPO, normArea_GL_PO_ER_byER,
            normArea_GL_PO_LD_byGL, normArea_GL_PO_LD_byPO, normArea_GL_PO_LD_byLD,
            normArea_LS_ER_LD_byLS, normArea_LS_ER_LD_byER, normArea_LS_ER_LD_byLD,
            normArea_LS_GL_LD_byLS, normArea_LS_GL_LD_byGL, normArea_LS_GL_LD_byLD,
            normArea_LS_GL_ER_byLS, normArea_LS_GL_ER_byGL, normArea_LS_GL_ER_byER,
            normArea_LS_GL_PO_byLS, normArea_LS_GL_PO_byGL, normArea_LS_GL_PO_byPO,
            normArea_LS_MT_ER_byLS, normArea_LS_MT_ER_byMT, normArea_LS_MT_ER_byER,
            normArea_LS_MT_GL_byLS, normArea_LS_MT_GL_byMT, normArea_LS_MT_GL_byGL,
            normArea_LS_MT_LD_byLS, normArea_LS_MT_LD_byMT, normArea_LS_MT_LD_byLD,
            normArea_LS_MT_PO_byLS, normArea_LS_MT_PO_byMT, normArea_LS_MT_PO_byPO,
            normArea_LS_PO_ER_byLS, normArea_LS_PO_ER_byPO, normArea_LS_PO_ER_byER,
            normArea_LS_PO_LD_byLS, normArea_LS_PO_LD_byPO, normArea_LS_PO_LD_byLD,
            normArea_MT_ER_LD_byMT, normArea_MT_ER_LD_byER, normArea_MT_ER_LD_byLD,
            normArea_MT_GL_ER_byMT, normArea_MT_GL_ER_byGL, normArea_MT_GL_ER_byER,
            normArea_MT_GL_LD_byMT, normArea_MT_GL_LD_byGL, normArea_MT_GL_LD_byLD,
            normArea_MT_GL_PO_byMT, normArea_MT_GL_PO_byGL, normArea_MT_GL_PO_byPO,
            normArea_MT_PO_ER_byMT, normArea_MT_PO_ER_byPO, normArea_MT_PO_ER_byER,
            normArea_MT_PO_LD_byMT, normArea_MT_PO_LD_byPO, normArea_MT_PO_LD_byLD,
            normArea_PO_ER_LD_byPO, normArea_PO_ER_LD_byER, normArea_PO_ER_LD_byLD,
            
            normArea_GL_PO_ER_LD_byGL, normArea_GL_PO_ER_LD_byPO, normArea_GL_PO_ER_LD_byER, normArea_GL_PO_ER_LD_byLD,
            normArea_LS_MT_GL_ER_byLS, normArea_LS_MT_GL_ER_byMT, normArea_LS_MT_GL_ER_byGL, normArea_LS_MT_GL_ER_byER,
            normArea_LS_MT_GL_LD_byLS, normArea_LS_MT_GL_LD_byMT, normArea_LS_MT_GL_LD_byGL, normArea_LS_MT_GL_LD_byLD,
            normArea_LS_MT_GL_PO_byLS, normArea_LS_MT_GL_PO_byMT, normArea_LS_MT_GL_PO_byGL, normArea_LS_MT_GL_PO_byPO,
            normArea_LS_MT_PO_ER_byLS, normArea_LS_MT_PO_ER_byMT, normArea_LS_MT_PO_ER_byPO, normArea_LS_MT_PO_ER_byER,
            normArea_LS_MT_PO_LD_byLS, normArea_LS_MT_PO_LD_byMT, normArea_LS_MT_PO_LD_byPO, normArea_LS_MT_PO_LD_byLD,
            normArea_MT_GL_ER_LD_byMT, normArea_MT_GL_ER_LD_byGL, normArea_MT_GL_ER_LD_byER, normArea_MT_GL_ER_LD_byLD,
            normArea_MT_PO_ER_LD_byMT, normArea_MT_PO_ER_LD_byPO, normArea_MT_PO_ER_LD_byER, normArea_MT_PO_ER_LD_byLD,
            normArea_MT_GL_PO_ER_byMT, normArea_MT_GL_PO_ER_byGL, normArea_MT_GL_PO_ER_byPO, normArea_MT_GL_PO_ER_byER,
            normArea_MT_GL_PO_LD_byMT, normArea_MT_GL_PO_LD_byGL, normArea_MT_GL_PO_LD_byPO, normArea_MT_GL_PO_LD_byLD,
            
            
            normArea_LS_MT_GL_ER_LD_byLS, normArea_LS_MT_GL_ER_LD_byMT, normArea_LS_MT_GL_ER_LD_byGL, normArea_LS_MT_GL_ER_LD_byER, normArea_LS_MT_GL_ER_LD_byLD,
            normArea_LS_MT_GL_PO_ER_byLS, normArea_LS_MT_GL_PO_ER_byMT, normArea_LS_MT_GL_PO_ER_byGL, normArea_LS_MT_GL_PO_ER_byPO, normArea_LS_MT_GL_PO_ER_byER,
            normArea_LS_MT_GL_PO_LD_byLS, normArea_LS_MT_GL_PO_LD_byMT, normArea_LS_MT_GL_PO_LD_byGL, normArea_LS_MT_GL_PO_LD_byPO, normArea_LS_MT_GL_PO_LD_byLD, 
            normArea_MT_GL_PO_ER_LD_byMT, normArea_MT_GL_PO_ER_LD_byGL, normArea_MT_GL_PO_ER_LD_byPO, normArea_MT_GL_PO_ER_LD_byER, normArea_MT_GL_PO_ER_LD_byLD,
            
            normArea_LS_MT_GL_ER_LD_byLS, normArea_LS_MT_GL_ER_LD_byMT, normArea_LS_MT_GL_ER_LD_byGL, normArea_LS_MT_GL_ER_LD_byER, normArea_LS_MT_GL_ER_LD_byLD,
            normArea_LS_MT_GL_PO_ER_byLS, normArea_LS_MT_GL_PO_ER_byMT, normArea_LS_MT_GL_PO_ER_byGL, normArea_LS_MT_GL_PO_ER_byPO, normArea_LS_MT_GL_PO_ER_byER,
            normArea_LS_MT_GL_PO_LD_byLS, normArea_LS_MT_GL_PO_LD_byMT, normArea_LS_MT_GL_PO_LD_byGL, normArea_LS_MT_GL_PO_LD_byPO, normArea_LS_MT_GL_PO_LD_byLD,
            normArea_MT_GL_PO_ER_LD_byMT, normArea_MT_GL_PO_ER_LD_byGL, normArea_MT_GL_PO_ER_LD_byPO, normArea_MT_GL_PO_ER_LD_byER, normArea_MT_GL_PO_ER_LD_byLD,
            
            normArea_LS_MT_GL_PO_ER_LD_byLS,

            normArea_LS_MT_byCY , normArea_LS_GL_byCY , normArea_LS_ER_byCY, normArea_LS_PO_byCY, normArea_LS_LD_byCY,
            normArea_MT_GL_byCY , normArea_MT_PO_byCY , normArea_MT_ER_byCY, normArea_MT_LD_byCY,
            normArea_GL_PO_byCY , normArea_GL_ER_byCY , normArea_GL_LD_byCY,
            normArea_PO_ER_byCY , normArea_PO_LD_byCY ,
            normArea_ER_LD_byCY ,
            
            normArea_GL_ER_LD_byCY,
            normArea_GL_PO_ER_byCY,
            normArea_GL_PO_LD_byCY,
            normArea_LS_ER_LD_byCY,
            normArea_LS_GL_LD_byCY,
            normArea_LS_GL_ER_byCY,
            normArea_LS_GL_PO_byCY,
            normArea_LS_MT_ER_byCY,
            normArea_LS_MT_GL_byCY,
            normArea_LS_MT_LD_byCY,
            normArea_LS_MT_PO_byCY,
            normArea_LS_PO_ER_byCY,
            normArea_LS_PO_LD_byCY,
            normArea_MT_ER_LD_byCY,
            normArea_MT_GL_ER_byCY,
            normArea_MT_GL_LD_byCY,
            normArea_MT_GL_PO_byCY,
            normArea_MT_PO_ER_byCY,
            normArea_MT_PO_LD_byCY,
            normArea_PO_ER_LD_byCY,
            
            normArea_GL_PO_ER_LD_byCY,
            normArea_LS_MT_GL_ER_byCY,
            normArea_LS_MT_GL_LD_byCY,
            normArea_LS_MT_GL_PO_byCY,
            normArea_LS_MT_PO_ER_byCY,
            normArea_LS_MT_PO_LD_byCY,
            normArea_MT_GL_ER_LD_byCY,
            normArea_MT_PO_ER_LD_byCY,
            normArea_MT_GL_PO_ER_byCY,
            normArea_MT_GL_PO_LD_byCY,
            
            normArea_LS_MT_GL_ER_LD_byCY,
            normArea_LS_MT_GL_PO_ER_byCY,
            normArea_LS_MT_GL_PO_LD_byCY,
            normArea_MT_GL_PO_ER_LD_byCY,
            
            normArea_LS_MT_GL_PO_ER_LD_byCY,
         
         
         number_all_2MCS,
         number_all_3MCS,
         number_all_4MCS,
         number_all_5MCS,
         number_all_6MCS)




            #Neighbors and Fraction Touching
#Lysosomes neighbors
LS_neighbor <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_LS.csv` %>% 
  select(ImageNumber, Neighbors_PercentTouching_GL_Adjacent,	Neighbors_PercentTouching_ER_Adjacent,Neighbors_PercentTouching_LD_Adjacent, Neighbors_PercentTouching_MT_Adjacent, Neighbors_PercentTouching_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_LS_bdry_GL = mean(Neighbors_PercentTouching_GL_Adjacent), 
            mean_perc_LS_bdry_PO = mean(Neighbors_PercentTouching_PO_Adjacent),
            mean_perc_LS_bdry_MT = mean(Neighbors_PercentTouching_MT_Adjacent),
            mean_perc_PO_bdry_LD = mean(Neighbors_PercentTouching_LD_Adjacent),
            mean_perc_LS_bdry_ER = mean(Neighbors_PercentTouching_ER_Adjacent))

#Mitochondria neighbors
MT_neighbor <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_MT.csv` %>% 
  select(ImageNumber, Neighbors_PercentTouching_GL_Adjacent,	Neighbors_PercentTouching_ER_Adjacent, Neighbors_PercentTouching_LD_Adjacent, Neighbors_PercentTouching_LS_Adjacent, Neighbors_PercentTouching_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_MT_bdry_GL = mean(Neighbors_PercentTouching_GL_Adjacent), 
            mean_perc_MT_bdry_PO = mean(Neighbors_PercentTouching_PO_Adjacent),
            mean_perc_MT_bdry_LS = mean(Neighbors_PercentTouching_LS_Adjacent),
            mean_perc_PO_bdry_LD = mean(Neighbors_PercentTouching_LD_Adjacent),
            mean_perc_MT_bdry_ER = mean(Neighbors_PercentTouching_ER_Adjacent))

#Golgi neighbors
GL_neighbor <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_GL.csv` %>% 
  select(ImageNumber, Neighbors_PercentTouching_LS_Adjacent,	Neighbors_PercentTouching_ER_Adjacent, Neighbors_PercentTouching_LD_Adjacent, Neighbors_PercentTouching_MT_Adjacent, Neighbors_PercentTouching_PO_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_GL_bdry_LS = mean(Neighbors_PercentTouching_LS_Adjacent), 
            mean_perc_GL_bdry_PO = mean(Neighbors_PercentTouching_PO_Adjacent),
            mean_perc_GL_bdry_MT = mean(Neighbors_PercentTouching_MT_Adjacent),
            mean_perc_PO_bdry_LD = mean(Neighbors_PercentTouching_LD_Adjacent),
            mean_perc_GL_bdry_ER = mean(Neighbors_PercentTouching_ER_Adjacent))

#Peroxisomes neighbors
PO_neighbor <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_PO.csv` %>% 
  select(ImageNumber, Neighbors_PercentTouching_GL_Adjacent,Neighbors_PercentTouching_ER_Adjacent, Neighbors_PercentTouching_LD_Adjacent, Neighbors_PercentTouching_MT_Adjacent, Neighbors_PercentTouching_LS_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_PO_bdry_GL = mean(Neighbors_PercentTouching_GL_Adjacent), 
            mean_perc_PO_bdry_LS = mean(Neighbors_PercentTouching_LS_Adjacent),
            mean_perc_PO_bdry_MT = mean(Neighbors_PercentTouching_MT_Adjacent),
            mean_perc_PO_bdry_LD = mean(Neighbors_PercentTouching_LD_Adjacent),	
            mean_perc_PO_bdry_ER = mean(Neighbors_PercentTouching_ER_Adjacent))

#Endoplasmic Reticulum neighbors
ER_neighbor <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_ER.csv` %>% 
  select(ImageNumber, Neighbors_PercentTouching_GL_Adjacent, Neighbors_PercentTouching_LS_Adjacent, Neighbors_PercentTouching_MT_Adjacent, Neighbors_PercentTouching_PO_Adjacent, Neighbors_PercentTouching_LD_Adjacent) %>% group_by(ImageNumber) %>%
  summarize(mean_perc_ER_bdry_GL = mean(Neighbors_PercentTouching_GL_Adjacent), 
            mean_perc_ER_bdry_PO = mean(Neighbors_PercentTouching_PO_Adjacent), 
            mean_perc_ER_bdry_MT = mean(Neighbors_PercentTouching_MT_Adjacent),
            mean_perc_ER_bdry_LD = mean(Neighbors_PercentTouching_LD_Adjacent),
            mean_perc_ER_bdry_LS = mean(Neighbors_PercentTouching_LS_Adjacent))

Contacts_2MCS_Neighbors_data <- cbind(meta, ER, PO, GL, MT, LS, LD, organelle_2MCS_overlap, PO_neighbor, LS_neighbor, MT_neighbor, ER_neighbor, GL_neighbor) %>% subset(select = which(!duplicated(names(.)))) %>%
  mutate(perc_ER_ovlp_PO = (pixel_area_PO_ER/ER_pixel_area)*100,
         perc_ER_ovlp_GL = (pixel_area_GL_ER/ER_pixel_area)*100, 
         perc_ER_ovlp_MT = (pixel_area_MT_ER/ER_pixel_area)*100, 
         perc_ER_ovlp_LS = (pixel_area_LS_ER/ER_pixel_area)*100,
         perc_PO_ovlp_ER = (pixel_area_PO_ER/PO_pixel_area)*100,
         perc_PO_ovlp_GL = (pixel_area_GL_PO/PO_pixel_area)*100,
         perc_PO_ovlp_MT = (pixel_area_MT_PO/PO_pixel_area)*100,
         perc_PO_ovlp_LS = (pixel_area_LS_PO/PO_pixel_area)*100,
         perc_GL_ovlp_ER = (pixel_area_GL_ER/GL_pixel_area)*100,
         perc_GL_ovlp_PO = (pixel_area_GL_PO/GL_pixel_area)*100,
         perc_GL_ovlp_MT = (pixel_area_MT_GL/GL_pixel_area)*100,
         perc_GL_ovlp_LS = (pixel_area_LS_GL/GL_pixel_area)*100,
         perc_MT_ovlp_ER = (pixel_area_MT_ER/MT_pixel_area)*100, 
         perc_MT_ovlp_PO = (pixel_area_MT_PO/MT_pixel_area)*100,
         perc_MT_ovlp_GL = (pixel_area_MT_GL/MT_pixel_area)*100,
         perc_MT_ovlp_LS = (pixel_area_LS_MT/MT_pixel_area)*100,
         perc_LS_ovlp_ER = (pixel_area_LS_ER/LS_pixel_area)*100, 
         perc_LS_ovlp_PO = (pixel_area_LS_PO/LS_pixel_area)*100,
         perc_LS_ovlp_GL = (pixel_area_LS_GL/LS_pixel_area)*100,
         perc_LS_ovlp_MT = (pixel_area_LS_MT/LS_pixel_area)*100) %>% 
  select(group, ImageNumber,
         pixel_area_LS_MT, perc_LS_ovlp_MT, perc_MT_ovlp_LS, mean_perc_LS_bdry_MT, mean_perc_LS_bdry_MT,
         pixel_area_LS_GL, perc_LS_ovlp_GL, perc_GL_ovlp_LS, mean_perc_LS_bdry_GL, mean_perc_GL_bdry_LS,
         pixel_area_LS_PO, perc_LS_ovlp_PO, perc_PO_ovlp_LS, mean_perc_LS_bdry_PO, mean_perc_PO_bdry_LS,
         pixel_area_LS_ER, perc_LS_ovlp_ER, perc_ER_ovlp_LS, mean_perc_LS_bdry_ER, mean_perc_ER_bdry_LS,
         pixel_area_MT_GL, perc_MT_ovlp_GL, perc_GL_ovlp_MT, mean_perc_MT_bdry_GL, mean_perc_GL_bdry_MT,
         pixel_area_MT_PO, perc_MT_ovlp_PO, perc_PO_ovlp_MT, mean_perc_MT_bdry_PO, mean_perc_PO_bdry_MT,
         pixel_area_MT_ER, perc_MT_ovlp_ER, perc_ER_ovlp_MT, mean_perc_MT_bdry_ER, mean_perc_ER_bdry_MT,
         pixel_area_GL_PO, perc_GL_ovlp_PO, perc_PO_ovlp_GL, mean_perc_GL_bdry_PO, mean_perc_PO_bdry_GL,
         pixel_area_GL_ER, perc_GL_ovlp_ER, perc_ER_ovlp_GL, mean_perc_GL_bdry_ER, mean_perc_ER_bdry_GL,
         pixel_area_PO_ER, perc_PO_ovlp_ER, perc_ER_ovlp_PO, mean_perc_PO_bdry_ER, mean_perc_ER_bdry_PO)
            
  



          #Distribution individual organelle grouped into Peri-Nuclear, Central, Peri-Plasmamembrane:

IndividualOrganelle_Distribution_data <-  `MCZ_hiPSCsOleciAcid_CellP2D_06212022_SO.csv` %>%
  summarise(ER_FAD_bin_1 = RadialDistribution_FracAtD_ER_conv_1of6, ER_FAD_bin_2 = RadialDistribution_FracAtD_ER_conv_2of6,
            ER_FAD_bin_3 = RadialDistribution_FracAtD_ER_conv_3of6, ER_FAD_bin_4 = RadialDistribution_FracAtD_ER_conv_4of6,
            ER_FAD_bin_5 = RadialDistribution_FracAtD_ER_conv_5of6, ER_FAD_bin_6 = RadialDistribution_FracAtD_ER_conv_6of6,
            GL_FAD_bin_1 = RadialDistribution_FracAtD_GL_conv_1of6, GL_FAD_bin_2 = RadialDistribution_FracAtD_GL_conv_2of6,
            GL_FAD_bin_3 = RadialDistribution_FracAtD_GL_conv_3of6, GL_FAD_bin_4 = RadialDistribution_FracAtD_GL_conv_4of6,
            GL_FAD_bin_5 = RadialDistribution_FracAtD_GL_conv_5of6, GL_FAD_bin_6 = RadialDistribution_FracAtD_GL_conv_6of6,
            LS_FAD_bin_1 = RadialDistribution_FracAtD_LS_conv_1of6, LS_FAD_bin_2 = RadialDistribution_FracAtD_LS_conv_2of6,
            LS_FAD_bin_3 = RadialDistribution_FracAtD_LS_conv_3of6, LS_FAD_bin_4 = RadialDistribution_FracAtD_LS_conv_4of6,
            LS_FAD_bin_5 = RadialDistribution_FracAtD_LS_conv_5of6, LS_FAD_bin_6 = RadialDistribution_FracAtD_LS_conv_6of6,
            MT_FAD_bin_1 = RadialDistribution_FracAtD_MT_conv_1of6, MT_FAD_bin_2 = RadialDistribution_FracAtD_MT_conv_2of6,
            MT_FAD_bin_3 = RadialDistribution_FracAtD_MT_conv_3of6, MT_FAD_bin_4 = RadialDistribution_FracAtD_MT_conv_4of6,
            MT_FAD_bin_5 = RadialDistribution_FracAtD_MT_conv_5of6, MT_FAD_bin_6 = RadialDistribution_FracAtD_MT_conv_6of6,
            PO_FAD_bin_1 = RadialDistribution_FracAtD_PO_conv_1of6, PO_FAD_bin_2 = RadialDistribution_FracAtD_PO_conv_2of6,
            PO_FAD_bin_3 = RadialDistribution_FracAtD_PO_conv_3of6, PO_FAD_bin_4 = RadialDistribution_FracAtD_PO_conv_4of6,
            PO_FAD_bin_5 = RadialDistribution_FracAtD_PO_conv_5of6, PO_FAD_bin_6 = RadialDistribution_FracAtD_PO_conv_6of6,
            LD_FAD_bin_1 = RadialDistribution_FracAtD_LD_conv_1of6, LD_FAD_bin_2 = RadialDistribution_FracAtD_LD_conv_2of6,
            LD_FAD_bin_3 = RadialDistribution_FracAtD_LD_conv_3of6, LD_FAD_bin_4 = RadialDistribution_FracAtD_LD_conv_4of6,
            LD_FAD_bin_5 = RadialDistribution_FracAtD_LD_conv_5of6, LD_FAD_bin_6 = RadialDistribution_FracAtD_LD_conv_6of6) %>%
  mutate(ER_perinuclear = ER_FAD_bin_1+ER_FAD_bin_2, ER_central = ER_FAD_bin_3+ER_FAD_bin_4, ER_periplasm = ER_FAD_bin_5+ER_FAD_bin_6,
         MT_perinuclear = MT_FAD_bin_1+MT_FAD_bin_2, MT_central = MT_FAD_bin_3+MT_FAD_bin_4, MT_periplasm = MT_FAD_bin_5+MT_FAD_bin_6,
         PO_perinuclear = PO_FAD_bin_1+PO_FAD_bin_2, PO_central = PO_FAD_bin_3+PO_FAD_bin_4, PO_periplasm = PO_FAD_bin_5+PO_FAD_bin_6,
         GL_perinuclear = GL_FAD_bin_1+GL_FAD_bin_2, GL_central = GL_FAD_bin_3+GL_FAD_bin_4, GL_periplasm = GL_FAD_bin_5+GL_FAD_bin_6,
         LS_perinuclear = LS_FAD_bin_1+LS_FAD_bin_2, LS_central = LS_FAD_bin_3+LS_FAD_bin_4, LS_periplasm = LS_FAD_bin_5+LS_FAD_bin_6,
         LD_perinuclear = LD_FAD_bin_1+LD_FAD_bin_2, LD_central = LD_FAD_bin_3+LD_FAD_bin_4, LD_periplasm = LD_FAD_bin_5+LD_FAD_bin_6,) %>%
  cbind(meta$group, meta$ImageNumber)


                              #Distribution 2MCS contacts 

Contacts_2MCS_Distribution_data <- `MCZ_hiPSCsOleciAcid_CellP2D_06212022_SO.csv` %>% 
  summarise (LS_ER_FAD_bin_1=RadialDistribution_FracAtD_LS_ER_conv_1of6,
LS_ER_FAD_bin_2=RadialDistribution_FracAtD_LS_ER_conv_2of6,
LS_ER_FAD_bin_3=RadialDistribution_FracAtD_LS_ER_conv_3of6,
LS_ER_FAD_bin_4=RadialDistribution_FracAtD_LS_ER_conv_4of6,
LS_ER_FAD_bin_5=RadialDistribution_FracAtD_LS_ER_conv_5of6,
LS_ER_FAD_bin_6=RadialDistribution_FracAtD_LS_ER_conv_6of6,

LS_LD_FAD_bin_1=RadialDistribution_FracAtD_LS_ER_conv_1of6,
LS_LD_FAD_bin_2=RadialDistribution_FracAtD_LS_ER_conv_2of6,
LS_LD_FAD_bin_3=RadialDistribution_FracAtD_LS_ER_conv_3of6,
LS_LD_FAD_bin_4=RadialDistribution_FracAtD_LS_ER_conv_4of6,
LS_LD_FAD_bin_5=RadialDistribution_FracAtD_LS_ER_conv_5of6,
LS_LD_FAD_bin_6=RadialDistribution_FracAtD_LS_ER_conv_6of6,

LS_GL_FAD_bin_1=RadialDistribution_FracAtD_LS_GL_conv_1of6,
LS_GL_FAD_bin_2=RadialDistribution_FracAtD_LS_GL_conv_2of6,
LS_GL_FAD_bin_3=RadialDistribution_FracAtD_LS_GL_conv_3of6,
LS_GL_FAD_bin_4=RadialDistribution_FracAtD_LS_GL_conv_4of6,
LS_GL_FAD_bin_5=RadialDistribution_FracAtD_LS_GL_conv_5of6,
LS_GL_FAD_bin_6=RadialDistribution_FracAtD_LS_GL_conv_6of6,

LS_MT_FAD_bin_1=RadialDistribution_FracAtD_LS_MT_conv_1of6,
LS_MT_FAD_bin_2=RadialDistribution_FracAtD_LS_MT_conv_2of6,
LS_MT_FAD_bin_3=RadialDistribution_FracAtD_LS_MT_conv_3of6,
LS_MT_FAD_bin_4=RadialDistribution_FracAtD_LS_MT_conv_4of6,
LS_MT_FAD_bin_5=RadialDistribution_FracAtD_LS_MT_conv_5of6,
LS_MT_FAD_bin_6=RadialDistribution_FracAtD_LS_MT_conv_6of6,

LS_PO_FAD_bin_1=RadialDistribution_FracAtD_LS_PO_conv_1of6,
LS_PO_FAD_bin_2=RadialDistribution_FracAtD_LS_PO_conv_2of6,
LS_PO_FAD_bin_3=RadialDistribution_FracAtD_LS_PO_conv_3of6,
LS_PO_FAD_bin_4=RadialDistribution_FracAtD_LS_PO_conv_4of6,
LS_PO_FAD_bin_5=RadialDistribution_FracAtD_LS_PO_conv_5of6,
LS_PO_FAD_bin_6=RadialDistribution_FracAtD_LS_PO_conv_6of6,

MT_ER_FAD_bin_1=RadialDistribution_FracAtD_MT_ER_conv_1of6,
MT_ER_FAD_bin_2=RadialDistribution_FracAtD_MT_ER_conv_2of6,
MT_ER_FAD_bin_3=RadialDistribution_FracAtD_MT_ER_conv_3of6,
MT_ER_FAD_bin_4=RadialDistribution_FracAtD_MT_ER_conv_4of6,
MT_ER_FAD_bin_5=RadialDistribution_FracAtD_MT_ER_conv_5of6,
MT_ER_FAD_bin_6=RadialDistribution_FracAtD_MT_ER_conv_6of6,

MT_LD_FAD_bin_1=RadialDistribution_FracAtD_MT_ER_conv_1of6,
MT_LD_FAD_bin_2=RadialDistribution_FracAtD_MT_ER_conv_2of6,
MT_LD_FAD_bin_3=RadialDistribution_FracAtD_MT_ER_conv_3of6,
MT_LD_FAD_bin_4=RadialDistribution_FracAtD_MT_ER_conv_4of6,
MT_LD_FAD_bin_5=RadialDistribution_FracAtD_MT_ER_conv_5of6,
MT_LD_FAD_bin_6=RadialDistribution_FracAtD_MT_ER_conv_6of6,

MT_GL_FAD_bin_1=RadialDistribution_FracAtD_MT_GL_conv_1of6,
MT_GL_FAD_bin_2=RadialDistribution_FracAtD_MT_GL_conv_2of6,
MT_GL_FAD_bin_3=RadialDistribution_FracAtD_MT_GL_conv_3of6,
MT_GL_FAD_bin_4=RadialDistribution_FracAtD_MT_GL_conv_4of6,
MT_GL_FAD_bin_5=RadialDistribution_FracAtD_MT_GL_conv_5of6,
MT_GL_FAD_bin_6=RadialDistribution_FracAtD_MT_GL_conv_6of6,

MT_PO_FAD_bin_1=RadialDistribution_FracAtD_MT_PO_conv_1of6,
MT_PO_FAD_bin_2=RadialDistribution_FracAtD_MT_PO_conv_2of6,
MT_PO_FAD_bin_3=RadialDistribution_FracAtD_MT_PO_conv_3of6,
MT_PO_FAD_bin_4=RadialDistribution_FracAtD_MT_PO_conv_4of6,
MT_PO_FAD_bin_5=RadialDistribution_FracAtD_MT_PO_conv_5of6,
MT_PO_FAD_bin_6=RadialDistribution_FracAtD_MT_PO_conv_6of6,

PO_ER_FAD_bin_1=RadialDistribution_FracAtD_PO_ER_conv_1of6,
PO_ER_FAD_bin_2=RadialDistribution_FracAtD_PO_ER_conv_2of6,
PO_ER_FAD_bin_3=RadialDistribution_FracAtD_PO_ER_conv_3of6,
PO_ER_FAD_bin_4=RadialDistribution_FracAtD_PO_ER_conv_4of6,
PO_ER_FAD_bin_5=RadialDistribution_FracAtD_PO_ER_conv_5of6,
PO_ER_FAD_bin_6=RadialDistribution_FracAtD_PO_ER_conv_6of6,

PO_LD_FAD_bin_1=RadialDistribution_FracAtD_PO_ER_conv_1of6,
PO_LD_FAD_bin_2=RadialDistribution_FracAtD_PO_ER_conv_2of6,
PO_LD_FAD_bin_3=RadialDistribution_FracAtD_PO_ER_conv_3of6,
PO_LD_FAD_bin_4=RadialDistribution_FracAtD_PO_ER_conv_4of6,
PO_LD_FAD_bin_5=RadialDistribution_FracAtD_PO_ER_conv_5of6,
PO_LD_FAD_bin_6=RadialDistribution_FracAtD_PO_ER_conv_6of6,

GL_LD_FAD_bin_1=RadialDistribution_FracAtD_PO_ER_conv_1of6,
GL_LD_FAD_bin_2=RadialDistribution_FracAtD_PO_ER_conv_2of6,
GL_LD_FAD_bin_3=RadialDistribution_FracAtD_PO_ER_conv_3of6,
GL_LD_FAD_bin_4=RadialDistribution_FracAtD_PO_ER_conv_4of6,
GL_LD_FAD_bin_5=RadialDistribution_FracAtD_PO_ER_conv_5of6,
GL_LD_FAD_bin_6=RadialDistribution_FracAtD_PO_ER_conv_6of6) %>%
  mutate(LS_ER_perinuclear = LS_ER_FAD_bin_1 +  LS_ER_FAD_bin_2, LS_ER_central= LS_ER_FAD_bin_3 +  LS_ER_FAD_bin_4, LS_ER_periplasma= LS_ER_FAD_bin_5 +  LS_ER_FAD_bin_6,
         LS_LD_perinuclear = LS_LD_FAD_bin_1 +  LS_LD_FAD_bin_2, LS_LD_central= LS_LD_FAD_bin_3 +  LS_LD_FAD_bin_4, LS_ER_periplasma= LS_LD_FAD_bin_5 +  LS_LD_FAD_bin_6,
         LS_GL_perinuclear = LS_GL_FAD_bin_1 +  LS_GL_FAD_bin_2, LS_GL_central= LS_GL_FAD_bin_3 +  LS_GL_FAD_bin_4, LS_ER_periplasma= LS_GL_FAD_bin_5 +  LS_GL_FAD_bin_6,
         LS_MT_perinuclear = LS_MT_FAD_bin_1 +  LS_MT_FAD_bin_2, LS_MT_central= LS_MT_FAD_bin_3 +  LS_MT_FAD_bin_4, LS_MT_periplasma= LS_MT_FAD_bin_5 +  LS_MT_FAD_bin_6,
         LS_PO_perinuclear = LS_PO_FAD_bin_1 +  LS_PO_FAD_bin_2, LS_PO_central= LS_PO_FAD_bin_3 +  LS_PO_FAD_bin_4, LS_PO_periplasma= LS_PO_FAD_bin_5 +  LS_PO_FAD_bin_6,
         MT_ER_perinuclear = MT_ER_FAD_bin_1 +  MT_ER_FAD_bin_2, MT_ER_central= MT_ER_FAD_bin_3 +  MT_ER_FAD_bin_4, MT_ER_periplasma= MT_ER_FAD_bin_5 +  MT_ER_FAD_bin_6,
         MT_LD_perinuclear = MT_LD_FAD_bin_1 +  MT_LD_FAD_bin_2, MT_LD_central= MT_LD_FAD_bin_3 +  MT_LD_FAD_bin_4, MT_LD_periplasma= MT_LD_FAD_bin_5 +  MT_LD_FAD_bin_6,
         MT_GL_perinuclear = MT_GL_FAD_bin_1 +  MT_GL_FAD_bin_2, MT_GL_central= MT_GL_FAD_bin_3 +  MT_GL_FAD_bin_4, MT_GL_periplasma= MT_GL_FAD_bin_5 +  MT_GL_FAD_bin_6,
         MT_PO_perinuclear = MT_PO_FAD_bin_1 +  MT_PO_FAD_bin_2, MT_PO_central= MT_PO_FAD_bin_3 +  MT_PO_FAD_bin_4, MT_PO_periplasma= MT_PO_FAD_bin_5 +  MT_PO_FAD_bin_6,
         PO_ER_perinuclear = PO_ER_FAD_bin_1 +  PO_ER_FAD_bin_2, PO_ER_central= PO_ER_FAD_bin_3 +  PO_ER_FAD_bin_4, PO_ER_periplasma= PO_ER_FAD_bin_5 +  PO_ER_FAD_bin_6,
         PO_LD_perinuclear = PO_LD_FAD_bin_1 +  PO_LD_FAD_bin_2, PO_LD_central= PO_LD_FAD_bin_3 +  PO_LD_FAD_bin_4, PO_LD_periplasma= PO_LD_FAD_bin_5 +  PO_LD_FAD_bin_6,
         GL_LD_perinuclear = GL_LD_FAD_bin_1 +  GL_LD_FAD_bin_2, GL_LD_central= GL_LD_FAD_bin_3 +  GL_LD_FAD_bin_4, GL_LD_periplasma= GL_LD_FAD_bin_5 +  GL_LD_FAD_bin_6) %>%
  cbind(meta$group, meta$ImageNumber)


#zscores
#numeric_organelle <- select_if(organelle_data, is.numeric) %>% select(-ImageNumber)
#numeric_neighbor <- select_if(ContactsFraction_data, is.numeric) %>% select(-ImageNumber)
#organelle_zscores <- scale(numeric_organelle) %>% cbind(meta)
#neighbor_zscores <- scale(numeric_neighbor) %>% cbind(meta)





#export the files, change the name if needed
dir.create(file.path('MCZ_06232022_cleandata'))
write.csv(IndividualOrganelle_SizeAndShape_NormAllAreas_data, "./MCZ_06232022_cleandata/IndividualOrganelle_SizeAndShape_NormAllAreas_data.csv", row.names = FALSE)
write.csv(Contacts_2MCS_Neighbors_data, "./MCZ_06232022_cleandata/Contacts_2MCS_Neighbors_data.csv", row.names = FALSE)
#write.csv(organelle_zscores, "./MCZ_05162022_cleandata/organelle_zscores_MCZ_05162022.csv", row.names = FALSE)
#write.csv(neighbor_zscores, "./MCZ_05162022_cleandata/neighbor_zscores_MCZ_05162022.csv", row.names = FALSE)
write.csv(IndividualOrganelle_Distribution_data, "./MCZ_06232022_cleandata/IndividualOrganelle_Distribution_data.csv", row.names = FALSE)
write.csv(Contacts_2MCS_Distribution_data, "./MCZ_06232022_cleandata/Contacts_2MCS_Distribution_data.csv", row.names = FALSE)
write.csv(IndividualOrganelle_Proximity_data, "./MCZ_06232022_cleandata/IndividualOrganelle_Proximity_data.csv", row.names = FALSE)
write.csv(organelle_2MCS_overlap, "./MCZ_06232022_cleandata/organelle_2MCS_overlap_05162022.csv", row.names = FALSE)
write.csv(organelle_3MCS_overlap, "./MCZ_06232022_cleandata/organelle_3MCS_overlap_05162022.csv", row.names = FALSE)
write.csv(organelle_4MCS_overlap, "./MCZ_06232022_cleandata/organelle_4MCS_overlap_05162022.csv", row.names = FALSE)
write.csv(organelle_5MCS_overlap, "./MCZ_06232022_cleandata/organelle_5MCS_overlap_05162022.csv", row.names = FALSE)
write.csv(organelle_6MCS_overlap, "./MCZ_06232022_cleandata/organelle_6MCS_overlap_05162022.csv", row.names = FALSE)
write.csv(Contacts_allMCS_overlap_data, "./MCZ_06232022_cleandata/Contacts_allMCS_overlap_data.csv", row.names = FALSE)
