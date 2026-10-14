################################################################################
###
###         Dr. Marco Baldo, MSc
###             07-10-2026
### 
### Reference:
###
### Contact: Marco Baldo (baldo@fld.czu.cz - bldmrc92@gmail.com)
###
################################################################################



#===============================================================================
#                  # ANALYSIS FOR APPENDIX #
#
#         REQUIRED TO UPLOAD THE DATABASE RELATED TO THE ANALYSIS
#         THEY ARE AVAILABLE IN THIS ZENODO DATA REPOSITORY : https://zenodo.org/records/23220120
#
#         VERY IMPORTANT (note there are 2 climate scenarios for the same dataframe names):
#         
#         lnd_scen.rds
#         removals.rds
#         bayesian_results_all_filtered.rds
#         Bayesian_BDV_model_V3_multi.rds
#
#
#         Official_table_of_predictors_and_sp_rich_for_the_Bayesian_models.xlsx
#         20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes_Copy.xlsx (just about forest category names, you can use the previous excel table and align with the names there in forest categories)
#===============================================================================


library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(RSQLite)
library(vegan)
library(fields)

# ============================================================
# UPLOAD THE DATABASE REQUIRED FOR THE ANALYSIS
# ============================================================

# Upload required database. Permanent repository here: https://zenodo.org/records/23220120 - Data & Script - SD2 
# Example for the climate series 1961-1990

# Volume - Harvest
lnd_scen <- readRDS("C:/Users/baldo/Desktop/Baldo_pen_1 (128)/Pen-128/Samsung USB/Article_submission/Baldo et al., 2026a_JAE/Submission_3/Data & Scripts/SD2/lnd_scen.rds")
removals <- readRDS("C:/Users/baldo/Desktop/Baldo_pen_1 (128)/Pen-128/Samsung USB/Article_submission/Baldo et al., 2026a_JAE/Submission_3/Data & Scripts/SD2/removals.rds")

# Bayesian
Bayesian_BDV_model_V3_multi_1961_1990 <- readRDS("C:/Users/baldo/Desktop/Baldo_pen_1 (128)/Pen-128/Samsung USB/Article_submission/Baldo et al., 2026a_JAE/Submission_3/Data & Scripts/SD2/Bayesian_BDV_model_V3_multi_1961_1990.rds")
bayesian_results_all_1961_1990 <- readRDS("C:/Users/baldo/Desktop/Baldo_pen_1 (128)/Pen-128/Samsung USB/Article_submission/Baldo et al., 2026a_JAE/Submission_3/Data & Scripts/SD2/bayesian_results_all_1961_1990.rds")

# BDV predictors by Jenik
BDV_predictors <- read_excel("C:/iLand/2023/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/Official_table_of_predictors_and_sp_rich_for_the_Bayesian_models.xlsx")

BDV_predictors <- BDV_predictors%>%
  group_by(plotID)%>%
  mutate(deadwood50 = (DeadC/4))


# ============================================================
# FILTER AND RENAME FOREST PLOTS
# ============================================================

# OLD-GROWTH PLOTS TO REMOVE
old_growth <- c(
  "L1_18", "L1_22", "L1_24", "L1_26",
  "L2_27", "L2_30", "L2_33", "L2_34",
  "L3_06", "L3_09", "L3_10", "L3_16",
  "L4_06", "L4_09", "L4_31", "L4_33",
  "L6_05", "L6_08", "L6_10"
)


# ------------------------------------------------------------
# FOREST CATEGORY FOR EACH PLOT
# ------------------------------------------------------------

forest_category <- BDV_predictors %>%
  select(
    plotID,
    Forest_categories
  ) %>%
  filter(
    Forest_categories != "Old-growth forests"
  )



# ------------------------------------------------------------
# EXTRACT PLOT ID AND MANAGEMENT FROM RUN
# ------------------------------------------------------------

lnd_scen_filtered <- lnd_scen %>%
  mutate(
    
    # Extract plot ID, e.g. L1_03
    plot = stringr::str_extract(
      run,
      "L[0-9]+_[0-9]+"
    ),
    
    # Identify management from the run name
    management = case_when(
      stringr::str_detect(run, "_mng") ~ "Managed",
      stringr::str_detect(run, "_plot") ~ "Unmanaged",
      TRUE ~ NA_character_
    )
  ) %>%
  
  
  # ----------------------------------------------------------
# FILTER
# ----------------------------------------------------------
filter(
  year >= 0,
  year <= 350,
  species != "psme",
  management == "Unmanaged",
  !plot %in% old_growth
) %>%
  
  # ----------------------------------------------------------
# ADD FOREST CATEGORY
# ----------------------------------------------------------
left_join(
  forest_category,
  by = c("plot" = "plotID")
) %>%
  
  # ----------------------------------------------------------
# CREATE FINAL RUN LABEL
# ----------------------------------------------------------
mutate(
  run_label = paste(
    plot,
    Forest_categories,
    sep = " - "
  )
)


#-------------------------------------------------------------------------------
# This tells the colors:

  species.we.have<-unique(lnd_scen$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE
  
  
  # LIST OF ALL POSSIBLE SPECIES
  
  cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
              "bepe"="#fadfad", "acca"="darkorange4",
              "casa"="#7eeadf", "coav"="#20c6b6",  
              "tipl"="#645394", "ulgl"="#311432" ,
              "saca"="#D8BFD8",  "soar"="#DDA0DD", "soau"="#BA55D3",
              "pice"="#D27D2D", "pini"="#a81c07",
              "algl"="#2ECBE9","tico"="#128FC8",  "potr"="#00468B","poni"="#5BAEB7",
              "frex"="#fe9cb5","cabe"="#fe6181","acps"="#fe223e",
              "lade"="#FFFE71","abal"="#FFD800", "pisy"="#A4DE02",
              "fasy"="#76BA1B", "piab"="#006600","psme"="green",
              "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900" 
  )
  
  
  # COLORATION ORDER FOR ALL THE POSSIBLE SPECIES
  
  new_order_gg.all=c("rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau",
                     "soar", "saca", "poni", "algl", "alvi","alin", "tico", 
                     "potr","frex","cabe", "acps","acpl","acca", "qupu", "qupe",
                     "quro", "fasy", "lade", "abal", "pini","pice","pisy","psme","piab")
  
  
  # This will show at the end only the species we really have on the landscape. 
  
  cols<-cols.all[names(cols.all) %in% species.we.have]
  new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]
  
  
# ============================================================
# VOLUME PLOT
# ============================================================
  
  
G1 <- ggplot(
  lnd_scen_filtered,
  aes(
    year,
    volume_m3,
    fill = factor(species, levels = new_order_gg)
  )
) +
  
  geom_area() +
  
  scale_fill_manual(
    values = cols[new_order_gg],
    guide = guide_legend(reverse = TRUE)
  ) +
  
  # ggtitle("Stand growing stock by species - Unmanaged scenario") +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  labs(
    x = "Year",
    y = "Growing Stock [m3/ha]",
    fill = "Species"
  ) +
  
  theme_bw() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = "black"
    ),
    
    strip.background = element_rect(
      fill = "white",
      color = "Black"
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.ticks = element_blank(),
    
    plot.title = element_text(
      hjust = 0.5,
      size = NULL
    ),
    
    axis.title = element_text(
      size = 24
    ),
    
    axis.text = element_text(
      size = 20
    ),
    
    strip.text = element_text(
      size = 12
    ),
    
    legend.text = element_text(
      size = 20
    ),
    
    legend.title = element_text(
      size = 20
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    
    legend.position = "right"
  )

G1


# ============================================================
# CREATE MULTIPLE TIFF OR PNG FIGURES FOR BAYESIAN - SITE AND ALL
# ============================================================


library(ggplot2)
library(ggh4x)
library(dplyr)
library(stringr)
library(ragg)

# ============================================================
# FILTER AND RENAME FOREST PLOTS
# ============================================================

# OLD-GROWTH PLOTS TO REMOVE
old_growth <- c(
  "L1_18", "L1_22", "L1_24", "L1_26",
  "L2_27", "L2_30", "L2_33", "L2_34",
  "L3_06", "L3_09", "L3_10", "L3_16",
  "L4_06", "L4_09", "L4_31", "L4_33",
  "L6_05", "L6_08", "L6_10"
)


# ------------------------------------------------------------
# FOREST CATEGORY FOR EACH PLOT
# ------------------------------------------------------------

forest_category <- BDV_predictors %>%
  select(
    plotID,
    Forest_categories
  ) %>%
  filter(
    Forest_categories != "Old-growth forests"
  )


# ------------------------------------------------------------
# EXTRACT PLOT ID AND MANAGEMENT FROM RUN
# ------------------------------------------------------------

lnd_scen_filtered <- lnd_scen %>%
  mutate(
    
    # Extract plot ID, e.g. L1_03
    plot = stringr::str_extract(
      run,
      "L[0-9]+_[0-9]+"
    ),
    
    # Identify management from the run name
    management = case_when(
      stringr::str_detect(run, "_mng") ~ "Managed",
      stringr::str_detect(run, "_plot") ~ "Unmanaged",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # ----------------------------------------------------------
# FILTER
# ----------------------------------------------------------
filter(
  year >= 0,
  year <= 350,
  species != "psme",
  management == "Unmanaged",
  !plot %in% old_growth
) %>%
  
  # ----------------------------------------------------------
# ADD FOREST CATEGORY
# ----------------------------------------------------------
left_join(
  forest_category,
  by = c("plot" = "plotID")
) %>%
  
  # ----------------------------------------------------------
# CREATE FINAL RUN LABEL AND SITE (L1...L6)
# ----------------------------------------------------------
mutate(
  run_label = paste(
    plot,
    Forest_categories,
    sep = " - "
  ),
  site = str_extract(plot, "^L[0-9]+")
)

table(lnd_scen_filtered$site, lnd_scen_filtered$Forest_categories)  # check


# Strip colours per forest category (names also define the block order)
cat_cols <- c("Broadleaved-dominated" = "#f3dfb8",
              "Coniferous-dominated"  = "#cfe3d4")


# ============================================================
# VOLUME PLOT: category blocks, coloured strips, no empty panels
# ============================================================

plot_volume <- function(data, ncol = 5) {
  
  lev <- data %>%
    distinct(run_label, Forest_categories) %>%
    mutate(Forest_categories = factor(Forest_categories, levels = names(cat_cols))) %>%
    arrange(Forest_categories, run_label)
  
  data$run_label <- factor(data$run_label, levels = lev$run_label)
  
  strip_fill <- unname(cat_cols[as.character(lev$Forest_categories)])
  strip_fill[is.na(strip_fill)] <- "white"
  
  ggplot(
    data,
    aes(
      year,
      volume_m3,
      fill = factor(species, levels = new_order_gg)
    )
  ) +
    
    geom_area() +
    
    scale_fill_manual(
      values = cols[new_order_gg],
      drop = FALSE,   # same species legend in every figure
      guide = guide_legend(reverse = TRUE)
    ) +
    
    # ggtitle("Stand growing stock by species - Unmanaged scenario") +
    
    facet_wrap2(
      ~run_label, ncol = ncol,
      strip = strip_themed(
        background_x = elem_list_rect(fill = strip_fill, colour = "black")
      )
    ) +
    
    labs(
      x = "Year",
      y = "Growing Stock [m3/ha]",
      fill = "Species"
    ) +
    
    theme_bw() +
    
    theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = "black"
      ),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.ticks = element_blank(),
      
      plot.title = element_text(
        hjust = 0.5,
        size = NULL
      ),
      
      axis.title = element_text(
        size = 24
      ),
      
      axis.text = element_text(
        size = 20
      ),
      
      strip.text = element_text(
        size = 12
      ),
      
      legend.text = element_text(
        size = 20
      ),
      
      legend.title = element_text(
        size = 20
      ),
      
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      
      legend.position = "right"
    )
}


# --- Save helper (direct TIFF via ragg, no plot pane) ---------
save_volume <- function(p, file, n_panels, ncol, dpi = 600) {
  n_rows <- ceiling(n_panels / ncol)
  ggsave(
    file, p,
    device = ragg::agg_tiff,
    compression = "lzw",
    width  = 4 * ncol + 7,        # panels + legend
    height = 3.5 * n_rows + 3,    # panels + axis title
    units = "in", dpi = dpi, limitsize = FALSE
  )
}


# --- One figure per site (broadleaved + coniferous together) ---
out_dir <- "figures_growing_stock_by_site"
dir.create(out_dir, showWarnings = FALSE)
ncol_site <- 5
sites <- sort(unique(lnd_scen_filtered$site))

for (s in sites) {
  d <- filter(lnd_scen_filtered, site == s)
  if (nrow(d) == 0) next
  
  p <- plot_volume(d, ncol = ncol_site)
  save_volume(p, file.path(out_dir, paste0("growing_stock_", s, ".tiff")),
              n_panels = n_distinct(d$run_label), ncol = ncol_site)
}


# --- Overview figure: all plots, categories as blocks ---------
out_all <- "figures_overview"
dir.create(out_all, showWarnings = FALSE)
ncol_all <- 10

G1 <- plot_volume(lnd_scen_filtered, ncol = ncol_all)
save_volume(G1, file.path(out_all, "growing_stock_all_plots.tiff"),
            n_panels = n_distinct(lnd_scen_filtered$run_label),
            ncol = ncol_all, dpi = 600)





# ============================================================
#  HARVEST PLOT
# ============================================================


removals_filtered_mng <- removals %>%
  mutate(
    plot = stringr::str_extract(
      run,
      "L[0-9]+_[0-9]+"
    ),
    
    management = case_when(
      stringr::str_detect(run, "_mng") ~ "Managed",
      stringr::str_detect(run, "_plot") ~ "Unmanaged",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    year >= 0,
    year <= 266,
    management == "Managed",
    !plot %in% old_growth
  ) %>%
  left_join(
    forest_category,
    by = c("plot" = "plotID")
  ) %>%
  mutate(
    run_label = paste(
      plot,
      Forest_categories,
      sep = " - "
    )
  )

#  Plot it

M1 <- ggplot(removals_filtered_mng, aes(year, volume, fill=factor(type, levels=c( "regcut","finalcut","thinning","salvager"))))+
  geom_bar(position="stack", stat="identity", width = 6)+
  facet_wrap(~run_label, ncol=10)+
  ggtitle("Harvest activities exspressed in growing stocks removed - Managed scenario") +
  labs(x = "Year",y="Removed volume [m3/ha]",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59","#B3C100"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_minimal(base_size = 25) +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 8),   # was 10
    axis.title = element_text(size = 18),   # was 12
    axis.text = element_text(size = 12),    # was 10
    legend.text = element_text(size = 16),  # was 10
    legend.title = element_text(size = 18), # was 10
    plot.title = element_text(size = 26, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # remove minor gridlines
    strip.background = element_rect(fill = "white")# white facet strip background
  )
M1


# ============================================================
# SECOND PART
# ============================================================

# Load required packages
#library(ggplot2)
#library(ggpubr)    # For stat_pvalue_manual
library(tidyr)
library(dplyr)
#library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files

# Load the data (if using the official papar excel file - then you can skip this part but you need to adapt the table in a similar way as below for the graphics)
BDV_predictors <- read_excel("C:/iLand/2023/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes_Copy.xlsx")

# Remake the names in the table for a better understanding and coding
BDV_predictors <- BDV_predictors %>%
  mutate(forest_cat = recode(forest_cat,
                             "Beech-Oak" = "Native Broadleaves",
                             "Conifer" = "Non-Native Coniferous",
                             "Old-Growth" = "Old-Growth")) %>%
  select(-c(Lichens_RD1, Macrofungi_RD1, Moths_RD1, `Macrofungi x red-listed (2.652)`, `Moths x red-listed (0.574)`)) %>%
  rename(
    Bryophytes = `Epiphytic / epixilic bryophytes (0.212)`,
    Lichens = `Lichens (0.137)`,
    Macrofungi = `Macrofungi (2.118)`,
    Beetles = `Non-flying beetles (0.053)`,
    Moths = `Moths (0.566)`
  )

# Check
table(BDV_predictors$forest_cat) # here you see the divisions between old growth coniferous and broadleaves
glimpse(BDV_predictors)
BDV_predictors

#-------------------------------------------------------------------------------
# Make Old growth forest quartiles for select the 25%, 50% , 75% BDV threshold per Taxa sp richness

# Filter only Old-growth category
BDV_old_growth <- BDV_predictors %>% 
  filter(forest_cat == "Old-Growth")

# Compute quartiles for the 5 taxa in Old-growth
taxa_quartiles_thrashold <- BDV_old_growth %>%
  summarise(across(c(Bryophytes, Lichens, Macrofungi, Beetles, Moths), quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))

# Transpose the result for better readability
taxa_quartiles_thrashold <- as.data.frame(t(taxa_quartiles_thrashold))

# Rename columns for clarity
colnames(taxa_quartiles_thrashold) <- c("Q1", "Median", "Q3")

# Print the quartiles
print(taxa_quartiles_thrashold)

#-------------------------------------------------------------------------------
#         START TO PLOT
#-------------------------------------------------------------------------------


library(ggplot2)
library(ggpointdensity)
library(patchwork)
library(dplyr)
library(scales)
library(viridis)

taxa_quartiles_thrashold # Remember to run the code in BDV_recovery_V4 or previous versions but maybe even not needed

################################################################################
#_______________________________________________________________________________
# FILTER FOR THE FIRST 350 YEARS
bayesian_results_all_filtered <- bayesian_results_all_1961_1990 %>% filter(year >= 0 & year <= 350)
Bayesian_BDV_model_V3_multi <- Bayesian_BDV_model_V3_multi_1961_1990 %>% filter(year >= 0 & year <= 350)

# Reduce data set of 90%
reduced_data <- bayesian_results_all_filtered %>%
  sample_frac(0.01)

# To select all except the runs having "_mng" inside the name
# reduced_data <- reduced_data[!grepl("_mng", reduced_data$run), ]
# Opposite -> To select all the runs having "_mng" inside the name
# Bayesian_BDV_model_V3_multi<- Bayesian_BDV_model_V3_multi[grepl("_mng", Bayesian_BDV_model_V3_multi$run), ]

# BEETLES OFFICIAL PLOT V4

# Replace plotID with row numbers
BDV_predictors$plotID <- seq_len(nrow(BDV_predictors)) # required for the data structure ansd plots argument names


taxa_quartiles_thrashold # Remember to run the code in BDV_recovery_V4 or previous versions but maybe even not needed

################################################################################
#_______________________________________________________________________________
# Redduce and filter the Bayesian BDV model V3 where the median fucntion come from

Bayesian_BDV_model_V3_multi_mng <- Bayesian_BDV_model_V3_multi %>%
  mutate(
    plot = stringr::str_extract(
      run,
      "L[0-9]+_[0-9]+"
    ),
    
    management = case_when(
      stringr::str_detect(run, "_mng") ~ "Managed",
      stringr::str_detect(run, "_plot") ~ "Unmanaged",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    management == "Unmanaged",
    !plot %in% old_growth
  ) %>%
  left_join(
    forest_category,
    by = c("plot" = "plotID")
  ) %>%
  mutate(
    run_label = paste(
      plot,
      Forest_categories,
      sep = " - "
    )
  )


# ============================================================
# FILTER BIODIVERSITY PREDICTIONS
# ============================================================

reduced_data_mng <- reduced_data %>%
  mutate(
    plot = stringr::str_extract(
      run,
      "L[0-9]+_[0-9]+"
    ),
    
    management = case_when(
      stringr::str_detect(run, "_mng") ~ "Managed",
      stringr::str_detect(run, "_plot") ~ "Unmanaged",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    management == "Unmanaged",
    !plot %in% old_growth
  ) %>%
  left_join(
    forest_category,
    by = c("plot" = "plotID")
  ) %>%
  mutate(
    run_label = paste(
      plot,
      Forest_categories,
      sep = " - "
    )
  )

#-------------------------------------------------------------------------------
# MACROFUNGI OFFICIAL PLOT V1

# Rename column to 'Macromycetes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Macrofungi (2.118)")] <- "Macrofungi"

### Make the plot FUNGI

B3 <- ggplot(
  reduced_data_mng,
  aes(
    x = year,
    y = PRED_RICH_MACROFUNGI
  )
) +
  
  
  geom_pointdensity(alpha = 0.2) + 
  scale_color_viridis(
    option = "H",
    name = "Species richness probability"
  ) + 
  
  geom_line(
    data = Bayesian_BDV_model_V3_multi_mng, 
    aes(
      x = year,
      y = MACROFUNGI_PRED_RICH_50,
      linetype = "Expected value"
    ),
    color = "darkred",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(
    aes(
      yintercept = 192,
      linetype = "Target species richness"
    ),
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_boxplot(
    data = BDV_predictors, 
    aes(
      x = 380,
      y = Macrofungi
    ),
    width = 30,
    notch = TRUE,
    notchwidth = 0.2,
    fill = NA,
    color = "black", 
    outlier.shape = NA,
    inherit.aes = FALSE
  ) +
  
  stat_summary(
    data = BDV_predictors,
    aes(
      x = 380,
      y = Macrofungi
    ),
    fun = median,
    geom = "point",
    color = "red",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Expected value" = "solid",
      "Target species richness" = "dashed"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Fungi Species Richness Prediction - Managed Scenario",
    x = "Year",
    y = "Predicted Species Richness of Fungi"
  ) +
  
  scale_y_continuous(
    limits = c(50, 285),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  theme_minimal(base_size = 52) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.5
    ),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 26, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "white"
    )
  )

print(B3)
############


# BEETLES

###########

# Rename column to 'beetles'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Non-flying beetles (0.053)")] <- "Beetles"


B4 <- ggplot(
  reduced_data_mng,
  aes(
    x = year,
    y = PRED_RICH_BEETLES
  )
) +
  
  geom_pointdensity(alpha = 0.2) + 
  scale_color_viridis(
    option = "H",
    name = "Species richness probability"
  )+
  
  geom_line(
    data = Bayesian_BDV_model_V3_multi_mng, 
    aes(
      x = year,
      y = BEETLES_PRED_RICH_50,
      linetype = "Expected value"
    ),
    color = "darkred",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(
    aes(
      yintercept = 11.5,
      linetype = "Target species richness"
    ),
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_boxplot(
    data = BDV_predictors, 
    aes(
      x = 380,
      y = Beetles
    ),
    width = 30,
    notch = TRUE,
    notchwidth = 0.2,
    fill = NA,
    color = "black", 
    outlier.shape = NA,
    inherit.aes = FALSE
  ) +
  
  stat_summary(
    data = BDV_predictors,
    aes(
      x = 380,
      y = Beetles
    ),
    fun = median,
    geom = "point",
    color = "red",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Expected value" = "solid",
      "Target species richness" = "dashed"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Beetles Species Richness Prediction - Managed Scenario",
    x = "Year",
    y = "Predicted Species Richness of Beetles"
  ) +
  
  scale_y_continuous(
    limits = c(4, 16),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  theme_minimal(base_size = 52) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.5
    ),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 26, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "white"
    )
  )

print(B4)


############


# BrYOPHYTES OFFICIAL PLOT V1

###########

# Rename column to 'Bryophytes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"


B5 <- ggplot(
  reduced_data_mng,
  aes(
    x = year,
    y = PRED_RICH_BRYOPHYTES
  )
) +
  
  
  
  geom_pointdensity(alpha = 0.4) + 
  scale_color_viridis(
    option = "H",
    name = "Species richness probability"
  ) + 
  
  geom_line(
    data = Bayesian_BDV_model_V3_multi_mng, 
    aes(
      x = year,
      y = BRYO_PRED_RICH_50,
      linetype = "Expected value"
    ),
    color = "darkred",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(
    aes(
      yintercept = 17.5,
      linetype = "Target species richness"
    ),
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_boxplot(
    data = BDV_predictors, 
    aes(
      x = 380,
      y = Bryophytes
    ),
    width = 30,
    notch = TRUE,
    notchwidth = 0.2,
    fill = NA,
    color = "black", 
    outlier.shape = NA,
    inherit.aes = FALSE
  ) +
  
  stat_summary(
    data = BDV_predictors,
    aes(
      x = 380,
      y = Bryophytes
    ),
    fun = median,
    geom = "point",
    color = "red",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Expected value" = "solid",
      "Target species richness" = "dashed"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Bryophytes Species Richness Prediction - Managed Scenario",
    x = "Year",
    y = "Predicted Species Richness of Bryophytes"
  ) +
  
  scale_y_continuous(
    limits = c(3, 30),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  theme_minimal(base_size = 52) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.5
    ),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 26, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "white"
    )
  )

print(B5)


############


# LICHENS OFFICIAL PLOT V1

###########

# Rename column to 'lichens'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Lichens (0.137)")] <- "Lichens"


B6 <- ggplot(
  reduced_data_mng,
  aes(
    x = year,
    y = PRED_RICH_LICHENS
  )
) +
  
  
  geom_pointdensity(alpha = 0.2) + 
  scale_color_viridis(
    option = "H",
    name = "Species richness probability"
  ) + 
  
  geom_line(
    data = Bayesian_BDV_model_V3_multi_mng, 
    aes(
      x = year,
      y = LICHEN_PRED_RICH_50,
      linetype = "Expected value"
    ),
    color = "darkred",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(
    aes(
      yintercept = 18.5,
      linetype = "Target species richness"
    ),
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_boxplot(
    data = BDV_predictors, 
    aes(
      x = 380,
      y = Lichens
    ),
    width = 30,
    notch = FALSE,
    notchwidth = 0.2,
    fill = NA,
    color = "black", 
    outlier.shape = NA,
    inherit.aes = FALSE
  ) +
  
  stat_summary(
    data = BDV_predictors,
    aes(
      x = 380,
      y = Lichens
    ),
    fun = median,
    geom = "point",
    color = "red",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Expected value" = "solid",
      "Target species richness" = "dashed"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Lichens Species Richness Prediction - Managed Scenario",
    x = "Year",
    y = "Predicted Species Richness of Lichens"
  ) +
  
  scale_y_continuous(
    limits = c(0, 35),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  theme_minimal(base_size = 52) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.5
    ),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 26, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "white"
    )
  )

print(B6)


############


#  Moths OFFICIAL PLOT V1

###########

# Rename column to 'Moths'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Moths (0.566)")] <- "Moths"


B7 <- ggplot(
  reduced_data_mng,
  aes(
    x = year,
    y = PRED_RICH_MOTHS
  )
) +
  
  
  geom_pointdensity(alpha = 0.4) + 
  scale_color_viridis(
    option = "H",
    name = "Species richness probability"
  ) + 
  
  geom_line(
    data = Bayesian_BDV_model_V3_multi_mng, 
    aes(
      x = year,
      y = MOTHS_PRED_RICH_50,
      linetype = "Expected value"
    ),
    color = "darkred",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(
    aes(
      yintercept = 72.75,
      linetype = "Target species richness"
    ),
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_boxplot(
    data = BDV_predictors, 
    aes(
      x = 380,
      y = Moths
    ),
    width = 30,
    notch = TRUE,
    notchwidth = 0.2,
    fill = NA,
    color = "black", 
    outlier.shape = NA,
    inherit.aes = FALSE
  ) +
  
  stat_summary(
    data = BDV_predictors,
    aes(
      x = 380,
      y = Moths
    ),
    fun = median,
    geom = "point",
    color = "red",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  scale_linetype_manual(
    values = c(
      "Expected value" = "solid",
      "Target species richness" = "dashed"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Moths Species Richness Prediction - Managed Scenario",
    x = "Year",
    y = "Predicted Species Richness of Moths"
  ) +
  
  scale_y_continuous(
    limits = c(0, 120),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  
  facet_wrap(
    ~run_label,
    ncol = 10
  ) +
  
  theme_minimal(base_size = 52) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.5
    ),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 26, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "white"
    )
  )

print(B7)



# ============================================================
# CREATE MULTIPLE TIFF OR PNG FIGURES FOR BAYESIAN - SITE AND ALL
# ============================================================


library(ggplot2)
library(ggpointdensity)
library(ggh4x)
library(dplyr)
library(stringr)
library(viridis)
library(ragg)

scenario <- "Unmanaged"   # used in the title; change to "Managed" if needed

# --- 1. Site column (L1...L6) ----------------------------------------------
reduced_data_mng <- reduced_data_mng %>%
  mutate(site = str_extract(plot, "^L[0-9]+"))

Bayesian_BDV_model_V3_multi_mng <- Bayesian_BDV_model_V3_multi_mng %>%
  mutate(site = str_extract(plot, "^L[0-9]+"))

table(reduced_data_mng$site, reduced_data_mng$Forest_categories)  # check

# --- 2. Taxon specifications (values from your B3-B7) ----------------------
taxa_specs <- list(
  fungi      = list(label = "Fungi",      pred = "PRED_RICH_MACROFUNGI",  line = "MACROFUNGI_PRED_RICH_50",
                    target = 192,   box = "Macrofungi", ylim = c(50, 285), alpha = 0.2, notch = TRUE),
  beetles    = list(label = "Beetles",    pred = "PRED_RICH_BEETLES",     line = "BEETLES_PRED_RICH_50",
                    target = 11.5,  box = "Beetles",    ylim = c(4, 16),   alpha = 0.2, notch = TRUE),
  bryophytes = list(label = "Bryophytes", pred = "PRED_RICH_BRYOPHYTES",  line = "BRYO_PRED_RICH_50",
                    target = 17.5,  box = "Bryophytes", ylim = c(3, 30),   alpha = 0.4, notch = TRUE),
  lichens    = list(label = "Lichens",    pred = "PRED_RICH_LICHENS",     line = "LICHEN_PRED_RICH_50",
                    target = 18.5,  box = "Lichens",    ylim = c(0, 35),   alpha = 0.2, notch = FALSE),
  moths      = list(label = "Moths",      pred = "PRED_RICH_MOTHS",       line = "MOTHS_PRED_RICH_50",
                    target = 72.75, box = "Moths",      ylim = c(0, 120),  alpha = 0.4, notch = TRUE)
)

# Strip colours per forest category (names also define the block order)
cat_cols <- c("Broadleaved-dominated" = "#f3dfb8",
              "Coniferous-dominated"  = "#cfe3d4")

# --- 3. Plot function: category blocks, coloured strips, no empty panels ---
plot_taxon <- function(spec, data, model, ncol = 5) {
  
  lev <- data %>%
    distinct(run_label, Forest_categories) %>%
    mutate(Forest_categories = factor(Forest_categories, levels = names(cat_cols))) %>%
    arrange(Forest_categories, run_label)
  
  data$run_label  <- factor(data$run_label,  levels = lev$run_label)
  model$run_label <- factor(model$run_label, levels = lev$run_label)
  
  strip_fill <- unname(cat_cols[as.character(lev$Forest_categories)])
  strip_fill[is.na(strip_fill)] <- "white"
  
  ggplot(data, aes(x = year, y = .data[[spec$pred]])) +
    geom_pointdensity(alpha = spec$alpha) +
    scale_color_viridis(
      option = "H", name = "Species richness probability",
      guide = guide_colorbar(barheight = unit(8, "cm"),
                             barwidth  = unit(1, "cm"),
                             order = 1)
    ) +
    geom_line(
      data = model,
      aes(x = year, y = .data[[spec$line]], linetype = "Expected value"),
      color = "darkred", linewidth = 1, inherit.aes = FALSE
    ) +
    geom_hline(
      aes(yintercept = spec$target, linetype = "Target species richness"),
      color = "red", linewidth = 0.5, inherit.aes = FALSE
    ) +
    geom_boxplot(
      data = BDV_predictors,
      aes(x = 380, y = .data[[spec$box]]),
      width = 30, notch = spec$notch, notchwidth = 0.2,
      fill = NA, color = "black", outlier.shape = NA, inherit.aes = FALSE
    ) +
    stat_summary(
      data = BDV_predictors,
      aes(x = 380, y = .data[[spec$box]]),
      fun = median, geom = "point", color = "red", size = 1, inherit.aes = FALSE
    ) +
    scale_linetype_manual(
      values = c("Expected value" = "solid", "Target species richness" = "dashed"),
      name = "Legend"
    ) +
    guides(
      linetype = guide_legend(override.aes = list(linewidth = 1.2), order = 2)
    ) +
    labs(
      #title = paste(spec$label, "Species Richness Prediction -", scenario, "Scenario"),
      x = "Year",
      y = paste("Predicted Species Richness of", spec$label)
    ) +
    scale_y_continuous(limits = spec$ylim, breaks = scales::pretty_breaks(n = 6)) +
    facet_wrap2(
      ~run_label, ncol = ncol,
      strip = strip_themed(background_x = elem_list_rect(fill = strip_fill))
    ) +
    theme_minimal(base_size = 22) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 34),
      axis.text = element_text(size = 12),
      
      # legend (enlarged)
      legend.text = element_text(size = 26),
      legend.title = element_text(size = 24, face = "bold"),
      legend.key.size = unit(1.6, "cm"),
      legend.spacing.y = unit(0.8, "cm"),
      
      # title (enlarged)
      #plot.title = element_text(size = 40, face = "bold", hjust = 0.5,
      #                          margin = margin(b = 20)),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# --- 4. Save helper (direct PNG via ragg, no plot pane) --------------------
save_taxon <- function(p, file, n_panels, ncol, dpi = 600) {
  n_rows <- ceiling(n_panels / ncol)
  ggsave(
    file, p,
    device = ragg::agg_png,
    width  = 4 * ncol + 7,        # panels + enlarged legend
    height = 3.5 * n_rows + 3,    # panels + enlarged title/axis
    units = "in", dpi = dpi, limitsize = FALSE
  )
}

# --- 5a. One figure per taxon x site (broadleaved + coniferous together) ---
out_dir <- "figures_by_site"
dir.create(out_dir, showWarnings = FALSE)
ncol_site <- 5
sites <- sort(unique(reduced_data_mng$site))

for (tx in names(taxa_specs)) {
  for (s in sites) {
    d <- filter(reduced_data_mng, site == s)
    m <- filter(Bayesian_BDV_model_V3_multi_mng, site == s)
    if (nrow(d) == 0) next
    
    p <- plot_taxon(taxa_specs[[tx]], d, m, ncol = ncol_site)
    save_taxon(p, file.path(out_dir, paste0(tx, "_", s, ".tiff")),
               n_panels = n_distinct(d$run_label), ncol = ncol_site)
  }
}

# --- 5b. Overview figure: all plots, categories as blocks ------------------
out_all <- "figures_overview"
dir.create(out_all, showWarnings = FALSE)
ncol_all <- 10

for (tx in names(taxa_specs)) {
  p <- plot_taxon(taxa_specs[[tx]], reduced_data_mng,
                  Bayesian_BDV_model_V3_multi_mng, ncol = ncol_all)
  save_taxon(p, file.path(out_all, paste0(tx, "_all_plots.tiff")),
             n_panels = n_distinct(reduced_data_mng$run_label),
             ncol = ncol_all, dpi = 600)
}


# --- 5c. Loop: one PNG per taxon x site -------------------------------------
out_dir <- "figures_by_site"
dir.create(out_dir, showWarnings = FALSE)

ncol_site <- 5
sites <- sort(unique(reduced_data_mng$site))

for (tx in names(taxa_specs)) {
  for (s in sites) {
    d <- filter(reduced_data_mng, site == s)
    m <- filter(Bayesian_BDV_model_V3_multi_mng, site == s)
    n_panels <- n_distinct(d$run_label)
    nrow_site <- ceiling(n_panels / ncol_site)
    
    p <- plot_taxon(taxa_specs[[tx]], d, m, ncol = ncol_site)
    
    ggsave(
      file.path(out_dir, paste0(tx, "_", s, ".png")),
      p,
      device = ragg::agg_png,
      width  = 4 * ncol_site + 3,   # panel width + legend
      height = 3.5 * nrow_site + 2, # panel height + title/axis
      units = "in", dpi = 300, limitsize = FALSE
    )
  }
}


#####################

# THE END OF THE SCRIPT - REFERENCE: 

#####################