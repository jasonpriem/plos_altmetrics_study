##### create list_df_backgrounds, df_research_norm, and df_research_norm_transform

# set up
PATH_TO_RAW_DATA = "../data/raw/"
PATH_TO_DERIVED_DATA = "../data/derived/"

source("utils.R")
source("lookup_tables.R")
source("preprocessing_eventcounts_norm.R")

# read unnormalized research data frame
df_research = read.csv(paste(PATH_TO_DERIVED_DATA, "df_research.txt.gz", sep=""), sep="\t")

# calculate backgrounds, normalization, transformation
response = get_dat_research_norm(df_research)
list_df_backgrounds = response$dat.backgrounds
df_research_norm = response$dat.research.norm
df_research_norm_transform = response$dat.research.norm.transform

# save them
save(list_df_backgrounds, file=paste(PATH_TO_DERIVED_DATA, "list_df_backgrounds.RData", sep=""), compress="gzip")
write.table.gzip(df_research_norm, PATH_TO_DERIVED_DATA, "df_research_norm.txt")
write.table.gzip(df_research_norm_transform, PATH_TO_DERIVED_DATA, "df_research_norm_transform.txt")

