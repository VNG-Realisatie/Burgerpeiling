
#-----------------------------------------------------------------------------------------------

#Mean

#-----------------------------------------------------------------------------------------------

message("Mean...")

df_aggr_mn<- df_weight %>%
group_by(GEOITEM,PERIOD) %>%
srvyr::summarize(
bp_respons=n(),
across(
.cols = all_of(mean_cols),
.fns  =~survey_mean(.x,na.rm=TRUE, vartype=vt),
.names="{col}"
)
) %>%
mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA)) 