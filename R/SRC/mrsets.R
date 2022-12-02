
#-----------------------------------------------------------------------------------------------

#multiple response set

#-----------------------------------------------------------------------------------------------


iter_vw <- list()
iter_vl <- list()
iter_bh <- list()

for(i in geoitem.active) {
  
print(i)
  
#-----------------------------------------------------------------------------------------------
#vrijwilligerswerk

vw_reason_mr<-df %>% 
  filter(GEOITEM==i)  %>% 
  tab_cells(mdset(zw08_0 %to% zw08_6)) %>%  # 'mdset' designate that with have multiple dichotomy set
  tab_weight(weging) %>% # weight
  tab_stat_cpct() %>% # statistic
  tab_pivot() 

vw<-as_tibble(vw_reason_mr[1:7,2]) %>%
  t()

colnames(vw)<-c("zw08_0_pin","zw08_1_pin","zw08_2_pin","zw08_3_pin",
                "zw08_4_pin","zw08_5_pin","zw08_6_pin")


vw<-as_tibble(vw)

iter_vw[[i]]<-vw

#-----------------------------------------------------------------------------------------------
#verenigingsleven

vl_mr<-df %>% 
  filter(GEOITEM==i)  %>%
  tab_cells(mdset(zw05_0 %to% zw05_5)) %>%  # 'mdset' designate that with have multiple dichotomy set
  tab_weight(weging) %>% # weight
  tab_stat_cpct() %>% # statistic
  tab_pivot() 

vl<-as_tibble(vl_mr[1:6,2]) %>%
  t()


colnames(vl)<-c("zw05_0_pin","zw05_1_pin","zw05_2_pin","zw05_3_pin",
                "zw05_4_pin","zw05_5_pin")


vl<-as_tibble(vl)

iter_vl[[i]]<-vl

#-----------------------------------------------------------------------------------------------
#burenhulp

bh_mr<-df %>% 
  filter(GEOITEM==i)  %>%
  tab_cells(mdset(zw13_0 %to% zw13_8)) %>%  # 'mdset' designate that with have multiple dichotomy set
  tab_weight(weging) %>% # weight
  tab_stat_cpct() %>% # statistic
  tab_pivot() 

bh<-as_tibble(bh_mr[1:9,2]) %>%
  t()

colnames(bh)<-c("zw13_0","zw13_1","zw13_2","zw13_3","zw13_4","zw13_5","zw13_6","zw13_7","zw13_8")

bh<-as_tibble(bh)

iter_bh[[i]]<-bh

}

#append 
vw_set<-do.call(rbind,iter_vw)
vl_set<-do.call(rbind,iter_vl)
bh_set<-do.call(rbind,iter_bh)

#merge
mr_sets<-cbind(vw_set,vl_set,bh_set)