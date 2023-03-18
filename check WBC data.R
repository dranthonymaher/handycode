df=read.csv("C:/Users/amaher2/Downloads/prm_filter_cust_merch_10_cust_Western Australia.csv")
t1<-table(df$num_cust)
plot(t1,xlim = c(1000,60000),ylim = c(0,1.5))
