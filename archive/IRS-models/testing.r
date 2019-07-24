

DATA_TEST = list(N = 30, ##// rows of data giving numbers fed and survived, unfed and survived and dead
                 M = 50,               ##// rows of data giving numbers DEAD OR FED ## so all data listed
                 n1_t = rep(c(50,345,43,25,34,66,567,56,43,124),3),    ##// Total number of mosquitoes entering IRS huts WITH ALL DATA
                 n2_t = c(rep(c(45,36,575,345,23,543,234,145,654,87),2),rep(c(50,345,43,25,34,66,567,56,43,124),3)), ##// Total number of mosquitoes entering IRS huts WITH AGGREGATED DATA
                 
                 fed_surv_t = round(n1_t*(seq(0.2,0.8,length=30)),0),##// Number of mosquitoes dead and unfed in sprayed huts
                 unfed_surv_t = c(n1_t - round(n1_t*(seq(0.2,0.8,length=30)),0)-8),##// Number of mosquitoes dead and unfed in sprayed huts
                   
                 deterrence_IRS = n2_t + 300, ##  // Number of mosquitoes in control and sprayed huts
                 ded_t = c(n1_t - fed_surv_t - unfed_surv_t, round(n2_t[31:50] * seq(0.8,0.2,length=20))),## ;// Number mosquites dead sprayed hut
                   
                 time1 = seq(1,265,length = 30),##       // predictor
                 time2 = c(seq(1,265,length = 30),seq(1,265,length = 20)) )##;       // predictor
)