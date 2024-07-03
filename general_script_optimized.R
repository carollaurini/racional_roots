library(phonTools)
library(data.table)
library(dplyr)
#Author : Ana C. Laurini Malara
############################################## Some functions Tested #######################################################################
#1+2*(cos(2*pi* x)+ cos(2*pi* y)+cos(2*pi*(x+y))+cos(4*pi* x)+cos(2*pi*(2*x+y))+cos(2*pi*(x+2*y))+cos(2*pi*(2*x+2*y)))
#1+2*cos(2*pi *x)+2*cos(2*pi *y)+2*cos(2*pi*(x+y))
#1+2*(cos(2*pi* x) +cos(4*pi* x)+ cos(2*pi* y)+cos(4*pi* y)+cos(2*pi*(x+y))+cos(2*pi*(x+2*y))+cos(2*pi*(x-y)) +cos(2*pi*(x-2*y))+cos(2*pi* (2*x+y))+cos(2*pi*(2*x-y)))
#1+2*(cos(2*pi* x) + cos(4*pi* x)+ cos(2*pi* y)+cos(2*pi* (x+y))+ cos(2*pi*(x-y)))
#1+2*(cos(2*pi* x)+cos(2*pi*(2*x))+cos(2*pi*(3*x))+ cos(2*pi*(2*x+y))+cos(2*pi*(x+y)))

#Losango Family 
#1+2*cos(2*pi *y) + 2*cos(2*pi *x)
#1+2*cos(2*pi *y) + 2*(cos(2*pi *x)+cos(4*pi *x))
#1+2*cos(2*pi *y) + 2*(cos(2*pi *x)+cos(4*pi *x)+cos(6*pi *x))
#1+2*cos(2*pi *y) + 2*(cos(2*pi *x)+cos(4*pi *x)+cos(6*pi *x)+cos(8*pi *x))

#1+2*cos(2*pi *y) +4*cos(2*pi *y)*(cos(2*pi *x)+cos(4*pi *x))
#1+2*cos(2*pi *y) +2*cos(2*pi *x)+4*cos(2*pi *y)*(cos(2*pi *x)+cos(4*pi *x))

########################################## Choose your gap for the fractions ###############################################################
denominator_gap=100 ##3045 fractions
######################################### Input you Function here ##########################################################################
expression <- function(x,y) { 1+2*cos(2*pi *y) + 2*cos(2*pi *x) }


############################################################################################################################################
d_bind =data.frame(fraction = c(), decimais = c())
for(laticce in 1:denominator_gap){
  sequencia = seq(0,1,1/laticce)
  fraction =c("0")
  nominator=c(0)
  denominator=c(0)
  for (i in 1:laticce){
    f = reduce.fraction(c(i,laticce))
    fraction[i+1]= paste('\\frac{',as.character(f[1]),"}{",as.character(f[2]),"}",sep="")
    nominator[i+1] = f[1]
    denominator[i+1] = f[2]
  }
  d_factors =data.frame(fraction = fraction, decimais = sequencia,nominator=nominator,denominator=denominator)
  d_bind = rbind(d_bind,d_factors)
}
d_bind = d_bind[!duplicated(d_bind[,c('nominator','denominator')]),] %>% arrange(nominator,denominator)
print(d_bind %>% nrow()) 



df_bind =data.frame(zero_x= c(),zero_y= c())
zero_x=c()
zero_y=c()
j=1

for(x in d_bind$decimais){
  for(y in d_bind$decimais){
    
    z=eval(expression(x,y))
    if(-10^(-14) <z && z <10^(-14) ){
      print(paste("x,y,z:", x,y,z))
        zero_x[j] = x
        zero_y[j] = y
        j=j+1
      }
    }
  }
  
df =data.frame(zero_x= zero_x,zero_y= zero_y)
df_bind = rbind(df_bind,df)
print(df_bind %>% nrow())
  

if(!df_bind %>% is.null){
  df_bind = df_bind %>% left_join(d_bind,by = c('zero_x' ='decimais')) %>% left_join(d_bind,by = c('zero_y' ='decimais'))
  df= df_bind %>% mutate(code =paste("\\left(",fraction.x,",",fraction.y,"\\right)",sep="")) %>% arrange(denominator.x,denominator.y,nominator.x,nominator.y) %>% select(code)
  write.table(t(df),file="out.txt",row.names = FALSE,sep=',',quote = FALSE,col.names = FALSE)
}

