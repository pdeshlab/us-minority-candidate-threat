library(data.table)

# local councils
candidates <- fread('~/Downloads/C3_Data_050521.tab')
candidates[, latino:=ifelse((race=='Latino'),1,0)]
candidates[, black:=ifelse(race=='Black',1,0)]

t <- candidates[generaloutcome!='', .(num_black=sum(black, na.rm = TRUE), num_latino=sum(latino, na.rm = TRUE)), by=.(state, chamber, dno)]

#t[, black_latino:=rowSums(t[,c("num_black", "num_latino")], na.rm=TRUE)]

fwrite(merge(candidates,
  t[, .(sum(num_black), sum(num_latino)), by=.(state, chamber, dno)][V1>=1 & V2>=1, .(state, chamber, dno)],
  by=c('state', 'chamber', 'dno')),
       file='~/Downloads/black_latino_contested_state_legislative_elections_2018.csv')


# mayors
mayors <- data.table(readRDS('~/Downloads/deshpande_zonszein_221202.rds'))
mayors[, latino:=ifelse((race_final=='hispanic'),1,0)]
mayors[, black:=ifelse(race_final=='black',1,0)]

t <- mayors[, .(num_black=sum(black, na.rm = TRUE), num_latino=sum(latino, na.rm = TRUE)), by=.(fips_char, year)]

merge(mayors,
      t[, .(sum(num_black), sum(num_latino)), by=.(fips_char, year)][V1>=1 | V2>=1, .(fips_char, year)],
      by=c('fips_char', 'year'))

fwrite(merge(mayors,
             t[, .(sum(num_black), sum(num_latino)), by=.(fips_char, year)][V1>=1 & V2>=1, .(fips_char, year)],
             by=c('fips_char', 'year')),
       file='~/Downloads/black_latino_contested_mayoral_elections_1991_2021.csv')
