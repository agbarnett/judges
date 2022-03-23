# get years from 2017 to 2018 (latest available)
files = c('Life_Table_males_2017.xlsx', 'Life_Table_females_2017.xlsx',
          'Life_Table_males_2018.xlsx', 'Life_Table_females_2018.xlsx')
extra = NULL
for (f in files){
  sex = ifelse(str_detect(f, pattern='female'), 'Female', 'Male')
  year = as.numeric(str_split(f, '_|\\.')[[1]][4])
  
  in_raw = read_excel(paste('data/', f, sep=''), skip=2) %>% # just skip 1 for better names
    clean_names() %>%
    dplyr::select(-'qx') %>% # remove probability version of qx
    mutate(tmp_chunks = stringr::str_split(x1, stringr::fixed("–"),  n = 2)) %>%
    mutate(Age = purrr::map_chr(tmp_chunks, 1),
           Age = ifelse(Age=='100 and over', '100', Age),
           qx = dx / lx_2, # deaths by person years (replace qx)
           Sex = sex,
           Year = year,
           imputed = 'No') %>%
    filter(!str_detect(Age, 'SOURCE')) %>%
    mutate(Age = as.numeric(Age)) %>%
    filter(Age >= 18) %>%
    rename('denom' = 'lx') %>%
    dplyr::select(Year, Age, Sex, qx, denom, imputed)
  extra = bind_rows(extra, in_raw)
}

# compare
to_compare = filter(life.table, Year %in% 2016:2017)
cplot = ggplot(data=to_compare, aes(x=Age, y=qx, col=factor(Year)))+
  geom_point()+
  facet_wrap(~Sex)
cplot
