####################### Importing the BPC database #######################

#Importing
library(readxl)
bd <- read_excel("Banco de Dados/ExportacaoIC_DrMiguel_2020_0805.xlsx")

#O campo s6mdtc não foi automaticamente convertido para formato Data
#Convertendo agora
bd = bd %>% 
      mutate(s6mdtc = as.POSIXct(as.numeric(s6mdtc)*24*3600, #p/ segundos 
                                origin = "1899-12-30", tz = 'UTC'))
#Testando 
test_that("A data de contato de 6 meses do excel é igual a convertida?",{
    expect_equal(as.POSIXct("2017-07-20 00:00:00", tz ='UTC'), bd$s6mdtc[1])
    expect_equal(as.POSIXct("2017-08-14 00:00:00", tz ='UTC'), bd$s6mdtc[2])
    expect_equal(as.POSIXct("2017-08-14 00:00:00", tz ='UTC'), bd$s6mdtc[3])
})

test_that("Há dados duplicados?", {
    expect_equal(FALSE, any(duplicated(bd)))
})

############# Changing Non-documented values as NA at some variables #######

#todos as variáveis em que 2 = NA
#two_as_na_vars é um vetor em que há os nomes de todas as variáveis em que 2 = NA

bd <- replace_with_na_at(data = bd,
                         .vars = two_as_na_vars,
                         condition = ~.x == 2)

#todos as variáveis em que 3 = NA
three_as_na_vars <- c("s30cpm2s","s30drcs","s30drh","s30src",
                      "s6mcpm2s","s6mdrcs","s6mdrh","s6msrc")

bd <- replace_with_na_at(data = bd,
                         .vars = three_as_na_vars,
                         condition = ~.x == 3)

#todos as variáveis em que 4 = NA
four_as_na_vars <- c("s30peucv","s30hosv","s6mpeucv","s6mhosv")

bd <-  replace_with_na_at(data = bd,
                          .vars = four_as_na_vars,
                          condition = ~.x == 4)   

#todos as variáveis em que 5 = NA
five_as_na_vars <- c("acfic","aphemod")

bd <-  replace_with_na_at(data = bd,
                          .vars = five_as_na_vars,
                          condition = ~.x == 5) 

#todos as variáveis em que 6 = NA
six_as_na_vars <- c("ifvddd","s30cpfrp","s6mcpfrp")

bd <-  replace_with_na_at(data = bd,
                          .vars = six_as_na_vars,
                          condition = ~.x == 6) 

#todos as variáveis em que 7 = NA
seven_as_na_vars <- c("s30tb","s6mtb")

bd <-  replace_with_na_at(data = bd,
                          .vars = seven_as_na_vars,
                          condition = ~.x == 7)

#todos as variáveis em que 8 = NA
eight_as_na_vars <- c("ahdest")

bd <-  replace_with_na_at(data = bd,
                          .vars = eight_as_na_vars,
                          condition = ~.x == 8)

##Testando
test_that('Quando a variável está como não documentada, isso está como NA', {
    #Paciente 4 na variável hmha era 2, agora deve ser NA
    expect_equal(TRUE,any_na(bd[4,11]))
    #Paciente 337 na variável s30cpm2s era 3, agora deve ser NA
    expect_equal(TRUE,any_na(bd[337,283]))
    #Paciente 2949 na variável s30hosv era 4, agora deve ser NA
    expect_equal(TRUE,any_na(bd[2949,221]))
    #Paciente 3 na variável aphemod era 5, agora deve ser NA
    expect_equal(TRUE,any_na(bd[3,55]))
    #Paciente 329 na variável ifvddd era 6, agora deve ser NA
    expect_equal(TRUE,any_na(bd[329,170]))
    #Paciente 814 na variável s30tb era 7, agora deve ser NA
    expect_equal(TRUE,any_na(bd[814,278]))
    #Paciente  na variável ahdest era 8, agora deve ser NA
    expect_equal(TRUE,any_na(bd[644,178]))
})

##################### Fixing hmfrca, hpcp, asems data structure ###############

#hmrca = Existe histórico de comorbidade? 1 - sim, 0 - não
#Após há uma lista de dummy vars com as comorbidades relacionadas. 
#No bd quando hmfrca = 0,as comorbidades se tornam NA, 
#quando na verdade deveriam ser 0 tbm.
#O mesmo ocorre para procedimentos (hpcp) e sintomas(asems)

#Fixing hmfrca
bd = correct_related_na(df = bd, var_check = "hmfrca", col_init = "hmha", 
                   col_end = "hmfrcot")
#Testando  hmfrca
test_that("Quando hmfrca = 0 ou NA, as dummy vars de comorb estão adequadas?",{
    
    #Paciente 11 em que hmfrca == 0, a soma das vars deve ser 0
    expect_equal(0, rowSums(bd[11,comorb_vec])) 
    
    #Paciente 35 em que hmfrca == NA, então todas as comorb devem o ser
    expect_equal(TRUE, all_na(bd[35,comorb_vec])) 
    
    #Paciente 1 em que hmfrca == 1, deve haver pelo menos uma comorb
    expect_gt(rowSums(bd[1,comorb_vec]),0) 
    
})

#Fixing hpcp
bd = correct_related_na(df = bd, var_check = "hpcp", col_init = "hmangio", 
                        col_end = "hmtc")

#Testando hpcp
proced_vec = bd %>% 
    select(hmangio:hmtc) %>% 
    names()

test_that("Quando hpcp = 0 ou NA, 
          as dummy vars de procedimento estão adequadas?",{
              
              #Paciente 5 em que hpcp == 0, a soma das vars deve ser 0
              expect_equal(0, rowSums(bd[5,proced_vec])) 
              
              #Paciente 35 em que hpcp == NA, então todas as comorb devem o ser
              expect_equal(TRUE, all_na(bd[35,proced_vec])) 
              
              #Paciente 1 em que hpcp == 1, deve haver pelo menos uma comorb
              expect_gt(rowSums(bd[1,proced_vec],na.rm = TRUE),0) 
              
          })


#Fixing asems #
bd = correct_related_na(df = bd, var_check = "asems", col_init = "asdr", 
                        col_end = 'abppcc')


#Testando asems
sintomas_vec = bd %>% 
    select(asdr:abppcc) %>% 
    names()

test_that("Quando asems = 0 ou NA, 
          as dummy vars de sintomas estão adequadas?",{
              
              #Paciente 49 em que asems == 0, a soma das vars deve ser 0
              expect_equal(0, rowSums(bd[49,sintomas_vec])) 
              
              #Paciente 24 em que asems == NA, então todas as comorb devem o ser
              expect_equal(TRUE, all_na(bd[24,sintomas_vec])) 
              
              #Paciente 1 em que asems == 1, deve haver pelo menos uma comorb
              expect_gt(rowSums(bd[1,sintomas_vec],na.rm = TRUE),0) 
              
          })

###################### Identifying out of scale dates ########################
 #In this section I will assert that the dates are compatible, in order to extra
 #ct other variables such as age and days until death or hospitalization

#Replace dates of hospitalization before discharge as NA
bd = bd %>% 
  mutate(s6mdthpe = replace(s6mdthpe, s6mdthpe < ahdt, NA),
         adtadm = replace(adtadm, adtadm < as.POSIXct("2016-01-01, UTC"), NA))

test_that("Are there outliers in dates columns?",{
  
  #Date of birth
  assert_all_are_in_past(bd$dtnasc)
    #can't be underaged at the date of inclusion in the study
  expect_equal(FALSE, any(bd$dtnasc > bd$dtincl - dyears(18)))
  
  #Data of admission can't be before 2016 - 01 - 01, when was the study started
  expect_equal(FALSE, any(bd$adtadm < as.POSIXct("2016-01-01, UTC"), 
                          na.rm = TRUE))
  
  #Date of discharge #can't be before date of admission
  expect_equal(FALSE, any(bd$ahdt < bd$adtadm, na.rm = TRUE)) 
  
  #Date of hospitalization in the 30 days contact has to be after DISCHARGE
  expect_equal(FALSE, any(bd$ahdt > bd$s30dthpe, na.rm = TRUE))
  
  #Date of hospitalization in the 6 months contact has to be after DISCHARGE
  expect_equal(FALSE, any(bd$ahdt > bd$s6mdthpe, na.rm = TRUE)) #CHANGE
  
  #Date of death can't be before discharge, unless it is intra-hospital death
  expect_equal(FALSE, any((bd$otdth < bd$ahdt) & bd$ahdest != 7, na.rm = TRUE)) 
  
})

#conferir
################### Creating useful variables (age,) #################

#
