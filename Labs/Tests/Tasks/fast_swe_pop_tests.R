
context("fast_swe_pop()")

test_that("fast_swe_pop()", {  
  library(pxweb)
  library(checkmate)
  expect_attached_package("pxweb")
  expect_true(exists("fast_swe_pop"),
              info = "Fel: fast_swe_pop() saknas.")
  checkmate::expect_function(fast_swe_pop, nargs = 0,
                             info = "Fel: babylon är inte en funktion.")
#  expect_function_self_contained(object = fast_swe_pop, "Fel: Funktionen innehåller fria variabler")

  
  expect_silent(fast_swe_pop_output <- fast_swe_pop())
  checkmate::expect_class(fast_swe_pop_output, "data.frame",
              info="Fel: fast_swe_pop() returnerar inte en data.frame.")
  expect_names(names(fast_swe_pop_output), permutation.of =  c("year", "population"),info="Variabelnamn är inte korrekta")
  expect_numeric(fast_swe_pop_output$year,info="year är inte nummerisk")
  expect_numeric(fast_swe_pop_output$population,info="population är inte nummerisk")

  expect_equal(fast_swe_pop_output$year[12:15], c(1979,1980,1981,1982),info = "Årtalen är inte korrekta")
  expect_equal(fast_swe_pop_output$population[12:15], c(8303010,8317937,8323033,8327484),info = "Värdet på populationen är inte korrekt")

  expect_equal(fast_swe_pop_output$year[1:3], c(1968,1969,1970),info = "Årtalen är inte korrekta")
  expect_equal(fast_swe_pop_output$population[1:3], c(7931193,8004270,8081142),info = "Värdet på populationen är inte korrekt")
  

  expect_equal(fast_swe_pop_output$year[49:51], c(2016,2017,2018),info = "Årtalen är inte korrekta")
  expect_equal(fast_swe_pop_output$population[49:51], c(9995153,10120242,10230185),info = "Värdet på populationen är inte korrekt")
  
    
})
