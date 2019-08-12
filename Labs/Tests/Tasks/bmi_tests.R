### Assignment : bmi() ###
context("bmi()")

test_that("Assignment: bmi()", {
  
  expect_true(exists("bmi"),
              info = "Fel: bmi() saknas.")
  checkmate::expect_function(bmi,
              info = "Fel: bmi är inte en funktion.")
  expect_function_self_contained(object = bmi,
                        "Fel: Funktionen innehåller fria variabler")
  
  expect_function_arguments(bmi, c("body_weight","body_height"),
              info = "Fel: Argumenten i funktionen har felaktiga namn.")

  expect_silent(bmi(body_weight = 71, body_height = 1.62))
  
  checkmate::expect_number(bmi(body_weight = 71, body_height = 1.78),
              info = "Fel: Funktionen returnerar inte ett numeriskt värde.")
  
  expect_equal(bmi(body_weight = 71, body_height = 1.78), 22.40879, tolerance = .001,
               info = "Fel: Fungerar inte för body_weight > 0 och body_height > 0")
  
  expect_equal(bmi(body_weight = 93, body_height = 1.88), 26.31281, tolerance = .001,
               info = "Fel: Fungerar inte för body_weight > 0 och body_height > 0")
  
  expect_warning(bmi(body_weight = 0, body_height = 1.78),
               info = "Fel: Genererar inget varningsmeddelande (då body_weight <= 0)")
  
  expect_warning(bmi(body_weight = -3, body_height = 1.78), "body_weight is not positive, calculation is not meaningful",
                 info = "Fel: Genererar fel varningsmeddelande (då body_weight <= 0)")
  
  expect_warning(bmi(body_weight = 71, body_height = 0),
                 info = "Fel: Genererar inget varningsmeddelande (då body_height <= 0)")
  expect_warning(bmi(body_weight = 71, body_height = -2), "body_height is not positive, calculation is not meaningful",
                 info = "Fel: Genererar fel varningsmeddelande (då body_height <= 0)")

  expect_warning(bmi(body_weight = -1, body_height = 0), 
                 info = "Fel: Genererar inte varningsmeddelande (då body_weight <= 0 och body_height<=0)")
  
  expect_function_code(object = bmi, expected = "return", 
                       info = "Fel: return() saknas i funktionen.")  
})
