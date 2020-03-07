### Assignment : pnr_ctrl ###
context("pnr_ctrl()")

test_that("Kontroll av pnr_ctrl.", {
  expect_true(exists("pnr_ctrl"),
              info = "Fel: pnr_ctrl() saknas.")
  expect_that(pnr_ctrl, is_a("function"),
              info = "Fel: pnr_ctrl är inte en funktion.")
  expect_function_self_contained(object = pnr_ctrl,
                        "Fel: Funktionen innehåller fria variabler")
  
  expect_true(all(names(formals(pnr_ctrl)) %in% c("pnr")),
              info = "Fel: Argumenten i funktionen har felaktiga namn.")
  
  expect_true(is.logical(pnr_ctrl("196408233234")),
              info = "Fel: Funktionen returnerar inte ett logiskt element.")
  
  expect_true(pnr_ctrl("196408233234"),
              info = "Fel: Ska returnera TRUE för 196408233234.")
  
  expect_false(pnr_ctrl("196408233238"),
              info = "Fel: Ska returnera FALSE för 196408233238.")
  
  expect_true(pnr_ctrl("198112189876"),
              info = "Fel: Ska returnera TRUE för 198112189876.")
  
  expect_false(pnr_ctrl("198112189879"),
              info = "Fel: Ska returnera FALSE för 198112189879.")
  
  expect_false(pnr_ctrl("199909090909"),
              info = "Fel: Ska returnera FALSE för 199909090909.")
  
  expect_function_code(object = pnr_ctrl, expected = "return", 
                       info = "Fel: return() saknas i funktionen.")  
})
