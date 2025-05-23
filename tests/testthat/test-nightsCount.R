# Fonction test pour vérifier le bon comportement de nightsCount

  # Test 1: Vérifier que le calcul des nuits est correct (exemple simple)
  test_that("Calcul des nuits entre deux dates est correct", {
    result <- nightsCount("202305012210", "202305051105")
    expect_equal(result, 4)  # 5 mai - 1 mai = 4 nuits
  })

  # Test 2: Vérifier que le calcul des nuits donne 0 quand les dates sont identiques
  test_that("Le calcul des nuits est 0 quand les dates sont identiques", {
    result <- nightsCount("202305011115", "202305012359")
    expect_equal(result, 0)
  })

  # Test 3: Vérifier que l'argument 'startDate' ou 'endDate' vide déclenche une erreur
  test_that("Erreur quand startDate ou endDate est vide", {
    expect_error(nightsCount("", "202305"), "startDate and endDate should be length 8 or more.")
    expect_error(nightsCount("202305011215", ""), "startDate and endDate should be length 8 or more.")
  })

  # Test 4: Vérifier une erreur quand le format de date est invalide
  test_that("Erreur si startDate ou endDate est invalide selon le format", {
    expect_error(nightsCount("202305011212", "invalid_date"), "startDate or endDate is invalid regarding the format.")
    expect_error(nightsCount("invalid_date", "202305050000"), "startDate or endDate is invalid regarding the format.")
  })

  # Test 6: Vérifier le cas où endDate est avant startDate
  test_that("Le calcul des nuits donne un résultat négatif quand endDate < startDate", {
    result <- nightsCount("202305051210", "202305011210")
    expect_equal(result, -4)  # 1er mai - 5 mai = -4 nuits
  })
#
