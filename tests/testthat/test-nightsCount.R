# Fonction test pour vérifier le bon comportement de nightsCount
test_nightsCount <- function() {

  # Test 1: Vérifier que le calcul des nuits est correct (exemple simple)
  test_that("Calcul des nuits entre deux dates est correct", {
    result <- nightsCount("20230501", "20230505", format = "%Y%m%d")
    expect_equal(result, 4)  # 5 mai - 1 mai = 4 nuits
  })

  # Test 2: Vérifier que le calcul des nuits donne 0 quand les dates sont identiques
  test_that("Le calcul des nuits est 0 quand les dates sont identiques", {
    result <- nightsCount("20230501", "20230501", format = "%Y%m%d")
    expect_equal(result, 0)
  })

  # Test 3: Vérifier que l'argument 'startDate' ou 'endDate' vide déclenche une erreur
  test_that("Erreur quand startDate ou endDate est vide", {
    expect_error(nightsCount("", "20230505", format = "%Y%m%d"), "Nor startDate nor endDate can be empty.")
    expect_error(nightsCount("20230501", "", format = "%Y%m%d"), "Nor startDate nor endDate can be empty.")
  })

  # Test 4: Vérifier une erreur quand le format de date est invalide
  test_that("Erreur si startDate ou endDate est invalide selon le format", {
    expect_error(nightsCount("20230501", "invalid_date", format = "%Y%m%d"), "startDate or endDate is invalid regarding the format.")
    expect_error(nightsCount("invalid_date", "20230505", format = "%Y%m%d"), "startDate or endDate is invalid regarding the format.")
  })

  # Test 5: Vérifier les dates avec un autre format (par exemple : %Y-%m-%d)
  test_that("Le calcul fonctionne avec un format de date différent", {
    result <- nightsCount("2023-05-01", "2023-05-05", format = "%Y-%m-%d")
    expect_equal(result, 4)
  })

  # Test 6: Vérifier le cas où endDate est avant startDate
  test_that("Le calcul des nuits donne un résultat négatif quand endDate < startDate", {
    result <- nightsCount("20230505", "20230501", format = "%Y%m%d")
    expect_equal(result, -4)  # 1er mai - 5 mai = -4 nuits
  })
}

# Exécution des tests
test_nightsCount()
