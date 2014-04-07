/**
 * Klasa obliczająca całkę oznaczoną
 * Pewnie można to napisać jakoś ładnie funkcyjnie i krócej, ale przynajmniej działa
 * Na pewno część niżej napisanego kodu stoi w sprzeczności z DRY i KISS, ale było mało czasu na napisanie...
 */
package backend

class Integration(start : Double = 0, end : Double = 1, var accuracy : Int = 10, variableOfIntegration : String = "x") {

  // Szerokość przedziałów
  var step : Double = (end - start).toDouble / accuracy

  def getXi(i : Double) : Double = start.toDouble + (i.toDouble / accuracy) * (end - start)

  // Metoda prostokątów
  // Przybliżanie za pomocą funkcji stałych
  // Dokładność dla "x^6+21*x^2+75" przy 1000 przedziałów: 10E2
  def rect(formula : String) : String = {
    var sum = 0.0
    var i = 0

    for(i <- 1 to accuracy) {
      var xi = getXi(i)
      val parser = new FormulaParser(constants = Map(variableOfIntegration -> xi))
      sum += parser.evaluate(formula)
    }

    return (step * sum).toString
  }

  // Metoda trapezów
  // Przybliżanie za pomocą funkcji liniowych
  // Dokładność dla "x^6+21*x^2+75" przy 1000 przedziałów: 10E-1
  def trapeze(formula : String) : String = {
    var sum = 0.0
    var i = 0

    val xiFormula = formula.replaceAll(variableOfIntegration, variableOfIntegration+"i")
    val xim1Formula = formula.replaceAll(variableOfIntegration, variableOfIntegration+"m")
    for(i <- 1 to accuracy) {
      // Parsowanie wyrażenia
      val parser = new FormulaParser(
        constants = Map(
          // Punkty dla końców obu podstaw trapezu
          variableOfIntegration+"i" -> getXi(i),
          variableOfIntegration+"m" -> getXi(i - 1)
      ))
      sum += step * (parser.evaluate(xiFormula) + parser.evaluate(xim1Formula)) / 2
    }

    return sum.toString
  }

  // Wzór Simpsona
  // Src: http://edu.i-lo.tarnow.pl/inf/alg/004_int/0004.php
  // Przybliżanie za pomocą wielomianów stopnia 2.
  // Dokładność dla "x^6+21*x^2+75" przy 1000 przedziałów: 10E-8
  def simpson(formula : String) : String = {
    var sum = 0.0
    var i = 0

    val xim1Formula = formula.replaceAll(variableOfIntegration, variableOfIntegration+"0")
    val xiFormula = formula.replaceAll(variableOfIntegration, variableOfIntegration+"1")
    val xitFormula = formula.replaceAll(variableOfIntegration, variableOfIntegration+"2")
    for(i <- 1 to accuracy) {
      // Parsowanie wyrażenia
      val parser = new FormulaParser(
        constants = Map(
          // Punkty dla końców obu podstaw trapezu
          variableOfIntegration+"0" -> getXi(i - 1),
          variableOfIntegration+"1" -> getXi(i),
          variableOfIntegration+"2" -> (getXi(i) + getXi(i - 1)) / 2
      ))
      sum += step * (parser.evaluate(xiFormula) + parser.evaluate(xim1Formula) + 4.0 * parser.evaluate(xitFormula)) / 6
    }

    return sum.toString
  }

  // Reguła 3/8
  // Src: https://en.wikipedia.org/wiki/Simpson%27s_rule#Simpson.27s_3.2F8_rule (przetłumaczony kod Python)
  // Przybliżanie za pomocą wielomianów stopnia 3.
  // Dokładność dla "x^6+21*x^2+75" przy 1000 przedziałów: 10E-7
  // Co ciekawe dla wielomianów stopnia 6. daje gorsze przybliżenie niż reguła Simpsona
  def three_over_eight(formula : String) : String = {
    val parser = new FormulaParser(
        constants = Map(
          variableOfIntegration+"0" -> start,
          variableOfIntegration+"1" -> end
      ))
    val h : Double = (end - start) / accuracy
    var s : Double = parser.evaluate(formula.replaceAll(variableOfIntegration, variableOfIntegration+"0")) + parser.evaluate(formula.replaceAll(variableOfIntegration, variableOfIntegration+"1"))
    var i = 0
    // Liczenie 1. "zestawu"
    for(i <- 1 to accuracy by 2) {
      val p = new FormulaParser( constants = Map(variableOfIntegration -> (h * i + start)) )
      s += 4.0 * p.evaluate(formula)
    }
    // Liczenie 2. "zestawu"
    for(i <- 2 to accuracy - 1 by 2) {
      val p = new FormulaParser( constants = Map(variableOfIntegration -> (h * i + start)) )
      s += 2.0 * p.evaluate(formula)
    }

    return (s * h / 3).toString
  }

  // Wzór Boole'a
  // Src: http://www.ijmsea.com/admin/docs/1339389844YES%201.pdf
  // Przybliżanie za pomocą wielomianu stopnia 4.
  // Dokładność dla "x^6+21*x^2+75" przy 1000 przedziałów: 10E-10
  def boole(formula : String) : String = {
    val parser = new FormulaParser(
        constants = Map(
          variableOfIntegration+"0" -> start,
          variableOfIntegration+"1" -> end
      ))
    val h : Double = (end - start) / accuracy
    var sum = 7.0 * (parser.evaluate(formula.replaceAll(variableOfIntegration, variableOfIntegration+"0")) + parser.evaluate(formula.replaceAll(variableOfIntegration, variableOfIntegration+"1")))
    // Liczenie 1. "zestawu"
    for(i <- 1 to accuracy - 1 by 2) {
      val p = new FormulaParser( constants = Map(variableOfIntegration -> (h * i + start)) )
      sum += 32.0 * p.evaluate(formula)
    }
    // Liczenie 2. "zestawu"
    for(i <- 2 to accuracy - 2 by 4) {
      val p = new FormulaParser( constants = Map(variableOfIntegration -> (h * i + start)) )
      sum += 12.0 * p.evaluate(formula)
    }
    // Liczenie 3. "zestawu"
    for(i <- 4 to accuracy - 4 by 4) {
      val p = new FormulaParser( constants = Map(variableOfIntegration -> (h * i + start)) )
      sum += 14.0 * p.evaluate(formula)
    }

    return (2.0 * h / 45 * sum).toString
  }

}
