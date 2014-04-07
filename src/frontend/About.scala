package frontend

import swing._

class About extends Dialog {
  title = "O programie"
  modal = false
  resizable = false

  contents = new BoxPanel(Orientation.Vertical) {
      contents += multiline_label(
          """<h1>Całkowanie numeryczne dowolnej funkcji</h1>
          &copy; 2014 Tomasz Stasiak
          Autor parsera wyrażeń arytmetycznych: Peter Schmitz (więcej w pliku FormulaParser.scala).
          Zmodyfikowany na własne potrzeby przeze mnie.

          Kalkulator umożliwia całkowanie dowolnej funkcji jednej zmiennej zawierającej dozwolone operacje.
          Nawiasy obsługiwane jako funkcja _() co jest spowodowane konfliktem w parserze gramatyki dla samych nawiasów.
          Ilość całkowanych funkcji zależy głównie od inwencji użytkownika.
          Przy niektórych wyrażeniach mogą występować problemy z obliczaniem wartości (np. całka z sinh(tan(x)) )
          Wyrażenie powinno być wpisywane bez dodatkowych oznaczeń, tj.:<ul>"""+
          "<li>bez nazwy funkcji [np. f(x)]</li>"+
          "<li>bez znaku równości [=]</li>"+
          "<li>bez oznaczenia zmiennej całkowania [np. dx za funkcją] - pole jest uzupełniane osobno</li>"+
          "<li>separatorem dziesiętnym jest kropka [.]</li>"+
          "</ul>Prawidłowy zapis funkcji: <i>x^6+21*x^2+75</i>\n"+
          "Dostępne metody całkowania:<ul>"+
          "<li>Metoda prostokątów</li>"+
          "<li>Metoda trapezów</li>"+
          "<li>Wzór Simpsona/li>"+
          "<li>Reguła 3/8/li>"+
          "<li>Reguła Boole'a/li>"+
          "</ul>Dozwolone działania i stałe matematyczne: takie same jak w kalkulatorze zaawansowanym"+"""
          Dokładność poszczególnych metod całkowania dla wielomianu "x^6+21*x^2+75" w zakresie od 0 do 5 dla 1000 przedziałów:<ul>"""+
          "<li>Metoda prostokątów (przybliżanie z użyciem funkcji stałych): 10<sup>2</sup></li>"+
          "<li>Metoda trapezów (przybliżanie z użyciem funkcji liniowych): 10<sup>-1</sup></li>"+
          "<li>Wzór Simpsona (przybliżanie z użyciem funkcji kwadratowych): 10<sup>-8</sup></li>"+
          "<li>Reguła 3/8 (przybliżanie z użyciem wielomianów stopnia 3.): 10<sup>-7</sup></li>"+
          "<li>Reguła Boole'a (przybliżanie z użyciem wielomianów stopnia 4.): 10<sup>-10</sup></li>"+
          "</ul>"+"""
          <i>Całkowicie napisane w języku Scala z użyciem Scala Swing
          Tworzenie pliku .jar z użyciem własnego skryptu .bat</i>"""
      )

      border = Swing.EmptyBorder(5, 5, 15, 5)
    }

  centerOnScreen()
  open()

  def multiline_label(t : String) : Label = new Label() {
    text = "<html>"+t.replaceAll("\n", "<br>")
  }
}