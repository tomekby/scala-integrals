package frontend

import swing._
import Swing._
import event._
import java.awt.Color
import backend._

object Main  extends SimpleSwingApplication {
  
  def top = new MainFrame {
    // Dane okienka
    title = "Całkowanie numeryczne"
    resizable = false
    centerOnScreen

    // Tworzenie menu
    menuBar = new MenuBar {
      contents += new Menu("Program") {
        mnemonic = Key.Alt
        contents += new MenuItem(Action("O programie i twórcy") {
            new About()
        }) { mnemonic = Key.O }
        contents += new MenuItem(Action("Wyjście") {
            dispose
        }) { mnemonic = Key.W }
      }
    }
    // Historia z texterea
    var textareaHistory : List[String] = List()
    // Textarea z wyrażeniem do całkowania
    val inputFormula = new TextArea {
      tooltip = "Wpisz wzór funkcji którą chcesz całkować"
      wordWrap = false
      columns = 40
      rows = 5
      lineWrap = true

      // Blokowanie whitespace'a, poprawa przecinka na kropkę
      listenTo(keys)
      reactions += {
        case e: KeyTyped => {
          if(e.char.isWhitespace || e.char.charValue.equals('=')) e.consume
          else if(e.char.charValue.equals(',')) {
            e.consume
            text += "."
          }
        }
        case e: KeyPressed => {
          if(e.key.equals(Key.Enter)) {
            e.consume
            calculate
          }
          else if(e.key.equals(Key.Tab)) {
            e.consume
            calculateButton.requestFocus
          }
          // Obsługa Ctrl-Z
          if(e.modifiers == 128 && e.key.equals(Key.Z)) {
            if(textareaHistory.length != 0) {
              textareaHistory = textareaHistory.dropRight(1)
              text = if(textareaHistory.length != 0) textareaHistory.last else ""
            }
            else {
              textareaHistory = List()
              text = ""
            }
          }
          else textareaHistory = textareaHistory ::: List(text)
        }
        // Obsługa Ctrl-V
        case e: KeyReleased => if(e.modifiers == 128 && e.key.equals(Key.V)) text = """\s""".r replaceAllIn(text, "")
      }
    }
    // Button od liczenia
    val calculateButton = new Button("Oblicz")
    // Button od kopiowania wyniku
    val copyIntoClipboard = new Button("Kopiuj wynik do schowka") {
      name = "clipboard"
    }

    // Input z wynikiem
    val result = new TextField("0.0") {
      editable = false
      background = Color.white
    }
    // Wybór metody całkowania
    val radioRect = new RadioButton("Metoda prostokątów") {
      selected = true
    }
    val radioTrapeze = new RadioButton("Metoda trapezów")
    val radioSimpson = new RadioButton("Wzór Simpsona")
    val radio38      = new RadioButton("Reguła 3/8")
    val radioBoole   = new RadioButton("Wzór Boole'a")
    val radioGroup   = new ButtonGroup(radioRect, radioTrapeze, radioSimpson, radio38, radioBoole)
    // Dokładność całkowania
    object slider extends Slider {
      min = 10
      max = 1000
      value = (max - min) / 2
    }
    val accuracy = new Label("Dokładność: "+slider.value+" przedziałów")
    // Zakres całkowanie i zamienna po której całkujemy
    val rangeStart = new TextField("0") {
      // Blokowanie whitespace'a, poprawa przecinka na kropkę i inna magia ze zdarzeniami klawiatury
      listenTo(keys)
      reactions += {
        case e: KeyReleased => calculate
        case e: KeyTyped => {
          // Jeśli już jest kropka, pominięcie jej
          if((e.char.charValue.equals(',') || e.char.charValue.equals('.')) && (text matches """\d+\.\d+""")) e.consume
          else if(e.char.charValue.equals(',')) {
            e.consume
            text += "."
          }
          else if( ! e.char.isDigit && ! e.char.charValue.equals(".")) e.consume
        }
      }
    }
    val rangeEnd = new TextField("1") {
      // Blokowanie whitespace'a, poprawa przecinka na kropkę i inna magia ze zdarzeniami klawiatury
      listenTo(keys)
      reactions += {
        case e: KeyTyped => {
          // Jeśli już jest kropka, pominięcie jej
          if((e.char.charValue.equals(',') || e.char.charValue.equals('.')) && (text matches """\d+\.\d+""")) e.consume
          else if(e.char.charValue.equals(',')) {
            e.consume
            text += "."
          }
          else if( ! e.char.isDigit && ! e.char.charValue.equals('.')) e.consume
        }
      }
    }
    val variableOfIntegration = new TextField("x")
    // Shortcut dla buttona z od razu ustawioną akcją
    def button(text : String, input : String) : Button = new Button(text) {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          inputFormula.text = input.replaceAll("val", inputFormula.text)
          inputFormula.requestFocus
          calculate
      }
    }
    // Przyciski wstawiające funkcje w formularz
    val buttons : Map[String,String] = Map(
      "|x|" -> "abs(val)", "ln(x)" -> "ln(val)", "<html>log<sub>10</sub>x" -> "log(val)", "<html>log<sub>x</sub>y" -> "log(val,y)",
      "(x)" -> "_(val)", "min(x,y)" -> "min(val,y)", "max(x,y)" -> "max(val,y)", "floor(x)" -> "floor(val)", 
      "ceil(x)" -> "ceil(val)", "round(x)" -> "round(val)", "sin(x)" -> "sin(val)", "cos(x)" -> "cos(val)",
      "tan(x)" -> "tan(val)", "ctg(x)" -> "ctg(val)", "asin(x)" -> "asin(val)", "acos(x)" -> "acos(val)",
      "atan(x)" -> "atan(val)", "sinh(x)" -> "sinh(val)", "cosh(x)" -> "cosh(val)", "tanh(x)" -> "tanh(val)"
    )
    // Tworzenie grida
    val buttonPanel = new GridPanel(4,5) {
      buttons.foreach{ case(key, value) => contents += button(key, value) }
    }

    /**
     * Ustawianie kontrolek
     * Czyli całe ręczne grzebanie w layoucie okienka
     */
    contents = new BoxPanel(Orientation.Vertical) {
      // Textarea na wpisanie równania
      contents += inputFormula
      contents += VStrut(10)
      // Buttony wstawiające funkcje
      contents += buttonPanel
      contents += VStrut(10)
      // Wybór metody całkowania
      contents += new BorderPanel {
        add(new Label("Wybierz metodę całkowania:"), BorderPanel.Position.West)
      }
      contents += new BorderPanel { add(radioRect, BorderPanel.Position.West) }
      contents += new BorderPanel { add(radioTrapeze, BorderPanel.Position.West) }
      contents += new BorderPanel { add(radioSimpson, BorderPanel.Position.West) }
      contents += new BorderPanel { add(radio38, BorderPanel.Position.West) }
      contents += new BorderPanel { add(radioBoole, BorderPanel.Position.West) }
      contents += VStrut(5)
      // Zakres całkowania i zmienna po której przeprowadzamy całkowanie
      contents += new GridPanel(3, 2) {
        contents += new Label("Zmienna całkowania: ") {
          horizontalAlignment = Alignment.Right
        }
        contents += variableOfIntegration
        contents += new Label("Początek zakresu całkowania: ") {
          horizontalAlignment = Alignment.Right
        }
        contents += rangeStart
        contents += new Label("Koniec zakresu całkowania: ") {
          horizontalAlignment = Alignment.Right
        }
        contents += rangeEnd
      }
      contents += VStrut(5)
      // Dokładność obliczeń
      contents += new BorderPanel {
        add(new Label("Ilość przedziałów:"), BorderPanel.Position.Center)
      }
      contents += VStrut(5)
      contents += slider
      contents += VStrut(5)
      contents += new BorderPanel {
        add(accuracy, BorderPanel.Position.Center)
      }
      contents += VStrut(10)
      // Buttony (liczenie, schowek)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new BorderPanel {
          add(calculateButton, BorderPanel.Position.South)
        }
        contents += new BorderPanel {
          add(copyIntoClipboard, BorderPanel.Position.South)
        }
      }
      contents += VStrut(10)
      // Wynik
      contents += new BorderPanel {
        add(new Label("Wynik:"), BorderPanel.Position.Center)
      }
      contents += result

      border = Swing.EmptyBorder(5, 5, 15, 5)
    }
    
    /**
     * OBSŁUGA EVENTÓW ETC
     */
    // Kopiowanie do schowka wyniku działań
    def copy() {
      val clipboard = java.awt.Toolkit.getDefaultToolkit.getSystemClipboard
      val sel = new java.awt.datatransfer.StringSelection(result.text)
      clipboard.setContents(sel, sel)
    }
    // Przelczanie wartości wyrażenia
    def calculate() {
      // Będzie Exception jeśli wyrażenie nie jest prawidłowe bądź jeśli zakres jset nieprawidłowy
      try {
        val integration = new Integration(start = rangeStart.text.toDouble, end = rangeEnd.text.toDouble, accuracy = slider.value, variableOfIntegration = variableOfIntegration.text)
        // Liczenie  wybraną metodą
        if(radioRect.selected) result.text = integration.rect(inputFormula.text)
        else if(radioTrapeze.selected) result.text = integration.trapeze(inputFormula.text) 
        else if(radioSimpson.selected) result.text = integration.simpson(inputFormula.text)
        else if(radio38.selected) result.text = integration.three_over_eight(inputFormula.text) 
        else result.text = integration.boole(inputFormula.text) 
      } catch {
        case e: Exception => result.text = "Błędne wyrażenie, zakres całkowania bądź zmienna całkowania"
      }
    }

    // Listenery
    listenTo(copyIntoClipboard)
    listenTo(inputFormula, calculateButton, slider, rangeStart, rangeEnd, variableOfIntegration)
    listenTo(radioRect, radioTrapeze, radioSimpson, radio38, radioBoole)
    // Reakcje na eventy
    reactions += {
      case ButtonClicked(button) => {
        button.name match {
          case "clipboard" => copy
          case _ => calculate
        }
      }
      case EditDone(`inputFormula`) | EditDone(`rangeStart`) | EditDone(`rangeEnd`) | EditDone(`variableOfIntegration`) => calculate
      case ValueChanged(`slider`) => {
        accuracy.text = "Dokładność: "+slider.value+" przedziałów"
        if( ! slider.adjusting) calculate
      }
    }
  }
}