import scala.util.Random
import scala.io.StdIn.readLine

object CoinFlipUtils {

  def showPrompt(): Unit = { print("\n(h)eads, (t)ails, or (q)uit: ") }

  def getUserInput(): String = readLine.trim.toUpperCase

  def printableFlipResult(flip: String): String = flip match {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  def printGameState(printableFlipResult: String, gameState: GameState, result: String): Unit = {
    print(s"$result: Flip was $printableFlipResult. \n")
    printGameState(gameState)
  }

  def printGameState(gameState: GameState): Unit = {
    println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")
  }

  def printGameOver(): Unit = println("\n=== GAME OVER === ")

  def winPercentage(gameState: GameState): Unit = {
    println(f" Win Percentage: ${(gameState.numCorrect.toFloat/gameState.numFlips.toFloat)*100.00}%1.2f"+"%")
  }

  // returns "H" for heads, "T" for tails
  def tossCoin(r: Random): String = {
    val i = r.nextInt(2)
    i match {
      case 0 => "H"
      case 1 => "T"
    }
  }
}
