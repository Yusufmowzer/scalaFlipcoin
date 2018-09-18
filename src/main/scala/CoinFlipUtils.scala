import CoinFlip.mainLoop

import scala.io.StdIn.readLine
import scala.util.Random
import java.io.{File, PrintWriter}

import scala.io.Source

object CoinFlipUtils {
  def homeScreen(): Unit = {
    print(Console.BLUE)
    print(Console.BOLD)
    //    print(Console.REVERSED)
    print("ooooooooO Flip Coin Ooooooooooo")
    print(Console.RESET)
  }

  def selectScreen(): Unit = {
    println("\n")
    println("1. Best out of 10")
    println("2. Free play")
    println("\n Press q to EXIT")
    print(Console.BOLD)
    print(Console.GREEN)
    print("Choose a game setting (1) or (2): ")
    print(Console.RESET)
    val r = Random
    val s = GameState(0, 0)
    val userSelect = getUserInput()

    print(Console.BOLD)
    print(Console.GREEN)
    println()
    userSelect match {
      case "1" ⇒ {
        println("\nO  --- Best Out of 10 ---  O")
        mainLoop(s, r, userSelect.toInt)
      }
      case "2" ⇒ {
        println("\nO     --- Free Play ---     O")
        mainLoop(s, r, userSelect.toInt)
      }
      case "Q" ⇒ {
        print(Console.RESET)
        println("\n\nGoodBye!")
      }
      case _ ⇒ {
        print(Console.BOLD)
        print(Console.RED)
        print(Console.REVERSED)
        print("Incorrect input. Please try again")
        print(Console.RESET)
        println()
        selectScreen()
      }
    }
    print(Console.RESET)
  }

  def BestOf10(gameSelect: Int, gameState: GameState): String = (gameSelect, gameState.numFlips) match {
    case (1, 10) ⇒ {
      printGameOver()
      printGameState(gameState)
      val result = winPercentage(gameState)
      highScore(result)
      print("\nBack to (m)enu or (r)etry:")
      getUserInput()
    }
    case (3, _) ⇒ {
      print("\n\nBack to (m)enu or (r)etry:")
      getUserInput()
    }
    case _ ⇒ {
      showPrompt()
      getUserInput()
    }
  }

  def getUserInput(): String = readLine.trim.toUpperCase

  def printGameOver(): Unit = {
    print(Console.RED)
    print(Console.BOLD)
    println("\n\n     === GAME OVER === \n")
    print(Console.RESET)
  }

  def showPrompt(): Unit = {
    print(Console.BOLD)
    print("\n(h)eads, (t)ails, or (m)enu: ")
    print(Console.RESET)
  }

  def highScore(score: Double): Unit ={
    val fileName = "highScore.txt"
    val prevHighScore = Source.fromFile(fileName).bufferedReader().readLine().toDouble
    print(Console.BOLD)
    print(Console.YELLOW)
    (score.toInt, prevHighScore.toInt) match {
      case(s, p) if s > p ⇒ {
        println(f"\nCongratulations :) \nNew High Score: $score%1.2f"+"%")
        val writer = new PrintWriter(new File(fileName))
        writer.write(f"$score%1.2f")
        writer.close()
      }
      case (s, p) if s < p ⇒ {
        println(f"\nTry Again :( \nHigh score: $prevHighScore%1.2f" + "%")
      }
      case (s, p) if s.equals(p) ⇒ {
        println(f"\nWell Done! :) \nEqualled the High Score: $score%1.2f"+"%")
      }
    }
    print(Console.RESET)
  }

  def winPercentage(gameState: GameState): Double = {
    val result : Double = (gameState.numCorrect.toFloat / gameState.numFlips.toFloat) * 100.00
    print(Console.BOLD)
    print(Console.BLUE)
    if(gameState.numFlips != 0) {
      println(f"Win Percentage: $result%1.2f" + "%")
      print(Console.RESET)
      result
    }
    else {
      println("Win Percentage: N/A")
      print(Console.RESET)
      0.00
    }
  }

  def printGameState(gameState: GameState): Unit = {
    print(Console.BLUE)
    println(s"Flips: ${gameState.numFlips}   Correct: ${gameState.numCorrect}")
    print(Console.RESET)
  }

  def printableFlipResult(flip: String): String = flip match {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  def printGameState(printableFlipResult: String, gameState: GameState, result: String): Unit = {
    print(Console.BOLD)
    print(Console.RED)
    print(s"\n$result: Flip was $printableFlipResult. \n")
    print(Console.RESET)
    printGameState(gameState)
    println("_______________________________")
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