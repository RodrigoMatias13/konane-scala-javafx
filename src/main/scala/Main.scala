import scala.io.StdIn.readLine
import Logica._
import Logica.Stone._

object Main {

  def main(args: Array[String]): Unit = {
    println("=== KONANE ===")
    mainMenu()
  }

  // ─── Menu Principal ───────────────────────────────────────────────────────

  @annotation.tailrec
  def mainMenu(): Unit = {
    println("\n1. Novo jogo")
    println("2. Continuar jogo guardado")
    println("3. Sair")
    print("Opcao: ")

    readLine().trim match {
      case "1" =>
        val (boardSize, maxSeconds, difficulty) = configMenu()
        startNewGame(boardSize, maxSeconds, difficulty)
        mainMenu()
      case "2" =>
        loadGame() match {
          case None =>
            println("Nenhum jogo guardado encontrado.")
          case Some(state) =>
            println("Jogo carregado!")
            gameLoop(state, Nil)
        }
        mainMenu()
      case "3" =>
        println("Ate logo!")
      case _ =>
        println("Opcao invalida!")
        mainMenu()
    }
  }

  // ─── Menu de Configuracao ─────────────────────────────────────────────────

  def configMenu(): (Int, Long, Int) = {
    val boardSize  = readBoardSize()
    val maxSeconds = readMaxSeconds()
    val difficulty = readDifficulty()
    (boardSize, maxSeconds, difficulty)
  }

  @annotation.tailrec
  def readBoardSize(): Int = {
    print("Tamanho do tabuleiro (minimo 4, par - ex: 4, 6, 8): ")
    parseIntInput(readLine()) match {
      case Some(size) if size >= 4 && size % 2 == 0 => size
      case _ =>
        println("Tamanho invalido! Deve ser um numero par maior ou igual a 4.")
        readBoardSize()
    }
  }

  @annotation.tailrec
  def readMaxSeconds(): Long = {
    print("Tempo maximo por jogada em segundos (ex: 30): ")
    parseIntInput(readLine()) match {
      case Some(seconds) if seconds > 0 => seconds.toLong
      case _ =>
        println("Valor invalido! Deve ser um numero positivo.")
        readMaxSeconds()
    }
  }

  @annotation.tailrec
  def readDifficulty(): Int = {
    println("Nivel de dificuldade:")
    println("  1. Facil (IA aleatoria)")
    println("  2. Dificil (IA greedy)")
    print("Opcao: ")
    parseIntInput(readLine()) match {
      case Some(level) if level == 1 || level == 2 => level
      case _ =>
        println("Opcao invalida!")
        readDifficulty()
    }
  }

  // ─── Inicio de Jogo ───────────────────────────────────────────────────────

  def startNewGame(boardSize: Int, maxSeconds: Long, difficultyLevel: Int): Unit = {
    val fullBoard = initialBoard(boardSize)
    val mid       = boardSize / 2
    val board     = removeInitialPair(fullBoard, (mid - 1, mid - 1), (mid - 1, mid), boardSize)
      .getOrElse(fullBoard - ((mid - 1, mid - 1)) - ((mid - 1, mid)))
    val random      = MyRandom(System.currentTimeMillis())
    val openSquares = fullBoard.keys.toList.filterNot(board.contains)
    val initialState = GameState(board, random, Stone.Black, boardSize, openSquares, maxSeconds, difficultyLevel)
    gameLoop(initialState, Nil)
  }

  // ─── Game Loop ────────────────────────────────────────────────────────────

  def gameLoop(state: GameState, history: List[GameState]): Unit = {
    val GameState(board, random, currentPlayer, boardSize, openSquares, maxSeconds, difficultyLevel) = state

    println("\nTabuleiro:")
    println(boardToString(board, boardSize))
    println(s"Tu jogas com Preto (B). IA joga com Branco (W).")
    println(s"Tempo por jogada: ${maxSeconds}s | Dificuldade: ${if difficultyLevel == 2 then "Dificil" else "Facil"}")

    if (allValidMoves(board, currentPlayer, boardSize).isEmpty) {
      val winner    = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
      val winnerStr = if (winner == Stone.Black) "Preto (tu)" else "IA (Branco)"
      println(s"Sem jogadas para ${if currentPlayer == Stone.Black then "Preto" else "Branco"}! Vencedor: $winnerStr")
      deleteSave()
    } else {
      println(s"Vez de: ${if currentPlayer == Stone.Black then "Preto (B) - TU" else "IA (Branco)"}")

      if (currentPlayer == Stone.Black) {
        humanTurn(state, history)
      } else {
        aiTurn(state, history)
      }
    }
  }

  // ─── Turno do Jogador ─────────────────────────────────────────────────────

  def humanTurn(state: GameState, history: List[GameState]): Unit = {
    val GameState(board, random, _, boardSize, openSquares, maxSeconds, difficultyLevel) = state

    println("\na) Jogar   b) Undo   c) Guardar e sair")
    print("Opcao: ")

    readLine().trim.toLowerCase match {
      case "b" =>
        undo(history) match {
          case None =>
            println("Sem jogadas para desfazer!")
            humanTurn(state, history)
          case Some((previousState, remainingHistory)) =>
            println("Jogada desfeita.")
            gameLoop(previousState, remainingHistory)
        }
      case "c" =>
        if (saveGame(state)) println("Jogo guardado! Ate logo.")
        else println("Erro ao guardar jogo.")
      case _ =>
        val moveStartTime = System.currentTimeMillis()
        val fromSquare    = readCoordDe(board, boardSize)

        if (isTimeExceeded(moveStartTime, System.currentTimeMillis(), maxSeconds)) {
          println("Tempo esgotado! Passa a vez.")
          val nextState = state.copy(currentPlayer = Stone.White)
          gameLoop(nextState, state :: history)
        } else {
          val (finalBoard, finalOpenSquares) =
            jumpChain(board, fromSquare, boardSize, openSquares, moveStartTime, maxSeconds)
          val nextState = state.copy(
            board         = finalBoard,
            currentPlayer = Stone.White,
            openSquares   = finalOpenSquares
          )
          gameLoop(nextState, state :: history)
        }
    }
  }

  // ─── Turno da IA ──────────────────────────────────────────────────────────

  def aiTurn(state: GameState, history: List[GameState]): Unit = {
    val GameState(board, random, player, boardSize, openSquares, maxSeconds, difficultyLevel) = state
    println("IA a jogar...")

    val (maybeBoard, newRandom, newOpenSquares, maybeDestination) =
      aiMove(board, player, boardSize, difficultyLevel, random, openSquares)

    maybeBoard match {
      case None =>
        println("IA sem jogadas! Ganhaste!")
        deleteSave()
      case Some(newBoard) =>
        val destStr = maybeDestination.map(d => s"(${d._1},${d._2})").getOrElse("?")
        println(s"IA jogou para $destStr")
        // Mostra sempre o tabuleiro com a jogada final da IA antes de continuar
        println(boardToString(newBoard, boardSize))
        val nextState = state.copy(
          board         = newBoard,
          random        = newRandom,
          currentPlayer = Stone.Black,
          openSquares   = newOpenSquares
        )
        gameLoop(nextState, state :: history)
    }
  }

  // ─── Cadeia de Saltos ─────────────────────────────────────────────────────

  def jumpChain(
                 board: Board,
                 fromSquare: Coord2D,
                 boardSize: Int,
                 openSquares: List[Coord2D],
                 moveStartTime: Long,
                 maxSeconds: Long
               ): (Board, List[Coord2D]) = {

    val toSquare = readCoordPara(board, fromSquare, boardSize)

    play(board, Stone.Black, fromSquare, toSquare, openSquares) match {
      case (None, _) =>
        println("Jogada invalida! Tenta outra vez.")
        jumpChain(board, fromSquare, boardSize, openSquares, moveStartTime, maxSeconds)

      case (Some(updatedBoard), updatedOpenSquares) =>
        println("\nTabuleiro apos salto:")
        println(boardToString(updatedBoard, boardSize))

        val moreJumps  = validJumpsFrom(updatedBoard, toSquare, Stone.Black, boardSize)
        val timeIsUp   = isTimeExceeded(moveStartTime, System.currentTimeMillis(), maxSeconds)

        if (moreJumps.nonEmpty && !timeIsUp) {
          println(s"Podes continuar a capturar a partir de (${toSquare._1},${toSquare._2})!")
          println("Queres continuar? (s = sim / n = nao)")
          if (readLine().trim.toLowerCase == "s")
            jumpChain(updatedBoard, toSquare, boardSize, updatedOpenSquares, moveStartTime, maxSeconds)
          else
            (updatedBoard, updatedOpenSquares)
        } else {
          if (timeIsUp && moreJumps.nonEmpty) println("Tempo esgotado! Jogada terminada.")
          (updatedBoard, updatedOpenSquares)
        }
    }
  }

  // ─── Leitura de Input ─────────────────────────────────────────────────────

  def parseIntInput(input: String): Option[Int] =
    try Some(input.trim.toInt)
    catch { case _: NumberFormatException => None }

  def parseCoord(input: String): Option[Coord2D] =
    input.split(",").map(_.trim) match {
      case Array(rowStr, colStr) =>
        try Some((rowStr.toInt, colStr.toInt))
        catch { case _: NumberFormatException => None }
      case _ => None
    }

  @annotation.tailrec
  def readCoordDe(board: Board, boardSize: Int): Coord2D = {
    print("De (linha,coluna): ")
    parseCoord(readLine()) match {
      case None =>
        println("Formato invalido! Usa linha,coluna com numeros inteiros.")
        readCoordDe(board, boardSize)
      case Some((row, col)) if row < 0 || row >= boardSize || col < 0 || col >= boardSize =>
        println("Coordenada fora do tabuleiro!")
        readCoordDe(board, boardSize)
      case Some((row, col)) if !board.get((row, col)).contains(Stone.Black) =>
        println("Nao podes mover esta casa! Escolhe uma pedra preta valida.")
        readCoordDe(board, boardSize)
      case Some((row, col)) if validJumpsFrom(board, (row, col), Stone.Black, boardSize).isEmpty =>
        println("Esta pedra nao tem saltos validos! Escolhe outra.")
        readCoordDe(board, boardSize)
      case Some(coord) =>
        coord
    }
  }

  @annotation.tailrec
  def readCoordPara(board: Board, fromSquare: Coord2D, boardSize: Int): Coord2D = {
    val validJumps = validJumpsFrom(board, fromSquare, Stone.Black, boardSize)
    println(s"Saltos possiveis: ${validJumps.map(c => s"(${c._1},${c._2})").mkString(", ")}")
    print("Para (linha,coluna): ")

    parseCoord(readLine()) match {
      case None =>
        println("Formato invalido! Usa linha,coluna com numeros inteiros.")
        readCoordPara(board, fromSquare, boardSize)
      case Some(coord) if !validJumps.contains(coord) =>
        println("Destino invalido! Escolhe uma das casas indicadas.")
        readCoordPara(board, fromSquare, boardSize)
      case Some(coord) =>
        coord
    }
  }
}