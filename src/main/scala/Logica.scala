import scala.collection.parallel.immutable.ParMap
import java.io._
import java.nio.file.{Files, Paths}

object Logica {

  type Coord2D = (Int, Int)
  type Board = ParMap[Coord2D, Stone]

  enum Stone:
    case Black, White

  // Guarda o estado completo para suportar undo e persistencia entre sessoes
  // Representa o estado completo de um jogo num dado momento:
  // usado para undo (historico de estados) e persistencia entre sessoes
  case class GameState(
                        board: Board,             // tabuleiro atual
                        random: MyRandom,         // gerador aleatorio atual (puro)
                        currentPlayer: Stone,     // jogador que vai jogar a seguir
                        boardSize: Int,           // dimensao do tabuleiro (ex: 6 para 6x6)
                        openSquares: List[Coord2D], // casas atualmente vazias
                        maxSecondsPerMove: Long,  // tempo maximo por jogada em segundos
                        difficultyLevel: Int      // nivel de dificuldade da IA (1=facil, 2=dificil)
                      )

  // ─── Inicializacao ────────────────────────────────────────────────────────

  // Cria o tabuleiro boardSize x boardSize cheio com padrao alternado:
  // preto nas posicoes em que (linha + coluna) e par, branco nas impares
  def initialBoard(boardSize: Int): Board = {
    val entries = for {
      row <- 0 until boardSize
      col <- 0 until boardSize
      stone = if ((row + col) % 2 == 0) Stone.Black else Stone.White
    } yield (row, col) -> stone
    ParMap(entries*)
  }

  // Remove uma pedra preta e uma pedra branca adjacentes do centro ou canto
  // para abrir o jogo. Devolve None se as coordenadas forem invalidas.
  def removeInitialPair(board: Board, blackSquare: Coord2D, whiteSquare: Coord2D, boardSize: Int): Option[Board] = {
    val blackIsValid  = board.get(blackSquare).contains(Stone.Black)
    val whiteIsValid  = board.get(whiteSquare).contains(Stone.White)
    val areAdjacent   = areOrthogonallyAdjacent(blackSquare, whiteSquare)
    val validPosition = isCenter(blackSquare, boardSize) || isCorner(blackSquare, boardSize) ||
      isCenter(whiteSquare, boardSize) || isCorner(whiteSquare, boardSize)
    if (blackIsValid && whiteIsValid && areAdjacent && validPosition)
      Some(board - blackSquare - whiteSquare)
    else None
  }

  // Verifica se uma casa e uma das 4 posicoes centrais do tabuleiro
  def isCenter(square: Coord2D, boardSize: Int): Boolean = {
    val mid = boardSize / 2
    square == (mid, mid) || square == (mid - 1, mid) ||
      square == (mid, mid - 1) || square == (mid - 1, mid - 1)
  }

  // Verifica se uma casa e um dos 4 cantos do tabuleiro
  def isCorner(square: Coord2D, boardSize: Int): Boolean =
    square == (0, 0) || square == (0, boardSize - 1) ||
      square == (boardSize - 1, 0) || square == (boardSize - 1, boardSize - 1)

  // Verifica se duas casas sao adjacentes ortogonalmente (cima, baixo, esquerda, direita)
  def areOrthogonallyAdjacent(squareA: Coord2D, squareB: Coord2D): Boolean = {
    val (rowA, colA) = squareA
    val (rowB, colB) = squareB
    (rowA == rowB && Math.abs(colA - colB) == 1) || (colA == colB && Math.abs(rowA - rowB) == 1)
  }

  // ─── T1: randomMove ──────────────────────────────────────────────────────

  // Escolhe aleatoriamente uma coordenada da lista de posicoes disponiveis.
  // Pura: recebe e devolve MyRandom sem side effects.
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (value, nextRand) = rand.nextInt
    val index = Math.abs(value % lstOpenCoords.size)
    (lstOpenCoords(index), nextRand)
  }

  // ─── Movimentos validos ───────────────────────────────────────────────────

  // Calcula para onde uma pedra pode saltar:
  // tem de haver adversario na casa do meio e destino tem de estar vazio
  def validJumpsFrom(board: Board, fromSquare: Coord2D, player: Stone, boardSize: Int): List[Coord2D] = {
    val opponent   = if player == Stone.Black then Stone.White else Stone.Black
    val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))
    directions.flatMap { case (rowDelta, colDelta) =>
      val middleSquare      = (fromSquare._1 + rowDelta,           fromSquare._2 + colDelta)
      val destinationSquare = (fromSquare._1 + 2 * rowDelta, fromSquare._2 + 2 * colDelta)
      val withinBounds =
        destinationSquare._1 >= 0 && destinationSquare._1 < boardSize &&
          destinationSquare._2 >= 0 && destinationSquare._2 < boardSize
      if (withinBounds && board.get(middleSquare).contains(opponent) && !board.contains(destinationSquare))
        List(destinationSquare)
      else Nil
    }
  }

  // Todas as jogadas validas do jogador: lista de pares (origem, destino)
  // Usa flatMap para iterar sobre todas as pedras do jogador e os seus saltos possiveis
  def allValidMoves(board: Board, player: Stone, boardSize: Int): List[(Coord2D, Coord2D)] =
    board.toList.flatMap {
      case (square, stone) if stone == player =>
        validJumpsFrom(board, square, player, boardSize).map(destination => (square, destination))
      case _ => Nil
    }

  // ─── T2: play ────────────────────────────────────────────────────────────

  // Executa uma jogada: move de fromSquare para toSquare se for valida.
  // Remove a pedra capturada (a que esta no meio).
  // Devolve (Some(novoTabuleiro), novasPosicoesLivres) ou (None, listaInalterada).
  def play(
            board: Board,
            player: Stone,
            coordFrom: Coord2D,
            coordTo: Coord2D,
            lstOpenCoords: List[Coord2D]
          ): (Option[Board], List[Coord2D]) = {

    // Deriva boardSize a partir das posicoes conhecidas (board + casas livres)
    val allPositions = board.seq.keys.toList ++ lstOpenCoords
    val boardSize = allPositions.map(_._1).max + 1

    val validDestinations = validJumpsFrom(board, coordFrom, player, boardSize)

    if (!board.get(coordFrom).contains(player) || !validDestinations.contains(coordTo))
      (None, lstOpenCoords)
    else {
      val rowDelta = coordTo._1 - coordFrom._1
      val colDelta = coordTo._2 - coordFrom._2

      val maybeCapturedSquare =
        if (rowDelta == 0 && Math.abs(colDelta) == 2)
          Some((coordFrom._1, coordFrom._2 + colDelta / 2))
        else if (colDelta == 0 && Math.abs(rowDelta) == 2)
          Some((coordFrom._1 + rowDelta / 2, coordFrom._2))
        else None

      maybeCapturedSquare match {
        case None => (None, lstOpenCoords)
        case Some(capturedSquare) =>
          val updatedBoard     = board - coordFrom - capturedSquare + (coordTo -> player)
          val updatedOpenCoords = (coordFrom :: capturedSquare :: lstOpenCoords).filterNot(_ == coordTo)
          (Some(updatedBoard), updatedOpenCoords)
      }
    }
  }

  // ─── T3: playRandomly ────────────────────────────────────────────────────

  // Joga aleatoriamente usando a funcao f para escolher pedra e destino.
  // Funcao de ordem superior: f e parametrizavel para injetar diferentes estrategias.
  def playRandomly(
                    board: Board,
                    r: MyRandom,
                    player: Stone,
                    lstOpenCoords: List[Coord2D],
                    f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    // Deriva boardSize a partir das posicoes conhecidas (board + casas livres)
    val allPositions = board.seq.keys.toList ++ lstOpenCoords
    val boardSize    = allPositions.map(_._1).max + 1
    val validMoves   = allValidMoves(board, player, boardSize)

    if (validMoves.isEmpty) {
      (None, r, lstOpenCoords, None)
    } else {
      val originSquares              = validMoves.map(_._1).distinct
      val (chosenOrigin, r2)          = f(originSquares, r)
      val destinations                = validMoves.filter(_._1 == chosenOrigin).map(_._2)
      val (chosenDestination, r3)     = f(destinations, r2)

      val (maybeBoard, updatedOpenCoords) =
        play(board, player, chosenOrigin, chosenDestination, lstOpenCoords)

      maybeBoard match {
        case Some(updatedBoard) =>
          (Some(updatedBoard), r3, updatedOpenCoords, Some(chosenDestination))
        case None =>
          (None, r3, lstOpenCoords, None)
      }
    }
  }

  // ─── T4: boardToString ───────────────────────────────────────────────────

  // Representa o tabuleiro na consola: B=preto, W=branco, .=vazio
  def boardToString(board: Board, boardSize: Int): String = {
    val header = "    " + (0 until boardSize).map(col => f"$col%-3d").mkString
    val rows = (0 until boardSize).map { row =>
      val rowContent = (0 until boardSize).map { col =>
        board.get((row, col)) match {
          case Some(Stone.Black) => "B  "
          case Some(Stone.White) => "W  "
          case None              => ".  "
        }
      }.mkString
      s"$row | $rowContent"
    }.mkString("\n")
    s"$header\n$rows"
  }

  // ─── T5: checkVictory ────────────────────────────────────────────────────

  // Se o jogador atual nao tem jogadas validas, o adversario ganha
  def checkVictory(board: Board, currentPlayer: Stone, boardSize: Int): Option[Stone] =
    if (allValidMoves(board, currentPlayer, boardSize).isEmpty)
      Some(if currentPlayer == Stone.Black then Stone.White else Stone.Black)
    else None

  // ─── T6: temporizador e undo ─────────────────────────────────────────────

  // Verifica se o tempo limite para uma jogada foi excedido
  def isTimeExceeded(startTime: Long, currentTime: Long, maxSecondsPerMove: Long): Boolean =
    (currentTime - startTime) > maxSecondsPerMove * 1000

  // Undo: devolve o estado anterior e o historico atualizado, ou None se nao ha historico
  def undo(history: List[GameState]): Option[(GameState, List[GameState])] =
    history match {
      case previous :: rest => Some((previous, rest))
      case Nil              => None
    }

  // ─── IA ──────────────────────────────────────────────────────────────────

  // Nivel 1: jogada completamente aleatoria (executa cadeia completa de saltos)
  // Nivel 2: escolhe a jogada que permite a maior cadeia de capturas
  def aiMove(
              board: Board,
              player: Stone,
              boardSize: Int,
              difficultyLevel: Int,
              random: MyRandom,
              openSquares: List[Coord2D]
            ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =
    difficultyLevel match {
      case 2 => greedyMove(board, player, boardSize, random, openSquares)
      case _ =>
        // Nivel 1: escolhe a primeira jogada aleatoriamente e depois executa a cadeia completa
        playRandomly(board, random, player, openSquares, randomMove) match {
          case (None, newRandom, newOpen, None) => (None, newRandom, newOpen, None)
          case (Some(newBoard), newRandom, newOpen, Some(destination)) =>
            val (finalBoard, finalOpen, finalDest) =
              executeFullChain(newBoard, destination, player, boardSize, newOpen, randomMove, newRandom)._1
            (Some(finalBoard), newRandom, finalOpen, Some(finalDest))
          case other => other
        }
    }

  // Executa a cadeia completa de saltos da IA a partir de uma posicao.
  // Continua a saltar enquanto houver saltos validos, usando f para escolher o proximo destino.
  // Devolve (tabuleiro final, casas livres finais, ultima posicao) e o random atualizado.
  private def executeFullChain(
                                board: Board,
                                currentSquare: Coord2D,
                                player: Stone,
                                boardSize: Int,
                                openSquares: List[Coord2D],
                                f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom),
                                random: MyRandom
                              ): ((Board, List[Coord2D], Coord2D), MyRandom) = {
    val moreJumps = validJumpsFrom(board, currentSquare, player, boardSize)
    if (moreJumps.isEmpty)
      ((board, openSquares, currentSquare), random)
    else {
      val (nextDestination, newRandom) = f(moreJumps, random)
      play(board, player, currentSquare, nextDestination, openSquares) match {
        case (None, _) =>
          // Nao devia acontecer, mas por seguranca para aqui
          ((board, openSquares, currentSquare), newRandom)
        case (Some(updatedBoard), updatedOpenSquares) =>
          executeFullChain(updatedBoard, nextDestination, player, boardSize, updatedOpenSquares, f, newRandom)
      }
    }
  }

  // Nivel 2: escolhe a pedra e primeiro salto com a cadeia mais longa, depois executa-a toda
  private def greedyMove(
                          board: Board,
                          player: Stone,
                          boardSize: Int,
                          random: MyRandom,
                          openSquares: List[Coord2D]
                        ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    val validMoves = allValidMoves(board, player, boardSize)
    if (validMoves.isEmpty) (None, random, openSquares, None)
    else {
      // Escolhe o movimento inicial com a cadeia de capturas mais longa
      val bestMove = validMoves.maxBy { case (origin, _) =>
        captureChainLength(board, origin, player, boardSize)
      }
      play(board, player, bestMove._1, bestMove._2, openSquares) match {
        case (None, _) => (None, random, openSquares, None)
        case (Some(updatedBoard), updatedOpenSquares) =>
          // Apos o primeiro salto, continua a cadeia escolhendo sempre o salto mais longo
          val greedyF: (List[Coord2D], MyRandom) => (Coord2D, MyRandom) = (destinations, rand) =>
            (destinations.maxBy(dest => captureChainLength(
              board, dest, player, boardSize
            )), rand)
          val ((finalBoard, finalOpenSquares, finalDest), _) =
            executeFullChain(updatedBoard, bestMove._2, player, boardSize, updatedOpenSquares, greedyF, random)
          (Some(finalBoard), random, finalOpenSquares, Some(finalDest))
      }
    }
  }

  // Calcula o comprimento maximo da cadeia de capturas a partir de uma posicao (recursivo)
  private def captureChainLength(board: Board, fromSquare: Coord2D, player: Stone, boardSize: Int): Int = {
    val jumps = validJumpsFrom(board, fromSquare, player, boardSize)
    if (jumps.isEmpty) 0
    else jumps.map { destination =>
      val rowDelta       = destination._1 - fromSquare._1
      val colDelta       = destination._2 - fromSquare._2
      val capturedSquare = (fromSquare._1 + rowDelta / 2, fromSquare._2 + colDelta / 2)
      val updatedBoard   = board - fromSquare - capturedSquare + (destination -> player)
      1 + captureChainLength(updatedBoard, destination, player, boardSize)
    }.max
  }

  // ─── Persistencia ────────────────────────────────────────────────────────

  private val saveFilePath = "konane_save.dat"

  // Guarda o estado atual em ficheiro para poder continuar noutra sessao
  def saveGame(state: GameState): Boolean =
    try {
      val outputStream = new ObjectOutputStream(new FileOutputStream(saveFilePath))
      outputStream.writeObject(state)
      outputStream.close()
      true
    } catch {
      case _: Exception => false
    }

  // Carrega um jogo guardado anteriormente; devolve None se nao existir ou falhar
  def loadGame(): Option[GameState] =
    try {
      if (!Files.exists(Paths.get(saveFilePath))) None
      else {
        val inputStream = new ObjectInputStream(new FileInputStream(saveFilePath))
        val state       = inputStream.readObject().asInstanceOf[GameState]
        inputStream.close()
        Some(state)
      }
    } catch {
      case _: Exception => None
    }

  // Remove o ficheiro de jogo guardado
  def deleteSave(): Unit =
    try { Files.deleteIfExists(Paths.get(saveFilePath)) }
    catch { case _: Exception => () }
}