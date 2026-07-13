import scalafx.application.JFXApp3
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, Alert}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.layout.{GridPane, VBox, HBox, BorderPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.text.{Font, FontWeight}
import Logica._
import Logica.Stone._

object GUI extends JFXApp3 {

  // ─── Estado mutavel da GUI (permitido pelo enunciado) ─────────────────────

  val fixedBoardSize = 6
  var difficultyLevel: Int = 1
  var maxSecondsPerMove: Long = 30L

  var board: Board = initialBoard(fixedBoardSize)
  var openSquares: List[Coord2D] = Nil
  var random: MyRandom = MyRandom(System.currentTimeMillis())
  var currentPlayer: Stone = Stone.Black
  var history: List[GameState] = Nil
  var selectedSquare: Option[Coord2D] = None
  var validDestinations: List[Coord2D] = Nil
  var moveStartTime: Long = 0L
  var gameOver: Boolean = false

  // ─── Componentes da UI ────────────────────────────────────────────────────

  var boardGrid: GridPane = _
  var statusLabel: Label = _
  var playerLabel: Label = _
  var timerLabel: Label = _
  var cells: Array[Array[Button]] = Array.ofDim[Button](fixedBoardSize, fixedBoardSize)

  // ─── Logica de suporte ────────────────────────────────────────────────────

  def computeOpenSquares(board: Board): List[Coord2D] =
    (for {
      row <- 0 until fixedBoardSize
      col <- 0 until fixedBoardSize
      if !board.contains((row, col))
    } yield (row, col)).toList

  // ─── Inicio / reinicio de jogo ────────────────────────────────────────────

  def startNewGame(): Unit = {
    val fullBoard = initialBoard(fixedBoardSize)
    val mid       = fixedBoardSize / 2
    board = removeInitialPair(fullBoard, (mid - 1, mid - 1), (mid - 1, mid), fixedBoardSize)
      .getOrElse(fullBoard - ((mid - 1, mid - 1)) - ((mid - 1, mid)))

    openSquares   = computeOpenSquares(board)
    random        = MyRandom(System.currentTimeMillis())
    currentPlayer = Stone.Black
    history       = Nil
    selectedSquare   = None
    validDestinations = Nil
    gameOver      = false
    moveStartTime = System.currentTimeMillis()

    updateBoard()
    statusLabel.text  = "Jogo iniciado! A tua vez (Preto)."
    playerLabel.text  = "Jogador: Preto (B)"
    timerLabel.text   = s"Tempo por jogada: ${maxSecondsPerMove}s"
  }

  // ─── Renderizacao do tabuleiro ────────────────────────────────────────────

  def updateBoard(): Unit = {
    for (row <- 0 until fixedBoardSize; col <- 0 until fixedBoardSize) {
      val button    = cells(row)(col)
      val isSelected  = selectedSquare.contains((row, col))
      val isValidDest = validDestinations.contains((row, col))

      button.style = (isSelected, isValidDest) match {
        case (true, _) =>
          "-fx-background-color: #f0c040; -fx-border-color: #c08000; -fx-border-width: 2;"
        case (_, true) =>
          "-fx-background-color: #80e080; -fx-border-color: #208020; -fx-border-width: 2;"
        case _ =>
          "-fx-background-color: #d4a84b; -fx-border-color: #a07830; -fx-border-width: 1;"
      }

      button.graphic = board.get((row, col)) match {
        case Some(Black) =>
          new Circle { radius = 18; fill = Color.Black; stroke = Color.DarkGray; strokeWidth = 1.5 }
        case Some(White) =>
          new Circle { radius = 18; fill = Color.White; stroke = Color.Gray; strokeWidth = 1.5 }
        case None =>
          null
      }
    }
  }

  // ─── Clique numa celula ───────────────────────────────────────────────────

  def handleCellClick(row: Int, col: Int): Unit = {
    if (gameOver || currentPlayer != Black) return

    if (isTimeExceeded(moveStartTime, System.currentTimeMillis(), maxSecondsPerMove)) {
      statusLabel.text = "Tempo esgotado! Passa a vez para a IA."
      currentPlayer = White
      updateBoard()
      runAiTurn()
      return
    }

    val clickedSquare = (row, col)

    selectedSquare match {
      case None =>
        // Selecionar uma pedra preta com saltos validos
        if (board.get(clickedSquare).contains(Black)) {
          val jumps = validJumpsFrom(board, clickedSquare, Black, fixedBoardSize)
          if (jumps.nonEmpty) {
            selectedSquare    = Some(clickedSquare)
            validDestinations = jumps
            statusLabel.text  = s"Pedra em ($row,$col) selecionada. Escolhe o destino (verde)."
            updateBoard()
          } else {
            statusLabel.text = "Esta pedra nao tem saltos validos! Escolhe outra."
          }
        }

      case Some(fromSquare) =>
        if (validDestinations.contains(clickedSquare)) {
          executePlayerMove(fromSquare, clickedSquare)
        } else {
          // Clique noutra pedra preta: trocar selecao
          if (board.get(clickedSquare).contains(Black)) {
            val jumps = validJumpsFrom(board, clickedSquare, Black, fixedBoardSize)
            if (jumps.nonEmpty) {
              selectedSquare    = Some(clickedSquare)
              validDestinations = jumps
              statusLabel.text  = s"Pedra em ($row,$col) selecionada."
              updateBoard()
              return
            }
          }
          // Clique invalido: cancelar selecao
          selectedSquare    = None
          validDestinations = Nil
          statusLabel.text  = "Selecao cancelada. Escolhe uma pedra preta."
          updateBoard()
        }
    }
  }

  // ─── Executar jogada do jogador ───────────────────────────────────────────

  def executePlayerMove(fromSquare: Coord2D, toSquare: Coord2D): Unit = {
    play(board, Black, fromSquare, toSquare, openSquares) match {
      case (None, _) =>
        statusLabel.text = "Jogada invalida!"

      case (Some(updatedBoard), updatedOpenSquares) =>
        // Guardar estado para undo antes de executar
        history = GameState(board, random, currentPlayer, fixedBoardSize, openSquares, maxSecondsPerMove, difficultyLevel) :: history

        board       = updatedBoard
        openSquares = updatedOpenSquares

        // Verificar se pode continuar a capturar
        val moreJumps = validJumpsFrom(board, toSquare, Black, fixedBoardSize)

        if (moreJumps.nonEmpty) {
          // Manter a pedra selecionada para continuar a cadeia
          selectedSquare    = Some(toSquare)
          validDestinations = moreJumps
          statusLabel.text  = s"Podes continuar a capturar! Clica num destino verde ou noutro sitio para parar."
          updateBoard()
        } else {
          selectedSquare    = None
          validDestinations = Nil
          updateBoard()
          checkAndContinue()
        }
    }
  }

  // Chamada quando o jogador clica fora dos destinos validos durante uma cadeia
  def finishPlayerTurn(): Unit = {
    selectedSquare    = None
    validDestinations = Nil
    updateBoard()
    checkAndContinue()
  }

  // Verifica vitoria ou passa para a IA
  def checkAndContinue(): Unit = {
    checkVictory(board, White, fixedBoardSize) match {
      case Some(_) =>
        gameOver = true
        statusLabel.text = "Vitoria do Preto (tu)! Parabens!"
        playerLabel.text = "GANHOU!"
      case None =>
        currentPlayer    = White
        playerLabel.text = "Vez da IA (Branco)"
        statusLabel.text = "IA a pensar..."
        updateBoard()
        // Pequeno delay para a UI atualizar antes da IA jogar
        new Thread(() => {
          Thread.sleep(400)
          Platform.runLater { runAiTurn() }
        }).start()
    }
  }

  // ─── Turno da IA ──────────────────────────────────────────────────────────

  def runAiTurn(): Unit = {
    aiMove(board, White, fixedBoardSize, difficultyLevel, random, openSquares) match {
      case (None, _, _, _) =>
        gameOver = true
        statusLabel.text = "IA sem jogadas! Vitoria do Preto (tu)!"
        playerLabel.text = "GANHOU!"

      case (Some(updatedBoard), newRandom, updatedOpenSquares, maybeDestination) =>
        board       = updatedBoard
        random      = newRandom
        openSquares = updatedOpenSquares

        val destStr = maybeDestination.map(d => s"(${d._1},${d._2})").getOrElse("?")

        // Atualiza sempre o tabuleiro para mostrar a jogada final da IA
        updateBoard()

        checkVictory(board, Black, fixedBoardSize) match {
          case Some(_) =>
            gameOver = true
            statusLabel.text = s"IA jogou para $destStr. IA venceu!"
            playerLabel.text = "IA VENCEU"
          case None =>
            currentPlayer    = Black
            moveStartTime    = System.currentTimeMillis()
            playerLabel.text = "Jogador: Preto (B)"
            statusLabel.text = s"IA jogou para $destStr. A tua vez!"
        }
    }
  }

  // ─── Undo ─────────────────────────────────────────────────────────────────

  def doUndo(): Unit = {
    if (currentPlayer == White) {
      statusLabel.text = "Nao podes fazer undo durante o turno da IA!"
      return
    }
    undo(history) match {
      case None =>
        statusLabel.text = "Sem jogadas para desfazer!"
      case Some((previousState, remainingHistory)) =>
        board             = previousState.board
        random            = previousState.random
        openSquares       = previousState.openSquares
        currentPlayer     = previousState.currentPlayer
        history           = remainingHistory
        selectedSquare    = None
        validDestinations = Nil
        gameOver          = false
        moveStartTime     = System.currentTimeMillis()
        updateBoard()
        statusLabel.text  = "Jogada desfeita."
        playerLabel.text  = "Jogador: Preto (B)"
    }
  }

  // ─── Configuracoes ────────────────────────────────────────────────────────

  def toggleDifficulty(): Unit = {
    difficultyLevel = if (difficultyLevel == 1) 2 else 1
    val label = if (difficultyLevel == 1) "Facil" else "Dificil"
    statusLabel.text = s"Dificuldade alterada para: $label"
  }

  // ─── Construcao da UI ─────────────────────────────────────────────────────

  override def start(): Unit = {

    // --- Tabuleiro ---
    boardGrid = new GridPane {
      hgap = 4
      vgap = 4
      padding = Insets(12)
      alignment = Pos.Center
      style = "-fx-background-color: #8B5E3C; -fx-border-color: #5a3010; -fx-border-width: 3;"
    }

    for (row <- 0 until fixedBoardSize; col <- 0 until fixedBoardSize) {
      val button = new Button {
        minWidth  = 60
        minHeight = 60
        maxWidth  = 60
        maxHeight = 60
      }
      val r = row
      val c = col
      button.onAction = _ => {
        // Se ha uma cadeia ativa e o jogador clica fora dos destinos validos, termina a jogada
        if (selectedSquare.isDefined && !validDestinations.contains((r, c)) && !board.get((r,c)).contains(Black)) {
          finishPlayerTurn()
        } else {
          handleCellClick(r, c)
        }
      }
      cells(row)(col) = button
      boardGrid.add(button, col, row)
    }

    // --- Labels ---
    playerLabel = new Label("Jogador: Preto (B)") {
      style = "-fx-font-size: 16px; -fx-font-weight: bold; -fx-text-fill: #2c1a0a;"
    }

    statusLabel = new Label("") {
      style = "-fx-font-size: 13px; -fx-text-fill: #3a2010;"
      wrapText = true
      maxWidth = 380
    }

    timerLabel = new Label("") {
      style = "-fx-font-size: 12px; -fx-text-fill: #555;"
    }

    // --- Botoes de controlo ---
    val newGameButton = new Button("Novo Jogo") {
      style = "-fx-background-color: #4a7c4e; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 110px;"
      onAction = _ => startNewGame()
    }

    val undoButton = new Button("Undo") {
      style = "-fx-background-color: #c07030; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 110px;"
      onAction = _ => doUndo()
    }

    val difficultyButton = new Button("Dificuldade: Facil") {
      style = "-fx-background-color: #5060a0; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 140px;"
      onAction = _ => {
        toggleDifficulty()
        text = if (difficultyLevel == 1) "Dificuldade: Facil" else "Dificuldade: Dificil"
      }
    }

    val finishTurnButton = new Button("Terminar Jogada") {
      style = "-fx-background-color: #a03030; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 140px;"
      onAction = _ => {
        if (selectedSquare.isDefined && currentPlayer == Black) finishPlayerTurn()
      }
    }

    val abandonButton = new Button("Abandonar") {
      style = "-fx-background-color: #804000; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 110px;"
      onAction = _ => {
        val alert = new Alert(AlertType.Confirmation) {
          title       = "Abandonar jogo"
          headerText  = "Tens a certeza que queres abandonar?"
          contentText = "O jogo atual sera perdido."
        }
        alert.showAndWait() match {
          case Some(scalafx.scene.control.ButtonType.OK) =>
            deleteSave()
            startNewGame()
          case _ => ()
        }
      }
    }

    val exitButton = new Button("Sair") {
      style = "-fx-background-color: #404040; -fx-text-fill: white; -fx-font-weight: bold; -fx-min-width: 80px;"
      onAction = _ => {
        val alert = new Alert(AlertType.Confirmation) {
          title       = "Sair"
          headerText  = "Tens a certeza que queres sair?"
          contentText = "O jogo nao guardado sera perdido."
        }
        alert.showAndWait() match {
          case Some(scalafx.scene.control.ButtonType.OK) => Platform.exit()
          case _ => ()
        }
      }
    }

    val buttonRow = new HBox(8, newGameButton, undoButton, difficultyButton, finishTurnButton, abandonButton, exitButton) {
      alignment = Pos.Center
      padding   = Insets(8)
    }

    val legendLabel = new Label("B = Preto (tu)   W = Branco (IA)   Amarelo = selecionado   Verde = destino valido") {
      style = "-fx-font-size: 11px; -fx-text-fill: #666;"
    }

    val root = new VBox(8,
      playerLabel,
      timerLabel,
      boardGrid,
      statusLabel,
      buttonRow,
      legendLabel
    ) {
      padding   = Insets(15)
      alignment = Pos.Center
      style     = "-fx-background-color: #f5e6d0;"
    }

    stage = new JFXApp3.PrimaryStage {
      title  = "Konane"
      scene  = new Scene(root, 680, 600)
      resizable = false
    }

    startNewGame()
  }
}