package tennis

case class Score(playerName: String, var score: Int) {
  def diffScore(player2Score: Score): Int = score - player2Score.score

  def isAdvantage(player2Score: Score) = (score >= 4 || player2Score.score >= 4)

  def wonPoint() = score += 1

  def isGameWon(player2Score: Score) = (this.diffScore(player2Score) >= 2) && (this.score >= 4)

  def scoreTxt() = {
    score match {
      case 0 => "Love"
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
    }
  }
}

object Score {
  def init(playerName: String) = Score(playerName, 0)
}

object ScoreCall {

  def calculateScore(player1Score: Score, player2Score: Score): String = {
    if (player1Score.score == player2Score.score) {
      parseScoreWhenDeuce(player1Score.score)
    }
    else if (player1Score.isAdvantage(player2Score)) {
      parseScoreWhenOver4(player1Score, player2Score)
    }
    else {
      parsePearScore(player1Score, player2Score)
    }
  }

  private def parseScoreWhenDeuce(scoreInt: Int): String = {
    scoreInt match {
      case 0 => "Love-All"
      case 1 => "Fifteen-All"
      case 2 => "Thirty-All"
      case _ => "Deuce"
    }
  }

  private def parseScoreWhenOver4(player1Score: Score, player2Score: Score): String = {
    val minusResult = player1Score.diffScore(player2Score)
    if (minusResult == 1) s"Advantage ${player1Score.playerName}"
    else if (minusResult == -1) s"Advantage ${player2Score.playerName}"
    else {
      if (player1Score.isGameWon(player2Score)) {
        s"Win for ${player1Score.playerName}"
      } else {
        s"Win for ${player2Score.playerName}"
      }
    }
  }


  private def parsePearScore(player1Score: Score, player2Score: Score) = player1Score.scoreTxt + "-" + player2Score.scoreTxt

}

class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {

  val player1Score = Score.init(player1Name)
  val player2Score = Score.init(player2Name)

  def wonPoint(playerName: String) {
    if (playerName == player1Score.playerName)
      player1Score.wonPoint()
    else
      player2Score.wonPoint()
  }

  def calculateScore(): String = {
    ScoreCall.calculateScore(player1Score, player2Score)
  }

}


