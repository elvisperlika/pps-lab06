package ex2

import ex2.ConferenceReviewing.Question.{CONFIDENCE, FINAL}
import ex2.ConferenceReviewing.{Article, article, score}

import scala.collection.immutable.List

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the 
 * system does not keep track of the identity of reviewers
 *
 */

object ConferenceReviewing:
  enum Question:
    case RELEVANCE, // ("È importante per questa conferenza?"),
    SIGNIFICANCE, // ("Produce contributo scientifico?"),
    CONFIDENCE, // ("Ti senti competente a commentarlo?");
    FINAL // ("É un articolo da accettare?")

  opaque type Article = Int
  opaque type Score = Int | Double

  def score(value: Int | Double): Score = value
  def article(id: Int): Article = id

  object Score:
    def fromDouble(d: Double): Score = d
    extension (s: Score)
      def toDouble: Double = s match
        case i: Int => i.toDouble
        case d: Double => d
      def +(other: Score): Score = s.toDouble + other.toDouble
      def /(other: Int | Double): Score = s.toDouble / other.toDouble
      def >(other: Int | Double): Boolean = s.toDouble > other.toDouble
      def >=(other: Int | Double): Boolean = s.toDouble >= other.toDouble
      def *(other: Int | Double | Score): Score = s.toDouble * other.toDouble

trait ConferenceReviewing {
  import ConferenceReviewing.*

  val N_QUESTIONS: Int = 4

  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Article, scores: Map[Question, Score]): Unit

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Article, relevance: Score, significance: Score, confidence: Score, fin: Score): Unit

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Article): Score

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles(): Set[Article]


  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Article, Score)]


  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10  
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Article, Score]

  /**
   *
    * @return number of reviews
   */
  def reviewsNumber: Int
}

class ConferenceReviewingImpl extends ConferenceReviewing {

  import ConferenceReviewing.Score.*
  import ConferenceReviewing.*

  private var reviews: List[(Article, Map[Question, Score])] = List()

  override def loadReview(article: Article, scores: Map[Question, Score]): Unit =
    reviews = (article, scores) :: reviews

  override def loadReview(article: Article, relevance: Score, significance: Score, confidence: Score, fin: Score): Unit =
    loadReview(article, Map(
      (Question.RELEVANCE, relevance),
      (Question.SIGNIFICANCE, significance),
      (Question.CONFIDENCE, confidence),
      (Question.FINAL, fin)
    ))

  private def getReviewsByArticle(article: Article): Map[Question, Score] =
    reviews.find((a, _) => a == article).get._2

  override def averageFinalScore(article: Article): Score =
    getReviewsByArticle(article).foldLeft[Score](score(0))({
          case (acc, (_, s)) => acc + s
    }) / getReviewsByArticle(article).size

  override def acceptedArticles(): Set[Article] =
    import ConferenceReviewing.Question.RELEVANCE
    reviews.filter((a, scores) => averageFinalScore(a) > 5 && scores(RELEVANCE) >= 8)
      .foldLeft(Set.empty[Article])((set, rev) => set + rev._1)

  override def sortedAcceptedArticles: List[(Article, Score)] =
    acceptedArticles().foldLeft(List[(Article, Score)]())({
        case (list, a) => (a, averageFinalScore(a)) :: list
    }).sorted((s1, s2) => s1._2.toDouble.compare(s2._2.toDouble))

  override def averageWeightedFinalScoreMap: Map[Article, Score] =
    reviews.foldLeft(Map[Article, Score]())({
      (map, review) => (map, review) match
        case (map, (article, scoresMap)) => map + (article -> scoresMap(CONFIDENCE) * scoresMap(FINAL) / 10)
    })

  override def reviewsNumber: Int = reviews.size
}

@main def test(): Unit =

  val cr: ConferenceReviewing = ConferenceReviewingImpl()

  cr.loadReview(article(1), score(3), score(5), score(6.5), score(10)) // average: 6,125
  cr.loadReview(article(2), score(4), score(4), score(6.5), score(1.5)) // 4
  cr.loadReview(article(3), score(5), score(5), score(1), score(3)) // 3.5
  cr.loadReview(article(4), score(8), score(5), score(0), score(8)) // 5,25
  cr.loadReview(article(5), score(9), score(8.2), score(2), score(9)) // 7.05
  cr.loadReview(article(6), score(8), score(2.2), score(2), score(9)) // 5,3

  // test load review
  println(5 == cr.reviewsNumber)

  // test average score
  println(6.125 == cr.averageFinalScore(article(1)).toDouble)

  // test accepted articles
  println(cr.acceptedArticles())

  // test accepted articles ordered by average score
  println(cr.sortedAcceptedArticles)

  // test average weighted final scores
  println(cr.averageWeightedFinalScoreMap)


