package net.fearlessmind

object DaySeven:

  enum CardCombo extends Ordered[CardCombo]:
    case FiveOAK, FourOAK, FullHouse, ThreeOAK, TwoP, OneP, HighCard

    def compare(that: CardCombo): Int =
      CardCombo.order(this).compare(CardCombo.order(that))

  object CardCombo:
    def order = CardCombo.values.reverse.zipWithIndex.toMap.mapValues(_ + 1)

  trait Hand extends Ordered[Hand]:
    val ranks: Map[Char, Int]
    val cards: String
    val reward: Long
    val frequencyMap = cards.toSeq.foldRight(Map[Char, Int]()) {

      (current, previous) =>
        previous.get(current) match
          case None        => previous ++ Map(current -> 1)
          case Some(value) => previous ++ Map(current -> (value + 1))
    }

    def toCardCombo: CardCombo

  type HandApply = (String, Long) => Hand

  def parse(input: String)(using f: HandApply): Seq[Hand] =
    input
      .split("\n")
      .map(line =>
        line.split(" ") match
          case Array(cards, reward) => f(cards, reward.toLong)
      )

  def calculateWinningsFrom(hands: Seq[Hand]) =
    hands.sorted.zipWithIndex.map { (hand, index) =>
      hand.reward * (index + 1)
    }.sum

  def calculateWinnings(using (String, Long) => Hand) =
    calculateWinningsFrom.compose(parse)

  object P1:
    case class P1Hand(cards: String, reward: Long)
        extends Hand
        with Ordered[Hand]:

      val ranks = Array(
        'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'
      ).reverse.zip(0 until 13).toMap

      def toCardCombo: CardCombo =
        val fourOAK = frequencyMap.filter((_, value) => value == 4)
        val threeOAK = frequencyMap.filter((_, value) => value == 3)
        val pairs = frequencyMap.filter((_, value) => value == 2)

        if (frequencyMap.values.filter(_ == 5).size == 1) then CardCombo.FiveOAK
        else if (fourOAK.size == 1) then CardCombo.FourOAK
        else if (threeOAK.size == 1 && pairs.size == 1) then CardCombo.FullHouse
        else if (threeOAK.size == 1) then CardCombo.ThreeOAK
        else if (pairs.size == 2) then CardCombo.TwoP
        else if (pairs.size == 1) then CardCombo.OneP
        else CardCombo.HighCard

      override def compare(that: Hand): Int =
        if (this.toCardCombo != that.toCardCombo) then
          this.toCardCombo.compare(that.toCardCombo)
        else
          cards
            .zip(that.cards)
            .find { (a, b) =>
              ranks(a).compare(ranks(b)) != 0
            }
            .get match
            case (a, b) => ranks(a).compare(ranks(b))

    end P1Hand

    given HandApply = P1Hand.apply

    def calculateWinnings = calculateWinningsFrom.compose(parse)
  end P1

  object P2:
    case class P2Hand(cards: String, reward: Long)
        extends Hand
        with Ordered[Hand]:

      val ranks = Array(
        'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'
      ).reverse.zip(0 until 13).toMap

      val numOfJokers = frequencyMap.getOrElse('J', 0)

      def toCardCombo: CardCombo =
        val fiveOAK =
          frequencyMap
            .filter((_, value) => value == 5)

        val fourOAK =
          frequencyMap
            .filter((_, value) => value == 4)

        val threeOAK = frequencyMap
          .filter((_, value) => value == 3)

        val pairs = frequencyMap
          .filter((_, value) => value == 2)

        val isFiveOAK = fiveOAK.size == 1 ||
          (fourOAK.size == 1 && numOfJokers == 1) ||
          (threeOAK.size == 1 && numOfJokers == 2) ||
          (pairs.size == 1 && numOfJokers == 3) ||
          (numOfJokers == 4)

        val isFourOAK = fourOAK.size == 1 ||
          (threeOAK.size == 1 && numOfJokers == 1) ||
          (pairs
            .filter((key, _) => key != 'J')
            .size == 1 && numOfJokers == 2) ||
          (numOfJokers == 3)

        val isFullHouse = (threeOAK.size == 1 && pairs.size == 1) ||
          (pairs.size == 2 && numOfJokers == 1)

        val isThreeOAK = threeOAK.size == 1 ||
          (pairs.size == 1 && numOfJokers == 1) ||
          numOfJokers == 2

        val isTwoPairs = pairs.size == 2 ||
          (pairs.size == 1 && numOfJokers == 1) ||
          numOfJokers == 2

        val isOnePair = pairs.size == 1 ||
          (pairs.size == 0 && numOfJokers == 1)

        if (isFiveOAK) then CardCombo.FiveOAK
        else if (isFourOAK) then CardCombo.FourOAK
        else if (isFullHouse) then CardCombo.FullHouse
        else if (isThreeOAK) then CardCombo.ThreeOAK
        else if (isTwoPairs) then CardCombo.TwoP
        else if (isOnePair) then CardCombo.OneP
        else CardCombo.HighCard

      override def compare(that: Hand): Int =
        if (this.toCardCombo != that.toCardCombo) then
          this.toCardCombo.compare(that.toCardCombo)
        else
          cards
            .zip(that.cards)
            .find { (a, b) =>
              ranks(a).compare(ranks(b)) != 0
            }
            .get match
            case (a, b) => ranks(a).compare(ranks(b))

    end P2Hand

    given HandApply = P2Hand.apply

    def calculateWinnings = calculateWinningsFrom.compose(parse)
  end P2
