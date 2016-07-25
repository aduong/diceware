import java.security.SecureRandom
import java.text.{NumberFormat,DecimalFormat}
import scala.io.Source
import scala.util.{Try,Random,Either}

case class Arguments(
  numWords: Int,
  noNumbers: Boolean = false,
  noSymbols: Boolean = false,
  strong: Boolean = false)

val FileName = "diceware.wordlist"
val NumFormat = NumberFormat.getInstance

def noExclusion[A]: (A => Boolean) = _ => false

def readArguments(args: Array[String]): Either[String, Arguments] = {
  for {
    numWords <- args.find(_.forall(_.isDigit))
      .map(_.toInt)
      .toRight("Password length missing. It must be one of the arguments")
      .right
  } yield Arguments(
    numWords,
    noNumbers = args.indexOf("--no-numbers") >= 0,
    noSymbols = args.indexOf("--no-symbols") >= 0,
    strong = args.indexOf("--strong") >= 0
  )
}

def readDicewareListFromFile(source: Source): Seq[String] =
  source.getLines.map(_.split("""\s+""")(1)).toIndexedSeq

def randStream(end: Int, strong: Boolean = false): Stream[Int] = {
  val rand = new Random(
    if (strong) SecureRandom.getInstanceStrong
    else new SecureRandom
  )
  Stream.continually(rand.nextInt(end))
}

def isInt(str: String): Boolean = Try(str.toInt).map(_ => true).getOrElse(false)

def isSymbol(str: String): Boolean = !str.forall(_.isLetterOrDigit)

def log(str: => String): Unit = {
  System.err.println(str)
}

readArguments(args) match {
  case Left(errorStr) =>
    log(errorStr)
    System.exit(1)

  case Right(Arguments(numWords, noNumbers, noSymbols, strong)) =>
    val numberExcluder: (String => Boolean) =
      if (noNumbers) s => isInt(s)
      else noExclusion

    val symbolExcluder: (String => Boolean) =
      if (noSymbols) s => isSymbol(s)
      else noExclusion

    val wordList = readDicewareListFromFile(Source.fromFile(FileName))
      .filterNot(numberExcluder)
      .filterNot(symbolExcluder)

    if (noNumbers) {
      log("Excluding numbers")
    }
    if (noSymbols) {
      log("Excluding symbols")
    }

    val keySpace = BigInt(wordList.size).pow(numWords)
    val keySpaceStr =
      if (keySpace > BigInt("1000000000")) new DecimalFormat("#.###E0").format(keySpace)
      else NumFormat.format(keySpace)
    val numWordsStr = NumFormat.format(numWords)
    val wordListSizeStr = NumFormat.format(wordList.size)
    val entropyPerWord = math.log(wordList.size) / math.log(2)

    log(s"Word list size: $wordListSizeStr")
    log(f"Entropy per word: $entropyPerWord%.3f bits")

    log(s"Number of words: $numWordsStr")
    log(f"Entropy: ${numWords * entropyPerWord}%.3f bits")
    log(s"Chance: 1 in $keySpaceStr")

    if (strong) {
      log("Using a strong PRNG... this may take some time...")
    }

    randStream(wordList.size, strong).map(wordList).take(numWords).foreach(println)
}
