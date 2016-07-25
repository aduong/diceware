import java.security.SecureRandom
import java.text.{NumberFormat,DecimalFormat}
import scala.io.Source
import scala.util.{Try,Random,Either}

case class Arguments(
  numWords: Int,
  noNumbers: Boolean,
  noSymbols: Boolean,
  strong: Boolean,
  separator: String
)

val FileName = "diceware.wordlist"
val NumFormat = NumberFormat.getInstance

val UsageStr = """Usage: [OPTIONS] LENGTH
                 |Options:
                 |  --help            displays this message
                 |  --no-symbols      omits symbols from the diceware list
                 |  --no-numbers      omits numbers from the diceware list
                 |  --separator SEP   string to use as separator between words
                 |  --strong          uses a stronger crypto-secure PRNG (may block)""".stripMargin

def noExclusion[A]: (A => Boolean) = _ => false

def isInt(str: String): Boolean = Try(str.toInt).map(_ => true).getOrElse(false)

def isSymbol(str: String): Boolean = !str.forall(_.isLetterOrDigit)

def readNumWords(args: Array[String]): Either[String, Int] =
  args.find(isInt).map(_.toInt)
    .toRight("Error: password length missing.\n" + UsageStr)

def readSeparator(args: Array[String]): Either[String, Option[String]] = {
  val i = args.indexOf("--separator")
  if (i < 0) Right(None)
  else {
    val separatorIdx = i + 1
    if (separatorIdx >= args.size) Left("\"--separator\" present but missing separator argument.\n" + UsageStr)
    else Right(Some(args(separatorIdx)))
  }
}

def readArguments(args: Array[String]): Either[String, Arguments] =
  if (args.indexOf("--help") >= 0) Left(UsageStr)
  else {
    for {
      numWords <- readNumWords(args).right
      separatorOpt <- readSeparator(args).right
    } yield Arguments(
      numWords,
      noNumbers = args.indexOf("--no-numbers") >= 0,
      noSymbols = args.indexOf("--no-symbols") >= 0,
      strong = args.indexOf("--strong") >= 0,
      separator = separatorOpt.getOrElse(" ")
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

def log(str: => String): Unit = {
  System.err.println(str)
}

readArguments(args) match {
  case Left(errorStr) =>
    log(errorStr)
    System.exit(1)

  case Right(Arguments(numWords, noNumbers, noSymbols, strong, separator)) =>
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

    log("")

    println(
      randStream(wordList.size, strong)
        .map(wordList)
        .take(numWords)
        .mkString(separator))
}
