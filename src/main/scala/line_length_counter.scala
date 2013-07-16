import scala.io.Source

object LineCounter extends App {
  if (args.length == 1) {
    def widthOfLength(line: String) = line.length.toString.length
    val lines = Source.fromFile(args(0)).getLines().toList
    val longestLine = lines.reduceLeft( (a, b) => if (a.length > b.length) a else b )
    val maxWidth = widthOfLength(longestLine)
    for (line <- lines) {
      val numSpaces = maxWidth - widthOfLength(line);
      val padding = " " * numSpaces;
      println(padding + line.length +" | "+ line);
    }
    } else {
      Console.err.println("Please provide a filename as an argument")
  }
}
