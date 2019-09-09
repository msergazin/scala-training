/*
val someCondition = true
val ifOrElse = if (someCondition == true) "IF" else "ELSE"
ifOrElse

var i = 0
while(i < 5) {
  println(i)
  i = i + 1
}
*/

val filesHere = (new java.io.File(".")).listFiles

/*
for (file <- filesHere)
  println(file)

for (file <- filesHere
     if file.getName.endsWith(".scala"))
  println(file)


for (
  file <- filesHere
  if file.isFile
  if file.getName.endsWith(".scala")

) println(file)
*/

println("sss")
val list = filesHere foreach (file => file)


/*
def scalaFiles =
  for {

    file <- filesHere

    if file.getName.endsWith(".scala")
  } yield file

println("exe files number: " + scalaFiles.length)


def matchTheParam(param: Any) =
  param match {

    case s: String => s match {
      case "salt" => println("pepper")
      case "chips" => println("salsa")
      case "eggs" => println("bacon")
      case _ => println("huh?")
    }
    case c: Char => c match {
      case 'c' => println('c')
      case _ => println("default char")
    }
    case _ => println("neither a string nor a char")
  }

matchTheParam("salt")
matchTheParam('c')
matchTheParam('d')
matchTheParam(4)


def matchTheParamWithValue(param: Any) =
  param match {

    case s: String => s match {
      case "salt" => ("pepper")
      case "chips" => ("salsa")
      case "eggs" => ("bacon")
      case _ => ("huh?")
    }
    case c: Char => c match {
      case 'c' => ('c')
      case _ => ("default char")
    }
    case _ => ("neither a string nor a char")
  }

matchTheParamWithValue("eggs")
*/
