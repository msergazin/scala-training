val filesHere = (new java.io.File(".")).listFiles
val list = filesHere foreach (file => println(file))
print(list.getClass)
print(list)