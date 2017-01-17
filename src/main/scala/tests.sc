case class Foo(s:String) {
  def getLongS : String = s + s
}
object Foo {
}

val f = new Foo("asd")

print("f.getLongS = " + f.getLongS)

print("asd")