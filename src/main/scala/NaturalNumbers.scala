object NaturalNumbers {
  private val IdGenerator = Stream.from(1).iterator

  def getNextId: Int = IdGenerator.next()
}
