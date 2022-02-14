import scala.collection.mutable.ArrayBuffer

object Question1 extends App{

  def addelements(min: Double, max: Double): ArrayBuffer[Double] ={
    val newArray = ArrayBuffer[Double]()
    var count = min
    count = (math rint count * 1000)/ 1000
    newArray+= count
    while(count<=max) {
      count += 0.049
      count = (math rint count * 1000)/ 1000
      newArray += count
      count += 0.001
      count = (math rint count * 1000)/ 1000
      newArray += count
    }
    return newArray
  }

  val min = 0.000
  val max = 100.000
  val totalRange = addelements(min, max)

//  totalRange.foreach((x: Double) => println(x))



  val sampleInput = List(12.05, 12.03, 10.33, 11.45, 13.50, 0.000, 100.0, 100.5)
  for(i <- sampleInput) {
    if (i>=min && i<=max) {
      val upperBoundIndex = totalRange.indexWhere(_ > i)
      val lowerBoundIndex = upperBoundIndex - 1
      if (upperBoundIndex == 0) {
        // Upper bound is first so there is no lower bound
        val lowerBoundIndex = -1
      } else if (upperBoundIndex == -1) {
        // the lower bound is probably totalRange.last
        val lowerBoundIndex = totalRange.length - 1
      } else {
        val lowerBoundIndex = upperBoundIndex - 1
      }

      if (lowerBoundIndex != -1)
        print(totalRange(lowerBoundIndex))
      print("-")
      if (upperBoundIndex != -1)
        println(totalRange(upperBoundIndex))

    }
    else {
      println("Element out of range")
    }
  }
}
