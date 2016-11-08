package fun

class ParserSpec extends FreeSpec {

  val data = Map(
    "transactionId" -> "12331",
    "amount" -> 0,
    "index_date" -> "2016-01-01",
    "meta" -> Map(
      "merchant_name" -> "bill",
      "location" -> Map(
        "lat" -> -33.22,
        "lng" -> 156.3
      )
    )
  )

  val metaData = Map(
    "merchant_name" -> "bill",
    "location" -> Map(
      "lat" -> -33.22,
      "lng" -> 156.3
    )
  )

  val locationData = Map(
    "lat" -> -33.22,
    "lng" -> 156.3
  )

  case class Meta(location: Location, merchantName: String)
  case class Location(lat: Double, lng: Double)


  trait Parser[T]{
    def parse(a:Any): Either[String,T]

    def flatMap[A](fn:T => Either[String,A]): Parser[A] = {
      val parent = this

      new Parser[A] {
        def parse(a: Any): Either[String, A] = {
          parent.parse(a).right.flatMap(fn)
        }
      }
    }
  }

  object StringParser extends Parser[String] {
    def parse(a:Any): Either[String,String] = a match {
      case s: String => Right(s)
      case _ => Left("wrong type, expected String found "+a.getClass.getSimpleName)
    }
  }

  object DoubleParser extends Parser[Double] {
    def parse(a:Any): Either[String,Double] = a match {
      case s: Double => Right(s)
      case _ => Left("wrong type, expected Double found "+a.getClass.getSimpleName)
    }
  }

  object MapSAParser extends Parser[Map[String,Any]] {
    def parse(a:Any): Either[String,Map[String,Any]] = a match {
      case s: Map[String,Any] => Right(s)
      case _ => Left("wrong type, expecte Map found "+a.getClass.getSimpleName)
    }
  }

  object Implicits {
    implicit val doubleParser = DoubleParser
  }

  import Implicits._



  def findT[T](label:String, data: Map[String,Any], parser: Parser[T]): Either[String,T] = {
    data.get(label).toRight("Not found "+label).right.flatMap { a =>
      parser.parse(a)
    }
  }

  "implicit find" in {
    def findI[T](label:String, data: Map[String,Any])(implicit parser: Parser[T]): Either[String,T] = {
      data.get(label).toRight("Not found "+label).right.flatMap { a =>
        parser.parse(a)
      }
    }

    val foundLocation = for {
      lat <- findI[Double]("lat", locationData).right
      lng <- findI[Double]("lng", locationData).right
    } yield Location(
      lat = lat,
      lng = lng
    )
  }

  "positive double parser flatMap" in {
    val data: Any = 3.3

    assertResult(Right(3.3))(DoubleParser.parse(data))

    val positiveDoubles = DoubleParser.flatMap {
      case dd if dd >= 0 => Right(dd)
      case other => Left("was negative "+other)
    }

    val neg: Any = 1.3

    assertResult(Right(1.3))(positiveDoubles.parse(neg))
  }

  "with location parser 1" in {

    object LocationParser extends Parser[Location] {
      def parse(a:Any): Either[String,Location] = a match {
        case map: Map[String,Any] => for {
          lat <- findT[Double]("lat", map, DoubleParser).right
          lng <- findT[Double]("lng", map, DoubleParser).right
        } yield {
          Location(lat,lng)
        }

        case _ => Left("wrong type, expecte Location found "+a.getClass.getSimpleName)
      }
    }

    val locationParser2 = MapSAParser.flatMap { map =>
      for {
        lat <- findT[Double]("lat", map, DoubleParser).right
        lng <- findT[Double]("lng", map, DoubleParser).right
      } yield Location(lat,lng)
    }


    val foundMeta = for {
      merchantName <- findT[String]("merchant_name", metaData, StringParser).right
      location <- findT[Location]("location", metaData, LocationParser).right
    } yield Meta(
      merchantName = merchantName,
      location = location)

    val expected = Meta(
      merchantName = "bill",
      location = Location(
        lat = -33.22,
        lng = 156.3
      )
    )

    assertResult(Right(expected))(foundMeta)

  }


  "do it" in {


    val foundLocation = for {
      lat <- findT[Double]("lat", locationData, DoubleParser).right
      lng <- findT[Double]("lng", locationData, DoubleParser).right

    } yield Location(
      lat = lat,
      lng = lng
    )

    assertResult(Right(Location(
      lat = -33.22,
      lng = 156.3
    )))(foundLocation)


    val foundMeta = for {
      merchantName <- findT[String]("merchant_name", metaData, StringParser).right
      locationMap <- findT[Map[String,Any]]("location", metaData, MapSAParser).right
      lat <- findT[Double]("lat", locationMap, DoubleParser).right
      lng <- findT[Double]("lng", locationMap, DoubleParser).right
    } yield Meta(
      merchantName = merchantName,
      location = Location(
      lat = lat,
      lng = lng
    ))

    val expected = Meta(
      merchantName = "bill",
      location = Location(
        lat = -33.22,
        lng = 156.3
      )
    )

    assertResult(Right(expected))(foundMeta)







  }


}