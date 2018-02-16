# dsentric
Data contract patterns, validation, lenses and query

dsentric is an explorative library designed to provided accessor to dynamic data which is independent of the underlying data type.

There is a monocle implementation which can be extended to customised data types.

There is also a very light-weight, performance focussed Map[String, Any], Vector[Any] based data structure implementation as well.  This also supports further functionality such as querying and projections.

dsentric works by describing a singleton contract which represents data we might wish to extract from the data structure.  By doing so, we get easy validation, lenses and even a type safe mongo db query generator.

```scala
  /*define a contract,
    \  \?  \! expected, optional, default properties
    \: \:?  \:!  expected, optional, default array properties
    \\ \\? expected, option object properties
  */
  object OrderLine extends Contract {
    val product = \[String]
    val quanity = \[Int]
  }
  
  object Order extends Contract {
    val firstName = \[String](nonEmptyOrWhiteSpace)
    val lastName = \[String](nonEmptyOrWhiteSpace)
    val orderId = \?[Int](reserved && immutable)

    val email = new \\ {
      val friendlyName = \?[String]
      val address = \[String]
    }
    val status = \?[String](in("pending", "processing", "sent") && reserved)
    val notes = \?[String](internal)

    val orderLines = \:(OrderLines)

    import ApplicationLens._
    //Combine properties to make a composite pattern matcher
    lazy val fullName = firstName @: lastName
  }

  //Create a new data object
  val newOrder = Order.$create{o =>
    o.firstName.$set("John") ~
    o.lastName.$set("Smith") ~
    o.email.address.$set("johnSmith@test.com") ~
    o.orderLines.$set(Vector(OrderLine.$create(l => l.product.$set("LightBulb") ~ l.quantity.$set(3))))
  }

  //validate a new data object
  val validated = Order.$validate(newOrder)

  //pattern match property values
  newOrder match {
    case Order.email.address(email) && Order.email.friendlyName(Some(name)) =>
      println(s"$email <$name>")
    case Order.email.address(email) && Order.fullName(firstName, lastName) =>
      println(s"$email <$firstName $lastName>")
  }

  //make changes to the data object.
  val pending =
    Order{o =>
      o.orderId.$set(123) ~
      o.status.$set("pending") ~
      o.notes.$modify(maybe => maybe.foldLeft("Order now pending.")(_ + _))
    }(newOrder)

  //strip out any properties marked internal
  val sendToClient = Order.$sanitize(pending)

  //generate query data
  val relatedOrdersQuery = Order.orderId.$gt(56) && Order.status.$in("processing", "sent")
  
  relatedOrdersQuery.isMatch(pending)
  
  //This produces a mongo query data structure
  
  //experimental convert to postgres jsonb clause
  val postgresQuery = QueryJsonb("data", relatedOrdersQuery)

  val statusDelta = Order.$create(_.status.$set("processing"))
  //validate against current state
  Order.$validate(statusDelta, pending)
  //apply delta to current state
  val processing = pending.applyDelta(statusDelta)

  //Define subcontract for reusable or recursive structures
  trait UserTimestamp extends SubContract {
    val account = \[String]
    val timestamp = \[Long]("timestamp")
  }
  object Element extends Contract {
    val created = new \\(immutable) with UserTimestamp
    val modified = new \\ with UserTimestamp
  }
  
  trait Recursive extends SubContract {
    lazy val rec = new \\? with Recursive
  }
  
  //Produce a mongo style projection which extracts only those fields
  val nameProjection = Order.firstName.$ & Order.lastName.$
  nameProjection.select(pending)
  
  //Create your own domain specific DObject
  case class Custom(value:Map[String, Any]) extends AnyVal with DObject with DObjectLike[Custom] {
    protected def wrap(value: Map[String, Any]): Custom =
      Custom(value)
  }
  //With its own contract type
  object CustomContract extends ContractFor[Custom]

```

*mongo query is not a full feature set.

