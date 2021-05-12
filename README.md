[![Build Status](https://travis-ci.org/HigherState/dsentric.svg?branch=master)](https://travis-ci.org/HigherState/dsentric)
[![Coverage Status](https://coveralls.io/repos/github/HigherState/dsentric/badge.svg?branch=master)](https://coveralls.io/github/HigherState/dsentric?branch=master)
[![License](http://img.shields.io/:license-Apache%202-red.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

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

#Verify

#Delta Verify

#Additional Properties object
 - Value object with Contract
 - Returns a D <: DObject
 - Number of dedicated lens operation for addional properties, 
   like add etc
 - Get Additional Properties as a Map method?

#Map Object
 - Value object with Contract
 - Returns a Map of correct types
 - How do we resolve a map of a map of a contract?
  ie `Map[String, Map[String, D]]` 
   Contract generates an internal DCodec for D?
   No not enough, unless we have a contract Decodec type?

##Needs a specific Delta object


- How to Handle delta collections like Set and Map when they have null values
- How to validate delta collections
- How to add delta values into a contract?
(I think validate only one the delta application result not the delta value itself, IE pass through currentState and previousState, could be slow on nested objects so need to optimise)
- Delta context - IE full replace of an nested object rather than merge (Thinking Fixtures)
- Perhaps the difference here is A DObject or \\ will delta merge, however a Case Class Type will replace?
- Delta validation IE standalone valid - Fixture?

- $get should work on Deltas/Dobjects as well as just D ie so can use contract to get delta value. OR you could provide a property object to a dobject to extract the value obj.apply(pProperty[T, D]):Option[T]

Notion of emptiable objects, IE if they are empty we remove them, thinking of say a Range object ( ), or the Object object
Perhaps the notion of emptiness can be confgurable?

?Notion of Healing support, when merging two objects,
- could ignore bad merged value
- In a Map[Key, Object] it could drop bad objects...

How to handle a keyed object map
Ie Map(Key, Element(Key)(values)) - might be solved by above validation :)
Projections
Projections Should support WildCard key match, or regex key match
maybe "$/*/"
Projection building ie  DProjection("nested" -> {​​​​​​​DProjection}​​​​​​​)
function to extract paths.
Validation
Immutable on optional, Ie must leave blank if blank initially. Maybe immutable and immutableOnSet?
Query Tree
Needs more robust tests
Include boolean identities
Ignore if not create
How to handle an update on a removed nested object?
Immutable required properties help...
DObject sub classing

PathSetter supports sub class transform, what about super class, Dobject => Dobject applied to .a D for example.
Needs a consistent elegant approach

WARNING

when adding a Contract element, ie as an Additional Property or element in a vector, the process doesnt validate 
correctness as being of the correct Type is sufficient under current definition.
We know this is fine for types, but for Contract DOBjects, we do allow adding manually adding elements that dont 
support the contract, where as say a delta operation, this would be checked.
Corollary
For Modify it will validate the entity coming out of the Object but not the value going in

This may need to change in the future.

Maps are tricky, empty maps will reduce to nothing

Terminology

unapply - 
    extract the value, type match return None if invalid type.
    If Contract match, also return None if invalid type

traversal -
    We dont need to validate contracts in the traversal path, 
    Just need to make sure it can be traversed

    its possible to get a default value for an expected object that is not present
    As the Expected object will return an empty object in the traversal if its not found

    Traversal paths go through both the contract and the codecs, as codecs can reference contracts.

reduce -
    Removes extraneous values.
    This would remove empty objects
    Would clear out null values
    If a codec returns NotFound on raw data

get -
    extract the value, return any type or other structural failures

Delta
    - if an expected property is empty, delta is Not required to fix it.  
      Delta does not need to repair, just cannot make things worse
    - If delta value is sent, but is the same as current value, but current value is invalid, it will fail validation.
