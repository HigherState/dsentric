package dsentric.performance

/**
  * Created by Jamie Pullar on 18/05/2016.
  */
object JsonStringExample {

  val text =
    """[
      |  {
      |    "_id": "573cc215686cfe3b3e0a9227",
      |    "index": 0,
      |    "guid": "fa0e6e4e-20cf-4657-ad14-d3d536d36a86",
      |    "isActive": true,
      |    "balance": "$3,661.56",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 31,
      |    "eyeColor": "blue",
      |    "name": {
      |      "first": "Deana",
      |      "last": "Odonnell"
      |    },
      |    "company": "LUXURIA",
      |    "email": "deana.odonnell@luxuria.com",
      |    "phone": "+1 (965) 491-3109",
      |    "address": "583 Batchelder Street, Remington, Marshall Islands, 1941",
      |    "about": "Dolore proident ex voluptate esse adipisicing ea ipsum nulla officia duis magna eu ut nulla. Irure non non veniam dolore excepteur consectetur aliquip nulla amet labore in consequat exercitation. Non cupidatat mollit nostrud exercitation culpa. Laborum ea qui nulla nisi voluptate culpa.",
      |    "registered": "Monday, December 22, 2014 1:57 AM",
      |    "latitude": "-77.376904",
      |    "longitude": "-68.559926",
      |    "tags": [
      |      "irure",
      |      "aute",
      |      "occaecat",
      |      "amet",
      |      "commodo"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Diann Dodson"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Charity Woods"
      |      },
      |      {
      |        "id": 2,
      |        "name": "Aida Sellers"
      |      }
      |    ],
      |    "greeting": "Hello, Deana! You have 9 unread messages.",
      |    "favoriteFruit": "banana"
      |  },
      |  {
      |    "_id": "573cc215183098618fe38f5d",
      |    "index": 1,
      |    "guid": "20b37b4e-b011-4421-b5af-6dc65d382378",
      |    "isActive": false,
      |    "balance": "$2,282.17",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 32,
      |    "eyeColor": "green",
      |    "name": {
      |      "first": "Letitia",
      |      "last": "Savage"
      |    },
      |    "company": "EMTRAC",
      |    "email": "letitia.savage@emtrac.biz",
      |    "phone": "+1 (838) 511-2790",
      |    "address": "455 Locust Street, Shaft, Virginia, 8133",
      |    "about": "Ea voluptate elit commodo ipsum qui id aute cillum amet nisi enim nostrud pariatur. Anim officia do fugiat est ea. Enim id ipsum do ex fugiat adipisicing cupidatat ullamco ad labore elit eu laboris.",
      |    "registered": "Saturday, April 16, 2016 9:12 AM",
      |    "latitude": "68.539662",
      |    "longitude": "13.97939",
      |    "tags": [
      |      "eu",
      |      "do",
      |      "sit",
      |      "consectetur",
      |      "ipsum"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Hancock Mcintosh"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Krista Roy"
      |      },
      |      {
      |        "id": 2,
      |        "name": "Mitzi Wilkinson"
      |      }
      |    ],
      |    "greeting": "Hello, Letitia! You have 5 unread messages.",
      |    "favoriteFruit": "strawberry"
      |  },
      |  {
      |    "_id": "573cc215c5409c67aa755efd",
      |    "index": 2,
      |    "guid": "00548e87-2218-44dd-b0f9-f3b0a662db3d",
      |    "isActive": true,
      |    "balance": "$2,404.17",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 36,
      |    "eyeColor": "blue",
      |    "name": {
      |      "first": "Shirley",
      |      "last": "Mckay"
      |    },
      |    "company": "ISONUS",
      |    "email": "shirley.mckay@isonus.info",
      |    "phone": "+1 (836) 426-2279",
      |    "address": "494 Moore Place, Lowgap, American Samoa, 7950",
      |    "about": "Dolore deserunt labore eiusmod ipsum commodo tempor. Voluptate exercitation et esse dolor nostrud eu voluptate consequat sit laborum. Exercitation duis commodo eiusmod et est eu magna ullamco laboris non adipisicing magna eiusmod nisi. Id cupidatat fugiat nisi nulla magna eu proident nostrud nostrud aute. Laborum commodo in anim tempor laboris officia commodo esse proident. Magna cupidatat nulla cupidatat nisi do nisi occaecat culpa occaecat consectetur tempor nostrud duis in.",
      |    "registered": "Wednesday, April 2, 2014 8:43 AM",
      |    "latitude": "2.645332",
      |    "longitude": "-166.789928",
      |    "tags": [
      |      "velit",
      |      "duis",
      |      "commodo",
      |      "laboris",
      |      "duis"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Gabrielle Mitchell"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Marisol Murray"
      |      },
      |      {
      |        "id": 2,
      |        "name": "Ora Peterson"
      |      }
      |    ],
      |    "greeting": "Hello, Shirley! You have 10 unread messages.",
      |    "favoriteFruit": "strawberry"
      |  },
      |  {
      |    "_id": "573cc215356748bf721dc0ac",
      |    "index": 3,
      |    "guid": "c73f6268-7af5-49f2-ae17-6b842ec8ad97",
      |    "isActive": false,
      |    "balance": "$3,239.61",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 25,
      |    "eyeColor": "blue",
      |    "name": {
      |      "first": "Blair",
      |      "last": "Pratt"
      |    },
      |    "company": "ORBAXTER",
      |    "email": "blair.pratt@orbaxter.co.uk",
      |    "phone": "+1 (877) 508-2017",
      |    "address": "112 Apollo Street, Norfolk, Georgia, 499",
      |    "about": "Laboris eu magna deserunt non aliqua. Sunt fugiat cillum amet duis ex occaecat. Aliquip ut do ut est occaecat est aliqua. Voluptate veniam nisi ipsum est aute ea labore eu. Reprehenderit non laborum laboris commodo qui. Lorem laboris dolor ex consectetur pariatur laborum officia.",
      |    "registered": "Tuesday, July 7, 2015 1:44 PM",
      |    "latitude": "66.711039",
      |    "longitude": "90.37958",
      |    "tags": [
      |      "tempor",
      |      "enim",
      |      "irure",
      |      "quis",
      |      "nisi"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Anastasia Conley"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Beard Hess"
      |      },
      |      {
      |        "id": 2,
      |        "name": "Mckinney Dotson"
      |      }
      |    ],
      |    "greeting": "Hello, Blair! You have 5 unread messages.",
      |    "favoriteFruit": "apple"
      |  },
      |  {
      |    "_id": "573cc21505cb2c1530363ce5",
      |    "index": 4,
      |    "guid": "c19a5b27-9701-4e76-aa57-3556751cb54d",
      |    "isActive": false,
      |    "balance": "$3,888.24",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 24,
      |    "eyeColor": "blue",
      |    "name": {
      |      "first": "Deleon",
      |      "last": "Cunningham"
      |    },
      |    "company": "TSUNAMIA",
      |    "email": "deleon.cunningham@tsunamia.me",
      |    "phone": "+1 (906) 533-2889",
      |    "address": "893 Sunnyside Avenue, Katonah, Montana, 8174",
      |    "about": "Proident duis elit aute officia mollit aute ipsum officia nisi. Excepteur ex sit nulla magna labore reprehenderit ad mollit quis tempor proident excepteur minim excepteur. Irure ex sunt proident nisi sunt non. Cillum laboris ex aliqua ea est adipisicing magna magna reprehenderit nulla ea ea dolor et. Cillum sunt pariatur enim quis reprehenderit aliqua deserunt ex dolor non excepteur deserunt et. Adipisicing ipsum in voluptate officia ullamco sit voluptate sit. Enim culpa veniam consectetur enim nulla.",
      |    "registered": "Friday, August 8, 2014 6:17 PM",
      |    "latitude": "-42.143229",
      |    "longitude": "-72.955954",
      |    "tags": [
      |      "enim",
      |      "laboris",
      |      "enim",
      |      "ut",
      |      "consectetur"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Day Howard"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Jana Jones"
      |      },
      |      {
      |        "id": 2,
      |        "name": "West Mckee"
      |      }
      |    ],
      |    "greeting": "Hello, Deleon! You have 10 unread messages.",
      |    "favoriteFruit": "banana"
      |  },
      |  {
      |    "_id": "573cc215a21ac1087a3c8fd4",
      |    "index": 5,
      |    "guid": "9d51cde4-fe12-4868-b359-bf282567306b",
      |    "isActive": true,
      |    "balance": "$2,144.05",
      |    "picture": "http://placehold.it/32x32",
      |    "age": 31,
      |    "eyeColor": "blue",
      |    "name": {
      |      "first": "Lakeisha",
      |      "last": "Figueroa"
      |    },
      |    "company": "CYCLONICA",
      |    "email": "lakeisha.figueroa@cyclonica.ca",
      |    "phone": "+1 (905) 400-3870",
      |    "address": "583 Maple Street, Waukeenah, District Of Columbia, 5565",
      |    "about": "Sunt non eiusmod do laborum est voluptate exercitation mollit nisi. Laborum labore cupidatat dolor in incididunt ipsum amet anim labore amet elit consequat excepteur culpa. Consectetur non qui ad consequat occaecat. Eu deserunt in mollit deserunt officia magna tempor aute dolor laboris ad Lorem velit. Sint aute cillum incididunt quis mollit cupidatat. Magna laborum laborum consectetur pariatur sunt quis magna laborum id excepteur tempor voluptate tempor.",
      |    "registered": "Monday, March 3, 2014 2:19 PM",
      |    "latitude": "30.213224",
      |    "longitude": "29.574454",
      |    "tags": [
      |      "Lorem",
      |      "esse",
      |      "excepteur",
      |      "laboris",
      |      "esse"
      |    ],
      |    "range": [
      |      0,
      |      1,
      |      2,
      |      3,
      |      4,
      |      5,
      |      6,
      |      7,
      |      8,
      |      9
      |    ],
      |    "friends": [
      |      {
      |        "id": 0,
      |        "name": "Ellen Monroe"
      |      },
      |      {
      |        "id": 1,
      |        "name": "Nixon Davidson"
      |      },
      |      {
      |        "id": 2,
      |        "name": "Patrica Roach"
      |      }
      |    ],
      |    "greeting": "Hello, Lakeisha! You have 8 unread messages.",
      |    "favoriteFruit": "apple"
      |  }
      |]""".stripMargin
}
