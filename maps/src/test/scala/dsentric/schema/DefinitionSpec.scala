package dsentric.schema

import namespaced.{TestFixture, TestFixture3, TestFixture4}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DefinitionTests extends AnyFunSuite with Matchers {
  test("nestedContractObjectDefinition") {
    val typeContract = TypeContract.of[TestFixture.type]
    val definition   = Definition.nestedContractObjectDefinition(TestFixture)(using typeContract)
    definition.properties should not be empty
  }

  test("nestedContractObjectDefinition for more complex case") {
    val typeContract = TypeContract.of[TestFixture3.type]
    val definition   = Definition.nestedContractObjectDefinition(TestFixture3)(using typeContract)
    val definition2  = Definition.nestedContractObjectDefinition(TestFixture4)
    definition.properties should not be empty
    definition2.properties should not be empty
  }
}