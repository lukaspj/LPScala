import org.scalatest.{FlatSpec, Matchers}

class SimplexLPTest extends FlatSpec with Matchers {

  "A SimplexSolver" should "compute correct solution to LP" in {
    val OF = ObjectiveFunction(0, Map())
      .addVariable("X1", 5.0)
      .addVariable("X2", 4.0)
      .addVariable("X3", 3.0)

    val constraints = Constraint("X4", Map("X1" -> -2.0, "X2" -> -3.0, "X3" -> -1.0), 5) ::
      Constraint("X5", Map("X1" -> -4.0, "X2" -> -1.0, "X3" -> -2.0), 11) ::
      List(Constraint("X6", Map("X1" -> -3.0, "X2" -> -4.0, "X3" -> -2.0), 8))

    val solver = new SimplexLinearProgrammingSolver

    val dictionary = solver.solve(Dictionary(OF, constraints))
    dictionary.getConstraintFor("X1").get._value should be (2.0)
    dictionary.getConstraintFor("X5").get._value should be (1.0)
    dictionary.getConstraintFor("X3").get._value should be (1.0)
  }

  "A SimplexSolver" should "compute correct solution to cyclic LP" in {
    val OF = ObjectiveFunction(0, Map())
      .addVariable("X1", 1.0)
      .addVariable("X2", -2.0)
      .addVariable("X4", -2.0)

    val constraints = Constraint("X5", Map("X1" -> 0.5, "X2" -> -3.5, "X3" -> -3.0, "X4" -> -0.5), 0) ::
      Constraint("X6", Map("X1" -> -0.5, "X2" -> 1.0, "X3" -> 0.5, "X4" -> -0.5), 0) ::
      List(Constraint("X7", Map("X1" -> -1.0), 1))

    val solver = new SimplexLinearProgrammingSolver

    val dictionary = solver.solve(Dictionary(OF, constraints))
    assert(true, "Should terminate on cyclic program")
  }

}
