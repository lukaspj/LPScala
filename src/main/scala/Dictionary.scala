case class Dictionary(objectiveFunction: ObjectiveFunction,
                      constraints: List[Constraint]) {

  def getMostLimitingConstraint(variable: String) : Constraint = {
    constraints.foldLeft(Double.PositiveInfinity -> (null: Constraint))((acc, constraint) => {
      val coeff = constraint.getCoeff(variable)
      val value = constraint._value
      val limit = -value / coeff.get
      if (coeff.get < 0 && acc._1 > limit) {
        limit -> constraint
      } else {
        acc
      }
    })
  }

  def getConstraintFor(variable: String) = {
    constraints.collectFirst {
      case c if c._slackVariable.equals(variable) => c
    }
  }

  def pivot(limitingConstraint : Constraint) = {
    Dictionary(
      objectiveFunction.substituteConstraint(limitingConstraint),
      limitingConstraint :: constraints.map(c => c.substituteConstraint(limitingConstraint))
    )
  }

  def filterOutConstraint(constraint: Constraint) = {
    Dictionary(
      objectiveFunction,
      constraints.filterNot(c => c == constraint)
    )
  }
}
