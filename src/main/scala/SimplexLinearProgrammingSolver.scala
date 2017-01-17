class SimplexLinearProgrammingSolver extends LinearProgrammingSolver {
  override def solve(dictionary: Dictionary): Dictionary = {
    if (dictionary.objectiveFunction.hasPositiveCoeff) {
      val variable = dictionary.objectiveFunction.getPositiveVariable.get
      val constraint = dictionary.getMostLimitingConstraint(variable)
      val limitConstraint = constraint.isolateVariable(variable)
      solve(dictionary
        .filterOutConstraint(constraint)
        .pivot(limitConstraint))
    } else {
      dictionary
    }
  }
}
