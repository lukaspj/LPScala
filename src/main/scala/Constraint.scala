case class Constraint(_slackVariable: String,
                      _variableCoeffMap: Map[String, Double],
                      _value: Double) {

  def getCoeff(variable: String) = _variableCoeffMap.get(variable)

  def addVariable(key: String, value: Double) = Constraint(_slackVariable, _variableCoeffMap ++ Map(key -> value), _value)

  def getEntries = _variableCoeffMap

  def substituteConstraint(substitute: Constraint) = {
    val substVariable: String = substitute._slackVariable
    val coeff: Option[Double] = _variableCoeffMap.get(substVariable)
    val substitutedConstraint: Constraint = Constraint(_slackVariable, Map(), _value + (coeff.get * substitute._value))

    substitute._variableCoeffMap.foldLeft(substitutedConstraint)((constraint, entry) => {
      val entryCoeff = _variableCoeffMap.get(entry._1)
      val newCoeff = (coeff.get * substitute.getCoeff(entry._1).get) + entryCoeff.getOrElse(0.0)
      constraint.addVariable(entry._1, newCoeff)
    })
  }

  override def toString: String =
    _variableCoeffMap.foldLeft(_slackVariable + " = ")((acc, entry) => acc + " " + entry._1 + entry._2)

  def isolateVariable(variable: String) = {
    val coeff = -_variableCoeffMap(variable)
    Constraint(
      variable,
      _variableCoeffMap
        .filterNot(p => p._1.equals(variable))
        .map(p => p._1 -> p._2 / coeff)
        ++ Map(_slackVariable -> -1.0 / coeff),
      _value / coeff
    )
  }
}
