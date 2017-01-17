case class ObjectiveFunction(_value: Double,
                        _variableCoeffMap: Map[String, Double]) {

  def getPositiveVariable = _variableCoeffMap.collectFirst {
    case i if i._2 > 0.0 => i._1
  }

  def hasPositiveCoeff: Boolean = _variableCoeffMap.foldLeft(false)((acc, entry) => acc || entry._2 > 0)

  def getPositiveCoeff = _variableCoeffMap.collectFirst {
    case (s, d) if d > 0 => s
  }

  def addVariable(key: String, value: Double) = ObjectiveFunction(_value, _variableCoeffMap ++ Map(key -> value))

  def substituteConstraint(substitute: Constraint) = {
    val substVariable: String = substitute._slackVariable
    val coeff: Option[Double] = _variableCoeffMap.get(substVariable)
    val substitutedObjectiveFunction: ObjectiveFunction = ObjectiveFunction(_value + (coeff.get * substitute._value), Map())

    substitute._variableCoeffMap.foldLeft(substitutedObjectiveFunction)((constraint, entry) => {
      val entryCoeff = _variableCoeffMap.get(entry._1)
      val newCoeff = (coeff.get * substitute.getCoeff(entry._1).get) + entryCoeff.getOrElse(0.0)
      constraint.addVariable(entry._1, newCoeff)
    })
  }

  override def toString: String =
    _variableCoeffMap.foldLeft(_value + " = ")((acc, entry) => acc + " " + entry._1 + entry._2)
}
