class UNION[A](var NFA1: Nfa[A], var NFA2: Nfa[A], var stareInitialaNoua: A, var stareFinalaNoua: A) extends Operatie[A] {
  def executaOperatie(): Nfa[A] = {
    var K = NFA1.getK | NFA2.getK
    K += stareInitialaNoua
    K += stareFinalaNoua
    var Sigma = NFA1.getSigma | NFA2.getSigma
    var DELTA = NFA1.getDelta | NFA2.getDelta
    DELTA += ((stareInitialaNoua, "eps", NFA1.getQ0))
    DELTA += ((stareInitialaNoua, "eps", NFA2.getQ0))
    DELTA += ((NFA1.getF, "eps", stareFinalaNoua))
    DELTA += ((NFA2.getF, "eps", stareFinalaNoua))
    new Nfa[A](K, Sigma, stareInitialaNoua, DELTA, stareFinalaNoua)
  }
}
