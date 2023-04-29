class STAR[A](var NFA: Nfa[A], var stareInitialaNoua: A, var stareFinalaNoua: A) extends Operatie[A] {
  /*
  * La operatia Kleene-Star, pe langa automatul initial, e nevoie si de 2 stari noi, una care sa fie cea finala a noului automat,
  * si alta care sa fie stare initiala
  * */
  def executaOperatie(): Nfa[A] = {
    var K = NFA.getK
    K += stareInitialaNoua
    K += stareFinalaNoua//adaugam cele 2 stari noi
    var Sigma = NFA.getSigma//pastram alfabetul
    var DELTA = NFA.getDelta
    /*
    * Adaugam la relatia de tranzitie umrmatoarele epsilon-tranzitii, conform regulilor:
    *  o epsilon tranzitie de la starea finala a automatului la starea initiala a acestuia
    *  o epsilon tranzitie de la noua stare initiala a noului automat la starea initiala a automatului initial
    *  o epsilon tranzitie de la noua stare initiala a noului automat la noua stare finala a automatului
    *  o epsilon tranzitie de la starea finala a automatului curent la noua stare finala.
    * */
    DELTA += ((NFA.getF, "eps", NFA.getQ0))
    DELTA += ((stareInitialaNoua, "eps", NFA.getQ0))
    DELTA += ((NFA.getF, "eps", stareFinalaNoua))
    DELTA += ((stareInitialaNoua, "eps", stareFinalaNoua))
    new Nfa[A](K, Sigma, stareInitialaNoua, DELTA, stareFinalaNoua)
  }
}

