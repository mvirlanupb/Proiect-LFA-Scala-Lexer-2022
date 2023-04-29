class CONCAT[A](var NFA1: Nfa[A], var NFA2: Nfa[A]) extends Operatie[A] {
  //Pasul de concatenare primeste ca input 2 NFA-uri si realizeaza operatie conform regulii cunoscute.
  def executaOperatie(): Nfa[A] = {
    val K = NFA1.getK | NFA2.getK//se aduc impreuna starile
    val Sigma = NFA1.getSigma | NFA2.getSigma //se unesc alfabetele celor 2 automate
    val Q0 = NFA1.getQ0//starea initiala a noului automat este starea initiala a primului automat
    val F = NFA2.getF//starea finala a noului automat este starea finala a celui de-al doilea automat
    var DELTA = NFA1.getDelta | NFA2.getDelta//aducem impreuna tranzitiile celor 2 automate
    DELTA += ((NFA1.getF, "eps", NFA2.getQ0))//la care adaugam epsilont-tranzitia dintre starea finala a primului automat si starea initiala a celui de-al doilea
    new Nfa[A](K, Sigma, Q0, DELTA, F) //se genereaza noul automat
  }

}
