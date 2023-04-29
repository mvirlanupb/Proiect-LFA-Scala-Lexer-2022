abstract class Operatie[A]{
    def executaOperatie():Nfa[A]
}
/*
* Am inclus aceasta clasa abstracta, Operatie[A], deoarece doream o metoda comuna pentru cele 3 operatii principale, anume:
* CONCAT, UNION si STAR.
* Fiecare operatie din cele 3 va avea un constructor particularizat.
* */
