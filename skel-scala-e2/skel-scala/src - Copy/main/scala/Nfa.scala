import scala.annotation.tailrec
import scala.collection.mutable

class Nfa[A](var K:scala.collection.mutable.Set[A],var Sigma:Set[String],var Q0:A,var DELTA:scala.collection.mutable.Set[(A,String,A)],var F:A) {
  /*
  * Un obiect de tipul Nfa[A] are exact elementele din definitia unui NFA: multimea starilor, alfabetul, starea initiala,
  * relatia de tranzitie si starea finala.
  * Motivul pentru care K este un set de tip Mutable se datoreaza pasilor din Algoritmul lui Thomson, pentru ca adaugam stari noi
  * pe masura ce algoritmul avanseaza.
  * */
/*
* Ce urmeaza aici este un set de getteri si setteri a caror functionalitate este similara cu cea din Java.
* */
  def getK:mutable.Set[A]={
    K
  }
  def getSigma:Set[String]={
    Sigma
  }
  def getQ0:A={
    Q0
  }
  def getDelta:mutable.Set[(A,String,A)]={
    DELTA
  }
  def getF:A={
    F
  }
  def setQ0(Q0nou:A): Unit ={
    Q0=Q0nou
  }
  def setF(Fnou:A): Unit ={
    F=Fnou
  }
  def setSigma(Sigmanou:Set[String]):Unit={
    Sigma=Sigmanou
  }
  def setDelta(Deltanou:mutable.Set[(A,String,A)]):Unit={
    DELTA=Deltanou
  }
  def setK(Knou:mutable.Set[A]):Unit={
    K=Knou
  }
  /*
  * Am avut nevoie de o metoda equals pentru a verifica daca doua stari ale unor automate sunt egale.
  * Am folosit asta pentru a proba daca un automat se identifica cu un "automat vid".
  * Am definit un automat vid intr-o metoda de tip apply, ca fiind un automat in care multimile din definitie sunt vide,
  * iar starea initiala si cea finala sunt egale cu o stare vida, stare ce poate fi definita un mod liber de catre
  * utilizator. Pentru ca in metoda fromPrenex returnam un Nfa cu starile numere naturale de la 0 in sus, am considerat ca
  * o stare nevida are ID-ul -1.
  * */
  def equals(obj: Nfa[A]): Boolean = {
    if(K.equals(obj.K))
      true
    else
      false
  }

  def map[B](f: A => B) : Nfa[B] = {
    val K_NFA_B = K.map(f)//transform starile
    val Q0_NFA_B = f(Q0)//transform starea initiala
    val DELTA_NFA_B = DELTA.map(elem=>(f(elem._1),elem._2,f(elem._3)))//transform relatia de tranzitie
    val F_NFA_B = f(F)//transform starea finala
    new Nfa[B](K_NFA_B,Sigma,Q0_NFA_B,DELTA_NFA_B,F_NFA_B)
  } // TODO implement map
  /*
  Pentru functia next, am pornit de la relatia de tranzitie DELTA, pe care o parcurgem prin foldLeft pentru
  a identifica perechile de forma(from,simbol,to) unde from=state si simbol este un caracter transformat in String.
  Reprezinta toate starile posibile in care putem ajunge din starea data ca parametru.
  * */
  def next(state:A, c: Char): Set[A] = {
    val rezultat = DELTA.foldLeft(Set[A]())((acc, element) =>
      if ((element._1 == state) && (element._2.head == c)) acc + element._3 else acc)
    rezultat
  } // TODO implement next

  def accepts(str: String): Boolean = {
    /*
    Pornim de la o stare actuala si stringul curent. Daca string-ul curent este cel vid si starea actuala este cea finala,
    atunci cuvantul este acceptat, altfel. In cazul in care starea actuala nu este finala, asta nu inseamna automat ca
    a fost respins cuvantul, ci trebuie sa vedem cat de departe putem ajunge folosind epsilon-tranzitiile. Deci, din starea actuala,
    gasim epsilon tranzitii. Daca exista, trecem la fiecare stare noua gasita, altfel (situatia in care nu exista epsilon tranzitii),
    cuvantul este respins de automat.

    In cazul in care sirul dat ca parametru nu este vid, gasim toate tranzitiile posibile din starea actuala pe primul caracter.
    Daca exista, atunci exploram toate starile in care am ajunge prin aceste tranzitii.
    Daca nu avem tranzitii pe primul caracter, dar avem epsilon tranzitii, atunci pastram sirul si mergem la noile stari.
    */
    def accepts_aux(str:String, stare_actuala:A):Boolean=str match {
      case ""=>
        if(stare_actuala==F)
          return true
        val eps_tranz=DELTA.filter(pereche=>(pereche._1==stare_actuala)&&(pereche._2=="eps"))//gasim toate epsilon tranzitiile din starea actuala
        eps_tranz.foldLeft(false)((acc,element)=>acc||accepts_aux(str,element._3))//parcurgem fiecare stare noua
      case _=>
        val cauta_tranzitii = DELTA.filter(pereche => (pereche._1 == stare_actuala) && (pereche._2.head == str.head))//cautam tranzitii pe primul caracter
        var raspuns=false//rezultatul acceptarii cuvantului raportat la starea curenta
        if(cauta_tranzitii.nonEmpty) {//am gasit tranzitii
          raspuns=raspuns||cauta_tranzitii.foldLeft(false)((acc,element)=>acc||accepts_aux(str.tail,element._3))
          //prin foldLeft, iau fiecare stare destinatie in parte, iar din sirul dat, scapam de primul caracter
          //folosesc sau logic pentru ca din starea actuala putem fie sa acceptam, fie sa nu acceptam cuvantul, fiind vorba despre NFA
        }
        //Luam in considerare si epsilon tranzitiile. Mai intai, le gasim.
        val tranz_epsilon = DELTA.filter(pereche=>(pereche._1==stare_actuala)&&(pereche._2=="eps"))
        if(tranz_epsilon.nonEmpty)//am gasit epsilon-tranzitii
          raspuns=raspuns||tranz_epsilon.foldLeft(false)((acc,element)=>acc||accepts_aux(str,element._3))//trecem la fiecare stare noua in parte si pastram sirul
        raspuns
    }
    accepts_aux(str,Q0)//se face parcurgerea de la starea initiala
  } // TODO implement accepts
  /*
  * Deoarece NFA-ul primeste ca parametru inclusiv multimea de stari intr-un set mutabil,
  * a trebuit sa scriu o functie care doar muta continutul dintr-un set mutabil intr-unul
  * imutabil
  * */
  def getStates : Set[A] = {
     @tailrec
     def getStates_aux(stariMutable:scala.collection.mutable.Set[A], rezultat:Set[A]):Set[A]= {
       if(stariMutable.isEmpty)
         rezultat
       else
         getStates_aux(stariMutable.tail,rezultat+stariMutable.head)
     }
    getStates_aux(K,Set[A]())
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
    if(state==F)
      true
    else
      false
  }  // TODO implement isFinal

}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def apply[A](stareInitiala:A,
               simbol:String,stareFinala:A): Nfa[A] = {//automat simplu care respecta cazurile de baza ale algoritmului Thomson
    val K:mutable.Set[A]= scala.collection.mutable.Set.empty[A]
    val DELTA =mutable.Set.empty[(A,String,A)]
    val Sigma = Set[String]()
    if((!simbol.equals("eps"))&&(!simbol.equals("void"))){
      K+=stareInitiala
      K+=stareFinala
      DELTA+=((stareInitiala,simbol,stareFinala))
      new Nfa[A](K,Sigma+simbol,stareInitiala,DELTA,stareFinala)
    }
    else{
      if(simbol.equals("eps")){
        K+=stareInitiala
        new Nfa[A](K,Sigma,stareInitiala,DELTA,stareInitiala)
      }
      else{
        K+=stareInitiala
        K+=stareFinala
        new Nfa[A](K,Sigma,stareInitiala,DELTA,stareFinala)
      }
    }
  }
  def apply[A](stareVida:A):Nfa[A]={//automatul vid

    new Nfa[A](mutable.Set.empty[A],Set[String](),stareVida,mutable.Set.empty[(A,String,A)],stareVida)
  }
  def apply[A](operatie:Operatie[A]):Nfa[A]={
    operatie.executaOperatie()
  }
  /*
  * Aceasta metoda creeaza un obiect de tip operatie, obiecte ce vor fi folosite in stiva.
  * Ca un motiv pentru care a trebuit sa recurg la asta, voi detalia aici cum vor decurge
  * operatiile de adaugare si reducere in stiva.
  *
  * In momentul cand, vedem o operatie in timp ce parsam PRENEX-ul, ii facem
  * o initializare cat mai simpla, nestiind in acel moment ce automat(e), sau stari vor fi puse.
  * Spre exemplu: Daca la se citeste CONCAT, vom initia obiectul
  *
  * ------------------------------------------------------------------
  * |                   CONCAT                                       |
  * |                                                                |
  * |               (NFA1 vid) (NFA2 vid)                            |
  * |----------------------------------------------------------------|
  *
  * la citirea UNION vom avea
  * ------------------------------------------------------------------
  * |                   UNION                                        |
  * |                                                                |
  * | (stare initiala vida) (NFA1 vid) (NFA2 vid) (stare finala vida)|
  * |----------------------------------------------------------------|
  *
  * la citirea STAR vom avea
  *
  * ------------------------------------------------------------------
  * |                      STAR                                      |
  * |                                                                |
  * | (stare initiala vida) (NFA vid) (stare finala vida)            |
  * |----------------------------------------------------------------|
  *
  * Fiecare obiect va fi pus intr-o stiva speciala, o stiva a operatiilor.
  *
  * Am initiat obiecte de acest tip, pentru ca nu stim in acel moment ce automate s-au creat dupa ce am citit numele operatiei.
  * Stim doar ca trebuie sa o aplicam, dar inca nu avem pe ce.
  *
  * In momentul in care, dupa ce am inclus obiecte de acest tip in stiva, apar primele simboluri,
  * vor aparea asadar si primele automate. Din momentul in care apare un nou automat, acesta se va adauga la ultima
  * operatie de pe stiva operatiilor. Pe scurt, in momentul in care incep sa apara automate, se va completa operatia
  * situata in  varful stivei. Cand operatia curenta de pe varful stivei a fost completata, o scoatem de pe stiva,
  * iar automatul generat va trece ca parametru la operatia urmatoare, care este noul varf al stivei. Procedeul continua
  * pana cand ajungem la o operatie ce nu mai poate fi completata, caz in care citirea sirului continua.
  *
  * Un exemplu: daca primim ca sir CONCAT a b, iar starile sunt numere intregi, iar starea vida are indexul -1.
  *
  * Pe langa stiva a obiectelor de tip operatie, vom avea nevoie si de o stiva care retine denumirea simbolurile si NUMELE operatiilor
  *
  * La citirea CONCAT vom avea
  *
  * ----------- -stiva stringuri
  * | CONCAT  |
  * ----------
  *  ------------------------------------------------------------------     -stiva operatiilor
  * |                   CONCAT                                       |
  * |                                                                |
  * |                     (NFA1 vid) (NFA2 vid)                      |
  * |----------------------------------------------------------------|
  *
  * La citirea a vom avea
  * --------
  * |  a    |
  * ----------- -stiva stringuri
  * | CONCAT  |
  * ---------- . Pentru ca in varful stivei string-urilor am gasit un simbol, il scoatem, cream automatul corespunzator lui a si vom avea
  *
  * ----------- -stiva stringuri
  * | CONCAT  |
  * ----------
  *   ------------------------------------------------------------------     -stiva operatiilor
  * |                   CONCAT                                       |
  * |                                                                |
  * |                (NFA1 a)     (NFA2 vid)                         |
  * |----------------------------------------------------------------|
  * In momentul creeari automatului NFA pentru a, starile utilizate au indicii 0 si 1
  * Pentru ca in acest moment nu stiu cine este NFA2, continuam citirea.
  *
  * La citirea lui b vom avea
  * ----------
  * |   b     |
  * ----------- -stiva stringuri
  * | CONCAT  |
  * ---------- Vedem simbolul b, deci cream un automat cu indicii starilor 2 si 3 si care il accepta pe b
  * Stiva string-urilor devine
  *  ----------- -stiva stringuri
  * | CONCAT  |
  * ----------
  *
  * Stiva operatiilor devine
  *
  *   ------------------------------------------------------------------     -stiva operatiilor
  * |                   CONCAT                                       |
  * |                                                                |
  * |               (NFA1 a) (NFA2 b)                                |
  * |----------------------------------------------------------------|
  * In acest moment avem automatele necesare pentru CONCAT, deci executam operatia CONCAT
  * si ne va rezulta automatul obitnut prin concatenarea NFA-ului pentru a cu NFA-ul pentru b.
  *
  * Similar se face si pentru UNION si STAR, cu precizarea ca, dupa ce am gasit automatele necesare
  * executarii operatiilor, trebuiesc adaugate si indicii noilor stari, astfel incat acestea sa nu coincida
  * cu starile automatului dat.
  * */
  def pregatesteOperatie[A](nume:String, stareVida:A):Operatie[A]=nume match {
    case "CONCAT" =>
      var NFA1 = Nfa.apply(stareVida)
      var NFA2 = Nfa.apply(stareVida)
      new CONCAT[A](NFA1, NFA2)
    case "UNION"=>
      var NFA1 = Nfa.apply(stareVida)
      var NFA2 = Nfa.apply(stareVida)
      new UNION[A](NFA1,NFA2,stareVida,stareVida)
    case "STAR"=>
      var NFA = Nfa.apply(stareVida)
      new STAR[A](NFA,stareVida,stareVida)

  }
  //Temporar, cazurile MAYBE si PLUS nu sunt disponibile
  def esteOperatie(nume:String):Boolean= nume match {
    case "CONCAT"=>true
    case "UNION"=>true
    case "STAR"=>true
    case _=>false
  }
  def verificaOperatiePregatita[A](nume:String, op:Operatie[A], NFAprec:Nfa[A], stareVida:A, stareCurenta:A, stareUrmatoare:A): Boolean =nume match {
    case "CONCAT"=>
      var op1 = op.asInstanceOf[CONCAT[A]]
      if(op1.NFA1.equals(Nfa.apply(stareVida))){
        op1.NFA1=NFAprec
        false
      }
      else{
        if(op1.NFA2.equals(Nfa.apply(stareVida))) {
          op1.NFA2=NFAprec
        }
        true
      }
    case "UNION"=>
      var op1 = op.asInstanceOf[UNION[A]]
      if(op1.NFA1.equals(Nfa.apply(stareVida))){
        op1.NFA1=NFAprec
        false
      }
      else{
        if(op1.NFA2.equals(Nfa.apply(stareVida))) {
          op1.NFA2=NFAprec
        }
        op1.stareInitialaNoua=stareCurenta
        op1.stareFinalaNoua=stareUrmatoare
        true
      }
    case "STAR"=>
      var op1=op.asInstanceOf[STAR[A]]
      if(op1.NFA.equals(Nfa.apply(stareVida))){
        op1.NFA=NFAprec
      }
      op1.stareInitialaNoua=stareCurenta
      op1.stareFinalaNoua=stareUrmatoare
      true
  }
  def fromPrenex(str: String): Nfa[Int] = {
    var stivaOperatiilor = mutable.Stack.empty[Operatie[Int]]//stiva obiectelor de tip operatie
    var stivaString = mutable.Stack.empty[String]//aici stocam denumirile operatiilor si denumirile simbolurilor
    val sir_de_parsat =str
    val parti_componente = sir_de_parsat.split(' ')//impartim in token-uri
    var indice_stare =0//lucram cu numere de la 0 in sus. Starea vida are indicele -1
    var automatFinal:Nfa[Int]=Nfa.apply(-1)
    val automatVid:Nfa[Int]=Nfa.apply(-1)
    for(parte<-parti_componente){
      stivaString.push(parte)
      if(esteOperatie(parte)) {
        var op = pregatesteOperatie[Int](parte, -1)
        stivaOperatiilor.push(op)
      }
      else{
        var simbol=stivaString.pop()//extragem simbolul citit si ne vom avea acum in varful stivei denumirea unei operatii
        if(stivaString.nonEmpty) {
          var ultima_instructiune = stivaString.top
          var operatia_extrasa = stivaOperatiilor.top
          if(simbol.equals(""))
            simbol=" "
          automatFinal = Nfa.apply(indice_stare, simbol, indice_stare + 1)
          indice_stare+=2//crestem indicele pentru ca un automat are cel putin 2 stari, iar noi vrem ca acestea sa fie diferite
          var este_gata = verificaOperatiePregatita(ultima_instructiune, stivaOperatiilor.top, automatFinal,
            -1, indice_stare, indice_stare + 1)
          if((ultima_instructiune=="CONCAT")&&este_gata)
            indice_stare-=2//caz aparte, deoarece la CONCAT nu adaugam stari.
          while (este_gata) {//cat timp am avea operatii gata de executie
            automatFinal = Nfa.apply(operatia_extrasa)//actualizam automatul
            stivaString.pop()
            stivaOperatiilor.pop()//am terminat cu operatia curenta si mergem mai departe
            if (stivaOperatiilor.nonEmpty) {
              indice_stare+=2
              ultima_instructiune = stivaString.top
              operatia_extrasa = stivaOperatiilor.top
              este_gata = verificaOperatiePregatita(ultima_instructiune, stivaOperatiilor.top, automatFinal, -1, indice_stare, indice_stare + 1)
              if((ultima_instructiune=="CONCAT")&&este_gata)
                indice_stare-=2
            }
            else
              este_gata = false
          }
        }
        else {
          automatFinal=Nfa.apply(indice_stare,simbol,indice_stare+1)
        }
      }
    }
    automatFinal
  }// TODO implement Prenex -> Nfa transformation.

  // You can add more methods to this object
}