import scala.collection.mutable
//consideram ca un DFA e construit pe baza NFA-ului din algoritmul Thomson
//adaugam si un sink state
class Dfa[A] (NFA_original:Nfa[A], val ID_sink:A){
   val K_NFA: mutable.Set[A] = NFA_original.K
   val Sigma_NFA: Set[String] = NFA_original.Sigma
   val Q0_NFA: A = NFA_original.Q0
   val DELTA_NFA: mutable.Set[(A, String, A)] = NFA_original.DELTA
   val F_NFA: A = NFA_original.F
   var valoriDeVerificat:mutable.Map[mutable.Set[A],Int]= mutable.Map.empty[mutable.Set[A],Int]


  /*
  * Aceasta metoda intoarce setul de stari la care putem ajunge prin inchiderea folosind
  * epsilon-tranzitii
  * */
  def epsInchidere(stare:A,acc:mutable.Set[A]):mutable.Set[A]={
    val contineEps = DELTA_NFA.filter(elem=>elem._1==stare && elem._2=="eps")
    acc+=stare
    if(contineEps.isEmpty)
      acc
    else{
      contineEps.foldLeft(acc)((acc,elem)=>acc++epsInchidere(elem._3,acc))
    }
  }
  def GenerareGrupStari(stare:A,simbol:String):mutable.Set[A]={//starea de aici este in NFA. Se genereaza pentru succesorul starii actuale pe simbolul "simbol"
    val succesori = DELTA_NFA.filter(elem=>elem._2==simbol && elem._1==stare)
    var rezultat:mutable.Set[A]=mutable.Set.empty[A]
    succesori.foldLeft(rezultat)((acc,elem)=>acc++epsInchidere(elem._3,acc))
  }
  /*
  * Aceasta metoda genereaza un map intre un grup de stari si un simbol, ca si cheie, iar valoarea este grupul de stari in care
  * se poate ajunge. Au fost incluse epsilon-tranzitiile.
  * Map-ul verificat ne ajuta pentru a evidentia care sunt grupurile de stari in curs de investigare si care au fost investigate.
  * */
  def GenereazaTranzitiiDFA(stareDFA:mutable.Set[A],acc:mutable.Map[(mutable.Set[A],String),mutable.Set[A]],verificat:mutable.Map[mutable.Set[A],Int])
  :mutable.Map[(mutable.Set[A],String),mutable.Set[A]]={
    var totalStariObtinute:mutable.Set[mutable.Set[A]]=mutable.Set.empty[mutable.Set[A]]
    for(simbol<-Sigma_NFA){//pentru fiecare simbol din alfabet
      var tranzitiiSimbol:mutable.Set[A]=mutable.Set.empty[A]//gasim grupul de stari in care putem ajunge plecand de la actualul grup de stari pe simbolul curent
      for(stareNFA<-stareDFA) {
        tranzitiiSimbol=tranzitiiSimbol|GenerareGrupStari(stareNFA,simbol)
        //stareDFA este un grup de stari din NFA, deci investigam fiecare bucata, mai intai gasind succesorul fiecarei stari NFA pe simbol
        //si dupa o inchidem prin epsilon-tranzitii
        // se formeaza astfel grupul de stari destinatie plecand de la grupul de stari dat ca parametru
      }
      if(tranzitiiSimbol.nonEmpty) {
        val cheie = (stareDFA,simbol)
        acc+=(cheie->tranzitiiSimbol)
        totalStariObtinute+=tranzitiiSimbol
      }
    }
    verificat(stareDFA)=1//verificarea "starii" este completa
    if(totalStariObtinute.isEmpty)
      acc
    else{
      for(stareObtinuta<-totalStariObtinute) {//luam fiecare grup de stari obtinute
        if(!verificat.contains(stareObtinuta)) {//daca stareaObtinuta nu este Map-ul verificat
          verificat+=(stareObtinuta->0)//o adaugam si o vom investiga
          acc++GenereazaTranzitiiDFA(stareObtinuta,acc,verificat)
        }
      }
      acc
    }
  }

  val Q0grup: mutable.Set[A] = epsInchidere(Q0_NFA,mutable.Set.empty[A])
  valoriDeVerificat+=(Q0grup->0)
  val tranzitiiSubset: mutable.Map[(mutable.Set[A], String), mutable.Set[A]]
  = GenereazaTranzitiiDFA(Q0grup,mutable.Map.empty[(mutable.Set[A],String),mutable.Set[A]],valoriDeVerificat)
  def stariPostSubsetConstruction(tranzitii:mutable.Map[(mutable.Set[A],String),mutable.Set[A]])
  :mutable.Set[mutable.Set[A]]={//avand tranzitiile, putem determina grupurile de stari
    var rezultat:mutable.Set[mutable.Set[A]]=mutable.Set.empty[mutable.Set[A]]
    val chei = tranzitii.keySet
    chei.foldLeft(rezultat)((acc,elem)=>acc+=elem._1)
    chei.foldLeft(rezultat)((acc,elem)=>acc+=tranzitii(elem))
    if(chei.isEmpty)
      rezultat+=Q0grup
    rezultat
  }

  val K_intermediar: mutable.Set[mutable.Set[A]]= stariPostSubsetConstruction(tranzitiiSubset)
  //pentru a fi usor de manevrat DFA-ul, schimbam starile

  def getStates : Set[A] = {
   //pentru simplitate, vom redenumi starile DFA-ului
    // nu vor ramane grupuri de stari, ci acestea vor prelua din denumirile starilor din NFA
    //comportamentul ramane cel al unui DFA
    def getStates_aux(acc:Set[A], grupuriStari:mutable.Set[mutable.Set[A]], stariNFA:mutable.Set[A]):Set[A]={
      if(grupuriStari.isEmpty)
        acc
      else{
        getStates_aux(acc+stariNFA.head,grupuriStari.tail,stariNFA.tail)
      }
    }
    getStates_aux(Set[A](),K_intermediar,K_NFA)
  } // TODO implement getStates

  val K_DFA: Set[A] = getStates
  //asocieri intre grupurile de stari obtinute din SubsetConstruction si noile stari
  def creeazaAsocieri(acc:Map[mutable.Set[A],A], grupuriStari:mutable.Set[mutable.Set[A]],stariNFA:mutable.Set[A]):Map[mutable.Set[A],A]={
    if(grupuriStari.isEmpty)
        acc
    else{
      creeazaAsocieri(acc+(grupuriStari.head->stariNFA.head),grupuriStari.tail,stariNFA.tail)
    }
  }
  var asocieriGrupuriStariSiStariNoi = creeazaAsocieri(Map[mutable.Set[A],A](),K_intermediar,K_NFA)
  //metoda de mai jos nu face altceva decat sa schimbam aspectul functiei de tranzitie
  def tranzitiiNoi(tranzitiiSubsetConstruction:mutable.Map[(mutable.Set[A],String),mutable.Set[A]]):Map[(A,String),A]={
     def tranzitiiNoi_aux(acc:Map[(A,String),A], from:mutable.Map[(mutable.Set[A],String),mutable.Set[A]]):Map[(A,String),A]={
        if(from.isEmpty)
          acc
        else{
           val tranzitie = from.head
           val nou_capat_stanga = asocieriGrupuriStariSiStariNoi(tranzitie._1._1)
           val nou_capat_dreapta = asocieriGrupuriStariSiStariNoi(tranzitie._2)
           val simbol_curent = tranzitie._1._2
           tranzitiiNoi_aux(acc+((nou_capat_stanga,simbol_curent)->nou_capat_dreapta),from.tail)
        }
     }
    tranzitiiNoi_aux(Map[(A,String),A](),tranzitiiSubsetConstruction)
  }
  var K_aux: mutable.Set[A] = K_NFA;
  var tranzitiiDFA: Map[(A, String), A] =tranzitiiNoi(tranzitiiSubset)
  def next(state:A, c: Char): A = {
    if(Sigma_NFA.contains(c.toString))
      tranzitiiDFA(state,c.toString)
    else{//in cazul in care am avea de cautat o tranzitie pe un caracter care nu e din sigma, aceea se va duce in sink state
      tranzitiiDFA+=((state,c.toString)->ID_sink)
      ID_sink
    }
  }// TODO implement next


 //starile finale in DFA-ul din Subset Construction sunt acele grupuri de stari care contin starea finala a NFA-ului
  //metoda intoarce, folosind asocieri starile finale.
  def obtineStarileFinale(stariSubsetConstruction:mutable.Set[mutable.Set[A]],acc:Set[A]):Set[A]={
     if(stariSubsetConstruction.isEmpty)
       acc
     else{
       val grupStari = stariSubsetConstruction.head
       if(grupStari.contains(F_NFA))
         obtineStarileFinale(stariSubsetConstruction.tail,acc+asocieriGrupuriStariSiStariNoi(grupStari))
       else
         obtineStarileFinale(stariSubsetConstruction.tail,acc)
     }
  }

  val starileFinale: Set[A] = obtineStarileFinale(K_intermediar,Set[A]())
  val Q0_DFA: A =asocieriGrupuriStariSiStariNoi(Q0grup)

  def adaugaSinkState:Set[A]={
    //iteram prin toate starile. Intoarcem multimea finala a starilor
    def adauga_aux(ID_sink:A,stari:Set[A]):Unit={
       if(stari.nonEmpty){
         val stare_curenta = stari.head
         //iteram prin tranzitiile posibile de pe toate simbolurile
         for(simbol<-Sigma_NFA){
           if(!tranzitiiDFA.contains(stare_curenta,simbol))
             tranzitiiDFA+=((stare_curenta,simbol)->ID_sink)
        }
         adauga_aux(ID_sink,stari.tail)
      }
    }

    adauga_aux(ID_sink,K_DFA)
    for(simbol<-Sigma_NFA)
      tranzitiiDFA+=((ID_sink,simbol)->ID_sink)
    K_DFA+K_aux.head
  }
  val K_DFA_FINAL: Set[A] = adaugaSinkState
  def accepts(str: String): Boolean = {
     def accepts_aux(str:String, stare_actuala:A):Boolean= str match {
       case ""=>
          starileFinale.contains(stare_actuala)
       case _=>
         var result:Boolean = false
         result=result||(accepts_aux(str.tail,next(stare_actuala,str.head)))
         result
     }
    accepts_aux(str,Q0_DFA)
  } // TODO implement accepts
  def isFinal(state: A): Boolean = starileFinale.contains(state) // TODO implement isFinal

  def map[B](f: A => B) : Dfa[B] = {
    val NFA_original_B = NFA_original.map(f)
    new Dfa[B](NFA_original_B,f(ID_sink))
  }// TODO implement map
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] ={
    val automatNedeterminist:Nfa[Int] = Nfa.fromPrenex(str)//am obtinut automatul nedeterminist din stringul str
    new Dfa[Int](automatNedeterminist,-100)
  }// TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  // You can add more methods to this object
}
