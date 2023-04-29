import scala.collection.mutable

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def epsInchidere(stareNFA:Int,acc:mutable.Set[Int],relatiile_de_tranzitie:mutable.Set[(Int,String,Int)]):mutable.Set[Int]={
    val stari_posibile=relatiile_de_tranzitie.filter(rel=>rel._2.equals("eps")&&rel._1.equals(stareNFA))
    acc+=stareNFA
    if(stari_posibile.isEmpty)
      acc
    else{
      stari_posibile.foldLeft(acc)((acc,elem)=>acc++epsInchidere(elem._3,acc, relatiile_de_tranzitie))
    }
  }

  def tranzitiiSimbol(stareNFA:Int,simbol:String,acc:mutable.Set[Int],relatiile_de_tranzitie:mutable.Set[(Int,String,Int)]):mutable.Set[Int]={
    val listaTranzitii=relatiile_de_tranzitie.filter(elem=>(elem._1==stareNFA)&&elem._2.equals(simbol))
    if(listaTranzitii.isEmpty)
      acc
    else{
      listaTranzitii.foldLeft(acc)((acc_aux,elem)=>acc_aux++epsInchidere(elem._3,mutable.Set.empty[Int],relatiile_de_tranzitie))
    }
  }
  var stariVerificate:mutable.Map[mutable.Set[Int],Int]=mutable.Map.empty

  def adauga_stari_si_tranzitii(grupStariNFA:mutable.Set[Int],relatiile_de_tranzitie:mutable.Set[(Int,String,Int)],
                                accStari:mutable.Set[mutable.Set[Int]],accFctTranzitie:mutable.Map[(mutable.Set[Int],String),
    mutable.Set[Int]],SIGMA:Set[String]):Unit={
    var totalGrupuriStari:mutable.Set[mutable.Set[Int]]=mutable.Set.empty
    for(simbol<-SIGMA){
      var grup_stari_destinatie:mutable.Set[Int]=mutable.Set.empty[Int]
      for(stareNFA<-grupStariNFA){
        val tranzitii = tranzitiiSimbol(stareNFA,simbol,mutable.Set.empty[Int],relatiile_de_tranzitie)
        grup_stari_destinatie=grup_stari_destinatie|tranzitii
      }
      if(grup_stari_destinatie.isEmpty)
        accFctTranzitie+=((grupStariNFA,simbol)->mutable.Set[Int](-100))
      else{
        if(!accStari.contains(grup_stari_destinatie))
          accStari+=grup_stari_destinatie
        accFctTranzitie+=((grupStariNFA,simbol)->grup_stari_destinatie)
        totalGrupuriStari=totalGrupuriStari+grup_stari_destinatie
      }
    }
    stariVerificate+=(grupStariNFA->1)
    for(pereche_noua_stari<-totalGrupuriStari){
      if(!stariVerificate.contains(pereche_noua_stari)){
        stariVerificate+=(pereche_noua_stari->0)
        adauga_stari_si_tranzitii(pereche_noua_stari,relatiile_de_tranzitie,accStari,accFctTranzitie,SIGMA)
      }
    }
  }

  /*
  *Mai sus, am reluat functii de la constructia DFA-ului pornind de la un NFA.
  *Asta se datoreaza faptului ca DFA-ul din clasa Dfa este construit pornind doar de la o expresie prenex
  *  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    var liste_NFA:List[Nfa[Int]]=List()//parcurgem fiecare token si adaugam NFA-urile
    var indice_maxim:Int =0
    var indice_ordine:Int=0
    val ordine_specificatie:mutable.Map[Int,Int]=mutable.Map.empty//asociaza o stare finala de NFA la indexul unui token
    val mapare_nume_intreg:mutable.Map[Int,String]=mutable.Map.empty//asociaza indexul unui token cu numele corespunzator
    for(str<-spec.split(";")) {
      val componente =str.split(":")
      var token=componente(0)
      if(token.head=='\n')
        token=token.tail
      if(componente.size>1){
        mapare_nume_intreg+=(indice_ordine->token)
        var regex = componente(1)
        regex=regex.tail
        if(token.contains("NEWLINE"))
          regex="\n"
        val formaPrenex=Regex.toPrenex(regex)
        val automat = Nfa.fromPrenex(formaPrenex)
        liste_NFA=liste_NFA:+automat.map(x=>x+indice_maxim)
        indice_maxim+=automat.K.max+1
        indice_ordine+=1
      }
    }
    //dupa ce am alcatuit lista NFA-urilor e timpul sa construim un nou NFA pornind de la acestea
    var K_nou_NFA:mutable.Set[Int]=mutable.Set.empty[Int]
    var nou_Sigma:mutable.Set[String]=mutable.Set.empty[String]
    var Q0_nou:Int=indice_maxim
    var completare_relatii_tranzitie:mutable.Set[(Int,String,Int)]=mutable.Set.empty[(Int,String,Int)]
    var stari_finale:mutable.Set[Int]=mutable.Set.empty[Int]
    indice_ordine=0
    //in cele ce urmeaza vom construi un NFA
    for(automat<-liste_NFA){//pentru fiecare NFA
      K_nou_NFA=K_nou_NFA|automat.K//completam multimea starilor
      nou_Sigma=nou_Sigma|automat.Sigma//completam alfabetul
      completare_relatii_tranzitie=completare_relatii_tranzitie|automat.DELTA//completam cu tranzitiile existente
      completare_relatii_tranzitie=completare_relatii_tranzitie+((Q0_nou,"eps",automat.Q0))//adaugam o epsilon tranzitie de la starea initiala noua la starea initiala a automatului din lista
      stari_finale=stari_finale+automat.F//completam starile finale
      ordine_specificatie+=(automat.F->indice_ordine)//fiecare stare finala este asociata unui index, index ce duce la denumirea token-ului
      indice_ordine+=1
    }
    //construirea DFA-ului
    val aux_SIGMA:Set[String]=nou_Sigma.toSet
    val Q0_nou_DFA=epsInchidere(Q0_nou,mutable.Set[Int](),completare_relatii_tranzitie)
    val stariDFA:mutable.Set[mutable.Set[Int]]=mutable.Set.empty
    stariDFA+=Q0_nou_DFA
    stariDFA+=mutable.Set[Int](-100)//sink
    val fctTranzitie:mutable.Map[(mutable.Set[Int],String),mutable.Set[Int]]=mutable.Map.empty
    adauga_stari_si_tranzitii(Q0_nou_DFA,completare_relatii_tranzitie,stariDFA,fctTranzitie,aux_SIGMA)//in aceasta functie adaugam tranzitiile si starile noului DFA
    val stari_finale_DFA:mutable.Set[mutable.Set[Int]]=mutable.Set.empty
    /*
    * Acum gasim starile finale si asociem fiecareia multimea de token-uri pe care s-ar face match
    * */
    val incadrari_posibile:mutable.Map[mutable.Set[Int],mutable.Set[Int]]=mutable.Map.empty
    for(stare_DFA<-stariDFA) {
      val intersectie = stare_DFA & stari_finale
      if (intersectie.nonEmpty) {
        stari_finale_DFA += stare_DFA
        val ordine:mutable.Set[Int]=mutable.Set.empty
        for(st<-intersectie)
          ordine+=ordine_specificatie(st)
        incadrari_posibile+=(stare_DFA->ordine)
      }
    }
    //incepem parcurgerea din starea initiala
    var stare_actuala=Q0_nou_DFA
    val sink_state=mutable.Set[Int](-100)
    var lexem=""
    var prev_str=""//trebuie sa retinem portiunea din cuvantul ramas de fiecare data cand trecem printr-o stare finala
    var prev_lexem=""//retinem lexemul gasit cand trecem printr-o stare finala
    var ultima_stareFinala:mutable.Set[Int]=mutable.Set.empty
    var remaining_string=word
    var rezultat: Either[String,List[(String,String)]]=Left("")
    var lista_token_lexema:List[(String,String)]=List()
    var eroare_serioasa:Boolean =false
    var caractere_citite=0
    var linie_curenta=0
    while(remaining_string.nonEmpty&&(!eroare_serioasa)){
      var conditie=true//se refera la conditia de inaintare(sa nu ajungem in sink)
      while(!stare_actuala.equals(sink_state)&&conditie){
        if(stari_finale_DFA.contains(stare_actuala)) {//daca am ajuns intr-o stare finala
          prev_str=remaining_string//retinem portiunea de cuvant ramasa
          prev_lexem=lexem//retinem lexemul in acel moment
          ultima_stareFinala=stare_actuala//si retinem ultima stare finala
        }
        if(remaining_string!=""){//daca mai avem de parcurs
          if(fctTranzitie((stare_actuala,word.head.toString)).isEmpty) {//daca nu exista tranzitie pe caracterul dat, intoarcem eroare
            conditie=false
            eroare_serioasa=true
            rezultat=Left("No viable alternative at character "+remaining_string.head+", line "+linie_curenta)
          }
          else{//altfel, actualizam lexemul
            lexem+=remaining_string.head
            stare_actuala=fctTranzitie((stare_actuala,remaining_string.head.toString))//mergem in starea urmatoare
            remaining_string=remaining_string.tail//consumam un caracter
            caractere_citite+=1//actualizam numarul de caractere consumate
            if(remaining_string!="") {
              if(remaining_string.head=='\n')
                linie_curenta+=1
            }
          }
        }
        else
          conditie=false
      }
      if((stare_actuala.equals(sink_state)&&ultima_stareFinala.nonEmpty)||remaining_string.isEmpty){
        val indicele_minim=incadrari_posibile(ultima_stareFinala).min//luam indicele tokenul-ui cu pozitia cea mai mica din specificatie
        val denumire=mapare_nume_intreg(indicele_minim)//gasim numele acestuia
        lista_token_lexema=lista_token_lexema:+(prev_lexem,denumire)//actualizam lista lexemelor
        stare_actuala=Q0_nou_DFA//reluam cautarea din starea initiala
        remaining_string=prev_str//plecam dela ultimul sir gasit la detectarea ultimei stari finale
        lexem=""//gasim un nou lexem
        ultima_stareFinala=mutable.Set.empty
      }
      else if(stare_actuala.equals(sink_state)&&ultima_stareFinala.isEmpty) {
        eroare_serioasa=true
        rezultat=Left("No viable alternative at character EOF"+", line "+linie_curenta)
      }
    }
    if(remaining_string.isEmpty)
      rezultat=Right(lista_token_lexema)
    rezultat
  }
}
