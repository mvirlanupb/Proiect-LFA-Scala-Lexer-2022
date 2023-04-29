import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  var ultimul_string:String=""
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    if(s.isEmpty)
      List()
    else{
      if((s.head=='*')||(s.head=='(')||(s.head==')')||(s.head=='|')||(s.head=='\'')||(s.head=='?')||(s.head=='+')) {
          Left(s.head)+:preprocess(s.tail)//cazul in care primul caracter este unul de control, sau daca este apostroful
      } else if(s.head!='['){
        ultimul_string=s.head.toString
        Right(s.head)+:preprocess(s.tail)
      }
      else{//cazul paranteza deschisa dreapta
        val capat_inceput =s.tail.head//vedem capatul initial din syntactic-sugar-ul dat
        var nou_string:String = "("//construim un nou string folosind reuniunea
        if((capat_inceput=='a')||(capat_inceput=='A')){//daca primul capat este litera a sau A
           var alfabet="";
           if(capat_inceput=='a')
            alfabet="abcdefghijklmnopqrstuvwxyz"
           else
             alfabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          for(litera<-alfabet){
            nou_string+=litera.toString
            if((litera!='z')&&(litera!='Z'))
              nou_string+='|'
          }
        }
        else{//e vorba de 0
          val totalulCifrelor="0123456789"
          for(cifra<-totalulCifrelor){
            nou_string+=cifra
            if(cifra!='9')
              nou_string+='|'
          }
        }
        nou_string+=')'
        val restul_stringului_original = s.drop(5)//am terminat cu selectia si mergem mai departe
        preprocess(nou_string.toList++:restul_stringului_original)
      }
    }
  }
  def eliminareApostroafe(str:String):String={//functie care plecand de la un sir cu caractere escapate, elimina ghilimelele simple
    val tipuri_caractere = preprocess(str.toList)
    def aux_eliminareApostroafe(clasificare:List[Either[Char,Char]],acc:String):String={
      if(clasificare.isEmpty)
          acc
      else{
        if(clasificare.head==Left('\'')) {
            val caracterul_escapat = clasificare.tail.head.fold(stanga=>stanga,caracter=>caracter)//eliberam caracterul escapat
            aux_eliminareApostroafe(clasificare.drop(3),acc+caracterul_escapat.toString)
        }
        else{
          val caracter_normal = clasificare.head.fold(control=>control,caracter_simplu=>caracter_simplu)
          aux_eliminareApostroafe(clasificare.tail,acc+caracter_normal.toString)
        }
      }
    }
    aux_eliminareApostroafe(tipuri_caractere,"")
  }
  var prioritati:mutable.Map[Char,Int]=mutable.Map.empty
  prioritati+=('|'->1)
  prioritati+=('.'->2)
  prioritati+=('*'->3)
  prioritati+=('('->4)
  prioritati+=(')'->4)
  prioritati+=('?'->1)
  prioritati+=('+'->2)


  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    var numere_sir:Int=0
    if(str.equals("eps"))
      return str
    /*
    * Mai intai de toate obtinem lista cu tipurile caracterelor (control sau caractere simple) dupa
    * eliminarea apostroafelor
    * */
    var sir_preprocesat=preprocess(eliminareApostroafe(str).toList)
    /*
    * In aceasta functie auxiliara se lucreaza cu lista tipurilor de caractere dupa preprocesare,
    * lista care se va actualiza (modifica, in anumite situatii) dinamic.
    * Ideea din spatele algoritmului:
    *  In mod implicit ne-am dori ca in timp ce parcurgem aceasta lista, sa avem numai caractere simple
    *  de aceea, de fiecare data cand intalnim un caracter simplu, il punem intr-o stiva care ne va ajuta
    *  sa obtinem forma prenex. In aceasta situatie, daca nu am intalni alte operatii (uniune, star, sau expresii
    *  in paranteze), rezultatul final este concatenarea a tot ce avem in stiva.
    *  Daca insa intalnim simbolul reuniunii, operatia cu cea mai mica prioritate, am fi in aceasta situatie
    *   expr1 | expr2, unde expr2 nu a fost inca prelucrata.
    *  Exact ca atunci cand tragem linie aici, rezultatul va fi UNION+expr1 prelucrata, la care vom adauga
    *  rezultatul prelucrarii expr2 folosind recursivitatea.
    *  Daca intalnim simbolul Kleene Star *, expresia din varful stivei este cea pe care vom aplica Kleene Star.
    *  In cazul in care intalnim paranteza deschisa (, apelam din nou functia si se va prelucra expresia din
    *  paranteza (pana la intalnirea cu paranteza inchisa ')' corespunzatoare), deoarece aceasta are prioritatea maxima.
    * */
    def toPrenex_aux:String={
      var stiva_nume_operatii:mutable.Stack[String]=mutable.Stack.empty[String]
      var stiva_procesare:mutable.Stack[String]=mutable.Stack.empty[String]
      var conditie_inaintare:Boolean=true
      var rezultat_etapa=""
      var lista_precedenta:List[Either[Char,Char]]=List()
      while(conditie_inaintare&&sir_preprocesat.nonEmpty){
        val caracter = sir_preprocesat.head
        if(caracter.isRight&&conditie_inaintare){
          stiva_procesare.push(caracter.fold(_ =>"", dreapta=>dreapta.toString))
          lista_precedenta=sir_preprocesat
          sir_preprocesat=sir_preprocesat.tail
        }
        else{
          val caracter_operatie=caracter.fold(stanga=>stanga.toString, _ =>"")
          if(prioritati(caracter_operatie.toCharArray.head)==1){
            sir_preprocesat=sir_preprocesat.tail
            var rezultat_sus = toPrenex_aux
            if(rezultat_sus.equals(""))
              rezultat_sus="eps"
            if(!caracter_operatie.equals("?")){
              conditie_inaintare=false
              while(stiva_procesare.size>=2){
                var arg2=stiva_procesare.pop()
                var arg1=stiva_procesare.pop()
                if(arg1.equals(" "))
                  arg1=""
                if(arg2.equals(" "))
                  arg2=""
                val rezultat_partial="CONCAT "+arg1+" "+arg2
                stiva_procesare.push(rezultat_partial)
              }
              val rezultat_jos=stiva_procesare.pop()
              if(!rezultat_jos.equals(" "))
                rezultat_etapa ="UNION "+rezultat_jos+" "+rezultat_sus
              else
                rezultat_etapa="UNION "+" "+rezultat_sus
            }
            else{
              var lista_noua:List[Either[Char,Char]]=List()
              lista_noua=Left('(')+:lista_noua
              val ultima_expresie = stiva_procesare.pop()
              for(caracter_expr<-ultima_expresie)
                lista_noua=lista_noua:+Right(caracter_expr)
              lista_noua=lista_noua:+Left('|')
              lista_noua=lista_noua:+Left(')')
              if(!ultima_expresie.equals(" "))
                rezultat_etapa="UNION "+ultima_expresie+" "+"eps"
              else
                rezultat_etapa="UNION "+" "+"eps"
              stiva_procesare.push(rezultat_etapa)
              if(!rezultat_sus.equals("eps"))
                stiva_procesare.push(rezultat_sus)

            }
          }
          else{
            if(prioritati(caracter_operatie.toCharArray.head)==3){//STAR
              val arg=stiva_procesare.pop()
              val rezultat_partial="STAR "+arg
              stiva_procesare.push(rezultat_partial)
              sir_preprocesat=sir_preprocesat.tail
            }
            else if(prioritati(caracter_operatie.toCharArray.head)==4){
              if(caracter_operatie.equals("(")){
                numere_sir=1
                sir_preprocesat=sir_preprocesat.tail
                val rezultat_partial=toPrenex_aux
                stiva_procesare.push(rezultat_partial)
              }
              else {
                conditie_inaintare=false
                sir_preprocesat=sir_preprocesat.tail
              }
            }
            else{
              val vf_stiva=stiva_procesare.pop()
              stiva_procesare.push("CONCAT "+vf_stiva+" "+"STAR"+" "+vf_stiva)
              sir_preprocesat=sir_preprocesat.tail
            }
          }
        }
      }
      if(stiva_procesare.size==1)
        rezultat_etapa=stiva_procesare.pop()
      while(stiva_procesare.size>=2){
        val arg2=stiva_procesare.pop()
        var arg1=stiva_procesare.pop()
        if(arg1.equals(" "))
          arg1=""
        val rezultat_partial="CONCAT "+arg1+" "+arg2
        rezultat_etapa=rezultat_partial
        stiva_procesare.push(rezultat_partial)
      }
      rezultat_etapa
    }
    toPrenex_aux
  }

  def main(args: Array[String]): Unit = {
    val prenex= Regex.toPrenex("' 'a' '")
    println(prenex)
    println(Dfa.fromPrenex(prenex).accepts(" a "))
  }
}
