

object pyramides extends App{
  var hauteur = 3
  var taille = hauteur*(hauteur+1)/2
  var permutations : List[Array[Int]] = Nil
  var tab = Array.fill(taille)(0)
  var juste = 0;

  
  def indice(l:Int,col:Int):Int={
    return l*(l-1)/2+col-1
  }
  
  
  
  def duplique(src : Array[Int]) : Array[Int] = {
    var dst = Array.fill(src.length)(0)
    for(i<-0 to src.length-1)
      dst(i)=src(i)
    return dst
  }
  
  def genere(rang : Int,tab : Array[Int]){
    if(rang >= tab.length){
      permutations = duplique(tab)::permutations
    }
    else{
      for(valeur <- 1 to tab.length){
        var dejaPris = false
        for(i<-0 to rang-1){
          if(tab(i)==valeur) dejaPris = true
        }
        if(!dejaPris){
            tab(rang)=valeur
            if (correcte(tab, rang)) genere(rang+1, tab)
          }
      }
    }
  }
  
  def correcte(tab:Array[Int],rang : Int):Boolean = {
    var permutation = new String()
    for(i<-0 to tab.length-1){
      permutation += tab(i)
    }
    for(ligne<-1 to hauteur-1){
      for(col<-1 to ligne){
        if (indice(ligne+1,col+1) <= rang) {
          val n1 = tab(indice(ligne+1,col))
          val n2 = tab(indice(ligne+1,col+1))
          val n3 = tab(indice(ligne,col))
          if(math.abs(n1-n2)!=n3) return false
        }
      }
    }
   return true
  }
  
  def affiche(t : Array[Int]){
    print("[")
    for(i<-0 to t.length-1){
      print(" "+t(i))
    }
    print("] \n")
  }
  
  genere(0,tab)
  var l = permutations
  var nb = 0;
  while(l != Nil){
    affiche(l.head)
    l=l.tail
    nb += 1
  }
  print("Nb : "+nb)
  
}