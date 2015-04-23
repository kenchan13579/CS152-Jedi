package ui

class UndefinedException(e : String ="UndefinedException") extends JediException(e){
def msg:String = e
}