// Hello World in Java

class HelloWorld {
  static public void main( String args[] ) {
 System.out.println( "Hola! Trataré de adivinar un número. Piensa un número entre 1 y 10." );
try {
    Thread.sleep(5000);
} catch(InterruptedException ex) {
    Thread.currentThread().interrupt();
}
System.out.println( "Ahora multiplícalo por 9." );
try {
    Thread.sleep(5000);
} catch(InterruptedException ex) {
    Thread.currentThread().interrupt();
}
System.out.println( "Si el número tiene 2 dígitos, súmalos entre si: Ej. 36 -> 3+6=9. Si tu número tiene un solo dígito, súmale 0." );
try {
    Thread.sleep(5000);
} catch(InterruptedException ex) {
    Thread.currentThread().interrupt();
}
System.out.println( "Al número resultante súmale 4." );
try {
    Thread.sleep(10000);
} catch(InterruptedException ex) {
    Thread.currentThread().interrupt();
}
System.out.println( "Muy bien. El resultado es 13 :3 " );
} 

}
