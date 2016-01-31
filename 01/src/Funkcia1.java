public class Funkcia1 {

	public static void main(String[] args) {
		Worker w = new Worker();
		new Thread(w).start();
	}
}
