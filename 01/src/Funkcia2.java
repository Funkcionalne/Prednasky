public class Funkcia2 {

	public static void main(String[] args) {
		Runnable w = () -> {
			System.out.println("just do it !");
		};
		new Thread(w).start();
	}
}
