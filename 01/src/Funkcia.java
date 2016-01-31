import java.util.Arrays;
import java.util.Comparator;

public class Funkcia {

	public static void main(String[] args) {
		String[] pole = { "GULA", "cerven", "zelen", "ZALUD" };
		Comparator<String> comp = 
				(fst, snd) -> Integer.compare(
						fst.length(), snd.length());
				
		Arrays.sort(pole, comp);
		for (String e : pole) System.out.println(e);

		//Comparator<String> comps = 
		//		(String fst, String snd) -> 
		//				fst.toUpperCase().compareTo(snd.toUpperCase());
		
		Arrays.sort(pole, 
				(String fst, String snd) -> 
		fst.toUpperCase().compareTo(snd.toUpperCase()));
		
		for (String e : pole)
			System.out.println(e);
		
	}
}
