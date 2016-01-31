import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Karta {
	int hodnota;
	String farba;

	public Karta(int hodnota, String farba) {
		this.hodnota = hodnota;
		this.farba = farba;
	}

	public void setFarba(String farba) {
		this.farba = farba;
	}

	public int getHodnota() {
		return hodnota;
	}

	public void setHodnota(int hodnota) {
		this.hodnota = hodnota;
	}

	public String getFarba() {
		return farba;
	}

	public String toString() {
		return farba + "/" + hodnota;
	}
}

public class MapFilter {

	public static void main(String[] args) {
		List<Karta> karty = new ArrayList<Karta>();
		karty.add(new Karta(7,"Gula"));
		karty.add(new Karta(8,"Zalud"));
		karty.add(new Karta(9,"Cerven"));
		karty.add(new Karta(10,"Zelen"));
		System.out.println(karty);
		
		karty.forEach(k -> k.setFarba("Cerven"));
		System.out.println(karty);
		
		Stream<Karta> vacsieKartyStream = 
			karty.stream().filter(k -> k.getHodnota() > 8);
		List<Karta> vacsieKarty = vacsieKartyStream.collect(Collectors.toList());
		System.out.println(vacsieKarty);
		
		List<Karta> vacsieKarty2 = 
				karty
				.stream()
				.filter(k -> k.getHodnota() > 8)
				.collect(Collectors.toList());
		System.out.println(vacsieKarty2);
		
		List<Karta> vacsieKarty3 = 
				karty
				.stream()
				.map(k->new Karta(k.getHodnota()+1,k.getFarba()))
				.filter(k -> k.getHodnota() > 8)
				.collect(Collectors.toList());
		System.out.println(vacsieKarty3);
		
		List<Karta> vacsieKarty4 = 
				karty
				.stream()
				.parallel()
				.filter(k -> k.getHodnota() > 8)
				.sequential()
				.collect(Collectors.toList());
		System.out.println(vacsieKarty4);			
		
	}
}
